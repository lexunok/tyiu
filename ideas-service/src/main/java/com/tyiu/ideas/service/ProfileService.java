package com.tyiu.ideas.service;

import com.tyiu.ideas.model.dto.ProfileDTO;
import com.tyiu.ideas.model.dto.SkillDTO;
import com.tyiu.ideas.model.entities.Idea;
import com.tyiu.ideas.model.entities.User;
import com.tyiu.ideas.model.entities.mappers.ProfileMapper;
import com.tyiu.ideas.model.entities.relations.Team2Member;
import com.tyiu.ideas.model.entities.relations.Team2Skill;
import com.tyiu.ideas.model.entities.relations.User2Skill;
import com.tyiu.ideas.model.enums.SkillType;
import com.tyiu.ideas.model.requests.ProfileUpdateRequest;
import com.tyiu.ideas.model.responses.ProfileIdeaResponse;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.io.FileSystemResource;
import org.springframework.core.io.Resource;
import org.springframework.core.io.ResourceLoader;
import org.springframework.data.r2dbc.core.R2dbcEntityTemplate;
import org.springframework.http.MediaType;
import org.springframework.http.codec.multipart.FilePart;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;

import static org.springframework.data.relational.core.query.Criteria.where;
import static org.springframework.data.relational.core.query.Query.query;
import static org.springframework.data.relational.core.query.Update.update;


@Service
@Slf4j
@RequiredArgsConstructor
public class ProfileService {
    private static final long MAX_FILE_SIZE_BYTES = 16 * 1024 * 1024;
    private final ProfileMapper mapper;
    private final R2dbcEntityTemplate template;
    private final ResourceLoader loader;

    @Value("${file.path}")
    String path;
    public Mono<FileSystemResource> uploadAvatar(String userId, FilePart file) {
        Path basePath = Paths.get(path);
        Path avatarPath = basePath.resolve(userId + "_avatar.jpg");

        if (!isValidImageFile(file) && file.headers().getContentLength() > MAX_FILE_SIZE_BYTES) {
            return Mono.error(new RuntimeException("Недопустимый тип или размер файла"));
        }

        try {
            Files.createDirectories(basePath);

            return file.transferTo(avatarPath)
                    .then(Mono.just(new FileSystemResource(avatarPath)))
                    .doOnError(e -> log.error("Ошибка загрузки аватара: {}", e.getMessage()));
        } catch (IOException e) {
            return Mono.error(new RuntimeException("Ошибка загрузки аватара", e));
        }
    }

    private boolean isValidImageFile(FilePart file) {
        MediaType contentType = file.headers().getContentType();
        return contentType != null && (contentType.isCompatibleWith(MediaType.IMAGE_JPEG) && contentType.isCompatibleWith(MediaType.IMAGE_PNG));
    }
    public Mono<Resource> getAvatar(String userId){
        Path basePath = Paths.get(path, userId + "_avatar.jpg");
        try {
            return Mono.just(new FileSystemResource(basePath));
        } catch (Exception e) {
            return null;
        }
    }
    public Mono<ProfileDTO> getUserProfile(String userId, String currentUserId) {
        String query = "SELECT u.id u_id, u.roles u_roles, u.email u_email, u.last_name u_last_name, u.first_name u_first_name, u.created_at u_created_at, " +
                "s.id s_id, s.name s_name, s.type s_type, i.id i_id, i.name i_name, i.solution i_solution, i.status i_status, " +
                "t.user_tag t_user_tag, t.is_visible t_is_visible " +
                "FROM users u LEFT JOIN team ON team.id = u.id " +
                "LEFT JOIN idea i ON i.initiator_id = u.id LEFT JOIN user_skill us ON us.user_id = u.id " +
                "LEFT JOIN skill s ON s.id = us.skill_id " +
                "LEFT JOIN users_telegram t ON t.user_email = u.email " +
                "WHERE u.id = :userId";
        return template.getDatabaseClient().sql(query)
                .bind("userId", userId)
                .flatMap(p -> {
                    ConcurrentHashMap<String,SkillDTO> skills = new ConcurrentHashMap<>();
                    ConcurrentHashMap<String,ProfileIdeaResponse> ideas = new ConcurrentHashMap<>();
                    ConcurrentHashMap<String,ProfileDTO> profiles = new ConcurrentHashMap<>();
                    return p.map((row,rowMetadata) -> {
                        String ideaId = row.get("i_id",String.class);
                        String skillId = row.get("s_id",String.class);
                        if (ideaId!=null) {
                            ideas.putIfAbsent(ideaId,
                                    ProfileIdeaResponse.builder()
                                            .id(ideaId)
                                            .status(Idea.Status.valueOf(row.get("i_status",String.class)))
                                            .name(row.get("i_name",String.class))
                                            .solution(row.get("i_solution",String.class))
                                            .build());
                        }
                        if (skillId!=null) {
                            skills.putIfAbsent(skillId,
                                    SkillDTO.builder()
                                            .id(skillId)
                                            .name(row.get("s_name",String.class))
                                            .type(SkillType.valueOf(row.get("s_type",String.class)))
                                            .build());
                        }
                        profiles.putIfAbsent(userId,mapper.apply(row,rowMetadata));
                        ProfileDTO profileDTO = profiles.get(userId);
                        if (Boolean.TRUE.equals(profileDTO.getIsUserTagVisible()) || currentUserId.equals(userId))
                            profileDTO.setUserTag(row.get("t_user_tag", String.class));
                        profileDTO.setIdeas(ideas.values().stream().toList());
                        profileDTO.setSkills(skills.values().stream().toList());
                        return profileDTO;
                    });
                }).last();
    }


    public Flux<SkillDTO> saveSkills(String userId, Flux<SkillDTO> skills) {
        template.delete(query(where("user_id").is(userId)), User2Skill.class)
                .then(skills.flatMap(s ->
                        template.insert(new User2Skill(userId,s.getId())))
                        .then()
                )
                .then(template.select(query(where("member_id").is(userId)), Team2Member.class)
                        .flatMap(t -> {
                            String QUERY = "SELECT user_skill.*, team_member.* " +
                                    "FROM team_member " +
                                    "LEFT JOIN user_skill ON user_skill.user_id = team_member.member_id " +
                                    "WHERE team_member.team_id = :teamId AND user_skill.user_id IS NOT NULL";

                            return template.delete(query(where("team_id").is(t.getTeamId())), Team2Skill.class)
                                    .then(template.getDatabaseClient()
                                            .sql(QUERY)
                                            .bind("teamId", t.getTeamId())
                                            .map((row, rowMetadata) -> Objects.requireNonNull(row.get("skill_id", String.class)))
                                            .all()
                                            .distinct()
                                            .flatMap(skill -> template.insert(new Team2Skill(t.getTeamId(), skill)))
                                            .then());
                        })
                        .then()
                ).subscribe();
        return skills;
    }

    public Mono<Void> updateFullName(String userId, ProfileUpdateRequest request){
        return template.update(query(where("id").is(userId)),
                update("first_name", request.getFirstName()).set("last_name", request.getLastName()), User.class).then();
    }
}
