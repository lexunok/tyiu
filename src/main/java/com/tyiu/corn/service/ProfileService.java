package com.tyiu.corn.service;

import com.tyiu.corn.model.dto.ProfileDTO;
import com.tyiu.corn.model.dto.SkillDTO;
import com.tyiu.corn.model.entities.*;
import com.tyiu.corn.model.entities.mappers.ProfileMapper;
import com.tyiu.corn.model.entities.relations.User2Skill;
import com.tyiu.corn.model.enums.SkillType;
import com.tyiu.corn.model.requests.ProfileUpdateRequest;
import com.tyiu.corn.model.responses.ProfileIdeaResponse;
import com.tyiu.corn.model.responses.ProfileProjectResponse;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.io.FileSystemResource;
import org.springframework.core.io.Resource;
import org.springframework.core.io.ResourceLoader;
import org.springframework.core.io.UrlResource;
import org.springframework.data.r2dbc.core.R2dbcEntityTemplate;
import org.springframework.http.codec.multipart.FilePart;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.concurrent.ConcurrentHashMap;

import static org.springframework.data.relational.core.query.Criteria.where;
import static org.springframework.data.relational.core.query.Query.query;
import static org.springframework.data.relational.core.query.Update.update;


@Service
@Slf4j
@RequiredArgsConstructor
public class ProfileService {

    private final ProfileMapper mapper;
    private final R2dbcEntityTemplate template;
    private final ResourceLoader loader;
    @Value("${file.path}")
    String path;

    public Mono<ProfileDTO> getUserProfile(String userId) {
        String query = "SELECT u.id u_id, u.roles u_roles, u.email u_email, u.last_name u_last_name, u.first_name u_first_name, u.created_at u_created_at, " +
                "s.id s_id, s.name s_name, s.type s_type, i.id i_id, i.name i_name, i.description i_description, i.status i_status " +
                "FROM users u LEFT JOIN team ON team.id = u.id " +
                "LEFT JOIN idea i ON i.initiator_email = u.email LEFT JOIN user_skill us ON us.user_id = u.id " +
                "LEFT JOIN skill s ON s.id = us.skill_id WHERE u.id = :userId";
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
                                            .description(row.get("i_description",String.class))
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
                        profileDTO.setIdeas(ideas.values().stream().toList());
                        profileDTO.setSkills(skills.values().stream().toList());
                        return profileDTO;
                        });
                    }).last();
    }
    public Resource uploadAvatar(String userId,FilePart file){
        Path basePath = Paths.get(path, userId + "_avatar.jpg");
        file.transferTo(basePath).subscribe();
        try {
            return new FileSystemResource(basePath);

        } catch (Exception e) {
            return null;
        }
    }
    public Mono<Resource> getAvatar(String userId){
        Path basePath = Paths.get(path, userId + "_avatar.jpg");
        try {
            return Mono.just(new FileSystemResource(basePath));
        } catch (Exception e) {
            return null;
        }
    }

    public Flux<SkillDTO> saveSkills(String userId, Flux<SkillDTO> skills) {
        template.delete(query(where("user_id").is(userId)), User2Skill.class).subscribe();
        return skills.map(s -> {
            template.insert(new User2Skill(userId,s.getId())).subscribe();
            return s;
        });
    }
    public Mono<Void> updateFullName(String userId, ProfileUpdateRequest request){
        return template.update(query(where("id").is(userId)),
                update("first_name", request.getFirstName()).set("last_name", request.getLastName()), User.class).then();
    }
}
