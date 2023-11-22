package com.tyiu.corn.service;

import com.tyiu.corn.model.dto.ProfileDTO;
import com.tyiu.corn.model.dto.SkillDTO;
import com.tyiu.corn.model.entities.*;
import com.tyiu.corn.model.entities.mappers.ProfileMapper;
import com.tyiu.corn.model.entities.relations.User2Skill;
import com.tyiu.corn.model.enums.SkillType;
import com.tyiu.corn.model.responses.ProfileIdeaResponse;
import com.tyiu.corn.model.responses.ProfileProjectResponse;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.io.Resource;
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


@Service
@RequiredArgsConstructor
@Slf4j
public class ProfileService {

    private final ProfileMapper mapper;
    private final R2dbcEntityTemplate template;
    @Value("${file.path}")
    String path;

    public Mono<ProfileDTO> getUserProfile(String email) {
        String query = "SELECT u.id u_id, u.roles u_roles, u.email u_email, u.last_name u_last_name, u.first_name u_first_name, " +
                "s.id s_id, s.name s_name, s.type s_type, i.id i_id, i.name i_name, i.description i_description, i.status i_status," +
                " p.id p_id, p.name p_name, p.description p_description" +
                " FROM users u LEFT JOIN team ON team.id = u.id LEFT JOIN project p ON p.team_id = team.id " +
                "LEFT JOIN idea i ON i.initiator_email = u.email LEFT JOIN user_skill us ON us.user_id = u.id " +
                "LEFT JOIN skill s ON s.id = us.skill_id WHERE u.email = :email";
        return template.getDatabaseClient().sql(query)
                .bind("email", email)
                .flatMap(p -> {
                    ConcurrentHashMap<String,SkillDTO> skills = new ConcurrentHashMap<>();
                    ConcurrentHashMap<String,ProfileIdeaResponse> ideas = new ConcurrentHashMap<>();
                    ConcurrentHashMap<String,ProfileProjectResponse> projects = new ConcurrentHashMap<>();
                    ConcurrentHashMap<String,ProfileDTO> profiles = new ConcurrentHashMap<>();
                    return p.map((row,rowMetadata) -> {
                        String userId = row.get("u_id",String.class);
                        String ideaId = row.get("i_id",String.class);
                        String skillId = row.get("s_id",String.class);
                        String projectId = row.get("p_id",String.class);
                        if (projectId!=null) {
                            projects.putIfAbsent(projectId,
                                    ProfileProjectResponse.builder()
                                            .id(projectId)
                                            .name(row.get("p_name",String.class))
                                            .description(row.get("p_description",String.class))
                                            .build());
                        }
                        if (ideaId!=null) {
                            ideas.putIfAbsent(ideaId,
                                    ProfileIdeaResponse.builder()
                                            .id(ideaId)
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
                        profileDTO.setProjects(projects.values().stream().toList());
                        return profileDTO;
                        });
                    }).last();
    }
    public Mono<Resource> uploadAvatar(String userId,FilePart file){
        log.info("hello");
        Path basePath = Paths.get(path,  userId + "_avatar.jpg");
        log.info(basePath.toString());
        file.transferTo(basePath).log().subscribe();
        log.info("ok");
        try {
            log.info("here");
            Resource resource = new UrlResource(basePath.toUri());
            log.info(resource.getFilename());
            return Mono.just(resource);
        } catch (Exception e) {
            return Mono.empty();
        }
    }
    public Mono<Resource> getAvatar(String email){
        return template.selectOne(query(where("email").is(email)), User.class)
                .flatMap(u -> {
                    Path basePath = Paths.get(path, u.getId() + "_avatar.jpg");
                    try {
                        Resource resource = new UrlResource(basePath.toUri());
                        return Mono.just(resource);
                    } catch (Exception e) {
                        return Mono.empty();
                    }
                });
    }

    public Flux<SkillDTO> saveSkills(String userId, Flux<SkillDTO> skills) {
        template.delete(query(where("user_id").is(userId)), User2Skill.class).subscribe();
        return skills.map(s -> {
            template.insert(new User2Skill(userId,s.getId())).subscribe();
            return s;
        });
    }
}
