package com.tyiu.corn.service;

import com.tyiu.corn.model.entities.*;
import com.tyiu.corn.model.entities.relations.User2Skill;
import com.tyiu.corn.model.enums.SkillType;
import com.tyiu.corn.model.requests.ProfileSkillRequest;
import com.tyiu.corn.model.responses.ProfileIdeaResponse;
import com.tyiu.corn.model.responses.ProfileProjectResponse;
import com.tyiu.corn.model.responses.ProfileSkillResponse;
import lombok.RequiredArgsConstructor;
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

import static org.springframework.data.relational.core.query.Criteria.where;
import static org.springframework.data.relational.core.query.Query.query;


@Service
@RequiredArgsConstructor
public class ProfileService {


    private final R2dbcEntityTemplate template;
    @Value("${file.path.avatar}")
    String path;

    public Mono<Resource> uploadAvatar(String userId,FilePart file){
        Path basePath = Paths.get(path, userId + ".jpg");
        file.transferTo(basePath).subscribe();
        try {
            Resource resource = new UrlResource(basePath.toUri());
            return Mono.just(resource);
        } catch (Exception e) {
            return Mono.empty();
        }
    }
    public Mono<Resource> getAvatar(String userId){
        Path basePath = Paths.get(path, userId + ".jpg");
        try {
            Resource resource = new UrlResource(basePath.toUri());
            return Mono.just(resource);
        } catch (Exception e) {
            return Mono.empty();
        }
    }

    /*public Mono<ProfileDTO> getProfile(Long userId){
        String query = "SELECT u.id u_id, u.email u_email, i.id i_id, i.description i_description, i.name i_name FROM users u JOIN idea i ON i.initiator_id = u.id WHERE u.id =: userId";
        return template.getDatabaseClient().sql(query)
                .bind("userId",userId)
                .map()
    }*/


    public Flux<ProfileIdeaResponse> getUserIdeas(Long userId) {
        return template.select(query(where("initiator_id").is(userId)), Idea.class)
                .flatMap(i -> Flux.just(ProfileIdeaResponse.builder()
                        .id(i.getId())
                        .description(i.getDescription())
                        .name(i.getName())
                        .build()));
    }

    public Flux<ProfileProjectResponse> getUserProjects(Long userId) {
        String query = "SELECT t.id t_id, p.id p_id, p.name p_name, p.description p_desc FROM team t " +
                "JOIN project p ON p.team_id = t.id WHERE t.owner_id =:userId";
        return template.getDatabaseClient().sql(query)
                .bind("userId", userId)
                .map((row, rowMetadata) -> ProfileProjectResponse.builder()
                        .id(row.get("p_id",Long.class))
                        .name(row.get("p_name",String.class))
                        .description(row.get("p_desc",String.class)).build())
                .all();

    }

    public Flux<ProfileSkillResponse> getSkills(Long userId){
        String query = "SELECT s.id s_id, s.name s_name, s.type s_type, user_skill.skill_id sk FROM user_skill " +
                "JOIN skill s ON skill.id = sk WHERE user_skill.user_id =:userId";
        return template.getDatabaseClient().sql(query)
                .bind("userId",userId)
                .map((row, rowMetadata) -> ProfileSkillResponse.builder()
                        .id(row.get("s_id",Long.class))
                        .name(row.get("s_name", String.class))
                        .type(SkillType.valueOf(row.get("s_type", String.class))).build())
                .all();
    }

    public Flux<ProfileSkillResponse> saveSkills(Long userId, Flux<ProfileSkillRequest> skills) {
        return skills.flatMap(s -> {
            template.insert(new User2Skill(userId, s.getSkillId()));
            String query = "SELECT s.id s_id, s.name s_name, s.type s_type, user_skill.skill_id sk FROM user_skill " +
                    "JOIN skill s ON skill.id = sk WHERE user_skill.user_id =:userId";
            return template.getDatabaseClient().sql(query)
                    .bind("userId",userId)
                    .map((row, rowMetadata) -> ProfileSkillResponse.builder()
                            .id(row.get("s_id",Long.class))
                            .name(row.get("s_name", String.class))
                            .type(SkillType.valueOf(row.get("s_type", String.class))).build())
                    .all();
        });
    }
}
