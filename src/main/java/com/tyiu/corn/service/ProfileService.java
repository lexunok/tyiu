package com.tyiu.corn.service;

import com.tyiu.corn.model.dto.SkillDTO;
import com.tyiu.corn.model.dto.UserDTO;
import com.tyiu.corn.model.entities.*;
import com.tyiu.corn.model.entities.relations.User2Skill;
import com.tyiu.corn.model.enums.SkillType;
import com.tyiu.corn.model.responses.ProfileIdeaResponse;
import com.tyiu.corn.model.responses.ProfileProjectResponse;
import com.tyiu.corn.model.responses.ProfileSkillResponse;
import lombok.RequiredArgsConstructor;
import org.modelmapper.ModelMapper;
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
    private final ModelMapper mapper;
    @Value("${file.path.avatar}")
    String path;

    public Mono<UserDTO> getUserProfile(String email){
        return template.selectOne(query(where("email").is(email)),User.class)
                .map(u -> mapper.map(u, UserDTO.class));
    }

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
    public Mono<Resource> getAvatar(String email){
        return template.selectOne(query(where("email").is(email)), User.class)
                .flatMap(u -> {
                    Path basePath = Paths.get(path, u.getId() + ".jpg");
                    try {
                        Resource resource = new UrlResource(basePath.toUri());
                        return Mono.just(resource);
                    } catch (Exception e) {
                        return Mono.empty();
                    }
                });
    }

    public Flux<ProfileIdeaResponse> getUserIdeas(String email) {
        return template.selectOne(query(where("email").is(email)),User.class).flux()
                .flatMap(u ->
                    template.select(query(where("initiator_id").is(u.getId())), Idea.class)
                            .flatMap(i -> Flux.just(ProfileIdeaResponse.builder()
                                    .id(i.getId())
                                    .description(i.getDescription())
                                    .name(i.getName())
                                    .build()))
                );
    }

    public Flux<ProfileProjectResponse> getUserProjects(String email) {
        String query = "SELECT t.id t_id, p.id p_id, p.name p_name, p.description p_desc FROM team t " +
                "JOIN project p ON p.team_id = t.id WHERE t.owner_id =:userId";
        return template.selectOne(query(where("email").is(email)), User.class).flux()
                .flatMap(u ->
                    template.getDatabaseClient().sql(query)
                            .bind("userId", u.getId())
                            .map((row, rowMetadata) -> ProfileProjectResponse.builder()
                                    .id(row.get("p_id",Long.class))
                                    .name(row.get("p_name",String.class))
                                    .description(row.get("p_desc",String.class)).build())
                            .all()
                );
    }

    public Flux<ProfileSkillResponse> getSkills(String email){
        String query = "SELECT s.id s_id, s.name s_name, s.type s_type, user_skill.skill_id FROM user_skill" +
                " JOIN skill s ON s.id = user_skill.skill_id WHERE user_skill.user_id =:userId";
        return template.selectOne(query(where("email").is(email)), User.class).flux()
                .flatMap(u ->
                    template.getDatabaseClient().sql(query)
                            .bind("userId",u.getId())
                            .map((row, rowMetadata) -> ProfileSkillResponse.builder()
                                    .id(row.get("s_id",Long.class))
                                    .name(row.get("s_name", String.class))
                                    .type(SkillType.valueOf(row.get("s_type", String.class))).build())
                            .all()
                );
    }

    public Flux<SkillDTO> saveSkills(Long userId, Flux<SkillDTO> skills) {
        template.delete(query(where("user_id").is(userId)), User2Skill.class).subscribe();
        return skills.map(s -> {
            template.insert(new User2Skill(userId,s.getId())).subscribe();
            return s;
        });
    }
}
