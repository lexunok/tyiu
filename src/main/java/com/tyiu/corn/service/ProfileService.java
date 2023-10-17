package com.tyiu.corn.service;

import com.tyiu.corn.model.entities.*;
import com.tyiu.corn.model.requests.UserSkillRequest;
import com.tyiu.corn.model.responses.UserIdeaResponse;
import com.tyiu.corn.model.responses.UserProjectResponse;
import com.tyiu.corn.model.responses.UserSkillResponse;
import lombok.RequiredArgsConstructor;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.core.io.Resource;
import org.springframework.core.io.ResourceLoader;
import org.springframework.core.io.UrlResource;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.data.mongodb.core.ReactiveMongoTemplate;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.data.mongodb.gridfs.ReactiveGridFsResource;
import org.springframework.data.mongodb.gridfs.ReactiveGridFsTemplate;
import org.springframework.http.codec.multipart.FilePart;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.nio.file.Path;
import java.nio.file.Paths;


@Service
@RequiredArgsConstructor
public class ProfileService {

    private final ModelMapper mapper;
    private final ReactiveMongoTemplate template;
    private final ResourceLoader loader;
    @Value("${file.path.avatar}")
    String path;

    public Mono<Resource> uploadAvatar(String userEmail,FilePart file){
        Path basePath = Paths.get(path, userEmail + ".jpg");
        file.transferTo(basePath).subscribe();
        try {
            Resource resource = new UrlResource(basePath.toUri());
            return Mono.just(resource);
        } catch (Exception e) {
            return Mono.empty();
        }
    }
    public Mono<Resource> getAvatar(String userEmail){
        Path basePath = Paths.get(path, userEmail + ".jpg");
        try {
            Resource resource = new UrlResource(basePath.toUri());
            return Mono.just(resource);
        } catch (Exception e) {
            return Mono.empty();
        }
    }


    @Cacheable(cacheNames = "ideas")
    public Flux<UserIdeaResponse> getUserIdeas(String userEmail) {
        Query profileQuery = Query.query(Criteria.where("userEmail").is(userEmail));
        return template.findOne(profileQuery,Profile.class).flatMapMany(p ->
            template.find(Query.query(Criteria.where("_id").in(p.getUserIdeasId())), Idea.class)
                    .flatMap(i ->
                        Flux.just(UserIdeaResponse.builder()
                                //.ideaId(i.getId())
                                .description(i.getDescription())
                                .name(i.getName()).build())
            )
        );
    }
    @Cacheable(cacheNames = "projects")
    public Flux<UserProjectResponse> getUserProjects(String userEmail) {
        Query profileQuery = Query.query(Criteria.where("userEmail").is(userEmail));
        return template.findOne(profileQuery,Profile.class).flatMapMany(p ->
                template.find(Query.query(Criteria.where("_id").in(p.getUserProjectsId())), Project.class)
                        .flatMap(pr ->
                                Flux.just(UserProjectResponse.builder()
                                        .projectId(pr.getId())
                                        .description(pr.getDescription())
                                        .name(pr.getName()).build())
                        )
        );
    }

    @Cacheable(cacheNames = "skills")
    public Flux<UserSkillResponse> getSkills(String userEmail){
        Query skillsQuery = Query.query(Criteria.where("userEmail").is(userEmail));
        return template.find(skillsQuery, UserSkill.class).flatMap(s ->
                Flux.just(mapper.map(s, UserSkillResponse.class))
        );
    }
    @CacheEvict(cacheNames = "skills",allEntries = true)
    public Flux<UserSkillResponse> saveSkills(String userEmail, Flux<UserSkillRequest> skills){
        return skills.flatMap(s ->
            template.findOne(Query.query(Criteria.where("id").is(s.getSkillId())), Skill.class)
                    .flatMapMany(skill ->
                        template.save(
                                UserSkill.builder()
                                        .level(s.getLevel())
                                        .userEmail(userEmail)
                                        .name(skill.getName())
                                        .type(skill.getType())
                                        .skillId(s.getSkillId()).build()
                                ).flatMapMany(us -> Flux.just(mapper.map(us, UserSkillResponse.class)))
                    )
        );
    }
}
