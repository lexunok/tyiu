package com.tyiu.corn.service;

import com.mongodb.client.gridfs.model.GridFSFile;
import com.tyiu.corn.model.entities.*;
import com.tyiu.corn.model.requests.UserSkillRequest;
import com.tyiu.corn.model.responses.UserIdeaResponse;
import com.tyiu.corn.model.responses.UserSkillResponse;
import lombok.RequiredArgsConstructor;
import org.modelmapper.ModelMapper;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.core.io.buffer.DataBufferUtils;
import org.springframework.data.mongodb.core.ReactiveMongoTemplate;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.data.mongodb.gridfs.GridFsResource;
import org.springframework.data.mongodb.gridfs.ReactiveGridFsResource;
import org.springframework.data.mongodb.gridfs.ReactiveGridFsTemplate;
import org.springframework.http.codec.multipart.FilePart;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;


@Service
@RequiredArgsConstructor
public class ProfileService {

    private final ModelMapper mapper;
    private final ReactiveMongoTemplate template;
    private final ReactiveGridFsTemplate gridFsTemplate;

    public Flux<UserIdeaResponse> getUserIdeas(String userEmail) {
        Query profileQuery = Query.query(Criteria.where("userEmail").is(userEmail));
        return template.findOne(profileQuery,Profile.class).flatMapMany(p ->
            template.find(Query.query(Criteria.where("_id").in(p.getUserIdeasId())), Idea.class)
                    .flatMap(i ->
                        Flux.just(UserIdeaResponse.builder()
                                .ideaId(i.getId())
                                .description(i.getDescription())
                                .name(i.getName()).build())
            )
        );
    }

    public Flux<DataBuffer> getAvatar(String userEmail) {
        Query profileQuery = Query.query(Criteria.where("userEmail").is(userEmail));
        return template.findOne(profileQuery, Profile.class).flatMap(p ->
            gridFsTemplate.findOne(Query.query(Criteria.where("_id").is(p.getAvatarId())))
                    .flatMap(gridFsTemplate::getResource)
        ).flatMapMany(ReactiveGridFsResource::getDownloadStream);
    }

    public Flux<DataBuffer> uploadAvatar(String userEmail,FilePart file){
        Query profileQuery = Query.query(Criteria.where("userEmail").is(userEmail));
        return template.findOne(profileQuery, Profile.class).flatMap(p ->
            gridFsTemplate.store(file.content(), file.filename())
                    .doOnSuccess(image -> p.setAvatarId(image.toString()))
                    .flatMap(image ->
                        gridFsTemplate.findOne(Query.query(Criteria.where("_id").is(image)))
                                .flatMap(gridFsTemplate::getResource)
                    )
        ).flatMapMany(ReactiveGridFsResource::getDownloadStream);
    }

    public Flux<UserSkillResponse> getSkills(String userEmail){
        Query skillsQuery = Query.query(Criteria.where("userEmail").is(userEmail));
        return template.find(skillsQuery, UserSkill.class).flatMap(s ->
                Flux.just(mapper.map(s, UserSkillResponse.class))
        );
    }

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
