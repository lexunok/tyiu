package com.tyiu.corn.service;

import com.mongodb.client.gridfs.model.GridFSFile;
import com.tyiu.corn.model.dto.ProfileDTO;
import com.tyiu.corn.model.dto.SkillDTO;
import com.tyiu.corn.model.entities.Profile;
import com.tyiu.corn.model.entities.Skill;
import com.tyiu.corn.model.entities.UserSkill;
import com.tyiu.corn.model.requests.ProfileUpdateRequest;
import com.tyiu.corn.model.requests.UserSkillRequest;
import com.tyiu.corn.model.responses.UserSkillResponse;
import com.tyiu.corn.repository.ProfileRepository;
import lombok.RequiredArgsConstructor;
import org.modelmapper.ModelMapper;
import org.modelmapper.TypeToken;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.data.mongodb.core.ReactiveMongoTemplate;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.data.mongodb.gridfs.GridFsTemplate;
import org.springframework.data.mongodb.gridfs.ReactiveGridFsTemplate;
import org.springframework.http.codec.multipart.FilePart;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.util.List;


@Service
@RequiredArgsConstructor
public class ProfileService {
    private final ModelMapper mapper;
    private final ReactiveMongoTemplate template;
    private final ReactiveGridFsTemplate gridFsTemplate;

    public Mono<ProfileDTO> getProfile(String userEmail) {
        Query profileQuery = Query.query(Criteria.where("userEmail").is(userEmail));
        return template.findOne(profileQuery, Profile.class).flatMap(p -> {
            Query avatarQuery = Query.query(Criteria.where("_id").is(p.getAvatarId()));
            ProfileDTO profileDTO = mapper.map(p, ProfileDTO.class);
            gridFsTemplate.findOne(avatarQuery).doOnSuccess(profileDTO::setAvatar).subscribe();
            return Mono.just(profileDTO);
        });
    }

    public Mono<GridFSFile> uploadAvatar(String userEmail,FilePart file){
        Query profileQuery = Query.query(Criteria.where("userEmail").is(userEmail));
        return template.findOne(profileQuery, Profile.class).flatMap(p ->
            gridFsTemplate.store(file.content(), file.filename())
                    .doOnSuccess(image -> p.setAvatarId(image.toString()))
                    .flatMap(image ->
                        gridFsTemplate.findOne(Query.query(Criteria.where("_id").is(image)))
                    )
        );
    }

    public Flux<UserSkillResponse> getSkills(String userEmail){
        Query skillsQuery = Query.query(Criteria.where("userEmail").is(userEmail));
        return template.find(skillsQuery, UserSkill.class).flatMap(s ->
                Flux.just(UserSkillResponse.builder()
                        .level(s.getLevel())
                        .skillId(s.getSkillId())
                        .userEmail(s.getUserEmail())
                        .build())
        );
    }

    public Flux<UserSkillResponse> saveSkills(String userEmail, Flux<UserSkillRequest> skills){
        return skills.flatMap(s ->
            template.findOne(Query.query(Criteria.where("_id").is(s.getSkillId())), Skill.class)
                    .flatMap(skill -> template.save(
                            UserSkill.builder()
                                    .level(s.getLevel())
                                    .userEmail(userEmail)
                                    .name(skill.getName())
                                    .type(skill.getType())
                                    .skillId(s.getSkillId()).build()
                    ))
                    .flatMap(skill ->
                                Mono.just(UserSkillResponse.builder()
                                        .skillId(skill.getSkillId())
                                        .userEmail(skill.getUserEmail())
                                        .level(skill.getLevel())
                                        .name(skill.getName())
                                        .type(skill.getType()).build())
                    ).flux()
        );
    }

}
