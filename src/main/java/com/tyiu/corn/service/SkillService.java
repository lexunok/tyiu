package com.tyiu.corn.service;

import com.tyiu.corn.config.exception.NotFoundException;
import com.tyiu.corn.model.dto.SkillDTO;
import com.tyiu.corn.model.entities.Skill;
import com.tyiu.corn.model.entities.User;
import com.tyiu.corn.model.entities.UserSkill;
import com.tyiu.corn.model.enums.SkillType;
import com.tyiu.corn.model.responses.TeamMemberResponse;

import lombok.RequiredArgsConstructor;

import org.modelmapper.ModelMapper;
import static org.springframework.data.relational.core.query.Query.query;
import static org.springframework.data.relational.core.query.Criteria.where;
import org.springframework.data.r2dbc.core.R2dbcEntityTemplate;
import static org.springframework.data.relational.core.query.Update.update;
import org.springframework.stereotype.Service;

import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;


@Service
@RequiredArgsConstructor
public class SkillService {

    private final R2dbcEntityTemplate template;
    private final ModelMapper mapper;

    public Flux<TeamMemberResponse> getAllUsersWithSkills(){
        return template.select(User.class).all().flatMap(u ->
                template.select(query(where("userEmail").is(u.getEmail())), UserSkill.class)
                        .collectList().flatMap(s -> Mono.just(TeamMemberResponse.builder()
                                .email(u.getEmail())
                                .firstName(u.getFirstName())
                                .lastName(u.getLastName())
                                .skills(s)
                                .build())));
    }

    public Flux<SkillDTO> getAllSkills() {
        return template.select(Skill.class).all()
                .flatMap(skill -> Flux.just(mapper.map(skill, SkillDTO.class)));
    }

    public Flux<SkillDTO> getAllConfirmedOrCreatorSkills(Long userId) {
        return template.select(query(where("creator_id").is(userId).or(where("confirmed").isTrue())), Skill.class)
                .flatMap(skill -> Flux.just(mapper.map(skill, SkillDTO.class)));
    }

    public Flux<SkillDTO> getSkillsByType(SkillType type) {
        return template.select(query(where("type").is(type)), Skill.class)
                .flatMap(skill -> Mono.just(mapper.map(skill, SkillDTO.class)));
    }

    public Mono<SkillDTO> addSkill(SkillDTO skillDTO, Long userId) {
            Skill skill = mapper.map(skillDTO, Skill.class);
            skill.setConfirmed(true);
            skill.setCreatorId(userId);
            return template.insert(skill).flatMap(s -> {
                skillDTO.setId(s.getId());
                skillDTO.setConfirmed(true);
                skillDTO.setCreatorId(userId);
                return Mono.just(skillDTO);
            });
    }

    public Mono<SkillDTO> addNoConfirmedSkill(SkillDTO skillDTO, Long userId) {
            Skill skill = mapper.map(skillDTO, Skill.class);
            skill.setConfirmed(false);
            skill.setCreatorId(userId);
            return template.insert(skill).flatMap(s -> {
                skillDTO.setId(s.getId());
                skillDTO.setConfirmed(false);
                skillDTO.setCreatorId(userId);
                return Mono.just(skillDTO);
            });
    }

    public Mono<Void> updateSkill(SkillDTO skillDTO, Long skillId, Long userId){
        return template.update(query(where("id").is(skillId)),
                        update("name", skillDTO.getName())
                                .set("updater_id", userId)
                                .set("type", skillDTO.getType()), Skill.class).then();
    }

    public Mono<Void> confirmSkill(Long skillId, Long userId) {
        return template.update(query(where("id").is(skillId)),
                update("confirmed", true)
                        .set("updater_id", userId), Skill.class).then();
    }

    public Mono<Void> deleteSkill(Long id) {
        return template.delete(query(where("id").is(id)), Skill.class).then()
                .onErrorResume(ex -> Mono.error(new NotFoundException("Not success!")));
    }
}