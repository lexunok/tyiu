package com.tyiu.ideas.service;

import com.tyiu.ideas.config.exception.NotFoundException;
import com.tyiu.ideas.model.dto.SkillDTO;
import com.tyiu.ideas.model.entities.Skill;
import com.tyiu.ideas.model.enums.SkillType;

import lombok.RequiredArgsConstructor;

import org.modelmapper.ModelMapper;
import static org.springframework.data.relational.core.query.Query.query;
import static org.springframework.data.relational.core.query.Criteria.where;
import org.springframework.data.r2dbc.core.R2dbcEntityTemplate;
import static org.springframework.data.relational.core.query.Update.update;
import org.springframework.stereotype.Service;

import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.util.Collection;
import java.util.Map;


@Service
@RequiredArgsConstructor
public class SkillService {

    private final R2dbcEntityTemplate template;
    private final ModelMapper mapper;


    public Flux<SkillDTO> getAllSkills() {
        return template.select(Skill.class).all()
                .flatMap(skill -> Flux.just(mapper.map(skill, SkillDTO.class)));
    }

    public Mono<Map<SkillType, Collection<SkillDTO>>> getAllConfirmedOrCreatorSkills(String userId) {
        return template.select(query(where("creator_id").is(userId).or(where("confirmed").isTrue())), Skill.class)
                .flatMap(skill -> Flux.just(mapper.map(skill, SkillDTO.class)))
                .collectMultimap(SkillDTO::getType);
    }

    public Flux<SkillDTO> getSkillsByType(SkillType type) {
        return template.select(query(where("type").is(type)), Skill.class)
                .flatMap(skill -> Mono.just(mapper.map(skill, SkillDTO.class)));
    }

    public Mono<SkillDTO> addSkill(SkillDTO skillDTO, String userId) {
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

    public Mono<SkillDTO> addNoConfirmedSkill(SkillDTO skillDTO, String userId) {
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

    public Mono<Void> updateSkill(SkillDTO skillDTO, String skillId, String userId){
        return template.update(query(where("id").is(skillId)),
                        update("name", skillDTO.getName())
                                .set("updater_id", userId)
                                .set("type", skillDTO.getType()), Skill.class).then();
    }

    public Mono<Void> confirmSkill(String skillId, String userId) {
        return template.update(query(where("id").is(skillId)),
                update("confirmed", true)
                        .set("updater_id", userId), Skill.class).then();
    }

    public Mono<Void> deleteSkill(String id) {
        return template.delete(query(where("id").is(id)), Skill.class).then()
                .onErrorResume(ex -> Mono.error(new NotFoundException("Not success!")));
    }
}