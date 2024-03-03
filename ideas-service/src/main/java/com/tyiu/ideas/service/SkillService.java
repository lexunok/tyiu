package com.tyiu.ideas.service;

import com.tyiu.ideas.config.exception.NotFoundException;
import com.tyiu.ideas.model.dto.SkillDTO;
import com.tyiu.ideas.model.entities.Skill;
import com.tyiu.ideas.model.enums.SkillType;
import lombok.RequiredArgsConstructor;
import org.modelmapper.ModelMapper;
import org.springframework.cache.annotation.CacheConfig;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.data.r2dbc.core.R2dbcEntityTemplate;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.util.Collection;
import java.util.Map;

import static org.springframework.data.relational.core.query.Criteria.where;
import static org.springframework.data.relational.core.query.Query.query;
import static org.springframework.data.relational.core.query.Update.update;


@Service
@RequiredArgsConstructor
@CacheConfig(cacheNames = "skills")
public class SkillService {

    private final R2dbcEntityTemplate template;
    private final ModelMapper mapper;

    private Mono<SkillDTO> newSkill(SkillDTO skillDTO, String userId, Boolean bool){
        skillDTO.setConfirmed(bool);
        skillDTO.setCreatorId(userId);
        return template.insert(mapper.map(skillDTO, Skill.class))
                .flatMap(s -> {
                    skillDTO.setId(s.getId());
                    return Mono.just(skillDTO);
                });
    }

    ///////////////////////
    //  _____   ____ ______
    // / ___/  / __//_  __/
    /// (_ /  / _/   / /
    //\___/  /___/  /_/
    ///////////////////////

    @Cacheable
    public Flux<SkillDTO> getAllSkills() {
        return template.select(Skill.class).all()
                .flatMap(skill -> Flux.just(mapper.map(skill, SkillDTO.class)));
    }

    @Cacheable
    public Mono<Map<SkillType, Collection<SkillDTO>>> getAllConfirmedOrCreatorSkills(String userId) {
        return template.select(query(where("creator_id").is(userId).or(where("confirmed").isTrue())), Skill.class)
                .flatMap(skill -> Flux.just(mapper.map(skill, SkillDTO.class)))
                .collectMultimap(SkillDTO::getType);
    }

    @Cacheable
    public Flux<SkillDTO> getSkillsByType(SkillType type) {
        return template.select(query(where("type").is(type)), Skill.class)
                .flatMap(skill -> Mono.just(mapper.map(skill, SkillDTO.class)));
    }

    //////////////////////////////
    //   ___   ____    ____ ______
    //  / _ \ / __ \  / __//_  __/
    // / ___// /_/ / _\ \   / /
    ///_/    \____/ /___/  /_/
    //////////////////////////////

    @CacheEvict(allEntries = true)
    public Mono<SkillDTO> addSkill(SkillDTO skillDTO, String userId) {
        return newSkill(skillDTO, userId, true);
    }

    @CacheEvict(allEntries = true)
    public Mono<SkillDTO> addNoConfirmedSkill(SkillDTO skillDTO, String userId) {
        return newSkill(skillDTO, userId, false);
    }

    ///////////////////////////////////////////
    //   ___    ____   __    ____ ______   ____
    //  / _ \  / __/  / /   / __//_  __/  / __/
    // / // / / _/   / /__ / _/   / /    / _/
    ///____/ /___/  /____//___/  /_/    /___/
    ///////////////////////////////////////////

    @CacheEvict(allEntries = true)
    public Mono<Void> deleteSkill(String id) {
        return template.delete(query(where("id").is(id)), Skill.class)
                .then()
                .onErrorResume(ex -> Mono.error(new NotFoundException("Not success!")));
    }

    ////////////////////////
    //   ___   __  __ ______
    //  / _ \ / / / //_  __/
    // / ___// /_/ /  / /
    ///_/    \____/  /_/
    ////////////////////////

    @CacheEvict(allEntries = true)
    public Mono<Void> updateSkill(SkillDTO skillDTO, String skillId, String userId){
        return template.update(query(where("id").is(skillId)),
                        update("name", skillDTO.getName())
                                .set("updater_id", userId)
                                .set("type", skillDTO.getType()), Skill.class)
                .then();
    }

    @CacheEvict(allEntries = true)
    public Mono<Void> confirmSkill(String skillId, String userId) {
        return template.update(query(where("id").is(skillId)),
                update("confirmed", true)
                        .set("updater_id", userId), Skill.class)
                .then();
    }
}