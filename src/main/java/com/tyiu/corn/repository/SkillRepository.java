package com.tyiu.corn.repository;

import com.tyiu.corn.model.entities.Skill;

import com.tyiu.corn.model.enums.SkillType;


import org.springframework.data.repository.reactive.ReactiveCrudRepository;
import org.springframework.stereotype.Repository;

import reactor.core.publisher.Flux;

@Repository
public interface SkillRepository extends ReactiveCrudRepository<Skill, Long> {
    Flux<Skill> findByType(SkillType type);
    Flux<Skill> findByConfirmedOrCreatorId(Boolean confirmed, Long creatorId);

}
