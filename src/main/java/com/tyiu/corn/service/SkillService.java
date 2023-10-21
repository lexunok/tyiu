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
import org.springframework.stereotype.Service;

import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.util.List;

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
                .flatMap(skill -> Flux.just(mapper.map(skill, SkillDTO.class)))
                .switchIfEmpty(Mono.error(new NotFoundException("Failed to get a list skills")));
    }

    public Mono<List<Skill>> getAllConfirmedOrCreatorSkills(Long userId) {
        return template.selectOne(query(where("id").is(userId)), User.class)
                .flatMap(user -> {
                    return template.select(query(where("creator_id").is(user.getId()).or(where("confirmed").isTrue())), Skill.class)
                            .collectList();
                });
    }

    public Flux<SkillDTO> getSkillsByType(SkillType type) {
        return template.select(query(where("type").is(type)), Skill.class)
                .flatMap(skill -> Mono.just(mapper.map(skill, SkillDTO.class)))
                .switchIfEmpty(Mono.error(new NotFoundException("Failed to get a list skills")));
    }

    public Mono<SkillDTO> addSkill(SkillDTO skillDTO, Long userId) {
        return template.selectOne(query(where("id").is(userId)), User.class).flatMap(user -> {
            Skill skill = mapper.map(skillDTO, Skill.class);
            return template.insert(skill).flatMap(s -> {
                skillDTO.setId(s.getId());
                skillDTO.setConfirmed(true);
                skillDTO.setCreatorId(user.getId());
                return Mono.just(skillDTO);
            }).switchIfEmpty(Mono.error(new NotFoundException("Failed to create a skill")));
        });
    }

    public Mono<SkillDTO> addNoConfirmedSkill(SkillDTO skillDTO, Long userId) {
        return template.selectOne(query(where("id").is(userId)), User.class).flatMap(user -> {
            Skill skill = mapper.map(skillDTO, Skill.class);
            return template.insert(skill).flatMap(s -> {
                skillDTO.setId(s.getId());
                skillDTO.setConfirmed(false);
                skillDTO.setCreatorId(user.getId());
                return Mono.just(skillDTO);
            }).switchIfEmpty(Mono.error(new NotFoundException("Failed to create a skill")));
        });
    }

    public Mono<SkillDTO> updateSkill(SkillDTO skillDTO, Long skillId, Long userId) {
        return template.selectOne(query(where("id").is(skillId)), Skill.class).flatMap(skill -> {
            return template.selectOne(query(where("id").is(userId)), User.class).flatMap(user -> {
                skill.setName(skillDTO.getName());
                skill.setType(skillDTO.getType());
                skill.setUpdaterId(user.getId());
                return template.insert(skill).flatMap(s -> {
                    s.setId(skill.getId());
                    return Mono.just(skillDTO);
                });
            });
        });
    }

    public Mono<SkillDTO> confirmSkill(Long skillId, Long userId) {
        return template.selectOne(query(where("id").is(skillId)), Skill.class).flatMap(skill -> {
            return template.selectOne(query(where("email").is(userId)), User.class).flatMap(user -> {
                skill.setConfirmed(true);
                skill.setUpdaterId(user.getId());
                return template.insert(skill).flatMap(savedSkill ->
                        Mono.just(mapper.map(savedSkill, SkillDTO.class)));
            });
        });
    }

    public Mono<Void> deleteSkill(Long id) {
        return template.delete(query(where("id").is(id)), Skill.class).then()
                .onErrorResume(ex -> Mono.error(new NotFoundException("Not success!")));
    }
}