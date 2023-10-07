package com.tyiu.corn.service;

import com.tyiu.corn.model.dto.SkillDTO;
import com.tyiu.corn.model.entities.Skill;
import com.tyiu.corn.model.entities.User;
import com.tyiu.corn.model.entities.UserSkill;
import com.tyiu.corn.model.enums.SkillType;
import com.tyiu.corn.model.responses.InfoResponse;
import com.tyiu.corn.model.responses.TeamMemberResponse;
import com.tyiu.corn.repository.SkillRepository;
import com.tyiu.corn.repository.UserRepository;

import lombok.RequiredArgsConstructor;

import org.modelmapper.ModelMapper;
import org.springframework.data.mongodb.core.ReactiveMongoTemplate;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.stereotype.Service;

import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.time.Instant;
import java.util.Collection;
import java.util.Map;

@Service
@RequiredArgsConstructor
public class SkillService {
    private final SkillRepository skillRepository;
    private final UserRepository userRepository;
    private final ReactiveMongoTemplate mongoTemplate;
    private final ModelMapper mapper;

    public Flux<TeamMemberResponse> getAllUsersWithSkills(){
        return mongoTemplate.findAll(User.class).flatMap(u ->
                mongoTemplate.find(Query.query(Criteria.where("userEmail").is(u.getEmail())), UserSkill.class)
                .collectList().flatMap(s -> Mono.just(TeamMemberResponse.builder()
                        .email(u.getEmail())
                        .firstName(u.getFirstName())
                        .lastName(u.getLastName())
                        .skills(s)
                        .build())));
    }

    public Flux<SkillDTO> getAllSkills() {
        return skillRepository.findAll().flatMap(skill ->
                Flux.just(mapper.map(skill, SkillDTO.class))
        );
    }

    public Mono<Map<SkillType, Collection<SkillDTO>>> getAllConfirmedOrCreatorSkills(String email) {
        return userRepository.findFirstByEmail(email).flatMap(user -> {
            return skillRepository.findByConfirmedOrCreatorId(true, user.getId())
                .flatMap(skill -> Flux.just(mapper.map(skill, SkillDTO.class)))
                .collectMultimap( SkillDTO::getType );
        });
    }

    public Flux<SkillDTO> getSkillsByType(SkillType skillType) {
        return skillRepository.findByType(skillType).flatMap(skill ->
                Mono.just(mapper.map(skill, SkillDTO.class))
        );
    }

    public Mono<SkillDTO> addSkill(SkillDTO skillDTO, String email) {
        return userRepository.findFirstByEmail(email).flatMap(user -> {
            Skill skill = Skill.builder()
                .name(skillDTO.getName())
                .type(skillDTO.getType())
                .confirmed(true)
                .createdAt(Instant.now())
                .creatorId(user.getId())
                .build();
            return skillRepository.save(skill).flatMap(savedSkill ->
                Mono.just(mapper.map(savedSkill, SkillDTO.class))
        );
        });
    }

    public Mono<SkillDTO> addNoConfirmedSkill(SkillDTO skillDTO, String email) {
        return userRepository.findFirstByEmail(email).flatMap(user -> {
            Skill skill = Skill.builder()
                .name(skillDTO.getName())
                .type(skillDTO.getType())
                .confirmed(false)
                .createdAt(Instant.now())
                .creatorId(user.getId())
                .build();
            return skillRepository.save(skill).flatMap(savedSkill ->
                Mono.just(mapper.map(savedSkill, SkillDTO.class))
        );
        });
    }

    public Mono<SkillDTO> updateSkill(SkillDTO skillDTO, String skillId, String email) {
        return skillRepository.findById(skillId).flatMap(skill -> {
            return userRepository.findFirstByEmail(email).flatMap(user -> {
                skill.setName(skillDTO.getName());
                skill.setType(skillDTO.getType());
                skill.setUpdaterId(user.getId());
                return skillRepository.save(skill).flatMap(updatedSkill ->
                    Mono.just(mapper.map(updatedSkill, SkillDTO.class))
                );
            });
        });
    }

    public Mono<SkillDTO> confirmSkill(String skillId, String email) {
        return skillRepository.findById(skillId).flatMap(skill -> {
            return userRepository.findFirstByEmail(email).flatMap(user -> {
                skill.setConfirmed(true);
                skill.setUpdaterId(user.getId());
                return skillRepository.save(skill).flatMap(savedSkill ->
                        Mono.just(mapper.map(savedSkill, SkillDTO.class))
            );
            });
        });
    }

    public Mono<InfoResponse> deleteSkill(String skillId, String email) {
        return skillRepository.existsById(skillId).flatMap(skill -> {
            if (skill) {
                skillRepository.deleteById(skillId).subscribe();
                return Mono.just(new InfoResponse(200, "Успешное удаление компетенции"));
            } else {
                return Mono.just(new InfoResponse(405, "Компетенция не найдена"));
            }
        });
    }
}
