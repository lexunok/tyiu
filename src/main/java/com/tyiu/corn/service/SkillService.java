package com.tyiu.corn.service;

import com.tyiu.corn.model.dto.SkillDTO;
import com.tyiu.corn.model.entities.Skill;
import com.tyiu.corn.model.enums.SkillType;
import com.tyiu.corn.model.responses.InfoResponse;
import com.tyiu.corn.repository.SkillRepository;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import org.modelmapper.ModelMapper;
import org.springframework.stereotype.Service;

import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.time.Instant;

@Service
@RequiredArgsConstructor
@Slf4j
public class SkillService {
    private final SkillRepository skillRepository;
    private final ModelMapper mapper;

    public Flux<SkillDTO> getAllSkills() {
        return skillRepository.findAll().flatMap(skill ->
                Flux.just(mapper.map(skill, SkillDTO.class))
        );
    }

    public Flux<SkillDTO> getSkillsByType(SkillType skillType) {
        return skillRepository.findByType(skillType).flatMap(skill ->
                Mono.just(mapper.map(skill, SkillDTO.class))
        );
    }

    public Mono<SkillDTO> addSkill(SkillDTO skillDTO) {
        Skill skill = Skill.builder()
                .name(skillDTO.getName())
                .type(skillDTO.getType())
                .confirmed(true)
                .createdAt(Instant.now())
                .build();
        return skillRepository.save(skill).flatMap(savedSkill ->
                Mono.just(mapper.map(savedSkill, SkillDTO.class))
        );
    }

    public Mono<SkillDTO> addNoConfirmedSkill(SkillDTO skillDTO) {
        Skill skill = Skill.builder()
                .name(skillDTO.getName())
                .type(skillDTO.getType())
                .confirmed(false)
                .createdAt(Instant.now())
                .build();
        return skillRepository.save(skill).flatMap(savedSkill ->
                Mono.just(mapper.map(savedSkill, SkillDTO.class))
        );
    }

    public Mono<SkillDTO> updateSkill(SkillDTO skillDTO, String skillId) {
        return skillRepository.findById(skillId).flatMap(skill -> {
            skill.setName(skillDTO.getName());
            skill.setType(skillDTO.getType());
            return skillRepository.save(skill).flatMap(updatedSkill ->
                Mono.just(mapper.map(updatedSkill, SkillDTO.class))
            );
        });
    }

    public Mono<SkillDTO> confirmSkill(String skillId) {
        return skillRepository.findById(skillId).flatMap(skill -> {
            skill.setConfirmed(true);
            return skillRepository.save(skill).flatMap(savedSkill ->
                    Mono.just(mapper.map(savedSkill, SkillDTO.class))
            );
        });
    }

    public Mono<InfoResponse> deleteSkill(String skillId) {
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
