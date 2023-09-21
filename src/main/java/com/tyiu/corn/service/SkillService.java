package com.tyiu.corn.service;

import com.tyiu.corn.model.dto.SkillDTO;
import com.tyiu.corn.model.entities.Skill;
import com.tyiu.corn.model.enums.SkillType;
import com.tyiu.corn.model.responses.InfoResponse;
import com.tyiu.corn.repository.SkillRepository;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import org.springframework.stereotype.Service;

import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.time.Instant;

@Service
@RequiredArgsConstructor
@Slf4j
public class SkillService {
    private final SkillRepository skillRepository;

    public Flux<Skill> getAllSkills() {
        return skillRepository.findAll();
    }

    public Flux<Skill> getSkillsByType(SkillType skillType) {
        return skillRepository.findByType(skillType);
    }

    public Mono<Skill> addSkill(SkillDTO skillDTO) {
        Skill skill = Skill.builder()
                .name(skillDTO.getName())
                .type(skillDTO.getType())
                .confirmed(true)
                .createdAt(Instant.now())
                .build();
        return skillRepository.save(skill);
    }

    public Mono<Skill> addNoConfirmedSkill(SkillDTO skillDTO) {
        Skill skill = Skill.builder()
                .name(skillDTO.getName())
                .type(skillDTO.getType())
                .confirmed(false)
                .createdAt(Instant.now())
                .build();
        return skillRepository.save(skill);
    }

    public Mono<Skill> confirmSkill(String skillId) {
        Mono<Skill> skill = skillRepository.findById(skillId);
        return skill.flatMap(s -> {
            s.setConfirmed(true);
            return skillRepository.save(s);
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
