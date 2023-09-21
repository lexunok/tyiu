package com.tyiu.corn.controller;

import com.tyiu.corn.model.dto.SkillDTO;
import com.tyiu.corn.model.entities.Skill;
import com.tyiu.corn.model.enums.SkillType;
import com.tyiu.corn.model.responses.InfoResponse;
import com.tyiu.corn.service.SkillService;

import lombok.RequiredArgsConstructor;

import org.springframework.web.bind.annotation.*;

import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

@RestController
@RequestMapping("/api/v1/skill")
@RequiredArgsConstructor
public class SkillController {
    private final SkillService skillService;

    @GetMapping("/all")
    public Flux<Skill> getAllSkills() {
        return skillService.getAllSkills();
    }

    @GetMapping("/{skillType}")
    public Flux<Skill> getSkillsByType(@PathVariable SkillType skillType) {
        return skillService.getSkillsByType(skillType);
    }

    @PostMapping("/add")
    public Mono<Skill> addSkill(@RequestBody SkillDTO skill) {
        return skillService.addSkill(skill);
    }

    @PostMapping("/add/no-confirmed")
    public Mono<Skill> addNoConfirmedSkill(@RequestBody SkillDTO skill) {
        return skillService.addNoConfirmedSkill(skill);
    }

    @PutMapping("/confirm/{skillId}")
    public Mono<Skill> confirmSkill(@PathVariable String skillId) {
        return skillService.confirmSkill(skillId);
    }

    @DeleteMapping("/delete/{skillId}")
    public Mono<InfoResponse> deleteSkill(@PathVariable String skillId) {
        return skillService.deleteSkill(skillId);
    }
}
