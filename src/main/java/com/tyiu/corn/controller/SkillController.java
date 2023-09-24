package com.tyiu.corn.controller;

import com.tyiu.corn.model.dto.SkillDTO;
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
    public Flux<SkillDTO> getAllSkills() {
        return skillService.getAllSkills();
    }

    @GetMapping("/{skillType}")
    public Flux<SkillDTO> getSkillsByType(@PathVariable SkillType skillType) {
        return skillService.getSkillsByType(skillType);
    }

    @PostMapping("/add")
    public Mono<SkillDTO> addSkill(@RequestBody SkillDTO skill) {
        return skillService.addSkill(skill);
    }

    @PostMapping("/add/no-confirmed")
    public Mono<SkillDTO> addNoConfirmedSkill(@RequestBody SkillDTO skill) {
        return skillService.addNoConfirmedSkill(skill);
    }

    @PutMapping("/update/{skillId}")
    public Mono<SkillDTO> updateSkill(@RequestBody SkillDTO skillDTO, @PathVariable String skillId) {
        return skillService.updateSkill(skillDTO, skillId);
    }

    @PutMapping("/confirm/{skillId}")
    public Mono<SkillDTO> confirmSkill(@PathVariable String skillId) {
        return skillService.confirmSkill(skillId);
    }

    @DeleteMapping("/delete/{skillId}")
    public Mono<InfoResponse> deleteSkill(@PathVariable String skillId) {
        return skillService.deleteSkill(skillId);
    }
}
