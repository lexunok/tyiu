package com.tyiu.corn.controller;

import com.tyiu.corn.model.dto.SkillDTO;
import com.tyiu.corn.model.enums.SkillType;
import com.tyiu.corn.model.responses.InfoResponse;
import com.tyiu.corn.service.SkillService;

import lombok.RequiredArgsConstructor;

import java.security.Principal;
import java.util.Collection;
import java.util.Map;

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

    @GetMapping("/all-confirmed-or-creator")
    public Mono<Map<SkillType, Collection<SkillDTO>>> getAllConfirmedSkills(Principal principal) {
        return skillService.getAllConfirmedOrCreatorSkills(principal.getName());
    }

    @GetMapping("/{skillType}")
    public Flux<SkillDTO> getSkillsByType(@PathVariable SkillType skillType) {
        return skillService.getSkillsByType(skillType);
    }

    @PostMapping("/add")
    public Mono<SkillDTO> addSkill(@RequestBody SkillDTO skill, Principal principal) {
        return skillService.addSkill(skill, principal.getName());
    }

    @PostMapping("/add/no-confirmed")
    public Mono<SkillDTO> addNoConfirmedSkill(@RequestBody SkillDTO skill, Principal principal) {
        return skillService.addNoConfirmedSkill(skill, principal.getName());
    }

    @PutMapping("/update/{skillId}")
    public Mono<SkillDTO> updateSkill(@RequestParam SkillDTO skillDTO, @PathVariable String skillId, Principal principal) {
        return skillService.updateSkill(skillDTO, skillId, principal.getName());
    }

    @PutMapping("/confirm/{skillId}")
    public Mono<SkillDTO> confirmSkill(@PathVariable String skillId, Principal principal) {
        return skillService.confirmSkill(skillId, principal.getName());
    }
    
    @DeleteMapping("/delete/{skillId}")
    public Mono<InfoResponse> deleteSkill(@PathVariable String skillId, Principal principal) {
        return skillService.deleteSkill(skillId, principal.getName());
    }
}
