package com.tyiu.ideas.controller;

import com.tyiu.client.exceptions.NotFoundException;
import com.tyiu.ideas.model.dto.SkillDTO;
import com.tyiu.ideas.model.enums.SkillType;
import com.tyiu.ideas.model.responses.InfoResponse;
import com.tyiu.ideas.service.SkillService;

import lombok.RequiredArgsConstructor;

import java.util.Collection;
import java.util.Map;

import org.springframework.http.HttpStatus;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.security.oauth2.jwt.Jwt;
import org.springframework.web.bind.annotation.*;

import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

@RestController
@RequestMapping("/api/v1/ideas-service/skill")
@RequiredArgsConstructor
public class SkillController {
    private final SkillService skillService;

    @GetMapping("/all")
    public Flux<SkillDTO> getAllSkills() {
        return skillService.getAllSkills();
    }

    @GetMapping("/all-confirmed-or-creator")
    public Mono<Map<SkillType, Collection<SkillDTO>>> getAllConfirmedSkills(@AuthenticationPrincipal Jwt user) {
        return skillService.getAllConfirmedOrCreatorSkills(user.getId());
    }

    @GetMapping("/{skillType}")
    public Flux<SkillDTO> getSkillsByType(@PathVariable SkillType skillType) {
        return skillService.getSkillsByType(skillType);
    }

    @PostMapping("/add")
    @PreAuthorize("hasRole('ADMIN')")
    public Mono<SkillDTO> addSkill(@RequestBody SkillDTO skill, @AuthenticationPrincipal Jwt user) {
        return skillService.addSkill(skill, user.getId())
                .switchIfEmpty(Mono.error(new NotFoundException("Not add!")));
    }

    @PostMapping("/add/no-confirmed")
    public Mono<SkillDTO> addNoConfirmedSkill(@RequestBody SkillDTO skill, @AuthenticationPrincipal Jwt user) {
        return skillService.addNoConfirmedSkill(skill, user.getId())
                .switchIfEmpty(Mono.error(new NotFoundException("Not add!")));
    }

    @PutMapping("/update/{skillId}")
    @PreAuthorize("hasRole('ADMIN')")
    public Mono<InfoResponse> updateSkill(@RequestBody SkillDTO skillDTO, @PathVariable String skillId, @AuthenticationPrincipal Jwt user) {
        return skillService.updateSkill(skillDTO, skillId, user.getId())
                .thenReturn(new InfoResponse(HttpStatus.OK, "Success updating"))
                .onErrorReturn(new InfoResponse(HttpStatus.BAD_REQUEST,"Update is not success"));
    }

    @PutMapping("/confirm/{skillId}")
    @PreAuthorize("hasRole('ADMIN')")
    public Mono<InfoResponse> confirmSkill(@PathVariable String skillId, @AuthenticationPrincipal Jwt user) {
        return skillService.confirmSkill(skillId, user.getId())
                .thenReturn(new InfoResponse(HttpStatus.OK,"Success confirming"))
                .onErrorReturn(new InfoResponse(HttpStatus.BAD_REQUEST,"Confirm is not success"));
    }
    
    @DeleteMapping("/delete/{skillId}")
    @PreAuthorize("hasRole('ADMIN')")
    public Mono<InfoResponse> deleteSkill(@PathVariable String skillId) {
        return skillService.deleteSkill(skillId)
                .thenReturn(new InfoResponse(HttpStatus.OK,"Success deleting"))
                .onErrorReturn(new InfoResponse(HttpStatus.BAD_REQUEST,"Delete is not success"));
    }
}
