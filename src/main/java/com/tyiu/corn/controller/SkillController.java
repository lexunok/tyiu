package com.tyiu.corn.controller;

import com.tyiu.corn.config.exception.NotFoundException;
import com.tyiu.corn.model.dto.SkillDTO;
import com.tyiu.corn.model.entities.Skill;
import com.tyiu.corn.model.enums.SkillType;
import com.tyiu.corn.model.responses.InfoResponse;
import com.tyiu.corn.model.responses.TeamMemberResponse;
import com.tyiu.corn.service.SkillService;

import lombok.RequiredArgsConstructor;

import java.security.Principal;
import java.util.Collection;
import java.util.List;
import java.util.Map;

import org.springframework.http.HttpStatus;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

@RestController
@RequestMapping("/api/v1/skill")
@RequiredArgsConstructor
public class SkillController {
    private final SkillService skillService;

    /*@GetMapping("/users/all")
    public Flux<TeamMemberResponse> getAllUsersWithSkills(){
        return skillService.getAllUsersWithSkills();
    }*/

    @GetMapping("/all")
    public Flux<SkillDTO> getAllSkills() {
        return skillService.getAllSkills();
    }

    @GetMapping("/all-confirmed-or-creator")
    public Mono<Map<SkillType, Collection<SkillDTO>>> getAllConfirmedSkills(Principal principal) {
        return skillService.getAllConfirmedOrCreatorSkills(Long.valueOf(principal.getName()));
    }

    @GetMapping("/{skillType}")
    public Flux<SkillDTO> getSkillsByType(@PathVariable SkillType skillType) {
        return skillService.getSkillsByType(skillType);
    }

    @PostMapping("/add")
    @PreAuthorize("hasAuthority('ADMIN')")
    public Mono<SkillDTO> addSkill(@RequestBody SkillDTO skill, Principal principal) {
        return skillService.addSkill(skill, Long.valueOf(principal.getName()))
                .switchIfEmpty(Mono.error(new NotFoundException("Not add!")));
    }

    @PostMapping("/add/no-confirmed")
    public Mono<SkillDTO> addNoConfirmedSkill(@RequestBody SkillDTO skill, Principal principal) {
        return skillService.addNoConfirmedSkill(skill, Long.valueOf(principal.getName()))
                .switchIfEmpty(Mono.error(new NotFoundException("Not add!")));
    }

    @PutMapping("/update/{skillId}")
    @PreAuthorize("hasAuthority('ADMIN')")
    public Mono<InfoResponse> updateSkill(@RequestBody SkillDTO skillDTO, @PathVariable Long skillId, Principal principal) {
        return skillService.updateSkill(skillDTO, skillId, Long.valueOf(principal.getName()))
                .thenReturn(new InfoResponse(HttpStatus.OK, "Success updating"))
                .onErrorReturn(new InfoResponse(HttpStatus.BAD_REQUEST,"Update is not success"));
    }

    @PutMapping("/confirm/{skillId}")
    @PreAuthorize("hasAuthority('ADMIN')")
    public Mono<InfoResponse> confirmSkill(@PathVariable Long skillId, Principal principal) {
        return skillService.confirmSkill(skillId, Long.valueOf(principal.getName()))
                .thenReturn(new InfoResponse(HttpStatus.OK,"Success confirming"))
                .onErrorReturn(new InfoResponse(HttpStatus.BAD_REQUEST,"Confirm is not success"));
    }
    
    @DeleteMapping("/delete/{skillId}")
    @PreAuthorize("hasAuthority('ADMIN')")
    public Mono<InfoResponse> deleteSkill(@PathVariable Long skillId) {
        return skillService.deleteSkill(skillId)
                .thenReturn(new InfoResponse(HttpStatus.OK,"Success deleting"))
                .onErrorReturn(new InfoResponse(HttpStatus.BAD_REQUEST,"Delete is not success"));
    }
}
