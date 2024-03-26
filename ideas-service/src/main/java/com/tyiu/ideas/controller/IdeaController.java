package com.tyiu.ideas.controller;


import com.tyiu.ideas.model.dto.*;
import com.tyiu.ideas.model.entities.*;
import com.tyiu.ideas.model.requests.*;
import com.tyiu.ideas.model.responses.*;
import com.tyiu.ideas.config.exception.*;
import com.tyiu.ideas.service.IdeaService;

import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.annotation.AuthenticationPrincipal;

import org.springframework.http.HttpStatus;

import reactor.core.publisher.Mono;
import reactor.core.publisher.Flux;

@RestController
@RequiredArgsConstructor
@RequestMapping("/api/v1/ideas-service/idea")
public class IdeaController {
    
    private final IdeaService ideaService;

    @GetMapping("/{ideaId}")
    @PreAuthorize("hasAuthority('MEMBER') || hasAuthority('TEACHER') || hasAuthority('PROJECT_OFFICE') || hasAuthority('EXPERT') || hasAuthority('ADMIN')")
    public Mono<IdeaDTO> getIdeaWithAuthorities(@PathVariable String ideaId,
                                                @AuthenticationPrincipal User user) {
        return ideaService.getIdea(ideaId, user.getId());
    }

    @GetMapping("/initiator/{ideaId}")
    @PreAuthorize("hasAuthority('INITIATOR')")
    public Mono<IdeaDTO> getIdeaForInitiator(@PathVariable String ideaId,
                                             @AuthenticationPrincipal User user) {
        return ideaService.getIdea(ideaId, user.getId())
                .filter(i -> i.getInitiator().getId().equals(user.getId()))
                .switchIfEmpty(Mono.error(new AccessException("Нет прав!")));

    }

    @GetMapping("/all")
    @PreAuthorize("hasAuthority('MEMBER')|| hasAuthority('TEACHER') || hasAuthority('PROJECT_OFFICE') || hasAuthority('EXPERT') || hasAuthority('ADMIN')")
    public Flux<IdeaDTO> showListIdea(@AuthenticationPrincipal User user) {
        return ideaService.getListIdea(user.getId());
    }

    @GetMapping("/all/on-confirmation")
    @PreAuthorize("hasAuthority('EXPERT') || hasAuthority('ADMIN')")
    public Flux<IdeaDTO> showListIdeaOnConfirmation(@AuthenticationPrincipal User user) {
        return ideaService.getListIdeaOnConfirmation(user.getId());
    }

    @GetMapping("/initiator/all")
    @PreAuthorize("hasAuthority('INITIATOR')")
    public Flux<IdeaDTO> showListIdeaByInitiator(@AuthenticationPrincipal User user) {
        return ideaService.getListIdeaByInitiator(user);
    }

    @PostMapping("/add")
    public Mono<IdeaDTO> saveIdeaToApproval(@RequestBody IdeaDTO idea,
                                            @AuthenticationPrincipal User user) {
        idea.setStatus(Idea.Status.ON_APPROVAL);
        return ideaService.saveIdea(idea, user.getId());
    }

    @PostMapping("/draft/add")
    public Mono<IdeaDTO> addIdeaInDraft(@RequestBody IdeaDTO idea,
                                        @AuthenticationPrincipal User user) {
        idea.setStatus(Idea.Status.NEW);
        return ideaService.saveIdea(idea, user.getId());
    }

    @DeleteMapping("/delete/{ideaId}")
    public Mono<InfoResponse> deleteIdea(@PathVariable String ideaId,
                                         @AuthenticationPrincipal User user) {
        return ideaService.deleteIdea(ideaId, user)
                .thenReturn(new InfoResponse(HttpStatus.OK,"Идея успешно удалена"))
                .onErrorReturn(new InfoResponse(HttpStatus.BAD_REQUEST,"Идея не удалена"));
    }


    @PutMapping("/initiator/send/{ideaId}")
    @PreAuthorize("hasAuthority('INITIATOR')")
    public Mono<InfoResponse> updateStatusByInitiator(@PathVariable String ideaId,
                                                      @AuthenticationPrincipal User user) {
        return ideaService.updateStatusByInitiator(ideaId, user.getId())
                .thenReturn(new InfoResponse(HttpStatus.OK,"Успешная отправка идеи"))
                .onErrorReturn(new InfoResponse(HttpStatus.BAD_REQUEST,"Идея не отправилась"));
    }

    @PutMapping("/status/update/{ideaId}")
    @PreAuthorize("hasAuthority('PROJECT_OFFICE') || hasAuthority('EXPERT') || hasAuthority('ADMIN') ")
    public Mono<InfoResponse> updateStatusIdea(@PathVariable String ideaId,
                                               @RequestBody StatusIdeaRequest status,
                                               @AuthenticationPrincipal User user) {
        return ideaService.updateStatusIdea(ideaId, status, user)
                .thenReturn(new InfoResponse(HttpStatus.OK,"Статус идеи обновлен"))
                .onErrorReturn(new InfoResponse(HttpStatus.BAD_REQUEST,"Статус не обновлен"));
    }

    @PutMapping("/admin/update/{ideaId}")
    @PreAuthorize("hasAuthority('ADMIN')")
    public Mono<InfoResponse> updateIdeaByAdmin(@PathVariable String ideaId,
                                                @RequestBody IdeaDTO updatedIdea,
                                                @AuthenticationPrincipal User user) {
        return ideaService.updateIdeaByAdmin(ideaId, updatedIdea, user)
                .thenReturn(new InfoResponse(HttpStatus.OK, "Идея обновлена"))
                .onErrorReturn(new InfoResponse(HttpStatus.BAD_REQUEST,"Идея не обновлена"));
    }

    @PutMapping("/skills/update")
    public Mono<InfoResponse> updateIdeaSkills(@RequestBody IdeaSkillRequest request,
                                               @AuthenticationPrincipal User user) {
        return ideaService.updateIdeaSkills(request, user)
                .thenReturn(new InfoResponse(HttpStatus.OK, "Скиллы для идеи обновлены"))
                .onErrorReturn(new InfoResponse(HttpStatus.BAD_REQUEST,"Скиллы для идеи не обновлены"));
    }

    @GetMapping("/skills/{ideaId}")
    public Mono<IdeaSkillRequest> getIdeaSkills(@PathVariable String ideaId,
                                                @AuthenticationPrincipal User user) {
        return ideaService.getIdeaSkills(ideaId, user);
    }

    @PostMapping("/skills/add")
    public Mono<InfoResponse> addIdeaSkills(@RequestBody IdeaSkillRequest request,
                                            @AuthenticationPrincipal User user) {
        return ideaService.addIdeaSkills(request, user)
                .thenReturn(new InfoResponse(HttpStatus.OK,"Успешное добавление скиллов идеи"))
                .onErrorReturn(new InfoResponse(HttpStatus.BAD_REQUEST,"Скиллы для идеи не добавлены"));
    }
}
