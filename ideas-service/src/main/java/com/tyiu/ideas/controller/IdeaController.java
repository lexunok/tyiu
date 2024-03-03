package com.tyiu.ideas.controller;

import com.tyiu.ideas.config.exception.AccessException;
import com.tyiu.ideas.model.dto.IdeaDTO;
import com.tyiu.ideas.model.entities.Idea;
import com.tyiu.ideas.model.requests.IdeaSkillRequest;
import com.tyiu.ideas.model.requests.StatusIdeaRequest;
import com.tyiu.ideas.model.responses.InfoResponse;
import com.tyiu.ideas.service.IdeaService;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.security.oauth2.jwt.Jwt;
import org.springframework.web.bind.annotation.*;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

@RestController
@RequestMapping("/api/v1/ideas-service/idea")
@RequiredArgsConstructor
public class IdeaController {
    
    private final IdeaService ideaService;

    @GetMapping("/{ideaId}")
    @PreAuthorize("hasRole('MEMBER') || hasRole('PROJECT_OFFICE') || hasRole('EXPERT') || hasRole('ADMIN')")
    public Mono<IdeaDTO> getIdeaWithAuthorities(@PathVariable String ideaId, @AuthenticationPrincipal Jwt jwt) {
        return ideaService.getIdea(ideaId, jwt.getId());
    }

    @GetMapping("/initiator/{ideaId}")
    @PreAuthorize("hasRole('INITIATOR')")
    public Mono<IdeaDTO> getIdeaForInitiator(@PathVariable String ideaId, @AuthenticationPrincipal Jwt jwt) {
        return ideaService.getIdea(ideaId, jwt.getId())
                .filter(i -> i.getInitiator().getId().equals(jwt.getId()))
                .switchIfEmpty(Mono.error(new AccessException("Нет прав!")));

    }

    @GetMapping("/all")
    @PreAuthorize("hasRole('MEMBER') || hasRole('PROJECT_OFFICE') || hasRole('EXPERT') || hasRole('ADMIN')")
    public Flux<IdeaDTO> showListIdea(@AuthenticationPrincipal Jwt jwt){
        return ideaService.getListIdea(jwt.getId());
    }

    @GetMapping("/all/on-confirmation")
    @PreAuthorize("hasRole('EXPERT') || hasRole('ADMIN')")
    public Flux<IdeaDTO> showListIdeaOnConfirmation(@AuthenticationPrincipal Jwt jwt){
        return ideaService.getListIdeaOnConfirmation(jwt.getId());
    }

    @GetMapping("/initiator/all")
    @PreAuthorize("hasRole('INITIATOR')")
    public Flux<IdeaDTO> showListIdeaByInitiator(@AuthenticationPrincipal Jwt jwt){
        return ideaService.getListIdeaByInitiator(jwt.getId());
    }

    @PostMapping("/add")
    public Mono<IdeaDTO> saveIdeaToApproval(@RequestBody IdeaDTO idea, @AuthenticationPrincipal Jwt jwt) {
        idea.setStatus(Idea.Status.ON_APPROVAL);
        return ideaService.saveIdea(idea, jwt.getId());
    }

    @PostMapping("/draft/add")
    public Mono<IdeaDTO> addIdeaInDraft(@RequestBody IdeaDTO idea, @AuthenticationPrincipal Jwt jwt) {
        idea.setStatus(Idea.Status.NEW);
        return ideaService.saveIdea(idea, jwt.getId());
    }

    @DeleteMapping("/delete/{ideaId}")
    public Mono<InfoResponse> deleteIdea(@PathVariable String ideaId, @AuthenticationPrincipal Jwt jwt) {
        return ideaService.deleteIdea(ideaId, jwt)
                .thenReturn(new InfoResponse(HttpStatus.OK,"Идея успешно удалена"))
                .onErrorReturn(new InfoResponse(HttpStatus.BAD_REQUEST,"Идея не удалена"));
    }


    @PutMapping("/initiator/send/{ideaId}")
    @PreAuthorize("hasRole('INITIATOR')")
    public Mono<InfoResponse> updateStatusByInitiator(@PathVariable String ideaId, @AuthenticationPrincipal Jwt jwt) {
        return ideaService.updateStatusByInitiator(ideaId, jwt.getId())
                .thenReturn(new InfoResponse(HttpStatus.OK,"Успешная отправка идеи"))
                .onErrorReturn(new InfoResponse(HttpStatus.BAD_REQUEST,"Идея не отправилась"));
    }

    @PutMapping("/status/update/{ideaId}")
    @PreAuthorize("hasRole('PROJECT_OFFICE') || hasRole('EXPERT') || hasRole('ADMIN') ")
    public Mono<InfoResponse> updateStatusIdea(@PathVariable String ideaId,
                                               @RequestBody StatusIdeaRequest status){
        return ideaService.updateStatusIdea(ideaId, status)
                .thenReturn(new InfoResponse(HttpStatus.OK,"Статус идеи обновлен"))
                .onErrorReturn(new InfoResponse(HttpStatus.BAD_REQUEST,"Статус не обновлен"));
    }

    @PutMapping("/admin/update/{ideaId}")
    @PreAuthorize("hasRole('ADMIN')")
    public Mono<InfoResponse> updateIdeaByAdmin(@PathVariable String ideaId,
                                                @RequestBody IdeaDTO updatedIdea) {
        return ideaService.updateIdeaByAdmin(ideaId, updatedIdea)
                .thenReturn(new InfoResponse(HttpStatus.OK, "Идея обновлена"))
                .onErrorReturn(new InfoResponse(HttpStatus.BAD_REQUEST,"Идея не обновлена"));
    }

    @PutMapping("/skills/update")
    public Mono<InfoResponse> updateIdeaSkills(@RequestBody IdeaSkillRequest request, @AuthenticationPrincipal Jwt jwt) {
        return ideaService.updateIdeaSkills(request, jwt)
                .thenReturn(new InfoResponse(HttpStatus.OK, "Скиллы для идеи обновлены"))
                .onErrorReturn(new InfoResponse(HttpStatus.BAD_REQUEST,"Скиллы для идеи не обновлены"));
    }

    @GetMapping("/skills/{ideaId}")
    public Mono<IdeaSkillRequest> getIdeaSkills(@PathVariable String ideaId, @AuthenticationPrincipal Jwt jwt) {
        return ideaService.getIdeaSkills(ideaId, jwt);
    }

    @PostMapping("/skills/add")
    public Mono<InfoResponse> addIdeaSkills(@RequestBody IdeaSkillRequest request, @AuthenticationPrincipal Jwt jwt) {
        return ideaService.addIdeaSkills(request, jwt)
                .thenReturn(new InfoResponse(HttpStatus.OK,"Успешное добавление скиллов идеи"))
                .onErrorReturn(new InfoResponse(HttpStatus.BAD_REQUEST,"Скиллы для идеи не добавлены"));
    }
}
