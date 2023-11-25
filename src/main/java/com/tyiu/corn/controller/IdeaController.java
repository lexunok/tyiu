package com.tyiu.corn.controller;

import com.tyiu.corn.config.exception.AccessException;
import com.tyiu.corn.model.dto.IdeaDTO;

import com.tyiu.corn.model.entities.User;
import com.tyiu.corn.model.enums.StatusIdea;
import com.tyiu.corn.model.requests.IdeaSkillRequest;
import com.tyiu.corn.model.requests.StatusIdeaRequest;
import com.tyiu.corn.model.responses.InfoResponse;
import org.springframework.http.HttpStatus;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;
import com.tyiu.corn.service.IdeaService;
import lombok.RequiredArgsConstructor;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.time.LocalDateTime;


@RestController
@RequestMapping("/api/v1/idea")
@RequiredArgsConstructor
public class IdeaController {
    
    private final IdeaService ideaService;

    @GetMapping("/{ideaId}")
    @PreAuthorize("hasAuthority('PROJECT_OFFICE') || hasAuthority('EXPERT') || hasAuthority('ADMIN')")
    public Mono<IdeaDTO> getIdeaWithAuthorities(@PathVariable String ideaId) {
        return ideaService.getIdea(ideaId);
    }

    @GetMapping("/initiator/{ideaId}")
    @PreAuthorize("hasAuthority('INITIATOR')")
    public Mono<IdeaDTO> getIdeaForInitiator(@PathVariable String ideaId, @AuthenticationPrincipal User user) {
        return ideaService.getIdea(ideaId)
                .filter(i -> i.getInitiatorEmail().equals(user.getEmail()))
                .switchIfEmpty(Mono.error(new AccessException("Нет прав!")));

    }

    @GetMapping("/all")
    @PreAuthorize("hasAuthority('PROJECT_OFFICE') || hasAuthority('EXPERT') || hasAuthority('ADMIN')")
    public Flux<IdeaDTO> showListIdea(){
        return ideaService.getListIdea();
    }

    @GetMapping("/all/on-confirmation")
    @PreAuthorize("hasAuthority('EXPERT') || hasAuthority('ADMIN')")
    public Flux<IdeaDTO> showListIdeaOnConfirmation(){
        return ideaService.getListIdeaOnConfirmation();
    }

    @GetMapping("/initiator/all")
    @PreAuthorize("hasAuthority('INITIATOR')")
    public Flux<IdeaDTO> showListIdeaByInitiator(@AuthenticationPrincipal User user){
        return ideaService.getListIdeaByInitiator(user.getEmail());
    }

    @PostMapping("/add")
    public Mono<IdeaDTO> saveIdeaToApproval(@RequestBody IdeaDTO idea, @AuthenticationPrincipal User user) {
        idea.setStatus(StatusIdea.ON_APPROVAL);
        idea.setCreatedAt(LocalDateTime.now());
        return ideaService.saveIdea(idea, user.getEmail());
    }

    @PostMapping("/draft/add")
    public Mono<IdeaDTO> addIdeaInDraft(@RequestBody IdeaDTO idea, @AuthenticationPrincipal User user) {
        idea.setStatus(StatusIdea.NEW);
        return ideaService.saveIdea(idea, user.getEmail());
    }

    @DeleteMapping("/delete/{ideaId}")
    public Mono<InfoResponse> deleteIdea(@PathVariable String ideaId, @AuthenticationPrincipal User user) {
        return ideaService.deleteIdea(ideaId, user)
                .thenReturn(new InfoResponse(HttpStatus.OK,"Идея успешно удалена"))
                .onErrorReturn(new InfoResponse(HttpStatus.BAD_REQUEST,"Идея не удалена"));
    }

    @PutMapping("/initiator/update/{ideaId}")
    @PreAuthorize("hasAuthority('INITIATOR')")
    public Mono<InfoResponse> updateIdeaByInitiator(@PathVariable String ideaId,
                                                    @RequestBody IdeaDTO updatedIdea,
                                                    @AuthenticationPrincipal User user) {
        return ideaService.updateIdeaByInitiator(ideaId, updatedIdea, user.getEmail())
                .thenReturn(new InfoResponse(HttpStatus.OK,"Успешное обновление идеи"))
                .onErrorReturn(new InfoResponse(HttpStatus.BAD_REQUEST,"Обновление идеи не удалось"));
    }

    @PutMapping("/initiator/send/{ideaId}")
    @PreAuthorize("hasAuthority('INITIATOR')")
    public Mono<InfoResponse> updateStatusByInitiator(@PathVariable String ideaId, @AuthenticationPrincipal User user) {
        return ideaService.updateStatusByInitiator(ideaId, user.getEmail())
                .thenReturn(new InfoResponse(HttpStatus.OK,"Успешная отправка идеи"))
                .onErrorReturn(new InfoResponse(HttpStatus.BAD_REQUEST,"Идея не отправилась"));
    }

    @PutMapping("/status/update/{ideaId}")
    @PreAuthorize("hasAuthority('PROJECT_OFFICE') || hasAuthority('EXPERT') || hasAuthority('ADMIN') ")
    public Mono<InfoResponse> updateStatusIdea(@PathVariable String ideaId,
                                                              @RequestBody StatusIdeaRequest status){
        return ideaService.updateStatusIdea(ideaId, status)
                .thenReturn(new InfoResponse(HttpStatus.OK,"Статус идеи обновлен"))
                .onErrorReturn(new InfoResponse(HttpStatus.BAD_REQUEST,"Статус не обновлен"));
    }

    @PutMapping("/admin/update/{ideaId}")
    @PreAuthorize("hasAuthority('ADMIN')")
    public Mono<InfoResponse> updateIdeaByAdmin(@PathVariable String ideaId,
                                                @RequestBody IdeaDTO updatedIdea) {
        return ideaService.updateIdeaByAdmin(ideaId, updatedIdea)
                .thenReturn(new InfoResponse(HttpStatus.OK, "Идея обновлена"))
                .onErrorReturn(new InfoResponse(HttpStatus.BAD_REQUEST,"Идея не обновлена"));
    }

    @PutMapping("/skills/update")
    public Mono<InfoResponse> updateIdeaSkills(@RequestBody IdeaSkillRequest request, @AuthenticationPrincipal User user) {
        return ideaService.updateIdeaSkills(request, user)
                .thenReturn(new InfoResponse(HttpStatus.OK, "Скиллы для идеи обновлены"))
                .onErrorReturn(new InfoResponse(HttpStatus.BAD_REQUEST,"Скиллы для идеи не обновлены"));
    }

    @GetMapping("/skills/{ideaId}")
    public Mono<IdeaSkillRequest> getIdeaSkills(@PathVariable String ideaId, @AuthenticationPrincipal User user) {
        return ideaService.getIdeaSkills(ideaId, user.getEmail());
    }

    @PostMapping("/skills/add")
    public Mono<InfoResponse> addIdeaSkills(@RequestBody IdeaSkillRequest request, @AuthenticationPrincipal User user) {
        return ideaService.addIdeaSkills(request, user)
                .thenReturn(new InfoResponse(HttpStatus.OK,"Успешное добавление скиллов идеи"))
                .onErrorReturn(new InfoResponse(HttpStatus.BAD_REQUEST,"Скиллы для идеи не добавлены"));
    }
}
