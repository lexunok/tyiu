package com.tyiu.corn.controller;

import com.tyiu.corn.config.exception.AccessException;
import com.tyiu.corn.config.exception.NotFoundException;
import com.tyiu.corn.model.dto.IdeaDTO;

import com.tyiu.corn.model.entities.User;
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
import java.security.Principal;


@RestController
@RequestMapping("/api/v1/idea")
@RequiredArgsConstructor
public class IdeaController {
    
    private final IdeaService ideaService;

    @GetMapping("/{ideaId}")
    @PreAuthorize("hasAuthority('PROJECT_OFFICE') || hasAuthority('EXPERT') || hasAuthority('ADMIN')")
    public Mono<IdeaDTO> getIdea(@PathVariable String ideaId) {
        return ideaService.getIdea(ideaId);
    }

    @GetMapping("/initiator/{ideaId}")
    public Mono<IdeaDTO> getIdeaForInitiator(@PathVariable String ideaId, @AuthenticationPrincipal User user) {
        return ideaService.getIdea(ideaId)
                .flatMap(idea -> {
                    if (idea.getInitiator().equals(user.getEmail())) {
                        return Mono.just(idea);
                    } else {
                        return Mono.error(new AccessException("Нет прав!"));
                    }
                });
    }

    @GetMapping("/all")
    @PreAuthorize("hasAuthority('PROJECT_OFFICE') || hasAuthority('EXPERT') || hasAuthority('ADMIN')")
    public Flux<IdeaDTO> showListIdea(){
        return ideaService.getListIdea();
    }

    @GetMapping("/initiator/all")
    public Flux<IdeaDTO> showListIdeaByInitiator(@AuthenticationPrincipal User user){
        return ideaService.getListIdeaByInitiator(user.getId());
    }

    @GetMapping("/skills/{ideaId}")
    public Mono<IdeaSkillRequest> getIdeaSkills(@PathVariable String ideaId) {
        return ideaService.getIdeaSkills(ideaId);
    }

    @PostMapping("/skills/add")
    @PreAuthorize("hasAuthority('INITIATOR') || hasAuthority('ADMIN')")
    public Mono<InfoResponse> addIdeaSkills(@RequestBody IdeaSkillRequest request) {
        return ideaService.addIdeaSkills(request)
                .thenReturn(new InfoResponse(HttpStatus.OK,"Success!"))
                .onErrorReturn(new InfoResponse(HttpStatus.BAD_REQUEST,"Not success."));
    }

    @PostMapping("/add")
    @PreAuthorize("hasAuthority('INITIATOR') || hasAuthority('ADMIN')")
    public Mono<IdeaDTO> saveIdea(@RequestBody IdeaDTO idea, Principal principal) {
        return ideaService.saveIdeaToApproval(idea, principal.getName())
                .switchIfEmpty(Mono.error(new NotFoundException("Not found!")));
    }

    @PostMapping("/draft/add")
    @PreAuthorize("hasAuthority('INITIATOR') || hasAuthority('ADMIN')")
    public Mono<IdeaDTO> addIdeaInDraft(@RequestBody IdeaDTO idea, Principal principal) {
        return ideaService.saveIdeaInDraft(idea, principal.getName())
                .switchIfEmpty(Mono.error(new NotFoundException("Not found!")));
    }

    @DeleteMapping("/delete/{ideaId}")
    @PreAuthorize("hasAuthority('INITIATOR') || hasAuthority('ADMIN')")
    public Mono<InfoResponse> deleteIdea(@PathVariable String ideaId) {
        return ideaService.deleteIdea(ideaId)
                .thenReturn(new InfoResponse(HttpStatus.OK,"Success deleting"))
                .onErrorReturn(new InfoResponse(HttpStatus.BAD_REQUEST,"Delete is not success"));
    }

    @PutMapping("/initiator/update/{ideaId}")
    @PreAuthorize("hasAuthority('INITIATOR') || hasAuthority('ADMIN')")
    public Mono<InfoResponse> updateIdeaByInitiator(@PathVariable String ideaId,
                                                    @RequestBody IdeaDTO updatedIdea) {
        return ideaService.updateIdeaByInitiator(ideaId, updatedIdea)
                .thenReturn(new InfoResponse(HttpStatus.OK,"Success updating"))
                .onErrorReturn(new InfoResponse(HttpStatus.BAD_REQUEST,"Update is not success"));
    }

    @PutMapping("/initiator/send/{ideaId}")
    @PreAuthorize("hasAuthority('INITIATOR') || hasAuthority('ADMIN')")
    public Mono<InfoResponse> updateStatusByInitiator(@PathVariable String ideaId) {
        return ideaService.updateStatusByInitiator(ideaId)
                .thenReturn(new InfoResponse(HttpStatus.OK,"Success updating"))
                .onErrorReturn(new InfoResponse(HttpStatus.BAD_REQUEST,"Update is not success"));
    }

    @PutMapping("/status/update/{ideaId}")
    @PreAuthorize("hasAuthority('PROJECT_OFFICE') || hasAuthority('EXPERT')")
    public Mono<InfoResponse> updateStatusIdea(@PathVariable String ideaId,
                                                              @RequestBody StatusIdeaRequest status){
        return ideaService.updateStatusIdea(ideaId, status)
                .thenReturn(new InfoResponse(HttpStatus.OK,"Success updating"))
                .onErrorReturn(new InfoResponse(HttpStatus.BAD_REQUEST,"Update is not success"));
    }

    @PutMapping("/admin/update/{ideaId}")
    @PreAuthorize("hasAuthority('ADMIN')")
    public Mono<InfoResponse> updateIdeaByAdmin(@PathVariable String ideaId,
                                                @RequestBody IdeaDTO updatedIdea) {
        return ideaService.updateIdeaByAdmin(ideaId, updatedIdea)
                .thenReturn(new InfoResponse(HttpStatus.OK, "Success updating"))
                .onErrorReturn(new InfoResponse(HttpStatus.BAD_REQUEST,"Update is not success"));
    }

    @PutMapping("/skills/update")
    @PreAuthorize("hasAuthority('INITIATOR') || hasAuthority('ADMIN')")
    public Mono<InfoResponse> updateIdeaSkills(@RequestBody IdeaSkillRequest request) {
        return ideaService.updateIdeaSkills(request)
                .thenReturn(new InfoResponse(HttpStatus.OK, "Success updating"))
                .onErrorReturn(new InfoResponse(HttpStatus.BAD_REQUEST,"Update is not success"));
    }
}
