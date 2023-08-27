package com.tyiu.corn.controller;

import com.tyiu.corn.model.dto.IdeaDTO;
import com.tyiu.corn.model.dto.RatingDTO;
import com.tyiu.corn.model.enums.StatusIdea;

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

    @GetMapping("/initiator")
    public Flux<IdeaDTO> showListIdeaForInitiator(Principal principal){
        return ideaService.getListIdeaForInitiator(principal.getName());
    }
    
    @GetMapping("/initiator/{ideaId}")
    public Mono<IdeaDTO> getIdeaForInitiator(@PathVariable Long ideaId, Principal principal) {
        return ideaService.getIdeaForInitiator(ideaId, principal.getName());
    }

    @GetMapping("/all")
    public Flux<IdeaDTO> showListIdeaForAdmin(){
        return ideaService.getListIdea();
    }

    @PostMapping("/initiator/add")
    public Mono<IdeaDTO> addIdea(@RequestBody IdeaDTO idea, Principal principal) {
        return ideaService.saveIdea(idea, principal.getName());
    }

    @DeleteMapping("/initiator/delete/{ideaId}")
    public Mono<Void> deleteIdeaByInitiator(@PathVariable Long ideaId, Principal principal) {
        ideaService.deleteIdeaByInitiator(ideaId, principal.getName());
        return Mono.empty();
    }

    @DeleteMapping("/admin/delete/{ideaId}")
    public Mono<Void> deleteIdeaByAdmin(@PathVariable Long ideaId) {
        ideaService.deleteIdeaByAdmin(ideaId);
        return Mono.empty();
    }

    @PutMapping("/initiator/update/{ideaId}")
    public Mono<Void> updateIdeaByInitiator(@PathVariable Long ideaId, Principal principal, @RequestBody IdeaDTO updatedIdea) {
        ideaService.updateIdeaByInitiator(ideaId, principal.getName(), updatedIdea);
        return Mono.empty();
    }

    @PutMapping("/initiator/send/{ideaId}")
    public Mono<Void> updateStatusByInitiator(@PathVariable Long ideaId, Principal principal) {
        ideaService.updateStatusByInitiator(ideaId, principal.getName());
        return Mono.empty();
    }

    @PutMapping("/project-office/update/{ideaId}")
    public Mono<Void> updateStatusIdeaByProjectOffice(@PathVariable Long ideaId, @RequestBody StatusIdea newStatus){
        ideaService.updateStatusByProjectOffice(ideaId, newStatus);
        return Mono.empty();
    }

    @PutMapping("/expert/update/{ideaId}")
    public Mono<Void> updateStatusByExpert(@PathVariable Long ideaId, @RequestBody RatingDTO ratingDTO){
        ideaService.updateStatusByExpert(ideaId, ratingDTO);
        return Mono.empty();
    }

    @PutMapping("/admin/update/{ideaId}")
    public Mono<Void> updateIdeaByAdmin(@PathVariable Long ideaId, @RequestBody IdeaDTO updatedIdea) {
        ideaService.updateIdeaByAdmin(ideaId, updatedIdea);
        return Mono.empty();
    }
}
