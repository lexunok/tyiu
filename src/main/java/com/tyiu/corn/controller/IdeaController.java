package com.tyiu.corn.controller;

import com.tyiu.corn.model.dto.IdeaDTO;

import com.tyiu.corn.model.requests.StatusIdeaRequest;
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
    public Mono<IdeaDTO> getIdeaForInitiator(@PathVariable String ideaId) {
        return ideaService.getIdea(ideaId);
    }

    @GetMapping("/all")
    public Flux<IdeaDTO> showListIdeaForAdmin(){
        return ideaService.getListIdea();
    }

    @PostMapping("/add")
    public Mono<IdeaDTO> addIdea(@RequestBody IdeaDTO idea, Principal principal) {
        return ideaService.saveIdea(idea, principal.getName());
    }

    @DeleteMapping("/delete/{ideaId}")
    public Mono<Void> deleteIdea(@PathVariable String ideaId) {
        return ideaService.deleteIdea(ideaId);
    }

    @PutMapping("/initiator/update/{ideaId}")
    public Mono<Void> updateIdeaByInitiator(@PathVariable String ideaId, @RequestBody IdeaDTO updatedIdea) {
        ideaService.updateIdeaByInitiator(ideaId, updatedIdea);
        return Mono.empty();
    }

    @PutMapping("/initiator/send/{ideaId}")
    public Mono<Void> updateStatusByInitiator(@PathVariable String ideaId, Principal principal) {
        ideaService.updateStatusByInitiator(ideaId, principal.getName());
        return Mono.empty();
    }

    @PutMapping("/project-office/update/{ideaId}")
    public Mono<Void> updateStatusIdeaByProjectOffice(@PathVariable String ideaId, @RequestBody StatusIdeaRequest status){
        ideaService.updateStatusByProjectOffice(ideaId, status);
        return Mono.empty();
    }

    @PutMapping("/admin/update/{ideaId}")
    public Mono<Void> updateIdeaByAdmin(@PathVariable String ideaId, @RequestBody IdeaDTO updatedIdea) {
        ideaService.updateIdeaByAdmin(ideaId, updatedIdea);
        return Mono.empty();
    }
}
