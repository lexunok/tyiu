package com.tyiu.corn.controller;

import com.tyiu.corn.model.dto.IdeaDTO;

import com.tyiu.corn.model.requests.StatusIdeaRequest;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
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
    public Mono<ResponseEntity<String>> deleteIdea(@PathVariable String ideaId) {
        ideaService.deleteIdea(ideaId).subscribe();
        return Mono.just(new ResponseEntity<>("Success deleting", HttpStatus.OK));
    }

    @PutMapping("/initiator/update/{ideaId}")
    public Mono<ResponseEntity<String>> updateIdeaByInitiator(
            @PathVariable String ideaId, @RequestBody IdeaDTO updatedIdea) {
        ideaService.updateIdeaByInitiator(ideaId, updatedIdea).subscribe();
        return Mono.just(new ResponseEntity<>("Success updating", HttpStatus.OK));
    }

    @PutMapping("/initiator/send/{ideaId}")
    public Mono<ResponseEntity<String>> updateStatusByInitiator(
            @PathVariable String ideaId, Principal principal) {
        ideaService.updateStatusByInitiator(ideaId, principal.getName()).subscribe();
        return Mono.just(new ResponseEntity<>("Success updating", HttpStatus.OK));
    }

    @PutMapping("/project-office/update/{ideaId}")
    public Mono<ResponseEntity<String>> updateStatusIdeaByProjectOffice(
            @PathVariable String ideaId, @RequestBody StatusIdeaRequest status){
        ideaService.updateStatusByProjectOffice(ideaId, status).subscribe();
        return Mono.just(new ResponseEntity<>("Success updating", HttpStatus.OK));
    }

    @PutMapping("/admin/update/{ideaId}")
    public Mono<ResponseEntity<String>> updateIdeaByAdmin(
            @PathVariable String ideaId, @RequestBody IdeaDTO updatedIdea) {
        ideaService.updateIdeaByAdmin(ideaId, updatedIdea).subscribe();
        return Mono.just(new ResponseEntity<>("Success updating", HttpStatus.OK));
    }
}
