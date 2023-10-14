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
    public Mono<IdeaDTO> getIdea(@PathVariable Long ideaId) {
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
    public Mono<ResponseEntity<String>> deleteIdea(@PathVariable Long ideaId) {
        return ideaService.deleteIdea(ideaId)
                .thenReturn(new ResponseEntity<>("Success deleting", HttpStatus.OK));
    }

    @PutMapping("/initiator/update/{ideaId}")
    public Mono<ResponseEntity<String>> updateIdeaByInitiator(
            @PathVariable Long ideaId, @RequestBody IdeaDTO updatedIdea) {
        return ideaService.updateIdeaByInitiator(ideaId, updatedIdea)
                .thenReturn(new ResponseEntity<>("Success updating", HttpStatus.OK));
    }

    @PutMapping("/initiator/send/{ideaId}")
    public Mono<ResponseEntity<String>> updateStatusByInitiator(
            @PathVariable Long ideaId) {
        return ideaService.updateStatusByInitiator(ideaId)
                .thenReturn(new ResponseEntity<>("Success updating", HttpStatus.OK));
    }

    @PutMapping("/project-office/update/{ideaId}")
    public Mono<ResponseEntity<String>> updateStatusIdeaByProjectOffice(
            @PathVariable Long ideaId, @RequestBody StatusIdeaRequest status){
        return ideaService.updateStatusByProjectOffice(ideaId, status)
                .thenReturn(new ResponseEntity<>("Success updating", HttpStatus.OK));
    }

    @PutMapping("/admin/update/{ideaId}")
    public Mono<ResponseEntity<String>> updateIdeaByAdmin(
            @PathVariable Long ideaId, @RequestBody IdeaDTO updatedIdea) {
        return ideaService.updateIdeaByAdmin(ideaId, updatedIdea)
                .thenReturn(new ResponseEntity<>("Success updating", HttpStatus.OK));
    }
}
