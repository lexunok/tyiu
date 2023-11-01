package com.tyiu.corn.controller;

import com.tyiu.corn.config.exception.NotFoundException;
import com.tyiu.corn.model.dto.IdeaDTO;

import com.tyiu.corn.model.requests.IdeaSkillRequest;
import com.tyiu.corn.model.requests.StatusIdeaRequest;
import com.tyiu.corn.model.responses.InfoResponse;
import org.springframework.http.HttpStatus;
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
        return ideaService.getIdea(ideaId)
                .switchIfEmpty(Mono.error(new NotFoundException("Not found!")));
    }

    @GetMapping("/all")
    public Flux<IdeaDTO> showListIdea(){
        return ideaService.getListIdea();
    }

    @GetMapping("/initiator/all")
    public Flux<IdeaDTO> showListIdeaByInitiator(Principal principal){
        return ideaService.getListIdeaByInitiator(Long.valueOf(principal.getName()));
    }


    @GetMapping("/skills/{ideaId}")
    public Mono<IdeaSkillRequest> getIdeaSkills(@PathVariable Long ideaId) {
        return ideaService.getIdeaSkills(ideaId);
    }

    @PostMapping("/skills/add")
    public Mono<InfoResponse> addIdeaSkills(@RequestBody IdeaSkillRequest request) {
        return ideaService.addIdeaSkills(request)
                .thenReturn(new InfoResponse(HttpStatus.OK,"Success!"))
                .onErrorReturn(new InfoResponse(HttpStatus.BAD_REQUEST,"Not success."));
    }

    @PostMapping("/add")
    public Mono<IdeaDTO> saveIdea(@RequestBody IdeaDTO idea, Principal principal) {
        return ideaService.saveIdeaToApproval(idea, Long.valueOf(principal.getName()))
                .switchIfEmpty(Mono.error(new NotFoundException("Not found!")));
    }
    @PostMapping("/draft/add")
    public Mono<IdeaDTO> addIdeaInDraft(@RequestBody IdeaDTO idea, Principal principal) {
        return ideaService.saveIdeaInDraft(idea, Long.valueOf(principal.getName()))
                .switchIfEmpty(Mono.error(new NotFoundException("Not found!")));
    }

    @DeleteMapping("/delete/{ideaId}")
    public Mono<InfoResponse> deleteIdea(@PathVariable Long ideaId) {
        return ideaService.deleteIdea(ideaId)
                .thenReturn(new InfoResponse(HttpStatus.OK,"Success deleting"))
                .onErrorReturn(new InfoResponse(HttpStatus.BAD_REQUEST,"Delete is not success"));
    }

    @PutMapping("/initiator/update/{ideaId}")
    public Mono<InfoResponse> updateIdeaByInitiator(@PathVariable Long ideaId,
                                                    @RequestBody IdeaDTO updatedIdea) {
        return ideaService.updateIdeaByInitiator(ideaId, updatedIdea)
                .thenReturn(new InfoResponse(HttpStatus.OK,"Success updating"))
                .onErrorReturn(new InfoResponse(HttpStatus.BAD_REQUEST,"Update is not success"));
    }

    @PutMapping("/initiator/send/{ideaId}")
    public Mono<InfoResponse> updateStatusByInitiator(@PathVariable Long ideaId) {
        return ideaService.updateStatusByInitiator(ideaId)
                .thenReturn(new InfoResponse(HttpStatus.OK,"Success updating"))
                .onErrorReturn(new InfoResponse(HttpStatus.BAD_REQUEST,"Update is not success"));
    }

    @PutMapping("/status/update/{ideaId}")
    public Mono<InfoResponse> updateStatusIdea(@PathVariable Long ideaId,
                                                              @RequestBody StatusIdeaRequest status){
        return ideaService.updateStatusIdea(ideaId, status)
                .thenReturn(new InfoResponse(HttpStatus.OK,"Success updating"))
                .onErrorReturn(new InfoResponse(HttpStatus.BAD_REQUEST,"Update is not success"));
    }

    @PutMapping("/admin/update/{ideaId}")
    public Mono<InfoResponse> updateIdeaByAdmin(@PathVariable Long ideaId,
                                                @RequestBody IdeaDTO updatedIdea) {
        return ideaService.updateIdeaByAdmin(ideaId, updatedIdea)
                .thenReturn(new InfoResponse(HttpStatus.OK, "Success updating"))
                .onErrorReturn(new InfoResponse(HttpStatus.BAD_REQUEST,"Update is not success"));
    }

    @PutMapping("/skills/update")
    public Mono<InfoResponse> updateIdeaSkills(@RequestBody IdeaSkillRequest request) {
        return ideaService.updateIdeaSkills(request)
                .thenReturn(new InfoResponse(HttpStatus.OK, "Success updating"))
                .onErrorReturn(new InfoResponse(HttpStatus.BAD_REQUEST,"Update is not success"));
    }
}
