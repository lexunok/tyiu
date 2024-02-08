package com.tyiu.ideas.controller;

import com.tyiu.ideas.config.exception.AccessException;
import com.tyiu.ideas.model.dto.IdeaDTO;
import com.tyiu.ideas.model.dto.IdeaInvitationDTO;
import com.tyiu.ideas.model.entities.User;
import com.tyiu.ideas.model.requests.IdeaInvitationStatusRequest;
import com.tyiu.ideas.service.IdeaInvitationService;
import lombok.RequiredArgsConstructor;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

@RestController
@RequestMapping("/api/v1/ideas-service/idea/invitation")
@RequiredArgsConstructor
public class IdeaInvitationController {

    private final IdeaInvitationService ideaInvitationService;

    @GetMapping("/team/all/{teamId}")
    public Flux<IdeaInvitationDTO> getAllInvitationsInTeam(@PathVariable String teamId) {
        return ideaInvitationService.getAllInvitationsInTeam(teamId);
    }

    @GetMapping("/all/{ideaId}")
    public Flux<IdeaInvitationDTO> getAllInvitationInIdea(@PathVariable String ideaId) {
        return ideaInvitationService.getAllInvitationInIdea(ideaId);
    }

    @PutMapping("/status")
    public Mono<Void> changeInvitationStatus(@RequestBody IdeaInvitationStatusRequest request){
        return ideaInvitationService.changeInvitationStatus(request);
    }

    @PostMapping("/{teamId}/{ideaId}")
    public Mono<Void> inviteToIdea(@PathVariable String ideaId, @PathVariable String teamId){
        return ideaInvitationService.inviteToIdea(ideaId, teamId);
    }
}
