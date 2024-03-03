package com.tyiu.ideas.controller;

import com.tyiu.ideas.model.dto.IdeaInvitationDTO;
import com.tyiu.ideas.model.dto.IdeaMarketDTO;
import com.tyiu.ideas.model.requests.IdeaInvitationStatusRequest;
import com.tyiu.ideas.service.IdeaInvitationService;
import lombok.RequiredArgsConstructor;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.security.oauth2.jwt.Jwt;
import org.springframework.web.bind.annotation.*;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

@RestController
@RequestMapping("/api/v1/ideas-service/idea/invitation")
@RequiredArgsConstructor
public class IdeaInvitationController {

    private final IdeaInvitationService ideaInvitationService;

    @GetMapping("/idea-market")
    public Flux<IdeaMarketDTO> getAllInitiatorMarketIdeasForInvitations(@AuthenticationPrincipal Jwt jwt) {
        return ideaInvitationService.getAllInitiatorMarketIdeasForInvitations(jwt.getId());
    }
    @GetMapping("/all/initiator")
    public Flux<IdeaInvitationDTO> getAllInvitationsByInitiator(@AuthenticationPrincipal Jwt jwt) {
        return ideaInvitationService.getAllInvitationsByInitiator(jwt.getId());
    }

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
    public Mono<IdeaInvitationDTO> inviteToIdea(@PathVariable String ideaId, @PathVariable String teamId){
        return ideaInvitationService.inviteToIdea(ideaId, teamId);
    }
}
