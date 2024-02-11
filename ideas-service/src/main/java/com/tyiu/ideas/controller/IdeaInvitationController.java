package com.tyiu.ideas.controller;

import com.tyiu.ideas.model.dto.IdeaInvitationDTO;
import com.tyiu.ideas.model.dto.IdeaMarketDTO;
import com.tyiu.ideas.model.entities.User;
import com.tyiu.ideas.model.requests.IdeaInvitationStatusRequest;
import com.tyiu.ideas.service.IdeaInvitationService;
import lombok.RequiredArgsConstructor;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

@RestController
@RequestMapping("/api/v1/ideas-service/idea/invitation")
@RequiredArgsConstructor
public class IdeaInvitationController {

    private final IdeaInvitationService ideaInvitationService;

    @GetMapping("/idea-market")
    public Flux<IdeaMarketDTO> getAllInitiatorMarketIdeasForInvitations(@AuthenticationPrincipal User user) {
        return ideaInvitationService.getAllInitiatorMarketIdeasForInvitations(user.getId());
    }
    @GetMapping("/all/initiator")
    public Flux<IdeaInvitationDTO> getAllInvitationsByInitiator(@AuthenticationPrincipal User user) {
        return ideaInvitationService.getAllInvitationsByInitiator(user.getId());
    }

    @GetMapping("/team/all/{teamId}")
    public Flux<IdeaInvitationDTO> getAllInvitationsInTeam(@PathVariable String teamId) {
        return ideaInvitationService.getAllInvitationsInTeam(teamId);
    }

    @GetMapping("/all/{ideaId}")
    public Flux<IdeaInvitationDTO> getAllInvitationInIdea(@PathVariable String ideaId) {
        return ideaInvitationService.getAllInvitationInIdea(ideaId);
    }

    @PutMapping("/status/team")
    public Mono<Void> changeInvitationStatusByTeam(@RequestBody IdeaInvitationStatusRequest request){
        return ideaInvitationService.changeInvitationStatusByTeam(request);
    }
    @PutMapping("/status/initiator")
    public Mono<Void> changeInvitationStatusByInitiator(@RequestBody IdeaInvitationStatusRequest request){
        return ideaInvitationService.changeInvitationStatusByInitiator(request);
    }


    @PostMapping("/{teamId}/{ideaId}")
    public Mono<IdeaInvitationDTO> inviteToIdea(@PathVariable String ideaId, @PathVariable String teamId){
        return ideaInvitationService.inviteToIdea(ideaId, teamId);
    }
}
