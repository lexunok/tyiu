package com.tyiu.emailservice.controller;

import com.tyiu.emailservice.model.dto.InvitationDTO;
import com.tyiu.emailservice.model.dto.InvitationsDTO;
import com.tyiu.emailservice.service.InvitationService;
import com.tyiu.emailservice.model.responses.InvitationResponse;

import com.tyiu.ideas.config.exception.CustomHttpException;
import com.tyiu.ideas.model.dto.UserDTO;
import com.tyiu.ideas.model.entities.User;
import com.tyiu.ideas.model.responses.InfoResponse;

import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;
import reactor.core.publisher.Mono;


@RestController
@RequestMapping("/api/v1/email-service/invitation")
@RequiredArgsConstructor
public class InvitationController {
    private final InvitationService invitationService;

    @PostMapping("/send/{email}/{id}")
    public Mono<Void> sendInvitation(@PathVariable String email, @PathVariable String id, @RequestBody UserDTO user){
        return invitationService.sendInvitation(email, id, user);
    }

//    @PostMapping("/send/emails")
//    @PreAuthorize("hasAuthority('ADMIN')")
//    public Mono<Void> invitationsSend(@RequestBody InvitationsDTO invitationDTO, @AuthenticationPrincipal User user){
//        invitationService.sendInvitations(invitationDTO, user);
//        return Mono.delay(Duration.ofSeconds(5)).then();
//    }

}
