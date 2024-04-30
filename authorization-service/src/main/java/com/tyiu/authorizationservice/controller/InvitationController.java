package com.tyiu.authorizationservice.controller;

import com.tyiu.authorizationservice.model.request.InvitationRequest;
import com.tyiu.authorizationservice.model.entity.User;
import com.tyiu.authorizationservice.service.InvitationService;
import lombok.RequiredArgsConstructor;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api/v1/authorization-service/invitation")
@RequiredArgsConstructor
public class InvitationController {

    private final InvitationService invitationService;

    @PostMapping("/send/email")
    public void sendInvitationToEmail(@RequestBody InvitationRequest invitation, @AuthenticationPrincipal User user) {
        invitationService.sendInvitationToEmail(invitation, user);
    }

    @DeleteMapping("/delete/{id}")
    public void deleteInvitation(@PathVariable String id) {
        invitationService.deleteInvitation(id);
    }

}
