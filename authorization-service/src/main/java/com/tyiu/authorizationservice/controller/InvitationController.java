package com.tyiu.authorizationservice.controller;

import com.nimbusds.jose.shaded.gson.Gson;
import com.tyiu.authorizationservice.model.request.InvitationRequest;
import com.tyiu.authorizationservice.model.request.ManyInvitationsRequest;
import com.tyiu.authorizationservice.service.InvitationService;
import com.tyiu.client.models.UserDTO;
import lombok.RequiredArgsConstructor;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.security.oauth2.jwt.Jwt;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api/v1/authorization-service/invitation")
@RequiredArgsConstructor
public class InvitationController {

    private final InvitationService invitationService;

    @PreAuthorize("hasRole('ADMIN')")
    @PostMapping("/send/email")
    public void sendInvitationToEmail(@RequestBody InvitationRequest invitation, @AuthenticationPrincipal Jwt jwt) {
        Gson gson = new Gson();
        UserDTO user = gson.fromJson(jwt.getClaim("user").toString(), UserDTO.class);
        invitationService.sendInvitationToEmail(invitation, user);
    }

    @PreAuthorize("hasRole('ADMIN')")
    @PostMapping("/send/many")
    public void sendManyInvitations(@RequestBody ManyInvitationsRequest request, @AuthenticationPrincipal Jwt jwt) {
        Gson gson = new Gson();
        UserDTO user = gson.fromJson(jwt.getClaim("user").toString(), UserDTO.class);
        invitationService.sendManyInvitations(request, user);
    }

}
