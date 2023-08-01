package com.tyiu.corn.controller;

import org.springframework.web.bind.annotation.*;
import lombok.RequiredArgsConstructor;

import com.tyiu.corn.model.Invitation;
import com.tyiu.corn.service.InvitationService;

@RestController
@RequestMapping("/api/v1/invitation")
@RequiredArgsConstructor
public class InvitationController {
    private final InvitationService invitationService;

    @PostMapping
    public void InvitationSend(@RequestBody Invitation invitation){
        invitationService.sandInvitations(invitation);
    }
}
