package com.tyiu.corn.controller;

import java.util.List;

import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import lombok.RequiredArgsConstructor;

import com.tyiu.corn.model.enums.Role;
import com.tyiu.corn.service.InvitationService;
import com.tyiu.corn.dto.InvitationDTO;

@RestController
@RequestMapping("/api/v1/invitation")
@RequiredArgsConstructor
public class InvitationController {
    private final InvitationService invitationService;

    @PostMapping("/list")
    public void InvitationSend(@RequestBody InvitationDTO invitations){
        invitationService.sendInvitations(invitations);
    }

    @PostMapping("/file")
    public void InvitationSend(@RequestParam MultipartFile file, @RequestBody() List<Role> roles){
        InvitationDTO invitations = new InvitationDTO();
        return;
    }
}
