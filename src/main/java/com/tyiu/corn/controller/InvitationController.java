package com.tyiu.corn.controller;

import java.util.Date;
import java.util.Map;

import org.springframework.web.bind.annotation.*;

import lombok.RequiredArgsConstructor;

import com.tyiu.corn.model.entities.Invitation;
import com.tyiu.corn.service.InvitationService;
import com.tyiu.corn.exception.FileReadException;
import com.tyiu.corn.model.dto.InvitationDTO;

@RestController
@RequestMapping("/api/v1/invitation")
@RequiredArgsConstructor
public class InvitationController {
    private final InvitationService invitationService;

    @PostMapping("/email")
    public Map<String, String> InvitationSend(@RequestBody Invitation invitation){
        invitationService.sendInvitation(invitation);
        return Map.of("success", "Успешное приглашение");
    }

    @PostMapping("/file")
    public Map<String, String> InvitationFileSend(
        @ModelAttribute InvitationDTO invitationDTO
    ) throws FileReadException{
        invitationService.sendFileInvitations(invitationDTO);
        return Map.of("success", "Успешное приглашение");
    }

    @GetMapping("get-invitation/{url}")
    public Invitation RegistrateByInvitation(@PathVariable String url){
        return invitationService.findByUrl(url);
    }
}