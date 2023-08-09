package com.tyiu.corn.controller;

import java.util.Map;

import org.springframework.web.bind.annotation.*;

import lombok.RequiredArgsConstructor;

import com.tyiu.corn.model.entities.Invitation;
import com.tyiu.corn.model.requests.ChangeEmailRequest;
import com.tyiu.corn.model.responses.ChangeEmailResponse;
import com.tyiu.corn.model.responses.InvitationResponse;
import com.tyiu.corn.service.InvitationService;
import com.tyiu.corn.model.dto.InvitationDTO;

@RestController
@RequestMapping("/api/v1/invitation")
@RequiredArgsConstructor
public class InvitationController {
    private final InvitationService invitationService;

    @PostMapping("/email")
    public Map<String, String> invitationSend(@RequestBody Invitation invitation){
        invitationService.sendInvitation(invitation);
        return Map.of("success", "Успешное приглашение");
    }

    @PostMapping("/emails")
    public Map<String, String> invitationFileSend(@RequestBody InvitationDTO invitationDTO){
        invitationService.sendInvitations(invitationDTO);
        return Map.of("success", "Успешное приглашение");
    }

    @PostMapping("/send-request-to-change-email")
    public Map<String, String> requestToChangeEmail(@RequestBody Invitation invitation){
        invitationService.sendChangeEmailbyEmail(invitation);
        return Map.of("succes", "Ссылка на изменение почты находится на новой почте");
    }

    @PutMapping("/change-email")
    public Map<String, String> changeEmailByUser(@RequestBody ChangeEmailRequest request){
        invitationService.changeEmailByUser(request);
        return Map.of("success", "Успешное изменение почты");
    }

    @GetMapping("/get-invitation/{url}")
    public InvitationResponse registrateByInvitation(@PathVariable String url){
        return invitationService.findByUrl(url);
    }

    @GetMapping("/change-email/{url}")
    public ChangeEmailResponse changeNewEmail(@PathVariable String url){
        return invitationService.findByUrlAndSendCode(url);
    }
    @DeleteMapping("/delete-invitation/{url}")
    public void DeleteByUrl(@PathVariable String url){
        invitationService.deleteInvitationByUrl(url);
    }    
}