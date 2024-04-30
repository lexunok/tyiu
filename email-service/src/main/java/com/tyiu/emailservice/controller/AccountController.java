package com.tyiu.emailservice.controller;

import com.tyiu.client.models.UserDTO;
import com.tyiu.emailservice.service.AccountService;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;
import reactor.core.publisher.Mono;

@RestController
@RequestMapping("/api/v1/email-service")
@RequiredArgsConstructor
public class AccountController {

    private final AccountService accountService;

    @PostMapping("/account/email/{email}/code/{code}")
    public Mono<Void> requestToChangeEmail(@PathVariable String email, @PathVariable String code){
        return accountService.sendCodeToChangeEmail(email, code);
    }

    @PostMapping("/account/password/{email}/code/{code}")
    public Mono<Void> requestToChangePassword(@PathVariable String email, @PathVariable String code) {
        return accountService.sendCodeToChangePassword(email, code);
    }

    @PostMapping("/invitation/send/{email}/{id}")
    public Mono<Void> sendInvitation(@PathVariable String email, @PathVariable String id, @RequestBody UserDTO user){
        return accountService.sendInvitation(email, id, user);
    }
    //    @PostMapping("/send/emails")
//    @PreAuthorize("hasAuthority('ADMIN')")
//    public Mono<Void> invitationsSend(@RequestBody InvitationsDTO invitationDTO, @AuthenticationPrincipal User user){
//        invitationService.sendInvitations(invitationDTO, user);
//        return Mono.delay(Duration.ofSeconds(5)).then();
//    }

}
