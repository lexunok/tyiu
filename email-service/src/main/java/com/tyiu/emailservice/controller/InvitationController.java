package com.tyiu.emailservice.controller;

import com.tyiu.emailservice.model.dto.InvitationDTO;
import com.tyiu.emailservice.model.dto.InvitationsDTO;
import com.tyiu.emailservice.service.InvitationService;
import com.tyiu.emailservice.model.responses.InvitationResponse;

import com.tyiu.ideas.config.exception.CustomHttpException;
import com.tyiu.ideas.model.entities.User;
import com.tyiu.ideas.model.responses.InfoResponse;

import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;
import reactor.core.publisher.Mono;

import java.time.Duration;

@RestController
@RequestMapping("/api/v1/email-service/invitation")
@RequiredArgsConstructor
public class InvitationController {
    private final InvitationService invitationService;

    ///////////////////////
    //  _____   ____ ______
    // / ___/  / __//_  __/
    /// (_ /  / _/   / /
    //\___/  /___/  /_/
    ///////////////////////

    @GetMapping("/get/{url}")
    public Mono<InvitationResponse> registerByInvitation(@PathVariable String url){
        return invitationService.findInvitationByUrl(url)
                .switchIfEmpty(Mono.error(new CustomHttpException("Not found!", HttpStatus.NOT_FOUND.value())));
    }

    //////////////////////////////
    //   ___   ____    ____ ______
    //  / _ \ / __ \  / __//_  __/
    // / ___// /_/ / _\ \   / /
    ///_/    \____/ /___/  /_/
    //////////////////////////////

    @PostMapping("/send/email")
    @PreAuthorize("hasAuthority('ADMIN')")
    public Mono<InfoResponse> invitationSend(@RequestBody InvitationDTO invitationDTO, @AuthenticationPrincipal User user){
        return invitationService.sendInvitation(invitationDTO, user)
                .thenReturn(new InfoResponse(HttpStatus.OK,"Успешное приглашение"))
                .onErrorReturn(new InfoResponse(HttpStatus.INTERNAL_SERVER_ERROR,"Ошибка при приглашении"));
    }

    @PostMapping("/send/emails")
    @PreAuthorize("hasAuthority('ADMIN')")
    public Mono<Void> invitationsSend(@RequestBody InvitationsDTO invitationDTO, @AuthenticationPrincipal User user){
        invitationService.sendInvitations(invitationDTO, user);
        return Mono.delay(Duration.ofSeconds(5)).then();
    }

    ///////////////////////////////////////////
    //   ___    ____   __    ____ ______   ____
    //  / _ \  / __/  / /   / __//_  __/  / __/
    // / // / / _/   / /__ / _/   / /    / _/
    ///____/ /___/  /____//___/  /_/    /___/
    ///////////////////////////////////////////

    @DeleteMapping("/delete/{url}")
    public Mono<InfoResponse> deleteByUrl(@PathVariable String url){
        return invitationService.deleteInvitationByUrl(url)
                .thenReturn(new InfoResponse(HttpStatus.OK,"Success delete"))
                .onErrorReturn(new InfoResponse(HttpStatus.INTERNAL_SERVER_ERROR,"Delete is not success"));
    }
}
