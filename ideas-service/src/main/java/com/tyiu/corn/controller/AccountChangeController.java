package com.tyiu.corn.controller;

import com.tyiu.corn.config.exception.CustomHttpException;
import com.tyiu.corn.model.dto.*;
import com.tyiu.corn.model.entities.User;
import com.tyiu.corn.model.responses.InfoResponse;
import org.springframework.http.HttpStatus;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;

import lombok.RequiredArgsConstructor;

import com.tyiu.corn.model.requests.ChangeRequest;
import com.tyiu.corn.model.responses.ChangeResponse;
import com.tyiu.corn.model.responses.InvitationResponse;
import com.tyiu.corn.service.AccountChangeService;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.time.Duration;

@RestController
@RequestMapping("/api/v1/profile")
@RequiredArgsConstructor
public class AccountChangeController {
    private final AccountChangeService accountChangeService;

    ///////////////////////
    //  _____   ____ ______
    // / ___/  / __//_  __/
    /// (_ /  / _/   / /
    //\___/  /___/  /_/
    ///////////////////////

    @GetMapping("/get/invitation/{url}")
    public Mono<InvitationResponse> registerByInvitation(@PathVariable String url){
        return accountChangeService.findInvitationByUrl(url)
                .switchIfEmpty(Mono.error(new CustomHttpException("Not found!", HttpStatus.NOT_FOUND.value())));
    }

    @GetMapping("/change/email/{url}")
    public Mono<ChangeResponse> changeNewEmail(@PathVariable String url){
        return accountChangeService.findByUrlAndSendCode(url)
                .switchIfEmpty(Mono.error(new CustomHttpException("Not found!", HttpStatus.NOT_FOUND.value())));
    }

    @GetMapping("/get/users")
    @PreAuthorize("hasAuthority('ADMIN')")
    public Flux<UserDTO> getUsersInfo(){
        return accountChangeService.getUsersInfo()
                .switchIfEmpty(Mono.error(new CustomHttpException("Not found!", HttpStatus.NOT_FOUND.value())));
    }

    @GetMapping("/get/emails")
    @PreAuthorize("hasAuthority('ADMIN')")
    public Flux<String> getUsersEmail(){
        return accountChangeService.getAllEmails()
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
        return accountChangeService.sendInvitation(invitationDTO, user)
                .thenReturn(new InfoResponse(HttpStatus.OK,"Успешное приглашение"))
                .onErrorReturn(new InfoResponse(HttpStatus.INTERNAL_SERVER_ERROR,"Ошибка при приглашении"));
    }

    @PostMapping("/send/emails")
    @PreAuthorize("hasAuthority('ADMIN')")
    public Mono<Void> invitationFileSend(@RequestBody InvitationsDTO invitationDTO, @AuthenticationPrincipal User user){
        accountChangeService.sendInvitations(invitationDTO, user);
        return Mono.delay(Duration.ofSeconds(5)).then();
    }

    @PostMapping("/send/change/email")
    public Mono<InfoResponse> requestToChangeEmail(@RequestBody ChangeEmailDataDTO changeEmail, @AuthenticationPrincipal User user){
        return accountChangeService.sendEmailToChangeEmail(changeEmail, user.getEmail())
                .thenReturn(new InfoResponse(HttpStatus.OK,"Ссылка на изменение почты находится на новой почте"))
                .onErrorReturn(new InfoResponse(HttpStatus.INTERNAL_SERVER_ERROR,"Не удалось отправить ссылку на новую почту"));
    }

    @PostMapping("/send/change/password")
    public Mono<String> requestToChangePassword(@RequestBody ChangePasswordDataDTO changePassword) {
        return accountChangeService.sendEmailToChangePassword(changePassword)
                .switchIfEmpty(Mono.error(new CustomHttpException("Не удалось отправить ссылку на почту", HttpStatus.INTERNAL_SERVER_ERROR.value())));
    }

    ///////////////////////////////////////////
    //   ___    ____   __    ____ ______   ____
    //  / _ \  / __/  / /   / __//_  __/  / __/
    // / // / / _/   / /__ / _/   / /    / _/
    ///____/ /___/  /____//___/  /_/    /___/
    ///////////////////////////////////////////

    @DeleteMapping("/delete/invitation/{url}")
    public Mono<InfoResponse> deleteByUrl(@PathVariable String url){
        return accountChangeService.deleteInvitationByUrl(url)
                .thenReturn(new InfoResponse(HttpStatus.OK,"Success delete"))
                .onErrorReturn(new InfoResponse(HttpStatus.INTERNAL_SERVER_ERROR,"Delete is not success"));
    }

    ////////////////////////
    //   ___   __  __ ______
    //  / _ \ / / / //_  __/
    // / ___// /_/ /  / /
    ///_/    \____/  /_/
    ////////////////////////

    @PutMapping("/change/password")
    public Mono<InfoResponse> changePasswordByUser(@RequestBody ChangeRequest request){
        return accountChangeService.changePasswordByUser(request)
                .thenReturn(new InfoResponse(HttpStatus.OK,"Успешное изменение пароля"));
    }

    @PutMapping("/change/email")
    public Mono<InfoResponse> changeEmailByUser(@RequestBody ChangeRequest request){
        return accountChangeService.changeEmailByUser(request)
                .thenReturn(new InfoResponse(HttpStatus.OK,"Успешное изменение почты"));
    }

    @PutMapping("/change/info")
    @PreAuthorize("hasAuthority('ADMIN')")
    public Mono<UserDTO> changeUserInfoByAdmin(@RequestBody UserDTO user){
        return accountChangeService.changeUserInfo(user)
                .switchIfEmpty(Mono.error(new CustomHttpException("Не удалось изменить пользователя", HttpStatus.INTERNAL_SERVER_ERROR.value())));
    }


}