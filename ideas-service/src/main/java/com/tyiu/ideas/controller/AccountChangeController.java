package com.tyiu.ideas.controller;

import com.tyiu.ideas.config.exception.CustomHttpException;
import com.tyiu.ideas.model.dto.*;
import com.tyiu.ideas.model.entities.User;
import com.tyiu.ideas.model.responses.InfoResponse;
import org.springframework.http.HttpStatus;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;

import lombok.RequiredArgsConstructor;

import com.tyiu.ideas.model.requests.ChangeRequest;
import com.tyiu.ideas.model.responses.ChangeResponse;
import com.tyiu.ideas.model.responses.InvitationResponse;
import com.tyiu.ideas.service.AccountChangeService;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.time.Duration;

@RestController
@RequestMapping("/api/v1/ideas-service/profile")
@RequiredArgsConstructor
public class AccountChangeController {

    private final AccountChangeService accountChangeService;


    //TODO: SNOS
    @GetMapping("/get/invitation/{url}")
    public Mono<InvitationResponse> registerByInvitation(@PathVariable String url){
        return accountChangeService.findInvitationByUrl(url)
                .switchIfEmpty(Mono.error(new CustomHttpException("Not found!", HttpStatus.NOT_FOUND.value())));
    }
    //TODO: SNOS
    @GetMapping("/change/email/{url}")
    public Mono<ChangeResponse> changeNewEmail(@PathVariable String url){
        return accountChangeService.findByUrlAndSendCode(url)
                .switchIfEmpty(Mono.error(new CustomHttpException("Not found!", HttpStatus.NOT_FOUND.value())));
    }
    //TODO: SNOS
    @GetMapping("/get/users")
    @PreAuthorize("hasAnyAuthority('ADMIN','TEACHER')")
    public Flux<UserDTO> getUsersInfo(){
        return accountChangeService.getUsersInfo()
                .switchIfEmpty(Mono.error(new CustomHttpException("Not found!", HttpStatus.NOT_FOUND.value())));
    }
    //TODO: SNOS
    @DeleteMapping("/delete/user/{userId}")
    @PreAuthorize("hasAuthority('ADMIN')")
    public Mono<Void> deleteUser(@PathVariable String userId){
        return accountChangeService.deleteUser(userId);
    }
    //TODO: SNOS
    @GetMapping("/get/emails")
    @PreAuthorize("hasAuthority('ADMIN')")
    public Flux<String> getUsersEmail(){
        return accountChangeService.getAllEmails()
                .switchIfEmpty(Mono.error(new CustomHttpException("Not found!", HttpStatus.NOT_FOUND.value())));
    }
    //TODO: SNOS
    @PostMapping("/send/email")
    @PreAuthorize("hasAuthority('ADMIN')")
    public Mono<InfoResponse> invitationSend(@RequestBody InvitationDTO invitationDTO, @AuthenticationPrincipal User user){
        return accountChangeService.sendInvitation(invitationDTO, user)
                .thenReturn(new InfoResponse(HttpStatus.OK,"Успешное приглашение"))
                .onErrorReturn(new InfoResponse(HttpStatus.INTERNAL_SERVER_ERROR,"Ошибка при приглашении"));
    }
    //TODO: SNOS
    @PostMapping("/send/emails")
    @PreAuthorize("hasAuthority('ADMIN')")
    public Mono<Void> invitationFileSend(@RequestBody InvitationsDTO invitationDTO, @AuthenticationPrincipal User user){
        accountChangeService.sendInvitations(invitationDTO, user);
        return Mono.delay(Duration.ofSeconds(5)).then();
    }
    //TODO: SNOS
    @PostMapping("/send/change/email")
    public Mono<InfoResponse> requestToChangeEmail(@RequestBody ChangeEmailDataDTO changeEmail, @AuthenticationPrincipal User user){
        return accountChangeService.sendEmailToChangeEmail(changeEmail, user.getEmail())
                .thenReturn(new InfoResponse(HttpStatus.OK,"Ссылка на изменение почты находится на новой почте"))
                .onErrorReturn(new InfoResponse(HttpStatus.INTERNAL_SERVER_ERROR,"Не удалось отправить ссылку на новую почту"));
    }
    //TODO: SNOS
    @PostMapping("/send/change/password")
    public Mono<String> requestToChangePassword(@RequestBody ChangePasswordDataDTO changePassword) {
        return accountChangeService.sendEmailToChangePassword(changePassword)
                .switchIfEmpty(Mono.error(new CustomHttpException("Не удалось отправить ссылку на почту", HttpStatus.INTERNAL_SERVER_ERROR.value())));
    }
    //TODO: SNOS
    @DeleteMapping("/delete/invitation/{url}")
    public Mono<InfoResponse> deleteByUrl(@PathVariable String url){
        return accountChangeService.deleteInvitationByUrl(url)
                .thenReturn(new InfoResponse(HttpStatus.OK,"Success delete"))
                .onErrorReturn(new InfoResponse(HttpStatus.INTERNAL_SERVER_ERROR,"Delete is not success"));
    }
    //TODO: SNOS
    @PutMapping("/change/password")
    public Mono<InfoResponse> changePasswordByUser(@RequestBody ChangeRequest request){
        return accountChangeService.changePasswordByUser(request)
                .thenReturn(new InfoResponse(HttpStatus.OK,"Успешное изменение пароля"));
    }
    //TODO: SNOS
    @PutMapping("/change/email")
    public Mono<InfoResponse> changeEmailByUser(@RequestBody ChangeRequest request){
        return accountChangeService.changeEmailByUser(request)
                .thenReturn(new InfoResponse(HttpStatus.OK,"Успешное изменение почты"));
    }
    //TODO: SNOS
    @PutMapping("/change/info")
    @PreAuthorize("hasAuthority('ADMIN')")
    public Mono<UserDTO> changeUserInfoByAdmin(@RequestBody UserDTO user){
        return accountChangeService.changeUserInfo(user)
                .switchIfEmpty(Mono.error(new CustomHttpException("Не удалось изменить пользователя", HttpStatus.INTERNAL_SERVER_ERROR.value())));
    }


}