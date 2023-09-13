package com.tyiu.corn.controller;

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import lombok.RequiredArgsConstructor;

import com.tyiu.corn.model.entities.Temporary;
import com.tyiu.corn.model.requests.ChangeRequest;
import com.tyiu.corn.model.requests.UserInfoRequest;
import com.tyiu.corn.model.responses.ChangeResponse;
import com.tyiu.corn.model.responses.InvitationResponse;
import com.tyiu.corn.model.responses.UserInfoResponse;
import com.tyiu.corn.service.AccountChangeService;
import com.tyiu.corn.model.dto.InvitationDTO;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.util.List;

@RestController
@RequestMapping("/api/v1/profile")
@RequiredArgsConstructor
public class AccountChangeController {
    private final AccountChangeService accountChangeService;

    @GetMapping("/get/invitation/{url}")
    public Mono<InvitationResponse> registerByInvitation(@PathVariable String url){
        return accountChangeService.findByUrl(url);
    }

    @GetMapping("/change/email/{url}")
    public Mono<ChangeResponse> changeNewEmail(@PathVariable String url){
        return accountChangeService.findByUrlAndSendCode(url);
    }

    @GetMapping("/get/users")
    public Flux<UserInfoResponse> getUsersInfo(){
        return accountChangeService.getUsersInfo();
    }

    @GetMapping("/get/emails")
    public Mono<List<String>> getUsersEmail(){
        return accountChangeService.getAllEmails();
    }

    @PostMapping("/send/email")
    public Mono<Void> invitationSend(@RequestBody Temporary invitation){
        return accountChangeService.sendInvitation(invitation);
        //return Mono.just(new ResponseEntity<>("Успешное приглашение", HttpStatus.OK));
    }

    @PostMapping("/send/emails")
    public Flux<Void> invitationFileSend(@RequestBody InvitationDTO invitationDTO){
        return accountChangeService.sendInvitations(invitationDTO);
        //return Mono.just(new ResponseEntity<>("Успешное приглашение", HttpStatus.OK));
    }

    @PostMapping("/send/change/email")
    public Mono<Void> requestToChangeEmail(@RequestBody Temporary changeEmail){
        return accountChangeService.sendEmailToChangeEmail(changeEmail);
        //return Mono.just(new ResponseEntity<>("Ссылка на изменение почты находится на новой почте", HttpStatus.OK));
    }

    @PostMapping("/send/change/password")
    public Mono<String> requestToChangePassword(@RequestBody Temporary changePassword) {
        return accountChangeService.sendEmailToChangePassword(changePassword);
    }

    @DeleteMapping("/delete/invitation/{url}")
    public Mono<Void> deleteByUrl(@PathVariable String url){
        accountChangeService.deleteDataByUrl(url);
        return Mono.empty();
    }

    @PutMapping("/change/password")
    public Mono<Void> changePasswordByUser(@RequestBody ChangeRequest request){
        return accountChangeService.changePasswordByUser(request);
        //return Mono.just(new ResponseEntity<>("Успешное изменение пароля", HttpStatus.OK));
    }

    @PutMapping("/change/email")
    public Mono<Void> changeEmailByUser(@RequestBody ChangeRequest request){
        return accountChangeService.changeEmailByUser(request);
        //return Mono.just(new ResponseEntity<>("Успешное изменение почты", HttpStatus.OK));
    }

    @PutMapping("/change/info")
    public Mono<Void> changeUserInfoByAdmin(@RequestBody UserInfoRequest request){
        return accountChangeService.changeUserInfo(request);
        //return Mono.just(new ResponseEntity<>("Успешное изменение пользователя", HttpStatus.OK));
    }


}