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

@RestController
@RequestMapping("/api/v1/profile")
@RequiredArgsConstructor
public class AccountChangeController {
    private final AccountChangeService accountChangeService;

    @GetMapping("/get/invitation/{url}")
    public Mono<InvitationResponse> registrateByInvitation(@PathVariable String url){
        return accountChangeService.findByUrl(url);
    }

    @GetMapping("/change/email/{url}")
    public Mono<ChangeResponse> changeNewEmail(@PathVariable String url){
        return accountChangeService.findByUrlAndSendCode(url);
    }

    @GetMapping("/get/info")
    public Flux<UserInfoResponse> getUsersInfo(){
        return accountChangeService.getUsersInfo();
    }

    @GetMapping("/get/email")
    public Flux<String> getUsersEmail(){
        return accountChangeService.getAllEmails();
    }

    @PostMapping("/send/email")
    public Mono<ResponseEntity<String>> invitationSend(@RequestBody Temporary invitation){
        accountChangeService.sendInvitation(invitation);
        return Mono.just(new ResponseEntity<>("Успешное приглашение", HttpStatus.OK));
    }

    @PostMapping("/send/email/file")
    public Mono<ResponseEntity<String>> invitationFileSend(@RequestBody InvitationDTO invitationDTO){
        accountChangeService.sendInvitations(invitationDTO);
        return Mono.just(new ResponseEntity<>("Успешное приглашение", HttpStatus.OK));
    }

//  @PostMapping("/send/request-to-change-email")
    @PostMapping("/send/change/email")
    public Mono<ResponseEntity<String>> requestToChangeEmail(@RequestBody Temporary changeEmail){
        accountChangeService.sendEmailToChangeEmail(changeEmail);
        return Mono.just(new ResponseEntity<>("Ссылка на изменение почты находится на новой почте", HttpStatus.OK));
    }

    @PostMapping("/send/change/password")
    public Mono<String> requestToChangePassword(@RequestBody Temporary changePassword) {
        return accountChangeService.sendEmailToChangePassword(changePassword);
    }

    @DeleteMapping("/delete/invitation/{url}")
    public Mono<Void> DeleteByUrl(@PathVariable String url){
        accountChangeService.deleteDataByUrl(url);
        return Mono.empty();
    }

    @PutMapping("/change/password")
    public Mono<ResponseEntity<String>> changePasswordByUser(@RequestBody ChangeRequest request){
        accountChangeService.changePasswordByUser(request);
        return Mono.just(new ResponseEntity<>("Успешное изменение пароля", HttpStatus.OK));
    }

    @PutMapping("/change/email")
    public Mono<ResponseEntity<String>> changeEmailByUser(@RequestBody ChangeRequest request){
        accountChangeService.changeEmailByUser(request);
        return Mono.just(new ResponseEntity<>("Успешное изменение почты", HttpStatus.OK));
    }

    @PutMapping("/change/info")
    public Mono<ResponseEntity<String>> changeUserInfoByAdmin(@RequestBody UserInfoRequest request){
        accountChangeService.changeUserInfo(request);
        return Mono.just(new ResponseEntity<>("Успешное изменение пользователя", HttpStatus.OK));
    }


}