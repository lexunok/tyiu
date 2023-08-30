package com.tyiu.corn.controller;

import java.util.List;
import java.util.Map;

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
//@RequestMapping("/api/v1/profile-action")
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

    @GetMapping("/get/users-info")
    public Flux<Map<String, List<UserInfoResponse>>> getUsersInfo(){
        return Map.of("users", accountChangeService.getUsersInfo());
    }

    @GetMapping("/get/users-email")
    public Flux<Map<String, List<String>>> getUsersEmail(){
        return Map.of("emails", accountChangeService.getAllEmails());
    }

    @PostMapping("/send/email")
    public Mono<Map<String, String>> invitationSend(@RequestBody Temporary invitation){
        accountChangeService.sendInvitation(invitation);
        return new Mono<ResponseEntity>(Map.of("success","Успешное приглашение"), HttpStatus.OK);
    }

    @PostMapping("/send/emails")
    public Mono<Map<String, String>> invitationFileSend(@RequestBody InvitationDTO invitationDTO){
        accountChangeService.sendInvitations(invitationDTO);
        return new Mono<ResponseEntity>(Map.of("success", "Успешное приглашение"), HttpStatus.OK);
    }

    @PostMapping("/send/request-to-change-email")
    public Mono<Map<String, String>> requestToChangeEmail(@RequestBody Temporary changeEmail){
        accountChangeService.sendEmailToChangeEmail(changeEmail);
        return new Mono<ResponseEntity>(Map.of("success", "Ссылка на изменение почты находится на новой почте"), HttpStatus.OK);
    }

    @PostMapping("/send/request-to-change-password")
    public Mono<Map<String, String>> requestToChangePassword(@RequestBody Temporary changePassword) {
        return Map.of("key", accountChangeService.sendEmailToChangePassword(changePassword));
    }

    @DeleteMapping("/delete/invitation/{url}")
    public Mono<Void> DeleteByUrl(@PathVariable String url){
        accountChangeService.deleteDataByUrl(url);
        return Mono.empty();
    }

    @PutMapping("/change/password")
    public Mono<Map<String, String>> changePasswordByUser(@RequestBody ChangeRequest request){
        accountChangeService.changePasswordByUser(request);
        return new Mono<ResponseEntity>(Map.of("success", "Успешное изменение пароля"), HttpStatus.OK);
    }

    @PutMapping("/change/email")
    public Mono<Map<String, String>> changeEmailByUser(@RequestBody ChangeRequest request){
        accountChangeService.changeEmailByUser(request);
        return new Mono<ResponseEntity>(Map.of("success", "Успешное изменение почты"), HttpStatus.OK);
    }

    @PutMapping("/change/user-info")
    public Mono<Map<String, String>> changeUserInfoByAdmin(@RequestBody UserInfoRequest request){
        accountChangeService.changeUserInfo(request);
        return new Mono<ResponseEntity>(Map.of("success", "Успешное изменение пользователя"), HttpStatus.OK);
    }


}