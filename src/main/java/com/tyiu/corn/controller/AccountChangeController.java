package com.tyiu.corn.controller;

import java.util.List;
import java.util.Map;

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

@RestController
@RequestMapping("/api/v1/profile-action")
@RequiredArgsConstructor
public class AccountChangeController {
    private final AccountChangeService accountChangeService;

    @PostMapping("/send/email")
    public Map<String, String> invitationSend(@RequestBody Temporary invitation){
        accountChangeService.sendInvitation(invitation);
        return Map.of("success", "Успешное приглашение");
    }

    @PostMapping("/send/emails")
    public Map<String, String> invitationFileSend(@RequestBody InvitationDTO invitationDTO){
        accountChangeService.sendInvitations(invitationDTO);
        return Map.of("success", "Успешное приглашение");
    }

    @PostMapping("/send/request-to-change-email")
    public Map<String, String> requestToChangeEmail(@RequestBody Temporary changeEmail){
        accountChangeService.sendEmailToChangeEmail(changeEmail);
        return Map.of("success", "Ссылка на изменение почты находится на новой почте");
    }

    @PostMapping("/send/request-to-change-password")
    public Map<String, String> requestToChangePassword(@RequestBody Temporary changePassword){
        accountChangeService.sendEmailToChangePassword(changePassword);
        return Map.of("succes", String.format("Код отправлен на вашу почту %s", changePassword.getEmail()));
    }

    @PutMapping("/change/password")
    public Map<String, String> changePasswordByUser(@RequestBody ChangeRequest request){
        accountChangeService.changePasswordByUser(request);
        return Map.of("success", "Успешное изменение пароля");
    }

    @PutMapping("/change/email")
    public Map<String, String> changeEmailByUser(@RequestBody ChangeRequest request){
        accountChangeService.changeEmailByUser(request);
        return Map.of("success", "Успешное изменение почты");
    }

    @PutMapping("/change/userInfo")
    public Map<String, String> changeUserInfoByAdmin(@RequestBody UserInfoRequest request){
        accountChangeService.changeUserInfo(request);
        return Map.of("success", "Успешное изменение пользователя");
    }

    @GetMapping("/get/invitation/{url}")
    public InvitationResponse registrateByInvitation(@PathVariable String url){
        return accountChangeService.findByUrl(url);
    }

    @GetMapping("/change/email/{url}")
    public ChangeResponse changeNewEmail(@PathVariable String url){
        return accountChangeService.findByUrlAndSendCode(url);
    }
    
    @GetMapping("/get/users-info")
    public Map<String, List<UserInfoResponse>> getUsersInfo(){
        return Map.of("users",accountChangeService.getUsersInfo());
    }

    @DeleteMapping("/delete/invitation/{url}")
    public void DeleteByUrl(@PathVariable String url){
        accountChangeService.deleteDataByUrl(url);
    }  
}