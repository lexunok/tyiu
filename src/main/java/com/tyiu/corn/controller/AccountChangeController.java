package com.tyiu.corn.controller;

import com.tyiu.corn.model.dto.UserDTO;
import org.springframework.web.bind.annotation.*;

import lombok.RequiredArgsConstructor;

import com.tyiu.corn.model.entities.Temporary;
import com.tyiu.corn.model.requests.ChangeRequest;
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

    ///////////////////////
    //  _____   ____ ______
    // / ___/  / __//_  __/
    /// (_ /  / _/   / /
    //\___/  /___/  /_/
    ///////////////////////

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

    //////////////////////////////
    //   ___   ____    ____ ______
    //  / _ \ / __ \  / __//_  __/
    // / ___// /_/ / _\ \   / /
    ///_/    \____/ /___/  /_/
    //////////////////////////////

    @PostMapping("/send/email")
    public Mono<Void> invitationSend(@RequestBody Temporary invitation){
        return accountChangeService.sendInvitation(invitation);
    }

    @PostMapping("/send/emails")
    public Flux<Void> invitationFileSend(@RequestBody InvitationDTO invitationDTO){
        return accountChangeService.sendInvitations(invitationDTO);
    }

    @PostMapping("/send/change/email")
    public Mono<Void> requestToChangeEmail(@RequestBody Temporary changeEmail){
        return accountChangeService.sendEmailToChangeEmail(changeEmail);
    }

    @PostMapping("/send/change/password")
    public Mono<String> requestToChangePassword(@RequestBody Temporary changePassword) {
        return accountChangeService.sendEmailToChangePassword(changePassword);
    }

    ///////////////////////////////////////////
    //   ___    ____   __    ____ ______   ____
    //  / _ \  / __/  / /   / __//_  __/  / __/
    // / // / / _/   / /__ / _/   / /    / _/
    ///____/ /___/  /____//___/  /_/    /___/
    ///////////////////////////////////////////

    @DeleteMapping("/delete/invitation/{url}")
    public Mono<Void> deleteByUrl(@PathVariable String url){
        return accountChangeService.deleteDataByUrl(url);
    }

    ////////////////////////
    //   ___   __  __ ______
    //  / _ \ / / / //_  __/
    // / ___// /_/ /  / /
    ///_/    \____/  /_/
    ////////////////////////

    @PutMapping("/change/password")
    public Mono<Void> changePasswordByUser(@RequestBody ChangeRequest request){
        return accountChangeService.changePasswordByUser(request);
    }

    @PutMapping("/change/email")
    public Mono<Void> changeEmailByUser(@RequestBody ChangeRequest request){
        return accountChangeService.changeEmailByUser(request);
    }

    @PutMapping("/change/info")
    public Mono<UserDTO> changeUserInfoByAdmin(@RequestBody UserDTO user){
        return accountChangeService.changeUserInfo(user);
    }


}