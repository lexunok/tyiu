package com.tyiu.corn.controller;

import com.tyiu.corn.config.exception.NotFoundException;
import com.tyiu.corn.model.dto.UserDTO;
import com.tyiu.corn.model.responses.InfoResponse;
import org.springframework.http.HttpStatus;
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
        return accountChangeService.findByUrl(url)
                .switchIfEmpty(Mono.error(new NotFoundException("Not found!")));
    }

    @GetMapping("/change/email/{url}")
    public Mono<ChangeResponse> changeNewEmail(@PathVariable String url){
        return accountChangeService.findByUrlAndSendCode(url)
                .switchIfEmpty(Mono.error(new NotFoundException("Not found!")));
    }

    @GetMapping("/get/users")
    public Flux<UserInfoResponse> getUsersInfo(){
        return accountChangeService.getUsersInfo()
                .switchIfEmpty(Mono.error(new NotFoundException("Not found!")));
    }

    @GetMapping("/get/emails")
    public Mono<List<String>> getUsersEmail(){
        return accountChangeService.getAllEmails()
                .switchIfEmpty(Mono.error(new NotFoundException("Not found!")));
    }

    //////////////////////////////
    //   ___   ____    ____ ______
    //  / _ \ / __ \  / __//_  __/
    // / ___// /_/ / _\ \   / /
    ///_/    \____/ /___/  /_/
    //////////////////////////////

    @PostMapping("/send/email")
    public Mono<InfoResponse> invitationSend(@RequestBody Temporary invitation){
        return accountChangeService.sendInvitation(invitation)
                .thenReturn(new InfoResponse(HttpStatus.OK,"Успешное приглашение"))
                .onErrorReturn(new InfoResponse(HttpStatus.BAD_REQUEST,"Ошибка при приглашении"));
    }

    @PostMapping("/send/emails")
    public Flux<Void> invitationFileSend(@RequestBody InvitationDTO invitationDTO){
        return accountChangeService.sendInvitations(invitationDTO);
    }

    @PostMapping("/send/change/email")
    public Mono<InfoResponse> requestToChangeEmail(@RequestBody Temporary changeEmail){
        return accountChangeService.sendEmailToChangeEmail(changeEmail)
                .thenReturn(new InfoResponse(HttpStatus.OK,"Ссылка на изменение почты находится на новой почте"))
                .onErrorReturn(new InfoResponse(HttpStatus.BAD_REQUEST,"Не удалось отправить ссылку на новую почту"));
    }

    @PostMapping("/send/change/password")
    public Mono<String> requestToChangePassword(@RequestBody Temporary changePassword) {
        return accountChangeService.sendEmailToChangePassword(changePassword)
                .switchIfEmpty(Mono.error(new NotFoundException("Не удалось отправить ссылку на почту")));
    }

    ///////////////////////////////////////////
    //   ___    ____   __    ____ ______   ____
    //  / _ \  / __/  / /   / __//_  __/  / __/
    // / // / / _/   / /__ / _/   / /    / _/
    ///____/ /___/  /____//___/  /_/    /___/
    ///////////////////////////////////////////

    @DeleteMapping("/delete/invitation/{url}")
    public Mono<InfoResponse> deleteByUrl(@PathVariable String url){
        return accountChangeService.deleteDataByUrl(url)
                .thenReturn(new InfoResponse(HttpStatus.OK,"Success delete"))
                .onErrorReturn(new InfoResponse(HttpStatus.BAD_REQUEST,"Delete is not success"));
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
                .thenReturn(new InfoResponse(HttpStatus.OK,"Успешное изменение пароля"))
                .onErrorReturn(new InfoResponse(HttpStatus.BAD_REQUEST,"Не удалось изменить пароль"));
    }

    @PutMapping("/change/email")
    public Mono<InfoResponse> changeEmailByUser(@RequestBody ChangeRequest request){
        return accountChangeService.changeEmailByUser(request)
                .thenReturn(new InfoResponse(HttpStatus.OK,"Успешное изменение почты"))
                .onErrorReturn(new InfoResponse(HttpStatus.BAD_REQUEST,"Не удалось изменить почту"));
    }

    @PutMapping("/change/info")
    public Mono<UserDTO> changeUserInfoByAdmin(@RequestBody UserDTO user){
        return accountChangeService.changeUserInfo(user)
                .switchIfEmpty(Mono.error(new NotFoundException("Не удалось изменить пользователя")));
    }


}