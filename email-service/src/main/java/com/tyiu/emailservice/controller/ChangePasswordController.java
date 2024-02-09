package com.tyiu.emailservice.controller;

import com.tyiu.emailservice.service.ChangePasswordService;
import com.tyiu.ideas.config.exception.CustomHttpException;
import com.tyiu.emailservice.model.dto.ChangePasswordDataDTO;
import com.tyiu.emailservice.model.requests.ChangeRequest;
import com.tyiu.ideas.model.responses.InfoResponse;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.*;
import reactor.core.publisher.Mono;

@RestController
@RequestMapping("/api/v1/email-service")
@RequiredArgsConstructor
public class ChangePasswordController {
    private final ChangePasswordService changePasswordService;

    //////////////////////////////
    //   ___   ____    ____ ______
    //  / _ \ / __ \  / __//_  __/
    // / ___// /_/ / _\ \   / /
    ///_/    \____/ /___/  /_/
    //////////////////////////////

    @PostMapping("/send/change/password")
    public Mono<String> requestToChangePassword(@RequestBody ChangePasswordDataDTO changePassword) {
        return changePasswordService.sendEmailToChangePassword(changePassword)
                .switchIfEmpty(Mono.error(new CustomHttpException("Не удалось отправить ссылку на почту", HttpStatus.INTERNAL_SERVER_ERROR.value())));
    }

    ////////////////////////
    //   ___   __  __ ______
    //  / _ \ / / / //_  __/
    // / ___// /_/ /  / /
    ///_/    \____/  /_/
    ////////////////////////

    @PutMapping("/change/password")
    public Mono<InfoResponse> changePasswordByUser(@RequestBody ChangeRequest request){
        return changePasswordService.changePasswordByUser(request)
                .thenReturn(new InfoResponse(HttpStatus.OK,"Успешное изменение пароля"));
    }
}
