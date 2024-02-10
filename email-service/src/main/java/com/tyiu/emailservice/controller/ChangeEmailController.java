package com.tyiu.emailservice.controller;


import com.tyiu.emailservice.service.ChangeEmailService;
import com.tyiu.ideas.config.exception.CustomHttpException;
import com.tyiu.emailservice.model.responses.ChangeResponse;
import com.tyiu.emailservice.model.dto.ChangeEmailDataDTO;
import com.tyiu.ideas.model.entities.User;
import com.tyiu.emailservice.model.requests.ChangeRequest;
import com.tyiu.ideas.model.responses.InfoResponse;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;
import reactor.core.publisher.Mono;

@RestController
@RequestMapping("/api/v1/email-service")
@RequiredArgsConstructor
public class ChangeEmailController {
    private final ChangeEmailService changeEmailService;

    ///////////////////////
    //  _____   ____ ______
    // / ___/  / __//_  __/
    /// (_ /  / _/   / /
    //\___/  /___/  /_/
    ///////////////////////

    @GetMapping("/change/email/{url}")
    public Mono<ChangeResponse> changeNewEmail(@PathVariable String url){
        return changeEmailService.findByUrlAndSendCode(url)
                .switchIfEmpty(Mono.error(new CustomHttpException("Not found!", HttpStatus.NOT_FOUND.value())));
    }

    //////////////////////////////
    //   ___   ____    ____ ______
    //  / _ \ / __ \  / __//_  __/
    // / ___// /_/ / _\ \   / /
    ///_/    \____/ /___/  /_/
    //////////////////////////////

    @PostMapping("/send/change/email")
    public Mono<InfoResponse> requestToChangeEmail(@RequestBody ChangeEmailDataDTO changeEmail, @AuthenticationPrincipal User user){
        return changeEmailService.sendEmailToChangeEmail(changeEmail, user.getEmail())
                .thenReturn(new InfoResponse(HttpStatus.OK,"Ссылка на изменение почты находится на новой почте"))
                .onErrorReturn(new InfoResponse(HttpStatus.INTERNAL_SERVER_ERROR,"Не удалось отправить ссылку на новую почту"));
    }

    ////////////////////////
    //   ___   __  __ ______
    //  / _ \ / / / //_  __/
    // / ___// /_/ /  / /
    ///_/    \____/  /_/
    ////////////////////////

    @PutMapping("/change/email")
    public Mono<InfoResponse> changeEmailByUser(@RequestBody ChangeRequest request){
        return changeEmailService.changeEmailByUser(request)
                .thenReturn(new InfoResponse(HttpStatus.OK,"Успешное изменение почты"));
    }
}
