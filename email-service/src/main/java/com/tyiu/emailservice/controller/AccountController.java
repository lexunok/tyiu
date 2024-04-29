package com.tyiu.emailservice.controller;

import com.tyiu.emailservice.service.AccountService;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;
import reactor.core.publisher.Mono;

@RestController
@RequestMapping("/api/v1/email-service")
@RequiredArgsConstructor
public class AccountController {

    private final AccountService accountService;

    @PostMapping("/account/password/{email}/code/{code}")
    public Mono<Void> requestToChangePassword(@PathVariable String email, @PathVariable String code) {
        return accountService.sendEmailToChangePassword(email, code);
    }

}
