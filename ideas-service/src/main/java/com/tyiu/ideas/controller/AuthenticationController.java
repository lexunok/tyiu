package com.tyiu.ideas.controller;

import com.tyiu.ideas.model.requests.RegisterRequest;
import com.tyiu.ideas.service.AuthenticationService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import reactor.core.publisher.Mono;


@RestController
@RequestMapping("/api/v1/ideas-service/auth")
@RequiredArgsConstructor
@Slf4j
public class AuthenticationController {

    private final AuthenticationService authenticationService;

    @PostMapping("/register")
    public Mono<Void> signUp(@RequestBody RegisterRequest request) {
        log.info("/register by " + request.getEmail());
        return authenticationService.register(request);
    }
}
