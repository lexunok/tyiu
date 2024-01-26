package com.tyiu.ideas.controller;

import com.tyiu.ideas.model.requests.LoginRequest;
import com.tyiu.ideas.model.requests.RegisterRequest;
import com.tyiu.ideas.model.responses.AuthenticationResponse;
import com.tyiu.ideas.service.AuthenticationService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import reactor.core.publisher.Mono;

import org.springframework.web.bind.annotation.*;


@RestController
@RequestMapping("/api/v1/ideas-service/auth")
@RequiredArgsConstructor
@Slf4j
public class AuthenticationController {

    private final AuthenticationService authenticationService;

    @PostMapping("/login")
    public Mono<AuthenticationResponse> signIn(@RequestBody LoginRequest request) {
        log.info("/login by " + request.getEmail());
        return authenticationService.login(request);
    }

    @PostMapping("/register")
    public Mono<AuthenticationResponse> signUp(@RequestBody RegisterRequest request) {
        log.info("/register by " + request.getEmail());
        return authenticationService.register(request);
    }
}
