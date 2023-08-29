package com.tyiu.corn.controller;

import com.tyiu.corn.model.requests.LoginRequest;
import com.tyiu.corn.model.requests.RegisterRequest;
import com.tyiu.corn.model.responses.AuthenticationResponse;
import com.tyiu.corn.service.AuthenticationService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import reactor.core.publisher.Mono;

import org.springframework.web.bind.annotation.*;


@RestController
@RequestMapping("/api/v1/auth")
@RequiredArgsConstructor
@Slf4j
public class AuthenticationController {
    /*private final AuthenticationService authenticationService;

    @PostMapping("/login")
    public Mono<AuthenticationResponse> signIn(@RequestBody LoginRequest request) {
        return Mono.fromCallable(() -> authenticationService.login(request));
    }

    @PostMapping("/register")
    public Mono<Mono<AuthenticationResponse>> signUp(@RequestBody RegisterRequest request) {
        return Mono.fromCallable(() -> authenticationService.register(request));
    }*/
}
