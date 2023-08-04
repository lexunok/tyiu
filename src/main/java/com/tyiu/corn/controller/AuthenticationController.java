package com.tyiu.corn.controller;

import com.tyiu.corn.model.requests.LoginRequest;
import com.tyiu.corn.model.requests.RegisterRequest;
import com.tyiu.corn.model.responses.AuthenticationResponse;
import com.tyiu.corn.service.AuthenticationService;
import com.tyiu.corn.util.security.UserService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.security.core.Authentication;
import org.springframework.web.bind.annotation.*;

import java.security.Principal;


@Slf4j
@RequiredArgsConstructor
@RequestMapping("/api/v1/auth")
@RestController
public class AuthenticationController {
    private final AuthenticationService authenticationService;
    @GetMapping("/user")
    public String getUser(Principal principal){
        return principal.getName();
    }
    @PostMapping("/login")
    public AuthenticationResponse signIn(@RequestBody LoginRequest request){
        return authenticationService.login(request);
    }
    @PostMapping("/register")
    public AuthenticationResponse signUp(@RequestBody RegisterRequest request){
        return authenticationService.register(request);
    }
  }
