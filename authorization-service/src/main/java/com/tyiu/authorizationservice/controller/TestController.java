package com.tyiu.authorizationservice.controller;

import com.tyiu.authorizationservice.model.entities.UserEntity;
import com.tyiu.authorizationservice.repository.UserRepository;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
public class TestController {

    private final UserRepository userRepository;

    public TestController(UserRepository userRepository) {
        this.userRepository = userRepository;
    }

    @GetMapping("/test")
    public String test() {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        return authentication.getName();
    }

    @PostMapping("/register")
    public void reg(){
        userRepository.save(UserEntity.builder()
                .email("admin")
                .passwordHash("password")
                .build());
    }
}
