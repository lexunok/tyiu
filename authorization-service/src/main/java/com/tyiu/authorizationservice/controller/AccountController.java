package com.tyiu.authorizationservice.controller;

import com.tyiu.authorizationservice.model.entity.User;
import com.tyiu.authorizationservice.service.AccountService;
import com.tyiu.client.models.UserDTO;
import lombok.RequiredArgsConstructor;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api/v1/authorization-service/account")
@RequiredArgsConstructor
public class AccountController {

    private final AccountService service;

    @PostMapping("/code/change/email/{newEmail}")
    public void requestToChangeEmail(@PathVariable String newEmail, @AuthenticationPrincipal User user){
        service.sendCodeToChangeEmail(newEmail, user.getEmail());
    }
    @PutMapping("/change/email/{code}")
    public void changeEmailByUser(@PathVariable String code, @AuthenticationPrincipal User user){
        service.changeEmailByUser(code, user.getEmail());
    }

    @PreAuthorize("hasAuthority('ADMIN')")
    @PostMapping("/register")
    public void register(@RequestBody UserDTO userDTO){
        service.registerForAdmin(userDTO);
    }

}
