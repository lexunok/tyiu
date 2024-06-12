package com.tyiu.authorizationservice.controller;

import com.nimbusds.jose.shaded.gson.Gson;
import com.tyiu.authorizationservice.service.AccountService;
import com.tyiu.client.models.UserDTO;
import lombok.RequiredArgsConstructor;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.security.oauth2.jwt.Jwt;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api/v1/authorization-service/account")
@RequiredArgsConstructor
public class AccountController {

    private final AccountService service;

    @PostMapping("/code/change/email/{newEmail}")
    public void requestToChangeEmail(@PathVariable String newEmail, @AuthenticationPrincipal Jwt jwt){
        Gson gson = new Gson();
        UserDTO user = gson.fromJson(jwt.getClaim("user").toString(), UserDTO.class);
        service.sendCodeToChangeEmail(newEmail, user.getEmail());
    }
    @PutMapping("/change/email/{code}")
    public void changeEmailByUser(@PathVariable String code, @AuthenticationPrincipal Jwt jwt){
        Gson gson = new Gson();
        UserDTO user = gson.fromJson(jwt.getClaim("user").toString(), UserDTO.class);
        service.changeEmailByUser(code, user.getEmail());
    }

    @PreAuthorize("hasRole('ADMIN')")
    @PostMapping("/register")
    public void register(@RequestBody UserDTO userDTO){
        service.registerForAdmin(userDTO);
    }

}
