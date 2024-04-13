package com.tyiu.authorizationservice.controllers;

import com.tyiu.authorizationservice.models.User;
import com.tyiu.authorizationservice.services.AccountService;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestParam;

@Controller
@RequiredArgsConstructor
public class AuthorizationViewController {
    private final AccountService accountService;

    @GetMapping("/login")
    public String login() {
        return "login";
    }

    @GetMapping("/registration")
    public String showRegistration(@RequestParam(name = "code") String code, Model model) {
        return accountService.showRegistration(code, model);
    }

    @PostMapping("/registration")
    public String register(@RequestParam(name = "code") String code, User user) {
        return accountService.register(code, user);
    }

    @GetMapping("/recovery-password")
    public String showRecoveryPasswordForm() {
        return "recovery-password";
    }

    @PostMapping("/recovery-password")
    public String recoveryPassword(@RequestParam String email) {
        return accountService.generateAndSendCode(email);
    }

    @GetMapping("/new-password")
    public String showNewPasswordForm() {
        return "new-password";
    }

    @PostMapping("/new-password")
    public String newPassword(@RequestParam String code, @RequestParam String password) {
        return "login";
    }
}
