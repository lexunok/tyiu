package com.tyiu.authorizationservice.controller;

import com.tyiu.authorizationservice.model.entity.User;
import com.tyiu.authorizationservice.model.request.PasswordChangeRequest;
import com.tyiu.authorizationservice.service.AccountService;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.servlet.ModelAndView;

@Controller
@RequestMapping("/auth")
@RequiredArgsConstructor
public class AuthorizationViewController {
    private final AccountService accountService;

    //TODO: Сделать при логине и других связанных с email to lowercase
    @GetMapping("/login")
    public String login() {
        return "login";
    }

    @GetMapping("/success")
    @ResponseBody
    public String success(@CookieValue(name = "JSESSIONID") String session){
        return session;
    }

    @GetMapping("/registration")
    public String showRegistration(@RequestParam(name = "code") String code, Model model) {
        return accountService.showRegistration(code, model);
    }

    @PostMapping("/registration")
    public String register(@RequestParam(name = "code") String code, @Valid User user) {
        return accountService.register(code, user);
    }

    @GetMapping("/recovery-password")
    public String showRecoveryPasswordForm() {
        return "recovery-password";
    }

    @PostMapping("/recovery-password")
    public ModelAndView recoveryPassword(@RequestParam String email) {
        return accountService.generateAndSendCode(email);
    }

    @PostMapping("/new-password")
    public String newPassword(PasswordChangeRequest request) {
        return accountService.changePassword(request);
    }
}
