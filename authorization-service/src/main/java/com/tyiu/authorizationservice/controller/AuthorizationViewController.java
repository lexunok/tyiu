package com.tyiu.authorizationservice.controller;

import com.tyiu.authorizationservice.model.entity.User;
import com.tyiu.authorizationservice.model.request.PasswordChangeRequest;
import com.tyiu.authorizationservice.service.AccountService;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.web.authentication.logout.SecurityContextLogoutHandler;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.servlet.ModelAndView;


@Controller
@RequestMapping("/auth")
@RequiredArgsConstructor
public class AuthorizationViewController {
    private final AccountService accountService;

    @GetMapping("/login")
    public String login() {
        return "login";
    }

    @GetMapping("/logout")
    public String logout(HttpServletRequest request, HttpServletResponse response) {
        String url = request.getParameter("url");
        Authentication auth = SecurityContextHolder.getContext().getAuthentication();
        if (auth != null){
            new SecurityContextLogoutHandler().logout(request, response, auth);
            request.getSession().invalidate();
        }
        return "redirect:" + url;
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
