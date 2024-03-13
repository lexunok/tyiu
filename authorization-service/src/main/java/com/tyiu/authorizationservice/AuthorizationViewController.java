package com.tyiu.authorizationservice;

import com.tyiu.client.connections.EmailClient;
import com.tyiu.client.models.InvitationDTO;
import com.tyiu.client.models.Role;
import lombok.RequiredArgsConstructor;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestParam;
import java.time.LocalDateTime;
import java.util.Arrays;

@Controller
@RequiredArgsConstructor
public class AuthorizationViewController {
    private final EmailClient emailClient;
    private final UserRepository repository;
    private final PasswordEncoder encoder;

    @GetMapping("/login")
    public String login() {
        return "login";
    }

    @GetMapping("/registration")
    public String showRegistration(@RequestParam(name = "code") String code, Model model) {
        InvitationDTO invitation = emailClient.findInvitationById(code);
        if (invitation!= null) {
            model.addAttribute("email", invitation.getEmail());
            model.addAttribute("user", new User());
            model.addAttribute("code", code);
            return "registration";
        }
        else if (code.equals("admin")) {
            model.addAttribute("email", "admin@admin.com");
            model.addAttribute("user", new User());
            model.addAttribute("code", code);
            return "registration";
        }
        return "login";
    }

    @PostMapping("/registration")
    public String register(@RequestParam(name = "code") String code, User user) {
        InvitationDTO invitation = emailClient.findInvitationById(code);
        if (invitation!= null) {
            user.setRoles(invitation.getRoles());
            user.setEmail(invitation.getEmail());
            user.setPassword(encoder.encode(user.getPassword()));
            user.setCreatedAt(LocalDateTime.now());
            repository.save(user);
        }
        else if (code.equals("admin")) {
            user.setRoles(Arrays.stream(Role.values()).toList());
            user.setEmail("admin@admin.com");
            user.setPassword(encoder.encode(user.getPassword()));
            user.setCreatedAt(LocalDateTime.now());
            repository.save(user);
        }
        return "redirect:/login";
    }
}
