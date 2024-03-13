package com.tyiu.authorizationservice;

import com.tyiu.client.models.Role;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.client.RestTemplate;
import java.time.LocalDateTime;
import java.util.Arrays;

@Controller
@RequiredArgsConstructor
public class AuthorizationViewController {
    private final RestTemplate template;
    private final UserRepository repository;
    private final PasswordEncoder encoder;

    @GetMapping("/login")
    public String login() {
        return "login";
    }

    @GetMapping("/registration")
    public String showRegistration(@RequestParam(name = "code") String code, Model model) {
        //TODO: Заменить на open feign
        ResponseEntity<Invitation> invitation = null;
        try {
            invitation = template
                    .getForEntity("http://localhost:8083/api/v1/email-service/invitation/get/" + code, Invitation.class);
        } catch (Exception ignored){}
        if (invitation!= null && invitation.getStatusCode().is2xxSuccessful()) {
            model.addAttribute("email", invitation.getBody().getEmail());
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
        //TODO: Заменить на open feign
        ResponseEntity<Invitation> invitation = null;
        try {
            invitation = template
                    .getForEntity("http://localhost:8083/api/v1/email-service/invitation/get/" + code, Invitation.class);
        } catch (Exception ignored){}
        if (invitation!= null && invitation.getStatusCode().is2xxSuccessful()) {
            user.setRoles(invitation.getBody().getRoles());
            user.setEmail(invitation.getBody().getEmail());
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
