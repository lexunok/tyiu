package com.tyiu.authorizationservice;

import com.tyiu.client.connections.EmailClient;
import com.tyiu.client.connections.IdeasClient;
import com.tyiu.client.models.ChangePasswordRequest;
import com.tyiu.client.models.InvitationDTO;
import com.tyiu.client.models.Role;
import com.tyiu.client.models.UserDTO;
import lombok.RequiredArgsConstructor;
import org.modelmapper.ModelMapper;
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
    private final IdeasClient ideasClient;
    private final UserRepository repository;
    private final ModelMapper mapper = new ModelMapper();
    private final PasswordEncoder encoder;

    //TODO: Сделать обработку ошибок
    //TODO: Код и почту админа вынести в конфиг
    //TODO: Вынести логику в сервис

    @GetMapping("/login")
    public String login() {
        return "login";
    }

    @GetMapping("/registration")
    public String showRegistration(@RequestParam(name = "code") String code, Model model) {
        if (code.equals("admin")) {
            model.addAttribute("email", "admin@admin.com");
            model.addAttribute("user", new User());
            model.addAttribute("code", code);
            return "registration";
        }
        InvitationDTO invitation = emailClient.findInvitationById(code);
        if (invitation!=null) {
            model.addAttribute("email", invitation.getEmail());
            model.addAttribute("user", new User());
            model.addAttribute("code", code);
            return "registration";
        }
        return "login";
    }

    @PostMapping("/registration")
    public String register(@RequestParam(name = "code") String code, User user) {
        if (code.equals("admin")) {
            user.setRoles(Arrays.stream(Role.values()).toList());
            user.setEmail("admin@admin.com");
            user.setPassword(encoder.encode(user.getPassword()));
            user.setCreatedAt(LocalDateTime.now());
            repository.save(user);
            ideasClient.registerUserToIdeas(mapper.map(user, UserDTO.class));
        }
        //TODO: Добавить проверку на существует ли пользователь
        InvitationDTO invitation = emailClient.findInvitationById(code);
        if (invitation!=null) {
            user.setRoles(invitation.getRoles());
            user.setEmail(invitation.getEmail());
            user.setPassword(encoder.encode(user.getPassword()));
            user.setCreatedAt(LocalDateTime.now());
            repository.save(user);
            ideasClient.registerUserToIdeas(mapper.map(user, UserDTO.class));
        }
        return "redirect:/login";
    }

    @GetMapping("/recovery-password")
    public String showRecoveryPasswordForm() {
        return "recovery-password";
    }

    @PostMapping("/recovery-password")
    public String recoveryPassword(@RequestParam String email) {
        //emailClient.sendCodeToChangePassword(new ChangePasswordRequest(email));
        return "new-password";
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
