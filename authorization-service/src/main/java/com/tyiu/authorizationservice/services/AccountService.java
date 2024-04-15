package com.tyiu.authorizationservice.services;

import com.tyiu.authorizationservice.models.PasswordChangeRequest;
import com.tyiu.authorizationservice.models.PasswordChangeData;
import com.tyiu.authorizationservice.models.User;
import com.tyiu.authorizationservice.repositories.PasswordChangeDataRepository;
import com.tyiu.authorizationservice.repositories.UserRepository;
import com.tyiu.client.connections.EmailClient;
import com.tyiu.client.connections.IdeasClient;
import com.tyiu.client.models.InvitationDTO;
import com.tyiu.client.models.Role;
import com.tyiu.client.models.UserDTO;
import lombok.RequiredArgsConstructor;
import org.modelmapper.ModelMapper;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.ui.Model;
import org.springframework.web.servlet.ModelAndView;

import java.security.SecureRandom;
import java.time.LocalDateTime;
import java.util.Arrays;

@RequiredArgsConstructor
@Service
@EnableScheduling
public class AccountService {
    private final ModelMapper mapper;
    private final PasswordEncoder encoder;
    private final IdeasClient ideasClient;
    private final EmailClient emailClient;
    private final UserRepository userRepository;
    private final PasswordChangeDataRepository passwordChangeRepository;
    //TODO: Код и почту админа вынести в конфиг
    //TODO: Сделать обработку ошибок

    @Scheduled(fixedRate = 600000)
    void deleteExpiredData() {
        passwordChangeRepository.deleteByDateExpiredEquals(LocalDateTime.now());
    }

    public ModelAndView generateAndSendCode(String email) {
        Boolean isUserExists = userRepository.existsByEmail(email);
        if (Boolean.FALSE.equals(isUserExists)) {
            //TODO: Ошибка что пользователя не существует
        }
        Boolean isPasswordChangeExist = passwordChangeRepository.existsByEmail(email);
        String code = String.valueOf(new SecureRandom().nextInt(900000)+100000);
        PasswordChangeData passwordChange = PasswordChangeData.builder()
                .email(email)
                .code(encoder.encode(code))
                .dateExpired(LocalDateTime.now().plusMinutes(5))
                .build();
        if (Boolean.TRUE.equals(isPasswordChangeExist)) {
            passwordChangeRepository.deleteByEmail(email);
        }
        PasswordChangeData savedData = passwordChangeRepository.save(passwordChange);
        emailClient.sendCodeToChangePassword(email, code);
        ModelAndView modelAndView = new ModelAndView("new-password");
        PasswordChangeRequest request = PasswordChangeRequest.builder().id(savedData.getId()).build();
        modelAndView.addObject("request", request);
        return modelAndView;
    }

    public String changePassword(PasswordChangeRequest request) {
        //TODO: Ошибка если не существует
        passwordChangeRepository.findById(request.getId()).ifPresent(passwordChangeData -> {
            if (encoder.matches(request.getCode(), passwordChangeData.getCode())) {
                if (LocalDateTime.now().isAfter(passwordChangeData.getDateExpired())) {
                    //TODO: Ошибка что просрочено
                    passwordChangeRepository.delete(passwordChangeData);
                }
                else {
                    String password = encoder.encode(request.getPassword());
                    userRepository.setUserPasswordByEmail(password, passwordChangeData.getEmail());
                    passwordChangeRepository.delete(passwordChangeData);
                }
            }
            else {
                if (passwordChangeData.getWrongTries()>=3) {
                    //TODO: Ошибка больше 3 попыток восстановления
                    passwordChangeRepository.delete(passwordChangeData);
                }
                else {
                    //TODO: Ошибка попробуйте еще раз
                    passwordChangeData.setWrongTries(passwordChangeData.getWrongTries() + 1);
                    passwordChangeRepository.save(passwordChangeData);
                }
            }
        });
        return "login";
    }

    public String showRegistration(String code, Model model) {
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

    //TODO: validation
    public String register(String code, User user) {
        if (code.equals("admin")) {
            user.setRoles(Arrays.stream(Role.values()).toList());
            user.setEmail("admin@admin.com");
            user.setIsDeleted(false);
            user.setPassword(encoder.encode(user.getPassword()));
            user.setCreatedAt(LocalDateTime.now());
            userRepository.save(user);
            ideasClient.registerUserToIdeas(mapper.map(user, UserDTO.class));
        }
        //TODO: Добавить проверку на существует ли пользователь
        InvitationDTO invitation = emailClient.findInvitationById(code);
        if (invitation!=null) {
            user.setRoles(invitation.getRoles());
            user.setEmail(invitation.getEmail());
            user.setIsDeleted(false);
            user.setPassword(encoder.encode(user.getPassword()));
            user.setCreatedAt(LocalDateTime.now());
            userRepository.save(user);
            ideasClient.registerUserToIdeas(mapper.map(user, UserDTO.class));
        }
        return "redirect:/login";
    }
}
