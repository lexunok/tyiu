package com.tyiu.authorizationservice.service;

import com.tyiu.authorizationservice.model.entity.EmailChangeData;
import com.tyiu.authorizationservice.model.entity.Invitation;
import com.tyiu.authorizationservice.model.entity.PasswordChangeData;
import com.tyiu.authorizationservice.model.entity.User;
import com.tyiu.authorizationservice.model.request.PasswordChangeRequest;
import com.tyiu.authorizationservice.repository.EmailChangeDataRepository;
import com.tyiu.authorizationservice.repository.InvitationRepository;
import com.tyiu.authorizationservice.repository.PasswordChangeDataRepository;
import com.tyiu.authorizationservice.repository.UserRepository;
import com.tyiu.client.connections.EmailClient;
import com.tyiu.client.connections.IdeasClient;
import com.tyiu.client.models.Role;
import com.tyiu.client.models.UserDTO;
import jakarta.transaction.Transactional;
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
import java.util.Optional;

@RequiredArgsConstructor
@Service
@EnableScheduling
public class AccountService {

    private final ModelMapper mapper;
    private final PasswordEncoder encoder;

    private final IdeasClient ideasClient;
    private final EmailClient emailClient;

    private final UserRepository userRepository;
    private final InvitationRepository invitationRepository;
    private final PasswordChangeDataRepository passwordChangeRepository;
    private final EmailChangeDataRepository emailChangeRepository;

    //TODO: Регистрация админа только в тестовой среде
    //TODO: Сделать обработку ошибок
    //TODO: некоторые запросы на почту сделать с rabbit
    //TODO: добавить кэширование

    @Scheduled(fixedRate = 6000000)
    @Transactional
    void deleteExpiredData() {
        //TODO проверить мало ли это не удаление а select :)
        LocalDateTime now = LocalDateTime.now();
        passwordChangeRepository.deleteByDateExpiredLessThan(now);
        emailChangeRepository.deleteByDateExpiredLessThan(now);
        invitationRepository.deleteByDateExpiredLessThan(now);
    }

    //Генерация кода для изменения пароля и отправка на почту
    public ModelAndView generateAndSendCode(String email) {
        Boolean isUserExists = userRepository.existsByEmail(email);
        if (Boolean.FALSE.equals(isUserExists)) {
            //TODO: Ошибка что пользователя не существует
        }
        String code = String.valueOf(new SecureRandom().nextInt(900000)+100000);
        PasswordChangeData passwordChange = PasswordChangeData.builder()
                .email(email)
                .code(encoder.encode(code))
                .dateExpired(LocalDateTime.now().plusMinutes(5))
                .build();
        passwordChangeRepository.deleteByEmail(email);
        PasswordChangeData savedData = passwordChangeRepository.save(passwordChange);
        emailClient.sendCodeToChangePassword(email, code);
        ModelAndView modelAndView = new ModelAndView("new-password");
        PasswordChangeRequest request = PasswordChangeRequest.builder().id(savedData.getId()).build();
        modelAndView.addObject("request", request);
        return modelAndView;
    }
    //Сравнение кода с действительным и изменение пароля если сходится
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
    //Показывает форму регистрации
    public String showRegistration(String code, Model model) {
        if (code.equals("admin")) {
            model.addAttribute("email", "admin@admin.com");
            model.addAttribute("user", new User());
            model.addAttribute("code", code);
            return "registration";
        }
        //TODO: Error not found
        Invitation invitation = invitationRepository.findById(code).orElseThrow();
        if (invitation!=null) {
            invitation.setDateExpired(LocalDateTime.now().plusHours(3));
            invitationRepository.save(invitation);
            model.addAttribute("email", invitation.getEmail());
            model.addAttribute("user", new User());
            model.addAttribute("code", code);
            return "registration";
        }
        return "login";
    }

    //TODO: validation
    //Регистрирует пользователя
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
        //TODO: Error not found
        Invitation invitation = invitationRepository.findById(code).orElseThrow();
        if (invitation!=null) {
            user.setRoles(invitation.getRoles());
            user.setEmail(invitation.getEmail().toLowerCase());
            user.setIsDeleted(false);
            user.setPassword(encoder.encode(user.getPassword()));
            user.setCreatedAt(LocalDateTime.now());
            userRepository.save(user);
            invitationRepository.deleteById(invitation.getId());
            ideasClient.registerUserToIdeas(mapper.map(user, UserDTO.class));
        }
        return "redirect:/login";
    }
    //Генерирует и отправляет код на изменение почты
    public void sendCodeToChangeEmail(String newEmail, String oldEmail) {
        String code = String.valueOf(new SecureRandom().nextInt(900000)+100000);
        EmailChangeData data = EmailChangeData.builder()
                .code(encoder.encode(code))
                .newEmail(newEmail)
                .dateExpired(LocalDateTime.now().plusHours(2))
                .oldEmail(oldEmail)
                .build();
        if (emailChangeRepository.existsByNewEmail(data.getNewEmail())) {
            //TODO: Ошибка, письмо уже отправлено
        }
        emailChangeRepository.deleteByOldEmail(oldEmail);
        EmailChangeData saved = emailChangeRepository.save(data);
        emailClient.sendCodeToChangeEmail(saved.getNewEmail(), code);
    }

    //Сверяет код и изменяет почту
    public void changeEmailByUser(String code, String oldEmail) {
        Optional<EmailChangeData> emailChangeData = emailChangeRepository.findByOldEmail(oldEmail);
        if (emailChangeData.isPresent()) {
            EmailChangeData data = emailChangeData.get();
            if (encoder.matches(code, data.getCode())) {
                userRepository.setUserEmailByEmail(data.getNewEmail(), data.getOldEmail());
                emailChangeRepository.deleteByOldEmail(data.getOldEmail());
            }
            else {
                if (data.getWrongTries()>=3) {
                    //TODO: TOO MANY TRIES EXCEPTION
                    emailChangeRepository.deleteByOldEmail(data.getOldEmail());
                }
                //TODO: TRY AGAIN EXCEPTION
                data.setWrongTries(data.getWrongTries() + 1);
                emailChangeRepository.save(data);
            }
        }
        else ; //TODO: NOT FOUND EXCEPTION
    }
}
