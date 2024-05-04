package com.tyiu.authorizationservice.service;

import com.tyiu.authorizationservice.config.exception.AccessException;
import com.tyiu.authorizationservice.config.exception.NotFoundException;
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
import com.tyiu.client.models.Role;
import com.tyiu.client.models.UserDTO;
import jakarta.annotation.PostConstruct;
import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.ui.Model;
import org.springframework.web.servlet.ModelAndView;

import java.security.SecureRandom;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;

@RequiredArgsConstructor
@Service
@EnableScheduling
public class AccountService {

    @Value("${server.admin.username}")
    private String adminUsername;
    @Value("${server.admin.password}")
    private String adminPassword;

    private final PasswordEncoder encoder;
    private final EmailClient emailClient;
    private final ModelMapper mapper;

    private final UserRepository userRepository;
    private final InvitationRepository invitationRepository;
    private final PasswordChangeDataRepository passwordChangeRepository;
    private final EmailChangeDataRepository emailChangeRepository;

    //TODO: Сделать обработку ошибок +

    @Scheduled(fixedRate = 6000000)
    @Transactional
    public void deleteExpiredData() {
        //TODO проверить мало ли это не удаление а select :)
        LocalDateTime now = LocalDateTime.now();
        passwordChangeRepository.deleteByDateExpiredLessThan(now);
        emailChangeRepository.deleteByDateExpiredLessThan(now);
        invitationRepository.deleteByDateExpiredLessThan(now);
    }
    @Cacheable(value = "password", key = "#email")
    public ModelAndView generateAndSendCode(String email) {
        Boolean isUserExists = userRepository.existsByEmail(email);
        if (Boolean.FALSE.equals(isUserExists)) {
            throw new NotFoundException("Пользователь не найден");
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

    public String changePassword(PasswordChangeRequest request) {
        Boolean isPasswordChangeRequestExists = passwordChangeRepository.existsById(request.getId());
        if (Boolean.FALSE.equals(isPasswordChangeRequestExists)) {
            throw new NotFoundException("Запроса на смену пароля не существует");
        }
        passwordChangeRepository.findById(request.getId()).ifPresent(passwordChangeData -> {
            if (encoder.matches(request.getCode(), passwordChangeData.getCode())) {
                if (LocalDateTime.now().isAfter(passwordChangeData.getDateExpired())) {
                    passwordChangeRepository.delete(passwordChangeData);
                    throw new AccessException("Время запроса истекло");
                }
                else {
                    String password = encoder.encode(request.getPassword());
                    userRepository.setUserPasswordByEmail(password, passwordChangeData.getEmail());
                    passwordChangeRepository.delete(passwordChangeData);
                }
            }
            else {
                if (passwordChangeData.getWrongTries()>=3) {
                    passwordChangeRepository.delete(passwordChangeData);
                    throw new AccessException("Превышено максимальное количество попыток");
                }
                else {
                    passwordChangeData.setWrongTries(passwordChangeData.getWrongTries() + 1);
                    passwordChangeRepository.save(passwordChangeData);
                    throw new AccessException("Ошибка, попробуйте ещё раз");
                }
            }
        });
        return "login";
    }
    @Cacheable(value = "registration", key = "#code")
    public String showRegistration(String code, Model model) {
        Optional<Invitation> invitation = invitationRepository.findById(code);
        if (invitation.isPresent()) {
            Invitation data = invitation.get();
            data.setDateExpired(LocalDateTime.now().plusHours(3));
            invitationRepository.save(data);
            model.addAttribute("email", data.getEmail());
            model.addAttribute("user", new User());
            model.addAttribute("code", code);
            return "registration";
        } else {
            throw new NotFoundException("Приглашение не найдено");
        }
    }

    //TODO: validation
    public String register(String code, User user) {
        Optional<Invitation> invitation = invitationRepository.findById(code);
        if (invitation.isPresent()) {
            Invitation data = invitation.get();
            user.setRoles(data.getRoles());
            user.setEmail(data.getEmail().toLowerCase());
            user.setIsDeleted(false);
            user.setPassword(encoder.encode(user.getPassword()));
            user.setCreatedAt(LocalDateTime.now());
            userRepository.save(user);
            invitationRepository.deleteById(data.getId());
        } else {
            throw new NotFoundException("Приглашение не найдено");
        }
        return "redirect:/login";
    }

    public void registerForAdmin(UserDTO userDTO) {
        User user = mapper.map(userDTO,User.class);
        user.setPassword(encoder.encode(user.getPassword()));
        user.setIsDeleted(false);
        userRepository.save(user);
    }

    @PostConstruct
    public void registerAdminOnInit() {
        if (Boolean.FALSE.equals(userRepository.existsByEmail(adminUsername))) {
            User user = User.builder()
                    .email(adminUsername)
                    .password(encoder.encode(adminPassword))
                    .isDeleted(false)
                    .roles(List.of(Role.values()))
                    .createdAt(LocalDateTime.now())
                    .build();
            userRepository.save(user);
        }
    }

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
            throw new AccessException("Письмо уже отправлено на почту");
        }
        emailChangeRepository.deleteByOldEmail(oldEmail);
        EmailChangeData saved = emailChangeRepository.save(data);
        emailClient.sendCodeToChangeEmail(saved.getNewEmail(), code);
    }

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
                    emailChangeRepository.deleteByOldEmail(data.getOldEmail());
                    throw new AccessException("Превышено максимальное количество попыток");
                }
                data.setWrongTries(data.getWrongTries() + 1);
                emailChangeRepository.save(data);
                throw new AccessException("Ошибка, попробуйте ещё раз");
            }
        }
        else {
            throw new NotFoundException("Код не найден");
        }
    }
}
