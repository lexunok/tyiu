package com.tyiu.corn.service;

import com.tyiu.corn.exception.NotFoundException;
import com.tyiu.corn.exception.ParseException;
import com.tyiu.corn.exception.UserExistsException;
import com.tyiu.corn.model.dto.InvitationDTO;
import com.tyiu.corn.exception.AuthorizationNotSuccessException;
import com.tyiu.corn.exception.DateExpiredException;
import com.tyiu.corn.exception.EmailSendException;
import com.tyiu.corn.model.entities.Temporary;
import com.tyiu.corn.model.entities.User;
import com.tyiu.corn.model.enums.Role;
import com.tyiu.corn.model.requests.ChangeRequest;
import com.tyiu.corn.model.requests.UserInfoRequest;
import com.tyiu.corn.model.responses.ChangeResponse;
import com.tyiu.corn.model.responses.InvitationResponse;
import com.tyiu.corn.model.responses.UserInfoResponse;
import com.tyiu.corn.repository.AccountChangeRepository;
import com.tyiu.corn.repository.UserRepository;

import jakarta.transaction.Transactional;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Random;
import java.util.UUID;
import java.util.regex.Pattern;

import lombok.RequiredArgsConstructor;

import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.mail.javamail.JavaMailSender;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.mail.MailParseException;
import org.springframework.mail.MailSendException;
import org.springframework.mail.SimpleMailMessage;

@Service
@RequiredArgsConstructor
@EnableScheduling
public class AccountChangeService {
    @Autowired
    private final JavaMailSender emailSender;
    
    private final AccountChangeRepository accountChangeRepository;

    private final UserRepository userRepository;

    private final PasswordEncoder passwordEncoder;

    private Pattern p = Pattern.compile("([\\w\\-]([\\.\\w])+[\\w]+@([\\w\\-]+\\.)+[A-Za-z]{1,10})");

    private void sendEmail(String toAdresses, String subject, String message){
        SimpleMailMessage simpleMailMessage = new SimpleMailMessage();
        simpleMailMessage.setTo(toAdresses);
        simpleMailMessage.setSubject(subject);
        simpleMailMessage.setText(message);
        this.emailSender.send(simpleMailMessage);
    }

    // private boolean containsRole(final List<Role> list, final Role string){
    //     return list.stream().filter(listEmail -> listEmail == string).findFirst().isPresent();
    // }

    public void sendInvitations(InvitationDTO invitations) throws MailSendException, NotFoundException, ParseException {
        if (invitations.getRoles() == null){
            throw new NotFoundException("Добавьте роли");
        }
        try {
            invitations.getEmails().stream().filter(email -> !userRepository.existsByEmail(email) 
            && p.matcher(email).matches()).forEach((email) ->
                {   
                    Date date = new Date();
                    long milliseconds = date.getTime() + 259200000;
                    date.setTime(milliseconds);
                    Temporary invitation = new Temporary();
                    invitation.setRoles(invitations.getRoles());
                    invitation.setEmail(email);
                    invitation.setDateExpired(date);
                    invitation.setUrl(UUID.randomUUID().toString());
                    sendEmail(
                        invitation.getEmail(), 
                        "Приглашение", 
                        String.format("Приглашение на регистрацию http/localhost:8080/register/%s", invitation.getUrl())
                    );
                    accountChangeRepository.save(invitation);
                }
            );
        } catch ( MailSendException e){
            throw new EmailSendException("В списке есть почта без домена");
        } catch (MailParseException e){
            throw new ParseException("В списке есть почта без имени пользователя");
        } catch ( NullPointerException e){
            throw new NotFoundException("Добавьте фаил с почтами");
        }
    }

    public void sendInvitation(Temporary invitation) throws EmailSendException, NotFoundException, ParseException{
        if (userRepository.existsByEmail(invitation.getEmail())){
            throw new UserExistsException("Пользователь с такой почтой существует");
        }
        UUID url = UUID.randomUUID();
        try {
            Date date = new Date();
            long milliseconds = date.getTime() + 259200000;
            date.setTime(milliseconds);
            invitation.setRoles(List.of(Role.INITIATOR));
            invitation.setDateExpired(date);
            invitation.setUrl(url.toString());
            sendEmail(
                invitation.getEmail(), 
                "Приглашение", 
                String.format("Приглашение на регистрацию http/localhost:8080/register/%s", invitation.getUrl())
            );
            if (accountChangeRepository.existsByEmail(invitation.getEmail())){
            accountChangeRepository.deleteByEmail(invitation.getEmail());
        }
        } catch (MailSendException e) {
            throw new EmailSendException("Добавьте домен почты");
        } catch (NullPointerException e) {
            throw new NotFoundException("Добавьте почту");
        } catch (MailParseException e){
            throw new ParseException("Добавьте имя пользователя почты");
        }
        accountChangeRepository.save(invitation);
    }

    public InvitationResponse findByUrl(String url) {
        Temporary invitation = accountChangeRepository.findByUrl(url).orElseThrow(
            () -> new NotFoundException("Приглашения " + url + " не существует"));
        return InvitationResponse.builder()
                .email(invitation.getEmail())
                .roles(invitation.getRoles())
                .build();
    }

    public ChangeResponse findByUrlAndSendCode(String url) {
        Temporary emailChange = accountChangeRepository.findByUrl(url).orElseThrow(
            () -> new NotFoundException("Доступ зарпрещен"));
        if (userRepository.existsByEmail(emailChange.getNewEmail())){
            throw new UserExistsException("Пользователь с такой почтой существует");
        }
        sendEmail(
            emailChange.getOldEmail(),
            "Код для изменения почты",
            String.format("Введите этот код для изменения почты %d", emailChange.getCode())
        );
        return ChangeResponse.builder()
            .newEmail(emailChange.getNewEmail())
            .oldEmail(emailChange.getOldEmail())
            .build();
    }

    public void sendEmailToChangeEmail(Temporary emailChange){
        if (userRepository.existsByEmail(emailChange.getNewEmail())){
            throw new UserExistsException("Пользователь с такой почтой существует");
        }
        if (accountChangeRepository.existsByEmail(emailChange.getEmail())){
            accountChangeRepository.deleteByEmail(emailChange.getEmail());
        }
        try{
            Date date = new Date();
            date.setTime(date.getTime()+43200000);
            emailChange.setUrl(UUID.randomUUID().toString());
            emailChange.setDateExpired(date);
            emailChange.setCode(new Random(System.currentTimeMillis()).nextInt(900000)+100000);
            sendEmail(
                emailChange.getNewEmail(),
                "Изменение почты",
                String.format("Ссылка для смены почты: http/localhost:8080/change-email/%s", emailChange.getUrl())
            );
            if (accountChangeRepository.existsByEmail(emailChange.getOldEmail())){
                accountChangeRepository.deleteByEmail(emailChange.getOldEmail());
            }
        }catch (MailSendException e) {
            throw new EmailSendException("Добавьте домен почты");
        } catch (NullPointerException e) {
            throw new NotFoundException("Добавьте почту");
        } catch (MailParseException e){
            throw new ParseException("Добавьте имя пользователя почты");
        }
        accountChangeRepository.save(emailChange);
    }

    public void sendEmailToChangePassword(Temporary passwordChange){
        if (!userRepository.existsByEmail(passwordChange.getEmail())){
            throw new NotFoundException(String.format("Пользователя с почтой %s не существует",  passwordChange.getEmail()));
        }
        if (accountChangeRepository.existsByEmail(passwordChange.getEmail())){
            accountChangeRepository.deleteByEmail(passwordChange.getEmail());
        }
        Date date = new Date();
        date.setTime(date.getTime()+300000);
        passwordChange.setDateExpired(date);
        passwordChange.setCode(new Random(System.currentTimeMillis()).nextInt(900000)+100000);
        sendEmail(
            passwordChange.getEmail(), 
            "Восстановление пароля", 
            String.format("Введите этот код для восстановления пароля: %d", passwordChange.getCode())
        );
        accountChangeRepository.save(passwordChange);
    }

    @Transactional
    public void changePasswordByUser(ChangeRequest request){
        Temporary changePassword = accountChangeRepository.findByEmail(request.getEmail()).orElseThrow(
            () -> new NotFoundException("Доступ зарпрещен")
        );
        if (new Date().getTime()>changePassword.getDateExpired().getTime()){
            throw new DateExpiredException("Время действия кода истекло");
        }
        if (request.getCode() == changePassword.getCode()){
            User user = userRepository.findByEmail(changePassword.getEmail()).get();
            userRepository.setPassword(passwordEncoder.encode(request.getPassword()), user.getId());
            accountChangeRepository.delete(changePassword);
        } else {
            throw new AuthorizationNotSuccessException("Неправильный код");
        }
        
    }

    @Transactional
    public void changeEmailByUser(ChangeRequest request){
        Temporary emailChange = accountChangeRepository.findByUrl(request.getUrl()).orElseThrow(
            () -> new NotFoundException("Доступ зарпрещен")
        );
        if (emailChange.getCode() == request.getCode()){
            User user = userRepository.findByEmail(request.getOldEmail()).get();
            userRepository.setEmail(request.getNewEmail(), user.getId());
            accountChangeRepository.delete(emailChange);
        } else {
            throw new AuthorizationNotSuccessException("Неправильный код");
        }
        
    }

    public List<UserInfoResponse> getUsersInfo(){
        List<UserInfoResponse> usersInfo = new ArrayList<UserInfoResponse>();
        userRepository.findAll().stream().forEach(
            user -> {
                usersInfo.add(UserInfoResponse.builder()
                                .email(user.getEmail())
                                .roles(user.getRoles())
                                .firstName(user.getFirstName())
                                .lastName(user.getLastName())
                                .build());
            }
        );
        return usersInfo;
    }

    @Transactional
    public void changeUserInfo(UserInfoRequest request){
        User user = userRepository.findByEmail(request.getEmail()).orElseThrow(
            () -> new NotFoundException("Пользователя с такой почтой не существует")
        );
        userRepository.setUserInfo(
            request.getNewEmail(),
            request.getNewFirstName(), 
            request.getNewLastName(),
            request.getNewRoles(),
            user.getId()
        );
    }

    public void deleteDataByUrl(String url){
        accountChangeRepository.deleteByUrl(url);
    }

    @Transactional
    @Scheduled(fixedRate = 43200000)
    public void deleteInvitation(){
        Date date = new Date();
        accountChangeRepository.deleteExpiredData(date);
    }
}
