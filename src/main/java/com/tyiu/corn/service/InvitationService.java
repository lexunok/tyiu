package com.tyiu.corn.service;

import com.tyiu.corn.exception.NotFoundException;
import com.tyiu.corn.exception.ParseException;
import com.tyiu.corn.exception.UserExistsException;
import com.tyiu.corn.model.dto.InvitationDTO;
import com.tyiu.corn.exception.AuthorizationNotSuccessException;
import com.tyiu.corn.exception.EmailSendException;
import com.tyiu.corn.model.entities.Invitation;
import com.tyiu.corn.model.entities.User;
import com.tyiu.corn.model.requests.ChangeEmailRequest;
import com.tyiu.corn.model.responses.ChangeEmailResponse;
import com.tyiu.corn.model.responses.InvitationResponse;
import com.tyiu.corn.repository.InvitationRepository;
import com.tyiu.corn.repository.UserRepository;

import jakarta.transaction.Transactional;

import java.util.Date;
import java.util.Random;
import java.util.UUID;
import java.util.regex.Pattern;

import lombok.RequiredArgsConstructor;

import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;
import org.springframework.mail.javamail.JavaMailSender;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.mail.MailParseException;
import org.springframework.mail.MailSendException;
import org.springframework.mail.SimpleMailMessage;

@Service
@RequiredArgsConstructor
@EnableScheduling
public class InvitationService {
    @Autowired
    private final JavaMailSender emailSender;
    
    private final InvitationRepository invitationRepository;

    private final UserRepository userRepository;

    private Pattern p = Pattern.compile("([\\w\\-]([\\.\\w])+[\\w]+@([\\w\\-]+\\.)+[A-Za-z]{1,10})");

    private void sendEmail(String toAdresses, String subject, String message){
        SimpleMailMessage simpleMailMessage = new SimpleMailMessage();
        simpleMailMessage.setTo(toAdresses);
        simpleMailMessage.setSubject(subject);
        simpleMailMessage.setText(message);
        this.emailSender.send(simpleMailMessage);
    }

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
                    Invitation invitation = new Invitation();
                    invitation.setRoles(invitations.getRoles());
                    invitation.setEmail(email);
                    invitation.setDateExpired(date);
                    invitation.setUrl(UUID.randomUUID().toString());
                    sendEmail(
                        invitation.getEmail(), 
                        "Приглашение", 
                        String.format("Приглашение на регистрацию http/localhost:8080/register/%s", invitation.getUrl())
                    );
                    invitationRepository.save(invitation);
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

    public void sendInvitation(Invitation invitation) throws EmailSendException, NotFoundException, ParseException{
        if (invitation.getRoles() == null){
            throw new NotFoundException("Добавьте роль");
        }
        if (userRepository.existsByEmail(invitation.getEmail())){
            throw new UserExistsException("Пользователь с такой почтой существует");
        }
        UUID url = UUID.randomUUID();
        try {
            Date date = new Date();
            long milliseconds = date.getTime() + 259200000;
            date.setTime(milliseconds);
            invitation.setDateExpired(date);
            invitation.setUrl(url.toString());
            sendEmail(
                invitation.getEmail(), 
                "Приглашение", 
                String.format("Приглашение на регистрацию http/localhost:8080/register/%s", invitation.getUrl())
            );
            if (invitationRepository.existsByEmail(invitation.getEmail())){
            invitationRepository.deleteByEmail(invitation.getEmail());
        }
        } catch (MailSendException e) {
            throw new EmailSendException("Добавьте домен почты");
        } catch (NullPointerException e) {
            throw new NotFoundException("Добавьте почту");
        } catch (MailParseException e){
            throw new ParseException("Добавьте имя пользователя почты");
        }
        invitationRepository.save(invitation);
    }

    public InvitationResponse findByUrl(String url) {
        Invitation invitation = invitationRepository.findByUrl(url).orElseThrow(
            () -> new NotFoundException("Приглашения " + url + " не существует"));
        return InvitationResponse.builder()
                .email(invitation.getEmail())
                .roles(invitation.getRoles())
                .build();
    }

    public ChangeEmailResponse findByUrlAndSendCode(String url) {
        Invitation invitation = invitationRepository.findByUrl(url).orElseThrow(
            () -> new NotFoundException("Доступ зарпрещен"));
        if (userRepository.existsByEmail(invitation.getEmail())){
            throw new UserExistsException("Пользователь с такой почтой существует");
        }
        
        sendEmail(
            invitation.getOldEmail(),
            "Код для изменения почты",
            String.format("Введите этот код для изменения почты %d", invitation.getCode())
        );
        return ChangeEmailResponse.builder()
            .email(invitation.getEmail())
            .oldEmail(invitation.getOldEmail())
            .build();
    }

    public void sendChangeEmailbyEmail(Invitation invitation){
        if (userRepository.existsByEmail(invitation.getEmail())){
            throw new UserExistsException("Пользователь с такой почтой существует");
        }
        int code = new Random(System.currentTimeMillis()).nextInt(900000)+100000;
        try{
            Date date = new Date();
            date.setTime(date.getTime()+86400000);
            invitation.setUrl(UUID.randomUUID().toString());
            invitation.setDateExpired(date);
            invitation.setCode(code);
            sendEmail(
                invitation.getEmail(),
                "Изменение почты",
                String.format("Ссылка для смены почты: http/localhost:8080/change-email/%s", invitation.getUrl())
            );
            if (invitationRepository.existsByEmail(invitation.getEmail())){
                invitationRepository.deleteByEmail(invitation.getEmail());
            }
        }catch (MailSendException e) {
            throw new EmailSendException("Добавьте домен почты");
        } catch (NullPointerException e) {
            throw new NotFoundException("Добавьте почту");
        } catch (MailParseException e){
            throw new ParseException("Добавьте имя пользователя почты");
        }
        invitationRepository.save(invitation);
    }
    @Transactional
    public void changeEmailByUser(ChangeEmailRequest request){
        Invitation invitation = invitationRepository.findByUrl(request.getUrl()).get();
        if (request.getCode() != invitation.getCode()){
            throw new AuthorizationNotSuccessException("Неправильный код");
        }
        User user = userRepository.findByEmail(request.getOldEmail()).get();
        userRepository.setEmail(request.getEmail(), user.getId());
        invitationRepository.delete(invitation);
    }

    public void deleteInvitationByUrl(String url){
        invitationRepository.deleteByUrl(url);
    }

    @Scheduled(cron = "@daily")
    public void deleteInvitation(){
        Date date = new Date();
        invitationRepository.deleteExpiredInvitations(date);
    }
}
