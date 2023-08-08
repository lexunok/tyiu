package com.tyiu.corn.service;

import com.tyiu.corn.exception.NotFoundException;
import com.tyiu.corn.model.dto.InvitationDTO;
import com.tyiu.corn.exception.EmailSendException;
import com.tyiu.corn.model.entities.Invitation;
import com.tyiu.corn.model.responses.InvitationResponse;
import com.tyiu.corn.repository.InvitationRepository;
import com.tyiu.corn.repository.UserRepository;

import java.util.Date;
import java.util.UUID;

import lombok.RequiredArgsConstructor;

import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;
import org.springframework.mail.javamail.JavaMailSender;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.mail.MailSendException;
import org.springframework.mail.SimpleMailMessage;

@Service
@RequiredArgsConstructor
@EnableScheduling
public class InvitationService {
    private final InvitationRepository invitationRepository;

    private final UserRepository userRepository;

    @Autowired
    private JavaMailSender emailSender;

    private void sendEmail(String toAdresses, String subject, String message){
        SimpleMailMessage simpleMailMessage = new SimpleMailMessage();
        simpleMailMessage.setTo(toAdresses);
        simpleMailMessage.setSubject(subject);
        simpleMailMessage.setText(message);
        this.emailSender.send(simpleMailMessage);
    }

    public void sendInvitations(InvitationDTO invitations) throws MailSendException, NotFoundException {
        if (invitations.getRoles() == null){
            throw new NotFoundException("Добавьте роли");
        }
        try {
            invitations.getEmails().stream().filter(email -> userRepository.existsByEmail(email)).forEach((email) ->
                {   
                    Invitation invitation = new Invitation();
                    invitation.setRoles(invitations.getRoles());
                    invitation.setEmail(email);
                    sendInvitation(invitation);
                }
            );
        } catch ( MailSendException e){
            throw new EmailSendException("В списке есть почта в неправильном формате");
        } catch ( NullPointerException e){
            throw new NotFoundException("Добавьте фаил с почтами");
        }
    }

    public void sendInvitation(Invitation invitation) throws EmailSendException, NotFoundException{
        Date date = new Date();
        long milliseconds = date.getTime() + 259200000;
        date.setTime(milliseconds);
        invitation.setDateExpired(date);
        invitation.setUrl(UUID.randomUUID().toString());
        if (invitation.getRoles() == null){
            throw new NotFoundException("Добавьте роль");
        }
        if (invitationRepository.existsByEmail(invitation.getEmail())){
                invitationRepository.deleteByEmail(invitation.getEmail());
        }
        try {
            sendEmail(
                invitation.getEmail(), 
                "Приглашение", 
                String.format("Приглашение на регистрацию http/localhost:8080/register/%s", invitation.getUrl())
            );
        } catch (MailSendException e) {
            throw new EmailSendException("Неправильный формат почты");
        } catch (NullPointerException e) {
            throw new NotFoundException("Добавьте почту");
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

    @Scheduled(cron = "@daily")
    public void deleteInvitation(){
        Date date = new Date();
        invitationRepository.deleteExpiredInvitations(date);
    }
}
