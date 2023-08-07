package com.tyiu.corn.service;

import com.tyiu.corn.exception.NotFoundException;
import com.tyiu.corn.model.dto.InvitationDTO;
import com.tyiu.corn.exception.EmailSendException;
import com.tyiu.corn.model.entities.Invitation;
import com.tyiu.corn.repository.InvitationRepository;

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
        try {
            invitations.getEmails().stream().forEach((email) ->
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

    public void sendInvitation(Invitation invitation) throws EmailSendException{
        Date date = new Date();
        long milliseconds = date.getTime() + 259200000;
        date.setTime(milliseconds);
        invitation.setDateExpired(date);
        invitation.setUrl(UUID.randomUUID().toString());
        if (invitation.getRoles() == null){
            throw new NotFoundException("Добавьте роли");
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

    public Invitation findByUrl(String url) {
        return invitationRepository.findByUrl(url);
    }

    @Scheduled(cron = "@daily")
    public void deleteInvitation(){
        Date date = new Date();
        invitationRepository.deleteExpiredInvitations(date);
    }
}
