package com.tyiu.corn.service;

import com.tyiu.corn.model.dto.InvitationDTO;
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

    public void sendInvitations(InvitationDTO invitations){
        Invitation invitation = new Invitation();
        Date date = new Date();
        long milliseconds = date.getTime() + 259200000;
        date.setTime(milliseconds);
        invitations.getEmails().stream().forEach((email) ->
            {
                invitation.setUrl(UUID.randomUUID().toString());
                invitation.setDateExpired(date);
                invitation.setRoles(invitations.getRoles());
                invitation.setEmail(email);
                invitationRepository.save(invitation);
                sendEmail(
                    email, 
                    "Приглашение", 
                    String.format("Приглашение на регистрацию http/localhost:8080/%s", invitation.getUrl())
                );
            }
        );
    }

    @Scheduled(cron = "@daily")
    public void deleteInvitation(){
        Date date = new Date();
        invitationRepository.deleteExpiredInvitations(date);
    }
}
