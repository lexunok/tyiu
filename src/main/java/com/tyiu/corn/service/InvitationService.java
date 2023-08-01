package com.tyiu.corn.service;

import com.tyiu.corn.model.Invitation;
import com.tyiu.corn.repository.InvitationRepository;

import java.util.Date;
import java.util.UUID;
import java.util.List;

import lombok.RequiredArgsConstructor;

import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;
import org.springframework.mail.javamail.JavaMailSender;
import org.springframework.mail.SimpleMailMessage;

@Service
@RequiredArgsConstructor
@EnableScheduling
public class InvitationService {
    private final InvitationRepository invitationRepository;
    
    private JavaMailSender emailSender;

    private void sendEmail(List<String> toAdresses, String subject, String message){
        String[] to = toAdresses.toArray(new String[toAdresses.size()]);

        SimpleMailMessage simpleMailMessage = new SimpleMailMessage();
        simpleMailMessage.setTo(to);
        simpleMailMessage.setSubject(subject);
        simpleMailMessage.setText(message);
        emailSender.send(simpleMailMessage);
    }

    public void sandInvitations(Invitation invitation){
        Date date = new Date();
        long milliseconds = date.getTime() + 259200000;
        date.setTime(milliseconds);
        invitation.setDateExpired(date);
        invitation.setUrl(UUID.randomUUID().toString());
        
        invitationRepository.save(invitation);

        sendEmail(
            invitation.getEmails(), 
            "Приглашение", 
            String.format("Приглашение на регистрацию http/localhost:8080/%s", invitation.getUrl())
            );
    }

    @Scheduled(cron = "@daily")
    public void deleteInvitation(Invitation invitation){
        Date date = new Date();
        invitationRepository.deleteExpiredInvitations(date);
    }
}
