package com.tyiu.corn.service;

import com.tyiu.corn.exception.NotFoundException;
import com.tyiu.corn.model.dto.InvitationDTO;
import com.tyiu.corn.exception.EmailSendException;
import com.tyiu.corn.exception.FileReadException;
import com.tyiu.corn.model.entities.Invitation;
import com.tyiu.corn.model.enums.Role;
import com.tyiu.corn.repository.InvitationRepository;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.UUID;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.io.IOException;
import java.nio.charset.StandardCharsets;

import lombok.RequiredArgsConstructor;

import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;
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

    private void sendInvitations(List<String> emails, List<Role> roles) throws MailSendException, NotFoundException {
        try {
            Date date = new Date();
            long milliseconds = date.getTime() + 259200000;
            date.setTime(milliseconds);
            emails.forEach((email) ->
                {
                    Invitation invitation = new Invitation();
                    invitation.setRoles(roles);
                    invitation.setEmail(email);
                    sendInvitation(invitation);
                }
            );
        } catch ( MailSendException e){
            throw new EmailSendException("В списке есть почта в неправильном формате");
        } catch ( NullPointerException e){
            throw new NotFoundException("Добавьте почту");
        }
    }

    private List<String> findEmails(String text) throws NotFoundException{
        Pattern p = Pattern.compile("([\\w\\-]([\\.\\w])+[\\w]+@([\\w\\-]+\\.)+[A-Za-z]{1,10})");

        List<String> emails = new ArrayList<>();

        Matcher m = p.matcher(text);
        while (m.find()){
            String email = m.group();
            System.out.printf("Найденная почта - %s%n", email);
            emails.add(email);
        }
        if (emails.size() == 0){
            throw new NotFoundException("В фаиле нет почт");
        }
        return emails;
    }

    private List<String> getEmailsFromFile(MultipartFile file) throws FileReadException, NotFoundException{
        try{
            String content = new String(file.getBytes(), StandardCharsets.UTF_8);
            System.out.println(content);
            return findEmails(content);
        } catch (IOException e){
            throw new FileReadException("Ошибка открытия фаила");
        } 
    }

    public void sendFileInvitations(InvitationDTO invitations) throws FileReadException{
        sendInvitations(getEmailsFromFile(invitations.getFile()), invitations.getRoles());
    }

    public void sendInvitation(Invitation invitation){
        Date date = new Date();
        long milliseconds = date.getTime() + 259200000;
        date.setTime(milliseconds);
        invitation.setDateExpired(date);
        invitation.setUrl(UUID.randomUUID().toString());
        sendEmail(
                    invitation.getEmail(), 
                    "Приглашение", 
                    String.format("Приглашение на регистрацию http/localhost:8080/%s", invitation.getUrl())
        );
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
