package com.tyiu.corn.service;

import jakarta.mail.MessagingException;
import jakarta.mail.internet.MimeMessage;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.mail.javamail.MimeMessageHelper;
import org.springframework.stereotype.Service;
import org.springframework.mail.javamail.JavaMailSender;
import org.thymeleaf.context.Context;
import org.thymeleaf.spring6.SpringTemplateEngine;
import reactor.core.publisher.Mono;

import java.io.IOException;

@Service
@RequiredArgsConstructor
@Slf4j
public class EmailService {

    private final SpringTemplateEngine springTemplateEngine;
    private final JavaMailSender javaMailSender;

//    Пример использования
//    public Mono<Void> sendHTMLMailInvitation(String subject, String to, Invitation invitation) throws MessagingException, IOException {
//        MimeMessage mimeMessage = javaMailSender.createMimeMessage();
//        MimeMessageHelper helper = new MimeMessageHelper(mimeMessage);
//        helper.setSubject(subject);
//        helper.setTo(to);
//        helper.setText(getInvitationEmailContent(), true);
//        javaMailSender.send(mimeMessage);
//        return Mono.empty();
//    }
//
//    Создать HTML в папки src/main/resources/templates и написать его название
//    private String getInvitationEmailContent() {
//        return springTemplateEngine.process("invitation.html", new Context());
//    }
}