package com.tyiu.corn.service;

import com.tyiu.corn.model.email.requests.InvitationEmailRequest;
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

@Service
@RequiredArgsConstructor
@Slf4j
public class EmailService {

    private final SpringTemplateEngine springTemplateEngine;
    private final JavaMailSender javaMailSender;

    public Mono<Void> sendMailInvitation(InvitationEmailRequest invitationEmailRequest){
        MimeMessage mimeMessage = javaMailSender.createMimeMessage();
        return Mono.just(new MimeMessageHelper(mimeMessage))
                .flatMap(h -> getInvitationContext(invitationEmailRequest).flatMap(html -> {
                try {
                    h.setSubject("Уведомление от портала HITS");
                    h.setFrom("HITS@mail.tyuiu.ru");
                    h.setTo(invitationEmailRequest.getTo());
                    h.setText(html,true);
                    javaMailSender.send(mimeMessage);
                    return Mono.empty();
                } catch (MessagingException e) {
                    throw new RuntimeException(e);
                }
                }));
    }

    private Mono<String> getInvitationContext(InvitationEmailRequest invitation){
        return Mono.just(new Context()).flatMap(context -> {
            context.setVariable("invitation", invitation);
            return Mono.just(springTemplateEngine.process("invitation.html", context));
        });
    }
}