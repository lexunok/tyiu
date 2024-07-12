package com.tyiu.emailservice.service;

import com.tyiu.client.exceptions.ServerProcessException;
import com.tyiu.client.models.TeamInvitationRequest;
import com.tyiu.emailservice.config.EmailConfig;
import jakarta.mail.internet.MimeMessage;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.amqp.rabbit.annotation.RabbitListener;
import org.springframework.mail.javamail.MimeMessageHelper;
import org.springframework.stereotype.Service;
import org.springframework.mail.javamail.JavaMailSender;
import org.springframework.ui.freemarker.FreeMarkerTemplateUtils;
import reactor.core.publisher.Mono;
import request.NotificationRequest;

import java.util.Map;

@Service
@RequiredArgsConstructor
@Slf4j
public class NotificationService {

    private final EmailConfig freeMarkerConfig;
    private final JavaMailSender javaMailSender;

    private String getNotificationContext(NotificationRequest notification) {
        try {
            return FreeMarkerTemplateUtils.processTemplateIntoString(
                    freeMarkerConfig.freemarkerClassLoaderConfig().getConfiguration()
                            .getTemplate("notification.ftl"), Map.of("notification", notification));
        } catch (Exception e) {
            throw new ServerProcessException("Failed to parse html, due to " + e.getMessage());
        }
    }

    public void sendMailNotification(NotificationRequest notificationRequest) {
        try {
            String html = getNotificationContext(notificationRequest);
            MimeMessage message = javaMailSender.createMimeMessage();
            MimeMessageHelper helper = new MimeMessageHelper(message, "utf-8");
            helper.setSubject(notificationRequest.getTitle());
            helper.setTo(notificationRequest.getConsumerEmail());
            helper.setText(html, true);
            helper.setFrom("hist@tyuiu.ru");
            javaMailSender.send(message);
        }
        catch (Exception e) {
            throw new ServerProcessException("Failed to send email " + notificationRequest.getConsumerEmail() +
                    " with subject " + notificationRequest.getTitle() +
                    ", due to " + e.getMessage());
        }
    }


    @RabbitListener(queues = "${rabbitmq.queues.team-invitation}", ackMode = "MANUAL")
    public Mono<Void> sendTeamInvitation(TeamInvitationRequest request) {
        return Mono.fromCallable(() -> {
            String message = String.format("Вас пригласил(-а) %s %s в команду \"%s\" в качестве участника.",
                    request.getSenderFirstName(), request.getSenderLastName(), request.getTeamName());
            NotificationRequest emailRequest = NotificationRequest.builder()
                    .consumerEmail(request.getReceiver())
                    .title("Приглашение в команду")
                    .message(message)
                    .link("https://hits.tyuiu.ru/teams/list/" + request.getTeamId())
                    .buttonName("Перейти в команду")
                    .build();
            sendMailNotification(emailRequest);
            return Mono.empty();
        }).then();
    }


}
