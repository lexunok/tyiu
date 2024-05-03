package com.tyiu.emailservice.service;

import com.tyiu.emailservice.config.EmailConfig;
import com.tyiu.emailservice.config.exception.ServerProcessException;
import jakarta.mail.internet.MimeMessage;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.mail.javamail.MimeMessageHelper;
import org.springframework.stereotype.Service;
import org.springframework.mail.javamail.JavaMailSender;
import org.springframework.ui.freemarker.FreeMarkerTemplateUtils;
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
            helper.setFrom("HITS@hits1.tyuiu.ru");
            javaMailSender.send(message);
        }
        catch (Exception e) {
            throw new ServerProcessException("Failed to send email " + notificationRequest.getConsumerEmail() +
                    " with subject " + notificationRequest.getTitle() +
                    ", due to " + e.getMessage());
        }
    }

}