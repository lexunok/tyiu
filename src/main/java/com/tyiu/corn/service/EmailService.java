package com.tyiu.corn.service;

import com.tyiu.corn.config.FreeMarkerConfig;
import com.tyiu.corn.model.email.requests.ChangeDataEmailRequest;
import com.tyiu.corn.model.email.requests.NotificationEmailRequest;
import freemarker.template.TemplateException;
import jakarta.mail.MessagingException;
import jakarta.mail.internet.MimeMessage;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.mail.javamail.MimeMessageHelper;
import org.springframework.stereotype.Service;
import org.springframework.mail.javamail.JavaMailSender;
import org.springframework.ui.freemarker.FreeMarkerTemplateUtils;
import reactor.core.publisher.Mono;
import reactor.core.scheduler.Schedulers;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.Map;

@Service
@RequiredArgsConstructor
@Slf4j
public class EmailService {

    private final FreeMarkerConfig freeMarkerConfig;
    private final JavaMailSender javaMailSender;

    private final String path = "https://hits.tyuiu.ru/";

    private Mono<String> getNotificationContext(NotificationEmailRequest notification) {
        return Mono.just(freeMarkerConfig
                .freemarkerClassLoaderConfig().getConfiguration()).flatMap(conf -> {
            try {
                return Mono.just(FreeMarkerTemplateUtils
                        .processTemplateIntoString(conf
                                .getTemplate("notification.ftl"), Map.of("notification", notification))
                );
            } catch (IOException | TemplateException e) {
                return Mono.fromRunnable(() -> {
                    log.error("Failed to parse html, due to {}", e.getMessage());
                });
            }
        });
    }

    private Mono<String> getChangingDataContext(ChangeDataEmailRequest changeData) {
        return Mono.just(freeMarkerConfig
                .freemarkerClassLoaderConfig().getConfiguration()).flatMap(conf -> {
            try {
                return Mono.just(FreeMarkerTemplateUtils
                        .processTemplateIntoString(conf
                                .getTemplate("changeData.ftl"), Map.of("changeData", changeData))
                );
            } catch (IOException | TemplateException e) {
                return Mono.fromRunnable(() -> {
                    log.error("Failed to parse html, due to {}", e.getMessage());
                });
            }
        });
    }

    public Mono<Void> sendMailNotification(NotificationEmailRequest notificationEmailRequest) {
        notificationEmailRequest.setLink(path + notificationEmailRequest.getLink());
        return Mono.just(javaMailSender.createMimeMessage())
                .flatMap(mimeMessage -> getNotificationContext(notificationEmailRequest)
                    .flatMap(html -> Mono.fromRunnable(() -> {
                        try {
                            MimeMessageHelper helper = new MimeMessageHelper(mimeMessage, true);
                            helper.setSubject("Уведомление от портала HITS");
                            helper.setTo(notificationEmailRequest.getTo());
                            helper.setText(html, true);
                            javaMailSender.send(mimeMessage);
                        }
                        catch (Exception e) {
                            log.error("Failed to send email {} with subject 'Уведомление от портала HITS', due to {}",
                                    notificationEmailRequest.getTo(), e.getMessage());
                        }
                    }))
                );
    }


    public Mono<Void> sendMailCodeToChangeData(ChangeDataEmailRequest changeEmailRequest){
        return Mono.just(javaMailSender.createMimeMessage())
                .flatMap(mimeMessage -> getChangingDataContext(changeEmailRequest)
                        .flatMap(html -> Mono.fromRunnable(() -> {
                            try {
                                MimeMessageHelper helper = new MimeMessageHelper(mimeMessage, true);
                                helper.setSubject(changeEmailRequest.getSubject());
                                helper.setTo(changeEmailRequest.getTo());
                                helper.setText(html, true);
                                javaMailSender.send(mimeMessage);
                            }
                            catch (Exception e) {
                                log.error("Failed to send email {} with subject {}, due to {}",
                                        changeEmailRequest.getTo(), changeEmailRequest.getSubject(), e.getMessage());
                            }
                        }))
                );
    }
}