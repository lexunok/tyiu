package com.tyiu.emailservice.service;

import com.tyiu.emailservice.config.FreeMarkerConfig;
import com.tyiu.ideas.config.exception.CustomHttpException;
import com.tyiu.emailservice.model.responses.ChangeDataEmailResponse;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
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
public class EmailService {

    private final FreeMarkerConfig freeMarkerConfig;
    private final JavaMailSender javaMailSender;


    private Mono<String> getNotificationContext(NotificationRequest notification) {
        return Mono.just(freeMarkerConfig
                .freemarkerClassLoaderConfig().getConfiguration()).flatMap(conf -> {
            try {
                return Mono.just(FreeMarkerTemplateUtils
                        .processTemplateIntoString(conf
                                .getTemplate("notification.ftl"), Map.of("notification", notification))
                );
            } catch (Exception e) {
                log.error("Failed to parse html, due to {}", e.getMessage());
                return Mono.error(new CustomHttpException(e.getMessage(), HttpStatus.CONFLICT.value()));
            }
        });
    }

    private Mono<String> getChangingDataContext(ChangeDataEmailResponse changeData) {
        return Mono.just(freeMarkerConfig
                .freemarkerClassLoaderConfig().getConfiguration()).flatMap(conf -> {
            try {
                return Mono.just(FreeMarkerTemplateUtils
                        .processTemplateIntoString(conf
                                .getTemplate("changeData.ftl"), Map.of("changeData", changeData))
                );
            } catch (Exception e) {
                log.error("Failed to parse html, due to {}", e.getMessage());
                return Mono.error(new CustomHttpException(e.getMessage(), HttpStatus.CONFLICT.value()));            }
        });
    }

    public Mono<String> sendMailNotification(NotificationRequest notificationRequest) {
        return Mono.just(javaMailSender.createMimeMessage())
                .flatMap(mimeMessage -> getNotificationContext(notificationRequest)
                    .flatMap(html -> {
                        try {
                            MimeMessageHelper helper = new MimeMessageHelper(mimeMessage, true);
                            helper.setSubject(notificationRequest.getTitle());
                            helper.setTo(notificationRequest.getConsumerEmail());
                            helper.setText(html, true);
                            helper.setFrom("HITS@hits1.tyuiu.ru");
                            javaMailSender.send(mimeMessage);
                            return Mono.just(html);
                        }
                        catch (Exception e) {
                            log.error("Failed to send email {} with subject 'Уведомление от портала HITS', due to {}",
                                    notificationRequest.getConsumerEmail(), e.getMessage());
                            return Mono.error(new CustomHttpException(e.getMessage(),
                                    HttpStatus.INTERNAL_SERVER_ERROR.value()));

                        }
                    })
                );
    }


    public Mono<String> sendMailCodeToChangeData(ChangeDataEmailResponse changeEmailRequest){
        return Mono.just(javaMailSender.createMimeMessage())
                .flatMap(mimeMessage -> getChangingDataContext(changeEmailRequest)
                        .flatMap(html -> {
                            try {
                                MimeMessageHelper helper = new MimeMessageHelper(mimeMessage, true);
                                helper.setSubject(changeEmailRequest.getSubject());
                                helper.setTo(changeEmailRequest.getTo());
                                helper.setText(html, true);
                                helper.setFrom("HITS@hits1.tyuiu.ru");
                                javaMailSender.send(mimeMessage);
                                return Mono.just(html);
                            }
                            catch (Exception e) {
                                log.error("Failed to send email {} with subject {}, due to {}",
                                        changeEmailRequest.getTo(), changeEmailRequest.getSubject(), e.getMessage());
                                return Mono.error(new CustomHttpException(e.getMessage(),
                                        HttpStatus.INTERNAL_SERVER_ERROR.value()));
                            }
                        })
                );
    }
}