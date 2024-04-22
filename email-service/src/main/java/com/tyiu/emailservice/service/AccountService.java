package com.tyiu.emailservice.service;

import com.tyiu.emailservice.config.FreeMarkerConfig;
import com.tyiu.emailservice.model.dto.ChangeDataDTO;

import com.tyiu.ideas.config.exception.CustomHttpException;

import freemarker.template.Configuration;
import jakarta.mail.internet.MimeMessage;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import org.springframework.http.HttpStatus;
import org.springframework.mail.javamail.JavaMailSender;
import org.springframework.mail.javamail.MimeMessageHelper;
import org.springframework.stereotype.Service;
import org.springframework.ui.freemarker.FreeMarkerTemplateUtils;
import reactor.core.publisher.Mono;
import java.util.Map;

@Service
@RequiredArgsConstructor
@Slf4j
public class AccountService {

    private final JavaMailSender javaMailSender;
    private final FreeMarkerConfig freeMarkerConfig;

    private String getChangingDataContext(ChangeDataDTO changeData) {
        Configuration config = freeMarkerConfig.freemarkerClassLoaderConfig().getConfiguration();
        try {
            return FreeMarkerTemplateUtils.processTemplateIntoString(
                    config.getTemplate("changeData.ftl"), Map.of("changeData", changeData));
        } catch (Exception e) {
            log.error("Failed to parse html, due to {}", e.getMessage());
            throw new CustomHttpException(e.getMessage(), HttpStatus.CONFLICT.value());
        }
    }

    public Mono<Void> sendEmailToChangePassword(String email, String code) {
        return Mono.fromCallable(() -> {
            ChangeDataDTO data = ChangeDataDTO
                    .builder()
                    .code(code).to(email)
                    .subject("Код для изменения пароля")
                    .text("Вы изменяете пароль на вашем аккаунте. Необходимо ввести код для подтверждения изменения")
                    .build();
                    try {
                        String html = getChangingDataContext(data);
                        MimeMessage message = javaMailSender.createMimeMessage();
                        MimeMessageHelper helper = new MimeMessageHelper(message, "utf-8");
                        helper.setSubject(data.getSubject());
                        helper.setTo(data.getTo());
                        helper.setText(html, true);
                        helper.setFrom("HITS@hits1.tyuiu.ru");
                        javaMailSender.send(message);
                        return Mono.empty();
                    }
                    catch (Exception e) {
                        log.error("Failed to send email {} with subject {}, due to {}",
                                data.getTo(), data.getSubject(), e.getMessage());
                        return Mono.error(new CustomHttpException(e.getMessage(),
                                HttpStatus.INTERNAL_SERVER_ERROR.value()));
                    }
                }).then();
    }
}
