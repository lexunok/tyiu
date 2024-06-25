package com.tyiu.emailservice.service;

import com.tyiu.client.exceptions.ServerProcessException;
import com.tyiu.client.models.ChangeDataDTO;
import com.tyiu.client.models.InvitationLinkRequest;
import com.tyiu.emailservice.config.EmailConfig;
import jakarta.mail.internet.MimeMessage;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.amqp.rabbit.annotation.RabbitListener;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.mail.javamail.JavaMailSender;
import org.springframework.mail.javamail.MimeMessageHelper;
import org.springframework.stereotype.Service;
import org.springframework.ui.freemarker.FreeMarkerTemplateUtils;
import reactor.core.publisher.Mono;
import request.NotificationRequest;

import java.util.Map;

@Service
@RequiredArgsConstructor
@Slf4j
public class AccountService {

    private final EmailConfig freeMarkerConfig;
    private final JavaMailSender javaMailSender;
    private final NotificationService notificationService;
    @Value("${authorization.host}")
    String authorizationHost;

    public Mono<Void> sendCodeToChangeEmail(String email, String code){
        return Mono.just(ChangeDataDTO.builder()
                        .code(code).to(email)
                        .subject("Код для изменения почты")
                        .text("Вы изменяете почту на вашем аккаунте. Необходимо " +
                                "ввести код для изменения почты для потверждения изменения")
                        .build())
                .map(this::sendMailCodeToChangeData).then();
    }
    public Mono<Void> sendCodeToChangePassword(String email, String code) {
        return Mono.just(ChangeDataDTO
                        .builder()
                        .code(code).to(email)
                        .subject("Код для изменения пароля")
                        .text("Вы изменяете пароль на вашем аккаунте. Необходимо ввести код для подтверждения изменения")
                        .build())
                .map(this::sendMailCodeToChangeData).then();
    }

    @RabbitListener(queues = "${rabbitmq.queues.invitation}", ackMode = "MANUAL")
    public Mono<Void> sendInvitation(InvitationLinkRequest request) {
        return Mono.fromCallable(() -> {
            log.info("Consume link for invitation");
            String invitationText = "Вас пригласил(-а) зарегистрироваться на портал HITS " +
                    request.getSenderFirstName() + " " + request.getSenderLastName() +
                    " в качестве пользователя. Для регистрации на сервисе перейдите по данной ссылке и заполните все поля.";
            NotificationRequest emailRequest = NotificationRequest.builder()
                    .consumerEmail(request.getReceiver())
                    .title("Приглашение на регистрацию")
                    .message(invitationText)
                    .link(authorizationHost + "/auth/registration?code=" + request.getLinkId())
                    .buttonName("Зарегистрироваться")
                    .build();
            notificationService.sendMailNotification(emailRequest);
            return Mono.empty();
        }).then();
    }

    private String sendMailCodeToChangeData(ChangeDataDTO changeRequest) {
        try {
            String html = FreeMarkerTemplateUtils
                    .processTemplateIntoString(freeMarkerConfig.freemarkerClassLoaderConfig().getConfiguration()
                            .getTemplate("changeData.ftl"), Map.of("changeData", changeRequest));
            MimeMessage message = javaMailSender.createMimeMessage();
            MimeMessageHelper helper = new MimeMessageHelper(message, "utf-8");
            helper.setSubject(changeRequest.getSubject());
            helper.setTo(changeRequest.getTo());
            helper.setText(html, true);
            helper.setFrom("HITS@hits1.tyuiu.ru");
            javaMailSender.send(message);
            return html;
        }
        catch (Exception e) {
            throw new ServerProcessException("Failed to send email " + changeRequest.getTo() +
                    " with subject " + changeRequest.getSubject() +
                    ", due to " + e.getMessage());
        }
    }
}
