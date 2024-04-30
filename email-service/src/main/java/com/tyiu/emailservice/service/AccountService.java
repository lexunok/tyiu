package com.tyiu.emailservice.service;

import com.tyiu.client.models.ChangeDataDTO;
import com.tyiu.client.models.UserDTO;
import com.tyiu.emailservice.config.ApplicationConfig;

import jakarta.mail.internet.MimeMessage;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

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

    private final ApplicationConfig freeMarkerConfig;
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

    public Mono<Void> sendInvitation(String email, String invitationId, UserDTO user) {
        return Mono.fromCallable(() -> {
            String invitationText = "Вас пригласил(-а) зарегистрироваться на портал HITS " +
                    user.getFirstName() + " " + user.getLastName() +
                    " в качестве пользователя. Для регистрации на сервисе перейдите по данной ссылке и заполните все поля.";
            NotificationRequest emailRequest = NotificationRequest.builder()
                    .consumerEmail(email)
                    .title("Приглашение на регистрацию")
                    .message(invitationText)
                    .link(authorizationHost + "registration?code=" + invitationId)
                    .buttonName("Зарегистрироваться")
                    .build();
            notificationService.sendMailNotification(emailRequest);
            return Mono.empty();
        }).then();
    }

    private String sendMailCodeToChangeData(ChangeDataDTO changeRequest){
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
            log.error("Failed to send email {} with subject {}, due to {}",
                    changeRequest.getTo(), changeRequest.getSubject(), e.getMessage());
            //TODO: exception
            throw new RuntimeException();
        }
    }
    //    public void sendInvitations(InvitationsDTO invitations, User user) {
//        Flux.fromIterable(invitations.getEmails())
//                .flatMap(email -> template.exists(query(where("email").is(email)), User.class)
//                        .flatMap(userExists -> {
//                            if (Boolean.FALSE.equals(userExists)) {
//                                return template.exists(query(where("email").is(email)), Invitation.class)
//                                        .flatMap(invitationExists -> {
//                                            Invitation invitation = Invitation.builder()
//                                                    .roles(invitations.getRoles())
//                                                    .email(email)
//                                                    .dateExpired(LocalDateTime.now().plusDays(1))
//                                                    .build();
//
//                                            if (Boolean.TRUE.equals(invitationExists))
//                                                return template.delete(query(where("email").is(email)), Invitation.class)
//                                                        .then(template.insert(invitation))
//                                                        .flatMap(i ->
//                                                                sendInvitation(
//                                                                        i.getEmail(),
//                                                                        String.format("register/%s", i.getId()),
//                                                                        user)
//                                                        )
//                                                        .onErrorResume(e -> Mono.fromRunnable(() -> {
//                                                            log.error("Error processing invitation for email {}: {}",
//                                                                    email, e.getMessage());
//                                                        }));
//                                            return template.insert(invitation)
//                                                    .flatMap(i ->
//                                                            sendInvitation(
//                                                                    i.getEmail(),
//                                                                    String.format("register/%s", i.getId()),
//                                                                    user)
//                                                    )
//                                                    .onErrorResume(e -> Mono.fromRunnable(() -> {
//                                                        log.error("Error processing invitation for email {}: {}",
//                                                                email, e.getMessage());
//
//                                                    }));
//                                        });
//                            }
//                            return Mono.empty();
//                        })
//                )
//                .publishOn(Schedulers.boundedElastic())
//                .subscribe();
//    }
}
