package com.tyiu.emailservice.service;


import com.tyiu.ideas.model.dto.UserDTO;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Mono;
import request.NotificationRequest;

@Service
@RequiredArgsConstructor
@EnableScheduling
@Slf4j
public class InvitationService {

    private final EmailService emailService;

    private String authorizationHost = "http://localhost:7777/";

    public Mono<Void> sendInvitation(String email, String invitationId, UserDTO user) {
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
        return emailService.sendMailNotification(emailRequest).then();
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



