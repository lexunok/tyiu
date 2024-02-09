package com.tyiu.emailservice.service;

import com.tyiu.emailservice.model.dto.*;
import com.tyiu.emailservice.model.entity.Invitation;
import com.tyiu.emailservice.model.responses.InvitationResponse;

import com.tyiu.ideas.model.entities.User;



import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.springframework.data.r2dbc.core.R2dbcEntityTemplate;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.core.scheduler.Schedulers;
import request.NotificationRequest;

import java.time.LocalDateTime;

import static org.springframework.data.relational.core.query.Criteria.where;
import static org.springframework.data.relational.core.query.Query.query;

@Service
@RequiredArgsConstructor
@EnableScheduling
@Slf4j
public class InvitationService {

    private final R2dbcEntityTemplate template;
    private final EmailService emailService;
    private final ModelMapper mapper;


    private Mono<Void> sendInvitation(String receiver, String link, User user) {
        String invitationText = "Вас пригласил(-а) зарегистрироваться на портал HITS " +
                user.getFirstName() + " " + user.getLastName() +
                " в качестве пользователя. Для регистрации на сервисе перейдите по данной ссылке и заполните все поля.";

        NotificationRequest emailRequest = NotificationRequest.builder()
                .consumerEmail(receiver)
                .title("Приглашение на регистрацию")
                .message(invitationText)
                .link(link)
                .buttonName("Зарегистрироваться")
                .build();

        return emailService.sendMailNotification(emailRequest).then();
    }


    @Scheduled(fixedRate = 28800000)
    private void deleteExpiredData() {
        template.delete(query(where("dateExpired").is(LocalDateTime.now())), Invitation.class).subscribe();
    }

    ///////////////////////
    //  _____   ____ ______
    // / ___/  / __//_  __/
    /// (_ /  / _/   / /
    //\___/  /___/  /_/
    ///////////////////////

    public Mono<InvitationResponse> findInvitationByUrl(String url) {
        return template.selectOne(query(where("id").is(url)), Invitation.class)
                .flatMap(i -> {
                    i.setDateExpired(LocalDateTime.now().plusHours(3));
                    return template.update(i).flatMap(inv -> Mono.just(InvitationResponse.builder()
                            .email(inv.getEmail())
                            .roles(inv.getRoles())
                            .build()));
                });
    }


    //////////////////////////////
    //   ___   ____    ____ ______
    //  / _ \ / __ \  / __//_  __/
    // / ___// /_/ / _\ \   / /
    ///_/    \____/ /___/  /_/
    //////////////////////////////

    public Mono<Void> sendInvitation(InvitationDTO invitationDTO, User user) {
        return Mono.just(invitationDTO.getEmail())
                .flatMap(e -> template.exists(query(where("email").is(e)), User.class)
                        .flatMap(u -> {
                            if (Boolean.TRUE.equals(u)) {
                                return Mono.error(new Exception("Ошибка приглашения."));
                            }
                            return template.exists(query(where("email").is(e)), Invitation.class)
                                    .flatMap(b -> {
                                        Invitation invitation = mapper.map(invitationDTO, Invitation.class);
                                        invitation.setDateExpired(LocalDateTime.now().plusDays(1));
                                        if (Boolean.TRUE.equals(b)) {
                                            return template.delete(query(where("email").is(e)), Invitation.class)
                                                    .then(template.insert(invitation)
                                                            .flatMap(i -> Mono.fromCallable(() -> sendInvitation(
                                                                    i.getEmail(),
                                                                    String.format("register/%s", i.getId()),
                                                                    user
                                                            )))).then();
                                        }
                                        return template.insert(invitation)
                                                .flatMap(i -> sendInvitation(
                                                        i.getEmail(),
                                                        String.format("register/%s", i.getId()),
                                                        user
                                                )).then();
                                    });

                        }));
    }

    public void sendInvitations(InvitationsDTO invitations, User user) {
        Flux.fromIterable(invitations.getEmails())
                .flatMap(email -> template.exists(query(where("email").is(email)), User.class)
                        .flatMap(userExists -> {
                            if (Boolean.FALSE.equals(userExists)) {
                                return template.exists(query(where("email").is(email)), Invitation.class)
                                        .flatMap(invitationExists -> {
                                            Invitation invitation = Invitation.builder()
                                                    .roles(invitations.getRoles())
                                                    .email(email)
                                                    .dateExpired(LocalDateTime.now().plusDays(1))
                                                    .build();

                                            if (Boolean.TRUE.equals(invitationExists))
                                                return template.delete(query(where("email").is(email)), Invitation.class)
                                                        .then(template.insert(invitation))
                                                        .flatMap(i ->
                                                                sendInvitation(
                                                                        i.getEmail(),
                                                                        String.format("register/%s", i.getId()),
                                                                        user)
                                                        )
                                                        .onErrorResume(e -> Mono.fromRunnable(() -> {
                                                            log.error("Error processing invitation for email {}: {}",
                                                                    email, e.getMessage());
                                                        }));
                                            return template.insert(invitation)
                                                    .flatMap(i ->
                                                            sendInvitation(
                                                                    i.getEmail(),
                                                                    String.format("register/%s", i.getId()),
                                                                    user)
                                                    )
                                                    .onErrorResume(e -> Mono.fromRunnable(() -> {
                                                        log.error("Error processing invitation for email {}: {}",
                                                                email, e.getMessage());

                                                    }));
                                        });
                            }
                            return Mono.empty();
                        })
                )
                .publishOn(Schedulers.boundedElastic())
                .subscribe();
    }


    ///////////////////////////////////////////
    //   ___    ____   __    ____ ______   ____
    //  / _ \  / __/  / /   / __//_  __/  / __/
    // / // / / _/   / /__ / _/   / /    / _/
    ///____/ /___/  /____//___/  /_/    /___/
    ///////////////////////////////////////////

    public Mono<Void> deleteInvitationByUrl(String url) {
        return template.delete(query(where("url").is(url)), Invitation.class).then();
    }
}



