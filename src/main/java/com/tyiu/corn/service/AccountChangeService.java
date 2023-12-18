package com.tyiu.corn.service;

import com.tyiu.corn.config.exception.AccessException;
import com.tyiu.corn.model.dto.*;
import com.tyiu.corn.model.email.requests.ChangeDataEmailRequest;
import com.tyiu.corn.model.email.requests.NotificationEmailRequest;
import com.tyiu.corn.model.entities.ChangeEmailData;
import com.tyiu.corn.model.entities.ChangePasswordData;
import com.tyiu.corn.model.entities.Invitation;
import com.tyiu.corn.model.entities.User;
import com.tyiu.corn.model.enums.CodeStatus;
import com.tyiu.corn.model.enums.Role;
import com.tyiu.corn.model.requests.ChangeRequest;
import com.tyiu.corn.model.responses.ChangeResponse;
import com.tyiu.corn.model.responses.InvitationResponse;
import com.tyiu.corn.model.responses.UserInfoResponse;

import java.security.SecureRandom;
import java.time.LocalDateTime;
import java.util.*;

import lombok.RequiredArgsConstructor;

import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.modelmapper.internal.bytebuddy.description.method.ParameterList;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.data.r2dbc.core.R2dbcEntityTemplate;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.core.publisher.ParallelFlux;
import reactor.core.scheduler.Scheduler;
import reactor.core.scheduler.Schedulers;

import static org.springframework.data.relational.core.query.Criteria.where;
import static org.springframework.data.relational.core.query.Query.query;
import static org.springframework.data.relational.core.query.Update.update;

@Service
@RequiredArgsConstructor
@EnableScheduling
@Slf4j
public class AccountChangeService {

    private final PasswordEncoder passwordEncoder;
    private final R2dbcEntityTemplate template;
    private final ModelMapper mapper;
    private final EmailService emailService;

    private Mono<Void> sendInvitation(String receiver, String link, User user) {
        String invitationText = "Вы были приглашены в качестве пользователя на Портал ВШЦТ. " +
                "Для регистрации на сервисе перейдите по данной ссылке и заполните все поля.";

        NotificationEmailRequest emailRequest = NotificationEmailRequest.builder()
                .to(receiver)
                .title("Приглашение на регистрацию на портале HITS")
                .message(invitationText)
                .link(link)
                .from(user.getFirstName() + " " + user.getLastName())
                .build();

        return emailService.sendMailNotification(emailRequest);
    }

    private Mono<Void> sendChangeDateCode(String subject, String text, String to, int code){
        return Mono.just(ChangeDataEmailRequest.builder()
                        .code(code).to(to)
                        .subject(subject)
                        .text(text)
                        .build())
                .flatMap(emailService::sendMailCodeToChangeData);
    }

    @Scheduled(fixedRate = 86400000)
    private void deleteExpiredData() {
        template.delete(query(where("dateExpired").is(LocalDateTime.now())), Invitation.class).subscribe();
        template.delete(query(where("dateExpired").is(LocalDateTime.now())), ChangeEmailData.class).subscribe();
        template.delete(query(where("dateExpired").is(LocalDateTime.now())), ChangePasswordData.class).subscribe();
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
                    i.setDateExpired(LocalDateTime.now().plusHours(5));
                    return template.update(i).flatMap(inv -> Mono.just(InvitationResponse.builder()
                            .email(inv.getEmail())
                            .roles(inv.getRoles())
                            .build()));
                });
    }

    public Mono<ChangeResponse> findByUrlAndSendCode(String url) {
        return template.selectOne(query(where("id").is(url)), ChangeEmailData.class)
                .flatMap(e -> sendChangeDateCode(
                            "Код для изменения почты",
                            "Вы изменяете почту на вашем аккаунте. Необходимо ввести код для изменения почты для потверждения изменения",
                            e.getOldEmail(),
                            e.getCode())
                        .then(Mono.just(ChangeResponse.builder()
                                    .newEmail(e.getNewEmail())
                                    .oldEmail(e.getOldEmail())
                                    .build()
                        ))
                );
    }

    public Flux<UserInfoResponse> getUsersInfo(){
        return template.select(User.class).all()
                .flatMap(u -> Mono.just(UserInfoResponse.builder()
                            .id(u.getId())
                            .email(u.getEmail())
                            .roles(u.getRoles())
                            .firstName(u.getFirstName())
                            .lastName(u.getLastName())
                            .build())
        );
    }

    public Mono<List<String>> getAllEmails(){
        return template.select(User.class).all().flatMap(u -> Mono.just(u.getEmail())).collectList();
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
                    if (Boolean.TRUE.equals(u)){
                        return Mono.error(new Exception("Ошибка приглашения."));
                    }
                    return template.exists(query(where("email").is(e)), Invitation.class)
                            .flatMap(b -> {
                                Invitation invitation = mapper.map(invitationDTO, Invitation.class);
                                invitation.setDateExpired(LocalDateTime.now().plusDays(1));
                                if (Boolean.TRUE.equals(b)){
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

    public Mono<Void> sendEmailToChangeEmail(ChangeEmailDataDTO changeEmailDataDTO, String email){
        return Mono.just(mapper.map(changeEmailDataDTO, ChangeEmailData.class))
        .flatMap(emailChange ->
                template.exists(query(where("email").is(emailChange.getNewEmail())), User.class)
                        .flatMap(b -> {
                            if (Boolean.TRUE.equals(b)){
                                return Mono.error(new VerifyError("Ошибка смены почты"));
                            }
                            return Mono.just(emailChange);
                        })
        )
        .flatMap(emailChange ->
                template.exists(query(where("old_email").is(email)), ChangeEmailData.class)
        .flatMap(b -> {
                emailChange.setCode(new SecureRandom().nextInt(90000000)+10000000);
                emailChange.setDateExpired(LocalDateTime.now().plusHours(12));
                emailChange.setOldEmail(email);
                if (Boolean.TRUE.equals(b)){
                    return template.delete(query(where("old_email").is(emailChange.getOldEmail())), ChangeEmailData.class)
                            .then(template.insert(emailChange)
                            .flatMap(d ->  Mono.just(NotificationEmailRequest.builder()
                                            .title("Изменение почты")
                                            .message("Вы собираетесь изменить почту. Необходимо подтвердить новую почту, перейдя по ссылке")
                                            .to(emailChange.getNewEmail())
                                            .link(String.format("change-email/%s", d.getId()))
                                            .build()).flatMap(emailService::sendMailNotification).then()
                            ));
                }
                return template.insert(emailChange).flatMap(d ->  Mono.just(NotificationEmailRequest.builder()
                        .title("Изменение почты")
                        .message("Вы собираетесь изменить почту. Необходимо подтвердить новую почту, перейдя по ссылке")
                        .to(emailChange.getNewEmail())
                        .link(String.format("change-email/%s", d.getId()))
                        .build()).flatMap(emailService::sendMailNotification).then()
                );
        }));
    }

    public Mono<String> sendEmailToChangePassword(ChangePasswordDataDTO changePasswordDataDTO) {
        return Mono.just(mapper.map(changePasswordDataDTO, ChangePasswordData.class))
                .flatMap(passwordChange -> template.exists(query(where("email").is(passwordChange.getEmail())), ChangePasswordData.class)
                        .flatMap(b -> {
                            passwordChange.setDateExpired(LocalDateTime.now().plusMinutes(5));
                            passwordChange.setCode(new SecureRandom().nextInt(90000000)+10000000);
                            if (Boolean.TRUE.equals(b)){
                                return template.delete(query(where("email").is(passwordChange.getEmail())), ChangePasswordData.class)
                                        .then(template.insert(passwordChange)
                                                .flatMap(p -> sendChangeDateCode(
                                                        "Код для изменения пароля",
                                                        "Вы изменяете пароль на вашем аккаунте. Необходимо ввести код для потверждения изменения",
                                                        p.getEmail(),
                                                        p.getCode()
                                                ).then(Mono.just(p.getId())))
                                        );
                            }
                            return template.insert(passwordChange)
                                        .flatMap(p -> sendChangeDateCode(
                                            "Код для изменения пароля",
                                            "Вы изменяете пароль на вашем аккаунте. Необходимо ввести код для потверждения изменения",
                                            p.getEmail(),
                                            p.getCode()
                                        ).then(Mono.just(p.getId())));
                        })
                );
    }

    ///////////////////////////////////////////
    //   ___    ____   __    ____ ______   ____
    //  / _ \  / __/  / /   / __//_  __/  / __/
    // / // / / _/   / /__ / _/   / /    / _/
    ///____/ /___/  /____//___/  /_/    /___/
    ///////////////////////////////////////////

    public Mono<Void> deleteInvitationByUrl(String url){
        return template.delete(query(where("url").is(url)), Invitation.class).then();
    }

    ////////////////////////
    //   ___   __  __ ______
    //  / _ \ / / / //_  __/
    // / ___// /_/ /  / /
    ///_/    \____/  /_/
    ////////////////////////

    public Mono<Void> changePasswordByUser(ChangeRequest request){
        return template.selectOne(query(where("id").is(request.getKey())), ChangePasswordData.class)
                .flatMap(c -> {
                    if (request.getCode().equals(c.getCode())) {
                        if (LocalDateTime.now().isAfter(c.getDateExpired())){
                            return template.delete(c).then(Mono.error(new AccessException(CodeStatus.CHANGE_FAILED.toString())));
                        }
                        return template.update(query(where("email").is(c.getEmail())),
                                        update("password", passwordEncoder.encode(request.getPassword())),
                                        User.class)
                                .then(template.delete(c))
                                .then();
                    } else {
                        if (c.getWrongTries()>=3){
                            return template.delete(c).then(Mono.error(new AccessException(CodeStatus.CHANGE_FAILED.toString())));
                        }
                        return  Mono.empty().then(template.update(query(where("id").is(c.getId())),
                                update("wrong_tries", c.getWrongTries() + 1), ChangePasswordData.class))
                                .then(Mono.error(new AccessException(CodeStatus.WRONG_CODE.toString())));
                    }
        });
    }

    public Mono<Void> changeEmailByUser(ChangeRequest request){
        return template.selectOne(query(where("id").is(request.getKey())), ChangeEmailData.class)
                .flatMap(e -> template.exists(query(where("email").is(e.getNewEmail())), User.class)
                        .flatMap(b -> {
                            if (Boolean.TRUE.equals(b)){
                                return Mono.error(new VerifyError("Ошибка смены почты"));
                            }
                            if (request.getCode().equals(e.getCode())){
                                return template.update(query(where("email").is(request.getOldEmail())),
                                                update("email", request.getNewEmail()),
                                                User.class)
                                        .then(template.delete(e))
                                        .then();
                            } else {
                                if (e.getWrongTries()>=3){
                                    return template.delete(e).then(Mono.error(new AccessException(CodeStatus.CHANGE_FAILED.toString())));
                                }
                                return  Mono.empty().then(template.update(query(where("id").is(e.getId())),
                                                update("wrong_tries", e.getWrongTries() + 1), ChangeEmailData.class))
                                        .then(Mono.error(new AccessException(CodeStatus.WRONG_CODE.toString())));
                            }
                        }));
    }

    public Mono<UserDTO> changeUserInfo(UserDTO userDTO){
        return template.update(query(where("id").is(userDTO.getId())),
                update("email", userDTO.getEmail())
                        .set("first_name", userDTO.getFirstName())
                        .set("last_name", userDTO.getLastName())
                        .set("roles", userDTO.getRoles().stream().map(Role::name).toArray(String[]::new)),
                User.class)
                .thenReturn(userDTO);
    }
}
