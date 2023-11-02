package com.tyiu.corn.service;

import com.tyiu.corn.model.dto.InvitationDTO;
import com.tyiu.corn.model.dto.UserDTO;
import com.tyiu.corn.model.entities.Temporary;
import com.tyiu.corn.model.entities.User;
import com.tyiu.corn.model.enums.Role;
import com.tyiu.corn.model.requests.ChangeRequest;
import com.tyiu.corn.model.responses.ChangeResponse;
import com.tyiu.corn.model.responses.InvitationResponse;
import com.tyiu.corn.model.responses.UserInfoResponse;

import java.time.LocalDateTime;
import java.util.*;

import lombok.RequiredArgsConstructor;

import org.springframework.data.r2dbc.core.R2dbcEntityTemplate;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.mail.javamail.JavaMailSender;
import org.springframework.mail.SimpleMailMessage;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import static org.springframework.data.relational.core.query.Criteria.where;
import static org.springframework.data.relational.core.query.Query.query;
import static org.springframework.data.relational.core.query.Update.update;

@Service
@RequiredArgsConstructor
@EnableScheduling
public class AccountChangeService {

    private final JavaMailSender emailSender;
    private final PasswordEncoder passwordEncoder;
    private final R2dbcEntityTemplate template;

    //private final String path = "http://localhost:8080";
    private final String path = "https://hits1.tyuiu.ru";

    private Mono<Void> sendEmail(String receiver, String subject, String message){
        return Mono.just(new SimpleMailMessage())
                            .flatMap(m -> {
                                m.setTo(receiver);
                                m.setSubject(subject);
                                m.setText(message);
                                emailSender.send(m);
                                return Mono.empty();
        });
    }

    @Scheduled(fixedRate = 43200000)
    public void deleteExpiredData(){
        template.delete(query(where("dateExpired").is(LocalDateTime.now())), Temporary.class).subscribe();
    }

    ///////////////////////
    //  _____   ____ ______
    // / ___/  / __//_  __/
    /// (_ /  / _/   / /
    //\___/  /___/  /_/
    ///////////////////////

    public Mono<InvitationResponse> findByUrl(String url) {
        return template.selectOne(query(where("url").is(url)), Temporary.class)
                .flatMap(i -> Mono.just(InvitationResponse.builder()
                                .email(i.getEmail())
                                .roles(i.getRoles())
                                .build()
                )
        );
    }

    public Mono<ChangeResponse> findByUrlAndSendCode(String url) {
        return template.selectOne(query(where("url").is(url)), Temporary.class)
                .flatMap(e -> {
                    sendEmail(
                            e.getOldEmail(),
                            "Код для изменения почты",
                            String.format("Введите этот код для изменения почты %d", e.getCode())
                    ).subscribe();
                    return Mono.just(ChangeResponse.builder()
                                    .newEmail(e.getNewEmail())
                                    .oldEmail(e.getOldEmail())
                                    .build()
                    );
        });
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

    public Mono<Void> sendInvitation(Temporary invitation) {
        return Mono.just(invitation.getEmail()).flatMap(e -> template.exists(query(where("email").is(e)), Temporary.class)
                .flatMap(b -> {
                    invitation.setRoles(List.of(Role.INITIATOR));
                    invitation.setDateExpired(LocalDateTime.now().plusDays(3));
                    invitation.setUrl(UUID.randomUUID().toString());
                    sendEmail(
                            invitation.getEmail(),
                            "Приглашение на портал ВШЦТ",
                            String.format("Приглашаем вас для регистрации на портале ВШЦТ" + path + "/register/%s", invitation.getUrl())
                    ).subscribe();
                    if (Boolean.TRUE.equals(b)){
                        return template.delete(query(where("email").is(e)), Temporary.class)
                                .then(template.insert(invitation));
                    }
                        return template.insert(invitation);
        })).then();
    }

    public Flux<Void> sendInvitations(InvitationDTO invitations){
        return Flux.fromIterable(invitations.getEmails()).flatMap(e -> template.exists(query(where("email").is(e)), User.class)
                .flatMap(b -> {
                    if(Boolean.FALSE.equals(b)){
                        Temporary invitation = Temporary.builder()
                                .roles(invitations.getRoles())
                                .email(e)
                                .dateExpired(LocalDateTime.now().plusDays(3))
                                .url(UUID.randomUUID().toString())
                                .build();
                        sendEmail(
                                invitation.getEmail(),
                                "Приглашение на портал ВШЦТ",
                                String.format("Приглашаем вас для регистрации на портале ВШЦТ" + path + "/register/%s", invitation.getUrl())
                        ).subscribe();
                        return template.insert(invitation).then();
                    }
                    return Mono.empty();
                }));
    }

    public Mono<Void> sendEmailToChangeEmail(Temporary emailChange){
        return Mono.just(emailChange.getOldEmail()).flatMap(e -> template.exists(query(where("oldEmail").is(e)), Temporary.class)
                .flatMap(b -> {
                        emailChange.setUrl(UUID.randomUUID().toString());
                        emailChange.setDateExpired(LocalDateTime.now().plusHours(12));
                        emailChange.setCode(new Random(System.currentTimeMillis()).nextInt(900000)+100000);
                        sendEmail(
                                emailChange.getNewEmail(),
                                "Изменение почты",
                                String.format("Ссылка для смены почты: " + path + "/change-email/%s", emailChange.getUrl())
                        ).subscribe();
                        if (Boolean.TRUE.equals(b)){
                            return template.delete(query(where("oldEmail").is(e)), Temporary.class)
                                    .then(template.insert(emailChange))
                                    .then();
                        }
                        return template.insert(emailChange).then();
        }));
    }

    public Mono<String> sendEmailToChangePassword(Temporary passwordChange) {
        passwordChange.setDateExpired(LocalDateTime.now().plusMinutes(5));
        passwordChange.setUrl(UUID.randomUUID().toString());
        passwordChange.setCode(new Random(System.currentTimeMillis()).nextInt(900000) + 100000);
        sendEmail(
                passwordChange.getEmail(),
                "Восстановление пароля",
                String.format("Введите этот код для восстановления пароля: %d", passwordChange.getCode())).subscribe();
        return template.insert(passwordChange).flatMap(p -> Mono.just(p.getUrl()));
    }

    ///////////////////////////////////////////
    //   ___    ____   __    ____ ______   ____
    //  / _ \  / __/  / /   / __//_  __/  / __/
    // / // / / _/   / /__ / _/   / /    / _/
    ///____/ /___/  /____//___/  /_/    /___/
    ///////////////////////////////////////////

    public Mono<Void> deleteDataByUrl(String url){
        return template.delete(query(where("url").is(url)), Temporary.class).then();
    }

    ////////////////////////
    //   ___   __  __ ______
    //  / _ \ / / / //_  __/
    // / ___// /_/ /  / /
    ///_/    \____/  /_/
    ////////////////////////

    public Mono<Void> changePasswordByUser(ChangeRequest request){
        return template.selectOne(query(where("url").is(request.getKey())), Temporary.class)
                .flatMap(c -> {
                    if (request.getCode() == c.getCode()) {
                        if (LocalDateTime.now().isAfter(c.getDateExpired())){
                            return template.delete(query(where("url").is(c.getUrl())), Temporary.class)
                                    .then(template.update(query(where("email").is(c.getEmail())),
                                                    update("password", passwordEncoder.encode(request.getPassword())),
                                                    User.class))
                                    .then(template.delete(c))
                                    .then();
                        }
                        return template.update(query(where("email").is(c.getEmail())),
                                        update("password", passwordEncoder.encode(request.getPassword())),
                                        User.class)
                                .then(template.delete(c))
                                .then();
                    }
                    return Mono.empty();
        });
    }


    public Mono<Void> changeEmailByUser(ChangeRequest request){
        return template.selectOne(query(where("url").is(request.getUrl())), Temporary.class)
                .flatMap(e -> {
                    if (request.getCode() == e.getCode()){
                        return template.update(query(where("email").is(request.getOldEmail())),
                                        update("email", request.getNewEmail()),
                                        User.class)
                                .then(template.delete(e))
                                .then();
                    }
                    return Mono.empty();
        });
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
