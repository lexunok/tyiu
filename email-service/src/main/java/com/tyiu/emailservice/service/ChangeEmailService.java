package com.tyiu.emailservice.service;

import com.tyiu.emailservice.model.dto.ChangeEmailDataDTO;
import com.tyiu.emailservice.model.entity.ChangeEmailData;
import com.tyiu.emailservice.model.enums.CodeStatus;
import com.tyiu.emailservice.model.requests.ChangeRequest;
import com.tyiu.emailservice.model.responses.ChangeDataEmailResponse;
import com.tyiu.emailservice.model.responses.ChangeResponse;
import com.tyiu.ideas.config.exception.CustomHttpException;
import com.tyiu.ideas.model.entities.User;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.springframework.data.r2dbc.core.R2dbcEntityTemplate;
import org.springframework.http.HttpStatus;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Mono;
import request.NotificationRequest;

import java.security.SecureRandom;
import java.time.LocalDateTime;

import static org.springframework.data.relational.core.query.Criteria.where;
import static org.springframework.data.relational.core.query.Query.query;
import static org.springframework.data.relational.core.query.Update.update;

@Service
@RequiredArgsConstructor
@EnableScheduling
@Slf4j
public class ChangeEmailService {

    private final R2dbcEntityTemplate template;
    private final EmailService emailService;
    private final ModelMapper mapper;

    @Scheduled(fixedRate = 7200000)
    private void deleteExpiredData() {
        template.delete(query(where("dateExpired").is(LocalDateTime.now())), ChangeEmailData.class).subscribe();
    }

    private Mono<Void> sendChangeDateCode(String to, String code){
        return Mono.just(ChangeDataEmailResponse.builder()
                        .code(code).to(to)
                        .subject("Код для изменения почты")
                        .text("Вы изменяете почту на вашем аккаунте. Необходимо " +
                                "ввести код для изменения почты для потверждения изменения")
                        .build())
                .flatMap(emailService::sendMailCodeToChangeData).then();
    }

    ///////////////////////
    //  _____   ____ ______
    // / ___/  / __//_  __/
    /// (_ /  / _/   / /
    //\___/  /___/  /_/
    ///////////////////////

    public Mono<ChangeResponse> findByUrlAndSendCode(String url) {
        return template.selectOne(query(where("id").is(url)), ChangeEmailData.class)
                .flatMap(e -> sendChangeDateCode(
                        e.getOldEmail(),
                        e.getCode().toString())
                        .then(Mono.just(ChangeResponse.builder()
                                .newEmail(e.getNewEmail())
                                .oldEmail(e.getOldEmail())
                                .build()
                        ))
                );
    }

    //////////////////////////////
    //   ___   ____    ____ ______
    //  / _ \ / __ \  / __//_  __/
    // / ___// /_/ / _\ \   / /
    ///_/    \____/ /___/  /_/
    //////////////////////////////

    public Mono<Void> sendEmailToChangeEmail(ChangeEmailDataDTO changeEmailDataDTO, String email){
        return Mono.just(mapper.map(changeEmailDataDTO, ChangeEmailData.class))
                .flatMap(emailChange ->
                        template.exists(query(where("email").is(emailChange.getNewEmail())), User.class)
                                .flatMap(b -> {
                                    if (Boolean.TRUE.equals(b)){
                                        return Mono.error(
                                                new CustomHttpException(CodeStatus.CHANGE_FAILED.toString(),
                                                        HttpStatus.CONFLICT.value()
                                                )
                                        );
                                    }
                                    return Mono.just(emailChange);
                                })
                )
                .flatMap(emailChange ->
                        template.exists(query(where("old_email").is(email)), ChangeEmailData.class)
                                .flatMap(b -> {
                                    emailChange.setCode(new SecureRandom().nextInt(900000)+100000);
                                    emailChange.setDateExpired(LocalDateTime.now().plusHours(2));
                                    emailChange.setOldEmail(email);
                                    if (Boolean.TRUE.equals(b)){
                                        return template.delete(query(where("old_email")
                                                        .is(emailChange.getOldEmail())), ChangeEmailData.class)
                                                .then(template.insert(emailChange)
                                                        .flatMap(d ->  Mono.just(NotificationRequest.builder()
                                                                .title("Изменение почты")
                                                                .message("Вы собираетесь изменить почту. Необходимо " +
                                                                        "подтвердить новую почту, перейдя по ссылке")
                                                                .consumerEmail(emailChange.getNewEmail())
                                                                .buttonName("Перейти по ссылке")
                                                                .link(String.format("change-email/%s", d.getId()))
                                                                .build()).flatMap(emailService::sendMailNotification).then()
                                                        ));
                                    }
                                    return template.insert(emailChange).flatMap(d ->  Mono.just(NotificationRequest.builder()
                                            .title("Изменение почты")
                                            .message("Вы собираетесь изменить почту. Необходимо подтвердить " +
                                                    "новую почту, перейдя по ссылке")
                                            .consumerEmail(emailChange.getNewEmail())
                                            .link(String.format("change-email/%s", d.getId()))
                                            .buttonName("Перейти по ссылке")
                                            .build()).flatMap(emailService::sendMailNotification).then()
                                    );
                                }));
    }

    ////////////////////////
    //   ___   __  __ ______
    //  / _ \ / / / //_  __/
    // / ___// /_/ /  / /
    ///_/    \____/  /_/
    ////////////////////////

    public Mono<Void> changeEmailByUser(ChangeRequest request){
        return template.exists(query(where("id").is(request.getKey())), ChangeEmailData.class)
                .flatMap(exists -> {
                    if (Boolean.FALSE.equals(exists)){
                        return Mono.error(new CustomHttpException(CodeStatus.CHANGE_FAILED.toString(),
                                HttpStatus.NOT_FOUND.value())
                        );
                    }
                    return template.selectOne(query(where("id").is(request.getKey())), ChangeEmailData.class)
                            .flatMap(e -> template.exists(query(where("email").is(e.getNewEmail())), User.class)
                                    .flatMap(b -> {
                                        if (Boolean.TRUE.equals(b)){
                                            return Mono.error(
                                                    new CustomHttpException(CodeStatus.CHANGE_FAILED.toString(),
                                                    HttpStatus.NOT_FOUND.value())
                                            );
                                        }
                                        if (request.getCode().equals(e.getCode().toString())){
                                            return template.update(query(where("email").is(request.getOldEmail())),
                                                            update("email", request.getNewEmail()),
                                                            User.class)
                                                    .then(template.delete(e))
                                                    .then();
                                        } else {
                                            if (e.getWrongTries()>=3){
                                                return template.delete(e).then(Mono.error(
                                                        new CustomHttpException(CodeStatus.CHANGE_FAILED.toString(),
                                                                HttpStatus.CONFLICT.value())
                                                        )
                                                );
                                            }
                                            return Mono.empty().then(template.update(query(where("id")
                                                                    .is(e.getId())),
                                                            update("wrong_tries", e.getWrongTries() + 1),
                                                            ChangeEmailData.class))
                                                    .then(Mono.error(
                                                            new CustomHttpException(CodeStatus.WRONG_CODE.toString(),
                                                                    HttpStatus.CONFLICT.value())
                                                            )
                                                    );
                                        }
                                    })
                            );
                });
    }
}
