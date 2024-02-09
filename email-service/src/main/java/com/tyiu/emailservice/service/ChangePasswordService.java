package com.tyiu.emailservice.service;

import com.tyiu.emailservice.model.dto.ChangePasswordDataDTO;
import com.tyiu.emailservice.model.entity.ChangePasswordData;
import com.tyiu.emailservice.model.enums.CodeStatus;
import com.tyiu.emailservice.model.requests.ChangeRequest;
import com.tyiu.emailservice.model.responses.ChangeDataEmailResponse;

import com.tyiu.ideas.config.exception.CustomHttpException;
import com.tyiu.ideas.model.entities.User;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import org.modelmapper.ModelMapper;
import org.springframework.data.r2dbc.core.R2dbcEntityTemplate;
import org.springframework.http.HttpStatus;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Mono;

import java.security.SecureRandom;
import java.time.LocalDateTime;

import static org.springframework.data.relational.core.query.Criteria.where;
import static org.springframework.data.relational.core.query.Query.query;
import static org.springframework.data.relational.core.query.Update.update;

@Service
@RequiredArgsConstructor
@EnableScheduling
@Slf4j
public class ChangePasswordService {

    private final R2dbcEntityTemplate template;
    private final ModelMapper mapper;
    private final EmailService emailService;
    private final PasswordEncoder passwordEncoder;


    private Mono<Void> sendChangeDateCode(String to, String code){
        return Mono.just(ChangeDataEmailResponse.builder()
                        .code(code).to(to)
                        .subject("Код для изменения пароля")
                        .text("Вы изменяете пароль на вашем аккаунте. Необходимо ввести код для подтверждения изменения")
                        .build())
                .flatMap(emailService::sendMailCodeToChangeData).then();
    }

    @Scheduled(fixedRate = 600000)
    private void deleteExpiredData() {
        template.delete(query(where("dateExpired").is(LocalDateTime.now())), ChangePasswordData.class).subscribe();
    }

    //////////////////////////////
    //   ___   ____    ____ ______
    //  / _ \ / __ \  / __//_  __/
    // / ___// /_/ / _\ \   / /
    ///_/    \____/ /___/  /_/
    //////////////////////////////

    public Mono<String> sendEmailToChangePassword(ChangePasswordDataDTO changePasswordDataDTO) {
        return Mono.just(mapper.map(changePasswordDataDTO, ChangePasswordData.class))
                .flatMap(passwordChange -> template.exists(query(where("email").is(changePasswordDataDTO.getEmail())), User.class)
                        .flatMap(exists -> {
                            if (Boolean.FALSE.equals(exists)){
                                return Mono.error(new CustomHttpException(CodeStatus.CHANGE_FAILED.toString(), HttpStatus.CONFLICT.value()));
                            }
                            return Mono.just(passwordChange);
                        }))
                .flatMap(passwordChange -> template.exists(query(where("email").is(passwordChange.getEmail())), ChangePasswordData.class)
                        .flatMap(b -> {
                            passwordChange.setDateExpired(LocalDateTime.now().plusMinutes(5));
                            passwordChange.setCode(new SecureRandom().nextInt(900000)+100000);
                            if (Boolean.TRUE.equals(b)){
                                return template.delete(query(where("email").is(passwordChange.getEmail())), ChangePasswordData.class)
                                        .then(template.insert(passwordChange)
                                                .flatMap(p -> sendChangeDateCode(
                                                        p.getEmail(),
                                                        p.getCode().toString()
                                                ).then(Mono.just(p.getId())))
                                        );
                            }
                            return template.insert(passwordChange)
                                    .flatMap(p -> sendChangeDateCode(
                                            p.getEmail(),
                                            p.getCode().toString()
                                    ).then(Mono.just(p.getId())));
                        })
                );
    }

    ////////////////////////
    //   ___   __  __ ______
    //  / _ \ / / / //_  __/
    // / ___// /_/ /  / /
    ///_/    \____/  /_/
    ////////////////////////

    public Mono<Void> changePasswordByUser(ChangeRequest request){
        return template.exists(query(where("id").is(request.getKey())), ChangePasswordData.class)
                .flatMap(exists -> {
                    if (Boolean.FALSE.equals(exists)) {
                        return Mono.error(new CustomHttpException(CodeStatus.CHANGE_FAILED.toString(), HttpStatus.NOT_FOUND.value()));
                    }
                    return template.selectOne(query(where("id").is(request.getKey())), ChangePasswordData.class)
                            .flatMap(c -> {
                                if (request.getCode().equals(c.getCode().toString())) {
                                    if (LocalDateTime.now().isAfter(c.getDateExpired())){
                                        return template.delete(c)
                                                .then(Mono.error(new CustomHttpException(CodeStatus.CHANGE_FAILED.toString(), HttpStatus.CONFLICT.value())));
                                    }
                                    return template.update(query(where("email").is(c.getEmail())),
                                                    update("password", passwordEncoder.encode(request.getPassword())),
                                                    User.class)
                                            .then(template.delete(c).then());
                                } else {
                                    if (c.getWrongTries()>=3){
                                        return template.delete(c)
                                                .then(Mono.error(new CustomHttpException(CodeStatus.CHANGE_FAILED.toString(), HttpStatus.CONFLICT.value())));
                                    }
                                    return template.update(query(where("id").is(c.getId())),
                                                    update("wrong_tries", c.getWrongTries() + 1), ChangePasswordData.class)
                                            .then(Mono.error(new CustomHttpException(CodeStatus.WRONG_CODE.toString(), HttpStatus.CONFLICT.value())));
                                }
                            });
                });
    }

}
