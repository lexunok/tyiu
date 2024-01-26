package com.tyiu.tgbotservice.service;

import com.tyiu.tgbotservice.model.UserTelegram;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.r2dbc.core.R2dbcEntityTemplate;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Mono;

import static org.springframework.data.relational.core.query.Criteria.where;
import static org.springframework.data.relational.core.query.Query.query;
import static org.springframework.data.relational.core.query.Update.update;

@Slf4j
@Service
@RequiredArgsConstructor
public class TelegramTagService {

    private final R2dbcEntityTemplate template;

    public Mono<Void> addTelegramTag(UserTelegram userTelegram, String userEmail) {

        return Mono.just(userTelegram.getUserTag())
                .flatMap(userTag -> template.exists(query(where("user_tag").is(userTag)), UserTelegram.class))
                .flatMap(tagExists -> {

                    if (Boolean.TRUE.equals(tagExists)) {
                        return Mono.error(new Exception("Пользователь с таким тегом уже зарегестрирован"));
                    }
                    userTelegram.setUserEmail(userEmail);
                    userTelegram.setUserTag(userTelegram.getUserTag());

                    return template.insert(userTelegram);
                }).then();
    }

    public Mono<Void> updateTelegramTag(String userTag, String userEmail) {

        return template.update(query(where("user_email").is(userEmail)),
                update("user_tag", userTag)
                        .set("chatId", null),
                UserTelegram.class).then();
    }

    public Mono<Void> setTelegramTagVisibility(UserTelegram userTelegram, String userEmail) {

        return Mono.just(userEmail)
                .flatMap(dbFind -> template.selectOne(query(where("user_email").is(dbFind)), UserTelegram.class))
                .flatMap(currentUser -> {

                    String userTag = userTelegram.getUserTag();
                    Boolean visibility = userTelegram.getIsVisible();

                    if (currentUser.getUserTag().equals(userTag)) {

                        return template.update(query(where("user_tag").is(userTag)),
                                update("is_visible", visibility),
                                UserTelegram.class).then();
                    }
                    return Mono.error(new Exception("Нет прав!"));
                });
    }

    public Mono<Void> deleteTelegramInfo(String userTag, String userEmail) {

        return Mono.just(userEmail)
                .flatMap(dbFind -> template.selectOne(query(where("user_email").is(dbFind)), UserTelegram.class))
                .flatMap(currentUser -> {

                    if (currentUser.getUserTag().equals(userTag)) {
                        return template.delete(query(where("user_tag").is(userTag)), UserTelegram.class).then();
                    }
                    return Mono.error(new Exception("Нет прав!"));
                });
    }
 }
