package com.tyiu.tgbotservice.service;

import com.tyiu.tgbotservice.model.entities.UserTelegram;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.amqp.rabbit.annotation.EnableRabbit;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.r2dbc.core.R2dbcEntityTemplate;
import org.springframework.stereotype.Component;
import org.springframework.stereotype.Service;

import org.telegram.telegrambots.bots.TelegramLongPollingBot;
import org.telegram.telegrambots.meta.api.methods.send.SendMessage;
import org.telegram.telegrambots.meta.api.objects.Update;
import org.telegram.telegrambots.meta.exceptions.TelegramApiException;

import reactor.core.publisher.Mono;

import static org.springframework.data.relational.core.query.Criteria.where;
import static org.springframework.data.relational.core.query.Query.query;
import static org.springframework.data.relational.core.query.Update.update;

@Slf4j
@Service
@Component
@EnableRabbit
@Configuration
@RequiredArgsConstructor
public class BotService extends TelegramLongPollingBot {

    @Value("${bot.name}")
    private String botName;

    @Value("${bot.token}")
    private String botToken;

    @Value("${rabbitmq.exchange}")
    private String exchange;

    @Value("${rabbitmq.routes.respond.to.notification}")
    private String routeRespond;

    private final R2dbcEntityTemplate template;

    @Override
    public String getBotUsername() {
        return botName;
    }

    @Override
    public String getBotToken() {
        return botToken;
    }

    @Override
    public void onUpdateReceived(Update update) {

        if (update.hasMessage() && update.getMessage().hasText()) {

            String messageText = update.getMessage().getText();
            String userTag = update.getMessage().getFrom().getUserName();
            long chatId = update.getMessage().getChatId();

            switch (messageText) {

                case "/start":
                    startCommand(userTag, chatId, update.getMessage().getChat().getFirstName());
                    break;

                case "/help":
                    helpCommand(chatId);
                    break;

                case "/check":
                    checkCommand(chatId);
                    break;

                default:
                    defaultMessage(chatId);
            }
        }
    }



    public void sendMessage(long chatId, String text) {

        SendMessage message = new SendMessage();
        message.setChatId(String.valueOf(chatId));
        message.setText(text);

        try {
            execute(message);
        } catch (TelegramApiException e) {
            log.error("Ошибка при доставке сообщения: " + e.getMessage());
        }
    }

    public void defaultMessage(long chatId) {

        String answer = "Я тебя не понимаю. Воспользуйся командой /help";
        sendMessage(chatId, answer);
    }

    public void sendNotificationToChat(String tag, String answer) {

        template.selectOne(query(where("user_tag").is(tag)), UserTelegram.class)
                .flatMap(send -> {

                    Long chatId = send.getChatId();
                    if (chatId != null) {
                        sendMessage(chatId, answer);
                    }
                    return Mono.empty();
                }).subscribe();
    }



    public void startCommand(String userTag, long chatId, String userFirstName) {

        Mono.just(userTag)
                .flatMap(tag -> template.exists(query(where("user_tag").is(tag)), UserTelegram.class))
                .flatMap(tagExists -> {

                    if (Boolean.TRUE.equals(tagExists)) {

                        return template.update(query(where("user_tag").is(userTag)),
                                update("chat_id", chatId),
                                UserTelegram.class);
                    }
                    else {
                        String answer = "Извини, " + userFirstName + ", я не смог найти тебя в списке пользователей. " +
                                "Пожалуйста, убедись, что ты верно указал свой тег в профиле.\n" +
                                "https://hits.tyuiu.ru";
                        sendMessage(chatId, answer);
                    }

                    return Mono.error(new Exception("Ошибка при дбавлении пользователя"));
                }).subscribe();

        String answer = "Привет, " + userFirstName + "! " +
                "Я буду твоим небольшим помощником в области нашего портала https://hits.tyuiu.ru";
        sendMessage(chatId, answer);
    }

    public void helpCommand(long chatId) {

        String answer = "Первым делом убедись, что ты указал свой тег на сайте. Введи команду /start, чтобы удостовериться в этом." +
                "Когда тебе придёт уведомления с портала, то я перешлю его тебе прямо в этот чат.\n" +
                "А пока, ты можешь ознакомиться с другими моими командами:\n\n" +
                "/check - просмотреть свои непрочитанне сообщения";
        sendMessage(chatId, answer);
    }

    public void checkCommand(long chatId) {
        sendMessage(chatId, "Команда недоступна. Мы ещё работаем над этим");
    }
}
