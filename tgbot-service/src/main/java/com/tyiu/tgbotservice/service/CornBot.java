package com.tyiu.tgbotservice.service;

import lombok.extern.slf4j.Slf4j;
import org.springframework.amqp.rabbit.annotation.RabbitListener;
import org.springframework.amqp.rabbit.core.RabbitTemplate;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Component;
import org.springframework.stereotype.Service;
import org.springframework.web.bind.annotation.*;
import org.telegram.telegrambots.bots.TelegramLongPollingBot;

import org.telegram.telegrambots.meta.api.methods.send.SendMessage;
import org.telegram.telegrambots.meta.api.objects.Update;
import org.telegram.telegrambots.meta.exceptions.TelegramApiException;

@Slf4j
@Component
@Service
@RestController
@RequestMapping("/api/v1")
public class CornBot extends TelegramLongPollingBot {

    @Value("${bot.name}")
    private String botName;
    @Value("${bot.token}")
    private String botToken;
    @Value("${rabbitmq.exchange}")
    private String exchange;
    @Value("${rabbitmq.routing.key}")
    private String routingKey;
    private final RabbitTemplate rabbitTemplate;

    public CornBot(RabbitTemplate rabbitTemplate) {
        this.rabbitTemplate = rabbitTemplate;
    }

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
                    startCommand(chatId, update.getMessage().getChat().getFirstName());
                    break;

                case "/help":
                    helpCommand(chatId);
                    break;

                case "/check":
//                    sendUserTagToReceiveNotification(userTag); Здесь потом команда для отправка тега и вывода уведолмений
                    sendMessage(chatId, "Команда недоступна. Мы ещё работаем над этим");
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

    public void startCommand(long chatId, String userFirstName) {

        String answer = "Привет, " + userFirstName + "! " +
                "Я буду твоим небольшим помощником в области нашего портала https://hits.tyuiu.ru";
        sendMessage(chatId, answer);
    }

    public void helpCommand(long chatId) {

        String answer = "Когда тебе придёт уведомления с портала, то я перешлю его тебе прямо в этот чат.\n" +
                "А пока, ты можешь ознакомиться с другими моими командами:\n\n" +
                "/check - просмотреть свои непрочитанне сообщения";
        sendMessage(chatId, answer);
    }



    // Получить данные из rabbitmq
    @RabbitListener(queues = {"${rabbitmq.queue.receive.notification}"})
    public void getNotification(String msg) {
        log.info(String.format("Received message! -> %s", msg));
    }

    // Отправить данные в rabbitmq
    @GetMapping("/notification/telegram/{userTag}")
    public void sendUserTagToReceiveNotification(@PathVariable String userTag) {
        sendUserTag(userTag);
        ResponseEntity.ok("User tag sent to RabbitMQ");
    }

    public void sendUserTag(String userTag) {
        log.info(String.format("message sent -> %s", userTag));
        rabbitTemplate.convertAndSend(exchange, routingKey, userTag);
    }
}
