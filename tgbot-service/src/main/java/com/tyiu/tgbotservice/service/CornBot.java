package com.tyiu.tgbotservice.service;

import com.tyiu.tgbotservice.model.NotificationTelegramResponse;
import lombok.extern.slf4j.Slf4j;
import org.springframework.amqp.rabbit.annotation.EnableRabbit;
import org.springframework.amqp.rabbit.annotation.RabbitListener;
import org.springframework.amqp.rabbit.core.RabbitTemplate;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Component;
import org.springframework.stereotype.Service;
import org.springframework.web.bind.annotation.*;
import org.telegram.telegrambots.bots.TelegramLongPollingBot;

import org.telegram.telegrambots.meta.api.methods.send.SendMessage;
import org.telegram.telegrambots.meta.api.objects.Update;
import org.telegram.telegrambots.meta.exceptions.TelegramApiException;

import java.util.Map;

@Slf4j
@Component
@Service
@EnableRabbit
@Configuration
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

    public void checkCommand(long chatId) {
        sendMessage(chatId, "Команда недоступна. Мы ещё работаем над этим");
    }



    public void sendUserTag(String userTag) {
        log.info(String.format("message sent -> %s", userTag));
        rabbitTemplate.convertAndSend(exchange, routingKey, Map.of("userTag", userTag));
    }

    @RabbitListener(queues = {"${rabbitmq.queue.receive.new}"})
    public void getNotification(NotificationTelegramResponse message) {

        String answer = "Вам пришло уведомление!\n\n" +
                message.getTitle() + "\n" +
                message.getMessage() + "\n\n" +
                "Подробнее можете ознакомиться здесь:\n" +
                message.getLink();
        log.info(answer);
    }

    // Пример по отправке Json файла в rabbitmq
    @PostMapping("/a")
    public ResponseEntity<String> sendJsonMessage(@RequestBody NotificationTelegramResponse notificationTelegramResponse) {

        log.info(String.format("Json message sent -> %s", notificationTelegramResponse.toString()));
        rabbitTemplate.convertAndSend(exchange, routingKey, notificationTelegramResponse);

        return ResponseEntity.ok("Notification sent to RabbitMQ");
    }
}
