package com.tyiu.tgbotservice.rabbitmq;

import com.tyiu.tgbotservice.service.BotService;
import interfaces.INotificationRabbitMQ;
import lombok.RequiredArgsConstructor;
import org.springframework.amqp.rabbit.annotation.EnableRabbit;
import org.springframework.amqp.rabbit.annotation.RabbitListener;
import org.springframework.amqp.rabbit.core.RabbitTemplate;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpStatusCode;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Component;
import org.springframework.stereotype.Service;
import requests.NotificationRequest;
import response.NotificationResponse;

@Service
@Component
@EnableRabbit
@RequiredArgsConstructor
public class ProdRabbitMQ implements INotificationRabbitMQ {

    @Value("${rabbitmq.exchange}")
    private String exchange;

    @Value("${rabbitmq.routes.respond.to.notification}")
    private String route;

    private final BotService bot;
    private final RabbitTemplate rabbitTemplate;

    @Override
    @RabbitListener(queues = {"${rabbitmq.queue.receive.new}"})
    public void makeNotification(NotificationRequest notificationRequest) {

        try {
            String answer = "Вам пришло уведомление от портале ВШЦТ!\n\n" +
                    notificationRequest.getTitle() + "\n" +
                    notificationRequest.getMessage() + "\n\n" +
                    "Подробнее можете ознакомиться здесь:\n" +
                    notificationRequest.getLink();

            String tag = notificationRequest.getTag();
            if (tag != null) {

                bot.sendNotificationToChat(tag, answer);

                String message = String.format("Notification (id = %s) was successfully sent to the user with the tag = %s",
                        notificationRequest.getNotificationId(),
                        notificationRequest.getTag());

                ResponseEntity<NotificationResponse> response = new ResponseEntity<NotificationResponse>(NotificationResponse.builder()
                        .message(message)
                        .notificationId(notificationRequest.getNotificationId())
                        .build(),
                        HttpStatusCode.valueOf(200));
                validateResponse(response);

            } else {

                String message = String.format("Error when sending notification (id = %s) to user",
                        notificationRequest.getNotificationId());

                ResponseEntity<NotificationResponse> response = new ResponseEntity<NotificationResponse>(NotificationResponse.builder()
                        .message(message)
                        .notificationId(notificationRequest.getNotificationId())
                        .build(),
                        HttpStatusCode.valueOf(404));
                validateResponse(response);
            }

        } catch (Exception e) {

            ResponseEntity<NotificationResponse> response = new ResponseEntity<NotificationResponse>(NotificationResponse.builder()
                    .message(e.getMessage())
                    .notificationId(notificationRequest.getNotificationId())
                    .build(),
                    HttpStatusCode.valueOf(500));
            validateResponse(response);
        }
    }

    @Override
    public void validateResponse(ResponseEntity<NotificationResponse> response) {
        rabbitTemplate.convertAndSend(exchange, route, response);
    }
}
