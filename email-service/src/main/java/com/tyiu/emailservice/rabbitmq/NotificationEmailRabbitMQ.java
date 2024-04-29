package com.tyiu.emailservice.rabbitmq;

import com.tyiu.emailservice.service.EmailService;
import interfaces.INotification;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.amqp.rabbit.annotation.RabbitListener;
import org.springframework.amqp.rabbit.core.RabbitTemplate;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpStatusCode;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Component;
import reactor.core.publisher.Mono;
import request.NotificationRequest;
import response.NotificationResponse;

//@RequiredArgsConstructor
//@Slf4j
//@Component("prodEmailClient")
//public class NotificationEmailRabbitMQ implements INotification {
//
//    private final EmailService emailService;
//    private final RabbitTemplate rabbitTemplate;
//
//    @Value("${rabbitmq.exchange}")
//    private String topic;
//
//    @Value("${rabbitmq.notification-route.validate")
//    private String validateRoute;
//
//    @Override
//    @RabbitListener(queues = {"${rabbitmq.notification-queue.make}"})
//    public void makeNotification(NotificationRequest notificationRequest) {
//        emailService.sendMailNotification(notificationRequest).flatMap(html -> Mono.fromRunnable(() -> {
//            String message = String.format("Successful sending to mail %s with notificationId = %s",
//                    notificationRequest.getConsumerEmail(),
//                    notificationRequest.getNotificationId());
//            ResponseEntity<NotificationResponse> response = new ResponseEntity<NotificationResponse>(NotificationResponse.builder()
//                    .message(message)
//                    .notificationId(notificationRequest.getNotificationId())
//                    .build(), HttpStatusCode.valueOf(200));
//            validateResponse(response);
//            log.info(message);
//        })).doOnError(throwable -> {
//            ResponseEntity<NotificationResponse> response = new ResponseEntity<NotificationResponse>(NotificationResponse.builder()
//                    .message(throwable.getMessage())
//                    .notificationId(notificationRequest.getNotificationId())
//                    .build(), HttpStatusCode.valueOf(500));
//            validateResponse(response);
//            log.info(throwable.getMessage());
//        }).subscribe();
//    }
//
//    @Override
//    public void validateResponse(ResponseEntity<NotificationResponse> response) {
//        rabbitTemplate.convertAndSend(topic, validateRoute, response);
//    }
//}
