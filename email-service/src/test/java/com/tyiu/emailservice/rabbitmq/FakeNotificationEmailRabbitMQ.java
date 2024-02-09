package com.tyiu.emailservice.rabbitmq;

import com.tyiu.ideas.config.exception.CustomHttpException;
import interfaces.INotification;
import org.springframework.http.HttpStatusCode;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Component;
import request.NotificationRequest;
import response.NotificationResponse;

@Component("testEmailClient")
public class FakeNotificationEmailRabbitMQ implements INotification {

    @Override
    public void makeNotification(NotificationRequest notificationRequest) {
        if (notificationRequest == null)
            throw new CustomHttpException("request is null", 404);

        String path = "https://hits.tyuiu.ru/";
        if (notificationRequest.getNotificationId() == null)
            this.validateResponse(new ResponseEntity<>(NotificationResponse.builder()
                    .message("Validation isn't successful with subject: " + notificationRequest.getTitle()
                            + ". NotificationId must not be null")
                    .build(), HttpStatusCode.valueOf(404)));

        else if (notificationRequest.getMessage() == null
                || notificationRequest.getButtonName() == null
                || notificationRequest.getLink() == null
                || notificationRequest.getTitle() == null)
            this.validateResponse(new ResponseEntity<>(NotificationResponse.builder()
                .message("Validation isn't successful with notificationId: "
                        + notificationRequest.getNotificationId()
                        + ". Message, title, button or link name must not be null")
                .notificationId(notificationRequest.getNotificationId())
                .build(), HttpStatusCode.valueOf(404))
            );

        else if (!notificationRequest.getLink().contains(path))
            this.validateResponse(new ResponseEntity<>(NotificationResponse.builder()
                    .message("Validation isn't successful with notificationId: "
                            + notificationRequest.getNotificationId()
                            + ". link must be start with \"https://hits.tyuiu.ru/\"")
                    .notificationId(notificationRequest.getNotificationId())
                    .build(), HttpStatusCode.valueOf(404)));

        else if (notificationRequest.getConsumerEmail() == null)
            this.validateResponse(new ResponseEntity<>(NotificationResponse.builder()
                    .message("Validation isn't successful with notificationId: "
                            + notificationRequest.getNotificationId()
                            + ". ConsumerEmail must be defined")
                    .notificationId(notificationRequest.getNotificationId())
                    .build(), HttpStatusCode.valueOf(500)));

        else
            this.validateResponse(new ResponseEntity<>(NotificationResponse.builder()
                    .message("Validation is successful with notificationId: " + notificationRequest.getNotificationId())
                    .notificationId(notificationRequest.getNotificationId())
                    .build(), HttpStatusCode.valueOf(200)));
    }

    @Override
    public void validateResponse(ResponseEntity<NotificationResponse> response) {
        if (response.getBody() != null){
            throw new CustomHttpException(response.getBody().getMessage(), response.getStatusCode().value());
        } else {
            throw new CustomHttpException("Response body is null", 404);
        }
    }
}
