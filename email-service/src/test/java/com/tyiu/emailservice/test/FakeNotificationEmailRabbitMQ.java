package com.tyiu.emailservice.test;

import interfaces.INotificationRabbitMQ;
import org.springframework.http.ResponseEntity;
import requests.NotificationRequest;
import response.NotificationResponse;

public class FakeNotificationEmailRabbitMQ implements INotificationRabbitMQ {
    @Override
    public void makeNotification(NotificationRequest notificationRequest) {

    }

    @Override
    public void validateResponse(ResponseEntity<NotificationResponse> response) {

    }
}
