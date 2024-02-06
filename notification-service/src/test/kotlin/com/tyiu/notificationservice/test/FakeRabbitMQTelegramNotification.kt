package com.tyiu.notificationservice.test

import interfaces.INotificationRabbitMQ
import org.springframework.http.ResponseEntity
import requests.NotificationRequest
import response.NotificationResponse

class FakeRabbitMQTelegramNotification: INotificationRabbitMQ {
    override fun makeNotification(notificationRequest: NotificationRequest?) {
        TODO("Not yet implemented")
    }

    override fun validateResponse(response: ResponseEntity<NotificationResponse>?) {
        TODO("Not yet implemented")
    }
}