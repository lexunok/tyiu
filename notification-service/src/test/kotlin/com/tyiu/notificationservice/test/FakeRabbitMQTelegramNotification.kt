package com.tyiu.notificationservice.test

import com.tyiu.notificationservice.rabbitmq.AbstractNotificationTelegram
import org.springframework.stereotype.Component
import org.springframework.http.ResponseEntity
import request.NotificationRequest
import response.NotificationResponse


@Component
class FakeRabbitMQTelegramNotification: AbstractNotificationTelegram() {
    override fun makeNotification(notificationRequest: NotificationRequest?) {
        TODO("Not yet implemented")
    }

    override fun validateResponse(response: ResponseEntity<NotificationResponse>?) {
        TODO("Not yet implemented")
    }
}