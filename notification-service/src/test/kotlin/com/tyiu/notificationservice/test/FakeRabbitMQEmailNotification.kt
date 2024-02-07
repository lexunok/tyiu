package com.tyiu.notificationservice.test

import com.tyiu.notificationservice.rabbitmq.AbstractNotificationEmail
import org.springframework.http.ResponseEntity
import org.springframework.stereotype.Component
import request.NotificationRequest
import response.NotificationResponse


@Component
class FakeRabbitMQEmailNotification: AbstractNotificationEmail() {
    override fun makeNotification(notificationRequest: NotificationRequest?) {
        TODO("Not yet implemented")
    }

    override fun validateResponse(response: ResponseEntity<NotificationResponse>?) {
        TODO("Not yet implemented")
    }
}