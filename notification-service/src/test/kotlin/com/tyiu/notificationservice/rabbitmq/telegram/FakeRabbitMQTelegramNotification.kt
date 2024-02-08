package com.tyiu.notificationservice.rabbitmq.telegram

import com.tyiu.ideas.config.exception.CustomHttpException
import com.tyiu.notificationservice.rabbitmq.AbstractNotificationTelegram
import org.springframework.http.HttpStatusCode
import org.springframework.http.ResponseEntity
import org.springframework.stereotype.Component
import request.NotificationRequest
import response.NotificationResponse

@Component
class FakeRabbitMQTelegramNotification: AbstractNotificationTelegram() {

    override fun makeNotification(notificationRequest: NotificationRequest?) {

        if (notificationRequest == null) {
            throw CustomHttpException("Notification is null", 500)
        }

        if (notificationRequest.notificationId == null) {

            val message = "Error when sending notification to telegram. Notification id must not be null"

            this.validateResponse(
                ResponseEntity<NotificationResponse>(
                    NotificationResponse.builder()
                        .message(message)
                        .notificationId(notificationRequest.notificationId)
                        .build(),
                    HttpStatusCode.valueOf(500)
                )
            )
        } else if (notificationRequest.title == null
            || notificationRequest.message == null
            || notificationRequest.link == null) {

            val message = String.format(
                "Error when sending notification (id = %s). Notification content must not be null",
                notificationRequest.notificationId
            )

            this.validateResponse(
                ResponseEntity<NotificationResponse>(
                    NotificationResponse.builder()
                        .message(message)
                        .notificationId(notificationRequest.notificationId)
                        .build(),
                    HttpStatusCode.valueOf(500)
                )
            )
        } else if (notificationRequest.tag == null) {

            val message = String.format(
                "Error when sending notification (id = %s) to user. Tag must be not null",
                notificationRequest.notificationId
            )

            this.validateResponse(
                ResponseEntity<NotificationResponse>(
                    NotificationResponse.builder()
                        .message(message)
                        .notificationId(notificationRequest.notificationId)
                        .build(),
                    HttpStatusCode.valueOf(404)
                )
            )
        } else {

            val message = String.format(
                "Notification (id = %s) was successfully sent to the user with the tag = %s",
                notificationRequest.notificationId,
                notificationRequest.tag
            )

            this.validateResponse(
                ResponseEntity<NotificationResponse>(
                    NotificationResponse.builder()
                        .message(message)
                        .notificationId(notificationRequest.notificationId)
                        .build(),
                    HttpStatusCode.valueOf(200)
                )
            )
        }
    }

    override fun validateResponse(response: ResponseEntity<NotificationResponse>?) {

        if (response?.body != null) {
            throw CustomHttpException(response.body!!.message, response.statusCode.value())
        } else {
            throw CustomHttpException("Response body is null", 404)
        }
    }
}