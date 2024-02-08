package com.tyiu.notificationservice.rabbitmq.email

import com.tyiu.ideas.config.exception.CustomHttpException
import com.tyiu.notificationservice.rabbitmq.AbstractNotificationEmail
import org.springframework.http.HttpStatusCode
import org.springframework.http.ResponseEntity
import org.springframework.stereotype.Component
import request.NotificationRequest
import response.NotificationResponse

@Component
class FakeRabbitMQEmailNotification: AbstractNotificationEmail() {

    override fun makeNotification(notificationRequest: NotificationRequest?) {

        val path = "https://hits.tyuiu.ru/"

        if (notificationRequest == null) {
            throw CustomHttpException("Notification is null", 500)
        }

        if (notificationRequest.notificationId == null) {

            val message = "Error when sending notification to email service. Notification id must not be null"

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
            || notificationRequest.link == null
            || notificationRequest.buttonName == null) {

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
        } else if (!notificationRequest.link.startsWith(path)) {

            val message = String.format(
                "Error when sending notification (id = %s). Link must starting with required path",
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
        } else if (notificationRequest.consumerEmail == null) {

            val message = String.format(
                "Error when sending notification (id = %s) to user. Email must be not null",
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
                "Notification (id = %s) was successfully sent to the user with the email = %s",
                notificationRequest.notificationId,
                notificationRequest.consumerEmail
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