package com.tyiu.notificationservice.rabbitmq.email

import com.tyiu.ideas.model.entities.User
import interfaces.INotification
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.data.r2dbc.core.R2dbcEntityTemplate;
import org.springframework.data.relational.core.query.Criteria.where
import org.springframework.data.relational.core.query.Query.query
import org.springframework.http.HttpStatusCode
import org.springframework.http.ResponseEntity
import org.springframework.stereotype.Component
import reactor.core.publisher.Mono
import request.NotificationRequest
import response.NotificationResponse

@Component("testEmailClient")
class TestEmailNotification(@Autowired private val template: R2dbcEntityTemplate): INotification {

    override fun makeNotification(notification: NotificationRequest) {

        val path = "https://hits.tyuiu.ru/"

        if (notification.notificationId == null) {

            val message = "Error when sending notification to email service. Notification id must not be null"

            this.validateResponse(
                ResponseEntity<NotificationResponse>(
                    NotificationResponse.builder()
                        .message(message)
                        .notificationId(notification.notificationId)
                        .build(),
                    HttpStatusCode.valueOf(500)
                )
            )
        } else if (notification.title == null
            || notification.message == null
            || notification.link == null
            || notification.buttonName == null) {

            val message = String.format(
                "Error when sending notification (id = %s). Notification content must not be null",
                notification.notificationId
            )

            this.validateResponse(
                ResponseEntity<NotificationResponse>(
                    NotificationResponse.builder()
                        .message(message)
                        .notificationId(notification.notificationId)
                        .build(),
                    HttpStatusCode.valueOf(500)
                )
            )
        } else if (!notification.link.startsWith(path)) {

            val message = String.format(
                "Error when sending notification (id = %s). Link must starting with required path",
                notification.notificationId
            )

            this.validateResponse(
                ResponseEntity<NotificationResponse>(
                    NotificationResponse.builder()
                        .message(message)
                        .notificationId(notification.notificationId)
                        .build(),
                    HttpStatusCode.valueOf(500)
                )
            )
        } else if (notification.consumerEmail == null) {

            val message = String.format(
                "Error when sending notification (id = %s) to user. Email must be not null",
                notification.notificationId
            )

            this.validateResponse(
                ResponseEntity<NotificationResponse>(
                    NotificationResponse.builder()
                        .message(message)
                        .notificationId(notification.notificationId)
                        .build(),
                    HttpStatusCode.valueOf(404)
                )
            )
        } else {

            Mono.just(notification.consumerEmail)
                .flatMap { dbFind: String? ->
                    template.exists(query(where("email").`is`(dbFind!!)), User::class.java)
                }
                .flatMap { userExists: Boolean ->

                    if (java.lang.Boolean.TRUE == userExists && notification.consumerEmail != null) {

                        val message = String.format(
                            "Notification (id = %s) was successfully sent to the user with the email = %s",
                            notification.notificationId,
                            notification.consumerEmail
                        )

                        this.validateResponse(
                            ResponseEntity(
                                NotificationResponse.builder()
                                    .message(message)
                                    .notificationId(notification.notificationId)
                                    .build(),
                                HttpStatusCode.valueOf(200)
                            )
                        )
                    } else {

                        val message = String.format(
                            "Error when sending notification (id = %s) to user. " +
                                    "This notification intended for another user",
                            notification.notificationId
                        )

                        this.validateResponse(
                            ResponseEntity(
                                NotificationResponse.builder()
                                    .message(message)
                                    .notificationId(notification.notificationId)
                                    .build(),
                                HttpStatusCode.valueOf(404)
                            )
                        )
                    }
                    Mono.empty<Any?>()
                }.block()
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