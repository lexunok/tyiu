package com.tyiu.notificationservice.rabbitmq.telegram

import com.tyiu.ideas.config.exception.CustomHttpException
import com.tyiu.ideas.model.entities.User
import interfaces.INotification
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.data.r2dbc.core.R2dbcEntityTemplate
import org.springframework.data.relational.core.query.Criteria
import org.springframework.data.relational.core.query.Query
import org.springframework.http.HttpStatusCode
import org.springframework.http.ResponseEntity
import org.springframework.stereotype.Component
import reactor.core.publisher.Mono
import request.NotificationRequest
import response.NotificationResponse

@Component("testTelegramClient")
class FakeRabbitMQTelegramNotification(@Autowired private val template: R2dbcEntityTemplate): INotification {

    override fun makeNotification(notificationRequest: NotificationRequest) {

        val path = "https://hits.tyuiu.ru/"

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

            Mono.just(notificationRequest.consumerEmail)
                .flatMap { dbFind: String? ->
                    template.exists(Query.query(Criteria.where("email").`is`(dbFind!!)), User::class.java)
                }
                .flatMap { userExists: Boolean ->

                    if (java.lang.Boolean.TRUE == userExists && notificationRequest.tag != null) {

                        val message = String.format(
                            "Notification (id = %s) was successfully sent to the user with the tag = %s",
                            notificationRequest.notificationId,
                            notificationRequest.tag
                        )

                        this.validateResponse(
                            ResponseEntity(
                                NotificationResponse.builder()
                                    .message(message)
                                    .notificationId(notificationRequest.notificationId)
                                    .build(),
                                HttpStatusCode.valueOf(200)
                            )
                        )
                    } else {

                        val message = String.format(
                            "Error when sending notification (id = %s) to user. " +
                                    "This notification intended for another user",
                            notificationRequest.notificationId
                        )

                        this.validateResponse(
                            ResponseEntity(
                                NotificationResponse.builder()
                                    .message(message)
                                    .notificationId(notificationRequest.notificationId)
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