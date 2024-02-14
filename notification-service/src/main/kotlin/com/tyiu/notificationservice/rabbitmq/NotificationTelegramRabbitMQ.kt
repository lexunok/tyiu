package com.tyiu.notificationservice.rabbitmq

import com.tyiu.notificationservice.service.NotificationService
import com.tyiu.tgbotservice.model.entities.UserTelegram
import interfaces.INotification
import org.slf4j.Logger
import org.slf4j.LoggerFactory
import org.springframework.amqp.rabbit.annotation.RabbitListener
import org.springframework.amqp.rabbit.core.RabbitTemplate
import org.springframework.beans.factory.annotation.Value
import org.springframework.data.r2dbc.core.R2dbcEntityTemplate
import org.springframework.data.relational.core.query.Criteria.where
import org.springframework.data.relational.core.query.Query.query
import org.springframework.http.ResponseEntity
import org.springframework.stereotype.Component
import reactor.core.publisher.Mono
import request.NotificationRequest
import response.NotificationResponse

abstract class AbstractNotificationTelegram: INotification


@Component("prodTelegramClient")
class NotificationTelegramRabbitMQ(private val rabbitTemplate: RabbitTemplate,
                                   private val notificationService: NotificationService,
                                   private val template: R2dbcEntityTemplate):
    INotification {

    private val log: Logger = LoggerFactory.getLogger(NotificationTelegramRabbitMQ::class.java)

    @Value("\${rabbitmq.topic}")
    private var topic: String = "topic"

    @Value("\${rabbitmq.routes.telegram.make}")
    private var makeTelegramNotificationRoute: String = "sendNewRoute"

    override fun makeNotification(notificationRequest: NotificationRequest) {

        Mono.just(notificationRequest.consumerEmail)
            .flatMap { consumerEmail: String? ->
                template.selectOne(query(where("user_email").`is`(consumerEmail!!)), UserTelegram::class.java)
                    .flatMap { userTelegram: UserTelegram ->

                        notificationRequest.tag = userTelegram.userTag
                        Mono.empty<Any?>()
                    }
            }

        rabbitTemplate.convertAndSend(
            topic, makeTelegramNotificationRoute, notificationRequest
        )
    }

    @RabbitListener(queues = ["\${rabbitmq.queues.telegram.validate}"])
    override fun validateResponse(response: ResponseEntity<NotificationResponse>) {
        if (response.statusCode.is2xxSuccessful){
            response.body?.let { notificationService.setSentByTelegramServiceFieldTrue(it.notificationId) }
        } else {
            log.error(response.body?.message)
        }
    }
}