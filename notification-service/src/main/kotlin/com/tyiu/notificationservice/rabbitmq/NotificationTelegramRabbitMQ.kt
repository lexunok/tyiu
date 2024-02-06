package com.tyiu.notificationservice.rabbitmq

import com.tyiu.notificationservice.service.NotificationService
import interfaces.INotificationRabbitMQ
import org.slf4j.Logger
import org.slf4j.LoggerFactory
import org.springframework.amqp.rabbit.annotation.RabbitListener
import org.springframework.amqp.rabbit.core.RabbitTemplate
import org.springframework.beans.factory.annotation.Value
import org.springframework.http.ResponseEntity
import org.springframework.stereotype.Component
import requests.NotificationRequest
import response.NotificationResponse

@Component
class NotificationTelegramRabbitMQ(private val rabbitTemplate: RabbitTemplate,
                                   private val notificationService: NotificationService): INotificationRabbitMQ {
    private val log: Logger = LoggerFactory.getLogger(NotificationTelegramRabbitMQ::class.java)

    @Value("\${rabbitmq.topic}")
    private var topic: String = "topic"

    @Value("\${rabbitmq.routes.telegram.make}")
    private var makeTelegramNotificationRoute: String = "sendNewRoute"

    override fun makeNotification(notificationRequest: NotificationRequest) {
        rabbitTemplate.convertAndSend(
            topic, makeTelegramNotificationRoute, notificationRequest
        )
    }

    @RabbitListener(queues = ["\${rabbitmq.queues.telegram.validate"])
    override fun validateResponse(response: ResponseEntity<NotificationResponse>) {
        if (response.statusCode.is2xxSuccessful){
            response.body?.let { notificationService.setSentByTelegramServiceFieldTrue(it.notificationId) }
        } else {
            log.error(response.body?.message)
        }
    }
}