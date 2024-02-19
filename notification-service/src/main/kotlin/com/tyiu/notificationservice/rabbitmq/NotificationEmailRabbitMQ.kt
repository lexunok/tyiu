package com.tyiu.notificationservice.rabbitmq

import com.tyiu.notificationservice.service.NotificationService
import interfaces.INotification
import org.slf4j.Logger
import org.slf4j.LoggerFactory
import org.springframework.amqp.rabbit.annotation.RabbitListener
import org.springframework.amqp.rabbit.core.RabbitTemplate
import org.springframework.beans.factory.annotation.Value
import org.springframework.http.HttpStatus
import response.ResponseEntity
import org.springframework.stereotype.Component
import request.NotificationRequest
import response.NotificationResponse

@Component("prodEmailClient")
class NotificationEmailRabbitMQ(private val rabbitTemplate: RabbitTemplate,
                                private val notificationService: NotificationService
): INotification {

    private val log: Logger = LoggerFactory.getLogger(NotificationEmailRabbitMQ::class.java)

    @Value("\${rabbitmq.topic}")
    private var topic: String = "topic"


    @Value("\${rabbitmq.routes.email.make}")
    private var makeEmailNotificationRoute: String = "emailRoute"


    override fun makeNotification(notificationRequest: NotificationRequest){
        rabbitTemplate.convertAndSend(
            topic, makeEmailNotificationRoute, notificationRequest
        )
    }

    @RabbitListener(queues = ["\${rabbitmq.queues.email.validate}"])
    override fun validateResponse(response: ResponseEntity<NotificationResponse>) {
        if (HttpStatus.valueOf(response.status.toString()).is2xxSuccessful){
            response.body?.let { notificationService.setSentByEmailServiceFieldTrue(it.notificationId) }
        } else{
            log.error(response.body?.message)
        }
    }

}