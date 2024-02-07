package com.tyiu.notificationservice.rabbitmq

import com.tyiu.notificationservice.service.NotificationService
import interfaces.INotification
import org.slf4j.Logger
import org.slf4j.LoggerFactory
import org.springframework.amqp.rabbit.annotation.RabbitListener
import org.springframework.amqp.rabbit.core.RabbitTemplate
import org.springframework.beans.factory.annotation.Value
import org.springframework.http.ResponseEntity
import org.springframework.stereotype.Component
import request.NotificationRequest
import response.NotificationResponse

abstract class AbstractNotificationEmail: INotification

@Component
class NotificationEmailRabbitMQ(private val rabbitTemplate: RabbitTemplate,
                                private val notificationService: NotificationService
): AbstractNotificationEmail() {

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

    @RabbitListener(queues = ["\${rabbitmq.queues.email.validate"])
    override fun validateResponse(response: ResponseEntity<NotificationResponse>) {
        if (response.statusCode.is2xxSuccessful){
            response.body?.let { notificationService.setSentByEmailServiceFieldTrue(it.notificationId) }
        } else{
            log.error(response.body?.message)
        }
    }

}