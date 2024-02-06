package com.tyiu.notificationservice.rabbitmq

import com.tyiu.notificationservice.model.NotificationDTO
import com.tyiu.notificationservice.model.toNotificationRequest
import com.tyiu.notificationservice.service.NotificationService
import org.springframework.amqp.rabbit.annotation.RabbitListener
import org.springframework.stereotype.Component

@Component
class NotificationRabbitMQ(private val notificationService: NotificationService,
                           private val notificationEmailRabbitMQ: NotificationEmailRabbitMQ,
                           private val notificationTelegramRabbitMQ: NotificationTelegramRabbitMQ)
{

    private val path = "https://hits.tyuiu.ru/"

    @RabbitListener(queues = ["\${rabbitmq.queues.notification.receive}"])
    suspend fun receiveNotification(notification: NotificationDTO) {
        val createdNotification = notificationService.createNotification(notification).toNotificationRequest()
        createdNotification.link = path + createdNotification.link
        notificationEmailRabbitMQ.makeNotification(createdNotification)
        notificationTelegramRabbitMQ.makeNotification(createdNotification)
    }
}