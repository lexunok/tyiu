package com.tyiu.notificationservice.rabbitmq

import com.tyiu.notificationservice.service.NotificationService
import interfaces.INotification
import kotlinx.coroutines.runBlocking
import org.springframework.amqp.rabbit.annotation.RabbitListener
import org.springframework.beans.factory.annotation.Qualifier
import org.springframework.stereotype.Component
import request.NotificationRequest


@Component
class NotificationRabbitMQ(private val notificationService: NotificationService,
                           @Qualifier("prodEmailClient") private val notificationEmailRabbitMQ: INotification,
                           @Qualifier("prodTelegramClient") private val notificationTelegramRabbitMQ: INotification)
{

    private val path = "https://hits.tyuiu.ru/"

    @RabbitListener(queues = ["\${rabbitmq.queues.notification.receive}"])
    fun receiveNotification(notificationRequest: NotificationRequest) = runBlocking {

        val createdNotification = notificationService.createNotification(notificationRequest)
        if (createdNotification.link != null){
            createdNotification.link = path + createdNotification.link
        }
        notificationEmailRabbitMQ.makeNotification(createdNotification)
        if (createdNotification.tag != null){
            notificationTelegramRabbitMQ.makeNotification(createdNotification)
        }
    }
}