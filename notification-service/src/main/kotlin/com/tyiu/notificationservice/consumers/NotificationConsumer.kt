package com.tyiu.notificationservice.consumers

import com.tyiu.notificationservice.config.QUEUE_NOTIFICATION
import com.tyiu.notificationservice.config.QUEUE_TELEGRAM
import com.tyiu.notificationservice.models.NotificationDTO
import com.tyiu.notificationservice.publishers.NotificationPublisher
import com.tyiu.notificationservice.services.NotificationService
import org.springframework.amqp.rabbit.annotation.RabbitListener
import org.springframework.stereotype.Component

@Component
class NotificationConsumer(private val notificationService: NotificationService,
                           private val notificationPublisher: NotificationPublisher) {
    @RabbitListener(queues = [QUEUE_NOTIFICATION])
    suspend fun receiveNotification(notification: NotificationDTO){
        notificationService.createNotification(notification)
    }
    @RabbitListener(queues = [QUEUE_TELEGRAM])
    suspend fun receiveTelegramTag(tag: String){
        notificationPublisher.sendUnreadNotification(tag, notificationService.getUnreadNotificationsByTag(tag))
    }
}