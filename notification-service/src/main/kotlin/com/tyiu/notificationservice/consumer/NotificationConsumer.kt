package com.tyiu.notificationservice.consumer

import com.tyiu.notificationservice.config.QUEUE_NOTIFICATION
import com.tyiu.notificationservice.config.QUEUE_TELEGRAM
import com.tyiu.notificationservice.model.NotificationDTO
import com.tyiu.notificationservice.publisher.NotificationPublisher
import com.tyiu.notificationservice.service.NotificationService
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