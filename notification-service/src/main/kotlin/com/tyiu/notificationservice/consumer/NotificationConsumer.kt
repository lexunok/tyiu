package com.tyiu.notificationservice.consumer

import com.tyiu.notificationservice.model.NotificationDTO
import com.tyiu.notificationservice.publisher.NotificationPublisher
import com.tyiu.notificationservice.service.NotificationService
import kotlinx.coroutines.flow.Flow
import org.springframework.amqp.rabbit.annotation.RabbitListener
import org.springframework.stereotype.Component

@Component
class NotificationConsumer(private val notificationService: NotificationService,
                           private val notificationPublisher: NotificationPublisher) {
    @RabbitListener(queues = ["\${rabbitmq.queues.notification.receive}"])
    suspend fun receiveNotification(notification: NotificationDTO){
        notificationService.createNotification(notification)
    }
    @RabbitListener(queues = ["\${rabbitmq.queues.telegram.receive}"])
    suspend fun receiveTelegramTag(telegram: Map<String, String>){
        val notifications: Flow<NotificationDTO> = notificationService
                                        .getUnreadNotificationsByTag(telegram["userTag"] as String)
        notificationPublisher.sendUnreadNotification(notifications)
    }
}
