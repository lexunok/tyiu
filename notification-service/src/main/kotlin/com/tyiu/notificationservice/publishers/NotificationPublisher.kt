package com.tyiu.notificationservice.publishers

import com.tyiu.notificationservice.config.TOPIC_TO_EXCHANGE
import com.tyiu.notificationservice.models.NotificationDTO
import com.tyiu.notificationservice.models.requests.Notification
import com.tyiu.notificationservice.models.requests.NotificationEmailRequest
import kotlinx.coroutines.flow.Flow
import org.springframework.amqp.rabbit.core.RabbitTemplate
import org.springframework.stereotype.Component

@Component
class NotificationPublisher(private val rabbitTemplate: RabbitTemplate) {

    suspend fun sendUnreadNotification(tag: String, notificationsDTO: Flow<NotificationDTO>){
        rabbitTemplate.convertAndSend(
            TOPIC_TO_EXCHANGE, "telegram", notificationsDTO
        )
    }

    suspend fun sendNewNotification(notification: NotificationDTO){
        rabbitTemplate.convertAndSend(
            TOPIC_TO_EXCHANGE, "telegram", Notification(
                notification.title,
                notification.message,
                notification.link
            )
        )
        rabbitTemplate.convertAndSend(
            TOPIC_TO_EXCHANGE, "email", NotificationEmailRequest(
                notification.consumerEmail,
                notification.title,
                notification.message,
                notification.link,
                notification.buttonName
            )
        )
    }
}