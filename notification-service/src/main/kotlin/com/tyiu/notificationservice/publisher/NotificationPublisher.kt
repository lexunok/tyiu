package com.tyiu.notificationservice.publisher

import com.tyiu.notificationservice.config.TOPIC_TO_EXCHANGE
import com.tyiu.notificationservice.model.NotificationDTO
import com.tyiu.notificationservice.model.requests.NotificationTelegramRequest
import com.tyiu.notificationservice.model.requests.NotificationEmailRequest
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

    suspend fun sendNewNotificationToTelegram(notification: NotificationDTO){
        rabbitTemplate.convertAndSend(
            TOPIC_TO_EXCHANGE, "telegram", NotificationTelegramRequest(
                notification.title,
                notification.message,
                notification.link
            )
        )
    }
    suspend fun sendNewNotificationToEmail(notification: NotificationDTO){
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