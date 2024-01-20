package com.tyiu.notificationservice.publisher

import com.tyiu.notificationservice.model.NotificationDTO
import com.tyiu.notificationservice.model.requests.NotificationTelegramRequest
import com.tyiu.notificationservice.model.requests.NotificationEmailRequest
import kotlinx.coroutines.flow.Flow
import org.springframework.amqp.rabbit.core.RabbitTemplate
import org.springframework.beans.factory.annotation.Value
import org.springframework.stereotype.Component

@Component
class NotificationPublisher(private val rabbitTemplate: RabbitTemplate) {

    @Value("\${rabbitmq.topic}")
    private var topic: String = "topic"

    @Value("\${rabbitmq.routes.telegram.send.new}")
    private var sendNewRoute: String = "sendNewRoute"

//    @Value("\${rabbitmq.routes.telegram.send.unread}")
//    private var sendUnreadRoute: String = "sendUnreadRoute"

    @Value("\${rabbitmq.routes.email.send}")
    private var emailRoute: String = "emailRoute"

//    suspend fun sendUnreadNotification(notificationsDTO: Flow<NotificationDTO>){
//        rabbitTemplate.convertAndSend(
//            topic, sendUnreadRoute, notificationsDTO
//        )
//    }

    suspend fun sendNewNotificationToTelegram(notification: NotificationDTO){
        rabbitTemplate.convertAndSend(
            topic, sendNewRoute, NotificationTelegramRequest(
                notification.title,
                notification.message,
                notification.link
            )
        )
    }
    suspend fun sendNewNotificationToEmail(notification: NotificationDTO){
        rabbitTemplate.convertAndSend(
            topic, emailRoute, NotificationEmailRequest(
                notification.consumerEmail,
                notification.title,
                notification.message,
                notification.link,
                notification.buttonName
            )
        )
    }

}