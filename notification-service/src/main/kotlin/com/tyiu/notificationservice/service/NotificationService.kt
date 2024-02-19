package com.tyiu.notificationservice.service

import com.tyiu.notificationservice.model.*
import kotlinx.coroutines.flow.Flow
import kotlinx.coroutines.flow.firstOrNull
import kotlinx.coroutines.flow.map
import org.springframework.stereotype.Service
import request.NotificationRequest

@Service
class NotificationService(private val notificationRepository: NotificationRepository,
                            private val userTagRepository: UserTagRepository) {

    fun getAllNotifications(): Flow<NotificationDTO> =
        notificationRepository.findAll().map { notification ->
            val notificationDto = notification.toDTO()
            notificationDto.isSentByEmailService = notification.isSentByEmailService
            notificationDto.isSentByTelegramService = notification.isSentByTelegramService
            return@map notificationDto
        }

    suspend fun createNotification(notification: NotificationRequest): NotificationRequest{
        val notificationRequest = notificationRepository.save(notification.toEntity()).toNotificationRequest();
        val userTag = userTagRepository.findUserTelegramTag(notificationRequest.consumerEmail).firstOrNull()
        if (userTag != null) {
            notificationRequest.tag = userTag
        }
        return notificationRequest
    }

    fun setSentByTelegramServiceFieldTrue(id: String){
        notificationRepository.setSentByTelegramServiceFieldTrue(id)
    }

    fun setSentByEmailServiceFieldTrue(id: String){
        notificationRepository.setSentByEmailServiceFieldTrue(id)
    }

    fun getAllNotificationsByEmail(email: String): Flow<NotificationDTO> {
        return notificationRepository.findAllNotificationsByEmail(email).map { n -> n.toDTO() }
    }
}
