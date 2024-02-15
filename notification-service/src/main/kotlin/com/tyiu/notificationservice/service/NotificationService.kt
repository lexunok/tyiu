package com.tyiu.notificationservice.service

import com.tyiu.notificationservice.model.NotificationDTO
import com.tyiu.notificationservice.model.NotificationRepository
import com.tyiu.notificationservice.model.toDTO
import com.tyiu.notificationservice.model.toEntity
import kotlinx.coroutines.flow.Flow
import kotlinx.coroutines.flow.map
import org.springframework.stereotype.Service

@Service
class NotificationService(private val notificationRepository: NotificationRepository) {

    fun getAllNotificationsByEmail(email: String): Flow<NotificationDTO> =
        notificationRepository.findAllByPublisherEmailOrConsumerEmail(email).map { notification -> notification.toDTO() }

    fun getAllNotifications(): Flow<NotificationDTO> =
        notificationRepository.findAll().map { notification ->
            val notificationDto = notification.toDTO()
            notificationDto.isSentByEmailService = notification.isSentByEmailService
            notificationDto.isSentByTelegramService = notification.isSentByTelegramService
            return@map notificationDto
        }

    suspend fun createNotification(notificationDTO: NotificationDTO): NotificationDTO{
        val notification = notificationDTO.toEntity()
        notification.link = notification.link
        return notificationRepository.save(notification).toDTO()
    }

    fun setSentByTelegramServiceFieldTrue(id: String){
        notificationRepository.setSentByTelegramServiceFieldTrue(id)
    }

    fun setSentByEmailServiceFieldTrue(id: String){
        notificationRepository.setSentByEmailServiceFieldTrue(id)
    }
}
