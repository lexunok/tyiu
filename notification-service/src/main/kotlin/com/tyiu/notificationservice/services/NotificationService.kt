package com.tyiu.notificationservice.services

import com.tyiu.notificationservice.models.NotificationDTO
import com.tyiu.notificationservice.models.NotificationRepository
import com.tyiu.notificationservice.models.toDTO
import com.tyiu.notificationservice.models.toEntity
import com.tyiu.notificationservice.publishers.NotificationPublisher
import kotlinx.coroutines.flow.Flow
import kotlinx.coroutines.flow.map
import org.springframework.stereotype.Service

@Service
class NotificationService(private val notificationRepository: NotificationRepository,
                          private val notificationPublisher: NotificationPublisher) {
    private val path = "https://hits.tyuiu.ru/"

    suspend fun getAllNotificationsByEmail(email: String): Flow<NotificationDTO> =
        notificationRepository.findAllByPublisherEmailOrConsumerEmail(email).map { notification -> notification.toDTO() }

    suspend fun getAllNotifications(): Flow<NotificationDTO> =
        notificationRepository.findAll().map { notification -> notification.toDTO() }

    suspend fun getUnreadNotificationsByTag(tag: String): Flow<NotificationDTO> =
        notificationRepository.findAllUnreadNotificationsByTag(tag).map { notification -> notification.toDTO() }

    suspend fun createNotification(notificationDTO: NotificationDTO){
        notificationRepository.save(notificationDTO.toEntity())
        notificationPublisher.sendNewNotification(notificationDTO)
    }
}