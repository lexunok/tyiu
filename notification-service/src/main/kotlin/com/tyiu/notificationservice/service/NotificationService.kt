package com.tyiu.notificationservice.service

import com.tyiu.notificationservice.model.NotificationDTO
import com.tyiu.notificationservice.model.NotificationRepository
import com.tyiu.notificationservice.model.toDTO
import com.tyiu.notificationservice.model.toEntity
import com.tyiu.notificationservice.publisher.NotificationPublisher
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

//    suspend fun getUnreadNotificationsByTag(tag: String): Flow<NotificationDTO> =
//        notificationRepository.findAllUnreadNotificationsByTag(tag).map { notification -> notification.toDTO() }

    suspend fun createNotification(notificationDTO: NotificationDTO){
        val notification = notificationDTO.toEntity()
        notification.link = path + notification.link
        val savedNotification = notificationRepository.save(notification)
        notificationPublisher.sendNewNotificationToEmail(savedNotification.toDTO())
        notificationPublisher.sendNewNotificationToTelegram(savedNotification.toDTO())
    }
}