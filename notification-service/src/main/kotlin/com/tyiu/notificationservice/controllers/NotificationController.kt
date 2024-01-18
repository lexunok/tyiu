package com.tyiu.notificationservice.controllers

import com.tyiu.notificationservice.models.NotificationDTO
import com.tyiu.notificationservice.models.NotificationType
import com.tyiu.notificationservice.publishers.NotificationPublisher
import com.tyiu.notificationservice.services.NotificationService
import kotlinx.coroutines.flow.Flow
import org.springframework.web.bind.annotation.GetMapping
import org.springframework.web.bind.annotation.PathVariable
import org.springframework.web.bind.annotation.RequestMapping
import org.springframework.web.bind.annotation.RestController

@RestController
@RequestMapping("/api/v1/notification")
class NotificationController(private val notificationService: NotificationService,
                             private val notificationPublisher: NotificationPublisher) {
    @GetMapping("/all/{email}")
    suspend fun getAllNotificationsByUserEmail(@PathVariable email: String): Flow<NotificationDTO> =
        notificationService.getAllNotificationsByEmail(email)

    @GetMapping("/all")
    suspend fun getAllNotification(): Flow<NotificationDTO> =
        notificationService.getAllNotifications()

    @GetMapping("/p")
    suspend fun sendNotificationToTGEMAIL() {
        val notificationDTO: NotificationDTO = NotificationDTO(
            null,
            "gandon",
            "kostagarifullin275@gmail.com",
            "konstanta172",
            "title",
            "Сасай",
            "https://www.youtube.com/watch?v=1-kKTOr5mcU",
            null,
            null,
            null,
            null,
            null,
            NotificationType.SUCCESS,
        );

        notificationPublisher.sendNewNotification(notificationDTO)
    }

}