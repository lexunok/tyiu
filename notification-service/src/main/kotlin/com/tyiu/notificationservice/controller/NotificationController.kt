package com.tyiu.notificationservice.controller

import com.tyiu.notificationservice.model.NotificationDTO
import com.tyiu.notificationservice.model.NotificationType
import com.tyiu.notificationservice.publisher.NotificationPublisher
import com.tyiu.notificationservice.service.NotificationService
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
            "Naked Snake",
            "timur.minyazeff@gmail.com",
            "TimurMA",
            "title",
            "Крутой чел",
            "https://www.youtube.com/watch?v=1-kKTOr5mcU",
            null,
            null,
            null,
            null,
            "Присоединиться в ЧВК 'DiamondDogs'",
            NotificationType.SUCCESS,
        );

        notificationPublisher.sendNewNotificationToEmail(notificationDTO)
        notificationPublisher.sendNewNotificationToTelegram(notificationDTO)
    }

}