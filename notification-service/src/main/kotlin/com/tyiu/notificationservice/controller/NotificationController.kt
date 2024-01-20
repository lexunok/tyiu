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
            "konstanta1337",
            "timur.minyazeff@gmail.com",
            "golngpostal",
            "Приглашение в команду",
            "Крутой чел, скиловый",
            "https://hits.tyuiu.ru/profile/7b79071b-91ab-4db1-930a-e94a9e89c8f1",
            null,
            null,
            null,
            null,
            "Присоединиться",
            NotificationType.SUCCESS,
        );

        notificationPublisher.sendNewNotificationToEmail(notificationDTO)
        notificationPublisher.sendNewNotificationToTelegram(notificationDTO)
    }

}