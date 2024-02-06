package com.tyiu.notificationservice.controller

import com.tyiu.notificationservice.model.NotificationDTO
import com.tyiu.notificationservice.service.NotificationService
import kotlinx.coroutines.flow.Flow
import org.springframework.web.bind.annotation.GetMapping
import org.springframework.web.bind.annotation.PathVariable
import org.springframework.web.bind.annotation.RequestMapping
import org.springframework.web.bind.annotation.RestController

@RestController
@RequestMapping("/api/v1/notification")
class NotificationController(private val notificationService: NotificationService) {
    @GetMapping("/all/{email}")
    suspend fun getAllNotificationsByUserEmail(@PathVariable email: String): Flow<NotificationDTO> =
        notificationService.getAllNotificationsByEmail(email)

    @GetMapping("/all")
    suspend fun getAllNotification(): Flow<NotificationDTO> =
        notificationService.getAllNotifications()
}