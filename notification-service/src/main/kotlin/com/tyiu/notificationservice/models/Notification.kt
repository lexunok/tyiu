package com.tyiu.notificationservice.models

import kotlinx.coroutines.flow.Flow
import org.springframework.data.annotation.Id
import org.springframework.data.r2dbc.repository.Query
import org.springframework.data.relational.core.mapping.Table
import org.springframework.data.repository.kotlin.CoroutineCrudRepository
import java.time.LocalDateTime

interface NotificationRepository: CoroutineCrudRepository<Notification, String> {

    @Query("SELECT * FROM notification n WHERE n.publisher_email = :email or n.consumer_email ORDER BY created_at ASC")
    fun findAllByPublisherEmailOrConsumerEmail(email: String): Flow<Notification>

    @Query("SELECT * FROM notification n WHERE n.consumer_tag = :tag AND n.is_readORDER BY created_at ASC")
    fun findAllUnreadNotificationsByTag(tag: String): Flow<Notification>
}

enum class NotificationType{
    SUCCESS,
    ERROR,
}

@Table
data class Notification (
    @Id
    val id: String? = null,
    val publisherEmail: String? = null,
    val consumerEmail: String? = null,
    val consumerTag: String? = null,
    val title: String? = null,
    val message: String? = null,
    val link: String? = null,
    val isShowed: Boolean? = null,
    val isRead: Boolean? = null,
    val isFavourite: Boolean? = null,
    val createdAt: LocalDateTime? = LocalDateTime.now(),
    val buttonName: String? = null,
    val notificationType: NotificationType
)

data class NotificationDTO (
    @Id
    val id: String? = null,
    val publisherEmail: String? = null,
    val consumerEmail: String? = null,
    val consumerTag: String? = null,
    val title: String? = null,
    val message: String? = null,
    val link: String? = null,
    val isShowed: Boolean? = null,
    val isRead: Boolean? = null,
    val isFavourite: Boolean? = null,
    val createdAt: LocalDateTime? = LocalDateTime.now(),
    val buttonName: String? = null,
    val notificationType: NotificationType
)

fun Notification.toDTO(): NotificationDTO = NotificationDTO(
    id = id,
    publisherEmail = publisherEmail,
    consumerEmail = consumerEmail,
    consumerTag = consumerTag,
    title = title,
    message = message,
    link = link,
    isRead = isRead,
    isFavourite = isFavourite,
    isShowed = isShowed,
    createdAt = createdAt,
    buttonName = buttonName,
    notificationType = notificationType
)

fun NotificationDTO.toEntity(): Notification = Notification(
    publisherEmail = publisherEmail,
    consumerEmail = consumerEmail,
    consumerTag = consumerTag,
    title = title,
    message = message,
    link = link,
    isRead = isRead,
    isFavourite = isFavourite,
    isShowed = isShowed,
    createdAt = createdAt,
    buttonName = buttonName,
    notificationType = notificationType
)