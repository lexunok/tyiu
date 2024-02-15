package com.tyiu.notificationservice.model

import com.tyiu.tgbotservice.model.entities.UserTelegram
import kotlinx.coroutines.flow.Flow
import lombok.*
import org.springframework.data.annotation.Id
import org.springframework.data.r2dbc.repository.Query
import org.springframework.data.relational.core.mapping.Table
import org.springframework.data.repository.kotlin.CoroutineCrudRepository
import request.NotificationRequest
import java.time.LocalDateTime

interface NotificationRepository: CoroutineCrudRepository<Notification, String> {

    @Query("SELECT * FROM notification n WHERE n.publisher_email = :email OR n.consumer_email = :email ORDER BY n.created_at ASC")
    fun findAllByPublisherEmailOrConsumerEmail(email: String): Flow<Notification>

    @Query("SELECT * FROM notification n WHERE n.consumer_tag = :tag AND n.is_read = :isRead ORDER BY n.created_at ASC")
    fun findAllUnreadNotificationsByTag(tag: String, isRead: Boolean = false): Flow<Notification>

    @Query("SELECT * FROM notification n WHERE n.consumer_email = :tag AND n.is_read = false ORDER BY n.created_at ASC")
    fun findAllUnreadNotificationsByEmail(email: String): Flow<Notification>

    @Query("UPDATE notification n SET n.is_sent_by_telegram_service = true WHERE n.id = :id")
    fun setSentByTelegramServiceFieldTrue(id: String)

    @Query("UPDATE notification n SET n.is_sent_by_email_service = true WHERE n.id = :id")
    fun setSentByEmailServiceFieldTrue(id: String)
}

interface UserTagRepository: CoroutineCrudRepository<UserTelegram, String> {

    @Query("SELECT consumer_tag FROM users_telegram WHERE consumer_email = :email LIMIT 1")
    fun findUserTelegramTag(email: String): String
}

enum class NotificationType{
    SUCCESS,
    ERROR,
}

@Getter
@Setter
@Table("notification")
data class Notification (
    @Id
    var id: String? = null,
    var publisherEmail: String? = null,
    var consumerEmail: String? = null,
    var consumerTag: String? = null,
    var title: String? = null,
    var message: String? = null,
    var link: String? = null,
    var isShowed: Boolean? = null,
    var isRead: Boolean? = null,
    var isFavourite: Boolean? = null,
    var createdAt: LocalDateTime? = LocalDateTime.now(),
    var buttonName: String? = null,
    var notificationType: NotificationType,
    var isSentByTelegramService: Boolean? = null,
    var isSentByEmailService: Boolean? = null
)



data class NotificationDTO (
    @Id
    var id: String? = null,
    var publisherEmail: String? = null,
    var consumerEmail: String? = null,
    var consumerTag: String? = null,
    var title: String? = null,
    var message: String? = null,
    var link: String? = null,
    var isShowed: Boolean? = null,
    var isRead: Boolean? = null,
    var isFavourite: Boolean? = null,
    var createdAt: LocalDateTime? = LocalDateTime.now(),
    var buttonName: String? = null,
    var notificationType: NotificationType,
    var isSentByTelegramService: Boolean? = null,
    var isSentByEmailService: Boolean? = null
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

fun NotificationDTO.toNotificationRequest(): NotificationRequest = NotificationRequest(
    id,
    consumerEmail,
    consumerTag,
    title,
    message,
    link,
    buttonName,
    publisherEmail
)