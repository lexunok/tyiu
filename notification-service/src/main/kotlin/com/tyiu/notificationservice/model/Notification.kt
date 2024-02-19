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

    @Query("SELECT * FROM notification n WHERE n.consumer_email = :email ORDER BY n.created_at ASC")
    fun findAllNotificationsByEmail(email: String): Flow<Notification>

    @Query("UPDATE notification n SET n.is_sent_by_telegram_service = true WHERE n.id = :id")
    fun setSentByTelegramServiceFieldTrue(id: String)

    @Query("UPDATE notification n SET n.is_sent_by_email_service = true WHERE n.id = :id")
    fun setSentByEmailServiceFieldTrue(id: String)
}

interface UserTagRepository: CoroutineCrudRepository<UserTelegram, String> {

    @Query("SELECT user_tag FROM users_telegram WHERE user_email = :email LIMIT 1")
    fun findUserTelegramTag(email: String): Flow<String>
}

@Getter
@Setter
@Table("notification")
data class Notification (
    @Id
    var id: String? = null,
    var publisherEmail: String? = null,
    var consumerEmail: String? = null,
    var title: String? = null,
    var message: String? = null,
    var link: String? = null,
    var isShowed: Boolean? = null,
    var isRead: Boolean? = null,
    var isFavourite: Boolean? = null,
    var createdAt: LocalDateTime? = LocalDateTime.now(),
    var buttonName: String? = null,
    var isSentByTelegramService: Boolean? = null,
    var isSentByEmailService: Boolean? = null
)



data class NotificationDTO (
    @Id
    var id: String? = null,
    var publisherEmail: String? = null,
    var consumerEmail: String? = null,
    var title: String? = null,
    var message: String? = null,
    var link: String? = null,
    var isShowed: Boolean? = null,
    var isRead: Boolean? = null,
    var isFavourite: Boolean? = null,
    var createdAt: LocalDateTime? = LocalDateTime.now(),
    var buttonName: String? = null,
    var isSentByTelegramService: Boolean? = null,
    var isSentByEmailService: Boolean? = null
)

fun Notification.toDTO(): NotificationDTO = NotificationDTO(
    id = id,
    publisherEmail = publisherEmail,
    consumerEmail = consumerEmail,
    title = title,
    message = message,
    link = link,
    isRead = isRead,
    isFavourite = isFavourite,
    isShowed = isShowed,
    createdAt = createdAt,
    buttonName = buttonName,
)

fun NotificationDTO.toEntity(): Notification = Notification(
    publisherEmail = publisherEmail,
    consumerEmail = consumerEmail,
    title = title,
    message = message,
    link = link,
    isRead = isRead,
    isFavourite = isFavourite,
    isShowed = isShowed,
    createdAt = createdAt,
    buttonName = buttonName,
)

fun NotificationDTO.toNotificationRequest(): NotificationRequest = NotificationRequest(
    id,
    consumerEmail,
    null,
    title,
    message,
    link,
    buttonName,
    publisherEmail
)

fun NotificationRequest.toEntity(): Notification = Notification(
    consumerEmail = consumerEmail,
    title = title,
    message = message,
    link = link,
    buttonName = buttonName,
    publisherEmail = publisherEmail,
    createdAt = LocalDateTime.now(),
    isFavourite = false,
    isRead = false,
    isShowed = false,
)
fun Notification.toNotificationRequest(): NotificationRequest = NotificationRequest(
    id,
    consumerEmail,
    null,
    title,
    message,
    link,
    buttonName,
    publisherEmail
)

