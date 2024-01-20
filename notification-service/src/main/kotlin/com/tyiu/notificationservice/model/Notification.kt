package com.tyiu.notificationservice.model

import kotlinx.coroutines.flow.Flow
import lombok.Getter
import lombok.Setter
import org.springframework.data.annotation.Id
import org.springframework.data.r2dbc.repository.Query
import org.springframework.data.relational.core.mapping.Table
import org.springframework.data.repository.kotlin.CoroutineCrudRepository
import java.time.LocalDateTime

interface NotificationRepository: CoroutineCrudRepository<Notification, String> {

    @Query("SELECT * FROM notification n WHERE n.publisher_email = :email OR n.consumer_email = :email ORDER BY n.created_at ASC")
    fun findAllByPublisherEmailOrConsumerEmail(email: String): Flow<Notification>

//    @Query("SELECT * FROM notification n WHERE n.consumer_tag = :tag AND n.is_read = false ORDER BY n.created_at ASC")
//    fun findAllUnreadNotificationsByTag(tag: String): Flow<Notification>

//    @Query("SELECT * FROM notification n WHERE n.consumer_email = :tag AND n.is_read = false ORDER BY n.created_at ASC")
//    fun findAllUnreadNotificationsByEmail(email: String): Flow<Notification>
}

enum class NotificationType{
    SUCCESS,
    ERROR,
}

@Table
@Getter
@Setter
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
    var notificationType: NotificationType
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
    var notificationType: NotificationType
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