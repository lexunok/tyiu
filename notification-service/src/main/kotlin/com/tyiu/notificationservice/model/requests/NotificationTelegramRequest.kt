package com.tyiu.notificationservice.model.requests

class NotificationTelegramRequest (
    val tag: String? = null,
    val title: String? = null,
    val message: String? = null,
    val link: String? = null
)