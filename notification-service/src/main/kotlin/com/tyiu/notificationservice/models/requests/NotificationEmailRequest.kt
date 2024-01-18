package com.tyiu.notificationservice.models.requests

class NotificationEmailRequest (
    val consumerEmail: String? = null,
    val title: String? = null,
    val message: String? = null,
    val link: String? = null,
    val buttonName: String? = null,
)