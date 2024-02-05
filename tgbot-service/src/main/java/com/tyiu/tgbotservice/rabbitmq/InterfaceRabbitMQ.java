package com.tyiu.tgbotservice.rabbitmq;

import com.tyiu.tgbotservice.model.request.NotificationTelegramResponse;
import com.tyiu.tgbotservice.model.response.Response;

public interface InterfaceRabbitMQ {

    Response listenToNewNotification(NotificationTelegramResponse notification);
}
