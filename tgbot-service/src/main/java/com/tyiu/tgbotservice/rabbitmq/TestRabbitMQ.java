package com.tyiu.tgbotservice.rabbitmq;

import com.tyiu.tgbotservice.model.request.NotificationTelegramResponse;
import com.tyiu.tgbotservice.model.response.Response;

public class TestRabbitMQ implements InterfaceRabbitMQ {

    @Override
    public Response listenToNewNotification(NotificationTelegramResponse notification) {

        if (notification.getTag() == null) {
            return new Response("Tag is null!");
        }
        else {
            return new Response("OK");
        }
    }
}
