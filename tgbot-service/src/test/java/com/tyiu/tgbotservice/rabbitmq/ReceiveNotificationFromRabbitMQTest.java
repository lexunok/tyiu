package com.tyiu.tgbotservice.rabbitmq;

import interfaces.INotificationRabbitMQ;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.springframework.boot.test.context.SpringBootTest;
import org.testcontainers.junit.jupiter.Testcontainers;
import requests.NotificationRequest;

import static org.springframework.boot.test.context.SpringBootTest.WebEnvironment.RANDOM_PORT;

@Testcontainers
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringBootTest(webEnvironment = RANDOM_PORT)
public class ReceiveNotificationFromRabbitMQTest extends TestContainersRabbitMQ {

    private NotificationRequest createNotification(String tag, String title, String message, String link) {

        return NotificationRequest.builder()
                .tag(tag)
                .title(title)
                .message(message)
                .link(link)
                .build();
    }

    @Test
    void testNotificationListener() {

        NotificationRequest notification1 = createNotification("123", "title", "message", "link");
        NotificationRequest notification2 = createNotification("321", null, null, null);
        NotificationRequest notification3 = createNotification(null, "title", "message", "link");

        INotificationRabbitMQ testRabbitMQ = new TestRabbitMQ();
    }
}
