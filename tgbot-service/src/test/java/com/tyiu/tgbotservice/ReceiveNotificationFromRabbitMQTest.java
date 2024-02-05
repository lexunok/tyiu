package com.tyiu.tgbotservice;

import com.tyiu.tgbotservice.model.request.NotificationTelegramResponse;
import com.tyiu.tgbotservice.rabbitmq.InterfaceRabbitMQ;
import com.tyiu.tgbotservice.rabbitmq.TestRabbitMQ;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.springframework.boot.test.context.SpringBootTest;
import org.testcontainers.junit.jupiter.Testcontainers;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.springframework.boot.test.context.SpringBootTest.WebEnvironment.RANDOM_PORT;

@Testcontainers
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringBootTest(webEnvironment = RANDOM_PORT)
public class ReceiveNotificationFromRabbitMQTest extends TestContainers {

    private NotificationTelegramResponse createNotification(String tag, String title, String message, String link) {

        return NotificationTelegramResponse.builder()
                .tag(tag)
                .title(title)
                .message(message)
                .link(link)
                .build();
    }

    @Test
    void testNotificationListener() {

        NotificationTelegramResponse notification1 = createNotification("123", "title", "message", "link");
        NotificationTelegramResponse notification2 = createNotification("321", null, null, null);
        NotificationTelegramResponse notification3 = createNotification(null, "title", "message", "link");

        InterfaceRabbitMQ testRabbitMQ = new TestRabbitMQ();

        assertEquals("OK", testRabbitMQ.listenToNewNotification(notification1).getMessage());
        assertEquals("OK", testRabbitMQ.listenToNewNotification(notification2).getMessage());
        assertEquals("Tag is null!", testRabbitMQ.listenToNewNotification(notification3).getMessage());
    }
}
