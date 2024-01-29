package com.tyiu.tgbotservice;

import com.tyiu.tgbotservice.model.NotificationTelegramResponse;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.springframework.amqp.rabbit.core.RabbitTemplate;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.core.ParameterizedTypeReference;
import org.testcontainers.junit.jupiter.Testcontainers;

import static org.springframework.boot.test.context.SpringBootTest.WebEnvironment.RANDOM_PORT;

@Testcontainers
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringBootTest(webEnvironment = RANDOM_PORT)
public class ReceiveNotificationFromRabbitMQTest extends TestContainers {

    @Value("${rabbitmq.queue.receive.new}")
    private String queue;
    @Value("${rabbitmq.exchange}")
    private String exchange;
    @Value("${rabbitmq.routes.receive.new}")
    private String routingKey;

    @Autowired
    private RabbitTemplate rabbitTemplate;

    private NotificationTelegramResponse createNotification(String tag, String title, String message, String link) {

        return NotificationTelegramResponse.builder()
                .tag(tag)
                .title(title)
                .message(message)
                .link(link)
                .build();
    }

    private void sendNotificationToRabbitMQ(NotificationTelegramResponse notification) {
        rabbitTemplate.convertAndSend(exchange, routingKey, notification);
    }

    private void readNotificationFromRabbitMQ() {
        ParameterizedTypeReference<NotificationTelegramResponse> notification = new ParameterizedTypeReference<>() {};
        rabbitTemplate.receiveAndConvert(queue, 1000, notification);
    }



    @Test
    void testNotificationListener() {

        NotificationTelegramResponse sentNotification = createNotification("123", "title", "message", "link");
        sendNotificationToRabbitMQ(sentNotification);

        readNotificationFromRabbitMQ();
        // Без понятия как это тестировать
    }
}
