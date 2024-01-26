package com.tyiu.tgbotservice;

import com.tyiu.tgbotservice.model.NotificationTelegramResponse;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.springframework.amqp.rabbit.core.RabbitTemplate;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.DynamicPropertyRegistry;
import org.springframework.test.context.DynamicPropertySource;
import org.testcontainers.containers.RabbitMQContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;
import org.testcontainers.utility.DockerImageName;

import java.util.concurrent.TimeUnit;

import static org.junit.jupiter.api.Assertions.*;
import static org.testcontainers.shaded.org.awaitility.Awaitility.await;

@Testcontainers
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
class TelegramTagControllerTest {

	@Value("${rabbitmq.exchange}")
	private String exchange;
	@Value("${rabbitmq.routes.receive.new}")
	private String routingKey;

	@Autowired
	private RabbitTemplate rabbitTemplate;

	@Container
	static RabbitMQContainer container = new RabbitMQContainer(
			DockerImageName.parse("rabbitmq").withTag("3.12.12-management")
	);

	@DynamicPropertySource
	static void configure(DynamicPropertyRegistry registry) {
		registry.add("rabbitmq.host", container::getHost);
		registry.add("rabbitmq.port", container::getAmqpPort);
	}

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
		rabbitTemplate.convertAndSend(exchange, routingKey, notification1);

		await().atMost(10, TimeUnit.SECONDS).untilAsserted(() -> {
            assertEquals("123", notification1.getTag());
			assertEquals("title", notification1.getTitle());
			assertEquals("message", notification1.getMessage());
			assertEquals("link", notification1.getLink());
		});

		NotificationTelegramResponse notification2 = createNotification("qqq", "title", "message", "link");
		rabbitTemplate.convertAndSend(exchange, routingKey, notification2);

		await().atMost(10, TimeUnit.SECONDS).untilAsserted(() -> {
			assertEquals("qqq", notification2.getTag());
			assertEquals("title", notification2.getTitle());
			assertEquals("message", notification2.getMessage());
			assertEquals("link", notification2.getLink());
		});

		NotificationTelegramResponse notification3 = createNotification(null, "title", "message", "link");
		rabbitTemplate.convertAndSend(exchange, routingKey, notification3);

		await().atMost(10, TimeUnit.SECONDS).untilAsserted(() -> {
            assertNull(notification3.getTag());
			assertEquals("title", notification3.getTitle());
			assertEquals("message", notification3.getMessage());
			assertEquals("link", notification3.getLink());
		});
	}
}
