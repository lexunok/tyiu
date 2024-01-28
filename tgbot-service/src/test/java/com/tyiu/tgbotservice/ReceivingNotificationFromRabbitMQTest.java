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

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.CoreMatchers.is;

@Testcontainers
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
class ReceivingNotificationFromRabbitMQTest extends TestContainers {

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

	private NotificationTelegramResponse readNotificationFromRabbitMQ() {
		ParameterizedTypeReference<NotificationTelegramResponse> notification = new ParameterizedTypeReference<>() {};
        return rabbitTemplate.receiveAndConvert(queue, 1000, notification);
	}

	@Test
	void testNotificationListener() {

		NotificationTelegramResponse sentNotification = createNotification("123", "title", "message", "link");
		sendNotificationToRabbitMQ(sentNotification);

		NotificationTelegramResponse receivedNotification = readNotificationFromRabbitMQ();
//		assertThat(receivedNotification.getTag(), is("123")); HELP ME PLS
	}
}
