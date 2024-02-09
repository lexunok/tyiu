package com.tyiu.tgbotservice.rabbitmq;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.test.context.SpringBootTest;
import org.testcontainers.shaded.com.google.common.collect.ImmutableMap;

import static org.assertj.core.api.Assertions.*;
import static org.springframework.boot.test.context.SpringBootTest.WebEnvironment.RANDOM_PORT;

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringBootTest(webEnvironment = RANDOM_PORT)
class RabbitMQContainerTest extends TestContainersRabbitMQ {

	@Value("${rabbitmq.queue.receive.new}")
	private String queue;

	@Test
	void testStartContainer() {

		assertThat(container.getAdminUsername()).isEqualTo("guest");
		assertThat(container.getAdminPassword()).isEqualTo("verySecretPassword");

		assertThat(container.isRunning()).isTrue();
	}

	@Test
	void testWrongMessageFormat() {

		assertThatCode(() -> container.withQueue(queue,
				true,
				false,
				ImmutableMap.of("message from rabbitMQ", container)))
				.hasMessageStartingWith("Failed to convert arguments into json");
	}
}
