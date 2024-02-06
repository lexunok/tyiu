package com.tyiu.tgbotservice.rabbitmq;

import org.springframework.test.context.DynamicPropertyRegistry;
import org.springframework.test.context.DynamicPropertySource;
import org.testcontainers.containers.RabbitMQContainer;
import org.testcontainers.utility.DockerImageName;

public abstract class TestContainersRabbitMQ {

    static RabbitMQContainer container;

    static {
        container = new RabbitMQContainer(DockerImageName
                .parse("rabbitmq")
                .withTag("3.12.12-management"))
        .withAdminPassword("verySecretPassword");

        container.start();
    }

    @DynamicPropertySource
    static void setProperties(DynamicPropertyRegistry registry) {

        registry.add("rabbitmq.host", container::getHost);
        registry.add("rabbitmq.port", container::getAmqpPort);
    }
}
