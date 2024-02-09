package com.tyiu.tgbotservice.rabbitmq.config;

import com.tyiu.tgbotservice.rabbitmq.TestRabbitMQ;
import interfaces.INotification;
import org.springframework.boot.test.context.TestConfiguration;

@TestConfiguration
public class TestConfig {
    public INotification testClient() {
        return new TestRabbitMQ();
    }
}
