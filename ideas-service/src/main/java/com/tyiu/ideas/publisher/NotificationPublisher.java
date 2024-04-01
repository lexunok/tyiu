package com.tyiu.ideas.publisher;

import request.NotificationRequest;

import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.beans.factory.annotation.Autowired;

import reactor.core.publisher.Mono;
import org.springframework.amqp.rabbit.core.RabbitTemplate;

@Slf4j
@Component
public class NotificationPublisher {

    @Autowired
    private RabbitTemplate rabbitTemplate;

    @Value("${rabbitmq.notification}")
    private String route;

    @Value("${rabbitmq.exchange}")
    private String topic;

    public Mono<Void> makeNotification(NotificationRequest notificationRequest) {
        rabbitTemplate.convertAndSend(topic, route, notificationRequest);
        return Mono.empty();
    }
}
