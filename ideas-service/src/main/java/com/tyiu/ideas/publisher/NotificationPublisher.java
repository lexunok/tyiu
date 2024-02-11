package com.tyiu.ideas.publisher;

import lombok.RequiredArgsConstructor;
import org.springframework.amqp.rabbit.core.RabbitTemplate;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import reactor.core.publisher.Mono;
import request.NotificationRequest;

@Component
@RequiredArgsConstructor
public class NotificationPublisher {
    @Autowired
    private final RabbitTemplate rabbitTemplate;

    @Value("${rabbitmq.notification}")
    private final String route;

    @Value("${rabbitmq.exchange}")
    private final String topic;

    public Mono<Void> makeNotification(NotificationRequest notificationRequest){
        return Mono.fromRunnable(() -> rabbitTemplate.convertAndSend(topic, route, notificationRequest));
    }

}
