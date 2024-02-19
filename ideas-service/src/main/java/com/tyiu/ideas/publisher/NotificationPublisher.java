package com.tyiu.ideas.publisher;

import lombok.extern.slf4j.Slf4j;
import org.springframework.amqp.rabbit.core.RabbitTemplate;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import reactor.core.publisher.Mono;
import request.NotificationRequest;

@Slf4j
@Component
public class NotificationPublisher {
    @Autowired
    private RabbitTemplate rabbitTemplate;

    @Value("${rabbitmq.notification}")
    private String route;

    @Value("${rabbitmq.exchange}")
    private String topic;

    public Mono<Void> makeNotification(NotificationRequest notificationRequest){
        log.error(notificationRequest.getMessage());
        rabbitTemplate.convertAndSend(topic, route, notificationRequest);
        return Mono.empty();
    }

}
