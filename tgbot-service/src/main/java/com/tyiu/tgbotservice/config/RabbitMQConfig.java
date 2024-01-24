package com.tyiu.tgbotservice.config;

import jakarta.validation.constraints.NotNull;
import org.springframework.amqp.core.*;
import org.springframework.amqp.rabbit.connection.ConnectionFactory;
import org.springframework.amqp.rabbit.core.RabbitTemplate;
import org.springframework.amqp.support.converter.Jackson2JsonMessageConverter;
import org.springframework.amqp.support.converter.MessageConverter;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.util.List;

@Configuration
public class RabbitMQConfig {

    @Value("${rabbitmq.queue.receive.new}")
    private String queueReceiveNew;
//    @Value("${rabbitmq.queue.receive.unread}")
//    private String queueReceiveUnread;
//    @Value("${rabbitmq.queue.send}")
//    private String queueSend;
    @Value("${rabbitmq.exchange}")
    private String exchange;
    @Value("${rabbitmq.routes.receive.new}")
    private String routeReceivingNew;
//    @Value("${rabbitmq.routes.receive.unread}")
//    private String routeReceiveUnread;
//    @Value("${rabbitmq.routes.send}")
//    private String routeSend;

    @Bean
    public TopicExchange exchange() {
        return new TopicExchange(exchange);
    }

    @Bean
    public Binding binding() {
        return BindingBuilder
                .bind(new Queue(queueReceiveNew))
                .to(exchange())
                .with(routeReceivingNew);
    }

    @Bean
    public MessageConverter messageConverter() {
        return new Jackson2JsonMessageConverter();
    }

    @Bean
    public AmqpTemplate amqpTemplate(ConnectionFactory connectionFactory) {
        RabbitTemplate rabbitTemplate = new RabbitTemplate(connectionFactory);
        rabbitTemplate.setMessageConverter(messageConverter());
        return rabbitTemplate;
    }
}