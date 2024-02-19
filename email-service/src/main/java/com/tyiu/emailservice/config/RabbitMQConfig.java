package com.tyiu.emailservice.config;

import org.springframework.amqp.core.*;
import org.springframework.amqp.rabbit.connection.ConnectionFactory;
import org.springframework.amqp.rabbit.core.RabbitTemplate;
import org.springframework.amqp.support.converter.Jackson2JsonMessageConverter;
import org.springframework.amqp.support.converter.MessageConverter;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class RabbitMQConfig {

    @Value("${rabbitmq.exchange}")
    private String topic;

    @Value("${rabbitmq.notification-queue.make}")
    private  String makeNotificationQueue;

    @Value("${rabbitmq.notification-queue.validate}")
    private  String validateResponseQueue;

    @Value("${rabbitmq.notification-route.make}")
    private  String makeNotificationRoute;

    @Value("${rabbitmq.notification-route.validate}")
    private  String validateResponseRoute;
    @Bean
    public MessageConverter messageConverter() {
        return new Jackson2JsonMessageConverter();
    }

    @Bean
    public TopicExchange exchange() { return new TopicExchange(topic); }

    @Bean
    public Queue makeQueue() { return new Queue(makeNotificationQueue); }

    @Bean
    public Queue validateQueue() { return new Queue(validateResponseQueue); }

    @Bean
    public Binding makeBinding(TopicExchange exchange) {
        return BindingBuilder.bind(makeQueue()).to(exchange).with(makeNotificationRoute);
    }

    @Bean
    public Binding validateBinding(TopicExchange exchange) {
        return BindingBuilder.bind(validateQueue()).to(exchange).with(validateResponseRoute);
    }


    @Bean
    public AmqpTemplate getTemplate(ConnectionFactory connectionFactory) {
        RabbitTemplate rabbitTemplate = new RabbitTemplate(connectionFactory);
        rabbitTemplate.setMessageConverter(messageConverter());
        return rabbitTemplate;
    }
}


