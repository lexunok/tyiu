package com.tyiu.tgbotservice.config;

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

    @Value("${rabbitmq.queue.receive.new}")
    private String queueReceiveNew;
    @Value("${rabbitmq.queue.respond.to.notification}")
    private String queueValidateResponse;

    @Value("${rabbitmq.exchange}")
    private String exchange;

    @Value("${rabbitmq.routes.receive.new}")
    private String routeReceivingNew;

    @Value("${rabbitmq.routes.respond.to.notification}")
    private String routeValidateResponse;

    @Bean
    public TopicExchange exchange() {
        return new TopicExchange(exchange);
    }

    @Bean
    public Binding makeNotificationBinding(TopicExchange exchange) {
        return BindingBuilder
                .bind(new Queue(queueReceiveNew))
                .to(exchange)
                .with(routeReceivingNew);
    }

    @Bean
    public Binding validateResponseBinding(TopicExchange exchange) {
        return BindingBuilder
                .bind(new Queue(queueValidateResponse))
                .to(exchange)
                .with(routeValidateResponse);
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
