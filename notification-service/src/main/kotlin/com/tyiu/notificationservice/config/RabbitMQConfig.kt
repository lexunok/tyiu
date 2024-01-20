package com.tyiu.notificationservice.config

import org.springframework.amqp.core.*
import org.springframework.amqp.rabbit.annotation.EnableRabbit
import org.springframework.amqp.rabbit.connection.ConnectionFactory
import org.springframework.amqp.rabbit.core.RabbitTemplate
import org.springframework.amqp.support.converter.Jackson2JsonMessageConverter
import org.springframework.amqp.support.converter.MessageConverter
import org.springframework.beans.factory.annotation.Value

import org.springframework.context.annotation.Bean
import org.springframework.context.annotation.Configuration

@Configuration
@EnableRabbit
open class RabbitMQConfig {

    @Value("\${rabbitmq.queues.telegram.receive}")
    private var telegramTagQueue: String = "telegramTagQueue"

//    @Value("\${rabbitmq.queues.telegram.send.unread}")
//    private var sendingUnreadQueue: String = "sendingUnreadQueue"

    @Value("\${rabbitmq.queues.telegram.send.new}")
    private var sendingNewQueue: String = "sendingNewQueue"

    @Value("\${rabbitmq.queues.email.send}")
    private var sendingEmailQueue: String = "sendingEmailQueue"

    @Value("\${rabbitmq.queues.notification.receive}")
    private var receiveNotificationQueue: String = "receiveNotificationQueue"

    @Value("\${rabbitmq.topic}")
    private var topic: String = "topic"

    @Value("\${rabbitmq.routes.telegram.send.new}")
    private var sendNewRoute: String = "sendNewRoute"

//    @Value("\${rabbitmq.routes.telegram.send.unread}")
//    private var sendUnreadRoute: String = "sendUnreadRoute"

    @Value("\${rabbitmq.routes.telegram.receive}")
    private var telegramTagRoute: String = "telegramTagRoute"

    @Value("\${rabbitmq.routes.email.send}")
    private var emailRoute: String = "emailRoute"

    @Value("\${rabbitmq.routes.notification.receive}")
    private var notificationRoute: String = "notificationRoute"

    @Bean
    open fun exchange(): TopicExchange = TopicExchange(topic)

    @Bean
    open fun telegramTagQueue(): Queue = Queue(telegramTagQueue)
//    @Bean
//    open fun sendingUnreadQueue(): Queue = Queue(sendingUnreadQueue)
    @Bean
    open fun sendingNewQueue(): Queue = Queue(sendingNewQueue)
    @Bean
    open fun receiveNotificationQueue(): Queue = Queue(receiveNotificationQueue)
    @Bean
    open fun sendingEmailQueue(): Queue = Queue(sendingEmailQueue)


    @Bean
    open fun messageConverter(): MessageConverter {
        return Jackson2JsonMessageConverter()
    }

    @Bean
    open fun getTemplate(connectionFactory: ConnectionFactory): AmqpTemplate {
        val rabbitTemplate = RabbitTemplate(connectionFactory)
        rabbitTemplate.messageConverter = messageConverter()
        return rabbitTemplate
    }

    @Bean
    open fun fanOutBindings(exchange: TopicExchange): List<Declarable> {
        val notificationQueue = Queue(receiveNotificationQueue)
        val telegramTagQueue = Queue(telegramTagQueue)
        val sendingNewQueue = Queue(sendingNewQueue)
//        val sendingUnreadQueue = Queue(sendingUnreadQueue)
        val emailQueue = Queue(sendingEmailQueue)

        val notificationBinding = BindingBuilder.bind(notificationQueue).to(exchange).with(notificationRoute)
        val telegramTagBinding = BindingBuilder.bind(telegramTagQueue).to(exchange).with(telegramTagRoute)
        val sendingNewBinding = BindingBuilder.bind(sendingNewQueue).to(exchange).with(sendNewRoute)
//        val sendingUnreadBinding = BindingBuilder.bind(sendingUnreadQueue).to(exchange).with(sendUnreadRoute)
        val emailBinding = BindingBuilder.bind(emailQueue).to(exchange).with(emailRoute)


        return listOf(
            notificationBinding,
            telegramTagBinding,
            sendingNewBinding,
//            sendingUnreadBinding,
            emailBinding
        )
    }
}