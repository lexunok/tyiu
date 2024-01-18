package com.tyiu.notificationservice.config

import org.springframework.amqp.core.*
import org.springframework.amqp.rabbit.annotation.EnableRabbit
import org.springframework.amqp.rabbit.connection.ConnectionFactory
import org.springframework.amqp.rabbit.core.RabbitTemplate
import org.springframework.amqp.support.converter.Jackson2JsonMessageConverter
import org.springframework.amqp.support.converter.MessageConverter
import org.springframework.context.annotation.Bean
import org.springframework.context.annotation.Configuration

@Configuration
@EnableRabbit
open class RabbitMQConfig {
    @Bean
    open fun exchange(): TopicExchange = TopicExchange(TOPIC_TO_EXCHANGE)

    @Bean
    open fun queue(): Queue = Queue(QUEUE_NOTIFICATION)

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
        val notificationQueue = Queue(QUEUE_NOTIFICATION)
        val telegramQueue = Queue(QUEUE_TELEGRAM)
        val emailQueue = Queue(QUEUE_EMAIL)

        val notificationBinding = BindingBuilder.bind(notificationQueue).to(exchange).with("success")
        val telegramBinding = BindingBuilder.bind(telegramQueue).to(exchange).with("telegram")
        val emailBinding = BindingBuilder.bind(emailQueue).to(exchange).with("email")


        return listOf(
            notificationBinding,
            telegramBinding,
            emailBinding
        )
    }
}

const val TOPIC_TO_EXCHANGE = "notification-service"
const val QUEUE_NOTIFICATION = "notification-create"
const val QUEUE_TELEGRAM = "notification-telegram"
const val QUEUE_EMAIL = "notification-email"