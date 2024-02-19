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

    @Value("\${rabbitmq.queues.telegram.make}")
    private var makeTelegramInvitationQueue: String = "makeTelegramInvitation"

    @Value("\${rabbitmq.routes.telegram.make}")
    private var makeTelegramInvitationRoute: String = "makeTelegramInvitation"

    @Value("\${rabbitmq.queues.telegram.validate}")
    private var validateTelegramResponseQueue: String = "validateTelegramResponse"

    @Value("\${rabbitmq.routes.telegram.validate}")
    private var validateTelegramResponseRoute: String = "validateTelegramResponse"

    @Value("\${rabbitmq.queues.email.make}")
    private var makeEmailInvitationQueue: String = "makeEmailInvitation"

    @Value("\${rabbitmq.routes.email.make}")
    private var makeEmailInvitationRoute: String = "makeEmailInvitation"

    @Value("\${rabbitmq.queues.email.validate}")
    private var validateEmailResponseQueue: String = "validateEmailResponse"

    @Value("\${rabbitmq.routes.email.validate}")
    private var validateEmailResponseRoute: String = "validateEmailResponse"

    @Value("\${rabbitmq.queues.notification.receive}")
    private var receiveNotificationQueue: String = "receiveNotificationQueue"

    @Value("\${rabbitmq.routes.notification.receive}")
    private var receiveNotificationRoute: String = "notificationRoute"

    @Value("\${rabbitmq.topic}")
    private var topic: String = "topic"

    @Bean
    open fun exchange(): TopicExchange = TopicExchange(topic)
    @Bean
    open fun makeTelegramInvitationQueue(): Queue = Queue(makeTelegramInvitationQueue)

    @Bean
    open fun makeEmailInvitationQueue(): Queue = Queue(makeEmailInvitationQueue)

    @Bean
    open fun validateTelegramResponseQueue(): Queue = Queue(validateTelegramResponseQueue)

    @Bean
    open fun validateEmailResponseQueue(): Queue = Queue(validateEmailResponseQueue)

    @Bean
    open fun notificationQueue(): Queue = Queue(receiveNotificationQueue)


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
    open fun fanoutBindings(exchange: TopicExchange): List<Declarable> {
        val notificationQueue = notificationQueue()
        val makeTelegramInvitationQueue = makeTelegramInvitationQueue()
        val makeEmailInvitationQueue = makeEmailInvitationQueue()
        val validateTelegramResponseQueue = validateTelegramResponseQueue()
        val validateEmailResponseQueue = validateEmailResponseQueue()

        val notificationBinding = BindingBuilder.bind(notificationQueue).to(exchange).with(receiveNotificationRoute)
        val makeTelegramInvitationBinding = BindingBuilder.bind(makeTelegramInvitationQueue).to(exchange).with(makeTelegramInvitationRoute)
        val makeEmailInvitationBinding = BindingBuilder.bind(makeEmailInvitationQueue).to(exchange).with(makeEmailInvitationRoute)
        val validateTelegramResponseBinding = BindingBuilder.bind(validateTelegramResponseQueue).to(exchange).with(validateTelegramResponseRoute)
        val validateEmailResponseBinding = BindingBuilder.bind(validateEmailResponseQueue).to(exchange).with(validateEmailResponseRoute)

        return listOf(
            notificationBinding,
            makeTelegramInvitationBinding,
            makeEmailInvitationBinding,
            validateTelegramResponseBinding,
            validateEmailResponseBinding
        )
    }
}