package com.tyiu.emailservice.config;

import freemarker.cache.ClassTemplateLoader;
import freemarker.cache.TemplateLoader;
import org.springframework.amqp.core.Binding;
import org.springframework.amqp.core.BindingBuilder;
import org.springframework.amqp.core.Queue;
import org.springframework.amqp.core.TopicExchange;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.reactive.result.view.freemarker.FreeMarkerConfigurer;

@Configuration
public class EmailConfig {

    @Value("${rabbitmq.exchanges.internal}")
    private String internalExchange;

    @Value("${rabbitmq.queues.invitation}")
    private String invitationQueue;
    @Value("${rabbitmq.routing-keys.internal-invitation}")
    private String internalInvitationRoutingKey;

    @Value("${rabbitmq.queues.team-invitation}")
    private String teamInvitationQueue;
    @Value("${rabbitmq.routing-keys.internal-team-invitation}")
    private String internalTeamInvitationRoutingKey;

    @Bean
    public TopicExchange internalTopicExchange(){
        return new TopicExchange(internalExchange);
    }
    @Bean
    public Queue invitationQueue() {
        return new Queue(invitationQueue);
    }
    @Bean
    public Queue teamInvitationQueue() {
        return new Queue(teamInvitationQueue);
    }
    @Bean
    public Binding internalInvitationBinding(){
        return BindingBuilder
                .bind(invitationQueue())
                .to(internalTopicExchange())
                .with(internalInvitationRoutingKey);
    }

    @Bean
    public Binding internalTeamInvitationBinding(){
        return BindingBuilder
                .bind(teamInvitationQueue())
                .to(internalTopicExchange())
                .with(internalTeamInvitationRoutingKey);
    }

    @Bean
    public FreeMarkerConfigurer freemarkerClassLoaderConfig() {
        freemarker.template.Configuration configuration = new freemarker.template.Configuration(freemarker.template.Configuration.VERSION_2_3_32);
        TemplateLoader templateLoader = new ClassTemplateLoader(this.getClass(), "/templates");
        configuration.setTemplateLoader(templateLoader);
        FreeMarkerConfigurer freeMarkerConfigurer = new FreeMarkerConfigurer();
        freeMarkerConfigurer.setConfiguration(configuration);
        return freeMarkerConfigurer;
    }

}