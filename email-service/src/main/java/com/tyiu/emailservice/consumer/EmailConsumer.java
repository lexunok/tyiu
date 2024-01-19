package com.tyiu.emailservice.consumer;

import com.tyiu.emailservice.model.requests.NotificationEmailRequest;
import com.tyiu.emailservice.model.responses.NotificationEmailResponse;
import com.tyiu.emailservice.service.EmailService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.amqp.rabbit.annotation.RabbitListener;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
@Slf4j
public class EmailConsumer {
    private final EmailService emailService;

    @RabbitListener(queues = {"${rabbitmq.queue}"})
    public void getNotification(NotificationEmailRequest notificationRequest){
        emailService.sendMailNotification(NotificationEmailResponse.builder()
                        .to(notificationRequest.getConsumerEmail())
                        .title(notificationRequest.getTitle())
                        .link(notificationRequest.getLink())
                        .message(notificationRequest.getMessage())
                        .buttonName(notificationRequest.getButtonName())
                        .build())
                .subscribe();
    }
}
