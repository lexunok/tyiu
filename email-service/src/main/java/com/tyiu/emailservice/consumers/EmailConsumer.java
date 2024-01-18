package com.tyiu.emailservice.consumers;

import com.tyiu.emailservice.model.requests.NotificationEmailRequest;
import com.tyiu.emailservice.model.responses.NotificationEmailResponse;
import com.tyiu.emailservice.service.EmailService;
import lombok.RequiredArgsConstructor;
import org.springframework.amqp.rabbit.annotation.RabbitListener;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
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
