package com.tyiu.tgbotservice.rabbitmq;

import com.tyiu.tgbotservice.model.request.NotificationTelegramResponse;
import com.tyiu.tgbotservice.model.response.Response;
import com.tyiu.tgbotservice.service.BotService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.amqp.rabbit.annotation.EnableRabbit;
import org.springframework.amqp.rabbit.annotation.RabbitListener;
import org.springframework.stereotype.Component;
import org.springframework.stereotype.Service;

@Slf4j
@Service
@Component
@EnableRabbit
@RequiredArgsConstructor
public class ProdRabbitMQ implements InterfaceRabbitMQ {

    private final BotService bot;

    @Override
    @RabbitListener(queues = {"${rabbitmq.queue.receive.new}"})
    public Response listenToNewNotification(NotificationTelegramResponse notification) {

        try {
            String answer = "Вам пришло уведомление от портале ВШЦТ!\n\n" +
                    notification.getTitle() + "\n" +
                    notification.getMessage() + "\n\n" +
                    "Подробнее можете ознакомиться здесь:\n" +
                    notification.getLink();

            String tag = notification.getTag();
            if (tag != null) {
                bot.sendNotificationToChat(tag, answer);
            } else {
                log.error("Incorrect consumed object");
            }
        } catch (Exception e) {
            log.warn("Error sending notification " + e.fillInStackTrace());
        }

        return Response.builder()
                .message("Done")
                .build();
    }
}
