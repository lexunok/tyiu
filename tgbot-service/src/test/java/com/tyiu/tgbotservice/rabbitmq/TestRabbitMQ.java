package com.tyiu.tgbotservice.rabbitmq;

import com.tyiu.ideas.config.exception.CustomHttpException;
import com.tyiu.ideas.model.entities.User;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.r2dbc.core.R2dbcEntityTemplate;
import org.springframework.http.HttpStatusCode;
import interfaces.INotification;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Component;
import reactor.core.publisher.Mono;
import request.NotificationRequest;
import response.NotificationResponse;

import static org.springframework.data.relational.core.query.Criteria.where;
import static org.springframework.data.relational.core.query.Query.query;

@Component
public class TestRabbitMQ implements INotification {

    @Autowired
    private R2dbcEntityTemplate template;

    @Override
    public void makeNotification(NotificationRequest notificationRequest) throws CustomHttpException {

        if (notificationRequest == null) {
            throw new CustomHttpException("Notification is null", 500);
        }

        if (notificationRequest.getNotificationId() == null) {

            String message = "Error when sending notification to telegram. Notification id must not be null";

            this.validateResponse(
                    new ResponseEntity<NotificationResponse>(NotificationResponse.builder()
                            .message(message)
                            .notificationId(notificationRequest.getNotificationId())
                            .build(),
                            HttpStatusCode.valueOf(500))
            );
        }
        else if (notificationRequest.getTitle() == null
                || notificationRequest.getMessage() == null
                || notificationRequest.getLink() == null) {

            String message = String.format("Error when sending notification (id = %s). Notification content must not be null",
                    notificationRequest.getNotificationId());

            this.validateResponse(
                    new ResponseEntity<NotificationResponse>(NotificationResponse.builder()
                            .message(message)
                            .notificationId(notificationRequest.getNotificationId())
                            .build(),
                            HttpStatusCode.valueOf(500))
            );
        }
        else if (notificationRequest.getTag() == null) {

            String message = String.format("Error when sending notification (id = %s). Tag must not be null",
                    notificationRequest.getNotificationId());

            this.validateResponse(
                    new ResponseEntity<NotificationResponse>(NotificationResponse.builder()
                            .message(message)
                            .notificationId(notificationRequest.getNotificationId())
                            .build(),
                            HttpStatusCode.valueOf(404))
            );
        }
        else {
            Mono.just(notificationRequest.getConsumerEmail())
                    .flatMap(dbFind -> template.exists(query(where("email").is(dbFind)), User.class))
                    .flatMap(userExists -> {

                        if (Boolean.TRUE.equals(userExists) && notificationRequest.getTag() != null) {

                            String message = String.format("Notification (id = %s) was successfully sent to the user with the tag = %s",
                                    notificationRequest.getNotificationId(),
                                    notificationRequest.getTag());

                            this.validateResponse(
                                    new ResponseEntity<NotificationResponse>(NotificationResponse.builder()
                                            .message(message)
                                            .notificationId(notificationRequest.getNotificationId())
                                            .build(),
                                            HttpStatusCode.valueOf(200))
                            );

                        } else {

                            String message = String.format("Error when sending notification (id = %s) to user. " +
                                            "This notification intended for another user",
                                    notificationRequest.getNotificationId());

                            this.validateResponse(
                                    new ResponseEntity<NotificationResponse>(NotificationResponse.builder()
                                            .message(message)
                                            .notificationId(notificationRequest.getNotificationId())
                                            .build(),
                                            HttpStatusCode.valueOf(404))
                            );
                        }
                        return Mono.empty();
                    }).block();
        }
    }

    @Override
    public void validateResponse(ResponseEntity<NotificationResponse> response) {

        if (response.getBody() != null) {
            throw new CustomHttpException(response.getBody().getMessage(), response.getStatusCode().value());
        } else {
            throw new CustomHttpException("Response body is null", 404);
        }
    }
}
