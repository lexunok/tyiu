package com.tyiu.emailservice.rabbitmq;

import com.tyiu.ideas.config.exception.CustomHttpException;
import interfaces.INotification;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.boot.test.context.SpringBootTest;
import request.NotificationRequest;

import static org.junit.jupiter.api.Assertions.*;


@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
public class EmailServiceApplicationTests {
    @Autowired
    @Qualifier("testEmailClient")
    private INotification fakeNotificationEmailRabbitMQ;

    @Test
    void testSuccessfulRequest() {
        NotificationRequest notificationRequest = NotificationRequest.builder()
                .notificationId("disjdughwwrg")
                .consumerEmail("sfgw@g.f")
                .title("Title")
                .message("Message")
                .link("https://hits.tyuiu.ru/")
                .buttonName("Проверка")
                .build();

        CustomHttpException thrown = assertThrows(CustomHttpException.class,
                () -> fakeNotificationEmailRabbitMQ.makeNotification(notificationRequest)
        );

        assertEquals("Validation is successful with notificationId: "
                        + notificationRequest.getNotificationId(),
                thrown.getMessage());
        assertEquals(thrown.getStatusCode(), 200);
    }

    @Test
    void testUnsuccessfulRequestWithOutNotificationId() {
        NotificationRequest notificationRequest = NotificationRequest.builder()
                .consumerEmail("sfgw@g.f")
                .title("Title")
                .message("rgwrg")
                .link("https://hits.tyuiu.ru/")
                .buttonName("Проверка")
                .build();

        CustomHttpException thrown = assertThrows(CustomHttpException.class,
                () -> fakeNotificationEmailRabbitMQ.makeNotification(notificationRequest)
        );

        assertEquals("Validation isn't successful with subject: "
                        + notificationRequest.getTitle()
                        + ". NotificationId must not be null",
                thrown.getMessage());
        assertEquals(thrown.getStatusCode(), 404);
    }

    @Test
    void testUnsuccessfulRequestWithOutTitleLinkButtonAndLink() {
        NotificationRequest notificationRequest = NotificationRequest.builder()
                .notificationId("dgweg")
                .consumerEmail("sfgw@g.f")
                .build();

        CustomHttpException thrown = assertThrows(CustomHttpException.class,
                () -> fakeNotificationEmailRabbitMQ.makeNotification(notificationRequest)
        );

        assertEquals("Validation isn't successful with notificationId: "
                        + notificationRequest.getNotificationId()
                        + ". Message, title, button or link name must not be null",
                thrown.getMessage());
        assertEquals(thrown.getStatusCode(), 404);
    }
    @Test
    void testUnsuccessfulRequestWithOutConsumerEmail() {
        NotificationRequest notificationRequest = NotificationRequest.builder()
                .notificationId("dgweg")
                .title("rgr")
                .message("dgr")
                .link("https://hits.tyuiu.ru/")
                .buttonName("Проверка")
                .build();

        CustomHttpException thrown = assertThrows(CustomHttpException.class,
                () -> fakeNotificationEmailRabbitMQ.makeNotification(notificationRequest)
        );

        assertEquals("Validation isn't successful with notificationId: "
                        + notificationRequest.getNotificationId()
                        + ". ConsumerEmail must be defined",
                thrown.getMessage());
        assertEquals(thrown.getStatusCode(), 500);
    }

    @Test
    void testUnsuccessfulRequestInvalidPathInLink() {
        NotificationRequest notificationRequest = NotificationRequest.builder()
                .consumerEmail("sfgw@g.f")
                .title("Title")
                .notificationId("wrgw")
                .message("rgwrg")
                .link("https://youtu.be/dQw4w9WgXcQ?si=4ZZhLv3QQpmnqag_")
                .buttonName("Проверка")
                .build();

        CustomHttpException thrown = assertThrows(CustomHttpException.class,
                () -> fakeNotificationEmailRabbitMQ.makeNotification(notificationRequest)
        );

        assertEquals("Validation isn't successful with notificationId: "
                        + notificationRequest.getNotificationId()
                        + ". link must be start with \"https://hits.tyuiu.ru/\"",
                thrown.getMessage());
        assertEquals(thrown.getStatusCode(), 404);
    }

    @Test
    void testUnsuccessfulRequestNullRequest() {

        CustomHttpException thrown = assertThrows(CustomHttpException.class,
                () -> fakeNotificationEmailRabbitMQ.makeNotification(null)
        );

        assertEquals("request is null",
                thrown.getMessage());
        assertEquals(thrown.getStatusCode(), 404);
    }
}
