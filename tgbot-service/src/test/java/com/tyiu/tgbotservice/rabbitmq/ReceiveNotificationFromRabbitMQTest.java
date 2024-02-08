package com.tyiu.tgbotservice.rabbitmq;

import com.tyiu.ideas.config.exception.CustomHttpException;
import com.tyiu.ideas.model.entities.User;
import com.tyiu.ideas.model.enums.Role;
import com.tyiu.tgbotservice.model.entities.UserTelegram;
import com.tyiu.tgbotservice.telegram.TestContainersDB;
import org.junit.jupiter.api.BeforeAll;
import interfaces.INotification;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.boot.test.context.SpringBootTest;
import request.NotificationRequest;
import org.springframework.data.r2dbc.core.R2dbcEntityTemplate;
import reactor.core.publisher.Mono;

import java.util.List;

import static org.junit.jupiter.api.Assertions.*;
import static org.springframework.boot.test.context.SpringBootTest.WebEnvironment.RANDOM_PORT;

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringBootTest(webEnvironment = RANDOM_PORT)
public class ReceiveNotificationFromRabbitMQTest extends TestContainersDB {

    @Autowired
    private R2dbcEntityTemplate template;

    @Autowired
    @Qualifier("testTelegramClient")
    private INotification testRabbitMQ;

    private Mono<Void> setUserTag(String email, String tag) {

        UserTelegram user = UserTelegram.builder()
                .userEmail(email)
                .userTag(tag)
                .build();
        template.insert(user);

        return Mono.empty();
    }

    private NotificationRequest createNotification(String id, String email, String tag, String something) {

        return NotificationRequest.builder()
                .notificationId(id)
                .consumerEmail(email)
                .tag(tag)
                .title(something)
                .message(something)
                .link(something)
                .buttonName(something)
                .build();
    }

    @BeforeAll
    void setUp() {

        User user = User.builder()
                .email("email")
                .password("password")
                .roles(List.of(Role.MEMBER))
                .build();

        template.insert(user).flatMap(u -> setUserTag(u.getEmail(), "tag")).block();
    }

    @Test
    void testNullNotificationException() {

        CustomHttpException thrown = assertThrows(CustomHttpException.class, () ->
                testRabbitMQ.makeNotification(null));
        assertEquals("Notification is null", thrown.getMessage());
        assertEquals(500, thrown.getStatusCode());
    }

    @Test
    void testSuccessSending() {

        NotificationRequest notification = createNotification("1", "email", "tag", "bla-bla-bla");

        CustomHttpException thrown = assertThrows(CustomHttpException.class, () ->
                testRabbitMQ.makeNotification(notification));
        assertEquals("Notification (id = 1) was successfully sent to the user with the tag = tag", thrown.getMessage());
        assertEquals(200, thrown.getStatusCode());
    }

    @Test
    void testNullIdException() {

        NotificationRequest notification = createNotification(null, "email", "tag", "bla-bla-bla");

        CustomHttpException thrown = assertThrows(CustomHttpException.class, () ->
                testRabbitMQ.makeNotification(notification));
        assertEquals("Error when sending notification to telegram. Notification id must not be null", thrown.getMessage());
        assertEquals(500, thrown.getStatusCode());
    }

    @Test
    void testNotificationContentIsEmpty() {

        NotificationRequest notification = createNotification("2", "email", "tag", null);

        CustomHttpException thrown = assertThrows(CustomHttpException.class, () ->
                testRabbitMQ.makeNotification(notification));
        assertEquals("Error when sending notification (id = 2). Notification content must not be null", thrown.getMessage());
        assertEquals(500, thrown.getStatusCode());
    }

    @Test
    void testNullTagException() {

        NotificationRequest notification = createNotification("3", "email", null, "bla-bla-bla");

        CustomHttpException thrown = assertThrows(CustomHttpException.class, () ->
                testRabbitMQ.makeNotification(notification));
        assertEquals("Error when sending notification (id = 3). Tag must not be null", thrown.getMessage());
        assertEquals(404, thrown.getStatusCode());
    }

    @Test
    void testNotificationForAnotherUserException() {

        NotificationRequest notification = createNotification("4", "not-my-email", "not-my-tag", "bla-bla-bla");

        CustomHttpException thrown = assertThrows(CustomHttpException.class, () ->
                testRabbitMQ.makeNotification(notification));
        assertEquals("Error when sending notification (id = 4) to user. " +
                "This notification intended for another user", thrown.getMessage());
        assertEquals(404, thrown.getStatusCode());
    }
}
