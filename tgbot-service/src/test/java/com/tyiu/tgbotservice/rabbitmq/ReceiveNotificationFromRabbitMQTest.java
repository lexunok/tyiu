package com.tyiu.tgbotservice.rabbitmq;

import com.tyiu.ideas.config.exception.CustomHttpException;
import com.tyiu.ideas.model.entities.User;
import com.tyiu.ideas.model.enums.Role;
import com.tyiu.tgbotservice.model.entities.UserTelegram;
import org.junit.jupiter.api.BeforeAll;
import interfaces.INotification;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import request.NotificationRequest;
import org.springframework.data.r2dbc.core.R2dbcEntityTemplate;
import reactor.core.publisher.Mono;

import java.util.List;

import static org.junit.jupiter.api.Assertions.*;
import static org.springframework.boot.test.context.SpringBootTest.WebEnvironment.RANDOM_PORT;

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringBootTest(webEnvironment = RANDOM_PORT)
public class ReceiveNotificationFromRabbitMQTest {

    @Autowired
    private R2dbcEntityTemplate template;

    @Autowired
    private INotification testRabbitMQ;

    private Mono<Void> setUserTag(String email, String tag) {

        UserTelegram user = UserTelegram.builder()
                .userEmail(email)
                .userTag(tag)
                .build();
        template.insert(user);

        return Mono.empty();
    }

    private NotificationRequest createNotification(String email, String tag) {

        return NotificationRequest.builder()
                .notificationId("1")
                .consumerEmail(email)
                .tag(tag)
                .title("title")
                .message("message")
                .link("link")
                .buttonName("button")
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
    void testNotificationListener() {

        NotificationRequest notification1 = createNotification("email", "tag");

        CustomHttpException thrown1 = assertThrows(CustomHttpException.class, () ->
                testRabbitMQ.makeNotification(notification1));
        assertEquals("Notification (id = 1) was successfully sent to the user with the tag = tag", thrown1.getMessage());
        assertEquals(200, thrown1.getStatusCode());



        NotificationRequest notification2 = createNotification("not-my-email", "not-my-tag");

        CustomHttpException thrown2 = assertThrows(CustomHttpException.class, () ->
                testRabbitMQ.makeNotification(notification2));
        assertEquals("Error when sending notification (id = 1) to user", thrown2.getMessage());
        assertEquals(404, thrown2.getStatusCode());



        NotificationRequest notification3 = createNotification("email", null);

        CustomHttpException thrown3 = assertThrows(CustomHttpException.class, () ->
                testRabbitMQ.makeNotification(notification3));
        assertEquals("Error when sending notification (id = 1) to user", thrown3.getMessage());
        assertEquals(404, thrown3.getStatusCode());
    }
}
