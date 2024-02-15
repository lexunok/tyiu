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
import static org.springframework.data.relational.core.query.Criteria.where;
import static org.springframework.data.relational.core.query.Query.query;

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringBootTest(webEnvironment = RANDOM_PORT)
public class ReceiveNotificationTest extends TestContainersDB {

    @Autowired
    private R2dbcEntityTemplate template;

    @Autowired
    @Qualifier("testTelegramClient")
    private INotification testNotification;

    private void createSQLTables() {

        template.getDatabaseClient()
                .sql("CREATE TABLE IF NOT EXISTS users " +
                        "(id TEXT DEFAULT gen_random_uuid()::TEXT PRIMARY KEY, " +
                        "email TEXT, " +
                        "first_name TEXT, " +
                        "last_name TEXT, " +
                        "roles TEXT[], " +
                        "password TEXT);")
                .fetch()
                .rowsUpdated()
                .block();

        template.getDatabaseClient()
                .sql("CREATE TABLE IF NOT EXISTS users_telegram " +
                        "(user_email TEXT REFERENCES users (email) ON UPDATE CASCADE, " +
                        "user_tag TEXT, " +
                        "chat_id BIGINT, " +
                        "is_visible BOOLEAN DEFAULT false::BOOLEAN);")
                .fetch()
                .rowsUpdated()
                .block();

        template.getDatabaseClient()
                .sql("CREATE TABLE IF NOT EXISTS notification_request " +
                        "(id TEXT, " +
                        "consumer_email TEXT REFERENCES users (email) ON UPDATE CASCADE, " +
                        "consumer_tag TEXT REFERENCES users_telegram (user_tag) ON UPDATE CASCADE, " +
                        "title TEXT, " +
                        "message TEXT, " +
                        "link TEXT, " +
                        "button_name TEXT);")
                .fetch()
                .rowsUpdated()
                .block();
    }

    private Mono<Void> setUserInfo(String email, String tag) {

        UserTelegram user = UserTelegram.builder()
                .userEmail(email)
                .userTag(tag)
                .build();

        return template.insert(user).then();
    }

    private NotificationRequest createNotification(String id, String email, String tag, String something) {

        NotificationRequest notification = NotificationRequest.builder()
                .notificationId(id)
                .consumerEmail(email)
                .tag(tag)
                .title(something)
                .message(something)
                .link(something)
                .buttonName(something)
                .build();

        template.insert(notification).then();

        Mono.just(notification.getConsumerEmail())
                .flatMap(dbFind -> template.exists(query(where("consumer_email").is(dbFind)), NotificationRequest.class))
                .flatMap(notificationExists -> {

                    if (Boolean.FALSE.equals(notificationExists))
                        return Mono.error(new Exception("Уведолмение не найдено"));

                    return Mono.empty();
                }).then();

        return notification;
    }

    @BeforeAll
    void setUp() {

        createSQLTables();

        User user = User.builder()
                .email("email")
                .firstName("name")
                .lastName("surname")
                .roles(List.of(Role.MEMBER))
                .password("password")
                .build();

        template.insert(user).flatMap(u -> setUserInfo(u.getEmail(), "tag")).block();
    }

    @Test
    void testNullNotificationException() {

        CustomHttpException thrown = assertThrows(CustomHttpException.class, () ->
                testNotification.makeNotification(null));
        assertEquals("Notification is null", thrown.getMessage());
        assertEquals(500, thrown.getStatusCode());
    }

    @Test
    void testSuccessSending() {

        NotificationRequest notification = createNotification("1", "email", "tag", "bla-bla-bla");

        CustomHttpException thrown = assertThrows(CustomHttpException.class, () ->
                testNotification.makeNotification(notification));
        assertEquals("Notification (id = 1) was successfully sent to the user with the tag = tag", thrown.getMessage());
        assertEquals(200, thrown.getStatusCode());
    }

    @Test
    void testNullIdException() {

        NotificationRequest notification = createNotification(null, "email", "tag", "bla-bla-bla");

        CustomHttpException thrown = assertThrows(CustomHttpException.class, () ->
                testNotification.makeNotification(notification));
        assertEquals("Error when sending notification to telegram. Notification id must not be null", thrown.getMessage());
        assertEquals(500, thrown.getStatusCode());
    }

    @Test
    void testNotificationContentIsEmpty() {

        NotificationRequest notification = createNotification("2", "email", "tag", null);

        CustomHttpException thrown = assertThrows(CustomHttpException.class, () ->
                testNotification.makeNotification(notification));
        assertEquals("Error when sending notification (id = 2). Notification content must not be null", thrown.getMessage());
        assertEquals(500, thrown.getStatusCode());
    }

    @Test
    void testNullTagException() {

        NotificationRequest notification = createNotification("3", "email", null, "bla-bla-bla");

        CustomHttpException thrown = assertThrows(CustomHttpException.class, () ->
                testNotification.makeNotification(notification));
        assertEquals("Error when sending notification (id = 3). Tag must not be null", thrown.getMessage());
        assertEquals(404, thrown.getStatusCode());
    }

    @Test
    void testNotificationForAnotherUserException() {

        NotificationRequest notification = createNotification("4", "not-my-email", "not-my-tag", "bla-bla-bla");

        CustomHttpException thrown = assertThrows(CustomHttpException.class, () ->
                testNotification.makeNotification(notification));
        assertEquals("Error when sending notification (id = 4) to user. " +
                "This notification intended for another user", thrown.getMessage());
        assertEquals(404, thrown.getStatusCode());
    }
}
