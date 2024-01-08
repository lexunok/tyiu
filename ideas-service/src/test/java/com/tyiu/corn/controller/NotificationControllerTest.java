package com.tyiu.corn.controller;

import com.tyiu.corn.model.dto.NotificationDTO;
import com.tyiu.corn.model.dto.UserDTO;
import com.tyiu.corn.model.enums.Role;
import com.tyiu.corn.model.requests.RegisterRequest;
import com.tyiu.corn.model.responses.AuthenticationResponse;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.web.reactive.server.WebTestClient;
import reactor.core.publisher.Mono;

import java.time.LocalDateTime;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;
import static org.springframework.boot.test.context.SpringBootTest.WebEnvironment.RANDOM_PORT;

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringBootTest(webEnvironment = RANDOM_PORT)
public class NotificationControllerTest extends TestContainers {

    @Autowired
    private WebTestClient webTestClient;
    private String jwt;
    private UserDTO userDTO;
    private NotificationDTO createNotification() {

        NotificationDTO notification = NotificationDTO.builder()
                .userId(userDTO.getId())
                .title("Добро пожаловать")
                .message("Вас пригласили в команду!")
                .link("api/v1/team/send-invite/123")
                .isShowed(false)
                .isReaded(false)
                .isFavourite(false)
                .createdAt(LocalDateTime.now())
                .build();

        return webTestClient
                .post()
                .uri("/api/v1/notification/create")
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(notification), NotificationDTO.class)
                .exchange()
                .expectBody(NotificationDTO.class)
                .returnResult().getResponseBody();
    }

    private NotificationDTO createAnotherNotification() {

        NotificationDTO notification = NotificationDTO.builder()
                .userId(userDTO.getId())
                .title("Нам не по пути")
                .message("Вы были кикнуты из команды :(")
                .link("api/v1/team/kick/456")
                .isShowed(false)
                .isReaded(false)
                .isFavourite(false)
                .createdAt(LocalDateTime.now())
                .build();

        return webTestClient
                .post()
                .uri("/api/v1/notification/create")
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(notification), NotificationDTO.class)
                .exchange()
                .expectBody(NotificationDTO.class)
                .returnResult().getResponseBody();
    }

    @BeforeAll
    public void setUp() {

        RegisterRequest request = new RegisterRequest(
                "notification-test@gmail.com", "fakename", "fakename", "fakepass",
                List.of(Role.ADMIN,
                        Role.EXPERT,
                        Role.PROJECT_OFFICE,
                        Role.INITIATOR));

        AuthenticationResponse response = webTestClient
                .post()
                .uri("/api/v1/auth/register")
                .body(Mono.just(request), RegisterRequest.class)
                .exchange()
                .expectBody(AuthenticationResponse.class)
                .returnResult().getResponseBody();
        assertNotNull(response);

        jwt = response.getToken();

        userDTO = UserDTO.builder()
                .id(response.getId())
                .email(response.getEmail())
                .lastName(response.getLastName())
                .firstName(response.getFirstName())
                .roles(response.getRoles())
                .build();
    }

    @Test
    void testCreateNotification() {

        NotificationDTO notification = createNotification();
        assertEquals("Добро пожаловать", notification.getTitle());
        assertEquals("Вас пригласили в команду!", notification.getMessage());
        assertEquals("https://hits.tyuiu.ru/api/v1/team/send-invite/123", notification.getLink());
    }

    @Test
    void testShowNotification() {

        NotificationDTO notification = createNotification();
        assertEquals("Добро пожаловать", notification.getTitle());
        assertEquals("Вас пригласили в команду!", notification.getMessage());
        assertEquals("https://hits.tyuiu.ru/api/v1/team/send-invite/123", notification.getLink());

        webTestClient
                .put()
                .uri("/api/v1/notification/show/{id}", notification.getId())
                .header("Authorization", "Bearer " + jwt)
                .exchange();

        List<NotificationDTO> unshownNotifications = webTestClient
                .get()
                .uri("/api/v1/notification/all")
                .header("Authorization", "Bearer " + jwt)
                .exchange()
                .expectBodyList(NotificationDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(unshownNotifications);

        unshownNotifications.forEach(currentNotification -> {
            if (currentNotification.getId().equals(notification.getId()))
                assertTrue(currentNotification.getIsShowed());
        });
    }

    @Test
    void testReadNotification() {

        NotificationDTO notification = createNotification();
        assertEquals("Добро пожаловать", notification.getTitle());
        assertEquals("Вас пригласили в команду!", notification.getMessage());
        assertEquals("https://hits.tyuiu.ru/api/v1/team/send-invite/123", notification.getLink());

        webTestClient
                .put()
                .uri("/api/v1/notification/read/{id}", notification.getId())
                .header("Authorization", "Bearer " + jwt)
                .exchange();

        List<NotificationDTO> unreadedNotifications = webTestClient
                .get()
                .uri("/api/v1/notification/all")
                .header("Authorization", "Bearer " + jwt)
                .exchange()
                .expectBodyList(NotificationDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(unreadedNotifications);

        unreadedNotifications.forEach(currentNotification -> {
            if (currentNotification.getId().equals(notification.getId()))
                assertTrue(currentNotification.getIsReaded());
        });
    }

    @Test
    void testReadAllNotifications() {

        NotificationDTO notification1 = createNotification();
        assertEquals("Добро пожаловать", notification1.getTitle());
        assertEquals("Вас пригласили в команду!", notification1.getMessage());
        assertEquals("https://hits.tyuiu.ru/api/v1/team/send-invite/123", notification1.getLink());

        NotificationDTO notification2 = createAnotherNotification();
        assertEquals("Нам не по пути", notification2.getTitle());
        assertEquals("Вы были кикнуты из команды :(", notification2.getMessage());
        assertEquals("https://hits.tyuiu.ru/api/v1/team/kick/456", notification2.getLink());

        webTestClient
                .put()
                .uri("/api/v1/notification/read/all")
                .header("Authorization", "Bearer " + jwt)
                .exchange();

        List<NotificationDTO> unreadedNotifications = webTestClient
                .get()
                .uri("/api/v1/notification/all")
                .header("Authorization", "Bearer " + jwt)
                .exchange()
                .expectBodyList(NotificationDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(unreadedNotifications);

        unreadedNotifications.forEach(currentNotification -> {
            if (currentNotification.getId().equals(notification1.getId()))
                assertTrue(currentNotification.getIsReaded());

            if (currentNotification.getId().equals(notification2.getId()))
                assertTrue(currentNotification.getIsReaded());
        });
    }

    @Test
    void testAddAndRemoveNotificationFromFavourite() {

        NotificationDTO notification = createNotification();
        assertEquals("Добро пожаловать", notification.getTitle());
        assertEquals("Вас пригласили в команду!", notification.getMessage());
        assertEquals("https://hits.tyuiu.ru/api/v1/team/send-invite/123", notification.getLink());

        webTestClient
                .put()
                .uri("/api/v1/notification/favourite/{id}", notification.getId())
                .header("Authorization", "Bearer " + jwt)
                .exchange();

        List<NotificationDTO> favouriteNotifications = webTestClient
                .get()
                .uri("/api/v1/notification/all")
                .header("Authorization", "Bearer " + jwt)
                .exchange()
                .expectBodyList(NotificationDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(favouriteNotifications);

        favouriteNotifications.forEach(currentNotification -> {
            if (currentNotification.getId().equals(notification.getId())) {
                assertTrue(currentNotification.getIsFavourite());
                assertTrue(currentNotification.getIsReaded());
            }
        });

        webTestClient
                .put()
                .uri("/api/v1/notification/unfavourite/{id}", notification.getId())
                .header("Authorization", "Bearer " + jwt)
                .exchange();

        List<NotificationDTO> notFavouriteNotifications = webTestClient
                .get()
                .uri("/api/v1/notification/favourite")
                .header("Authorization", "Bearer " + jwt)
                .exchange()
                .expectBodyList(NotificationDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(notFavouriteNotifications);

        notFavouriteNotifications.forEach(currentNotification -> {
            if (currentNotification.getId().equals(notification.getId())) {
                assertFalse(currentNotification.getIsFavourite());
                assertTrue(currentNotification.getIsReaded());
            }
        });
    }

    @Test
    void testGetAllNotifications() {

        NotificationDTO notification1 = createNotification();
        assertEquals("Добро пожаловать", notification1.getTitle());
        assertEquals("Вас пригласили в команду!", notification1.getMessage());

        NotificationDTO notification2 = createAnotherNotification();
        assertEquals("Нам не по пути", notification2.getTitle());
        assertEquals("Вы были кикнуты из команды :(", notification2.getMessage());

        List<NotificationDTO> allNotifications = webTestClient
                .get()
                .uri("/api/v1/notification/all")
                .header("Authorization", "Bearer " + jwt)
                .exchange()
                .expectBodyList(NotificationDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(allNotifications);
        assertTrue(allNotifications.size() > 1);

        allNotifications.forEach(currentNotification -> {
            if (currentNotification.getId().equals(notification1.getId()))
                assertEquals("https://hits.tyuiu.ru/api/v1/team/send-invite/123", notification1.getLink());

            if (currentNotification.getId().equals(notification2.getId()))
                assertEquals("https://hits.tyuiu.ru/api/v1/team/kick/456", notification2.getLink());
        });
    }

    @Test
    void testGetAllFavouriteNotifications() {

        NotificationDTO notification1 = createNotification();
        assertEquals("Добро пожаловать", notification1.getTitle());
        assertEquals("Вас пригласили в команду!", notification1.getMessage());

        webTestClient
                .put()
                .uri("/api/v1/notification/favourite/{id}", notification1.getId())
                .header("Authorization", "Bearer " + jwt)
                .exchange();

        NotificationDTO notification2 = createAnotherNotification();
        assertEquals("Нам не по пути", notification2.getTitle());
        assertEquals("Вы были кикнуты из команды :(", notification2.getMessage());

        webTestClient
                .put()
                .uri("/api/v1/notification/favourite/{id}", notification2.getId())
                .header("Authorization", "Bearer " + jwt)
                .exchange();

        List<NotificationDTO> allFavouriteNotifications = webTestClient
                .get()
                .uri("/api/v1/notification/favourite")
                .header("Authorization", "Bearer " + jwt)
                .exchange()
                .expectBodyList(NotificationDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(allFavouriteNotifications);
        assertTrue(allFavouriteNotifications.size() > 1);

        allFavouriteNotifications.forEach(currentNotification -> {
            if (currentNotification.getId().equals(notification1.getId()))
                assertEquals("https://hits.tyuiu.ru/api/v1/team/send-invite/123", notification1.getLink());

            if (currentNotification.getId().equals(notification2.getId()))
                assertEquals("https://hits.tyuiu.ru/api/v1/team/kick/456", notification2.getLink());
        });
    }
}
