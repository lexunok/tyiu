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
                .title("title")
                .message("message")
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
        assertEquals("title", notification.getTitle());
    }

    @Test
    void testShowNotification() {

        NotificationDTO notification = createNotification();
        assertEquals("title", notification.getTitle());

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
        assertEquals("title", notification.getTitle());

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
        NotificationDTO notification2 = createNotification();

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
            if (currentNotification.getId().equals(notification1.getId()) && currentNotification.getId().equals(notification2.getId()))
                assertTrue(currentNotification.getIsReaded());
        });
    }

    @Test
    void testAddAndRemoveNotificationFromFavourite() {

        NotificationDTO notification = createNotification();
        assertEquals("title", notification.getTitle());

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

        createNotification();
        createNotification();

        List<NotificationDTO> allNotifications = webTestClient
                .get()
                .uri("/api/v1/notification/all")
                .header("Authorization", "Bearer " + jwt)
                .exchange()
                .expectBodyList(NotificationDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(allNotifications);
        assertTrue(allNotifications.size() > 1);
    }

    @Test
    void testGetAllFavouriteNotifications() {

        NotificationDTO notification1 = createNotification();
        assertEquals("title", notification1.getTitle());

        webTestClient
                .put()
                .uri("/api/v1/notification/read/{id}", notification1.getId())
                .header("Authorization", "Bearer " + jwt)
                .exchange();

        NotificationDTO notification2 = createNotification();
        assertEquals("title", notification2.getTitle());

        webTestClient
                .put()
                .uri("/api/v1/notification/read/{id}", notification2.getId())
                .header("Authorization", "Bearer " + jwt)
                .exchange();

        List<NotificationDTO> allFavouriteNotifications = webTestClient
                .get()
                .uri("/api/v1/notification/all")
                .header("Authorization", "Bearer " + jwt)
                .exchange()
                .expectBodyList(NotificationDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(allFavouriteNotifications);
        assertTrue(allFavouriteNotifications.size() > 1);
    }
}
