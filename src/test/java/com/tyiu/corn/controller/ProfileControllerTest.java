package com.tyiu.corn.controller;

import com.tyiu.corn.model.dto.ProfileDTO;
import com.tyiu.corn.model.entities.User;
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

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.springframework.boot.test.context.SpringBootTest.WebEnvironment.RANDOM_PORT;

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringBootTest(
        webEnvironment = RANDOM_PORT,
        properties = "de.flapdoodle.mongodb.embedded.version=5.0.5"
)

public class ProfileControllerTest {
    @Autowired
    private WebTestClient webTestClient;
    private String jwt;
    private User user;
    private String userProfileId;

    @BeforeAll
    public void setUp(){
        RegisterRequest request = new RegisterRequest(
                "fakemail","fakename","fakename","fakepass", List.of(Role.ADMIN));
        AuthenticationResponse response = webTestClient
                .post()
                .uri("/api/v1/auth/register")
                .body(Mono.just(request), RegisterRequest.class)
                .exchange()
                .expectBody(AuthenticationResponse.class)
                .returnResult().getResponseBody();
        assertNotNull(response);
        jwt = response.getToken();
        user = User.builder()
                .id(response.getId())
                .email(response.getEmail())
                .lastName(response.getLastName())
                .firstName(response.getFirstName())
                .roles(response.getRoles())
                .build();
    }

    @Test
    void createProfile() {
        ProfileDTO profile = ProfileDTO.builder().user(user).build();
        ProfileDTO response = webTestClient
                .post()
                .uri("/api/v1/my-profile/create")
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(profile), ProfileDTO.class)
                .exchange()
                .expectBody(ProfileDTO.class)
                .returnResult().getResponseBody();
        assertEquals(profile.getId(), response.getId());
        userProfileId = response.getId();
    }


    @Test
    void updateProfile() {
        ProfileDTO profile = ProfileDTO.builder().user(user).build();
        ProfileDTO response = webTestClient
                .put()
                .uri("/api/v1/my-profile/update/{userProfileId}", userProfileId)
                .header("Authorization","Bearer " + jwt)
                .body(Mono.just(profile), ProfileDTO.class)
                .exchange()
                .expectBody(ProfileDTO.class)
                .returnResult().getResponseBody();
    }

    @Test
    void getProfileId() {
        ProfileDTO response = webTestClient
                .get()
                .uri("/api/v1/my-profile/{userProfileId}", userProfileId)
                .header("Authorization","Bearer " + jwt)
                .exchange()
                .expectBody(ProfileDTO.class)
                .returnResult().getResponseBody();
        assertEquals(userProfileId, response.getId());
    }
}
