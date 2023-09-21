package com.tyiu.corn.controller;

import com.tyiu.corn.model.enums.Role;
import com.tyiu.corn.model.requests.LoginRequest;
import com.tyiu.corn.model.requests.RegisterRequest;
import com.tyiu.corn.model.responses.AuthenticationResponse;
import org.junit.jupiter.api.*;
import org.springframework.beans.factory.annotation.Autowired;

import org.springframework.boot.test.context.SpringBootTest;

import org.springframework.test.web.reactive.server.WebTestClient;
import reactor.core.publisher.Mono;
import static org.junit.jupiter.api.Assertions.*;

import java.util.List;

import static org.springframework.boot.test.context.SpringBootTest.WebEnvironment.RANDOM_PORT;


@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringBootTest(
        webEnvironment = RANDOM_PORT,
        properties = "de.flapdoodle.mongodb.embedded.version=5.0.5"
)
class AuthenticationControllerTest {
    @Autowired
    private WebTestClient webTestClient;

    @Order(1)
    @Test
    void canRegister(){
        RegisterRequest request = new RegisterRequest(
                "edqmail","lastnasme","firsdstname","psdassword", List.of(Role.ADMIN,Role.EXPERT));
        AuthenticationResponse response =webTestClient
                .post()
                .uri("/api/v1/auth/register")
                .body(Mono.just(request), RegisterRequest.class)
                .exchange()
                .expectBody(AuthenticationResponse.class)
                .returnResult().getResponseBody();
        assertNotNull(response);
        assertEquals(request.getEmail(),response.getEmail());
        assertEquals(request.getFirstName(),response.getFirstName());
        assertEquals(request.getLastName(),response.getLastName());
        assertEquals(request.getRoles(),response.getRoles());
        assertTrue(response.getToken().length()>10);
    }

    @Test
    void canLoginWhenUserNotExists(){
        LoginRequest request = new LoginRequest(
                "edasmail3","lasdfsdsdsf");
        webTestClient
                .post()
                .uri("/api/v1/auth/login")
                .body(Mono.just(request), RegisterRequest.class)
                .exchange()
                .expectStatus().isBadRequest();
    }
    @Test
    void canLoginWhenUserExists(){
        RegisterRequest registerRequest = new RegisterRequest(
                "email1","lastnasme","firsdstname","psdassword", List.of(Role.ADMIN,Role.EXPERT));
        webTestClient
                .post()
                .uri("/api/v1/auth/register")
                .body(Mono.just(registerRequest), RegisterRequest.class)
                .exchange()
                .expectBody(AuthenticationResponse.class);
        LoginRequest request = new LoginRequest(
                "email1","psdassword");
        AuthenticationResponse response = webTestClient
                .post()
                .uri("/api/v1/auth/login")
                .body(Mono.just(request), RegisterRequest.class)
                .exchange()
                .expectBody(AuthenticationResponse.class).returnResult().getResponseBody();
        assertNotNull(response);
        assertNotNull(response.getToken());
    }
    @Test
    void canLoginWhenPasswordNotCorrect(){
        RegisterRequest registerRequest = new RegisterRequest(
                "email2","lastnasme","firsdstname","psdassword", List.of(Role.ADMIN,Role.EXPERT));
        webTestClient
                .post()
                .uri("/api/v1/auth/register")
                .body(Mono.just(registerRequest), RegisterRequest.class)
                .exchange()
                .expectBody(AuthenticationResponse.class);
        LoginRequest request = new LoginRequest(
                "email2","psdasdfdsfssword");
        webTestClient
                .post()
                .uri("/api/v1/auth/login")
                .body(Mono.just(request), RegisterRequest.class)
                .exchange()
                .expectStatus()
                .is4xxClientError();
    }

}
