package com.tyiu.ideas.controller;

import com.tyiu.ideas.model.enums.Role;
import com.tyiu.ideas.model.requests.LoginRequest;
import com.tyiu.ideas.model.requests.RegisterRequest;
import com.tyiu.ideas.model.responses.AuthenticationResponse;
import org.junit.jupiter.api.*;
import org.springframework.beans.factory.annotation.Autowired;

import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.web.reactive.server.WebTestClient;
import reactor.core.publisher.Mono;

import static org.junit.jupiter.api.Assertions.*;

import java.util.List;

import static org.springframework.boot.test.context.SpringBootTest.WebEnvironment.RANDOM_PORT;

@SpringBootTest(webEnvironment = RANDOM_PORT)
class AuthenticationControllerTest extends TestContainers{
    @Autowired
    private WebTestClient webTestClient;

    @Test
    void canRegister(){
        RegisterRequest request = new RegisterRequest(
                "authentication","auth",
                "auth","password", List.of(Role.ADMIN,Role.EXPERT));
        AuthenticationResponse response = webTestClient
                .post()
                .uri("/api/v1/ideas-service/auth/register")
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
    void canRegisterWhenUserExists(){
        RegisterRequest request = new RegisterRequest(
                "authentication2","auth",
                "auth","password", List.of(Role.ADMIN,Role.EXPERT));
        webTestClient
                .post()
                .uri("/api/v1/ideas-service/auth/register")
                .body(Mono.just(request), RegisterRequest.class)
                .exchange()
                .expectStatus().isOk();
        webTestClient
                .post()
                .uri("/api/v1/auth/register")
                .body(Mono.just(request), RegisterRequest.class)
                .exchange()
                .expectStatus().is4xxClientError();
    }

    @Test
    void canLoginWhenUserNotExists(){
        LoginRequest request = new LoginRequest(
                "notregistered","password");
        webTestClient
                .post()
                .uri("/api/v1/ideas-service/auth/login")
                .body(Mono.just(request), RegisterRequest.class)
                .exchange()
                .expectStatus().isBadRequest();
    }
    @Test
    void canLoginWhenUserExists(){
        RegisterRequest registerRequest = new RegisterRequest(
                "authentication3","auth",
                "auth","password", List.of(Role.ADMIN,Role.EXPERT));
        webTestClient
                .post()
                .uri("/api/v1/ideas-service/auth/register")
                .body(Mono.just(registerRequest), RegisterRequest.class)
                .exchange()
                .expectStatus().isOk();
        LoginRequest request = new LoginRequest("authentication3","password");
        AuthenticationResponse response = webTestClient
                .post()
                .uri("/api/v1/ideas-service/auth/login")
                .body(Mono.just(request), RegisterRequest.class)
                .exchange()
                .expectBody(AuthenticationResponse.class).returnResult().getResponseBody();
        assertNotNull(response);
        assertNotNull(response.getToken());
    }
    @Test
    void canLoginWhenPasswordNotCorrect(){
        RegisterRequest registerRequest = new RegisterRequest(
                "authentication4","auth","auth",
                "password", List.of(Role.ADMIN,Role.EXPERT));
        webTestClient
                .post()
                .uri("/api/v1/ideas-service/auth/register")
                .body(Mono.just(registerRequest), RegisterRequest.class)
                .exchange()
                .expectStatus().isOk();
        LoginRequest request = new LoginRequest("authentication4","passwordnotcorrect");
        webTestClient
                .post()
                .uri("/api/v1/ideas-service/auth/login")
                .body(Mono.just(request), RegisterRequest.class)
                .exchange()
                .expectStatus()
                .is4xxClientError();
    }

}
