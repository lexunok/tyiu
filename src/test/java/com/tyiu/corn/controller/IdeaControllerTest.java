package com.tyiu.corn.controller;

import com.tyiu.corn.model.entities.Idea;

import com.tyiu.corn.model.enums.Role;
import com.tyiu.corn.model.requests.RegisterRequest;
import com.tyiu.corn.model.responses.AuthenticationResponse;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.web.reactive.server.WebTestClient;

import static org.junit.jupiter.api.Assertions.*;
import reactor.core.publisher.Mono;

import java.util.List;
;

import static org.springframework.boot.test.context.SpringBootTest.WebEnvironment.RANDOM_PORT;
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringBootTest(webEnvironment = RANDOM_PORT)
public class IdeaControllerTest {
    @Autowired
    private WebTestClient webTestClient;
    private String jwt;
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
    }
    @Test
    void canGetIdea(){
        webTestClient
                .get()
                .uri("/api/v1/idea")
                .header("Authorization","Bearer " + jwt)
                .exchange()
                .expectBodyList(Idea.class)
                .returnResult().getResponseBody();
    }

    @Test
    void canAddIdea(){
        Idea idea = Idea.builder().date("31.31.3131").title("title").build();
        Idea response = webTestClient
                .post()
                .uri("/api/v1/idea/add")
                .body(Mono.just(idea), Idea.class)
                .exchange()
                .expectBody(Idea.class)
                .returnResult().getResponseBody();
    }

    @Test
    void canDeleteIdea(){
        Idea idea = Idea.builder()
                .status("Status.FAILED").type("sdlfkj")
                .build();
        webTestClient
                .post()
                .uri("/api/v1/idea/add")
                .header("Authorization","Bearer " + jwt)
                .body(Mono.just(idea), Idea.class)
                .exchange()
                .expectStatus().isOk();
        Long id = 1L;
        webTestClient
                .delete()
                .uri("/api/v1/idea/delete/{id}", id)
                .header("Authorization","Bearer " + jwt)
                .exchange()
                .expectStatus().isOk();
    }

    @Test
    void canUpdateIdea(){
        Long id = 1L;
        Idea idea = Idea.builder().date("32.32.3232").title("title2").build();
        webTestClient
                .post()
                .uri("/api/v1/idea/add")
                .header("Authorization","Bearer " + jwt)
                .body(Mono.just(idea), Idea.class)
                .exchange()
                .expectBody(Idea.class)
                .returnResult().getResponseBody();
        idea = Idea.builder().date("31.31.3131").title("title").build();
        webTestClient
                .put()
                .uri("/api/v1/idea/update/{id}", id)
                .header("Authorization","Bearer " + jwt)
                .body(Mono.just(idea), Idea.class)
                .exchange()
                .expectStatus().isOk();
    }
}
