package com.tyiu.corn.controller;

import com.tyiu.corn.model.entities.Idea;
import com.tyiu.corn.model.entities.Idea.IdeaBuilder;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.web.reactive.server.WebTestClient;
import org.springframework.web.reactive.function.client.WebClient;

import reactor.core.publisher.Mono;

import java.util.Date;
import java.util.List;

import static org.junit.Assert.assertNotNull;
import static org.springframework.boot.test.context.SpringBootTest.WebEnvironment.RANDOM_PORT;

@SpringBootTest(webEnvironment = RANDOM_PORT)
public class IdeaControllerTest {
    @Autowired
    private WebTestClient webTestClient;

    @Test
    void canGetIdea(){
        webTestClient
                .get()
                .uri("/api/v1/idea")
                .exchange()
                .expectBodyList(Idea.class)
                .returnResult().getResponseBody();
    }

    @Test
    void canAddIdea(){
        Idea idea = Idea.builder().date("31.31.3131").title("title").build();
        webTestClient
                .post()
                .uri("/api/v1/idea/add")
                .body(Mono.just(idea), Idea.class)
                .exchange()
                .expectBody(Idea.class)
                .returnResult().getResponseBody();
    }

    @Test
    void canDeleteIdea(){
        Idea idea = Idea.builder().id(1L).date("31.31.3131").title("title").build();
        Idea response = webTestClient
                .post()
                .uri("/api/v1/idea/add")
                .body(Mono.just(idea), Idea.class)
                .exchange()
                .expectBody(Idea.class)
                .returnResult().getResponseBody();
        Long id = 1L;
        assertNotNull(id);
        webTestClient
                .delete()
                .uri("/api/v1/idea/delete/{id}", id)
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
                .body(Mono.just(idea), Idea.class)
                .exchange()
                .expectBody(Idea.class)
                .returnResult().getResponseBody();
        idea = Idea.builder().date("31.31.3131").title("title").build();
        webTestClient
                .put()
                .uri("/api/v1/idea/update/{id}", id)
                .body(Mono.just(idea), Idea.class)
                .exchange()
                .expectStatus().isOk();
    }
}
