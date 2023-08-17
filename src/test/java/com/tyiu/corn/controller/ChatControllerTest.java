package com.tyiu.corn.controller;

import com.tyiu.corn.model.entities.Chat;
import com.tyiu.corn.model.enums.Role;
import com.tyiu.corn.model.requests.RegisterRequest;
import com.tyiu.corn.model.responses.AuthenticationResponse;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.web.reactive.server.WebTestClient;

import static org.junit.jupiter.api.Assertions.*;
import reactor.core.publisher.Mono;

import java.util.List;

import static org.springframework.boot.test.context.SpringBootTest.WebEnvironment.RANDOM_PORT;

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringBootTest(webEnvironment = RANDOM_PORT)
public class ChatControllerTest {
    @Autowired
    private WebTestClient webTestClient;
    private String jwt;
    @BeforeAll
    public void setUp(){
        RegisterRequest request = new RegisterRequest(
                "fakefsfddskeemail","fakename","fakename","fakepass", List.of(Role.ADMIN));
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
    void testGetUserChats(){
        Chat chat = new Chat();
        webTestClient
                .post()
                .uri("/api/v1/chat/create")
                .header("Authorization","Bearer " + jwt)
                .body(Mono.just(chat), Chat.class)
                .exchange()
                .expectBody(Chat.class)
                .returnResult().getResponseBody();
        List<Chat> response3 = webTestClient
                .get()
                .uri("/api/v1/chat/all")
                .header("Authorization","Bearer " + jwt)
                .exchange()
                .expectBodyList(Chat.class)
                .returnResult().getResponseBody();
        assertNotNull(response3);
    }

    @Test
    void testFindChat(){
        Chat chat = new Chat();
        Chat response = webTestClient
                .post()
                .uri("/api/v1/chat/create")
                .header("Authorization","Bearer " + jwt)
                .body(Mono.just(chat), Chat.class)
                .exchange()
                .expectBody(Chat.class)
                .returnResult().getResponseBody();
        Long id = response.getId(); 
        List<Chat> response3 = webTestClient
                .get()
                .uri("/api/v1/chat/get/{id}", id)
                .header("Authorization","Bearer " + jwt)
                .exchange()
                .expectBodyList(Chat.class)
                .returnResult().getResponseBody();
        assertNotNull(response3);
    }

    @Test
    void testCreateChat(){
        Chat chat = new Chat();
        Chat response = webTestClient
                .post()
                .uri("/api/v1/chat/create")
                .header("Authorization","Bearer " + jwt)
                .body(Mono.just(chat), Chat.class)
                .exchange()
                .expectBody(Chat.class)
                .returnResult().getResponseBody();
        assertNotNull(response);
    }

    @Test
    void testSendMessageToLocal(){
        Chat chat = new Chat();
        Chat response = webTestClient
                .post()
                .uri("/api/v1/chat/create")
                .header("Authorization","Bearer " + jwt)
                .body(Mono.just(chat), Chat.class)
                .exchange()
                .expectBody(Chat.class)
                .returnResult().getResponseBody();
        Long id = response.getId();
        webTestClient
                .post()
                .uri("/api/v1/chat/add/{id}", id)
                .header("Authorization","Bearer " + jwt)
                .body(Mono.just(chat), Chat.class)
                .exchange()
                .expectBody(Chat.class)
                .returnResult().getResponseBody();
        assertNotNull(response);
    }
}
