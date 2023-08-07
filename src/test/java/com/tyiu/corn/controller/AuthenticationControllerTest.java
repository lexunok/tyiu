package com.tyiu.corn.controller;

import com.tyiu.corn.model.enums.Role;
import com.tyiu.corn.model.requests.RegisterRequest;
import com.tyiu.corn.model.responses.AuthenticationResponse;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.web.reactive.server.WebTestClient;
import reactor.core.publisher.Mono;

import java.util.List;

import static org.springframework.boot.test.context.SpringBootTest.WebEnvironment.RANDOM_PORT;

@SpringBootTest(webEnvironment = RANDOM_PORT)
public class AuthenticationControllerTest {
    @Autowired
    private WebTestClient webTestClient;

    @Test
    void canRegister(){
        RegisterRequest request = new RegisterRequest(
                "edmail","lastnasme","firsdstname","psdassword", List.of(Role.ADMIN,Role.EXPERT));
        webTestClient
                .post()
                .uri("/api/v1/auth/register")
                .body(Mono.just(request), RegisterRequest.class)
                .exchange()
                .expectBody(AuthenticationResponse.class);
    }
}
