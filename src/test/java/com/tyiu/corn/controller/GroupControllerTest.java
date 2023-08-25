package com.tyiu.corn.controller;

import com.tyiu.corn.model.dto.GroupDTO;
import com.tyiu.corn.model.entities.Group;
import com.tyiu.corn.model.entities.Idea;
import com.tyiu.corn.model.enums.Role;
import com.tyiu.corn.model.requests.RegisterRequest;
import com.tyiu.corn.model.responses.AuthenticationResponse;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.BeforeAll;
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
@SpringBootTest(webEnvironment = RANDOM_PORT)
public class GroupControllerTest {

    @Autowired
    private WebTestClient webTestClient;

    private String jwt;


    @BeforeAll
    public void setUp() {
        RegisterRequest request = new RegisterRequest(
                "fakemail", "fakename", "fakename", "fakepass", List.of(Role.ADMIN));
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
    void testCreateGroup() {
        GroupDTO group3 = GroupDTO.builder().name("title").build();
        GroupDTO response3 = webTestClient
                .post()
                .uri("/api/v1/group/add")
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(group3), GroupDTO.class)
                .exchange()
                .expectBody(GroupDTO.class)
                .returnResult().getResponseBody();
        assertEquals(group3.getName(), response3.getName());
    }

    @Test
    void testUpdateGroup(){
        GroupDTO group5 = GroupDTO.builder().name("title").build();
        GroupDTO response4 = webTestClient
                .post()
                .uri("/api/v1/group/add")
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(group5), GroupDTO.class)
                .exchange()
                .expectBody(GroupDTO.class)
                .returnResult().getResponseBody();
        assertEquals(group5.getName(), response4.getName());
        Long id = response4.getId();
        group5 = GroupDTO.builder().name("title 2").build();
        GroupDTO response5 = webTestClient
                .put()
                .uri("/api/v1/group/update/{id}", id)
                .header("Authorization","Bearer " + jwt)
                .body(Mono.just(group5), GroupDTO.class)
                .exchange()
                .expectBody(GroupDTO.class)
                .returnResult().getResponseBody();
    }

}
