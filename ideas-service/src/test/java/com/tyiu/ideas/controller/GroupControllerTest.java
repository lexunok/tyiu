package com.tyiu.ideas.controller;

import com.tyiu.ideas.TestContainers;
import com.tyiu.ideas.model.dto.GroupDTO;
import com.tyiu.ideas.model.dto.UserDTO;
import com.tyiu.ideas.model.enums.Role;
import com.tyiu.ideas.model.requests.RegisterRequest;
import com.tyiu.ideas.model.responses.AuthenticationResponse;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.TestInstance;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.web.reactive.server.WebTestClient;
import reactor.core.publisher.Mono;

import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.springframework.boot.test.context.SpringBootTest.WebEnvironment.RANDOM_PORT;

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringBootTest(webEnvironment = RANDOM_PORT)
class GroupControllerTest extends TestContainers {

    @Autowired
    private WebTestClient webTestClient;

    private String jwt;
    private UserDTO userDTO;

    private List<Role> roles;

    private GroupDTO createGroup(){
        GroupDTO group = GroupDTO.builder()
                .name("title")
                .users(List.of(userDTO))
                .roles(roles)
                .build();
        return webTestClient
                .post()
                .uri("/api/v1/ideas-service/group/create")
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(group), GroupDTO.class)
                .exchange()
                .expectBody(GroupDTO.class)
                .returnResult().getResponseBody();
    }



    @BeforeAll
    public void setUp() {
        RegisterRequest request = new RegisterRequest(
                "fakemail2", "fakename", "fakename", "fakepass",
                List.of(Role.ADMIN,
                        Role.EXPERT,
                        Role.PROJECT_OFFICE,
                        Role.INITIATOR));
        AuthenticationResponse response = webTestClient
                .post()
                .uri("/api/v1/ideas-service/auth/register")
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
        roles = new ArrayList<>();
        roles.add(Role.EXPERT);
    }

    @Test
    void testCreateGroup() {
        GroupDTO createdGroup = createGroup();
        assertEquals("title", createdGroup.getName());
    }

    @Test
    void testUpdateGroup(){
        GroupDTO group5 = GroupDTO.builder()
                .name("title")
                .users(List.of(userDTO))
                .roles(roles)
                .build();
        GroupDTO response4 = webTestClient
                .post()
                .uri("/api/v1/ideas-service/group/create")
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(group5), GroupDTO.class)
                .exchange()
                .expectBody(GroupDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(response4);
        assertEquals(group5.getName(), response4.getName());
        String id = response4.getId();
        group5 = GroupDTO.builder()
                .name("title 2")
                .users(List.of(userDTO))
                .roles(roles)
                .build();
        webTestClient
                .put()
                .uri("/api/v1/ideas-service/group/update/{id}", id)
                .header("Authorization","Bearer " + jwt)
                .body(Mono.just(group5), GroupDTO.class)
                .exchange()
                .expectStatus().isOk();
    }

    @Test
    void testDeleteGroup(){
        GroupDTO group6 = GroupDTO.builder()
                .name("title")
                .users(List.of(userDTO))
                .roles(roles)
                .build();
        GroupDTO response6 = webTestClient
                .post()
                .uri("/api/v1/ideas-service/group/create")
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(group6), GroupDTO.class)
                .exchange()
                .expectBody(GroupDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(response6);
        assertEquals(group6.getName(), response6.getName());
        String id = response6.getId();
        webTestClient
                .delete()
                .uri("/api/v1/ideas-service/group/delete/{id}", id)
                .header("Authorization","Bearer " + jwt)
                .exchange()
                .expectStatus().isOk();
    }

    @Test
    void testGetGroupById(){
        GroupDTO group = GroupDTO.builder()
                .name("title")
                .users(List.of(userDTO))
                .roles(roles)
                .build();
        GroupDTO addGroupResponse = webTestClient
                .post()
                .uri("/api/v1/ideas-service/group/create")
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(group), GroupDTO.class)
                .exchange()
                .expectBody(GroupDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(addGroupResponse);
        assertEquals(group.getName(), addGroupResponse.getName());
        String id = addGroupResponse.getId();

        GroupDTO responseGet = webTestClient
                .get()
                .uri("/api/v1/ideas-service/group/{id}", id)
                .header("Authorization","Bearer " + jwt)
                .exchange()
                .expectBody(GroupDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(responseGet);
        assertEquals(group.getName(), responseGet.getName());
    }

    @Test
    void testGetAllGroups() {
        GroupDTO group1 = GroupDTO.builder()
                .name("title")
                .users(List.of(userDTO))
                .roles(roles)
                .build();
        webTestClient
                .post()
                .uri("/api/v1/ideas-service/group/create")
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(group1), GroupDTO.class)
                .exchange()
                .expectBody(GroupDTO.class)
                .returnResult();

        GroupDTO group = GroupDTO.builder()
                .name("title 2")
                .users(List.of(userDTO))
                .roles(roles)
                .build();

        webTestClient
                .post()
                .uri("/api/v1/ideas-service/group/create")
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(group), GroupDTO.class)
                .exchange()
                .expectBody(GroupDTO.class)
                .returnResult();

        List<GroupDTO> allGroup = webTestClient
                .get()
                .uri("api/v1/ideas-service/group/all")
                .header("Authorization", "Bearer " + jwt)
                .exchange()
                .expectBodyList(GroupDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(allGroup);
    }

    @Test
    void testGetAllUsersByGroup(){
        RegisterRequest request = new RegisterRequest("user@mail.com", "firstname", "lastname", "password", List.of(Role.ADMIN));
        AuthenticationResponse user = webTestClient
                .post()
                .uri("/api/v1/ideas-service/auth/register")
                .body(Mono.just(request), RegisterRequest.class)
                .exchange()
                .expectBody(AuthenticationResponse.class)
                .returnResult().getResponseBody();
        assertNotNull(user);

        UserDTO userDTO = UserDTO.builder()
                .id(user.getId())
                .email(user.getEmail())
                .lastName(user.getLastName())
                .firstName(user.getFirstName())
                .roles(user.getRoles())
                .build();

        GroupDTO expectedGroup = GroupDTO.builder()
                .name("Group 1")
                .roles(List.of(Role.ADMIN))
                .users(List.of(userDTO))
                .build();

        webTestClient
                .post()
                .uri("/api/v1/ideas-service/group/create")
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(expectedGroup), GroupDTO.class)
                .exchange();

        GroupDTO groupWithOutUser =  GroupDTO.builder()
                .name("Group 2")
                .roles(List.of(Role.ADMIN))
                .build();

        webTestClient
                .post()
                .uri("/api/v1/ideas-service/group/create")
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(groupWithOutUser), GroupDTO.class)
                .exchange();

        webTestClient
                .get()
                .uri("/api/v1/ideas-service/group/all/{userId}", user.getId())
                .header("Authorization", "Bearer " + jwt)
                .exchange()
                .expectBodyList(GroupDTO.class)
                .returnResult();
    }
}

