package com.tyiu.corn.controller;

import com.tyiu.corn.model.dto.GroupDTO;
import com.tyiu.corn.model.dto.IdeaDTO;
import com.tyiu.corn.model.dto.RatingDTO;
import com.tyiu.corn.model.dto.UserDTO;
import com.tyiu.corn.model.entities.Group;
import com.tyiu.corn.model.entities.Idea;

import com.tyiu.corn.model.enums.Role;
import com.tyiu.corn.model.enums.StatusIdea;
import com.tyiu.corn.model.requests.RegisterRequest;
import com.tyiu.corn.model.requests.StatusIdeaRequest;
import com.tyiu.corn.model.responses.AuthenticationResponse;

import org.junit.jupiter.api.*;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.annotation.DirtiesContext;
import org.springframework.test.web.reactive.server.WebTestClient;

import static org.junit.jupiter.api.Assertions.*;
import static org.springframework.boot.test.context.SpringBootTest.WebEnvironment.RANDOM_PORT;


import reactor.core.publisher.Mono;

import java.util.List;

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringBootTest(
        webEnvironment = RANDOM_PORT,
        properties = "de.flapdoodle.mongodb.embedded.version=5.0.5"
)
@DirtiesContext(classMode = DirtiesContext.ClassMode.AFTER_CLASS)
class IdeaControllerTest {

    @Autowired
    private WebTestClient webTestClient;
    private String jwt;
    private String ideaId;

    private UserDTO userDTO;
    private Group group;

    @BeforeAll
    public void setUp(){
        RegisterRequest request = new RegisterRequest("user@mail.com","firstname","lastname","password", List.of(Role.ADMIN, Role.INITIATOR, Role.EXPERT));
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

        GroupDTO expertGroup = GroupDTO.builder()
                .users(List.of(userDTO))
                .build();

        group = webTestClient
                .post()
                .uri("/api/v1/group/add")
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(expertGroup), GroupDTO.class)
                .exchange()
                .expectBody(Group.class)
                .returnResult().getResponseBody();

        IdeaDTO idea = IdeaDTO.builder()
                .experts(group)
                .name("Идея")
                .status(StatusIdea.NEW)
                .build();

        webTestClient
                .post()
                .uri("/api/v1/idea/add")
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(idea), IdeaDTO.class)
                .exchange()
                .expectStatus().isOk();

    }
    @Order(2)
    @Test
    void testGetIdeaForInitiator(){
        IdeaDTO getResponse = webTestClient
                .get()
                .uri("/api/v1/idea/{id}", ideaId)
                .header("Authorization","Bearer " + jwt)
                .exchange()
                .expectBody(IdeaDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(getResponse);
        assertEquals(getResponse.getId(), ideaId);

    }

    @Order(1)
    @Test
    void testShowListIdeaForAdmin(){
        List<IdeaDTO> responseForAdmin = webTestClient
                .get()
                .uri("/api/v1/idea/all")
                .header("Authorization","Bearer " + jwt)
                .exchange()
                .expectBodyList(IdeaDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(responseForAdmin);
    }

    @Order(2)
    @Test
    void testUpdateIdeaByInitiator(){
        IdeaDTO updatedGroup = IdeaDTO.builder()
                .name("Идея 2")
                .build();
        webTestClient
                .put()
                .uri("/api/v1/idea/initiator/send/{ideaId}", ideaId)
                .header("Authorization","Bearer " + jwt)
                .body(Mono.just(updatedGroup), GroupDTO.class)
                .exchange()
                .expectStatus().isOk();
    }

    @Order(3)
    @Test
    void testUpdateStatusByInitiator(){
        StatusIdeaRequest newStatus = new StatusIdeaRequest();
        newStatus.setStatus(StatusIdea.ON_EDITING);
        webTestClient
                .put()
                .uri("/api/v1/idea/project-office/update/{ideaId}", ideaId)
                .header("Authorization","Bearer " + jwt)
                .body(Mono.just(newStatus), StatusIdeaRequest.class)
                .exchange()
                .expectStatus().isOk();
    }
    @Order(4)
    @Test
    void testUpdateStatusIdeaByProjectOffice(){
        StatusIdeaRequest newStatus = new StatusIdeaRequest();
        newStatus.setStatus(StatusIdea.ON_EDITING);
        webTestClient
                .put()
                .uri("/api/v1/idea/project-office/update/{ideaId}", ideaId)
                .header("Authorization","Bearer " + jwt)
                .body(Mono.just(newStatus), StatusIdeaRequest.class)
                .exchange()
                .expectStatus().isOk();
    }
    @Order(5)
    @Test
    void testUpdateIdeaByAdmin(){
        IdeaDTO updatedGroup = IdeaDTO.builder()
                .name("Идея 2")
                .initiator(userDTO.getEmail())
                .experts(group)
                .build();
        webTestClient
                .put()
                .uri("/api/v1/idea/admin/update/{ideaId}", ideaId)
                .header("Authorization","Bearer " + jwt)
                .body(Mono.just(updatedGroup), GroupDTO.class)
                .exchange()
                .expectStatus().isOk();
    }

    @Order(6)
    @Test
    void testDeleteIdea(){
        webTestClient
                .delete()
                .uri("/api/v1/idea/delete/{ideaId}", ideaId)
                .header("Authorization","Bearer " + jwt)
                .exchange()
                .expectStatus().isOk();
    }
}
