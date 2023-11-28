package com.tyiu.corn.controller;

import com.tyiu.corn.model.dto.*;
import com.tyiu.corn.model.entities.Idea;
import com.tyiu.corn.model.enums.Role;
import com.tyiu.corn.model.requests.RegisterRequest;
import com.tyiu.corn.model.responses.AuthenticationResponse;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Order;
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
@SpringBootTest(webEnvironment = RANDOM_PORT)
class RatingControllerTest extends TestContainers {
    @Autowired
    private WebTestClient webTestClient;

    private String jwt;
    private UserDTO userDTO;
    private GroupDTO expertGroup;
    private GroupDTO projectGroup;

    private String ideaId;

    @BeforeAll
    void setUp() {
        RegisterRequest request = new RegisterRequest("user.rating@mail.com", "firstname", "lastname", "password",
                List.of(Role.ADMIN,
                        Role.EXPERT,
                        Role.PROJECT_OFFICE,
                        Role.INITIATOR));
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

        GroupDTO expertGroupDTO = GroupDTO.builder()
                .name("Эксперты")
                .users(List.of(userDTO))
                .roles(List.of(Role.EXPERT))
                .build();

        expertGroup = webTestClient
                .post()
                .uri("/api/v1/group/create")
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(expertGroupDTO), GroupDTO.class)
                .exchange()
                .expectBody(GroupDTO.class)
                .returnResult().getResponseBody();

        GroupDTO projectGroupDTO = GroupDTO.builder()
                .name("Проекты")
                .users(List.of(userDTO))
                .roles(List.of(Role.PROJECT_OFFICE))
                .build();

        projectGroup = webTestClient
                .post()
                .uri("/api/v1/group/create")
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(projectGroupDTO), GroupDTO.class)
                .exchange()
                .expectBody(GroupDTO.class)
                .returnResult().getResponseBody();

//        CompanyDTO company = CompanyDTO.builder()
//                .name("Газпром")
//                .users(List.of(userDTO))
//                .build();
        IdeaDTO idea = IdeaDTO.builder()
                .initiatorEmail(userDTO.getEmail())
                .experts(expertGroup)
                .projectOffice(projectGroup)
                .name("Идея")
                .status(Idea.Status.NEW)
//                .company(company)
                .build();

        IdeaDTO ideaResponse = webTestClient
                .post()
                .uri("/api/v1/idea/add")
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(idea), IdeaDTO.class)
                .exchange()
                .expectBody(IdeaDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(ideaResponse);
        ideaId = ideaResponse.getId();
    }

    @Test
    void testGetIdea(){
        IdeaDTO getResponse = webTestClient
                .get()
                .uri("/api/v1/idea/{ideaId}", ideaId)
                .header("Authorization","Bearer " + jwt)
                .exchange()
                .expectBody(IdeaDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(getResponse);
        assertEquals(getResponse.getId(), ideaId);
    }

    @Order(4)
    @Test
    void testConfirmRating(){
        RatingDTO ratingDTO = RatingDTO.builder()
                .expertId(userDTO.getId())
                .ideaId(ideaId)
                .confirmed(false)
                .build();

        webTestClient
                .put()
                .uri("/api/v1/rating/confirm")
                .header("Authorization","Bearer " + jwt)
                .body(Mono.just(ratingDTO), RatingDTO.class)
                .exchange()
                .expectStatus().isOk();
    }
    @Order(1)
    @Test
    void testSaveRating(){
        RatingDTO ratingDTO = RatingDTO.builder()
                .expertId(userDTO.getId())
                .budget(400000L)
                .marketValue(2L)
                .originality(3L)
                .suitability(4L)
                .technicalRealizability(3L)
                .ideaId(ideaId)
                .build();

        webTestClient
                .put()
                .uri("/api/v1/rating/save")
                .header("Authorization","Bearer " + jwt)
                .body(Mono.just(ratingDTO), RatingDTO.class)
                .exchange()
                .expectStatus().isOk();
    }

    @Order(2)
    @Test
    void testGetAllIdeasRatings() {
        List<RatingDTO> rating = webTestClient
                .get()
                .uri("/api/v1/rating/all/{ideaId}", ideaId)
                .header("Authorization","Bearer " + jwt)
                .exchange()
                .expectBodyList(RatingDTO.class)
                .returnResult().getResponseBody();

        assertNotNull(rating);
    }

    @Order(3)
    @Test
    void testGetExpertRatingForIdea(){
        RatingDTO rating = webTestClient
                .get()
                .uri("/api/v1/rating/{ideaId}", ideaId)
                .header("Authorization","Bearer " + jwt)
                .exchange()
                .expectBody(RatingDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(rating);
    }
}
