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
import com.tyiu.corn.model.responses.AuthenticationResponse;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
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

public class IdeaControllerTest {

    @Autowired
    private WebTestClient webTestClient;
    private String jwt;
    private String ideaId;

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

        UserDTO userDTO = UserDTO.builder()
                .id(response.getId())
                .email(response.getEmail())
                .lastName(response.getLastName())
                .firstName(response.getFirstName())
                .roles(response.getRoles())
                .build();

        GroupDTO expertGroup = GroupDTO.builder()
                .name("ExpertGroup")
                .roles(List.of(Role.ADMIN, Role.EXPERT))
                .users(List.of(userDTO))
                .build();

        Group addGroupResponse = webTestClient
                .post()
                .uri("/api/v1/group/add")
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(expertGroup), GroupDTO.class)
                .exchange()
                .expectBody(Group.class)
                .returnResult().getResponseBody();

        IdeaDTO idea = IdeaDTO.builder()
                .experts(addGroupResponse)
                .name("Идея")
                .status(StatusIdea.NEW)
                .build();

        IdeaDTO postResponse = webTestClient
                .post()
                .uri("/api/v1/idea/add")
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(idea), IdeaDTO.class)
                .exchange()
                .expectBody(IdeaDTO.class)
                .returnResult().getResponseBody();

        ideaId = postResponse.getId();

    }

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

    }
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


    @Test
    void testDeleteIdea(){
        webTestClient
                .delete()
                .uri("/api/v1/idea/delete/{ideaId}", ideaId)
                .header("Authorization","Bearer " + jwt)
                .exchange()
                .expectStatus().isOk();
    }
    @Test
    void testUpdateIdeaByInitiator(){

    }

    @Test
    void testUpdateStatusByInitiator(){

    }

    @Test
    void testUpdateStatusIdeaByProjectOffice(){

    }

    @Test
    void testUpdateIdeaByAdmin(){

    }


//    @Test
//    void b(){
//        Idea idea8 = Idea.builder().name("title1").build();
//        Idea response0 = webTestClient
//                .post()
//                .uri("/api/v1/idea/initiator/add")
//                .header("Authorization","Bearer " + jwt)
//                .body(Mono.just(idea8), Idea.class)
//                .exchange()
//                .expectBody(Idea.class)
//                .returnResult().getResponseBody();
//        String id = response0.getId();
//        webTestClient
//                .put()
//                .uri("/api/v1/idea/initiator/send/{id}", id)
//                .header("Authorization","Bearer " + jwt)
//                .exchange()
//                .expectStatus().isOk();
//        StatusIdea response = webTestClient
//                .put()
//                .uri("/api/v1/idea/project-office/update/{id}", id)
//                .header("Authorization","Bearer " + jwt)
//                .body(Mono.just(StatusIdea.ON_CONFIRMATION), StatusIdea.class)
//                .exchange()
//                .expectBody(StatusIdea.class)
//                .returnResult().getResponseBody();
//        assertNull(response);
//    }
//
//    @Test
//    void testUpdateStatusByExpert(){
//        Idea idea9 = Idea.builder().name("title1").build();
//        Idea response0 = webTestClient
//                .post()
//                .uri("/api/v1/idea/initiator/add")
//                .header("Authorization","Bearer " + jwt)
//                .body(Mono.just(idea9), Idea.class)
//                .exchange()
//                .expectBody(Idea.class)
//                .returnResult().getResponseBody();
//        String id = response0.getId();
//        RatingDTO ratingDTO = RatingDTO.builder().status(StatusIdea.CONFIRMED).rating(4.4)
//                                    .build();
//        RatingDTO response = webTestClient
//                .put()
//                .uri("/api/v1/idea/expert/update/{id}", id)
//                .header("Authorization","Bearer " + jwt)
//                .body(Mono.just(ratingDTO), RatingDTO.class)
//                .exchange()
//                .expectBody(RatingDTO.class)
//                .returnResult().getResponseBody();
//        assertNull(response);
//    }
//
//    @Test
//    void testUpdateIdeaByAdmin(){
//        Idea idea = Idea.builder().name("title").build();
//        Idea response0 = webTestClient
//                .post()
//                .uri("/api/v1/idea/initiator/add")
//                .header("Authorization","Bearer " + jwt)
//                .body(Mono.just(idea), Idea.class)
//                .exchange()
//                .expectBody(Idea.class)
//                .returnResult().getResponseBody();
//        String id = response0.getId();
//        idea = Idea.builder().name("title2").build();
//        Idea response = webTestClient
//                .put()
//                .uri("/api/v1/idea//admin/update/{id}", id)
//                .header("Authorization","Bearer " + jwt)
//                .body(Mono.just(idea), Idea.class)
//                .exchange()
//                .expectBody(Idea.class)
//                .returnResult().getResponseBody();
//        assertNull(response);
//    }
}
