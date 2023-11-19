package com.tyiu.corn.controller;

import com.tyiu.corn.model.dto.*;
import com.tyiu.corn.model.enums.Role;
import com.tyiu.corn.model.enums.StatusIdea;
import com.tyiu.corn.model.requests.RegisterRequest;
import com.tyiu.corn.model.responses.AuthenticationResponse;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.codec.cbor.Jackson2CborDecoder;
import org.springframework.http.codec.cbor.Jackson2CborEncoder;
import org.springframework.messaging.rsocket.RSocketRequester;
import org.springframework.messaging.rsocket.RSocketStrategies;
import org.springframework.test.web.reactive.server.WebTestClient;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.util.List;

import static org.junit.jupiter.api.Assertions.*;
import static org.springframework.boot.test.context.SpringBootTest.WebEnvironment.RANDOM_PORT;

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringBootTest(webEnvironment = RANDOM_PORT)
public class CommentControllerTest extends TestContainers {
    @Autowired
    private RSocketRequester.Builder rsockerRequesterBuilder;
    private RSocketRequester rsocketRequester;
    @Autowired
    private WebTestClient webTestClient;
    private UserDTO userDTO;
    private String jwt;
    private GroupDTO projectGroup;
    private GroupDTO expertGroup;



    @BeforeAll
    public void setUp() {

        RegisterRequest request = new RegisterRequest("user.comment@mail.com", "firstname", "lastname", "password",
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

        IdeaDTO ideaDTO = IdeaDTO.builder()
                .initiatorEmail(userDTO.getEmail())
                .name("ГДЗ онлайн")
                .status(StatusIdea.ON_APPROVAL)
                .experts(expertGroup)
                .projectOffice(projectGroup)
                .problem("Отсутствия готовых решений задач")
                .solution("Форум, где студенты могут оставить свои решения")
                .result("Удобная онлайн платформа")
                .customer("Студенты")
                .contactPerson("Стас")
                .description("Для студентов!")
                .build();

        IdeaDTO ideaResponse = webTestClient
                .post()
                .uri("/api/v1/idea/add")
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(ideaDTO), IdeaDTO.class)
                .exchange()
                .expectBody(IdeaDTO.class)
                .returnResult().getResponseBody();

        assertNotNull(ideaResponse);
//        int port = 7000;
//        RSocketStrategies strategies = RSocketStrategies.builder()
//                .encoders(encoders -> encoders.add(new Jackson2CborEncoder()))
//                .decoders(decoders -> decoders.add(new Jackson2CborDecoder()))
//                .build();
//
//        rsocketRequester = RSocketRequester.builder()
//                .rsocketStrategies(strategies)
//                .tcp("localhost", port);
    }
    public String createIdea() {
        IdeaDTO ideaDTO = IdeaDTO.builder()
                .initiatorEmail(userDTO.getEmail())
                .name("ГДЗ онлайн 2")
                .status(StatusIdea.ON_APPROVAL)
                .experts(expertGroup)
                .projectOffice(projectGroup)
                .problem("Отсутствия готовых решений задач")
                .solution("Форум, где студенты могут оставить свои решения")
                .result("Удобная онлайн платформа")
                .customer("Студенты")
                .contactPerson("Стас")
                .description("Для студентов!")
                .build();

        IdeaDTO ideaResponse = webTestClient
                .post()
                .uri("/api/v1/idea/add")
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(ideaDTO), IdeaDTO.class)
                .exchange()
                .expectBody(IdeaDTO.class)
                .returnResult().getResponseBody();
        return ideaResponse.getId();
    }

    @Test
    public void testCreateComment() {

        CommentDTO commentDTO = CommentDTO.builder()
                .text("Коммент 1")
                .ideaId(createIdea())
                .build();

        CommentDTO response = webTestClient
                .post()
                .uri("/api/v1/comment/send")
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(commentDTO), CommentDTO.class)
                .exchange()
                .expectBody(CommentDTO.class)
                .returnResult().getResponseBody();

        assertNotNull(response);
        assertEquals(commentDTO.getText(), response.getText());
    }
    /*
    @Test
    public void testReceiveNewComments(){
        Flux<CommentDTO> response = rsocketRequester
                .route("/api/v1/comment/comment.{ideaId}.receive", ideaId)
                .retrieveFlux(CommentDTO.class);
        Flux<String> responseList = response.map(c -> c.getText());

        CommentDTO commentDTO = CommentDTO.builder()
                .text("Коммент 1")
                .ideaId(ideaId)
                .build();

        CommentDTO responsePost = webTestClient
                .post()
                .uri("/api/v1/comment/send")
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(commentDTO), CommentDTO.class)
                .exchange()
                .expectBody(CommentDTO.class)
                .returnResult().getResponseBody();

        assertNotNull(responsePost);
        assertEquals(commentDTO.getText(), responsePost.getText());

        StepVerifier.create(responseList)
                .expectNext(commentDTO.getText())
                .verifyComplete();

    }
    */

    @Test
    public void testGetAllComments() {
        String ideaId = createIdea();
                CommentDTO commentDTO = CommentDTO.builder()
                .text("Коммент 2")
                .ideaId(ideaId)
                .build();

        CommentDTO response = webTestClient
                .post()
                .uri("/api/v1/comment/send")
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(commentDTO), CommentDTO.class)
                .exchange()
                .expectBody(CommentDTO.class)
                .returnResult().getResponseBody();

        List<CommentDTO> comments = webTestClient
                .get()
                .uri("api/v1/comment/all/{ideaId}", ideaId)
                .header("Authorization", "Bearer " + jwt)
                .exchange()
                .expectBodyList(CommentDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(comments);
        assertFalse(comments.isEmpty());
    }

    @Test
    public void testCheckComment() {
        String ideaId = createIdea();
        CommentDTO commentDTO = CommentDTO.builder()
                .text("Коммент 3")
                .ideaId(ideaId)
                .build();

        CommentDTO response = webTestClient
                .post()
                .uri("/api/v1/comment/send")
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(commentDTO), CommentDTO.class)
                .exchange()
                .expectBody(CommentDTO.class)
                .returnResult().getResponseBody();

        assertNotNull(response);
        assertEquals(commentDTO.getText(), response.getText());

        webTestClient
                .put()
                .uri("/api/v1/comment/check/{commentId}", response.getId())
                .header("Authorization", "Bearer " + jwt)
                .exchange()
                .expectStatus().isOk();
    }

    @Test
    public void testDeleteComment() {
        String ideaId = createIdea();
        CommentDTO commentDTO = CommentDTO.builder()
                .text("Коммент 4")
                .ideaId(ideaId)
                .build();

        CommentDTO response = webTestClient
                .post()
                .uri("/api/v1/comment/send")
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(commentDTO), CommentDTO.class)
                .exchange()
                .expectBody(CommentDTO.class)
                .returnResult().getResponseBody();

        assertNotNull(response);
        assertEquals(commentDTO.getText(), response.getText());

        webTestClient
                .delete()
                .uri("/api/v1/comment/delete/{commentId}", response.getId())
                .header("Authorization", "Bearer " + jwt)
                .exchange()
                .expectStatus().isOk();

    }

//    @AfterAll
//    public void tearDown() {
//        rsocketRequester.rsocket().dispose();
//    }

}
