package com.tyiu.corn.controller;

import com.tyiu.corn.model.entities.Comment;
import com.tyiu.corn.model.entities.Idea;
import com.tyiu.corn.model.enums.Role;
import com.tyiu.corn.model.requests.RegisterRequest;
import com.tyiu.corn.model.responses.AuthenticationResponse;
import com.tyiu.corn.model.responses.ErrorResponse;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.web.reactive.server.WebTestClient;

import static org.junit.jupiter.api.Assertions.*;
import reactor.core.publisher.Mono;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import static org.springframework.boot.test.context.SpringBootTest.WebEnvironment.RANDOM_PORT;

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringBootTest(webEnvironment = RANDOM_PORT)
public class CommentControllerTest {
    @Autowired
    private WebTestClient webTestClient;
    private String jwt;
    private Long ideaId;
    private String sender;
    private String initiatorEmail;
    private String initiatorJwt;
    @BeforeAll
    public void setUp(){
        RegisterRequest initiator = new RegisterRequest(
                "initiator","fakename","fakename","fakepass", List.of(Role.INITIATOR));
        AuthenticationResponse initiatorResponse = webTestClient
                .post()
                .uri("/api/v1/auth/register")
                .body(Mono.just(initiator), RegisterRequest.class)
                .exchange()
                .expectBody(AuthenticationResponse.class)
                .returnResult().getResponseBody();
        assertNotNull(initiatorResponse);
        RegisterRequest projectOffice = new RegisterRequest(
                "projectOffice","fakename","fakename","fakepass", List.of(Role.PROJECT_OFFICE));
        AuthenticationResponse projectOfficeResponse = webTestClient
                .post()
                .uri("/api/v1/auth/register")
                .body(Mono.just(projectOffice), RegisterRequest.class)
                .exchange()
                .expectBody(AuthenticationResponse.class)
                .returnResult().getResponseBody();
        assertNotNull(projectOfficeResponse);
        Idea idea = Idea.builder().budget(12345L).name("Пирожки")
        .customer("f").build();
        Idea ideaResponse = webTestClient.post()
        .uri("/api/v1/idea/initiator/add")
        .header("Authorization","Bearer " + initiatorResponse.getToken())
        .body(Mono.just(idea), Idea.class)
        .exchange().expectBody(Idea.class)
        .returnResult().getResponseBody();

        assertNotNull(ideaResponse);
        ideaId = ideaResponse.getId();
        jwt = projectOfficeResponse.getToken();
        sender = projectOfficeResponse.getEmail();
        initiatorEmail = initiator.getEmail();
        initiatorJwt = initiatorResponse.getToken();
    }    

    @Test
    void createComment(){
        Comment comment = Comment.builder().sender(sender).checkedBy(List.of(sender)).comment("Доделай").build();
        
        Comment response = webTestClient.post()
        .uri("/api/v1/comment/add/{ideaId}", ideaId)
        .header("Authorization","Bearer " + jwt)
        .body(Mono.just(comment), Comment.class)
        .exchange().expectBody(Comment.class)
        .returnResult().getResponseBody();

        assertNotNull(response);
        assertEquals(comment.getSender(), response.getSender());
        assertEquals(comment.getCheckedBy(), response.getCheckedBy());
        assertEquals(comment.getComment(), response.getComment());
    }

    @Test
    void createCommentIfIdeaNotExist(){
        Comment comment = Comment.builder().sender(sender).checkedBy(List.of(sender)).comment("Доделай").build();
        Long id = 1314513L;
        ErrorResponse response = webTestClient.post()
        .uri("/api/v1/comment/add/{id}", id)
        .header("Authorization","Bearer " + jwt)
        .body(Mono.just(comment), Comment.class)
        .exchange().expectBody(ErrorResponse.class)
        .returnResult().getResponseBody();

        assertNotNull(response);
        assertEquals(String.format("Идеи с id %d не существует", id), response.getError());
    }

    @Test
    void checkComment(){
        Comment commentRequest = Comment.builder().sender(sender).checkedBy(List.of(sender)).comment("Неплохо").build();
        
        Comment response = webTestClient.post()
        .uri("/api/v1/comment/add/{ideaId}", ideaId)
        .header("Authorization","Bearer " + jwt)
        .body(Mono.just(commentRequest), Comment.class)
        .exchange().expectBody(Comment.class)
        .returnResult().getResponseBody();

        assertNotNull(response);
        assertEquals(commentRequest.getSender(), response.getSender());
        assertEquals(commentRequest.getCheckedBy(), response.getCheckedBy());
        assertEquals(commentRequest.getComment(), response.getComment());

        List<String> checkedBy = new ArrayList<String>(commentRequest.getCheckedBy());
        checkedBy.add(initiatorEmail);
        commentRequest.setCheckedBy(checkedBy);
        webTestClient.put()
        .uri("/api/v1/comment/check/{ideaId}",ideaId)
        .header("Authorization","Bearer " + initiatorJwt)
        .body(Mono.just(commentRequest), Comment.class)
        .exchange().expectBody(void.class);

        List<Idea> initiatorIdeas = webTestClient.get()
                .uri("/api/v1/idea/initiator")
                .header("Authorization","Bearer " + initiatorJwt)
                .exchange().expectBodyList(Idea.class)
                .returnResult().getResponseBody();

        assertNotNull(initiatorIdeas);
        initiatorIdeas.stream()
        .filter(idea -> idea.getName()=="Пирожки")
        .forEach(idea -> {
                idea.getComments().stream()
                .filter(comment -> comment.getComment() == "Неплохо" && comment.getSender() == sender)
                .forEach(comment -> {
                        assertEquals(2, comment.getCheckedBy().size());
                        assertEquals(sender, comment.getCheckedBy().get(0));
                        assertEquals(initiatorEmail, comment.getCheckedBy().get(1));
                });
        });
    }

    @Test
    @SuppressWarnings("unchecked")
    void deleteYourComment(){
        Comment commentRequest = Comment.builder().sender(sender).checkedBy(List.of(sender)).comment("Все плохо").build();
        
        Comment commentResponse = webTestClient.post()
        .uri("/api/v1/comment/add/{ideaId}", ideaId)
        .header("Authorization","Bearer " + jwt)
        .body(Mono.just(commentRequest), Comment.class)
        .exchange().expectBody(Comment.class)
        .returnResult().getResponseBody();

        assertNotNull(commentResponse);
        assertEquals(commentRequest.getSender(), commentResponse.getSender());
        assertEquals(commentRequest.getCheckedBy(), commentResponse.getCheckedBy());
        assertEquals(commentRequest.getComment(), commentResponse.getComment());

        Long commentId = commentResponse.getId();
        Map<String,String> response = webTestClient.delete()
                .uri("/api/v1/comment/delete/{ideaId}/{commentId}", ideaId, commentId)
                .header("Authorization","Bearer " + jwt)
                .exchange().expectBody(Map.class)
                .returnResult().getResponseBody();
        
        assertNotNull(response);
        assertNull(response.get("error"));
        assertNotNull(response.get("success"));
        assertEquals("Успешное удаление комментария", response.get("success"));
    }

    @Test
    void deleteNotYourComment(){
        Comment commentRequest = Comment.builder().sender(sender).checkedBy(List.of(sender)).comment("Все плохо").build();
        
        Comment commentResponse = webTestClient.post()
        .uri("/api/v1/comment/add/{ideaId}", ideaId)
        .header("Authorization","Bearer " + jwt)
        .body(Mono.just(commentRequest), Comment.class)
        .exchange().expectBody(Comment.class)
        .returnResult().getResponseBody();

        assertNotNull(commentResponse);
        assertEquals(commentRequest.getSender(), commentResponse.getSender());
        assertEquals(commentRequest.getCheckedBy(), commentResponse.getCheckedBy());
        assertEquals(commentRequest.getComment(), commentResponse.getComment());

        Long commentId = commentResponse.getId();
        ErrorResponse response = webTestClient.delete()
                .uri("/api/v1/comment/delete/{ideaId}/{commentId}", ideaId, commentId)
                .header("Authorization","Bearer " + initiatorJwt)
                .exchange().expectBody(ErrorResponse.class)
                .returnResult().getResponseBody();
        
        assertNotNull(response);
        assertNotNull(response.getError());
        assertEquals("Доступ запрещен", response.getError());
    }
}
