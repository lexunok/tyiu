package com.tyiu.corn.controller;

import static org.junit.Assert.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import java.util.Date;
import java.util.List;

import java.util.Map;
import java.util.UUID;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.reactive.AutoConfigureWebTestClient;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.context.SpringBootTest.WebEnvironment;
import org.springframework.test.web.reactive.server.WebTestClient;

import com.tyiu.corn.model.dto.InvitationDTO;
import com.tyiu.corn.model.entities.Invitation;
import com.tyiu.corn.model.enums.Role;
import com.tyiu.corn.model.requests.RegisterRequest;
import com.tyiu.corn.model.responses.AuthenticationResponse;
import com.tyiu.corn.model.responses.ErrorResponse;
import com.tyiu.corn.model.responses.InvitationResponse;
import com.tyiu.corn.repository.InvitationRepository;

import reactor.core.publisher.Mono;

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringBootTest(webEnvironment = WebEnvironment.RANDOM_PORT)
@AutoConfigureWebTestClient(timeout = "100000")//100 seconds
public class InvitationControllerTest {
    @Autowired
    private WebTestClient webTestClient;

    private String jwt;

    @Autowired
    private InvitationRepository invitationRepository;

    @BeforeAll
    public void setUp(){
        RegisterRequest request = new RegisterRequest(
                "fakemail","fakename","fakename","fakepass", List.of(Role.ADMIN));
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
    void sendRightListEmails(){
        List<String> emails = List.of(
            "wgweg@gfefemail.com",
            "awgweg@gfefemail.com",
            "bawgweg@gfefemail.com",
            "cbadwfggweg@gfefemail.com",
            "cbadwwdgweg@gfefemail.com",
            "cbadwefgweg@gfefemail.com",
            "cbadwf3gweg@gfefemail.com",
            "cbadwgwerfrg@gfefemail.com",
            "cbadwgweg@gfefemail.com"
        );
        InvitationDTO request = InvitationDTO.builder()
        .emails(emails)
        .roles(List.of(Role.ADMIN, Role.EXPERT))
        .build();
        
        webTestClient
            .post()
            .uri("/api/v1/invitation/emails")
            .header("Authorization","Bearer " + jwt)
            .body(Mono.just(request), InvitationDTO.class)
            .exchange().expectBody(Map.class);
    }

    @Test
    void sendRightOneEmail(){
        Invitation request = Invitation.builder()
        .email("timur.minyazeff@gmail.com")
        .roles(List.of(Role.ADMIN, Role.PROJECT_OFFICE))
        .build();

        webTestClient
            .post()
            .uri("/api/v1/invitation/email")
            .header("Authorization","Bearer " + jwt)
            .body(Mono.just(request), Invitation.class)
            .exchange()
            .expectBody(Map.class);
    }
    @Test
    void findInvitationByUrlIfExist(){
        String request = UUID.randomUUID().toString();
        Date date = new Date();
        long milsec = date.getTime() + 259200000;
        date.setTime(milsec);
        Invitation invitation = Invitation.builder()
                .email("Emailssd")
                .roles(List.of(Role.ADMIN))
                .url(request)
                .dateExpired(date)
                .build();
        invitationRepository.save(invitation);
        
        InvitationResponse response = webTestClient
        .get()
        .uri(String.format("/api/v1/invitation/get-invitation/%s", request))
        .exchange()
        .expectBody(InvitationResponse.class)
        .returnResult().getResponseBody();

        assertNotNull(response);
        assertEquals(invitation.getEmail(), response.getEmail());
        assertEquals(invitation.getRoles(), response.getRoles());
    }
    @Test
    void findInvitationByUrlIfNotExist(){
        String request = UUID.randomUUID().toString();
        
        ErrorResponse response = webTestClient
        .get()
        .uri("/api/v1/invitation/get-invitation/{request}", request)
        .exchange()
        .expectBody(ErrorResponse.class)
        .returnResult().getResponseBody();

        assertNotNull(response);
        assertEquals("Приглашения " + request + " не существует", response.getError());
    }
    @Test
    void catchEmailSenderException(){
        Invitation request = Invitation.builder()
        .email("ideasmanager")
        .roles(List.of(Role.ADMIN, Role.PROJECT_OFFICE))
        .build();

        ErrorResponse response = webTestClient
            .post()
            .uri("/api/v1/invitation/email")
            .header("Authorization","Bearer " + jwt)
            .body(Mono.just(request), Invitation.class)
            .exchange().expectBody(ErrorResponse.class)
            .returnResult().getResponseBody();
        
        assertNotNull(response);
        assertEquals("Добавьте домен почты", response.getError());
    }
    @Test
    void catchEmailParseException(){
        Invitation request = Invitation.builder()
        .email("@gmail.com")
        .roles(List.of(Role.ADMIN, Role.PROJECT_OFFICE))
        .build();

        ErrorResponse response = webTestClient
            .post()
            .uri("/api/v1/invitation/email")
            .header("Authorization","Bearer " + jwt)
            .body(Mono.just(request), Invitation.class)
            .exchange().expectBody(ErrorResponse.class)
            .returnResult().getResponseBody();
        
        assertNotNull(response);
        assertEquals("Добавьте имя пользователя почты", response.getError());
    }
    @Test
    void catchNotFoundExceptionWithoutEmail(){
        Invitation request = Invitation.builder()
        .roles(List.of(Role.ADMIN, Role.PROJECT_OFFICE))
        .build();

        ErrorResponse response = webTestClient
            .post()
            .uri("/api/v1/invitation/email")
            .header("Authorization","Bearer " + jwt)
            .body(Mono.just(request), Invitation.class)
            .exchange().expectBody(ErrorResponse.class)
            .returnResult().getResponseBody();
        
        assertNotNull(response);
        assertEquals("Добавьте почту", response.getError());
    }
    @Test
    void catchNotFoundExceptionWithEmptyBody(){
        Invitation request = Invitation.builder()
        .build();

        ErrorResponse response = webTestClient
            .post()
            .uri("/api/v1/invitation/email")
            .header("Authorization","Bearer " + jwt)
            .body(Mono.just(request), Invitation.class)
            .exchange().expectBody(ErrorResponse.class)
            .returnResult().getResponseBody();
        
        assertNotNull(response);
        assertEquals("Добавьте роль", response.getError());
    }
}
