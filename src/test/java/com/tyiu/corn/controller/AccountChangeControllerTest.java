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
import com.tyiu.corn.model.entities.Temporary;
import com.tyiu.corn.model.enums.Role;
import com.tyiu.corn.model.requests.RegisterRequest;
import com.tyiu.corn.model.responses.AuthenticationResponse;
import com.tyiu.corn.model.responses.ErrorResponse;
import com.tyiu.corn.model.responses.InvitationResponse;
import com.tyiu.corn.repository.AccountChangeRepository;

import reactor.core.publisher.Mono;

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringBootTest(webEnvironment = WebEnvironment.RANDOM_PORT)
@AutoConfigureWebTestClient(timeout = "100000")//100 seconds
public class AccountChangeControllerTest {
    @Autowired
    private WebTestClient webTestClient;

    private String jwt;

    @Autowired
    private AccountChangeRepository accountChangeRepository;

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
            .uri("/api/v1/profile-action/send/emails")
            .header("Authorization","Bearer " + jwt)
            .body(Mono.just(request), InvitationDTO.class)
            .exchange().expectBody(Map.class);
    }

    @Test
    void sendRightOneEmail(){
        Temporary request = Temporary.builder()
        .email("timur.minyazeff@gmail.com")
        .roles(List.of(Role.ADMIN, Role.PROJECT_OFFICE))
        .build();

        webTestClient
            .post()
            .uri("/api/v1/profile-action/send/email")
            .header("Authorization","Bearer " + jwt)
            .body(Mono.just(request), Temporary.class)
            .exchange()
            .expectBody(Map.class);
    }
    @Test
    void findInvitationByUrlIfExist(){
        String request = UUID.randomUUID().toString();
        Date date = new Date();
        long milsec = date.getTime() + 259200000;
        date.setTime(milsec);
        Temporary invitation = Temporary.builder()
                .email("Emailssd")
                .roles(List.of(Role.ADMIN))
                .url(request)
                .dateExpired(date)
                .build();
        accountChangeRepository.save(invitation);
        
        InvitationResponse response = webTestClient
        .get()
        .uri("/api/v1/profile-action/get/invitation/{request}", request)
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
        .uri("/api/v1/profile-action/get/invitation/{request}", request)
        .exchange()
        .expectBody(ErrorResponse.class)
        .returnResult().getResponseBody();

        assertNotNull(response);
        assertEquals("Приглашения " + request + " не существует", response.getError());
    }
    @Test
    void catchEmailSenderException(){
        Temporary request = Temporary.builder()
        .email("ideasmanager")
        .roles(List.of(Role.ADMIN, Role.PROJECT_OFFICE))
        .build();

        ErrorResponse response = webTestClient
            .post()
            .uri("/api/v1/profile-action/send/email")
            .header("Authorization","Bearer " + jwt)
            .body(Mono.just(request), Temporary.class)
            .exchange().expectBody(ErrorResponse.class)
            .returnResult().getResponseBody();
        
        assertNotNull(response);
        assertEquals("Добавьте домен почты", response.getError());
    }
    @Test
    void catchEmailParseException(){
        Temporary request = Temporary.builder()
        .email("@gmail.com")
        .roles(List.of(Role.ADMIN, Role.PROJECT_OFFICE))
        .build();

        ErrorResponse response = webTestClient
            .post()
            .uri("/api/v1/profile-action/send/email")
            .header("Authorization","Bearer " + jwt)
            .body(Mono.just(request), Temporary.class)
            .exchange().expectBody(ErrorResponse.class)
            .returnResult().getResponseBody();
        
        assertNotNull(response);
        assertEquals("Добавьте имя пользователя почты", response.getError());
    }
    @Test
    void catchNotFoundExceptionWithoutEmail(){
        Temporary request = Temporary.builder()
        .roles(List.of(Role.ADMIN, Role.PROJECT_OFFICE))
        .build();

        ErrorResponse response = webTestClient
            .post()
            .uri("/api/v1/profile-action/send/email")
            .header("Authorization","Bearer " + jwt)
            .body(Mono.just(request), Temporary.class)
            .exchange().expectBody(ErrorResponse.class)
            .returnResult().getResponseBody();
        
        assertNotNull(response);
        assertEquals("Добавьте почту", response.getError());
    }
    @Test
    void catchNotFoundExceptionWithEmptyBody(){
        Temporary request = Temporary.builder()
        .build();

        ErrorResponse response = webTestClient
            .post()
            .uri("/api/v1/profile-action/send/email")
            .header("Authorization","Bearer " + jwt)
            .body(Mono.just(request), Temporary.class)
            .exchange().expectBody(ErrorResponse.class)
            .returnResult().getResponseBody();
        
        assertNotNull(response);
        assertEquals("Добавьте почту", response.getError());
    }
}
