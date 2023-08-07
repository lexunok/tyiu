package com.tyiu.corn.controller;

import java.util.List;

import java.util.Map;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.context.SpringBootTest.WebEnvironment;
import org.springframework.test.web.reactive.server.WebTestClient;

import com.tyiu.corn.model.dto.InvitationDTO;
import com.tyiu.corn.model.entities.Invitation;
import com.tyiu.corn.model.enums.Role;

import reactor.core.publisher.Mono;

@SpringBootTest(webEnvironment = WebEnvironment.RANDOM_PORT)
public class InvitationControllerTest {
    @Autowired
    private WebTestClient webTestClient;

    @Test
    void sendRightListEmails(){
        List<String> emails = List.of(
            "wgweg@gfefemail.com",
            "awgweg@gfefemail.com",
            "bawgweg@gfefemail.com",
            "cbawgweg@gfefemail.com",
            "acbawgweg@gfefemail.com",
            "bacbawgweg@gfefemail.com",
            "cbacbawgweg@gfefemail.com",
            "acbacbawgweg@gfefemail.com",
            "bacbacbawgweg@gfefemail.com",
            "cbacbacbawgweg@gfefemail.com",
            "acbacbacbawgweg@gfefemail.com"
        );
        InvitationDTO request = InvitationDTO.builder()
                                                    .emails(emails)
                                                    .roles(List.of(Role.ADMIN, Role.EXPERT))
                                                    .build();
        webTestClient
            .post()
            .uri("/api/v1/invitation/emails")
            .body(Mono.just(request), InvitationDTO.class)
            .exchange()
            .expectBody(Map.class)
            .isEqualTo(Map.of("success", "Успешное приглашение"));
    }

    @Test
    void sendRightOneEmail(){
        Invitation request = Invitation.builder()
        .email("timur.minyazeff@gmail.com")
        .roles(List.of(Role.ADMIN, Role.PROJECT_OFFICE))
        .build();

        webTestClient
            .post()
            .uri("/api/v1/invitation/emails")
            .body(Mono.just(request), InvitationDTO.class)
            .exchange()
            .expectBody(Map.class)
            .isEqualTo(Map.of("success", "Успешное приглашение"));
    }
}
