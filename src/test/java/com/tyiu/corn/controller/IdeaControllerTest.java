package com.tyiu.corn.controller;

import com.tyiu.corn.model.dto.RiskDTO;
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
import org.springframework.test.web.reactive.server.WebTestClient.RequestBodySpec;
import org.springframework.test.web.reactive.server.WebTestClient.ResponseSpec;

import static org.junit.jupiter.api.Assertions.*;
import reactor.core.publisher.Mono;

import java.util.List;

import static org.springframework.boot.test.context.SpringBootTest.WebEnvironment.RANDOM_PORT;
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringBootTest(webEnvironment = RANDOM_PORT)
public class IdeaControllerTest {
    @Autowired
    private WebTestClient webTestClient;
    private String jwt;
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
    void testShowListIdeaForInitiator(){
        String principal = "fakeemail";
        List<Idea> response = ((RequestBodySpec) webTestClient
                .get()
                .uri("/api/v1/idea/initiator")
                .header("Authorization","Bearer " + jwt))
                .body(Mono.just(principal), String.class)
                .exchange()
                .expectBodyList(Idea.class)
                .returnResult().getResponseBody();
        // assertNotNull(response);
        // assertEquals(1,response.size());
    }


    @Test
    void testShowIdeaForProjectOffice(){
        StatusIdea status = StatusIdea.ON_CONFIRMATION;
        ((RequestBodySpec) webTestClient
                .get()
                .uri("/api/v1/idea/project-office")
                .header("Authorization","Bearer " + jwt))
                .body(Mono.just(status), StatusIdea.class)
                .exchange()
                .expectBodyList(Idea.class)
                .returnResult().getResponseBody();
    }
    @Test
    void testShowListIdeaForExpert(){
        StatusIdea status = StatusIdea.ON_APPROVAL;
        ((RequestBodySpec) webTestClient
                .get()
                .uri("/api/v1/idea/expert")
                .header("Authorization","Bearer " + jwt))
                .body(Mono.just(status), StatusIdea.class)
                .exchange()
                .expectBodyList(Idea.class)
                .returnResult().getResponseBody();
    }
    @Test
    void testShowListIdeaForAdmin(){
        webTestClient
                .get()
                .uri("/api/v1/idea/admin")
                .header("Authorization","Bearer " + jwt)
                .exchange()
                .expectBodyList(Idea.class)
                .returnResult().getResponseBody();
    }

    @Test
    void testAddIdea(){
        Idea idea = Idea.builder().name("title").build();
        Idea response = webTestClient
                .post()
                .uri("/api/v1/idea/initiator/add")
                .header("Authorization","Bearer " + jwt)
                .body(Mono.just(idea), Idea.class)
                .exchange()
                .expectBody(Idea.class)
                .returnResult().getResponseBody();
        assertNotNull(response);
        assertEquals(idea.getName(),response.getName());
    }

    // @Test
    // void testDeleteIdeaByInitiator(){
    //     Idea idea = Idea.builder().name("title").build();
    //     webTestClient
    //             .post()
    //             .uri("/api/v1/idea/initiator/add")
    //             .header("Authorization","Bearer " + jwt)
    //             .body(Mono.just(idea), Idea.class)
    //             .exchange()
    //             .expectStatus().isOk();
    //     Long id = 1L;
    //     webTestClient
    //             .delete()
    //             .uri("/api/v1/idea/initiator/delete/{id}", id)
    //             .header("Authorization","Bearer " + jwt)
    //             .exchange()
    //             .expectStatus().isOk();
    // }

    @Test
    void testDeleteIdeaByAdmin(){
        Idea idea = Idea.builder().name("title").build();
        webTestClient
                .post()
                .uri("/api/v1/idea/initiator/add")
                .header("Authorization","Bearer " + jwt)
                .body(Mono.just(idea), Idea.class)
                .exchange()
                .expectStatus().isOk();
        Long id = 1L;
        webTestClient
                .delete()
                .uri("/api/v1/idea/admin/delete/{id}", id)
                .header("Authorization","Bearer " + jwt)
                .exchange()
                .expectStatus().isOk();
    }

    @Test
    void testUpdateStatusIdeaByProjectOffice(){
        Long id = 1L;
        Idea idea = Idea.builder().name("title1").build();
        webTestClient
                .post()
                .uri("/api/v1/idea/initiator/add")
                .header("Authorization","Bearer " + jwt)
                .body(Mono.just(idea), Idea.class)
                .exchange()
                .expectBody(Idea.class)
                .returnResult().getResponseBody();
        StatusIdea status = StatusIdea.CONFIRMED;
        ResponseSpec response = webTestClient
                .put()
                .uri("/api/v1/idea/project-office/update/{Id}", id)
                .header("Authorization","Bearer " + jwt)
                .body(Mono.just(status), StatusIdea.class)
                .exchange()
                .expectStatus().isOk();
        // assertNotNull(response);
        // assertEquals(status.getClass(),response.getClass());
    }

    @Test
    void testUpdateStatusByExpert(){
        Long id = 1L;
        Idea idea = Idea.builder().name("title1").build();
        webTestClient
                .post()
                .uri("/api/v1/idea/initiator/add")
                .header("Authorization","Bearer " + jwt)
                .body(Mono.just(idea), Idea.class)
                .exchange()
                .expectBody(Idea.class)
                .returnResult().getResponseBody();
        RiskDTO riskDTO = RiskDTO.builder().status(StatusIdea.ON_CONFIRMATION).risk(4.4).price("price")
                                    .originality("orig").technicalFeasibility("tech").understanding("under").build();
        webTestClient
                .put()
                .uri("/api/v1/idea/expert/update/{Id}", id)
                .header("Authorization","Bearer " + jwt)
                .body(Mono.just(riskDTO), RiskDTO.class)
                .exchange()
                .expectStatus().isOk();
    }

    @Test
    void testUpdateIdeaByAdmin(){
        Long id = 1L;
        Idea idea = Idea.builder().name("title1").build();
        webTestClient
                .post()
                .uri("/api/v1/idea/initiator/add")
                .header("Authorization","Bearer " + jwt)
                .body(Mono.just(idea), Idea.class)
                .exchange()
                .expectBody(Idea.class)
                .returnResult().getResponseBody();
        idea = Idea.builder().name("title2").build();
        webTestClient
                .put()
                .uri("/api/v1/idea//admin/update/{Id}", id)
                .header("Authorization","Bearer " + jwt)
                .body(Mono.just(idea), Idea.class)
                .exchange()
                .expectStatus().isOk();
        // assertNotNull(response);
        // assertEquals(idea.getName(),response.getName());
    }
}
