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
        Idea idea1 = Idea.builder().name("title").build();
        Idea response = webTestClient
                .post()
                .uri("/api/v1/idea/initiator/add")
                .header("Authorization","Bearer " + jwt)
                .body(Mono.just(idea1), Idea.class)
                .exchange()
                .expectBody(Idea.class)
                .returnResult().getResponseBody();
        List<Idea> response3 = webTestClient
                .get()
                .uri("/api/v1/idea/initiator")
                .header("Authorization","Bearer " + jwt)
                .exchange()
                .expectBodyList(Idea.class)
                .returnResult().getResponseBody();
        assertNotNull(response3);
        List<Idea> actualIdeas = response3.stream().filter(idea -> idea1.getName().equals(response.getName())).toList();
        assertTrue(actualIdeas.size() >= 1);
    }


    @Test
    void testShowIdeaForProjectOffice(){
        StatusIdea status = StatusIdea.ON_CONFIRMATION;
        Idea idea2 = Idea.builder().name("title").build();
        Idea response1 = webTestClient
                .post()
                .uri("/api/v1/idea/initiator/add")
                .header("Authorization","Bearer " + jwt)
                .body(Mono.just(idea2), Idea.class)
                .exchange()
                .expectBody(Idea.class)
                .returnResult().getResponseBody();
        Long id = response1.getId();
        webTestClient
                .put()
                .uri("/api/v1/idea/initiator/send/{id}", id)
                .header("Authorization","Bearer " + jwt)
                .exchange()
                .expectStatus().isOk();
        List<Idea> response3 = ((RequestBodySpec) webTestClient
                .get()
                .uri("/api/v1/idea/project-office")
                .header("Authorization","Bearer " + jwt))
                .body(Mono.just(status), StatusIdea.class)
                .exchange()
                .expectBodyList(Idea.class)
                .returnResult().getResponseBody();
        assertNotNull(response3);
    }
    @Test
    void testShowListIdeaForExpert(){
        StatusIdea status = StatusIdea.ON_APPROVAL;
        Idea idea2 = Idea.builder().name("title").build();
        Idea response1 = webTestClient
                .post()
                .uri("/api/v1/idea/initiator/add")
                .header("Authorization","Bearer " + jwt)
                .body(Mono.just(idea2), Idea.class)
                .exchange()
                .expectBody(Idea.class)
                .returnResult().getResponseBody();
        Long id = response1.getId();
        webTestClient
                .put()
                .uri("/api/v1/idea/initiator/send/{id}", id)
                .header("Authorization","Bearer " + jwt)
                .exchange()
                .expectStatus().isOk();
        webTestClient
                .put()
                .uri("/api/v1/idea/project-office/update/{id}", id)
                .header("Authorization","Bearer " + jwt)
                .body(Mono.just(StatusIdea.ON_CONFIRMATION), StatusIdea.class)
                .exchange()
                .expectBody(StatusIdea.class)
                .returnResult().getResponseBody();
        List<Idea> response3 = ((RequestBodySpec) webTestClient
                .get()
                .uri("/api/v1/idea/expert")
                .header("Authorization","Bearer " + jwt))
                .body(Mono.just(status), StatusIdea.class)
                .exchange()
                .expectBodyList(Idea.class)
                .returnResult().getResponseBody();
        assertNotNull(response3);
    }
    @Test
    void testShowListIdeaForAdmin(){
        Idea idea4 = Idea.builder().name("title").build();
        Idea response1 = webTestClient
                .post()
                .uri("/api/v1/idea/initiator/add")
                .header("Authorization","Bearer " + jwt)
                .body(Mono.just(idea4), Idea.class)
                .exchange()
                .expectBody(Idea.class)
                .returnResult().getResponseBody();
        List<Idea> response2 = webTestClient
                .get()
                .uri("/api/v1/idea/admin")
                .header("Authorization","Bearer " + jwt)
                .exchange()
                .expectBodyList(Idea.class)
                .returnResult().getResponseBody();
        assertNotNull(response2);
        List<Idea> actualIdeas = response2.stream().filter(idea -> idea4.getName().equals(response1.getName())).toList();
        assertTrue(actualIdeas.size() >= 3);
    }

    @Test
    void testAddIdea(){
        Idea idea5 = Idea.builder().name("title").build();
        Idea response = webTestClient
                .post()
                .uri("/api/v1/idea/initiator/add")
                .header("Authorization","Bearer " + jwt)
                .body(Mono.just(idea5), Idea.class)
                .exchange()
                .expectBody(Idea.class)
                .returnResult().getResponseBody();
        assertNotNull(response);
        assertEquals(idea5.getName(),response.getName());
    }

    @Test
    void testDeleteIdeaByInitiator(){
        Idea idea6 = Idea.builder().name("title").build();
        Idea response = webTestClient
                .post()
                .uri("/api/v1/idea/initiator/add")
                .header("Authorization","Bearer " + jwt)
                .body(Mono.just(idea6), Idea.class)
                .exchange()
                .expectBody(Idea.class)
                .returnResult().getResponseBody();
        Long id = response.getId();
        webTestClient
                .delete()
                .uri("/api/v1/idea/initiator/delete/{id}", id)
                .header("Authorization","Bearer " + jwt)
                .exchange()
                .expectStatus().isOk();
    }

    @Test
    void testDeleteIdeaByAdmin(){
        Idea idea7 = Idea.builder().name("title").build();
        Idea response = webTestClient
                .post()
                .uri("/api/v1/idea/initiator/add")
                .header("Authorization","Bearer " + jwt)
                .body(Mono.just(idea7), Idea.class)
                .exchange()
                .expectBody(Idea.class)
                .returnResult().getResponseBody();
        Long id = response.getId();
        webTestClient
                .delete()
                .uri("/api/v1/idea/admin/delete/{id}", id)
                .header("Authorization","Bearer " + jwt)
                .exchange()
                .expectStatus().isOk();
    }

    @Test
    void testUpdateStatusIdeaByProjectOffice(){
        Idea idea8 = Idea.builder().name("title1").build();
        Idea response0 = webTestClient
                .post()
                .uri("/api/v1/idea/initiator/add")
                .header("Authorization","Bearer " + jwt)
                .body(Mono.just(idea8), Idea.class)
                .exchange()
                .expectBody(Idea.class)
                .returnResult().getResponseBody();
        Long id = response0.getId();
        webTestClient
                .put()
                .uri("/api/v1/idea/initiator/send/{id}", id)
                .header("Authorization","Bearer " + jwt)
                .exchange()
                .expectStatus().isOk();
        StatusIdea response = webTestClient
                .put()
                .uri("/api/v1/idea/project-office/update/{id}", id)
                .header("Authorization","Bearer " + jwt)
                .body(Mono.just(StatusIdea.ON_CONFIRMATION), StatusIdea.class)
                .exchange()
                .expectBody(StatusIdea.class)
                .returnResult().getResponseBody();
        assertNull(response);
    }

    @Test
    void testUpdateStatusByExpert(){
        Idea idea9 = Idea.builder().name("title1").build();
        Idea response0 = webTestClient
                .post()
                .uri("/api/v1/idea/initiator/add")
                .header("Authorization","Bearer " + jwt)
                .body(Mono.just(idea9), Idea.class)
                .exchange()
                .expectBody(Idea.class)
                .returnResult().getResponseBody();
        Long id = response0.getId();
        RiskDTO riskDTO = RiskDTO.builder().status(StatusIdea.CONFIRMED).risk(4.4).price("price")
                                    .originality("orig").technicalFeasibility("tech").understanding("under").build();
        RiskDTO response = webTestClient
                .put()
                .uri("/api/v1/idea/expert/update/{id}", id)
                .header("Authorization","Bearer " + jwt)
                .body(Mono.just(riskDTO), RiskDTO.class)
                .exchange()
                .expectBody(RiskDTO.class)
                .returnResult().getResponseBody();
        assertNull(response);
    }

    @Test
    void testUpdateIdeaByAdmin(){
        Idea idea = Idea.builder().name("title").build();
        Idea response0 = webTestClient
                .post()
                .uri("/api/v1/idea/initiator/add")
                .header("Authorization","Bearer " + jwt)
                .body(Mono.just(idea), Idea.class)
                .exchange()
                .expectBody(Idea.class)
                .returnResult().getResponseBody();
        Long id = response0.getId();
        idea = Idea.builder().name("title2").build();
        Idea response = webTestClient
                .put()
                .uri("/api/v1/idea//admin/update/{id}", id)
                .header("Authorization","Bearer " + jwt)
                .body(Mono.just(idea), Idea.class)
                .exchange()
                .expectBody(Idea.class)
                .returnResult().getResponseBody();
        assertNull(response);
    }
}
