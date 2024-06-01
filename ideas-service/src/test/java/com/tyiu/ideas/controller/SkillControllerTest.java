package com.tyiu.ideas.controller;

import com.tyiu.ideas.model.dto.SkillDTO;
import com.tyiu.ideas.model.enums.SkillType;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.web.reactive.server.WebTestClient;
import reactor.core.publisher.Mono;

import java.util.List;

import static org.junit.jupiter.api.Assertions.*;
import static org.springframework.boot.test.context.SpringBootTest.WebEnvironment.RANDOM_PORT;

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringBootTest(webEnvironment = RANDOM_PORT)
public class SkillControllerTest extends TestContainers {
    @Autowired
    private WebTestClient webTestClient;

    private String jwt;

    private String skillId;

    @BeforeAll
    void setUp(){
        RegisterRequest request = new RegisterRequest(
                "email@email.com", "lastName", "firstName", "password",
                List.of(Role.ADMIN,
                        Role.EXPERT,
                        Role.PROJECT_OFFICE,
                        Role.INITIATOR));
        AuthenticationResponse response = webTestClient
                .post()
                .uri("/api/v1/ideas-service/auth/register")
                .body(Mono.just(request), RegisterRequest.class)
                .exchange()
                .expectBody(AuthenticationResponse.class)
                .returnResult().getResponseBody();
        assertNotNull(response);
        jwt = response.getToken();
    }
    @Order(1)
    @Test
    void testAddNoConfirmedSkill(){
        SkillDTO skillDTO = SkillDTO.builder()
                .name("Python")
                .type(SkillType.LANGUAGE)
                .build();

        SkillDTO addSkillResponse = webTestClient
                .post()
                .uri("/api/v1/ideas-service/skill/add/no-confirmed")
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(skillDTO), SkillDTO.class)
                .exchange()
                .expectBody(SkillDTO.class)
                .returnResult().getResponseBody();

        assertNotNull(addSkillResponse);
    }
    @Order(2)
    @Test
    void testAddSkill(){
        SkillDTO skillDTO = SkillDTO.builder()
                .name("Java")
                .type(SkillType.LANGUAGE)
                .build();

        SkillDTO addSkillResponse = webTestClient
                .post()
                .uri("/api/v1/ideas-service/skill/add")
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(skillDTO), SkillDTO.class)
                .exchange()
                .expectBody(SkillDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(addSkillResponse);
    }

    @Order(3)
    @Test
    void testGetAllConfirmedSkills(){
        List<SkillDTO> skills = webTestClient
                .get()
                .uri("api/v1/ideas-service/skill/all-confirmed-or-creator")
                .header("Authorization", "Bearer " + jwt)
                .exchange()
                .expectBodyList(SkillDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(skills);
        assertEquals(skills.size(), 1);
    }

    @Order(4)
    @Test
    void testGetAllSkills() {
        List<SkillDTO> skills = webTestClient
                .get()
                .uri("api/v1/ideas-service/skill/all")
                .header("Authorization", "Bearer " + jwt)
                .exchange()
                .expectBodyList(SkillDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(skills);

    }

    @Order(5)
    @Test
    void testConfirmSkill() {
        SkillDTO skillDTO = SkillDTO.builder()
                .name("Python")
                .type(SkillType.LANGUAGE)
                .build();

        SkillDTO addSkillResponse = webTestClient
                .post()
                .uri("/api/v1/ideas-service/skill/add/no-confirmed")
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(skillDTO), SkillDTO.class)
                .exchange()
                .expectBody(SkillDTO.class)
                .returnResult().getResponseBody();

        assertNotNull(addSkillResponse);
        skillId = addSkillResponse.getId();

        webTestClient
                .put()
                .uri("api/v1/ideas-service/skill/confirm/{skillId}", skillId)
                .header("Authorization","Bearer " + jwt)
                .exchange()
                .expectStatus().isOk();
    }

    @Order(6)
    @Test
    void testUpdateSkill() {
        SkillDTO skillUpdateDTO = SkillDTO.builder()
                .name("MongoDB")
                .type(SkillType.DATABASE)
                .build();
        SkillDTO skillResponse = webTestClient
                .put()
                .uri("/api/v1/ideas-service/skill/update/{skillId}", skillId)
                .header("Authorization","Bearer " + jwt)
                .body(Mono.just(skillUpdateDTO), SkillDTO.class)
                .exchange()
                .expectBody(SkillDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(skillResponse);
        assertNotEquals(skillResponse.getName(), "Python");
    }

    @Order(7)
    @Test
    void testGetSkillsByType(){
        SkillType skillType = SkillType.LANGUAGE;
        List<SkillDTO> skills = webTestClient
                .get()
                .uri("api/v1/ideas-service/skills/{skillType}", skillType)
                .header("Authorization", "Bearer " + jwt)
                .exchange()
                .expectBodyList(SkillDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(skills);
        assertEquals(skills.size(), 1);
    }

    @Order(8)
    @Test
    void testDeleteSkills(){
        webTestClient
                .delete()
                .uri("/api/v1/ideas-service/group/delete/{skillId}", skillId)
                .header("Authorization","Bearer " + jwt)
                .exchange()
                .expectStatus().isOk();
    }

}
