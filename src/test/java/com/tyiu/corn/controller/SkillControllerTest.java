package com.tyiu.corn.controller;

import com.tyiu.corn.model.dto.SkillDTO;
import com.tyiu.corn.model.enums.Role;
import com.tyiu.corn.model.enums.SkillType;
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

import static org.junit.jupiter.api.Assertions.*;
import static org.springframework.boot.test.context.SpringBootTest.WebEnvironment.RANDOM_PORT;

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringBootTest(webEnvironment = RANDOM_PORT)
public class SkillControllerTest extends TestContainers{
    @Autowired
    private WebTestClient webTestClient;

    private String jwt;

    private Long skillId;

    @BeforeAll
    void setUp(){
        RegisterRequest request = new RegisterRequest(
                "email@email.com", "lastName", "firstName", "password", List.of(Role.ADMIN));
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
    @Order(1)
    @Test
    void testAddNoConfirmedSkill(){
        SkillDTO skillDTO = SkillDTO.builder()
                .name("Python")
                .type(SkillType.LANGUAGE)
                .build();

        SkillDTO addSkillResponse = webTestClient
                .post()
                .uri("/api/v1/skill/add/no-confirmed")
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
                .uri("/api/v1/skill/add")
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
                .uri("api/v1/skill/all-confirmed-or-creator")
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
                .uri("api/v1/skill/all")
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
                .uri("/api/v1/skill/add/no-confirmed")
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(skillDTO), SkillDTO.class)
                .exchange()
                .expectBody(SkillDTO.class)
                .returnResult().getResponseBody();

        assertNotNull(addSkillResponse);
        skillId = addSkillResponse.getId();

        webTestClient
                .put()
                .uri("api/v1/skill/confirm/{skillId}", skillId)
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
                .uri("/api/v1/skill/update/{skillId}", skillId)
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
                .uri("api/v1/skills/{skillType}", skillType)
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
                .uri("/api/v1/group/delete/{skillId}", skillId)
                .header("Authorization","Bearer " + jwt)
                .exchange()
                .expectStatus().isOk();
    }

}
