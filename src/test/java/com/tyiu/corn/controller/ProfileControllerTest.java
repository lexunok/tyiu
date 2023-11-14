package com.tyiu.corn.controller;

import com.tyiu.corn.model.dto.ProfileDTO;
import com.tyiu.corn.model.dto.SkillDTO;
import com.tyiu.corn.model.dto.UserDTO;
import com.tyiu.corn.model.enums.Role;
import com.tyiu.corn.model.enums.SkillType;
import com.tyiu.corn.model.requests.RegisterRequest;
import com.tyiu.corn.model.responses.AuthenticationResponse;
import com.tyiu.corn.model.responses.ProfileSkillResponse;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.web.reactive.server.WebTestClient;
import reactor.core.publisher.Mono;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.springframework.boot.test.context.SpringBootTest.WebEnvironment.RANDOM_PORT;

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringBootTest(webEnvironment = RANDOM_PORT)
public class ProfileControllerTest extends TestContainers {

    @Autowired
    private WebTestClient webTestClient;
    private UserDTO userDTO;
    private String jwt;

    @BeforeAll
    public void setUp() {
        RegisterRequest request = new RegisterRequest(
                "fakemailPROFILE", "2", "1", "fakepass",
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
    }

    @Test
    void testGetProfile() {
        ProfileDTO profile = ProfileDTO.builder()
                .id(userDTO.getId())
                .email(userDTO.getEmail())
                .firstName("1")
                .lastName("2")
                .build();
        String email = profile.getEmail();

        ProfileDTO responseGetProfile = webTestClient
                .get()
                .uri("/api/v1/profile/{email}", email)
                .header("Authorization", "Bearer " + jwt)
                .exchange()
                .expectBody(ProfileDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(responseGetProfile.getId());
        assertEquals(profile.getEmail(), responseGetProfile.getEmail());
    }

    @Test
    void testUploadAvatar() {
        ProfileDTO profile = ProfileDTO.builder()
                .id(userDTO.getId())
                .email(userDTO.getEmail())
                .firstName("1")
                .lastName("2")
                .build();

        ProfileDTO responseUploadAvatar = webTestClient
                .post()
                .uri("/api/v1/profile/avatar/upload")
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(profile), ProfileDTO.class)
                .exchange()
                .expectBody(ProfileDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(responseUploadAvatar);
    }

    @Test
    void testGetAvatar() {
        ProfileDTO profile = ProfileDTO.builder()
                .id(userDTO.getId())
                .email(userDTO.getEmail())
                .firstName("1")
                .lastName("2")
                .build();
        String email = profile.getEmail();

        webTestClient
                .post()
                .uri("/api/v1/profile/avatar/upload")
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(profile), ProfileDTO.class)
                .exchange()
                .expectBody(ProfileDTO.class)
                .returnResult();

        ProfileDTO responseGetAvatar = webTestClient
                .get()
                .uri("/api/v1/profile/{email}", email)
                .header("Authorization", "Bearer " + jwt)
                .exchange()
                .expectBody(ProfileDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(responseGetAvatar.getId());
        assertEquals(profile.getEmail(), responseGetAvatar.getEmail());
    }

//    @Test
//    void testSaveSkills() {
//        SkillDTO skill = SkillDTO.builder()
//                .name("Java")
//                .type(SkillType.LANGUAGE)
//                .build();
//
//        webTestClient
//                .post()
//                .uri("/api/v1/skill/add")
//                .header("Authorization", "Bearer " + jwt)
//                .body(Mono.just(skill), SkillDTO.class)
//                .exchange()
//                .expectBody(SkillDTO.class);
//
//        ProfileSkillResponse skills = ProfileSkillResponse.builder()
//                .id(skill.getId())
//                .name(skill.getName())
//                .type(skill.getType())
//                .build();
//
//        ProfileDTO profile = ProfileDTO.builder()
//                .id(userDTO.getId())
//                .email(userDTO.getEmail())
//                .firstName("1")
//                .lastName("2")
//                .skills(List.of(skills))
//                .build();
//
//        ProfileDTO responseSaveSkill = webTestClient
//                .post()
//                .uri("/api/v1/profile/skills/save")
//                .header("Authorization", "Bearer " + jwt)
//                .body(Mono.just(profile), ProfileDTO.class)
//                .exchange()
//                .expectBody(ProfileDTO.class)
//                .returnResult().getResponseBody();
//        assertNotNull(responseSaveSkill);
//    }
}
