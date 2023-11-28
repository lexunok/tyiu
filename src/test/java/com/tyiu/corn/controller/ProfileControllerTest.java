package com.tyiu.corn.controller;

import com.tyiu.corn.model.dto.*;
import com.tyiu.corn.model.entities.Idea;
import com.tyiu.corn.model.enums.Role;
import com.tyiu.corn.model.enums.SkillType;
import com.tyiu.corn.model.requests.RegisterRequest;
import com.tyiu.corn.model.responses.AuthenticationResponse;
import com.tyiu.corn.model.responses.ProfileIdeaResponse;
import org.junit.jupiter.api.BeforeAll;
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
public class ProfileControllerTest extends TestContainers {

    @Autowired
    private WebTestClient webTestClient;
    private UserDTO userDTO;
    private String jwt;
    private GroupDTO expertGroup;
    private GroupDTO projectGroup;
    private SkillDTO skill;
    private ProfileDTO createProfile() {

        IdeaDTO idea = IdeaDTO.builder()
                .initiatorEmail(userDTO.getEmail())
                .name("Сдал ИГЭ по барьба")
                .status(Idea.Status.ON_APPROVAL)
                .experts(expertGroup)
                .projectOffice(projectGroup)
                .problem("Гопники на улице")
                .solution("Барьба")
                .result("Знания по барьба")
                .customer("Чо?")
                .contactPerson("Абдул")
                .description("Барьба должны знать все")
                .build();

        IdeaDTO responseCreateIdea = webTestClient
                .post()
                .uri("/api/v1/idea/add")
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(idea), IdeaDTO.class)
                .exchange()
                .expectBody(IdeaDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(responseCreateIdea);

        ProfileIdeaResponse ideas = ProfileIdeaResponse.builder()
                .name(responseCreateIdea.getName())
                .description(responseCreateIdea.getDescription())
                .build();

        SkillDTO skills = SkillDTO.builder()
                .name(skill.getName())
                .type(skill.getType())
                .build();

        return ProfileDTO.builder()
                .id(userDTO.getId())
                .email(userDTO.getEmail())
                .firstName("1")
                .lastName("2")
                .roles(userDTO.getRoles())
                .skills(List.of(skills))
                .ideas(List.of(ideas))
                .build();
    }

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

        SkillDTO skillDTO = SkillDTO.builder()
                .name("Java")
                .type(SkillType.LANGUAGE)
                .build();

        skill = webTestClient
                .post()
                .uri("/api/v1/skill/add")
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(skillDTO), SkillDTO.class)
                .exchange()
                .expectBody(SkillDTO.class)
                .returnResult().getResponseBody();
    }

    @Test
    void testGetProfile() {

        ProfileDTO profile = createProfile();
        String email = profile.getEmail();

        ProfileDTO responseGetProfile = webTestClient
                .get()
                .uri("/api/v1/profile/{email}", email)
                .header("Authorization", "Bearer " + jwt)
                .exchange()
                .expectBody(ProfileDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(responseGetProfile);
        assertEquals(profile.getEmail(), responseGetProfile.getEmail());
    }

    @Test
    void testUploadAvatar() {

        ProfileDTO profile = createProfile();

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

        ProfileDTO profile = createProfile();
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
        assertNotNull(responseGetAvatar);
        assertEquals(profile.getEmail(), responseGetAvatar.getEmail());
    }

    @Test
    void testSaveSkills() {

        ProfileDTO responseSaveSkills = webTestClient
                .post()
                .uri("/api/v1/profile/skills/save")
                .header("Authorization", "Bearer " + jwt)
                .exchange()
                .expectBody(ProfileDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(responseSaveSkills);
    }
}
