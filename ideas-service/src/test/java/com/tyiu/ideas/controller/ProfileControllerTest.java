package com.tyiu.ideas.controller;

import com.tyiu.ideas.model.dto.*;
import com.tyiu.ideas.model.entities.Idea;
import com.tyiu.ideas.model.enums.Role;
import com.tyiu.ideas.model.enums.SkillType;
import com.tyiu.ideas.model.requests.RegisterRequest;
import com.tyiu.ideas.model.responses.AuthenticationResponse;
import com.tyiu.ideas.model.responses.ProfileIdeaResponse;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.data.r2dbc.core.R2dbcEntityTemplate;
import org.springframework.r2dbc.core.DatabaseClient;
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
    @Autowired
    private R2dbcEntityTemplate template;
    @Autowired
    private DatabaseClient databaseClient;
    private UserDTO userDTO;
    private String jwt;
    private String jwt_randomUser;
    private GroupDTO expertGroup;
    private GroupDTO projectGroup;
    private SkillDTO skill;
    private ProfileDTO createProfile() {

        IdeaDTO idea = IdeaDTO.builder()
                .initiator(userDTO)
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
                .uri("/api/v1/ideas-service/idea/add")
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(idea), IdeaDTO.class)
                .exchange()
                .expectBody(IdeaDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(responseCreateIdea);

        ProfileIdeaResponse ideas = ProfileIdeaResponse.builder()
                .name(responseCreateIdea.getName())
                .solution(responseCreateIdea.getSolution())
                .build();

        SkillDTO skills = SkillDTO.builder()
                .name(skill.getName())
                .type(skill.getType())
                .build();

        return ProfileDTO.builder()
                .id(userDTO.getId())
                .email(userDTO.getEmail())
                .userTag("lerlfee")
                .isUserTagVisible(false)
                .firstName("1")
                .lastName("2")
                .roles(userDTO.getRoles())
                .skills(List.of(skills))
                .ideas(List.of(ideas))
                .createdAt(userDTO.getCreatedAt())
                .build();
    }

    private void setUserTag(String userEmail, String userTag, Long chatId, Boolean isVisible) {

        template.getDatabaseClient()
                .sql("INSERT INTO users_telegram (user_email, user_tag, chat_id, is_visible) " +
                        "VALUES(:userEmail, :userTag, :chatId, :isVisible)")
                .bind("userEmail", userEmail)
                .bind("userTag", userTag)
                .bind("chatId", chatId)
                .bind("isVisible", isVisible)
                .fetch()
                .rowsUpdated()
                .block();
    }

    private void setUserTagVisible(Boolean isVisible, String userEmail) {
        template.getDatabaseClient()
                .sql("UPDATE users_telegram SET is_visible = :isVisible WHERE user_email = :userEmail")
                .bind("isVisible", isVisible)
                .bind("userEmail", userEmail)
                .fetch()
                .rowsUpdated()
                .block();
    }

    private AuthenticationResponse register(String email, String firstName, String lastName, String password){
        RegisterRequest request = new RegisterRequest(
                email, firstName, lastName, password,
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
        return response;
    }

    private UserDTO userBuild(String id, String email, String firstname, String lastname, List<Role> roles){
        return UserDTO.builder()
                .id(id)
                .email(email)
                .firstName(firstname)
                .lastName(lastname)
                .roles(roles)
                .build();
    }

    @BeforeAll
    public void setUp() {

        AuthenticationResponse response1 = register("profileOwner@gmail.com", "ddd", "E", "profile-test");
        jwt = response1.getToken();

        AuthenticationResponse response2 = register("randomUser@gmail.com", "ddd", "E", "profile-test");
        jwt_randomUser = response2.getToken();

        userDTO = userBuild(response1.getId(), response1.getEmail(), response1.getFirstName(), response1.getLastName(), response1.getRoles());

        GroupDTO expertGroupDTO = GroupDTO.builder()
                .name("Эксперты")
                .users(List.of(userDTO))
                .roles(List.of(Role.EXPERT))
                .build();

        expertGroup = webTestClient
                .post()
                .uri("/api/v1/ideas-service/group/create")
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
                .uri("/api/v1/ideas-service/group/create")
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
                .uri("/api/v1/ideas-service/skill/add")
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(skillDTO), SkillDTO.class)
                .exchange()
                .expectBody(SkillDTO.class)
                .returnResult().getResponseBody();
    }

    @Test
    void testGetProfile() {

        ProfileDTO profile = createProfile();
        String userId = profile.getId();

        String userTag = "lerlfee";

        setUserTag(profile.getEmail(), userTag, 323123232L, false);


        ProfileDTO responseGetProfileByOwner = webTestClient
                .get()
                .uri("/api/v1/ideas-service/profile/{userId}", userId)
                .header("Authorization", "Bearer " + jwt)
                .exchange()
                .expectBody(ProfileDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(responseGetProfileByOwner);
        assertEquals(profile.getEmail(), responseGetProfileByOwner.getEmail());
        assertEquals(responseGetProfileByOwner.getUserTag(), userTag);

        ProfileDTO responseGetProfileByRandomUser = webTestClient
                .get()
                .uri("/api/v1/ideas-service/profile/{userId}", userId)
                .header("Authorization", "Bearer " + jwt_randomUser)
                .exchange()
                .expectBody(ProfileDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(responseGetProfileByRandomUser);
        assertEquals(profile.getEmail(), responseGetProfileByRandomUser.getEmail());
        assertNull(responseGetProfileByRandomUser.getUserTag());

        setUserTagVisible(true, profile.getEmail());

        ProfileDTO secondResponseGetProfileByRandomUser = webTestClient
                .get()
                .uri("/api/v1/ideas-service/profile/{userId}", userId)
                .header("Authorization", "Bearer " + jwt_randomUser)
                .exchange()
                .expectBody(ProfileDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(secondResponseGetProfileByRandomUser);
        assertEquals(profile.getEmail(), secondResponseGetProfileByRandomUser.getEmail());
        assertEquals(userTag, secondResponseGetProfileByRandomUser.getUserTag());

    }

    @Test
    void testUploadAvatar() {

        ProfileDTO profile = createProfile();

        ProfileDTO responseUploadAvatar = webTestClient
                .post()
                .uri("/api/v1/ideas-service/profile/avatar/upload")
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
        String userId = profile.getId();

        webTestClient
                .post()
                .uri("/api/v1/ideas-service/profile/avatar/upload")
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(profile), ProfileDTO.class)
                .exchange()
                .expectBody(ProfileDTO.class)
                .returnResult();

        ProfileDTO responseGetAvatar = webTestClient
                .get()
                .uri("/api/v1/ideas-service/profile/{userId}", userId)
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
                .uri("/api/v1/ideas-service/profile/skills/save")
                .header("Authorization", "Bearer " + jwt)
                .exchange()
                .expectBody(ProfileDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(responseSaveSkills);
    }
}
