package com.tyiu.corn.controller;

import com.tyiu.corn.model.dto.*;
import com.tyiu.corn.model.entities.Idea;
import com.tyiu.corn.model.enums.Role;
import com.tyiu.corn.model.enums.SkillType;
import com.tyiu.corn.model.requests.IdeaSkillRequest;
import com.tyiu.corn.model.requests.RegisterRequest;
import com.tyiu.corn.model.requests.StatusIdeaRequest;
import com.tyiu.corn.model.responses.AuthenticationResponse;
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
class IdeaControllerTest extends TestContainers {
    @Autowired
    private WebTestClient webTestClient;

    private String jwt;
    private UserDTO userDTO;
    private GroupDTO expertGroup;
    private GroupDTO projectGroup;
    private String ideaId;

    @BeforeAll
    void setUp() {
        RegisterRequest request = new RegisterRequest("user.idea@mail.com", "firstname", "lastname", "password",
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


    }
    @Test
    void testSaveIdea(){
        IdeaDTO ideaDTO = IdeaDTO.builder()
                .initiatorEmail(userDTO.getEmail())
                .name("ГДЗ онлайн")
                .status(Idea.Status.ON_APPROVAL)
                .experts(expertGroup)
                .projectOffice(projectGroup)
                .problem("Отсутствия готовых решений задач")
                .solution("Форум, где студенты могут оставить свои решения")
                .result("Удобная онлайн платформа")
                .customer("Студенты")
                .contactPerson("Стас")
                .description("Для студентов!")
                .build();

        IdeaDTO ideaResponse = webTestClient
                .post()
                .uri("/api/v1/idea/add")
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(ideaDTO), IdeaDTO.class)
                .exchange()
                .expectBody(IdeaDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(ideaResponse);
        assertNotNull(ideaResponse.getId(), String.valueOf(ideaDTO.getId()));
        ideaId = ideaResponse.getId();
    }

    @Test
    void testAddIdeaInDraft(){
        IdeaDTO ideaDTOinDraft = IdeaDTO.builder()
                .initiatorEmail(userDTO.getEmail())
                .name("ГДЗ онлайн 2")
                .experts(expertGroup)
                .projectOffice(projectGroup)
                .status(Idea.Status.NEW)
                .problem("Отсутствия готовых решений задач")
                .solution("Форум, где студенты могут оставить свои решения")
                .result("Удобная онлайн платформа")
                .customer("Студенты")
                .contactPerson("Стас")
                .description("Для студентов!")
                .build();

        IdeaDTO ideaResponse = webTestClient
                .post()
                .uri("/api/v1/idea/draft/add")
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(ideaDTOinDraft), IdeaDTO.class)
                .exchange()
                .expectBody(IdeaDTO.class)
                .returnResult().getResponseBody();

        assertNotNull(ideaResponse);
        assertNotNull(ideaResponse.getName(), ideaDTOinDraft.getName());
    }

    @Test
    void testGetIdea(){
        IdeaDTO ideaResponse = webTestClient
                .get()
                .uri("/api/v1/idea/{ideaId}", ideaId)
                .header("Authorization","Bearer " + jwt)
                .exchange()
                .expectBody(IdeaDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(ideaResponse);
        assertEquals(ideaResponse.getId(), ideaId);
    }

    @Test
    void testShowListIdea(){
        List<IdeaDTO> ideaList = webTestClient
                .get()
                .uri("/api/v1/idea/all")
                .header("Authorization","Bearer " + jwt)
                .exchange()
                .expectBodyList(IdeaDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(ideaList);
        assertFalse(ideaList.isEmpty());
    }

    @Test
    void testShowListIdeaByInitiator(){
        List<IdeaDTO> ideaList = webTestClient
                .get()
                .uri("/api/v1/idea/initiator/all")
                .header("Authorization","Bearer " + jwt)
                .exchange()
                .expectBodyList(IdeaDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(ideaList);
        assertFalse(ideaList.isEmpty());
    }

    @Test
    void testAddIdeaSkills(){

        SkillDTO skillDTO = SkillDTO.builder()
                .name("Python")
                .type(SkillType.LANGUAGE)
                .build();

        SkillDTO skillDTO1 = SkillDTO.builder()
                .name("VUE")
                .type(SkillType.FRAMEWORK)
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
        assertEquals(addSkillResponse.getName(), skillDTO.getName());

        SkillDTO addSkillResponse1 = webTestClient
                .post()
                .uri("/api/v1/skill/add")
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(skillDTO1), SkillDTO.class)
                .exchange()
                .expectBody(SkillDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(addSkillResponse1);
        assertEquals(addSkillResponse1.getName(), skillDTO1.getName());

        IdeaSkillRequest ideaSkill = IdeaSkillRequest.builder()
                .skills(List.of(addSkillResponse1, addSkillResponse))
                .ideaId(ideaId)
                .build();

        webTestClient
                .post()
                .uri("/api/v1/idea/skills/add")
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(ideaSkill), IdeaSkillRequest.class)
                .exchange()
                .expectStatus().isOk();
    }

    @Test
    void testUpdateIdeaSkills(){
        SkillDTO skillDTO = SkillDTO.builder()
                .name("Java")
                .type(SkillType.LANGUAGE)
                .build();

        SkillDTO skillDTO1 = SkillDTO.builder()
                .name("React JS")
                .type(SkillType.FRAMEWORK)
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
        assertEquals(addSkillResponse.getName(), skillDTO.getName());

        SkillDTO addSkillResponse1 = webTestClient
                .post()
                .uri("/api/v1/skill/add")
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(skillDTO1), SkillDTO.class)
                .exchange()
                .expectBody(SkillDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(addSkillResponse1);
        assertEquals(addSkillResponse1.getName(), skillDTO1.getName());

        IdeaDTO ideaDTO = IdeaDTO.builder()
                .name("АнтиПлагиат 3")
                .id(ideaId)
                .initiatorEmail(userDTO.getEmail())
                .experts(expertGroup)
                .projectOffice(projectGroup)
                .status(Idea.Status.NEW)
                .problem("Отсутствия готовых решений задач")
                .solution("Форум, где студенты могут оставить свои решения")
                .result("Удобная онлайн платформа")
                .customer("Студенты")
                .contactPerson("Стас")
                .description("Для студентов!")
                .build();

        IdeaDTO ideaAddResponse = webTestClient
                .post()
                .uri("/api/v1/idea/add")
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(ideaDTO), IdeaDTO.class)
                .exchange()
                .expectBody(IdeaDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(ideaAddResponse);

        IdeaSkillRequest ideaSkill = IdeaSkillRequest.builder()
                .skills(List.of(addSkillResponse1, addSkillResponse))
                .ideaId(ideaAddResponse.getId())
                .build();
        webTestClient
                .put()
                .uri("/api/v1/idea/skills/update")
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(ideaSkill), IdeaSkillRequest.class)
                .exchange()
                .expectStatus().isOk();
    }

    @Test
    void testGetIdeaSkills(){
        IdeaSkillRequest skillResponse = webTestClient
                .get()
                .uri("/api/v1/idea/skills/{ideaId}", ideaId)
                .header("Authorization","Bearer " + jwt)
                .exchange()
                .expectBody(IdeaSkillRequest.class)
                .returnResult().getResponseBody();
        assertNotNull(skillResponse);
        assertEquals(ideaId,  skillResponse.getIdeaId());
    }

    @Test
    void updateIdeaByAdmin(){
        IdeaDTO ideaDTO = IdeaDTO.builder()
                .name("АнтиПлагиат 3")
                .id(ideaId)
                .initiatorEmail(userDTO.getEmail())
                .experts(expertGroup)
                .projectOffice(projectGroup)
                .status(Idea.Status.NEW)
                .problem("Отсутствия готовых решений задач")
                .solution("Форум, где студенты могут оставить свои решения")
                .result("Удобная онлайн платформа")
                .customer("Студенты")
                .contactPerson("Стас")
                .description("Для студентов!")
                .build();

        IdeaDTO ideaAddResponse = webTestClient
                .post()
                .uri("/api/v1/idea/add")
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(ideaDTO), IdeaDTO.class)
                .exchange()
                .expectBody(IdeaDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(ideaAddResponse);

        IdeaDTO updatedIdea = IdeaDTO.builder()
                .id(ideaAddResponse.getId())
                .name("АнтиПлагиат 2")
                .experts(ideaAddResponse.getExperts())
                .contactPerson(ideaAddResponse.getContactPerson())
                .description(ideaAddResponse.getDescription())
                .initiatorEmail(ideaAddResponse.getInitiatorEmail())
                .solution(ideaAddResponse.getSolution())
                .build();

        webTestClient
                .put()
                .uri("/api/v1/idea/admin/update/{ideaId}", ideaAddResponse.getId())
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(updatedIdea), IdeaDTO.class)
                .exchange()
                .expectStatus().isOk();
    }

    @Test
    void testUpdateStatusByInitiator(){
        IdeaDTO ideaDTO = IdeaDTO.builder()
                .name("АнтиПлагиат 3")
                .id(ideaId)
                .initiatorEmail(userDTO.getEmail())
                .experts(expertGroup)
                .projectOffice(projectGroup)
                .status(Idea.Status.NEW)
                .problem("Отсутствия готовых решений задач")
                .solution("Форум, где студенты могут оставить свои решения")
                .result("Удобная онлайн платформа")
                .customer("Студенты")
                .contactPerson("Стас")
                .description("Для студентов!")
                .build();

        IdeaDTO ideaAddResponse = webTestClient
                .post()
                .uri("/api/v1/idea/add")
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(ideaDTO), IdeaDTO.class)
                .exchange()
                .expectBody(IdeaDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(ideaAddResponse);

        webTestClient
                .put()
                .uri("/api/v1/idea/initiator/send/{ideaId}", ideaAddResponse.getId())
                .header("Authorization", "Bearer " + jwt)
                .exchange()
                .expectStatus().isOk();
    }
    @Test
    void testUpdateStatusIdea(){
        IdeaDTO ideaDTO = IdeaDTO.builder()
                .name("АнтиПлагиат 4")
                .id(ideaId)
                .initiatorEmail(userDTO.getEmail())
                .experts(expertGroup)
                .projectOffice(projectGroup)
                .status(Idea.Status.NEW)
                .problem("Отсутствия готовых решений задач")
                .solution("Форум, где студенты могут оставить свои решения")
                .result("Удобная онлайн платформа")
                .customer("Студенты")
                .contactPerson("Стас")
                .description("Для студентов!")
                .build();

        IdeaDTO ideaAddResponse = webTestClient
                .post()
                .uri("/api/v1/idea/add")
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(ideaDTO), IdeaDTO.class)
                .exchange()
                .expectBody(IdeaDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(ideaAddResponse);

        StatusIdeaRequest statusIdeaRequest = new StatusIdeaRequest();
        statusIdeaRequest.setStatus(Idea.Status.ON_CONFIRMATION);

        webTestClient
                .put()
                .uri("/api/v1/idea/status/update/{ideaId}", ideaAddResponse.getId())
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(statusIdeaRequest), StatusIdeaRequest.class)
                .exchange()
                .expectStatus().isOk();
    }

    @Test
    void deleteIdea(){
        IdeaDTO ideaDTO = IdeaDTO.builder()
                .name("АнтиПлагиат 3")
                .id(ideaId)
                .initiatorEmail(userDTO.getEmail())
                .experts(expertGroup)
                .projectOffice(projectGroup)
                .status(Idea.Status.NEW)
                .problem("Отсутствия готовых решений задач")
                .solution("Форум, где студенты могут оставить свои решения")
                .result("Удобная онлайн платформа")
                .customer("Студенты")
                .contactPerson("Стас")
                .description("Для студентов!")
                .build();

        IdeaDTO ideaAddResponse = webTestClient
                .post()
                .uri("/api/v1/idea/add")
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(ideaDTO), IdeaDTO.class)
                .exchange()
                .expectBody(IdeaDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(ideaAddResponse);

        webTestClient
                .delete()
                .uri("/api/v1/idea/delete/{ideaId}", ideaAddResponse.getId())
                .header("Authorization", "Bearer " + jwt)
                .exchange()
                .expectStatus().isOk();
    }
}
