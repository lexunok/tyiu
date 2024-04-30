package com.tyiu.ideas.controller;

import com.tyiu.ideas.model.*;
import com.tyiu.ideas.model.enums.Role;
import com.tyiu.ideas.model.requests.RegisterRequest;
import com.tyiu.ideas.model.responses.AuthenticationResponse;
import com.tyiu.ideas.model.responses.InfoResponse;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.HttpStatus;
import org.springframework.test.web.reactive.server.StatusAssertions;
import org.springframework.test.web.reactive.server.WebTestClient;
import reactor.core.publisher.Mono;

import java.util.List;
import java.util.Objects;

import static org.junit.jupiter.api.Assertions.*;
import static org.springframework.boot.test.context.SpringBootTest.WebEnvironment.RANDOM_PORT;

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringBootTest(webEnvironment = RANDOM_PORT)
public class TagControllerTest extends TestContainers {

    @Autowired
    private WebTestClient webTestClient;
    private final String main_path = "/api/v1/scrum-service/tag";

    private String jwt_initiator;
    private String jwt_expert;
    private String jwt_office;
    private String jwt_admin;
    private String jwt_member;
    private String jwt_leader;
    private String jwt_owner;
    private String jwt_teacher;

    private RegisterRequest buildRegisterRequest(String email, String lastName, List<Role> roles){
        return new RegisterRequest(email, lastName, "task", "password", roles);
    }

    private TagDTO buildTag(){
        return new TagDTO(
                null,
                "Фронтенд",
                "Красный",
                null,
                null,
                null,
                null
        );
    }

    private AuthenticationResponse createUser(RegisterRequest request){
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

    private TagDTO createTag(TagDTO buildTag, String jwt){
        TagDTO createdTag = webTestClient
                .post()
                .uri("/api/v1/scrum-service/tag/add/no-confirmed")
                .header("Authorization", jwt)
                .body(Mono.just(buildTag), TagDTO.class)
                .exchange()
                .expectBody(TagDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(createdTag);
        return createdTag;
    }

    private List<TagDTO> getListTag(String path){
        return webTestClient
                .get()
                .uri(main_path + path)
                .header("Authorization", jwt_admin)
                .exchange()
                .expectBodyList(TagDTO.class)
                .returnResult().getResponseBody();
    }

    private void assertTag(TagDTO expected, TagDTO actual){
        assertEquals(expected.getName(), actual.getName());
        assertEquals(expected.getColor(), actual.getColor());
        assertEquals(expected.getConfirmed(), actual.getConfirmed());
        assertEquals(expected.getCreatorId(), actual.getCreatorId());
    }

    private StatusAssertions checkGetAllTags(String jwt){
        return webTestClient
                .get()
                .uri(main_path + "/all")
                .header("Authorization", jwt)
                .exchange()
                .expectStatus();
    }

    private StatusAssertions checkPostCreateConfTag(TagDTO tagDTO, String jwt){
        return webTestClient
                .post()
                .uri(main_path + "/add")
                .header("Authorization", jwt)
                .body(Mono.just(tagDTO), TagDTO.class)
                .exchange()
                .expectStatus();
    }

    private StatusAssertions checkPostCreateNoConfirmedTag(TagDTO tagDTO, String jwt){
        return webTestClient
                .post()
                .uri(main_path + "/add/no-confirmed")
                .header("Authorization", jwt)
                .body(Mono.just(tagDTO), TagDTO.class)
                .exchange()
                .expectStatus();
    }

    private StatusAssertions checkPutConfirmTag(String tagId, String jwt){
        return webTestClient
                .put()
                .uri(main_path + "/confirm/{tagId}", tagId)
                .header("Authorization", jwt)
                .exchange()
                .expectStatus();
    }

    private StatusAssertions checkPutUpdateTag(TagDTO buildTag, String tagId, String jwt){
        return webTestClient
                .put()
                .uri(main_path + "/update/{taskId}", tagId)
                .header("Authorization", jwt)
                .body(Mono.just(buildTag), TagDTO.class)
                .exchange()
                .expectStatus();
    }

    private StatusAssertions checkDeleteTag(String tagId, String jwt){
        return webTestClient
                .delete()
                .uri(main_path + "/delete/{tagId}", tagId)
                .header("Authorization", jwt)
                .exchange()
                .expectStatus();
    }

    @BeforeAll
    public void setUp() {
        RegisterRequest initiatorRequest = buildRegisterRequest("tag.initiator@gmail.com", "initiator", List.of(Role.INITIATOR));
        RegisterRequest expertRequest = buildRegisterRequest("tag.expert@gmail.com", "expert", List.of(Role.EXPERT));
        RegisterRequest officeRequest = buildRegisterRequest("tag.office@gmail.com", "office", List.of(Role.PROJECT_OFFICE));
        RegisterRequest adminRequest = buildRegisterRequest("tag.admin@gmail.com", "admin", List.of(Role.ADMIN));
        RegisterRequest memberRequest = buildRegisterRequest("tag.member@gmail.com", "member", List.of(Role.MEMBER));
        RegisterRequest leaderRequest = buildRegisterRequest("tag.leader@gmail.com", "leader", List.of(Role.TEAM_LEADER));
        RegisterRequest ownerRequest = buildRegisterRequest("tag.owner@gmail.com", "owner", List.of(Role.TEAM_OWNER));
        RegisterRequest teacherRequest = buildRegisterRequest("tag.teacher@gmail.com", "teacher", List.of(Role.TEACHER));

        AuthenticationResponse initiatorResponse = createUser(initiatorRequest);
        AuthenticationResponse expertResponse = createUser(expertRequest);
        AuthenticationResponse officeResponse = createUser(officeRequest);
        AuthenticationResponse adminResponse = createUser(adminRequest);
        AuthenticationResponse memberResponse = createUser(memberRequest);
        AuthenticationResponse leaderResponse = createUser(leaderRequest);
        AuthenticationResponse ownerResponse = createUser(ownerRequest);
        AuthenticationResponse teacherResponse = createUser(teacherRequest);

        jwt_initiator = "Bearer " + initiatorResponse.getToken();
        jwt_expert = "Bearer " + expertResponse.getToken();
        jwt_office = "Bearer " + officeResponse.getToken();
        jwt_admin = "Bearer " + adminResponse.getToken();
        jwt_member = "Bearer " + memberResponse.getToken();
        jwt_leader = "Bearer " + leaderResponse.getToken();
        jwt_owner = "Bearer " + ownerResponse.getToken();
        jwt_teacher = "Bearer " + teacherResponse.getToken();
    }

    @Test
    void testGetAllTags(){
        TagDTO tag1 = createTag(buildTag(),jwt_admin);
        TagDTO tag2 = createTag(buildTag(),jwt_admin);
        TagDTO tag3 = createTag(buildTag(),jwt_admin);
        TagDTO tag4 = createTag(buildTag(),jwt_admin);
        List<TagDTO> tags = getListTag("/all");
        assertNotNull(tags);
        assertTrue(tags.size() >= 4);
        tags.forEach(t -> {
            if (Objects.equals(tag1.getId(), t.getId())){
                assertTag(tag1, t);
            }
            else if (Objects.equals(tag2.getId(), t.getId())){
                assertTag(tag2, t);
            }
            else if (Objects.equals(tag3.getId(), t.getId())){
                assertTag(tag3, t);
            }
            else if (Objects.equals(tag4.getId(), t.getId())){
                assertTag(tag4, t);
            }
        });
        checkGetAllTags(jwt_initiator).isOk();
        checkGetAllTags(jwt_office).isOk();
        checkGetAllTags(jwt_member).isOk();
        checkGetAllTags(jwt_owner).isOk();
        checkGetAllTags(jwt_teacher).isForbidden();
        checkGetAllTags(jwt_expert).isForbidden();
        checkGetAllTags(jwt_leader).isForbidden();
    }

    @Test
    void testPostCreateConfTag(){
        checkPostCreateConfTag(buildTag(),jwt_admin).isOk();
        checkPostCreateConfTag(buildTag(),jwt_initiator).isForbidden();
        checkPostCreateConfTag(buildTag(),jwt_office).isForbidden();
        checkPostCreateConfTag(buildTag(),jwt_member).isForbidden();
        checkPostCreateConfTag(buildTag(),jwt_owner).isForbidden();
        checkPostCreateConfTag(buildTag(),jwt_teacher).isForbidden();
        checkPostCreateConfTag(buildTag(),jwt_expert).isForbidden();
        checkPostCreateConfTag(buildTag(),jwt_leader).isForbidden();
    }

    @Test
    void testPostCreateNoConfTag(){
        checkPostCreateNoConfirmedTag(buildTag(),jwt_admin).isOk();
        checkPostCreateNoConfirmedTag(buildTag(),jwt_initiator).isOk();
        checkPostCreateNoConfirmedTag(buildTag(),jwt_office).isOk();
        checkPostCreateNoConfirmedTag(buildTag(),jwt_member).isOk();
        checkPostCreateNoConfirmedTag(buildTag(),jwt_owner).isOk();
        checkPostCreateNoConfirmedTag(buildTag(),jwt_teacher).isForbidden();
        checkPostCreateNoConfirmedTag(buildTag(),jwt_expert).isForbidden();
        checkPostCreateNoConfirmedTag(buildTag(),jwt_leader).isForbidden();
    }

    @Test
    void testPutConfirmTag(){
        TagDTO tag = createTag(buildTag(),jwt_admin);
        InfoResponse response = webTestClient
                .put()
                .uri(main_path + "/confirm/{tagId}", tag.getId())
                .header("Authorization", jwt_admin)
                .exchange()
                .expectBody(InfoResponse.class)
                .returnResult().getResponseBody();
        assertNotNull(response);
        assertEquals(response.getStatusCode(), HttpStatus.OK);
        assertEquals(response.getMessage(), "Тег утверждён");
        checkPutConfirmTag(tag.getId(),jwt_initiator).isForbidden();
        checkPutConfirmTag(tag.getId(),jwt_office).isForbidden();
        checkPutConfirmTag(tag.getId(),jwt_member).isForbidden();
        checkPutConfirmTag(tag.getId(),jwt_owner).isForbidden();
        checkPutConfirmTag(tag.getId(),jwt_teacher).isForbidden();
        checkPutConfirmTag(tag.getId(),jwt_expert).isForbidden();
        checkPutConfirmTag(tag.getId(),jwt_leader).isForbidden();
    }

    @Test
    void testPutUpdateTag(){
        TagDTO tag = createTag(buildTag(),jwt_admin);
        TagDTO updateTag = new TagDTO(
                tag.getId(),
                "Новое имя",
                "Теперь фиолетовый",
                tag.getConfirmed(),
                tag.getCreatorId(),
                null,
                null
        );
        assertNotEquals(updateTag.getName(), tag.getName());
        assertNotEquals(updateTag.getColor(), tag.getColor());
        InfoResponse response = webTestClient
                .put()
                .uri(main_path + "/update/{tagId}", tag.getId())
                .header("Authorization", jwt_admin)
                .body(Mono.just(updateTag), TagDTO.class)
                .exchange()
                .expectBody(InfoResponse.class)
                .returnResult().getResponseBody();
        assertNotNull(response);
        assertEquals(response.getStatusCode(), HttpStatus.OK);
        assertEquals(response.getMessage(), "Тег обновлён успешно");
        checkPutUpdateTag(updateTag, tag.getId(), jwt_initiator).isForbidden();
        checkPutUpdateTag(updateTag, tag.getId(), jwt_office).isForbidden();
        checkPutUpdateTag(updateTag, tag.getId(), jwt_member).isForbidden();
        checkPutUpdateTag(updateTag, tag.getId(), jwt_owner).isForbidden();
        checkPutUpdateTag(updateTag, tag.getId(), jwt_teacher).isForbidden();
        checkPutUpdateTag(updateTag, tag.getId(), jwt_expert).isForbidden();
        checkPutUpdateTag(updateTag, tag.getId(), jwt_leader).isForbidden();
    }

    @Test
    void testDeleteTag(){
        TagDTO tag1 = createTag(buildTag(),jwt_admin);
        TagDTO tag2 = createTag(buildTag(),jwt_admin);
        TagDTO tag3 = createTag(buildTag(),jwt_admin);
        TagDTO tag4 = createTag(buildTag(),jwt_admin);
        InfoResponse response = webTestClient
                .delete()
                .uri(main_path + "/delete/{tagId}", tag2.getId())
                .header("Authorization", jwt_admin)
                .exchange()
                .expectBody(InfoResponse.class)
                .returnResult().getResponseBody();
        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
        assertEquals("Тег удалён успешно", response.getMessage());
        checkDeleteTag(createTag(buildTag(),jwt_admin).getId(),jwt_initiator).isForbidden();
        checkDeleteTag(createTag(buildTag(),jwt_admin).getId(),jwt_office).isForbidden();
        checkDeleteTag(createTag(buildTag(),jwt_admin).getId(),jwt_member).isForbidden();
        checkDeleteTag(createTag(buildTag(),jwt_admin).getId(),jwt_owner).isForbidden();
        checkDeleteTag(createTag(buildTag(),jwt_admin).getId(),jwt_teacher).isForbidden();
        checkDeleteTag(createTag(buildTag(),jwt_admin).getId(),jwt_expert).isForbidden();
        checkDeleteTag(createTag(buildTag(),jwt_admin).getId(),jwt_leader).isForbidden();
    }
}
