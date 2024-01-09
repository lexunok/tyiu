package com.tyiu.ideas.controller;

import com.tyiu.ideas.model.dto.*;
import com.tyiu.ideas.model.entities.*;
import com.tyiu.ideas.model.enums.CodeStatus;
import com.tyiu.ideas.model.enums.Role;
import com.tyiu.ideas.model.requests.ChangeRequest;
import com.tyiu.ideas.model.requests.LoginRequest;
import com.tyiu.ideas.model.requests.RegisterRequest;
import com.tyiu.ideas.model.responses.*;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.TestInstance;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.data.r2dbc.core.R2dbcEntityTemplate;
import static org.springframework.data.relational.core.query.Criteria.where;
import static org.springframework.data.relational.core.query.Query.query;

import org.springframework.http.HttpStatus;
import org.springframework.r2dbc.core.DatabaseClient;
import org.springframework.test.web.reactive.server.WebTestClient;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.time.Duration;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.*;
import static org.springframework.boot.test.context.SpringBootTest.WebEnvironment.RANDOM_PORT;

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringBootTest(webEnvironment = RANDOM_PORT)
public class AccountChangeControllerTest extends TestContainers {
    @Autowired
    private WebTestClient webTestClient;

    @Autowired
    private R2dbcEntityTemplate template;

    @Autowired
    private DatabaseClient databaseClient;

    private String jwt;
    private UserDTO userDTO;
    private String userPassword;

    private String invitationId;

    private GroupDTO projectGroup;
    private GroupDTO expertGroup;


    private Integer code;
    private String changeDataId;

    private TeamDTO buildTeam(String name,String description, Integer membersCount,
                              UserDTO owner, UserDTO leader, List<UserDTO> members,
                              List<SkillDTO> wantedSkills){
        return TeamDTO.builder()
                .name(name)
                .description(description)
                .closed(false)
                .membersCount(membersCount)
                .createdAt(LocalDate.now())
                .owner(owner)
                .leader(leader)
                .members(members)
                .wantedSkills(wantedSkills)
                .build();
    }

    private void createObjectsWithUserReference(UserDTO userDTO, String userToken){
        TeamDTO team = buildTeam("Богатыри","Слава Руси!",1,
                userDTO,userDTO,List.of(userDTO),List.of());
        TeamDTO responseAddTeam = webTestClient.post().uri("/api/v1/team/add")
                .header("Authorization", "Bearer " + userToken)
                .body(Mono.just(team), TeamDTO.class)
                .exchange().expectBody(TeamDTO.class).returnResult().getResponseBody();

        assertNotNull(responseAddTeam);
        assertEquals(team.getName(), responseAddTeam.getName());
        assertEquals(team.getDescription(), responseAddTeam.getDescription());
        assertEquals(team.getClosed(), responseAddTeam.getClosed());
        assertEquals(team.getMembersCount(), responseAddTeam.getMembersCount());
        assertEquals(team.getOwner(), responseAddTeam.getOwner());
        assertEquals(team.getLeader(), responseAddTeam.getLeader());
        assertEquals(team.getSkills(), responseAddTeam.getSkills());
        assertEquals(team.getWantedSkills(), responseAddTeam.getWantedSkills());

        createProjectOfficeAndExpertGroups();

        IdeaDTO ideaDTOinDraft = IdeaDTO.builder()
                .initiator(userDTO)
                .name("ГДЗ онлайн 2")
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
                .header("Authorization", "Bearer " + userToken)
                .body(Mono.just(ideaDTOinDraft), IdeaDTO.class)
                .exchange()
                .expectBody(IdeaDTO.class)
                .returnResult().getResponseBody();

        assertNotNull(ideaResponse);
        assertNotNull(ideaResponse.getName(), ideaDTOinDraft.getName());
    }
    private void createProjectOfficeAndExpertGroups() {
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

        assertNotNull(expertGroup);
        assertNotNull(projectGroup);
    }

    @BeforeAll
    public void setUp() {
        RegisterRequest request = new RegisterRequest(
                "account.change@gmail.com", "account", "change", "account.change",
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
                .createdAt(response.getCreatedAt())
                .build();
        userPassword = request.getPassword();
        webTestClient = webTestClient.mutate()
                .responseTimeout(Duration.ofMillis(30000))
                .build();
    }

    @Test
    void testGetNotExistedInvitation() {
        ErrorResponse errorResponse = webTestClient
                .get()
                .uri("/api/v1/profile/get/invitation/not-existed")
                .exchange()
                .expectBody(ErrorResponse.class)
                .returnResult().getResponseBody();

        assertNotNull(errorResponse);
        assertEquals("Not found!", errorResponse.getError());
    }

    @Test
    void testSendInvitation() {
        InvitationDTO invitationDTO = InvitationDTO.builder()
                .email("test1@gmailerge.com")
                .roles(List.of(Role.INITIATOR))
                .build();
        InfoResponse infoResponse = webTestClient
                .post()
                .uri("/api/v1/profile/send/email")
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(invitationDTO), InvitationDTO.class)
                .exchange()
                .expectBody(InfoResponse.class)
                .returnResult().getResponseBody();
        assertNotNull(infoResponse);
        assertEquals("Успешное приглашение", infoResponse.getMessage());

        template.exists(query(where("email").is(invitationDTO.getEmail())
                        .and(where("roles").is(invitationDTO.getRoles()
                                .stream().map(String::valueOf).toArray()))), Invitation.class)
                .flatMap(b -> Mono.fromRunnable(() -> assertTrue(b)))
                .as(StepVerifier::create)
                .expectComplete()
                .verify();
    }

    @Test
    void testGetInvitation() {
        template.insert(Invitation.builder()
                .dateExpired(LocalDateTime.now().plusDays(1))
                .roles(List.of(Role.ADMIN))
                .email("proverkaget@gmairgfrl.com")
                .build()
        ).flatMap(i -> {
            invitationId = i.getId();
            return Mono.empty();
        }).as(StepVerifier::create).expectComplete().verify();

        InvitationResponse invitationResponse = webTestClient
                .get().uri("/api/v1/profile/get/invitation/{url}", invitationId)
                .exchange().expectBody(InvitationResponse.class).returnResult().getResponseBody();

        assertNotNull(invitationResponse);
        assertEquals("proverkaget@gmairgfrl.com", invitationResponse.getEmail());
        assertEquals(List.of(Role.ADMIN), invitationResponse.getRoles());
    }

    @Test
    void testSendInvitationToRegisteredUser() {
        InvitationDTO invitationDTO = InvitationDTO.builder()
                .email("account.change@gmail.com")
                .roles(List.of(Role.INITIATOR))
                .build();
        InfoResponse infoResponse = webTestClient
                .post()
                .uri("/api/v1/profile/send/email")
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(invitationDTO), InvitationDTO.class)
                .exchange()
                .expectBody(InfoResponse.class)
                .returnResult().getResponseBody();
        assertNotNull(infoResponse);
        assertEquals("Ошибка при приглашении", infoResponse.getMessage());
        assertEquals(HttpStatus.INTERNAL_SERVER_ERROR, infoResponse.getStatusCode());

        template.exists(query(where("email").is(invitationDTO.getEmail())
                        .and(where("roles").is(invitationDTO.getRoles().stream().map(String::valueOf).toArray()))), Invitation.class)
                .flatMap(b -> Mono.fromRunnable(() -> assertFalse(b)))
                .as(StepVerifier::create)
                .expectComplete()
                .verify();
    }

    @Test
    void testSendInvitations() {
        RegisterRequest request = new RegisterRequest(
                "account2.change@gmail.com", "account", "change", "account.change",
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
        InvitationsDTO invitations = InvitationsDTO.builder()
                .emails(List.of("1@mail.com",
                        "12@marfrfil.com",
                        "3@marfril.com",
                        "4@marfril.com",
                        "5@mafrfil.com",
                        "1@mfrfail.com",
                        "12452grwrg@mail.com",
                        "1@mgwergewail.com",
                        "1@margwrgwwrgwril.com",
                        "1@mgwrail.com",
                        "1@mgwrgwrail.com",
                        "1@margwril.com",
                        "1@mail.com",
                        "account2.change@gmail.com",
                        "account.change@gmail.com",
                        "1@mrgwrgwragwrgwril.com",
                        "1@magwrgwil.com",
                        "1@mgwrgwail.com"))
                .roles(List.of(Role.INITIATOR))
                .build();
        Void result = webTestClient
                .post()
                .uri("/api/v1/profile/send/emails")
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(invitations), InvitationsDTO.class)
                .exchange().expectBody(Void.class).returnResult().getResponseBody();

        assertNull(result);

        template.exists(query(where("email").is("account2.change@gmail.com")
                        .and(where("email").is("account.change@gmail.com"))), Invitation.class)
                .flatMap(b -> Mono.fromRunnable(() -> assertFalse(b)))
                .as(StepVerifier::create)
                .expectComplete()
                .verify();
    }

    @Test
    void testChangeEmailToNew() {
        RegisterRequest request = new RegisterRequest(
                "egerger.eg@gwrg.wrg", "account", "change", "account.change",
                List.of(Role.ADMIN,
                        Role.EXPERT,
                        Role.PROJECT_OFFICE,
                        Role.INITIATOR,
                        Role.MEMBER,
                        Role.TEAM_OWNER));
        AuthenticationResponse response = webTestClient
                .post()
                .uri("/api/v1/auth/register")
                .body(Mono.just(request), RegisterRequest.class)
                .exchange()
                .expectBody(AuthenticationResponse.class)
                .returnResult().getResponseBody();
        assertNotNull(response);

        createObjectsWithUserReference(
                UserDTO.builder()
                        .id(response.getId())
                        .email(response.getEmail())
                        .build(), response.getToken()
        );

        ChangeEmailDataDTO changeEmailDataDTO = ChangeEmailDataDTO.builder()
                .newEmail("wrgwrgwg@rgwrg.wrg")
                .build();
        InfoResponse changerUrl = webTestClient
                .post()
                .uri("/api/v1/profile/send/change/email")
                .header("Authorization", "Bearer " + response.getToken())
                .body(Mono.just(changeEmailDataDTO), ChangeEmailDataDTO.class)
                .exchange()
                .expectBody(InfoResponse.class)
                .returnResult().getResponseBody();
        assertNotNull(changerUrl);
        assertEquals("Ссылка на изменение почты находится на новой почте", changerUrl.getMessage());

        template.selectOne(query(where("new_email").is(changeEmailDataDTO.getNewEmail())
                        .and(where("old_email").is(response.getEmail()))), ChangeEmailData.class)
                .flatMap(changeEmailData -> Mono.fromRunnable(() -> {
                    code = changeEmailData.getCode();
                    changeDataId = changeEmailData.getId();
                }))
                .as(StepVerifier::create).expectComplete().verify();

        ChangeResponse changeResponse = webTestClient
                .get()
                .uri("/api/v1/profile/change/email/{url}", changeDataId)
                .header("Authorization", "Bearer " + response.getToken())
                .exchange().expectBody(ChangeResponse.class).returnResult().getResponseBody();

        assertNotNull(changeResponse);
        assertEquals(changeResponse.getNewEmail(), changeEmailDataDTO.getNewEmail());
        assertEquals(changeResponse.getOldEmail(), response.getEmail());

        ChangeRequest changeRequest = new ChangeRequest();
        changeRequest.setCode(code.toString());
        changeRequest.setKey(changeDataId);
        changeRequest.setNewEmail(changeResponse.getNewEmail());
        changeRequest.setOldEmail(response.getEmail());

        InfoResponse infoResponse = webTestClient.put()
                .uri("/api/v1/profile/change/email")
                .header("Authorization", "Bearer " + response.getToken())
                .body(Mono.just(changeRequest), ChangeRequest.class).exchange()
                .expectBody(InfoResponse.class).returnResult().getResponseBody();

        assertNotNull(infoResponse);
        assertEquals("Успешное изменение почты", infoResponse.getMessage());

        template.exists(query(where("email").is(response.getEmail())), User.class)
                .flatMap(e -> Mono.fromRunnable(() -> assertFalse(e)))
                .as(StepVerifier::create).expectComplete().verify();

        template.exists(query(where("email").is(changeResponse.getNewEmail())), User.class)
                .flatMap(e -> Mono.fromRunnable(() -> assertTrue(e)))
                .as(StepVerifier::create).expectComplete().verify();
    }

    @Test
    void testFailLinkChangeEmail() {
        ErrorResponse errorResponse = webTestClient
                .get()
                .uri("/api/v1/profile/change/email/not-existed-letter")
                .header("Authorization", "Bearer " + jwt)
                .exchange().expectBody(ErrorResponse.class).returnResult().getResponseBody();

        assertNotNull(errorResponse);
        assertEquals("Not found!", errorResponse.getError());
    }

    @Test
    void testFailCodeChangeEmail() {
        ChangeEmailData changeEmailData = ChangeEmailData.builder()
                .oldEmail(userDTO.getEmail())
                .newEmail("1234567@gerhe.rgwr")
                .code(123533)
                .dateExpired(LocalDateTime.now().plusDays(1))
                .build();

        template.insert(changeEmailData).flatMap(e -> Mono.fromRunnable(() -> changeDataId = e.getId()))
                .as(StepVerifier::create).expectComplete().verify();

        ChangeRequest changeRequest = new ChangeRequest();
        changeRequest.setOldEmail(changeEmailData.getOldEmail());
        changeRequest.setNewEmail(changeEmailData.getNewEmail());
        changeRequest.setKey(changeDataId);
        Stream.of("r3g3g3g", "123456", "12345etnetbne").forEach(code -> {
            changeRequest.setCode(code);
            ErrorResponse errorResponse = webTestClient.put()
                    .uri("/api/v1/profile/change/email")
                    .header("Authorization", "Bearer " + jwt)
                    .body(Mono.just(changeRequest), ChangeRequest.class)
                    .exchange().expectBody(ErrorResponse.class).returnResult().getResponseBody();

            assertNotNull(errorResponse);
            assertEquals(CodeStatus.WRONG_CODE.toString(), errorResponse.getError());

            template.exists(query(where("id").is(changeDataId)), ChangeEmailData.class)
                    .flatMap(e -> Mono.fromRunnable(() -> assertTrue(e)))
                    .as(StepVerifier::create).expectComplete().verify();
        });
        ErrorResponse firstErrorResponse = webTestClient.put()
                .uri("/api/v1/profile/change/email")
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(changeRequest), ChangeRequest.class)
                .exchange().expectBody(ErrorResponse.class).returnResult().getResponseBody();

        assertNotNull(firstErrorResponse);
        assertEquals(CodeStatus.CHANGE_FAILED.toString(), firstErrorResponse.getError());

        changeRequest.setCode("123533");

        ErrorResponse secondErrorResponse = webTestClient.put()
                .uri("/api/v1/profile/change/email")
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(changeRequest), ChangeRequest.class)
                .exchange().expectBody(ErrorResponse.class).returnResult().getResponseBody();

        assertNotNull(secondErrorResponse);
        assertEquals(CodeStatus.CHANGE_FAILED.toString(), secondErrorResponse.getError());

        template.exists(query(where("id").is(changeDataId)), ChangeEmailData.class)
                .flatMap(e -> Mono.fromRunnable(() -> assertFalse(e)))
                .as(StepVerifier::create).expectComplete().verify();

        template.exists(query(where("email").is(changeRequest.getNewEmail())), User.class)
                .flatMap(e -> Mono.fromRunnable(() -> assertFalse(e)))
                .as(StepVerifier::create).expectComplete().verify();

        template.exists(query(where("email").is(userDTO.getEmail())), User.class)
                .flatMap(e -> Mono.fromRunnable(() -> assertTrue(e)))
                .as(StepVerifier::create).expectComplete().verify();
    }

    @Test
    void testChangePasswordToNew() {
        RegisterRequest request = new RegisterRequest(
                "delete@me.now", "account", "change", "account.change",
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

        ChangePasswordDataDTO changePasswordDataDTO = ChangePasswordDataDTO.builder()
                .email(response.getEmail())
                .build();
        String changerUrl = webTestClient
                .post()
                .uri("/api/v1/profile/send/change/password")
                .body(Mono.just(changePasswordDataDTO), ChangePasswordDataDTO.class)
                .exchange()
                .expectBody(String.class)
                .returnResult().getResponseBody();
        assertNotNull(changerUrl);

        template.selectOne(query(where("id").is(changerUrl)
                        .and(where("email").is(changePasswordDataDTO.getEmail()))), ChangePasswordData.class)
                .flatMap(changePasswordData -> Mono.fromRunnable(() -> {
                    code = changePasswordData.getCode();
                }))
                .as(StepVerifier::create).expectComplete().verify();

        ChangeRequest changeRequest = new ChangeRequest();
        changeRequest.setCode(code.toString());
        changeRequest.setKey(changerUrl);
        changeRequest.setEmail(changePasswordDataDTO.getEmail());
        changeRequest.setPassword("1234");

        InfoResponse infoResponse = webTestClient.put()
                .uri("/api/v1/profile/change/password")
                .body(Mono.just(changeRequest), ChangeRequest.class).exchange()
                .expectBody(InfoResponse.class).returnResult().getResponseBody();

        assertNotNull(infoResponse);
        assertEquals("Успешное изменение пароля", infoResponse.getMessage());

        LoginRequest loginRequest = new LoginRequest(response.getEmail(), request.getPassword());
        ErrorResponse errorResponse = webTestClient
                .post()
                .uri("/api/v1/auth/login")
                .body(Mono.just(loginRequest), LoginRequest.class)
                .exchange()
                .expectBody(ErrorResponse.class)
                .returnResult().getResponseBody();
        assertNotNull(errorResponse);
        assertEquals("Авторизация не удалась!", errorResponse.getError());

        loginRequest.setPassword(changeRequest.getPassword());
        AuthenticationResponse authenticationResponse = webTestClient
                .post()
                .uri("/api/v1/auth/login")
                .body(Mono.just(loginRequest), LoginRequest.class)
                .exchange()
                .expectBody(AuthenticationResponse.class)
                .returnResult().getResponseBody();
        assertNotNull(authenticationResponse);
        assertEquals(response.getEmail(), authenticationResponse.getEmail());
        assertEquals(response.getId(), authenticationResponse.getId());
    }
    @Test
    void testSendNonExistedUserEmailToChangePassword(){
        ChangePasswordDataDTO changePasswordDataDTO = ChangePasswordDataDTO.builder()
                .email("nonExistedMail@gergwrbh.tr")
                .build();
        ErrorResponse changerUrl = webTestClient
                .post()
                .uri("/api/v1/profile/send/change/password")
                .body(Mono.just(changePasswordDataDTO), ChangePasswordDataDTO.class)
                .exchange()
                .expectBody(ErrorResponse.class)
                .returnResult().getResponseBody();
        assertNotNull(changerUrl);
        assertEquals(CodeStatus.CHANGE_FAILED.toString(), changerUrl.getError());
        assertEquals(HttpStatus.CONFLICT.value(), changerUrl.getStatusCode());

        template.exists(query(where("email").is(changePasswordDataDTO.getEmail())), ChangePasswordData.class)
                .flatMap(e -> Mono.fromRunnable(() -> assertFalse(e)))
                .as(StepVerifier::create).expectComplete().verify();
    }

    @Test
    void testFailCodeChangePassword() {
        ChangePasswordData changePasswordData = ChangePasswordData.builder()
                .email(userDTO.getEmail())
                .code(123533)
                .dateExpired(LocalDateTime.now().plusDays(1))
                .build();

        template.insert(changePasswordData).flatMap(e -> Mono.fromRunnable(() -> changeDataId = e.getId()))
                .as(StepVerifier::create).expectComplete().verify();

        ChangeRequest changeRequest = new ChangeRequest();
        changeRequest.setEmail(changePasswordData.getEmail());
        changeRequest.setKey(changeDataId);
        changeRequest.setPassword("1234");
        Stream.of("wrg3wrgwr", "3153", "wegwrhwr").forEach(code -> {
            changeRequest.setCode(code.toString());
            ErrorResponse errorResponse = webTestClient.put()
                    .uri("/api/v1/profile/change/password")
                    .header("Authorization", "Bearer " + jwt)
                    .body(Mono.just(changeRequest), ChangeRequest.class)
                    .exchange().expectBody(ErrorResponse.class).returnResult().getResponseBody();

            assertNotNull(errorResponse);
            assertEquals(CodeStatus.WRONG_CODE.toString(), errorResponse.getError());

            template.exists(query(where("id").is(changeRequest.getKey())), ChangePasswordData.class)
                    .flatMap(e -> Mono.fromRunnable(() -> assertTrue(e)))
                    .as(StepVerifier::create).expectComplete().verify();
        });
        ErrorResponse firstErrorResponse = webTestClient.put()
                .uri("/api/v1/profile/change/password")
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(changeRequest), ChangeRequest.class)
                .exchange().expectBody(ErrorResponse.class).returnResult().getResponseBody();

        assertNotNull(firstErrorResponse);
        assertEquals(CodeStatus.CHANGE_FAILED.toString(), firstErrorResponse.getError());

        changeRequest.setCode("123533");

        ErrorResponse secondErrorResponse = webTestClient.put()
                .uri("/api/v1/profile/change/password")
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(changeRequest), ChangeRequest.class)
                .exchange().expectBody(ErrorResponse.class).returnResult().getResponseBody();

        assertNotNull(secondErrorResponse);
        assertEquals(CodeStatus.CHANGE_FAILED.toString(), secondErrorResponse.getError());

        template.exists(query(where("id").is(changeDataId)), ChangePasswordData.class)
                .flatMap(e -> Mono.fromRunnable(() -> assertFalse(e)))
                .as(StepVerifier::create).expectComplete().verify();

        LoginRequest loginRequest = new LoginRequest(userDTO.getEmail(), changeRequest.getPassword());
        ErrorResponse failLogin = webTestClient
                .post()
                .uri("/api/v1/auth/login")
                .body(Mono.just(loginRequest), LoginRequest.class)
                .exchange()
                .expectBody(ErrorResponse.class)
                .returnResult().getResponseBody();
        assertNotNull(failLogin);
        assertEquals("Авторизация не удалась!", failLogin.getError());

        loginRequest.setPassword(userPassword);
        AuthenticationResponse authenticationResponse = webTestClient
                .post()
                .uri("/api/v1/auth/login")
                .body(Mono.just(loginRequest), LoginRequest.class)
                .exchange()
                .expectBody(AuthenticationResponse.class)
                .returnResult().getResponseBody();
        assertNotNull(authenticationResponse);
        assertEquals(userDTO.getEmail(), authenticationResponse.getEmail());
        assertEquals(userDTO.getId(), authenticationResponse.getId());
    }

    @Test
    void testDeleteExpiredCodeToChangePassword() {
        ChangePasswordData changePasswordData = ChangePasswordData.builder()
                .email(userDTO.getEmail())
                .code(123533)
                .dateExpired(LocalDateTime.now().minusMinutes(30))
                .build();

        template.insert(changePasswordData).flatMap(e -> Mono.fromRunnable(() -> changeDataId = e.getId()))
                .as(StepVerifier::create).expectComplete().verify();

        ChangeRequest changeRequest = new ChangeRequest();
        changeRequest.setEmail(changePasswordData.getEmail());
        changeRequest.setKey(changeDataId);
        changeRequest.setPassword("1234");
        changeRequest.setCode(changePasswordData.getCode().toString());

        ErrorResponse errorResponse = webTestClient.put()
                .uri("/api/v1/profile/change/password")
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(changeRequest), ChangeRequest.class)
                .exchange().expectBody(ErrorResponse.class).returnResult().getResponseBody();

        assertNotNull(errorResponse);
        assertEquals(CodeStatus.CHANGE_FAILED.toString(), errorResponse.getError());

        template.exists(query(where("id").is(changeRequest.getKey())), ChangePasswordData.class)
                .flatMap(e -> Mono.fromRunnable(() -> assertFalse(e)))
                .as(StepVerifier::create).expectComplete().verify();
    }

    @Test
    void testGetUsersInfoByAdmin() {
        List<UserDTO> users = webTestClient
                .get()
                .uri("/api/v1/profile/get/users")
                .header("Authorization", "Bearer " + jwt)
                .exchange()
                .expectBodyList(UserDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(users);
        boolean isContained = users.stream().anyMatch(user ->
                user.getEmail().equals(userDTO.getEmail())
                        && user.getLastName().equals(userDTO.getLastName())
                        && user.getId().equals(userDTO.getId())
                        && user.getRoles().equals(userDTO.getRoles())
        );
        assertTrue(isContained);
    }

    @Test
    void testGetUsersEmailsByAdmin() {
        List<String> emails = webTestClient
                .get()
                .uri("/api/v1/profile/get/emails")
                .header("Authorization", "Bearer " + jwt)
                .exchange()
                .expectBodyList(String.class)
                .returnResult().getResponseBody();
        assertNotNull(emails);
        boolean isContained = emails.stream().anyMatch(email -> email.equals("account.change@gmail.com"));
        assertTrue(isContained);
    }
}
