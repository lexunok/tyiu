package com.tyiu.ideas.notification;

import com.tyiu.ideas.model.dto.*;
import com.tyiu.ideas.model.enums.*;
import com.tyiu.ideas.model.entities.*;
import com.tyiu.ideas.model.requests.*;
import com.tyiu.ideas.model.responses.*;
import com.tyiu.ideas.service.TeamService;

import enums.NotificationCase;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.TestInstance;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.beans.factory.annotation.Autowired;

import reactor.test.StepVerifier;
import reactor.core.publisher.Mono;
import reactor.core.publisher.Flux;
import com.tyiu.ideas.TestContainers;
import org.springframework.test.web.reactive.server.WebTestClient;

import java.time.*;
import java.util.*;

import static org.mockito.Mockito.*;
import static org.junit.jupiter.api.Assertions.*;
import static org.springframework.boot.test.context.SpringBootTest.WebEnvironment.RANDOM_PORT;

@SpringBootTest(webEnvironment = RANDOM_PORT)
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
public class TeamNotificationTest extends TestContainers {

    private String jwt_teamOwner;
    private UserDTO teamOwner;
    private User authTeamOwner;

    private String jwt_teamLeader;
    private UserDTO teamLeader;
    private User authTeamLeader;

    private String jwt_teamMember;
    private UserDTO teamMember;
    private User authTeamMember;

    private String jwt_randomUser;
    private UserDTO randomUser;
    private User authRandomUser;

    private String jwt_admin;
    private UserDTO admin;
    private User authAdmin;

    @Autowired
    private WebTestClient webTestClient;
    private final TeamService teamService = mock(TeamService.class);

    private AuthenticationResponse register(String email, String firstName, String lastName, List<Role> roles) {

        AuthenticationResponse response = webTestClient
                .post()
                .uri("/api/v1/ideas-service/auth/register")
                .body(Mono.just(new RegisterRequest(email, firstName, lastName, "bla-bla-bla", roles)), RegisterRequest.class)
                .exchange()
                .expectBody(AuthenticationResponse.class)
                .returnResult().getResponseBody();
        assertNotNull(response);

        return response;
    }

    private UserDTO userBuild(AuthenticationResponse response) {

        return UserDTO.builder()
                .id(response.getId())
                .email(response.getEmail())
                .firstName(response.getFirstName())
                .lastName(response.getLastName())
                .roles(response.getRoles())
                .build();
    }

    private User getAuthenticationPrincipal(AuthenticationResponse response) {

        return User.builder()
                .id(response.getId())
                .email(response.getEmail())
                .firstName(response.getFirstName())
                .lastName(response.getLastName())
                .roles(response.getRoles())
                .build();
    }

    private TeamDTO addTeam() {

        TeamDTO teamDTO = TeamDTO.builder()
                .name("name")
                .description("description")
                .closed(false)
                .membersCount(2)
                .createdAt(LocalDate.now())
                .owner(teamOwner)
                .leader(teamLeader)
                .members(List.of(teamLeader, teamMember))
                .wantedSkills(List.of())
                .build();

        TeamDTO responseAddTeam = webTestClient
                .post()
                .uri("/api/v1/ideas-service/team/add")
                .header("Authorization", "Bearer " + jwt_teamOwner)
                .body(Mono.just(teamDTO), TeamDTO.class)
                .exchange()
                .expectBody(TeamDTO.class)
                .returnResult()
                .getResponseBody();
        assertNotNull(responseAddTeam);
        assertEquals(teamDTO.getName(), responseAddTeam.getName());

        return responseAddTeam;
    }

    @BeforeAll
    void setUp() {

        AuthenticationResponse response1 = register("team-owner@gmail.com", "Owner", "Ownerov", List.of(Role.TEAM_OWNER));
        jwt_teamOwner = response1.getToken();
        teamOwner = userBuild(response1);
        authTeamOwner = getAuthenticationPrincipal(response1);

        AuthenticationResponse response2 = register("team-leader@gmail.com", "Leader", "leaderov", List.of(Role.TEAM_LEADER));
        jwt_teamLeader = response2.getToken();
        teamLeader = userBuild(response2);
        authTeamLeader = getAuthenticationPrincipal(response2);

        AuthenticationResponse response3 = register("team-member@gmail.com", "Member", "Memberov", List.of(Role.MEMBER));
        jwt_teamMember = response3.getToken();
        teamMember = userBuild(response3);
        authTeamMember = getAuthenticationPrincipal(response3);

        AuthenticationResponse response4 = register("random-user@gmail.com", "Random", "User", List.of(Role.MEMBER));
        jwt_randomUser = response4.getToken();
        randomUser = userBuild(response4);
        authRandomUser = getAuthenticationPrincipal(response4);

        AuthenticationResponse response5 = register("admin@gmail.com", "Admin", "Adminov", List.of(Role.ADMIN));
        jwt_admin = response5.getToken();
        admin = userBuild(response5);
        authAdmin = getAuthenticationPrincipal(response5);

        webTestClient = webTestClient.mutate()
                .responseTimeout(Duration.ofMillis(90000))
                .build();
    }

    @Test
    void testTeamOwnerInvitesUser() {

        when(teamService.sendNotification(anyString(), anyString(), anyString(), any(NotificationCase.class))).thenReturn(Mono.empty());

        TeamDTO team = addTeam();

        TeamInvitation teamInvitation = TeamInvitation.builder()
                .teamId(team.getId())
                .userId(randomUser.getId())
                .email(randomUser.getEmail())
                .firstName(randomUser.getFirstName())
                .lastName(randomUser.getLastName())
                .status(RequestStatus.NEW)
                .build();

        teamService.sendInvitesToUsers(Flux.just(teamInvitation), authTeamOwner);
        StepVerifier.create(
                teamService.sendNotification(
                        teamInvitation.getTeamId(),
                        authTeamOwner.getId(),
                        teamInvitation.getUserId(),
                        NotificationCase.TEAM_OWNER_INVITES_USER)
        ).verifyComplete();

//        teamService.sendInvitesToUsers(Flux.just(teamInvitation), authRandomUser);
//        StepVerifier.create(
//                teamService.sendNotification(
//                        teamInvitation.getTeamId(),
//                        authTeamOwner.getId(),
//                        teamInvitation.getUserId(),
//                        NotificationCase.TEAM_OWNER_INVITES_USER)
//        ).verifyError(); TODO: ДОЛЖНО ВОЗВРАЩАТЬ ОШИБКУ
    }
}
