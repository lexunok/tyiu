//package com.tyiu.corn.controller;
//
//import com.tyiu.corn.model.dto.*;
//import com.tyiu.corn.model.enums.Role;
//import com.tyiu.corn.model.enums.SkillType;
//import com.tyiu.corn.model.requests.RegisterRequest;
//import com.tyiu.corn.model.responses.AuthenticationResponse;
//import org.junit.jupiter.api.BeforeAll;
//import org.junit.jupiter.api.Test;
//import org.junit.jupiter.api.TestInstance;
//import org.springframework.beans.factory.annotation.Autowired;
//import org.springframework.boot.test.context.SpringBootTest;
//import org.springframework.test.web.reactive.server.WebTestClient;
//import reactor.core.publisher.Flux;
//import reactor.core.publisher.Mono;
//
//import java.time.LocalDate;
//import java.util.List;
//
//import static org.junit.jupiter.api.Assertions.*;
//import static org.springframework.boot.test.context.SpringBootTest.WebEnvironment.RANDOM_PORT;
//
//@TestInstance(TestInstance.Lifecycle.PER_CLASS)
//@SpringBootTest(webEnvironment = RANDOM_PORT)
//public class TeamControllerTest extends TestContainers {
//
//    @Autowired
//    private WebTestClient webTestClient;
//    private UserDTO user1;
//    private UserDTO user2;
//    private String jwt1;
//    private String jwt2;
//    private SkillDTO skill1;
//    private SkillDTO skill2;
//    private SkillDTO skill3;
//
//    private TeamDTO createTeam() {
//        TeamDTO team = TeamDTO.builder()
//                .name("Богатыри")
//                .description("Слава Руси!")
//                .closed(false)
//                .owner(user1)
//                .leader(user2)
//                .membersCount(List.of(user2).size())
//                .members(List.of(user2))
//                .skills(List.of(skill1, skill2))
//                .desiredSkills(List.of(skill1, skill2, skill3))
//                .createdAt(LocalDate.now())
//                .build();
//        TeamDTO responseAddTeam = webTestClient
//                .post()
//                .uri("/api/v1/team/add")
//                .header("Authorization", "Bearer " + jwt1)
//                .body(Mono.just(team), TeamDTO.class)
//                .exchange()
//                .expectBody(TeamDTO.class)
//                .returnResult().getResponseBody();
//        assertNotNull(responseAddTeam);
//        assertEquals(team.getName(), responseAddTeam.getName());
//        return responseAddTeam;
//    }
//
//    private SkillDTO createSkill(String name){
//        SkillDTO skillDTO = SkillDTO.builder()
//                .name(name)
//                .type(SkillType.LANGUAGE)
//                .build();
//
//        SkillDTO createdSkill = webTestClient
//                .post()
//                .uri("/api/v1/skill/add")
//                .header("Authorization", "Bearer " + jwt1)
//                .body(Mono.just(skillDTO), SkillDTO.class)
//                .exchange()
//                .expectBody(SkillDTO.class)
//                .returnResult().getResponseBody();
//        assertNotNull(createdSkill);
//        return createdSkill;
//    }
//
//    private TeamDTO getTeam(String id, String name){
//        TeamDTO responseGetTeam = webTestClient
//                .get()
//                .uri("/api/v1/team/{id}", id)
//                .header("Authorization", "Bearer " + jwt1)
//                .exchange()
//                .expectBody(TeamDTO.class)
//                .returnResult().getResponseBody();
//        assertNotNull(responseGetTeam);
//        assertEquals(name, responseGetTeam.getName());
//        return responseGetTeam;
//    }
//
//    private AuthenticationResponse register(String email, String lastName, String firstName, String password){
//        RegisterRequest request = new RegisterRequest(
//                email, lastName, firstName, password,
//                List.of(Role.ADMIN,
//                        Role.EXPERT,
//                        Role.PROJECT_OFFICE,
//                        Role.INITIATOR));
//        AuthenticationResponse response = webTestClient
//                .post()
//                .uri("/api/v1/auth/register")
//                .body(Mono.just(request), RegisterRequest.class)
//                .exchange()
//                .expectBody(AuthenticationResponse.class)
//                .returnResult().getResponseBody();
//        assertNotNull(response);
//        return response;
//    }
//
//    @BeforeAll
//    public void setUp() {
//        AuthenticationResponse response1 = register("team.test@gmail.com", "team", "test", "team-test");
//        jwt1 = response1.getToken();
//
//        AuthenticationResponse response2 = register("test.team@gmail.com", "test", "team", "test-team");
//        jwt2 = response2.getToken();
//
//        user1 = UserDTO.builder()
//                .id(response1.getId())
//                .email(response1.getEmail())
//                .lastName(response1.getLastName())
//                .firstName(response1.getFirstName())
//                .roles(response1.getRoles())
//                .build();
//
//        user2 = UserDTO.builder()
//                .id(response2.getId())
//                .email(response2.getEmail())
//                .lastName(response2.getLastName())
//                .firstName(response2.getFirstName())
//                .roles(response2.getRoles())
//                .build();
//        skill1 = createSkill("Java");
//        skill2 = createSkill("JavaScript");
//        skill3 = createSkill("Какой-то ноунейм язык");
//    }
//
//    @Test
//    void testAddTeam() {
//        createTeam();
//    }
//
//    @Test
//    void testUpdateTeam() {
//        String id = createTeam().getId();
//        TeamDTO Team = getTeam(id, "Богатыри");
//        assertEquals(1, Team.getMembers().size());
//
//        webTestClient
//                .post()
//                .uri("/api/v1/profile/skills/save")
//                .header("Authorization", "Bearer " + jwt1)
//                .body(Flux.just(skill1,skill2), SkillDTO.class)
//                .exchange()
//                .expectBodyList(SkillDTO.class)
//                .returnResult().getResponseBody();
//
//        webTestClient
//                .post()
//                .uri("/api/v1/profile/skills/save")
//                .header("Authorization", "Bearer " + jwt2)
//                .body(Flux.just(skill2, skill3), SkillDTO.class)
//                .exchange()
//                .expectBodyList(SkillDTO.class)
//                .returnResult().getResponseBody();
//
//        TeamDTO teamDTO = TeamDTO.builder()
//                .id(id)
//                .name("Ящеры")
//                .description("Слава Подикам!")
//                .closed(Team.getClosed())
//                .createdAt(Team.getCreatedAt())
//                .owner(Team.getOwner())
//                .leader(Team.getLeader())
//                .membersCount(Team.getMembersCount())
//                .members(List.of(user1,user2))
//                .skills(Team.getSkills())
//                .desiredSkills(Team.getDesiredSkills())
//                .build();
//
//        TeamDTO updatedTeam = webTestClient
//                .put()
//                .uri("/api/v1/team/update/{id}", id)
//                .header("Authorization", "Bearer " + jwt1)
//                .body(Mono.just(teamDTO), TeamDTO.class)
//                .exchange()
//                .expectBody(TeamDTO.class)
//                .returnResult().getResponseBody();
//        assertNotNull(updatedTeam);
//        assertEquals(teamDTO.getName(), updatedTeam.getName());
//        assertEquals(2, getTeam(id, updatedTeam.getName()).getMembers().size());
//
//        TeamDTO teamAgainDTO = TeamDTO.builder()
//                .id(id)
//                .name("Ящеры")
//                .description("Слава Подикам!")
//                .closed(Team.getClosed())
//                .createdAt(Team.getCreatedAt())
//                .owner(Team.getOwner())
//                .leader(Team.getLeader())
//                .membersCount(Team.getMembersCount())
//                .members(List.of(user1))
//                .skills(Team.getSkills())
//                .desiredSkills(Team.getDesiredSkills())
//                .build();
//
//        TeamDTO updatedAgainTeam = webTestClient
//                .put()
//                .uri("/api/v1/team/update/{id}", id)
//                .header("Authorization", "Bearer " + jwt1)
//                .body(Mono.just(teamAgainDTO), TeamDTO.class)
//                .exchange()
//                .expectBody(TeamDTO.class)
//                .returnResult().getResponseBody();
//        assertNotNull(updatedAgainTeam);
//        assertEquals(teamAgainDTO.getName(), updatedAgainTeam.getName());
//        assertEquals(1, getTeam(id, updatedAgainTeam.getName()).getMembers().size());
//    }
//
//    @Test
//    void testGetTeam() {
//        getTeam(createTeam().getId(),"Богатыри");
//    }
//
//    @Test
//    void testGetTeams() {
//        createTeam();
//        createTeam();
//
//        List<TeamDTO> allTeams = webTestClient
//                .get()
//                .uri("api/v1/team/all")
//                .header("Authorization", "Bearer " + jwt1)
//                .exchange()
//                .expectBodyList(TeamDTO.class)
//                .returnResult().getResponseBody();
//        assertNotNull(allTeams);
//        assertTrue(allTeams.size() > 1);
//    }
//
//    @Test
//    void testDeleteTeam() {
//        webTestClient
//                .delete()
//                .uri("/api/v1/team/delete/{id}", createTeam().getId())
//                .header("Authorization", "Bearer " + jwt1)
//                .exchange()
//                .expectStatus().isOk();
//    }
//}
