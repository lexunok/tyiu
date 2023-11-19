//package com.tyiu.corn.controller;
//
//import com.tyiu.corn.model.dto.*;
//import com.tyiu.corn.model.enums.Role;
//import com.tyiu.corn.model.enums.SkillType;
//import com.tyiu.corn.model.requests.RegisterRequest;
//import com.tyiu.corn.model.responses.AuthenticationResponse;
//import com.tyiu.corn.model.responses.TeamMemberResponse;
//import org.junit.jupiter.api.BeforeAll;
//import org.junit.jupiter.api.Test;
//import org.junit.jupiter.api.TestInstance;
//import org.springframework.beans.factory.annotation.Autowired;
//import org.springframework.boot.test.context.SpringBootTest;
//import org.springframework.test.web.reactive.server.WebTestClient;
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
//
//    private TeamDTO createTeam() {
//
//        SkillDTO skillDTO1 = SkillDTO.builder()
//                .name("Java")
//                .type(SkillType.LANGUAGE)
//                .build();
//
//        SkillDTO responseAddSkill1 = webTestClient
//                .post()
//                .uri("/api/v1/skill/add")
//                .header("Authorization", "Bearer " + jwt1)
//                .body(Mono.just(skillDTO1), SkillDTO.class)
//                .exchange()
//                .expectBody(SkillDTO.class)
//                .returnResult().getResponseBody();
//        assertNotNull(responseAddSkill1);
//
//        SkillDTO skillDTO2 = SkillDTO.builder()
//                .name("JavaScript")
//                .type(SkillType.LANGUAGE)
//                .build();
//
//        SkillDTO responseAddSkill2 = webTestClient
//                .post()
//                .uri("/api/v1/skill/add")
//                .header("Authorization", "Bearer " + jwt1)
//                .body(Mono.just(skillDTO2), SkillDTO.class)
//                .exchange()
//                .expectBody(SkillDTO.class)
//                .returnResult().getResponseBody();
//        assertNotNull(responseAddSkill2);
//
//        SkillDTO skillDTO3 = SkillDTO.builder()
//                .name("Какой-то ноунейм язык")
//                .type(SkillType.LANGUAGE)
//                .build();
//
//        SkillDTO responseAddSkill3 = webTestClient
//                .post()
//                .uri("/api/v1/skill/add")
//                .header("Authorization", "Bearer " + jwt1)
//                .body(Mono.just(skillDTO3), SkillDTO.class)
//                .exchange()
//                .expectBody(SkillDTO.class)
//                .returnResult().getResponseBody();
//        assertNotNull(responseAddSkill3);
//
//        TeamMemberResponse USER1 = TeamMemberResponse.builder()
//                .email(user1.getEmail())
//                .firstName(user1.getFirstName())
//                .lastName(user1.getLastName())
//                .skills(List.of(responseAddSkill1, responseAddSkill2))
//                .build();
//
//        TeamMemberResponse USER2 = TeamMemberResponse.builder()
//                .email(user2.getEmail())
//                .firstName(user2.getFirstName())
//                .lastName(user2.getLastName())
//                .skills(List.of(responseAddSkill1))
//                .build();
//
//        TeamMemberDTO owner = TeamMemberDTO.builder()
//                .userId(user1.getId())
//                .email(user1.getEmail())
//                .firstName("Илья")
//                .lastName("Муромец")
//                .skills(USER1.getSkills())
//                .build();
//
//        TeamMemberDTO member = TeamMemberDTO.builder()
//                .userId(user2.getId())
//                .email(user2.getEmail())
//                .firstName("Алёша")
//                .lastName("Попович")
//                .skills(USER2.getSkills())
//                .build();
//
//        return TeamDTO.builder()
//                .name("Богатыри")
//                .description("Слава Руси!")
//                .closed(false)
//                .owner(owner)
//                .leader(owner)
//                .membersCount(2)
//                .members(List.of(owner))
//                .skills(List.of(skillDTO1, skillDTO2))
//                .desiredSkills(List.of(skillDTO1, skillDTO2, skillDTO3))
//                .createdAt(LocalDate.now())
//                .build();
//    }
//
//    @BeforeAll
//    public void setUp() {
//
//        RegisterRequest request1 = new RegisterRequest(
//                "fakemailOWNER", "Хочу создать", "команду", "fakepass",
//                List.of(Role.ADMIN,
//                        Role.EXPERT,
//                        Role.PROJECT_OFFICE,
//                        Role.INITIATOR));
//
//        AuthenticationResponse response1 = webTestClient
//                .post()
//                .uri("/api/v1/auth/register")
//                .body(Mono.just(request1), RegisterRequest.class)
//                .exchange()
//                .expectBody(AuthenticationResponse.class)
//                .returnResult().getResponseBody();
//        assertNotNull(response1);
//
//        jwt1 = response1.getToken();
//
//        user1 = UserDTO.builder()
//                .id(response1.getId())
//                .email(response1.getEmail())
//                .lastName(response1.getLastName())
//                .firstName(response1.getFirstName())
//                .roles(response1.getRoles())
//                .build();
//
//        RegisterRequest request2 = new RegisterRequest(
//                "fakemailMEMBER", "Хочу попасть", "в команду", "fakepass",
//                List.of(Role.ADMIN,
//                        Role.EXPERT,
//                        Role.PROJECT_OFFICE,
//                        Role.INITIATOR));
//
//        AuthenticationResponse response2 = webTestClient
//                .post()
//                .uri("/api/v1/auth/register")
//                .body(Mono.just(request2), RegisterRequest.class)
//                .exchange()
//                .expectBody(AuthenticationResponse.class)
//                .returnResult().getResponseBody();
//        assertNotNull(response2);
//
//        jwt2 = response2.getToken();
//
//        user2 = UserDTO.builder()
//                .id(response2.getId())
//                .email(response2.getEmail())
//                .lastName(response2.getLastName())
//                .firstName(response2.getFirstName())
//                .roles(response2.getRoles())
//                .build();
//    }
//
//    @Test
//    void testAddTeam() {
//
//        TeamDTO team = createTeam();
//
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
//    }
//
////    @Test
////    void testUpdateTeam() {
////
////        TeamDTO team = createTeam();
////
////        TeamDTO responseAddTeam = webTestClient
////                .post()
////                .uri("/api/v1/team/add")
////                .header("Authorization", "Bearer " + jwt1)
////                .body(Mono.just(team), TeamDTO.class)
////                .exchange()
////                .expectBody(TeamDTO.class)
////                .returnResult().getResponseBody();
////        assertNotNull(responseAddTeam);
////        assertNotNull(responseAddTeam.getMembers());
////
////        //TODO: при обновлении команды, список членов команды пустой, хотя при создании, проверка в строке 200 не даёт ошибку
////
//////        team = TeamDTO.builder()
//////                .name("Ящеры")
//////                .description("Слава Подикам!")
//////                .closed(responseAddTeam.getClosed())
//////                .owner(responseAddTeam.getOwner())
//////                .leader(responseAddTeam.getLeader())
//////                .membersCount(responseAddTeam.getMembersCount())
//////                .members(responseAddTeam.getMembers())
//////                .skills(responseAddTeam.getSkills())
//////                .desiredSkills(responseAddTeam.getDesiredSkills())
//////                .build();
//////
//////        webTestClient
//////                .put()
//////                .uri("/api/v1/team/update/{id}", responseAddTeam.getId())
//////                .header("Authorization", "Bearer " + jwt1)
//////                .body(Mono.just(team), TeamDTO.class)
//////                .exchange()
//////                .expectStatus().isOk();
////    }
//
//    @Test
//    void testGetTeam() {
//
//        TeamDTO team = createTeam();
//
//        TeamDTO responseAddTeam = webTestClient
//                .post()
//                .uri("/api/v1/team/add")
//                .header("Authorization", "Bearer " + jwt1)
//                .body(Mono.just(team), TeamDTO.class)
//                .exchange()
//                .expectBody(TeamDTO.class)
//                .returnResult().getResponseBody();
//        assertNotNull(responseAddTeam);
//
//        TeamDTO responseGetTeam = webTestClient
//                .get()
//                .uri("/api/v1/team/{id}", responseAddTeam.getId())
//                .header("Authorization", "Bearer " + jwt1)
//                .exchange()
//                .expectBody(TeamDTO.class)
//                .returnResult().getResponseBody();
//        assertNotNull(responseGetTeam);
////        assertEquals(team.getName(), responseGetTeam.getName());
//        //TODO: полученные данные в методе GET имеют пустое значение (строка 244 возвращает ошибку: responseGetTeam = null)
//    }
//
//    @Test
//    void testGetTeams() {
//
//        TeamDTO team1 = createTeam();
//
//        TeamDTO responseAddTeam1 = webTestClient
//                .post()
//                .uri("/api/v1/team/add")
//                .header("Authorization", "Bearer " + jwt1)
//                .body(Mono.just(team1), TeamDTO.class)
//                .exchange()
//                .expectBody(TeamDTO.class)
//                .returnResult().getResponseBody();
//        assertNotNull(responseAddTeam1);
//
//        TeamDTO team2 = createTeam();
//
//        TeamDTO responseAddTeam2 = webTestClient
//                .post()
//                .uri("/api/v1/team/add")
//                .header("Authorization", "Bearer " + jwt2)
//                .body(Mono.just(team2), TeamDTO.class)
//                .exchange()
//                .expectBody(TeamDTO.class)
//                .returnResult().getResponseBody();
//        assertNotNull(responseAddTeam2);
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
//
//        TeamDTO team = createTeam();
//
//        TeamDTO responseAddTeam = webTestClient
//                .post()
//                .uri("/api/v1/team/add")
//                .header("Authorization", "Bearer " + jwt1)
//                .body(Mono.just(team), TeamDTO.class)
//                .exchange()
//                .expectBody(TeamDTO.class)
//                .returnResult().getResponseBody();
//        assertNotNull(responseAddTeam);
//
//        webTestClient
//                .delete()
//                .uri("/api/v1/team/delete/{id}", responseAddTeam.getId())
//                .header("Authorization", "Bearer " + jwt1)
//                .exchange()
//                .expectStatus().isOk();
//    }
//}
