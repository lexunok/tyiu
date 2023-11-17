package com.tyiu.corn.controller;

import com.tyiu.corn.model.dto.*;
import com.tyiu.corn.model.enums.*;
import com.tyiu.corn.model.requests.IdeaSkillRequest;
import com.tyiu.corn.model.requests.RegisterRequest;
import com.tyiu.corn.model.responses.AuthenticationResponse;
import com.tyiu.corn.model.responses.InfoResponse;
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
public class IdeaMarketControllerTest extends TestContainers {

    @Autowired
    private WebTestClient webTestClient;

    private String jwt;
    private UserDTO userDTO;
    private GroupDTO groupExpert;
    private GroupDTO groupProjectOffice;
    private SkillDTO skill1;
    private SkillDTO skill2;

    private IdeaMarketDTO createMarketIdea(){
        IdeaDTO ideaDTO1 = IdeaDTO.builder()
                .initiatorEmail(userDTO.getEmail())
                .name("idea1")
                .status(StatusIdea.ON_APPROVAL)
                .experts(groupExpert)
                .projectOffice(groupProjectOffice)
                .problem("Отсутствия готовых решений задач")
                .solution("Форум, где студенты могут оставить свои решения")
                .result("Удобная онлайн платформа")
                .customer("Студенты")
                .contactPerson("Стас")
                .description("Для студентов!")
                .maxTeamSize((short) 5)
                .build();
        IdeaDTO createdIdea1 = webTestClient
                .post()
                .uri("/api/v1/idea/add")
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(ideaDTO1), IdeaDTO.class)
                .exchange()
                .expectBody(IdeaDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(createdIdea1);
        assertEquals(ideaDTO1.getName(), createdIdea1.getName());
        IdeaDTO idea1 = webTestClient
                .get()
                .uri("/api/v1/idea/{ideaId}", createdIdea1.getId())
                .header("Authorization", "Bearer " + jwt)
                .exchange()
                .expectBody(IdeaDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(idea1);
        assertEquals(ideaDTO1.getName(), idea1.getName());
        IdeaDTO ideaDTO2 = IdeaDTO.builder()
                .initiatorEmail(userDTO.getEmail())
                .name("idea1")
                .status(StatusIdea.ON_APPROVAL)
                .experts(groupExpert)
                .projectOffice(groupProjectOffice)
                .problem("Отсутствия готовых решений задач")
                .solution("Форум, где студенты могут оставить свои решения")
                .result("Удобная онлайн платформа")
                .customer("Студенты")
                .contactPerson("Стас")
                .description("Для студентов!")
                .maxTeamSize((short) 5)
                .build();
        IdeaDTO createdIdea2 = webTestClient
                .post()
                .uri("/api/v1/idea/add")
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(ideaDTO2), IdeaDTO.class)
                .exchange()
                .expectBody(IdeaDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(createdIdea2);
        assertEquals(ideaDTO2.getName(), createdIdea2.getName());
        IdeaDTO idea2 = webTestClient
                .get()
                .uri("/api/v1/idea/{ideaId}", createdIdea2.getId())
                .header("Authorization", "Bearer " + jwt)
                .exchange()
                .expectBody(IdeaDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(idea2);
        assertEquals(ideaDTO2.getName(), idea2.getName());
        IdeaSkillRequest ideaSkillRequest1 = IdeaSkillRequest.builder()
                .ideaId(createdIdea1.getId())
                .skills(List.of(skill1,skill2))
                .build();
        InfoResponse skillRequest1 = webTestClient
                .post()
                .uri("/api/v1/idea/skills/add")
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(ideaSkillRequest1), IdeaSkillRequest.class)
                .exchange()
                .expectBody(InfoResponse.class)
                .returnResult().getResponseBody();
        assertNotNull(skillRequest1);
        assertEquals("Success!", skillRequest1.getMessage());
        IdeaSkillRequest ideaSkillRequest2 = IdeaSkillRequest.builder()
                .ideaId(createdIdea2.getId())
                .skills(List.of(skill1,skill2))
                .build();
        InfoResponse skillRequest2 = webTestClient
                .post()
                .uri("/api/v1/idea/skills/add")
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(ideaSkillRequest2), IdeaSkillRequest.class)
                .exchange()
                .expectBody(InfoResponse.class)
                .returnResult().getResponseBody();
        assertNotNull(skillRequest2);
        assertEquals("Success!", skillRequest2.getMessage());
        List<IdeaMarketDTO> createdMarketIdea = webTestClient
                .post()
                .uri("/api/v1/market/send")
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(List.of(idea1, idea2)), IdeaDTO.class)
                .exchange()
                .expectBodyList(IdeaMarketDTO.class)
                .returnResult().getResponseBody();
        IdeaMarketDTO ideaMarketDTO = createdMarketIdea.get(0);
        assertNotNull(ideaMarketDTO);
        assertEquals(ideaDTO1.getName(), ideaMarketDTO.getName());
        return ideaMarketDTO;
    }

    private IdeaMarketDTO getMarketIdea(String ideaMarketId){
        IdeaMarketDTO responseBody = webTestClient
                .get()
                .uri("/api/v1/market/{ideaMarketId}", ideaMarketId)
                .header("Authorization", "Bearer " + jwt)
                .exchange()
                .expectBody(IdeaMarketDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(responseBody);
        assertEquals("idea1", responseBody.getName());
        return responseBody;
    }

    private TeamMarketRequestDTO createMarketTeamRequest(String ideaMarketId){
        TeamMemberDTO teamMemberDTO = TeamMemberDTO.builder()
                .userId(userDTO.getId())
                .email(userDTO.getEmail())
                .firstName(userDTO.getFirstName())
                .lastName(userDTO.getLastName())
                .build();
        TeamDTO teamDTO = TeamDTO.builder()
                .name("name")
                .description("description")
                .closed(false)
                .owner(teamMemberDTO)
                .leader(teamMemberDTO)
                .members(List.of(teamMemberDTO))
                .skills(List.of(skill1,skill2))
                .build();
        TeamDTO createdTeam = webTestClient
                .post()
                .uri("/api/v1/team/add")
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(teamDTO), TeamDTO.class)
                .exchange()
                .expectBody(TeamDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(createdTeam);
        assertEquals(teamDTO.getName(),createdTeam.getName());
        TeamMarketRequestDTO teamMarketRequest = TeamMarketRequestDTO.builder()
                .ideaMarketId(ideaMarketId)
                .teamId(createdTeam.getId())
                .accepted(false)
                .name(createdTeam.getName())
                .closed(createdTeam.getClosed())
                .description(createdTeam.getDescription())
                .owner(teamMemberDTO)
                .leader(teamMemberDTO)
                .members(List.of(teamMemberDTO))
                .skills(List.of(skill1,skill2))
                .letter("letter")
                .build();
        TeamMarketRequestDTO createdTeamMarketRequest = webTestClient
                .post()
                .uri("/api/v1/market/declare")
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(teamMarketRequest), TeamMarketRequestDTO.class)
                .exchange()
                .expectBody(TeamMarketRequestDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(createdTeamMarketRequest);
        assertEquals(teamMarketRequest.getName(),createdTeamMarketRequest.getName());
        return createdTeamMarketRequest;
    }

    private void makeFavorite(String ideaMarketId){
        webTestClient
                .post()
                .uri("/api/v1/market/favorite/{ideaMarketId}", ideaMarketId)
                .header("Authorization", "Bearer " + jwt)
                .exchange();
        assertEquals(true, getMarketIdea(ideaMarketId).getIsFavorite());
    }

    @BeforeAll
    public void setUp() {
        RegisterRequest request = new RegisterRequest(
                "fakemail104", "fakename104", "fakename104", "fakepass104",
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

        GroupDTO experts = GroupDTO.builder()
                .name("exp")
                .users(List.of(userDTO))
                .roles(List.of(Role.EXPERT))
                .build();
        groupExpert = webTestClient
                .post()
                .uri("/api/v1/group/create")
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(experts), GroupDTO.class)
                .exchange()
                .expectBody(GroupDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(groupExpert);
        assertEquals(experts.getName(), groupExpert.getName());
        GroupDTO projectOffice = GroupDTO.builder()
                .name("pro")
                .users(List.of(userDTO))
                .roles(List.of(Role.PROJECT_OFFICE))
                .build();
        groupProjectOffice = webTestClient
                .post()
                .uri("/api/v1/group/create")
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(projectOffice), GroupDTO.class)
                .exchange()
                .expectBody(GroupDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(groupProjectOffice);
        assertEquals(projectOffice.getName(), groupProjectOffice.getName());
        SkillDTO skillDTO1 = SkillDTO.builder()
                .name("skill1")
                .type(SkillType.LANGUAGE)
                .build();
        skill1 = webTestClient
                .post()
                .uri("/api/v1/skill/add")
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(skillDTO1), SkillDTO.class)
                .exchange()
                .expectBody(SkillDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(skill1);
        assertEquals(skillDTO1.getName(), skill1.getName());
        SkillDTO skillDTO2 = SkillDTO.builder()
                .name("skill2")
                .type(SkillType.LANGUAGE)
                .build();
        skill2 = webTestClient
                .post()
                .uri("/api/v1/skill/add")
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(skillDTO2), SkillDTO.class)
                .exchange()
                .expectBody(SkillDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(skill2);
        assertEquals(skillDTO2.getName(), skill2.getName());
    }

    ///////////////////////
    //  _____   ____ ______
    // / ___/  / __//_  __/
    /// (_ /  / _/   / /
    //\___/  /___/  /_/
    ///////////////////////

    @Test
    void testGetAllMarketIdeas() {
        String ideaId = createMarketIdea().getId();
        createMarketTeamRequest(ideaId);
        createMarketTeamRequest(ideaId);
        List<IdeaMarketDTO> marketIdeas = webTestClient
                .get()
                .uri("/api/v1/market/all")
                .header("Authorization", "Bearer " + jwt)
                .exchange()
                .expectBodyList(IdeaMarketDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(marketIdeas);
        assertEquals(34, marketIdeas.size());
    }

    @Test
    void testGetAllInitiatorMarketIdeas() {
        String ideaId = createMarketIdea().getId();
        createMarketTeamRequest(ideaId);
        createMarketTeamRequest(ideaId);
        List<IdeaMarketDTO> marketIdeas = webTestClient
                .get()
                .uri("/api/v1/market/initiator/all")
                .header("Authorization", "Bearer " + jwt)
                .exchange()
                .expectBodyList(IdeaMarketDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(marketIdeas);
        assertEquals(38, marketIdeas.size());
    }

    @Test
    void testGetMarketIdea() {
        getMarketIdea(createMarketIdea().getId());
    }

    @Test
    void testGetAllFavoriteMarketIdeas() {
        makeFavorite(createMarketIdea().getId());
        List<IdeaMarketDTO> marketFavoriteIdeas = webTestClient
                .get()
                .uri("/api/v1/market/favorite")
                .header("Authorization", "Bearer " + jwt)
                .exchange()
                .expectBodyList(IdeaMarketDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(marketFavoriteIdeas);
        assertEquals(2, marketFavoriteIdeas.size());
    }

    @Test
    void testGetAllTeamMarketRequests() {
        String ideaMarketId = createMarketIdea().getId();
        createMarketTeamRequest(ideaMarketId);
        List<TeamMarketRequestDTO> marketTeamsRequests =  webTestClient
                .get()
                .uri("/api/v1/market/requests/", ideaMarketId)
                .header("Authorization", "Bearer " + jwt)
                .exchange()
                .expectBodyList(TeamMarketRequestDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(marketTeamsRequests);
        assertEquals(1, marketTeamsRequests.size());
    }

    //////////////////////////////
    //   ___   ____    ____ ______
    //  / _ \ / __ \  / __//_  __/
    // / ___// /_/ / _\ \   / /
    ///_/    \____/ /___/  /_/
    //////////////////////////////

    @Test
    void testCreateMarketIdea() {
        createMarketIdea();
    }

    @Test
    void testCreateTeamMarketRequest() {
        createMarketTeamRequest(createMarketIdea().getId());
    }

    @Test
    void testMakeMarketIdeaFavorite() {
        makeFavorite(createMarketIdea().getId());
    }

    ///////////////////////////////////////////
    //   ___    ____   __    ____ ______   ____
    //  / _ \  / __/  / /   / __//_  __/  / __/
    // / // / / _/   / /__ / _/   / /    / _/
    ///____/ /___/  /____//___/  /_/    /___/
    ///////////////////////////////////////////

    @Test
    void testDeleteMarketIdea() {
        String ideaMarketId = createMarketIdea().getId();
        getMarketIdea(ideaMarketId);
        webTestClient
                .delete()
                .uri("/api/v1/market/delete/idea/{ideaMarketId}", ideaMarketId)
                .header("Authorization", "Bearer " + jwt)
                .exchange()
                .expectStatus().isOk();
    }

    @Test
    void testDeleteTeamMarketRequest() {
        webTestClient
                .delete()
                .uri("/api/v1/market/delete/request/{teamMarketRequestId}",
                        createMarketTeamRequest(createMarketIdea().getId()).getId())
                .header("Authorization", "Bearer " + jwt)
                .exchange()
                .expectStatus().isOk();
    }

    @Test
    void testUnfavoriteIdea() {
        String ideaMarketId = createMarketIdea().getId();
        makeFavorite(ideaMarketId);
        webTestClient
                .delete()
                .uri("/api/v1/market/unfavorite/{ideaMarketId}", ideaMarketId)
                .header("Authorization", "Bearer " + jwt)
                .exchange()
                .expectStatus().isOk();
    }

    ////////////////////////
    //   ___   __  __ ______
    //  / _ \ / / / //_  __/
    // / ___// /_/ /  / /
    ///_/    \____/  /_/
    ////////////////////////

    @Test
    void testAcceptTeam() {
        webTestClient
                .put()
                .uri("/api/v1/market/accept/{teamMarketId}",
                        createMarketTeamRequest(createMarketIdea().getId()).getId())
                .header("Authorization", "Bearer " + jwt)
                .exchange()
                .expectStatus().isOk();
    }
}
