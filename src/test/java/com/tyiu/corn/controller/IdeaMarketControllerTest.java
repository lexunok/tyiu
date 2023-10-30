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

    private IdeaMarketDTO createMarketIdea(){
        GroupDTO experts = GroupDTO.builder()
                .name("exp")
                .users(List.of(userDTO))
                .roles(List.of(Role.EXPERT))
                .build();
        GroupDTO groupExpert = webTestClient
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
        GroupDTO groupProjectOffice = webTestClient
                .post()
                .uri("/api/v1/group/create")
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(projectOffice), GroupDTO.class)
                .exchange()
                .expectBody(GroupDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(groupProjectOffice);
        assertEquals(projectOffice.getName(), groupProjectOffice.getName());
        CompanyDTO company = CompanyDTO.builder()
                .name("company")
                .users(List.of(userDTO))
                .build();
        CompanyDTO createdCompany = webTestClient
                .post()
                .uri("/api/v1/company/create")
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(company), CompanyDTO.class)
                .exchange()
                .expectBody(CompanyDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(createdCompany);
        assertEquals(company.getName(), createdCompany.getName());
        IdeaDTO ideaDTO = IdeaDTO.builder()
                .name("idea")
                .experts(groupExpert)
                .projectOffice(groupProjectOffice)
                .company(createdCompany)
                .status(StatusIdea.NEW)
                .projectType(ProjectType.INSIDE)
                .problem("problem")
                .solution("solution")
                .result("result")
                .customer("customer")
                .contactPerson("contactPerson")
                .description("description")
                .suitability(1L)
                .budget(2L)
                .technicalRealizability(3L)
                .preAssessment(4.0)
                .rating(4.0)
                .build();
        IdeaDTO createdIdea = webTestClient
                .post()
                .uri("/api/v1/idea/add")
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(ideaDTO), IdeaDTO.class)
                .exchange()
                .expectBody(IdeaDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(createdIdea);
        assertEquals(ideaDTO.getName(), createdIdea.getName());
        Long ideaId = createdIdea.getId();
        SkillDTO skillDTO1 = SkillDTO.builder()
                .name("skill1")
                .type(SkillType.LANGUAGE)
                .build();
        SkillDTO addSkillResponse1 = webTestClient
                .post()
                .uri("/api/v1/skill/add")
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(skillDTO1), SkillDTO.class)
                .exchange()
                .expectBody(SkillDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(addSkillResponse1);
        assertEquals(skillDTO1.getName(), addSkillResponse1.getName());
        SkillDTO skillDTO2 = SkillDTO.builder()
                .name("skill2")
                .type(SkillType.LANGUAGE)
                .build();
        SkillDTO addSkillResponse2 = webTestClient
                .post()
                .uri("/api/v1/skill/add")
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(skillDTO2), SkillDTO.class)
                .exchange()
                .expectBody(SkillDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(addSkillResponse2);
        assertEquals(skillDTO2.getName(), addSkillResponse2.getName());
        IdeaSkillRequest ideaSkillRequest = IdeaSkillRequest.builder()
                .ideaId(ideaId)
                .skills(List.of(addSkillResponse1,addSkillResponse2))
                .build();
        InfoResponse skillRequest = webTestClient
                .post()
                .uri("/api/v1/idea/skills/add")
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(ideaSkillRequest), IdeaSkillRequest.class)
                .exchange()
                .expectBody(InfoResponse.class)
                .returnResult().getResponseBody();
        assertNotNull(skillRequest);
        assertEquals("Success!", skillRequest.getMessage());
        IdeaMarketDTO ideaMarketDTO = IdeaMarketDTO.builder()
                .position(1L)
                .name(createdIdea.getName())
                .initiator(createdIdea.getInitiator())
                .description(createdIdea.getDescription())
                .maxTeamSize(10L)
                .status(IdeaMarketStatusType.RECRUITMENT_IS_OPEN)
                .build();
        IdeaMarketDTO createdMarketIdea = webTestClient
                .post()
                .uri("/api/v1/market/send/{ideaId}", ideaId)
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(ideaMarketDTO), IdeaMarketDTO.class)
                .exchange()
                .expectBody(IdeaMarketDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(createdMarketIdea);
        assertEquals(ideaMarketDTO.getName(), createdMarketIdea.getName());
        return createdMarketIdea;
    }

    private IdeaMarketDTO getMarketIdea(Long ideaMarketId){
        IdeaMarketDTO responseBody = webTestClient
                .get()
                .uri("/api/v1/market/{ideaMarketId}", ideaMarketId)
                .header("Authorization", "Bearer " + jwt)
                .exchange()
                .expectBody(IdeaMarketDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(responseBody);
        assertEquals("idea", responseBody.getName());
        return responseBody;
    }

    private TeamMarketRequestDTO createMarketTeamRequest(Long ideaMarketId){
        SkillDTO skillDTO1 = SkillDTO.builder()
                .name("skill1")
                .type(SkillType.LANGUAGE)
                .build();
        SkillDTO addSkillResponse1 = webTestClient
                .post()
                .uri("/api/v1/skill/add")
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(skillDTO1), SkillDTO.class)
                .exchange()
                .expectBody(SkillDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(addSkillResponse1);
        SkillDTO skillDTO2 = SkillDTO.builder()
                .name("skill2")
                .type(SkillType.LANGUAGE)
                .build();
        SkillDTO addSkillResponse2 = webTestClient
                .post()
                .uri("/api/v1/skill/add")
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(skillDTO2), SkillDTO.class)
                .exchange()
                .expectBody(SkillDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(addSkillResponse2);
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
                .skills(List.of(addSkillResponse1,addSkillResponse2))
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
                .ideaId(ideaMarketId)
                .accepted(false)
                .name(createdTeam.getName())
                .closed(createdTeam.getClosed())
                .description(createdTeam.getDescription())
                .owner(teamMemberDTO)
                .leader(teamMemberDTO)
                .members(List.of(teamMemberDTO))
                .skills(List.of(addSkillResponse1,addSkillResponse2))
                .letter("letter")
                .build();
        TeamMarketRequestDTO createdTeamMarketRequest = webTestClient
                .post()
                .uri("/api/v1/market/declare/{teamId}", createdTeam.getId())
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(teamMarketRequest), TeamMarketRequestDTO.class)
                .exchange()
                .expectBody(TeamMarketRequestDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(createdTeamMarketRequest);
        assertEquals(teamMarketRequest.getName(),createdTeamMarketRequest.getName());
        return createdTeamMarketRequest;
    }

    @BeforeAll
    public void setUp() {
        RegisterRequest request = new RegisterRequest(
                "fakemail104", "fakename104", "fakename104", "fakepass104", List.of(Role.ADMIN));

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

    ///////////////////////
    //  _____   ____ ______
    // / ___/  / __//_  __/
    /// (_ /  / _/   / /
    //\___/  /___/  /_/
    ///////////////////////

    @Test
    void testGetAllMarketIdeas() {
        createMarketIdea();
        createMarketIdea();
        List<IdeaMarketDTO> marketIdeas = webTestClient
                .get()
                .uri("/api/v1/market/all")
                .header("Authorization", "Bearer " + jwt)
                .exchange()
                .expectBodyList(IdeaMarketDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(marketIdeas);
        assertEquals(9, marketIdeas.size());
    }

    @Test
    void testGetMarketIdea() {
        getMarketIdea(createMarketIdea().getId());
    }

    @Test
    void testGetAllFavoriteMarketIdeas() {
        Long ideaMarketId = createMarketIdea().getId();
        webTestClient
                .post()
                .uri("/api/v1/market/favorite/{ideaMarketId}", ideaMarketId)
                .header("Authorization", "Bearer " + jwt)
                .exchange();
        assertEquals(true, getMarketIdea(ideaMarketId).getIsFavorite());
    }

    @Test
    void testGetAllTeamMarketRequests() {
        Long ideaMarketId = createMarketIdea().getId();
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
        Long ideaMarketId = createMarketIdea().getId();
        webTestClient
                .post()
                .uri("/api/v1/market/favorite/{ideaMarketId}", ideaMarketId)
                .header("Authorization", "Bearer " + jwt)
                .exchange();
        assertEquals(true, getMarketIdea(ideaMarketId).getIsFavorite());
    }

    ///////////////////////////////////////////
    //   ___    ____   __    ____ ______   ____
    //  / _ \  / __/  / /   / __//_  __/  / __/
    // / // / / _/   / /__ / _/   / /    / _/
    ///____/ /___/  /____//___/  /_/    /___/
    ///////////////////////////////////////////

    @Test
    void testDeleteMarketIdea() {
        Long ideaMarketId = createMarketIdea().getId();
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
        Long ideaMarketId = createMarketIdea().getId();
        webTestClient
                .post()
                .uri("/api/v1/market/favorite/{ideaMarketId}", ideaMarketId)
                .header("Authorization", "Bearer " + jwt)
                .exchange();
        assertEquals(true, getMarketIdea(ideaMarketId).getIsFavorite());
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
