package com.tyiu.corn.controller;

import com.tyiu.corn.model.dto.*;
import com.tyiu.corn.model.entities.Idea;
import com.tyiu.corn.model.enums.*;
import com.tyiu.corn.model.requests.IdeaMarketRequest;
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
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.time.LocalDate;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicBoolean;

import static org.junit.jupiter.api.Assertions.*;
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

    private final String path = "/api/v1/market/idea";

    private IdeaSkillRequest buildSkillRequest(String id, List<SkillDTO> skills){
        return IdeaSkillRequest.builder().ideaId(id).skills(skills).build();
    }

    private IdeaDTO buildIdea(String name){
        return IdeaDTO.builder()
                .initiatorEmail(userDTO.getEmail())
                .name(name)
                .status(Idea.Status.ON_APPROVAL)
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
    }

    private IdeaMarketRequest buildIdeaMarket(IdeaDTO ideaDTO){
        return IdeaMarketRequest.builder()
                .initiatorEmail(ideaDTO.getInitiatorEmail())
                .name(ideaDTO.getName())
                .id(ideaDTO.getId())
                .createdAt(ideaDTO.getCreatedAt())
                .problem(ideaDTO.getProblem())
                .solution(ideaDTO.getSolution())
                .result(ideaDTO.getResult())
                .customer(ideaDTO.getCustomer())
                .description(ideaDTO.getDescription())
                .maxTeamSize(ideaDTO.getMaxTeamSize())
                .startDate(LocalDate.now())
                .finishDate(LocalDate.now().plusDays(14))
                .build();
    }

    private GroupDTO buildGroup(String name, List<Role> roles){
        return GroupDTO.builder().name(name).users(List.of(userDTO))
                .roles(roles).build();
    }

    private SkillDTO buildSkill(String name){
        return SkillDTO.builder().name(name).type(SkillType.LANGUAGE)
                .build();
    }

    private IdeaDTO createIdea(IdeaDTO ideaDTO){
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
        return createdIdea;
    }

    private IdeaMarketDTO createMarketIdea(){
        LocalDate localDate = LocalDate.now();
        MarketDTO buildMarket = MarketDTO.builder()
                .name("Зимняя биржа 2024")
                .startDate(localDate)
                .finishDate(localDate.plusDays(30))
                .build();
        MarketDTO market = webTestClient
                .post()
                .uri("/api/v1/market/create")
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(buildMarket), MarketDTO.class)
                .exchange()
                .expectBody(MarketDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(market);
        assertEquals(buildMarket.getName(), market.getName());
        assertSame(market.getStatus(), MarketStatus.NEW);

        webTestClient
                .put()
                .uri("/api/v1/market/status/{marketId}/{status}", market.getId(), MarketStatus.ACTIVE)
                .header("Authorization", "Bearer " + jwt)
                .exchange()
                .expectStatus().isOk();

        IdeaDTO ideaDTO1 = buildIdea("idea1");
        IdeaDTO idea1 = getIdea(createIdea(ideaDTO1).getId(),ideaDTO1.getName());

        IdeaDTO ideaDTO2 = buildIdea("idea2");
        IdeaDTO idea2 = getIdea(createIdea(ideaDTO2).getId(),ideaDTO2.getName());

        addSkills(buildSkillRequest(idea1.getId(),List.of(skill1,skill2)));
        addSkills(buildSkillRequest(idea2.getId(),List.of(skill1,skill2)));

        List<IdeaMarketDTO> createdMarketIdea = webTestClient
                .post()
                .uri(path + "/send/{marketId}", market.getId())
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(List.of(buildIdeaMarket(idea1), buildIdeaMarket(idea2))), IdeaMarketRequest.class)
                .exchange()
                .expectBodyList(IdeaMarketDTO.class)
                .returnResult().getResponseBody();
        IdeaMarketDTO ideaMarketDTO = createdMarketIdea.get(0);
        assertNotNull(ideaMarketDTO);
        assertTrue(Objects.equals(ideaMarketDTO.getName(), ideaDTO1.getName()) || Objects.equals(ideaMarketDTO.getName(), ideaDTO2.getName()));
        assertSame(getIdea(ideaMarketDTO.getIdeaId(), ideaMarketDTO.getName()).getStatus(), Idea.Status.ON_MARKET);
        return ideaMarketDTO;
    }

    private TeamMarketRequestDTO createMarketTeamRequest(String ideaMarketId){
        TeamDTO teamDTO = TeamDTO.builder()
                .name("name")
                .description("description")
                .closed(false)
                .owner(userDTO)
                .leader(userDTO)
                .members(List.of(userDTO))
                .wantedSkills(List.of(skill1, skill2))
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
                .name(createdTeam.getName())
                .letter("letter")
                .build();
        TeamMarketRequestDTO createdTeamMarketRequest = webTestClient
                .post()
                .uri(path + "/declare")
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(teamMarketRequest), TeamMarketRequestDTO.class)
                .exchange()
                .expectBody(TeamMarketRequestDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(createdTeamMarketRequest);
        assertEquals(teamMarketRequest.getName(),createdTeamMarketRequest.getName());
        return createdTeamMarketRequest;
    }

    private SkillDTO createSkill(SkillDTO skillDTO){
        SkillDTO createdSkill = webTestClient
                .post()
                .uri("/api/v1/skill/add")
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(skillDTO), SkillDTO.class)
                .exchange()
                .expectBody(SkillDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(createdSkill);
        assertEquals(skillDTO.getName(), createdSkill.getName());
        return createdSkill;
    }

    private GroupDTO createGroup(GroupDTO groupDTO){
        GroupDTO createdGroup = webTestClient
                .post()
                .uri("/api/v1/group/create")
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(groupDTO), GroupDTO.class)
                .exchange()
                .expectBody(GroupDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(createdGroup);
        assertEquals(groupDTO.getName(), createdGroup.getName());
        return createdGroup;
    }

    private IdeaDTO getIdea(String id, String name){
        IdeaDTO idea = webTestClient
                .get()
                .uri("/api/v1/idea/{ideaId}", id)
                .header("Authorization", "Bearer " + jwt)
                .exchange()
                .expectBody(IdeaDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(idea);
        assertEquals(name, idea.getName());
        return idea;
    }

    private IdeaMarketDTO getMarketIdea(String ideaMarketId){
        IdeaMarketDTO responseBody = webTestClient
                .get()
                .uri(path + "/{ideaMarketId}", ideaMarketId)
                .header("Authorization", "Bearer " + jwt)
                .exchange()
                .expectBody(IdeaMarketDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(responseBody);
        assertTrue(Objects.equals(responseBody.getName(), "idea1") || Objects.equals(responseBody.getName(), "idea2"));
        return responseBody;
    }

    private List<IdeaMarketDTO> getMarketIdeaList(String uri){
        List<IdeaMarketDTO> marketIdeas = webTestClient
                .get()
                .uri(uri)
                .header("Authorization", "Bearer " + jwt)
                .exchange()
                .expectBodyList(IdeaMarketDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(marketIdeas);
        assertTrue(marketIdeas.get(0).getStack().size() >= 2);
        return marketIdeas;
    }

    private TeamDTO getTeam(String teamId){
        TeamDTO team = webTestClient
                .get()
                .uri("/api/v1/team/{teamId}", teamId)
                .header("Authorization", "Bearer " + jwt)
                .exchange()
                .expectBody(TeamDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(team);
        assertEquals("name", team.getName());
        return team;
    }

    private void makeFavorite(String ideaMarketId){
        webTestClient
                .put()
                .uri(path + "/favorite/{ideaMarketId}", ideaMarketId)
                .header("Authorization", "Bearer " + jwt)
                .exchange();
        assertEquals(true, getMarketIdea(ideaMarketId).getIsFavorite());
    }

    private void addSkills(IdeaSkillRequest ideaSkillRequest){
        InfoResponse skillRequest = webTestClient
                .post()
                .uri("/api/v1/idea/skills/add")
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(ideaSkillRequest), IdeaSkillRequest.class)
                .exchange()
                .expectBody(InfoResponse.class)
                .returnResult().getResponseBody();
        assertNotNull(skillRequest);
    }

    private void deleteModel(String uri, String id){
        webTestClient
                .delete()
                .uri(uri, id)
                .header("Authorization", "Bearer " + jwt)
                .exchange()
                .expectStatus().isOk();
    }

    private void changeRequestStatus(String teamMarketId, RequestStatus requestStatus){
        webTestClient
                .put()
                .uri(path + "/change-status/request/{teamMarketId}/{status}",
                        teamMarketId, requestStatus)
                .header("Authorization", "Bearer " + jwt)
                .exchange()
                .expectStatus().isOk();
    }

    private void acceptTeam(String ideaMarketId, String teamId){
        TeamDTO teamDTO = webTestClient
                .put()
                .uri(path + "/accept/request/{ideaMarketId}/{teamId}",
                        ideaMarketId, teamId)
                .header("Authorization", "Bearer " + jwt)
                .exchange()
                .expectBody(TeamDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(teamDTO.getId());
        assertTrue(teamDTO.getSkills().size() >= 2);
        TeamDTO ideaMarketTeam = getMarketIdea(ideaMarketId).getTeam();
        assertNotNull(ideaMarketTeam.getId());
        assertNotNull(ideaMarketTeam.getSkills().get(0).getId());
        assertTrue(ideaMarketTeam.getSkills().size() >= 2);
        assertSame(getTeam(teamId).getHasActiveProject(), Boolean.TRUE);
    }

    @BeforeAll
    public void setUp() {
        RegisterRequest request = new RegisterRequest(
                "fakemail104", "fakename104", "fakename104", "fakepass104",
                List.of(Role.ADMIN,
                        Role.EXPERT,
                        Role.PROJECT_OFFICE,
                        Role.INITIATOR,
                        Role.TEAM_OWNER));

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

        groupExpert = createGroup(buildGroup("exp",List.of(Role.EXPERT)));
        groupProjectOffice = createGroup(buildGroup("pro",List.of(Role.PROJECT_OFFICE)));

        skill1 = createSkill(buildSkill("skill1"));
        skill2 = createSkill(buildSkill("skill2"));

        webTestClient
                .post()
                .uri("/api/v1/profile/skills/save")
                .header("Authorization", "Bearer " + jwt)
                .body(Flux.just(skill1, skill2), SkillDTO.class)
                .exchange()
                .expectStatus().isOk();
    }

    ///////////////////////
    //  _____   ____ ______
    // / ___/  / __//_  __/
    /// (_ /  / _/   / /
    //\___/  /___/  /_/
    ///////////////////////

    @Test
    void testGetAllMarketIdeas() {
        String ideaId1 = createMarketIdea().getId();
//        String ideaId2 = createMarketIdea().getId();
//        String ideaId3 = createMarketIdea().getId();
//        String ideaId4 = createMarketIdea().getId();
//        String ideaId5 = createMarketIdea().getId();
        createMarketTeamRequest(ideaId1);
        createMarketTeamRequest(ideaId1);
//        createMarketTeamRequest(ideaId2);
//        createMarketTeamRequest(ideaId3);
//        createMarketTeamRequest(ideaId3);
//        createMarketTeamRequest(ideaId3);
//        createMarketTeamRequest(ideaId4);
//        createMarketTeamRequest(ideaId4);
//        createMarketTeamRequest(ideaId5);
//        createMarketTeamRequest(ideaId5);
//        createMarketTeamRequest(ideaId5);
//        createMarketTeamRequest(ideaId5);
        List<IdeaMarketDTO> ideaMarketDTOList = getMarketIdeaList(path + "/all");
        assertTrue(ideaMarketDTOList.size() >= 2);
//        System.out.print(ideaMarketDTOList.get(0).getPosition());
//        System.out.print(ideaMarketDTOList.get(1).getPosition());
//        System.out.print(ideaMarketDTOList.get(2).getPosition());
//        System.out.print(ideaMarketDTOList.get(3).getPosition());
//        System.out.print(ideaMarketDTOList.get(4).getPosition());
    }

    @Test
    void testGetAllInitiatorMarketIdeas() {
        String ideaId = createMarketIdea().getId();
        createMarketTeamRequest(ideaId);
        createMarketTeamRequest(ideaId);
        assertTrue(getMarketIdeaList(path + "/all").size() >= 2);
    }

    @Test
    void testGetMarketIdea() {
        String ideaMarketId = createMarketIdea().getId();
        createMarketTeamRequest(ideaMarketId);
        createMarketTeamRequest(ideaMarketId);
        changeRequestStatus(createMarketTeamRequest(ideaMarketId).getId(), RequestStatus.ACCEPTED);
        IdeaMarketDTO ideaMarketDTO = getMarketIdea(ideaMarketId);
        assertEquals(ideaMarketDTO.getRequests(), 3);
        assertEquals(ideaMarketDTO.getAcceptedRequests(), 1);
    }

    @Test
    void testGetAllFavoriteMarketIdeas() {
        makeFavorite(createMarketIdea().getId());
        assertTrue(getMarketIdeaList(path + "/favorite").size() >= 1);
    }

    @Test
    void testGetAllTeamMarketRequests() {
        String ideaMarketId = createMarketIdea().getId();
        createMarketTeamRequest(ideaMarketId);
        List<TeamMarketRequestDTO> marketTeamsRequests =  webTestClient
                .get()
                .uri(path + "/requests/{ideaMarketId}", ideaMarketId)
                .header("Authorization", "Bearer " + jwt)
                .exchange()
                .expectBodyList(TeamMarketRequestDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(marketTeamsRequests);
        assertEquals(1, marketTeamsRequests.size());
        assertTrue(marketTeamsRequests.get(0).getSkills().size() >= 2);
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
        String ideaMarketId = createMarketIdea().getId();
        changeRequestStatus(createMarketTeamRequest(ideaMarketId).getId(), RequestStatus.CANCELED);
        changeRequestStatus(createMarketTeamRequest(ideaMarketId).getId(), RequestStatus.ACCEPTED);
        List<TeamDTO> teamDTOS = webTestClient
                .get()
                .uri("/api/v1/team/owner/all/{ideaMarketId}", ideaMarketId)
                .header("Authorization", "Bearer " + jwt)
                .exchange()
                .expectBodyList(TeamDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(teamDTOS);
        AtomicBoolean check = new AtomicBoolean(false);
        teamDTOS.stream().map(TeamDTO::getIsRefused).forEach(b -> {
            if (b) {
                check.set(true);
            }
        });
        assertTrue(check.get());
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
        deleteModel(path + "/delete/idea/{ideaMarketId}", ideaMarketId);
    }

    @Test
    void testDeleteTeamMarketRequest() {
        deleteModel(path + "/delete/request/{teamMarketRequestId}",
                createMarketTeamRequest(createMarketIdea().getId()).getId());
    }

    @Test
    void testUnfavoriteIdea() {
        String ideaMarketId = createMarketIdea().getId();
        makeFavorite(ideaMarketId);
        deleteModel(path + "/unfavorite/{ideaMarketId}", ideaMarketId);
    }

    ////////////////////////
    //   ___   __  __ ______
    //  / _ \ / / / //_  __/
    // / ___// /_/ /  / /
    ///_/    \____/  /_/
    ////////////////////////

    @Test
    void testMakeMarketIdeaFavorite() {
        makeFavorite(createMarketIdea().getId());
    }

    @Test
    void testChangeIdeaMarketStatus() {
        webTestClient
                .put()
                .uri(path + "/idea-status/{ideaMarketId}/{status}",
                        createMarketIdea().getId(), IdeaMarketStatusType.RECRUITMENT_IS_CLOSED)
                .header("Authorization", "Bearer " + jwt)
                .exchange()
                .expectStatus().isOk();
    }

    @Test
    void testChangeRequestStatus() {
        changeRequestStatus(createMarketTeamRequest(createMarketIdea().getId()).getId(), RequestStatus.ACCEPTED);
    }

    @Test
    void testSetAcceptedTeam() {
        String ideaMarketId = createMarketIdea().getId();
        assertNull(getMarketIdea(ideaMarketId).getTeam());
        acceptTeam(ideaMarketId, createMarketTeamRequest(ideaMarketId).getTeamId());
    }
}
