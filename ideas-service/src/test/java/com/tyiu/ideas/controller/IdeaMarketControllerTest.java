package com.tyiu.ideas.controller;

import com.tyiu.ideas.model.dto.*;
import com.tyiu.ideas.model.entities.Idea;
import com.tyiu.ideas.model.enums.*;
import com.tyiu.ideas.model.requests.IdeaSkillRequest;
import com.tyiu.ideas.model.responses.InfoResponse;
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
    private String marketId;
    private TeamDTO createdTeam;

    private final String path = "/api/v1/ideas-service/market/idea";

    private IdeaSkillRequest buildSkillRequest(String id, List<SkillDTO> skills){
        return IdeaSkillRequest.builder().ideaId(id).skills(skills).build();
    }

    private IdeaDTO buildIdea(String name){
        return IdeaDTO.builder()
                .initiator(userDTO)
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

    private GroupDTO buildGroup(String name, List<Role> roles){
        return GroupDTO.builder().name(name).users(List.of(userDTO))
                .roles(roles).build();
    }

    private SkillDTO buildSkill(String name){
        return SkillDTO.builder().name(name).type(SkillType.LANGUAGE)
                .build();
    }

    private IdeaMarketAdvertisementDTO buildAdvertisement(String name, String id){
        return IdeaMarketAdvertisementDTO.builder().text(name).ideaMarketId(id).build();
    }

    private IdeaDTO createIdea(IdeaDTO ideaDTO){
        IdeaDTO createdIdea = webTestClient
                .post()
                .uri("/api/v1/ideas-service/idea/add")
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
        IdeaDTO ideaDTO1 = buildIdea("idea1");
        IdeaDTO idea1 = getIdea(createIdea(ideaDTO1).getId(),ideaDTO1.getName());

        IdeaDTO ideaDTO2 = buildIdea("idea2");
        IdeaDTO idea2 = getIdea(createIdea(ideaDTO2).getId(),ideaDTO2.getName());

        addSkills(buildSkillRequest(idea1.getId(),List.of(skill1,skill2)));
        addSkills(buildSkillRequest(idea2.getId(),List.of(skill1,skill2)));

        List<IdeaMarketDTO> createdMarketIdea = webTestClient
                .post()
                .uri(path + "/send/{marketId}", marketId)
                .header("Authorization", "Bearer " + jwt)
                .body(Flux.just(idea1, idea2), IdeaDTO.class)
                .exchange()
                .expectBodyList(IdeaMarketDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(createdMarketIdea);
        IdeaMarketDTO ideaMarketDTO = createdMarketIdea.get(0);
        assertNotNull(ideaMarketDTO);
        assertTrue(Objects.equals(ideaMarketDTO.getName(), ideaDTO1.getName()) || Objects.equals(ideaMarketDTO.getName(), ideaDTO2.getName()));
        assertSame(getIdea(ideaMarketDTO.getIdeaId(), ideaMarketDTO.getName()).getStatus(), Idea.Status.ON_MARKET);
        return ideaMarketDTO;
    }

    private TeamMarketRequestDTO createMarketTeamRequest(String ideaMarketId){
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
        assertEquals(ideaMarketId, createdTeamMarketRequest.getIdeaMarketId());
        return createdTeamMarketRequest;
    }

    private SkillDTO createSkill(SkillDTO skillDTO){
        SkillDTO createdSkill = webTestClient
                .post()
                .uri("/api/v1/ideas-service/skill/add")
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
                .uri("/api/v1/ideas-service/group/create")
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(groupDTO), GroupDTO.class)
                .exchange()
                .expectBody(GroupDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(createdGroup);
        assertEquals(groupDTO.getName(), createdGroup.getName());
        return createdGroup;
    }

    private IdeaMarketAdvertisementDTO createAdvertisement(IdeaMarketAdvertisementDTO advertisementDTO){
        IdeaMarketAdvertisementDTO createdAdvertisementDTO =  webTestClient
                .post()
                .uri(path + "/add/advertisement")
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(advertisementDTO), IdeaMarketAdvertisementDTO.class)
                .exchange()
                .expectBody(IdeaMarketAdvertisementDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(createdAdvertisementDTO);
        assertEquals(advertisementDTO.getText(), createdAdvertisementDTO.getText());
        return createdAdvertisementDTO;
    }

    private IdeaDTO getIdea(String id, String name){
        IdeaDTO idea = webTestClient
                .get()
                .uri("/api/v1/ideas-service/idea/{ideaId}", id)
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
                .uri(uri, marketId)
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
                .uri("/api/v1/ideas-service/team/{teamId}", teamId)
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
                .uri("/api/v1/ideas-service/idea/skills/add")
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
        assertNotNull(teamDTO);
        assertNotNull(teamDTO.getId());
        assertTrue(teamDTO.getSkills().size() >= 2);
        TeamDTO ideaMarketTeam = getMarketIdea(ideaMarketId).getTeam();
        assertNotNull(ideaMarketTeam.getId());
        assertNotNull(ideaMarketTeam.getSkills().get(0).getId());
        assertNotNull(ideaMarketTeam.getOwner());
        assertTrue(ideaMarketTeam.getSkills().size() >= 2);
        assertSame(getTeam(teamId).getHasActiveProject(), Boolean.TRUE);
    }

    @BeforeAll
    public void setUp() {
        RegisterRequest request = new RegisterRequest(
                "idea.market@gmail.com", "idea", "market", "idea-market",
                List.of(Role.ADMIN,
                        Role.EXPERT,
                        Role.PROJECT_OFFICE,
                        Role.INITIATOR,
                        Role.TEAM_OWNER));

        AuthenticationResponse response = webTestClient
                .post()
                .uri("/api/v1/ideas-service/auth/register")
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
                .uri("/api/v1/ideas-service/profile/skills/save")
                .header("Authorization", "Bearer " + jwt)
                .body(Flux.just(skill1, skill2), SkillDTO.class)
                .exchange()
                .expectStatus().isOk();

        MarketDTO buildMarket = MarketDTO.builder()
                .name("Зимняя биржа 2024")
                .startDate(LocalDate.now())
                .finishDate(LocalDate.now().plusDays(30))
                .build();
        MarketDTO market = webTestClient
                .post()
                .uri("/api/v1/ideas-service/market/create")
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
                .uri("/api/v1/ideas-service/market/status/{marketId}/{status}", market.getId(), MarketStatus.ACTIVE)
                .header("Authorization", "Bearer " + jwt)
                .exchange()
                .expectStatus().isOk();

        marketId = market.getId();

        TeamDTO teamDTO = TeamDTO.builder()
                .name("name")
                .description("description")
                .closed(false)
                .owner(userDTO)
                .leader(userDTO)
                .members(List.of(userDTO))
                .wantedSkills(List.of(skill1, skill2))
                .build();
        createdTeam = webTestClient
                .post()
                .uri("/api/v1/ideas-service/team/add")
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(teamDTO), TeamDTO.class)
                .exchange()
                .expectBody(TeamDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(createdTeam);
        assertEquals(teamDTO.getName(),createdTeam.getName());
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
        createMarketTeamRequest(ideaId1);
        createMarketTeamRequest(ideaId1);
        createMarketTeamRequest(createMarketIdea().getId());
        List<IdeaMarketDTO> ideaMarketDTOList = webTestClient
                .get()
                .uri(path + "/all")
                .header("Authorization", "Bearer " + jwt)
                .exchange()
                .expectBodyList(IdeaMarketDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(ideaMarketDTOList);
        assertTrue(ideaMarketDTOList.size() >= 2);
        assertTrue(ideaMarketDTOList.get(0).getStack().size() >= 2);
    }

    @Test
    void testGetAllInitiatorMarketIdeas() {
        String ideaId = createMarketIdea().getId();
        createMarketTeamRequest(ideaId);
        createMarketTeamRequest(ideaId);
        assertTrue(getMarketIdeaList(path + "/market/{marketId}/initiator").size() >= 2);
    }

    @Test
    void testGetAllMarketIdeasForMarket() {
        createMarketIdea();
        String marketIdeaId = createMarketIdea().getId();
        createMarketTeamRequest(marketIdeaId);
        createMarketTeamRequest(marketIdeaId);
        assertTrue(getMarketIdeaList(path + "/market/{marketId}/all").size() >= 2);
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
        assertTrue(getMarketIdeaList(path + "/favourite/{marketId}").size() >= 1);
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
        assertEquals(ideaMarketId, marketTeamsRequests.get(0).getIdeaMarketId());
        marketTeamsRequests.forEach(request -> assertTrue(request.getSkills().size() >= 2));
    }

    @Test
    void testGetIdeaMarketAdvertisement() {
        String ideaMarketId = createMarketIdea().getId();
        createAdvertisement(buildAdvertisement("Очень крутое сообщение", ideaMarketId));
        createAdvertisement(buildAdvertisement("Очень некрутое сообщение", ideaMarketId));
        List<IdeaMarketAdvertisementDTO> advertisementDTOS =  webTestClient
                .get()
                .uri(path + "/get/advertisements/{ideaMarketId}", ideaMarketId)
                .header("Authorization", "Bearer " + jwt)
                .exchange()
                .expectBodyList(IdeaMarketAdvertisementDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(advertisementDTOS);
        assertEquals(ideaMarketId, advertisementDTOS.get(0).getIdeaMarketId());
        assertEquals(ideaMarketId, advertisementDTOS.get(1).getIdeaMarketId());
        assertTrue(advertisementDTOS.size() >= 2);
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
                .uri("/api/v1/ideas-service/team/owner/all/{ideaMarketId}", ideaMarketId)
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

    @Test
    void testAddAdvertisement() {
        createAdvertisement(buildAdvertisement("Очень крутое сообщение", createMarketIdea().getId()));
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
    void testUnfavoriteIdea() {
        String ideaMarketId = createMarketIdea().getId();
        makeFavorite(ideaMarketId);
        deleteModel(path + "/unfavorite/{ideaMarketId}", ideaMarketId);
    }

    @Test
    void testDeleteIdeaMarketAdvertisement() {
        deleteModel(path + "/delete/advertisement/{ideaMarketAdvertisementId}",
                createAdvertisement(buildAdvertisement("Очень крутое сообщение", createMarketIdea().getId())).getId());
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
        changeRequestStatus(createMarketTeamRequest(createMarketIdea().getId()).getId(), RequestStatus.CANCELED);
        changeRequestStatus(createMarketTeamRequest(createMarketIdea().getId()).getId(), RequestStatus.WITHDRAWN);
        changeRequestStatus(createMarketTeamRequest(createMarketIdea().getId()).getId(), RequestStatus.NEW);
    }

    @Test
    void testSetAcceptedTeam() {
        String ideaMarketId = createMarketIdea().getId();
        assertNull(getMarketIdea(ideaMarketId).getTeam());
        acceptTeam(ideaMarketId, createMarketTeamRequest(ideaMarketId).getTeamId());
    }

    @Test
    void testUpdateCheckByAdvertisement() {
        webTestClient
                .put()
                .uri(path + "/check/advertisement/{ideaMarketAdvertisementId}",
                        createAdvertisement(buildAdvertisement("Очень крутое сообщение", createMarketIdea().getId())).getId())
                .header("Authorization", "Bearer " + jwt)
                .exchange()
                .expectStatus().isOk();
    }
}
