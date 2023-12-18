package com.tyiu.corn.controller;

import com.tyiu.corn.model.dto.*;
import com.tyiu.corn.model.entities.Idea;
import com.tyiu.corn.model.entities.TeamInvitation;
import com.tyiu.corn.model.entities.TeamRequest;
import com.tyiu.corn.model.enums.MarketStatus;
import com.tyiu.corn.model.enums.RequestStatus;
import com.tyiu.corn.model.enums.Role;
import com.tyiu.corn.model.enums.SkillType;
import com.tyiu.corn.model.requests.IdeaMarketRequest;
import com.tyiu.corn.model.requests.IdeaSkillRequest;
import com.tyiu.corn.model.requests.RegisterRequest;
import com.tyiu.corn.model.responses.AuthenticationResponse;
import com.tyiu.corn.model.responses.InfoResponse;
import org.junit.jupiter.api.*;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.HttpStatus;
import org.springframework.test.web.reactive.server.WebTestClient;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.time.Duration;
import java.time.LocalDate;
import java.util.List;
import java.util.Objects;

import static org.junit.jupiter.api.Assertions.*;
import static org.springframework.boot.test.context.SpringBootTest.WebEnvironment.RANDOM_PORT;

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringBootTest(webEnvironment = RANDOM_PORT)
public class TeamControllerTest extends TestContainers {

    @Autowired
    private WebTestClient webTestClient;
    private UserDTO admin;
    private UserDTO owner;
    private UserDTO leader;
    private UserDTO member;
    private UserDTO randomUser;
    private UserDTO initiator;
    private UserDTO kostya;
    private String jwt_admin;
    private String jwt_owner;
    private String jwt_leader;
    private String jwt_member;
    private String jwt_randomUser;
    private String jwt_initiator;
    private String jwt_kostya;
    private GroupDTO groupExpert;
    private GroupDTO groupProjectOffice;
    private SkillDTO skill1;
    private SkillDTO skill2;
    private SkillDTO skill3;
    private SkillDTO skill4;

    private AuthenticationResponse register(String email, String firstName, String lastName, List<Role> roles) {
        AuthenticationResponse response = webTestClient
                .post()
                .uri("/api/v1/auth/register")
                .body(Mono.just(new RegisterRequest(email, firstName, lastName, "bla-bla-bla", roles)), RegisterRequest.class)
                .exchange()
                .expectBody(AuthenticationResponse.class)
                .returnResult().getResponseBody();
        assertNotNull(response);
        return response;
    }

    private UserDTO userBuild(String userId, String email, String firstname, String lastname, List<Role> roles) {
        return UserDTO.builder().id(userId).email(email).firstName(firstname).lastName(lastname).roles(roles)
                .build();
    }

    private IdeaSkillRequest buildSkillRequest(String id, List<SkillDTO> skills) {
        return IdeaSkillRequest.builder().ideaId(id).skills(skills).build();
    }

    private GroupDTO buildGroup(String name, List<Role> roles) {
        return GroupDTO.builder().name(name).users(List.of(admin)).roles(roles).build();
    }

    private IdeaDTO buildIdea(String name) {
        return IdeaDTO.builder()
                .initiatorEmail(initiator.getEmail())
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

    private IdeaMarketRequest buildIdeaMarket(IdeaDTO ideaDTO) {
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

    private TeamInvitation buildInvitation(String teamId, UserDTO userDTO){
        return TeamInvitation.builder()
                .teamId(teamId)
                .userId(userDTO.getId())
                .email(userDTO.getEmail())
                .firstName(userDTO.getFirstName())
                .lastName(userDTO.getLastName())
                .build();
    }

    private TeamDTO getTeam(String teamId) {
        TeamDTO responseGetTeam = getRequest("/api/v1/team/{teamId}", teamId, "Bearer " + jwt_randomUser)
                .expectBody(TeamDTO.class).returnResult().getResponseBody();
        assertNotNull(responseGetTeam);
        return responseGetTeam;
    }

    private List<TeamDTO> getTeamsByVacancies(List<SkillDTO> skills, String jwt) {
        List<TeamDTO> allTeamsByVacancies = postRequest("api/v1/team/vacancy-filter", jwt)
                .body(Flux.fromIterable(skills), SkillDTO.class)
                .exchange()
                .expectBodyList(TeamDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(allTeamsByVacancies);
        return allTeamsByVacancies;
    }

    private List<TeamDTO> getTeamsBySkills(List<SkillDTO> skills, Role role, String jwt) {
        List<TeamDTO> allTeamsByVacancies = webTestClient
                .post()
                .uri("api/v1/team/skill-filter/{role}", role)
                .header("Authorization",jwt)
                .body(Flux.fromIterable(skills), SkillDTO.class)
                .exchange()
                .expectBodyList(TeamDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(allTeamsByVacancies);
        return allTeamsByVacancies;
    }

    private List<SkillDTO> getSkillsByInvitations(List<TeamInvitation> users) {
        List<SkillDTO> allSkillsByInvitation = postRequest("api/v1/team/skills/invitations", "Bearer " + jwt_owner)
                .body(Flux.fromIterable(users), SkillDTO.class)
                .exchange()
                .expectBodyList(SkillDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(allSkillsByInvitation);
        return allSkillsByInvitation;
    }

    private List<SkillDTO> getSkillsByRequests(List<TeamRequest> users) {
        List<SkillDTO> allSkillsByRequests = postRequest("api/v1/team/skills/requests", "Bearer " + jwt_owner)
                .body(Flux.fromIterable(users), SkillDTO.class)
                .exchange()
                .expectBodyList(SkillDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(allSkillsByRequests);
        return allSkillsByRequests;
    }

    private IdeaDTO getIdea(String id, String name) {
        IdeaDTO idea = getRequest("/api/v1/idea/{ideaId}", id, "Bearer " + jwt_admin)
                .expectBody(IdeaDTO.class).returnResult().getResponseBody();
        assertNotNull(idea);
        assertEquals(name, idea.getName());
        return idea;
    }

    private SkillDTO createSkill(String name, SkillType type) {
        SkillDTO skill = SkillDTO.builder()
                .name(name)
                .type(type)
                .build();
        SkillDTO responseCreateSkill = postRequest("/api/v1/skill/add","Bearer " + jwt_admin)
                .body(Mono.just(skill), SkillDTO.class)
                .exchange()
                .expectBody(SkillDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(responseCreateSkill);
        assertEquals(skill.getName(), responseCreateSkill.getName());
        assertEquals(skill.getType(), responseCreateSkill.getType());
        return responseCreateSkill;
    }

    private GroupDTO createGroup(GroupDTO groupDTO) {
        GroupDTO createdGroup = postRequest("/api/v1/group/create","Bearer " + jwt_admin)
                .body(Mono.just(groupDTO), GroupDTO.class)
                .exchange()
                .expectBody(GroupDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(createdGroup);
        assertEquals(groupDTO.getName(), createdGroup.getName());
        return createdGroup;
    }

    private IdeaDTO createIdea(IdeaDTO ideaDTO) {
        IdeaDTO createdIdea = postRequest("/api/v1/idea/add","Bearer " + jwt_admin)
                .body(Mono.just(ideaDTO), IdeaDTO.class)
                .exchange()
                .expectBody(IdeaDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(createdIdea);
        assertEquals(ideaDTO.getName(), createdIdea.getName());
        return createdIdea;
    }

    private IdeaMarketDTO createMarketIdea() {
        MarketDTO buildMarket = MarketDTO.builder()
                .name("Зимняя биржа 2024")
                .startDate(LocalDate.now())
                .finishDate(LocalDate.now().plusDays(30))
                .build();
        MarketDTO market = postRequest("/api/v1/market/create","Bearer " + jwt_admin)
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
                .header("Authorization", "Bearer " + jwt_admin)
                .exchange()
                .expectStatus().isOk();

        IdeaDTO ideaDTO1 = buildIdea("idea1");
        IdeaDTO idea1 = getIdea(createIdea(ideaDTO1).getId(),ideaDTO1.getName());

        IdeaDTO ideaDTO2 = buildIdea("idea2");
        IdeaDTO idea2 = getIdea(createIdea(ideaDTO2).getId(),ideaDTO2.getName());

        addSkills(buildSkillRequest(idea1.getId(),List.of(skill1, skill2)));
        addSkills(buildSkillRequest(idea2.getId(),List.of(skill1, skill2)));

        List<IdeaMarketDTO> createdMarketIdea = postRequest("/api/v1/market/idea/send/{marketId}",market.getId(),"Bearer " + jwt_admin)
                .body(Mono.just(List.of(buildIdeaMarket(idea1), buildIdeaMarket(idea2))), IdeaMarketRequest.class)
                .exchange()
                .expectBodyList(IdeaMarketDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(createdMarketIdea);
        IdeaMarketDTO ideaMarketDTO = createdMarketIdea.get(0);
        assertTrue(Objects.equals(ideaMarketDTO.getName(), ideaDTO1.getName()) || Objects.equals(ideaMarketDTO.getName(), ideaDTO2.getName()));
        assertSame(getIdea(ideaMarketDTO.getIdeaId(), ideaMarketDTO.getName()).getStatus(), Idea.Status.ON_MARKET);
        return ideaMarketDTO;
    }

    private TeamDTO createTeam(List<UserDTO> members) {
        TeamDTO team = buildTeam("Богатыри","Слава Руси!",members.size(),
                owner,leader,members,List.of());
        TeamDTO responseAddTeam = createTeamRequest(team)
                .expectBody(TeamDTO.class).returnResult().getResponseBody();
        assertNotNull(responseAddTeam);
        assertEquals(team.getName(), responseAddTeam.getName());
        assertEquals(team.getDescription(), responseAddTeam.getDescription());
        assertEquals(team.getClosed(), responseAddTeam.getClosed());
        assertEquals(team.getMembersCount(), responseAddTeam.getMembersCount());
        assertEquals(team.getOwner(), responseAddTeam.getOwner());
        assertEquals(team.getLeader(), responseAddTeam.getLeader());
        assertEquals(team.getSkills(), responseAddTeam.getSkills());
        assertEquals(team.getWantedSkills(), responseAddTeam.getWantedSkills());
        return responseAddTeam;
    }

    private TeamRequest sendTeamRequest(String teamId, UserDTO user, String jwt) {
        TeamRequest request = TeamRequest.builder()
                .teamId(teamId)
                .userId(user.getId())
                .email(user.getEmail())
                .firstName(user.getFirstName())
                .lastName(user.getLastName())
                .createdAt(LocalDate.now())
                .status(RequestStatus.NEW)
                .build();
        TeamRequest responseSendRequest = postRequest("/api/v1/team/request/send/{teamId}",teamId,"Bearer " + jwt)
                .body(Mono.just(request), TeamRequest.class)
                .exchange()
                .expectBody(TeamRequest.class)
                .returnResult().getResponseBody();
        assertNotNull(responseSendRequest);
        assertEquals(request.getTeamId(), responseSendRequest.getTeamId());
        assertEquals(request.getUserId(), responseSendRequest.getUserId());
        assertEquals(request.getEmail(), responseSendRequest.getEmail());
        assertEquals(request.getFirstName(), responseSendRequest.getFirstName());
        assertEquals(request.getLastName(), responseSendRequest.getLastName());
        assertEquals(request.getStatus(), responseSendRequest.getStatus());
        return responseSendRequest;
    }

    private List<TeamInvitation> sendInvites(List<TeamInvitation> user) {
        List<TeamInvitation> invites = postRequest("/api/v1/team/send-invites","Bearer " + jwt_owner)
                .body(Flux.fromIterable(user), TeamMemberDTO.class)
                .exchange()
                .expectBodyList(TeamInvitation.class)
                .returnResult().getResponseBody();
        assertNotNull(invites);
        assertNotNull(invites.get(0).getUserId());
        return invites;
    }

    private TeamInvitation inviteUserInTeam(String teamId, String userId) {
        TeamInvitation responseInviteInTeam = webTestClient
                .post()
                .uri("/api/v1/team/invite/{teamId}/{userId}", teamId, userId)
                .header("Authorization", "Bearer " + jwt_owner)
                .exchange()
                .expectBody(TeamInvitation.class)
                .returnResult().getResponseBody();
        assertNotNull(responseInviteInTeam);
        return responseInviteInTeam;
    }

    private TeamDTO updateTeam(String teamId, List<UserDTO> members) {
        TeamDTO team = TeamDTO.builder()
                .name("Ящеры")
                .description("Слава Подикам!")
                .closed(true)
                .membersCount(members.size())
                .createdAt(LocalDate.now())
                .owner(owner)
                .leader(leader)
                .members(members)
                .skills(List.of())
                .wantedSkills(List.of())
                .build();
        TeamDTO responseUpdateTeam = webTestClient
                .put()
                .uri("/api/v1/team/update/{teamId}", teamId)
                .header("Authorization", "Bearer " + jwt_owner)
                .body(Mono.just(team), TeamDTO.class)
                .exchange()
                .expectBody(TeamDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(responseUpdateTeam);
        return responseUpdateTeam;
    }

    private WebTestClient.ResponseSpec changeLeader(String teamId, String userId, String jwt){
        return webTestClient
                .put()
                .uri("/api/v1/team/change/leader/{teamId}/{userId}", teamId, userId)
                .header("Authorization", jwt)
                .exchange();
    }

    private WebTestClient.ResponseSpec updateSkills(String teamId, Flux<SkillDTO> skills, String jwt){
        return webTestClient
                .put()
                .uri("/api/v1/team/skills/update/{teamId}", teamId)
                .header("Authorization", jwt)
                .body(skills, SkillDTO.class)
                .exchange();
    }

    private WebTestClient.ResponseSpec createTeamRequest(TeamDTO teamDTO){
        return postRequest("/api/v1/team/add","Bearer " + jwt_owner)
                .body(Mono.just(teamDTO), TeamDTO.class)
                .exchange();
    }

    private WebTestClient.ResponseSpec getRequest(String path, String jwt){
        return webTestClient.get().uri(path).header("Authorization", jwt)
                .exchange();
    }

    private WebTestClient.ResponseSpec getRequest(String path, String object, String jwt){
        return webTestClient.get().uri(path, object).header("Authorization", jwt)
                .exchange();
    }

    private WebTestClient.RequestBodySpec postRequest(String path, String jwt){
        return webTestClient.post().uri(path)
                .header("Authorization", jwt);
    }

    private WebTestClient.RequestBodySpec postRequest(String path, String object, String jwt){
        return webTestClient.post().uri(path, object)
                .header("Authorization", jwt);
    }

    private WebTestClient.ResponseSpec deleteRequest(String path, String object, String jwt){
        return webTestClient.delete().uri(path, object).header("Authorization", jwt)
                .exchange();
    }

    private void createMarketTeamRequest(String ideaMarketId, TeamDTO createdTeam){
        TeamMarketRequestDTO teamMarketRequest = TeamMarketRequestDTO.builder()
                .ideaMarketId(ideaMarketId)
                .teamId(createdTeam.getId())
                .name(createdTeam.getName())
                .letter("letter")
                .build();
        TeamMarketRequestDTO createdTeamMarketRequest = postRequest("/api/v1/market/idea/declare",
                "Bearer " + jwt_owner)
                .body(Mono.just(teamMarketRequest), TeamMarketRequestDTO.class)
                .exchange()
                .expectBody(TeamMarketRequestDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(createdTeamMarketRequest);
        assertEquals(teamMarketRequest.getName(),createdTeamMarketRequest.getName());
    }

    private void saveSkillToUser(List<SkillDTO> skills, String jwt) {
        postRequest("/api/v1/profile/skills/save","Bearer " + jwt)
                .body(Mono.just(skills), SkillDTO.class)
                .exchange()
                .expectStatus().isOk();
    }

    private void addSkills(IdeaSkillRequest ideaSkillRequest) {
        InfoResponse skillRequest = postRequest("/api/v1/idea/skills/add","Bearer " + jwt_admin)
                .body(Mono.just(ideaSkillRequest), IdeaSkillRequest.class)
                .exchange()
                .expectBody(InfoResponse.class)
                .returnResult().getResponseBody();
        assertNotNull(skillRequest);
    }

    private void kickUserFromTeam(String teamId, String userId) {
        webTestClient
                .delete()
                .uri("/api/v1/team/kick/{teamId}/{userId}", teamId, userId)
                .header("Authorization", "Bearer " + jwt_randomUser)
                .exchange()
                .expectStatus().isOk();
    }

    private void getOwnerTeams(String ideaMarketId){
        List<TeamDTO> ownerTeams = getRequest("api/v1/team/owner/all/{ideaMarketId}", ideaMarketId,"Bearer " + jwt_owner)
                .expectBodyList(TeamDTO.class).returnResult().getResponseBody();
        assertNotNull(ownerTeams);
        assertEquals(2, ownerTeams.size());
    }

    private void leaveFromTeam(String teamId, String jwt){
        deleteRequest("/api/v1/team/leave/{teamId}", teamId, jwt).expectStatus().isOk();
    }

    private void checkSkills(List<UserDTO> users ,Integer checkNum){
        List<SkillDTO> empty = postRequest("api/v1/team/skills/users","Bearer " + jwt_owner)
                .body(Flux.fromIterable(users), UserDTO.class)
                .exchange()
                .expectBodyList(SkillDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(empty);
        assertEquals(checkNum, empty.size());
    }

    private void changeStatusRequest(String requestId, RequestStatus status, String jwt){
        webTestClient
                .put()
                .uri("/api/v1/team/request/{requestId}/update/{newStatus}",
                        requestId, status)
                .header("Authorization", jwt)
                .exchange()
                .expectStatus().isOk();
    }

    private void changeStatusRequestCheck(String requestId, RequestStatus status, String jwt, HttpStatus httpStatus){
        if (httpStatus == HttpStatus.FORBIDDEN) {
            assertThrows(AssertionError.class, () -> {
                changeStatusRequest(requestId,status,jwt);
            }, "Нет Прав");
            return;
        }
        changeStatusRequest(requestId,status,jwt);
    }

    private void threeRequestChangeStatus(String teamId, String jwt, HttpStatus status1,  HttpStatus status2,  HttpStatus status3){
        changeStatusRequestCheck(sendTeamRequest(teamId, randomUser, jwt_randomUser).getId(),
                RequestStatus.ACCEPTED, jwt, status1);
        changeStatusRequestCheck(sendTeamRequest(teamId, randomUser, jwt_randomUser).getId(),
                RequestStatus.CANCELED, jwt, status2);
        changeStatusRequestCheck(sendTeamRequest(teamId, randomUser, jwt_randomUser).getId(),
                RequestStatus.WITHDRAWN, jwt, status3);
    }

    private void changeStatusInvitation(String invitationId, RequestStatus status, String jwt){
        webTestClient
                .put()
                .uri("/api/v1/team/invitation/{invitationId}/update/{newStatus}",
                        invitationId, status)
                .header("Authorization", jwt)
                .exchange()
                .expectStatus().isOk();
    }

    private void changeStatusInvitationCheck(String invitationId, RequestStatus status, String jwt, HttpStatus httpStatus){
        if (httpStatus == HttpStatus.FORBIDDEN) {
            assertThrows(AssertionError.class, () -> {
                changeStatusInvitation(invitationId,status,jwt);
            }, "Нет Прав");
            return;
        }
        changeStatusInvitation(invitationId,status,jwt);
    }

    private void threeInvitationChangeStatus(List<TeamInvitation> invitations, String jwt, HttpStatus status1,  HttpStatus status2,  HttpStatus status3){
        changeStatusInvitationCheck(sendInvites(invitations).get(0).getId(),
                RequestStatus.ACCEPTED, jwt, status1);
        changeStatusInvitationCheck(sendInvites(invitations).get(0).getId(),
                RequestStatus.CANCELED, jwt, status2);
        changeStatusInvitationCheck(sendInvites(invitations).get(0).getId(),
                RequestStatus.WITHDRAWN, jwt, status3);
    }

    @BeforeAll
    public void setUp() {
        AuthenticationResponse response = register("admin.addSkill@gmail.com", "Admin", "Adminov", List.of(Role.ADMIN));
        AuthenticationResponse response1 = register("owner.team@gmail.com", "Owner", "Ownerov", List.of(Role.TEAM_OWNER));
        AuthenticationResponse response2 = register("leader.team@gmail.com", "Leader", "Leaderov", List.of(Role.TEAM_OWNER));
        AuthenticationResponse response3 = register("member.team@gmail.com", "Member", "Memberov", List.of(Role.MEMBER));
        AuthenticationResponse response4 = register("randomUser.team@gmail.com", "Random", "User", List.of(Role.MEMBER));
        AuthenticationResponse response5 = register("initiator.team@gmail.com", "Init", "Idea", List.of(Role.INITIATOR));
        AuthenticationResponse response6 = register("kostagarifullin275@gmail.com", "a", "a", List.of(Role.MEMBER));

        jwt_admin = response.getToken();
        jwt_owner = response1.getToken();
        jwt_leader = response2.getToken();
        jwt_member = response3.getToken();
        jwt_randomUser = response4.getToken();
        jwt_initiator = response5.getToken();
        jwt_kostya = response6.getToken();

        admin = userBuild(response.getId(), response.getEmail(), response.getFirstName(), response.getLastName(), response.getRoles());
        owner = userBuild(response1.getId(), response1.getEmail(), response1.getFirstName(), response1.getLastName(), response1.getRoles());
        leader = userBuild(response2.getId(), response2.getEmail(), response2.getFirstName(), response2.getLastName(), response2.getRoles());
        member = userBuild(response3.getId(), response3.getEmail(), response3.getFirstName(), response3.getLastName(), response3.getRoles());
        randomUser = userBuild(response4.getId(), response4.getEmail(), response4.getFirstName(), response4.getLastName(), response4.getRoles());
        initiator = userBuild(response5.getId(), response5.getEmail(), response5.getFirstName(), response5.getLastName(), response5.getRoles());
        kostya = userBuild(response6.getId(), response6.getEmail(), response6.getFirstName(), response6.getLastName(), response6.getRoles());

        groupExpert = createGroup(buildGroup("exp", List.of(Role.EXPERT)));
        groupProjectOffice = createGroup(buildGroup("pro", List.of(Role.PROJECT_OFFICE)));

        skill1 = createSkill("Java", SkillType.LANGUAGE);
        skill2 = createSkill("SpringBoot", SkillType.FRAMEWORK);
        skill3 = createSkill("JavaScript", SkillType.LANGUAGE);
        skill4 = createSkill("PSQL", SkillType.DATABASE);

        saveSkillToUser(List.of(skill1, skill2), jwt_leader);
        saveSkillToUser(List.of(skill3), jwt_member);
        saveSkillToUser(List.of(skill4), jwt_kostya);

        webTestClient = webTestClient.mutate()
                .responseTimeout(Duration.ofMillis(90000))
                .build();
    }

    ///////////////////////
    //  _____   ____ ______
    // / ___/  / __//_  __/
    /// (_ /  / _/   / /
    //\___/  /___/  /_/
    ///////////////////////

    @Test
    void testGetTeam() {
        String teamId1 = createTeam(List.of(leader)).getId();
        String teamId2 = createTeam(List.of(leader, member)).getId();
        assertEquals("Богатыри", getTeam(teamId1).getName());
        assertEquals("Слава Руси!", getTeam(teamId1).getDescription());
        assertFalse(getTeam(teamId1).getClosed());
        assertNotNull(getTeam(teamId1).getMembers());
        assertEquals(1, getTeam(teamId1).getMembersCount());
        assertEquals(2, getTeam(teamId2).getMembersCount());
        assertEquals(owner.getId(), getTeam(teamId1).getOwner().getId());
        assertNotEquals(leader.getId(), getTeam(teamId1).getOwner().getId());
        assertNotEquals(member.getId(), getTeam(teamId1).getOwner().getId());
        assertEquals(leader.getId(), getTeam(teamId1).getLeader().getId());
        assertNotEquals(owner.getId(), getTeam(teamId1).getLeader().getId());
        assertNotEquals(member.getId(), getTeam(teamId2).getLeader().getId());
    }

    @Test
    void testGetAllTeams() {
        createTeam(List.of(leader));
        createTeam(List.of(leader, member));

        List<TeamDTO> allTeams = getRequest("api/v1/team/all","Bearer " + jwt_randomUser)
                .expectBodyList(TeamDTO.class).returnResult().getResponseBody();
        assertNotNull(allTeams);
        assertTrue(allTeams.size() > 1);
    }

    @Test
    void testGetOwnerTeams() {
        String ideaMarketId = createMarketIdea().getId();

        createTeam(List.of(leader));
        createTeam(List.of(leader, member));

        getOwnerTeams(ideaMarketId);

        TeamDTO team3 = buildTeam("Не моя команда","Хз кто это",1,
                randomUser,owner,List.of(owner),List.of());
        postRequest("/api/v1/team/add","Bearer " + jwt_randomUser)
                .body(Mono.just(team3), TeamDTO.class)
                .exchange();

        getOwnerTeams(ideaMarketId);
    }

    @Test
    void testGetAllUsersWithSkills() {
        List<TeamMemberDTO> allUsersWithSkills = getRequest("api/v1/team/users","Bearer " + jwt_randomUser)
                .expectBodyList(TeamMemberDTO.class).returnResult().getResponseBody();
        assertNotNull(allUsersWithSkills);

        allUsersWithSkills.forEach(user -> {
            if (user.getUserId().equals(leader.getId()))
                assertEquals(2, user.getSkills().size());

            if (user.getUserId().equals(member.getId()))
                assertEquals(1, user.getSkills().size());

            if (user.getUserId().equals(kostya.getId()))
                assertEquals(1, user.getSkills().size());
        });
    }

    @Test
    void testGetInvitations() {
        sendInvites(List.of(buildInvitation(createTeam(List.of(leader)).getId(), kostya), buildInvitation(createTeam(List.of(leader, member)).getId(), kostya)));
        List<TeamInvitation> invitations = getRequest("api/v1/team/invites","Bearer " + jwt_kostya)
                .expectBodyList(TeamInvitation.class)
                .returnResult().getResponseBody();
        assertNotNull(invitations);
        assertTrue(invitations.size() >= 2);
    }

    @Test
    void testGetTeamRequests() {
        String teamId = createTeam(List.of(leader)).getId();

        sendTeamRequest(teamId, member, jwt_member);
        sendTeamRequest(teamId, randomUser, jwt_randomUser);

        List<TeamRequest> allTeamRequests = getRequest("api/v1/team/users/requests/{teamId}",
                teamId, "Bearer " + jwt_owner).expectBodyList(TeamRequest.class)
                .returnResult().getResponseBody();
        assertNotNull(allTeamRequests);
        assertTrue(allTeamRequests.size() > 1);
    }

    @Test
    void testGetInvitationsByTeam() {
        String teamId = createTeam(List.of(leader)).getId();
        sendInvites(List.of(buildInvitation(teamId, kostya), buildInvitation(teamId, randomUser)));
        List<TeamInvitation> invitationsByTeam = getRequest("api/v1/team/invitations/{teamId}",
                teamId,"Bearer " + jwt_owner).expectBodyList(TeamInvitation.class)
                .returnResult().getResponseBody();
        assertNotNull(invitationsByTeam);
        assertEquals(2, invitationsByTeam.size());
    }

    @Test
    void testGetTeamMarketRequests(){
        createMarketIdea();
        TeamDTO team = createTeam(List.of(kostya, member));
        List<IdeaMarketDTO> marketIdeas = getRequest("/api/v1/market/idea/all", "Bearer " + jwt_owner)
                .expectBodyList(IdeaMarketDTO.class).returnResult().getResponseBody();
        assertNotNull(marketIdeas);
        createMarketTeamRequest(marketIdeas.get(0).getId(), team);
        createMarketTeamRequest(marketIdeas.get(1).getId(), team);
        List<TeamMarketRequestDTO> requestDTOS = getRequest("/api/v1/team/idea/requests/{teamId}", team.getId(),
                "Bearer " + jwt_owner).expectBodyList(TeamMarketRequestDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(requestDTOS);
        assertTrue(requestDTOS.size() >= 2);
        assertNotEquals(team.getName(), requestDTOS.get(0).getName());
    }

    //////////////////////////////
    //   ___   ____    ____ ______
    //  / _ \ / __ \  / __//_  __/
    // / ___// /_/ / _\ \   / /
    ///_/    \____/ /___/  /_/
    //////////////////////////////

    @Test
    void testAddTeam() {
        createTeam(List.of(leader, member));
    }

    @Test
    void testGetTeamsBySkills() {
        TeamDTO team1 = buildTeam("Богатыри", "Слава Руси!",1,
                owner,leader,List.of(leader),List.of(skill3, skill4));
        postRequest("/api/v1/team/add", "Bearer " + jwt_owner)
                .body(Mono.just(team1), TeamDTO.class)
                .exchange();
        TeamDTO team2 = buildTeam("Богатыри", "Слава Руси!",1,
                owner,leader,List.of(member),List.of(skill1, skill4));
        postRequest("/api/v1/team/add", "Bearer " + jwt_owner)
                .body(Mono.just(team2), TeamDTO.class)
                .exchange();

        assertTrue(getTeamsBySkills(List.of(skill1), Role.INITIATOR, "Bearer " + jwt_initiator).size() >= 1);
        assertTrue(getTeamsBySkills(List.of(skill2), Role.INITIATOR, "Bearer " + jwt_initiator).size() >= 1);
        assertTrue(getTeamsBySkills(List.of(skill1, skill2), Role.INITIATOR, "Bearer " + jwt_initiator).size() >= 1);
        assertTrue(getTeamsBySkills(List.of(skill3), Role.INITIATOR, "Bearer " + jwt_initiator).size() >= 1);
        assertTrue(getTeamsBySkills(List.of(skill4), Role.INITIATOR, "Bearer " + jwt_initiator).size() >= 0);
        assertTrue(getTeamsBySkills(List.of(skill3, skill4), Role.INITIATOR, "Bearer " + jwt_initiator).size() >= 1);
        assertTrue(getTeamsBySkills(List.of(skill1, skill2, skill3, skill4), Role.INITIATOR, "Bearer " + jwt_initiator).size() >= 2);

        assertTrue(getTeamsBySkills(List.of(skill1), Role.MEMBER, "Bearer " + jwt_member).size() >= 2);
        assertTrue(getTeamsBySkills(List.of(skill2), Role.MEMBER, "Bearer " + jwt_member).size() >= 1);
        assertTrue(getTeamsBySkills(List.of(skill1, skill2), Role.MEMBER, "Bearer " + jwt_member).size() >= 2);
        assertTrue(getTeamsBySkills(List.of(skill3), Role.MEMBER, "Bearer " + jwt_member).size() >= 2);
        assertTrue(getTeamsBySkills(List.of(skill4), Role.MEMBER, "Bearer " + jwt_member).size() >= 2);
        assertTrue(getTeamsBySkills(List.of(skill3, skill4), Role.MEMBER, "Bearer " + jwt_member).size() >= 2);
        assertTrue(getTeamsBySkills(List.of(skill1, skill2, skill3, skill4), Role.MEMBER, "Bearer " + jwt_member).size() >= 2);
    }

    @Test
    void testGetTeamsByVacancies() {
        TeamDTO team = buildTeam("Богатыри","Слава Руси!",1,
                owner,leader,List.of(leader),List.of(skill3, skill4));
        createTeamRequest(team);
        assertEquals(0, getTeamsByVacancies(List.of(skill1, skill2), "Bearer " + jwt_owner).size());
        assertEquals(0, getTeamsByVacancies(List.of(skill1), "Bearer " + jwt_leader).size());
        assertEquals(0, getTeamsByVacancies(List.of(skill2), "Bearer " + jwt_member).size());
        assertEquals(1, getTeamsByVacancies(List.of(skill3), "Bearer " + jwt_randomUser).size());
        assertEquals(1, getTeamsByVacancies(List.of(skill4), "Bearer " + jwt_owner).size());
        assertEquals(1, getTeamsByVacancies(List.of(skill3, skill4), "Bearer " + jwt_leader).size());
    }

    @Test
    void testSendTeamRequest() {
        String teamId = createTeam(List.of(leader)).getId();
        TeamRequest request = sendTeamRequest(teamId, randomUser, jwt_randomUser);
        assertEquals(request.getTeamId(), teamId);
        assertEquals(request.getUserId(), randomUser.getId());
        assertEquals(request.getEmail(), randomUser.getEmail());
        assertEquals(request.getFirstName(), randomUser.getFirstName());
        assertEquals(request.getLastName(), randomUser.getLastName());
        assertEquals(RequestStatus.NEW, request.getStatus());
    }

    @Test
    void testSendInvitesToUsers() {
        assertNotNull(sendInvites(List.of(buildInvitation(createTeam(List.of(leader)).getId(), kostya))).get(0).getUserId());
    }

    @Test
    void testInviteUserInTeam() {
        String teamId = createTeam(List.of(leader)).getId();
        assertEquals(1, getTeam(teamId).getMembersCount());
        assertEquals(randomUser.getId(), inviteUserInTeam(teamId, randomUser.getId()).getUserId());
        assertEquals(2, getTeam(teamId).getMembersCount());
    }

    @Test
    void testGetSkillsByUsers() {
        checkSkills(List.of(admin, owner), 0);
        checkSkills(List.of(leader, member), 3);
        checkSkills(List.of(leader, member, kostya), 4);
    }

    @Test
    void testGetSkillsByInvitations() {
        String teamId = createTeam(List.of(leader)).getId();
        TeamInvitation teamMember1 = buildInvitation(teamId, member);
        TeamInvitation teamMember2 = buildInvitation(teamId, randomUser);
        assertEquals(1, getSkillsByInvitations(sendInvites(List.of(teamMember1))).size());
        assertEquals(0, getSkillsByInvitations(sendInvites(List.of(teamMember2))).size());
        assertEquals(1, getSkillsByInvitations(sendInvites(List.of(buildInvitation(teamId, kostya)))).size());
        assertEquals(1, getSkillsByInvitations(sendInvites(List.of(teamMember1, teamMember2))).size());
    }

    @Test
    void testGetSkillsByRequests() {
        String teamId = createTeam(List.of(leader)).getId();
        assertEquals(0, getSkillsByRequests(List.of(sendTeamRequest(teamId, randomUser, jwt_randomUser))).size());
        assertEquals(1, getSkillsByRequests(List.of(sendTeamRequest(teamId, kostya, jwt_kostya))).size());
    }

    ///////////////////////////////////////////
    //   ___    ____   __    ____ ______   ____
    //  / _ \  / __/  / /   / __//_  __/  / __/
    // / // / / _/   / /__ / _/   / /    / _/
    ///____/ /___/  /____//___/  /_/    /___/
    ///////////////////////////////////////////

    @Test
    void testDeleteTeam() {
        InfoResponse response1 = deleteRequest("/api/v1/team/delete/{teamId}",
                createTeam(List.of(leader)).getId(),"Bearer " + jwt_owner)
                .expectBody(InfoResponse.class)
                .returnResult().getResponseBody();
        assertNotNull(response1);
        assertEquals("Успешное удаление", response1.getMessage());
        InfoResponse response2 = deleteRequest("/api/v1/team/delete/{teamId}",
                createTeam(List.of(leader)).getId(),"Bearer " + jwt_leader)
                .expectBody(InfoResponse.class)
                .returnResult().getResponseBody();
        assertNotNull(response2);
        assertEquals("Ошибка при удалении", response2.getMessage());

        String teamId = createTeam(List.of(leader, member)).getId();
        deleteRequest("/api/v1/team/delete/{teamId}", teamId,"Bearer " + jwt_member).expectStatus().isForbidden();
        deleteRequest("/api/v1/team/delete/{teamId}", teamId,"Bearer " + jwt_randomUser).expectStatus().isForbidden();
    }

    @Test
    void testKickUserFromTeam() {
        String teamId = createTeam(List.of(leader, member)).getId();
        assertEquals(2, getTeam(teamId).getMembersCount());
        kickUserFromTeam(teamId, member.getId());
        assertEquals(1, getTeam(teamId).getMembersCount());
    }

    @Test
    void testLeaveFromTeam() {
        String teamId = createTeam(List.of(leader, member, randomUser)).getId();
        assertEquals(3, getTeam(teamId).getMembersCount());

        leaveFromTeam(teamId, "Bearer " + jwt_member);
        assertEquals(2, getTeam(teamId).getMembersCount());

        leaveFromTeam(teamId, "Bearer " + jwt_randomUser);
        assertEquals(1, getTeam(teamId).getMembersCount());

        leaveFromTeam(teamId, "Bearer " + jwt_leader);
        assertEquals(0, getTeam(teamId).getMembersCount());
    }

    ////////////////////////
    //   ___   __  __ ______
    //  / _ \ / / / //_  __/
    // / ___// /_/ /  / /
    ///_/    \____/  /_/
    ////////////////////////

    @Test
    void testUpdateTeam() {
        TeamDTO team = createTeam(List.of(leader));
        assertEquals("Ящеры", updateTeam(team.getId(), List.of(leader, member)).getName());
        assertEquals("Слава Подикам!", updateTeam(team.getId(), List.of(leader, member)).getDescription());
        assertTrue(updateTeam(team.getId(), List.of(leader, member)).getClosed());
        assertNotNull(updateTeam(team.getId(), List.of(leader, member)).getMembers());
        assertEquals(2, updateTeam(team.getId(), List.of(leader, member)).getMembersCount());
        assertEquals(3, updateTeam(team.getId(), List.of(leader, member, randomUser)).getMembersCount());
        assertEquals(team.getOwner().getId(), updateTeam(team.getId(), List.of(leader)).getOwner().getId());
        assertEquals(team.getLeader().getId(), updateTeam(team.getId(), List.of(leader)).getLeader().getId());
    }

    @Test
    void testUpdateTeamSkills() {
        TeamDTO team = createTeam(List.of(leader));
        Flux<SkillDTO> skills = Flux.just(skill3, skill4);

        updateSkills(team.getId(), skills, "Bearer " + jwt_owner).expectStatus().isOk();
        assertTrue(team.getWantedSkills().size() < getTeam(team.getId()).getWantedSkills().size());

        InfoResponse response = updateSkills(team.getId(), skills, "Bearer " + jwt_leader)
                .expectBody(InfoResponse.class).returnResult().getResponseBody();
        assertNotNull(response);
        assertSame(HttpStatus.BAD_REQUEST, response.getStatusCode());

        updateSkills(team.getId(), skills, "Bearer " + jwt_randomUser).expectStatus().isForbidden();
    }

    @Test
    void testChangeLeader() {
        TeamDTO team1 = createTeam(List.of(leader, member));
        changeLeader(team1.getId(), randomUser.getId(), "Bearer " + jwt_owner)
                .expectStatus().isOk();
        assertNotEquals(team1.getLeader().getId(), getTeam(team1.getId()).getLeader().getId());

        TeamDTO team2 = createTeam(List.of(leader, member));
        InfoResponse response = changeLeader(team2.getId(), randomUser.getId(), "Bearer " + jwt_leader)
                .expectBody(InfoResponse.class).returnResult().getResponseBody();
        assertNotNull(response);
        assertSame(HttpStatus.BAD_REQUEST, response.getStatusCode());
        assertEquals(team2.getLeader().getId(), getTeam(team2.getId()).getLeader().getId());

        TeamDTO team3 = createTeam(List.of(leader, member));
        changeLeader(team3.getId(), member.getId(), "Bearer " + jwt_member)
                .expectStatus().isForbidden();
        assertEquals(team3.getLeader().getId(), getTeam(team3.getId()).getLeader().getId());

        TeamDTO team4 = createTeam(List.of(leader, member));
        changeLeader(team4.getId(), randomUser.getId(), "Bearer " + jwt_randomUser)
                .expectStatus().isForbidden();
        assertEquals(team4.getLeader().getId(), getTeam(team4.getId()).getLeader().getId());
    }

    @Test
    void testUpdateTeamRequestStatus() {
        String teamId = createTeam(List.of(leader, member)).getId();
        threeRequestChangeStatus(teamId, "Bearer " + jwt_randomUser,HttpStatus.FORBIDDEN, HttpStatus.FORBIDDEN, HttpStatus.OK);
        threeRequestChangeStatus(teamId, "Bearer " + jwt_owner,HttpStatus.OK, HttpStatus.OK, HttpStatus.FORBIDDEN);
        threeRequestChangeStatus(teamId, "Bearer " + jwt_leader,HttpStatus.FORBIDDEN, HttpStatus.FORBIDDEN, HttpStatus.FORBIDDEN);
        threeRequestChangeStatus(teamId, "Bearer " + jwt_member,HttpStatus.FORBIDDEN, HttpStatus.FORBIDDEN, HttpStatus.FORBIDDEN);
        threeRequestChangeStatus(teamId, "Bearer " + jwt_kostya,HttpStatus.FORBIDDEN, HttpStatus.FORBIDDEN, HttpStatus.FORBIDDEN);
    }

    @Test
    void testUpdateInvitationStatus() {
        TeamInvitation invitation = buildInvitation(createTeam(List.of(leader, member)).getId(),kostya);
        threeInvitationChangeStatus(List.of(invitation),"Bearer " + jwt_kostya, HttpStatus.OK, HttpStatus.OK, HttpStatus.FORBIDDEN);
        threeInvitationChangeStatus(List.of(invitation),"Bearer " + jwt_owner, HttpStatus.FORBIDDEN, HttpStatus.FORBIDDEN, HttpStatus.OK);
        threeInvitationChangeStatus(List.of(invitation),"Bearer " + jwt_leader, HttpStatus.FORBIDDEN, HttpStatus.FORBIDDEN, HttpStatus.FORBIDDEN);
        threeInvitationChangeStatus(List.of(invitation),"Bearer " + jwt_member, HttpStatus.FORBIDDEN, HttpStatus.FORBIDDEN, HttpStatus.FORBIDDEN);
        threeInvitationChangeStatus(List.of(invitation),"Bearer " + jwt_randomUser, HttpStatus.FORBIDDEN, HttpStatus.FORBIDDEN, HttpStatus.FORBIDDEN);
    }
}
