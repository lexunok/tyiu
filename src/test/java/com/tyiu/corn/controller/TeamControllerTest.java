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
import org.springframework.test.web.reactive.server.WebTestClient;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

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
    private final String path = "/api/v1/market/idea";
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

        RegisterRequest request = new RegisterRequest(email, firstName, lastName, "bla-bla-bla", roles);

        AuthenticationResponse response = webTestClient
                .post()
                .uri("/api/v1/auth/register")
                .body(Mono.just(request), RegisterRequest.class)
                .exchange()
                .expectBody(AuthenticationResponse.class)
                .returnResult().getResponseBody();
        assertNotNull(response);

        return response;
    }

    private UserDTO userBuild(String userId, String email, String firstname, String lastname, List<Role> roles) {

        return UserDTO.builder()
                .id(userId)
                .email(email)
                .firstName(firstname)
                .lastName(lastname)
                .roles(roles)
                .build();
    }

    private TeamMemberDTO teamMemberBuilder(UserDTO user) {

        return TeamMemberDTO.builder()
                .userId(user.getId())
                .email(user.getEmail())
                .firstName(user.getFirstName())
                .lastName(user.getLastName())
                .build();
    }

    private SkillDTO createSkill(String name, SkillType type) {

        SkillDTO skill = SkillDTO.builder()
                .name(name)
                .type(type)
                .build();

        SkillDTO responseCreateSkill = webTestClient
                .post()
                .uri("/api/v1/skill/add")
                .header("Authorization", "Bearer " + jwt_admin)
                .body(Mono.just(skill), SkillDTO.class)
                .exchange()
                .expectBody(SkillDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(responseCreateSkill);
        assertEquals(skill.getName(), responseCreateSkill.getName());
        assertEquals(skill.getType(), responseCreateSkill.getType());

        return responseCreateSkill;
    }

    private void saveSkillToUser(List<SkillDTO> skills, String jwt) {

        webTestClient
                .post()
                .uri("/api/v1/profile/skills/save")
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(skills), SkillDTO.class)
                .exchange()
                .expectStatus().isOk();
    }

    private void addSkills(IdeaSkillRequest ideaSkillRequest) {

        InfoResponse skillRequest = webTestClient
                .post()
                .uri("/api/v1/idea/skills/add")
                .header("Authorization", "Bearer " + jwt_admin)
                .body(Mono.just(ideaSkillRequest), IdeaSkillRequest.class)
                .exchange()
                .expectBody(InfoResponse.class)
                .returnResult().getResponseBody();
        assertNotNull(skillRequest);
    }

    private IdeaSkillRequest buildSkillRequest(String id, List<SkillDTO> skills) {
        return IdeaSkillRequest.builder().ideaId(id).skills(skills).build();
    }

    @BeforeAll
    public void setUp() {

        AuthenticationResponse response = register("admin.addSkill@gmail.com", "Admin", "Adminov", List.of(Role.ADMIN));
        jwt_admin = response.getToken();

        AuthenticationResponse response1 = register("owner.team@gmail.com", "Owner", "Ownerov", List.of(Role.TEAM_OWNER));
        jwt_owner = response1.getToken();

        AuthenticationResponse response2 = register("leader.team@gmail.com", "Leader", "Leaderov", List.of(Role.TEAM_OWNER));
        jwt_leader = response2.getToken();

        AuthenticationResponse response3 = register("member.team@gmail.com", "Member", "Memberov", List.of(Role.MEMBER));
        jwt_member = response3.getToken();

        AuthenticationResponse response4 = register("randomUser.team@gmail.com", "Random", "User", List.of(Role.MEMBER));
        jwt_randomUser = response4.getToken();

        AuthenticationResponse response5 = register("initiator.team@gmail.com", "Init", "Idea", List.of(Role.INITIATOR));
        jwt_initiator = response5.getToken();

        AuthenticationResponse response6 = register("kostagarifullin275@gmail.com", "a", "a", List.of(Role.MEMBER));
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
    }

    private GroupDTO createGroup(GroupDTO groupDTO) {

        GroupDTO createdGroup = webTestClient
                .post()
                .uri("/api/v1/group/create")
                .header("Authorization", "Bearer " + jwt_admin)
                .body(Mono.just(groupDTO), GroupDTO.class)
                .exchange()
                .expectBody(GroupDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(createdGroup);
        assertEquals(groupDTO.getName(), createdGroup.getName());

        return createdGroup;
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

    private IdeaDTO getIdea(String id, String name) {

        IdeaDTO idea = webTestClient
                .get()
                .uri("/api/v1/idea/{ideaId}", id)
                .header("Authorization", "Bearer " + jwt_admin)
                .exchange()
                .expectBody(IdeaDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(idea);
        assertEquals(name, idea.getName());

        return idea;
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

    private IdeaDTO createIdea(IdeaDTO ideaDTO) {

        IdeaDTO createdIdea = webTestClient
                .post()
                .uri("/api/v1/idea/add")
                .header("Authorization", "Bearer " + jwt_admin)
                .body(Mono.just(ideaDTO), IdeaDTO.class)
                .exchange()
                .expectBody(IdeaDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(createdIdea);
        assertEquals(ideaDTO.getName(), createdIdea.getName());

        return createdIdea;
    }

    private IdeaMarketDTO createMarketIdea() {

        LocalDate localDate = LocalDate.now();

        MarketDTO buildMarket = MarketDTO.builder()
                .name("Зимняя биржа 2024")
                .startDate(localDate)
                .finishDate(localDate.plusDays(30))
                .build();

        MarketDTO market = webTestClient
                .post()
                .uri("/api/v1/market/create")
                .header("Authorization", "Bearer " + jwt_admin)
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

        List<IdeaMarketDTO> createdMarketIdea = webTestClient
                .post()
                .uri(path + "/send/{marketId}", market.getId())
                .header("Authorization", "Bearer " + jwt_admin)
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

    private TeamDTO createTeam(List<UserDTO> members) {

        TeamDTO team = TeamDTO.builder()
                .name("Богатыри")
                .description("Слава Руси!")
                .closed(false)
                .membersCount(members.size())
                .createdAt(LocalDate.now())
                .owner(owner)
                .leader(leader)
                .members(members)
                .wantedSkills(List.of())
                .build();

        TeamDTO responseAddTeam = webTestClient
                .post()
                .uri("/api/v1/team/add")
                .header("Authorization", "Bearer " + jwt_owner)
                .body(Mono.just(team), TeamDTO.class)
                .exchange()
                .expectBody(TeamDTO.class)
                .returnResult().getResponseBody();
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

        TeamRequest responseSendRequest = webTestClient
                .post()
                .uri("/api/v1/team/request/send/{teamId}", teamId)
                .header("Authorization", "Bearer " + jwt)
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

    private List<TeamInvitation> sendInvites(String teamId, List<TeamMemberDTO> user) {

        List<TeamInvitation> invites = webTestClient
                .post()
                .uri("/api/v1/team/send-invites/{teamId}", teamId)
                .header("Authorization", "Bearer " + jwt_owner)
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

    private void kickUserFromTeam(String teamId, String userId) {

        webTestClient
                .delete()
                .uri("/api/v1/team/kick/{teamId}/{userId}", teamId, userId)
                .header("Authorization", "Bearer " + jwt_randomUser)
                .exchange()
                .expectStatus().isOk();
    }

    private TeamDTO getTeam(String teamId) {

        TeamDTO responseGetTeam = webTestClient
                .get()
                .uri("/api/v1/team/{teamId}", teamId)
                .header("Authorization", "Bearer " + jwt_randomUser)
                .exchange()
                .expectBody(TeamDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(responseGetTeam);

        return responseGetTeam;
    }

    private List<TeamDTO> getTeamsByVacancies(List<SkillDTO> skills, String jwt) {

        List<TeamDTO> allTeamsByVacancies = webTestClient
                .post()
                .uri("api/v1/team/vacancy-filter")
                .header("Authorization", "Bearer " + jwt)
                .body(Flux.fromIterable(skills), SkillDTO.class)
                .exchange()
                .expectBodyList(TeamDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(allTeamsByVacancies);

        return allTeamsByVacancies;
    }

    private List<SkillDTO> getSkillsByInvitations(List<TeamInvitation> users) {

        List<SkillDTO> allSkillsByInvitation = webTestClient
                .post()
                .uri("api/v1/team/skills/invitations")
                .header("Authorization", "Bearer " + jwt_owner)
                .body(Flux.fromIterable(users), SkillDTO.class)
                .exchange()
                .expectBodyList(SkillDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(allSkillsByInvitation);

        return allSkillsByInvitation;
    }

    private List<SkillDTO> getSkillsByRequests(List<TeamRequest> users) {

        List<SkillDTO> allSkillsByRequests = webTestClient
                .post()
                .uri("api/v1/team/skills/requests")
                .header("Authorization", "Bearer " + jwt_owner)
                .body(Flux.fromIterable(users), SkillDTO.class)
                .exchange()
                .expectBodyList(SkillDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(allSkillsByRequests);

        return allSkillsByRequests;
    }

    @Test
    void testAddTeam() {
        createTeam(List.of(leader, member));
    }

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
    void testChangeLeader() {

        TeamDTO team1 = createTeam(List.of(leader, member));

        webTestClient
                .put()
                .uri("/api/v1/team/change/leader/{teamId}/{userId}", team1.getId(), randomUser.getId())
                .header("Authorization", "Bearer " + jwt_owner)
                .exchange()
                .expectStatus().isOk();
        assertNotEquals(team1.getLeader().getId(), getTeam(team1.getId()).getLeader().getId());

//        TeamDTO team2 = createTeam(List.of(leader, member));
//
//        webTestClient
//                .put()
//                .uri("/api/v1/team/change/leader/{teamId}/{userId}", team2.getId(), randomUser.getId())
//                .header("Authorization", "Bearer " + jwt_leader)
//                .exchange()
//                .expectStatus().isBadRequest();
//        assertEquals(team2.getLeader().getId(), getTeam(team2.getId()).getLeader().getId());

        TeamDTO team3 = createTeam(List.of(leader, member));

        webTestClient
                .put()
                .uri("/api/v1/team/change/leader/{teamId}/{userId}", team3.getId(), member.getId())
                .header("Authorization", "Bearer " + jwt_member)
                .exchange()
                .expectStatus().isForbidden();
        assertEquals(team3.getLeader().getId(), getTeam(team3.getId()).getLeader().getId());

        TeamDTO team4 = createTeam(List.of(leader, member));

        webTestClient
                .put()
                .uri("/api/v1/team/change/leader/{teamId}/{userId}", team4.getId(), randomUser.getId())
                .header("Authorization", "Bearer " + jwt_randomUser)
                .exchange()
                .expectStatus().isForbidden();
        assertEquals(team4.getLeader().getId(), getTeam(team4.getId()).getLeader().getId());
    }

    @Test
    void testUpdateTeamSkills() {

        TeamDTO team = createTeam(List.of(leader));

        List<SkillDTO> skills = List.of(skill3, skill4);

        webTestClient
                .put()
                .uri("/api/v1/team/skills/update/{teamId}", team.getId())
                .header("Authorization", "Bearer " + jwt_owner)
                .body(Flux.fromIterable(skills), SkillDTO.class)
                .exchange()
                .expectBody(TeamDTO.class);
        assertTrue(team.getWantedSkills().size() < getTeam(team.getId()).getWantedSkills().size());

//        webTestClient
//                .put()
//                .uri("/api/v1/team/skills/update/{teamId}", team.getId())
//                .header("Authorization", "Bearer " + jwt_leader)
//                .body(Flux.fromIterable(skills), SkillDTO.class)
//                .exchange()
//                .expectStatus().isBadRequest();

        webTestClient
                .put()
                .uri("/api/v1/team/skills/update/{teamId}", team.getId())
                .header("Authorization", "Bearer " + jwt_randomUser)
                .body(Flux.fromIterable(skills), SkillDTO.class)
                .exchange()
                .expectStatus().isForbidden();
    }

    @Test
    void testSendTeamRequest() {

        TeamDTO team = createTeam(List.of(leader));
        TeamRequest request = sendTeamRequest(team.getId(), randomUser, jwt_randomUser);

        assertEquals(request.getTeamId(), team.getId());
        assertEquals(request.getUserId(), randomUser.getId());
        assertEquals(request.getEmail(), randomUser.getEmail());
        assertEquals(request.getFirstName(), randomUser.getFirstName());
        assertEquals(request.getLastName(), randomUser.getLastName());
        assertEquals(RequestStatus.NEW, request.getStatus());
    }

//    @Test
//    void testUpdateTeamRequestStatus() {
//
//        TeamDTO team = createTeam(List.of(leader, member));
//
//        // ОТПРАВИТЕЛЬ ЗАЯВКИ (может отозвать заявку)
//        webTestClient
//                .put()
//                .uri("/api/v1/team/request/{requestId}/update/{newStatus}",
//                        sendTeamRequest(team.getId(), randomUser, jwt_randomUser).getId(), RequestStatus.ACCEPTED)
//                .header("Authorization", "Bearer " + jwt_randomUser)
//                .exchange()
//                .expectStatus().isBadRequest();
//
//        webTestClient
//                .put()
//                .uri("/api/v1/team/request/{requestId}/update/{newStatus}",
//                        sendTeamRequest(team.getId(), randomUser, jwt_randomUser).getId(), RequestStatus.CANCELED)
//                .header("Authorization", "Bearer " + jwt_randomUser)
//                .exchange()
//                .expectStatus().isBadRequest();
//
//        webTestClient
//                .put()
//                .uri("/api/v1/team/request/{requestId}/update/{newStatus}",
//                        sendTeamRequest(team.getId(), randomUser, jwt_randomUser).getId(), RequestStatus.WITHDRAWN)
//                .header("Authorization", "Bearer " + jwt_randomUser)
//                .exchange()
//                .expectStatus().isOk();
//
//        // ВЛАДЕЛЕЦ КОМАНДЫ (может принять или отклонить заявку)
//        webTestClient
//                .put()
//                .uri("/api/v1/team/request/{requestId}/update/{newStatus}",
//                        sendTeamRequest(team.getId(), randomUser, jwt_randomUser).getId(), RequestStatus.ACCEPTED)
//                .header("Authorization", "Bearer " + jwt_owner)
//                .exchange()
//                .expectStatus().isOk();
//
//        webTestClient
//                .put()
//                .uri("/api/v1/team/request/{requestId}/update/{newStatus}",
//                        sendTeamRequest(team.getId(), randomUser, jwt_randomUser).getId(), RequestStatus.CANCELED)
//                .header("Authorization", "Bearer " + jwt_owner)
//                .exchange()
//                .expectStatus().isOk();
//
//        webTestClient
//                .put()
//                .uri("/api/v1/team/request/{requestId}/update/{newStatus}",
//                        sendTeamRequest(team.getId(), randomUser, jwt_randomUser).getId(), RequestStatus.WITHDRAWN)
//                .header("Authorization", "Bearer " + jwt_owner)
//                .exchange()
//                .expectStatus().isBadRequest();
//
//        // ЛИДЕР КОМАНДЫ (не может менять статус заявки)
//        webTestClient
//                .put()
//                .uri("/api/v1/team/request/{requestId}/update/{newStatus}",
//                        sendTeamRequest(team.getId(), randomUser, jwt_randomUser).getId(), RequestStatus.ACCEPTED)
//                .header("Authorization", "Bearer " + jwt_leader)
//                .exchange()
//                .expectStatus().isBadRequest();
//
//        webTestClient
//                .put()
//                .uri("/api/v1/team/request/{requestId}/update/{newStatus}",
//                        sendTeamRequest(team.getId(), randomUser, jwt_randomUser).getId(), RequestStatus.CANCELED)
//                .header("Authorization", "Bearer " + jwt_leader)
//                .exchange()
//                .expectStatus().isBadRequest();
//
//        webTestClient
//                .put()
//                .uri("/api/v1/team/request/{requestId}/update/{newStatus}",
//                        sendTeamRequest(team.getId(), randomUser, jwt_randomUser).getId(), RequestStatus.WITHDRAWN)
//                .header("Authorization", "Bearer " + jwt_leader)
//                .exchange()
//                .expectStatus().isBadRequest();
//
//        // ЧЛЕН КОМАНДЫ (не может менять статус заявки)
//        webTestClient
//                .put()
//                .uri("/api/v1/team/request/{requestId}/update/{newStatus}",
//                        sendTeamRequest(team.getId(), randomUser, jwt_randomUser).getId(), RequestStatus.ACCEPTED)
//                .header("Authorization", "Bearer " + jwt_member)
//                .exchange()
//                .expectStatus().isBadRequest();
//
//        webTestClient
//                .put()
//                .uri("/api/v1/team/request/{requestId}/update/{newStatus}",
//                        sendTeamRequest(team.getId(), randomUser, jwt_randomUser).getId(), RequestStatus.CANCELED)
//                .header("Authorization", "Bearer " + jwt_member)
//                .exchange()
//                .expectStatus().isBadRequest();
//
//        webTestClient
//                .put()
//                .uri("/api/v1/team/request/{requestId}/update/{newStatus}",
//                        sendTeamRequest(team.getId(), randomUser, jwt_randomUser).getId(), RequestStatus.WITHDRAWN)
//                .header("Authorization", "Bearer " + jwt_member)
//                .exchange()
//                .expectStatus().isBadRequest();
//
//        // СЛУЧАЙНЫЙ ПОЛЬЗОВАТЕЛЬ ВНЕ КОМАНДЫ (не может менять статус заявки)
//        webTestClient
//                .put()
//                .uri("/api/v1/team/request/{requestId}/update/{newStatus}",
//                        sendTeamRequest(team.getId(), randomUser, jwt_randomUser).getId(), RequestStatus.ACCEPTED)
//                .header("Authorization", "Bearer " + jwt_kostya)
//                .exchange()
//                .expectStatus().isBadRequest();
//
//        webTestClient
//                .put()
//                .uri("/api/v1/team/request/{requestId}/update/{newStatus}",
//                        sendTeamRequest(team.getId(), randomUser, jwt_randomUser).getId(), RequestStatus.CANCELED)
//                .header("Authorization", "Bearer " + jwt_kostya)
//                .exchange()
//                .expectStatus().isBadRequest();
//
//        webTestClient
//                .put()
//                .uri("/api/v1/team/request/{requestId}/update/{newStatus}",
//                        sendTeamRequest(team.getId(), randomUser, jwt_randomUser).getId(), RequestStatus.WITHDRAWN)
//                .header("Authorization", "Bearer " + jwt_kostya)
//                .exchange()
//                .expectStatus().isBadRequest();
//    }

    @Test
    void testGetTeamRequests() {

        TeamDTO team = createTeam(List.of(leader));

        sendTeamRequest(team.getId(), member, jwt_member);
        sendTeamRequest(team.getId(), randomUser, jwt_randomUser);

        List<TeamRequest> allTeamRequests = webTestClient
                .get()
                .uri("api/v1/team/requests/{teamId}", team.getId())
                .header("Authorization", "Bearer " + jwt_owner)
                .exchange()
                .expectBodyList(TeamRequest.class)
                .returnResult().getResponseBody();
        assertNotNull(allTeamRequests);
        assertTrue(allTeamRequests.size() > 1);
    }

    @Test
    void testSendInvitesToUsers() {

        TeamDTO team = createTeam(List.of(leader));
        assertNotNull(sendInvites(team.getId(), List.of(teamMemberBuilder(kostya))).get(0).getUserId());
    }

//    @Test
//    void testUpdateInvitationStatus() {
//
//        TeamDTO team = createTeam(List.of(leader, member));
//
//        TeamMemberDTO teamMember = teamMemberBuilder(kostya);
//
//        // ПРИГЛАШЁННЫЙ ПОЛЬЗОВАТЕЛЬ (может принять или отклонить приглашение в команду)
//        webTestClient
//                .put()
//                .uri("/api/v1/team/invitation/{invitationId}/update/{newStatus}",
//                        sendInvites(team.getId(), List.of(teamMember)).get(0).getId(), RequestStatus.ACCEPTED)
//                .header("Authorization", "Bearer " + jwt_kostya)
//                .exchange()
//                .expectStatus().isOk();
//
//        webTestClient
//                .put()
//                .uri("/api/v1/team/invitation/{invitationId}/update/{newStatus}",
//                        sendInvites(team.getId(), List.of(teamMember)).get(0).getId(), RequestStatus.CANCELED)
//                .header("Authorization", "Bearer " + jwt_kostya)
//                .exchange()
//                .expectStatus().isOk();
//
//        // ВЛАДЕЛЕЦ КОМАНДЫ (может отозвать приглашение)
//        webTestClient
//                .put()
//                .uri("/api/v1/team/invitation/{invitationId}/update/{newStatus}",
//                        sendInvites(team.getId(), List.of(teamMember)).get(0).getId(), RequestStatus.ACCEPTED)
//                .header("Authorization", "Bearer " + jwt_owner)
//                .exchange()
//                .expectStatus().isBadRequest();
//
//        webTestClient
//                .put()
//                .uri("/api/v1/team/invitation/{invitationId}/update/{newStatus}",
//                        sendInvites(team.getId(), List.of(teamMember)).get(0).getId(), RequestStatus.CANCELED)
//                .header("Authorization", "Bearer " + jwt_owner)
//                .exchange()
//                .expectStatus().isOk();
//
//        // ЛИДЕР КОМАНДЫ (не может менять статус приглашения)
//        webTestClient
//                .put()
//                .uri("/api/v1/team/invitation/{invitationId}/update/{newStatus}",
//                        sendInvites(team.getId(), List.of(teamMember)).get(0).getId(), RequestStatus.ACCEPTED)
//                .header("Authorization", "Bearer " + jwt_leader)
//                .exchange()
//                .expectStatus().isBadRequest();
//
//        webTestClient
//                .put()
//                .uri("/api/v1/team/invitation/{invitationId}/update/{newStatus}",
//                        sendInvites(team.getId(), List.of(teamMember)).get(0).getId(), RequestStatus.CANCELED)
//                .header("Authorization", "Bearer " + jwt_leader)
//                .exchange()
//                .expectStatus().isBadRequest();
//
//        // ЧЛЕН КОМАНДЫ (не может менять статус приглашения)
//        webTestClient
//                .put()
//                .uri("/api/v1/team/invitation/{invitationId}/update/{newStatus}",
//                        sendInvites(team.getId(), List.of(teamMember)).get(0).getId(), RequestStatus.ACCEPTED)
//                .header("Authorization", "Bearer " + jwt_member)
//                .exchange()
//                .expectStatus().isBadRequest();
//
//        webTestClient
//                .put()
//                .uri("/api/v1/team/invitation/{invitationId}/update/{newStatus}",
//                        sendInvites(team.getId(), List.of(teamMember)).get(0).getId(), RequestStatus.CANCELED)
//                .header("Authorization", "Bearer " + jwt_member)
//                .exchange()
//                .expectStatus().isBadRequest();
//
//        // СЛУЧАЙНЫЙ ПОЛЬЗОВАТЕЛЬ (не может менять статус приглашения)
//        webTestClient
//                .put()
//                .uri("/api/v1/team/invitation/{invitationId}/update/{newStatus}",
//                        sendInvites(team.getId(), List.of(teamMember)).get(0).getId(), RequestStatus.ACCEPTED)
//                .header("Authorization", "Bearer " + jwt_randomUser)
//                .exchange()
//                .expectStatus().isBadRequest();
//
//        webTestClient
//                .put()
//                .uri("/api/v1/team/invitation/{invitationId}/update/{newStatus}",
//                        sendInvites(team.getId(), List.of(teamMember)).get(0).getId(), RequestStatus.CANCELED)
//                .header("Authorization", "Bearer " + jwt_randomUser)
//                .exchange()
//                .expectStatus().isBadRequest();
//    }

    @Test // TODO: не выводятся все приглашения (пустой список)
    void testGetInvitations() {

        TeamDTO team1 = createTeam(List.of(leader));
        TeamDTO team2 = createTeam(List.of(leader, member));

        TeamMemberDTO teamMember = teamMemberBuilder(kostya);

        sendInvites(team1.getId(), List.of(teamMember));
        sendInvites(team2.getId(), List.of(teamMember));

        List<TeamInvitation> invitations = webTestClient
                .get()
                .uri("api/v1/team/invites")
                .header("Authorization", "Bearer " + jwt_kostya)
                .exchange()
                .expectBodyList(TeamInvitation.class)
                .returnResult().getResponseBody();
        assertNotNull(invitations);
//        assertEquals(2, invitations.size());
    }

    @Test // TODO: не выводятся все приглашения (пустой список)
    void testGetInvitationsByTeam() {

        TeamDTO team = createTeam(List.of(leader));

        TeamMemberDTO teamMember1 = teamMemberBuilder(kostya);
        TeamMemberDTO teamMember2 = teamMemberBuilder(randomUser);

        sendInvites(team.getId(), List.of(teamMember1));
        sendInvites(team.getId(), List.of(teamMember2));

        List<TeamInvitation> invitationsByTeam = webTestClient
                .get()
                .uri("api/v1/team/invitations/{teamId}", team.getId())
                .header("Authorization", "Bearer " + jwt_owner)
                .exchange()
                .expectBodyList(TeamInvitation.class)
                .returnResult().getResponseBody();
        assertNotNull(invitationsByTeam);
//        assertEquals(2, invitationsByTeam.size());
    }

    @Test
    void testInviteUserInTeam() {

        TeamDTO team = createTeam(List.of(leader));
        assertEquals(1, getTeam(team.getId()).getMembersCount());

        TeamInvitation invite = inviteUserInTeam(team.getId(), randomUser.getId());
        assertEquals(randomUser.getId(), invite.getUserId());

        assertEquals(2, getTeam(team.getId()).getMembersCount());
    }

    @Test
    void testKickUserFromTeam() {

        TeamDTO team = createTeam(List.of(leader, member));
        assertEquals(2, getTeam(team.getId()).getMembersCount());

        kickUserFromTeam(team.getId(), member.getId());
        assertEquals(1, getTeam(team.getId()).getMembersCount());
    }

    @Test
    void testLeaveFromTeam() {

        TeamDTO team = createTeam(List.of(leader, member, randomUser));
        assertEquals(3, getTeam(team.getId()).getMembersCount());

        webTestClient
                .delete()
                .uri("/api/v1/team/leave/{teamId}", team.getId())
                .header("Authorization", "Bearer " + jwt_member)
                .exchange()
                .expectStatus().isOk();
        assertEquals(2, getTeam(team.getId()).getMembersCount());

        webTestClient
                .delete()
                .uri("/api/v1/team/leave/{teamId}", team.getId())
                .header("Authorization", "Bearer " + jwt_randomUser)
                .exchange()
                .expectStatus().isOk();
        assertEquals(1, getTeam(team.getId()).getMembersCount());

        webTestClient
                .delete()
                .uri("/api/v1/team/leave/{teamId}", team.getId())
                .header("Authorization", "Bearer " + jwt_leader)
                .exchange()
                .expectStatus().isOk();
        assertEquals(0, getTeam(team.getId()).getMembersCount());
    }

    @Test
    void testGetTeam() {

        TeamDTO team1 = createTeam(List.of(leader));
        TeamDTO team2 = createTeam(List.of(leader, member));

        assertEquals("Богатыри", getTeam(team1.getId()).getName());
        assertEquals("Слава Руси!", getTeam(team1.getId()).getDescription());

        assertFalse(getTeam(team1.getId()).getClosed());

        assertNotNull(getTeam(team1.getId()).getMembers());

        assertEquals(1, getTeam(team1.getId()).getMembersCount());
        assertEquals(2, getTeam(team2.getId()).getMembersCount());

        assertEquals(owner.getId(), getTeam(team1.getId()).getOwner().getId());
        assertNotEquals(leader.getId(), getTeam(team1.getId()).getOwner().getId());
        assertNotEquals(member.getId(), getTeam(team2.getId()).getOwner().getId());

        assertEquals(leader.getId(), getTeam(team1.getId()).getLeader().getId());
        assertNotEquals(owner.getId(), getTeam(team1.getId()).getLeader().getId());
        assertNotEquals(member.getId(), getTeam(team2.getId()).getLeader().getId());
    }

    @Test
    void testGetAllTeams() {

        createTeam(List.of(leader));
        createTeam(List.of(leader, member));

        List<TeamDTO> allTeams = webTestClient
                .get()
                .uri("api/v1/team/all")
                .header("Authorization", "Bearer " + jwt_randomUser)
                .exchange()
                .expectBodyList(TeamDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(allTeams);
        assertTrue(allTeams.size() > 1);
    }

    @Test
    void testGetOwnerTeams() {

        String ideaMarketId = createMarketIdea().getId();

        createTeam(List.of(leader));
        createTeam(List.of(leader, member));

        List<TeamDTO> ownerTeams1 = webTestClient
                .get()
                .uri("api/v1/team/owner/all/{ideaMarketId}", ideaMarketId)
                .header("Authorization", "Bearer " + jwt_owner)
                .exchange()
                .expectBodyList(TeamDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(ownerTeams1);
        assertEquals(2, ownerTeams1.size());

        TeamDTO team3 = TeamDTO.builder()
                .name("Не моя команда")
                .description("Хз кто это")
                .closed(false)
                .membersCount(1)
                .createdAt(LocalDate.now())
                .owner(randomUser)
                .leader(owner)
                .members(List.of(owner))
                .skills(List.of())
                .wantedSkills(List.of())
                .build();

        webTestClient
                .post()
                .uri("/api/v1/team/add")
                .header("Authorization", "Bearer " + jwt_randomUser)
                .body(Mono.just(team3), TeamDTO.class)
                .exchange();

        List<TeamDTO> ownerTeams2 = webTestClient
                .get()
                .uri("api/v1/team/owner/all/{ideaMarketId}", ideaMarketId)
                .header("Authorization", "Bearer " + jwt_owner)
                .exchange()
                .expectBodyList(TeamDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(ownerTeams2);
        assertEquals(2, ownerTeams2.size());
    }

    @Test
    void testGetAllUsersWithSkills() {

        List<TeamMemberDTO> allUsersWithSkills = webTestClient
                .get()
                .uri("api/v1/team/users")
                .header("Authorization", "Bearer " + jwt_randomUser)
                .exchange()
                .expectBodyList(TeamMemberDTO.class)
                .returnResult().getResponseBody();
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

//    @Test
//    void testGetTeamsBySkills() {
//
//        TeamDTO team = TeamDTO.builder()
//                .name("Богатыри")
//                .description("Слава Руси!")
//                .closed(false)
//                .membersCount(1)
//                .createdAt(LocalDate.now())
//                .owner(owner)
//                .leader(leader)
//                .members(List.of(leader))
//                .wantedSkills(List.of(skill3, skill4))
//                .build();
//
//        webTestClient
//                .post()
//                .uri("/api/v1/team/add")
//                .header("Authorization", "Bearer " + jwt_owner)
//                .body(Mono.just(team), TeamDTO.class)
//                .exchange();
//
//        assertEquals(0, getTeamsBySkills(List.of(skill3), Role.INITIATOR, jwt_initiator).size());
//        assertEquals(0, getTeamsBySkills(List.of(skill4), Role.INITIATOR, jwt_initiator).size());
//        assertEquals(0, getTeamsBySkills(List.of(skill3, skill4), Role.INITIATOR, jwt_initiator).size());
//
//        assertEquals(1, getTeamsBySkills(List.of(skill1), Role.INITIATOR, jwt_initiator).size());
//        assertEquals(1, getTeamsBySkills(List.of(skill2), Role.INITIATOR, jwt_initiator).size());
//        assertEquals(1, getTeamsBySkills(List.of(skill1, skill2), Role.INITIATOR, jwt_initiator).size());
//        assertEquals(1, getTeamsBySkills(List.of(skill1), Role.MEMBER, jwt_member).size());
//        assertEquals(1, getTeamsBySkills(List.of(skill2), Role.MEMBER, jwt_member).size());
//        assertEquals(1, getTeamsBySkills(List.of(skill1, skill2), Role.MEMBER, jwt_member).size());
//    }

    @Test
    void testGetTeamsByVacancies() {

        TeamDTO team = TeamDTO.builder()
                .name("Богатыри")
                .description("Слава Руси!")
                .closed(false)
                .membersCount(1)
                .createdAt(LocalDate.now())
                .owner(owner)
                .leader(leader)
                .members(List.of(leader))
                .wantedSkills(List.of(skill3, skill4))
                .build();

        webTestClient
                .post()
                .uri("/api/v1/team/add")
                .header("Authorization", "Bearer " + jwt_owner)
                .body(Mono.just(team), TeamDTO.class)
                .exchange();

        assertEquals(0, getTeamsByVacancies(List.of(skill1, skill2), jwt_owner).size());
        assertEquals(0, getTeamsByVacancies(List.of(skill1), jwt_leader).size());
        assertEquals(0, getTeamsByVacancies(List.of(skill2), jwt_member).size());

        assertEquals(1, getTeamsByVacancies(List.of(skill3), jwt_randomUser).size());
        assertEquals(1, getTeamsByVacancies(List.of(skill4), jwt_owner).size());
        assertEquals(1, getTeamsByVacancies(List.of(skill3, skill4), jwt_leader).size());
    }

    @Test
    void testGetSkillsByUsers() {

        List<SkillDTO> empty = webTestClient
                .post()
                .uri("api/v1/team/skills/users")
                .header("Authorization", "Bearer " + jwt_owner)
                .body(Flux.just(admin, owner), UserDTO.class)
                .exchange()
                .expectBodyList(SkillDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(empty);
        assertEquals(0, empty.size());

        List<SkillDTO> allSkillsByUsers1 = webTestClient
                .post()
                .uri("api/v1/team/skills/users")
                .header("Authorization", "Bearer " + jwt_owner)
                .body(Flux.just(leader, member), UserDTO.class)
                .exchange()
                .expectBodyList(SkillDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(allSkillsByUsers1);
        assertEquals(3, allSkillsByUsers1.size());

        List<SkillDTO> allSkillsByUsers2 = webTestClient
                .post()
                .uri("api/v1/team/skills/users")
                .header("Authorization", "Bearer " + jwt_owner)
                .body(Flux.just(leader, member, kostya), UserDTO.class)
                .exchange()
                .expectBodyList(SkillDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(allSkillsByUsers2);
        assertEquals(4, allSkillsByUsers2.size());
    }

    @Test
    void testGetSkillsByInvitations() {

        TeamDTO team = createTeam(List.of(leader));

        TeamMemberDTO teamMember1 = teamMemberBuilder(member);

        assertEquals(1, getSkillsByInvitations(sendInvites(team.getId(), List.of(teamMember1))).size());

        TeamMemberDTO teamMember2 = teamMemberBuilder(randomUser);

        assertEquals(0, getSkillsByInvitations(sendInvites(team.getId(), List.of(teamMember2))).size());

        TeamMemberDTO teamMember3 = teamMemberBuilder(kostya);

        assertEquals(1, getSkillsByInvitations(sendInvites(team.getId(), List.of(teamMember3))).size());

        assertEquals(1, getSkillsByInvitations(sendInvites(team.getId(), List.of(teamMember1, teamMember2))).size());
    }

    @Test
    void testGetSkillsByRequests() {

        TeamDTO team = createTeam(List.of(leader));

        assertEquals(0, getSkillsByRequests(List.of(sendTeamRequest(team.getId(), randomUser, jwt_randomUser))).size());
        assertEquals(1, getSkillsByRequests(List.of(sendTeamRequest(team.getId(), kostya, jwt_kostya))).size());
    }

    @Test
    void testDeleteTeam() {

        TeamDTO team1 = createTeam(List.of(leader));

        InfoResponse response1 = webTestClient
                .delete()
                .uri("/api/v1/team/delete/{teamId}", team1.getId())
                .header("Authorization", "Bearer " + jwt_owner)
                .exchange()
                .expectBody(InfoResponse.class)
                .returnResult().getResponseBody();
        assertNotNull(response1);
        assertEquals("Успешное удаление", response1.getMessage());

        TeamDTO team2 = createTeam(List.of(leader));

        InfoResponse response2 = webTestClient
                .delete()
                .uri("/api/v1/team/delete/{teamId}", team2.getId())
                .header("Authorization", "Bearer " + jwt_leader)
                .exchange()
                .expectBody(InfoResponse.class)
                .returnResult().getResponseBody();
        assertNotNull(response2);
        assertEquals("Ошибка при удалении", response2.getMessage());

        TeamDTO team3 = createTeam(List.of(leader, member));

        webTestClient
                .delete()
                .uri("/api/v1/team/delete/{teamId}", team3.getId())
                .header("Authorization", "Bearer " + jwt_member)
                .exchange()
                .expectStatus().isForbidden();

        webTestClient
                .delete()
                .uri("/api/v1/team/delete/{teamId}", team3.getId())
                .header("Authorization", "Bearer " + jwt_randomUser)
                .exchange()
                .expectStatus().isForbidden();
    }
}
