package com.tyiu.ideas.controller;

import com.tyiu.ideas.model.*;
import com.tyiu.ideas.model.dto.*;
import com.tyiu.ideas.model.entities.Idea;
import com.tyiu.ideas.model.enums.MarketStatus;
import com.tyiu.ideas.model.enums.Role;
import com.tyiu.ideas.model.enums.SkillType;
import com.tyiu.ideas.model.requests.IdeaSkillRequest;
import com.tyiu.ideas.model.requests.RegisterRequest;
import com.tyiu.ideas.model.responses.AuthenticationResponse;
import com.tyiu.ideas.model.responses.InfoResponse;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.HttpStatus;
import org.springframework.test.web.reactive.server.StatusAssertions;
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
public class ProjectControllerTest extends TestContainers {

    @Autowired
    private WebTestClient webTestClient;

    private final String path = "/api/v1/ideas-service/market/idea";
    private final String main_path = "/api/v1/scrum-service/project";

    private String jwt_initiator;
    private String jwt_expert;
    private String jwt_office;
    private String jwt_admin;
    private String jwt_member;
    private String jwt_leader;
    private String jwt_owner;
    private String jwt_teacher;

    private UserDTO admin;
    private UserDTO member;
    private UserDTO initiator;

    private GroupDTO groupExpert;
    private GroupDTO groupProjectOffice;
    private SkillDTO skill1;
    private SkillDTO skill2;
    private String marketId;
    private TeamDTO createdTeam;
    private SprintDTO createdSprint;
    private IdeaMarketDTO createdIdeaMarket;

    private RegisterRequest buildRegisterRequest(String email, String lastName, List<Role> roles){
        return new RegisterRequest(email, lastName, "task", "password", roles);
    }

    private UserDTO buildUser(AuthenticationResponse response){
        return UserDTO.builder()
                .id(response.getId())
                .email(response.getEmail())
                .lastName(response.getLastName())
                .firstName(response.getFirstName())
                .roles(response.getRoles())
                .build();
    }

    private GroupDTO buildGroup(String name, List<Role> roles, UserDTO user){
        return GroupDTO.builder().name(name).users(List.of(user))
                .roles(roles).build();
    }

    private IdeaDTO buildIdea(String name, UserDTO user){
        return IdeaDTO.builder()
                .initiator(user)
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

    private SkillDTO buildSkill(String name){
        return SkillDTO.builder().name(name).type(SkillType.LANGUAGE)
                .build();
    }

    private IdeaSkillRequest buildSkillRequest(String id, List<SkillDTO> skills){
        return IdeaSkillRequest.builder().ideaId(id).skills(skills).build();
    }

    private ProjectMemberDTO createProjectMember(String projectId, AddToProjectRequest add, String jwt){
        ProjectMemberDTO createdMember = webTestClient
                .post()
                .uri(main_path + "/{projectId}/add/members" , projectId)
                .header("Authorization", jwt)
                .body(Mono.just(add), AddToProjectRequest.class)
                .exchange()
                .expectBody(ProjectMemberDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(createdMember);
        assertEquals(add.getUserId(), createdMember.getUserId());
        return createdMember;
    }

    private AuthenticationResponse createUser(RegisterRequest request){
        AuthenticationResponse response = webTestClient
                .post()
                .uri("/api/v1/ideas-service/auth/register")
                .body(Mono.just(request), RegisterRequest.class)
                .exchange()
                .expectBody(AuthenticationResponse.class)
                .returnResult().getResponseBody();
        assertNotNull(response);
        return response;
    }

    private IdeaDTO createIdea(IdeaDTO ideaDTO, String jwt){
        IdeaDTO createdIdea = webTestClient
                .post()
                .uri("/api/v1/ideas-service/idea/add")
                .header("Authorization", jwt)
                .body(Mono.just(ideaDTO), IdeaDTO.class)
                .exchange()
                .expectBody(IdeaDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(createdIdea);
        assertEquals(ideaDTO.getName(), createdIdea.getName());
        return createdIdea;
    }

    private GroupDTO createGroup(GroupDTO groupDTO, String jwt){
        GroupDTO createdGroup = webTestClient
                .post()
                .uri("/api/v1/ideas-service/group/create")
                .header("Authorization", jwt)
                .body(Mono.just(groupDTO), GroupDTO.class)
                .exchange()
                .expectBody(GroupDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(createdGroup);
        assertEquals(groupDTO.getName(), createdGroup.getName());
        return createdGroup;
    }

    private SkillDTO createSkill(SkillDTO skillDTO, String jwt){
        SkillDTO createdSkill = webTestClient
                .post()
                .uri("/api/v1/ideas-service/skill/add")
                .header("Authorization", jwt)
                .body(Mono.just(skillDTO), SkillDTO.class)
                .exchange()
                .expectBody(SkillDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(createdSkill);
        assertEquals(skillDTO.getName(), createdSkill.getName());
        return createdSkill;
    }

    private TeamMarketRequestDTO createMarketTeamRequest(String ideaMarketId, String jwt){
        TeamMarketRequestDTO teamMarketRequest = TeamMarketRequestDTO.builder()
                .ideaMarketId(ideaMarketId)
                .teamId(createdTeam.getId())
                .name(createdTeam.getName())
                .letter("letter")
                .build();
        TeamMarketRequestDTO createdTeamMarketRequest = webTestClient
                .post()
                .uri(path + "/declare")
                .header("Authorization", jwt)
                .body(Mono.just(teamMarketRequest), TeamMarketRequestDTO.class)
                .exchange()
                .expectBody(TeamMarketRequestDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(createdTeamMarketRequest);
        assertEquals(teamMarketRequest.getName(),createdTeamMarketRequest.getName());
        return createdTeamMarketRequest;
    }

    private IdeaDTO getIdea(String id, String name, String jwt){
        IdeaDTO idea = webTestClient
                .get()
                .uri("/api/v1/ideas-service/idea/{ideaId}", id)
                .header("Authorization", jwt)
                .exchange()
                .expectBody(IdeaDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(idea);
        assertEquals(name, idea.getName());
        return idea;
    }

    private IdeaMarketDTO getMarketIdea(String ideaMarketId, String jwt){
        IdeaMarketDTO responseBody = webTestClient
                .get()
                .uri(path + "/{ideaMarketId}", ideaMarketId)
                .header("Authorization", jwt)
                .exchange()
                .expectBody(IdeaMarketDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(responseBody);
        assertTrue(Objects.equals(responseBody.getName(), "idea1") || Objects.equals(responseBody.getName(), "idea2"));
        return responseBody;
    }

    private void addSkills(IdeaSkillRequest ideaSkillRequest, String jwt){
        InfoResponse skillRequest = webTestClient
                .post()
                .uri("/api/v1/ideas-service/idea/skills/add")
                .header("Authorization", jwt)
                .body(Mono.just(ideaSkillRequest), IdeaSkillRequest.class)
                .exchange()
                .expectBody(InfoResponse.class)
                .returnResult().getResponseBody();
        assertNotNull(skillRequest);
    }

    private IdeaMarketDTO createMarketIdea(UserDTO user, String jwt){
        IdeaDTO ideaDTO1 = buildIdea("idea1", user);
        IdeaDTO idea1 = getIdea(createIdea(ideaDTO1, jwt).getId(),ideaDTO1.getName(), jwt);

        IdeaDTO ideaDTO2 = buildIdea("idea2", user);
        IdeaDTO idea2 = getIdea(createIdea(ideaDTO2, jwt).getId(),ideaDTO2.getName(), jwt);

        addSkills(buildSkillRequest(idea1.getId(),List.of(skill1,skill2)), jwt);
        addSkills(buildSkillRequest(idea2.getId(),List.of(skill1,skill2)), jwt);

        List<IdeaMarketDTO> createdMarketIdea = webTestClient
                .post()
                .uri(path + "/send/{marketId}", marketId)
                .header("Authorization", jwt)
                .body(Flux.just(idea1, idea2), IdeaDTO.class)
                .exchange()
                .expectBodyList(IdeaMarketDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(createdMarketIdea);
        assertEquals(2, createdMarketIdea.size());
        IdeaMarketDTO ideaMarketDTO = createdMarketIdea.get(0);
        assertNotNull(ideaMarketDTO);
        assertTrue(Objects.equals(ideaMarketDTO.getName(), ideaDTO1.getName()) || Objects.equals(ideaMarketDTO.getName(), ideaDTO2.getName()));
        assertSame(getIdea(ideaMarketDTO.getIdeaId(), ideaMarketDTO.getName(), jwt).getStatus(), Idea.Status.ON_MARKET);
        return ideaMarketDTO;
    }

    private ProjectDTO createProject(IdeaMarketDTO ideaMarketDTO, String jwt){
        ProjectDTO createdProject = webTestClient
                .post()
                .uri("/api/v1/scrum-service/project/send")
                .header("Authorization", jwt)
                .body(Mono.just(getMarketIdea(ideaMarketDTO.getId(), jwt)), IdeaMarketDTO.class)
                .exchange()
                .expectBody(ProjectDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(createdProject);
        assertNotNull(createdProject);
        ProjectDTO projectDTO = getProject(createdProject.getId(), jwt);
        assertNotNull(projectDTO);
        assertNotNull(projectDTO.getId());
        assertEquals(projectDTO.getIdeaId(), ideaMarketDTO.getIdeaId());
        assertEquals(projectDTO.getName(), ideaMarketDTO.getName());
        assertEquals(projectDTO.getDescription(), ideaMarketDTO.getDescription());
        assertEquals(projectDTO.getCustomer(), ideaMarketDTO.getCustomer());
        assertEquals(projectDTO.getInitiator(), ideaMarketDTO.getInitiator());
        //assertEquals(projectDTO.getTeam(), getMarketIdea(createdIdeaMarket.getId(), jwt).getTeam());
        assertNotNull(projectDTO.getMembers());
        return projectDTO;
    }

    private TeamDTO getTeam(String teamId, String jwt){
        TeamDTO team = webTestClient
                .get()
                .uri("/api/v1/ideas-service/team/{teamId}", teamId)
                .header("Authorization", jwt)
                .exchange()
                .expectBody(TeamDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(team);
        assertEquals("name", team.getName());
        return team;
    }

    private ProjectDTO getProject(String id, String jwt){
        ProjectDTO projectDTO = webTestClient
                .get()
                .uri(main_path + "/{projectId}", id)
                .header("Authorization", jwt)
                .exchange()
                .expectBody(ProjectDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(projectDTO);
        assertNotNull(projectDTO.getId());
        assertNotNull(projectDTO.getIdeaId());
        assertNotNull(projectDTO.getName());
        assertNotNull(projectDTO.getDescription());
        assertNotNull(projectDTO.getCustomer());
        assertNotNull(projectDTO.getInitiator());
        assertNotNull(projectDTO.getTeam());
        assertNotNull(projectDTO.getMembers());
        assertNotNull(projectDTO.getReport());
        assertNotNull(projectDTO.getStartDate());
        assertNotNull(projectDTO.getFinishDate());
        assertNotNull(projectDTO.getStatus());
        return  projectDTO;
    }

    private List<ProjectDTO> getListProject(String path){
        return webTestClient
                .get()
                .uri(main_path + path)
                .header("Authorization", jwt_admin)
                .exchange()
                .expectBodyList(ProjectDTO.class)
                .returnResult().getResponseBody();
    }

    private List<ProjectDTO> getListPrivateProject(String path, String jwt){
        return webTestClient
                .get()
                .uri(main_path + path)
                .header("Authorization", jwt)
                .exchange()
                .expectBodyList(ProjectDTO.class)
                .returnResult().getResponseBody();
    }

    private List<ProjectMemberDTO> getListProjectMembers(String path,String projectId){
        return webTestClient
                .get()
                .uri(main_path + path,projectId)
                .header("Authorization", jwt_admin)
                .exchange()
                .expectBodyList(ProjectMemberDTO.class)
                .returnResult().getResponseBody();
    }

    private List<ProjectMarksDTO> getListProjectMarks(String path,String projectId){
        return webTestClient
                .get()
                .uri(main_path + path,projectId)
                .header("Authorization", jwt_admin)
                .exchange()
                .expectBodyList(ProjectMarksDTO.class)
                .returnResult().getResponseBody();
    }

    private void acceptTeam(String ideaMarketId, String teamId, String jwt){
        TeamDTO teamDTO = webTestClient
                .put()
                .uri(path + "/accept/request/{ideaMarketId}/{teamId}",
                        ideaMarketId, teamId)
                .header("Authorization", jwt)
                .exchange()
                .expectBody(TeamDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(teamDTO);
        assertNotNull(teamDTO.getId());
        assertTrue(teamDTO.getSkills().size() >= 2);
        TeamDTO ideaMarketTeam = getMarketIdea(ideaMarketId, jwt).getTeam();
        assertNotNull(ideaMarketTeam.getId());
        assertNotNull(ideaMarketTeam.getSkills().get(0).getId());
        assertNotNull(ideaMarketTeam.getOwner());
        assertTrue(ideaMarketTeam.getSkills().size() >= 2);
        assertSame(getTeam(teamId, jwt).getHasActiveProject(), Boolean.TRUE);
    }

    private void assertProject(ProjectDTO expected, ProjectDTO actual){
        assertEquals(expected.getIdeaId(), actual.getIdeaId());
        assertEquals(expected.getName(), actual.getName());
        assertEquals(expected.getDescription(), actual.getDescription());
        assertEquals(expected.getCustomer(), actual.getCustomer());
        assertNotNull(expected.getInitiator());
        assertNotNull(actual.getInitiator());
        assertEquals(expected.getInitiator().getId(), actual.getInitiator().getId());
        assertEquals(expected.getInitiator().getEmail(), actual.getInitiator().getEmail());
        assertEquals(expected.getInitiator().getFirstName(), actual.getInitiator().getFirstName());
        assertEquals(expected.getInitiator().getLastName(), actual.getInitiator().getLastName());
        assertNotNull(expected.getTeam());
        assertNotNull(actual.getTeam());
        assertEquals(expected.getTeam().getId(), actual.getTeam().getId());
        assertEquals(expected.getTeam().getMembersCount(), actual.getTeam().getMembersCount());
        assertEquals(expected.getStartDate(), actual.getStartDate());
        assertEquals(expected.getFinishDate(), actual.getFinishDate());
        assertEquals(expected.getStatus(), actual.getStatus());
    }

    private StatusAssertions checkGetOneProjectById(String id, String jwt){
        return webTestClient
                .get()
                .uri(main_path + "/{projectId}", id)
                .header("Authorization", jwt)
                .exchange()
                .expectStatus();
    }

    private StatusAssertions checkGetAllProjects(String jwt){
        return webTestClient
                .get()
                .uri(main_path + "/all")
                .header("Authorization", jwt)
                .exchange()
                .expectStatus();
    }

    private StatusAssertions checkGetAllYourProjects(String jwt){
        return webTestClient
                .get()
                .uri(main_path + "/private/all")
                .header("Authorization", jwt)
                .exchange()
                .expectStatus();
    }

    private StatusAssertions checkGetAllActiveProjects(String jwt){
        return webTestClient
                .get()
                .uri(main_path + "/active/all")
                .header("Authorization", jwt)
                .exchange()
                .expectStatus();
    }

    private StatusAssertions checkGetAllMembers(String jwt, String id){
        return webTestClient
                .get()
                .uri(main_path + "/members/{projectId}/all", id)
                .header("Authorization", jwt)
                .exchange()
                .expectStatus();
    }

    private StatusAssertions checkGetAllMarks(String jwt, String id){
        return webTestClient
                .get()
                .uri(main_path + "/marks/{projectId}/all", id)
                .header("Authorization", jwt)
                .exchange()
                .expectStatus();
    }

    private StatusAssertions checkPostCreateProject(IdeaMarketDTO ideaMarketDTO, String jwt){
        return webTestClient
                .post()
                .uri(main_path + "/send")
                .header("Authorization", jwt)
                .body(Mono.just(ideaMarketDTO), IdeaMarketDTO.class)
                .exchange()
                .expectStatus();
    }

    private StatusAssertions checkPostAddMemberInProject(String projectId, AddToProjectRequest addToProjectRequest, String jwt){
        return webTestClient
                .post()
                .uri(main_path + "/{projectId}/add/members",projectId)
                .header("Authorization", jwt)
                .body(Mono.just(addToProjectRequest), AddToProjectRequest.class)
                .exchange()
                .expectStatus();
    }

    private StatusAssertions checkPutPauseProject(String projectId, String jwt){
        return webTestClient
                .put()
                .uri(main_path + "/{projectId}/status/change", projectId)
                .header("Authorization", jwt)
                .exchange()
                .expectStatus();
    }

    private StatusAssertions checkPutFinishProject(String report, String projectId, String jwt){
        return webTestClient
                .put()
                .uri(main_path + "/finish/{projectId}", projectId)
                .header("Authorization", jwt)
                .body(Mono.just(report), String.class)
                .exchange()
                .expectStatus();
    }

    @BeforeAll
    public void setUp() {
        RegisterRequest initiatorRequest = buildRegisterRequest("project.initiator@gmail.com", "initiator", List.of(Role.INITIATOR));
        RegisterRequest expertRequest = buildRegisterRequest("project.expert@gmail.com", "expert", List.of(Role.EXPERT));
        RegisterRequest officeRequest = buildRegisterRequest("project.office@gmail.com", "office", List.of(Role.PROJECT_OFFICE));
        RegisterRequest adminRequest = buildRegisterRequest("project.admin@gmail.com", "admin", List.of(Role.ADMIN));
        RegisterRequest memberRequest = buildRegisterRequest("project.member@gmail.com", "member", List.of(Role.MEMBER));
        RegisterRequest leaderRequest = buildRegisterRequest("project.leader@gmail.com", "leader", List.of(Role.TEAM_LEADER));
        RegisterRequest ownerRequest = buildRegisterRequest("project.owner@gmail.com", "owner", List.of(Role.TEAM_OWNER));
        RegisterRequest teacherRequest = buildRegisterRequest("project.teacher@gmail.com", "teacher", List.of(Role.TEACHER));

        AuthenticationResponse initiatorResponse = createUser(initiatorRequest);
        AuthenticationResponse expertResponse = createUser(expertRequest);
        AuthenticationResponse officeResponse = createUser(officeRequest);
        AuthenticationResponse adminResponse = createUser(adminRequest);
        AuthenticationResponse memberResponse = createUser(memberRequest);
        AuthenticationResponse leaderResponse = createUser(leaderRequest);
        AuthenticationResponse ownerResponse = createUser(ownerRequest);
        AuthenticationResponse teacherResponse = createUser(teacherRequest);

        jwt_initiator = "Bearer " + initiatorResponse.getToken();
        jwt_expert = "Bearer " + expertResponse.getToken();
        jwt_office = "Bearer " + officeResponse.getToken();
        jwt_admin = "Bearer " + adminResponse.getToken();
        jwt_member = "Bearer " + memberResponse.getToken();
        jwt_leader = "Bearer " + leaderResponse.getToken();
        jwt_owner = "Bearer " + ownerResponse.getToken();
        jwt_teacher = "Bearer " + teacherResponse.getToken();

        admin = buildUser(adminResponse);
        initiator = buildUser(initiatorResponse);
        member = buildUser(memberResponse);

        groupExpert = createGroup(buildGroup("exp",List.of(Role.EXPERT), buildUser(expertResponse)), jwt_admin);
        groupProjectOffice = createGroup(buildGroup("pro",List.of(Role.PROJECT_OFFICE), buildUser(officeResponse)), jwt_admin);

        skill1 = createSkill(buildSkill("skill1"), jwt_admin);
        skill2 = createSkill(buildSkill("skill2"), jwt_admin);

        webTestClient
                .post()
                .uri("/api/v1/ideas-service/profile/skills/save")
                .header("Authorization", jwt_member)
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
                .header("Authorization", jwt_admin)
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
                .header("Authorization", jwt_admin)
                .exchange()
                .expectStatus().isOk();

        marketId = market.getId();

        TeamDTO teamDTO = TeamDTO.builder()
                .name("name")
                .description("description")
                .closed(false)
                .owner(buildUser(ownerResponse))
                .leader(buildUser(leaderResponse))
                .members(List.of(member))
                .wantedSkills(List.of(skill1, skill2))
                .build();
        createdTeam = webTestClient
                .post()
                .uri("/api/v1/ideas-service/team/add")
                .header("Authorization", jwt_owner)
                .body(Mono.just(teamDTO), TeamDTO.class)
                .exchange()
                .expectBody(TeamDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(createdTeam);
        assertEquals(teamDTO.getName(),createdTeam.getName());

        createdIdeaMarket = createMarketIdea(admin, jwt_admin);
        String ideaMarketDTOId = createdIdeaMarket.getId();
        assertNull(getMarketIdea(ideaMarketDTOId, jwt_admin).getTeam());
        acceptTeam(ideaMarketDTOId, createMarketTeamRequest(ideaMarketDTOId, jwt_owner).getTeamId(), jwt_admin);
    }

    @Test
    void testGetAllProjects(){
        ProjectDTO prj1 = getProject(createProject(getMarketIdea(createdIdeaMarket.getId(),jwt_admin),jwt_admin).getId(),jwt_admin);
        ProjectDTO prj2 = getProject(createProject(getMarketIdea(createdIdeaMarket.getId(),jwt_admin),jwt_admin).getId(),jwt_admin);
        ProjectDTO prj3 = getProject(createProject(getMarketIdea(createdIdeaMarket.getId(),jwt_admin),jwt_admin).getId(),jwt_admin);
        ProjectDTO prj4 = getProject(createProject(getMarketIdea(createdIdeaMarket.getId(),jwt_admin),jwt_admin).getId(),jwt_admin);
        List<ProjectDTO> projects = getListProject("/all");
        assertNotNull(projects);
        assertTrue(projects.size() >= 4);
        projects.forEach(p -> {
            if (Objects.equals(prj1.getId(), p.getId())){
                assertProject(prj1, p);
            }
            else if (Objects.equals(prj2.getId(), p.getId())){
                assertProject(prj2, p);
            }
            else if (Objects.equals(prj3.getId(), p.getId())){
                assertProject(prj3, p);
            }
            else if (Objects.equals(prj4.getId(), p.getId())){
                assertProject(prj4, p);
            }
        });
        checkGetAllProjects(jwt_initiator).isForbidden();
        checkGetAllProjects(jwt_office).isOk();
        checkGetAllProjects(jwt_member).isForbidden();
        checkGetAllProjects(jwt_owner).isForbidden();
        checkGetAllProjects(jwt_leader).isForbidden();
        checkGetAllProjects(jwt_expert).isForbidden();
        checkGetAllProjects(jwt_teacher).isForbidden();
    }

    @Test
    void testGetAllYourProjects(){
        ProjectDTO prj1 = getProject(createProject(getMarketIdea(createdIdeaMarket.getId(),jwt_admin),jwt_admin).getId(),jwt_admin);
        ProjectDTO prj2 = getProject(createProject(getMarketIdea(createdIdeaMarket.getId(),jwt_admin),jwt_admin).getId(),jwt_admin);
        ProjectDTO prj3 = getProject(createProject(getMarketIdea(createdIdeaMarket.getId(),jwt_admin),jwt_admin).getId(),jwt_admin);
        ProjectDTO prj4 = getProject(createProject(getMarketIdea(createdIdeaMarket.getId(),jwt_admin),jwt_admin).getId(),jwt_admin);
        List<ProjectDTO> yourProjects = getListPrivateProject("/private/all", jwt_member);
        assertNotNull(yourProjects);
        List<ProjectDTO> projects = getListProject("/all");
        assertNotNull(projects);
        assertTrue(yourProjects.size() <= projects.size());
        yourProjects.forEach(p -> {
            if (Objects.equals(prj1.getId(), p.getId())){
                assertProject(prj1, p);
            }
            else if (Objects.equals(prj2.getId(), p.getId())){
                assertProject(prj2, p);
            }
            else if (Objects.equals(prj3.getId(), p.getId())){
                assertProject(prj3, p);
            }
            else if (Objects.equals(prj4.getId(), p.getId())){
                assertProject(prj4, p);
            }
        });
        checkGetAllYourProjects(jwt_initiator).isOk();
        checkGetAllYourProjects(jwt_office).isForbidden();
        checkGetAllYourProjects(jwt_member).isOk();
        checkGetAllYourProjects(jwt_owner).isOk();
        checkGetAllYourProjects(jwt_leader).isOk();
        checkGetAllYourProjects(jwt_expert).isForbidden();
        checkGetAllYourProjects(jwt_teacher).isForbidden();
    }

    @Test
    void testGetAllActiveProjects(){
        ProjectDTO prj1 = getProject(createProject(getMarketIdea(createdIdeaMarket.getId(),jwt_admin),jwt_admin).getId(),jwt_admin);
        ProjectDTO prj2 = getProject(createProject(getMarketIdea(createdIdeaMarket.getId(),jwt_admin),jwt_admin).getId(),jwt_admin);
        ProjectDTO prj3 = getProject(createProject(getMarketIdea(createdIdeaMarket.getId(),jwt_admin),jwt_admin).getId(),jwt_admin);
        ProjectDTO prj4 = getProject(createProject(getMarketIdea(createdIdeaMarket.getId(),jwt_admin),jwt_admin).getId(),jwt_admin);
        List<ProjectDTO> activeProjects = getListPrivateProject("/active/all", jwt_member);
        assertNotNull(activeProjects);
        List<ProjectDTO> projects = getListProject("/all");
        assertNotNull(projects);
        assertTrue(activeProjects.size() <= projects.size());
        activeProjects.forEach(p -> {
            if (Objects.equals(prj1.getId(), p.getId())){
                assertProject(prj1, p);
            }
            else if (Objects.equals(prj2.getId(), p.getId())){
                assertProject(prj2, p);
            }
            else if (Objects.equals(prj3.getId(), p.getId())){
                assertProject(prj3, p);
            }
            else if (Objects.equals(prj4.getId(), p.getId())){
                assertProject(prj4, p);
            }
        });
        checkGetAllActiveProjects(jwt_initiator).isOk();
        checkGetAllActiveProjects(jwt_office).isForbidden();
        checkGetAllActiveProjects(jwt_member).isOk();
        checkGetAllActiveProjects(jwt_owner).isOk();
        checkGetAllActiveProjects(jwt_leader).isOk();
        checkGetAllActiveProjects(jwt_expert).isForbidden();
        checkGetAllActiveProjects(jwt_teacher).isForbidden();
    }

    @Test
    void testGetOneProjectById(){
        String projectId = getProject(createProject(getMarketIdea(createdIdeaMarket.getId(),jwt_admin),jwt_admin).getId(),jwt_admin).getId();
        checkGetOneProjectById(projectId, jwt_initiator).isOk();
        checkGetOneProjectById(projectId, jwt_office).isOk();
        checkGetOneProjectById(projectId, jwt_member).isOk();
        checkGetOneProjectById(projectId, jwt_owner).isOk();
        checkGetOneProjectById(projectId, jwt_leader).isOk();
        checkGetOneProjectById(projectId, jwt_expert).isForbidden();
        checkGetOneProjectById(projectId, jwt_teacher).isForbidden();
    }

    @Test
    void testGetAllProjectMembers(){
        AddToProjectRequest add = new AddToProjectRequest(
                getTeam(createdTeam.getId(),jwt_admin).getId(),
                member.getId()
        );
        String projectId = getProject(createProject(getMarketIdea(createdIdeaMarket.getId(),jwt_admin),jwt_admin).getId(),jwt_admin).getId();
        createProjectMember(projectId,add,jwt_admin);
        List<ProjectMemberDTO> members = getListProjectMembers("/members/{projectId}/all",projectId);
        assertNotNull(members);
        assertTrue(members.size() >= 3);
        checkGetAllMembers(jwt_initiator,projectId).isOk();
        checkGetAllMembers(jwt_office,projectId).isOk();
        checkGetAllMembers(jwt_member,projectId).isOk();
        checkGetAllMembers(jwt_owner,projectId).isOk();
        checkGetAllMembers(jwt_leader,projectId).isOk();
        checkGetAllMembers(jwt_expert,projectId).isForbidden();
        checkGetAllMembers(jwt_teacher,projectId).isForbidden();
    }

    @Test
    void testGetAllProjectMarks(){ //переделать оценки выставляются со спринтов
        String projectId = getProject(createProject(getMarketIdea(createdIdeaMarket.getId(),jwt_admin),jwt_admin).getId(),jwt_admin).getId();

        SprintDTO sprintDTO = new SprintDTO(
                null,
                projectId,
                "спринт",
                null,
                null,
                "мега цель",
                LocalDate.now(),
                LocalDate.now().plusDays(20),
                2L,
                List.of()
        );
        createdSprint = webTestClient
                .post()
                .uri("/api/v1/scrum-service/sprint/add")
                .header("Authorization", jwt_admin)
                .body(Mono.just(sprintDTO), SprintDTO.class)
                .exchange()
                .expectBody(SprintDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(createdSprint);

        List<ProjectMarksDTO> marks = getListProjectMarks("/marks/{projectId}/all",projectId);
        assertNotNull(marks);
        assertTrue(marks.size() >= 0);
        checkGetAllMembers(jwt_initiator,projectId).isOk();
        checkGetAllMembers(jwt_office,projectId).isOk();
        checkGetAllMembers(jwt_member,projectId).isOk();
        checkGetAllMembers(jwt_owner,projectId).isOk();
        checkGetAllMembers(jwt_leader,projectId).isOk();
        checkGetAllMembers(jwt_expert,projectId).isForbidden();
        checkGetAllMembers(jwt_teacher,projectId).isForbidden();
    }

    @Test
    void testPostCreateProject(){
        checkPostCreateProject(getMarketIdea(createdIdeaMarket.getId(),jwt_admin),jwt_admin).isOk();
        checkPostCreateProject(getMarketIdea(createdIdeaMarket.getId(),jwt_admin),jwt_initiator).isForbidden();
        checkPostCreateProject(getMarketIdea(createdIdeaMarket.getId(),jwt_admin),jwt_office).isOk();
        checkPostCreateProject(getMarketIdea(createdIdeaMarket.getId(),jwt_admin),jwt_member).isForbidden();
        checkPostCreateProject(getMarketIdea(createdIdeaMarket.getId(),jwt_admin),jwt_owner).isForbidden();
        checkPostCreateProject(getMarketIdea(createdIdeaMarket.getId(),jwt_admin),jwt_leader).isForbidden();
        checkPostCreateProject(getMarketIdea(createdIdeaMarket.getId(),jwt_admin),jwt_expert).isForbidden();
        checkPostCreateProject(getMarketIdea(createdIdeaMarket.getId(),jwt_admin),jwt_teacher).isForbidden();
    }

    @Test
    void testPostAddMembersInProject(){
        String projectId = getProject(createProject(getMarketIdea(createdIdeaMarket.getId(),jwt_admin),jwt_admin).getId(),jwt_admin).getId();
        AddToProjectRequest add = new AddToProjectRequest(
                getTeam(createdTeam.getId(),jwt_admin).getId(),
                member.getId()
        );
        checkPostAddMemberInProject(projectId,add,jwt_admin).isOk();
        checkPostAddMemberInProject(projectId,add,jwt_initiator).isForbidden();
        checkPostAddMemberInProject(projectId,add,jwt_office).isForbidden();
        checkPostAddMemberInProject(projectId,add,jwt_member).isForbidden();
        checkPostAddMemberInProject(projectId,add,jwt_owner).isOk();
        checkPostAddMemberInProject(projectId,add,jwt_leader).isOk();
        checkPostAddMemberInProject(projectId,add,jwt_expert).isForbidden();
        checkPostAddMemberInProject(projectId,add,jwt_teacher).isForbidden();
    }

    @Test
    void testPutPauseProject(){
        ProjectDTO prj = getProject(createProject(getMarketIdea(createdIdeaMarket.getId(),jwt_admin),jwt_admin).getId(),jwt_admin);

        InfoResponse response = webTestClient
                .put()
                .uri(main_path + "/{projectId}/status/change", prj.getId())
                .header("Authorization", jwt_admin)
                .exchange()
                .expectBody(InfoResponse.class)
                .returnResult().getResponseBody();
        assertNotNull(response);
        assertEquals(response.getStatusCode(), HttpStatus.OK);
        assertEquals(response.getMessage(), "Проект успешно приостановлен");
        checkPutPauseProject(prj.getId(),jwt_initiator).isOk();
        checkPutPauseProject(prj.getId(),jwt_office).isOk();
        checkPutPauseProject(prj.getId(),jwt_member).isForbidden();
        checkPutPauseProject(prj.getId(),jwt_owner).isForbidden();
        checkPutPauseProject(prj.getId(),jwt_leader).isOk();
        checkPutPauseProject(prj.getId(),jwt_expert).isForbidden();
        checkPutPauseProject(prj.getId(),jwt_teacher).isForbidden();
    }

    @Test
    void testPutFinishProject(){
        ProjectDTO prj = getProject(createProject(getMarketIdea(createdIdeaMarket.getId(),jwt_admin),jwt_admin).getId(),jwt_admin);

        InfoResponse response = webTestClient
                .put()
                .uri(main_path + "/finish/{projectId}", prj.getId())
                .header("Authorization", jwt_admin)
                .body(Mono.just("report"), String.class)
                .exchange()
                .expectBody(InfoResponse.class)
                .returnResult().getResponseBody();
        assertNotNull(response);
        assertEquals(response.getStatusCode(), HttpStatus.OK);
        assertEquals(response.getMessage(), "Проект успешно завершён");
        checkPutFinishProject("крутой",prj.getId(),jwt_initiator).isOk();
        checkPutFinishProject("крутой",prj.getId(),jwt_office).isOk();
        checkPutFinishProject("крутой",prj.getId(),jwt_member).isForbidden();
        checkPutFinishProject("крутой",prj.getId(),jwt_owner).isForbidden();
        checkPutFinishProject("крутой",prj.getId(),jwt_leader).isForbidden();
        checkPutFinishProject("крутой",prj.getId(),jwt_expert).isForbidden();
        checkPutFinishProject("крутой",prj.getId(),jwt_teacher).isForbidden();
    }
}
