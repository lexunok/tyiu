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
public class TaskMovementLogControllerTest extends TestContainers {

    @Autowired
    private WebTestClient webTestClient;
    private final String path = "/api/v1/ideas-service/market/idea";
    private final String main_path = "/api/v1/scrum-service/log";

    private String jwt_initiator;
    private String jwt_expert;
    private String jwt_office;
    private String jwt_admin;
    private String jwt_member;
    private String jwt_leader;
    private String jwt_owner;
    private String jwt_teacher;

    private UserDTO leader;
    private UserDTO member;

    private GroupDTO groupExpert;
    private GroupDTO groupProjectOffice;
    private SkillDTO skill1;
    private SkillDTO skill2;
    private TagDTO tag1;
    private TagDTO tag2;
    private String marketId;
    private TeamDTO createdTeam;
    private ProjectDTO createdProject;
    private SprintDTO createdSprint;
    private TaskDTO createdTask;

    private RegisterRequest buildRegisterRequest(String email, String lastName, List<Role> roles){
        return new RegisterRequest(email, lastName, "sprint", "password", roles);
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

    private TagDTO buildTag(String name, String color){
        return new TagDTO(null,name, color, true, null, null, null);
    }

    private TaskMovementLogDTO buildLog(TaskDTO task, UserDTO user){
        return new TaskMovementLogDTO(
                null,
                task,
                null,
                user,
                null,
                null,
                "10часов",
                TaskStatus.NewTask
        );
    }

    private TaskMovementLogDTO createLog(TaskMovementLogDTO buildLog, String jwt){
        TaskMovementLogDTO createdLog = webTestClient
                .post()
                .uri(main_path + "/add")
                .header("Authorization", jwt)
                .body(Mono.just(buildLog), TaskMovementLogDTO.class)
                .exchange()
                .expectBody(TaskMovementLogDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(createdLog);
        return createdLog;
    }

    private List<TaskMovementLogDTO> getListLog(String taskId){
        return webTestClient
                .get()
                .uri(main_path + "/all/{taskId}", taskId)
                .header("Authorization", jwt_admin)
                .exchange()
                .expectBodyList(TaskMovementLogDTO.class)
                .returnResult().getResponseBody();
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

    private TagDTO createTagDTO(TagDTO buildTag, String jwt){
        TagDTO tagDTO = webTestClient
                .post()
                .uri("/api/v1/scrum-service/tag/add")
                .header("Authorization", jwt)
                .body(Mono.just(buildTag), TagDTO.class)
                .exchange()
                .expectBody(TagDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(tagDTO);
        return tagDTO;
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

    private TaskDTO getTask(String id, String jwt){
        TaskDTO taskDTO = webTestClient
                .get()
                .uri("/api/v1/scrum-service/task/{taskId}", id)
                .header("Authorization", jwt)
                .exchange()
                .expectBody(TaskDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(taskDTO);
        return  taskDTO;
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
    }

    private void assertTag(TagDTO expected, TagDTO actual){
        assertEquals(expected.getName(), actual.getName());
        assertEquals(expected.getColor(), actual.getColor());
    }

    private void assertLog(TaskMovementLogDTO expected, TaskMovementLogDTO actual){
        assertNotNull(actual.getTask());
        assertEquals(expected.getTask().getId(), actual.getTask().getId());
        assertNotNull(actual.getUser());
        assertEquals(expected.getUser().getId(), actual.getUser().getId());
        assertEquals(expected.getUser().getEmail(), actual.getUser().getEmail());
        assertEquals(expected.getUser().getFirstName(), actual.getUser().getFirstName());
        assertEquals(expected.getUser().getLastName(), actual.getUser().getLastName());
        //assertEquals(expected.getStartDate(), actual.getStartDate());
        //assertEquals(expected.getEndDate(), actual.getEndDate());
        //assertEquals(expected.getWastedTime(), actual.getWastedTime());
        assertEquals(expected.getStatus(), actual.getStatus());
    }

    @BeforeAll
    public void setUp() {
        RegisterRequest initiatorRequest = buildRegisterRequest("sprint.initiator@gmail.com", "initiator", List.of(Role.INITIATOR));
        RegisterRequest expertRequest = buildRegisterRequest("sprint.expert@gmail.com", "expert", List.of(Role.EXPERT));
        RegisterRequest officeRequest = buildRegisterRequest("sprint.office@gmail.com", "office", List.of(Role.PROJECT_OFFICE));
        RegisterRequest adminRequest = buildRegisterRequest("sprint.admin@gmail.com", "admin", List.of(Role.ADMIN));
        RegisterRequest memberRequest = buildRegisterRequest("sprint.member@gmail.com", "member", List.of(Role.MEMBER));
        RegisterRequest leaderRequest = buildRegisterRequest("sprint.leader@gmail.com", "leader", List.of(Role.TEAM_LEADER));
        RegisterRequest ownerRequest = buildRegisterRequest("sprint.owner@gmail.com", "owner", List.of(Role.TEAM_OWNER));
        RegisterRequest teacherRequest = buildRegisterRequest("sprint.teacher@gmail.com", "teacher", List.of(Role.TEACHER));

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

        UserDTO admin = buildUser(adminResponse);
        member = buildUser(memberResponse);
        leader = buildUser(leaderResponse);

        groupExpert = createGroup(buildGroup("exp",List.of(Role.EXPERT), buildUser(expertResponse)), jwt_admin);
        groupProjectOffice = createGroup(buildGroup("pro",List.of(Role.PROJECT_OFFICE), buildUser(officeResponse)), jwt_admin);

        skill1 = createSkill(buildSkill("skill1"), jwt_admin);
        skill2 = createSkill(buildSkill("skill2"), jwt_admin);

        tag1 = createTagDTO(buildTag("Frontend","blue"), jwt_admin);
        tag2 = createTagDTO(buildTag("Backend","orange"), jwt_admin);

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
                .leader(leader)
                .members(List.of(leader, member))
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

        String ideaMarketDTOId = createMarketIdea(admin, jwt_admin).getId();
        assertNull(getMarketIdea(ideaMarketDTOId, jwt_admin).getTeam());
        acceptTeam(ideaMarketDTOId, createMarketTeamRequest(ideaMarketDTOId, jwt_owner).getTeamId(), jwt_admin);
        createdProject = webTestClient
                .post()
                .uri("/api/v1/scrum-service/project/send")
                .header("Authorization", jwt_admin)
                .body(Mono.just(getMarketIdea(ideaMarketDTOId, jwt_admin)), IdeaMarketDTO.class)
                .exchange()
                .expectBody(ProjectDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(createdProject);

        SprintDTO sprintDTO = new SprintDTO(
                null,
                createdProject.getId(),
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

        TaskDTO taskDTO = new TaskDTO(
                null,
                createdSprint.getId(),
                createdProject.getId(),
                null,
                "задача",
                "мега описание задачи",
                null,
                null,
                null,
                22.0,
                LocalDate.now(),
                LocalDate.now().plusDays(2),
                List.of(tag1,tag2),
                null
        );
        createdTask = webTestClient
                .post()
                .uri("/api/v1/scrum-service/task/add")
                .header("Authorization", jwt_admin)
                .body(Mono.just(taskDTO), TaskDTO.class)
                .exchange()
                .expectBody(TaskDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(createdTask);
    }

    @Test
    void testGetAllLogs(){
        TaskMovementLogDTO log1 = createLog(buildLog(createdTask,member),jwt_admin);
        TaskMovementLogDTO log2 = createLog(buildLog(createdTask,member),jwt_admin);
        TaskMovementLogDTO log3 = createLog(buildLog(createdTask,member),jwt_admin);
        List<TaskMovementLogDTO> logs = getListLog(createdTask.getId());
        assertNotNull(logs);
        assertTrue(logs.size() >= 3);
        logs.forEach(l -> {
            if (Objects.equals(log1.getId(), l.getId())){
                assertLog(log1, l);
            }
            else if (Objects.equals(log2.getId(), l.getId())){
                assertLog(log2, l);
            }
            else if (Objects.equals(log3.getId(), l.getId())){
                assertLog(log3, l);
            }
        });
    }

    /*@Test
    void testAddTaskLog() {

    }*/
}