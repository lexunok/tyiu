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
public class TaskControllerTest extends TestContainers {

    @Autowired
    private WebTestClient webTestClient;

    private final String path = "/api/v1/ideas-service/market/idea";
    private final String main_path = "/api/v1/scrum-service/task";

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

    private TagDTO buildTag(String name, String color){
        return new TagDTO(null,name, color, true, null, null, null);
    }

    private TaskDTO buildTask(List<TagDTO> tags, String sprintId){
        return new TaskDTO(
                null,
                sprintId,
                createdProject.getId(),
                null,
                "задача",
                "мега описание задачи",
                null,
                null,
                null,
                null,
                22.0,
                LocalDate.now(),
                LocalDate.now().plusDays(2),
                tags,
                null
        );
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

    private TaskDTO createTask(TaskDTO buildTask, String jwt, UserDTO user){
        TaskDTO createdTask = webTestClient
                .post()
                .uri("/api/v1/scrum-service/task/add")
                .header("Authorization", jwt)
                .body(Mono.just(buildTask), TaskDTO.class)
                .exchange()
                .expectBody(TaskDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(createdTask);
        TaskDTO taskDTO = getTask(createdTask.getId(), jwt_admin);
        assertNotNull(taskDTO);
        assertNotNull(taskDTO.getId());
        assertEquals(taskDTO.getProjectId(), buildTask.getProjectId());
        if (taskDTO.getSprintId() == null){
            assertNotNull(taskDTO.getPosition());
            assertTrue(taskDTO.getPosition() >= 1);
            assertEquals(taskDTO.getStatus(), TaskStatus.InBackLog);
        }
        else {
            assertEquals(taskDTO.getSprintId(), buildTask.getSprintId());
            assertNull(taskDTO.getPosition());
            assertEquals(taskDTO.getStatus(), TaskStatus.NewTask);
        }
        assertEquals(taskDTO.getName(), buildTask.getName());
        assertEquals(taskDTO.getDescription(), buildTask.getDescription());
        assertNotNull(taskDTO.getInitiator());
        assertEquals(taskDTO.getInitiator().getId(), user.getId());
        assertEquals(taskDTO.getWorkHour(), buildTask.getWorkHour());
        assertNotNull(taskDTO.getTags());
        assertNotNull(buildTask.getTags());
        taskDTO.getTags().forEach(taskTag -> {
            if (Objects.equals(taskTag.getId(), tag1.getId())){
                assertTag(taskTag, tag1);
            }
            else if (Objects.equals(taskTag.getId(), tag2.getId())){
                assertTag(taskTag, tag2);
            }
            else {
                fail();
            }
        });
        assertEquals(taskDTO.getTags().size(), buildTask.getTags().size());
        return taskDTO;
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

    private TaskDTO getTask(String id, String jwt){
        TaskDTO taskDTO = webTestClient
                .get()
                .uri(main_path + "/{taskId}", id)
                .header("Authorization", jwt)
                .exchange()
                .expectBody(TaskDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(taskDTO);
        assertNotNull(taskDTO.getId());
        assertNotNull(taskDTO.getProjectId());
        if (taskDTO.getSprintId() == null){
            assertNotNull(taskDTO.getPosition());
            assertTrue(taskDTO.getPosition() >= 1);
            assertEquals(taskDTO.getStatus(), TaskStatus.InBackLog);
        }
        else {
            assertNull(taskDTO.getPosition());
            assertEquals(taskDTO.getStatus(), TaskStatus.NewTask);
        }
        assertNotNull(taskDTO.getName());
        assertNotNull(taskDTO.getDescription());
        assertNotNull(taskDTO.getInitiator());
        assertNotNull(taskDTO.getWorkHour());
        assertNotNull(taskDTO.getTags());
        assertNotNull(taskDTO.getTags().get(0));
        assertNotNull(taskDTO.getStatus());
        return  taskDTO;
    }

    private List<TaskDTO> getListTasks(String path, String id){
        return webTestClient
                .get()
                .uri(main_path + path, id)
                .header("Authorization", jwt_admin)
                .exchange()
                .expectBodyList(TaskDTO.class)
                .returnResult().getResponseBody();
    }

    private void updateTaskPosition(String taskId, Integer position){
        webTestClient
                .put()
                .uri(main_path + "/move/{taskId}/{position}", taskId, position)
                .header("Authorization", jwt_admin)
                .exchange()
                .expectStatus().isOk();
    }

    private void updateSomething(String path, String taskId, String data){
        webTestClient
                .put()
                .uri(main_path + path, taskId)
                .body(Mono.just(data), String.class)
                .header("Authorization", jwt_admin)
                .exchange()
                .expectStatus().isOk();
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

    private void assertTask(TaskDTO expected, TaskDTO actual){
        assertEquals(expected.getSprintId(), actual.getSprintId());
        assertEquals(expected.getProjectId(), actual.getProjectId());
        assertEquals(expected.getPosition(), actual.getPosition());
        assertEquals(expected.getName(), actual.getName());
        assertEquals(expected.getDescription(), actual.getDescription());
        assertEquals(expected.getLeaderComment(), actual.getLeaderComment());
        assertNotNull(expected.getInitiator());
        assertNotNull(actual.getInitiator());
        assertEquals(expected.getInitiator().getId(), actual.getInitiator().getId());
        assertEquals(expected.getInitiator().getEmail(), actual.getInitiator().getEmail());
        assertEquals(expected.getInitiator().getFirstName(), actual.getInitiator().getFirstName());
        assertEquals(expected.getInitiator().getLastName(), actual.getInitiator().getLastName());
        assertNotNull(expected.getExecutor());
        assertNotNull(actual.getExecutor());
        assertEquals(expected.getExecutor().getId(), actual.getExecutor().getId());
        assertEquals(expected.getExecutor().getEmail(), actual.getExecutor().getEmail());
        assertEquals(expected.getExecutor().getFirstName(), actual.getExecutor().getFirstName());
        assertEquals(expected.getExecutor().getLastName(), actual.getExecutor().getLastName());
        assertEquals(expected.getWorkHour(), actual.getWorkHour());
        assertEquals(expected.getStartDate(), actual.getStartDate());
        assertEquals(expected.getFinishDate(), actual.getFinishDate());
        assertNotNull(expected.getTags());
        assertNotNull(actual.getTags());
        actual.getTags().forEach(tag ->
                expected.getTags().forEach(taskTag -> {
                    if (Objects.equals(taskTag.getId(), tag1.getId())){
                        assertTag(taskTag, tag1);
                    }
                    else if (Objects.equals(taskTag.getId(), tag2.getId())){
                        assertTag(taskTag, tag2);
                    }
                    else {
                        fail();
                    }
                })
        );
        assertEquals(expected.getTags().size(), actual.getTags().size());
        assertEquals(expected.getStatus(), actual.getStatus());
    }

    private void assertTag(TagDTO expected, TagDTO actual){
        assertEquals(expected.getName(), actual.getName());
        assertEquals(expected.getColor(), actual.getColor());
    }

    private StatusAssertions checkGetAllTaskByProject(String jwt){
        return webTestClient
                .get()
                .uri(main_path + "/project/all/{projectId}", createdProject.getId())
                .header("Authorization", jwt)
                .exchange()
                .expectStatus();
    }

    private StatusAssertions checkGetAllTasksInBackLog(String jwt){
        return webTestClient
                .get()
                .uri(main_path + "/project/backlog/{projectId}", createdProject.getId())
                .header("Authorization", jwt)
                .exchange()
                .expectStatus();
    }

    private StatusAssertions checkGetAllTasksInSprint(String jwt){
        return webTestClient
                .get()
                .uri(main_path + "/project/sprint/{sprintId}", createdProject.getId())
                .header("Authorization", jwt)
                .exchange()
                .expectStatus();
    }

    private StatusAssertions checkGetOneTaskById(String id, String jwt){
        return webTestClient
                .get()
                .uri(main_path + "/{taskId}", id)
                .header("Authorization", jwt)
                .exchange()
                .expectStatus();
    }

    private StatusAssertions checkPostCreateTask(TaskDTO buildTask, String jwt){
        return webTestClient
                .post()
                .uri(main_path + "/add")
                .header("Authorization", jwt)
                .body(Mono.just(buildTask), TaskDTO.class)
                .exchange()
                .expectStatus();
    }

    private StatusAssertions checkPutUpdateTask(TaskDTO buildTask, String taskId, String jwt){
        return webTestClient
                .put()
                .uri(main_path + "/update/{taskId}", taskId)
                .header("Authorization", jwt)
                .body(Mono.just(buildTask), TaskDTO.class)
                .exchange()
                .expectStatus();
    }

    private StatusAssertions checkPutUpdateExecutorTask(String taskId, String jwt){
        return webTestClient
                .put()
                .uri(main_path + "/executor/{taskId}/{executorId}", taskId, member.getId())
                .header("Authorization", jwt)
                .exchange()
                .expectStatus();
    }

    private StatusAssertions checkDeleteTask(String taskId, String jwt){
        return webTestClient
                .delete()
                .uri(main_path + "/delete/{taskId}", taskId)
                .header("Authorization", jwt)
                .exchange()
                .expectStatus();
    }

    @BeforeAll
    public void setUp() {
        RegisterRequest initiatorRequest = buildRegisterRequest("task.initiator@gmail.com", "initiator", List.of(Role.INITIATOR));
        RegisterRequest expertRequest = buildRegisterRequest("task.expert@gmail.com", "expert", List.of(Role.EXPERT));
        RegisterRequest officeRequest = buildRegisterRequest("task.office@gmail.com", "office", List.of(Role.PROJECT_OFFICE));
        RegisterRequest adminRequest = buildRegisterRequest("task.admin@gmail.com", "admin", List.of(Role.ADMIN));
        RegisterRequest memberRequest = buildRegisterRequest("task.member@gmail.com", "member", List.of(Role.MEMBER));
        RegisterRequest leaderRequest = buildRegisterRequest("task.leader@gmail.com", "leader", List.of(Role.TEAM_LEADER));
        RegisterRequest ownerRequest = buildRegisterRequest("task.owner@gmail.com", "owner", List.of(Role.TEAM_OWNER));
        RegisterRequest teacherRequest = buildRegisterRequest("task.teacher@gmail.com", "teacher", List.of(Role.TEACHER));

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
        member = buildUser(memberResponse);

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
    }

    @Test
    void testGetAllTaskByProject(){
        TaskDTO task1 = getTask(createTask(buildTask(List.of(tag1, tag2), null), jwt_admin, admin).getId(), jwt_admin);
        TaskDTO task2 = getTask(createTask(buildTask(List.of(tag1), null), jwt_admin, admin).getId(), jwt_admin);
        TaskDTO task3 = getTask(createTask(buildTask(List.of(tag2), null), jwt_admin, admin).getId(), jwt_admin);
        TaskDTO task4 = getTask(createTask(buildTask(List.of(tag2, tag1), null), jwt_admin, admin).getId(), jwt_admin);
        List<TaskDTO> projectTasks = getListTasks("/project/all/{projectId}", createdProject.getId());
        assertNotNull(projectTasks);
        assertTrue(projectTasks.size() >= 4);
        projectTasks.forEach(t -> {
            if (Objects.equals(task1.getId(), t.getId())){
                assertTask(task1, t);
            }
            else if (Objects.equals(task2.getId(), t.getId())){
                assertTask(task2, t);
            }
            else if (Objects.equals(task3.getId(), t.getId())){
                assertTask(task3, t);
            }
            else if (Objects.equals(task4.getId(), t.getId())){
                assertTask(task4, t);
            }
        });
        checkGetAllTaskByProject(jwt_initiator).isOk();
        checkGetAllTaskByProject(jwt_expert).isForbidden();
        checkGetAllTaskByProject(jwt_office).isOk();
        checkGetAllTaskByProject(jwt_member).isOk();
        checkGetAllTaskByProject(jwt_leader).isOk();
        checkGetAllTaskByProject(jwt_owner).isOk();
        checkGetAllTaskByProject(jwt_teacher).isForbidden();
    }

    @Test
    void testGetAllTasksInBackLog(){
        TaskDTO task1 = getTask(createTask(buildTask(List.of(tag1, tag2), null), jwt_admin, admin).getId(), jwt_admin);
        TaskDTO task2 = getTask(createTask(buildTask(List.of(tag1), null), jwt_admin, admin).getId(), jwt_admin);
        TaskDTO task3 = getTask(createTask(buildTask(List.of(tag2), null), jwt_admin, admin).getId(), jwt_admin);
        TaskDTO task4 = getTask(createTask(buildTask(List.of(tag2, tag1), null), jwt_admin, admin).getId(), jwt_admin);
        getTask(createTask(buildTask(List.of(tag2, tag1), createdSprint.getId()), jwt_admin, admin).getId(), jwt_admin);
        getTask(createTask(buildTask(List.of(tag2, tag1), createdSprint.getId()), jwt_admin, admin).getId(), jwt_admin);
        List<TaskDTO> projectBackLogTasks = getListTasks("/project/backlog/{projectId}", createdProject.getId());
        assertNotNull(projectBackLogTasks);
        List<TaskDTO> projectTasks = getListTasks("/project/all/{projectId}", createdProject.getId());
        assertNotNull(projectTasks);
        assertTrue(projectBackLogTasks.size() <= projectTasks.size());
        projectBackLogTasks.forEach(t -> {
            if (Objects.equals(task1.getId(), t.getId())){
                assertTask(task1, t);
            }
            else if (Objects.equals(task2.getId(), t.getId())){
                assertTask(task2, t);
            }
            else if (Objects.equals(task3.getId(), t.getId())){
                assertTask(task3, t);
            }
            else if (Objects.equals(task4.getId(), t.getId())){
                assertTask(task4, t);
            }
        });
        checkGetAllTasksInBackLog(jwt_initiator).isOk();
        checkGetAllTasksInBackLog(jwt_expert).isForbidden();
        checkGetAllTasksInBackLog(jwt_office).isOk();
        checkGetAllTasksInBackLog(jwt_member).isOk();
        checkGetAllTasksInBackLog(jwt_leader).isOk();
        checkGetAllTasksInBackLog(jwt_owner).isOk();
        checkGetAllTasksInBackLog(jwt_teacher).isForbidden();
    }

    @Test
    void testGetAllTasksInSprint(){
        TaskDTO task1 = getTask(createTask(buildTask(List.of(tag1, tag2), createdSprint.getId()), jwt_admin, admin).getId(), jwt_admin);
        TaskDTO task2 = getTask(createTask(buildTask(List.of(tag1), createdSprint.getId()), jwt_admin, admin).getId(), jwt_admin);
        TaskDTO task3 = getTask(createTask(buildTask(List.of(tag2), createdSprint.getId()), jwt_admin, admin).getId(), jwt_admin);
        TaskDTO task4 = getTask(createTask(buildTask(List.of(tag2, tag1), createdSprint.getId()), jwt_admin, admin).getId(), jwt_admin);
        List<TaskDTO> sprintTasks = getListTasks("/project/sprint/{sprintId}", createdSprint.getId());
        assertNotNull(sprintTasks);
        sprintTasks.forEach(t -> {
            if (Objects.equals(task1.getId(), t.getId())){
                assertTask(task1, t);
            }
            else if (Objects.equals(task2.getId(), t.getId())){
                assertTask(task2, t);
            }
            else if (Objects.equals(task3.getId(), t.getId())){
                assertTask(task3, t);
            }
            else if (Objects.equals(task4.getId(), t.getId())){
                assertTask(task4, t);
            }
        });
        checkGetAllTasksInSprint(jwt_initiator).isOk();
        checkGetAllTasksInSprint(jwt_expert).isForbidden();
        checkGetAllTasksInSprint(jwt_office).isOk();
        checkGetAllTasksInSprint(jwt_member).isOk();
        checkGetAllTasksInSprint(jwt_leader).isOk();
        checkGetAllTasksInSprint(jwt_owner).isOk();
        checkGetAllTasksInSprint(jwt_teacher).isForbidden();
    }

    @Test
    void testGetOneTaskById(){
        String taskId = getTask(createTask(buildTask(List.of(tag1, tag2), null), jwt_admin, admin).getId(), jwt_admin).getId();
        getTask(createTask(buildTask(List.of(tag1, tag2), createdSprint.getId()), jwt_admin, admin).getId(), jwt_admin);
        checkGetOneTaskById(taskId, jwt_initiator).isOk();
        checkGetOneTaskById(taskId, jwt_expert).isForbidden();
        checkGetOneTaskById(taskId, jwt_office).isOk();
        checkGetOneTaskById(taskId, jwt_member).isOk();
        checkGetOneTaskById(taskId, jwt_leader).isOk();
        checkGetOneTaskById(taskId, jwt_owner).isOk();
        checkGetOneTaskById(taskId, jwt_teacher).isForbidden();
    }

    @Test
    void testPostCreateTask(){
        createTask(buildTask(List.of(tag1, tag2), null), jwt_admin, admin);
        createTask(buildTask(List.of(tag1), createdSprint.getId()), jwt_admin, admin);
        checkPostCreateTask(buildTask(List.of(tag1, tag2), null), jwt_initiator).isOk();
        checkPostCreateTask(buildTask(List.of(tag1, tag2), null), jwt_expert).isForbidden();
        checkPostCreateTask(buildTask(List.of(tag1, tag2), null), jwt_office).isOk();
        checkPostCreateTask(buildTask(List.of(tag1, tag2), null), jwt_member).isOk();
        checkPostCreateTask(buildTask(List.of(tag1, tag2), null), jwt_leader).isOk();
        checkPostCreateTask(buildTask(List.of(tag1, tag2), null), jwt_owner).isOk();
        checkPostCreateTask(buildTask(List.of(tag1, tag2), null), jwt_teacher).isForbidden();
    }

    @Test
    void testPutUpdateTask(){
        TaskDTO task1 = getTask(createTask(buildTask(List.of(tag1, tag2), createdSprint.getId()), jwt_admin, admin).getId(), jwt_admin);
        TaskDTO updateTask = new TaskDTO(
                task1.getId(),
                task1.getSprintId(),
                task1.getProjectId(),
                task1.getPosition(),
                "ЗАДАЧА НОВОГО УРОВНЯ",
                "ОПИСАНИЕ НОВОГО УРОВНЯ",
                task1.getLeaderComment(),
                task1.getExecutorComment(),
                task1.getInitiator(),
                task1.getExecutor(),
                45.0,
                task1.getStartDate(),
                task1.getFinishDate(),
                List.of(tag2),
                task1.getStatus()
        );
        assertNotEquals(updateTask.getName(), task1.getName());
        assertNotEquals(updateTask.getDescription(), task1.getDescription());
        assertNotEquals(updateTask.getWorkHour(), task1.getWorkHour());
        assertNotNull(updateTask.getTags());
        assertNotNull(task1.getTags());
        assertNotEquals(updateTask.getTags().size(), task1.getTags().size());
        InfoResponse response = webTestClient
                .put()
                .uri(main_path + "/update/{taskId}", task1.getId())
                .header("Authorization", jwt_admin)
                .body(Mono.just(updateTask), TaskDTO.class)
                .exchange()
                .expectBody(InfoResponse.class)
                .returnResult().getResponseBody();
        assertNotNull(response);
        assertEquals(response.getStatusCode(), HttpStatus.OK);
        assertEquals(response.getMessage(), "Задача успешно изменена");
        assertTask(updateTask, getTask(task1.getId(), jwt_admin));
        checkPutUpdateTask(updateTask, task1.getId(), jwt_initiator).isOk();
        checkPutUpdateTask(updateTask, task1.getId(), jwt_expert).isForbidden();
        checkPutUpdateTask(updateTask, task1.getId(), jwt_office).isOk();
        checkPutUpdateTask(updateTask, task1.getId(), jwt_member).isOk();
        checkPutUpdateTask(updateTask, task1.getId(), jwt_leader).isOk();
        checkPutUpdateTask(updateTask, task1.getId(), jwt_owner).isOk();
        checkPutUpdateTask(updateTask, task1.getId(), jwt_teacher).isForbidden();
    }

    @Test
    void testPutUpdateExecutorTask(){
        TaskDTO task1 = getTask(createTask(buildTask(List.of(tag1, tag2), createdSprint.getId()), jwt_admin, admin).getId(), jwt_admin);
        assertNotNull(task1.getExecutor());
        assertNotEquals(task1.getExecutor().getId(), member.getId());
        InfoResponse response = webTestClient
                .put()
                .uri(main_path + "/executor/{taskId}/{executorId}", task1.getId(), member.getId())
                .header("Authorization", jwt_admin)
                .exchange()
                .expectBody(InfoResponse.class)
                .returnResult().getResponseBody();
        assertNotNull(response);
        assertEquals(response.getStatusCode(), HttpStatus.OK);
        assertEquals(response.getMessage(), "Новый исполнитель успешно назначен");
        task1.setExecutor(member);
        assertTask(task1, getTask(task1.getId(), jwt_admin));
        checkPutUpdateExecutorTask(task1.getId(), jwt_initiator).isOk();
        checkPutUpdateExecutorTask(task1.getId(), jwt_expert).isForbidden();
        checkPutUpdateExecutorTask(task1.getId(), jwt_office).isOk();
        checkPutUpdateExecutorTask(task1.getId(), jwt_member).isOk();
        checkPutUpdateExecutorTask(task1.getId(), jwt_leader).isOk();
        checkPutUpdateExecutorTask(task1.getId(), jwt_owner).isOk();
        checkPutUpdateExecutorTask(task1.getId(), jwt_teacher).isForbidden();
    }

    @Test
    void testMoveTask(){
        TaskDTO task1 = getTask(createTask(buildTask(List.of(tag1, tag2), null), jwt_admin, admin).getId(), jwt_admin);
        TaskDTO task2 = getTask(createTask(buildTask(List.of(tag1, tag2), null), jwt_admin, admin).getId(), jwt_admin);
        TaskDTO task3 = getTask(createTask(buildTask(List.of(tag1, tag2), null), jwt_admin, admin).getId(), jwt_admin);
        TaskDTO task4 = getTask(createTask(buildTask(List.of(tag1, tag2), null), jwt_admin, admin).getId(), jwt_admin);
        TaskDTO task5 = getTask(createTask(buildTask(List.of(tag1, tag2), null), jwt_admin, admin).getId(), jwt_admin);
        updateTaskPosition(task1.getId(), task3.getPosition());
        assertEquals(getTask(task1.getId(), jwt_admin).getPosition(), task3.getPosition());
        assertEquals(getTask(task2.getId(), jwt_admin).getPosition(), task1.getPosition());
        assertEquals(getTask(task3.getId(), jwt_admin).getPosition(), task2.getPosition());
        assertEquals(getTask(task4.getId(), jwt_admin).getPosition(), task4.getPosition());
        assertEquals(getTask(task5.getId(), jwt_admin).getPosition(), task5.getPosition());
        updateTaskPosition(task1.getId(), task1.getPosition());
        assertEquals(getTask(task1.getId(), jwt_admin).getPosition(), task1.getPosition());
        assertEquals(getTask(task2.getId(), jwt_admin).getPosition(), task2.getPosition());
        assertEquals(getTask(task3.getId(), jwt_admin).getPosition(), task3.getPosition());
        assertEquals(getTask(task4.getId(), jwt_admin).getPosition(), task4.getPosition());
        assertEquals(getTask(task5.getId(), jwt_admin).getPosition(), task5.getPosition());
    }

    @Test
    void testUpdateLeaderCommentInTask(){
        TaskDTO task1 = getTask(createTask(buildTask(List.of(tag1, tag2), createdSprint.getId()), jwt_admin, admin).getId(), jwt_admin);
        String comment = "коммент???";
        assertNotEquals(task1.getLeaderComment(), comment);
        updateSomething("/leader/comment/{taskId}", task1.getId(), comment);
        assertEquals(getTask(task1.getId(), jwt_admin).getLeaderComment(), comment);
    }

    @Test
    void testUpdateExecutorCommentInTask(){
        TaskDTO task1 = getTask(createTask(buildTask(List.of(tag1, tag2), createdSprint.getId()), jwt_admin, admin).getId(), jwt_admin);
        String comment = "коммент???";
        assertNotEquals(task1.getExecutorComment(), comment);
        updateSomething("/executor/comment/{taskId}", task1.getId(), comment);
        assertEquals(getTask(task1.getId(), jwt_admin).getExecutorComment(), comment);
    }

    @Test
    void testDeleteTask(){
        TaskDTO task1 = getTask(createTask(buildTask(List.of(tag1, tag2), null), jwt_admin, admin).getId(), jwt_admin);
        TaskDTO task2 = getTask(createTask(buildTask(List.of(tag1, tag2), null), jwt_admin, admin).getId(), jwt_admin);
        TaskDTO task3 = getTask(createTask(buildTask(List.of(tag1, tag2), null), jwt_admin, admin).getId(), jwt_admin);
        TaskDTO task4 = getTask(createTask(buildTask(List.of(tag1, tag2), null), jwt_admin, admin).getId(), jwt_admin);
        InfoResponse response = webTestClient
                .delete()
                .uri(main_path + "/delete/{taskId}", task2.getId())
                .header("Authorization", jwt_admin)
                .exchange()
                .expectBody(InfoResponse.class)
                .returnResult().getResponseBody();
        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
        assertEquals("Задача успешно удалена", response.getMessage());
        assertEquals(getTask(task1.getId(), jwt_admin).getPosition(), task1.getPosition());
        assertEquals(getTask(task3.getId(), jwt_admin).getPosition(), task2.getPosition());
        assertEquals(getTask(task4.getId(), jwt_admin).getPosition(), task3.getPosition());
        checkDeleteTask(createTask(buildTask(List.of(tag1, tag2), null), jwt_admin, admin).getId(), jwt_initiator).isOk();
        checkDeleteTask(createTask(buildTask(List.of(tag1, tag2), null), jwt_admin, admin).getId(), jwt_expert).isForbidden();
        checkDeleteTask(createTask(buildTask(List.of(tag1, tag2), null), jwt_admin, admin).getId(), jwt_office).isOk();
        checkDeleteTask(createTask(buildTask(List.of(tag1, tag2), null), jwt_admin, admin).getId(), jwt_member).isOk();
        checkDeleteTask(createTask(buildTask(List.of(tag1, tag2), null), jwt_admin, admin).getId(), jwt_leader).isOk();
        checkDeleteTask(createTask(buildTask(List.of(tag1, tag2), null), jwt_admin, admin).getId(), jwt_owner).isOk();
        checkDeleteTask(createTask(buildTask(List.of(tag1, tag2), null), jwt_admin, admin).getId(), jwt_teacher).isForbidden();
    }
}
