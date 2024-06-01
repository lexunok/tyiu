package com.tyiu.ideas.controller;

import com.tyiu.ideas.model.*;
import com.tyiu.ideas.model.dto.*;
import com.tyiu.ideas.model.entities.Idea;
import com.tyiu.ideas.model.enums.MarketStatus;
import com.tyiu.ideas.model.enums.SkillType;
import com.tyiu.ideas.model.requests.IdeaSkillRequest;
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
public class SprintControllerTest extends TestContainers {

    @Autowired
    private WebTestClient webTestClient;

    private final String path = "/api/v1/ideas-service/market/idea";
    private final String main_path = "/api/v1/scrum-service/sprint";

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

    private TaskDTO buildTask(List<TagDTO> tags){
        return new TaskDTO(
                null,
                null,
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

    private SprintDTO buildSprint(){
        return new SprintDTO(
                null,
                createdProject.getId(),
                "спринт",
                null,
                null,
                "мега цель",
                LocalDate.now(),
                LocalDate.now().plusDays(20),
                22L,
                List.of(createTask(buildTask(List.of(tag1, tag2)), jwt_admin),
                        createTask(buildTask(List.of(tag1, tag2)), jwt_admin))
        );
    }

    private SprintDTO buildSprint(List<TaskDTO> tasks){
        return new SprintDTO(
                null,
                createdProject.getId(),
                "спринт",
                null,
                null,
                "мега цель",
                LocalDate.now(),
                LocalDate.now().plusDays(20),
                22L,
                tasks
        );
    }

    private SprintMarkRequest buildSprintMarkDTO(String sprintId, UserDTO user, ProjectRole role, List<TaskDTO> tasks){
        return new SprintMarkRequest(
                null,
                createdProject.getId(),
                sprintId,
                user.getId(),
                user.getFirstName(),
                user.getLastName(),
                role,
                5.0,
                tasks
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

    private TaskDTO createTask(TaskDTO buildTask, String jwt){
        TaskDTO createdTask = webTestClient
                .post()
                .uri("/api/v1/scrum-service/task/add")
                .header("Authorization", jwt)
                .body(Mono.just(buildTask), TaskDTO.class)
                .exchange()
                .expectBody(TaskDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(createdTask);
        return getTask(createdTask.getId(), jwt_admin);
    }

    private SprintDTO createSprint(SprintDTO buildSprint){
        SprintDTO sprintDTO = webTestClient
                .post()
                .uri(main_path + "/add")
                .header("Authorization", jwt_admin)
                .body(Mono.just(buildSprint), SprintDTO.class)
                .exchange()
                .expectBody(SprintDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(sprintDTO);
        return sprintDTO;
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

    private void finishSprint(String sprintId){
        InfoResponse finishSprint = webTestClient
                .put()
                .uri(main_path + "/finish/{sprintId}", sprintId)
                .header("Authorization", jwt_admin)
                .body(Mono.just("репорт, бан, кик"), String.class)
                .exchange()
                .expectBody(InfoResponse.class)
                .returnResult().getResponseBody();
        assertNotNull(finishSprint);
        assertEquals(HttpStatus.OK, finishSprint.getStatusCode());
        assertEquals("Спринт успешно завершён", finishSprint.getMessage());
    }

    private void assertTag(TagDTO expected, TagDTO actual){
        assertEquals(expected.getName(), actual.getName());
        assertEquals(expected.getColor(), actual.getColor());
    }

    private void assertTask(TaskDTO expected, TaskDTO actual){
        assertEquals(expected.getProjectId(), actual.getProjectId());
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
    }

    private StatusAssertions checkGetAllSprintsByProject(String jwt){
        return webTestClient
                .get()
                .uri(main_path + "/{projectId}/all", createdProject.getId())
                .header("Authorization", jwt)
                .exchange()
                .expectStatus();
    }

    private StatusAssertions checkGetSprintById(String jwt, String sprintId){
        return webTestClient
                .get()
                .uri(main_path + "/{id}", sprintId)
                .header("Authorization", jwt)
                .exchange()
                .expectStatus();
    }

    private StatusAssertions checkGetActiveSprint(String jwt){
        return webTestClient
                .get()
                .uri(main_path + "/{projectId}/active", createdProject.getId())
                .header("Authorization", jwt)
                .exchange()
                .expectStatus();
    }

    private StatusAssertions checkGetAllSprintMarks(String jwt, String sprintId){
        return webTestClient
                .get()
                .uri(main_path + "/marks/{sprintId}/all", sprintId)
                .header("Authorization", jwt)
                .exchange()
                .expectStatus();
    }

    private StatusAssertions checkCreateSprint(String jwt, SprintDTO sprintDTO) {
        WebTestClient.ResponseSpec responseSpec = webTestClient
                .post()
                .uri(main_path + "/add")
                .header("Authorization", jwt)
                .body(Mono.just(sprintDTO), SprintDTO.class)
                .exchange()
                .expectAll();
        if (Objects.equals(jwt, jwt_leader) || Objects.equals(jwt, jwt_office)){
            SprintDTO sprintDTO1 = responseSpec.expectBody(SprintDTO.class).returnResult().getResponseBody();
            assertNotNull(sprintDTO1);
            finishSprint(sprintDTO1.getId());
        }
        return responseSpec.expectStatus();
    }

    private StatusAssertions checkAddSprintMarks(String jwt, String sprintId, List<TaskDTO> tasks){
        finishSprint(sprintId);
        return webTestClient
                .post()
                .uri(main_path + "/marks/{projectId}/{sprintId}/add", createdProject.getId(), sprintId)
                .header("Authorization", jwt)
                .body(Flux.fromIterable(List.of(buildSprintMarkDTO(sprintId, member, ProjectRole.MEMBER, List.of(tasks.get(0))),
                                buildSprintMarkDTO(sprintId, leader, ProjectRole.TEAM_LEADER, List.of(tasks.get(1))))),
                        SprintMarkRequest.class)
                .exchange()
                .expectStatus();
    }

    private StatusAssertions checkUpdateSprintInfo(String jwt, String sprintId, SprintDTO sprintDTO){
        return webTestClient
                .put()
                .uri(main_path + "/{sprintId}/update", sprintId)
                .body(Mono.just(sprintDTO), SprintDTO.class)
                .header("Authorization", jwt)
                .exchange()
                .expectStatus();
    }

    private StatusAssertions checkFinishSprint(String jwt, String sprintId){
        return webTestClient
                .put()
                .uri(main_path + "/finish/{sprintId}", sprintId)
                .body(Mono.just("репорт, бан, кик"), String.class)
                .header("Authorization", jwt)
                .exchange()
                .expectStatus();
    }

    private StatusAssertions checkDeleteSprint(String jwt, String sprintId){
        return webTestClient
                .delete()
                .uri(main_path + "/{sprintId}/delete", sprintId)
                .header("Authorization", jwt)
                .exchange()
                .expectStatus();
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
    }

    @Test
    void testGetAllSprintsByProject(){
        finishSprint(createSprint(buildSprint()).getId());
        String sprintId = createSprint(buildSprint()).getId();
        List<SprintDTO> projectSprints = webTestClient
                .get()
                .uri(main_path + "/{projectId}/all", createdProject.getId())
                .header("Authorization", jwt_admin)
                .exchange()
                .expectBodyList(SprintDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(projectSprints);
        assertTrue(projectSprints.size() >= 2);
        projectSprints.forEach(s -> {
            assertNotNull(s.getId());
            assertNotNull(s.getProjectId());
            assertNotNull(s.getName());
            assertNotNull(s.getStatus());
            assertNotNull(s.getGoal());
            assertNotNull(s.getStartDate());
            assertNotNull(s.getFinishDate());
            assertNotNull(s.getWorkingHours());
            assertNull(s.getTasks());
        });

        checkGetAllSprintsByProject(jwt_initiator).isOk();
        checkGetAllSprintsByProject(jwt_expert).isForbidden();
        checkGetAllSprintsByProject(jwt_office).isOk();
        checkGetAllSprintsByProject(jwt_member).isOk();
        checkGetAllSprintsByProject(jwt_leader).isOk();
        checkGetAllSprintsByProject(jwt_owner).isOk();
        checkGetAllSprintsByProject(jwt_teacher).isForbidden();

        finishSprint(sprintId);
    }

    @Test
    void testGetSprintById(){
        TaskDTO task1 = createTask(buildTask(List.of(tag1, tag2)), jwt_admin);
        TaskDTO task2 = createTask(buildTask(List.of(tag1, tag2)), jwt_admin);

        SprintDTO createdSprint = createSprint(buildSprint(List.of(task1, task2)));
        finishSprint(createdSprint.getId());

        SprintDTO activeSprint = webTestClient
                .get()
                .uri(main_path + "/{id}", createdSprint.getId())
                .header("Authorization", jwt_admin)
                .exchange()
                .expectBody(SprintDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(activeSprint);
        assertEquals(createdSprint.getId(), activeSprint.getId());
        assertEquals(createdSprint.getProjectId(), activeSprint.getProjectId());
        assertEquals(createdSprint.getName(), activeSprint.getName());
        assertEquals("репорт, бан, кик", activeSprint.getReport());
        assertEquals(SprintStatus.DONE, activeSprint.getStatus());
        assertEquals(createdSprint.getGoal(), activeSprint.getGoal());
        assertEquals(createdSprint.getStartDate(), activeSprint.getStartDate());
        assertEquals(LocalDate.now(), activeSprint.getFinishDate());
        assertEquals(createdSprint.getWorkingHours(), activeSprint.getWorkingHours());
        assertNotNull(createdSprint.getTasks());
        createdSprint.getTasks().forEach(t -> {
            if (Objects.equals(t.getId(), task1.getId())){
                assertEquals(t.getSprintId(), createdSprint.getId());
                assertEquals(t.getStatus(), TaskStatus.NewTask);
                assertTask(t,task1);
            }
            else if (Objects.equals(t.getId(), task2.getId())){
                assertEquals(t.getSprintId(), createdSprint.getId());
                assertEquals(t.getStatus(), TaskStatus.NewTask);
                assertTask(t,task2);
            }
            else {
                fail();
            }
        });

        checkGetSprintById(jwt_initiator, createdSprint.getId()).isOk();
        checkGetSprintById(jwt_expert, createdSprint.getId()).isForbidden();
        checkGetSprintById(jwt_office, createdSprint.getId()).isOk();
        checkGetSprintById(jwt_member, createdSprint.getId()).isOk();
        checkGetSprintById(jwt_leader, createdSprint.getId()).isOk();
        checkGetSprintById(jwt_owner, createdSprint.getId()).isOk();
        checkGetSprintById(jwt_teacher, createdSprint.getId()).isForbidden();
    }

    @Test
    void testGetActiveSprint(){
        TaskDTO task1 = createTask(buildTask(List.of(tag1, tag2)), jwt_admin);
        TaskDTO task2 = createTask(buildTask(List.of(tag1, tag2)), jwt_admin);

        SprintDTO createdSprint = createSprint(buildSprint(List.of(task1, task2)));

        SprintDTO activeSprint = webTestClient
                .get()
                .uri(main_path + "/{projectId}/active", createdProject.getId())
                .header("Authorization", jwt_admin)
                .exchange()
                .expectBody(SprintDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(activeSprint);
        assertEquals(createdSprint.getId(), activeSprint.getId());
        assertEquals(createdSprint.getProjectId(), activeSprint.getProjectId());
        assertEquals(createdSprint.getName(), activeSprint.getName());
        assertEquals(createdSprint.getReport(), activeSprint.getReport());
        assertEquals(createdSprint.getStatus(), activeSprint.getStatus());
        assertEquals(createdSprint.getGoal(), activeSprint.getGoal());
        assertEquals(createdSprint.getStartDate(), activeSprint.getStartDate());
        assertEquals(createdSprint.getFinishDate(), activeSprint.getFinishDate());
        assertEquals(createdSprint.getWorkingHours(), activeSprint.getWorkingHours());
        assertNotNull(createdSprint.getTasks());
        createdSprint.getTasks().forEach(t -> {
            if (Objects.equals(t.getId(), task1.getId())){
                assertEquals(t.getSprintId(), createdSprint.getId());
                assertEquals(t.getStatus(), TaskStatus.NewTask);
                assertTask(t,task1);
            }
            else if (Objects.equals(t.getId(), task2.getId())){
                assertEquals(t.getSprintId(), createdSprint.getId());
                assertEquals(t.getStatus(), TaskStatus.NewTask);
                assertTask(t,task2);
            }
            else {
                fail();
            }
        });

        checkGetActiveSprint(jwt_initiator).isOk();
        checkGetActiveSprint(jwt_expert).isForbidden();
        checkGetActiveSprint(jwt_office).isOk();
        checkGetActiveSprint(jwt_member).isOk();
        checkGetActiveSprint(jwt_leader).isOk();
        checkGetActiveSprint(jwt_owner).isOk();
        checkGetActiveSprint(jwt_teacher).isForbidden();

        finishSprint(createdSprint.getId());
    }

    @Test
    void testGetAllSprintMarks(){
        TaskDTO task1 = createTask(buildTask(List.of(tag1, tag2)), jwt_admin);
        TaskDTO task2 = createTask(buildTask(List.of(tag1, tag2)), jwt_admin);
        List<TaskDTO> tasks = List.of(
                task1,
                task2
        );
        String sprintId = createSprint(buildSprint(tasks)).getId();
        checkAddSprintMarks(jwt_admin, sprintId, tasks).isOk();
        List<SprintMarkDTO> sprintMarkDTOS = webTestClient
                .get()
                .uri(main_path + "/marks/{sprintId}/all", sprintId)
                .header("Authorization", jwt_admin)
                .exchange()
                .expectBodyList(SprintMarkDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(sprintMarkDTOS);
        sprintMarkDTOS.forEach(m -> {
            assertNotNull(m.getId());
            assertEquals(sprintId, m.getSprintId());
            assertEquals(createdProject.getId(), m.getProjectId());
            if (Objects.equals(m.getUserId(), member.getId())){
                assertEquals(member.getFirstName(), m.getFirstName());
                assertEquals(member.getLastName(), m.getLastName());
                assertEquals(ProjectRole.MEMBER, m.getProjectRole());
            } else if (Objects.equals(m.getUserId(), leader.getId())){
                assertEquals(leader.getFirstName(), m.getFirstName());
                assertEquals(leader.getLastName(), m.getLastName());
                assertEquals(ProjectRole.TEAM_LEADER, m.getProjectRole());
            }
            assertEquals(5.0, m.getMark());
            assertEquals(1, m.getCountCompletedTasks());
        });
        checkGetAllSprintMarks(jwt_initiator, sprintId).isOk();
        checkGetAllSprintMarks(jwt_expert, sprintId).isForbidden();
        checkGetAllSprintMarks(jwt_office, sprintId).isOk();
        checkGetAllSprintMarks(jwt_member, sprintId).isOk();
        checkGetAllSprintMarks(jwt_leader, sprintId).isOk();
        checkGetAllSprintMarks(jwt_owner, sprintId).isOk();
        checkGetAllSprintMarks(jwt_teacher, sprintId).isForbidden();
    }

    @Test
    void testCreateSprint() {
        finishSprint(createSprint(buildSprint()).getId());

        checkCreateSprint(jwt_initiator, buildSprint()).isForbidden();
        checkCreateSprint(jwt_expert, buildSprint()).isForbidden();
        checkCreateSprint(jwt_office, buildSprint()).isOk();
        checkCreateSprint(jwt_member, buildSprint()).isForbidden();
        checkCreateSprint(jwt_owner, buildSprint()).isForbidden();
        checkCreateSprint(jwt_teacher, buildSprint()).isForbidden();
        checkCreateSprint(jwt_leader, buildSprint()).isOk();
    }

    @Test
    void testAddSprintMarks(){
        List<TaskDTO> tasks = List.of(
                createTask(buildTask(List.of(tag1, tag2)), jwt_admin),
                createTask(buildTask(List.of(tag1, tag2)), jwt_admin)
        );
        checkAddSprintMarks(jwt_expert, createSprint(buildSprint(tasks)).getId(), tasks).isForbidden();
        checkAddSprintMarks(jwt_office, createSprint(buildSprint(tasks)).getId(), tasks).isOk();
        checkAddSprintMarks(jwt_member, createSprint(buildSprint(tasks)).getId(), tasks).isForbidden();
        checkAddSprintMarks(jwt_owner, createSprint(buildSprint(tasks)).getId(), tasks).isForbidden();
        checkAddSprintMarks(jwt_initiator, createSprint(buildSprint(tasks)).getId(), tasks).isOk();
        checkAddSprintMarks(jwt_teacher, createSprint(buildSprint(tasks)).getId(), tasks).isForbidden();
        checkAddSprintMarks(jwt_leader, createSprint(buildSprint(tasks)).getId(), tasks).isForbidden();
        checkAddSprintMarks(jwt_admin, createSprint(buildSprint(tasks)).getId(), tasks).isOk();
    }

    @Test
    void testUpdateSprintInfo(){
        TaskDTO task1 = createTask(buildTask(List.of(tag1, tag2)), jwt_admin);
        TaskDTO task2 = createTask(buildTask(List.of(tag1, tag2)), jwt_admin);
        SprintDTO createdSprint = createSprint(buildSprint(List.of(task1, task2)));
        TaskDTO task3 = createTask(buildTask(List.of(tag1, tag2)), jwt_admin);
        TaskDTO task4 = createTask(buildTask(List.of(tag1, tag2)), jwt_admin);
        SprintDTO buildNewSprint = new SprintDTO(
                createdSprint.getId(),
                createdSprint.getProjectId(),
                "супер мега новое название",
                createdSprint.getReport(),
                createdSprint.getStatus(),
                "передуманная цель",
                LocalDate.now().plusDays(15),
                LocalDate.now().plusDays(25),
                238L,
                List.of(task3, task4)
        );
        InfoResponse updateSprint = webTestClient
                .put()
                .uri(main_path + "/{sprintId}/update", createdSprint.getId())
                .header("Authorization", jwt_admin)
                .body(Mono.just(buildNewSprint), SprintDTO.class)
                .exchange()
                .expectBody(InfoResponse.class)
                .returnResult().getResponseBody();
        assertNotNull(updateSprint);
        assertEquals(HttpStatus.OK, updateSprint.getStatusCode());
        assertEquals("Успешное изменение спринта", updateSprint.getMessage());
        SprintDTO activeSprint = webTestClient
                .get()
                .uri(main_path + "/{projectId}/active", createdProject.getId())
                .header("Authorization", jwt_admin)
                .exchange()
                .expectBody(SprintDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(activeSprint);
        assertEquals(buildNewSprint.getId(), activeSprint.getId());
        assertEquals(buildNewSprint.getProjectId(), activeSprint.getProjectId());
        assertEquals(buildNewSprint.getName(), activeSprint.getName());
        assertEquals(buildNewSprint.getReport(), activeSprint.getReport());
        assertEquals(buildNewSprint.getStatus(), activeSprint.getStatus());
        assertEquals(buildNewSprint.getGoal(), activeSprint.getGoal());
        assertEquals(buildNewSprint.getStartDate(), activeSprint.getStartDate());
        assertEquals(buildNewSprint.getFinishDate(), activeSprint.getFinishDate());
        assertEquals(buildNewSprint.getWorkingHours(), activeSprint.getWorkingHours());
        assertNotNull(activeSprint.getTasks());
        activeSprint.getTasks().forEach(t -> {
            if (Objects.equals(t.getId(), task3.getId())){
                assertEquals(t.getSprintId(), createdSprint.getId());
                assertEquals(t.getStatus(), TaskStatus.NewTask);
                assertNull(t.getPosition());
                assertTask(t,task3);
            }
            else if (Objects.equals(t.getId(), task4.getId())){
                assertEquals(t.getSprintId(), createdSprint.getId());
                assertEquals(t.getStatus(), TaskStatus.NewTask);
                assertNull(t.getPosition());
                assertTask(t,task4);
            }
            else {
                fail();
            }
        });
        finishSprint(createdSprint.getId());

        SprintDTO createSprint2 = createSprint(buildSprint(List.of(task1, task2)));
        checkUpdateSprintInfo(jwt_initiator, createSprint2.getId(), buildNewSprint).isForbidden();
        checkUpdateSprintInfo(jwt_expert, createSprint2.getId(), buildNewSprint).isForbidden();
        checkUpdateSprintInfo(jwt_office, createSprint2.getId(), buildNewSprint).isOk();
        finishSprint(createSprint2.getId());
        SprintDTO createSprint3 = createSprint(buildSprint(List.of(task1, task2)));
        checkUpdateSprintInfo(jwt_member, createSprint3.getId(), buildNewSprint).isForbidden();
        checkUpdateSprintInfo(jwt_owner, createSprint3.getId(), buildNewSprint).isForbidden();
        checkUpdateSprintInfo(jwt_teacher, createSprint3.getId(), buildNewSprint).isForbidden();
        checkUpdateSprintInfo(jwt_leader, createSprint3.getId(), buildNewSprint).isOk();
        finishSprint(createSprint3.getId());
    }

    @Test
    void testFinishSprint(){
        finishSprint(createSprint(buildSprint()).getId());
        String createdSprint2 = createSprint(buildSprint()).getId();
        checkFinishSprint(jwt_leader, createdSprint2).isForbidden();
        checkFinishSprint(jwt_owner, createdSprint2).isForbidden();
        checkFinishSprint(jwt_teacher, createdSprint2).isForbidden();
        checkFinishSprint(jwt_expert, createdSprint2).isForbidden();
        checkFinishSprint(jwt_initiator, createdSprint2).isOk();
        String createdSprint3 = createSprint(buildSprint()).getId();
        checkFinishSprint(jwt_member, createdSprint3).isForbidden();
        checkFinishSprint(jwt_office, createdSprint3).isOk();
    }

    @Test
    void testDeleteSprint(){
        InfoResponse deletedSprint = webTestClient
                .delete()
                .uri(main_path + "/{sprintId}/delete", createSprint(buildSprint()).getId())
                .header("Authorization", jwt_admin)
                .exchange()
                .expectBody(InfoResponse.class)
                .returnResult().getResponseBody();
        assertNotNull(deletedSprint);
        assertEquals(HttpStatus.OK, deletedSprint.getStatusCode());
        assertEquals("Спринт успешно удалён", deletedSprint.getMessage());

        String createdSprint = createSprint(buildSprint()).getId();
        checkDeleteSprint(jwt_initiator, createdSprint).isForbidden();
        checkDeleteSprint(jwt_expert, createdSprint).isForbidden();
        checkDeleteSprint(jwt_office, createdSprint).isOk();
        String createdSprint2 = createSprint(buildSprint()).getId();
        checkDeleteSprint(jwt_member, createdSprint2).isForbidden();
        checkDeleteSprint(jwt_owner, createdSprint2).isForbidden();
        checkDeleteSprint(jwt_teacher, createdSprint2).isForbidden();
        checkDeleteSprint(jwt_leader, createdSprint2).isOk();
    }
}