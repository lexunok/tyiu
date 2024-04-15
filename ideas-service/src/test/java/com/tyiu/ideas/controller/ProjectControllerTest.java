package com.tyiu.ideas.controller;

import com.tyiu.ideas.model.*;
import com.tyiu.ideas.model.dto.*;
import com.tyiu.ideas.model.entities.Idea;
import com.tyiu.ideas.model.entities.IdeaMarket;
import com.tyiu.ideas.model.enums.Role;
import com.tyiu.ideas.model.requests.IdeaSkillRequest;
import com.tyiu.ideas.model.requests.RegisterRequest;
import com.tyiu.ideas.model.responses.AuthenticationResponse;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.web.reactive.server.WebTestClient;
import reactor.core.publisher.Mono;

import java.time.Duration;
import java.time.LocalDate;
import java.time.temporal.ChronoUnit;
import java.time.temporal.TemporalUnit;
import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.springframework.boot.test.context.SpringBootTest.WebEnvironment.RANDOM_PORT;

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringBootTest(webEnvironment = RANDOM_PORT)
public class ProjectControllerTest {

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
                .uri("/api/v1/ideas-service/auth/register")
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

    private TeamDTO buildTeam(String name, String description, Integer membersCount,
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

    private ProjectMemberDTO buildProjectMember(String userId, String teamId, String email, String firstname,
                                                String lastname, ProjectRole role, LocalDate finishDate){
        return new ProjectMemberDTO(
                userId,
                teamId,
                email,
                firstname,
                lastname,
                role,
                LocalDate.now(),
                finishDate
        );
    }

    private ProjectMarksDTO buildProjectMarks(String projectId, String userId, String firstname, String lastname,
                                                   ProjectRole role, Double mark, List<TaskDTO> tasks){
        return new ProjectMarksDTO(
                projectId,
                userId,
                firstname,
                lastname,
                role,
                mark,
                tasks
        );
    }

    private ReportProject buildReport(String projectId, List<ProjectMarksDTO> marks, String report){
        return new ReportProject(
                projectId,
                marks,
                report
        );
    }

    private ProjectDTO buildProject(String name, String description, String customer, UserDTO initiator,
                                    TeamDTO team, List<ProjectMemberDTO> members, ReportProject report,
                                    LocalDate finishDate, ProjectStatus status){
        return new ProjectDTO(
                report.getProjectId(),
                name,
                description,
                customer,
                initiator,
                team,
                members,
                report,
                LocalDate.now(),
                finishDate,
                status
        );
    }

    private ProjectDTO getProject(String projectId) {
        ProjectDTO responseGetProject = getRequest("/api/v1/ideas-service/project/{projectId}", projectId, "Bearer " + jwt_randomUser)
                .expectBody(ProjectDTO.class).returnResult().getResponseBody();
        assertNotNull(responseGetProject);
        return responseGetProject;
    }

    private ProjectDTO createProject(UserDTO initiator, TeamDTO teamDTO) {
        ProjectDTO project = buildProject("Крутой проект","ща все сделаем","чел",
                initiator, teamDTO, List.of(),buildReport("любой",List.of(),"отчёт"),
                LocalDate.now().plus(100, ChronoUnit.DAYS),ProjectStatus.ACTIVE);

        ProjectDTO responseAddProject = createProjectRequest(project)
                .expectBody(ProjectDTO.class).returnResult().getResponseBody();

        assertNotNull(responseAddProject);
        assertEquals(project.getName(), responseAddProject.getName());
        assertEquals(project.getDescription(), responseAddProject.getDescription());
        assertEquals(project.getCustomer(), responseAddProject.getCustomer());
        assertEquals(project.getInitiator(), responseAddProject.getInitiator());
        assertEquals(project.getTeam(), responseAddProject.getTeam());
        assertEquals(project.getMembers(), responseAddProject.getMembers());
        assertEquals(project.getReport(), responseAddProject.getReport());
        assertEquals(project.getFinishDate(), responseAddProject.getFinishDate());
        assertEquals(project.getStatus(), responseAddProject.getStatus());
        return responseAddProject;
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

    private WebTestClient.ResponseSpec createProjectRequest(ProjectDTO projectDTO){
        return postRequest("/api/v1/ideas-service/project/send","Bearer " + jwt_owner)
                .body(Mono.just(projectDTO), ProjectDTO.class).exchange();
    }

    private WebTestClient.ResponseSpec createTeamRequest(TeamDTO teamDTO){
        return postRequest("/api/v1/ideas-service/team/add","Bearer " + jwt_owner)
                .body(Mono.just(teamDTO), TeamDTO.class)
                .exchange();
    }

    @BeforeAll
    public void setUp() {
        AuthenticationResponse response = register("admin.addSkill@gmail.com", "Admin", "Adminov", List.of(Role.ADMIN));
        AuthenticationResponse response1 = register("owner.team@gmail.com", "Owner", "Ownerov", List.of(Role.TEAM_OWNER));
        AuthenticationResponse response2 = register("leader.team@gmail.com", "Leader", "Leaderov", List.of(Role.TEAM_OWNER));
        AuthenticationResponse response3 = register("member.team@gmail.com", "Member", "Memberov", List.of(Role.MEMBER));
        AuthenticationResponse response4 = register("randomUser.team@gmail.com", "Random", "User", List.of(Role.MEMBER));
        AuthenticationResponse response5 = register("initiator.team@gmail.com", "Init", "Idea", List.of(Role.INITIATOR));
        AuthenticationResponse response6 = register("kostya@gmail.com", "kostya", "da", List.of(Role.MEMBER));

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

        webTestClient = webTestClient.mutate()
                .responseTimeout(Duration.ofMillis(90000))
                .build();
    }

    @Test
    void testGetAllProjects() {
        createProject(initiator,createTeam(List.of(leader,member)));
        createProject(initiator,createTeam(List.of(member)));
        List<ProjectDTO> allProjects = getRequest("api/v1/ideas-service/project/all","Bearer " + jwt_randomUser)
                .expectBodyList(ProjectDTO.class).returnResult().getResponseBody();
        assertNotNull(allProjects);
        assertTrue(allProjects.size() > 1);
    }
}
