package com.tyiu.ideas.service;

import com.tyiu.amqp.RabbitMQMessageProducer;
import com.tyiu.client.exceptions.AccessException;
import com.tyiu.client.models.Role;
import com.tyiu.client.models.TeamInvitationRequest;
import com.tyiu.client.models.UserDTO;
import com.tyiu.ideas.model.*;
import com.tyiu.ideas.model.dto.*;
import com.tyiu.ideas.model.enums.*;
import com.tyiu.ideas.model.entities.*;
import com.tyiu.ideas.model.requests.*;
import com.tyiu.ideas.model.entities.relations.*;
import com.tyiu.ideas.model.entities.mappers.TeamMapper;

import io.r2dbc.spi.Row;
import io.r2dbc.spi.Batch;
import org.springframework.beans.factory.annotation.Value;
import reactor.core.publisher.Mono;
import reactor.core.publisher.Flux;
import org.springframework.data.r2dbc.core.R2dbcEntityTemplate;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.*;
import java.time.LocalDate;
import java.util.concurrent.ConcurrentHashMap;

import static org.springframework.data.relational.core.query.Query.query;
import static org.springframework.data.relational.core.query.Update.update;
import static org.springframework.data.relational.core.query.Criteria.where;

@Service
@RequiredArgsConstructor
public class TeamService {

    @Value("${rabbitmq.exchanges.internal}")
    private String internalExchange;
    @Value("${rabbitmq.routing-keys.internal-team-invitation}")
    private String internalTeamInvitationRoutingKey;

    private final R2dbcEntityTemplate template;
    private final RabbitMQMessageProducer rabbitProducer;


    private Mono<Void> sendMailToInviteUserInTeam(String userId, UserDTO userInviter, String teamId) {
        return template.selectOne(query(where("id").is(teamId)), Team.class)
                .flatMap(t -> template.selectOne(query(where("id").is(userId)), User.class)
                        .flatMap(u -> {
                            TeamInvitationRequest request = TeamInvitationRequest.builder()
                                    .teamId(teamId)
                                    .teamName(t.getName())
                                    .receiver(u.getEmail())
                                    .senderFirstName(userInviter.getFirstName())
                                    .senderLastName(userInviter.getLastName())
                                    .build();
                            rabbitProducer.publish(request, internalExchange, internalTeamInvitationRoutingKey);
                            return Mono.empty();
                        }));
    }

    private Flux<SkillDTO> getSkillsByList(List<String> skills) {
        String QUERY = "SELECT user_skill.*, skill.id, skill.name, skill.type " +
                "FROM user_skill " +
                "LEFT JOIN skill ON skill.id = user_skill.skill_id " +
                "WHERE user_skill.user_id IN (:skills)";

        return template.getDatabaseClient()
                .sql(QUERY)
                .bind("skills", skills)
                .map((row, rowMetadata) -> SkillDTO.builder()
                        .id(row.get("id", String.class))
                        .name(row.get("name", String.class))
                        .type(SkillType.valueOf(row.get("type", String.class)))
                        .build())
                .all();
    }

    private Flux<TeamDTO> getFilteredTeam(String QUERY, List<SkillDTO> selectedSkills, String userId) {
        return template.getDatabaseClient()
                .sql(QUERY)
                .bind("skills",selectedSkills.stream().map(SkillDTO::getId).toList())
                .bind("userId", userId)
                .map((row, rowMetadata) -> buildTeamDTO(row))
                .all()
                .distinct();
    }

    private TeamDTO buildTeamDTO(Row row) {
        String teamId = row.get("team_id", String.class);
        TeamDTO teamDTO = TeamDTO.builder()
                .id(teamId)
                .name(row.get("team_name", String.class))
                .description(row.get("team_description", String.class))
                .closed(row.get("team_closed", Boolean.class))
                .hasActiveProject(row.get("team_has_active_project", Boolean.class))
                .membersCount(row.get("member_count", Integer.class))
                .createdAt(row.get("team_created_at", LocalDate.class))
                .owner(UserDTO.builder()
                        .id(row.get("owner_id", String.class))
                        .email(row.get("owner_email", String.class))
                        .firstName(row.get("owner_first_name", String.class))
                        .lastName(row.get("owner_last_name", String.class))
                        .build())
                .isRefused(Objects.equals(row.get("refused_team_id", String.class), teamId) || Boolean.TRUE.equals(row.get("existed_member", Boolean.class)))
                .build();
        String leaderId = row.get("leader_id", String.class);
        if (leaderId != null) {
            teamDTO.setLeader(UserDTO.builder()
                    .id(leaderId)
                    .email(row.get("leader_email", String.class))
                    .firstName(row.get("leader_first_name", String.class))
                    .lastName(row.get("leader_last_name", String.class))
                    .build());
        }
        return teamDTO;
    }

    private Mono<Void> annul(String userId) {
        return template.update(query(where("user_id").is(userId)
                                .and("status").is(RequestStatus.NEW)),
                        update("status", RequestStatus.ANNULLED),
                        TeamRequest.class)
                .then(template.update(query(where("user_id").is(userId)
                                .and("status").is(RequestStatus.NEW)),
                        update("status", RequestStatus.ANNULLED),
                        TeamInvitation.class)).then();
    }

    private Mono<Boolean> checkOwner(String teamId, String userId){
        return template.exists(query(where("id").is(teamId)
                .and("owner_id").is(userId)), Team.class);
    }

    private Mono<Boolean> checkOwnerOrLeader(String teamId, String userId){
        return template.exists(query(where("id").is(teamId)
                .and(where("owner_id").is(userId).or("leader_id").is(userId))), Team.class);
    }

    private Mono<Boolean> checkInitiator(String invitationId, String userId){
        return template.exists(query(where("id").is(invitationId)
                .and("user_id").is(userId)), TeamInvitation.class);
    }

    public Mono<TeamDTO> getTeam(String teamId, String userId) {
        String QUERY = "SELECT " +
                "t.id as team_id, t.name as team_name, t.description as team_description, t.closed as team_closed, t.created_at as team_created_at, t.has_active_project as team_has_active_project, " +
                "tr.team_id as refused_team_id, " +
                "o.id as owner_id, o.email as owner_email, o.first_name as owner_first_name, o.last_name as owner_last_name, " +
                "l.id as leader_id, l.email as leader_email, l.first_name as leader_first_name, l.last_name as leader_last_name, " +
                "m.id as member_id, m.email as member_email, m.first_name as member_first_name, m.last_name as member_last_name, " +
                "s.id as skill_id, s.name as skill_name, s.type as skill_type, " +
                "ws.id as wanted_skill_id, ws.name as wanted_skill_name, ws.type as wanted_skill_type," +
                "(SELECT COUNT(*) FROM team_member WHERE team_id = t.id AND finish_date IS NULL) as member_count, " +
                "(SELECT EXISTS (SELECT 1 FROM team_member WHERE member_id = :userId AND finish_date IS NULL)) as existed_member " +
                "FROM team t " +
                "LEFT JOIN team_refused tr ON tr.user_id = :userId AND tr.team_id = t.id " +
                "LEFT JOIN users o ON t.owner_id = o.id " +
                "LEFT JOIN users l ON t.leader_id = l.id " +
                "LEFT JOIN team_member tm ON t.id = tm.team_id AND tm.finish_date IS NULL " +
                "LEFT JOIN users m ON tm.member_id = m.id " +
                "LEFT JOIN user_skill us ON us.user_id = tm.member_id " +
                "LEFT JOIN skill s ON us.skill_id = s.id " +
                "LEFT JOIN team_wanted_skill tws ON tws.team_id = t.id " +
                "LEFT JOIN skill ws ON tws.skill_id = ws.id " +
                "WHERE t.id = :teamId";

        TeamMapper teamMapper = new TeamMapper();

        return template.getDatabaseClient()
                .sql(QUERY)
                .bind("teamId", teamId)
                .bind("userId", userId)
                .map(teamMapper::apply)
                .all()
                .collectList()
                .map(t -> t.get(0));
    }


    public Flux<TeamDTO> getTeams(String userId) {
        String QUERY = "SELECT " +
                "t.id as team_id, t.name as team_name, t.description as team_description, t.closed as team_closed, t.created_at as team_created_at, t.has_active_project as team_has_active_project, " +
                "tr.team_id AS refused_team_id, " +
                "o.id as owner_id, o.email as owner_email, o.first_name as owner_first_name, o.last_name as owner_last_name, " +
                "l.id as leader_id, l.email as leader_email, l.first_name as leader_first_name, l.last_name as leader_last_name, " +
                "(SELECT COUNT(*) FROM team_member WHERE team_id = t.id AND finish_date IS NULL) as member_count, " +
                "(SELECT EXISTS (SELECT 1 FROM team_member WHERE member_id = :userId AND finish_date IS NULL)) as existed_member " +
                "FROM team t " +
                "LEFT JOIN team_refused tr ON tr.user_id = :userId AND tr.team_id = t.id " +
                "LEFT JOIN users o ON t.owner_id = o.id " +
                "LEFT JOIN users l ON t.leader_id = l.id " +
                "WHERE t.is_deleted = false";

        return template.getDatabaseClient()
                .sql(QUERY)
                .bind("userId", userId)
                .map((row, rowMetadata) -> buildTeamDTO(row)).all();
    }

    public Flux<TeamDTO> getOwnerTeams(String ownerId, String ideaMarketId) {
        String QUERY = "SELECT " +
                "t.id as team_id, t.name as team_name, t.description as team_description, t.closed as team_closed, t.created_at as team_created_at, t.has_active_project as team_has_active_project, " +
                "imr.team_id AS refused_team_id, " +
                "o.id as owner_id, o.email as owner_email, o.first_name as owner_first_name, o.last_name as owner_last_name, " +
                "l.id as leader_id, l.email as leader_email, l.first_name as leader_first_name, l.last_name as leader_last_name, " +
                "(SELECT COUNT(*) FROM team_member WHERE team_id = t.id AND finish_date IS NULL) as member_count " +
                "FROM team t " +
                "LEFT JOIN idea_market_refused imr ON imr.team_id = t.id AND imr.idea_id = (SELECT idea_id FROM idea_market WHERE id = :ideaMarketId) " +
                "LEFT JOIN users o ON t.owner_id = o.id " +
                "LEFT JOIN users l ON t.leader_id = l.id " +
                "WHERE (t.owner_id = :userId OR t.leader_id = :userId) AND t.is_deleted = false";

        return template.getDatabaseClient()
                .sql(QUERY)
                .bind("userId", ownerId)
                .bind("ideaMarketId", ideaMarketId)
                .map((row, rowMetadata) -> {
                    String teamId = row.get("team_id", String.class);
                    TeamDTO teamDTO = TeamDTO.builder()
                            .id(teamId)
                            .name(row.get("team_name", String.class))
                            .description(row.get("team_description", String.class))
                            .closed(row.get("team_closed", Boolean.class))
                            .hasActiveProject(row.get("team_has_active_project", Boolean.class))
                            .membersCount(row.get("member_count", Integer.class))
                            .createdAt(row.get("team_created_at", LocalDate.class))
                            .owner(UserDTO.builder()
                                    .id(row.get("owner_id", String.class))
                                    .email(row.get("owner_email", String.class))
                                    .firstName(row.get("owner_first_name", String.class))
                                    .lastName(row.get("owner_last_name", String.class))
                                    .build())
                            .isRefused(Objects.equals(row.get("refused_team_id", String.class), teamId))
                            .build();
                    String leaderId = row.get("leader_id", String.class);
                    if (leaderId != null) {
                        teamDTO.setLeader(UserDTO.builder()
                                .id(leaderId)
                                .email(row.get("leader_email", String.class))
                                .firstName(row.get("leader_first_name", String.class))
                                .lastName(row.get("leader_last_name", String.class))
                                .build());
                    }
                    return teamDTO;
                }).all().flatMap(team -> template.exists(query(where("team_id").is(team.getId())
                                .and(where("status").in(ProjectStatus.ACTIVE, ProjectStatus.PAUSED))), Project.class)
                        .flatMap(isExist -> {
                            team.setIsAcceptedToIdea(isExist);
                            return Mono.empty();
                        })
                        .thenReturn(team)
                ).sort(Comparator.comparing(TeamDTO::getHasActiveProject)
                        .reversed()
                        .thenComparing(TeamDTO::getIsAcceptedToIdea)
                        .reversed());
    }

    public Flux<TeamMemberDTO> getAllUsersWithSkills() {
        String query = "SELECT u.id as user_id, u.email, u.first_name, u.last_name, " +
                "s.id AS skill_id, s.name AS skill_name, s.type AS skill_type " +
                "FROM users u " +
                "LEFT JOIN user_skill us ON u.id = us.user_id " +
                "LEFT JOIN skill s ON us.skill_id = s.id ORDER BY u.id";

        ConcurrentHashMap<String, TeamMemberDTO> map = new ConcurrentHashMap<>();

        return template.getDatabaseClient()
                .sql(query)
                .map((row, rowMetadata) -> {
                    String userId = row.get("user_id", String.class);
                    String skillId = row.get("skill_id", String.class);
                    TeamMemberDTO member = map.getOrDefault(userId,TeamMemberDTO.builder()
                            .firstName(row.get("first_name", String.class))
                            .lastName(row.get("last_name", String.class))
                            .userId(userId)
                            .email(row.get("email", String.class))
                            .skills(new ArrayList<>())
                            .build());
                    if (skillId!=null) {
                        SkillDTO skill = SkillDTO.builder()
                                .name(row.get("skill_name", String.class))
                                .type(SkillType.valueOf(row.get("skill_type", String.class)))
                                .id(skillId)
                                .build();
                        member.getSkills().add(skill);
                    }
                    map.put(userId,member);
                    return member;
                })
                .all().thenMany(Flux.fromIterable(map.values()));

    }

    public Flux<TeamInvitation> getInvitations(String userId) {
        return template.select(query(where("user_id").is(userId)), TeamInvitation.class);
    }

    public Flux<TeamRequest> getTeamRequests(String teamId) {
        return template.select(query(where("team_id").is(teamId)), TeamRequest.class);
    }

    public Flux<TeamInvitation> getInvitationByTeam(String teamId) {
        return template.select(query(where("team_id").is(teamId)), TeamInvitation.class);
    }

    public Flux<TeamMarketRequestDTO> getTeamMarketRequests(String teamId) {
        String QUERY = """
                SELECT
                    tmr.id, tmr.market_id, tmr.team_id, tmr.status, tmr.letter, 
                    i.name AS name, im.id AS idea_market_id
                FROM team_market_request tmr
                    LEFT JOIN idea i ON i.id = tmr.idea_id
                    LEFT JOIN idea_market im ON im.idea_id = tmr.idea_id
                WHERE tmr.team_id = :teamId
            """;
        return template.getDatabaseClient()
                .sql(QUERY)
                .bind("teamId",teamId)
                .map((row, rowMetadata) -> TeamMarketRequestDTO.builder()
                        .id(row.get("id", String.class))
                        .ideaMarketId(row.get("idea_market_id", String.class))
                        .marketId(row.get("market_id", String.class))
                        .teamId(row.get("team_id", String.class))
                        .name(row.get("name", String.class))
                        .status(RequestStatus.valueOf(row.get("status", String.class)))
                        .letter(row.get("letter", String.class))
                        .build())
                .all();
    }

    public Flux<TeamMemberDTO> getAllUsersInTeams() {
        String QUERY = "SELECT tm.*, u.id, u.email, u.first_name, u.last_name " +
                "FROM team_member tm " +
                "LEFT JOIN users u ON u.id = tm.member_id " +
                "WHERE tm.finish_date IS NULL";
        return template.getDatabaseClient()
                .sql(QUERY)
                .map((row, rowMetadata) -> TeamMemberDTO.builder()
                        .userId(row.get("id", String.class))
                        .email(row.get("email", String.class))
                        .firstName(row.get("first_name", String.class))
                        .lastName(row.get("last_name", String.class))
                        .build())
                .all().distinct();
    }

    public Flux<ProjectDTO> getAllProjectsForTeam(String teamId){
        String QUERY = """
                SELECT
                    p.id AS p_id, p.idea_id AS p_idea_id, p.start_date AS p_start_date, p.finish_date AS p_finish_date, p.status AS p_status,
                    i.name AS i_name, i.description AS i_description, i.customer AS i_customer,
                    u.id AS u_id, u.email AS u_email, u.first_name AS u_first_name, u.last_name AS u_last_name
                FROM project p
                    LEFT JOIN idea i ON i.id = p.idea_id
                    LEFT JOIN users u ON u.id = i.initiator_id
                WHERE p.team_id = :teamId
                """;
        return template.getDatabaseClient()
                .sql(QUERY)
                .bind("teamId", teamId)
                .map((row, rowMetadata) -> new ProjectDTO(
                        row.get("p_id", String.class),
                        row.get("p_idea_id", String.class),
                        row.get("i_name", String.class),
                        row.get("i_description", String.class),
                        row.get("i_customer", String.class),
                        UserDTO.builder()
                                .id(row.get("u_id", String.class))
                                .email(row.get("u_email", String.class))
                                .firstName(row.get("u_first_name", String.class))
                                .lastName(row.get("u_last_name", String.class))
                                .build(),
                        null,
                        null,
                        null,
                        row.get("p_start_date", LocalDate.class),
                        row.get("p_finish_date", LocalDate.class),
                        ProjectStatus.valueOf(row.get("p_status", String.class)))
                )
                .all();
    }

    public Mono<TeamDTO> addTeam(TeamDTO teamDTO) {
        Team team = Team.builder()
                .name(teamDTO.getName())
                .description(teamDTO.getDescription())
                .closed(teamDTO.getClosed())
                .hasActiveProject(false)
                .ownerId(teamDTO.getOwner().getId())
                .leaderId(teamDTO.getLeader() != null && teamDTO.getLeader().getId() != null ? teamDTO.getLeader().getId() : null)
                .createdAt(LocalDate.now())
                .isDeleted(Boolean.FALSE)
                .build();

        return template.insert(team)
                .flatMap(t -> {
                    teamDTO.setId(t.getId());
                    teamDTO.setMembersCount((teamDTO.getMembers() != null) ? teamDTO.getMembers().size() : 0);
                    teamDTO.setCreatedAt(team.getCreatedAt());

                    return template.getDatabaseClient()
                            .sql("UPDATE users SET roles = ARRAY_APPEND(roles, 'TEAM_LEADER') WHERE id = :userId")
                            .bind("userId", t.getLeaderId() != null ? t.getLeaderId() : t.getOwnerId())
                            .then()
                            .then(template.getDatabaseClient().inConnection(connection -> {
                                Batch batch = connection.createBatch();
                                teamDTO.getMembers().forEach(u -> batch.add(
                                        String.format(
                                                "INSERT INTO team_member (team_id, member_id, is_active, start_date) VALUES ('%s', '%s', '%s', '%s');",
                                                t.getId(), u.getId(), Boolean.TRUE, LocalDate.now()
                                        ))
                                );

                                return Mono.from(batch.execute());

                            }))
                            .then(template.getDatabaseClient().inConnection(connection -> {
                                Batch batch = connection.createBatch();
                                teamDTO.getWantedSkills().forEach(s -> batch.add(
                                        String.format(
                                                "INSERT INTO team_wanted_skill (team_id, skill_id) VALUES ('%s', '%s');",
                                                t.getId(), s.getId()
                                        ))
                                );

                                return Mono.from(batch.execute());

                            }))
                            .thenReturn(teamDTO);
                });
    }

    public Flux<TeamDTO> getTeamsBySkills(List<SkillDTO> selectedSkills, Role role, String userId) {
        String QUERY = """
                SELECT
                    t.id as team_id, t.name as team_name, t.description as team_description, t.closed as team_closed,
                    t.created_at as team_created_at, t.has_active_project as team_has_active_project,
                    tr.team_id as refused_team_id,
                    o.id as owner_id, o.email as owner_email, o.first_name as owner_first_name, o.last_name as owner_last_name,
                    l.id as leader_id, l.email as leader_email, l.first_name as leader_first_name, l.last_name as leader_last_name,
                    (SELECT COUNT(*) FROM team_member WHERE team_id = t.id AND finish_date IS NULL) as member_count,
                    (SELECT EXISTS (SELECT 1 FROM team_member WHERE member_id = :userId AND finish_date IS NULL)) as existed_member
                FROM team t
                    LEFT JOIN team_refused tr ON tr.user_id = :userId AND tr.team_id = t.id
                    LEFT JOIN users o ON t.owner_id = o.id
                    LEFT JOIN users l ON t.leader_id = l.id
                    LEFT JOIN team_member tm ON tm.team_id = t.id AND tm.finish_date IS NULL
                    LEFT JOIN user_skill us ON us.user_id = tm.member_id
                """;
        if (role == Role.INITIATOR)
        {
            QUERY = QUERY + "WHERE us.skill_id IN (:skills) AND t.is_deleted = false";
        }
        else {
            QUERY = QUERY + "LEFT JOIN team_wanted_skill tws ON tws.team_id = t.id " +
                    "WHERE (us.skill_id IN (:skills) OR tws.skill_id IN (:skills)) AND t.is_deleted = false";
        }

        return getFilteredTeam(QUERY, selectedSkills, userId);
    }

    public Flux<TeamDTO> getTeamsByVacancies(List<SkillDTO> selectedSkills, String userId) {
        String QUERY = "SELECT " +
                "t.id as team_id, t.name as team_name, t.description as team_description, t.closed as team_closed, t.created_at as team_created_at, t.has_active_project as team_has_active_project, " +
                "tr.team_id as refused_team_id, " +
                "o.id as owner_id, o.email as owner_email, o.first_name as owner_first_name, o.last_name as owner_last_name, " +
                "l.id as leader_id, l.email as leader_email, l.first_name as leader_first_name, l.last_name as leader_last_name, " +
                "(SELECT COUNT(*) FROM team_member WHERE team_id = t.id AND finish_date IS NULL) as member_count, " +
                "(SELECT EXISTS (SELECT 1 FROM team_member WHERE member_id = :userId AND finish_date IS NULL)) as existed_member " +
                "FROM team t " +
                "LEFT JOIN team_refused tr ON tr.user_id = :userId AND tr.team_id = t.id " +
                "LEFT JOIN users o ON t.owner_id = o.id " +
                "LEFT JOIN users l ON t.leader_id = l.id " +
                "LEFT JOIN team_member tm ON tm.team_id = t.id AND tm.finish_date IS NULL " +
                "LEFT JOIN user_skill us ON us.user_id = tm.member_id " +
                "LEFT JOIN team_wanted_skill tws ON tws.team_id = t.id " +
                "WHERE tws.skill_id IN (:skills) AND us.skill_id NOT IN (:skills) AND t.is_deleted = false";

        return getFilteredTeam(QUERY, selectedSkills, userId);
    }

    public Flux<TeamInvitation> sendInvitesToUsers(Flux<TeamInvitation> users, UserDTO userInviter) {
        return users.flatMap(user -> {
            user.setStatus(RequestStatus.NEW);
            return template.insert(user)
                    .flatMap(teamInvitation -> sendMailToInviteUserInTeam(user.getUserId(), userInviter, user.getTeamId())
                            .thenReturn(teamInvitation));
        });
    }

    public Mono<TeamRequest> sendTeamRequest(String teamId, UserDTO user) {
        return template.insert(TeamRequest.builder()
                .teamId(teamId)
                .userId(user.getId())
                .email(user.getEmail())
                .firstName(user.getFirstName())
                .lastName(user.getLastName())
                .createdAt(LocalDate.now())
                .status(RequestStatus.NEW)
                .build());
    }

    public Mono<TeamMemberDTO> addTeamMember(String teamId, String userId) {
        String query = "SELECT u.id as user_id, u.email, u.first_name, u.last_name, " +
                "s.id as skill_id, s.name as skill_name, s.type as skill_type " +
                "FROM users u " +
                "LEFT JOIN user_skill us ON u.id = us.user_id " +
                "LEFT JOIN skill s ON us.skill_id = s.id " +
                "WHERE u.id = :userId";

        return template.insert(new Team2Member(teamId, userId, Boolean.TRUE, LocalDate.now(), null))
                .then(template.selectOne(query(where("team_id").is(teamId)), Project.class)
                        .flatMap(project ->
                                template.insert(new ProjectMember(project.getId(), userId, teamId, ProjectRole.MEMBER, LocalDate.now(), null))
                        )
                )
                .then(template.getDatabaseClient()
                        .sql(query)
                        .bind("userId", userId)
                        .flatMap(t -> {
                            List<SkillDTO> skills = new ArrayList<>();
                            return t.map((row, rowMetadata) -> {
                                TeamMemberDTO teamMemberDTO = TeamMemberDTO.builder()
                                        .userId(userId)
                                        .email(row.get("email", String.class))
                                        .firstName(row.get("first_name", String.class))
                                        .lastName(row.get("last_name", String.class))
                                        .build();

                                String skillId = row.get("skill_id", String.class);
                                if (skillId != null) {
                                    skills.add(SkillDTO.builder()
                                            .id(skillId)
                                            .name(row.get("skill_name", String.class))
                                            .type(SkillType.valueOf(row.get("skill_type", String.class)))
                                            .build());
                                    teamMemberDTO.setSkills(skills);
                                }
                                return teamMemberDTO;
                            });
                        }).last());
    }

    public Flux<SkillDTO> getSkillsByUsers(List<UserDTO> users) {
        return getSkillsByList(users.stream().map(UserDTO::getId).toList());
    }

    public Flux<SkillDTO> getSkillsByInvitations(List<TeamInvitation> users) {
        return getSkillsByList(users.stream().map(TeamInvitation::getUserId).toList());
    }

    public Flux<SkillDTO> getSkillsByRequests(List<TeamRequest> users) {
        return getSkillsByList(users.stream().map(TeamRequest::getUserId).toList());
    }

    // TODO: работает неисправно при отправке двух айдишников (айдишники команды слипаются в ответе)
    public Flux<TeamWithMembersDTO> getTeamMembers(Flux<String> teamIds) {

        return teamIds.flatMap(
                teamId -> template.getDatabaseClient()
                        .sql("SELECT tm.member_id FROM team_member tm WHERE tm.team_id = :teamId")
                        .bind("teamId", teamId)
                        .map((row, rowMetaData) -> row.get("member_id", String.class))
                        .all().collectList()
                        .map(userIds -> new TeamWithMembersDTO(teamId, userIds))
                );
    }

    public Flux<UsersFullNamesDTO> getUsersFullNames(Flux<UsersFromAndToByResultIdRequest> requests) {

        return requests.flatMap(
                request -> template.selectOne(query(where("id").is(request.getToIdToUser())), User.class)
                        .flatMap(toUser -> template.selectOne(query(where("id").is(request.getFromIdUser())), User.class)
                                .flatMap(fromUser -> {

                                    UsersFullNamesDTO usersFullNamesDTO = UsersFullNamesDTO.builder()
                                            .resultId(request.getResultId())
                                            .fullNameToUser(toUser.getFirstName() + " " + toUser.getLastName())
                                            .fullNameFromUser(fromUser.getFirstName() + " " + fromUser.getLastName())
                                            .build();

                                    return Mono.just(usersFullNamesDTO);
                                })
                        )
                );
    }


    public Mono<Void> deleteTeam(String teamId, UserDTO user) {
        return checkOwner(teamId, user.getId())
                .flatMap(isExists -> {
                    if (Boolean.TRUE.equals(isExists) || user.getRoles().contains(Role.ADMIN)){
                        return template.update(query(where("id").is(teamId)),
                                update("is_deleted", Boolean.TRUE), Team.class)
                                .then(template.update(query(where("team_id").is(teamId)
                                                .and(where("is_active").is(Boolean.TRUE))),
                                        update("finish_date", LocalDate.now())
                                                .set("is_active", Boolean.FALSE), Team2Member.class));
                    }
                    return Mono.error(new AccessException("Нет Прав"));
                }).then();
    }

    public Mono<Void> kickFromTeam(String teamId, String userId) {
        LocalDate currentTime = LocalDate.now();
        return template.update(query(where("team_id").is(teamId)
                                .and("member_id").is(userId)),
                        update("is_active", Boolean.FALSE)
                                .set("finish_date", currentTime),
                        Team2Member.class)
                .then(template.insert(new Team2Refused(teamId, userId)))
                .then(template.exists(query(where("user_id").is(userId)
                        .and(where("team_id").is(teamId))), ProjectMember.class).flatMap(isExist -> {
                            if (isExist) {
                                return template.update(query(where("user_id").is(userId)
                                        .and(where("team_id").is(teamId))),
                                        update("finish_date", currentTime), ProjectMember.class)
                                        .then();
                            }
                            return Mono.empty();
                }));
    }

    public Mono<Void> leaveFromTeam(String teamId, String userId) {
        return template.update(query(where("team_id").is(teamId)
                                .and("member_id").is(userId)),
                        update("is_active", Boolean.FALSE)
                                .set("finish_date", LocalDate.now()),
                        Team2Member.class)
                .then();
    }


    public Mono<Void> setMarketForTeam(Flux<TeamDTO> teams, String marketId){
        return teams.flatMap(t ->
                template.update(query(where("id").is(t.getId())),
                        update("market_id", marketId),
                        Team.class)
        ).then();
    }

    public Mono<TeamDTO> updateTeam(String teamId, TeamDTO teamDTO, UserDTO user) {
        return checkOwnerOrLeader(teamId, user.getId())
                .flatMap(isExists -> {
                    if (Boolean.TRUE.equals(isExists) || user.getRoles().contains(Role.ADMIN)) {
                        return template.update(query(where("id").is(teamId)),
                                update("name", teamDTO.getName())
                                        .set("description", teamDTO.getDescription())
                                        .set("closed", teamDTO.getClosed()),
                                Team.class)
                                .thenReturn(teamDTO);
                    }
                    return Mono.error(new AccessException("Нет Прав"));
                });
    }

    public Mono<Void> updateTeamSkills(String teamId, Flux<SkillDTO> wantedSkills, UserDTO user) {
        return checkOwnerOrLeader(teamId, user.getId())
                .flatMap(isExists -> {
                    if (Boolean.TRUE.equals(isExists) || user.getRoles().contains(Role.ADMIN)) {
                        return template.delete(query(where("team_id").is(teamId)), Team2WantedSkill.class)
                                .then(wantedSkills.flatMap(s -> template.insert(new Team2WantedSkill(teamId, s.getId()))).then());
                    }
                    return Mono.error(new AccessException("Нет Прав"));
                });
    }

    public Mono<Void> changeTeamLeader(String teamId, String userId, UserDTO user) {
        return checkOwner(teamId, user.getId())
                .flatMap(isExists -> {
                    if (Boolean.TRUE.equals(isExists) || user.getRoles().contains(Role.ADMIN)) {
                        return template.selectOne(query(where("id").is(teamId)), Team.class)
                                .flatMap(t -> {
                                    if (Objects.equals(userId, t.getLeaderId())){
                                        return Mono.empty();
                                    }
                                    return template.update(query(where("id").is(teamId)),
                                                    update("leader_id", userId),
                                                    Team.class)
                                            .then(template.exists(query(where("team_id").is(t.getId())), Project.class)
                                                    .flatMap(thisExists -> {
                                                        if (Boolean.TRUE.equals(thisExists)){
                                                            return template.update(query(where("user_id").is(t.getLeaderId())),
                                                                            update("project_role", ProjectRole.MEMBER),
                                                                            ProjectMember.class)
                                                                    .then(template.update(query(where("user_id").is(userId)),
                                                                            update("project_role", ProjectRole.TEAM_LEADER),
                                                                            ProjectMember.class));
                                                        }
                                                        return Mono.empty();
                                                    })
                                            );
                                });
                    }
                    return Mono.error(new AccessException("Нет Прав"));
                }).then();
    }

    public Mono<TeamInvitation> updateTeamInvitationStatus(String invitationId, RequestStatus newStatus, UserDTO user) {
        return template.selectOne(query(where("id").is(invitationId)), TeamInvitation.class)
                .flatMap(invitation -> {
                    invitation.setStatus(newStatus);
                    if (newStatus.equals(RequestStatus.ACCEPTED)) {
                        return checkInitiator(invitationId, user.getId())
                                .flatMap(isExists -> {
                                    if (Boolean.TRUE.equals(isExists) || user.getRoles().contains(Role.ADMIN)){
                                        return annul(invitation.getUserId())
                                                .then(template.update(invitation))
                                                .thenReturn(invitation);
                                    }
                                    return Mono.error(new AccessException("Нет Прав"));
                                });
                    }
                    else if (newStatus.equals(RequestStatus.CANCELED)){
                        return checkInitiator(invitationId, user.getId())
                                .flatMap(isExists -> {
                                    if (Boolean.TRUE.equals(isExists) || user.getRoles().contains(Role.ADMIN)){
                                        return template.update(invitation).thenReturn(invitation);
                                    }
                                    return Mono.error(new AccessException("Нет Прав"));
                                });
                    }
                    else if (newStatus.equals(RequestStatus.WITHDRAWN)) {
                        return checkOwnerOrLeader(invitation.getTeamId(), user.getId())
                                .flatMap(isExists -> {
                                    if (Boolean.TRUE.equals(isExists) || user.getRoles().contains(Role.ADMIN)){
                                        return template.update(invitation).thenReturn(invitation);
                                    }
                                    return Mono.error(new AccessException("Нет Прав"));
                                });
                    }
                    else if (user.getRoles().contains(Role.ADMIN) &&
                            (newStatus.equals(RequestStatus.NEW) || newStatus.equals(RequestStatus.ANNULLED))) {
                        return template.update(invitation).thenReturn(invitation);
                    }
                    return Mono.error(new AccessException("Нет Прав"));
                });
    }

    public Mono<TeamRequest> updateTeamRequestStatus(String requestId, RequestStatus newStatus, UserDTO user) {
        return template.selectOne(query(where("id").is(requestId)), TeamRequest.class)
                .flatMap(request -> {
                    request.setStatus(newStatus);
                    if (newStatus.equals(RequestStatus.CANCELED)){
                        return checkOwnerOrLeader(request.getTeamId(), user.getId())
                                .flatMap(isExists -> {
                                    if (Boolean.TRUE.equals(isExists) || user.getRoles().contains(Role.ADMIN)){
                                        return template.insert(new Team2Refused(request.getTeamId(), request.getUserId()))
                                                .then(template.update(request))
                                                .thenReturn(request);
                                    }
                                    return Mono.error(new AccessException("Нет Прав"));
                                });
                    }
                    else if (newStatus.equals(RequestStatus.ACCEPTED)){
                        return checkOwnerOrLeader(request.getTeamId(), user.getId())
                                .flatMap(isExists -> {
                                    if (Boolean.TRUE.equals(isExists) || user.getRoles().contains(Role.ADMIN)){
                                        return annul(request.getUserId())
                                                .then(template.update(request))
                                                .thenReturn(request);
                                    }
                                    return Mono.error(new AccessException("Нет Прав"));
                                });
                    }
                    else if (newStatus.equals(RequestStatus.WITHDRAWN)) {
                        return template.exists(query(where("user_id").is(user.getId())
                                        .and("id").is(requestId)), TeamRequest.class)
                                .flatMap(isExists -> {
                                    if (Boolean.TRUE.equals(isExists) || user.getRoles().contains(Role.ADMIN)){
                                        return template.update(request).thenReturn(request);
                                    }
                                    return Mono.error(new AccessException("Нет Прав"));
                                });
                    }
                    else if (user.getRoles().contains(Role.ADMIN) &&
                            (newStatus.equals(RequestStatus.NEW) || newStatus.equals(RequestStatus.ANNULLED))){
                        return template.update(request).thenReturn(request);
                    }
                    return Mono.error(new AccessException("Нет Прав"));
                });
    }
}
