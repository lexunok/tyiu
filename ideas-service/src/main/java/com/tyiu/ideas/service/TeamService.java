package com.tyiu.ideas.service;

import com.tyiu.ideas.config.exception.AccessException;
import com.tyiu.ideas.model.dto.*;
import com.tyiu.ideas.model.entities.Team;
import com.tyiu.ideas.model.entities.TeamInvitation;
import com.tyiu.ideas.model.entities.TeamRequest;
import com.tyiu.ideas.model.entities.User;
import com.tyiu.ideas.model.entities.mappers.TeamMapper;
import com.tyiu.ideas.model.entities.relations.Team2Member;
import com.tyiu.ideas.model.entities.relations.Team2Refused;
import com.tyiu.ideas.model.entities.relations.Team2WantedSkill;
import enums.PortalLinks;
import com.tyiu.ideas.model.enums.RequestStatus;
import com.tyiu.ideas.model.enums.Role;
import com.tyiu.ideas.model.enums.SkillType;
import com.tyiu.ideas.publisher.NotificationPublisher;
import io.r2dbc.spi.Batch;
import io.r2dbc.spi.Row;
import lombok.RequiredArgsConstructor;
import org.modelmapper.ModelMapper;
import org.springframework.data.r2dbc.core.R2dbcEntityTemplate;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import request.NotificationRequest;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

import static org.springframework.data.relational.core.query.Criteria.where;
import static org.springframework.data.relational.core.query.Query.query;
import static org.springframework.data.relational.core.query.Update.update;

@Service
@RequiredArgsConstructor
public class TeamService {

    private final R2dbcEntityTemplate template;
    private final ModelMapper mapper;
    private final NotificationPublisher notificationPublisher;

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
                .map((row, rowMetadata) -> buildTeamDTO(row)).all().distinct();
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

    private Mono<Boolean> checkOwner(String teamId, String userId) {
        return template.exists(query(where("id").is(teamId)
                .and("owner_id").is(userId)), Team.class);
    }

    private Mono<Boolean> checkInitiator(String invitationId, String userId){
        return template.exists(query(where("id").is(invitationId)
                .and("user_id").is(userId)), TeamInvitation.class);
    }

    ///////////////////////
    //  _____   ____ ______
    // / ___/  / __//_  __/
    /// (_ /  / _/   / /
    //\___/  /___/  /_/
    ///////////////////////

    public Mono<TeamDTO> getTeam(String teamId, String userId) {
        String QUERY = "SELECT " +
                "t.id as team_id, t.name as team_name, t.description as team_description, t.closed as team_closed, t.created_at as team_created_at, t.has_active_project as team_has_active_project, " +
                "tr.team_id as refused_team_id, " +
                "o.id as owner_id, o.email as owner_email, o.first_name as owner_first_name, o.last_name as owner_last_name, " +
                "l.id as leader_id, l.email as leader_email, l.first_name as leader_first_name, l.last_name as leader_last_name, " +
                "m.id as member_id, m.email as member_email, m.first_name as member_first_name, m.last_name as member_last_name, " +
                "s.id as skill_id, s.name as skill_name, s.type as skill_type, " +
                "ws.id as wanted_skill_id, ws.name as wanted_skill_name, ws.type as wanted_skill_type," +
                "(SELECT COUNT(*) FROM team_member WHERE team_id = t.id) as member_count, " +
                "(SELECT EXISTS (SELECT 1 FROM team_member WHERE member_id = :userId)) as existed_member " +
                "FROM team t " +
                "LEFT JOIN team_refused tr ON tr.user_id = :userId AND tr.team_id = t.id " +
                "LEFT JOIN users o ON t.owner_id = o.id " +
                "LEFT JOIN users l ON t.leader_id = l.id " +
                "LEFT JOIN team_member tm ON t.id = tm.team_id " +
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
                "(SELECT COUNT(*) FROM team_member WHERE team_id = t.id) as member_count, " +
                "(SELECT EXISTS (SELECT 1 FROM team_member WHERE member_id = :userId)) as existed_member " +
                "FROM team t " +
                "LEFT JOIN team_refused tr ON tr.user_id = :userId AND tr.team_id = t.id " +
                "LEFT JOIN users o ON t.owner_id = o.id " +
                "LEFT JOIN users l ON t.leader_id = l.id";

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
                "(SELECT COUNT(*) FROM team_member WHERE team_id = t.id) as member_count " +
                "FROM team t " +
                "LEFT JOIN idea_market_refused imr ON imr.team_id = t.id AND imr.idea_market_id = :ideaMarketId  " +
                "LEFT JOIN users o ON t.owner_id = o.id " +
                "LEFT JOIN users l ON t.leader_id = l.id " +
                "WHERE t.owner_id = :userId";

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
                }).all();
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
                    map.put(userId, member);
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
        String QUERY = "SELECT tmr.id, tmr.idea_market_id, tmr.market_id, tmr.team_id, tmr.status, tmr.letter, " +
                "i.name AS name " +
                "FROM team_market_request tmr " +
                "LEFT JOIN idea_market im ON im.id = tmr.idea_market_id " +
                "LEFT JOIN idea i ON i.id = im.idea_id " +
                "WHERE tmr.team_id = :teamId";
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
                "LEFT JOIN users u ON u.id = tm.member_id";
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

    //////////////////////////////
    //   ___   ____    ____ ______
    //  / _ \ / __ \  / __//_  __/
    // / ___// /_/ / _\ \   / /
    ///_/    \____/ /___/  /_/
    //////////////////////////////

    public Mono<TeamDTO> addTeam(TeamDTO teamDTO) {
        Team team = Team.builder()
                .name(teamDTO.getName())
                .description(teamDTO.getDescription())
                .closed(teamDTO.getClosed())
                .hasActiveProject(false)
                .ownerId(teamDTO.getOwner().getId())
                .createdAt(LocalDate.now())
                .build();

        if (teamDTO.getLeader() != null && teamDTO.getLeader().getId() != null) {
            team.setLeaderId(teamDTO.getLeader().getId());
        }

        return template.insert(team)
                .flatMap(t -> {
                    teamDTO.setId(t.getId());
                    teamDTO.setMembersCount((teamDTO.getMembers() != null) ? teamDTO.getMembers().size() : 0);
                    teamDTO.setCreatedAt(team.getCreatedAt());

                    return template.getDatabaseClient().inConnection(connection -> {
                                Batch batch = connection.createBatch();
                                teamDTO.getMembers().forEach(u -> batch.add(
                                        String.format(
                                                "INSERT INTO team_member (team_id, member_id) VALUES ('%s', '%s');",
                                                t.getId(), u.getId()
                                        ))
                                );

                                return Mono.from(batch.execute());

                            })
                            .then(template.getDatabaseClient().inConnection(connection -> {
                                Batch batch = connection.createBatch();
                                teamDTO.getWantedSkills().forEach(s -> batch.add(
                                        String.format(
                                                "INSERT INTO team_wanted_skill (team_id, skill_id) VALUES ('%s', '%s');",
                                                t.getId(), s.getId()
                                        ))
                                );

                                return Mono.from(batch.execute());

                            })).thenReturn(teamDTO);
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
                    (SELECT COUNT(*) FROM team_member WHERE team_id = t.id) as member_count,
                    (SELECT EXISTS (SELECT 1 FROM team_member WHERE member_id = :userId)) as existed_member
                FROM team t
                    LEFT JOIN team_refused tr ON tr.user_id = :userId AND tr.team_id = t.id
                    LEFT JOIN users o ON t.owner_id = o.id
                    LEFT JOIN users l ON t.leader_id = l.id
                    LEFT JOIN team_member tm ON tm.team_id = t.id
                    LEFT JOIN user_skill us ON us.user_id = tm.member_id
                """;
        if (role == Role.INITIATOR)
        {
            QUERY = QUERY + "WHERE us.skill_id IN (:skills)";
        }
        else {
            QUERY = QUERY + "LEFT JOIN team_wanted_skill tws ON tws.team_id = t.id " +
                    "WHERE us.skill_id IN (:skills) OR tws.skill_id IN (:skills)";
        }

        return getFilteredTeam(QUERY, selectedSkills, userId);
    }

    public Flux<TeamDTO> getTeamsByVacancies(List<SkillDTO> selectedSkills, String userId) {
        String QUERY = "SELECT " +
                "t.id as team_id, t.name as team_name, t.description as team_description, t.closed as team_closed, t.created_at as team_created_at, t.has_active_project as team_has_active_project, " +
                "tr.team_id as refused_team_id, " +
                "o.id as owner_id, o.email as owner_email, o.first_name as owner_first_name, o.last_name as owner_last_name, " +
                "l.id as leader_id, l.email as leader_email, l.first_name as leader_first_name, l.last_name as leader_last_name, " +
                "(SELECT COUNT(*) FROM team_member WHERE team_id = t.id) as member_count, " +
                "(SELECT EXISTS (SELECT 1 FROM team_member WHERE member_id = :userId)) as existed_member " +
                "FROM team t " +
                "LEFT JOIN team_refused tr ON tr.user_id = :userId AND tr.team_id = t.id " +
                "LEFT JOIN users o ON t.owner_id = o.id " +
                "LEFT JOIN users l ON t.leader_id = l.id " +
                "LEFT JOIN team_member tm ON tm.team_id = t.id " +
                "LEFT JOIN user_skill us ON us.user_id = tm.member_id " +
                "LEFT JOIN team_wanted_skill tws ON tws.team_id = t.id " +
                "WHERE tws.skill_id IN (:skills) AND us.skill_id NOT IN (:skills)";

        return getFilteredTeam(QUERY, selectedSkills, userId);
    }

    public Flux<TeamInvitation> sendInvitesToUsers(Flux<TeamInvitation> invites, String userInviterId) {

        return invites.flatMap(invite -> {
            invite.setStatus(RequestStatus.NEW);
            return template.insert(invite)
                    .flatMap(sendInvites -> {

                        template.selectOne(query(where("id").is(invite.getTeamId())), Team.class)
                                .flatMap(team -> template.selectOne(query(where("id").is(userInviterId)), User.class)
                                        .flatMap(inviter -> template.selectOne(query(where("id").is(invite.getUserId())), User.class)
                                                .flatMap(invitedUser -> notificationPublisher.makeNotification(

                                                        NotificationRequest.builder()
                                                                .publisherEmail(inviter.getEmail())
                                                                .consumerEmail(invitedUser.getEmail())
                                                                .title("Вас пригласили в команду")
                                                                .message(
                                                                        String.format("%s %s пригласил вас в команду \"%s\". " +
                                                                                        "Перейдите по ссылке, чтобы ответить на приглашение",
                                                                                inviter.getFirstName(),
                                                                                inviter.getLastName(),
                                                                                team.getName()
                                                                        ))
                                                                .link(String.valueOf(PortalLinks.TEAM_INVITES))
                                                                .buttonName("Перейти к приглашениям")
                                                                .build())))
                                ).subscribe();

                        return Mono.just(invite);
                    });
        });
    }

    public Mono<TeamRequest> sendTeamRequest(String teamId, User user) {

        TeamRequest request = TeamRequest.builder()
                .teamId(teamId)
                .userId(user.getId())
                .email(user.getEmail())
                .firstName(user.getFirstName())
                .lastName(user.getLastName())
                .createdAt(LocalDate.now())
                .status(RequestStatus.NEW)
                .build();

        return template.insert(request)
                .flatMap(sendRequest -> {

                    template.selectOne(query(where("id").is(request.getTeamId())), Team.class)
                            .flatMap(team -> template.selectOne(query(where("id").is(user.getId())), User.class)
                                    .flatMap(requestSender -> template.selectOne(query(where("id").is(team.getOwnerId())), User.class)
                                            .flatMap(teamOwner -> notificationPublisher.makeNotification(

                                                    NotificationRequest.builder()
                                                            .publisherEmail(requestSender.getEmail())
                                                            .consumerEmail(teamOwner.getEmail())
                                                            .title("В вашу команду желает вступит пользователь")
                                                            .message(
                                                                    String.format("%s %s хочет вступить в вашу команду \"%s\". " +
                                                                                    "Перейдите по ссылке, чтобы ответить на заявку",
                                                                            requestSender.getFirstName(),
                                                                            requestSender.getLastName(),
                                                                            team.getName()
                                                                    ))
                                                            .link(PortalLinks.TEAM_REQUESTS + teamId)
                                                            .buttonName("Перейти к заявке")
                                                            .build())))
                            ).subscribe();

                    return Mono.just(request);
                });
    }

    public Mono<TeamMemberDTO> addTeamMember(String teamId, String userId) {

        String query = "SELECT u.id as user_id, u.email, u.first_name, u.last_name, " +
                "s.id as skill_id, s.name as skill_name, s.type as skill_type " +
                "FROM users u " +
                "LEFT JOIN user_skill us ON u.id = us.user_id " +
                "LEFT JOIN skill s ON us.skill_id = s.id " +
                "WHERE u.id = :userId";

        return template.insert(new Team2Member(teamId, userId))
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

                                template.selectOne(query(where("id").is(teamId)), Team.class)
                                        .flatMap(team -> template.selectOne(query(where("id").is(userId)), User.class)
                                                .flatMap(user -> template.selectOne(query(where("id").is(team.getOwnerId())), User.class)
                                                        .flatMap(teamOwner -> notificationPublisher.makeNotification(

                                                                NotificationRequest.builder()
                                                                        .publisherEmail(user.getEmail())
                                                                        .consumerEmail(teamOwner.getEmail())
                                                                        .title("Пользователь принял ваше приглашение в команду")
                                                                        .message(
                                                                                String.format("%s %s принял ваше приглашение в команду \"%s\". " +
                                                                                                "Перейдите по ссылке, чтобы ознакомиться подробнее",
                                                                                        user.getFirstName(),
                                                                                        user.getLastName(),
                                                                                        team.getName()
                                                                                ))
                                                                        .link(PortalLinks.TEAM + teamId)
                                                                        .buttonName("Перейти к команде")
                                                                        .build())))
                                        ).subscribe();

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

    ///////////////////////////////////////////
    //   ___    ____   __    ____ ______   ____
    //  / _ \  / __/  / /   / __//_  __/  / __/
    // / // / / _/   / /__ / _/   / /    / _/
    ///____/ /___/  /____//___/  /_/    /___/
    ///////////////////////////////////////////

    public Mono<Void> deleteTeam(String teamId, User userThatDeletesTeam) {

        return checkOwner(teamId, userThatDeletesTeam.getId())
                .flatMap(userThatKicksIsOwnerOfThisTeam -> {

                    if (Boolean.TRUE.equals(userThatKicksIsOwnerOfThisTeam)) {

                        template.getDatabaseClient()
                                .sql("SELECT member_id FROM team_member WHERE team_id = :teamId")
                                .bind("teamId", teamId)
                                .map((row, rowMetadata) -> row.get("member_id", String.class))
                                .all()
                                .flatMap(memberId -> template.selectOne(query(where("id").is(teamId)), Team.class)
                                        .flatMap(team -> template.selectOne(query(where("member_id").is(memberId)), User.class)
                                                .flatMap(teamMember -> notificationPublisher.makeNotification(

                                                            NotificationRequest.builder()
                                                                    .publisherEmail(userThatDeletesTeam.getEmail())
                                                                    .consumerEmail(teamMember.getEmail())
                                                                    .title("Команда, в который вы состояли, была удалена")
                                                                    .message(
                                                                            String.format("%s %s удалил команду \"%s\".",
                                                                                    userThatDeletesTeam.getFirstName(),
                                                                                    userThatDeletesTeam.getLastName(),
                                                                                    team.getName()
                                                                            ))
                                                                    .build())))
                                ).subscribe();

                        return template.delete(query(where("id").is(teamId)), Team.class);
                    }
                    else if (userThatDeletesTeam.getRoles().contains(Role.ADMIN)) {

                        template.selectOne(query(where("id").is(teamId)), Team.class)
                                .flatMap(team -> template.selectOne(query(where("id").is(userThatDeletesTeam)), User.class)
                                        .flatMap(teamOwner -> notificationPublisher.makeNotification(

                                                NotificationRequest.builder()
                                                        .publisherEmail(userThatDeletesTeam.getEmail())
                                                        .consumerEmail(teamOwner.getEmail())
                                                        .title("Ваша команда была удалена с портала")
                                                        .message(
                                                                String.format("Админ %s %s удалил вашу команду \"%s\".",
                                                                        userThatDeletesTeam.getFirstName(),
                                                                        userThatDeletesTeam.getLastName(),
                                                                        team.getName()
                                                                ))
                                                        .build()

                                        ).thenReturn(template.getDatabaseClient()
                                                        .sql("SELECT member_id FROM team_member WHERE team_id = :teamId")
                                                        .bind("teamId", teamId)
                                                        .map((row, rowMetadata) -> row.get("member_id", String.class))
                                                        .all()
                                                        .flatMap(memberId -> template.selectOne(query(where("member_id").is(memberId)), User.class)
                                                                        .flatMap(teamMember -> notificationPublisher.makeNotification(

                                                                                NotificationRequest.builder()
                                                                                        .publisherEmail(userThatDeletesTeam.getEmail())
                                                                                        .consumerEmail(teamMember.getEmail())
                                                                                        .title("Команда, в который вы состояли, была удалена")
                                                                                        .message(
                                                                                                String.format("Админ %s %s удалил команду \"%s\".",
                                                                                                        userThatDeletesTeam.getFirstName(),
                                                                                                        userThatDeletesTeam.getLastName(),
                                                                                                        team.getName()
                                                                                                ))
                                                                                        .build())))))
                                ).subscribe();

                        return template.delete(query(where("id").is(teamId)), Team.class);
                    }
                    else return Mono.error(new AccessException("Нет Прав"));
                }).then();
    }

    public Mono<Void> kickFromTeam(String teamId, String userToKickId, User userThatKicks) {

        return checkOwner(teamId, userThatKicks.getId())
                .flatMap(userThatKicksIsOwnerOfThisTeam -> {

                    if (Boolean.TRUE.equals(userThatKicksIsOwnerOfThisTeam)) {

                        template.selectOne(query(where("id").is(teamId)), Team.class)
                                .flatMap(team -> template.selectOne(query(where("id").is(userToKickId)), User.class)
                                        .flatMap(userToKick -> notificationPublisher.makeNotification(

                                                NotificationRequest.builder()
                                                        .publisherEmail(userThatKicks.getEmail())
                                                        .consumerEmail(userToKick.getEmail())
                                                        .title("Вы были исключены из команды")
                                                        .message(
                                                                String.format("%s %s исключил вас из команды \"%s\".",
                                                                        userThatKicks.getFirstName(),
                                                                        userThatKicks.getLastName(),
                                                                        team.getName()
                                                                ))
                                                        .build()))
                                ).subscribe();

                        return template.delete(query(where("team_id").is(teamId)
                                        .and("member_id").is(userToKickId)),Team2Member.class)
                                .then(template.insert(new Team2Refused(teamId, userToKickId)))
                                .then();
                    }
                    else if (userThatKicks.getRoles().contains(Role.ADMIN)) {

                        template.selectOne(query(where("id").is(teamId)), Team.class)
                                .flatMap(team -> template.selectOne(query(where("id").is(userThatKicks.getId())), User.class)
                                        .flatMap(userToKick -> notificationPublisher.makeNotification(

                                                        NotificationRequest.builder()
                                                                .publisherEmail(userThatKicks.getEmail())
                                                                .consumerEmail(userToKick.getEmail())
                                                                .title("Вы были исключены из команды")
                                                                .message(
                                                                        String.format("Админ %s %s исключил вас из команды \"%s\".",
                                                                                userThatKicks.getFirstName(),
                                                                                userThatKicks.getLastName(),
                                                                                team.getName()
                                                                        ))
                                                                .build()))
                                ).subscribe();

                        return template.delete(query(where("team_id").is(teamId)
                                        .and("member_id").is(userToKickId)),Team2Member.class)
                                .then(template.insert(new Team2Refused(teamId, userToKickId)))
                                .then();
                    }
                    else return Mono.error(new Exception("Нет прав!"));
                });
    }

    public Mono<Void> leaveFromTeam(String teamId, String userThatLeavesId) {

        template.selectOne(query(where("id").is(teamId)), Team.class)
                .flatMap(team -> template.selectOne(query(where("id").is(userThatLeavesId)), User.class)
                        .flatMap(userThatLeaves -> template.selectOne(query(where("id").is(team.getOwnerId())), User.class)
                                .flatMap(teamOwner -> notificationPublisher.makeNotification(

                                        NotificationRequest.builder()
                                                .publisherEmail(userThatLeaves.getEmail())
                                                .consumerEmail(teamOwner.getEmail())
                                                .title("Пользователь покинул вашу команду")
                                                .message(
                                                        String.format("%s %s покинул вашу команду \"%s\". " +
                                                                        "Перейдите по ссылке, чтобы ознакомиться подробнее",
                                                                userThatLeaves.getFirstName(),
                                                                userThatLeaves.getLastName(),
                                                                team.getName()
                                                        ))
                                                .link(PortalLinks.TEAM + teamId)
                                                .buttonName("Перейти к команде")
                                                .build())))
                ).subscribe();

        return template.delete(query(where("team_id").is(teamId)
                        .and("member_id").is(userThatLeavesId)),Team2Member.class)
                .then();
    }

    ////////////////////////
    //   ___   __  __ ______
    //  / _ \ / / / //_  __/
    // / ___// /_/ /  / /
    ///_/    \____/  /_/
    ////////////////////////

    public Mono<TeamDTO> updateTeam(String id, TeamDTO teamDTO, User updater) {

        Team team = mapper.map(teamDTO, Team.class);
        team.setId(id);
        if (teamDTO.getOwner() != null) {
            team.setOwnerId(teamDTO.getOwner().getId());
        }
        if (teamDTO.getLeader() != null) {
            team.setLeaderId(teamDTO.getLeader().getId());
        }
        return template.update(team)
                .flatMap(t -> template.select(query(where("team_id").is(id)), Team2Member.class)
                        .flatMap(team2Member -> template.selectOne(query(where("id").is(team2Member.getMemberId())), User.class))
                        .flatMap(user -> Mono.just(user.getId()))
                        .collectList()
                        .flatMap(oldMembers -> template.delete(query(where("team_id").is(id)), Team2Member.class)
                                .then(Mono.fromRunnable(() -> {
                                    List<String> newId = teamDTO.getMembers().stream().map(UserDTO::getId).toList();
                                    Flux.fromIterable(oldMembers.stream().distinct()
                                            .filter(newId::contains).collect(Collectors.toSet())
                                    ).flatMap(idToStay ->  template.insert(new Team2Member(id, idToStay))).subscribe();
                                    Flux.fromIterable(oldMembers.stream().distinct()
                                            .filter(userId -> !newId.contains(userId)).collect(Collectors.toSet())
                                    ).flatMap(idToDelete -> template.selectOne(query(where("id").is(idToDelete)), User.class))
                                            .flatMap(user -> notificationPublisher.makeNotification(
                                                    NotificationRequest.builder()
                                                            .consumerEmail(user.getEmail())
                                                            .title("Вас исключили из команды на портале HITS")
                                                            .message(String.format(
                                                                    "%s %s исключил вас из команды " +
                                                                            "\"%s\"",
                                                                    updater.getFirstName(),
                                                                    updater.getLastName(),
                                                                    teamDTO.getName()
                                                            ))
                                                            .publisherEmail(updater.getEmail())
                                                            .build()
                                            )).subscribe();
                                }))
                        )
                ).thenReturn(teamDTO);
    }

    public Mono<Void> updateTeamSkills(String teamId, Flux<SkillDTO> wantedSkills, User user) {
        return checkOwner(teamId, user.getId())
                .flatMap(isExists -> {
                    if (Boolean.TRUE.equals(isExists) || user.getRoles().contains(Role.ADMIN)) {
                        return template.delete(query(where("team_id").is(teamId)), Team2WantedSkill.class)
                                .then(wantedSkills.flatMap(s -> template.insert(new Team2WantedSkill(teamId, s.getId()))).then());
                    }
                    return Mono.error(new AccessException("Нет Прав"));
                });
    }

    public Mono<Void> changeTeamLeader(String teamId, String newLeaderIdId, User userThatChangesTeamLeader) {

        return checkOwner(teamId, userThatChangesTeamLeader.getId())
                .flatMap(userThatKicksIsOwnerOfThisTeam -> {

                    if (Boolean.TRUE.equals(userThatKicksIsOwnerOfThisTeam)) {

                        template.selectOne(query(where("id").is(teamId)), Team.class)
                                .flatMap(team -> template.selectOne(query(where("id").is(newLeaderIdId)), User.class)
                                        .flatMap(newLeader -> template.selectOne(query(where("leader_id").is(team.getLeaderId())), User.class)
                                        .flatMap(currentLeader -> notificationPublisher.makeNotification(

                                                        NotificationRequest.builder()
                                                                .publisherEmail(userThatChangesTeamLeader.getEmail())
                                                                .consumerEmail(currentLeader.getEmail())
                                                                .title("Вас сняли с должности лидера команды")
                                                                .message(
                                                                        String.format("%s %s передал лидерство команды \"%s\" к %s %s. " +
                                                                                        "Перейдите по ссылке, чтобы ознакомиться подробнее",
                                                                                userThatChangesTeamLeader.getFirstName(),
                                                                                userThatChangesTeamLeader.getLastName(),
                                                                                team.getName(),
                                                                                newLeader.getFirstName(),
                                                                                newLeader.getLastName()
                                                                        ))
                                                                .link(PortalLinks.TEAM + teamId)
                                                                .buttonName("Перейти к команде")
                                                                .build()

                                                ).then(notificationPublisher.makeNotification(

                                                        NotificationRequest.builder()
                                                                .publisherEmail(userThatChangesTeamLeader.getEmail())
                                                                .consumerEmail(newLeader.getEmail())
                                                                .title("Вас назначили на должность лидера команды")
                                                                .message(
                                                                        String.format("%s %s передал вам лидерство команды \"%s\". " +
                                                                                        "Перейдите по ссылке, чтобы ознакомиться подробнее",
                                                                                userThatChangesTeamLeader.getFirstName(),
                                                                                userThatChangesTeamLeader.getLastName(),
                                                                                team.getName()
                                                                        ))
                                                                .link(PortalLinks.TEAM + teamId)
                                                                .buttonName("Перейти к команде")
                                                                .build()))))
                                ).subscribe();

                        return template.update(query(where("id").is(teamId)),
                                update("leader_id", newLeaderIdId),
                                Team.class);
                    }
                    else if (userThatChangesTeamLeader.getRoles().contains(Role.ADMIN)) {

                        template.selectOne(query(where("id").is(teamId)), Team.class)
                                .flatMap(team -> template.selectOne(query(where("id").is(newLeaderIdId)), User.class)
                                        .flatMap(newLeader -> template.selectOne(query(where("leader_id").is(team.getLeaderId())), User.class)
                                                .flatMap(currentLeader -> notificationPublisher.makeNotification(

                                                        NotificationRequest.builder()
                                                                .publisherEmail(userThatChangesTeamLeader.getEmail())
                                                                .consumerEmail(currentLeader.getEmail())
                                                                .title("Вас сняли с должности лидера команды")
                                                                .message(
                                                                        String.format("Админ %s %s передал лидерство команды \"%s\" к %s %s. " +
                                                                                        "Перейдите по ссылке, чтобы ознакомиться подробнее",
                                                                                userThatChangesTeamLeader.getFirstName(),
                                                                                userThatChangesTeamLeader.getLastName(),
                                                                                team.getName(),
                                                                                newLeader.getFirstName(),
                                                                                newLeader.getLastName()
                                                                        ))
                                                                .link(PortalLinks.TEAM + teamId)
                                                                .buttonName("Перейти к команде")
                                                                .build()

                                                ).then(notificationPublisher.makeNotification(

                                                        NotificationRequest.builder()
                                                                .publisherEmail(userThatChangesTeamLeader.getEmail())
                                                                .consumerEmail(newLeader.getEmail())
                                                                .title("Вас назначили на должность лидера команды")
                                                                .message(
                                                                        String.format("Админ %s %s передал вам лидерство команды \"%s\". " +
                                                                                        "Перейдите по ссылке, чтобы ознакомиться подробнее",
                                                                                userThatChangesTeamLeader.getFirstName(),
                                                                                userThatChangesTeamLeader.getLastName(),
                                                                                team.getName()
                                                                        ))
                                                                .link(PortalLinks.TEAM + teamId)
                                                                .buttonName("Перейти к команде")
                                                                .build()))))
                                ).subscribe();

                        return template.update(query(where("id").is(teamId)),
                                update("leader_id", newLeaderIdId),
                                Team.class);
                    }
                    return Mono.error(new AccessException("Нет Прав"));
                }).then();
    }

    public Mono<TeamInvitation> updateTeamInvitationStatus(String invitationId, RequestStatus newStatus, User user) {

        return template.selectOne(query(where("id").is(invitationId)), TeamInvitation.class)
                .flatMap(invitation -> {

                    invitation.setStatus(newStatus);
                    if (newStatus.equals(RequestStatus.ACCEPTED)) {

                        return checkInitiator(invitationId, user.getId())
                                .flatMap(isInitiator -> {

                                    if (Boolean.TRUE.equals(isInitiator) || user.getRoles().contains(Role.ADMIN)) {

                                        template.selectOne(query(where("id").is(invitationId)), TeamInvitation.class)
                                                .flatMap(teamInvitation -> template.selectOne(query(where("id").is(teamInvitation.getTeamId())), Team.class)
                                                        .flatMap(team -> template.selectOne(query(where("owner_id").is(team.getOwnerId())), User.class)
                                                                .flatMap(teamOwner -> notificationPublisher.makeNotification(

                                                                        NotificationRequest.builder()
                                                                                .publisherEmail(user.getEmail())
                                                                                .consumerEmail(teamOwner.getEmail())
                                                                                .title("Пользователь принял ваше приглашение в команду")
                                                                                .message(
                                                                                        String.format("%s %s принял ваше приглашение в команду \"%s\". " +
                                                                                                        "Перейдите по ссылке, чтобы ознакомиться подробнее",
                                                                                                user.getFirstName(),
                                                                                                user.getLastName(),
                                                                                                team.getName()
                                                                                        ))
                                                                                .link(PortalLinks.TEAM + team.getId())
                                                                                .buttonName("Перейти к команде")
                                                                                .build())))
                                                ).subscribe();

                                        return annul(invitation.getUserId())
                                                .then(template.update(invitation))
                                                .thenReturn(invitation);
                                    }
                                    return Mono.error(new AccessException("Нет Прав"));
                                });
                    }
                    else if (newStatus.equals(RequestStatus.CANCELED)){
                        return checkInitiator(invitationId, user.getId())
                                .flatMap(isInitiator -> {

                                    if (Boolean.TRUE.equals(isInitiator) || user.getRoles().contains(Role.ADMIN)) {

                                        template.selectOne(query(where("id").is(invitationId)), TeamInvitation.class)
                                                .flatMap(teamInvitation -> template.selectOne(query(where("id").is(teamInvitation.getTeamId())), Team.class)
                                                        .flatMap(team -> template.selectOne(query(where("owner_id").is(team.getOwnerId())), User.class)
                                                                .flatMap(teamOwner -> notificationPublisher.makeNotification(

                                                                        NotificationRequest.builder()
                                                                                .publisherEmail(user.getEmail())
                                                                                .consumerEmail(teamOwner.getEmail())
                                                                                .title("Пользователь отклонил ваше приглашение в команду")
                                                                                .message(
                                                                                        String.format("%s %s отклонил ваше приглашение в команду \"%s\".",
                                                                                                user.getFirstName(),
                                                                                                user.getLastName(),
                                                                                                team.getName()
                                                                                        ))
                                                                                .build())))
                                                ).subscribe();

                                        return template.update(invitation).thenReturn(invitation);
                                    }
                                    return Mono.error(new AccessException("Нет Прав"));
                                });
                    }
                    else if (newStatus.equals(RequestStatus.WITHDRAWN)) {

                        return checkOwner(invitation.getTeamId(), user.getId())
                                .flatMap(isOwner -> {

                                    if (Boolean.TRUE.equals(isOwner) || user.getRoles().contains(Role.ADMIN)) {
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

    public Mono<TeamRequest> updateTeamRequestStatus(String requestId, RequestStatus newStatus, User user) {

        return template.selectOne(query(where("id").is(requestId)), TeamRequest.class)
                .flatMap(request -> {

                    request.setStatus(newStatus);
                    if (newStatus.equals(RequestStatus.CANCELED)){
                        return checkOwner(request.getTeamId(), user.getId())
                                .flatMap(isOwner -> {

                                    if (Boolean.TRUE.equals(isOwner) || user.getRoles().contains(Role.ADMIN)) {

                                        template.selectOne(query(where("id").is(requestId)), TeamRequest.class)
                                                .flatMap(teamRequest -> template.selectOne(query(where("id").is(teamRequest.getTeamId())), Team.class)
                                                        .flatMap(team -> template.selectOne(query(where("owner_id").is(team.getOwnerId())), User.class)
                                                                .flatMap(teamOwner -> notificationPublisher.makeNotification(

                                                                        NotificationRequest.builder()
                                                                                .publisherEmail(teamOwner.getEmail())
                                                                                .consumerEmail(teamRequest.getEmail())
                                                                                .title("Ваша заявка на вступление в команду была отклонена")
                                                                                .message(
                                                                                        String.format("%s %s отклонил вашу заявку на вступление " +
                                                                                                        "в команду \"%s\".",
                                                                                                teamOwner.getFirstName(),
                                                                                                teamOwner.getLastName(),
                                                                                                team.getName()
                                                                                        ))
                                                                                .build())))
                                                ).subscribe();

                                        return template.insert(new Team2Refused(request.getTeamId(), request.getUserId()))
                                                .then(template.update(request))
                                                .thenReturn(request);
                                    }
                                    return Mono.error(new AccessException("Нет Прав"));
                                });
                    }
                    else if (newStatus.equals(RequestStatus.ACCEPTED)) {

                        return checkOwner(request.getTeamId(), user.getId())
                                .flatMap(isOwner -> {

                                    if (Boolean.TRUE.equals(isOwner) || user.getRoles().contains(Role.ADMIN)) {

                                        template.selectOne(query(where("id").is(requestId)), TeamRequest.class)
                                                .flatMap(teamRequest -> template.selectOne(query(where("id").is(teamRequest.getTeamId())), Team.class)
                                                        .flatMap(team -> template.selectOne(query(where("owner_id").is(team.getOwnerId())), User.class)
                                                                .flatMap(teamOwner -> notificationPublisher.makeNotification(

                                                                        NotificationRequest.builder()
                                                                                .publisherEmail(teamOwner.getEmail())
                                                                                .consumerEmail(teamRequest.getEmail())
                                                                                .title("Ваша заявка на вступление в команду была одобрена")
                                                                                .message(
                                                                                        String.format("%s %s одобрил вашу заявку на вступление " +
                                                                                                        "в команду \"%s\". " +
                                                                                                        "Перейдите по ссылке, чтобы ознакомиться подробнее",
                                                                                                teamOwner.getFirstName(),
                                                                                                teamOwner.getLastName(),
                                                                                                team.getName()
                                                                                        ))
                                                                                .link(PortalLinks.TEAM + team.getId())
                                                                                .buttonName("Перейти к команде")
                                                                                .build())))
                                                ).subscribe();

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
