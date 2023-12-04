package com.tyiu.corn.service;

import com.tyiu.corn.model.dto.*;
import com.tyiu.corn.model.email.requests.InvitationEmailRequest;
import com.tyiu.corn.model.entities.Team;
import com.tyiu.corn.model.entities.TeamInvitation;
import com.tyiu.corn.model.entities.TeamRequest;
import com.tyiu.corn.model.entities.User;
import com.tyiu.corn.model.entities.mappers.TeamMapper;
import com.tyiu.corn.model.entities.relations.Team2Member;
import com.tyiu.corn.model.entities.relations.Team2Skill;
import com.tyiu.corn.model.enums.RequestStatus;
import com.tyiu.corn.model.enums.SkillType;
import io.r2dbc.spi.Batch;
import lombok.RequiredArgsConstructor;
import org.modelmapper.ModelMapper;
import org.springframework.data.r2dbc.core.R2dbcEntityTemplate;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import static org.springframework.data.relational.core.query.Criteria.where;
import static org.springframework.data.relational.core.query.Query.query;

@Service
@RequiredArgsConstructor
public class TeamService {

    private final R2dbcEntityTemplate template;
    private final ModelMapper mapper;
    private final EmailService emailService;
    private final String path = "https://hits.tyuiu.ru/";

    public Flux<TeamRequest> updateTeamRequestStatus(String requestId, RequestStatus newStatus) {
        return template.select(query(where("id").is(requestId)), TeamRequest.class)
                .flatMap(request -> {
                    request.setStatus(newStatus);
                    return template.update(request);
                });
    }

    public Mono<TeamRequest> sendTeamRequest(String teamId, User user) {
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

    private Mono<Void> sendMailToInviteUserInTeam(String userId, User userInviter, String teamName) {

        String message = String.format("Вас пригласили в команду \"%s\". Перейдите по ссылке, чтобы ответить на приглашение", teamName);

        return template.selectOne(query(where("id").is(userId)), User.class)
                .flatMap(u -> Mono.just(InvitationEmailRequest.builder()
                        .to(u.getEmail())
                        .from(userInviter.getFirstName() + " " + userInviter.getLastName())
                        .title("Приглашение в команду")
                        .message(message)
                        .link(path + "team/invitations")
                        .build()))
                .flatMap(emailService::sendMailInvitation);
    }

    /*public Flux<TeamDTO> getTeamsByVacancies(List<String> selectedSkills) {
        String query = "SELECT t.id as team_id, t.name as team_name, t.description as team_description, " +
                "t.closed as team_closed, t.created_at as team_created_at, " +
                "COUNT(ts.skill_id) as total_skills " +
                "FROM team t " +
                "LEFT JOIN team_skill ts ON t.id = ts.team_id " +
                "LEFT JOIN skill s ON ts.skill_id = s.id " +
                "WHERE s.name IN (:selectedSkills) " +
                "AND t.id NOT IN (" +
                "SELECT t.id " +
                "FROM team t " +
                "LEFT JOIN team_skill ts ON t.id = ts.team_id " +
                "LEFT JOIN skill s ON ts.skill_id = s.id " +
                "LEFT JOIN team_wanted_skill tws ON t.id = tws.team_id " +
                "WHERE s.name IN (:selectedSkills) " +
                "AND tws.wanted_skill NOT IN (:selectedSkills) " +
                "GROUP BY t.id " +
                "HAVING COUNT(DISTINCT tws.wanted_skill) > 0" +
                ") " +
                "GROUP BY t.id " +
                "HAVING COUNT(s.name) < COUNT(DISTINCT ts.skill_id)";

        return template.getDatabaseClient()
                .sql(query)
                .bind("selectedSkills", selectedSkills)
                .map(row -> {
                    TeamDTO teamDTO = new TeamDTO();
                    teamDTO.setId(row.get("team_id", String.class));
                    teamDTO.setName(row.get("team_name", String.class));
                    teamDTO.setDescription(row.get("team_description", String.class));
                    teamDTO.setClosed(row.get("team_closed", Boolean.class));
                    teamDTO.setCreatedAt(row.get("team_created_at", LocalDate.class));
                    return teamDTO;
                })
                .all();
    }*/

    /*public Flux<TeamDTO> getTeamsBySkills(List<String> selectedSkills, boolean isInitiator) {
        Query query;

        if (isInitiator) {
            query = query(where("skills").in(selectedSkills));
        } else {
            query = query(where("skills").in(selectedSkills)
                    .or("wantedSkills").in(selectedSkills));
        }
        return template.select(Team.class)
                .matching(query)
                .all()
                .flatMap(t -> template.select(query(where("team_id").is(t.getId())), Team2Member.class)
                        .collectList()
                        .flatMap(list -> Mono.just(TeamDTO.builder()
                                .id(t.getId())
                                .name(t.getName())
                                .description(t.getDescription())
                                .closed(t.getIsClosed())
                                .createdAt(t.getCreatedAt())
                                .membersCount(list.size())
                                .build())));
    }*/

    public Flux<TeamMemberDTO> getUsersInTeamWithSkills(String teamId) {
        String query = "SELECT u.id as user_id, u.email, u.first_name, u.last_name, " +
                "s.id as skill_id, s.name as skill_name " +
                "FROM users u " +
                "JOIN team_member tm ON u.id = tm.member_id " +
                "LEFT JOIN user_skill us ON u.id = us.user_id " +
                "LEFT JOIN skill s ON us.skill_id = s.id " +
                "WHERE tm.team_id = :teamId";

        return template.getDatabaseClient()
                .sql(query)
                .bind("teamId", teamId)
                .map((row, rowMetadata) -> {
                    TeamMemberDTO teamMemberDTO = new TeamMemberDTO();
                    teamMemberDTO.setUserId(row.get("user_id", String.class));
                    teamMemberDTO.setEmail(row.get("email", String.class));
                    teamMemberDTO.setFirstName(row.get("first_name", String.class));
                    teamMemberDTO.setLastName(row.get("last_name", String.class));

                    SkillDTO skillDTO = new SkillDTO();
                    skillDTO.setId(row.get("skill_id", String.class));
                    skillDTO.setName(row.get("skill_name", String.class));

                    if (skillDTO.getId() != null && skillDTO.getName() != null) {
                        if (teamMemberDTO.getSkills() == null) {
                            teamMemberDTO.setSkills(new ArrayList<>());
                        }
                        teamMemberDTO.getSkills().add(skillDTO);
                    }

                    return teamMemberDTO;
                })
                .all();
    }

    public Mono<TeamDTO> getTeamSkills(String teamId) {
        String queryTeamSkills = "SELECT skill_id FROM team_skill WHERE team_id = :teamId";
        String queryWantedSkills = "SELECT wanted_skill_id FROM team_wanted_skill WHERE team_id = :teamId";

        return template.getDatabaseClient()
                .sql(queryTeamSkills)
                .bind("teamId", teamId)
                .map(row -> row.get("skill_id", String.class))
                .all()
                .collectList()
                .flatMap(teamSkills -> {
                    TeamDTO teamDTO = new TeamDTO();
                    teamDTO.setId(teamId);
                    List<SkillDTO> skills = teamSkills.stream()
                            .map(skillId -> SkillDTO.builder().id(skillId).build())
                            .collect(Collectors.toList());
                    teamDTO.setSkills(skills);

                    return template.getDatabaseClient()
                            .sql(queryWantedSkills)
                            .bind("teamId", teamId)
                            .map(row -> row.get("wanted_skill_id", String.class))
                            .all()
                            .collectList()
                            .flatMap(wantedSkills -> {
                                List<SkillDTO> wantedSkillsList = wantedSkills.stream()
                                        .map(wantedSkillId -> SkillDTO.builder().id(wantedSkillId).build())
                                        .collect(Collectors.toList());
                                teamDTO.setWantedSkills(wantedSkillsList);
                                return Mono.just(teamDTO);
                            });
                });
    }
    public Mono<Void> updateTeamSkills(String teamId, List<SkillDTO> skills, List<SkillDTO> wantedSkills) {
        return template.delete(query(where("team_id").is(teamId)), Team2Skill.class)
                .thenMany(Flux.fromIterable(skills)
                        .map(skillDTO -> new Team2Skill(teamId, skillDTO.getId()))
                        .flatMap(template::insert)
                )
                .thenMany(Flux.fromIterable(wantedSkills)
                        .map(skillDTO -> new Team2Skill(teamId, skillDTO.getId()))
                        .flatMap(template::insert)
                )
                .then();
    }

    public Mono<Void> createTeamSkills(String teamId, List<SkillDTO> skillIds) {
        return Flux.fromIterable(skillIds)
                .flatMap(skillDTO -> template.insert(new Team2Skill(teamId, skillDTO.getId())))
                .then();
    }

    public Mono<Void> updateSkills(String teamId){
        String QUERY = "SELECT team_member.*, user_skill.* " +
                "FROM team_member " +
                "LEFT JOIN user_skill ON user_skill.user_id = team_member.member_id " +
                "WHERE team_member.team_id = :teamId";
        return template.delete(query(where("team_id").is(teamId)), Team2Skill.class)
                .then(template.getDatabaseClient()
                        .sql(QUERY)
                        .bind("teamId", teamId)
                        .map((row, rowMetadata) -> row.get("skill_id", String.class))
                        .all()
                        .distinct()
                        .collectList()
                        .flatMap(skills -> {
                            if (!skills.isEmpty()) {
                                skills.forEach(s -> template.insert(new Team2Skill(teamId, s)).subscribe());
                            }
                            return Mono.empty();
                        }));
    }

    public Mono<TeamDTO> getTeam(String teamId) {
        String QUERY = "SELECT " +
                "t.id as team_id, t.name as team_name, t.description as team_description, t.closed as team_closed, t.created_at as team_created_at, " +
                "o.id as owner_id, o.email as owner_email, o.first_name as owner_first_name, o.last_name as owner_last_name, " +
                "l.id as leader_id, l.email as leader_email, l.first_name as leader_first_name, l.last_name as leader_last_name, " +
                "m.id as member_id, m.email as member_email, m.first_name as member_first_name, m.last_name as member_last_name, " +
                "s.id as skill_id, s.name as skill_name, s.type as skill_type, " +
                "ws.id as wanted_skill_id, ws.name as wanted_skill_name, ws.type as wanted_skill_type " +
                "FROM team t " +
                "LEFT JOIN users o ON t.owner_id = o.id " +
                "LEFT JOIN users l ON t.leader_id = l.id " +
                "LEFT JOIN team_member tm ON t.id = tm.team_id " +
                "LEFT JOIN users m ON tm.member_id = m.id " +
                "LEFT JOIN team_skill ts ON ts.team_id = t.id " +
                "LEFT JOIN skill s ON ts.skill_id = s.id " +
                "LEFT JOIN team_wanted_skill tws ON tws.team_id = t.id " +
                "LEFT JOIN skill ws ON tws.skill_id = ws.id " +
                "WHERE t.id = :teamId";

        TeamMapper teamMapper = new TeamMapper();

        return template.getDatabaseClient()
                .sql(QUERY)
                .bind("teamId", teamId)
                .map(teamMapper::apply)
                .all()
                .collectList()
                .map(t -> t.get(0));
    }


    public Flux<TeamDTO> getTeams() {
        String QUERY = "SELECT " +
                "t.id as team_id, t.name as team_name, t.description as team_description, t.closed as team_closed, t.created_at as team_created_at, " +
                "o.id as owner_id, o.email as owner_email, o.first_name as owner_first_name, o.last_name as owner_last_name, " +
                "l.id as leader_id, l.email as leader_email, l.first_name as leader_first_name, l.last_name as leader_last_name, " +
                "(SELECT COUNT(*) FROM team_member WHERE team_id = t.id) as member_count " +
                "FROM team t " +
                "LEFT JOIN users o ON t.owner_id = o.id " +
                "LEFT JOIN users l ON t.leader_id = l.id";

        return template.getDatabaseClient()
                .sql(QUERY)
                .map((row, rowMetadata) -> {

                    TeamDTO teamDTO = TeamDTO.builder()
                            .id(row.get("team_id", String.class))
                            .name(row.get("team_name", String.class))
                            .description(row.get("team_description", String.class))
                            .closed(row.get("team_closed", Boolean.class))
                            .membersCount(row.get("member_count", Integer.class))
                            .createdAt(row.get("team_created_at", LocalDate.class))
                            .owner(UserDTO.builder()
                                    .id(row.get("owner_id", String.class))
                                    .email(row.get("owner_email", String.class))
                                    .firstName(row.get("owner_first_name", String.class))
                                    .lastName(row.get("owner_last_name", String.class))
                                    .build())
                            .build();

                    if (row.get("leader_id", String.class) != null) {
                        teamDTO.setLeader(UserDTO.builder()
                                .id(row.get("leader_id", String.class))
                                .email(row.get("leader_email", String.class))
                                .firstName(row.get("leader_first_name", String.class))
                                .lastName(row.get("leader_last_name", String.class))
                                .build());
                    }

                   return teamDTO;

                }).all();
    }

    public Flux<TeamInvitation> getInvitations(String userId) {
        return template.select(query(where("receiver_id").is(userId)), TeamInvitation.class);
    }
    public Flux<TeamRequest> getTeamRequests(String teamId) {
        return template.select(query(where("team_id").is(teamId)), TeamRequest.class);
    }

    public Flux<TeamInvitation> getInvitationByTeam(String teamId){
        return template.select(query(where("team_id").is(teamId)), TeamInvitation.class);
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
                            .then(updateSkills(t.getId()))
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

    /*public Mono<Void> sendInviteToUser(String teamId, List<UserDTO> users, User userInviter) {
        return template.selectOne(query(where("id").is(teamId)), Team.class)
                .flatMap(t -> Flux.fromIterable(users)
                        .flatMap(user -> template.insert(TeamInvitation.builder()
                                        .userId(user.getId())
                                        .teamId(teamId)
                                        .build())
                                .flatMap(teamInvitation -> sendMailToInviteUserInTeam(user.getId(), userInviter, t.getName()))
                        ).then()
                );
    }*/

    public Flux<TeamInvitation> sendInvitesToUsers(String teamId, List<UserDTO> users, User userInviter) {

        /*
        return template.selectOne(query(where("id").is(teamId)), Team.class)
                .flatMap(t -> Flux.fromIterable(users)
                        .flatMap(user -> template.insert(TeamInvitation.builder()
                                        .userId(user.getId())
                                        .teamId(teamId)
                                        .email(user.getEmail())
                                        .firstName(user.getFirstName())
                                        .lastName(user.getLastName())
                                        .status(RequestStatus.NEW)
                                        .build())
//                                .flatMap(teamInvitation -> sendMailToInviteUserInTeam(user.getId(), userInviter, t.getName()))
                        ).then()
                ); */
        return Flux.fromIterable(users)
                    .flatMap(user -> template.insert(
                            TeamInvitation.builder()
                                    .userId(user.getId())
                                    .teamId(teamId)
                                    .email(user.getEmail())
                                    .firstName(user.getFirstName())
                                    .lastName(user.getLastName())
                                    .status(RequestStatus.NEW)
                                    .build()));
    }


    public Mono<TeamMemberDTO> addTeamMember(String teamId, String userId){
        String query = "SELECT u.id as user_id, u.email, u.first_name, u.last_name, " +
                "s.id as skill_id, s.name as skill_name, s.type as skill_type " +
                "FROM users u " +
                "LEFT JOIN user_skill us ON u.id = us.user_id " +
                "LEFT JOIN skill s ON us.skill_id = s.id " +
                "WHERE u.id = :userId";


        return template.insert(new Team2Member(teamId, userId))
                .then(template.delete(query(where("team_id").is(teamId)
                        .and("receiver_id").is(userId)),TeamInvitation.class))
                .then(template.delete(query(where("team_id").is(teamId)
                        .and("user_id").is(userId)), TeamRequest.class))
                .then(updateSkills(teamId))
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
                                skills.add(SkillDTO.builder()
                                        .id(row.get("skill_id", String.class))
                                        .name(row.get("skill_name", String.class))
                                        .type(SkillType.valueOf(row.get("skill_type", String.class)))
                                        .build());
                                teamMemberDTO.setSkills(skills);
                                return teamMemberDTO;
                            });
                        }).last());
    }

    ///////////////////////////////////////////
    //   ___    ____   __    ____ ______   ____
    //  / _ \  / __/  / /   / __//_  __/  / __/
    // / // / / _/   / /__ / _/   / /    / _/
    ///____/ /___/  /____//___/  /_/    /___/
    ///////////////////////////////////////////

    public Mono<Void> deleteTeam(String id) {
        return template.delete(query(where("id").is(id)), Team.class).then();
    }

    public Mono<Void> deleteInvite(String id) {
        return template.delete(query(where("id").is(id)), TeamInvitation.class).then();
    }

    public Mono<Void> deleteRequest(String id){
        return template.delete(query(where("id").is(id)), TeamRequest.class).then();
    }

    public Mono<Void> kickFromTeam(String teamId, String userId){
        return template.delete(query(where("team_id").is(teamId)
                .and("member_id").is(userId)),Team2Member.class).then(updateSkills(teamId)).then();
    }

    ////////////////////////
    //   ___   __  __ ______
    //  / _ \ / / / //_  __/
    // / ___// /_/ /  / /
    ///_/    \____/  /_/
    ////////////////////////

    public Mono<TeamDTO> updateTeam(String id, TeamDTO teamDTO) {
        Team team = mapper.map(teamDTO, Team.class);
        team.setId(id);
        if (teamDTO.getOwner() != null) {
            team.setOwnerId(teamDTO.getOwner().getId());
        }
        if (teamDTO.getLeader() != null) {
            team.setLeaderId(teamDTO.getLeader().getId());
        }
        return template.update(team)
                .flatMap(t ->
                        template.delete(query(where("team_id").is(id)), Team2Member.class)
                                .thenReturn(teamDTO.getMembers()).mapNotNull(list -> {
                                    if (list != null) {
                                        list.forEach(member -> template.insert(new Team2Member(id, member.getId())).subscribe());
                                    }
                                    return list;
                                })).then(updateSkills(id)).thenReturn(teamDTO);
    }

}
