package com.tyiu.corn.service;

import com.tyiu.corn.config.exception.NotFoundException;
import com.tyiu.corn.model.dto.TeamDTO;
import com.tyiu.corn.model.entities.*;
import com.tyiu.corn.model.entities.mappers.TeamMapper;
import com.tyiu.corn.model.entities.relations.Team2Member;
import com.tyiu.corn.model.entities.relations.Team2Skill;
import lombok.RequiredArgsConstructor;
import org.springframework.data.r2dbc.core.R2dbcEntityTemplate;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.time.LocalDate;

import static org.springframework.data.relational.core.query.Criteria.where;
import static org.springframework.data.relational.core.query.Query.query;


@Service
@RequiredArgsConstructor
public class TeamService {
    private final R2dbcEntityTemplate template;

    public Mono<TeamDTO> getTeam(Long teamId) {
        TeamMapper teamMapper = new TeamMapper();
        String QUERY = "SELECT team.*, " +
                "o.id o_id, o.email o_email, o.first_name o_first_name, o.last_name o_last_name, " +
                "l.id l_id, l.email l_email, l.first_name l_first_name, l.last_name l_last_name, " +
                "m.id m_id, m.email m_email, m.first_name m_first_name, m.last_name m_last_name, " +
                "s.id s_id, s.name s_name, s.type " +
                "FROM team " +
                "LEFT JOIN team_member ON team.id = team_member.team_id " +
                "LEFT JOIN team_skill ON team.id = team_skill.team_id " +
                "LEFT JOIN users o ON team.owner_id = o.id " +
                "LEFT JOIN users l ON team.leader_id = l.id " +
                "LEFT JOIN users m ON team_member.member_id = m.id " +
                "LEFT JOIN skill s ON team_skill.skill_id = s.id " +
                "WHERE team.id = :teamId";
        return template.getDatabaseClient()
                .sql(QUERY)
                .bind("teamId", teamId)
                .map(teamMapper::apply)
                .all()
                .collectList()
                .map(groupDTOMap -> groupDTOMap.get(0));
                //.switchIfEmpty(Mono.error(new ErrorException("Failed to get a group")));
    }

    public Flux<TeamDTO> getTeams() {
        return template.select(Team.class).all()
                .flatMap(t -> template.select(query(where("team_id").is(t.getId())), Team2Member.class)
                        .collectList()
                        .flatMap(list -> Mono.just(TeamDTO.builder()
                                                        .id(t.getId())
                                                        .name(t.getName())
                                                        .description(t.getDescription())
                                                        .closed(t.getClosed())
                                                        .createdAt(t.getCreatedAt())
                                                        .membersCount(list.size())
                                                        .build())));
    }

    public Flux<TeamInvitation> getInvitations(String email) {
        return template.selectOne(query(where("email").is(email)), User.class)
                .flatMapMany(u -> template.select(query(where("receiver_id").is(u.getId())), TeamInvitation.class));
    }

    public Mono<TeamDTO> addTeam(TeamDTO teamDTO) {
        Team team = Team.builder()
                .name(teamDTO.getName())
                .description(teamDTO.getDescription())
                .closed(teamDTO.getClosed())
                .ownerId(teamDTO.getOwner().getUserId())
                .leaderId(teamDTO.getLeader().getUserId())
                .createdAt(LocalDate.now())
                .build();
        return template.insert(team)
                .flatMap(t -> {
                    teamDTO.setId(t.getId());
                    teamDTO.setMembersCount(teamDTO.getMembers().size());
                    teamDTO.setCreatedAt(team.getCreatedAt());

                    teamDTO.getMembers().forEach(m ->
                            template.insert(new Team2Member(t.getId(),m.getUserId())).subscribe());

                    teamDTO.getSkills().forEach(s ->
                            template.insert(new Team2Skill(t.getId(), s.getId())).subscribe());

                    return Mono.just(teamDTO);
                });
                //.onErrorResume(ex -> Mono.error(new ErrorException("Failed to add team")));
    }

    public Mono<TeamInvitation> sendInviteToUser(Long userId, Long teamId) {
        return template.selectOne(query(where("id").is(teamId)), Team.class)
                .flatMap(t ->
                        template.insert(TeamInvitation.builder()
                                .receiverId(userId)
                                .teamId(teamId)
                                .teamName(t.getName())
                                .createdAt(LocalDate.now())
                                .build()));
                //.onErrorResume(ex -> Mono.error(new ErrorException("Failed to send invitation")));
    }

    //
    //
    //
    //
    //
    //
    //

    public Mono<Void> deleteTeam(Long id) {
        return template.delete(query(where("id").is(id)), Team.class)
                .then(template.delete(query(where("team_id").is(id)), Team2Member.class))
                .then(template.delete(query(where("team_id").is(id)), Team2Skill.class))
                .then(template.delete(query(where("team_id").is(id)), TeamInvitation.class))
                .then()
                .onErrorResume(ex -> Mono.error(new NotFoundException("Not success!")));
    }

    public Mono<Void> deleteInvite(Long id) {
        return template.delete(query(where("id").is(id)), TeamInvitation.class).then()
                .onErrorResume(ex -> Mono.error(new NotFoundException("Not success!")));
    }

//    public Mono<Void> inviteInTeam(Long teamId, String email) {
//        return template.select(Team.class)
//                .matching(where("id").is(teamId))
//                .one()
//                .flatMap(team -> {
//                    team.getMembers().add(email);
//                    team.setMembersCount(team.getMembers().size());
//                    return template.update(team)
//                            .then(template.delete()
//                                    .from(TeamInvitation.class)
//                                    .matching(where("teamId").is(teamId)
//                                            .and("receiverEmail").is(email))
//                                    .all())
//                            .then();
//                })
//                .onErrorResume(ex -> Mono.error(new ErrorException("Failed to invite")));
//    }
//    public Mono<Void> updateTeam(Long id, TeamDTO teamDTO) {
//        return template.selectOne(query(where("id").is(id)), Team.class)
//                .flatMap(team -> {
//                    if (teamDTO.getMembers() != null) {
//                        team.setMembers(teamDTO.getMembers().stream()
//                                .map(UserDTO::getEmail)
//                                .collect(toList()));
//                    }
//                    if (teamDTO.getSkills() != null) {
//                        team.setSkills(teamDTO.getSkills().stream()
//                                .map(SkillDTO::getId)
//                                .collect(toList()));
//                    }
//
//                    team.setName(teamDTO.getName());
//                    team.setDescription(teamDTO.getDescription());
//                    team.setClosed(teamDTO.getClosed());
//                    team.setMembersCount(teamDTO.getMembers().size());
//                    team.setOwnerEmail(teamDTO.getOwner().getEmail());
//                    team.setLeaderEmail(teamDTO.getLeader().getEmail());
//
//                    return template.update(team)
//                            .then();
//                })
//                .onErrorResume(ex -> Mono.error(new ErrorException("Not success!")));
//    }
//    public Mono<Void> applyToOpenTeam(Long teamId, User user) {
//        String updateSql = "UPDATE team " +
//                "SET applications = array_append(applications, :userId) " +
//                "WHERE id = :teamId AND status = 'OPEN' " +
//                "AND :userId NOT IN members " +
//                "AND :userId NOT IN invitations";
//
//        return template.getDatabaseClient()
//                .sql(updateSql)
//                .bind("teamId", teamId)
//                .bind("userId", user.getId())
//                .fetch()
//                .rowsUpdated()
//                .flatMap(updatedRows -> {
//                    if (updatedRows > 0) {
//                        return Mono.empty();
//                    }
//                    return Mono.error(new ErrorException("Not success!"));
//                });
//    }

    public Mono<Void> acceptApplication(Long teamId, Long userId) {
        String updateSql = "UPDATE team " +
                "SET applications = array_remove(applications, :userId), " +
                "members = array_append(members, :userId) " +
                "WHERE id = :teamId AND :userId = ANY(applications)";

        return template.getDatabaseClient()
                .sql(updateSql)
                .bind("teamId", teamId)
                .bind("userId", userId)
                .fetch()
                .rowsUpdated()
                .flatMap(updatedRows -> {
                    if (updatedRows > 0) {
                        return Mono.empty();
                    }
                    return Mono.error(new NotFoundException("Not success!"));
                });
    }

    public Mono<Void> rejectApplication(Long teamId, Long userId) {
        String updateSql = "UPDATE team " +
                "SET applications = array_remove(applications, :userId) " +
                "WHERE id = :teamId AND :userId = ANY(applications)";

        return template.getDatabaseClient()
                .sql(updateSql)
                .bind("teamId", teamId)
                .bind("userId", userId)
                .fetch()
                .rowsUpdated()
                .flatMap(updatedRows -> {
                    if (updatedRows > 0) {
                        return Mono.empty();
                    }
                    return Mono.error(new NotFoundException("Not success!"));
                });
    }

    public Mono<Void> sendInvitationToClosedTeam(Long teamId, Long userId) {
        String updateSql = "UPDATE team " +
                "SET invitations = array_append(invitations, :userId) " +
                "WHERE id = :teamId AND status = 'CLOSED'";

        return template.getDatabaseClient()
                .sql(updateSql)
                .bind("teamId", teamId)
                .bind("userId", userId)
                .fetch()
                .rowsUpdated()
                .flatMap(updatedRows -> {
                    if (updatedRows > 0) {
                        return Mono.empty();
                    }
                    return Mono.error(new NotFoundException("Not success!"));
                });
    }

    public Mono<Void> acceptInvitation(Long teamId, Long userId) {
        String updateSql = "UPDATE team " +
                "SET invitations = array_remove(invitations, :userId), " +
                "members = array_append(members, :userId) " +
                "WHERE id = :teamId AND :userId = ANY(invitations)";

        return template.getDatabaseClient()
                .sql(updateSql)
                .bind("teamId", teamId)
                .bind("userId", userId)
                .fetch()
                .rowsUpdated()
                .flatMap(updatedRows -> {
                    if (updatedRows > 0) {
                        return Mono.empty();
                    }
                    return Mono.error(new NotFoundException("Not success!"));
                });
    }

    public Mono<Void> declineInvitation(Long teamId, Long userId) {
        String updateSql = "UPDATE team " +
                "SET invitations = array_remove(invitations, :userId) " +
                "WHERE id = :teamId AND :userId = ANY(invitations)";

        return template.getDatabaseClient()
                .sql(updateSql)
                .bind("teamId", teamId)
                .bind("userId", userId)
                .fetch()
                .rowsUpdated()
                .flatMap(updatedRows -> {
                    if (updatedRows > 0) {
                        return Mono.empty();
                    }
                    return Mono.error(new NotFoundException("Not success!"));
                });
    }
}
