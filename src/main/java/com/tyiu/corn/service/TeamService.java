package com.tyiu.corn.service;

import com.tyiu.corn.model.dto.TeamDTO;
import com.tyiu.corn.model.entities.Team;
import com.tyiu.corn.model.entities.TeamInvitation;
import com.tyiu.corn.model.entities.TeamRequest;
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
import static org.springframework.data.relational.core.query.Update.update;


@Service
@RequiredArgsConstructor
public class TeamService {
    private final R2dbcEntityTemplate template;

    private Mono<Void> updateSkills(String teamId){
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
                            skills.forEach(s -> new Team2Skill(teamId, s));
                            return Mono.empty();
                        }));
    }

    public Mono<TeamDTO> getTeam(String teamId) {
        String QUERY = "SELECT " +
                "t.id as team_id, t.name as team_name, t.description as team_description, t.closed as team_closed, t.created_at as team_created_at, " +
                "o.id as owner_id, o.email as owner_email, o.first_name as owner_first_name, o.last_name as owner_last_name, " +
                "l.id as leader_id, l.email as leader_email, l.first_name as leader_first_name, l.last_name as leader_last_name, " +
                "m.id as member_id, m.email as member_email, m.first_name as member_first_name, m.last_name as member_last_name, " +
                "s.id as skill_id, s.name as skill_name, " +
                "ts.skill_id as team_skill_id " +
                "FROM team t " +
                "LEFT JOIN users o ON t.owner_id = o.id " +
                "LEFT JOIN users l ON t.leader_id = l.id " +
                "LEFT JOIN team_member tm ON t.id = tm.team_id " +
                "LEFT JOIN users m ON tm.member_id = m.id " +
                "LEFT JOIN team_skill ts ON tm.member_id = ts.team_id " +
                "LEFT JOIN skill s ON ts.skill_id = s.id " +
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

    public Flux<TeamInvitation> getInvitations(String userId) {
        return template.select(query(where("receiver_id").is(userId)), TeamInvitation.class);
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
                .leaderId(teamDTO.getLeader() != null ? teamDTO.getLeader().getId() : null)
                .createdAt(LocalDate.now())
                .build();

        

        return template.insert(team)
                .flatMap(t -> {
                    teamDTO.setId(t.getId());
                    teamDTO.setMembersCount(teamDTO.getMembers().size());
                    teamDTO.setCreatedAt(team.getCreatedAt());

                    teamDTO.getMembers().forEach(m ->
                            template.insert(new Team2Member(t.getId(), m.getId())).subscribe());

                    if (teamDTO.getSkills() != null){
                        teamDTO.getSkills().forEach(s ->
                            template.insert(new Team2Skill(t.getId(), s.getId())).subscribe());
                    }

                    return Mono.just(teamDTO);
                });
    }

    public Mono<TeamInvitation> sendInviteToUser(String teamId, String userId) {
        return template.selectOne(query(where("id").is(teamId)), Team.class)
                .flatMap(t -> template.insert(TeamInvitation.builder()
                                .receiverId(userId)
                                .teamId(teamId)
                                .teamName(t.getName())
                                .createdAt(LocalDate.now())
                                .build()));
    }

    public Mono<Void> inviteInTeam(String teamId, String userId){
        return template.insert(new Team2Member(teamId, userId))
                .then(template.delete(query(where("teamI_id").is(teamId)
                        .and("receiver_id").is(userId)),TeamInvitation.class))
                .then(template.delete(query(where("teamI_id").is(teamId)
                        .and("user_id").is(userId)), TeamRequest.class))
                .then(updateSkills(teamId))
                .then();
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

    public Mono<Void> updateTeam(String id, TeamDTO teamDTO) {
        return template.delete(query(where("team_id").is(id)), Team2Member.class).thenReturn(teamDTO.getMembers()).map(list -> {
            if (list != null) {
                list.forEach(member -> template.insert(new Team2Member(id, member.getId())).subscribe());
            }
            return list;
        }).then(template.update(query(where("id").is(id)),
                update("name", teamDTO.getName())
                        .set("description", teamDTO.getDescription())
                        .set("closed", teamDTO.getClosed())
                        .set("owner_id", teamDTO.getOwner().getId())
                        .set("leader_id", teamDTO.getLeader() != null ? teamDTO.getLeader().getId() : null),
                Team.class).then());
    }

}
