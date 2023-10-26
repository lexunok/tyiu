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
import static org.springframework.data.relational.core.query.Update.update;


@Service
@RequiredArgsConstructor
public class TeamService {
    private final R2dbcEntityTemplate template;

    ///////////////////////
    //  _____   ____ ______
    // / ___/  / __//_  __/
    /// (_ /  / _/   / /
    //\___/  /___/  /_/
    ///////////////////////

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

    public Flux<TeamInvitation> getInvitations(Long userId) {
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
    }

    public Mono<Void> inviteInTeam(Long teamId, Long userId){
        return template.insert(new Team2Member(teamId, userId))
                        .then(template.delete(query(where("teamI_id").is(teamId)
                                .and("receiver_id").is(userId)),TeamInvitation.class))
                        .then(template.delete(query(where("teamI_id").is(teamId)
                                .and("user_id").is(userId)), TeamRequest.class)).then();
    }

    ///////////////////////////////////////////
    //   ___    ____   __    ____ ______   ____
    //  / _ \  / __/  / /   / __//_  __/  / __/
    // / // / / _/   / /__ / _/   / /    / _/
    ///____/ /___/  /____//___/  /_/    /___/
    ///////////////////////////////////////////

    public Mono<Void> deleteTeam(Long id) {
        return template.delete(query(where("id").is(id)), Team.class).then();
    }

    public Mono<Void> deleteInvite(Long id) {
        return template.delete(query(where("id").is(id)), TeamInvitation.class).then();
    }

    public Mono<Void> deleteRequest(Long id){
        return template.delete(query(where("id").is(id)), TeamRequest.class).then();
    }

    public Mono<Void> kickFromTeam(Long teamId, Long userId){
        return template.delete(query(where("team_id").is(teamId)
                .and("member_id").is(userId)),Team2Member.class).then();
    }

    ////////////////////////
    //   ___   __  __ ______
    //  / _ \ / / / //_  __/
    // / ___// /_/ /  / /
    ///_/    \____/  /_/
    ////////////////////////

    public Mono<Void> updateTeam(Long id, TeamDTO teamDTO) {
        return template.update(query(where("id").is(id)),
                        update("name", teamDTO.getName())
                                .set("description", teamDTO.getDescription())
                                .set("closed", teamDTO.getClosed())
                                .set("membersCount", teamDTO.getMembers().size())
                                .set("ownerEmail", teamDTO.getOwner().getEmail())
                                .set("leaderEmail", teamDTO.getLeader().getEmail()),
                        Team.class).then();
    }
}
