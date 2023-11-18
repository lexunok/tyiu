package com.tyiu.corn.service;

import com.tyiu.corn.model.dto.SkillDTO;
import com.tyiu.corn.model.dto.TeamAccessionDTO;
import com.tyiu.corn.model.dto.TeamDTO;
import com.tyiu.corn.model.dto.TeamMemberDTO;
import com.tyiu.corn.model.entities.Team;
import com.tyiu.corn.model.entities.TeamAccession;
import com.tyiu.corn.model.entities.User;
import com.tyiu.corn.model.entities.mappers.TeamMapper;
import com.tyiu.corn.model.entities.relations.Team2Member;
import com.tyiu.corn.model.entities.relations.Team2Skill;
import com.tyiu.corn.model.enums.AccessionStage;
import com.tyiu.corn.model.enums.RequestType;
import lombok.RequiredArgsConstructor;
import org.springframework.data.r2dbc.core.R2dbcEntityTemplate;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.time.LocalDate;
import java.util.ArrayList;

import java.util.List;
import java.util.Objects;

import static org.springframework.data.relational.core.query.Criteria.where;
import static org.springframework.data.relational.core.query.Query.query;
import static org.springframework.data.relational.core.query.Update.update;


@Service
@RequiredArgsConstructor
public class TeamService {
    private final R2dbcEntityTemplate template;

    private Mono<Void> updateSkills(String teamId) {
        return template.select(Team2Member.class)
                .matching(query(where("team_id").is(teamId)))
                .all()
                .collectList()
                .flatMap(teamMembers -> {
                    List<SkillDTO> updatedSkills = new ArrayList<>();
                    for (Team2Member teamMember : teamMembers) {
                        List<SkillDTO> memberSkills = (List<SkillDTO>) template.select(TeamMemberDTO.class)
                                .matching(query(where("userId").is(teamMember.getMemberId())))
                                .one()
                                .map(TeamMemberDTO::getSkills);

                        for (SkillDTO skill : memberSkills) {
                            if (updatedSkills.stream().noneMatch(existingSkill -> Objects.equals(existingSkill.getId(), skill.getId()))) {
                                updatedSkills.add(skill);
                            }
                        }
                    }

                    return template.update(Team.class)
                            .matching(query(where("id").is(teamId)))
                            .apply(update("skills", updatedSkills))
                            .then();
                });
    }

    public Mono<TeamDTO> getTeam(String teamId) {
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
                .flatMap(t -> {
                    return template.select(query(where("team_id").is(t.getId())), Team2Member.class)
                            .collectList()
                            .map(memberList -> {
                                return template.select(User.class)
                                        .matching(query(where("id").is(userId)))
                                        .one()
                                        .map(user -> {
                                            return TeamDTO.builder()
                                                .id(t.getId())
                                                .name(t.getName())
                                                .description(t.getDescription())
                                                .closed(t.getClosed())
                                                .createdAt(t.getCreatedAt())
                                                .owner(TeamMemberDTO.builder()
                                                            .userId(user.getId())
                                                            .email(user.getEmail())
                                                            .firstName(user.getFirstName())
                                                            .lastName(user.getLastName())
                                                            .build();)
                                                .membersCount(memberList.size())
                                                .desiredSkills(t.getDesiredSkills())
                                                .build();
                                        });
                                
                                
                            });
                });
    }


    public Flux<TeamAccessionDTO> getAccessions(String teamId) {
        return template.select(TeamAccessionDTO.class)
                .matching(query(where("teamId").is(teamId)))
                .all();
    }
    public Mono<TeamAccessionDTO> getAccessionByTargetId(String targetId) {
        return template.select(TeamAccessionDTO.class)
                .matching(query(where("targetId").is(targetId)))
                .one();
    }
    public Mono<TeamMemberDTO> getTeamProfile(String userId) {
        return template.select(User.class)
                .matching(query(where("id").is(userId)))
                .one()
                .map(user -> {
                    return TeamMemberDTO.builder()
                            .userId(user.getId())
                            .email(user.getEmail())
                            .firstName(user.getFirstName())
                            .lastName(user.getLastName())
                            .build();
                });
    }
    public Flux<TeamMemberDTO> getTeamProfiles() {
        return template.select(User.class)
                .matching(query(where("registeredUser").is(true)))
                .all()
                .map(user -> TeamMemberDTO.builder()
                        .userId(user.getId())
                        .email(user.getEmail())
                        .firstName(user.getFirstName())
                        .lastName(user.getLastName())
                        .build());
    }
    public Mono<TeamDTO> addTeam(TeamDTO teamDTO) {
        Team team = Team.builder()
                .name(teamDTO.getName())
                .description(teamDTO.getDescription())
                .closed(teamDTO.getClosed())
                .ownerId(teamDTO.getOwner().getUserId())
                .leaderId(teamDTO.getLeader() != null ? teamDTO.getLeader().getUserId() : null)
                .createdAt(LocalDate.now())
                .build();

        return template.insert(team)
                .flatMap(t -> {
                    teamDTO.setId(t.getId());
                    teamDTO.setMembersCount(teamDTO.getMembers().size());
                    teamDTO.setCreatedAt(team.getCreatedAt());

                    teamDTO.getMembers().forEach(m ->
                            template.insert(new Team2Member(t.getId(), m.getUserId())).subscribe());

                    teamDTO.getSkills().forEach(s ->
                            template.insert(new Team2Skill(t.getId(), s.getId())).subscribe());

                    return Mono.just(teamDTO);
                });
    }

    public Mono<Void> responseToRequest(String teamId, String userId) {
        return template.select(TeamAccession.class)
                .matching(query(where("team_id").is(teamId).and("user_id").is(userId).and("accessionStage").is(AccessionStage.ACCEPTED)))
                .one()
                .flatMap(accession -> {
                    if (accession.getAccessionStage() == AccessionStage.ACCEPTED) {
                        accession.setAccessionStage(AccessionStage.ACCEPTED);
                        return template.update(accession)
                                .then(template.insert(new Team2Member(teamId, userId)))
                                .then(updateSkills(teamId))
                                .then();
                    } else {
                        accession.setAccessionStage(AccessionStage.REJECTED);
                        return template.update(accession).then();
                    }
                })
                .switchIfEmpty(Mono.error(new IllegalArgumentException("Заявка не найдена")));
    }

    

    public Mono<Void> deleteTeam(Long id) {
        return template.delete(query(where("id").is(id)), Team.class).then();
    }



    public Mono<Void> kickFromTeam(String teamId, String userId) {
        return template.delete(query(where("team_id").is(teamId).and("member_id").is(userId)), Team2Member.class)
                .then(updateSkills(teamId))
                .then();
    }

    public Mono<Void> updateTeam(String id, TeamDTO teamDTO) {
        return template.update(query(where("id").is(id)),
                update("name", teamDTO.getName())
                        .set("description", teamDTO.getDescription())
                        .set("closed", teamDTO.getClosed())
                        .set("membersCount", teamDTO.getMembers().size())
                        .set("ownerEmail", teamDTO.getOwner().getEmail())
                        .set("leaderEmail", teamDTO.getLeader() != null ? teamDTO.getLeader().getEmail() : null)
                        .set("teamMembers", teamDTO.getMembers())
                        .set("skills", teamDTO.getSkills()),
                Team.class).then();
    }

    public Mono<Void> inviteRegisteredUsers(List<User> users) {
        return Flux.fromIterable(users)
                .flatMap(user -> {
                    TeamAccession teamAccession = TeamAccession.builder()
                            .text("Invitation to the team")
                            .requestType(RequestType.ENTER)
                            .accessionStage(AccessionStage.REQUEST)
                            .targetRegistered(true)
                            .targetId(user.getId())
                            .build();
                    return template.insert(teamAccession).then();
                })
                .then();
    }

    public Mono<Void> inviteUnregisteredUser(String email, String teamId) {
        TeamAccession teamAccession = TeamAccession.builder()
                .targetEmail(email)
                .teamId(teamId)
                .accessionStage(AccessionStage.INVITATION)
                .requestType(RequestType.ENTER)
                .targetRegistered(false)
                .text("Invitation to the team")
                .updateAt(LocalDate.now())
                .build();

        return template.insert(teamAccession)
                .then();
    }

    public Mono<Void> deleteAccession(Long accessionId) {
        return template.delete(TeamAccessionDTO.class)
                .matching(query(where("id").is(accessionId)))
                .all()
                .then();
    }
    public Mono<Void> responseToInvitation(String accessionId, TeamAccessionDTO teamAccessionDTO) {
        return template.selectOne(query(where("id").is(accessionId)), TeamAccessionDTO.class)
                .flatMap(accession -> {
                    teamAccessionDTO.setTargetRegistered(true);
                    return template.update(teamAccessionDTO).then();
                });
    }

    public Mono<Void> sendRequest(TeamAccessionDTO teamAccessionDTO) {
        return template.insert(teamAccessionDTO)
                .then();
    }

}
