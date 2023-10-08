package com.tyiu.corn.service;

import com.tyiu.corn.config.exception.ErrorException;
import com.tyiu.corn.model.dto.SkillDTO;
import com.tyiu.corn.model.dto.TeamDTO;
import com.tyiu.corn.model.dto.TeamMemberDTO;
import com.tyiu.corn.model.entities.*;
import lombok.RequiredArgsConstructor;
import org.modelmapper.ModelMapper;
import org.springframework.data.mongodb.core.ReactiveMongoTemplate;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.time.Instant;
import java.util.ArrayList;
import java.util.List;


@Service
@RequiredArgsConstructor
public class TeamService {
    private final ReactiveMongoTemplate mongoTemplate;
    private final ModelMapper mapper;

    public Mono<TeamDTO> getTeam(String id) {
        return mongoTemplate.findById(id, Team.class).flatMap(team -> {
            TeamDTO teamDTO = TeamDTO.builder()
                    .id(team.getId())
                    .name(team.getName())
                    .description(team.getDescription())
                    .closed(team.getClosed())
                    .membersCount(team.getMembersCount())
                    .createdAt(team.getCreatedAt())
                    .build();

            return mongoTemplate.find(Query.query(Criteria.where("id").in(team.getSkills())), Skill.class)
                    .flatMap(s -> Flux.just(mapper.map(s, SkillDTO.class))).collectList().flatMap(s -> {
                        teamDTO.setSkills(s);
                        return Mono.empty();
                    })
                .then(mongoTemplate.findOne(Query.query(Criteria.where("email").is(team.getOwnerEmail())), User.class).flatMap(u ->
                        mongoTemplate.find(Query.query(Criteria.where("userEmail").is(u.getEmail())), UserSkill.class)
                                .collectList().flatMap(s -> {
                                    teamDTO.setOwner(TeamMemberDTO.builder()
                                            .email(u.getEmail())
                                            .firstName(u.getFirstName())
                                            .lastName(u.getLastName())
                                            .skills(s)
                                            .build());
                                    return Mono.just(teamDTO);
                                })))
                .then(mongoTemplate.findOne(Query.query(Criteria.where("email").is(team.getLeaderEmail())), User.class).flatMap(u ->
                        mongoTemplate.find(Query.query(Criteria.where("userEmail").is(u.getEmail())), UserSkill.class)
                                .collectList().flatMap(s -> {
                                    teamDTO.setLeader(TeamMemberDTO.builder()
                                            .email(u.getEmail())
                                            .firstName(u.getFirstName())
                                            .lastName(u.getLastName())
                                            .skills(s)
                                            .build());
                                    return Mono.just(teamDTO);
                                })))
                .then(mongoTemplate.find(Query.query(Criteria.where("email").in(team.getMembers())), User.class).flatMap(u ->
                        mongoTemplate.find(Query.query(Criteria.where("userEmail").is(u.getEmail())), UserSkill.class)
                                .collectList().flatMap(s -> Mono.just(TeamMemberDTO.builder()
                                        .email(u.getEmail())
                                        .firstName(u.getFirstName())
                                        .lastName(u.getLastName())
                                        .skills(s)
                                        .build())))
                        .collectList().flatMap(u -> {
                            teamDTO.setMembers(u);
                            return Mono.just(teamDTO);
                        }));
            }).onErrorResume(ex -> Mono.error(new ErrorException("Not success!")));
    }

    public Flux<TeamDTO> getTeams(){
        return mongoTemplate.findAll(Team.class).flatMap(t -> Flux.just(TeamDTO.builder()
                        .id(t.getId())
                        .name(t.getName())
                        .description(t.getDescription())
                        .closed(t.getClosed())
                        .membersCount(t.getMembers().size())
                        .createdAt(t.getCreatedAt())
                        .build()))
                .onErrorResume(ex -> Mono.error(new ErrorException("Couldn't get a list of ideas")));
    }

    public Flux<TeamInvitation> getInvitation(String email) {
        return mongoTemplate.find(Query.query(Criteria.where("receiverEmail").is(email)), TeamInvitation.class)
                .onErrorResume(ex -> Mono.error(new ErrorException("Failed to receive invitations")));
    }

    public Mono<TeamDTO> addTeam(TeamDTO teamDTO) {
        Team team = Team.builder()
                .name(teamDTO.getName())
                .description(teamDTO.getDescription())
                .closed(teamDTO.getClosed())
                .membersCount(teamDTO.getMembers().size())
                .ownerEmail(teamDTO.getOwner().getEmail())
                .leaderEmail(teamDTO.getLeader().getEmail())
                .createdAt(Instant.now())
                .build();
        if (teamDTO.getMembers() != null)
        {
            List<String> users = new ArrayList<>();
            teamDTO.getMembers().forEach(u -> users.add(u.getEmail()));
            team.setMembers(users);
        }
        if (teamDTO.getSkills() != null)
        {
            List<String> skills = new ArrayList<>();
            teamDTO.getSkills().forEach(s -> skills.add(s.getId()));
            team.setSkills(skills);
        }
        return mongoTemplate.save(team).flatMap(t -> {
            teamDTO.setId(t.getId());
            teamDTO.setMembersCount(t.getMembersCount());
            return Mono.just(teamDTO);
        }).onErrorResume(ex -> Mono.error(new ErrorException("Failed to add team")));
    }

    public Mono<TeamInvitation> sendInviteToUser(String email, String teamId) {
        return mongoTemplate.findById(teamId, Team.class).flatMap(t ->
                mongoTemplate.save(TeamInvitation.builder()
                    .receiverEmail(email)
                    .teamId(teamId)
                    .teamName(t.getName())
                    .createdAt(Instant.now())
                    .build()))
                .onErrorResume(ex -> Mono.error(new ErrorException("Failed to send invitation")));
    }
    public Mono<Void> deleteTeam(String id) {
        return mongoTemplate.remove(Query.query(Criteria.where("id").is(id)),Team.class).then()
                .onErrorResume(ex -> Mono.error(new ErrorException("Not success!")));
    }

    public Mono<Void> deleteInvite(String id) {
        return mongoTemplate.remove(Query.query(Criteria.where("id").is(id)),TeamInvitation.class).then()
                .onErrorResume(ex -> Mono.error(new ErrorException("Not success!")));
    }

    public Mono<Void> kickFromTeam(String teamId, String email) {
        return mongoTemplate.findById(teamId, Team.class)
                .flatMap(t -> {
                    t.getMembers().remove(email);
                    t.setMembersCount(t.getMembers().size());
                    return mongoTemplate.save(t).then();
                })
                .onErrorResume(ex -> Mono.error(new ErrorException("Failed to kick")));
    }

    public Mono<Void> inviteInTeam(String teamId, String email) {
        return mongoTemplate.findById(teamId, Team.class)
                .flatMap(t -> {
                    t.getMembers().add(email);
                    t.setMembersCount(t.getMembers().size());
                    return mongoTemplate.save(t)
                        .then(mongoTemplate.remove(Query.query(Criteria.where("teamId").is(teamId)
                                .and("receiverEmail").is(email)),TeamInvitation.class))
                        .then();
                })
                .onErrorResume(ex -> Mono.error(new ErrorException("Failed to invite")));
    }
    public Mono<Void> updateTeam(String id, TeamDTO teamDTO) {
        return mongoTemplate.findById(id, Team.class).flatMap(t -> {
            if (teamDTO.getMembers() != null)
            {
                List<String> users = new ArrayList<>();
                teamDTO.getMembers().forEach(u -> users.add(u.getEmail()));
                t.setMembers(users);
            }
            if (teamDTO.getSkills() != null)
            {
                List<String> skills = new ArrayList<>();
                teamDTO.getSkills().forEach(s -> skills.add(s.getId()));
                t.setSkills(skills);
            }

            t.setName(teamDTO.getName());
            t.setDescription(teamDTO.getDescription());
            t.setClosed(teamDTO.getClosed());
            t.setMembersCount(teamDTO.getMembers().size());
            t.setOwnerEmail(teamDTO.getOwner().getEmail());
            t.setLeaderEmail(teamDTO.getLeader().getEmail());
            return mongoTemplate.save(t).then();
        }).onErrorResume(ex -> Mono.error(new ErrorException("Not success!")));
    }
}
