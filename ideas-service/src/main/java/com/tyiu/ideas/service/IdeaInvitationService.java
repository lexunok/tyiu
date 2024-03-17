package com.tyiu.ideas.service;

import com.tyiu.ideas.model.dto.IdeaInvitationDTO;
import com.tyiu.ideas.model.dto.IdeaMarketDTO;
import com.tyiu.ideas.model.dto.SkillDTO;
import com.tyiu.ideas.model.entities.*;
import com.tyiu.ideas.model.enums.IdeaMarketStatusType;
import com.tyiu.ideas.model.enums.RequestStatus;
import com.tyiu.ideas.model.enums.SkillType;
import com.tyiu.ideas.model.requests.IdeaInvitationStatusRequest;
import com.tyiu.ideas.publisher.NotificationPublisher;
import enums.NotificationCase;
import enums.PortalLinks;
import lombok.RequiredArgsConstructor;
import org.springframework.data.r2dbc.core.R2dbcEntityTemplate;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import request.NotificationRequest;

import java.util.HashSet;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import static org.springframework.data.relational.core.query.Criteria.where;
import static org.springframework.data.relational.core.query.Query.query;
import static org.springframework.data.relational.core.query.Update.update;

@Service
@RequiredArgsConstructor
public class IdeaInvitationService {

    private final R2dbcEntityTemplate template;
    private final NotificationPublisher notificationPublisher;

    private void sendNotification(String ideaId, String teamId, NotificationCase notificationCase) {

        template.selectOne(query(where("id").is(ideaId)), Idea.class)
                        .flatMap(idea -> template.selectOne(query(where("id").is(idea.getInitiatorId())), User.class)
                                .flatMap(initiator -> template.selectOne(query(where("id").is(teamId)), Team.class)
                                        .flatMap(team -> template.selectOne(query(where("owner_id").is(team.getOwnerId())), User.class)
                                                .flatMap(teamOwner -> {

                                                    switch (notificationCase) {

                                                        case INITIATOR_INVITES_TEAM -> notificationPublisher.makeNotification(

                                                                NotificationRequest.builder()
                                                                        .publisherEmail(initiator.getEmail())
                                                                        .consumerEmail(teamOwner.getEmail())
                                                                        .title("Вашу команду пригласили в идею")
                                                                        .message(String.format(
                                                                                "Инициатор %s %s пригласил вашу команду \"%s\" в идею \"%s\".",
                                                                                initiator.getFirstName(),
                                                                                initiator.getLastName(),
                                                                                idea.getName(),
                                                                                team.getName()
                                                                        ))
                                                                        .link(PortalLinks.IDEA_INVITATION + teamId)
                                                                        .buttonName("Перейти к приглашению")
                                                                        .build());

                                                        case TEAM_INVITE_IS_ACCEPTED -> notificationPublisher.makeNotification(

                                                                NotificationRequest.builder()
                                                                        .publisherEmail(teamOwner.getEmail())
                                                                        .consumerEmail(initiator.getEmail())
                                                                        .title("Команда приняла ваше приглашение")
                                                                        .message(String.format(
                                                                                "Команда \"%s\" приняла ваше приглашение в идею \"%s\".",
                                                                                team.getName(),
                                                                                idea.getName()
                                                                        ))
                                                                        .link(PortalLinks.TEAM + teamId)
                                                                        .buttonName("Перейти к команде")
                                                                        .build());

                                                        case TEAM_INVITE_IS_CANCELLED -> notificationPublisher.makeNotification(

                                                                NotificationRequest.builder()
                                                                        .publisherEmail(initiator.getEmail())
                                                                        .consumerEmail(teamOwner.getEmail())
                                                                        .title("Команда отклонила ваше приглашение")
                                                                        .message(String.format(
                                                                                "Команда \"%s\" отклонила ваше приглашение в идею \"%s\".",
                                                                                team.getName(),
                                                                                idea.getName()
                                                                        ))
                                                                        .build());
                                                    }

                                                    return Mono.empty();
                                                })))
                        ).subscribe();
    }

    public Flux<IdeaInvitationDTO> getAllInvitationsInTeam(String teamId) {

        String query = "SELECT inv.*, team.name team_name,idea.name idea_name, idea.initiator_id initiator_id, " +
                "ids.skill_id skill_id, s.name skill_name, s.type skill_type FROM idea_invitation inv " +
                "LEFT JOIN idea ON idea.id = inv.idea_id " +
                "LEFT JOIN team ON team.id = inv.team_id " +
                "LEFT JOIN idea_skill ids ON ids.idea_id = inv.idea_id LEFT JOIN skill s ON s.id = skill_id " +
                "WHERE inv.team_id = :teamId";

        ConcurrentHashMap<String, IdeaInvitationDTO> map = new ConcurrentHashMap<>();

        return template.getDatabaseClient()
                .sql(query)
                .bind("teamId", teamId)
                .map((row, rowMetadata) -> {

                    String id = row.get("id", String.class);
                    String skillId = row.get("skill_id", String.class);
                    IdeaInvitationDTO invitation = map.getOrDefault(id,
                            IdeaInvitationDTO.builder()
                                    .id(id)
                                    .initiatorId(row.get("initiator_id", String.class))
                                    .ideaId(row.get("idea_id", String.class))
                                    .ideaName(row.get("idea_name", String.class))
                                    .skills(new HashSet<>())
                                    .teamName(row.get("team_name", String.class))
                                    .teamId(row.get("team_id", String.class))
                                    .status(RequestStatus.valueOf(row.get("status", String.class)))
                                    .build());

                    if (skillId != null) {

                        SkillDTO skill = SkillDTO.builder()
                                .name(row.get("skill_name", String.class))
                                .type(SkillType.valueOf(row.get("skill_type", String.class)))
                                .id(skillId)
                                .build();
                        invitation.getSkills().add(skill);
                    }
                    map.put(id, invitation);
                    return invitation;
                })
                .all()
                .thenMany(Flux.fromIterable(map.values()));
    }

    public Flux<IdeaInvitationDTO> getAllInvitationInIdea(String ideaId) {

        String query = "SELECT inv.*, team.name team_name, " +
                "(SELECT COUNT(*) FROM team_member WHERE team_id = team.id AND finish_date IS NULL) " +
                "team_members_count, s.skill_id skill_id, s.name skill_name, s.type skill_type FROM idea_invitation inv " +
                "LEFT JOIN team ON team.id = inv.team_id " +
                "LEFT JOIN idea ON idea.id = inv.idea_id " +
                "LEFT JOIN team_member tm ON tm.team_id = t.id AND tm.finish_date IS NULL " +
                "LEFT JOIN user_skill us ON us.user_id = tm.member_id " +
                "LEFT JOIN skill s ON s.id = skill_id " +
                "WHERE inv.idea_id = :ideaId";

        ConcurrentHashMap<String, IdeaInvitationDTO> map = new ConcurrentHashMap<>();

        return template.getDatabaseClient()
                .sql(query)
                .bind("ideaId", ideaId)
                .map((row, rowMetadata) -> {

                    String id = row.get("id", String.class);
                    String skillId = row.get("skill_id", String.class);
                    IdeaInvitationDTO invitation = map.getOrDefault(id,
                            IdeaInvitationDTO.builder()
                                    .id(id)
                                    .ideaId(row.get("idea_id", String.class))
                                    .teamName(row.get("team_name", String.class))
                                    .skills(new HashSet<>())
                                    .teamMembersCount(row.get("team_members_count", Short.class))
                                    .teamId(row.get("team_id", String.class))
                                    .status(RequestStatus.valueOf(row.get("status", String.class)))
                                    .build());

                    if (skillId != null) {

                        SkillDTO skill = SkillDTO.builder()
                                .name(row.get("skill_name", String.class))
                                .type(SkillType.valueOf(row.get("skill_type", String.class)))
                                .id(skillId)
                                .build();
                        invitation.getSkills().add(skill);
                    }
                    map.put(id, invitation);
                    return invitation;
                })
                .all()
                .thenMany(Flux.fromIterable(map.values()));
    }

    public Mono<Void> changeInvitationStatus(IdeaInvitationStatusRequest request) {

        if (request.getStatus().equals(RequestStatus.ACCEPTED)) {

            sendNotification(
                    request.getIdeaId(),
                    request.getTeamId(),
                    NotificationCase.TEAM_INVITE_IS_ACCEPTED
            );

            return template.update(query(where("team_id").is(request.getTeamId())
                                    .or("idea_id").is(request.getIdeaId())
                                    .and(where("id").not(request.getId()))
                                    .and(where("status").is(RequestStatus.NEW))),
                            update("status", RequestStatus.ANNULLED), IdeaInvitation.class)
                            .then(template.getDatabaseClient()
                                    .sql("""
                                    UPDATE team_market_request SET status = :status 
                                    WHERE idea_market_id = (SELECT id FROM idea_market WHERE idea_id = :ideaId) OR team_id = :teamId
                                    """)
                                    .bind("status", "ANNULLED")
                                    .bind("ideaId", request.getIdeaId())
                                    .bind("teamId", request.getTeamId())
                                    .fetch()
                                    .all()
                                    .then())

                    .then(template.update(query(where("idea_id").is(request.getIdeaId())),
                            update("team_id", request.getTeamId())
                                    .set("status", IdeaMarketStatusType.RECRUITMENT_IS_CLOSED),
                            IdeaMarket.class))

                    .then(template.update(query(where("id").is(request.getTeamId())),
                            update("has_active_project", true),
                            Team.class))

                    .then(template.update(query(where("id").is(request.getId())),
                            update("status", request.getStatus()),
                            IdeaInvitation.class)).then();
        }
        if (request.getStatus().equals(RequestStatus.CANCELED))
            sendNotification(
                    request.getIdeaId(),
                    request.getTeamId(),
                    NotificationCase.TEAM_INVITE_IS_CANCELLED
            );

        return template.update(query(where("id").is(request.getId())),
                update("status", request.getStatus()), IdeaInvitation.class).then();
    }

    public Mono<IdeaInvitationDTO> inviteToIdea(String ideaId, String teamId) {

        String query = """
                SELECT inv.*, team.name team_name,idea.name idea_name, idea.initiator_id initiator_id,
                s.skill_id skill_id, s.name skill_name, s.type skill_type,
                (SELECT COUNT(*) FROM team_member WHERE team_id = inv.team_id AND finish_date IS NULL) team_members_count FROM idea_invitation inv
                LEFT JOIN idea ON idea.id = inv.idea_id
                LEFT JOIN team ON team.id = inv.team_id
                LEFT JOIN team_member tm ON tm.team_id = t.id AND tm.finish_date IS NULL
                LEFT JOIN user_skill us ON us.user_id = tm.member_id
                LEFT JOIN skill s ON s.id = us.skill_id
                WHERE inv.team_id = :teamId AND inv.idea_id = :ideaId
                """;

        IdeaInvitation invitation = new IdeaInvitation(null, ideaId, teamId, RequestStatus.NEW);

        return template.insert(invitation)
                .then(template.getDatabaseClient()
                        .sql(query)
                        .bind("teamId", teamId)
                        .bind("ideaId", ideaId)
                        .flatMap(r -> {

                            Set<SkillDTO> skills = new HashSet<>();

                            return r.map((row, rowMetadata) -> {

                                SkillDTO skill = SkillDTO.builder()
                                        .name(row.get("skill_name", String.class))
                                        .type(SkillType.valueOf(row.get("skill_type", String.class)))
                                        .id(row.get("skill_id", String.class))
                                        .build();
                                skills.add(skill);

                                IdeaInvitationDTO invite = IdeaInvitationDTO.builder()
                                        .id(row.get("id", String.class))
                                        .initiatorId(row.get("initiator_id", String.class))
                                        .ideaId(row.get("idea_id", String.class))
                                        .ideaName(row.get("idea_name", String.class))
                                        .teamMembersCount(row.get("team_members_count", Short.class))
                                        .skills(skills)
                                        .teamName(row.get("team_name", String.class))
                                        .teamId(row.get("team_id", String.class))
                                        .status(RequestStatus.valueOf(row.get("status", String.class)))
                                        .build();

                                sendNotification(
                                        ideaId,
                                        teamId,
                                        NotificationCase.INITIATOR_INVITES_TEAM
                                );

                                return invite;
                                });
                            }).last());
    }

    public Flux<IdeaInvitationDTO> getAllInvitationsByInitiator(String userId) {

        String query = """
                SELECT i.id idea_id, i.name idea_name, i.initiator_id initiator_id,
                inv.id invitation_id, inv.status status ,inv.team_id team_id, t.name team_name FROM idea i
                RIGHT JOIN idea_invitation inv ON inv.idea_id = i.id
                LEFT JOIN team t ON t.id = inv.team_id
                WHERE i.initiator_id = :userId
                """;

        return template.getDatabaseClient()
                .sql(query)
                .bind("userId",userId)
                .map((row, rowMetadata) ->
                    IdeaInvitationDTO.builder()
                            .id(row.get("invitation_id", String.class))
                            .ideaId(row.get("idea_id", String.class))
                            .ideaName(row.get("idea_name", String.class))
                            .initiatorId(row.get("initiator_id", String.class))
                            .teamId(row.get("team_id", String.class))
                            .teamName(row.get("team_name", String.class))
                            .status(RequestStatus.valueOf(row.get("status", String.class)))
                            .build()
                ).all();
    }

    public Flux<IdeaMarketDTO> getAllInitiatorMarketIdeasForInvitations(String userId) {

        String query = """
                SELECT i.name name, i.id id, im.status status ,im.market_id market_id, m.status status FROM idea i
                LEFT JOIN idea_market im ON im.idea_id = i.id
                LEFT JOIN market m ON m.id = im.market_id
                WHERE i.initiator_id = :userId AND m.status = 'ACTIVE'
                """;

        return template.getDatabaseClient()
                .sql(query)
                .bind("userId", userId)
                .map((row, rowMetadata) ->
                        IdeaMarketDTO.builder()
                                .id(row.get("id", String.class))
                                .status(IdeaMarketStatusType.valueOf(row.get("status", String.class)))
                                .name(row.get("name", String.class))
                                .build()
                ).all();
    }
}
