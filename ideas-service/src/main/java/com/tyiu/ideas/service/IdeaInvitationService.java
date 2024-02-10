package com.tyiu.ideas.service;

import com.tyiu.ideas.model.dto.IdeaDTO;
import com.tyiu.ideas.model.dto.IdeaInvitationDTO;
import com.tyiu.ideas.model.dto.SkillDTO;
import com.tyiu.ideas.model.entities.IdeaInvitation;
import com.tyiu.ideas.model.entities.IdeaMarket;
import com.tyiu.ideas.model.entities.Team;
import com.tyiu.ideas.model.enums.IdeaMarketStatusType;
import com.tyiu.ideas.model.enums.RequestStatus;
import com.tyiu.ideas.model.enums.SkillType;
import com.tyiu.ideas.model.requests.IdeaInvitationStatusRequest;
import lombok.RequiredArgsConstructor;
import org.springframework.data.r2dbc.core.R2dbcEntityTemplate;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.util.ArrayList;
import java.util.concurrent.ConcurrentHashMap;

import static org.springframework.data.relational.core.query.Criteria.where;
import static org.springframework.data.relational.core.query.Query.query;
import static org.springframework.data.relational.core.query.Update.update;

@Service
@RequiredArgsConstructor
public class IdeaInvitationService {

    private final R2dbcEntityTemplate template;

    public Flux<IdeaInvitationDTO> getAllInvitationsInTeam(String teamId) {
        String query = "SELECT inv.*, idea.name idea_name, idea.initiator_id initiator_id " +
                "ids.skill_id skill_id, s.name skill_name, s.type skill_type FROM idea_invitation inv " +
                "LEFT JOIN idea ON idea.id = inv.idea_id " +
                "LEFT JOIN idea_skill ids ON ids.idea_id = inv.idea_id LEFT JOIN skill s ON s.id = skill_id " +
                "WHERE inv.team_id = :teamId";
        ConcurrentHashMap<String, IdeaInvitationDTO> map = new ConcurrentHashMap<>();
        return template.getDatabaseClient().sql(query)
                .bind("teamId", teamId)
                .map((row, rowMetadata) -> {
                    String id = row.get("id", String.class);
                    String skillId = row.get("skill_id", String.class);
                    IdeaInvitationDTO invitation = map.getOrDefault(id,
                            IdeaInvitationDTO.builder()
                                    .id(id)
                                    .initiatorId(row.get("initiator_id",String.class))
                                    .ideaId(row.get("idea_id", String.class))
                                    .ideaName(row.get("idea_name", String.class))
                                    .skills(new ArrayList<>())
                                    .teamId(row.get("team_id", String.class))
                                    .status(RequestStatus.valueOf(row.get("status", String.class))).build());
                    if (skillId!=null) {
                        SkillDTO skill = SkillDTO.builder()
                                .name(row.get("skill_name", String.class))
                                .type(SkillType.valueOf(row.get("skill_type", String.class)))
                                .id(skillId)
                                .build();
                        invitation.getSkills().add(skill);
                    }
                    map.put(id,invitation);
                    return invitation;
                })
                .all().thenMany(Flux.fromIterable(map.values()));
    }

    public Flux<IdeaInvitationDTO> getAllInvitationInIdea(String ideaId) {
        String query = "SELECT inv.*, team.name team_name, " +
                "(SELECT COUNT(*) FROM team_member WHERE team_id = team.id) " +
                "team_members_count, ts.skill_id skill_id, s.name skill_name, s.type skill_type FROM idea_invitation inv " +
                "LEFT JOIN team ON team.id = inv.team_id " +
                "LEFT JOIN idea ON idea.id = inv.idea_id " +
                "LEFT JOIN team_skill ts ON ts.team_id = inv.team_id LEFT JOIN skill s ON s.id = skill_id " +
                "WHERE inv.idea_id = :ideaId";
        ConcurrentHashMap<String, IdeaInvitationDTO> map = new ConcurrentHashMap<>();
        return template.getDatabaseClient().sql(query)
                .bind("ideaId", ideaId)
                .map((row, rowMetadata) -> {
                    String id = row.get("id", String.class);
                    String skillId = row.get("skill_id", String.class);
                    IdeaInvitationDTO invitation = map.getOrDefault(id,
                            IdeaInvitationDTO.builder()
                                    .id(id)
                                    .ideaId(row.get("idea_id", String.class))
                                    .teamName(row.get("team_name", String.class))
                                    .skills(new ArrayList<>())
                                    .teamMembersCount(row.get("team_members_count", Short.class))
                                    .teamId(row.get("team_id", String.class))
                                    .status(RequestStatus.valueOf(row.get("status", String.class))).build());
                    if (skillId!=null) {
                        SkillDTO skill = SkillDTO.builder()
                                .name(row.get("skill_name", String.class))
                                .type(SkillType.valueOf(row.get("skill_type", String.class)))
                                .id(skillId)
                                .build();
                        invitation.getSkills().add(skill);
                    }
                    map.put(id,invitation);
                    return invitation;
                })
                .all().thenMany(Flux.fromIterable(map.values()));
    }

    public Mono<Void> changeInvitationStatus(IdeaInvitationStatusRequest request) {
        if (request.getStatus().equals(RequestStatus.ACCEPTED)) {
            template.update(query(where("team_id").is(request.getTeamId())
                            .and(where("id").not(request.getId()))),
                                    update("status", RequestStatus.CANCELED), IdeaInvitation.class)
                            .then(template.update(query(where("idea_id").is(request.getIdeaId())),
                                    update("team_id", request.getTeamId())
                                            .set("status", IdeaMarketStatusType.RECRUITMENT_IS_CLOSED),
                                    IdeaMarket.class))
                            .then(template.update(query(where("id").is(request.getTeamId())),
                                    update("has_active_project", true), Team.class)).subscribe();
        }
        return template.update(query(where("id").is(request.getId())),
                update("status",request.getStatus()), IdeaInvitation.class).then();
    }

    public Mono<Void> inviteToIdea(String ideaId, String teamId) {
        IdeaInvitation invitation = new IdeaInvitation(null, ideaId, teamId, RequestStatus.NEW);
        return template.insert(invitation).then();
    }

    public Flux<IdeaInvitationDTO> getAllInvitationsByInitiator(String userId) {
        String query = """
                SELECT i.id idea_id, i.name idea_name, i.initiator_id initiator_id,
                inv.id invitation_id, inv.status status ,inv.team_id team_id, t.name team_name FROM idea i
                RIGHT JOIN idea_invitation inv ON inv.idea_id = i.id
                LEFT JOIN team t ON t.id = inv.team_id
                WHERE i.initiator_id = :userId
                """;
        return template.getDatabaseClient().sql(query)
                .bind("userId",userId)
                .map((row, rowMetadata) ->
                    IdeaInvitationDTO.builder()
                            .id(row.get("invitation_id",String.class))
                            .ideaId(row.get("idea_id",String.class))
                            .ideaName(row.get("idea_name",String.class))
                            .initiatorId(row.get("initiator_id",String.class))
                            .teamId(row.get("team_id",String.class))
                            .teamName(row.get("team_name",String.class))
                            .status(RequestStatus.valueOf(row.get("status", String.class)))
                            .build())
                .all();
    }
    public Flux<IdeaDTO> getAllInitiatorMarketIdeasForInvitations(String userId) {
        String query = """
                SELECT i.name name, i.id id, im.market_id mid, m.status status FROM idea i
                LEFT JOIN idea_market im ON im.idea_id = i.id
                LEFT JOIN market m ON m.id = im.market_id
                WHERE i.initiator_id = :userId AND m.status = 'ACTIVE'
                """;
        return template.getDatabaseClient().sql(query)
                .bind("userId", userId)
                .map((row, rowMetadata) ->
                        IdeaDTO.builder()
                                .id(row.get("id",String.class))
                                .name(row.get("name", String.class)).build())
                .all();
    }
}
