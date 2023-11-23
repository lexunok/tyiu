package com.tyiu.corn.service;

import com.tyiu.corn.model.dto.IdeaDTO;
import com.tyiu.corn.model.dto.IdeaMarketDTO;
import com.tyiu.corn.model.dto.SkillDTO;
import com.tyiu.corn.model.dto.TeamMarketRequestDTO;
import com.tyiu.corn.model.entities.IdeaMarket;
import com.tyiu.corn.model.entities.TeamMarketRequest;
import com.tyiu.corn.model.entities.mappers.IdeaMarketMapper;
import com.tyiu.corn.model.entities.mappers.TeamMarketMapper;
import com.tyiu.corn.model.entities.relations.Favorite2Idea;
import com.tyiu.corn.model.enums.IdeaMarketStatusType;
import com.tyiu.corn.model.enums.SkillType;
import com.tyiu.corn.model.requests.IdeaMarketRequest;
import lombok.RequiredArgsConstructor;
import org.modelmapper.ModelMapper;
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
public class IdeaMarketService {

    private final R2dbcEntityTemplate template;
    private final ModelMapper mapper;

    private Mono<IdeaMarketDTO> getOneMarketIdea(String userId, String ideaMarketId){
        String QUERY = "SELECT idea_market.*, skill.id s_id, skill.name s_name, skill.type, favorite_idea.* " +
                "FROM idea_market " +
                "LEFT JOIN idea_skill ON idea_skill.idea_id = idea_market.idea_id " +
                "LEFT JOIN favorite_idea ON favorite_idea.user_id = :userId " +
                "LEFT JOIN skill ON idea_skill.skill_id = skill.id " +
                "WHERE idea_market.id = :ideaMarketId";
        IdeaMarketMapper ideaMarketMapper = new IdeaMarketMapper();
        return template.getDatabaseClient()
                .sql(QUERY)
                .bind("ideaMarketId",ideaMarketId)
                .bind("userId", userId)
                .map(ideaMarketMapper::apply)
                .all()
                .collectList()
                .map(i -> i.get(0));
    }

    ///////////////////////
    //  _____   ____ ______
    // / ___/  / __//_  __/
    /// (_ /  / _/   / /
    //\___/  /___/  /_/
    ///////////////////////

    public Flux<IdeaMarketDTO> getAllMarketIdeas(String userId){
        return template.getDatabaseClient()
                .sql("SELECT idea_market.*, favorite_idea.*, ROW_NUMBER () OVER (ORDER BY idea_market.requests DESC) " +
                        "FROM idea_market " +
                        "LEFT JOIN favorite_idea ON favorite_idea.user_id = :userId")
                .bind("userId",userId)
                .map((row, rowMetadata) -> {
                    IdeaMarketDTO ideaMarketDTO = IdeaMarketDTO.builder()
                            .id(row.get("id", String.class))
                            .ideaId(row.get("idea_id", String.class))
                            .position(row.get("row_number", Long.class))
                            .name(row.get("name", String.class))
                            .initiator(row.get("initiator", String.class))
                            .description(row.get("description", String.class))
                            .stack(new ArrayList<>())
                            .createdAt(row.get("created_at", LocalDate.class))
                            .maxTeamSize(row.get("max_team_size", Short.class))
                            .status(IdeaMarketStatusType.valueOf(row.get("status", String.class)))
                            .requests(row.get("requests", Long.class))
                            .acceptedRequests(row.get("accepted_requests", Long.class))
                            .startDate(row.get("start_date", LocalDate.class))
                            .finishDate(row.get("finish_date", LocalDate.class))
                            .isFavorite(false)
                            .build();
                    if (Objects.equals(row.get("idea_market_id", String.class), ideaMarketDTO.getId()))
                    {
                        ideaMarketDTO.setIsFavorite(true);
                    }
                    return ideaMarketDTO;
                })
                .all()
                .flatMap(idea -> template.getDatabaseClient()
                        .sql("SELECT skill.* " +
                            "FROM skill " +
                            "LEFT JOIN idea_skill ON idea_skill.idea_id = :ideaMarketId " +
                            "WHERE skill.id = idea_skill.skill_id")
                        .bind("ideaMarketId", idea.getIdeaId())
                        .map((row, rowMetadata) -> {
                            SkillDTO skillDTO = SkillDTO.builder()
                                    .id(row.get("id", String.class))
                                    .name(row.get("name", String.class))
                                    .type(SkillType.valueOf(row.get("type", String.class)))
                                    .build();
                            if(idea.getStack().stream().noneMatch(skill -> skill.getId().equals(skillDTO.getId()))) {
                                idea.getStack().add(skillDTO);
                            }
                            return idea;
                        })
                        .all()
                        .collectList()
                        .map(i -> i.get(0)));
    }

    public Flux<IdeaMarketDTO> getAllInitiatorMarketIdeas(String userId){
        return template.getDatabaseClient()
                .sql("SELECT idea_market.*, favorite_idea.*, ROW_NUMBER () OVER (ORDER BY idea_market.requests DESC) " +
                        "FROM idea_market " +
                        "LEFT JOIN favorite_idea ON favorite_idea.user_id = :userId " +
                        "LEFT JOIN users ON users.id = :userId " +
                        "WHERE idea_market.initiator = users.email")
                .bind("userId",userId)
                .map((row, rowMetadata) -> {
                    IdeaMarketDTO ideaMarketDTO = IdeaMarketDTO.builder()
                            .id(row.get("id", String.class))
                            .ideaId(row.get("idea_id", String.class))
                            .position(row.get("row_number", Long.class))
                            .name(row.get("name", String.class))
                            .initiator(row.get("initiator", String.class))
                            .description(row.get("description", String.class))
                            .stack(new ArrayList<>())
                            .createdAt(row.get("created_at", LocalDate.class))
                            .maxTeamSize(row.get("max_team_size", Short.class))
                            .status(IdeaMarketStatusType.valueOf(row.get("status", String.class)))
                            .requests(row.get("requests", Long.class))
                            .acceptedRequests(row.get("accepted_requests", Long.class))
                            .startDate(row.get("start_date", LocalDate.class))
                            .finishDate(row.get("finish_date", LocalDate.class))
                            .isFavorite(false)
                            .build();
                    if (Objects.equals(row.get("idea_market_id", String.class), ideaMarketDTO.getId()))
                    {
                        ideaMarketDTO.setIsFavorite(true);
                    }
                    return ideaMarketDTO;
                })
                .all()
                .flatMap(idea -> template.getDatabaseClient()
                        .sql("SELECT skill.* " +
                                "FROM skill " +
                                "LEFT JOIN idea_skill ON idea_skill.idea_id = :ideaMarketId " +
                                "WHERE skill.id = idea_skill.skill_id")
                        .bind("ideaMarketId", idea.getIdeaId())
                        .map((row, rowMetadata) -> {
                            SkillDTO skillDTO = SkillDTO.builder()
                                    .id(row.get("id", String.class))
                                    .name(row.get("name", String.class))
                                    .type(SkillType.valueOf(row.get("type", String.class)))
                                    .build();
                            if(idea.getStack().stream().noneMatch(skill -> skill.getId().equals(skillDTO.getId()))) {
                                idea.getStack().add(skillDTO);
                            }
                            return idea;
                        })
                        .all()
                        .collectList()
                        .map(i -> i.get(0)));
    }

    public Mono<IdeaMarketDTO> getMarketIdea(String userId, String ideaMarketId){
        return getOneMarketIdea(userId, ideaMarketId);
    }

    public Flux<IdeaMarketDTO> getAllFavoriteMarketIdeas(String userId){
        return template.select(query(where("user_id").is(userId)), Favorite2Idea.class)
                .flatMap(id -> getOneMarketIdea(userId, id.getIdeaMarketId()));
    }

    public Flux<TeamMarketRequestDTO> getAllTeamsRequests(String ideaId){
        String query = "SELECT team_market_request.id, team_market_request.idea_market_id " +
                "FROM team_market_request " +
                "WHERE team_market_request.idea_market_id = :ideaId";
        return template.getDatabaseClient()
                .sql(query)
                .bind("ideaId", ideaId)
                .map((row, rowMetadata) ->
                        IdeaMarketDTO.builder()
                                .id(row.get("id",String.class))
                                .build())
                .all()
                .flatMap(id -> {
                    String QUERY = "SELECT team_market_request.*, " +
                            "o.id o_id, o.email o_email, o.first_name o_first_name, o.last_name o_last_name, " +
                            "l.id l_id, l.email l_email, l.first_name l_first_name, l.last_name l_last_name, " +
                            "m.id m_id, m.email m_email, m.first_name m_first_name, m.last_name m_last_name, " +
                            "s.id s_id, s.name s_name, s.type " +
                            "FROM team_market_request " +
                            "LEFT JOIN team_member ON team_market_request.team_id = team_member.team_id " +
                            "LEFT JOIN team_skill ON team_market_request.team_id = team_skill.team_id " +
                            "LEFT JOIN users o ON team_market_request.owner_id = o.id " +
                            "LEFT JOIN users l ON team_market_request.leader_id = l.id " +
                            "LEFT JOIN users m ON team_member.member_id = m.id " +
                            "LEFT JOIN skill s ON team_skill.skill_id = s.id " +
                            "WHERE team_market_request.idea_market_id = :ideaMarketId";
                    TeamMarketMapper teamMarketMapper = new TeamMarketMapper();
                    return template.getDatabaseClient()
                            .sql(QUERY)
                            .bind("ideaMarketId",id.getId())
                            .map(teamMarketMapper::apply)
                            .all()
                            .collectList()
                            .map(i -> i.get(0));
                });
    }

    //////////////////////////////
    //   ___   ____    ____ ______
    //  / _ \ / __ \  / __//_  __/
    // / ___// /_/ / _\ \   / /
    ///_/    \____/ /___/  /_/
    //////////////////////////////

    public Flux<IdeaMarketDTO> sendIdeaOnMarket(List<IdeaMarketRequest> ideaDTOList) {
        return Flux.fromIterable(ideaDTOList)
                .flatMap(ideaDTO -> template.insert(IdeaMarket.builder()
                                .ideaId(ideaDTO.getId())
                                .position(1L)
                                .name(ideaDTO.getName())
                                .initiator(ideaDTO.getInitiatorEmail())
                                .description(ideaDTO.getDescription())
                                .createdAt(LocalDate.from(ideaDTO.getCreatedAt()))
                                .maxTeamSize(ideaDTO.getMaxTeamSize())
                                .status(IdeaMarketStatusType.RECRUITMENT_IS_OPEN)
                                .requests(0L)
                                .acceptedRequests(0L)
                                .startDate(ideaDTO.getStartDate())
                                .finishDate(ideaDTO.getFinishDate())
                                .build())
                        .map(i -> mapper.map(i, IdeaMarketDTO.class))
                );
    }

    public Mono<TeamMarketRequestDTO> declareTeam(TeamMarketRequestDTO teamMarketRequestDTO){
        teamMarketRequestDTO.setUpdatedAt(LocalDate.now());
        TeamMarketRequest teamMarketRequest = mapper.map(teamMarketRequestDTO, TeamMarketRequest.class);
        teamMarketRequest.setOwnerId(teamMarketRequestDTO.getOwner().getId());
        teamMarketRequest.setLeaderId(teamMarketRequestDTO.getLeader().getId());
        return template.insert(teamMarketRequest).flatMap(r -> {
            teamMarketRequestDTO.setId(r.getId());
            String id = r.getIdeaMarketId();
            return template.select(query(where("idea_market_id").is(id)),TeamMarketRequest.class).count().flatMap(c ->
                    template.update(query(where("id").is(id)),
                            update("requests", c),
                            IdeaMarket.class).thenReturn(teamMarketRequestDTO));
        });
    }

    public Mono<Void> makeMarketIdeaFavorite(String userId, String ideaMarketId){
        return template.insert(new Favorite2Idea(userId,ideaMarketId)).then();
    }

    ///////////////////////////////////////////
    //   ___    ____   __    ____ ______   ____
    //  / _ \  / __/  / /   / __//_  __/  / __/
    // / // / / _/   / /__ / _/   / /    / _/
    ///____/ /___/  /____//___/  /_/    /___/
    ///////////////////////////////////////////

    public Mono<Void> deleteMarketIdea(String ideaMarketId){
        return template.delete(query(where("id").is(ideaMarketId)), IdeaMarket.class).then();
    }

    public Mono<Void> deleteTeamMarketRequest(String teamMarketRequestId){
        return template.delete(query(where("id").is(teamMarketRequestId)), TeamMarketRequest.class).then();
    }

    public Mono<Void> deleteMarketIdeaFromFavorite(String userId, String ideaMarketId){
        return template.delete(query(where("user_id").is(userId)
                .and("idea_market_id").is(ideaMarketId)), Favorite2Idea.class).then();
    }

    ////////////////////////
    //   ___   __  __ ______
    //  / _ \ / / / //_  __/
    // / ___// /_/ /  / /
    ///_/    \____/  /_/
    ////////////////////////

    public Mono<Void> acceptTeam(String teamMarketId){
        return template.update(query(where("id").is(teamMarketId)),
                update("accepted", true),
                TeamMarketRequest.class).then();
    }
}
