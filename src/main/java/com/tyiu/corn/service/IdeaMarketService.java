package com.tyiu.corn.service;

import com.tyiu.corn.model.dto.IdeaMarketDTO;
import com.tyiu.corn.model.dto.TeamMarketRequestDTO;
import com.tyiu.corn.model.entities.IdeaMarket;
import com.tyiu.corn.model.entities.TeamMarketRequest;
import com.tyiu.corn.model.entities.mappers.IdeaMarketMapper;
import com.tyiu.corn.model.entities.mappers.TeamMarketMapper;
import com.tyiu.corn.model.entities.relations.Favorite2Idea;
import lombok.RequiredArgsConstructor;
import org.modelmapper.ModelMapper;
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
public class IdeaMarketService {

    private final R2dbcEntityTemplate template;
    private final ModelMapper mapper;

    private Mono<IdeaMarketDTO> getOneMarketIdea(Long userId, Long ideaMarketId){
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

    public Flux<IdeaMarketDTO> getAllMarketIdeas(Long userId){
        return template.getDatabaseClient()
                .sql("SELECT idea_market.id FROM idea_market")
                .map((row, rowMetadata) ->
                        IdeaMarketDTO.builder()
                                .id(row.get("id",Long.class))
                                .build())
                .all()
                .flatMap(id -> getOneMarketIdea(userId, id.getId()));
    }

    public Mono<IdeaMarketDTO> getMarketIdea(Long userId, Long ideaMarketId){
        return getOneMarketIdea(userId, ideaMarketId);
    }

    public Flux<IdeaMarketDTO> getAllFavoriteMarketIdeas(Long userId){
        return template.select(query(where("user_id").is(userId)), Favorite2Idea.class)
                .flatMap(id -> getOneMarketIdea(userId, id.getIdeaMarketId()));
    }

    public Flux<TeamMarketRequestDTO> getAllTeamsRequests(Long ideaId){
        String query = "SELECT team_market_request.id, team_market_request.idea_id " +
                "FROM team_market_request " +
                "WHERE team_market_request.idea_id = :ideaId";
        return template.getDatabaseClient()
                .sql(query)
                .bind("ideaId", ideaId)
                .map((row, rowMetadata) ->
                        IdeaMarketDTO.builder()
                                .id(row.get("id",Long.class))
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
                            "WHERE team_market_request.idea_id = :ideaMarketId";
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

    public Mono<IdeaMarketDTO> sendIdeaOnMarket(Long ideaId, IdeaMarketDTO ideaMarketDTO){
        ideaMarketDTO.setCreatedAt(LocalDate.now());
        ideaMarketDTO.setRequests(0L);
        ideaMarketDTO.setAcceptedRequests(0L);
        ideaMarketDTO.setIsFavorite(false);
        IdeaMarket ideaMarket = mapper.map(ideaMarketDTO, IdeaMarket.class);
        ideaMarket.setIdeaId(ideaId);
        return template.insert(ideaMarket)
                .flatMap(i -> {
                    ideaMarketDTO.setId(i.getId());
                    return Mono.just(ideaMarketDTO);
                });
    }

    public Mono<TeamMarketRequestDTO> declareTeam(Long teamId, TeamMarketRequestDTO teamMarketRequestDTO){
        teamMarketRequestDTO.setUpdatedAt(LocalDate.now());
        TeamMarketRequest teamMarketRequest = mapper.map(teamMarketRequestDTO, TeamMarketRequest.class);
        teamMarketRequest.setTeamId(teamId);
        teamMarketRequest.setOwnerId(teamMarketRequestDTO.getOwner().getUserId());
        teamMarketRequest.setLeaderId(teamMarketRequestDTO.getLeader().getUserId());
        return template.insert(teamMarketRequest).flatMap(r -> {
            teamMarketRequestDTO.setId(r.getId());
            return Mono.just(teamMarketRequestDTO);
        });
    }

    public Mono<Void> makeMarketIdeaFavorite(Long userId, Long ideaMarketId){
        return template.insert(new Favorite2Idea(userId,ideaMarketId)).then();
    }

    ///////////////////////////////////////////
    //   ___    ____   __    ____ ______   ____
    //  / _ \  / __/  / /   / __//_  __/  / __/
    // / // / / _/   / /__ / _/   / /    / _/
    ///____/ /___/  /____//___/  /_/    /___/
    ///////////////////////////////////////////

    public Mono<Void> deleteMarketIdea(Long ideaMarketId){
        return template.delete(query(where("id").is(ideaMarketId)), IdeaMarket.class).then();
    }

    public Mono<Void> deleteTeamMarketRequest(Long teamMarketRequestId){
        return template.delete(query(where("id").is(teamMarketRequestId)), TeamMarketRequest.class).then();
    }

    public Mono<Void> deleteMarketIdeaFromFavorite(Long userId, Long ideaMarketId){
        return template.delete(query(where("user_id").is(userId)
                .and("idea_market_id").is(ideaMarketId)), Favorite2Idea.class).then();
    }

    ////////////////////////
    //   ___   __  __ ______
    //  / _ \ / / / //_  __/
    // / ___// /_/ /  / /
    ///_/    \____/  /_/
    ////////////////////////

    public Mono<Void> acceptTeam(Long teamMarketId){
        return template.update(query(where("id").is(teamMarketId)),
                update("accepted", true),
                TeamMarketRequest.class).then();
    }
}
