package com.tyiu.corn.service;

import com.tyiu.corn.model.dto.*;
import com.tyiu.corn.model.entities.IdeaMarket;
import com.tyiu.corn.model.entities.TeamMarketRequest;
import com.tyiu.corn.model.entities.User;
import com.tyiu.corn.model.entities.mappers.IdeaMarketMapper;
import com.tyiu.corn.model.entities.mappers.TeamMarketMapper;
import com.tyiu.corn.model.entities.relations.Favorite2Idea;
import com.tyiu.corn.model.enums.IdeaMarketStatusType;
import com.tyiu.corn.model.enums.SkillType;
import com.tyiu.corn.model.requests.IdeaMarketRequest;
import lombok.RequiredArgsConstructor;
import org.modelmapper.ModelMapper;
import org.springframework.data.r2dbc.core.R2dbcEntityTemplate;
import org.springframework.data.relational.core.query.Query;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;

import static org.springframework.data.relational.core.query.Criteria.where;
import static org.springframework.data.relational.core.query.Query.query;
import static org.springframework.data.relational.core.query.Update.update;

@Service
@RequiredArgsConstructor
public class IdeaMarketService {

    private final R2dbcEntityTemplate template;
    private final ModelMapper mapper;

    private Mono<IdeaMarketDTO> getOneMarketIdea(String userId, String ideaMarketId){
        String QUERY = "SELECT idea_market.*, " +
                "users.id u_id, users.first_name u_fn, users.last_name u_ln, users.email u_e, " +
                "skill.id s_id, skill.name s_name, skill.type, favorite_idea.* " +
                "FROM idea_market " +
                "LEFT JOIN idea_skill ON idea_skill.idea_id = idea_market.idea_id " +
                "LEFT JOIN favorite_idea ON favorite_idea.user_id = :userId " +
                "LEFT JOIN skill ON idea_skill.skill_id = skill.id " +
                "LEFT JOIN users ON users.id = idea_market.initiator_id " +
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

    private Flux<IdeaMarketDTO> getListMarketIdea(String QUERY, String userId){
        ConcurrentHashMap<String, IdeaMarketDTO> map = new ConcurrentHashMap<>();
        return template.getDatabaseClient()
                .sql(QUERY)
                .bind("userId",userId)
                .map((row, rowMetadata) -> {
                    String id = row.get("id", String.class);
                    String skillId = row.get("skill_id", String.class);
                    IdeaMarketDTO ideaMarketDTO = IdeaMarketDTO.builder()
                            .id(id)
                            .ideaId(row.get("idea_id", String.class))
                            .position(row.get("row_number", Long.class))
                            .name(row.get("name", String.class))
                            .initiator(UserDTO.builder()
                                    .id(row.get("u_id", String.class))
                                    .email(row.get("u_e", String.class))
                                    .firstName(row.get("u_fn", String.class))
                                    .lastName(row.get("u_ln", String.class))
                                    .build())
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
                    IdeaMarketDTO idea = map.getOrDefault(id, ideaMarketDTO);
                    if (skillId != null){
                        SkillDTO skill = SkillDTO.builder()
                                .name(row.get("skill_name", String.class))
                                .type(SkillType.valueOf(row.get("skill_type", String.class)))
                                .id(skillId)
                                .build();
                        ideaMarketDTO.getStack().add(skill);
                    }
                    map.put(id,idea);
                    return idea;
                })
                .all().thenMany(Flux.fromIterable(map.values()));
    }

    ///////////////////////
    //  _____   ____ ______
    // / ___/  / __//_  __/
    /// (_ /  / _/   / /
    //\___/  /___/  /_/
    ///////////////////////

    public Flux<IdeaMarketDTO> getAllMarketIdeas(String userId){
        String QUERY = "SELECT idea_market.*, " +
                "users.id u_id, users.first_name u_fn, users.last_name u_ln, users.email u_e, " +
                "skill.id skill_id, skill.name skill_name, skill.type skill_type, " +
                "favorite_idea.*, ROW_NUMBER () OVER (ORDER BY idea_market.requests DESC) " +
                "FROM idea_market " +
                "LEFT JOIN favorite_idea ON favorite_idea.user_id = :userId " +
                "LEFT JOIN idea_skill ON idea_skill.idea_id = idea_market.idea_id " +
                "LEFT JOIN skill ON skill.id = idea_skill.skill_id " +
                "LEFT JOIN users ON users.id = idea_market.initiator_id " +
                "ORDER BY idea_market.id";
        return getListMarketIdea(QUERY, userId);
    }

    public Flux<IdeaMarketDTO> getAllInitiatorMarketIdeas(String userId){
        String QUERY = "SELECT idea_market.*, " +
                "users.id u_id, users.first_name u_fn, users.last_name u_ln, users.email u_e, " +
                "skill.id skill_id, skill.name skill_name, skill.type skill_type, " +
                "favorite_idea.*, ROW_NUMBER () OVER (ORDER BY idea_market.requests DESC) " +
                "FROM idea_market " +
                "LEFT JOIN favorite_idea ON favorite_idea.user_id = :userId " +
                "LEFT JOIN idea_skill ON idea_skill.idea_id = idea_market.idea_id " +
                "LEFT JOIN skill ON skill.id = idea_skill.skill_id " +
                "LEFT JOIN users ON users.id = idea_market.initiator_id " +
                "WHERE idea_market.initiator_id = :userId " +
                "ORDER BY idea_market.id";
        return getListMarketIdea(QUERY, userId);
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
                .map((row, rowMetadata) -> row.get("id",String.class))
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
                            "WHERE team_market_request.id = :id";
                    TeamMarketMapper teamMarketMapper = new TeamMarketMapper();
                    return template.getDatabaseClient()
                            .sql(QUERY)
                            .bind("id",id)
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
                .flatMap(ideaDTO -> {
                    IdeaMarket ideaMarket = IdeaMarket.builder()
                            .ideaId(ideaDTO.getId())
                            .name(ideaDTO.getName())
                            .description(ideaDTO.getDescription())
                            .problem(ideaDTO.getProblem())
                            .result(ideaDTO.getResult())
                            .solution(ideaDTO.getSolution())
                            .customer(ideaDTO.getCustomer())
                            .createdAt(LocalDate.from(ideaDTO.getCreatedAt()))
                            .maxTeamSize(ideaDTO.getMaxTeamSize())
                            .status(IdeaMarketStatusType.RECRUITMENT_IS_OPEN)
                            .requests(0L)
                            .acceptedRequests(0L)
                            .startDate(ideaDTO.getStartDate())
                            .finishDate(ideaDTO.getFinishDate())
                            .build();
                    return template.selectOne(query(where("email").is(ideaDTO.getInitiatorEmail())), User.class)
                            .flatMap(u -> {
                                ideaMarket.setInitiatorId(u.getId());
                                return Mono.empty();
                            })
                            .then(template.insert(ideaMarket)
                                    .map(i -> mapper.map(i, IdeaMarketDTO.class)));
                        }
                );
    }

    public Mono<TeamMarketRequestDTO> declareTeam(TeamMarketRequestDTO teamMarketRequestDTO){
        teamMarketRequestDTO.setCreatedAt(LocalDate.now());
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

    public Mono<Void> makeMarketIdeaFavorite(String userId, String ideaMarketId){
        return template.insert(new Favorite2Idea(userId,ideaMarketId)).then();
    }

    public Mono<Void> acceptTeam(String teamMarketId, Boolean status){
        return template.update(query(where("id").is(teamMarketId)),
                update("accepted", status),
                TeamMarketRequest.class).then();
    }
}
