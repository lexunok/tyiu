package com.tyiu.corn.service;

import com.tyiu.corn.model.dto.*;
import com.tyiu.corn.model.entities.*;
import com.tyiu.corn.model.entities.mappers.IdeaMarketMapper;
import com.tyiu.corn.model.entities.relations.Favorite2Idea;
import com.tyiu.corn.model.enums.IdeaMarketStatusType;
import com.tyiu.corn.model.enums.RequestStatus;
import com.tyiu.corn.model.enums.SkillType;
import com.tyiu.corn.model.requests.IdeaMarketRequest;
import io.r2dbc.spi.Row;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.springframework.data.r2dbc.core.R2dbcEntityTemplate;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;

import static org.springframework.data.relational.core.query.Criteria.where;
import static org.springframework.data.relational.core.query.Query.query;
import static org.springframework.data.relational.core.query.Update.update;

@Service
@RequiredArgsConstructor
@Slf4j
public class IdeaMarketService {

    private final R2dbcEntityTemplate template;
    private final ModelMapper mapper;

    private Mono<IdeaMarketDTO> getOneMarketIdea(String userId, String ideaMarketId){
        String QUERY = "SELECT im.*, " +
                "u.id AS u_id, u.first_name AS u_fn, u.last_name AS u_ln, u.email AS u_e, " +
                "l.id AS l_id, l.first_name AS l_fn, l.last_name AS l_ln, l.email AS l_e, " +
                "si.id AS si_id, si.name AS si_name, si.type AS si_type, " +
                "st.id AS st_id, st.name AS st_name, st.type AS st_type, " +
                "fi.*, " +
                "t.id AS t_id, t.name AS t_name, " +
                "(SELECT COUNT(*) FROM team_market_request WHERE idea_market_id = im.id) AS request_count, " +
                "(SELECT COUNT(*) FROM team_market_request WHERE idea_market_id = im.id AND status = 'ACCEPTED') AS accepted_request_count, " +
                "(SELECT COUNT(*) FROM team_member WHERE team_id = t.id) AS member_count " +
                "FROM idea_market im " +
                "LEFT JOIN favorite_idea fi ON fi.user_id = :userId AND fi.idea_market_id = im.id " +
                "LEFT JOIN team t ON t.id = im.team_id " +
                "LEFT JOIN idea_skill ids ON ids.idea_id = im.idea_id " +
                "LEFT JOIN team_skill ts ON ts.team_id = im.team_id " +
                "LEFT JOIN skill si ON si.id = ids.skill_id " +
                "LEFT JOIN skill st ON st.id = ts.skill_id " +
                "LEFT JOIN users u ON u.id = im.initiator_id " +
                "LEFT JOIN users l ON l.id = t.leader_id " +
                "WHERE im.id = :ideaMarketId";
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

    private IdeaMarketDTO buildIdeaMarket(Row row, ConcurrentHashMap<String, IdeaMarketDTO> map){
        String ideaMarketId = row.get("id", String.class);
        String skillId = row.get("s_id", String.class);
        IdeaMarketDTO ideaMarketDTO = map.getOrDefault(ideaMarketId, IdeaMarketDTO.builder()
                .id(ideaMarketId)
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
                .requests(row.get("request_count", Integer.class))
                .acceptedRequests(row.get("accepted_request_count", Integer.class))
                .startDate(row.get("start_date", LocalDate.class))
                .finishDate(row.get("finish_date", LocalDate.class))
                .isFavorite(Objects.equals(row.get("idea_market_id", String.class), ideaMarketId))
                .build());
        if (skillId != null){
            SkillDTO skill = SkillDTO.builder()
                    .name(row.get("s_name", String.class))
                    .type(SkillType.valueOf(row.get("s_type", String.class)))
                    .id(skillId)
                    .build();
            ideaMarketDTO.getStack().add(skill);
        }
        map.put(ideaMarketId, ideaMarketDTO);
        return ideaMarketDTO;
    }

    @Scheduled(cron = "0 0 0 * * *", zone = "Asia/Yekaterinburg")
    public void checkFinalDate(){
        template.update(query(where("finish_date").is(LocalDate.now())),
                update("status", IdeaMarketStatusType.RECRUITMENT_IS_CLOSED),
                IdeaMarket.class).subscribe();
    }

    ///////////////////////
    //  _____   ____ ______
    // / ___/  / __//_  __/
    /// (_ /  / _/   / /
    //\___/  /___/  /_/
    ///////////////////////

    public Flux<IdeaMarketDTO> getAllMarketIdeas(String userId){
        String QUERY = "SELECT im.*, u.id AS u_id, u.first_name AS u_fn, u.last_name AS u_ln, u.email AS u_e, " +
                "fi.*, " +
                "s.id AS s_id, s.name AS s_name, s.type AS s_type, " +
                "(SELECT COUNT(*) FROM team_market_request WHERE idea_market_id = im.id) AS request_count, " +
                "(SELECT COUNT(*) FROM team_market_request WHERE idea_market_id = im.id AND status = 'ACCEPTED') AS accepted_request_count, " +
                "ROW_NUMBER () OVER (ORDER BY (SELECT COUNT(*) FROM team_market_request WHERE idea_market_id = im.id) DESC) AS row_number " +
                "FROM idea_market im " +
                "LEFT JOIN favorite_idea fi ON fi.user_id = :userId " +
                "LEFT JOIN users u ON u.id = im.initiator_id " +
                "LEFT JOIN idea_skill ids ON ids.idea_id = im.idea_id " +
                "LEFT JOIN skill s ON s.id = ids.skill_id " +
                "ORDER BY im.id";
        ConcurrentHashMap<String, IdeaMarketDTO> map = new ConcurrentHashMap<>();
        return template.getDatabaseClient()
                .sql(QUERY)
                .bind("userId",userId)
                .map((row, rowMetadata) -> buildIdeaMarket(row, map))
                .all().thenMany(Flux.fromIterable(map.values()));
    }

    public Flux<IdeaMarketDTO> getAllInitiatorMarketIdeas(String userId){
        String QUERY = "SELECT im.*, u.id AS u_id, u.first_name AS u_fn, u.last_name AS u_ln, u.email AS u_e, " +
                "fi.*, " +
                "s.id AS s_id, s.name AS s_name, s.type AS s_type, " +
                "(SELECT COUNT(*) FROM team_market_request WHERE idea_market_id = im.id) AS request_count, " +
                "(SELECT COUNT(*) FROM team_market_request WHERE idea_market_id = im.id AND status = 'ACCEPTED') AS accepted_request_count, " +
                "ROW_NUMBER () OVER (ORDER BY (SELECT COUNT(*) FROM team_market_request WHERE idea_market_id = im.id) DESC) AS row_number " +
                "FROM idea_market im " +
                "LEFT JOIN favorite_idea fi ON fi.user_id = :userId " +
                "LEFT JOIN users u ON u.id = im.initiator_id " +
                "LEFT JOIN idea_skill ids ON ids.idea_id = im.idea_id " +
                "LEFT JOIN skill s ON s.id = ids.skill_id " +
                "WHERE im.initiator_id = :userId " +
                "ORDER BY im.id";
        ConcurrentHashMap<String, IdeaMarketDTO> map = new ConcurrentHashMap<>();
        return template.getDatabaseClient()
                .sql(QUERY)
                .bind("userId",userId)
                .map((row, rowMetadata) -> buildIdeaMarket(row, map))
                .all().thenMany(Flux.fromIterable(map.values()));
    }

    public Mono<IdeaMarketDTO> getMarketIdea(String userId, String ideaMarketId){
        return getOneMarketIdea(userId, ideaMarketId);
    }

    public Flux<IdeaMarketDTO> getAllFavoriteMarketIdeas(String userId){
        return template.select(query(where("user_id").is(userId)), Favorite2Idea.class)
                .flatMap(id -> getOneMarketIdea(userId, id.getIdeaMarketId()));
    }

    public Flux<TeamMarketRequestDTO> getAllTeamsRequests(String ideaId){
        String QUERY = "SELECT tmr.*, " +
                "s.id AS s_id, s.name AS s_name, s.type AS s_type, " +
                "(SELECT COUNT(*) FROM team_member WHERE team_id = tmr.team_id) AS member_count " +
                "FROM team_market_request tmr " +
                "LEFT JOIN team_skill ts ON tmr.team_id = ts.team_id " +
                "LEFT JOIN skill s ON s.id = ts.skill_id " +
                "WHERE tmr.idea_market_id = :ideaId";
        ConcurrentHashMap<String, TeamMarketRequestDTO> map = new ConcurrentHashMap<>();
        return template.getDatabaseClient()
                .sql(QUERY)
                .bind("ideaId",ideaId)
                .map((row, rowMetadata) -> {
                    String teamRequestId = row.get("id", String.class);
                    String skillId = row.get("s_id", String.class);
                    TeamMarketRequestDTO teamMarketRequestDTO = map.getOrDefault(teamRequestId, TeamMarketRequestDTO.builder()
                            .id(teamRequestId)
                            .ideaMarketId(row.get("idea_market_id", String.class))
                            .teamId(row.get("team_id", String.class))
                            .name(row.get("name", String.class))
                            .letter(row.get("letter", String.class))
                            .status(RequestStatus.valueOf(row.get("status", String.class)))
                            .membersCount(row.get("member_count", Integer.class))
                            .skills(new ArrayList<>())
                            .build());
                    if (skillId != null){
                        SkillDTO skill = SkillDTO.builder()
                                .name(row.get("s_name", String.class))
                                .type(SkillType.valueOf(row.get("s_type", String.class)))
                                .id(skillId)
                                .build();
                        teamMarketRequestDTO.getSkills().add(skill);
                    }
                    map.put(teamRequestId, teamMarketRequestDTO);
                    return teamMarketRequestDTO;
                })
                .all().thenMany(Flux.fromIterable(map.values()));
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
                                    .startDate(ideaDTO.getStartDate())
                                    .finishDate(ideaDTO.getFinishDate())
                                    .build();
                            log.info(ideaMarket.toString());
                            return template.selectOne(query(where("email").is(ideaDTO.getInitiatorEmail())), User.class)
                                    .flatMap(u -> {
                                        log.info(u.getId());
                                        ideaMarket.setInitiatorId(u.getId());
                                        return Mono.empty();
                                    })
                                    .then(template.update(query(where("id").is(ideaDTO.getId())),
                                            update("status", Idea.Status.ON_MARKET),
                                            Idea.class)).log()
                                    .then(template.insert(ideaMarket)
                                            .map(i -> mapper.map(i, IdeaMarketDTO.class))).log();
                        }
                );
    }

    public Mono<TeamMarketRequestDTO> declareTeam(TeamMarketRequestDTO teamMarketRequestDTO){
        teamMarketRequestDTO.setStatus(RequestStatus.NEW);
        TeamMarketRequest teamMarketRequest = mapper.map(teamMarketRequestDTO, TeamMarketRequest.class);
        return template.insert(teamMarketRequest).flatMap(r -> {
            teamMarketRequestDTO.setId(r.getId());
            return Mono.just(teamMarketRequestDTO);
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

    public Mono<Void> changeIdeaMarketStatus(String ideaMarketId, IdeaMarketStatusType statusType){
        return template.update(query(where("id").is(ideaMarketId)),
                update("status", statusType),
                IdeaMarket.class).then();
    }

    public Mono<Void> changeRequestStatus(String teamMarketId, RequestStatus status){
        return template.update(query(where("id").is(teamMarketId)),
                update("status", status),
                TeamMarketRequest.class).then();
    }

    public Mono<TeamDTO> setAcceptedTeam(String ideaMarketId, String teamId){
        String QUERY = "SELECT " +
                "t.id as team_id, t.name as team_name, " +
                "l.id as leader_id, l.email as leader_email, l.first_name as leader_first_name, l.last_name as leader_last_name, " +
                "s.id as skill_id, s.name as skill_name, s.type as skill_type, " +
                "(SELECT COUNT(*) FROM team_member WHERE team_id = t.id) as member_count " +
                "FROM team t " +
                "LEFT JOIN users l ON t.leader_id = l.id " +
                "LEFT JOIN team_skill ts ON ts.team_id = t.id " +
                "LEFT JOIN skill s ON ts.skill_id = s.id " +
                "WHERE t.id = :teamId";
        ConcurrentHashMap<String, TeamDTO> map = new ConcurrentHashMap<>();
        return template.update(query(where("id").is(ideaMarketId)),
                        update("team_id", teamId),
                        IdeaMarket.class)
                .then(template.update(query(where("id").is(teamId)),
                        update("has_active_project", true),
                        Team.class))
                .then(template.getDatabaseClient()
                .sql(QUERY)
                .bind("teamId", teamId)
                .map((row, rowMetadata) -> {
                    String skillId = row.get("skill_id", String.class);
                    TeamDTO teamDTO = map.getOrDefault(teamId, TeamDTO.builder()
                            .id(teamId)
                            .name(row.get("team_name", String.class))
                            .leader(UserDTO.builder()
                                    .id(row.get("leader_id", String.class))
                                    .email(row.get("leader_email", String.class))
                                    .firstName(row.get("leader_first_name", String.class))
                                    .lastName(row.get("leader_last_name", String.class))
                                    .build())
                            .skills(new ArrayList<>())
                            .build());
                    if (skillId!=null) {
                        SkillDTO skill = SkillDTO.builder()
                                .name(row.get("skill_name", String.class))
                                .type(SkillType.valueOf(row.get("skill_type", String.class)))
                                .id(skillId)
                                .build();
                        teamDTO.getSkills().add(skill);
                    }
                    map.put(teamId,teamDTO);
                    return teamDTO;
                })
                .all().thenMany(Flux.fromIterable(map.values()))
                .collectList().map(i -> i.get(0)));
    }
    public Mono<Void> resetAcceptedTeam(String ideaMarketId){
        return template.selectOne(query(where("id").is(ideaMarketId)), IdeaMarket.class)
                .flatMap(i -> {
                    String teamId = i.getTeamId();
                    i.setTeamId(null);
                    return template.update(query(where("id").is(teamId)),
                            update("has_active_project", false),
                            Team.class)
                            .then(template.update(i));
                }).then();
    }
}