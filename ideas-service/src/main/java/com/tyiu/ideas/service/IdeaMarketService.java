package com.tyiu.ideas.service;

import com.tyiu.ideas.config.exception.AccessException;
import com.tyiu.ideas.model.dto.*;
import com.tyiu.ideas.model.entities.*;
import com.tyiu.ideas.model.entities.mappers.IdeaMarketMapper;
import com.tyiu.ideas.model.entities.relations.Favorite2Idea;
import com.tyiu.ideas.model.entities.relations.IdeaMarket2Refused;
import com.tyiu.ideas.model.enums.IdeaMarketStatusType;
import com.tyiu.ideas.model.enums.RequestStatus;
import com.tyiu.ideas.model.enums.Role;
import com.tyiu.ideas.model.enums.SkillType;
import io.r2dbc.spi.Row;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.springframework.cache.annotation.CacheConfig;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.data.r2dbc.core.R2dbcEntityTemplate;
import org.springframework.security.oauth2.jwt.Jwt;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;

import static org.springframework.data.relational.core.query.Criteria.where;
import static org.springframework.data.relational.core.query.Query.query;
import static org.springframework.data.relational.core.query.Update.update;

@Slf4j
@Service
@RequiredArgsConstructor
@CacheConfig(cacheNames = "idea_markets")
public class IdeaMarketService {

    private final R2dbcEntityTemplate template;
    private final ModelMapper mapper;

    private Mono<IdeaMarketDTO> getOneMarketIdea(String userId, String ideaMarketId){
        String QUERY = "SELECT im.*, " +
                "u.id AS u_id, u.first_name AS u_fn, u.last_name AS u_ln, u.email AS u_e, " +
                "o.id AS o_id, o.first_name AS o_fn, o.last_name AS o_ln, o.email AS o_e, " +
                "l.id AS l_id, l.first_name AS l_fn, l.last_name AS l_ln, l.email AS l_e, " +
                "si.id AS si_id, si.name AS si_name, si.type AS si_type, " +
                "st.id AS st_id, st.name AS st_name, st.type AS st_type, " +
                "i.name, i.problem, i.description, i.solution, i.result, i.max_team_size, i.customer, " +
                "fi.*, " +
                "t.id AS t_id, t.name AS t_name, " +
                "(SELECT COUNT(*) FROM team_market_request WHERE idea_market_id = im.id AND status = 'ACCEPTED') AS accepted_request_count, " +
                "(SELECT COUNT(*) FROM team_market_request WHERE idea_market_id = im.id) AS request_count, " +
                "(SELECT COUNT(*) FROM team_member WHERE team_id = t.id AND finish_date IS NULL) AS member_count " +
                "FROM idea_market im " +
                "LEFT JOIN favorite_idea fi ON fi.user_id = :userId AND fi.idea_market_id = im.id " +
                "LEFT JOIN idea i ON i.id = im.idea_id " +
                "LEFT JOIN team t ON t.id = im.team_id " +
                "LEFT JOIN idea_skill ids ON ids.idea_id = im.idea_id " +
                "LEFT JOIN team_member tm ON tm.team_id = t.id AND tm.finish_date IS NULL " +
                "LEFT JOIN user_skill us ON us.user_id = tm.member_id " +
                "LEFT JOIN skill si ON si.id = ids.skill_id " +
                "LEFT JOIN skill st ON st.id = us.skill_id " +
                "LEFT JOIN users u ON u.id = i.initiator_id " +
                "LEFT JOIN users o ON o.id = t.owner_id " +
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

    private Flux<IdeaMarketDTO> getListMarketIdea(String QUERY, String userId, String marketId){
        ConcurrentHashMap<String, IdeaMarketDTO> map = new ConcurrentHashMap<>();
        return template.getDatabaseClient()
                .sql(QUERY)
                .bind("userId", userId)
                .bind("marketId", marketId)
                .map((row, rowMetadata) -> buildIdeaMarket(row, map))
                .all()
                .thenMany(Flux.fromIterable(map.values()).sort(Comparator.comparing(IdeaMarketDTO::getPosition)));
    }

    private IdeaMarketDTO buildIdeaMarket(Row row, ConcurrentHashMap<String, IdeaMarketDTO> map){
        String ideaMarketId = row.get("id", String.class);
        String skillId = row.get("s_id", String.class);
        IdeaMarketDTO ideaMarketDTO = map.get(ideaMarketId);
        if (ideaMarketDTO == null) {
            ideaMarketDTO = map.getOrDefault(ideaMarketId, IdeaMarketDTO.builder()
                    .id(ideaMarketId)
                    .ideaId(row.get("idea_id", String.class))
                    .position(row.get("row_number", Integer.class))
                    .name(row.get("name", String.class))
                    .initiator(UserDTO.builder()
                            .id(row.get("u_id", String.class))
                            .email(row.get("u_e", String.class))
                            .firstName(row.get("u_fn", String.class))
                            .lastName(row.get("u_ln", String.class))
                            .build())
                    .solution(row.get("solution", String.class))
                    .stack(new ArrayList<>())
                    .maxTeamSize(row.get("max_team_size", Short.class))
                    .status(IdeaMarketStatusType.valueOf(row.get("status", String.class)))
                    .requests(row.get("request_count", Integer.class))
                    .acceptedRequests(row.get("accepted_request_count", Integer.class))
                    .isFavorite(Objects.equals(row.get("favorite", String.class), ideaMarketId))
                    .build());
        }
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

    private Mono<Boolean> checkInitiator(String ideaMarketId, String userId){
        String QUERY = """
                        SELECT EXISTS (
                            SELECT 1 
                            FROM idea_market im 
                            LEFT JOIN idea i ON i.id = im.idea_id 
                            WHERE im.id = :ideaMarketId AND i.initiator_id = :userId
                        )
                        """;
        return template.getDatabaseClient()
                .sql(QUERY)
                .bind("userId", userId)
                .bind("ideaMarketId", ideaMarketId)
                .map((row, rowMetadata) -> row.get("exists", Boolean.class))
                .first();
    }

    private Mono<Boolean> checkOwner(String teamId, String userId){
        return template.exists(query(where("id").is(teamId)
                .and("owner_id").is(userId)), Team.class);
    }

    private final String getAllMarketQUERYMain = """
                SELECT
                    im_sub.*, u.id AS u_id, u.first_name AS u_fn, u.last_name AS u_ln, u.email AS u_e,
                    fi.idea_market_id AS favorite,
                    s.id AS s_id, s.name AS s_name, s.type AS s_type,
                    i.name, i.solution, i.max_team_size
                FROM (
                    SELECT
                        im.*,
                        (SELECT COUNT(*) FROM team_market_request WHERE idea_market_id = im.id) AS request_count,
                        (SELECT COUNT(*) FROM team_market_request WHERE idea_market_id = im.id AND status = 'ACCEPTED') AS accepted_request_count,
                        ROW_NUMBER() OVER (ORDER BY (SELECT COUNT(*) FROM team_market_request WHERE idea_market_id = im.id) DESC) AS row_number
                    FROM idea_market im
                        INNER JOIN market m ON m.id = im.market_id
                """;

    private final String getAllMarketQUERYLast = """
                ) AS im_sub
                    LEFT JOIN favorite_idea fi ON fi.user_id = :userId AND fi.idea_market_id = im_sub.id
                    LEFT JOIN idea i ON i.id = im_sub.idea_id
                    LEFT JOIN users u ON u.id = i.initiator_id
                    LEFT JOIN idea_skill ids ON ids.idea_id = im_sub.idea_id
                    LEFT JOIN skill s ON s.id = ids.skill_id
                """;

    ///////////////////////
    //  _____   ____ ______
    // / ___/  / __//_  __/
    /// (_ /  / _/   / /
    //\___/  /___/  /_/
    ///////////////////////

    @Cacheable
    public Flux<IdeaMarketDTO> getAllMarketIdeas(String userId){
        String QUERY = getAllMarketQUERYMain + """
                WHERE m.status = 'ACTIVE'
                """ + getAllMarketQUERYLast;
        ConcurrentHashMap<String, IdeaMarketDTO> map = new ConcurrentHashMap<>();
        return template.getDatabaseClient()
                .sql(QUERY)
                .bind("userId",userId)
                .map((row, rowMetadata) -> buildIdeaMarket(row, map))
                .all()
                .thenMany(Flux.fromIterable(map.values()).sort(Comparator.comparing(IdeaMarketDTO::getPosition)));
    }

    @Cacheable
    public Flux<IdeaMarketDTO> getAllMarketIdeasForMarket(String userId, String marketId){
        String QUERY = getAllMarketQUERYMain + """
                WHERE im.market_id = :marketId
                """ + getAllMarketQUERYLast;
        return getListMarketIdea(QUERY, userId, marketId);
    }

    @Cacheable
    public Flux<IdeaMarketDTO> getAllInitiatorMarketIdeas(String userId, String marketId){
        String QUERY = getAllMarketQUERYMain + """
                    LEFT JOIN idea i ON i.id = im.idea_id
                WHERE i.initiator_id = :userId AND im.market_id = :marketId
                """ + getAllMarketQUERYLast;
        return getListMarketIdea(QUERY, userId, marketId);
    }

    @Cacheable
    public Mono<IdeaMarketDTO> getMarketIdea(String userId, String ideaMarketId){
        return getOneMarketIdea(userId, ideaMarketId);
    }

    @Cacheable
    public Flux<IdeaMarketDTO> getAllFavoriteMarketIdeas(String userId, String marketId){
        String QUERY = getAllMarketQUERYMain + """
                WHERE im.market_id = :marketId
                """ + getAllMarketQUERYLast + """
                WHERE fi.idea_market_id = im_sub.id
                """;
        return getListMarketIdea(QUERY, userId, marketId);
    }

    @Cacheable
    public Flux<TeamMarketRequestDTO> getAllTeamsRequests(String ideaId){
        String QUERY = "SELECT tmr.*, " +
                "s.id AS s_id, s.name AS s_name, s.type AS s_type, " +
                "(SELECT COUNT(*) FROM team_member WHERE team_id = tmr.team_id AND finish_date IS NULL) AS member_count " +
                "FROM team_market_request tmr " +
                "LEFT JOIN team_member tm ON tm.team_id = tmr.team_id AND tm.finish_date IS NULL " +
                "LEFT JOIN user_skill us ON us.user_id = tm.member_id " +
                "LEFT JOIN skill s ON s.id = us.skill_id " +
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
                            .marketId(row.get("market_id", String.class))
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

    @Cacheable
    public Flux<IdeaMarketAdvertisementDTO> getIdeaMarketAdvertisement(String ideaMarketId){
        String QUERY = "SELECT ima.*, " +
                "u.id AS u_id, u.email AS u_email, u.first_name AS u_first_name, u.last_name AS u_last_name " +
                "FROM idea_market_advertisement ima " +
                "LEFT JOIN users u ON u.id = ima.sender_id " +
                "WHERE ima.idea_market_id = :ideaMarketId";

        return template.getDatabaseClient()
                .sql(QUERY)
                .bind("ideaMarketId", ideaMarketId)
                .map((row, rowMetadata) -> IdeaMarketAdvertisementDTO.builder()
                        .id(row.get("id", String.class))
                        .ideaMarketId(row.get("idea_market_id", String.class))
                        .createdAt(row.get("created_at", LocalDateTime.class))
                        .text(row.get("text", String.class))
                        .sender(UserDTO.builder()
                                .id(row.get("u_id", String.class))
                                .email(row.get("u_email", String.class))
                                .firstName(row.get("u_first_name", String.class))
                                .lastName(row.get("u_last_name", String.class))
                                .build())
                        .checkedBy(List.of(Objects.requireNonNull(row.get("checked_by", String[].class))))
                        .build()).all().sort(Comparator.comparing(IdeaMarketAdvertisementDTO::getCreatedAt));
    }

    //////////////////////////////
    //   ___   ____    ____ ______
    //  / _ \ / __ \  / __//_  __/
    // / ___// /_/ / _\ \   / /
    ///_/    \____/ /___/  /_/
    //////////////////////////////

    @CacheEvict(allEntries = true)
    public Flux<IdeaMarketDTO> sendIdeaOnMarket(String marketId, Flux<IdeaDTO> ideaDTOList) {
        return ideaDTOList.flatMap(ideaDTO -> {
            IdeaMarketDTO ideaMarketDTO = IdeaMarketDTO.builder()
                    .ideaId(ideaDTO.getId())
                    .initiator(ideaDTO.getInitiator())
                    .marketId(marketId)
                    .name(ideaDTO.getName())
                    .description(ideaDTO.getDescription())
                    .problem(ideaDTO.getProblem())
                    .result(ideaDTO.getResult())
                    .solution(ideaDTO.getSolution())
                    .maxTeamSize(ideaDTO.getMaxTeamSize())
                    .customer(ideaDTO.getCustomer())
                    .status(IdeaMarketStatusType.RECRUITMENT_IS_OPEN)
                    .build();
            return template.update(query(where("id").is(ideaDTO.getId())),
                            update("status", Idea.Status.ON_MARKET)
                                    .set("is_active", true),
                            Idea.class)
                    .then(template.insert(mapper.map(ideaMarketDTO, IdeaMarket.class))
                            .flatMap(i -> {
                                ideaMarketDTO.setId(i.getId());
                                return Mono.just(ideaMarketDTO);
                            }));
        });
    }

    @CacheEvict(allEntries = true)
    public Mono<TeamMarketRequestDTO> declareTeam(TeamMarketRequestDTO teamMarketRequestDTO, String userId){
        return checkOwner(teamMarketRequestDTO.getTeamId(), userId)
                .flatMap(isExists -> {
                    if (Boolean.TRUE.equals(isExists)){
                        teamMarketRequestDTO.setStatus(RequestStatus.NEW);
                        TeamMarketRequest teamMarketRequest = mapper.map(teamMarketRequestDTO, TeamMarketRequest.class);
                        return template.insert(teamMarketRequest).flatMap(r -> {
                            teamMarketRequestDTO.setId(r.getId());
                            return Mono.just(teamMarketRequestDTO);
                        });
                    }
                    return Mono.error(new AccessException("Нет Прав"));
                });
    }

    @CacheEvict(allEntries = true)
    public Mono<IdeaMarketAdvertisementDTO> addAdvertisement(IdeaMarketAdvertisementDTO advertisementDTO, Jwt jwt){
        return checkInitiator(advertisementDTO.getIdeaMarketId(), jwt.getId())
                .flatMap(isExists -> {
                    if (Boolean.TRUE.equals(isExists)){
                        advertisementDTO.setCreatedAt(LocalDateTime.now());
                        advertisementDTO.setCheckedBy(List.of(jwt.getClaimAsString("sub")));
                        return template.insert(IdeaMarketAdvertisement.builder()
                                        .ideaMarketId(advertisementDTO.getIdeaMarketId())
                                        .checkedBy(advertisementDTO.getCheckedBy())
                                        .createdAt(advertisementDTO.getCreatedAt())
                                        .text(advertisementDTO.getText())
                                        .senderId(jwt.getId())
                                        .build())
                                .flatMap(a -> {
                                    advertisementDTO.setId(a.getId());
                                    return template.selectOne(query(where("id").is(jwt.getId())), User.class)
                                                    .flatMap(u -> {
                                                        advertisementDTO.setSender(UserDTO.builder()
                                                                .id(u.getId())
                                                                .email(u.getEmail())
                                                                .firstName(u.getFirstName())
                                                                .lastName(u.getLastName())
                                                                .build());
                                                        return Mono.just(advertisementDTO);
                                                    });
                                });
                    }
                    return Mono.error(new AccessException("Нет Прав"));
                });
    }

    ///////////////////////////////////////////
    //   ___    ____   __    ____ ______   ____
    //  / _ \  / __/  / /   / __//_  __/  / __/
    // / // / / _/   / /__ / _/   / /    / _/
    ///____/ /___/  /____//___/  /_/    /___/
    ///////////////////////////////////////////

    @CacheEvict(allEntries = true)
    public Mono<Void> deleteMarketIdea(String ideaMarketId, Jwt jwt){
        return checkInitiator(ideaMarketId,jwt.getId())
                .flatMap(isExists -> {
                    if (Boolean.TRUE.equals(isExists) || jwt.getClaimAsStringList("roles").contains(String.valueOf(Role.ADMIN))){
                        return template.delete(query(where("id").is(ideaMarketId)), IdeaMarket.class);
                    }
                    return Mono.error(new AccessException("Нет Прав"));
                }).then();
    }

    @CacheEvict(allEntries = true)
    public Mono<Void> deleteMarketIdeaFromFavorite(String userId, String ideaMarketId){
        return template.delete(query(where("user_id").is(userId)
                .and("idea_market_id").is(ideaMarketId)), Favorite2Idea.class).then();
    }

    @CacheEvict(allEntries = true)
    public Mono<Void> deleteIdeaMarketAdvertisement(String ideaMarketAdvertisementId, Jwt jwt){
        return template.exists(query(where("id").is(ideaMarketAdvertisementId)
                        .and("sender_id").is(jwt.getId())), IdeaMarketAdvertisement.class)
                .flatMap(isExists -> {
                    if (Boolean.TRUE.equals(isExists) || jwt.getClaimAsStringList("roles").contains(String.valueOf(Role.ADMIN))){
                        return template.delete(query(where("id").is(ideaMarketAdvertisementId)), IdeaMarketAdvertisement.class);
                    }
                    return Mono.error(new AccessException("Нет Прав"));
                }).then();
    }

    ////////////////////////
    //   ___   __  __ ______
    //  / _ \ / / / //_  __/
    // / ___// /_/ /  / /
    ///_/    \____/  /_/
    ////////////////////////

    @CacheEvict(allEntries = true)
    public Mono<Void> makeMarketIdeaFavorite(String userId, String ideaMarketId){
        return template.insert(new Favorite2Idea(userId,ideaMarketId)).then();
    }

    @CacheEvict(allEntries = true)
    public Mono<Void> changeIdeaMarketStatus(String ideaMarketId, IdeaMarketStatusType statusType, Jwt jwt){
        return checkInitiator(ideaMarketId,jwt.getId())
                .flatMap(isExists -> {
                    if (Boolean.TRUE.equals(isExists) || jwt.getClaimAsStringList("roles").contains(String.valueOf(Role.ADMIN))) {
                        return template.update(query(where("id").is(ideaMarketId)),
                                update("status", statusType),
                                IdeaMarket.class).then();
                    }
                    return Mono.error(new AccessException("Нет Прав"));
                });
    }

    @CacheEvict(allEntries = true)
    public Mono<Void> changeRequestStatus(String teamMarketId, RequestStatus status, Jwt jwt){
        return template.selectOne(query(where("id").is(teamMarketId)), TeamMarketRequest.class)
                .flatMap(r -> {
                    r.setStatus(status);
                    if (status.equals(RequestStatus.CANCELED)){
                        return checkInitiator(r.getIdeaMarketId(),jwt.getId())
                                .flatMap(isExists -> {
                                    if (Boolean.TRUE.equals(isExists) || jwt.getClaimAsStringList("roles").contains(String.valueOf(Role.ADMIN))){
                                        return template.insert(new IdeaMarket2Refused(r.getIdeaMarketId(), r.getTeamId()))
                                                .then(template.update(r))
                                                .then();
                                    }
                                    return Mono.error(new AccessException("Нет Прав"));
                                });
                    }
                    else if (status.equals(RequestStatus.ACCEPTED)) {
                        return checkInitiator(r.getIdeaMarketId(),jwt.getId())
                                .flatMap(isExists -> {
                                    if (Boolean.TRUE.equals(isExists) || jwt.getClaimAsStringList("roles").contains(String.valueOf(Role.ADMIN))){
                                        return template.update(query(where("team_id").is(r.getTeamId())
                                                                .or("idea_market_id").is(r.getIdeaMarketId())
                                                                .and("status").is(RequestStatus.NEW)),
                                                        update("status", RequestStatus.ANNULLED),
                                                        TeamMarketRequest.class)
                                                .then(template.getDatabaseClient().sql("""
                                                        UPDATE idea_invitation SET status = :status 
                                                        WHERE idea_id = (SELECT idea_id FROM idea_market WHERE id = :ideaMarketId) OR team_id = :teamId
                                                        """)
                                                        .bind("status", "ANNULLED")
                                                        .bind("ideaMarketId", r.getIdeaMarketId())
                                                        .bind("teamId", r.getTeamId()).fetch().all().then())
                                                .then(template.update(r))
                                                .then();
                                    }
                                    return Mono.error(new AccessException("Нет Прав"));
                                });
                    }
                    else if (status.equals(RequestStatus.WITHDRAWN)){
                        return checkOwner(r.getTeamId(), jwt.getId())
                                .flatMap(isExists -> {
                                    if (Boolean.TRUE.equals(isExists) || jwt.getClaimAsStringList("roles").contains(String.valueOf(Role.ADMIN))){
                                        return template.update(r).then();
                                    }
                                    return Mono.error(new AccessException("Нет Прав"));
                                });
                    }
                    else if (jwt.getClaimAsStringList("roles").contains(String.valueOf(Role.ADMIN)) &&
                            (status.equals(RequestStatus.NEW) || status.equals(RequestStatus.ANNULLED)))
                    {
                        return template.update(r).then();
                    }
                    return Mono.error(new AccessException("Ошибка"));
                });
    }

    @CacheEvict(allEntries = true)
    public Mono<TeamDTO> setAcceptedTeam(String ideaMarketId, String teamId, Jwt jwt){
        return template.exists(query(where("team_id").is(teamId)), IdeaMarket.class)
                .flatMap(isExistsTeam -> {
                    if (Boolean.FALSE.equals(isExistsTeam)) {
                        String QUERY = "SELECT " +
                                "t.id as team_id, t.name as team_name, " +
                                "l.id as leader_id, l.email as leader_email, l.first_name as leader_first_name, l.last_name as leader_last_name, " +
                                "s.id as skill_id, s.name as skill_name, s.type as skill_type, " +
                                "(SELECT COUNT(*) FROM team_member WHERE team_id = t.id AND finish_date IS NULL) as member_count " +
                                "FROM team t " +
                                "LEFT JOIN users l ON t.leader_id = l.id " +
                                "LEFT JOIN team_member tm ON tm.team_id = t.id AND tm.finish_date IS NULL " +
                                "LEFT JOIN user_skill us ON us.user_id = tm.member_id " +
                                "LEFT JOIN skill s ON us.skill_id = s.id " +
                                "WHERE t.id = :teamId";
                        ConcurrentHashMap<String, TeamDTO> map = new ConcurrentHashMap<>();
                        return checkInitiator(ideaMarketId,jwt.getId())
                                .flatMap(isExists -> {
                                    if (Boolean.TRUE.equals(isExists) || jwt.getClaimAsStringList("roles").contains(String.valueOf(Role.ADMIN))){
                                        return template.update(query(where("id").is(ideaMarketId)),
                                                        update("team_id", teamId)
                                                                .set("status", IdeaMarketStatusType.RECRUITMENT_IS_CLOSED),
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
                                    return Mono.error(new AccessException("Нет Прав"));
                                });
                    }
                    return Mono.error(new AccessException("Данная команда уже занята"));
                });
    }

    @CacheEvict(allEntries = true)
    public Mono<Void> updateCheckByAdvertisement(String ideaMarketAdvertisementId, String email){
        return template.getDatabaseClient()
                .sql("UPDATE idea_market_advertisement SET checked_by = array_append(checked_by,:userEmail) WHERE id =:advertisementId")
                .bind("advertisementId", ideaMarketAdvertisementId)
                .bind("userEmail", email).then();
    }
}