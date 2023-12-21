package com.tyiu.corn.service;

import com.tyiu.corn.config.exception.AccessException;
import com.tyiu.corn.config.exception.NotFoundException;
import com.tyiu.corn.model.dto.GroupDTO;
import com.tyiu.corn.model.dto.IdeaDTO;
import com.tyiu.corn.model.dto.SkillDTO;
import com.tyiu.corn.model.entities.Idea;
import com.tyiu.corn.model.entities.User;
import com.tyiu.corn.model.entities.mappers.IdeaMapper;
import com.tyiu.corn.model.entities.relations.Group2User;
import com.tyiu.corn.model.entities.relations.Idea2Checked;
import com.tyiu.corn.model.entities.relations.Idea2Skill;
import com.tyiu.corn.model.enums.Role;
import com.tyiu.corn.model.enums.SkillType;
import com.tyiu.corn.model.requests.IdeaSkillRequest;
import com.tyiu.corn.model.requests.StatusIdeaRequest;
import io.r2dbc.spi.Batch;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.springframework.cache.annotation.CacheConfig;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.data.r2dbc.core.R2dbcEntityTemplate;
import org.springframework.stereotype.Service;

import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import java.time.LocalDateTime;

import static org.springframework.data.relational.core.query.Criteria.where;
import static org.springframework.data.relational.core.query.Query.query;
import static org.springframework.data.relational.core.query.Update.update;

@Service
@RequiredArgsConstructor
@CacheConfig(cacheNames = "ideas")
@Slf4j
public class IdeaService {

    private final R2dbcEntityTemplate template;
    private final IdeaMapper ideaMapper;
    private final ModelMapper mapper;

    @Cacheable
    public Mono<IdeaDTO> getIdea(String ideaId, String userId) {
        String query = """
                SELECT idea.*, e.name experts_name, e.id experts_id, p.name project_office_name, p.id project_office_id,
                ic.idea_id checked_idea
                FROM idea LEFT JOIN groups e ON idea.group_expert_id = e.id
                LEFT JOIN groups p ON idea.group_project_office_id = p.id
                LEFT JOIN idea_checked ic ON ic.user_id = :userId AND ic.idea_id = idea.id
                WHERE idea.id =:ideaId""";
        return template.getDatabaseClient()
                .sql(query)
                .bind("ideaId", ideaId)
                .bind("userId", userId)
                .map((row, rowMetadata) -> {
                    IdeaDTO idea = ideaMapper.apply(row,rowMetadata);
                    idea.setProjectOffice(GroupDTO.builder()
                                    .id(row.get("project_office_id", String.class))
                                    .name(row.get("project_office_name",String.class))
                            .build());
                    idea.setExperts(GroupDTO.builder()
                            .id(row.get("experts_id", String.class))
                            .name(row.get("experts_name",String.class))
                            .build());
                    return idea;
                })
                .first()
                .flatMap(i -> template.exists(query(where("user_id").is(userId)
                        .and("idea_id").is(i.getId())), Idea2Checked.class).flatMap(isExists -> {
                    if (Boolean.FALSE.equals(isExists)){
                        return template.insert(new Idea2Checked(userId, i.getId()));
                    }
                    return Mono.empty();
                }).thenReturn(i))
                .switchIfEmpty(Mono.error(new NotFoundException("Не найдена!")));
    }

    @Cacheable
    public Flux<IdeaDTO> getListIdea(String userId) {
        String query = """
                SELECT idea.*, ic.idea_id checked_idea
                FROM idea
                JOIN idea_checked ic ON ic.user_id = :userId AND ic.idea_id = idea.id""";
        return template.getDatabaseClient().sql(query)
                .bind("userId", userId)
                .map(ideaMapper::apply).all();
    }

    @Cacheable
    public Flux<IdeaDTO> getListIdeaByInitiator(User user) {
        String query = """
                SELECT idea.*, ic.idea_id checked_idea
                FROM idea
                JOIN idea_checked ic ON ic.user_id = :userId AND ic.idea_id = idea.id
                WHERE idea.initiator_email = :email""";
        return template.getDatabaseClient().sql(query)
                .bind("userId", user.getId())
                .bind("email", user.getEmail())
                .map(ideaMapper::apply).all();
    }

    public Flux<IdeaDTO> getListIdeaOnConfirmation(String userId) {
        String query = """
                SELECT r.idea_id, r.is_confirmed, idea.* 
                FROM rating r 
                JOIN idea ON idea.id = r.idea_id AND idea.status = 'ON_CONFIRMATION' 
                LEFT JOIN idea_checked ic ON ic.user_id = :userId AND ic.idea_id = idea.id
                WHERE expert_id = :userId AND r.is_confirmed IS FALSE
                """;
        return template.getDatabaseClient().sql(query)
                .bind("userId", userId)
                .map(ideaMapper::apply).all();
    }

    @CacheEvict(allEntries = true)
    public Mono<IdeaDTO> saveIdea(IdeaDTO ideaDTO, String initiatorEmail) {
        Idea idea = mapper.map(ideaDTO, Idea.class);
        idea.setInitiatorEmail(initiatorEmail);
        idea.setIsActive(true);
        idea.setRating(0.0);
        return Mono.just(idea)
                .flatMap(i -> template.getDatabaseClient()
                        .sql("SELECT id FROM groups WHERE 'EXPERT' = ANY(roles) ORDER BY id LIMIT 1")
                        .map((row, rowMetadata) -> row.get("id",String.class))
                        .one()
                        .map(g -> {
                            i.setGroupExpertId(g);
                            return i;
                        }))
                .flatMap(i -> template.getDatabaseClient()
                        .sql("SELECT id FROM groups WHERE 'PROJECT_OFFICE' = ANY(roles) ORDER BY id LIMIT 1")
                        .map((row, rowMetadata) -> row.get("id",String.class))
                        .one()
                        .map(g -> {
                            i.setGroupProjectOfficeId(g);
                            return i;
                        }))
                .flatMap(i -> {
                    IdeaDTO savedDTO = mapper.map(i, IdeaDTO.class);
                    GroupDTO experts = new GroupDTO();
                    GroupDTO projectOffice = new GroupDTO();
                    experts.setId(i.getGroupExpertId());
                    projectOffice.setId(i.getGroupProjectOfficeId());
                    savedDTO.setExperts(experts);
                    savedDTO.setProjectOffice(projectOffice);
                    if (i.getId()!=null) {
                        return template.exists(query(where("initiator_email").is(initiatorEmail)
                                        .and(where("id").is(i.getId()))),Idea.class)
                                .flatMap(isExist -> {
                                    if (Boolean.TRUE.equals(isExist)) {
                                        i.setModifiedAt(LocalDateTime.now());
                                        return template.update(i).thenReturn(savedDTO);
                                    }
                                    else return Mono.error(new AccessException("Нет Прав!"));
                                });
                    } else {
                        i.setCreatedAt(LocalDateTime.now());
                        return template.insert(i).flatMap(savedIdea -> {
                            savedDTO.setId(savedIdea.getId());
                            return template.select(query(where("group_id")
                                            .is(savedIdea.getGroupExpertId())), Group2User.class).collectList()
                                    .flatMap(list ->
                                        template.getDatabaseClient().inConnection(connection -> {
                                            Batch batch = connection.createBatch();
                                            list.forEach(u -> batch.add(
                                                    String.format(
                                                            "INSERT INTO rating (expert_id, is_confirmed, idea_id) VALUES ('%s', FALSE, '%s');",
                                                            u.getUserId(), savedIdea.getId()
                                                    ))
                                            );
                                            return Mono.from(batch.execute());
                                        }).then())
                                    .thenReturn(savedDTO);
                        });
                    }
                });
    }

    @CacheEvict(allEntries = true)
    public Mono<Void> deleteIdea(String ideaId, User user) {
        if (user.getRoles().contains(Role.ADMIN)) {
            return template.delete(query(where("id").is(ideaId)), Idea.class).then();
        }
        else return template.delete(query(where("id").is(ideaId)
                .and("initiator_email").is(user.getEmail())), Idea.class).then()
                .switchIfEmpty(Mono.error(new AccessException("Нет Прав!")));
    }

    @CacheEvict(allEntries = true)
    public Mono<Void> updateStatusByInitiator (String ideaId, String initiatorEmail){
        return template.update(query(where("id").is(ideaId).and(where("initiator_email").is(initiatorEmail))),
                update("status", Idea.Status.ON_APPROVAL).set("modified_at", LocalDateTime.now()),Idea.class).then();
    }

    @CacheEvict(allEntries = true)
    public Mono<Void> updateStatusIdea(String ideaId, StatusIdeaRequest newStatus){
        return template.update(query(where("id").is(ideaId)),
                        update("status", newStatus.getStatus())
                                .set("modified_at", LocalDateTime.now()),Idea.class).then();
    }

    @CacheEvict(allEntries = true)
    public Mono<Void> updateIdeaByAdmin(String ideaId, IdeaDTO updatedIdea) {
        updatedIdea.setId(ideaId);
        updatedIdea.setModifiedAt(LocalDateTime.now());
        return template.update(mapper.map(updatedIdea,Idea.class)).then();
    }

    public Mono<Void> addIdeaSkills(IdeaSkillRequest request, User user) {
        if (user.getRoles().contains(Role.ADMIN)) {
            return template.getDatabaseClient().inConnection(connection -> {
                Batch batch = connection.createBatch();
                request.getSkills().forEach(s -> batch.add(
                        String.format("INSERT INTO idea_skill (idea_id, skill_id) VALUES ('%s', '%s');",
                                request.getIdeaId(),s.getId())
                ));
                return Mono.from(batch.execute());
            }).then();
        }
        return template.exists(query(where("initiator_email").is(user.getEmail())
                .and(where("id").is(request.getIdeaId()))),Idea.class)
                .flatMap(isExists -> {
                    if (Boolean.TRUE.equals(isExists)) {
                        return template.getDatabaseClient().inConnection(connection -> {
                            Batch batch = connection.createBatch();
                            request.getSkills().forEach(s -> batch.add(
                                    String.format("INSERT INTO idea_skill (idea_id, skill_id) VALUES ('%s', '%s');",
                                            request.getIdeaId(),s.getId())
                            ));
                            return Mono.from(batch.execute());
                        }).then();
                    }
                    return Mono.error(new AccessException("Нет Прав!"));
                }).then();
    }

    public Mono<Void> updateIdeaSkills(IdeaSkillRequest request, User user) {
        if (user.getRoles().contains(Role.ADMIN)) {
            return template.delete(query(where("idea_id").is(request.getIdeaId())), Idea2Skill.class)
                    .flatMap(r ->
                            template.getDatabaseClient().inConnection(connection -> {
                                Batch batch = connection.createBatch();
                                request.getSkills().forEach(s -> batch.add(
                                        String.format("INSERT INTO idea_skill (idea_id, skill_id) VALUES ('%s', '%s');",
                                                request.getIdeaId(),s.getId())
                                ));
                                return Mono.from(batch.execute());
                            })
                    ).then(template.update(query(where("id").is(request.getIdeaId())),
                            update("modified_at",LocalDateTime.now()), Idea.class))
                    .then();
        }
        return template.exists(query(where("initiator_email").is(user.getEmail())
                .and("id").is(request.getIdeaId())),Idea.class)
                .flatMap(isExists -> {
                    if (Boolean.TRUE.equals(isExists)) {
                        return template.delete(query(where("idea_id").is(request.getIdeaId())), Idea2Skill.class)
                                .flatMap(r ->
                                        template.getDatabaseClient().inConnection(connection -> {
                                            Batch batch = connection.createBatch();
                                            request.getSkills().forEach(s -> batch.add(
                                                    String.format("INSERT INTO idea_skill (idea_id, skill_id) VALUES ('%s', '%s');",
                                                            request.getIdeaId(),s.getId())
                                            ));
                                            return Mono.from(batch.execute());
                                        }).then()
                                ).then(template.update(query(where("id").is(request.getIdeaId())),
                                        update("modified_at",LocalDateTime.now()), Idea.class))
                                .then();
                    }
                    return Mono.error(new AccessException("Нет Прав"));
                });
    }

    public Mono<IdeaSkillRequest> getIdeaSkills(String ideaId, User user) {
        String query = """
                SELECT skill.*, i.skill_id skill_id FROM idea_skill i
                LEFT JOIN skill ON skill.id = skill_id WHERE i.idea_id =:ideaId""";
        return template.getDatabaseClient().sql(query)
                .bind("ideaId", ideaId)
                .map((row, rowMetadata) ->
                    SkillDTO.builder()
                            .id(row.get("id",String.class))
                            .name(row.get("name",String.class))
                            .type(SkillType.valueOf(row.get("type",String.class)))
                            .confirmed(row.get("confirmed",Boolean.class))
                            .build()
                ).all().collectList()
                .flatMap(list ->
                    template.exists(query(where("initiator_email").is(user.getEmail())
                            .and("id").is(ideaId)),Idea.class)
                                    .flatMap(isExists -> {
                                        if (Boolean.TRUE.equals(isExists)
                                                || user.getRoles().contains(Role.ADMIN)
                                                || user.getRoles().contains(Role.EXPERT)
                                                || user.getRoles().contains(Role.PROJECT_OFFICE)) {
                                            return  Mono.just(IdeaSkillRequest.builder()
                                                    .skills(list)
                                                    .ideaId(ideaId)
                                                    .build());
                                        }
                                        return Mono.error(new AccessException("Нет Прав"));
                                    })
                );
    }
}
