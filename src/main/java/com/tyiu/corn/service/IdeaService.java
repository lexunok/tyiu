package com.tyiu.corn.service;

import com.tyiu.corn.model.dto.IdeaDTO;
import com.tyiu.corn.model.dto.SkillDTO;
import com.tyiu.corn.model.entities.Idea;
import com.tyiu.corn.model.entities.Rating;
import com.tyiu.corn.model.entities.mappers.IdeaMapper;
import com.tyiu.corn.model.entities.relations.Group2User;
import com.tyiu.corn.model.entities.relations.Idea2Skill;
import com.tyiu.corn.model.enums.SkillType;
import com.tyiu.corn.model.enums.StatusIdea;
import com.tyiu.corn.model.requests.IdeaSkillRequest;
import com.tyiu.corn.model.requests.StatusIdeaRequest;
import lombok.RequiredArgsConstructor;
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
public class IdeaService {

    private final R2dbcEntityTemplate template;
    private final ModelMapper mapper;

    @Cacheable
    public Mono<IdeaDTO> getIdea(Long ideaId) {
        String query = "SELECT idea.*, u.email initiator_email, e.name e_name, e.id e_id, p.name p_name, p.id p_id" +
                " FROM idea LEFT JOIN groups e ON idea.group_expert_id = e.id" +
                " LEFT JOIN groups p ON idea.group_project_office_id = p.id" +
                " LEFT JOIN users u ON idea.initiator_id = u.id" +
                " WHERE idea.id =:ideaId";
        IdeaMapper ideaMapper = new IdeaMapper();
        return template.getDatabaseClient()
                .sql(query)
                .bind("ideaId", ideaId)
                .map(ideaMapper::apply)
                .first();
    }

    @Cacheable
    public Flux<IdeaDTO> getListIdea() {
        return template.select(Idea.class).all()
                .flatMap(i -> Flux.just(mapper.map(i, IdeaDTO.class)));
    }
    @Cacheable
    public Flux<IdeaDTO> getListIdeaByInitiator(Long initiatorId) {
        return template.select(query(where("initiator_id").is(initiatorId)),Idea.class)
                .flatMap(i -> Flux.just(mapper.map(i, IdeaDTO.class)));
    }

    @CacheEvict(allEntries = true)
    public Mono<IdeaDTO> saveIdeaToApproval(IdeaDTO ideaDTO, Long initiatorId) {
        Idea idea = mapper.map(ideaDTO, Idea.class);
        idea.setInitiatorId(initiatorId);
        idea.setStatus(StatusIdea.ON_APPROVAL);
        idea.setCreatedAt(LocalDateTime.now());
        idea.setModifiedAt(LocalDateTime.now());
        idea.setGroupExpertId(ideaDTO.getExperts().getId());
        idea.setGroupProjectOfficeId(ideaDTO.getProjectOffice().getId());
        //TODO: Добавить проверку на уровне бд
        return Mono.just(ideaDTO.getId()!=null)
                .flatMap(b -> {
                    if (b) {
                        return template.update(idea);
                    } else return template.insert(idea);
                })
                .flatMap(savedIdea -> {
                    IdeaDTO savedDTO = mapper.map(savedIdea, IdeaDTO.class);
                    savedDTO.setExperts(ideaDTO.getExperts());
                    savedDTO.setProjectOffice(ideaDTO.getProjectOffice());
                    //TODO: сохранять рейтинг одним запросом
                    return template.select(query(where("group_id")
                                    .is(savedIdea.getGroupExpertId())), Group2User.class)
                            .flatMap(u -> {
                                Rating rating = Rating.builder()
                                        .expertId(u.getUserId())
                                        .confirmed(false)
                                        .ideaId(savedIdea.getId())
                                        .build();
                                return template.insert(rating);
                            }).then().thenReturn(savedDTO);
                });
    }
    @CacheEvict(allEntries = true)
    public Mono<IdeaDTO> saveIdeaInDraft(IdeaDTO ideaDTO, Long initiatorId) {
        Idea idea = mapper.map(ideaDTO, Idea.class);
        idea.setInitiatorId(initiatorId);
        idea.setStatus(StatusIdea.NEW);
        idea.setModifiedAt(LocalDateTime.now());
        idea.setGroupExpertId(ideaDTO.getExperts().getId());
        idea.setGroupProjectOfficeId(ideaDTO.getProjectOffice().getId());
        //TODO: Добавить проверку на уровне бд
        return Mono.just(ideaDTO.getId()!=null)
                .flatMap(b -> {
                    if (b) {
                        return template.update(idea);
                    } else return template.insert(idea);
                })
                .flatMap(savedIdea ->{
                    IdeaDTO savedDTO = mapper.map(savedIdea, IdeaDTO.class);
                    savedDTO.setExperts(ideaDTO.getExperts());
                    savedDTO.setProjectOffice(ideaDTO.getProjectOffice());
                    return Mono.just(savedDTO);
                });
    }

    @CacheEvict(allEntries = true)
    public Mono<Void> deleteIdea(Long id) {
        return template.delete(query(where("id").is(id)), Idea.class).then();
    }

    @CacheEvict(allEntries = true)
    public Mono<Void> updateStatusByInitiator (Long id){
        return template.update(query(where("id").is(id)),
                update("status", StatusIdea.ON_APPROVAL),Idea.class).then();
    }

    @CacheEvict(allEntries = true)
    public Mono<Void> updateIdeaByInitiator(Long id, IdeaDTO updatedIdea) {
        return template.update(query(where("id").is(id)),
                update("name", updatedIdea.getName())
                        .set("max_team_size", updatedIdea.getMaxTeamSize())
                        .set("problem", updatedIdea.getProblem())
                        .set("solution", updatedIdea.getSolution())
                        .set("result", updatedIdea.getResult())
                        .set("customer", updatedIdea.getCustomer())
                        .set("contact_person", updatedIdea.getContactPerson())
                        .set("description", updatedIdea.getDescription())
                        .set("min_team_size", updatedIdea.getMinTeamSize())
                        .set("suitability", updatedIdea.getSuitability())
                        .set("budget", updatedIdea.getBudget())
                        .set("pre_assessment", updatedIdea.getPreAssessment())
                        .set("modified_at", LocalDateTime.now()), Idea.class).then();
    }

    @CacheEvict(allEntries = true)
    public Mono<Void> updateStatusIdea(Long id, StatusIdeaRequest newStatus){
        return template.update(query(where("id").is(id)),
                        update("status",newStatus.getStatus()),Idea.class).then();
    }

    @CacheEvict(allEntries = true)
    public Mono<Void> updateIdeaByAdmin(Long id, IdeaDTO updatedIdea) {
        return template.update(query(where("id").is(id)),
                update("name", updatedIdea.getName())
                        .set("max_team_size", updatedIdea.getMaxTeamSize())
                        .set("group_expert_id", updatedIdea.getExperts().getId())
                        .set("problem", updatedIdea.getProblem())
                        .set("solution", updatedIdea.getSolution())
                        .set("result", updatedIdea.getResult())
                        .set("customer", updatedIdea.getCustomer())
                        .set("contact_person", updatedIdea.getContactPerson())
                        .set("description", updatedIdea.getDescription())
                        .set("min_team_size", updatedIdea.getMinTeamSize())
                        .set("suitability", updatedIdea.getSuitability())
                        .set("budget", updatedIdea.getBudget())
                        .set("status", updatedIdea.getStatus())
                        .set("rating", updatedIdea.getRating()), Idea.class).then();
    }

    //TODO:сохранять одним запросом
    public Mono<Void> addIdeaSkills(IdeaSkillRequest request) {
        return Flux.fromIterable(request.getSkills()).flatMap(s ->
                template.insert(new Idea2Skill(request.getIdeaId(),s.getId()))).then();
    }

    public Mono<Void> updateIdeaSkills(IdeaSkillRequest request) {
        template.delete(query(where("idea_id").is(request.getIdeaId())), Idea2Skill.class).subscribe();
        return Flux.fromIterable(request.getSkills()).flatMap(s ->
                template.insert(new Idea2Skill(request.getIdeaId(),s.getId()))).then();
    }

    public Mono<IdeaSkillRequest> getIdeaSkills(Long ideaId) {
        String query = "SELECT skill.*, i.skill_id skill_id FROM idea_skill i " +
                "LEFT JOIN skill ON skill.id = skill_id WHERE i.idea_id =:ideaId";
        return template.getDatabaseClient().sql(query)
                .bind("ideaId", ideaId)
                .map((row, rowMetadata) ->
                    SkillDTO.builder()
                            .id(row.get("id",Long.class))
                            .name(row.get("name",String.class))
                            .type(SkillType.valueOf(row.get("type",String.class)))
                            .creatorId(row.get("creator_id",Long.class))
                            .deleterId(row.get("deleter_id",Long.class))
                            .updaterId(row.get("updater_id",Long.class))
                            .confirmed(row.get("confirmed",Boolean.class))
                            .build()
                ).all().collectList()
                .flatMap(list -> Mono.just(IdeaSkillRequest.builder()
                        .skills(list)
                        .ideaId(ideaId)
                        .build())
                );
    }
}
