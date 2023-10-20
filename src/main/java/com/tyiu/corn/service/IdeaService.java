package com.tyiu.corn.service;

import com.tyiu.corn.model.dto.IdeaDTO;
import com.tyiu.corn.model.entities.Idea;
import com.tyiu.corn.model.entities.Rating;
import com.tyiu.corn.model.entities.mappers.IdeaMapper;
import com.tyiu.corn.model.entities.relations.Group2User;
import com.tyiu.corn.model.enums.StatusIdea;
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
        String query = "SELECT idea.*, e.name e_name, e.id e_id, p.name p_name, p.id p_id" +
                " FROM idea LEFT JOIN groups e ON idea.group_expert_id = e.id" +
                " LEFT JOIN groups p ON idea.group_project_office_id = p.id" +
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

    //TODO: Добавлять идею в профиль
    @CacheEvict(allEntries = true)
    public Mono<IdeaDTO> saveIdea(IdeaDTO ideaDTO, Long initiatorId) {
        Idea idea = mapper.map(ideaDTO, Idea.class);
        idea.setInitiatorId(initiatorId);
        idea.setStatus(StatusIdea.NEW);
        idea.setCreatedAt(LocalDateTime.now());
        idea.setModifiedAt(LocalDateTime.now());
        idea.setGroupExpertId(ideaDTO.getExperts().getId());
        idea.setGroupProjectOfficeId(ideaDTO.getProjectOffice().getId());
        return template.insert(idea).flatMap(savedIdea -> {
            IdeaDTO savedDTO = mapper.map(savedIdea, IdeaDTO.class);
            savedDTO.setExperts(ideaDTO.getExperts());
            savedDTO.setProjectOffice(ideaDTO.getProjectOffice());
            //TODO: сохранять рейтинг одним запросом
            template.select(query(where("group_id").is(savedIdea.getGroupExpertId())), Group2User.class)
                    .flatMap(u -> {
                        Rating rating = Rating.builder()
                                .expertId(u.getUserId())
                                .confirmed(false)
                                .ideaId(savedIdea.getId())
                                .build();
                        return template.insert(rating);
                    }).subscribe();
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
                update("name",updatedIdea.getName())
                        .set("project_type",updatedIdea.getProjectType())
                        .set("problem", updatedIdea.getProblem())
                        .set("solution", updatedIdea.getSolution())
                        .set("result",updatedIdea.getResult())
                        .set("customer",updatedIdea.getCustomer())
                        .set("contact_person",updatedIdea.getContactPerson())
                        .set("description",updatedIdea.getDescription())
                        .set("technical_realizability",updatedIdea.getTechnicalRealizability())
                        .set("suitability",updatedIdea.getSuitability())
                        .set("budget",updatedIdea.getBudget())
                        .set("pre_assessment",updatedIdea.getPreAssessment())
                        .set("modified_at", LocalDateTime.now()), Idea.class).then();
    }
    @CacheEvict(allEntries = true)
    public Mono<Void> updateStatusByProjectOffice(Long id, StatusIdeaRequest newStatus){
        return template.update(query(where("id").is(id)),
                        update("status",newStatus.getStatus()),Idea.class).then();
    }

    @CacheEvict(allEntries = true)
    public Mono<Void> updateIdeaByAdmin(Long id, IdeaDTO updatedIdea) {
        return template.update(query(where("id").is(id)),
                update("name",updatedIdea.getName())
                        .set("project_type",updatedIdea.getProjectType())
                        .set("group_expert_id",updatedIdea.getExperts().getId())
                        .set("problem", updatedIdea.getProblem())
                        .set("solution", updatedIdea.getSolution())
                        .set("result",updatedIdea.getResult())
                        .set("customer",updatedIdea.getCustomer())
                        .set("contact_person",updatedIdea.getContactPerson())
                        .set("description",updatedIdea.getDescription())
                        .set("technical_realizability",updatedIdea.getTechnicalRealizability())
                        .set("suitability",updatedIdea.getSuitability())
                        .set("budget",updatedIdea.getBudget())
                        .set("status",updatedIdea.getStatus())
                        .set("rating", updatedIdea.getRating()), Idea.class).then();
    }
}