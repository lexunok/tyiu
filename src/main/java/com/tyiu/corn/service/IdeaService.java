package com.tyiu.corn.service;

import com.tyiu.corn.config.exception.ErrorException;
import com.tyiu.corn.model.dto.GroupDTO;
import com.tyiu.corn.model.dto.IdeaDTO;
import com.tyiu.corn.model.entities.Group;
import com.tyiu.corn.model.entities.Idea;
import com.tyiu.corn.model.entities.Rating;
import com.tyiu.corn.model.entities.mappers.IdeaMapper;
import com.tyiu.corn.model.entities.relations.Group2User;
import com.tyiu.corn.model.enums.StatusIdea;
import com.tyiu.corn.model.requests.StatusIdeaRequest;
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

import java.time.Instant;
import java.util.List;

import static org.springframework.data.relational.core.query.Criteria.where;
import static org.springframework.data.relational.core.query.Query.query;
import static org.springframework.data.relational.core.query.Update.update;

@Service
@RequiredArgsConstructor
@Slf4j
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
                .first()
                .switchIfEmpty(Mono.error(new ErrorException("Not found!")));
    }

    @Cacheable
    public Flux<IdeaDTO> getListIdea() {
        return template.select(Idea.class).all()
                .flatMap(i -> Flux.just(mapper.map(i, IdeaDTO.class)))
                .switchIfEmpty(Mono.error(new ErrorException("Not found!")));
    }

    //TODO: Добавлять идею в профиль
    @CacheEvict(allEntries = true)
    public Mono<IdeaDTO> saveIdea(IdeaDTO ideaDTO, String initiator) {
        Idea idea = mapper.map(ideaDTO, Idea.class);
        idea.setInitiator(initiator);
        idea.setStatus(StatusIdea.NEW);
        idea.setCreatedAt(Instant.now());
        idea.setModifiedAt(Instant.now());
        idea.setGroupExpertId(ideaDTO.getExperts().getId());
        idea.setGroupProjectOfficeId(ideaDTO.getProjectOffice().getId());
        return template.insert(idea).flatMap(savedIdea ->
            template.select(query(where("group_id").is(savedIdea.getGroupExpertId())), Group2User.class)
                    .flatMap(u -> {
                        Rating rating = Rating.builder()
                                .expert(u.getUserId())
                                .confirmed(false)
                                .ideaId(savedIdea.getId())
                                .build();
                        IdeaDTO savedDTO = mapper.map(savedIdea, IdeaDTO.class);
                        savedDTO.setExperts(GroupDTO.builder().id(u.getGroupId()).build());
                        return template.insert(rating).thenReturn(savedDTO);
                    }).next()
        ).onErrorResume(ex -> Mono.error(new ErrorException("Not success!")));
    }
    @CacheEvict(allEntries = true)
    public Mono<Void> deleteIdea(Long id) {
        return template.delete(query(where("id").is(id)), Idea.class).then()
                .onErrorResume(ex -> Mono.error(new ErrorException("Not success!")));
    }

    @CacheEvict(allEntries = true)
    public Mono<Void> updateStatusByInitiator (Long id){
        return template.update(query(where("id").is(id)),
                update("staus", StatusIdea.ON_APPROVAL),Idea.class).then()
                .onErrorResume(ex -> Mono.error(new ErrorException("Not success!")));
    }

    @CacheEvict(allEntries = true)
    public Mono<Void> updateIdeaByInitiator(Long id, IdeaDTO updatedIdea) {
        return template.selectOne(query(where("id").is(id)),Idea.class)
                .flatMap(i -> {
                    i.setName(updatedIdea.getName());
                    i.setProjectType(updatedIdea.getProjectType());
                    i.setProblem(updatedIdea.getProblem());
                    i.setSolution(updatedIdea.getSolution());
                    i.setResult(updatedIdea.getResult());
                    i.setCustomer(updatedIdea.getCustomer());
                    i.setContactPerson(updatedIdea.getContactPerson());
                    i.setDescription(updatedIdea.getDescription());
                    i.setTechnicalRealizability(updatedIdea.getTechnicalRealizability());
                    i.setSuitability(updatedIdea.getSuitability());
                    i.setBudget(updatedIdea.getBudget());
                    i.setPreAssessment(updatedIdea.getPreAssessment());
                    i.setModifiedAt(Instant.now());
            return template.insert(i).then();
        }).onErrorResume(ex -> Mono.error(new ErrorException("Not success!")));
    }
    @CacheEvict(allEntries = true)
    public Mono<Void> updateStatusByProjectOffice(Long id, StatusIdeaRequest newStatus){
        return template.update(query(where("id").is(id)),
                        update("status",newStatus.getStatus()),Idea.class).then()
                .onErrorResume(ex -> Mono.error(new ErrorException("Not success!")));
    }

    @CacheEvict(allEntries = true)
    public Mono<Void> updateIdeaByAdmin(Long id, IdeaDTO updatedIdea) {
        return template.selectOne(query(where("id").is(id)), Idea.class).flatMap(i -> {
            i.setName(updatedIdea.getName());
            i.setProjectType(updatedIdea.getProjectType());
            i.setGroupExpertId(updatedIdea.getExperts().getId());
            i.setProblem(updatedIdea.getProblem());
            i.setSolution(updatedIdea.getSolution());
            i.setResult(updatedIdea.getResult());
            i.setCustomer(updatedIdea.getCustomer());
            i.setDescription(updatedIdea.getDescription());
            i.setSuitability(updatedIdea.getSuitability());
            i.setBudget(updatedIdea.getBudget());
            i.setTechnicalRealizability(updatedIdea.getTechnicalRealizability());
            i.setStatus(updatedIdea.getStatus());
            i.setRating(updatedIdea.getRating());
            return template.insert(i).then();
        }).onErrorResume(ex -> Mono.error(new ErrorException("Not success!")));
    }
}