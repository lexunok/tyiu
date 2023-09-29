package com.tyiu.corn.service;

import com.tyiu.corn.config.exception.ErrorException;
import com.tyiu.corn.model.dto.IdeaDTO;
import com.tyiu.corn.model.entities.Group;
import com.tyiu.corn.model.entities.Idea;
import com.tyiu.corn.model.entities.Rating;
import com.tyiu.corn.model.enums.StatusIdea;
import com.tyiu.corn.model.requests.StatusIdeaRequest;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.springframework.cache.annotation.CacheConfig;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.data.mongodb.core.ReactiveMongoTemplate;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.stereotype.Service;

import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.time.Instant;

@Service
@RequiredArgsConstructor
@Slf4j
@CacheConfig(cacheNames = "ideas")
public class IdeaService {

    private final ReactiveMongoTemplate template;
    private final ModelMapper mapper;

    @Cacheable
    public Mono<IdeaDTO> getIdea(String id) {
        return template.findById(id, Idea.class).flatMap(i -> template.findById(i.getExperts(), Group.class)
                .then(template.findById(i.getProjectOffice(), Group.class))
                .then(Mono.just(mapper.map(i, IdeaDTO.class))))
                .onErrorResume(ex -> Mono.error(new ErrorException("Not success!")));
    }
    @Cacheable
    public Flux<IdeaDTO> getListIdea() {
        return template.findAll(Idea.class).flatMap(i -> Flux.just(mapper.map(i, IdeaDTO.class)))
                .onErrorResume(ex -> Mono.error(new ErrorException("Not success!")));
    }
    @CacheEvict(allEntries = true)
    public Mono<IdeaDTO> saveIdea(IdeaDTO ideaDTO, String initiator) {
        Idea idea = mapper.map(ideaDTO, Idea.class);
        idea.setInitiator(initiator);
        idea.setStatus(StatusIdea.NEW);
        idea.setCreatedAt(Instant.now());
        return template.save(idea).flatMap(savedIdea ->
                {
                    IdeaDTO savedDTO = mapper.map(savedIdea, IdeaDTO.class);
                    return template.findById(savedIdea.getExperts(), Group.class).flatMap(g -> {
                        g.getUsersId()
                                .forEach(r ->
                                        template.save(Rating.builder()
                                                .expert(r)
                                                .ideaId(savedIdea.getId())
                                                .confirmed(false)
                                                .build()
                                        ).subscribe()
                                );
                        savedDTO.setExperts(g);
                        return Mono.empty();
                    }).then(template.findById(savedIdea.getProjectOffice(), Group.class).flatMap(p -> {
                                savedDTO.setProjectOffice(p);
                                return Mono.empty();
                            })).then(Mono.just(savedDTO));
                }).onErrorResume(ex -> Mono.error(new ErrorException("Not success!")));
    }
    @CacheEvict(allEntries = true)
    public Mono<Void> deleteIdea(String id) {
        return template.remove(Query.query(Criteria.where("id").is(id)), Idea.class).then()
                .onErrorResume(ex -> Mono.error(new ErrorException("Not success!")));
    }
    @CacheEvict(allEntries = true)
    public Mono<Void> updateStatusByInitiator (String id, String initiator){
        return template.findById(id, Idea.class).flatMap(i -> {
            if (initiator.equals(i.getInitiator())) {
                i.setStatus(StatusIdea.ON_APPROVAL);
                return template.save(i).then();
            }
            return Mono.empty();
        }).onErrorResume(ex -> Mono.error(new ErrorException("Not success!")));
    }
    @CacheEvict(allEntries = true)
    public Mono<Void> updateIdeaByInitiator(String id, IdeaDTO updatedIdea) {
        return template.findById(id, Idea.class).flatMap(i -> {
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
            return template.save(i).then();
        }).onErrorResume(ex -> Mono.error(new ErrorException("Not success!")));
    }
    @CacheEvict(allEntries = true)
    public Mono<Void> updateStatusByProjectOffice(String id, StatusIdeaRequest newStatus){
        return template.findById(id, Idea.class).flatMap(i -> {
            i.setStatus(newStatus.getStatus());
            return template.save(i).then();
        }).onErrorResume(ex -> Mono.error(new ErrorException("Not success!")));
    }

    @CacheEvict(allEntries = true)
    public Mono<Void> updateIdeaByAdmin(String id, IdeaDTO updatedIdea) {
        return template.findById(id, Idea.class).flatMap(i -> {
            i.setName(updatedIdea.getName());
            i.setProjectType(updatedIdea.getProjectType());
            i.setExperts(updatedIdea.getExperts().getId());
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
            return template.save(i).then();
        }).onErrorResume(ex -> Mono.error(new ErrorException("Not success!")));
    }
}