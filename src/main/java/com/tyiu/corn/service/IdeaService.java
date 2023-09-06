package com.tyiu.corn.service;

import java.time.Instant;
import java.util.Date;

import com.tyiu.corn.model.dto.IdeaDTO;
import com.tyiu.corn.model.entities.Idea;
import com.tyiu.corn.model.dto.RatingDTO;
import com.tyiu.corn.model.enums.StatusIdea;
import lombok.RequiredArgsConstructor;

import lombok.Value;
import org.modelmapper.ModelMapper;
import org.springframework.cache.annotation.CacheConfig;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.data.mongodb.core.aggregation.ArrayOperators;
import org.springframework.stereotype.Service;

import com.tyiu.corn.repository.IdeaRepository;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;


@Service
@RequiredArgsConstructor
@CacheConfig(cacheNames = {"ideas"})
public class IdeaService {

    private final IdeaRepository ideaRepository;
    private final ModelMapper mapper;

    @Cacheable
    public Flux<IdeaDTO> getListIdeaForInitiator(String initiator) {
        Flux<Idea> ideas = ideaRepository.findAllByInitiator(initiator);
        return ideas.flatMap(i -> Flux.just(mapper.map(i, IdeaDTO.class)));
    }

    @Cacheable(key = "#id")
    public Mono<IdeaDTO> getIdeaForInitiator(String id) {
        Mono<Idea> idea = ideaRepository.findById(id);
        return idea.flatMap(i -> Mono.just(mapper.map(i, IdeaDTO.class)));
    }
    @Cacheable
    public Flux<IdeaDTO> getListIdea() {
        Flux<Idea> ideas = ideaRepository.findAll();
        return ideas.flatMap(i -> Flux.just(mapper.map(i, IdeaDTO.class)));
    }
    @CacheEvict(allEntries = true)
    public Mono<IdeaDTO> saveIdea(IdeaDTO ideaDTO, String initiator) {
        ideaDTO.setCreatedAt(Instant.now());
        ideaDTO.setInitiator(initiator);
        ideaDTO.setStatus(StatusIdea.NEW);
        Mono<Idea> idea = ideaRepository.save(mapper.map(ideaDTO, Idea.class));
        return idea.flatMap(i -> Mono.just(mapper.map(i, IdeaDTO.class)));
    }
    @CacheEvict(allEntries = true)
    public void deleteIdeaByInitiator(String id) {
        ideaRepository.deleteById(id).subscribe();
    }
    @CacheEvict(allEntries = true)
    public void deleteIdeaByAdmin(String id) {
        ideaRepository.deleteById(id).subscribe();
    }

    @CacheEvict(allEntries = true)
    public void updateStatusByInitiator (String id){
        Mono<Idea> idea = ideaRepository.findById(id);
        idea.flatMap(i -> {
            i.setStatus(StatusIdea.ON_CONFIRMATION);
            return ideaRepository.save(i);
        }).subscribe();
    }

    @CacheEvict(allEntries = true)
    public void updateIdeaByInitiator(String id, IdeaDTO updatedIdea) {
        Mono<Idea> idea = ideaRepository.findById(id);
        idea.flatMap(i -> {
            i.setName(updatedIdea.getName());
            i.setProjectType(updatedIdea.getProjectType());
            i.setProblem(updatedIdea.getProblem());
            i.setSolution(updatedIdea.getSolution());
            i.setResult(updatedIdea.getResult());
            i.setCustomer(updatedIdea.getCustomer());
            i.setContactPerson(updatedIdea.getContactPerson());
            i.setDescription(updatedIdea.getDescription());
            i.setTechnicalRealizability(updatedIdea.getRealizability());
            i.setSuitability(updatedIdea.getSuitability());
            i.setBudget(updatedIdea.getBudget());
            i.setRating(updatedIdea.getRating());
            i.setModifiedAt(Instant.now());
            return ideaRepository.save(i);
        }).subscribe();
    }

    @CacheEvict(allEntries = true)
    public void updateStatusByProjectOffice (String ideaId, StatusIdea newStatus){
        Mono<Idea> idea = ideaRepository.findById(ideaId);
        idea.flatMap(i ->{
            i.setStatus(newStatus);
            return ideaRepository.save(i);
        }).subscribe();
    }

    @CacheEvict(allEntries = true)
    public void updateStatusByExpert(String ideaId, RatingDTO ratingDTO){
        Mono<Idea> idea = ideaRepository.findById(ideaId);
        idea.flatMap(i -> {
            if (i.getExperts().getUsers().size() == i.getConfirmedBy().size())
            {
                i.setStatus(ratingDTO.getStatus());
            }
            else {
                i.setStatus(StatusIdea.ON_APPROVAL);
            }
            i.setRating(ratingDTO.getRating());
            i.setMarketValue(ratingDTO.getMarketValue());
            i.setOriginality(ratingDTO.getOriginality());
            i.setUnderstanding(ratingDTO.getUnderstanding());
            return ideaRepository.save(i);
        }).subscribe();
    }

    @CacheEvict(allEntries = true)
    public void updateIdeaByAdmin(String id, IdeaDTO updatedIdea) {
        Mono<Idea> idea = ideaRepository.findById(id);
        idea.flatMap(i -> {
            i.setName(updatedIdea.getName());
            i.setProjectType(updatedIdea.getProjectType());
            i.setExperts(updatedIdea.getExperts());
            i.setProblem(updatedIdea.getProblem());
            i.setSolution(updatedIdea.getSolution());
            i.setResult(updatedIdea.getResult());
            i.setCustomer(updatedIdea.getCustomer());
            i.setDescription(updatedIdea.getDescription());
            i.setSuitability(updatedIdea.getSuitability());
            i.setBudget(updatedIdea.getBudget());
            i.setStatus(updatedIdea.getStatus());
            i.setRating(updatedIdea.getRating());
            i.setRisk(updatedIdea.getRisk());
            i.setModifiedAt(Instant.now());
            return ideaRepository.save(i);
        }).subscribe();
    }
}