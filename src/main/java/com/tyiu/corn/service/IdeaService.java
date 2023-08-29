package com.tyiu.corn.service;

import java.util.Date;
import java.util.List;

import com.tyiu.corn.exception.AccessException;
import com.tyiu.corn.exception.NotFoundException;
import com.tyiu.corn.model.dto.IdeaDTO;
import com.tyiu.corn.model.entities.Idea;
import com.tyiu.corn.model.dto.RatingDTO;
import com.tyiu.corn.model.enums.StatusIdea;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import com.tyiu.corn.repository.IdeaRepository;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;


@Service
@RequiredArgsConstructor
// После полного перехода на реактивный стэк @CacheConfig(cacheNames = {"ideas"})
public class IdeaService {

    private final IdeaRepository ideaRepository;

    // После полного перехода на реактивный стэк @Cacheable
    public Flux<IdeaDTO> getListIdeaForInitiator(String initiator) {
        return ideaRepository.findAllByInitiator(initiator).cast(IdeaDTO.class);
    }

    // После полного перехода на реактивный стэк @Cacheable(key = "#id")
    public Mono<IdeaDTO> getIdeaForInitiator(Long id, String email) {
        return ideaRepository.findById(id).cast(IdeaDTO.class);
    }
    // После полного перехода на реактивный стэк @Cacheable
    public Flux<IdeaDTO> getListIdea() {
        return ideaRepository.findAll().cast(IdeaDTO.class);
    }
    // После полного перехода на реактивный стэк @CacheEvict(allEntries = true)
    public Mono<IdeaDTO> saveIdea(IdeaDTO ideaDTO, String initiator) {
        ideaDTO.setDateCreated(new Date());
        ideaDTO.setInitiator(initiator);
        ideaDTO.setStatus(StatusIdea.NEW);
        return Mono.just(ideaDTO).cast(Idea.class).flatMap(ideaRepository::save).cast(IdeaDTO.class);
    }
    // После полного перехода на реактивный стэк @CacheEvict(allEntries = true)
    public void deleteIdeaByInitiator(Long id, String email) {
        ideaRepository.deleteById(id);
    }
    // После полного перехода на реактивный стэк @CacheEvict(allEntries = true)
    public void deleteIdeaByAdmin(Long id) {
        ideaRepository.deleteById(id);
    }

    // После полного перехода на реактивный стэк @CacheEvict(allEntries = true)
    public void updateStatusByInitiator (Long id, String email){
        Mono<Idea> idea = ideaRepository.findById(id);
        idea.flatMap(i -> {
            i.setStatus(StatusIdea.ON_CONFIRMATION);
            return ideaRepository.save(i);
        });
    }

    // После полного перехода на реактивный стэк @CacheEvict(allEntries = true)
    public void updateIdeaByInitiator(Long id, String email, IdeaDTO updatedIdea) {
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
            i.setRealizability(updatedIdea.getRealizability());
            i.setSuitability(updatedIdea.getSuitability());
            i.setBudget(updatedIdea.getBudget());
            i.setRating(updatedIdea.getRating());
            i.setDateModified(new Date());
            return ideaRepository.save(i);
        });
    }

    // После полного перехода на реактивный стэк @CacheEvict(allEntries = true)
    public void updateStatusByProjectOffice (Long ideaId, StatusIdea newStatus){
        Mono<Idea> idea = ideaRepository.findById(ideaId);
        idea.flatMap(i ->{
            i.setStatus(newStatus);
            return ideaRepository.save(i);
        });
    }

    // После полного перехода на реактивный стэк @CacheEvict(allEntries = true)
    public void updateStatusByExpert(Long ideaId, RatingDTO ratingDTO){
        Mono<Idea> idea = ideaRepository.findById(ideaId);
        idea.flatMap(i -> {
            i.setStatus(ratingDTO.getStatus());
            i.setRating(ratingDTO.getRating());
            i.setMarketValue(ratingDTO.getMarketValue());
            i.setOriginality(ratingDTO.getOriginality());
            i.setTechnicalFeasibility(ratingDTO.getTechnicalFeasibility());
            i.setUnderstanding(ratingDTO.getUnderstanding());
            return ideaRepository.save(i);
        });
    }

    // После полного перехода на реактивный стэк @CacheEvict(allEntries = true)
    public void updateIdeaByAdmin(Long id, IdeaDTO updatedIdea) {
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
            i.setRealizability(updatedIdea.getRealizability());
            i.setSuitability(updatedIdea.getSuitability());
            i.setBudget(updatedIdea.getBudget());
            i.setStatus(updatedIdea.getStatus());
            i.setRating(updatedIdea.getRating());
            i.setRisk(updatedIdea.getRisk());
            i.setDateModified(new Date());
            return ideaRepository.save(i);
        });
    }
}