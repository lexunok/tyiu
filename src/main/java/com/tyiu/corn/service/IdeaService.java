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
import org.modelmapper.ModelMapper;
import org.springframework.cache.annotation.CacheConfig;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.stereotype.Service;

import com.tyiu.corn.repository.IdeaRepository;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;


@Service
@RequiredArgsConstructor
// После полного перехода на реактивный стэк @CacheConfig(cacheNames = {"ideas"})
public class IdeaService {

    private final IdeaRepository ideaRepository;
    private final ModelMapper mapper;

    // После полного перехода на реактивный стэк @Cacheable
    public Flux<IdeaDTO> getListIdeaForInitiator(String initiator) {
        List<Idea> ideas = ideaRepository.findAllByInitiator(initiator);
        return Flux.just(ideas).map(i -> mapper.map(i,IdeaDTO.class));
    }

    // После полного перехода на реактивный стэк @Cacheable(key = "#id")
    public Mono<IdeaDTO> getIdeaForInitiator(Long id, String email) {
        Idea idea = ideaRepository.findById(id).orElseThrow(() -> new NotFoundException("Идея не найдена"));
        if (email.equals(idea.getInitiator())) {
            return Mono.just(mapper.map(idea, IdeaDTO.class));
        } else {
            throw new AccessException("Идея не принадлежит инициатору");
        }
    }
    // После полного перехода на реактивный стэк @Cacheable
    public Flux<IdeaDTO> getListIdea() {
        List<Idea> ideas = ideaRepository.findAll();
        return Flux.just(ideas).map(i -> mapper.map(i, IdeaDTO.class));
    }
    // После полного перехода на реактивный стэк @CacheEvict(allEntries = true)
    public Mono<IdeaDTO> saveIdea(IdeaDTO ideaDTO, String initiator) {
        ideaDTO.setDateCreated(new Date());
        ideaDTO.setInitiator(initiator);
        ideaDTO.setStatus(StatusIdea.NEW);
        Idea idea = ideaRepository.save(mapper.map(ideaDTO, Idea.class));
        return Mono.just(mapper.map(idea, IdeaDTO.class));
    }
    // После полного перехода на реактивный стэк @CacheEvict(allEntries = true)
    public void deleteIdeaByInitiator(Long id, String email) {
        Idea idea = ideaRepository.findById(id).orElseThrow(() -> new NotFoundException("Идея не найдена"));
        if (email.equals(idea.getInitiator())){
            ideaRepository.deleteById(id);
        }
        else {
            throw new AccessException("Идея не принадлежит инициатору");
        }
    }
    // После полного перехода на реактивный стэк @CacheEvict(allEntries = true)
    public void deleteIdeaByAdmin(Long id) {
        ideaRepository.deleteById(id);
    }

    // После полного перехода на реактивный стэк @CacheEvict(allEntries = true)
    public void updateStatusByInitiator (Long id, String email){
        Idea idea = ideaRepository.findById(id).orElseThrow(() -> new NotFoundException("Идея не найдена"));
        if (email.equals(idea.getInitiator())){
            idea.setStatus(StatusIdea.ON_CONFIRMATION);
            ideaRepository.save(idea);
        }
        else {
            throw new AccessException("Идея не принадлежит инициатору");
        }
    }

    // После полного перехода на реактивный стэк @CacheEvict(allEntries = true)
    public void updateIdeaByInitiator(Long id, String email, IdeaDTO updatedIdea) {
        Idea idea = ideaRepository.findById(id).orElseThrow(() -> new NotFoundException("Идея не найдена"));
        if (email.equals(idea.getInitiator())){
            idea.setName(updatedIdea.getName());
            idea.setProjectType(updatedIdea.getProjectType());
            idea.setProblem(updatedIdea.getProblem());
            idea.setSolution(updatedIdea.getSolution());
            idea.setResult(updatedIdea.getResult());
            idea.setCustomer(updatedIdea.getCustomer());
            idea.setContactPerson(updatedIdea.getContactPerson());
            idea.setDescription(updatedIdea.getDescription());
            idea.setRealizability(updatedIdea.getRealizability());
            idea.setSuitability(updatedIdea.getSuitability());
            idea.setBudget(updatedIdea.getBudget());
            idea.setRating(updatedIdea.getRating());
            idea.setDateModified(new Date());
            ideaRepository.save(idea);
        }
        else {
            throw new AccessException("Идея не принадлежит инициатору");
        }
    }

    // После полного перехода на реактивный стэк @CacheEvict(allEntries = true)
    public void updateStatusByProjectOffice (Long ideaId, StatusIdea newStatus){
        Idea idea = ideaRepository.findById(ideaId).orElseThrow(() -> new NotFoundException("Идея не найдена"));
        idea.setStatus(newStatus);
        ideaRepository.save(idea);
    }

    // После полного перехода на реактивный стэк @CacheEvict(allEntries = true)
    public void updateStatusByExpert(Long ideaId, RatingDTO ratingDTO){
        Idea idea = ideaRepository.findById(ideaId).orElseThrow(() -> new NotFoundException("Идея не найдена"));
        idea.setStatus(ratingDTO.getStatus());
        idea.setRating(ratingDTO.getRating());
        idea.setMarketValue(ratingDTO.getMarketValue());
        idea.setOriginality(ratingDTO.getOriginality());
        idea.setTechnicalFeasibility(ratingDTO.getTechnicalFeasibility());
        idea.setUnderstanding(ratingDTO.getUnderstanding());
        ideaRepository.save(idea);
    }

    // После полного перехода на реактивный стэк @CacheEvict(allEntries = true)
    public void updateIdeaByAdmin(Long id, IdeaDTO updatedIdea) {
        Idea idea = ideaRepository.findById(id).orElseThrow(() -> new NotFoundException("Идея не найдена"));
        idea.setName(updatedIdea.getName());
        idea.setProjectType(updatedIdea.getProjectType());
        idea.setExperts(updatedIdea.getExperts());
        idea.setProblem(updatedIdea.getProblem());
        idea.setSolution(updatedIdea.getSolution());
        idea.setResult(updatedIdea.getResult());
        idea.setCustomer(updatedIdea.getCustomer());
        idea.setDescription(updatedIdea.getDescription());
        idea.setRealizability(updatedIdea.getRealizability());
        idea.setSuitability(updatedIdea.getSuitability());
        idea.setBudget(updatedIdea.getBudget());
        idea.setStatus(updatedIdea.getStatus());
        idea.setRating(updatedIdea.getRating());
        idea.setRisk(updatedIdea.getRisk());
        idea.setDateModified(new Date());
        ideaRepository.save(idea);
    }
}