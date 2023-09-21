package com.tyiu.corn.service;

import java.util.Date;

import com.tyiu.corn.model.dto.IdeaDTO;
import com.tyiu.corn.model.entities.Idea;
import com.tyiu.corn.model.dto.RatingDTO;
import com.tyiu.corn.model.enums.StatusIdea;
import lombok.RequiredArgsConstructor;

import lombok.extern.slf4j.Slf4j;
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
@Slf4j
@CacheConfig(cacheNames = "ideas")
public class IdeaService {

    private final IdeaRepository ideaRepository;
    private final ModelMapper mapper;
    @Cacheable
    public Mono<IdeaDTO> getIdea(String id) {
        return ideaRepository.findById(id).flatMap(i -> Mono.just(mapper.map(i, IdeaDTO.class)));
    }
    @Cacheable
    public Flux<IdeaDTO> getListIdea() {
        return ideaRepository.findAll().flatMap(i -> Flux.just(mapper.map(i, IdeaDTO.class)));
    }
    @CacheEvict(allEntries = true)
    public Mono<IdeaDTO> saveIdea(IdeaDTO ideaDTO, String initiator) {
        ideaDTO.setCreatedAt(new Date());
        ideaDTO.setModifiedAt(new Date());
        ideaDTO.setInitiator(initiator);
        ideaDTO.setStatus(StatusIdea.NEW);
        return ideaRepository.save(mapper.map(ideaDTO, Idea.class)).flatMap(savedIdea ->
                Mono.just(mapper.map(savedIdea, IdeaDTO.class)));
    }
    @CacheEvict(allEntries = true)
    public Mono<Void> deleteIdea(String id) {
        ideaRepository.deleteById(id).subscribe();
        return Mono.empty();
    }
    @CacheEvict(allEntries = true)
    public Mono<Void> updateStatusByInitiator (String id, String initiator){
        ideaRepository.findById(id).subscribe(i -> {
            if (initiator.equals(i.getInitiator())) {
                i.setStatus(StatusIdea.ON_APPROVAL);
                ideaRepository.save(i).subscribe();
            }
        });
        return Mono.empty();
    }
    @CacheEvict(allEntries = true)
    public Mono<Void> updateIdeaByInitiator(String id, IdeaDTO updatedIdea) {
        ideaRepository.findById(id).subscribe(i -> {
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
            i.setPreAssessment(updatedIdea.getPreAssessment());
            i.setModifiedAt(new Date());
            ideaRepository.save(i).subscribe();
        });
        return Mono.empty();
    }
    @CacheEvict(allEntries = true)
    public Mono<Void> updateStatusByProjectOffice (String id, StatusIdea newStatus){
        ideaRepository.findById(id).subscribe(i -> {
            i.setStatus(newStatus);
            ideaRepository.save(i).subscribe();
        });
        return Mono.empty();
    }
    @CacheEvict(allEntries = true)
    public Mono<Void> updateStatusByExpert(String id, RatingDTO ratingDTO){
        ideaRepository.findById(id).subscribe(i -> {
            if (ratingDTO.getStatus() == StatusIdea.ON_EDITING) {
                i.setStatus(ratingDTO.getStatus());
                ideaRepository.save(i).subscribe();
            }
            else {
                i.setStatus(ratingDTO.getStatus());
                i.setRating(ratingDTO.getRating());
                i.setOriginality(ratingDTO.getOriginality());
                i.setTechnicalRealizability(ratingDTO.getTechnicalRealizability());
                i.setSuitability(ratingDTO.getSuitability());
                ideaRepository.save(i).subscribe();
            }
        });
        return Mono.empty();
    }
    @CacheEvict(allEntries = true)
    public Mono<Void>  updateIdeaByAdmin(String id, IdeaDTO updatedIdea) {
        ideaRepository.findById(id).subscribe(i -> {
            i.setName(updatedIdea.getName());
            i.setProjectType(updatedIdea.getProjectType());
            i.setExperts(updatedIdea.getExperts());
            i.setProblem(updatedIdea.getProblem());
            i.setSolution(updatedIdea.getSolution());
            i.setResult(updatedIdea.getResult());
            i.setCustomer(updatedIdea.getCustomer());
            i.setDescription(updatedIdea.getDescription());
            i.setSuitability(updatedIdea.getSuitability());
            i.setStatus(updatedIdea.getStatus());
            i.setRating(updatedIdea.getRating());
            i.setRisk(updatedIdea.getRisk());
            i.setModifiedAt(new Date());
            ideaRepository.save(i).subscribe();
        });
        return Mono.empty();
    }
}