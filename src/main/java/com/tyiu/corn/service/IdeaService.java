package com.tyiu.corn.service;

import java.util.Date;

import com.tyiu.corn.model.dto.IdeaDTO;
import com.tyiu.corn.model.entities.Idea;
import com.tyiu.corn.model.entities.Rating;
import com.tyiu.corn.model.enums.StatusIdea;
import com.tyiu.corn.model.requests.StatusIdeaRequest;
import com.tyiu.corn.repository.RatingRepository;
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
    private final RatingRepository ratingRepository;
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
                {
                    savedIdea.getExperts().getUsers()
                            .forEach(r -> ratingRepository.save(Rating.builder()
                                    .expert(r.getEmail())
                                    .ideaId(savedIdea.getId())
                                    .confirmed(false)
                                    .build())
                                    .subscribe());
                    return Mono.just(mapper.map(savedIdea, IdeaDTO.class));
                });
    }
    @CacheEvict(allEntries = true)
    public Mono<Void> deleteIdea(String id) {
        ideaRepository.deleteById(id).subscribe();
        return Mono.empty();
    }
    @CacheEvict(allEntries = true)
    public void updateStatusByInitiator (String id, String initiator){
        ideaRepository.findById(id).flatMap(i -> {
            if (initiator.equals(i.getInitiator())) {
                i.setStatus(StatusIdea.ON_APPROVAL);
                ideaRepository.save(i).subscribe();
                return Mono.empty();
            }
            return Mono.empty();
        }).subscribe();
    }
    @CacheEvict(allEntries = true)
    public void updateIdeaByInitiator(String id, IdeaDTO updatedIdea) {
        ideaRepository.findById(id).flatMap(i -> {
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
            return Mono.empty();
        }).subscribe();
    }
    @CacheEvict(allEntries = true)
    public void updateStatusByProjectOffice(String id, StatusIdeaRequest newStatus){
        ideaRepository.findById(id).flatMap(i -> {
            i.setStatus(newStatus.getStatus());
            ideaRepository.save(i).subscribe();
            return Mono.empty();
        }).subscribe();
    }

    @CacheEvict(allEntries = true)
    public void updateIdeaByAdmin(String id, IdeaDTO updatedIdea) {
        ideaRepository.findById(id).flatMap(i -> {
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
            i.setTechnicalRealizability(updatedIdea.getTechnicalRealizability());
            i.setStatus(updatedIdea.getStatus());
            i.setRating(updatedIdea.getRating());
            i.setModifiedAt(new Date());
            ideaRepository.save(i).subscribe();
            return Mono.empty();
        }).subscribe();
    }
}