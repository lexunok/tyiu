package com.tyiu.corn.service;

import java.time.Duration;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Date;
import java.util.concurrent.TimeUnit;

import com.tyiu.corn.model.dto.IdeaDTO;
import com.tyiu.corn.model.entities.Idea;
import com.tyiu.corn.model.dto.RatingDTO;
import com.tyiu.corn.model.enums.StatusIdea;
import com.tyiu.corn.util.redis.RedisCacheTemplate;
import lombok.RequiredArgsConstructor;

import org.modelmapper.ModelMapper;
import org.springframework.cache.annotation.CacheConfig;
import org.springframework.data.redis.core.ReactiveRedisTemplate;
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
    private final ReactiveRedisTemplate<String, Object> reactiveRedisTemplate;
    private final RedisCacheTemplate cache;

    public Flux<IdeaDTO> getListIdeaForInitiator(String initiator) {
        Flux<Idea> ideas = ideaRepository.findAllByInitiator(initiator);
        return ideas.flatMap(i -> Flux.just(mapper.map(i, IdeaDTO.class)));
    }

    public Mono<IdeaDTO> getIdeaForInitiator(String id) {
        return cache.get("IDEA", id).flatMap(ideaFromRedis -> Mono.just(mapper.map(ideaFromRedis, IdeaDTO.class)))
                .switchIfEmpty(Mono.defer(() -> ideaRepository.findById(id)
                        .flatMap(ideaFromRepo -> {
                            IdeaDTO ideaDTO = mapper.map(ideaFromRepo, IdeaDTO.class);
                            cache.set("IDEA_" + id, ideaDTO);
                            return Mono.just(ideaDTO);
                        })));
    }

    public Flux<IdeaDTO> getListIdea() {
        Flux<Idea> ideas = ideaRepository.findAll();
        return ideas.flatMap(i -> Flux.just(mapper.map(i, IdeaDTO.class)));
    }

    public Mono<IdeaDTO> saveIdea(IdeaDTO ideaDTO, String initiator) {
        ideaDTO.setCreatedAt(new Date());
        ideaDTO.setModifiedAt(new Date());
        ideaDTO.setInitiator(initiator);
        ideaDTO.setStatus(StatusIdea.NEW);

        Mono<Idea> ideaMono = ideaRepository.save(mapper.map(ideaDTO, Idea.class));

        return ideaMono.flatMap(savedIdea -> {
            IdeaDTO savedIdeaDTO = mapper.map(savedIdea, IdeaDTO.class);

            cache.set("IDEA_" + savedIdeaDTO.getId(), savedIdeaDTO);
            return Mono.just(savedIdeaDTO);
        });
    }

    public void deleteIdeaByInitiator(String id) {
        ideaRepository.deleteById(id).doOnNext(ide -> reactiveRedisTemplate.opsForValue().delete("IDEA_" + id).subscribe()).subscribe();
    }

    public void deleteIdeaByAdmin(String id) {
        ideaRepository.deleteById(id).doOnNext(ide -> reactiveRedisTemplate.opsForValue().delete("IDEA_" + id).subscribe()).subscribe();
    }

    public void updateStatusByInitiator (String id){
        Mono<Idea> idea = reactiveRedisTemplate.opsForValue().get("IDEA_" + id)
                .flatMap(i -> Mono.justOrEmpty(i)
                        .switchIfEmpty(ideaRepository.findById(id))
                ).cast(Idea.class);
        idea.flatMap(i -> {
            i.setStatus(StatusIdea.ON_APPROVAL);
            return ideaRepository.save(i).doOnNext(ide -> reactiveRedisTemplate.opsForValue().set("IDEA_" + id, ide, 600).subscribe());
        }).subscribe();
    }

    public void updateIdeaByInitiator(String id, IdeaDTO updatedIdea) {
        Mono<Idea> idea = reactiveRedisTemplate.opsForValue().get("IDEA_" + id)
                .flatMap(i -> Mono.justOrEmpty(i)
                        .switchIfEmpty(ideaRepository.findById(id))
                ).cast(Idea.class);
        idea.flatMap(i -> {
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
            return ideaRepository.save(i).doOnNext(ide -> reactiveRedisTemplate.opsForValue().set("IDEA_" + id, ide, 600).subscribe());
        }).subscribe();
    }

    public void updateStatusByProjectOffice (String id, StatusIdea newStatus){
        Mono<Idea> idea = reactiveRedisTemplate.opsForValue().get("IDEA_" + id)
                .flatMap(i -> Mono.justOrEmpty(i)
                        .switchIfEmpty(ideaRepository.findById(id))
                ).cast(Idea.class);
        idea.flatMap(i ->{
            i.setStatus(newStatus);
            return ideaRepository.save(i).doOnNext(ide -> reactiveRedisTemplate.opsForValue().set("IDEA_" + id, ide, 600).subscribe());
        }).subscribe();
    }

    public void updateStatusByExpert(String id, RatingDTO ratingDTO, String email){
        Mono<Idea> idea = reactiveRedisTemplate.opsForValue().get("IDEA_" + id)
                .flatMap(i -> Mono.justOrEmpty(i)
                        .switchIfEmpty(ideaRepository.findById(id))
                ).cast(Idea.class);
        idea.flatMap(i -> {
            if (ratingDTO.getStatus() == StatusIdea.ON_EDITING)
            {
                i.setStatus(ratingDTO.getStatus());
                return ideaRepository.save(i).doOnNext(ide -> reactiveRedisTemplate.opsForValue().set("IDEA_" + id, ide, 600).subscribe());
            }
            i.setStatus(ratingDTO.getStatus());
            i.setRating(ratingDTO.getRating());
            i.setOriginality(ratingDTO.getOriginality());
            i.setTechnicalRealizability(ratingDTO.getTechnicalRealizability());
            i.setSuitability(ratingDTO.getSuitability());
            return ideaRepository.save(i).doOnNext(ide -> reactiveRedisTemplate.opsForValue().set("IDEA_" + id, ide, 600).subscribe());
        }).subscribe();
    }

    public void updateIdeaByAdmin(String id, IdeaDTO updatedIdea) {
        Mono<Idea> idea = reactiveRedisTemplate.opsForValue().get("IDEA_" + id)
                .flatMap(i -> Mono.justOrEmpty(i)
                        .switchIfEmpty(ideaRepository.findById(id))
                ).cast(Idea.class);
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
            i.setStatus(updatedIdea.getStatus());
            i.setRating(updatedIdea.getRating());
            i.setRisk(updatedIdea.getRisk());
            i.setModifiedAt(new Date());
            return ideaRepository.save(i).doOnNext(ide -> reactiveRedisTemplate.opsForValue().set("IDEA_" + id, ide, 600).subscribe());
        }).subscribe();
    }
}