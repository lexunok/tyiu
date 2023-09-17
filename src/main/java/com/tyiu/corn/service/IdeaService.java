package com.tyiu.corn.service;

import java.util.Date;

import com.tyiu.corn.model.dto.IdeaDTO;
import com.tyiu.corn.model.entities.Idea;
import com.tyiu.corn.model.dto.RatingDTO;
import com.tyiu.corn.model.enums.StatusIdea;
import com.tyiu.corn.util.redis.RedisCacheTemplate;
import lombok.RequiredArgsConstructor;

import org.modelmapper.ModelMapper;
import org.springframework.cache.annotation.CacheConfig;
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
    private final RedisCacheTemplate cache;
    private final String ideaKey = "IDEA";
    private final String ideasKey = "IDEAS";

    private Mono<Void> updateIDEAS(){
        cache.delete(ideasKey);
        ideaRepository.findAll().flatMap(ideasFromRepo -> {
            Flux.just(mapper.map(ideasFromRepo, IdeaDTO.class)).collectList().subscribe(list -> cache.setList(ideasKey, list));
            return Flux.empty();
        }).subscribe();
        return Mono.empty();
    }

    public Mono<IdeaDTO> getIdeaForInitiator(String id) {
        return cache.get(ideaKey, id).flatMap(ideaFromRedis -> Mono.just(mapper.map(ideaFromRedis, IdeaDTO.class)))
                .switchIfEmpty(Mono.defer(() -> ideaRepository.findById(id)
                        .flatMap(ideaFromRepo -> {
                            IdeaDTO ideaDTO = mapper.map(ideaFromRepo, IdeaDTO.class);
                            cache.set(ideaKey, id, ideaDTO);
                            return Mono.just(ideaDTO);
                        })));
    }

    public Flux<IdeaDTO> getListIdea() {
        return cache.getList(ideasKey).flatMap(ideasFromRedis -> Flux.just(mapper.map(ideasFromRedis, IdeaDTO.class)))
                .switchIfEmpty(Flux.defer(() -> ideaRepository.findAll()
                        .flatMap(ideasFromRepo -> {
                            Flux<IdeaDTO> ideas = Flux.just(mapper.map(ideasFromRepo, IdeaDTO.class));
                            ideas.collectList().subscribe(list -> cache.setList(ideasKey, list));
                            return ideas;
                        })));
    }

    public Mono<IdeaDTO> saveIdea(IdeaDTO ideaDTO, String initiator) {
        ideaDTO.setCreatedAt(new Date());
        ideaDTO.setModifiedAt(new Date());
        ideaDTO.setInitiator(initiator);
        ideaDTO.setStatus(StatusIdea.NEW);
        return ideaRepository.save(mapper.map(ideaDTO, Idea.class)).flatMap(savedIdea -> {
            IdeaDTO savedIdeaDTO = mapper.map(savedIdea, IdeaDTO.class);
            ideaRepository.save(savedIdea).subscribe();
            cache.set(ideaKey, savedIdea.getId(), savedIdeaDTO);
            updateIDEAS().subscribe();
            return Mono.just(savedIdeaDTO);
        });
    }

    public Mono<Void> deleteIdeaByInitiator(String id) {
        cache.delete(ideaKey + "_" + id);
        ideaRepository.deleteById(id).subscribe();
        return updateIDEAS();
    }

    public Mono<Void> deleteIdeaByAdmin(String id) {
        cache.delete(ideaKey + "_" + id);
        ideaRepository.deleteById(id).subscribe();
        return updateIDEAS();
    }

    public void updateStatusByInitiator (String id){
        cache.get(ideaKey, id).flatMap(ideaFromRedis -> Mono.just(mapper.map(ideaFromRedis, Idea.class)))
                .switchIfEmpty(Mono.defer(() -> ideaRepository.findById(id)))
        .flatMap(i -> {
            i.setStatus(StatusIdea.ON_APPROVAL);
            ideaRepository.save(i).subscribe();
            cache.set(ideaKey, i.getId(), mapper.map(i, IdeaDTO.class));
            return updateIDEAS();
        }).subscribe();
    }

    public void updateIdeaByInitiator(String id, IdeaDTO updatedIdea) {
        cache.get(ideaKey, id).flatMap(ideaFromRedis -> Mono.just(mapper.map(ideaFromRedis, Idea.class)))
                .switchIfEmpty(Mono.defer(() -> ideaRepository.findById(id)))
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
            i.setPreAssessment(updatedIdea.getPreAssessment());
            i.setModifiedAt(new Date());
            ideaRepository.save(i).subscribe();
            cache.set(ideaKey, i.getId(), mapper.map(i, IdeaDTO.class));
            return updateIDEAS();
        }).subscribe();
    }

    public void updateStatusByProjectOffice (String id, StatusIdea newStatus){
        cache.get(ideaKey, id).flatMap(ideaFromRedis -> Mono.just(mapper.map(ideaFromRedis, Idea.class)))
                .switchIfEmpty(Mono.defer(() -> ideaRepository.findById(id)))
        .flatMap(i ->{
            i.setStatus(newStatus);
            ideaRepository.save(i).subscribe();
            cache.set(ideaKey, i.getId(), mapper.map(i, IdeaDTO.class));
            return updateIDEAS();
        }).subscribe();
    }

    public void updateStatusByExpert(String id, RatingDTO ratingDTO){
        cache.get(ideaKey, id).flatMap(ideaFromRedis -> Mono.just(mapper.map(ideaFromRedis, Idea.class)))
                .switchIfEmpty(Mono.defer(() -> ideaRepository.findById(id)))
        .flatMap(i -> {
            if (ratingDTO.getStatus() == StatusIdea.ON_EDITING) {
                i.setStatus(ratingDTO.getStatus());
                ideaRepository.save(i).subscribe();
                cache.set(ideaKey, i.getId(), mapper.map(i, IdeaDTO.class));
                return updateIDEAS();
            }
            i.setStatus(ratingDTO.getStatus());
            i.setRating(ratingDTO.getRating());
            i.setOriginality(ratingDTO.getOriginality());
            i.setTechnicalRealizability(ratingDTO.getTechnicalRealizability());
            i.setSuitability(ratingDTO.getSuitability());
            ideaRepository.save(i).subscribe();
            cache.set(ideaKey, i.getId(), mapper.map(i, IdeaDTO.class));
            return updateIDEAS();
        }).subscribe();
    }

    public void updateIdeaByAdmin(String id, IdeaDTO updatedIdea) {
        cache.get(ideaKey, id).flatMap(ideaFromRedis -> Mono.just(mapper.map(ideaFromRedis, Idea.class)))
                .switchIfEmpty(Mono.defer(() -> ideaRepository.findById(id)))
        .flatMap(i -> {
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
            cache.set(ideaKey, i.getId(), mapper.map(i, IdeaDTO.class));
            return updateIDEAS();
        }).subscribe();
    }
}