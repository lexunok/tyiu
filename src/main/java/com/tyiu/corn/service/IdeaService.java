package com.tyiu.corn.service;

import java.util.Date;

import com.tyiu.corn.model.dto.IdeaDTO;
import com.tyiu.corn.model.entities.Idea;
import com.tyiu.corn.model.dto.RatingDTO;
import com.tyiu.corn.model.entities.IdeaStack;
import com.tyiu.corn.model.entities.Skill;
import com.tyiu.corn.model.enums.StatusIdea;
import com.tyiu.corn.repository.IdeaStackRepository;
import com.tyiu.corn.util.redis.RedisCacheTemplate;
import lombok.RequiredArgsConstructor;

import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.springframework.stereotype.Service;

import com.tyiu.corn.repository.IdeaRepository;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;


@Service
@RequiredArgsConstructor
@Slf4j
public class IdeaService {

    private final IdeaRepository ideaRepository;
    private final IdeaStackRepository ideaStackRepository;
    private final ModelMapper mapper;
    private final RedisCacheTemplate cache;
    private static final String IDEA = "IDEA";
    private static final String IDEAS = "IDEAS";


    public Mono<IdeaDTO> getIdea(String id) {
        return cache.get(IDEA,id).cast(IdeaDTO.class)
                .switchIfEmpty(Mono.defer(() ->
                        ideaRepository
                            .findById(id)
                            .flatMap(i -> {
                                IdeaDTO ideaDTO = mapper.map(i, IdeaDTO.class);
                                cache.set(IDEA,id,ideaDTO).subscribe();
                                return Mono.just(ideaDTO);
                            })
                )
        );
    }

    public Flux<IdeaDTO> getListIdea() {
        return cache.getList(IDEAS)
                .flatMap(ideasFromRedis -> Flux.just(mapper.map(ideasFromRedis, IdeaDTO.class)))
                .switchIfEmpty(Flux.defer(() -> ideaRepository.findAll()
                        .flatMap(ideasFromRepo -> {
                            Flux<IdeaDTO> ideas = Flux.just(mapper.map(ideasFromRepo, IdeaDTO.class));
                            cache.setList(IDEAS,ideas);
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

            ideaDTO.getStack().forEach(skill -> {
                IdeaStack ideaStack = IdeaStack.builder()
                        .ideaId(savedIdea.getId())
                        .skillId(skill.getId())
                        .build();
                ideaStackRepository.save(ideaStack).subscribe();
            });
            //cache.set(IDEA, savedIdea.getId(), savedIdeaDTO);
            return Mono.just(savedIdeaDTO);
        });
    }

    public Mono<Void> deleteIdeaByInitiator(String id) {
        cache.delete(IDEA + "_" + id);
        ideaRepository.deleteById(id).subscribe();
        return Mono.empty();
    }

    public Mono<Void> deleteIdeaByAdmin(String id) {
        cache.delete(IDEA + "_" + id);
        ideaRepository.deleteById(id).subscribe();
        return Mono.empty();
    }

    public void updateStatusByInitiator (String id){
        cache.get(IDEA, id).flatMap(ideaFromRedis -> Mono.just(mapper.map(ideaFromRedis, Idea.class)))
                .switchIfEmpty(Mono.defer(() -> ideaRepository.findById(id)))
                .flatMap(i -> {
                    i.setStatus(StatusIdea.ON_APPROVAL);
                    ideaRepository.save(i).subscribe();
                    //cache.set(IDEA, i.getId(), mapper.map(i, IdeaDTO.class));
                    return Mono.empty();
                }).subscribe();
    }

    public void updateIdeaByInitiator(String id, IdeaDTO updatedIdea) {
        cache.get(IDEA, id).flatMap(ideaFromRedis -> Mono.just(mapper.map(ideaFromRedis, Idea.class)))
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
                    //cache.set(IDEA, i.getId(), mapper.map(i, IdeaDTO.class));
                    return Mono.empty();
                }).subscribe();
    }

    public void updateStatusByProjectOffice (String id, StatusIdea newStatus){
        cache.get(IDEA, id).flatMap(ideaFromRedis -> Mono.just(mapper.map(ideaFromRedis, Idea.class)))
                .switchIfEmpty(Mono.defer(() -> ideaRepository.findById(id)))
                .flatMap(i ->{
                    i.setStatus(newStatus);
                    ideaRepository.save(i).subscribe();
                    //cache.set(IDEA, i.getId(), mapper.map(i, IdeaDTO.class));
                    return Mono.empty();
                }).subscribe();
    }

    public void updateStatusByExpert(String id, RatingDTO ratingDTO){
        cache.get(IDEA, id).flatMap(ideaFromRedis -> Mono.just(mapper.map(ideaFromRedis, Idea.class)))
                .switchIfEmpty(Mono.defer(() -> ideaRepository.findById(id)))
                .flatMap(i -> {
                    if (ratingDTO.getStatus() == StatusIdea.ON_EDITING) {
                        i.setStatus(ratingDTO.getStatus());
                        ideaRepository.save(i).subscribe();
                        //cache.set(IDEA, i.getId(), mapper.map(i, IdeaDTO.class));
                        return Mono.empty();
                    }
                    i.setStatus(ratingDTO.getStatus());
                    i.setRating(ratingDTO.getRating());
                    i.setOriginality(ratingDTO.getOriginality());
                    i.setTechnicalRealizability(ratingDTO.getTechnicalRealizability());
                    i.setSuitability(ratingDTO.getSuitability());
                    ideaRepository.save(i).subscribe();
                    //cache.set(IDEA, i.getId(), mapper.map(i, IdeaDTO.class));
                    return Mono.empty();
                }).subscribe();
    }

    public void updateIdeaByAdmin(String id, IdeaDTO updatedIdea) {
        cache.get(IDEA, id).flatMap(ideaFromRedis -> Mono.just(mapper.map(ideaFromRedis, Idea.class)))
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
                    //cache.set(IDEA, i.getId(), mapper.map(i, IdeaDTO.class));
                    return Mono.empty();
                }).subscribe();
    }
}