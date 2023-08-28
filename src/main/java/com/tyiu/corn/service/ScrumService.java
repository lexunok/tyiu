package com.tyiu.corn.service;

import com.tyiu.corn.model.dto.ScrumDTO;
import com.tyiu.corn.model.entities.Scrum;
import com.tyiu.corn.repository.ScrumRepository;
import lombok.RequiredArgsConstructor;

import org.modelmapper.ModelMapper;
import org.springframework.cache.annotation.CacheConfig;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.util.List;
@Service
@RequiredArgsConstructor
@CacheConfig(cacheNames = "scrums")
public class ScrumService {
    private final ScrumRepository scrumRepository;
    private final ModelMapper mapper;

    //@Cacheable
    public Flux<ScrumDTO> getListScrum() {
        List<Scrum> scrums = scrumRepository.findAll();
        return Flux.just(scrums).map(s -> mapper.map(s, ScrumDTO.class));
    }

//    @CacheEvict(allEntries = true)
    public Mono<ScrumDTO> saveScrum(ScrumDTO scrumDTO) {
       Scrum scrum = scrumRepository.save(mapper.map(scrumDTO, Scrum.class));
       return Mono.just(mapper.map(scrum, ScrumDTO.class));
    }
//    @CacheEvict(allEntries = true)
    public void deleteScrum(Long id) {
        scrumRepository.deleteById(id);
    }
//    @CacheEvict(allEntries = true)
    public void updateScrum(Long id, ScrumDTO updatedScrum) {
        Scrum scrum = scrumRepository.findById(id).orElseThrow();
        scrum.setDescription(updatedScrum.getDescription());
        scrum.setName(updatedScrum.getName());
        scrumRepository.save(scrum);
    }
}
