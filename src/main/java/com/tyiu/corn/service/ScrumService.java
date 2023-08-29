package com.tyiu.corn.service;

import com.tyiu.corn.model.dto.ScrumDTO;
import com.tyiu.corn.model.entities.Scrum;
import com.tyiu.corn.repository.ScrumRepository;
import lombok.RequiredArgsConstructor;

import org.springframework.stereotype.Service;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.util.List;
@Service
@RequiredArgsConstructor
//@CacheConfig(cacheNames = "scrums")
public class ScrumService {
    private final ScrumRepository scrumRepository;

    //@Cacheable
    public Flux<ScrumDTO> getListScrum() {
        return scrumRepository.findAll().cast(ScrumDTO.class);
    }

//    @CacheEvict(allEntries = true)
    public Mono<ScrumDTO> saveScrum(ScrumDTO scrumDTO) {
        return  Mono.just(scrumDTO).cast(Scrum.class).flatMap(scrumRepository::save).cast(ScrumDTO.class);
    }
//    @CacheEvict(allEntries = true)
    public void deleteScrum(Long id) {
        scrumRepository.deleteById(id);
    }
//    @CacheEvict(allEntries = true)
    public void updateScrum(Long id, ScrumDTO updatedScrum) {
        Mono<Scrum> scrum = scrumRepository.findById(id);
        scrum.flatMap(s -> {
            s.setDescription(updatedScrum.getDescription());
            s.setName(updatedScrum.getName());
            return scrumRepository.save(s);
        });
    }
}
