package com.tyiu.corn.controller;

import com.tyiu.corn.model.dto.IdeaDTO;
import com.tyiu.corn.model.dto.ScrumDTO;
import com.tyiu.corn.model.entities.Scrum;
import com.tyiu.corn.service.ScrumService;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.util.List;
@RestController
@RequestMapping("/api/v1/scrum")
@RequiredArgsConstructor
public class ScrumController {

    private final ScrumService scrumService;

    @GetMapping("/all")
    public Flux<ScrumDTO> showListScrum() {
        return scrumService.getListScrum();
    }

    @PostMapping("/add")
    public Mono<ScrumDTO> addScrum(@RequestBody ScrumDTO scrum) {
        return scrumService.saveScrum(scrum);
    }

    @DeleteMapping("/delete/{id}")
    public Mono<Void> deleteScrum(@PathVariable Long id) {
        scrumService.deleteScrum(id);
        return Mono.empty();
    }

    @PutMapping("/update/{id}")
    public Mono<Void> updateScrum(@PathVariable Long scrumId, @RequestBody ScrumDTO updatedScrum) {
        scrumService.updateScrum(scrumId, updatedScrum);
        return Mono.empty();
    }
}
