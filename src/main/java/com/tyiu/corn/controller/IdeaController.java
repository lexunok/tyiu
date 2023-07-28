package com.tyiu.corn.controller;

import com.tyiu.corn.model.Idea;
import com.tyiu.corn.service.IdeaService;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping("/api/v1/idea")
@RequiredArgsConstructor
public class IdeaController {
    private final IdeaService ideaService;

    @GetMapping("/")
    public List<Idea> listIdea() {
        return ideaService.listIdea();
    }
    @PostMapping("/add")
    public void addTask(@RequestBody Idea idea) {
        ideaService.saveIdea(idea);
    }

    @DeleteMapping("/delete/{id}")
    public void delTask(@PathVariable Long id) {
        ideaService.deleteIdea(id);
    }

    @PutMapping("/update/{id}")
    public void updateTask(@PathVariable Long id, @RequestBody Idea updatedIdea) {
        ideaService.updateIdea(id, updatedIdea);
    }
}