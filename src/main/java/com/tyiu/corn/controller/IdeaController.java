package com.tyiu.corn.controller;

import org.springframework.web.bind.annotation.*;
import com.tyiu.corn.service.IdeaService;
import com.tyiu.corn.model.Idea;
import lombok.RequiredArgsConstructor;

import java.util.List;

@RestController
@RequestMapping("/api/v1/idea")
@RequiredArgsConstructor
public class IdeaController {
    
    private final IdeaService ideaService;

    @GetMapping("/get")
    public List<Idea> getIdea(){
        return ideaService.listIdea();
    }

    @PostMapping("/add")
    public void addScram(@RequestBody Idea idea) {
        ideaService.saveIdea(idea);
    }

    @DeleteMapping("/delete/{id}")
    public void deleteIdea(@PathVariable Long id) {
        ideaService.deleteIdea(id);
    }

    @PutMapping("/update/{id}")
    public void updateIdea(@PathVariable Long id, @RequestBody Idea updatedIdea) {
        ideaService.updateIdea(id, updatedIdea);
    }
}
