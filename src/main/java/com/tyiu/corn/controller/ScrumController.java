package com.tyiu.corn.controller;

import com.tyiu.corn.model.Scrum;
import com.tyiu.corn.service.ScrumService;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

import java.util.List;
@RestController
@RequestMapping("/api/v1/scrum")
@RequiredArgsConstructor
public class ScrumController {
    private final ScrumService scrumService;

    @GetMapping
    public List<Scrum> showListScrum() {
        return scrumService.getListScrum();
    }

    @PostMapping("/add")
    public void addScrum(@RequestBody Scrum scrum) {
        scrumService.saveScrum(scrum);
    }

    @DeleteMapping("/delete/{id}")
    public void deleteScrum(@PathVariable Long id) {
        scrumService.deleteScrum(id);
    }

    @PutMapping("/update/{id}")
    public void updateScrum(@PathVariable Long id, @RequestBody Scrum updatedScrum) {
        scrumService.updateScrum(id, updatedScrum);
    }
}
