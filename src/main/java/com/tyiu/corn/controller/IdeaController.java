package com.tyiu.corn.controller;

import com.tyiu.corn.model.entities.Comment;
import com.tyiu.corn.model.enums.StatusIdea;
import org.springframework.web.bind.annotation.*;
import com.tyiu.corn.service.IdeaService;
import com.tyiu.corn.model.entities.Idea;
import lombok.RequiredArgsConstructor;

import java.util.List;

@RestController
@RequestMapping("/api/v1/idea")
@RequiredArgsConstructor
public class IdeaController {
    
    private final IdeaService ideaService;

    @PostMapping("/add")
    public Idea addIdea(@RequestBody Idea idea) {
        return ideaService.saveIdea(idea);
    }

    @DeleteMapping("/delete/{id}")
    public void deleteIdea(@PathVariable Long id) {
        ideaService.deleteIdea(id);
    }

    @PutMapping("/admin/update/{ideaId}")
    public void updateIdeaByAdmin(@PathVariable Long ideaId, @RequestBody Idea updatedIdea) {
        ideaService.updateIdeaByAdmin(ideaId, updatedIdea);
    }

    @PutMapping("/admin/{ideaId}/add-comment")
    public void createCommentAdmin(@PathVariable Long ideaId, @RequestBody Comment comment, @RequestBody String commentByAdmin){
        ideaService.createComment(ideaId, comment, commentByAdmin);
    }

    @PutMapping("/project-office/update-status/{ideaId}")
    public void updateStatusIdeaByProjectOffice(@PathVariable Long ideaId, @RequestBody StatusIdea newStatus){
        ideaService.updateStatusByProjectOffice(ideaId, newStatus);
    }

    @PutMapping("/project-office/{ideaId}/add-comment")
    public void createCommentProjectOffice(@PathVariable Long ideaId, @RequestBody Comment comment, @RequestBody String commentByProjectOffice){
        ideaService.createComment(ideaId, comment, commentByProjectOffice);
    }

    @PutMapping("/expert/update-status/{ideaId}")
    public void updateStatusByExpert(@PathVariable Long ideaId, @RequestBody StatusIdea newStatus){
        ideaService.updateStatusByExpert(ideaId, newStatus);
    }

    @PutMapping("expert/add-risk/{ideaId}")
    public void addRisk(@PathVariable Long ideaId, @RequestBody double riskValue){
        ideaService.updateRiskByExpert(ideaId, riskValue);
    }



}
