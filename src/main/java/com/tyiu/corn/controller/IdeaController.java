package com.tyiu.corn.controller;

import com.tyiu.corn.model.dto.RiskDTO;
import com.tyiu.corn.model.entities.Comment;
import com.tyiu.corn.model.enums.StatusIdea;
import org.springframework.context.annotation.Role;
import org.springframework.security.access.annotation.Secured;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.Authentication;
import org.springframework.web.bind.annotation.*;
import com.tyiu.corn.service.IdeaService;
import com.tyiu.corn.model.entities.Idea;
import lombok.RequiredArgsConstructor;

import java.security.Principal;
import java.util.List;

@RestController
@RequestMapping("/api/v1/idea")
@RequiredArgsConstructor
public class IdeaController {
    
    private final IdeaService ideaService;


    @GetMapping("/initiator")
    public List<Idea> showListIdeaForInitiator(Principal principal){
        return ideaService.getListIdeaForInitiator(principal.getName());
    }

    @GetMapping("/project-office")
    public List<Idea> showListIdeaForProjectOffice(@RequestBody StatusIdea status){
        return ideaService.getListIdeaByStatus(status);
    }

    @GetMapping("/expert")
    public List<Idea> showListIdeaForExpert(@RequestBody StatusIdea status){
        return ideaService.getListIdeaByStatus(status);
    }

    @GetMapping("/admin")
    public List<Idea> showListIdeaForAdmin(){
        return ideaService.getListIdea();
    }

    @PostMapping("/initiator/add")
    public Idea addIdea(@RequestBody Idea idea, Principal principal) {
        return ideaService.saveIdea(idea, principal.getName());
    }

    @DeleteMapping("/initiator/delete/{ideaId}")
    public void deleteIdeaByInitiator(@PathVariable Long ideaId, Principal principal) {
        ideaService.deleteIdeaByInitiator(ideaId, principal.getName());
    }

    @DeleteMapping("/admin/delete/{ideaId}")
    public void deleteIdeaByAdmin(@PathVariable Long ideaId) {
        ideaService.deleteIdeaByAdmin(ideaId);
    }

    @PutMapping("/admin/update/{ideaId}")
    public void updateIdeaByAdmin(@PathVariable Long ideaId, @RequestBody Idea updatedIdea) {
        ideaService.updateIdeaByAdmin(ideaId, updatedIdea);
    }


    @PutMapping("/project-office/update/{ideaId}")
    public void updateStatusIdeaByProjectOffice(@PathVariable Long ideaId, @RequestBody StatusIdea newStatus){
        ideaService.updateStatusByProjectOffice(ideaId, newStatus);
    }

    @PutMapping("/expert/update/{ideaId}")
    public void updateStatusByExpert(@PathVariable Long ideaId, @RequestBody RiskDTO riskDTO){
        ideaService.updateStatusByExpert(ideaId, riskDTO);
    }
}
