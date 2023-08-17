package com.tyiu.corn.controller;

import com.tyiu.corn.model.dto.RiskDTO;
import com.tyiu.corn.model.enums.StatusIdea;
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
    public List<Idea> showListIdeaForProjectOffice(){
        return ideaService.getListIdeaOnConfirmation();
    }

    @GetMapping("/expert")
    public List<Idea> showListIdeaForExpert(){
        return ideaService.getListIdeaOnApproval();
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

    @PutMapping("/initiator/update/{ideaId}")
    public void updateIdeaByInitiator(@PathVariable Long ideaId, Principal principal, @RequestBody Idea updatedIdea) {
        ideaService.updateIdeaByInitiator(ideaId, principal.getName(), updatedIdea);
    }

    @PutMapping("/initiator/send/{ideaId}")
    public void updateStatusByInitiator(@PathVariable Long ideaId, Principal principal) {
        ideaService.updateStatusByInitiator(ideaId, principal.getName());
    }

    @PutMapping("/project-office/update/{ideaId}")
    public void updateStatusIdeaByProjectOffice(@PathVariable Long ideaId, @RequestBody StatusIdea newStatus){
        ideaService.updateStatusByProjectOffice(ideaId, newStatus);
    }

    @PutMapping("/expert/update/{ideaId}")
    public void updateStatusByExpert(@PathVariable Long ideaId, @RequestBody RiskDTO riskDTO){
        ideaService.updateStatusByExpert(ideaId, riskDTO);
    }

    @PutMapping("/admin/update/{ideaId}")
    public void updateIdeaByAdmin(@PathVariable Long ideaId, @RequestBody Idea updatedIdea) {
        ideaService.updateIdeaByAdmin(ideaId, updatedIdea);
    }
}
