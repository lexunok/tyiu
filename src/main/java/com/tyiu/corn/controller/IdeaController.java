package com.tyiu.corn.controller;

import com.tyiu.corn.model.dto.IdeaDTO;
import com.tyiu.corn.model.dto.RiskDTO;
import com.tyiu.corn.model.enums.StatusIdea;

import org.springframework.web.bind.annotation.*;
import com.tyiu.corn.service.IdeaService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import java.security.Principal;
import java.util.List;

@Slf4j
@RestController
@RequestMapping("/api/v1/idea")
@RequiredArgsConstructor
public class IdeaController {
    
    private final IdeaService ideaService;

    @GetMapping("/initiator")
    public List<IdeaDTO> showListIdeaForInitiator(Principal principal){
        return ideaService.getListIdeaForInitiator(principal.getName());
    }
    
    @GetMapping("/initiator/{ideaId}")
    public IdeaDTO getIdeaForInitiator(@PathVariable Long ideaId, Principal principal) {
        return ideaService.getIdeaForInitiator(ideaId, principal.getName());
    }

    @GetMapping("/project-office")
    public List<IdeaDTO> showListIdeaForProjectOffice(){
        return ideaService.getListIdeaOnConfirmation();
    }

    @GetMapping("/expert")
    public List<IdeaDTO> showListIdeaForExpert(){
        return ideaService.getListIdeaOnApproval();
    }

    @GetMapping("/admin")
    public List<IdeaDTO> showListIdeaForAdmin(){
        return ideaService.getListIdea();
    }

    @PostMapping("/initiator/add")
    public IdeaDTO addIdea(@RequestBody IdeaDTO idea, Principal principal) {
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
    public void updateIdeaByInitiator(@PathVariable Long ideaId, Principal principal, @RequestBody IdeaDTO updatedIdea) {
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
    public void updateIdeaByAdmin(@PathVariable Long ideaId, @RequestBody IdeaDTO updatedIdea) {
        ideaService.updateIdeaByAdmin(ideaId, updatedIdea);
    }
}
