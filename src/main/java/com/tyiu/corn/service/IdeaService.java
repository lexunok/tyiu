package com.tyiu.corn.service;

import java.util.Date;
import java.util.List;

import com.tyiu.corn.model.entities.Comment;
import com.tyiu.corn.model.entities.Idea;
import com.tyiu.corn.model.enums.StatusIdea;
import com.tyiu.corn.repository.CommentRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import com.tyiu.corn.repository.IdeaRepository;
@Service
@RequiredArgsConstructor
public class IdeaService {
    private final IdeaRepository ideaRepository;
    private final CommentRepository commentRepository;

    public List<Idea> getAllListIdea() {
        return ideaRepository.findAll();
    }

    public Idea saveIdea(Idea idea) {
        return ideaRepository.save(idea);
    }

    public void deleteIdea(Long id) {
        ideaRepository.deleteById(id);
    }

    public void updateIdeaByAdmin(Long id, Idea updatedIdea) {
        Idea idea = ideaRepository.findById(id).orElseThrow();
        idea.setInitiator(updatedIdea.getInitiator());
        idea.setName(updatedIdea.getName());
        idea.setProjectType(updatedIdea.getProjectType());
        idea.setExperts(updatedIdea.getExperts());
        idea.setProblem(updatedIdea.getProblem());
        idea.setSolution(updatedIdea.getSolution());
        idea.setResult(updatedIdea.getResult());
        idea.setCustomer(updatedIdea.getCustomer());
        idea.setDescription(updatedIdea.getDescription());
        idea.setRealizability(updatedIdea.getRealizability());
        idea.setSuitability(updatedIdea.getSuitability());
        idea.setBudget(updatedIdea.getBudget());
        idea.setStatus(updatedIdea.getStatus());
        idea.setRating(updatedIdea.getRating());
        idea.setRisk(updatedIdea.getRisk());
        idea.setDateModified(new Date());
        ideaRepository.save(idea);
    }

    public void addCommentByAdmin(Long ideaId, Comment comment) {

    }
    public void updateStatusByProjectOffice (Long ideaId, StatusIdea newStatus){
        Idea idea = ideaRepository.findById(ideaId).orElseThrow();
        idea.setStatus(newStatus);
    }

    public void updateStatusByExpert(Long ideaId, StatusIdea newStatus){
        Idea idea = ideaRepository.findById(ideaId).orElseThrow();
        idea.setStatus(newStatus);
    }

    public void updateRiskByExpert(Long ideaId, double riskValue){
        Idea idea = ideaRepository.findById(ideaId).orElseThrow();
        idea.setRisk(riskValue);
    }

}