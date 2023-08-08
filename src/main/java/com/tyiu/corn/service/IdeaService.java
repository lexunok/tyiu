package com.tyiu.corn.service;

import java.util.Date;
import java.util.List;

import com.tyiu.corn.model.entities.Comment;
import com.tyiu.corn.model.entities.Idea;
import com.tyiu.corn.model.dto.RiskDTO;
import com.tyiu.corn.model.enums.StatusIdea;
import com.tyiu.corn.repository.CommentRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import com.tyiu.corn.repository.IdeaRepository;

import jakarta.transaction.Transactional;
@Service
@RequiredArgsConstructor
public class IdeaService {
    private final IdeaRepository ideaRepository;
    private final CommentRepository commentRepository;

    public List<Idea> getListIdeaForInitiator(String initiator) {
        return ideaRepository.findAllByInitiator(initiator);
    }

    public List<Idea> getListIdeaByStatus(StatusIdea status) {
        return ideaRepository.findAllByStatus(status);
    }

    public List<Idea> getListIdea() {
        return ideaRepository.findAll();
    }

    public Idea saveIdea(Idea idea, String initiator) {
        idea.setDateCreated(new Date());
        idea.setInitiator(initiator);
        return ideaRepository.save(idea);
    }

    public void deleteIdeaByInitiator(Long id, String email) {
        Idea idea = ideaRepository.findById(id).orElseThrow(() -> new RuntimeException());
        if (idea.getInitiator() == email){
            ideaRepository.deleteById(id);
        }
        else {
            throw new RuntimeException("Идея не принадлежит инициатору");
        }
    }
    public void deleteIdeaByAdmin(Long id) {
        ideaRepository.deleteById(id);
    }


    public void updateIdeaByAdmin(Long id, Idea updatedIdea) {
        Idea idea = ideaRepository.findById(id).orElseThrow(() -> new RuntimeException());
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

    @Transactional
    public void createComment(Long ideaId, Comment comment) {
        Idea idea = ideaRepository.findById(ideaId).orElseThrow(() -> new RuntimeException());
        comment.setIdea(idea);
        Comment savedComment = commentRepository.save(comment);
        idea.getComments().add(savedComment);
        ideaRepository.save(idea);

    }
    public void updateStatusByProjectOffice (Long ideaId, StatusIdea newStatus){
        Idea idea = ideaRepository.findById(ideaId).orElseThrow(() -> new RuntimeException());
        idea.setStatus(newStatus);
        ideaRepository.save(idea);
    }

    public void updateStatusByExpert(Long ideaId, RiskDTO riskDTO){
        Idea idea = ideaRepository.findById(ideaId).orElseThrow(() -> new RuntimeException());
        idea.setStatus(riskDTO.getStatus());
        idea.setRisk(riskDTO.getRisk());
        idea.setPrice(riskDTO.getPrice());
        idea.setOriginality(riskDTO.getOriginality());
        idea.setTechnicalFeasibility(riskDTO.getTechnicalFeasibility());
        idea.setUnderstanding(riskDTO.getUnderstanding());
        ideaRepository.save(idea);
    }
}