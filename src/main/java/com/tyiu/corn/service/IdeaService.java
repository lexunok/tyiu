package com.tyiu.corn.service;

import java.util.List;

import com.tyiu.corn.model.entities.Idea;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import com.tyiu.corn.repository.IdeaRepository;
@Service
@RequiredArgsConstructor
public class IdeaService {
    private final IdeaRepository ideaRepository;


    public List<Idea> getListIdea() {
        return ideaRepository.findAll();
    }

    public void saveIdea(Idea idea) {
        ideaRepository.save(idea);
    }

    public void deleteIdea(Long id) {
        ideaRepository.deleteById(id);
    }
    
    public void updateIdea(Long id, Idea updatedIdea) {
        Idea idea = ideaRepository.findById(id).orElseThrow();
        idea.setTitle(updatedIdea.getTitle());
        idea.setType(updatedIdea.getType());
        idea.setProblem(updatedIdea.getProblem());
        idea.setSolution(updatedIdea.getSolution());
        idea.setResult(updatedIdea.getResult());
        idea.setCustomer(updatedIdea.getCustomer());
        idea.setDescription(updatedIdea.getDescription());
        idea.setBudget(updatedIdea.getBudget());
        idea.setFeasibility(updatedIdea.getFeasibility());
        idea.setSuitability(updatedIdea.getSuitability());
        idea.setStatus(updatedIdea.getStatus());
        ideaRepository.save(idea);
    }

}