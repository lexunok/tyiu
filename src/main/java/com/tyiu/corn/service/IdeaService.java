package com.tyiu.corn.service;

import java.util.List;

import com.tyiu.corn.model.Idea;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.tyiu.corn.model.Task;
import com.tyiu.corn.repository.IdeaRepository;
@Service
public class IdeaService {
    private final IdeaRepository ideaRepository;

    @Autowired
    IdeaService(IdeaRepository ideaRepository) {
        this.ideaRepository = ideaRepository;
    }

    public List<Idea> listIdea() {
        return ideaRepository.findAll();
    }

    public void saveIdea(Idea idea) {
        ideaRepository.save(idea);
    }

    public void deleteIdea(Long id) {
        ideaRepository.deleteById(id);
    }
    
        public void updateTask(Long id, Idea updatedIdea) {
            Idea idea = ideaRepository.findById(id).orElseThrow();
            idea.setStatus(updatedIdea.getStatus());
            ideaRepository.save(idea);}

}