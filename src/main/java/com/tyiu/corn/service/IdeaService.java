package com.tyiu.corn.service;

import java.util.Date;
import java.util.List;

import com.tyiu.corn.model.dto.IdeaDTO;
import com.tyiu.corn.model.entities.Idea;
import com.tyiu.corn.model.dto.RiskDTO;
import com.tyiu.corn.model.enums.StatusIdea;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import org.modelmapper.ModelMapper;
import org.modelmapper.TypeToken;
import org.springframework.cache.annotation.CacheConfig;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.cache.annotation.CachePut;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.stereotype.Service;

import com.tyiu.corn.repository.IdeaRepository;

@Slf4j
@Service
@RequiredArgsConstructor
@CacheConfig(cacheNames = {"ideas"})
public class IdeaService {
    private final IdeaRepository ideaRepository;
    private final ModelMapper mapper;
    @Cacheable
    public List<IdeaDTO> getListIdeaForInitiator(String initiator) {
        List<Idea> ideas = ideaRepository.findAllByInitiator(initiator);
        return mapper.map(ideas, new TypeToken<List<IdeaDTO>>(){}.getType());
    }
    
    @Cacheable(key = "#id")
    public IdeaDTO getIdeaForInitiator(Long id, String email) {
        Idea idea = ideaRepository.findById(id).orElseThrow(() -> new RuntimeException("Идея не найдена"));
        if (email.equals(idea.getInitiator())) {
            return mapper.map(idea, IdeaDTO.class);
        } else {
            throw new RuntimeException("Идея не принадлежит инициатору");
        }
    }
    @Cacheable
    public List<IdeaDTO> getListIdeaOnApproval() {
        List<Idea> ideas = ideaRepository.findAllByStatus(StatusIdea.ON_APPROVAL);
        return mapper.map(ideas, new TypeToken<List<IdeaDTO>>(){}.getType());
    }
    @Cacheable
    public List<IdeaDTO> getListIdeaOnConfirmation() {
        List<Idea> ideas = ideaRepository.findAllByStatus(StatusIdea.ON_CONFIRMATION);
        return mapper.map(ideas, new TypeToken<List<IdeaDTO>>(){}.getType());
    }
    @Cacheable
    public List<IdeaDTO> getListIdea() {
        List<Idea> ideas = ideaRepository.findAll();
        return mapper.map(ideas, new TypeToken<List<IdeaDTO>>(){}.getType());
    }
    @CacheEvict(allEntries = true)
    public IdeaDTO saveIdea(IdeaDTO ideaDTO, String initiator) {
        ideaDTO.setDateCreated(new Date());
        ideaDTO.setInitiator(initiator);
        ideaDTO.setStatus(StatusIdea.NEW);
        Idea idea = ideaRepository.save(mapper.map(ideaDTO, Idea.class));
        return mapper.map(idea, IdeaDTO.class);
    }
    @CacheEvict(allEntries = true)
    public void deleteIdeaByInitiator(Long id, String email) {
        Idea idea = ideaRepository.findById(id).orElseThrow(() -> new RuntimeException("Идея не найдена"));
        if (email.equals(idea.getInitiator())){
            ideaRepository.deleteById(id);
        }
        else {
            throw new RuntimeException("Идея не принадлежит инициатору");
        }
    }
    @CacheEvict(allEntries = true)
    public void deleteIdeaByAdmin(Long id) {
        ideaRepository.deleteById(id);
    }

    @CacheEvict(allEntries = true)
    @CacheEvict(allEntries = true)
    public void updateStatusByInitiator (Long id, String email){
        Idea idea = ideaRepository.findById(id).orElseThrow(() -> new RuntimeException(""));
        if (email.equals(idea.getInitiator())){
            idea.setStatus(StatusIdea.ON_CONFIRMATION);
            ideaRepository.save(idea);
        }
        else {
            throw new RuntimeException("Идея не принадлежит инициатору");
        }
    }
    
    @CacheEvict(allEntries = true)
    public void updateIdeaByInitiator(Long id, String email, IdeaDTO updatedIdea) {
        Idea idea = ideaRepository.findById(id).orElseThrow(() -> new RuntimeException(""));
        if (email.equals(idea.getInitiator())){
            idea.setName(updatedIdea.getName());
            idea.setProjectType(updatedIdea.getProjectType());
            idea.setProblem(updatedIdea.getProblem());
            idea.setSolution(updatedIdea.getSolution());
            idea.setResult(updatedIdea.getResult());
            idea.setCustomer(updatedIdea.getCustomer());
            idea.setContactPerson(updatedIdea.getContactPerson());
            idea.setDescription(updatedIdea.getDescription());
            idea.setRealizability(updatedIdea.getRealizability());
            idea.setSuitability(updatedIdea.getSuitability());
            idea.setBudget(updatedIdea.getBudget());
            idea.setRating(updatedIdea.getRating());
            idea.setDateModified(new Date());
            ideaRepository.save(idea);
        }
        else {
            throw new RuntimeException("Идея не принадлежит инициатору");
        }
    }
    
    @CacheEvict(allEntries = true)
    public void updateStatusByProjectOffice (Long ideaId, StatusIdea newStatus){
        Idea idea = ideaRepository.findById(ideaId).orElseThrow(() -> new RuntimeException(""));
        idea.setStatus(newStatus);
        ideaRepository.save(idea);
    }
    
    @CacheEvict(allEntries = true)
    public void updateStatusByExpert(Long ideaId, RiskDTO riskDTO){
        Idea idea = ideaRepository.findById(ideaId).orElseThrow(() -> new RuntimeException(""));
        idea.setStatus(riskDTO.getStatus());
        idea.setRisk(riskDTO.getRisk());
        idea.setPrice(riskDTO.getPrice());
        idea.setOriginality(riskDTO.getOriginality());
        idea.setTechnicalFeasibility(riskDTO.getTechnicalFeasibility());
        idea.setUnderstanding(riskDTO.getUnderstanding());
        ideaRepository.save(idea);
    }
    
    @CacheEvict(allEntries = true)
    public void updateIdeaByAdmin(Long id, IdeaDTO updatedIdea) {
        Idea idea = ideaRepository.findById(id).orElseThrow(() -> new RuntimeException(""));
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
}