package com.tyiu.corn.service;

import com.tyiu.corn.model.entities.Scrum;
import com.tyiu.corn.repository.ScrumRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.List;
@Service
@RequiredArgsConstructor
public class ScrumService {
    private final ScrumRepository scrumRepository;


    public List<Scrum> getListScrum() {
        return scrumRepository.findAll();
    }

    public void saveScrum(Scrum scrum) {
        scrumRepository.save(scrum);
    }

    public void deleteScrum(Long id) {
        scrumRepository.deleteById(id);
    }

    public void updateScrum(Long id, Scrum updatedScrum) {
        Scrum scrum = scrumRepository.findById(id).orElseThrow();
        scrum.setDescription(updatedScrum.getDescription());
        scrum.setName(updatedScrum.getName());
        scrumRepository.save(scrum);
    }
}
