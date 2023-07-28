package com.tyiu.corn.service;

import com.tyiu.corn.model.Scram;
import com.tyiu.corn.model.Task;
import com.tyiu.corn.repository.ScramRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.Date;
import java.util.List;
@Service
@RequiredArgsConstructor
public class ScramService {
    private final ScramRepository scramRepository;
    public List<Scram> listScram() {
        return scramRepository.findAll();
    }

    public void saveScram(Scram scram) {
        scramRepository.save(scram);
    }

    public void deleteScram(Long id) {
        scramRepository.deleteById(id);
    }

    public void updateScram(Long id, Scram updatedScram) {
        Scram scram = scramRepository.findById(id).orElseThrow();
        scram.setDescription(updatedScram.getDescription());
        scram.setName(updatedScram.getName());
        scramRepository.save(scram);
    }
}
