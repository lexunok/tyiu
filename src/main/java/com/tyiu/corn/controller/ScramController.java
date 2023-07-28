package com.tyiu.corn.controller;

import com.tyiu.corn.model.Scram;
import com.tyiu.corn.model.Task;
import com.tyiu.corn.service.ScramService;
import com.tyiu.corn.service.TaskService;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

import java.util.List;
@RestController
@RequestMapping("/api/v1/scram")
@RequiredArgsConstructor
public class ScramController {
    private final ScramService scramService;

    @GetMapping("/")
    public List<Scram> showScram() {
        return scramService.listScram();
    }

    @PostMapping("/add")
    public void addScram(@RequestBody Scram scram) {
        scramService.saveScram(scram);
    }

    @DeleteMapping("/delete/{id}")
    public void delScram(@PathVariable Long id) {
        scramService.deleteScram(id);
    }

    @PutMapping("/update/{id}")
    public void updateScram(@PathVariable Long id, @RequestBody Scram updatedScram) {
        scramService.updateScram(id, updatedScram);
    }
}
