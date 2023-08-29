package com.tyiu.corn.controller;

import com.tyiu.corn.model.dto.TaskDTO;
import com.tyiu.corn.model.entities.Task;
import com.tyiu.corn.service.TaskService;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.util.List;

@RestController
@RequestMapping("/api/v1/task")
@RequiredArgsConstructor
public class TaskController {

    private final TaskService taskService;

    @GetMapping("/all")
    public Flux<TaskDTO> showListTask() {
        return taskService.getListTask();
    }

    @PostMapping("/add")
    public Mono<TaskDTO> addTask(@RequestBody TaskDTO task) {
        return taskService.saveTask(task);
    }

    @DeleteMapping("/delete/{id}")
    public Mono<Void> deleteTask(@PathVariable Long id) {
        taskService.deleteTask(id);
        return Mono.empty();
    }

    @PutMapping("/update/{id}")
    public Mono<Void> updateTask(@PathVariable Long id, @RequestBody TaskDTO updatedTask) {
        taskService.updateTask(id, updatedTask);
        return Mono.empty();
    }
}
