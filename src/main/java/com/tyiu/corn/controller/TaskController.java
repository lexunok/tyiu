package com.tyiu.corn.controller;

import com.tyiu.corn.model.entities.Task;
import com.tyiu.corn.service.TaskService;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping("/api/v1/task")
@RequiredArgsConstructor
public class TaskController {

    private final TaskService taskService;

    @GetMapping
    public List<Task> showListTask() {
        return taskService.getListTask();
    }

    @PostMapping("/add")
    public void addTask(@RequestBody Task task) {
        taskService.saveTask(task);
    }

    @DeleteMapping("/delete/{id}")
    public void deleteTask(@PathVariable Long id) {
        taskService.deleteTask(id);
    }

    @PutMapping("/update/{id}")
    public void updateTask(@PathVariable Long id, @RequestBody Task updatedTask) {
        taskService.updateTask(id, updatedTask);
    }
}
