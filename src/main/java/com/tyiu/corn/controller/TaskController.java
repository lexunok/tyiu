package com.tyiu.corn.controller;

import com.tyiu.corn.model.Task;
import com.tyiu.corn.service.TaskService;
import lombok.RequiredArgsConstructor;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping("/task")
@RequiredArgsConstructor
public class TaskController {
    private final TaskService taskService;

    @GetMapping("/")
    public String showTask(Model model) {
        List<Task> tasks = taskService.listTask();
        model.addAttribute("tasks", tasks);
        return "task";
    }

    @PostMapping("/add")
    public String addTask(Task task) {
        taskService.saveTask(task);
        return "redirect:/";
    }

    @PostMapping("/delete/{id}")
    public String delTask(@PathVariable Long id) {
        taskService.deleteTask(id);
        return "redirect/";
    }

    @PutMapping("/update/{id}")
    public void updateTask(@PathVariable Long id, Task updatedTask) {
        taskService.updateTask(id, updatedTask);
    }
}
