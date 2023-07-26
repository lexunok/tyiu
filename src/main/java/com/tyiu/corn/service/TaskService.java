package com.tyiu.corn.service;

import com.tyiu.corn.model.Task
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import com.tyiu.corn.repositories.TaskRepository;

import java.util.List;

@Service
class TaskService {


    private final TaskRepository taskRepository;

    @Autowired
    TaskService(TaskRepository taskRepository) {
        this.taskRepository = taskRepository;
    }

    public List<Task> listTask(String title) {
        if (title != null)  taskRepository.findByTitle(title);
        return taskRepository.findAll();
    }

    public void saveTask(Task task) {
        taskRepository.save(task);
    }

    public void deleteTask(Long id) {
        taskRepository.deleteById(id);
    }

}
