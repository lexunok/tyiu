package com.tyiu.corn.service;

import com.tyiu.corn.model.Task;
import com.tyiu.corn.repository.ScramRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import com.tyiu.corn.repository.TaskRepository;


import java.util.Date;
import java.util.List;

@Service
public class TaskService {
    private final TaskRepository taskRepository;
    private final ScramRepository scramRepository;

    @Autowired
    TaskService(TaskRepository taskRepository, ScramRepository scramRepository) {
        this.taskRepository = taskRepository;
        this.scramRepository = scramRepository;
    }

    public List<Task> listTask() {
        return taskRepository.findAll();
    }

    public void saveTask(Task task) {
        task.setCreatedAt(new Date());
        taskRepository.save(task);
    }

    public void deleteTask(Long id) {
        taskRepository.deleteById(id);
    }

    public void updateTask(Long id, Task updatedTask) {
        Task task = taskRepository.findById(id).orElseThrow();
        task.setTitle(updatedTask.getTitle());
        task.setDescription(updatedTask.getDescription());
        task.setAssignedTo(updatedTask.getAssignedTo());
        task.setPriority(updatedTask.getPriority());
        task.setDeadline(updatedTask.getDeadline());
        task.setStatus(updatedTask.getStatus());
        taskRepository.save(task);
    }


}
