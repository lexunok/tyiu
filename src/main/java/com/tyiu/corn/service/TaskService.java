package com.tyiu.corn.service;

import com.tyiu.corn.model.entities.Task;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import com.tyiu.corn.repository.TaskRepository;


import java.util.Date;
import java.util.List;

@Service
@RequiredArgsConstructor
public class TaskService {

    private final TaskRepository taskRepository;

    public List<Task> getListTask() {
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
