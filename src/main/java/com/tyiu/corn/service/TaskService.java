package com.tyiu.corn.service;


import lombok.RequiredArgsConstructor;
import com.tyiu.corn.model.Task;
import org.springframework.stereotype.Service;
import com.tyiu.corn.repositories.TaskRepository;

import java.util.List;

@Service
@RequiredArgsConstructor
class TaskService {
    private final TaskRepository taskRepository;

    public List<Task> listTask(String title) {
        if (title != null)  taskRepository.findByTitle(title);
        return taskRepository.findAll();
    }

//    public void saveTask(Task task) {
//        TaskRepository.save(task);
//    }
//
//    public void deleteTask(Long id) {
//        TaskRepository.deleteById(id);
//    }
//
}
