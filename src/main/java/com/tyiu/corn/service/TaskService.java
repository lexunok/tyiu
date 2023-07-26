package com.tyiu.corn.service;


import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import com.tyiu.corn.repositories.TaskRepository;

@Service
class TaskService {


    private final TaskRepository taskRepository;
    @Autowired
    TaskService(TaskRepository taskRepository) {
        this.taskRepository = taskRepository;
    }

//    public List<Task> listTask(String title) {
//        if (title != null)  taskRepository.findByTitle(title);
//        return taskRepository.findAll();
//    }

//    public void saveTask(Task task) {
//        TaskRepository.save(task);
//    }
//
//    public void deleteTask(Long id) {
//        TaskRepository.deleteById(id);
//    }
//
}
