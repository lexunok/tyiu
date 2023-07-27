package com.tyiu.corn.servers;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.tyiu.corn.model.Task;
import com.tyiu.corn.repository.Trepository;
@Service
public class tservice {
    private final Trepository taskRepository;

    @Autowired
    tservice(Trepository taskRepository) {
        this.taskRepository = taskRepository;
    }

    public List<Task> listTask() {
        return taskRepository.findAll();
    }

    public void saveTask(Task task) {
        taskRepository.save(task);
    }

    public void deleteTask(Long id) {
        taskRepository.deleteById(id);
    }
    
        public void updateTask(Long id, Task updatedTask) {
            Task task = taskRepository.findById(id).orElseThrow();
            task.setTitle(updatedTask.getTitle());
            task.setName(updatedTask.getName());
            task.setPriority(updatedTask.getPriority());
            taskRepository.save(task);}

}