package com.tyiu.corn.repository;

import com.tyiu.corn.model.Task;
import org.springframework.data.jpa.repository.JpaRepository;



public interface TaskRepository extends JpaRepository<Task, Long> {
}
