package com.tyiu.corn.repository;

import com.tyiu.corn.model.entities.Task;
import org.springframework.data.repository.reactive.ReactiveCrudRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface TaskRepository extends ReactiveCrudRepository<Task, String> {
}
