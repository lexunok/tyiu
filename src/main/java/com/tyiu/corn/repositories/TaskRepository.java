package com.tyiu.corn.repositories;

import com.tyiu.corn.model.Task;
import org.springframework.data.jpa.repository.JpaRepository;
<<<<<<< Updated upstream:src/main/java/com/tyiu/corn/repositories/TaskRepository.java

import java.util.List;
=======
>>>>>>> Stashed changes:src/main/java/com/tyiu/corn/repository/TaskRepository.java

public interface TaskRepository extends JpaRepository<Task, Long> {
    List<Task>findByTitle(String title);

}
