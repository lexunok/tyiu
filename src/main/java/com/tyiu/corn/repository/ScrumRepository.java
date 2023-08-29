package com.tyiu.corn.repository;

import com.tyiu.corn.model.entities.Scrum;
import org.springframework.data.repository.reactive.ReactiveCrudRepository;

public interface ScrumRepository extends ReactiveCrudRepository<Scrum, Long> {
}
