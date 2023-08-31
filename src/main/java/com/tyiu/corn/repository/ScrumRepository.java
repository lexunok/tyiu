package com.tyiu.corn.repository;

import com.tyiu.corn.model.entities.Scrum;
import org.springframework.data.repository.reactive.ReactiveCrudRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface ScrumRepository extends ReactiveCrudRepository<Scrum, String> {
}
