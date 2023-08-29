package com.tyiu.corn.repository;


import com.tyiu.corn.model.entities.Group;
import org.springframework.data.repository.reactive.ReactiveCrudRepository;

public interface GroupRepository extends ReactiveCrudRepository<Group, Long> {

}