package com.tyiu.corn.repository;


import com.tyiu.corn.model.entities.Group;
import org.springframework.data.repository.reactive.ReactiveCrudRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface GroupRepository extends ReactiveCrudRepository<Group, String> {

}