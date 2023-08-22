package com.tyiu.corn.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;

import com.tyiu.corn.model.entities.Group;
import com.tyiu.corn.model.entities.Idea;

public interface GroupRepository extends JpaRepository<Idea, Long>{

    Group save(Group group);

    void delete(Group group);
    List<Group> findByCategory(String category);
    
    List<Group> findByMembersGreaterThan(int members);
}