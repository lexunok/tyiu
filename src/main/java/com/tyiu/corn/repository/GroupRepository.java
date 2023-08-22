package com.tyiu.corn.repository;

import org.springframework.data.jpa.repository.JpaRepository;

import com.tyiu.corn.model.entities.Idea;

public interface GroupRepository extends JpaRepository<Idea, Long>{
    

}