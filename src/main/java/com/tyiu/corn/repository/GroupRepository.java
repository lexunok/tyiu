package com.tyiu.corn.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;

import com.tyiu.corn.model.entities.Group;

public interface GroupRepository extends JpaRepository<Group, Long>{

}