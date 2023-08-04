package com.tyiu.corn.repository;

import com.tyiu.corn.model.entities.Idea;
import org.springframework.data.jpa.repository.JpaRepository;

public interface IdeaRepository extends JpaRepository<Idea, Long> {
}