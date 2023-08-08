package com.tyiu.corn.repository;

import com.tyiu.corn.model.entities.Idea;
import com.tyiu.corn.model.enums.StatusIdea;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;

public interface IdeaRepository extends JpaRepository<Idea, Long> {
    List<Idea> findAllByStatus (StatusIdea status);
    List<Idea> findAllByInitiator (String initiator);
}