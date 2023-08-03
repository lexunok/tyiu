package com.tyiu.corn.repository;


import com.tyiu.corn.model.Idea;
import org.springframework.data.jpa.repository.JpaRepository;


public interface IdeaRepository extends JpaRepository<Idea, Long> {
}