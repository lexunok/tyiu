package com.tyiu.corn.repository;

import java.util.List;

import com.tyiu.corn.model.Idea;
import org.springframework.data.jpa.repository.JpaRepository;

import com.tyiu.corn.model.Task;

public interface IdeaRepository extends JpaRepository<Idea, Long> {
}