package com.tyiu.corn.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;

import com.tyiu.corn.model.Task;

public interface Trepository extends JpaRepository<Task , Long> {
    List<Task> findByTitle(String title);
}