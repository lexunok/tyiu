package com.tyiu.corn.repository;

import org.springframework.data.jpa.repository.JpaRepository;

import com.tyiu.corn.model.entities.Chat;

public interface ChatRepository extends JpaRepository<Chat,Long> {
}
