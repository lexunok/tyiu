package com.tyiu.corn.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;

import com.tyiu.corn.model.entities.Chat;

public interface ChatRepository extends JpaRepository<Chat,Long> {
    List<Chat> findByMembers_Email (String email);
}
