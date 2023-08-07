package com.tyiu.corn.repository;

import com.tyiu.corn.model.entities.Comment;
import org.springframework.data.jpa.repository.JpaRepository;


public interface CommentRepository extends JpaRepository<Comment, Long> {
}
