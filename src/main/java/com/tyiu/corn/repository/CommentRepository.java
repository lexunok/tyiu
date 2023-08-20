package com.tyiu.corn.repository;

import com.tyiu.corn.model.entities.Comment;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;

import java.util.List;



public interface CommentRepository extends JpaRepository<Comment, Long> {
    List<Comment> findAllByIdea_Id(Long ideaId);
    @Modifying(clearAutomatically = true, flushAutomatically = true)
    @Query("DELETE FROM Comment c WHERE c.id = ?1")
    void deleteById(Long id);
}
