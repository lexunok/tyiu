package com.tyiu.corn.repository;

import com.tyiu.corn.model.entities.Comment;
import com.tyiu.corn.model.entities.Idea;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.jdbc.AutoConfigureTestDatabase;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.transaction.annotation.EnableTransactionManagement;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Date;

@DataJpaTest
@AutoConfigureTestDatabase(replace = AutoConfigureTestDatabase.Replace.NONE)
@EnableTransactionManagement
public class CommentRepositoryTest {
    @Autowired
    private CommentRepository underTest;
    
    @Autowired
    private IdeaRepository ideaRepository;
    @Test
    void deleteById(){
        //Before
        Idea idea = Idea.builder().name("Пирожки").build();
        idea = ideaRepository.save(idea);
        assertTrue(ideaRepository.existsById(idea.getId()));
        Comment comment = Comment.builder().comment("Хорошо").dateCreated(new Date()).idea(idea).build();

        comment = underTest.save(comment);
        assertTrue(underTest.existsById(comment.getId()));
        //When
        underTest.deleteById(comment.getId());
        //Then
        assertFalse(underTest.existsById(comment.getId()));
        assertTrue(ideaRepository.existsById(idea.getId()));
    }
}
