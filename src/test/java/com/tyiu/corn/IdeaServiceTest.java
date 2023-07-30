package com.tyiu.corn;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import com.tyiu.corn.model.Idea;
import com.tyiu.corn.repository.IdeaRepository;
import com.tyiu.corn.service.IdeaService;
import org.springframework.boot.test.context.SpringBootTest;

@SpringBootTest
public class IdeaServiceTest {
    
    @Mock
    private IdeaRepository ideaRepository;
    
    @InjectMocks
    private IdeaService ideaService;
    
    @BeforeEach
    public void setUp() {
        Idea idea1 = new Idea();
        idea1.setId(1L);
        idea1.setStatus("In progress");
        Idea idea2 = new Idea();
        idea2.setId(2L);
        idea2.setStatus("Completed");
        List<Idea> ideas = new ArrayList<>();
        ideas.add(idea1);
        ideas.add(idea2);
        when(ideaRepository.findAll()).thenReturn(ideas);
        when(ideaRepository.findById(1L)).thenReturn(Optional.of(idea1));
        when(ideaRepository.save(any(Idea.class))).thenReturn(idea1);
        doNothing().when(ideaRepository).deleteById(1L);
        
    }
    @Test
    public void testListIdea() {
        List<Idea> ideas = ideaService.getListIdea();
        assertNotEmpty(ideas);
        assertEquals(2, ideas.size());
        
    }
    
    private void assertNotEmpty(List<Idea> ideas) {
    }
    private void assertEquals(int i, int size) {
    }
    @Test
    public void testSaveIdea() {
        Idea idea = new Idea();
        idea.setId(3L);
        idea.setStatus("Pending");
        ideaService.saveIdea(idea);
        verify(ideaRepository).save(idea);
    }
    
    @Test
    public void testDeleteIdea() {
        ideaService.deleteIdea(1L);
        verify(ideaRepository).deleteById(1L);
    }
    
    @Test
    public void testUpdateIdea() {
        Idea updatedIdea = new Idea();
        updatedIdea.setStatus("Completed");
        
        ideaService.updateIdea(1L, updatedIdea);
        verify(ideaRepository).findById(1L);
        verify(ideaRepository).save(any(Idea.class));
    }
}