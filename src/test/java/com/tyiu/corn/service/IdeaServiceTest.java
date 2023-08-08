package com.tyiu.corn.service;

import com.tyiu.corn.model.entities.Idea;
import com.tyiu.corn.model.enums.Feasibility;
import com.tyiu.corn.repository.IdeaRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
public class IdeaServiceTest {
    @Mock
    private IdeaService ideaService;
    @Mock
    private IdeaRepository  ideaRepository;

    @BeforeEach
    void setUp() {
        ideaService = new IdeaService(ideaRepository);
    }

    @Test
    void testGetListIdea() {
        // Given
        List<Idea> ideas = new ArrayList<>();
        Idea idea1 = Idea.builder()
                .title("Идея 1")
                .type("Идея 1")
                .problem("Проблема 1")
                .solution("Решение 1")
                .result("Результат 1")
                .customer("Заказчик 1")
                .description("Описание 1")
                .budget(2000L)
                .feasibility(Feasibility.POSSIBLE)
                .suitability("Пригодность 1")
                .build();


        Idea idea2 = Idea.builder()
                .title("Идея 2")
                .type("Идея 2")
                .problem("Проблема 2")
                .solution("Решение 2")
                .result("Результат 2")
                .customer("Заказчик 2")
                .description("Описание 2")
                .budget(2000L)
                .feasibility(Feasibility.POSSIBLE)
                .suitability("Пригодность 2")
                .build();

        ideas.add(idea1);
        ideas.add(idea2);

        // When
        when(ideaRepository.findAll()).thenReturn(ideas);
        List<Idea> result = ideaService.getListIdea();


        // Then
        assertEquals(ideas.size(), result.size());
        verify(ideaRepository).findAll();
    }

    @Test
    void testSaveIdea(){
        // Given
        Idea idea = Idea.builder()
                .title("Идея 1")
                .type("Идея 1")
                .problem("Проблема 1")
                .solution("Решение 1")
                .result("Результат 1")
                .customer("Заказчик 1")
                .description("Описание 1")
                .budget(2000L)
                .feasibility(Feasibility.POSSIBLE)
                .suitability("Пригодность 1")
                .build();

        // When
        when(ideaRepository.save(idea)).thenReturn(idea);

        // Then
        ideaService.saveIdea(idea);

        verify(ideaRepository).save(idea);
    }

    @Test
    void testDeleteIdea(){
        // Given
        Idea idea = Idea.builder()
                .title("Идея 1")
                .type("Идея 1")
                .problem("Проблема 1")
                .solution("Решение 1")
                .result("Результат 1")
                .customer("Заказчик 1")
                .description("Описание 1")
                .budget(2000L)
                .feasibility(Feasibility.POSSIBLE)
                .suitability("Пригодность 1")
                .build();
        // When
        ideaService.deleteIdea(idea.getId());


        // Then
        verify(ideaRepository).deleteById(idea.getId());
    }

    @Test
    void testUpdateIdea() {
        // Given
        Idea updatedIdea = Idea.builder()
                .title("Идея 100")
                .type("Идея 100")
                .problem("Проблема 100")
                .solution("Решение 100")
                .result("Результат 100")
                .customer("Заказчик 100")
                .description("Описание 100")
                .budget(2600L)
                .feasibility(Feasibility.POSSIBLE)
                .suitability("Пригодность 100")
                .build();

        Idea idea = Idea.builder()
                .title("Идея 1")
                .type("Идея 1")
                .problem("Проблема 1")
                .solution("Решение 1")
                .result("Результат 1")
                .customer("Заказчик 1")
                .description("Описание 1")
                .budget(2000L)
                .feasibility(Feasibility.POSSIBLE)
                .suitability("Пригодность 1")
                .build();

        // When
        when(ideaRepository.findById(idea.getId())).thenReturn(Optional.of(idea));
        when(ideaRepository.save(idea)).thenReturn(idea);
        ideaService.updateIdea(idea.getId(), updatedIdea);


        // Then
        assertEquals(updatedIdea.getTitle(), idea.getTitle());
        assertEquals(updatedIdea.getType(), idea.getType());
        assertEquals(updatedIdea.getProblem(), idea.getProblem());
        assertEquals(updatedIdea.getSolution(), idea.getSolution());
        assertEquals(updatedIdea.getResult(), idea.getResult());
        assertEquals(updatedIdea.getCustomer(), idea.getCustomer());
        assertEquals(updatedIdea.getDescription(), idea.getDescription());
        assertEquals(updatedIdea.getBudget(), idea.getBudget());
        assertEquals(updatedIdea.getFeasibility(), idea.getFeasibility());
        assertEquals(updatedIdea.getSuitability(), idea.getSuitability());

        verify(ideaRepository).save(idea);
        verify(ideaRepository).save(idea);

    }
}
