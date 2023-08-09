package com.tyiu.corn.service;

import com.tyiu.corn.model.dto.RiskDTO;
import com.tyiu.corn.model.entities.Comment;
import com.tyiu.corn.model.entities.Idea;
import com.tyiu.corn.model.enums.ProjectType;
import com.tyiu.corn.model.enums.StatusIdea;
import com.tyiu.corn.repository.CommentRepository;
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
    @Mock
    private CommentRepository commentRepository;

    @BeforeEach
    void setUp() {
        ideaService = new IdeaService(ideaRepository, commentRepository);
    }

    @Test
    void testGetListIdeaForInitiator() {
        // Given
        List<Idea> ideas = new ArrayList<>();
        String initiator = "example@example.com";

        Idea idea = Idea.builder()
                .initiator(initiator)
                .build();
        ideas.add(idea);

        Idea idea1 = Idea.builder()
                .initiator("notexample@example.com")
                .build();
        ideas.add(idea1);


        // When
        List<Idea> ideasByInitiator = ideas.stream().filter(i -> i.getInitiator().equals(initiator)).toList();
        when(ideaRepository.findAllByInitiator(initiator)).thenReturn(ideasByInitiator);
        List<Idea> result = ideaService.getListIdeaForInitiator(initiator);

        // Then
        assertEquals(1, result.size());
        assertEquals(initiator, result.get(0).getInitiator());
        verify(ideaRepository).findAllByInitiator(initiator);
    }

    @Test
    void testGetListIdeaByStatus() {
        List<Idea> ideas = new ArrayList<>();
        StatusIdea status = StatusIdea.ON_CONFIRMATION;

        Idea idea = Idea.builder()
                .status(status)
                .build();
        ideas.add(idea);

        Idea idea1 = Idea.builder()
                .status(StatusIdea.ON_APPROVAL)
                .build();
        ideas.add(idea1);

        // When
        List<Idea> ideasByStatus = ideas.stream().filter(i -> i.getStatus().equals(status)).toList();
        when(ideaRepository.findAllByStatus(status)).thenReturn(ideasByStatus);
        List<Idea> result = ideaService.getListIdeaByStatus(status);

        // Then
        assertEquals(1, result.size());
        assertEquals(status, result.get(0).getStatus());
        verify(ideaRepository).findAllByStatus(status);
    }

    @Test
    void testGetListIdea(){
        // Given
        List<Idea> ideas = new ArrayList<>();
        Idea idea1 = Idea.builder()
                .initiator("example@example.com")
                .name("Идея 1")
                .projectType(ProjectType.INSIDE)
                .experts("expert@expert.com")
                .problem("Проблема 100")
                .solution("Решение 100")
                .result("Результат 100")
                .customer("Заказчик 100")
                .description("Описание 100")
                .suitability("Пригодность 100")
                .realizability("Реализуемость 100")
                .budget(2600L)
                .status(StatusIdea.ON_CONFIRMATION)
                .rating(2.6)
                .build();
        Idea idea2 = Idea.builder()
                .initiator("example@example.com")
                .name("Идея 2")
                .projectType(ProjectType.INSIDE)
                .experts("expert@expert.com")
                .problem("Проблема 100")
                .solution("Решение 100")
                .result("Результат 100")
                .customer("Заказчик 100")
                .description("Описание 100")
                .suitability("Пригодность 100")
                .realizability("Реализуемость 100")
                .budget(2600L)
                .status(StatusIdea.ON_CONFIRMATION)
                .rating(2.6)
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
                .name("Идея 1")
                .projectType(ProjectType.INSIDE)
                .experts("expert@expert.com")
                .problem("Проблема 100")
                .solution("Решение 100")
                .result("Результат 100")
                .customer("Заказчик 100")
                .description("Описание 100")
                .suitability("Пригодность 100")
                .realizability("Реализуемость 100")
                .budget(2600L)
                .status(StatusIdea.ON_CONFIRMATION)
                .rating(2.6)
                .build();

        // When
        when(ideaRepository.save(idea)).thenReturn(idea);

        // Then
        ideaService.saveIdea(idea, "example@exampe.com");
        verify(ideaRepository).save(idea);
    }

    @Test
    void testDeleteIdeaByInitiator(){
        // Given
        String initiator = "example@example.com";
        Idea idea = Idea.builder()
                .initiator(initiator)
                .name("Идея 1")
                .build();

        // When
        when(ideaRepository.findById(idea.getId())).thenReturn(Optional.of(idea));
        ideaService.deleteIdeaByInitiator(idea.getId(), initiator);

        // Then
        verify(ideaRepository).deleteById(idea.getId());
    }
    @Test
    void testDeleteIdeaByAdmin(){
        // Given
        Idea idea = Idea.builder()
                .initiator("example@example.com")
                .name("Идея 1")
                .projectType(ProjectType.INSIDE)
                .experts("expert@expert.com")
                .problem("Проблема 100")
                .solution("Решение 100")
                .result("Результат 100")
                .customer("Заказчик 100")
                .description("Описание 100")
                .suitability("Пригодность 100")
                .realizability("Реализуемость 100")
                .budget(2600L)
                .status(StatusIdea.ON_CONFIRMATION)
                .rating(2.6)
                .build();
        // When
        ideaService.deleteIdeaByAdmin(idea.getId());


        // Then
        verify(ideaRepository).deleteById(idea.getId());
    }

    @Test
    void testIdeaUpdateByAdmin() {
        // Given
        Idea updatedIdea = Idea.builder()
                .initiator("example@example.com")
                .name("Идея 1")
                .projectType(ProjectType.INSIDE)
                .experts("expert@expert.com")
                .problem("Проблема 100")
                .solution("Решение 100")
                .result("Результат 100")
                .customer("Заказчик 100")
                .description("Описание 100")
                .suitability("Пригодность 100")
                .realizability("Реализуемость 100")
                .budget(2600L)
                .status(StatusIdea.ON_CONFIRMATION)
                .rating(2.6)
                .build();

        Idea idea = Idea.builder()
                .initiator("example@example.com")
                .name("Идея 2")
                .projectType(ProjectType.INSIDE)
                .experts("expert@expert.com")
                .problem("Проблема 100")
                .solution("Решение 100")
                .result("Результат 100")
                .customer("Заказчик 100")
                .description("Описание 100")
                .suitability("Пригодность 100")
                .realizability("Реализуемость 100")
                .budget(2600L)
                .status(StatusIdea.ON_CONFIRMATION)
                .rating(2.6)
                .build();

        // When
        when(ideaRepository.findById(idea.getId())).thenReturn(Optional.of(idea));
        when(ideaRepository.save(idea)).thenReturn(idea);
        ideaService.updateIdeaByAdmin(idea.getId(), updatedIdea);


        // Then
        assertEquals(updatedIdea.getInitiator(), idea.getInitiator());
        assertEquals(updatedIdea.getName(), idea.getName());
        assertEquals(updatedIdea.getProjectType(), idea.getProjectType());
        assertEquals(updatedIdea.getExperts(), idea.getExperts());
        assertEquals(updatedIdea.getProblem(), idea.getProblem());
        assertEquals(updatedIdea.getSolution(), idea.getSolution());
        assertEquals(updatedIdea.getResult(), idea.getResult());
        assertEquals(updatedIdea.getCustomer(), idea.getCustomer());
        assertEquals(updatedIdea.getDescription(), idea.getDescription());
        assertEquals(updatedIdea.getRealizability(), idea.getRealizability());
        assertEquals(updatedIdea.getSuitability(), idea.getSuitability());
        assertEquals(updatedIdea.getBudget(), idea.getBudget());
        assertEquals(updatedIdea.getStatus(), idea.getStatus());
        assertEquals(updatedIdea.getRating(), idea.getRating());


        verify(ideaRepository).save(idea);
        verify(ideaRepository).save(idea);

    }

    @Test
    void testCreateComment(){
        Idea idea = Idea.builder()
                .name("Идея 1")
                .comments(new ArrayList<>())
                .build();
        Comment comment = Comment.builder()
                .status(true)
                .comment("Комментарий")
                .sender("Отправитель")
                .build();

        when(ideaRepository.findById(idea.getId())).thenReturn(Optional.of(idea));
        when(commentRepository.save(comment)).thenReturn(comment);

        ideaService.createComment(idea.getId(), comment);
    }

    @Test
    void testUpdateStatusByProjectOffice(){
        //Given
        StatusIdea newStatus = StatusIdea.ON_CONFIRMATION;
        Idea idea = Idea.builder()
                .status(StatusIdea.ON_EDITING)
                .build();

        when(ideaRepository.findById(idea.getId())).thenReturn(Optional.of(idea));
        when(ideaRepository.save(idea)).thenReturn(idea);

        // When
        ideaService.updateStatusByProjectOffice(idea.getId(), newStatus);

        // Then
        assertEquals(newStatus, idea.getStatus());
    }

    @Test
    void testUpdateStatusByExpert(){
        RiskDTO riskDTO = new RiskDTO();
        riskDTO.setStatus(StatusIdea.CONFIRMED);
        riskDTO.setRisk(3.5);
        riskDTO.setPrice("3000");
        riskDTO.setOriginality("Очень оригинально");
        riskDTO.setTechnicalFeasibility("Очень возможно");
        riskDTO.setUnderstanding("Очень понятно");

        Idea idea = Idea.builder()
                .status(StatusIdea.ON_APPROVAL)
                .risk(2)
                .price("2000")
                .originality("Оригинально")
                .technicalFeasibility("Возможно")
                .understanding("Понятно")
                .build();

        when(ideaRepository.findById(idea.getId())).thenReturn(Optional.of(idea));
        when(ideaRepository.save(idea)).thenReturn(idea);

        ideaService.updateStatusByExpert(idea.getId(), riskDTO);
        assertEquals(riskDTO.getStatus(), idea.getStatus());
        assertEquals(riskDTO.getRisk(), idea.getRisk());
    }
}
