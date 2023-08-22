package com.tyiu.corn.service;

import com.tyiu.corn.model.dto.IdeaDTO;
import com.tyiu.corn.model.dto.RiskDTO;
import com.tyiu.corn.model.entities.Idea;
import com.tyiu.corn.model.enums.ProjectType;
import com.tyiu.corn.model.enums.StatusIdea;
import com.tyiu.corn.repository.IdeaRepository;
import jakarta.persistence.Id;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.modelmapper.ModelMapper;
import org.modelmapper.TypeToken;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
public class IdeaServiceTest {
    @Mock
    private IdeaService ideaService;
    @Mock
    private IdeaRepository  ideaRepository;
    @Mock
    private ModelMapper mapper;

    @BeforeEach
    void setUp() {
        ideaService = new IdeaService(ideaRepository, mapper);
    }

    @Test
    void testGetListIdeaForInitiator() {
        // Given
        List<IdeaDTO> ideasDTO = new ArrayList<>();
        String initiator = "example@example.com";

        IdeaDTO ideaDTO = IdeaDTO.builder()
                .initiator(initiator)
                .build();
        ideasDTO.add(ideaDTO);

        List<Idea> ideas = new ArrayList<>();
        Idea idea = Idea.builder()
                .initiator(initiator)
                .build();
        ideas.add(idea);

        // When
        when(ideaRepository.findAllByInitiator(initiator)).thenReturn(ideas);
        when(mapper.map(ideas, new TypeToken<List<IdeaDTO>>(){}.getType())).thenReturn(ideasDTO);
        List<IdeaDTO> result = ideaService.getListIdeaForInitiator(initiator);

        // Then
        assertEquals(1, result.size());
        assertEquals(initiator, result.get(0).getInitiator());
        verify(ideaRepository).findAllByInitiator(initiator);
    }

    @Test
    void testGetListIdeaOnConfirmation() {
        List<IdeaDTO> ideasDTO = new ArrayList<>();
        StatusIdea status = StatusIdea.ON_CONFIRMATION;

        IdeaDTO ideaDTO = IdeaDTO.builder()
                .status(status)
                .build();
        ideasDTO.add(ideaDTO);


        List<Idea> ideas = new ArrayList<>();
        Idea idea = Idea.builder()
                .status(status)
                .build();
        ideas.add(idea);


        // When
        when(ideaRepository.findAllByStatus(status)).thenReturn(ideas);
        when(mapper.map(ideas, new TypeToken<List<IdeaDTO>>(){}.getType())).thenReturn(ideasDTO);
        List<IdeaDTO> result = ideaService.getListIdeaOnConfirmation();

        // Then
        assertEquals(1, result.size());
        assertEquals(status, result.get(0).getStatus());
        verify(ideaRepository).findAllByStatus(status);
    }

    @Test
    void testGetListIdeaOnApproval() {
        List<IdeaDTO> ideasDTO = new ArrayList<>();
        StatusIdea status = StatusIdea.ON_APPROVAL;

        IdeaDTO ideaDTO = IdeaDTO.builder()
                .status(status)
                .build();
        ideasDTO.add(ideaDTO);

        List<Idea> ideas = new ArrayList<>();
        Idea idea = Idea.builder()
                .status(status)
                .build();
        ideas.add(idea);

        // When
        when(ideaRepository.findAllByStatus(status)).thenReturn(ideas);
        when(mapper.map(ideas, new TypeToken<List<IdeaDTO>>(){}.getType())).thenReturn(ideasDTO);
        List<IdeaDTO> result = ideaService.getListIdeaOnApproval();

        // Then
        assertEquals(1, result.size());
        assertEquals(status, result.get(0).getStatus());
        verify(ideaRepository).findAllByStatus(status);
    }

    @Test
    void testGetListIdea(){
        // Given
        List<IdeaDTO> ideasDTO = new ArrayList<>();
        IdeaDTO ideaDTO1 = IdeaDTO.builder()
                .initiator("example@example.com")
                .name("Идея 1")
                .build();
        IdeaDTO ideaDTO2 = IdeaDTO.builder()
                .initiator("example@example.com")
                .name("Идея 2")
                .build();

        ideasDTO.add(ideaDTO1);
        ideasDTO.add(ideaDTO2);

        List<Idea> ideas = new ArrayList<>();
        Idea idea1 = Idea.builder()
                .initiator("example@example.com")
                .name("Идея 1")
                .build();
        Idea idea2 = Idea.builder()
                .initiator("example@example.com")
                .name("Идея 2")
                .build();

        ideas.add(idea1);
        ideas.add(idea2);

        // When
        when(ideaRepository.findAll()).thenReturn(ideas);
        when(mapper.map(ideas, new TypeToken<List<IdeaDTO>>(){}.getType())).thenReturn(ideasDTO);
        List<IdeaDTO> result = ideaService.getListIdea();


        // Then
        assertEquals(ideas.size(), result.size());
        verify(ideaRepository).findAll();
    }

    @Test
    void testSaveIdea() {
        IdeaDTO ideaDTO = new IdeaDTO();
        String initiator = "JohnDoe";

        Idea idea = Idea.builder()
                .name("Идея 1")
                .build();

        when(mapper.map(ideaDTO, Idea.class)).thenReturn(idea);
        when(ideaRepository.save(idea)).thenReturn(idea);
        when(mapper.map(idea, IdeaDTO.class)).thenReturn(ideaDTO);


        IdeaDTO savedIdeaDTO = ideaService.saveIdea(ideaDTO, initiator);

        assertEquals(ideaDTO, savedIdeaDTO);
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
        IdeaDTO updatedIdea = IdeaDTO.builder()
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
    void testUpdateStatusByProjectOffice(){
        //Given
        StatusIdea status = StatusIdea.ON_APPROVAL;
        Idea idea = Idea.builder()
                .name("title")
                .build();


        when(ideaRepository.findById(idea.getId())).thenReturn(Optional.of(idea));
        when(ideaRepository.save(idea)).thenReturn(idea);

        // When
        ideaService.updateStatusByProjectOffice(idea.getId(),status);

        // Then
        assertEquals(status, idea.getStatus());
    }

    @Test
    void testUpdateStatusByExpert(){
        RiskDTO riskDTO = RiskDTO.builder()
                .status(StatusIdea.CONFIRMED)
                .risk(3.5).price("3000")
                .originality("Очень оригинально")
                .technicalFeasibility("Очень возможно")
                .understanding("Очень понятно")
                .build();

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
