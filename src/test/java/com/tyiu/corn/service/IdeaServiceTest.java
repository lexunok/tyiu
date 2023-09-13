package com.tyiu.corn.service;

import com.tyiu.corn.model.dto.IdeaDTO;
import com.tyiu.corn.model.dto.RatingDTO;
import com.tyiu.corn.model.dto.UserDTO;
import com.tyiu.corn.model.entities.Group;
import com.tyiu.corn.model.entities.Idea;
import com.tyiu.corn.model.enums.ProjectType;
import com.tyiu.corn.model.enums.Role;
import com.tyiu.corn.model.enums.StatusIdea;
import com.tyiu.corn.repository.IdeaRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.modelmapper.ModelMapper;
import org.modelmapper.TypeToken;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

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

        when(ideaRepository.findAllByInitiator(initiator)).thenReturn(Flux.fromIterable(ideas));
        when(mapper.map(idea, IdeaDTO.class)).thenReturn(ideaDTO);

        Flux<IdeaDTO> result = ideaService.getListIdeaForInitiator(initiator);

        // Assert
        assertEquals(ideasDTO, result.collectList().block());
        verify(ideaRepository).findAllByInitiator(initiator);
    }


    @Test
    void testGetListIdea(){
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

        when(ideaRepository.findAll()).thenReturn(Flux.fromIterable(ideas));
        when(mapper.map(idea1, IdeaDTO.class)).thenReturn(ideaDTO1);
        when(mapper.map(idea2, IdeaDTO.class)).thenReturn(ideaDTO2);

        // Act
        Flux<IdeaDTO> result = ideaService.getListIdea();

        // Assert
        assertEquals(ideasDTO, result.collectList().block());
        verify(ideaRepository).findAll();
    }

    @Test
    public void testGetIdeaForInitiator() {
        String initiator = "example@example.com";

        Idea idea = Idea.builder()
                .initiator(initiator)
                .build();

        IdeaDTO ideaDTO = IdeaDTO.builder()
                .initiator(initiator)
                .build();

        when(ideaRepository.findById(idea.getId())).thenReturn(Mono.just(idea));
        when(mapper.map(idea, IdeaDTO.class)).thenReturn(ideaDTO);

        // Act
        Mono<IdeaDTO> result = ideaService.getIdeaForInitiator(idea.getId());

        // Assert
        assertEquals(ideaDTO, result.block());
        verify(ideaRepository).findById(idea.getId());
    }

    @Test
    void testSaveIdea() {
        String initiator = "email@email.com";

        IdeaDTO ideaDTO = IdeaDTO.builder()
                .name("Идея 1")
                .build();


        Idea idea = Idea.builder()
                .name("Идея 1")
                .build();

        when(mapper.map(ideaDTO, Idea.class)).thenReturn(idea);
        when(ideaRepository.save(idea)).thenReturn(Mono.just(idea));
        when(mapper.map(idea, IdeaDTO.class)).thenReturn(ideaDTO);

        // Act
        Mono<IdeaDTO> savedIdeaMono = ideaService.saveIdea(ideaDTO, initiator);
        IdeaDTO savedIdeaDTO = savedIdeaMono.block();

        // Assert
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
        when(ideaRepository.deleteById(idea.getId())).thenReturn(Mono.empty());
        ideaService.deleteIdeaByInitiator(idea.getId());

        // Then
        verify(ideaRepository).deleteById(idea.getId());
    }
    @Test
    void testDeleteIdeaByAdmin(){
        String initiator = "example@example.com";
        Idea idea = Idea.builder()
                .initiator(initiator)
                .name("Идея 1")
                .build();
        // When
        when(ideaRepository.deleteById(idea.getId())).thenReturn(Mono.empty());
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
                .build();
        Idea idea = Idea.builder()
                .initiator("example@example.com")
                .name("Идея 2")
                .build();

        // When
        when(ideaRepository.findById(idea.getId())).thenReturn(Mono.just(idea));
        when(ideaRepository.save(idea)).thenReturn(Mono.just(idea));
        ideaService.updateIdeaByAdmin(idea.getId(), updatedIdea);

        // Then
        verify(ideaRepository).findById(idea.getId());
        verify(ideaRepository).save(idea);
    }

    @Test
    void testUpdateStatusByProjectOffice(){
        //Given
        RatingDTO ratingDTO = RatingDTO.builder().status(StatusIdea.ON_APPROVAL).build();

        IdeaDTO ideaDTO = IdeaDTO.builder()
                .name("Идея 1")
                .build();


        Idea idea = Idea.builder()
                .name("Идея 2")
                .build();


        when(ideaRepository.findById(idea.getId())).thenReturn(Mono.just(idea));
        when(ideaRepository.save(idea)).thenReturn(Mono.just(idea));
        ideaService.updateStatusByProjectOffice(idea.getId(), ratingDTO.getStatus());

        verify(ideaRepository).findById(idea.getId());
        verify(ideaRepository).save(idea);
        assertEquals(ratingDTO.getStatus(), idea.getStatus());
    }

//    @Test
//    void testUpdateStatusByExpert(){
//        RatingDTO ratingDTO = RatingDTO.builder()
//                .status(StatusIdea.CONFIRMED)
//                .rating(2)
//                .marketValue("3000")
//                .originality("Очень оригинально")
//                .technicalFeasibility("Очень возможно")
//                .understanding("Очень понятно")
//                .build();
//        Group group = new Group();
//        group.setUsers(List.of(UserDTO.builder().build()));
//        Idea idea = Idea.builder()
//                .experts(group)
//                .confirmedBy(List.of(""))
//                .status(StatusIdea.ON_APPROVAL)
//                .rating(2)
//                .marketValue("2000")
//                .originality("Оригинально")
//                .technicalRealizability(1L)
//                .understanding("Понятно")
//                .build();
//
//        when(ideaRepository.findById(idea.getId())).thenReturn(Mono.just(idea));
//        when(ideaRepository.save(idea)).thenReturn(Mono.just(idea));
//        ideaService.updateStatusByExpert(idea.getId(), ratingDTO);
//
//        // Then
//        verify(ideaRepository).findById(idea.getId());
//        verify(ideaRepository).save(idea);
//        assertEquals(ratingDTO.getStatus(), idea.getStatus());
//        assertEquals(ratingDTO.getRating(), idea.getRating());
//    }
}
