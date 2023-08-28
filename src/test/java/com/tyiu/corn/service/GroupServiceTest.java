package com.tyiu.corn.service;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.List;
import java.util.Optional;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.modelmapper.ModelMapper;
import com.tyiu.corn.model.dto.GroupDTO;
import com.tyiu.corn.model.entities.Group;
import com.tyiu.corn.repository.GroupRepository;

@ExtendWith(MockitoExtension.class)
public class GroupServiceTest {

    @Mock
    private GroupRepository groupRepository;
    
    @Mock
    private ModelMapper mapper;

    @InjectMocks
    private GroupService groupService;

    @BeforeEach
    void setUp() {
        groupService = new GroupService(groupRepository, mapper);
    }
//
    @Test
    public void testGetGroups() {
        // Arrange
        List<Group> groups = Arrays.asList(new Group(), new Group());
        when(groupRepository.findAll()).thenReturn(groups);
        when(mapper.map(any(Group.class), eq(GroupDTO.class))).thenReturn(new GroupDTO());
        
        // Act
        List<GroupDTO> result = groupService.getGroups();
        
        // Assert
        assertEquals(groups.size(), result.size());
        verify(groupRepository, times(1)).findAll();
        verify(mapper, times(groups.size())).map(any(Group.class), eq(GroupDTO.class));
    }
    
    
    @Test
    public void testGetGroupById() {
        // Arrange
        Long groupId = 1L;
        Group group = new Group();
        when(groupRepository.findById(groupId)).thenReturn(Optional.of(group));
        when(mapper.map(any(Group.class), eq(GroupDTO.class))).thenReturn(new GroupDTO());
        
        // Act
        GroupDTO result = groupService.getGroupById(groupId);
        
        // Assert
        assertNotNull(result);
        verify(groupRepository, times(1)).findById(groupId);
        verify(mapper, times(1)).map(any(Group.class), eq(GroupDTO.class));
    }

    @Test
    public void createGroupTest() {
        GroupDTO groupDTO = new GroupDTO();
        Group group = new Group();
        when(mapper.map(groupDTO, Group.class)).thenReturn(group);
        when(groupRepository.save(group)).thenReturn(group);
        when(mapper.map(group, GroupDTO.class)).thenReturn(groupDTO);
        
        GroupDTO result = groupService.createGroup(groupDTO);
        
        verify(mapper, times(1)).map(groupDTO, Group.class);
        verify(groupRepository, times(1)).save(group);
        verify(mapper, times(1)).map(group, GroupDTO.class);
        assertEquals(result, groupDTO);
    }
    

    @Test
    public void testUpdateGroup() {
        // Given
        GroupDTO updatedGroup = new GroupDTO();
        Group group = new Group();

        // When
        when(groupRepository.findById(group.getId())).thenReturn(Optional.of(group));
        when(groupRepository.save(group)).thenReturn(group);
        groupService.updateGroup(group.getId(), updatedGroup);
        // Then
        assertEquals(updatedGroup.getName(), group.getName());
        verify(groupRepository).save(group);
        verify(groupRepository).save(group);
    }

    @Test
    public void testDeleteGroup(){
        
        Group group = Group.builder()
                .name(" G1")
                .build();
        
        groupService.deleteGroup(group.getId());
        verify(groupRepository).deleteById(group.getId());
    }
}

