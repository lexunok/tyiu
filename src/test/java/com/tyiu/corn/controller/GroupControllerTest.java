package com.tyiu.corn.controller;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.List;

import org.junit.Test;
import org.junit.jupiter.api.BeforeEach;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.http.ResponseEntity;
import org.springframework.http.HttpStatus;
import com.tyiu.corn.model.entities.Group;
import com.tyiu.corn.service.GroupService;

public class GroupControllerTest {
    @Mock
    private GroupService groupService;
    
    @InjectMocks
    private GroupController groupController;


    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
    }

    @Test
    void getGroupById_returnsListOfGroups() {
        Long id = 1L;
        List<Group> expectedGroups = new ArrayList<>();
        expectedGroups.add(new Group());
        expectedGroups.add(new Group());

        when(groupService.getGroupsById(id)).thenReturn(expectedGroups);

        ResponseEntity<List<Group>> response = groupController.getGroupById(id);

        assertThat(response.getStatusCode()).isEqualTo(HttpStatus.OK);
        assertThat(response.getBody()).isEqualTo(expectedGroups);
    }
    
    @Test
    void createGroup_returnsCreatedGroup() {
        Group group = new Group();
        Group createdGroup = new Group();
        
        when(groupService.createGroup(group)).thenReturn(createdGroup);
        
        ResponseEntity<Group> response = groupController.createGroup(group);
        
        assertThat(response.getStatusCode()).isEqualTo(HttpStatus.CREATED);
        assertThat(response.getBody()).isEqualTo(createdGroup);
    }
    
    @Test
    void createGroup_callsGroupServiceWithCorrectGroup() {
        Group group = new Group();

        groupController.createGroup(group);
        
        ArgumentCaptor<Group> captor = ArgumentCaptor.forClass(Group.class);
        verify(groupService).createGroup(captor.capture());
        assertThat(captor.getValue()).isEqualTo(group);
    }

    
}