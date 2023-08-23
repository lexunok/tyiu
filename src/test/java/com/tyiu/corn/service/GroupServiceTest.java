package com.tyiu.corn.service;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import org.junit.Test;
import org.junit.jupiter.api.BeforeEach;
import org.mockito.Mock;

import com.tyiu.corn.model.entities.Group;
import com.tyiu.corn.repository.GroupRepository;

public class GroupServiceTest {
    private GroupService groupService;

    @Mock
    private GroupRepository groupRepository;

    @BeforeEach
    public void setup() {
        groupService = new GroupService(groupRepository);
    }

    @Test
    public void testGetGroupsById() {
        Long id = 1L;
        Group group1 = new Group(id, "Group 1", null);
        Group group2 = new Group(id, "Group 2", null);
        List<Group> groups = new ArrayList<>();
        groups.add(group1);
        groups.add(group2);

        when(groupRepository.findAllById(id)).thenReturn(groups);

        List<Group> result = groupService.getGroupsById(id);

        assertEquals(groups, result);
        verify(groupRepository, times(1)).findAllById(id);
    }

    @Test
    public void testCreateGroup() {
        Group group = new Group(1L, "Group 1", null);

        when(groupRepository.save(group)).thenReturn(group);

        Group result = groupService.createGroup(group);

        assertEquals(group, result);
        verify(groupRepository, times(1)).save(group);
    }

    @Test
    public void testUpdateGroup() {
        Long id = 1L;
        Group group = new Group(id, "Updated Group", null);

        when(groupRepository.findById(id)).thenReturn(Optional.of(new Group(id, "Group", null)));
        when(groupRepository.save(group)).thenReturn(group);

        Group result = groupService.updateGroup(id, group);

        assertEquals(group, result);
        verify(groupRepository, times(1)).findById(id);
        verify(groupRepository, times(1)).save(group);
    }

    @Test
    public void testDeleteGroup() {
        Long id = 1L;
        Group group = new Group(id, "Group", null);

        when(groupRepository.findById(id)).thenReturn(Optional.of(group));

        groupService.deleteGroup(id);

        verify(groupRepository, times(1)).findById(id);
        verify(groupRepository, times(1)).delete(group);
    }
}