package com.tyiu.corn.service;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import com.tyiu.corn.model.entities.Group;
import com.tyiu.corn.repository.GroupRepository;

import lombok.RequiredArgsConstructor;

@Service
public class GroupService {
    
    @Autowired
    private final GroupRepository groupRepository;

    public Group getGroupById(Long id) {
        Group group = groupRepository.findById(id).orElseThrow();
    }
    
    public Group createGroup(Group group) {
        Group savedGroup = groupRepository.save(group);
        return savedGroup;
    }
    
    public Group updateGroup(Long id, Group group) {
        Group group = groupRepository.findById(id).orElseThrow();
        Group updatedGroup = groupRepository.save(group);
    }
    
    public void deleteGroup(Long id) {
        Group group = groupRepository.findById(id).orElseThrow();
        groupRepository.delete(group);
    }
      
}