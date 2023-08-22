package com.tyiu.corn.service;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.tyiu.corn.model.entities.Group;
import com.tyiu.corn.repository.GroupRepository;

import lombok.RequiredArgsConstructor;

@Service
@RequiredArgsConstructor
public class GroupService {
    
    @Autowired
    private final GroupRepository groupRepository;

    public Group getGroupById(Long id) {
        return groupRepository.findById(id).orElseThrow();
    }
    
    public Group createGroup(Group group) {
        Group savedGroup = groupRepository.save(group);
        return savedGroup;
    }
    
    public Group updateGroup(Long id, Group group) {
    Group existingGroup = groupRepository.findById(id).orElseThrow();
    return groupRepository.save(existingGroup);
}
    
    public void deleteGroup(Long id) {
    Group group = groupRepository.findById(id).orElseThrow();
    groupRepository.delete(group);
}
      
}



