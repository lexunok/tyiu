package com.tyiu.corn.service;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cache.annotation.CacheConfig;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.stereotype.Service;

import com.tyiu.corn.model.entities.Group;
import com.tyiu.corn.repository.GroupRepository;
import lombok.RequiredArgsConstructor;

@Service
@RequiredArgsConstructor
@CacheConfig(cacheNames = "groups")
public class GroupService {
    
    @Autowired
    private final GroupRepository groupRepository;
    @Cacheable
    public List<Group> getGroups() {
        return groupRepository.findAll();
    }
    @Cacheable
    public Group getGroupById(Long id) {
        return groupRepository.findById(id).orElseThrow(() -> new RuntimeException(""));
    }
    @CacheEvict
    public Group createGroup(Group group) {
        return groupRepository.save(group);
    }
}



