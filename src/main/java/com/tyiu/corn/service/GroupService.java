package com.tyiu.corn.service;

import java.util.List;

import com.tyiu.corn.exception.NotFoundException;
import com.tyiu.corn.model.dto.GroupDTO;
import com.tyiu.corn.model.entities.User;
import org.modelmapper.ModelMapper;
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

    private final GroupRepository groupRepository;
    private final ModelMapper mapper;

    @Cacheable
    public List<GroupDTO> getGroups() {
        return groupRepository.findAll().stream().map(g -> mapper.map(g, GroupDTO.class)).toList();
    }
    @Cacheable
    public GroupDTO getGroupById(Long id) {
        Group group = groupRepository.findById(id).orElseThrow(() -> new NotFoundException("Not Found"));
        return mapper.map(group, GroupDTO.class);
    }
    @CacheEvict
    public GroupDTO createGroup(GroupDTO groupDTO) {
        Group group = mapper.map(groupDTO, Group.class);
        group = groupRepository.save(group);
        return mapper.map(group, GroupDTO.class);
    }
    @CacheEvict
    public GroupDTO updateGroup(Long id,GroupDTO groupDTO) {
        Group group = groupRepository.findById(id).orElseThrow(() -> new NotFoundException("Not found"));
        group.setName(groupDTO.getName());
        group = groupRepository.save(group);
        return mapper.map(group, GroupDTO.class);
    }
    @CacheEvict
    public void deleteGroup(Long id) {
        groupRepository.deleteById(id);
    }
}



