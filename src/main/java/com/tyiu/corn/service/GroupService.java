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
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

@Service
@RequiredArgsConstructor
@CacheConfig(cacheNames = "groups")
public class GroupService {

    private final GroupRepository groupRepository;
    private final ModelMapper mapper;

//    @Cacheable
    public Flux<GroupDTO> getGroups() {
        List<Group> groups = groupRepository.findAll();
        return Flux.just(groups).map(g -> mapper.map(g, GroupDTO.class));
    }
//    @Cacheable(key = "#id")
    public Mono<GroupDTO> getGroupById(Long id) {
        Group group = groupRepository.findById(id).orElseThrow(() -> new NotFoundException("Not Found"));
        return Mono.just(mapper.map(group, GroupDTO.class));
    }
    //@CacheEvict(allEntries = true)
    public Mono<GroupDTO> createGroup(GroupDTO groupDTO) {
        Group group = mapper.map(groupDTO, Group.class);
        group = groupRepository.save(group);
        return Mono.just(mapper.map(group, GroupDTO.class));
    }
    //@CacheEvict(allEntries = true)
    public void updateGroup(Long id,GroupDTO groupDTO) {
        Group group = groupRepository.findById(id).orElseThrow(() -> new NotFoundException("Not found"));
        group.setName(groupDTO.getName());
        groupRepository.save(group);
    }
    //@CacheEvict(allEntries = true)
    public void deleteGroup(Long id) {
        groupRepository.deleteById(id);
    }
}



