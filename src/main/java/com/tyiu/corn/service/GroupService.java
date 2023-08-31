package com.tyiu.corn.service;

import java.util.List;

import com.tyiu.corn.exception.NotFoundException;
import com.tyiu.corn.model.dto.GroupDTO;
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

//    @Cacheable
    public Flux<GroupDTO> getGroups() {
        Flux<Group> groups = groupRepository.findAll();
        return groups.cast(GroupDTO.class);
    }
//    @Cacheable(key = "#id")
    public Mono<GroupDTO> getGroupById(String id) {
        Mono<Group> group = groupRepository.findById(id);
        return group.cast(GroupDTO.class);
    }
    //@CacheEvict(allEntries = true)
    public Mono<GroupDTO> createGroup(GroupDTO groupDTO) {
        return Mono.just(groupDTO).cast(Group.class).flatMap(groupRepository::save).cast(GroupDTO.class);
    }
    //@CacheEvict(allEntries = true)
    public void updateGroup(String id,GroupDTO groupDTO) {
        Mono<Group> group = groupRepository.findById(id);
        group.flatMap(g -> {
            g.setName(groupDTO.getName());
            groupRepository.save(g);
            return null;
        });
    }
    //@CacheEvict(allEntries = true)
    public void deleteGroup(String id) {
        groupRepository.deleteById(id);
    }
}



