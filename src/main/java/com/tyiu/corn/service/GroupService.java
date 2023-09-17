package com.tyiu.corn.service;

import com.tyiu.corn.model.dto.GroupDTO;
import org.modelmapper.ModelMapper;
import org.springframework.stereotype.Service;

import com.tyiu.corn.model.entities.Group;
import com.tyiu.corn.repository.GroupRepository;
import lombok.RequiredArgsConstructor;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

@Service
@RequiredArgsConstructor
public class GroupService {

    private final GroupRepository groupRepository;

    private final ModelMapper mapper;

    public Flux<GroupDTO> getGroups() {
        Flux<Group> groups = groupRepository.findAll();
        return groups.flatMap(g -> Flux.just(mapper.map(g, GroupDTO.class)));
    }

    public Mono<GroupDTO> getGroupById(String id) {
        Mono<Group> group = groupRepository.findById(id);
        return group.flatMap(g -> Mono.just(mapper.map(g, GroupDTO.class)));
    }

    public Mono<GroupDTO> createGroup(GroupDTO groupDTO) {
        Mono<Group> group = groupRepository.save(mapper.map(groupDTO, Group.class));
        return group.flatMap(g -> Mono.just(mapper.map(g, GroupDTO.class)));
    }

    public void updateGroup(String id,GroupDTO groupDTO) {
        Mono<Group> group = groupRepository.findById(id);
        group.flatMap(g -> {
            g.setName(groupDTO.getName());
            return groupRepository.save(g);
        }).subscribe();
    }

    public void deleteGroup(String id) {
        groupRepository.deleteById(id).subscribe();
    }
}



