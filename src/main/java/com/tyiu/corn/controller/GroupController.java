package com.tyiu.corn.controller;


import com.tyiu.corn.model.dto.GroupDTO;
import com.tyiu.corn.model.dto.UserDTO;
import org.springframework.beans.factory.annotation.Autowired;

import org.springframework.web.bind.annotation.*;

import com.tyiu.corn.model.entities.Group;
import com.tyiu.corn.service.GroupService;

import lombok.RequiredArgsConstructor;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

@RestController
@RequiredArgsConstructor
@RequestMapping("/api/v1/group")
public class GroupController {
    @Autowired
    private final GroupService groupService;

    @GetMapping("/all")
    public Flux<GroupDTO> getGroups() {
        return groupService.getGroups();
    }

    @GetMapping("/{id}")
    public Mono<GroupDTO> getGroupById(@PathVariable String id) {
        return groupService.getGroupById(id);
    }

    @PostMapping("/add")
    public Mono<GroupDTO> createGroup(@RequestBody GroupDTO group) {
        return groupService.createGroup(group);
    }

    @PutMapping("/update/{id}")
    public Mono<Void> updateGroup(@PathVariable String id,@RequestBody GroupDTO group) {
        groupService.updateGroup(id, group);
        return Mono.empty();

    }
    @DeleteMapping("/delete/{id}")
    public Mono<Void> deleteGroup(@PathVariable String id) {
        groupService.deleteGroup(id);
        return Mono.empty();
    }
    
}
    