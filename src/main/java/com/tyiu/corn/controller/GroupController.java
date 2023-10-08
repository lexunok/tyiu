package com.tyiu.corn.controller;


import com.tyiu.corn.model.dto.GroupDTO;
import org.springframework.beans.factory.annotation.Autowired;

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
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
    public Mono<GroupDTO> updateGroup(@PathVariable String id, @RequestBody GroupDTO group) {
        return groupService.updateGroup(id, group);

    }
    @DeleteMapping("/delete/{id}")
    public Mono<ResponseEntity<String>> deleteGroup(@PathVariable String id) {
        groupService.deleteGroup(id).subscribe();
        return Mono.just(new ResponseEntity<>("Success deleting", HttpStatus.OK));
    }
    
}
    