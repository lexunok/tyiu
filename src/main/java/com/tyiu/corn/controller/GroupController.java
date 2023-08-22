package com.tyiu.corn.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.tyiu.corn.model.entities.Group;
import com.tyiu.corn.service.GroupService;

@RestController

@RequestMapping("/api/v1/group")
public class GroupController {
    
    @Autowired
    private final GroupService groupService;

    @GetMapping("/{id}")
    public ResponseEntity<Group> getGroupById(@PathVariable Long id) {
        final Group group = groupService.getGroupById(id);
        return ResponseEntity.ok(group);
    }
    
    @PostMapping
    public ResponseEntity<Group> createGroup(@RequestBody Group group) {
        final Group createdGroup = groupService.createGroup(group);
        return ResponseEntity.status(HttpStatus.CREATED).body(createdGroup);
    }
    
}
    