package com.tyiu.corn.controller;

import java.util.List;

import com.tyiu.corn.model.dto.GroupDTO;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import com.tyiu.corn.model.entities.Group;
import com.tyiu.corn.service.GroupService;

import lombok.RequiredArgsConstructor;

@RestController
@RequiredArgsConstructor
@RequestMapping("/api/v1/group")
public class GroupController {
    @Autowired
    private final GroupService groupService;

    @GetMapping("/all")
    public List<GroupDTO> getGroupById() {
        return groupService.getGroups();
    }

    @GetMapping("/{id}")
    public GroupDTO getGroupById(@PathVariable Long id) {
        return groupService.getGroupById(id);
    }

    @PostMapping("/add")
    public GroupDTO createGroup(@RequestBody GroupDTO group) {
        return groupService.createGroup(group);
    }
    @PutMapping("/update/{id}")
    public GroupDTO createGroup(@PathVariable Long id,@RequestBody GroupDTO group) {
        return groupService.updateGroup(id, group);
    }
    @DeleteMapping("/delete/{id}")
    public void createGroup(@PathVariable Long id) {
        groupService.deleteGroup(id);
    }
    
}
    