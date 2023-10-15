package com.tyiu.corn.controller;

import com.tyiu.corn.model.dto.GroupDTO;

import com.tyiu.corn.model.responses.AuthenticationResponse;
import org.springframework.web.bind.annotation.*;
import com.tyiu.corn.service.GroupService;

import lombok.RequiredArgsConstructor;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.util.List;

@RestController
@RequiredArgsConstructor
@RequestMapping("/api/v1/group")
public class GroupController {

    private final GroupService groupService;

    ///////////////////////
    //  _____   ____ ______
    // / ___/  / __//_  __/
    /// (_ /  / _/   / /
    //\___/  /___/  /_/
    ///////////////////////

    @GetMapping("/all")
    public Flux<GroupDTO> getGroups() {
        return groupService.getGroups();
    }

    @GetMapping("/{groupId}")
    public Mono<GroupDTO> getGroupById(@PathVariable Long groupId) {
        return groupService.getGroupById(groupId);
    }

    //////////////////////////////
    //   ___   ____    ____ ______
    //  / _ \ / __ \  / __//_  __/
    // / ___// /_/ / _\ \   / /
    ///_/    \____/ /___/  /_/
    //////////////////////////////

    @PostMapping("/create")
    public Mono<GroupDTO> createGroup(@RequestBody GroupDTO group) {
        return groupService.createGroup(group);
    }

    ///////////////////////////////////////////
    //   ___    ____   __    ____ ______   ____
    //  / _ \  / __/  / /   / __//_  __/  / __/
    // / // / / _/   / /__ / _/   / /    / _/
    ///____/ /___/  /____//___/  /_/    /___/
    ///////////////////////////////////////////

    @DeleteMapping("/delete/{groupId}")
    public Mono<Void> deleteGroup(@PathVariable Long groupId) {
        return groupService.deleteGroup(groupId);
    }

    ////////////////////////
    //   ___   __  __ ______
    //  / _ \ / / / //_  __/
    // / ___// /_/ /  / /
    ///_/    \____/  /_/
    ////////////////////////

    @PutMapping("/add/{groupId}")
    public Mono<Void> addInGroup(@PathVariable Long groupId, @RequestBody AuthenticationResponse response) {
        return groupService.addInGroup(groupId, response.getId());
    }

    @PutMapping("/add-list/{groupId}")
    public Mono<Void> addListInGroup(@PathVariable Long groupId, @RequestBody List<Long> usersId) {
        return groupService.addListInGroup(groupId, usersId);
    }

    @PutMapping("/kick/{groupId}")
    public Mono<Void> kickFromGroup(@PathVariable Long groupId, @RequestBody AuthenticationResponse response) {
        return groupService.kickFromGroup(groupId, response.getId());
    }

    @PutMapping("/update/{groupId}")
    public Mono<Void> updateGroup(@PathVariable Long groupId, @RequestBody GroupDTO group) {
        return groupService.updateGroup(groupId, group);
    }
}
    