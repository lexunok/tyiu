package com.tyiu.corn.controller;

import com.tyiu.corn.config.exception.NotFoundException;
import com.tyiu.corn.model.dto.GroupDTO;

import com.tyiu.corn.model.responses.InfoResponse;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.*;
import com.tyiu.corn.service.GroupService;

import lombok.RequiredArgsConstructor;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

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
        return groupService.getGroups()
                .switchIfEmpty(Mono.error(new NotFoundException("Not found!")));
    }

    @GetMapping("/{groupId}")
    public Mono<GroupDTO> getGroupById(@PathVariable Long groupId) {
        return groupService.getGroupById(groupId)
                .switchIfEmpty(Mono.error(new NotFoundException("Not found!")));
    }

    //////////////////////////////
    //   ___   ____    ____ ______
    //  / _ \ / __ \  / __//_  __/
    // / ___// /_/ / _\ \   / /
    ///_/    \____/ /___/  /_/
    //////////////////////////////

    @PostMapping("/create")
    public Mono<GroupDTO> createGroup(@RequestBody GroupDTO group) {
        return groupService.createGroup(group)
                .switchIfEmpty(Mono.error(new NotFoundException("Not found!")));
    }

    ///////////////////////////////////////////
    //   ___    ____   __    ____ ______   ____
    //  / _ \  / __/  / /   / __//_  __/  / __/
    // / // / / _/   / /__ / _/   / /    / _/
    ///____/ /___/  /____//___/  /_/    /___/
    ///////////////////////////////////////////

    @DeleteMapping("/delete/{groupId}")
    public Mono<InfoResponse> deleteGroup(@PathVariable Long groupId) {
        return groupService.deleteGroup(groupId)
                .thenReturn(new InfoResponse(HttpStatus.OK,"Success deleting"))
                .onErrorReturn(new InfoResponse(HttpStatus.BAD_REQUEST,"Delete is not success"));
    }

    ////////////////////////
    //   ___   __  __ ______
    //  / _ \ / / / //_  __/
    // / ___// /_/ /  / /
    ///_/    \____/  /_/
    ////////////////////////

    @PutMapping("/update/{groupId}")
    public Mono<GroupDTO> updateGroup(@PathVariable Long groupId, @RequestBody GroupDTO group) {
        return groupService.updateGroup(groupId, group);
    }
}
    