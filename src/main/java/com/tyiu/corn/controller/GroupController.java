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

    @GetMapping("/all")
    public Flux<GroupDTO> getGroups() {
        return groupService.getGroups();
    }

    @GetMapping("/{groupId}")
    public Mono<GroupDTO> getGroupById(@PathVariable String groupId) {
        return groupService.getGroupById(groupId)
                .switchIfEmpty(Mono.error(new NotFoundException("Not found!")));
    }

    @PostMapping("/create")
    public Mono<GroupDTO> createGroup(@RequestBody GroupDTO group) {
        return groupService.createGroup(group)
                .switchIfEmpty(Mono.error(new NotFoundException("Not success!")));
    }


    @DeleteMapping("/delete/{groupId}")
    public Mono<InfoResponse> deleteGroup(@PathVariable String groupId) {
        return groupService.deleteGroup(groupId)
                .thenReturn(new InfoResponse(HttpStatus.OK,"Success deleting"))
                .onErrorReturn(new InfoResponse(HttpStatus.BAD_REQUEST,"Delete is not success"));
    }

    @PutMapping("/update/{groupId}")
    public Mono<GroupDTO> updateGroup(@PathVariable String groupId, @RequestBody GroupDTO group) {
        return groupService.updateGroup(groupId, group)
                .switchIfEmpty(Mono.error(new NotFoundException("Update is not success")));
    }
}
    