package com.tyiu.ideas.controller;

import com.tyiu.ideas.config.exception.NotFoundException;
import com.tyiu.ideas.model.dto.GroupDTO;

import com.tyiu.ideas.model.entities.User;
import com.tyiu.ideas.model.responses.InfoResponse;
import org.springframework.http.HttpStatus;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;
import com.tyiu.ideas.service.GroupService;

import lombok.RequiredArgsConstructor;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

@RestController
@RequiredArgsConstructor
@RequestMapping("/api/v1/ideas-service/group")
public class GroupController {

    private final GroupService groupService;

    @GetMapping("/all")
    @PreAuthorize("hasAuthority('ADMIN')")
    public Flux<GroupDTO> getGroups() {
        return groupService.getGroups();
    }

    @GetMapping("/{groupId}")
    @PreAuthorize("hasAuthority('ADMIN')")
    public Mono<GroupDTO> getGroupById(@PathVariable String groupId) {
        return groupService.getGroupById(groupId)
                .switchIfEmpty(Mono.error(new NotFoundException("Not found!")));
    }

    @PostMapping("/create")
    @PreAuthorize("hasAuthority('ADMIN')")
    public Mono<GroupDTO> createGroup(@RequestBody GroupDTO group, @AuthenticationPrincipal User admin) {
        return groupService.createGroup(group, admin)
                .switchIfEmpty(Mono.error(new NotFoundException("Not success!")));
    }


    @DeleteMapping("/delete/{groupId}")
    @PreAuthorize("hasAuthority('ADMIN')")
    public Mono<InfoResponse> deleteGroup(@PathVariable String groupId, @AuthenticationPrincipal User admin) {
        return groupService.deleteGroup(groupId, admin)
                .thenReturn(new InfoResponse(HttpStatus.OK,"Success deleting"))
                .onErrorReturn(new InfoResponse(HttpStatus.BAD_REQUEST,"Delete is not success"));
    }

    @PutMapping("/update/{groupId}")
    @PreAuthorize("hasAuthority('ADMIN')")
    public Mono<GroupDTO> updateGroup(@PathVariable String groupId,
                                      @RequestBody GroupDTO group,
                                      @AuthenticationPrincipal User user) {
        return groupService.updateGroup(groupId, group, user)
                .switchIfEmpty(Mono.error(new NotFoundException("Update is not success")));
    }
}
    