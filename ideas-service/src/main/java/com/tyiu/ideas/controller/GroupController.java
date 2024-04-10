package com.tyiu.ideas.controller;

import com.tyiu.ideas.config.exception.NotFoundException;
import com.tyiu.ideas.model.dto.GroupDTO;

import com.tyiu.ideas.model.responses.InfoResponse;
import org.springframework.http.HttpStatus;
import org.springframework.security.access.prepost.PreAuthorize;
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
    @PreAuthorize("hasRole('ADMIN')")
    public Flux<GroupDTO> getGroups() {
        return groupService.getGroups();
    }

    @GetMapping("/{groupId}")
    @PreAuthorize("hasRole('ADMIN')")
    public Mono<GroupDTO> getGroupById(@PathVariable String groupId) {
        return groupService.getGroupById(groupId)
                .switchIfEmpty(Mono.error(new NotFoundException("Группа не найдена")));
    }

    @PostMapping("/create")
    @PreAuthorize("hasRole('ADMIN')")
    public Mono<GroupDTO> createGroup(@RequestBody GroupDTO group) {
        return groupService.createGroup(group)
                .switchIfEmpty(Mono.error(new NotFoundException("Не удалось создать группу")));
    }


    @DeleteMapping("/delete/{groupId}")
    @PreAuthorize("hasRole('ADMIN')")
    public Mono<InfoResponse> deleteGroup(@PathVariable String groupId) {
        return groupService.deleteGroup(groupId)
                .thenReturn(new InfoResponse(HttpStatus.OK,"Группа успешно удалена"))
                .onErrorReturn(new InfoResponse(HttpStatus.BAD_REQUEST,"Ошибка при удалении группы"));
    }

    @PutMapping("/update/{groupId}")
    @PreAuthorize("hasRole('ADMIN')")
    public Mono<GroupDTO> updateGroup(@PathVariable String groupId, @RequestBody GroupDTO group) {
        return groupService.updateGroup(groupId, group)
                .switchIfEmpty(Mono.error(new NotFoundException("Ошибка при обновлении группы")));
    }
}
    