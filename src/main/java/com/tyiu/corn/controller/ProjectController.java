package com.tyiu.corn.controller;


import com.tyiu.corn.config.exception.NotFoundException;
import com.tyiu.corn.model.dto.ProjectDTO;
import com.tyiu.corn.model.entities.ProjectRequest;
import com.tyiu.corn.model.entities.ProjectInvitation;
import com.tyiu.corn.model.responses.AuthenticationResponse;
import com.tyiu.corn.model.responses.InfoResponse;
import com.tyiu.corn.service.ProjectService;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.*;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.security.Principal;

@RestController
@RequestMapping("/api/v1/project")
@RequiredArgsConstructor
public class ProjectController {

    private final ProjectService projectService;

    ///////////////////////
    //  _____   ____ ______
    // / ___/  / __//_  __/
    /// (_ /  / _/   / /
    //\___/  /___/  /_/
    ///////////////////////

    @GetMapping("/all")
    public Flux<ProjectDTO> getAllProject(){
        return projectService.getAll()
                .switchIfEmpty(Mono.error(new NotFoundException("Not found!")));
    }

    @GetMapping("/get/{projectId}")
    public Mono<ProjectDTO> getProject(@PathVariable Long projectId){
        return projectService.getProject(projectId)
                .switchIfEmpty(Mono.error(new NotFoundException("Not found!")));
    }

    @GetMapping("/invites")
    public Flux<ProjectInvitation> getProjectInvitations(Principal principal){
        return projectService.getProjectInvitations(Long.valueOf(principal.getName()))
                .switchIfEmpty(Mono.error(new NotFoundException("Not found!")));
    }

    @GetMapping("/request/{projectId}")
    public Flux<ProjectRequest> getProjectRequests(@PathVariable Long projectId){
        return projectService.getProjectRequests(projectId)
                .switchIfEmpty(Mono.error(new NotFoundException("Not found!")));
    }

    //////////////////////////////
    //   ___   ____    ____ ______
    //  / _ \ / __ \  / __//_  __/
    // / ___// /_/ / _\ \   / /
    ///_/    \____/ /___/  /_/
    //////////////////////////////

    @PostMapping("/create")
    public Mono<ProjectDTO> createProject(@RequestBody ProjectDTO projectDTO){
        return projectService.createProject(projectDTO)
                .switchIfEmpty(Mono.error(new NotFoundException("Not found!")));
    }

    @PostMapping("/send/invite/{projectId}")
    public Mono<ProjectInvitation> sendInvitation(@RequestBody AuthenticationResponse invitation, @PathVariable Long projectId){
        return projectService.sendInvitation(invitation.getId(), projectId)
                .switchIfEmpty(Mono.error(new NotFoundException("Not found!")));
    }

    @PostMapping("/send/request/{projectId}")
    public Mono<ProjectRequest> sendApplication(Principal principal, @PathVariable Long projectId){
        return projectService.sendApplication(Long.valueOf(principal.getName()), projectId)
                .switchIfEmpty(Mono.error(new NotFoundException("Not found!")));
    }

    @PostMapping("/invite/{projectId}")
    public Mono<InfoResponse> inviteInProject(@PathVariable Long projectId, @RequestBody AuthenticationResponse invitation){
        return projectService.addInProject(projectId, invitation.getId())
                .thenReturn(new InfoResponse(HttpStatus.OK,"Successful invitation"))
                .onErrorReturn(new InfoResponse(HttpStatus.BAD_REQUEST,"The invitation was not successful"));
    }

    ///////////////////////////////////////////
    //   ___    ____   __    ____ ______   ____
    //  / _ \  / __/  / /   / __//_  __/  / __/
    // / // / / _/   / /__ / _/   / /    / _/
    ///____/ /___/  /____//___/  /_/    /___/
    ///////////////////////////////////////////

    @DeleteMapping("/delete/{projectId}")
    public Mono<InfoResponse> deleteProject(@PathVariable Long projectId){
        return projectService.deleteProject(projectId)
                .thenReturn(new InfoResponse(HttpStatus.OK,"Success deleting"))
                .onErrorReturn(new InfoResponse(HttpStatus.BAD_REQUEST,"Delete is not success"));
    }

    @DeleteMapping("/delete/invite/{inviteId}")
    public Mono<InfoResponse> deleteInvite(@PathVariable Long inviteId){
        return projectService.deleteInvite(inviteId)
                .thenReturn(new InfoResponse(HttpStatus.OK,"Success deleting"))
                .onErrorReturn(new InfoResponse(HttpStatus.BAD_REQUEST,"Delete is not success"));
    }

    @DeleteMapping("/delete/request/{requestId}")
    public Mono<InfoResponse> deleteRequest(@PathVariable Long requestId){
        return projectService.deleteRequest(requestId)
                .thenReturn(new InfoResponse(HttpStatus.OK,"Success deleting"))
                .onErrorReturn(new InfoResponse(HttpStatus.BAD_REQUEST,"Delete is not success"));
    }

    @DeleteMapping("/kick/{projectId}")
    public Mono<InfoResponse> kickFromProject(@PathVariable Long projectId, @RequestBody AuthenticationResponse invitation){
        return projectService.kickFromProject(projectId, invitation.getId())
                .thenReturn(new InfoResponse(HttpStatus.OK,"Successful exclusion"))
                .onErrorReturn(new InfoResponse(HttpStatus.BAD_REQUEST,"The exception was not successful"));
    }

    ////////////////////////
    //   ___   __  __ ______
    //  / _ \ / / / //_  __/
    // / ___// /_/ /  / /
    ///_/    \____/  /_/
    ////////////////////////

    @PutMapping("/update/{projectId}")
    public Mono<InfoResponse> updateProject(@RequestBody ProjectDTO projectDTO, @PathVariable Long projectId){
        return projectService.updateProject(projectDTO, projectId)
                .thenReturn(new InfoResponse(HttpStatus.OK,"Success updating"))
                .onErrorReturn(new InfoResponse(HttpStatus.BAD_REQUEST,"Update is not success"));
    }

}
