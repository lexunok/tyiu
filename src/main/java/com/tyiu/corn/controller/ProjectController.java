package com.tyiu.corn.controller;


import com.tyiu.corn.model.dto.ProjectDTO;
import com.tyiu.corn.model.entities.ProjectRequest;
import com.tyiu.corn.model.entities.ProjectInvitation;
import com.tyiu.corn.model.responses.AuthenticationResponse;
import com.tyiu.corn.service.ProjectService;
import lombok.RequiredArgsConstructor;
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
        return projectService.getAll();
    }

    @GetMapping("/get/{projectId}")
    public Mono<ProjectDTO> getProject(@PathVariable String projectId){
        return projectService.getProject(projectId);
    }

    @GetMapping("/invites")
    public Flux<ProjectInvitation> getProjectInvitations(Principal principal){
        return projectService.getProjectInvitations(principal.getName());
    }

    @GetMapping("/applications/{projectId}")
    public Flux<ProjectRequest> getProjectApplications(@PathVariable String projectId){
        return projectService.getProjectApplications(projectId);
    }

    //////////////////////////////
    //   ___   ____    ____ ______
    //  / _ \ / __ \  / __//_  __/
    // / ___// /_/ / _\ \   / /
    ///_/    \____/ /___/  /_/
    //////////////////////////////

    @PostMapping("/create")
    public Mono<ProjectDTO> createProject(@RequestBody ProjectDTO projectDTO){
        return projectService.createProject(projectDTO);
    }

    @PostMapping("/send/invite/{projectId}")
    public Mono<ProjectInvitation> sendInvitation(@RequestBody AuthenticationResponse invitation, @PathVariable String projectId){
        return projectService.sendInvitation(invitation.getEmail(), projectId);
    }

    @PostMapping("/send/application/{projectId}")
    public Mono<ProjectRequest> sendApplication(Principal principal, @PathVariable String projectId){
        return projectService.sendApplication(principal.getName(), projectId);
    }

    ///////////////////////////////////////////
    //   ___    ____   __    ____ ______   ____
    //  / _ \  / __/  / /   / __//_  __/  / __/
    // / // / / _/   / /__ / _/   / /    / _/
    ///____/ /___/  /____//___/  /_/    /___/
    ///////////////////////////////////////////

    @DeleteMapping("/delete/{projectId}")
    public Mono<Void> deleteProject(@PathVariable String projectId){
        return projectService.deleteProject(projectId);
    }

    @DeleteMapping("/delete/invite/{inviteId}")
    public Mono<Void> deleteInvite(@PathVariable String inviteId){
        return projectService.deleteInvite(inviteId);
    }

    @DeleteMapping("/delete/application/{applicationId}")
    public Mono<Void> deleteApplication(@PathVariable String applicationId){
        return projectService.deleteApplication(applicationId);
    }

    ////////////////////////
    //   ___   __  __ ______
    //  / _ \ / / / //_  __/
    // / ___// /_/ /  / /
    ///_/    \____/  /_/
    ////////////////////////

    @PutMapping("/update/{projectId}")
    public Mono<Void> updateProject(@RequestBody ProjectDTO projectDTO, @PathVariable String projectId){
        return projectService.updateProject(projectDTO, projectId);
    }

    @PutMapping("/invite/{projectId}")
    public Mono<Void> inviteInProject(@PathVariable String projectId, @RequestBody AuthenticationResponse invitation){
        return projectService.addInProject(projectId, invitation.getEmail());
    }

    @PutMapping("/kick/{projectId}")
    public Mono<Void> kickFromProject(@PathVariable String projectId, @RequestBody AuthenticationResponse invitation){
        return projectService.kickFromProject(projectId, invitation.getEmail());
    }

}
