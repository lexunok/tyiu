package com.tyiu.scrumservice.controller

import com.tyiu.ideas.model.dto.IdeaMarketDTO
import com.tyiu.scrumservice.model.*
import com.tyiu.scrumservice.service.ProjectService
import kotlinx.coroutines.reactor.asFlux
import org.springframework.http.HttpStatus
import org.springframework.security.access.prepost.PreAuthorize
import org.springframework.security.core.annotation.AuthenticationPrincipal
import org.springframework.security.oauth2.jwt.Jwt
import org.springframework.web.bind.annotation.*
import reactor.core.publisher.Flux
import reactor.core.publisher.Mono
import reactor.kotlin.core.publisher.toMono

@RestController
@RequestMapping("/api/v1/scrum-service/project")
class ProjectController (private val projectService: ProjectService) {

    @GetMapping("/all")
    @PreAuthorize("hasAnyRole('PROJECT_OFFICE' , 'ADMIN')")
    fun getAllProjects(): Flux<ProjectDTO> = projectService.getAllProjects().asFlux()

    @GetMapping("/private/all")
    @PreAuthorize("hasAnyRole('INITIATOR', 'MEMBER', 'TEAM_OWNER', 'ADMIN')")
    fun getYourProjects(@AuthenticationPrincipal jwt: Jwt): Flux<ProjectDTO> = projectService.getYourProjects(jwt.id).asFlux()

    @GetMapping("/active/all")
    @PreAuthorize("hasAnyRole('INITIATOR', 'MEMBER', 'TEAM_OWNER', 'ADMIN')")
    fun getYourActiveProjects(@AuthenticationPrincipal jwt: Jwt): Flux<ProjectDTO> = projectService.getYourActiveProjects(jwt.id).asFlux()

    @GetMapping("/{projectId}")
    //@PreAuthorize("hasAnyRole('INITIATOR', 'PROJECT_OFFICE', 'MEMBER', 'TEAM_OWNER', 'ADMIN')")
    suspend fun getOneProject(@PathVariable projectId: String): Mono<ProjectDTO> = projectService.getOneProject(projectId).toMono()

    @GetMapping("/members/{projectId}/all")
    @PreAuthorize("hasAnyRole('INITIATOR', 'PROJECT_OFFICE', 'MEMBER', 'TEAM_OWNER', 'ADMIN')")
    fun getProjectMembers(@PathVariable projectId: String): Flux<ProjectMemberDTO>? = projectService.getProjectMembers(projectId)?.asFlux()

    @GetMapping("/marks/{projectId}/all")
    @PreAuthorize("hasAnyRole('INITIATOR', 'PROJECT_OFFICE', 'MEMBER', 'TEAM_OWNER', 'ADMIN')")
    fun getProjectMarks(@PathVariable projectId: String): Flux<ProjectMarksDTO>? = projectService.getProjectMarks(projectId)?.asFlux()

    @PostMapping("/send")
    //@PreAuthorize("hasAnyRole('PROJECT_OFFICE' , 'ADMIN')")
    suspend fun createProject(@RequestBody ideaMarketDTO: IdeaMarketDTO): Flux<ProjectMember> = projectService.createProject(ideaMarketDTO).asFlux()

    @PostMapping("/{projectId}/add/members")
    //@PreAuthorize("hasAnyRole('TEAM_OWNER', 'ADMIN')")
    suspend fun addMembersInProject(@PathVariable projectId: String, @RequestBody teamMemberRequest: TeamMemberRequest): Mono<ProjectMember> = projectService.addMembersInProject(projectId,teamMemberRequest).toMono()

    @PostMapping("/{projectId}/add/marks")
    //@PreAuthorize("hasAnyRole('INITIATOR', 'PROJECT_OFFICE', 'ADMIN')")
    suspend fun addMarksInProject(@PathVariable projectId: String,@RequestBody projectMarksRequest: projectMarksRequest): Mono<ProjectMarks> = projectService.addMarksInProject(projectId, projectMarksRequest).toMono()

    @PutMapping("/{projectId}/status/change")
    //@PreAuthorize("hasAnyRole('INITIATOR', 'PROJECT_OFFICE', 'ADMIN')")
    suspend fun pauseProject(@PathVariable projectId: String) : Mono<InfoResponse> {
        return Mono.just(projectService.pauseProject(projectId))
            .thenReturn(InfoResponse(HttpStatus.OK,"Статус проекта изменён"))
            .onErrorReturn(InfoResponse(HttpStatus.BAD_REQUEST,"Статус проекта не был изменён"))
    }

    @PutMapping("/{projectId}/finish/change")
    //@PreAuthorize("hasAnyRole('INITIATOR', 'PROJECT_OFFICE', 'ADMIN')")
    suspend fun putFinishProject(@PathVariable projectId: String,@RequestBody projectFinishRequest: ProjectFinishRequest) : Mono<InfoResponse> {
        return Mono.just(projectService.putFinishProject(projectId,projectFinishRequest))
            .thenReturn(InfoResponse(HttpStatus.OK,"Проект успешно завершён"))
            .onErrorReturn(InfoResponse(HttpStatus.BAD_REQUEST,"Проект не был завершён"))
    }

    @PutMapping("/leader/change")
    //@PreAuthorize("hasAnyRole('INITIATOR', 'PROJECT_OFFICE', 'ADMIN')")
    suspend fun putTeamLeader(@RequestBody projectLeaderRequest: ProjectLeaderRequest) : Mono<InfoResponse> {
        return Mono.just(projectService.putTeamLeader(projectLeaderRequest))
            .thenReturn(InfoResponse(HttpStatus.OK,"Лидер команды назначён"))
            .onErrorReturn(InfoResponse(HttpStatus.BAD_REQUEST,"Лидер команды не был назначен"))
    }

}