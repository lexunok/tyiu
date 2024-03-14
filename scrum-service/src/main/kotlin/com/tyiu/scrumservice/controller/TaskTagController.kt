package com.tyiu.scrumservice.controller


import com.tyiu.scrumservice.model.InfoResponse
import com.tyiu.scrumservice.model.TaskTagDTO
import com.tyiu.scrumservice.model.TaskTagRequest
import com.tyiu.scrumservice.service.TaskTagService
import kotlinx.coroutines.reactor.asFlux
import org.springframework.http.HttpStatus
import org.springframework.security.access.prepost.PreAuthorize
import org.springframework.web.bind.annotation.*
import reactor.core.publisher.Flux
import reactor.core.publisher.Mono
import reactor.kotlin.core.publisher.toMono

@RestController
@RequestMapping("/api/v1/scrum-service/tag")
class TaskTagController(private val tagService: TaskTagService) {

    @GetMapping("/{projectId}/all")
    @PreAuthorize("hasAnyRole('INITIATOR', 'PROJECT_OFFICE', 'MEMBER', 'TEAM_OWNER', 'ADMIN')")
    fun getAllTags(@PathVariable projectId: String): Flux<TaskTagDTO> = tagService.getAllTags(projectId).asFlux()

    @PostMapping("/{projectId}/add")
    //@PreAuthorize("hasAnyRole('INITIATOR', 'PROJECT_OFFICE', 'MEMBER', 'TEAM_OWNER', 'ADMIN')")
    suspend fun createTag(@PathVariable projectId: String,@RequestBody taskTagDTO: TaskTagDTO): Mono<TaskTagDTO> = tagService.createTag(projectId, taskTagDTO).toMono()

    @PutMapping("/{tagId}/update")
    //@PreAuthorize("hasAnyRole('INITIATOR', 'PROJECT_OFFICE', 'MEMBER', 'TEAM_OWNER', 'ADMIN')")
    suspend fun updateTag(@PathVariable tagId: String, @RequestBody taskTagRequest: TaskTagRequest) : Mono<InfoResponse> {
        return Mono.just(tagService.updateTag(tagId,taskTagRequest))
            .thenReturn(InfoResponse(HttpStatus.OK,"Тег был изменён успешно"))
            .onErrorReturn(InfoResponse(HttpStatus.BAD_REQUEST,"Тег не был изменён"))
    }

    @DeleteMapping("/{tagId}/delete")
    //@PreAuthorize("hasAnyRole('INITIATOR', 'PROJECT_OFFICE', 'MEMBER', 'TEAM_OWNER', 'ADMIN')")
    suspend fun deleteTag(@PathVariable tagId: String): Mono<InfoResponse> {
        return Mono.just(tagService.deleteTag(tagId))
            .thenReturn(InfoResponse(HttpStatus.OK,"Тег удалён успешно"))
            .onErrorReturn(InfoResponse(HttpStatus.BAD_REQUEST,"Тег не был удалён"))
    }
}