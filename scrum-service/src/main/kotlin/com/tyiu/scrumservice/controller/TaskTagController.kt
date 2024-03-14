package com.tyiu.scrumservice.controller


import com.tyiu.ideas.model.entities.User
import com.tyiu.ideas.model.enums.Role
import com.tyiu.scrumservice.model.InfoResponse
import com.tyiu.scrumservice.model.TaskTagDTO
import com.tyiu.scrumservice.model.TaskTagRequest
import com.tyiu.scrumservice.service.TaskTagService
import kotlinx.coroutines.flow.Flow
import org.springframework.http.HttpStatus
import org.springframework.security.core.annotation.AuthenticationPrincipal
import org.springframework.web.bind.annotation.*

@RestController
@RequestMapping("/api/v1/scrum-service/tag")
class TaskTagController(private val tagService: TaskTagService) {

    @GetMapping("/{projectId}/all")
    fun getAllTags(@PathVariable projectId: String,@AuthenticationPrincipal user: User): Flow<TaskTagDTO>? {
        return if (user.roles.contains(Role.MEMBER)||user.roles.contains(Role.INITIATOR)||user.roles.contains(Role.TEAM_OWNER)
            ||user.roles.contains(Role.ADMIN)||user.roles.contains(Role.PROJECT_OFFICE)) {
            tagService.getAllTags(projectId)}
        else {null}
    }

    @PostMapping("/{projectId}/add")
    suspend fun createTag(@PathVariable projectId: String,@RequestBody taskTagDTO: TaskTagDTO,
                          @AuthenticationPrincipal user: User): TaskTagDTO? {
        return if (user.roles.contains(Role.MEMBER)||user.roles.contains(Role.INITIATOR)||user.roles.contains(Role.TEAM_OWNER)
            ||user.roles.contains(Role.ADMIN)||user.roles.contains(Role.PROJECT_OFFICE)) {
            tagService.createTag(projectId, taskTagDTO)}
        else {null}
    }

    @PutMapping("/{tagId}/update")
    suspend fun updateTag(@PathVariable tagId: String, @RequestBody taskTagRequest: TaskTagRequest,
                          @AuthenticationPrincipal user: User) : InfoResponse {
        return if (user.roles.contains(Role.MEMBER)||user.roles.contains(Role.INITIATOR)||user.roles.contains(Role.TEAM_OWNER)
            ||user.roles.contains(Role.ADMIN)||user.roles.contains(Role.PROJECT_OFFICE)) {
            try {
                tagService.updateTag(tagId,taskTagRequest)
                InfoResponse(HttpStatus.OK,"Тег был изменён успешно")
            }
            catch(e: Exception){
                InfoResponse(HttpStatus.BAD_REQUEST,"Тег не был изменён")
            } }
        else {InfoResponse(HttpStatus.BAD_REQUEST,"Тег не был изменён")}
    }

    @DeleteMapping("/{tagId}/delete")
    suspend fun deleteTag(@PathVariable tagId: String,@AuthenticationPrincipal user: User): InfoResponse {
        return if (user.roles.contains(Role.MEMBER)||user.roles.contains(Role.INITIATOR)||user.roles.contains(Role.TEAM_OWNER)
            ||user.roles.contains(Role.ADMIN)||user.roles.contains(Role.PROJECT_OFFICE)) {
            try {
                tagService.deleteTag(tagId)
                InfoResponse(HttpStatus.OK,"Тег удалён успешно")
            }
            catch(e: Exception){
                InfoResponse(HttpStatus.BAD_REQUEST,"Тег не был удалён")
            } }
        else {InfoResponse(HttpStatus.BAD_REQUEST,"Тег не был удалён")}
    }
}