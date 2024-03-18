package com.tyiu.ideas.controller


import com.tyiu.ideas.config.exception.AccessException
import com.tyiu.ideas.model.InfoResponse
import com.tyiu.ideas.model.TaskTagDTO
import com.tyiu.ideas.model.TaskTagRequest
import com.tyiu.ideas.model.entities.User
import com.tyiu.ideas.model.enums.Role
import com.tyiu.ideas.service.TaskTagService
import com.tyiu.ideas.util.roleCheck
import kotlinx.coroutines.flow.Flow
import org.springframework.http.HttpStatus
import org.springframework.security.core.annotation.AuthenticationPrincipal
import org.springframework.web.bind.annotation.*

@RestController
@RequestMapping("/api/v1/scrum-service/tag")
class TaskTagController(private val tagService: TaskTagService) {

    @GetMapping("/{projectId}/all")
    fun getAllTags(@PathVariable projectId: String,@AuthenticationPrincipal user: User): Flow<TaskTagDTO> {
        return if (user.roles.roleCheck(listOf(Role.INITIATOR,Role.PROJECT_OFFICE,Role.MEMBER,Role.TEAM_OWNER,Role.ADMIN))) {
            tagService.getAllTags(projectId)
        }
        else {
            throw AccessException("Нет прав")
        }
    }

    @PostMapping("/{projectId}/add")
    suspend fun createTag(@PathVariable projectId: String,@RequestBody taskTagDTO: TaskTagDTO,
                          @AuthenticationPrincipal user: User): TaskTagDTO {
        return if (user.roles.roleCheck(listOf(Role.INITIATOR,Role.PROJECT_OFFICE,Role.MEMBER,Role.TEAM_OWNER,Role.ADMIN))) {
            tagService.createTag(projectId, taskTagDTO)
        }
        else {
            throw AccessException("Нет прав")
        }
    }

    @PutMapping("/{tagId}/update")
    suspend fun updateTag(@PathVariable tagId: String, @RequestBody taskTagRequest: TaskTagRequest,
                          @AuthenticationPrincipal user: User) : InfoResponse {
        return if (user.roles.roleCheck(listOf(Role.INITIATOR,Role.PROJECT_OFFICE,Role.MEMBER,Role.TEAM_OWNER,Role.ADMIN))) {
            try {
                tagService.updateTag(tagId,taskTagRequest)
                InfoResponse(HttpStatus.OK,"Тег был изменён успешно")
            }
            catch(e: Exception){
                InfoResponse(HttpStatus.BAD_REQUEST,"Тег не был изменён")
            }
        }
        else {
            throw AccessException("Нет прав")
        }
    }

    @DeleteMapping("/{tagId}/delete")
    suspend fun deleteTag(@PathVariable tagId: String,@AuthenticationPrincipal user: User): InfoResponse {
        return if (user.roles.roleCheck(listOf(Role.INITIATOR,Role.PROJECT_OFFICE,Role.MEMBER,Role.TEAM_OWNER,Role.ADMIN))) {
            try {
                tagService.deleteTag(tagId)
                InfoResponse(HttpStatus.OK,"Тег удалён успешно")
            }
            catch(e: Exception){
                InfoResponse(HttpStatus.BAD_REQUEST,"Тег не был удалён")
            }
        }
        else {
            throw AccessException("Нет прав")
        }
    }
}