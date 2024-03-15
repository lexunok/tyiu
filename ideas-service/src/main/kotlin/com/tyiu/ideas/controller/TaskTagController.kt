package com.tyiu.ideas.controller


import com.tyiu.ideas.model.InfoResponse
import com.tyiu.ideas.model.TaskTagDTO
import com.tyiu.ideas.model.TaskTagRequest
import com.tyiu.ideas.model.enums.Role
import com.tyiu.ideas.service.TaskTagService
import kotlinx.coroutines.flow.Flow
import org.springframework.http.HttpStatus
import org.springframework.security.core.annotation.AuthenticationPrincipal
import org.springframework.security.oauth2.jwt.Jwt
import org.springframework.web.bind.annotation.*

@RestController
@RequestMapping("/api/v1/scrum-service/tag")
class TaskTagController(private val tagService: TaskTagService) {

    private fun roleCheck(jwt: Jwt): Boolean {
        return (jwt.getClaimAsStringList("roles").contains(Role.MEMBER.toString())
                || jwt.getClaimAsStringList("roles").contains(Role.INITIATOR.toString())
                || jwt.getClaimAsStringList("roles").contains(Role.TEAM_OWNER.toString())
                || jwt.getClaimAsStringList("roles").contains(Role.ADMIN.toString())
                || jwt.getClaimAsStringList("roles").contains(Role.PROJECT_OFFICE.toString()))
    }

    @GetMapping("/{projectId}/all")
    fun getAllTags(@PathVariable projectId: String,@AuthenticationPrincipal jwt: Jwt): Flow<TaskTagDTO>? {
        return if (roleCheck(jwt))
        {
            tagService.getAllTags(projectId)
        }
        else {null}
    }

    @PostMapping("/{projectId}/add")
    suspend fun createTag(@PathVariable projectId: String,@RequestBody taskTagDTO: TaskTagDTO,
                          @AuthenticationPrincipal jwt: Jwt): TaskTagDTO? {
        return if (roleCheck(jwt))
        {
            tagService.createTag(projectId, taskTagDTO)
        }
        else {null}
    }

    @PutMapping("/{tagId}/update")
    suspend fun updateTag(@PathVariable tagId: String, @RequestBody taskTagRequest: TaskTagRequest,
                          @AuthenticationPrincipal jwt: Jwt) : InfoResponse {
        return if (roleCheck(jwt))
        {
            try {
                tagService.updateTag(tagId,taskTagRequest)
                InfoResponse(HttpStatus.OK,"Тег был изменён успешно")
            }
            catch(e: Exception){
                InfoResponse(HttpStatus.BAD_REQUEST,"Тег не был изменён")
            }
        }
        else {InfoResponse(HttpStatus.BAD_REQUEST,"Тег не был изменён")}
    }

    @DeleteMapping("/{tagId}/delete")
    suspend fun deleteTag(@PathVariable tagId: String,@AuthenticationPrincipal jwt: Jwt): InfoResponse {
        return if (roleCheck(jwt))
        {
            try {
                tagService.deleteTag(tagId)
                InfoResponse(HttpStatus.OK,"Тег удалён успешно")
            }
            catch(e: Exception){
                InfoResponse(HttpStatus.BAD_REQUEST,"Тег не был удалён")
            }
        }
        else {InfoResponse(HttpStatus.BAD_REQUEST,"Тег не был удалён")}
    }
}