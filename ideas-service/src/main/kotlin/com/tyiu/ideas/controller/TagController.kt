package com.tyiu.ideas.controller


import com.tyiu.ideas.config.exception.AccessException
import com.tyiu.ideas.model.InfoResponse
import com.tyiu.ideas.model.TagDTO
import com.tyiu.ideas.model.TaskTagRequest
import com.tyiu.ideas.model.entities.User
import com.tyiu.ideas.model.enums.Role
import com.tyiu.ideas.service.TagService
import com.tyiu.ideas.util.roleCheck
import kotlinx.coroutines.flow.Flow
import org.springframework.http.HttpStatus
import org.springframework.security.core.annotation.AuthenticationPrincipal
import org.springframework.web.bind.annotation.*

@RestController
@RequestMapping("/api/v1/scrum-service/tag")
class TagController(private val tagService: TagService) {

    @GetMapping("/all")
    fun getAllTags(@AuthenticationPrincipal user: User): Flow<TagDTO> {
        return if (user.roles.roleCheck(listOf(Role.INITIATOR,Role.PROJECT_OFFICE,Role.MEMBER,Role.TEAM_OWNER,Role.ADMIN))) {
            tagService.getAllTags()
        }
        else {
            throw AccessException("Нет прав")
        }
    }

    @PostMapping("/add")
    suspend fun createTag(@RequestBody tagDTO: TagDTO,
                          @AuthenticationPrincipal user: User): TagDTO {
        return if (user.roles.roleCheck(listOf(Role.ADMIN))) {
            tagService.createTag(tagDTO, user.id)
        }
        else {
            throw AccessException("Нет прав")
        }
    }

    @PostMapping("/add/no-confirmed")
    suspend fun createNoConfirmedTag(@RequestBody tagDTO: TagDTO,
                                     @AuthenticationPrincipal user: User) : TagDTO {
        return if (user.roles.roleCheck(listOf(Role.INITIATOR,Role.PROJECT_OFFICE,Role.MEMBER,Role.TEAM_OWNER,Role.ADMIN))) {
            tagService.createNoConfirmedTag(tagDTO, user.id)
        }
        else {
            throw AccessException("Нет прав")
        }
    }

    @PutMapping("/confirm/{tagId}")
    suspend fun confirmTag(@PathVariable tagId: String,
                           @AuthenticationPrincipal user: User): InfoResponse{
        return if (user.roles.roleCheck(listOf(Role.ADMIN))) {
            try {
                tagService.confirmTag(user.id, tagId)
                InfoResponse(HttpStatus.OK,"Тег утверждён")
            }
            catch(e: Exception){
                InfoResponse(HttpStatus.BAD_REQUEST,"Не удалось утвердить тег")
            }
        }
        else {
            throw AccessException("Нет прав")
        }
    }

    @PutMapping("/update/{tagId}")
    suspend fun updateTag(@PathVariable tagId: String,
                          @RequestBody tagDTO: TagDTO,
                          @AuthenticationPrincipal user: User): InfoResponse{
        return if (user.roles.roleCheck(listOf(Role.ADMIN))) {
            try {
                tagService.updateTag(tagDTO, user.id, tagId)
                InfoResponse(HttpStatus.OK,"Тег обновлён успешно")
            }
            catch(e: Exception){
                InfoResponse(HttpStatus.BAD_REQUEST,"Ошибка при обновлении тега")
            }
        }
        else {
            throw AccessException("Нет прав")
        }
    }

    @DeleteMapping("/delete/{tagId}")
    suspend fun deleteTag(@PathVariable tagId: String, @AuthenticationPrincipal user: User): InfoResponse {
        return if (user.roles.roleCheck(listOf(Role.ADMIN))) {
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