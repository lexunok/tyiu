package com.tyiu.ideas.controller


import com.tyiu.ideas.config.exception.AccessException
import com.tyiu.ideas.model.InfoResponse
import com.tyiu.ideas.model.TagDTO
import com.tyiu.ideas.model.enums.Role
import com.tyiu.ideas.service.TagService
import com.tyiu.ideas.util.roleCheck
import kotlinx.coroutines.flow.Flow
import org.springframework.http.HttpStatus
import org.springframework.security.core.annotation.AuthenticationPrincipal
import org.springframework.security.oauth2.jwt.Jwt
import org.springframework.web.bind.annotation.*

@RestController
@RequestMapping("/api/v1/scrum-service/tag")
class TagController(private val tagService: TagService) {

    private val member = Role.MEMBER.toString()
    private val initiator = Role.INITIATOR.toString()
    private val projectOffice = Role.PROJECT_OFFICE.toString()
    private val teamOwner = Role.TEAM_OWNER.toString()
    private val admin = Role.ADMIN.toString()

    private val roles = listOf(initiator,projectOffice,member,teamOwner,admin)

    @GetMapping("/all")
    fun getAllTags(@AuthenticationPrincipal jwt: Jwt): Flow<TagDTO> {
        return if (jwt.getClaimAsStringList("roles").roleCheck(roles)) {
            tagService.getAllTags()
        }
        else {
            throw AccessException("Нет прав")
        }
    }

    @PostMapping("/add")
    suspend fun createTag(@RequestBody tagDTO: TagDTO,
                          @AuthenticationPrincipal jwt: Jwt): TagDTO {
        return if (jwt.getClaimAsStringList("roles").roleCheck(listOf(admin))) {
            tagService.createTag(tagDTO, jwt.id)
        }
        else {
            throw AccessException("Нет прав")
        }
    }

    @PostMapping("/add/no-confirmed")
    suspend fun createNoConfirmedTag(@RequestBody tagDTO: TagDTO,
                                     @AuthenticationPrincipal jwt: Jwt) : TagDTO {
        return if (jwt.getClaimAsStringList("roles").roleCheck(roles)) {
            tagService.createNoConfirmedTag(tagDTO, jwt.id)
        }
        else {
            throw AccessException("Нет прав")
        }
    }

    @PutMapping("/confirm/{tagId}")
    suspend fun confirmTag(@PathVariable tagId: String,
                           @AuthenticationPrincipal jwt: Jwt): InfoResponse{
        return if (jwt.getClaimAsStringList("roles").roleCheck(listOf(admin))) {
            try {
                tagService.confirmTag(jwt.id, tagId)
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
                          @AuthenticationPrincipal jwt: Jwt): InfoResponse{
        return if (jwt.getClaimAsStringList("roles").roleCheck(listOf(admin))) {
            try {
                tagService.updateTag(tagDTO, jwt.id, tagId)
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
    suspend fun deleteTag(@PathVariable tagId: String, @AuthenticationPrincipal jwt: Jwt): InfoResponse {
        return if (jwt.getClaimAsStringList("roles").roleCheck(listOf(admin))) {
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