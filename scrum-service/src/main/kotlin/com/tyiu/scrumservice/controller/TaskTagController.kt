package com.tyiu.scrumservice.controller


import com.tyiu.scrumservice.model.TaskTag
import com.tyiu.scrumservice.model.TaskTagDTO
import com.tyiu.scrumservice.model.TaskTagRequest
import com.tyiu.scrumservice.service.TaskTagService
import kotlinx.coroutines.flow.Flow
import org.springframework.web.bind.annotation.*

@RestController
@RequestMapping("/api/v1/scrum-service/tag")
class TaskTagController(private val tagService: TaskTagService) {

    @GetMapping("/{projectId}/all")
    fun getAllTags(@PathVariable projectId: String): Flow<TaskTagDTO> = tagService.getAllTags(projectId)

    @PostMapping("/{projectId}/add")
    suspend fun createTag(@PathVariable projectId: String,@RequestBody taskTagDTO: TaskTagDTO): TaskTagDTO = tagService.createTag(projectId, taskTagDTO)

    @PutMapping("/{tagId}/update")
    suspend fun updateTag(@PathVariable tagId: String, @RequestBody taskTagRequest: TaskTagRequest) = tagService.updateTag(tagId,taskTagRequest)

    @DeleteMapping("/{tagId}/delete")
    suspend fun deleteTag(@PathVariable tagId: String): Flow<TaskTag> = tagService.deleteTag(tagId)
}