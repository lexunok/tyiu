package com.tyiu.scrumservice.controller


import com.tyiu.scrumservice.model.TaskTag
import com.tyiu.scrumservice.model.TaskTagDTO
import com.tyiu.scrumservice.model.TaskTagRequest
import com.tyiu.scrumservice.service.TaskTagService
import kotlinx.coroutines.flow.Flow
import org.springframework.web.bind.annotation.*
import java.math.BigInteger

@RestController
@RequestMapping("/api/v1/scrum-service/tag")
class TaskTagController(private val tagService: TaskTagService) {

    @GetMapping("/all")
    fun getAllTags(): Flow<TaskTagDTO> = tagService.getAllTags()

    @PostMapping("/add")
    suspend fun createTag(@RequestBody taskTagDTO: TaskTagDTO): TaskTagDTO = tagService.createTag(taskTagDTO)

    @PutMapping("/{tagId}/update")
    suspend fun updateTag(@PathVariable tagId: BigInteger, @RequestBody taskTagRequest: TaskTagRequest) = tagService.updateTag(tagId,taskTagRequest)

    @DeleteMapping("/{tagId}/delete")
    suspend fun deleteTag(@PathVariable tagId: BigInteger): Flow<TaskTag> = tagService.deleteTag(tagId)
}