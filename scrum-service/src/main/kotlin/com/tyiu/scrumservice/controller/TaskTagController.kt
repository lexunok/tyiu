package com.tyiu.scrumservice.controller


import com.tyiu.scrumservice.model.TaskTag
import com.tyiu.scrumservice.model.TaskTagDTO
import com.tyiu.scrumservice.service.TaskTagService
import kotlinx.coroutines.flow.Flow
import org.springframework.web.bind.annotation.*

@RestController
@RequestMapping("/api/v1/scrum-service/tag")
class TaskTagController(private val tagService: TaskTagService) {

    @GetMapping("/all")
    fun getAllTags(): Flow<TaskTagDTO> = tagService.getAllTags()

    @PostMapping("/add")
    fun postTag(): Flow<TaskTag> = tagService.postTag()

    @PutMapping("/{tagId}/update")
    fun updateTag(@PathVariable tagId: String): Flow<TaskTag> = tagService.updateTag()

    @DeleteMapping("/{tagId}/delete")
    fun deleteTag(@PathVariable tagId: String): Flow<TaskTag> = tagService.deleteTag()
}