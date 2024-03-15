package com.tyiu.ideas.controller

import com.tyiu.ideas.model.*
import com.tyiu.ideas.service.SprintService
import kotlinx.coroutines.flow.Flow
import org.springframework.web.bind.annotation.*

@RestController
@RequestMapping("/api/v1/scrum-service/sprint")
class SprintController(private val sprintService: SprintService)
{
    @GetMapping("{projectId}/all")
    fun getAllSprintsByProject(@PathVariable projectId: String): Flow<SprintDTO> = sprintService.getAllSprintsByProject(projectId)

    @GetMapping("{id}")
    suspend fun getSprintById(@PathVariable id: String): SprintDTO? = sprintService.getSprintById(id)

    @GetMapping("{projectId}/active")
    fun getActiveSprint(@PathVariable projectId: String): Flow<SprintDTO> = sprintService.getActiveSprint(projectId)

    @GetMapping("marks/{sprintId}/all")
    fun getAllSprintMarks(@PathVariable sprintId: String): Flow<SprintMarks> = sprintService.getAllSprintMarks(sprintId)

    @PostMapping("add")
    suspend fun createSprint(@RequestBody sprintDTO: SprintDTO): SprintDTO = sprintService.createSprint(sprintDTO)
    @PostMapping("{sprintId}/add/marks")
    suspend fun  addSprintMarks(@PathVariable sprintId: String, @RequestBody sprintMarksRequest: SprintMarksRequest) = sprintService.addSprintMarks(sprintId, sprintMarksRequest)

    @PutMapping("status/change")
    suspend fun changeSprintStatus(@RequestBody sprintStatusRequest: sprintStatusRequest) = sprintService.changeSprintStatus(sprintStatusRequest)
    @PutMapping("{sprintId}/update")
    suspend fun updateSprintInfo(@PathVariable sprintId: String, @RequestBody sprintInfoRequest: sprintInfoRequest) = sprintService.updateSprintInfo(sprintId, sprintInfoRequest)

    @PutMapping("{sprintId}/finish")
    suspend fun putSprintFinish(@PathVariable sprintId: String, @RequestBody sprintFinishRequest: SprintFinishRequest) = sprintService.putSprintFinish(sprintId, sprintFinishRequest)
    @DeleteMapping("{id}/delete")
    suspend fun deleteSprint(@PathVariable id: String) = sprintService.deleteSprint(id)

}