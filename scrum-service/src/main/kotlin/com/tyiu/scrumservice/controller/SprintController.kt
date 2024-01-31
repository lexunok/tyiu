package com.tyiu.scrumservice.controller

import com.tyiu.scrumservice.model.Sprint
import com.tyiu.scrumservice.model.SprintDTO
import com.tyiu.scrumservice.model.SprintMarks
import com.tyiu.scrumservice.service.SprintService
import kotlinx.coroutines.flow.Flow
import org.springframework.web.bind.annotation.GetMapping
import org.springframework.web.bind.annotation.PathVariable
import org.springframework.web.bind.annotation.RequestMapping
import org.springframework.web.bind.annotation.RestController
import java.math.BigInteger

@RestController
@RequestMapping("/api/v1/scrum-service/sprint")
class SprintController(private val sprintService: SprintService)
{
    @GetMapping("{projectId}/all")
    fun getAllSprintsByProject(@PathVariable projectId: String): Flow<SprintDTO> = sprintService.getAllSprintsByProject(projectId)

    @GetMapping("{id}")
    fun getSprintById(@PathVariable id: BigInteger): Flow<SprintDTO> = sprintService.getSprintById(id)

    @GetMapping("{projectId}/active")
    fun getActiveSprint(@PathVariable projectId: String): Flow<SprintDTO> = sprintService.getActiveSprint(projectId)

    @GetMapping("marks/{sprintId}/all")
    fun getAllSprintMarks(@PathVariable sprintId: String): Flow<SprintMarks> = sprintService.getAllSprintMarks(sprintId)

}