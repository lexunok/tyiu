package com.tyiu.scrumservice.service

import com.tyiu.scrumservice.model.*
import kotlinx.coroutines.flow.map
import com.tyiu.scrumservice.model.SprintDTO
import com.tyiu.scrumservice.model.SprintRepository
import kotlinx.coroutines.flow.Flow
import org.springframework.stereotype.Service
import java.math.BigInteger

@Service
class SprintService (
        private val sprintRepository: SprintRepository,
        private val sprintMarksRepository: SprintMarksRepository
)
{
    fun getAllSprintsByProject(projectId: String): Flow<SprintDTO> =
            sprintRepository.findAllSprintsByProject(projectId).map {
                s -> val sprint = s.toDTO()
                return@map sprint
            }
    fun getSprintById(id: BigInteger): Flow<SprintDTO> =
            sprintRepository.findSprintById(id).map{
                s -> val sprint = s.toDTO()
                return@map sprint
            }
    fun getActiveSprint(projectId: String): Flow<SprintDTO> =
            sprintRepository.findActiveSprint(projectId).map{
                s -> val sprint = s.toDTO()
                return@map sprint
            }
    fun getAllSprintMarks(sprintId: String): Flow<SprintMarks> = sprintMarksRepository.findSprintMarks(sprintId)
}

