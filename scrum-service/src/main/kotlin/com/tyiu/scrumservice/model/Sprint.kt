package com.tyiu.scrumservice.model


import kotlinx.coroutines.flow.Flow
import org.springframework.data.annotation.Id
import org.springframework.data.r2dbc.repository.Query
import org.springframework.data.relational.core.mapping.Table
import org.springframework.data.repository.kotlin.CoroutineCrudRepository
import java.time.LocalDate

interface SprintRepository: CoroutineCrudRepository<Sprint, String>{
    @Query("SELECT * FROM sprint WHERE project_id =:projectId")
    fun findAllSprintsByProject(projectId: String): Flow<Sprint>

    @Query("SELECT * FROM sprint WHERE project_id =:projectId and status = 'ACTIVE'")
    fun findActiveSprint(projectId: String): Flow<Sprint>


}
@Table

data class Sprint(
        @Id
        val id:String? = null,
        val projectId:String? = null,
        val name:String? = null,
        val goal:String? = null,
        val report:String? = null,
        val startDate:LocalDate? = LocalDate.now(),
        val finishDate:LocalDate? = null,
        val workingHours:Long? = null,
        var status:SprintStatus? = SprintStatus.ACTIVE,
)

enum class SprintStatus{
    ACTIVE, DONE
}

data class SprintDTO(
        val id:String? = null,
        val projectId:String? = null,
        val name:String? = null,
        val report:String? = null,
        val goal:String? = null,
        val startDate:LocalDate? = LocalDate.now(),
        val finishDate:LocalDate? = null,
        val workingHours:Long? = null,
        val tasks:HashMap<TaskStatus, List<TaskDTO>>? = null,

)

data class sprintStatusRequest(
        val sprintId:String? = null,
        val sprintStatus:String? = null,
)

data class sprintInfoRequest(
        val sprintId:String? = null,
        val sprintName:String? = null,
        val sprintGoal:String? = null,
        val startDate:LocalDate? = LocalDate.now(),
        val finishDate:LocalDate? = null,
        val sprintWorkingHours: Long? = null,
)
data class SprintFinishRequest(
        val sprintReport: String? = null,
        val finishDate: LocalDate? = null,
)

fun Sprint.toDTO(): SprintDTO = SprintDTO (
        id = id,
        projectId = projectId,
        goal = goal,
        name = name,
        report = report,
        startDate = startDate,
        finishDate = finishDate,
        workingHours = workingHours,
)