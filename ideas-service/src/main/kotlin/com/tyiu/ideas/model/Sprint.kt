package com.tyiu.ideas.model


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
    suspend fun findActiveSprint(projectId: String): Sprint


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
    var status: SprintStatus? = SprintStatus.ACTIVE,
)

enum class SprintStatus{
    ACTIVE, DONE
}

data class SprintDTO(
    val id:String? = null,
    val projectId:String? = null,
    val name:String? = null,
    val report:String? = null,
    val status:SprintStatus? = null,
    val goal:String? = null,
    val startDate:LocalDate? = null,
    val finishDate:LocalDate? = null,
    val workingHours:Long? = null,
    var tasks:List<TaskDTO>? = null,
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
    status = status,
    report = report,
    startDate = startDate,
    finishDate = finishDate,
    workingHours = workingHours,
)