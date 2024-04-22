package com.tyiu.ideas.model

import org.springframework.data.relational.core.mapping.Table

@Table
data class ProjectMarks(
        val projectId:String? = null,
        val userId:String? = null,
        val mark:Double? = null,
)

data class ProjectMarksDTO(
        val projectId:String? = null,
        val userId: String? = null,
        var firstName: String? = null,
        var lastName: String? = null,
        var projectRole: ProjectRole = ProjectRole.MEMBER,
        val mark:Double? = null,
        var tasks: List<TaskDTO>? = null,
)