package com.tyiu.scrumservice.model

import org.springframework.data.annotation.Id
import org.springframework.data.relational.core.mapping.Table
import org.springframework.data.repository.kotlin.CoroutineCrudRepository

interface ProjectRepository: CoroutineCrudRepository<Project, String>
@Table
data class Project (
        @Id
        val id:String,
        val name:String,
        val status: String
)

data class ProjectDTO (
        val id:String,
        val name:String,
        val status: String
)
