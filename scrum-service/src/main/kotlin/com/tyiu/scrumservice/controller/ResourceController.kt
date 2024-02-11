package com.tyiu.scrumservice.controller

import org.springframework.web.bind.annotation.GetMapping
import org.springframework.web.bind.annotation.RequestMapping
import org.springframework.web.bind.annotation.RestController

@RestController
@RequestMapping("/api/v1/scrum-service")
class ResourceController {
    @GetMapping("/resource")
    fun getResource() = "resource"
}