package com.tyiu.scrumservice.controller

import org.springframework.security.core.annotation.AuthenticationPrincipal
import org.springframework.security.oauth2.jwt.Jwt
import org.springframework.web.bind.annotation.GetMapping
import org.springframework.web.bind.annotation.RequestMapping
import org.springframework.web.bind.annotation.RestController

@RestController
@RequestMapping("/api/v1/scrum-service")
class ResourceController {
    @GetMapping("/resource")
    fun getResource(@AuthenticationPrincipal jwt:Jwt) = jwt

    @GetMapping("/userId")
    fun getUserId(@AuthenticationPrincipal jwt:Jwt) = jwt.id
}