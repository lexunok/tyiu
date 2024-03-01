package com.tyiu.scrumservice.controller

import org.springframework.security.access.prepost.PreAuthorize
import org.springframework.security.core.Authentication
import org.springframework.security.core.annotation.AuthenticationPrincipal
import org.springframework.security.oauth2.jwt.Jwt
import org.springframework.web.bind.annotation.GetMapping
import org.springframework.web.bind.annotation.RequestMapping
import org.springframework.web.bind.annotation.RestController
import reactor.core.publisher.Mono

@RestController
@RequestMapping("/api/v1/scrum-service")
class ResourceController {

    @GetMapping("/resource")
    fun getResource(@AuthenticationPrincipal jwt:Jwt): Mono<Jwt> = Mono.just(jwt)

    @GetMapping("/authorization")
    fun getAuthorization(@AuthenticationPrincipal authorization: Authentication): Mono<Authentication> = Mono.just(authorization)

    @GetMapping("/userId/admin")
    @PreAuthorize("hasRole('ADMIN')")
    fun getUserIdAdmin(@AuthenticationPrincipal jwt:Jwt): Mono<String> {
        return Mono.just("Ваш ID = " + jwt.id + " и ваша роль Админ")
    }

    @GetMapping("/userId/initiator")
    @PreAuthorize("hasRole('INITIATOR')")
    fun getUserIdInitiator(@AuthenticationPrincipal jwt:Jwt): Mono<String> {
        return Mono.just("Ваш ID = " + jwt.id + " и ваша роль Инициатор")
    }
}