package com.tyiu.scrumservice.controller;

import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.bind.annotation.RequestMapping;


@RestController
@RequestMapping("/api/v1/scrum-service")
public class ResourceController {
    
    @GetMapping("/resource")
    public String getResource() {
        return "Resource";
    }
}
