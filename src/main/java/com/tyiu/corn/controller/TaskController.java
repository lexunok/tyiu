package com.tyiu.corn.controller;

import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/")
public class TaskController {
    @GetMapping("/task")
    public String showTask() {
        return "showTask";
    }
}
