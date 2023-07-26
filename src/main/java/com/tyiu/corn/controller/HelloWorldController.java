package com.tyiu.corn.controller;

import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.tyiu.corn.model.Scram;

import java.util.ArrayList;
import java.util.List;

@RestController
@RequestMapping("/api/v1")
public class HelloWorldController {

    List<Scram> list = new ArrayList<>();

    @GetMapping("/hello")
    public String sayHello(){
        return "Hello World!";
    }


    @GetMapping("/projects")
    public List<Scram> getScram(){
        return this.list;
    }

    @PostMapping("/projects")
    public  List<Scram> postScram(@RequestBody Scram scram){
        this.list.add(scram);
        return this.list;
    }
}
