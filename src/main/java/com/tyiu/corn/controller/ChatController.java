package com.tyiu.corn.controller;

import java.util.List;

import org.springframework.web.bind.annotation.*;

import com.tyiu.corn.model.entities.Chat;
import com.tyiu.corn.model.entities.Message;
import com.tyiu.corn.service.ChatService;

@RestController
@RequestMapping("api/v1/chat")
public class ChatController {

    private final ChatService chatService;

    @GetMapping
    public List<Message> findChat(@PathVariable Long id){
        return chatService.;
    }

    @PostMapping
    public Message sendMessageToLocal(@PathVariable Long id, @RequestBody Message message){
        return chatService.;
    }
    
}
