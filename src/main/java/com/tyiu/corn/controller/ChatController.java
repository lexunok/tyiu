package com.tyiu.corn.controller;


import org.springframework.web.bind.annotation.*;

import com.tyiu.corn.model.entities.Message;

import lombok.RequiredArgsConstructor;

@RestController
@RequiredArgsConstructor
public class ChatController {
    
//    @MessageMapping("/chat")
//    @SendTo("/topic/messages")
//    public Message sendMessage(@Payload Message message) {
//        return message;
//    }
}
