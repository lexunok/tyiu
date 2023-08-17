package com.tyiu.corn.controller;

import java.security.Principal;
import java.util.List;

import org.springframework.web.bind.annotation.*;

import com.tyiu.corn.model.entities.Chat;
import com.tyiu.corn.model.entities.Message;
import com.tyiu.corn.service.ChatService;

import lombok.RequiredArgsConstructor;

@RestController
@RequestMapping("api/v1/chat")
@RequiredArgsConstructor
public class ChatController {

    private final ChatService chatService;

    @GetMapping("/all")
    public List<Chat> getUserChats(Principal principal){
        return chatService.getUserChats(principal.getName());
    }

    @GetMapping("/get/{id}")
    public List<Message> findChat(@PathVariable Long id){
        return chatService.getChatMessages(id);
    }

    @PostMapping("/add/{id}")
    public Message sendMessageToLocal(@PathVariable Long id, @RequestBody Message message){
        return chatService.addMessageToChat(id, message);
    } 

    @PostMapping("/create")
    public Chat createChat(@RequestBody Chat chat,Principal principal){
        return chatService.createChat(chat, principal.getName());
    } 
}
