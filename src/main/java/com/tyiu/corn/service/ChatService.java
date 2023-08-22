package com.tyiu.corn.service;

import org.springframework.stereotype.Service;

import com.tyiu.corn.model.entities.Chat;
import com.tyiu.corn.model.entities.Message;
import com.tyiu.corn.repository.ChatRepository;

import lombok.RequiredArgsConstructor;

@Service
@RequiredArgsConstructor
public class ChatService {

    private final ChatRepository chatRepository;

    /*
    public List<Chat> getUserChats(String email) {
        return chatRepository.findByMembers_Email(email);
    }

    public Chat createChat(Chat chat, String email) {
        User user = userRepository.findByEmail(email).orElseThrow(() -> new RuntimeException());
        chat.setMembers(List.of(user));
        return chatRepository.save(chat);
    }
    */

    public Message addMessageToChat(Long chatId, Message message) {
        Chat chat = chatRepository.findById(chatId).orElseThrow(() -> new RuntimeException());
        chat.getMessages().add(message);
        chatRepository.save(chat);
        return message;
    }

    /*
    public List<Message> getChatMessages(Long chatId) {
        Chat chat = chatRepository.findById(chatId).orElseThrow(() -> new RuntimeException());
        return chat.getMessages();
    }
    */
}
