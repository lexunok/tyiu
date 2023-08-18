package com.tyiu.corn.service;

import java.util.List;
import java.util.Optional;

import org.springframework.stereotype.Service;

import com.tyiu.corn.model.entities.Chat;
import com.tyiu.corn.model.entities.Message;
import com.tyiu.corn.model.entities.User;
import com.tyiu.corn.repository.ChatRepository;
import com.tyiu.corn.repository.UserRepository;

import lombok.RequiredArgsConstructor;

@Service
@RequiredArgsConstructor
public class ChatService {

    private final ChatRepository chatRepository;
    private final UserRepository userRepository;

    public List<Chat> getUserChats(String email) {
        return chatRepository.findByMembers_Email(email);
    }

    public Chat createChat(Chat chat, String email) {
        User user = userRepository.findByEmail(email).orElseThrow(() -> new RuntimeException());
        chat.setMembers(List.of(user));
        return chatRepository.save(chat);
    }

    public Message addMessageToChat(Long chatId, Message message) {
        Chat chat = chatRepository.findById(chatId).orElseThrow(() -> new RuntimeException());
        chat.getMessages().add(message);
        chatRepository.save(chat);
        return message;
    }

    public List<Message> getChatMessages(Long chatId) {
        Chat chat = chatRepository.findById(chatId).orElseThrow(() -> new RuntimeException());
        return chat.getMessages();
    }

}
