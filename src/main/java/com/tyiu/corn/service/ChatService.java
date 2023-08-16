package com.tyiu.corn.service;

import org.springframework.stereotype.Service;

import com.tyiu.corn.repository.ChatRepository;

@Service
@RequiredArgsConstructor
public class ChatService {

    private final ChatRepository chatRepository;

    public void addMessageToChat(Long chatId, Message message) {
        Chat chat = chatRepository.findById(chatId).orElseThrow(() -> new RuntimeException());
        chat.getMessages().add(message);
        chatRepository.save(chat);
    }

    public List<Message> getChatMessages(Long chatId) {
        Chat chat = chatRepository.findById(chatId).orElseThrow(() -> new RuntimeException());
        return chat.getMessages();
    }

}
