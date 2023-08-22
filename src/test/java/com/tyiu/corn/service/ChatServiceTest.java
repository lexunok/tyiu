package com.tyiu.corn.service;

import com.tyiu.corn.model.entities.Chat;
import com.tyiu.corn.model.entities.Message;
import com.tyiu.corn.model.entities.User;
import com.tyiu.corn.repository.ChatRepository;
import com.tyiu.corn.repository.UserRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
public class ChatServiceTest {
    @Mock
    private ChatRepository chatRepository;

    @Mock
    private ChatService chatService;

    @Mock
    private UserRepository userRepository;
    @BeforeEach
    void setUp() {
        chatService = new ChatService(chatRepository, userRepository);
    }

    @Test
    void testGetUserChats(){
        // Given
        String email = "example@example.com";

        User user = User.builder()
                .email(email)
                .build();

        Chat chat = Chat.builder()
                .members(List.of(user))
                .build();

        List<Chat> chats = List.of(chat);

        when(chatRepository.findByMembers_Email(email)).thenReturn(chats);

        // When
        List<Chat> result = chatService.getUserChats(email);

        // Then
        assertEquals(1, result.size());
        assertEquals(email, result.get(0).getMembers().get(0).getEmail());
    }

    @Test
    void testCreateChat(){
        // Given
        String email = "example@example.com";

        User user = User.builder()
                .email(email)
                .build();

        Chat chat = Chat.builder()
                .build();

        when(userRepository.findByEmail(email)).thenReturn(Optional.of(user));
        when(chatRepository.save(chat)).thenReturn(chat);

        // When
        Chat result = chatService.createChat(chat, email);

        // Then
        assertEquals(1, result.getMembers().size());
        assertEquals(email, result.getMembers().get(0).getEmail());
    }

    @Test
    void testAddMessageToChat(){
        Message message = Message.builder()
                .sender("example@example.com")
                .text("Hello")
                .build();

        Chat chat = Chat.builder()
                .messages(new ArrayList<>())
                .build();

        when(chatRepository.findById(chat.getId())).thenReturn(Optional.of(chat));
        when(chatRepository.save(chat)).thenReturn(chat);

        // When
        Message result = chatService.addMessageToChat(chat.getId(), message);

        // Then
        assertEquals(1, chat.getMessages().size());
        assertEquals(message.getSender(), chat.getMessages().get(0).getSender());
        assertEquals(message.getText(), chat.getMessages().get(0).getText());
    }

    @Test
    void testGetChatMessage(){
        Message message = Message.builder()
                .sender("example@example.com")
                .text("Hello")
                .build();

        Chat chat = Chat.builder()
                .messages(List.of(message))
                .build();

       when(chatRepository.findById(chat.getId())).thenReturn(Optional.of(chat));

        // When
        List<Message> result = chatService.getChatMessages(chat.getId());

        // Then
        assertEquals(1, result.size());
        assertEquals(message.getSender(), result.get(0).getSender());
        assertEquals(message.getText(), result.get(0).getText());
    }

}
