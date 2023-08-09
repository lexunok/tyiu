package com.tyiu.corn.service;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.List;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.mail.SimpleMailMessage;
import org.springframework.mail.javamail.JavaMailSender;

import com.tyiu.corn.exception.UserExistsException;
import com.tyiu.corn.model.dto.InvitationDTO;
import com.tyiu.corn.model.entities.Invitation;
import com.tyiu.corn.model.entities.User;
import com.tyiu.corn.model.enums.Role;
import com.tyiu.corn.repository.InvitationRepository;
import com.tyiu.corn.repository.UserRepository;

@ExtendWith(MockitoExtension.class)
public class InvitationServiceTest {
    private InvitationService underTest;

    boolean containsEmail(final List<String> emails, final String email){
        return emails.stream().filter(listEmail -> listEmail == email).findFirst().isPresent();
    }

    @Mock
    private JavaMailSender emailSender;
    @Mock
    private InvitationRepository invitationRepository;
    @Mock
    private UserRepository userRepository;

    @BeforeEach
    void setUp(){
        User user = User.builder()
        .email("12345@gmail.com")
        .firstName("Timur")
        .lastName("Minyazeff")
        .password("12345")
        .roles(List.of(Role.ADMIN))
        .build();

        userRepository.save(user);
        underTest = new InvitationService(emailSender, invitationRepository ,userRepository);
    }
    @Test
    void saveInvitation(){
        //Given
        Invitation request = new Invitation();
        request.setRoles(List.of(Role.ADMIN));
        request.setEmail("dgwrh@gmail.com");
        //When
        underTest.sendInvitation(request);
        //Then
        ArgumentCaptor<Invitation> captor = ArgumentCaptor.forClass(Invitation.class);
        ArgumentCaptor<SimpleMailMessage> emailCaptor = ArgumentCaptor.forClass(SimpleMailMessage.class);
        verify(invitationRepository).save(captor.capture());
        verify(emailSender).send(emailCaptor.capture());
        Invitation invitation = captor.getValue();
        assertEquals(invitation.getRoles(), request.getRoles());
        assertEquals(invitation.getEmail(), request.getEmail());
        assertDoesNotThrow(() -> underTest.sendInvitation(request));
    }

    @Test
    void saveNewInvitations(){
        //Before
        //Given
        List<String> emails = List.of(
            "wgweg@gfefemail.com",
            "awgweg@gfefemail.com",
            "bawgweg@gfefemail.com",
            "cbadwfggweg@gfefemail.com",
            "cbadwwdgweg@gfefemail.com",
            "cbadwefgweg@gfefemail.com",
            "cbadwf3gweg@gfefemail.com",
            "cbadwgwerfrg@gfefemail.com",
            "cbadwgweg@gfefemail.com"
        );
        InvitationDTO request = InvitationDTO.builder()
        .roles(List.of(Role.ADMIN))
        .emails(emails)
        .build();
        underTest.sendInvitations(request);
        //Then
        ArgumentCaptor<Invitation> captor = ArgumentCaptor.forClass(Invitation.class);
        ArgumentCaptor<SimpleMailMessage> emailCaptor = ArgumentCaptor.forClass(SimpleMailMessage.class);
        captor.getAllValues().stream().forEach(invitation -> {
            verify(invitationRepository).save(invitation);
            assertEquals(invitation.getRoles(), request.getRoles());
            assertTrue(containsEmail(emails, invitation.getEmail()));
        });
        emailCaptor.getAllValues().stream().forEach(mailMessage -> {
            verify(emailSender).send(mailMessage);
            assertTrue(containsEmail(emails, mailMessage.getTo()[0]));
        });
        assertDoesNotThrow(() -> underTest.sendInvitations(request));
    }
    @Test
    void saveInvitationIfUserExists(){
        //Given
        Invitation request = new Invitation();
        request.setRoles(List.of(Role.ADMIN));
        request.setEmail("12345@gmail.com");
        //When
        when(userRepository.existsByEmail(request.getEmail())).thenReturn(true);
        //Then
        verify(invitationRepository, never()).save(any());
        assertThrows(UserExistsException.class, () -> underTest.sendInvitation(request));
    }
    @Test
    void saveInvitationIfInvitationExists(){
        //Before 
        Invitation lastInvitation = new Invitation();
        lastInvitation.setRoles(List.of(Role.ADMIN));
        lastInvitation.setEmail("123456@gmail.com");
        lastInvitation.setUrl("12345");
        lastInvitation.setOldEmail("1234567@gmail.com");
        invitationRepository.save(lastInvitation);

        //Given
        Invitation request = new Invitation();
        request.setRoles(List.of(Role.ADMIN));
        request.setEmail("123456@gmail.com");
        //When
        underTest.sendInvitation(request);
        //Then
        ArgumentCaptor<Invitation> captor = ArgumentCaptor.forClass(Invitation.class);
        captor.getAllValues().stream().filter(invitation -> invitation.getUrl()!="12345").forEach(invitation -> {
            verify(invitationRepository).save(invitation);
            assertNotEquals("12345", invitation.getUrl());
        });
    }
    @Test
    void saveNewInvitationsIfOneOrMoreExist(){
        //Before
        //Given
        List<String> emails = List.of(
            "wgweg@gfefemail.com",
            "awgweg@gfefemail.com",
            "bawgweg@gfefemail.com",
            "cbadwfggweg@gfefemail.com",
            "cbadwwdgweg@gfefemail.com",
            "cbadwefgweg@gfefemail.com",
            "cbadwf3gweg@gfefemail.com",
            "cbadwgwerfrg@gfefemail.com",
            "12345@gmail.com"
        );
        InvitationDTO request = InvitationDTO.builder()
        .roles(List.of(Role.ADMIN))
        .emails(emails)
        .build();
        //When
        underTest.sendInvitations(request);
        //Then
        assertTrue(!invitationRepository.existsByEmail("12345@gmail.com"));
        assertDoesNotThrow(() -> underTest.sendInvitations(request));
    }
}
