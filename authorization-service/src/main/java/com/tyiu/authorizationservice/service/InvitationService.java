package com.tyiu.authorizationservice.service;

import com.tyiu.authorizationservice.model.Invitation;
import com.tyiu.authorizationservice.model.InvitationDTO;
import com.tyiu.authorizationservice.model.User;
import com.tyiu.authorizationservice.repository.InvitationRepository;
import com.tyiu.authorizationservice.repository.UserRepository;
import com.tyiu.client.connections.EmailClient;
import com.tyiu.client.models.UserDTO;
import lombok.RequiredArgsConstructor;
import org.modelmapper.ModelMapper;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;

@Service
@RequiredArgsConstructor
public class InvitationService {

    private final InvitationRepository invitationRepository;
    private final UserRepository userRepository;
    private final EmailClient emailClient;
    private final ModelMapper mapper;

    @Scheduled(fixedRate = 28800000)
    void deleteExpiredInvitations() {
        invitationRepository.deleteByDateExpiredLessThan(LocalDateTime.now());
    }
    public void sendInvitationToEmail(InvitationDTO invitationDTO, User user) {
        Boolean userIsExists = userRepository.existsByEmail(user.getEmail());
        if (Boolean.TRUE.equals(userIsExists)) {
            //TODO: Ошибка пользователь уже существует
        }
        Invitation invitation = mapper.map(invitationDTO, Invitation.class);
        invitation.setDateExpired(LocalDateTime.now().plusDays(1));
        Boolean invitationIsExists = invitationRepository.existsByEmail(invitationDTO.getEmail());
        if (Boolean.TRUE.equals(invitationIsExists)) {
            invitationRepository.deleteByEmail(invitationDTO.getEmail());
        }
        Invitation saved = invitationRepository.save(invitation);
        //TODO: вместо отправки пользователя нужно сделать в client отправку токена
        emailClient.sendInvitationToEmail(saved.getEmail(), saved.getId(), mapper.map(user, UserDTO.class));
    }

    public void deleteInvitation(String id){
        invitationRepository.deleteById(id);
    }
}
