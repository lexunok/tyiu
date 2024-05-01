package com.tyiu.authorizationservice.service;

import com.tyiu.amqp.RabbitMQMessageProducer;
import com.tyiu.authorizationservice.model.entity.Invitation;
import com.tyiu.authorizationservice.model.request.InvitationRequest;
import com.tyiu.authorizationservice.model.entity.User;
import com.tyiu.authorizationservice.repository.InvitationRepository;
import com.tyiu.authorizationservice.repository.UserRepository;
import com.tyiu.client.connections.EmailClient;
import com.tyiu.client.models.UserDTO;
import lombok.RequiredArgsConstructor;
import org.modelmapper.ModelMapper;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;

@Service
@RequiredArgsConstructor
public class InvitationService {

    private final InvitationRepository invitationRepository;
    private final UserRepository userRepository;
    private final EmailClient emailClient;
    private final RabbitMQMessageProducer rabbitProducer;
    private final ModelMapper mapper;

    public void sendInvitationToEmail(InvitationRequest invitationRequest, User user) {
        Boolean userIsExists = userRepository.existsByEmail(invitationRequest.getEmail());
        if (Boolean.TRUE.equals(userIsExists)) {
            //TODO: Ошибка пользователь уже существует
            throw new RuntimeException("Пользователь уже существует");
        }
        Invitation invitation = mapper.map(invitationRequest, Invitation.class);
        invitation.setDateExpired(LocalDateTime.now().plusDays(1));
        invitationRepository.deleteByEmail(invitationRequest.getEmail());
        Invitation saved = invitationRepository.save(invitation);
        rabbitProducer.publish(saved, "internal.exchange", "internal.invitation.routing-key");
        //emailClient.sendInvitationToEmail(saved.getEmail(), saved.getId(), mapper.map(user, UserDTO.class));
    }

    public void deleteInvitation(String id){
        invitationRepository.deleteById(id);
    }
}
