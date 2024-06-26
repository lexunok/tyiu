package com.tyiu.authorizationservice.service;

import com.tyiu.amqp.RabbitMQMessageProducer;
import com.tyiu.authorizationservice.model.entity.Invitation;
import com.tyiu.authorizationservice.model.request.ManyInvitationsRequest;
import com.tyiu.authorizationservice.repository.InvitationRepository;
import com.tyiu.authorizationservice.repository.UserRepository;
import com.tyiu.client.models.InvitationLinkRequest;
import com.tyiu.client.models.UserDTO;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;

@Service
@RequiredArgsConstructor
public class InvitationService {

    @Value("${rabbitmq.exchanges.internal}")
    private String internalExchange;
    @Value("${rabbitmq.routing-keys.internal-invitation}")
    private String internalInvitationRoutingKey;

    private final InvitationRepository invitationRepository;
    private final UserRepository userRepository;
    private final RabbitMQMessageProducer rabbitProducer;

    public void sendManyInvitations(ManyInvitationsRequest request, UserDTO user) {
        request.getEmail().forEach(email -> {
            Boolean userIsExists = userRepository.existsByEmail(email);
            if (Boolean.FALSE.equals(userIsExists)) {
                Invitation invitation = Invitation.builder()
                        .roles(request.getRoles())
                        .email(email)
                        .dateExpired(LocalDateTime.now().plusDays(1))
                        .build();
                invitationRepository.deleteByEmail(email);
                Invitation saved = invitationRepository.save(invitation);
                InvitationLinkRequest linkRequest = InvitationLinkRequest.builder()
                        .linkId(saved.getId())
                        .receiver(saved.getEmail())
                        .senderFirstName(user.getFirstName())
                        .senderLastName(user.getLastName())
                        .build();
                rabbitProducer.publish(linkRequest, internalExchange, internalInvitationRoutingKey);
            }
        });
    }
}
