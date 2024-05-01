package com.tyiu.authorizationservice.service;

import com.tyiu.amqp.RabbitMQMessageProducer;
import com.tyiu.authorizationservice.config.exception.AccessException;
import com.tyiu.authorizationservice.model.entity.Invitation;
import com.tyiu.authorizationservice.model.request.InvitationRequest;
import com.tyiu.authorizationservice.model.entity.User;
import com.tyiu.authorizationservice.repository.InvitationRepository;
import com.tyiu.authorizationservice.repository.UserRepository;
import com.tyiu.client.models.InvitationLinkRequest;
import lombok.RequiredArgsConstructor;
import org.modelmapper.ModelMapper;
import org.springframework.cache.annotation.CacheConfig;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;

@Service
@RequiredArgsConstructor
@CacheConfig(cacheNames = "invitation")
public class InvitationService {

    @Value("${rabbitmq.exchanges.internal}")
    private String internalExchange;
    @Value("${rabbitmq.routing-keys.internal-invitation}")
    private String internalInvitationRoutingKey;

    private final InvitationRepository invitationRepository;
    private final UserRepository userRepository;
    private final RabbitMQMessageProducer rabbitProducer;
    private final ModelMapper mapper;

    public void sendInvitationToEmail(InvitationRequest invitationRequest, User user) {
        Boolean userIsExists = userRepository.existsByEmail(invitationRequest.getEmail());
        if (Boolean.TRUE.equals(userIsExists)) {
            //TODO: Ошибка пользователь уже существует
            throw new AccessException("Пользователь уже существует");
        }
        Invitation invitation = mapper.map(invitationRequest, Invitation.class);
        invitation.setDateExpired(LocalDateTime.now().plusDays(1));
        invitationRepository.deleteByEmail(invitationRequest.getEmail());
        Invitation saved = invitationRepository.save(invitation);
        InvitationLinkRequest linkRequest = InvitationLinkRequest.builder()
                .linkId(saved.getId())
                .receiver(saved.getEmail())
                .senderFirstName(user.getFirstName())
                .senderLastName(user.getLastName())
                .build();
        rabbitProducer.publish(linkRequest, internalExchange, internalInvitationRoutingKey);
    }

    public void deleteInvitation(String id){
        invitationRepository.deleteById(id);
    }

    //    public void sendInvitations(InvitationsDTO invitations, User user) {
//        Flux.fromIterable(invitations.getEmails())
//                .flatMap(email -> template.exists(query(where("email").is(email)), User.class)
//                        .flatMap(userExists -> {
//                            if (Boolean.FALSE.equals(userExists)) {
//                                return template.exists(query(where("email").is(email)), Invitation.class)
//                                        .flatMap(invitationExists -> {
//                                            Invitation invitation = Invitation.builder()
//                                                    .roles(invitations.getRoles())
//                                                    .email(email)
//                                                    .dateExpired(LocalDateTime.now().plusDays(1))
//                                                    .build();
//
//                                            if (Boolean.TRUE.equals(invitationExists))
//                                                return template.delete(query(where("email").is(email)), Invitation.class)
//                                                        .then(template.insert(invitation))
//                                                        .flatMap(i ->
//                                                                sendInvitation(
//                                                                        i.getEmail(),
//                                                                        String.format("register/%s", i.getId()),
//                                                                        user)
//                                                        )
//                                                        .onErrorResume(e -> Mono.fromRunnable(() -> {
//                                                            log.error("Error processing invitation for email {}: {}",
//                                                                    email, e.getMessage());
//                                                        }));
//                                            return template.insert(invitation)
//                                                    .flatMap(i ->
//                                                            sendInvitation(
//                                                                    i.getEmail(),
//                                                                    String.format("register/%s", i.getId()),
//                                                                    user)
//                                                    )
//                                                    .onErrorResume(e -> Mono.fromRunnable(() -> {
//                                                        log.error("Error processing invitation for email {}: {}",
//                                                                email, e.getMessage());
//
//                                                    }));
//                                        });
//                            }
//                            return Mono.empty();
//                        })
//                )
//                .publishOn(Schedulers.boundedElastic())
//                .subscribe();
//    }
}
