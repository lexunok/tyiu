package com.tyiu.authorizationservice.service;

import com.tyiu.authorizationservice.config.exception.AccessException;
import com.tyiu.authorizationservice.model.entity.Invitation;
import com.tyiu.authorizationservice.model.request.InvitationRequest;
import com.tyiu.authorizationservice.model.entity.User;
import com.tyiu.authorizationservice.repository.InvitationRepository;
import com.tyiu.authorizationservice.repository.UserRepository;
import com.tyiu.client.connections.EmailClient;
import com.tyiu.client.models.UserDTO;
import lombok.RequiredArgsConstructor;
import org.modelmapper.ModelMapper;
import org.springframework.cache.annotation.CacheConfig;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;

@Service
@RequiredArgsConstructor
@CacheConfig(cacheNames = "invitation")
public class InvitationService {

    private final InvitationRepository invitationRepository;
    private final UserRepository userRepository;
    private final EmailClient emailClient;
    private final ModelMapper mapper;

    @Cacheable
    public void sendInvitationToEmail(InvitationRequest invitationRequest, User user) {
        Boolean userIsExists = userRepository.existsByEmail(user.getEmail());
        if (Boolean.TRUE.equals(userIsExists)) {
            //TODO: Ошибка пользователь уже существует
            throw new AccessException("Пользователь уже существует");
        }
        Invitation invitation = mapper.map(invitationRequest, Invitation.class);
        invitation.setDateExpired(LocalDateTime.now().plusDays(1));
        invitationRepository.deleteByEmail(invitationRequest.getEmail());
        Invitation saved = invitationRepository.save(invitation);
        emailClient.sendInvitationToEmail(saved.getEmail(), saved.getId(), mapper.map(user, UserDTO.class));
    }

    @CacheEvict(allEntries = true)
    public void deleteInvitation(String id){
        invitationRepository.deleteById(id);
    }
}
