package com.tyiu.corn.service;

import com.tyiu.corn.model.Invitation;
import com.tyiu.corn.repository.InvitationRepository;

import java.util.Date;
import java.util.UUID;
import lombok.RequiredArgsConstructor;

import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;


@Service
@RequiredArgsConstructor
@EnableScheduling
public class InvitationService {
    private final InvitationRepository invitationRepository;
    
    public void saveInvitation(Invitation invitation){
        Date date = new Date();
        long milliseconds = date.getTime() + 259200000;
        date.setTime(milliseconds);
        invitation.setDateExpired(date);
        invitation.setUrl(UUID.randomUUID().toString());
        
        invitationRepository.save(invitation);
    }

    @Scheduled(cron = "@daily")
    public void deleteInvitation(Invitation invitation){
        Date date = new Date();
        invitationRepository.deleteExpiredInvitations(date);
    }
}
