package com.tyiu.authorizationservice.repository;

import com.tyiu.authorizationservice.model.entity.Invitation;
import jakarta.transaction.Transactional;
import org.springframework.data.jpa.repository.JpaRepository;

import java.time.LocalDateTime;

public interface InvitationRepository extends JpaRepository<Invitation, String> {
    @Transactional
    void deleteByDateExpiredLessThan(LocalDateTime dateExpired);
    @Transactional
    void deleteByEmail(String email);
}
