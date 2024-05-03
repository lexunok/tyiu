package com.tyiu.authorizationservice.repository;

import com.tyiu.authorizationservice.model.entity.EmailChangeData;
import jakarta.transaction.Transactional;
import org.springframework.data.jpa.repository.JpaRepository;

import java.time.LocalDateTime;
import java.util.Optional;

public interface EmailChangeDataRepository extends JpaRepository<EmailChangeData, String> {
    @Transactional
    void deleteByOldEmail(String oldEmail);
    Boolean existsByNewEmail(String newEmail);
    Optional<EmailChangeData> findByOldEmail(String oldEmail);
    @Transactional
    void deleteByDateExpiredLessThan(LocalDateTime dateExpired);
}
