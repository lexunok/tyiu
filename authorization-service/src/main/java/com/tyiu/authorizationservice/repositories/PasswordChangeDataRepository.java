package com.tyiu.authorizationservice.repositories;

import com.tyiu.authorizationservice.models.PasswordChangeData;
import jakarta.transaction.Transactional;
import org.springframework.data.jpa.repository.JpaRepository;

import java.time.LocalDateTime;

public interface PasswordChangeDataRepository extends JpaRepository<PasswordChangeData,String> {
    @Transactional
    void deleteByEmail(String email);
    Boolean existsByEmail(String email);
    @Transactional
    void deleteByDateExpiredEquals(LocalDateTime dateExpired);
}
