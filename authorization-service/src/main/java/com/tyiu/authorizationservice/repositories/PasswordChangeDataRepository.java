package com.tyiu.authorizationservice.repositories;

import com.tyiu.authorizationservice.models.PasswordChangeData;
import org.springframework.data.jpa.repository.JpaRepository;

import java.time.LocalDateTime;

public interface PasswordChangeDataRepository extends JpaRepository<PasswordChangeData,Long> {
    void deleteByEmail(String email);
    Boolean existsByEmail(String email);
    void deleteByDateExpiredEquals(LocalDateTime dateExpired);
}
