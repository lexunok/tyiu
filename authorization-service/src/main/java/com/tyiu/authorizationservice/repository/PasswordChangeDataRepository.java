package com.tyiu.authorizationservice.repository;

import com.tyiu.authorizationservice.model.entity.PasswordChangeData;
import jakarta.transaction.Transactional;
import org.springframework.data.jpa.repository.JpaRepository;

import java.time.LocalDateTime;

public interface PasswordChangeDataRepository extends JpaRepository<PasswordChangeData,String> {

    @Transactional
    void deleteByEmail(String email);

    @Transactional
    void deleteByDateExpiredLessThan(LocalDateTime dateExpired);
}
