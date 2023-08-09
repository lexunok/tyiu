package com.tyiu.corn.repository;

import com.tyiu.corn.model.entities.Invitation;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;

import java.util.Date;
import java.util.Optional;

public interface InvitationRepository extends JpaRepository<Invitation, Long>{
    @Modifying
    @Query("DELETE FROM Invitation i WHERE i.dateExpired < ?1")
    void deleteExpiredInvitations(Date date);
    Optional<Invitation> findByUrl(String url);
    boolean existsByEmail(String email);
    @Modifying
    @Query("DELETE FROM Invitation i WHERE i.email = ?1")
    void deleteByEmail(String email);
    @Modifying
    @Query("DELETE FROM Invitation i WHERE i.url = ?1")
    void deleteByUrl(String url);
}
