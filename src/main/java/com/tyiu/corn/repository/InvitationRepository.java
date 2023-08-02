package com.tyiu.corn.repository;

import com.tyiu.corn.model.Invitation;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.util.Date;

public interface InvitationRepository extends JpaRepository<Invitation, Long>{
    @Query("DELETE FROM Invitation i WHERE i.dateExpired > ?1")
    void deleteExpiredInvitations(Date date);

}
