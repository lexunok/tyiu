package com.tyiu.corn.repository;

import com.tyiu.corn.PostgresTest;
import com.tyiu.corn.model.entities.Invitation;
import com.tyiu.corn.model.enums.Role;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.jdbc.AutoConfigureTestDatabase;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;

import java.util.Date;
import java.util.List;
import java.util.NoSuchElementException;

import static org.junit.jupiter.api.Assertions.*;

@DataJpaTest
@AutoConfigureTestDatabase(replace = AutoConfigureTestDatabase.Replace.NONE)
class InvitationRepositoryTest extends PostgresTest {
    @Autowired
    private InvitationRepository underTest;

    @Test
    void deleteInvitationsWhenExpired() {
        // Given
        Date date = new Date();
        long milsec = date.getTime() - 1000;
        date.setTime(milsec);
        Invitation invitation = Invitation.builder()
                .email("Emaisdfl")
                .roles(List.of(Role.ADMIN))
                .url("sdfdsfsdfds")
                .dateExpired(date)
                .build();
        underTest.save(invitation);
        // When
        underTest.deleteExpiredInvitations(new Date());
        // Then
        assertThrows(NoSuchElementException.class, 
                        () -> underTest.findByUrl("sdfdsfsdfds").get(), "No such element");
    }
    @Test
    void deleteInvitationsWhenNotExpired() {
        // Given
        Date date = new Date();
        long milsec = date.getTime() + 259200000;
        date.setTime(milsec);
        Invitation invitation = Invitation.builder()
                .email("Email")
                .roles(List.of(Role.ADMIN))
                .url("sdfdsf")
                .dateExpired(date)
                .build();
        underTest.save(invitation);
        // When
        underTest.deleteExpiredInvitations(new Date());
        // Then
        assertNotNull(underTest.findByUrl("sdfdsf").get());
    }
    @Test
    void deleteInvitationByEmail(){
        // Given
        Date date = new Date();
        long milsec = date.getTime() + 25920004;
        date.setTime(milsec);
        Invitation invitation = Invitation.builder()
                .email("Emaifeasl")
                .roles(List.of(Role.ADMIN))
                .url("sdfdseeff")
                .dateExpired(date)
                .build();
        underTest.save(invitation);
        //When
        underTest.deleteByEmail(invitation.getEmail());
        //Then
        assertThrows(NoSuchElementException.class, 
                        () -> underTest.findByUrl("sdfdseeff").get(), "No such element");
    }
    @Test
    void deleteInvitationByUrl(){
        // Given
        Date date = new Date();
        long milsec = date.getTime() + 25920004;
        date.setTime(milsec);
        Invitation invitation = Invitation.builder()
                .email("Emaifeasl")
                .roles(List.of(Role.ADMIN))
                .url("sdfdseeff")
                .dateExpired(date)
                .build();
        underTest.save(invitation);
        //When
        underTest.deleteByUrl(invitation.getUrl());
        //Then
        assertThrows(NoSuchElementException.class, 
                        () -> underTest.findByUrl("sdfdseeff").get(), "No such element");
    }
}
