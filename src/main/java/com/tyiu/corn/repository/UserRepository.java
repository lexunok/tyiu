package com.tyiu.corn.repository;

import com.tyiu.corn.model.entities.User;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;

import java.util.Optional;

public interface UserRepository extends JpaRepository<User,Long> {
    Optional<User> findByEmail(String email);
    boolean existsByEmail(String email);
    @Modifying
    @Query("UPDATE User u SET u.email = ?1 WHERE u.id = ?2")
    void setEmail(String email, Long id);

}
