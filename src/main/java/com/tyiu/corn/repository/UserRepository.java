package com.tyiu.corn.repository;

import com.tyiu.corn.model.entities.User;
import com.tyiu.corn.model.enums.Role;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;

import java.util.List;
import java.util.Optional;

public interface UserRepository extends JpaRepository<User,Long> {
    Optional<User> findByEmail(String email);
    boolean existsByEmail(String email);
    @Modifying(clearAutomatically = true, flushAutomatically = true)
    @Query("UPDATE User u SET u.email = ?1 WHERE u.id = ?2")
    void setEmail(String email, Long id);
    @Modifying(clearAutomatically = true, flushAutomatically = true)
    @Query("UPDATE User u SET u.password = ?1 WHERE u.id = ?2")
    void setPassword(String password, Long id);
    @Modifying(clearAutomatically = true, flushAutomatically = true)
    @Query("UPDATE User u SET u.email = ?1, u.firstName = ?2, u.lastName = ?3, u.roles = ?4 WHERE u.id = ?5")
    void setUserInfo(String email, String firstName, String lastName, List<Role> roles, Long id);
}
