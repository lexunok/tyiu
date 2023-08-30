package com.tyiu.corn.repository;

import com.tyiu.corn.model.entities.User;
import com.tyiu.corn.model.enums.Role;
import org.springframework.data.r2dbc.repository.Modifying;
import org.springframework.data.r2dbc.repository.Query;
import org.springframework.data.repository.reactive.ReactiveCrudRepository;
import reactor.core.publisher.Mono;

import java.util.List;
import java.util.Optional;

public interface UserRepository extends ReactiveCrudRepository<User,Long> {
    Mono<User> findByEmail(String email);
    Mono<Boolean> existsByEmail(String email);
    @Modifying
    @Query("UPDATE User u SET u.email = ?1 WHERE u.id = ?2")
    void setEmail(String email, Long id);
    @Modifying
    @Query("UPDATE User u SET u.password = ?1 WHERE u.id = ?2")
    void setPassword(String password, Long id);
    @Modifying
    @Query("UPDATE User u SET u.email = ?1, u.firstName = ?2, u.lastName = ?3, u.roles = ?4 WHERE u.id = ?5")
    void setUserInfo(String email, String firstName, String lastName, List<Role> roles, Long id);
}
