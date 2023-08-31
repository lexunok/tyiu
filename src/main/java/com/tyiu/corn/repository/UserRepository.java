package com.tyiu.corn.repository;

import com.tyiu.corn.model.entities.User;
import com.tyiu.corn.model.enums.Role;
import org.springframework.data.mongodb.repository.Query;
import org.springframework.data.repository.reactive.ReactiveCrudRepository;
import org.springframework.stereotype.Repository;
import reactor.core.publisher.Mono;

import java.util.List;
import java.util.Optional;
@Repository
public interface UserRepository extends ReactiveCrudRepository<User,String> {
    Mono<User> findFirstByEmail(String email);
    Mono<Boolean> existsByEmail(String email);
    @Query("UPDATE User u SET u.email = ?1 WHERE u.id = ?2")
    void setEmail(String email, Long id);
    @Query("UPDATE User u SET u.password = ?1 WHERE u.id = ?2")
    void setPassword(String password, Long id);
    @Query("UPDATE User u SET u.email = ?1, u.firstName = ?2, u.lastName = ?3, u.roles = ?4 WHERE u.id = ?5")
    void setUserInfo(String email, String firstName, String lastName, List<Role> roles, Long id);
}
