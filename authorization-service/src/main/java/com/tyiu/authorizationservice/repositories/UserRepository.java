package com.tyiu.authorizationservice.repositories;

import com.tyiu.authorizationservice.models.User;
import org.springframework.data.jpa.repository.JpaRepository;

public interface UserRepository extends JpaRepository<User,Long> {
    //TODO: add OPTIONAL
    User findByEmail(String email);
    Boolean existsByEmail(String email);
}
