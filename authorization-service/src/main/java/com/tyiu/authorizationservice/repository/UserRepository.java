package com.tyiu.authorizationservice.repository;

import com.tyiu.authorizationservice.model.entities.User;
import org.springframework.data.jpa.repository.JpaRepository;

public interface UserRepository extends JpaRepository<User, String> {

    User findUserByEmail(String email);
}