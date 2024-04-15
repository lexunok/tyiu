package com.tyiu.authorizationservice.repositories;

import com.tyiu.authorizationservice.models.User;
import jakarta.transaction.Transactional;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;

public interface UserRepository extends JpaRepository<User,String> {
    //TODO: add OPTIONAL
    User findByEmail(String email);
    Boolean existsByEmail(String email);
    @Modifying
    @Query("update User u set u.password = ?1 where u.email = ?2")
    @Transactional
    void setUserPasswordByEmail(String password, String email);
}
