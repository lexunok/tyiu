package com.tyiu.authorizationservice.repository;

import com.tyiu.authorizationservice.model.entity.User;
import jakarta.transaction.Transactional;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;

import java.util.List;
import java.util.Optional;

public interface UserRepository extends JpaRepository<User,String> {

    Optional<User> findByEmail(String email);

    Boolean existsByEmail(String email);

    @Modifying
    @Query("update User u set u.password = ?1 where u.email = ?2")
    @Transactional
    void setUserPasswordByEmail(String password, String email);

    @Modifying
    @Query("update User u set u.isDeleted = ?1 where u.id = ?2")
    @Transactional
    void setUserIsDeletedById(Boolean isDeleted, String id);

    @Modifying
    @Query("update User u set u.email = ?1 where u.email = ?2")
    @Transactional
    void setUserEmailByEmail(String newEmail, String oldEmail);

    @Modifying
    @Query("update User u set u.firstName = ?1, u.lastName = ?2, u.telephone = ?3, u.studyGroup = ?4 where u.id = ?5")
    @Transactional
    void updateProfileById(String firstName, String lastName, String telephone, String studyGroup, String id);

    List<User> findByIsDeletedFalse();
}
