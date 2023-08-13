package com.tyiu.corn.repository;

import com.tyiu.corn.model.entities.Temporary;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;

import java.util.Date;
import java.util.Optional;

public interface AccountChangeRepository extends JpaRepository<Temporary, Long>{
    @Modifying
    @Query("DELETE FROM Temporary i WHERE i.dateExpired < ?1")
    void deleteExpiredData(Date date);
    Optional<Temporary> findByUrl(String url);
    Optional<Temporary> findByEmail(String email);
    boolean existsByEmail(String email);
    @Modifying
    @Query("DELETE FROM Temporary i WHERE i.email = ?1")
    void deleteByEmail(String email);
    @Modifying
    @Query("DELETE FROM Temporary i WHERE i.url = ?1")
    void deleteByUrl(String url);
}
