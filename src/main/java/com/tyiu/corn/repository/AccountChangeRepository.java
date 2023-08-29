package com.tyiu.corn.repository;

import com.tyiu.corn.model.entities.Temporary;
import org.springframework.data.r2dbc.repository.Modifying;
import org.springframework.data.r2dbc.repository.Query;
import org.springframework.data.repository.reactive.ReactiveCrudRepository;
import reactor.core.publisher.Mono;

import java.util.Date;
import java.util.Optional;

public interface AccountChangeRepository extends ReactiveCrudRepository<Temporary, Long> {
    @Modifying
    @Query("DELETE FROM Temporary i WHERE i.dateExpired < ?1")
    void deleteExpiredData(Date date);
    Mono<Temporary> findByUrl(String url);
    boolean existsByEmail(String email);
    boolean existsByOldEmail(String oldEmail);
    void deleteByEmail(String email);
    void deleteByOldEmail(String email);
    void deleteByUrl(String url);
}
