package com.tyiu.corn.repository;

import com.tyiu.corn.model.entities.Temporary;
import org.springframework.data.mongodb.repository.Query;
import org.springframework.data.repository.reactive.ReactiveCrudRepository;
import org.springframework.stereotype.Repository;
import reactor.core.publisher.Mono;

import java.util.Date;
@Repository
public interface AccountChangeRepository extends ReactiveCrudRepository<Temporary, String> {
    @Query("DELETE FROM Temporary i WHERE i.dateExpired < ?1")
    void deleteExpiredData(Date date);
    Mono<Temporary> findByUrl(String url);
    Mono<Boolean> existsByEmail(String email);
    Mono<Boolean> existsByOldEmail(String oldEmail);
    void deleteByEmail(String email);
    void deleteByOldEmail(String email);
    void deleteByUrl(String url);
}
