package com.tyiu.corn.repository;

import com.tyiu.corn.model.entities.Temporary;
import org.springframework.data.mongodb.repository.Query;
import org.springframework.data.repository.reactive.ReactiveCrudRepository;
import org.springframework.stereotype.Repository;
import reactor.core.publisher.Mono;

import java.util.Date;
import java.util.Optional;
@Repository
public interface AccountChangeRepository extends ReactiveCrudRepository<Temporary, String> {
    @Query("DELETE FROM Temporary i WHERE i.dateExpired < ?1")
    void deleteExpiredData(Date date);
    Mono<Temporary> findByUrl(String url);
    boolean existsByEmail(String email);
    boolean existsByOldEmail(String oldEmail);
    void deleteByEmail(String email);
    void deleteByOldEmail(String email);
    void deleteByUrl(String url);
}
