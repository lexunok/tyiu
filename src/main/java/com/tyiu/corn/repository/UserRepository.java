package com.tyiu.corn.repository;

import com.tyiu.corn.model.entities.User;
import com.tyiu.corn.model.enums.Role;
import org.springframework.data.repository.reactive.ReactiveCrudRepository;
import org.springframework.stereotype.Repository;
import reactor.core.publisher.Mono;

import java.util.List;
@Repository
public interface UserRepository extends ReactiveCrudRepository<User,String> {
    Mono<User> findFirstByEmail(String email);
    Mono<Boolean> existsByEmail(String email);
}
