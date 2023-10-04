package com.tyiu.corn.repository;

import com.tyiu.corn.model.dto.ProfileDTO;
import com.tyiu.corn.model.entities.Profile;
import org.springframework.data.repository.reactive.ReactiveCrudRepository;
import org.springframework.stereotype.Repository;
import reactor.core.publisher.Mono;

@Repository
public interface ProfileRepository extends ReactiveCrudRepository<Profile, String> {
}
