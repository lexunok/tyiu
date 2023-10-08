package com.tyiu.corn.util;


import com.tyiu.corn.model.entities.User;
import lombok.RequiredArgsConstructor;
import org.springframework.data.mongodb.core.ReactiveMongoTemplate;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.security.core.userdetails.ReactiveUserDetailsService;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Mono;

@Service
@RequiredArgsConstructor
public class UserService implements ReactiveUserDetailsService {

    private final ReactiveMongoTemplate template;

    @Override
    public Mono<UserDetails> findByUsername(String email) {
        return template.findOne(Query.query(Criteria.where("email").is(email)), User.class)
                .map(CustomUserDetails::new);
    }
}
