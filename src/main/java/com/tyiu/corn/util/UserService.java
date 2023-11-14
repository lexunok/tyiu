package com.tyiu.corn.util;


import com.tyiu.corn.model.entities.User;
import lombok.RequiredArgsConstructor;
import org.springframework.data.r2dbc.core.R2dbcEntityTemplate;
import org.springframework.security.core.userdetails.ReactiveUserDetailsService;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Mono;

import static org.springframework.data.relational.core.query.Criteria.where;
import static org.springframework.data.relational.core.query.Query.query;

@Service
@RequiredArgsConstructor
public class UserService implements ReactiveUserDetailsService {

    private final R2dbcEntityTemplate template;

    @Override
    public Mono<UserDetails> findByUsername(String id) {
        return template.selectOne(query(where("id").is(id)), User.class)
                .cast(UserDetails.class);
    }
}
