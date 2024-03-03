package com.tyiu.ideas.service;

import com.tyiu.ideas.model.entities.User;
import com.tyiu.ideas.model.requests.RegisterRequest;
import lombok.RequiredArgsConstructor;
import org.modelmapper.ModelMapper;
import org.springframework.data.r2dbc.core.R2dbcEntityTemplate;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Mono;

@Service
@RequiredArgsConstructor
public class AuthenticationService {

    private final R2dbcEntityTemplate template;
    private final ModelMapper mapper;

    public Mono<Void> register(RegisterRequest request){
        return template.insert(mapper.map(request, User.class)).then();
    }
}