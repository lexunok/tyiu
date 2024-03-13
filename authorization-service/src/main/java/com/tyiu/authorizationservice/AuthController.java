package com.tyiu.authorizationservice;

import com.tyiu.client.connections.IdeasClient;
import com.tyiu.client.models.UserDTO;
import lombok.RequiredArgsConstructor;
import org.modelmapper.ModelMapper;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/v1/authorization-service")
@RequiredArgsConstructor
public class AuthController {
    private final UserRepository repository;
    private final IdeasClient ideasClient;
    private final ModelMapper mapper = new ModelMapper();
    private final PasswordEncoder encoder;

    @PostMapping("/register")
    public void register(@RequestBody UserDTO userDTO){
        User user = mapper.map(userDTO,User.class);
        user.setPassword(encoder.encode(user.getPassword()));
        repository.save(user);
        ideasClient.registerUserToIdeas(mapper.map(user, UserDTO.class));
    }
}
