package com.tyiu.client.connections;

import com.tyiu.client.models.UserDTO;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestBody;

@FeignClient(value = "ideas", url = "http://localhost:8082", path = "/api/v1/ideas-service")
public interface IdeasClient {

    @GetMapping("/register")
    void registerUserToIdeas(@RequestBody UserDTO user);

}