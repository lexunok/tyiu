package com.tyiu.client.connections;

import com.tyiu.client.models.UserDTO;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;

@FeignClient(value = "ideas",  url = "${service.ideas.url}", path = "/api/v1/ideas-service/profile")
public interface ProfileClient {

    @PostMapping()
    void checkUser(@RequestBody UserDTO userDTO);
}
