package com.tyiu.client.connections;

import com.tyiu.client.models.UserDTO;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;

@FeignClient(value = "email", url = "http://localhost:8084", path = "/api/v1/email-service")
public interface EmailClient {

    @PostMapping("/account/password/{email}/code/{code}")
    void sendCodeToChangePassword(@PathVariable String email, @PathVariable String code);

    @PostMapping("/invitation/send/{email}/{id}")
    void sendInvitationToEmail(@PathVariable String email, @PathVariable String id, @RequestBody UserDTO user);

}