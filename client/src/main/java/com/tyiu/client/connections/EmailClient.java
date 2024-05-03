package com.tyiu.client.connections;

import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;

@FeignClient(value = "email",  url = "${service.email.url}", path = "/api/v1/email-service")
public interface EmailClient {

    @PostMapping("/account/password/{email}/code/{code}")
    void sendCodeToChangePassword(@PathVariable String email, @PathVariable String code);


    @PostMapping("/account/email/{email}/code/{code}")
    void sendCodeToChangeEmail(@PathVariable String email, @PathVariable String code);

}