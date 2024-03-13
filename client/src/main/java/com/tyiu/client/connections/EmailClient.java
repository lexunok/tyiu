package com.tyiu.client.connections;

import com.tyiu.client.models.InvitationDTO;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;

@FeignClient(value = "email", url = "http://localhost:8084", path = "/api/v1/email-service")
public interface EmailClient {

    @GetMapping("/invitation/{invitationId}")
    InvitationDTO findInvitationById(@PathVariable String invitationId);

}