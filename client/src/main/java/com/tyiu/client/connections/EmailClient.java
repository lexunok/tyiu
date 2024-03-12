package com.tyiu.client.connections;

import com.tyiu.client.models.InvitationDTO;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;

@FeignClient(value = "email", url = "${feign.email.url}")
public interface EmailClient {
    @GetMapping("/api/v1/email-service/invitation/{invitationId}")
    InvitationDTO findInvitationById(@PathVariable String invitationId);
}