package com.tyiu.authorizationservice.model.request;

import com.tyiu.client.models.Role;
import lombok.Builder;
import lombok.Data;

import java.time.LocalDateTime;
import java.util.List;

@Data
@Builder
public class ManyInvitationsRequest {
    private LocalDateTime dateExpired;
    private List<String> email;
    private List<Role> roles;
}
