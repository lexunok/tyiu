package com.tyiu.authorizationservice.model;

import com.tyiu.client.models.Role;
import lombok.Builder;
import lombok.Data;

import java.time.LocalDateTime;
import java.util.List;

@Data
@Builder
public class InvitationDTO {
    private LocalDateTime dateExpired;
    private String email;
    private List<Role> roles;
}
