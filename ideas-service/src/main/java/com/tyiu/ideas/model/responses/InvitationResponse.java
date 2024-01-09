package com.tyiu.ideas.model.responses;

import com.tyiu.ideas.model.enums.Role;
import lombok.Builder;
import lombok.Data;

import java.util.List;

@Data
@Builder
public class InvitationResponse {
    private String email;
    private List<Role> roles;
}