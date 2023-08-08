package com.tyiu.corn.model.responses;

import com.tyiu.corn.model.enums.Role;
import lombok.Builder;
import lombok.Data;

import java.util.List;

@Data
@Builder
public class InvitationResponse {
    private String email;
    private List<Role> roles;
}