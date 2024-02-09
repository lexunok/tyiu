package com.tyiu.emailservice.model.dto;

import com.tyiu.ideas.model.enums.Role;
import lombok.Builder;
import lombok.Data;

import java.util.List;

@Data
@Builder
public class InvitationsDTO {
    List<Role> roles;
    List<String> emails;
}