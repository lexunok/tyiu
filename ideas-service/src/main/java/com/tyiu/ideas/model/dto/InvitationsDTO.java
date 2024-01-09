package com.tyiu.ideas.model.dto;

import java.util.List;

import com.tyiu.ideas.model.enums.Role;

import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class InvitationsDTO {
    List<Role> roles;
    List<String> emails;
}