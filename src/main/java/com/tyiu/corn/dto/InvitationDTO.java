package com.tyiu.corn.dto;

import java.util.List;

import com.tyiu.corn.model.enums.Role;

import lombok.Data;

@Data
public class InvitationDTO {
    List<String> emails;
    List<Role> roles;
}