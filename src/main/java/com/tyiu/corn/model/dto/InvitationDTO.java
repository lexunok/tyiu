package com.tyiu.corn.model.dto;

import java.util.List;

import org.springframework.web.multipart.MultipartFile;

import com.tyiu.corn.model.enums.Role;

import lombok.Data;

@Data
public class InvitationDTO {
    List<String> emails;
    List<Role> roles;
    MultipartFile file;
}