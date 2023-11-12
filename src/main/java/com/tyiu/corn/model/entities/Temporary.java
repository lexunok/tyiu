package com.tyiu.corn.model.entities;


import com.tyiu.corn.model.enums.Role;

import java.time.LocalDateTime;
import java.util.List;

import lombok.*;
import org.springframework.data.annotation.Id;
import org.springframework.data.relational.core.mapping.Table;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Table
public class Temporary {
    @Id
    private String id;
    private String url;
    private LocalDateTime dateExpired;
    private String email;
    private String newEmail;
    private String oldEmail;
    private int code;
    private List<Role> roles;
}
