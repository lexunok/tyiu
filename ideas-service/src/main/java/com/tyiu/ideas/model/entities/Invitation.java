package com.tyiu.ideas.model.entities;


import com.tyiu.ideas.model.enums.Role;

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
public class Invitation {
    @Id
    private String id;
    private LocalDateTime dateExpired;
    private String email;
    private List<Role> roles;
}
