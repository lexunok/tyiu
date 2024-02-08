package com.tyiu.emailservice.model.entity;


import com.tyiu.ideas.model.enums.Role;
import lombok.*;
import org.springframework.data.annotation.Id;
import org.springframework.data.relational.core.mapping.Table;

import java.time.LocalDateTime;
import java.util.List;

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
