package com.tyiu.authorizationservice.model;

import com.tyiu.client.models.Role;
import jakarta.persistence.*;
import lombok.*;

import java.time.LocalDateTime;
import java.util.List;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Entity
public class Invitation {
    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private String id;
    private LocalDateTime dateExpired;
    private String email;
    @Enumerated(EnumType.STRING)
    private List<Role> roles;
}
