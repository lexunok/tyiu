package com.tyiu.corn.model;


import com.tyiu.corn.model.enums.Role;

import java.util.Set;
import java.util.Date;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.Id;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Entity
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class Invitation {
    @Id
    @GeneratedValue
    private Long id;
    private Set<Role> roles;
    @Column(unique = true)
    private String url;
    private Date dateExpired;
}
