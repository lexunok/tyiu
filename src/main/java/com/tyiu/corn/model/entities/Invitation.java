package com.tyiu.corn.model.entities;


import com.tyiu.corn.model.enums.Role;

import java.util.Date;
import java.util.List;

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
    @Column(unique = true)
    private String url;
    private Date dateExpired;
    private String email;
    private String newEmail;
    private String oldEmail;
    private int code;
    private List<Role> roles;
}
