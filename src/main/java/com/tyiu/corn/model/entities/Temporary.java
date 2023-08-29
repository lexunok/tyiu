package com.tyiu.corn.model.entities;


import com.tyiu.corn.model.enums.Role;

import java.util.Date;
import java.util.List;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.springframework.data.annotation.Id;
import org.springframework.data.relational.core.mapping.Column;
import org.springframework.data.relational.core.mapping.Table;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Table
public class Temporary {
    @Id
    private Long id;
    private String url;
    private Date dateExpired;
    private String email;
    private String newEmail;
    private String oldEmail;
    private int code;
    private List<Role> roles;
}
