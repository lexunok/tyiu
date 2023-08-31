package com.tyiu.corn.model.entities;


import com.tyiu.corn.model.enums.Role;

import java.util.Date;
import java.util.List;

import lombok.*;
import org.springframework.data.annotation.Id;
import org.springframework.data.mongodb.core.mapping.Document;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Document
public class Temporary {
    @Id
    private String id;
    private String url;
    private Date dateExpired;
    private String email;
    private String newEmail;
    private String oldEmail;
    private int code;
    private List<Role> roles;
}
