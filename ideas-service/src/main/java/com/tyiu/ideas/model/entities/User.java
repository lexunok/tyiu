package com.tyiu.ideas.model.entities;

import lombok.*;
import org.springframework.data.annotation.Id;
import org.springframework.data.relational.core.mapping.Table;

import java.time.LocalDateTime;


@Setter
@Getter
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Table(name = "users")
public class User {
    @Id
    private String id;
    private String email;
    private String lastName;
    private String firstName;
    private Boolean isDeleted;
    private String studyGroup;
    private String telephone;
    private LocalDateTime createdAt;

    public String getEmail() {
        return email;
    }
    public String getId() { return id; }
    public String getFirstName() { return firstName; }
    public String getLastName() { return lastName; }
}
