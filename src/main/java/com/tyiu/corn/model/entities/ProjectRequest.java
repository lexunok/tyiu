package com.tyiu.corn.model.entities;

import lombok.*;
import org.springframework.data.annotation.Id;
import org.springframework.data.relational.core.mapping.Table;

import java.time.LocalDate;

@Setter
@Getter
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Table
public class ProjectRequest {
    @Id
    private Long id;
    private Long projectId;
    private Long userId;
    private String email;
    private String firstName;
    private String lastName;
    private LocalDate createdAt;
}
