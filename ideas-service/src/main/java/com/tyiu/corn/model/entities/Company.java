package com.tyiu.corn.model.entities;

import lombok.*;
import org.springframework.data.annotation.Id;
import org.springframework.data.relational.core.mapping.Table;

@Setter
@Getter
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Table(name = "company")
public class Company {
    @Id
    private String id;
    private String name;
    private String ownerId;
}
