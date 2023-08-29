package com.tyiu.corn.model.entities;

import java.util.List;
import lombok.*;
import org.springframework.data.annotation.Id;
import org.springframework.data.relational.core.mapping.Table;

@Setter
@Getter
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Table
public class Company {
    @Id
    private Long id;
    private String name;
    private List<User> staff;
}
