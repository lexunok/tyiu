package com.tyiu.corn.model.entities;

import java.util.List;


import jakarta.persistence.*;
import lombok.*;

@Entity
@Setter
@Getter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class Company {
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    private Long id;
    private String name;
      
    @OneToMany(fetch = FetchType.LAZY)
    private List<User> staff;
}
