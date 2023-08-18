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
public class Chat {
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    private Long id;
    @OneToMany(cascade = CascadeType.ALL)
    private List<Message> messages;
    @ManyToMany(cascade = CascadeType.ALL, fetch = FetchType.LAZY)
    private List<User> members;
}
