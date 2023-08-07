package com.tyiu.corn.model;

import jakarta.persistence.*;
import lombok.*;

import java.util.List;

@Entity
@Setter
@Getter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class Profile {
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    private Long id;

    @ManyToMany
    private List<Scrum> scrums;

    @ManyToMany
    private List<Idea> ideas;

    @OneToMany
    private List<Task> tasks;

    @OneToOne
    private Comment comment;

}
