package com.tyiu.corn.model;

import jakarta.persistence.*;
import lombok.*;

@Entity
@Setter
@Getter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class Comment {
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    private Long id;

    private String comment;
    private String sender;
    private boolean status;

    @OneToOne
    private Profile profile;

    @ManyToOne
    private Idea idea;

}
