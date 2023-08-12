package com.tyiu.corn.model.entities;

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

    @ManyToOne
    private Idea idea;

}
