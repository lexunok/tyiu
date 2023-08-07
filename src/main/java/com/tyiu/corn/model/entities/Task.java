package com.tyiu.corn.model.entities;



import com.tyiu.corn.model.enums.Priority;
import com.tyiu.corn.model.enums.Status;
import jakarta.persistence.*;
import lombok.*;
import org.hibernate.annotations.CreationTimestamp;

import java.util.Date;


@Entity
@Setter
@Getter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class Task{
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    private Long id;
    private String title;
    private String description;
    private String assignedTo;
    private Priority priority;
    private String deadline;
    private Status status;

    @CreationTimestamp
    private Date createdAt;

    @ManyToOne
    private Scrum scrum;

    @ManyToOne
    private Profile profile;
}
