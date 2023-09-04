package com.tyiu.corn.model.entities;



import com.tyiu.corn.model.enums.Priority;
import com.tyiu.corn.model.enums.Status;
import lombok.*;
import org.springframework.data.annotation.Id;
import org.springframework.data.mongodb.core.mapping.Document;

import java.util.Date;


@Setter
@Getter
@AllArgsConstructor
@NoArgsConstructor
@Builder
@Document
public class Task{
    @Id
    private String id;
    private String title;
    private String description;
    private String assignedTo;
    private Priority priority;
    private String deadline;
    private Status status;
    private Date createdAt;
    private Scrum scrum;
    private User profile;
}
