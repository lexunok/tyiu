package com.tyiu.corn.model.dto;

import com.tyiu.corn.model.entities.Profile;
import com.tyiu.corn.model.entities.Scrum;
import com.tyiu.corn.model.enums.Priority;
import com.tyiu.corn.model.enums.Status;
import jakarta.persistence.ManyToOne;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.hibernate.annotations.CreationTimestamp;

import java.util.Date;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class TaskDTO {
    private Long id;
    private String title;
    private String description;
    private String assignedTo;
    private Priority priority;
    private String deadline;
    private Status status;

    private Date createdAt;

    private Scrum scrum;

    private Profile profile;
}
