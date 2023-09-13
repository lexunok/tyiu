package com.tyiu.corn.model.dto;

import com.tyiu.corn.model.entities.Scrum;
import com.tyiu.corn.model.entities.User;
import com.tyiu.corn.model.enums.Priority;
import com.tyiu.corn.model.enums.Status;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import java.util.Date;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class TaskDTO {
    private String id;
    private String title;
    private String description;
    private String assignedTo;
    private Priority priority;
    private String deadline;
    private Status status;

    private Date createdAt;

    private Scrum scrum;

    private UserDTO profile;
}
