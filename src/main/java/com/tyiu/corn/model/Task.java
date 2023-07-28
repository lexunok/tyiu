package com.tyiu.corn.model;


import com.tyiu.corn.enumerate.Priority;
import com.tyiu.corn.enumerate.Status;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.hibernate.annotations.CreationTimestamp;

import java.util.Date;


@Entity
@Data
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
    private String scram;
    @CreationTimestamp
    private Date createdAt;
}
