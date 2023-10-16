package com.tyiu.corn.model.entities;
import lombok.*;


import java.time.LocalDateTime;
import org.springframework.data.annotation.Id;
import org.springframework.data.relational.core.mapping.Table;


@Builder
@Table
public class Comment {
    @Id
    private Long id;
    private String text;
    private String senderEmail;
    private LocalDateTime createdAt;
    private Long ideaId;
}