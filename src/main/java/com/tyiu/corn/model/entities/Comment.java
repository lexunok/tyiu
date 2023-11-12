package com.tyiu.corn.model.entities;
import lombok.*;


import java.time.LocalDateTime;
import java.util.List;

import org.springframework.data.annotation.Id;
import org.springframework.data.relational.core.mapping.Table;


@Builder
@Table
@Getter
public class Comment {
    @Id
    private String id;
    private String text;
    private String senderEmail;
    private List<Long> checkedBy;
    private LocalDateTime createdAt;
    private Long ideaId;
}