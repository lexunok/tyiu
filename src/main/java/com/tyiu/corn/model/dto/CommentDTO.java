package com.tyiu.corn.model.dto;

import java.time.LocalDateTime;
import java.util.List;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class CommentDTO {
    private String id;
    private String text;
    private String senderEmail;
    private List<Long> checkedBy;
    private LocalDateTime createdAt;
    private Long ideaId;
}
