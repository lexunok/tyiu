package com.tyiu.ideas.model.entities;

import lombok.*;
import org.springframework.data.annotation.Id;
import org.springframework.data.relational.core.mapping.Table;

import java.util.List;

@Setter
@Getter
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Table
public class TestResult {
    @Id
    private String id;
    private String userId;
    private String testName;
    private List<Integer> score;
    private String testResult;
}
