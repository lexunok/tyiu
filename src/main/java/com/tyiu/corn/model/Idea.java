package com.tyiu.corn.model;

import com.tyiu.corn.model.enums.Feasibility;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.Id;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Entity
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class Idea {
    @Id
    @GeneratedValue
    private Long id;
    private String title;
    private String type;
    private String problem;
    private String solution;
    private String result;
    private String customer;
    private String description;
    private Long budget;
    private Feasibility feasibility;
    private String suitability;
    private String status;
}
