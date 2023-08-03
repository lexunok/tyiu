package com.tyiu.corn.model;

import com.tyiu.corn.model.enums.Feasibility;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.Id;
import jakarta.persistence.ManyToMany;
import lombok.*;

import java.util.List;

@Entity
@Setter
@Getter
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

    @ManyToMany
    private List<Profile> profiles;
    //Поля для фронтенда
    private String idea;
    private String status;
    private String date;
    private double rating;
    private double risk;

}
