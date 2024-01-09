package com.tyiu.ideas.model.entities;

import java.util.List;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.springframework.data.annotation.Id;
import com.tyiu.ideas.model.enums.Role;
import org.springframework.data.relational.core.mapping.Table;

@Setter
@Getter
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Table(name = "groups")
public class Group {
    @Id
    private String id;
    private String name;
    private List<Role> roles;
}