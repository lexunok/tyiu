package com.tyiu.corn.model.entities.relations;

import lombok.AllArgsConstructor;
import lombok.NoArgsConstructor;
import org.springframework.data.relational.core.mapping.Table;

@AllArgsConstructor
@NoArgsConstructor
@Table("company_user")
public class Company2User {

    private Long userId;
    private Long companyId;

}
