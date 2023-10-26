package com.tyiu.corn.model.entities.mappers;

import com.tyiu.corn.model.dto.CompanyDTO;
import com.tyiu.corn.model.dto.UserDTO;
import io.r2dbc.spi.Row;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.function.BiFunction;

public class CompanyMapper implements BiFunction<Row, Object, CompanyDTO> {

    private final Map<Long, CompanyDTO> companyDTOMap = new LinkedHashMap<>();

    @Override
    public CompanyDTO apply(Row row, Object o) {
        UserDTO userDTO = UserDTO.builder()
                .id(row.get("member_id",Long.class))
                .email(row.get("email", String.class))
                .firstName(row.get("first_name", String.class))
                .lastName(row.get("last_name", String.class))
                .build();

        Long companyId = row.get("id", Long.class);

        CompanyDTO existingCompany = companyDTOMap.get(companyId);

        if (existingCompany == null) {
            existingCompany = CompanyDTO.builder()
                    .id(companyId)
                    .name(row.get("name", String.class))
                    .users(new ArrayList<>())
                    .build();
            companyDTOMap.put(companyId, existingCompany);
        }

        if(existingCompany.getUsers().stream().noneMatch(user -> user.getId().equals(userDTO.getId()))) {
            existingCompany.getUsers().add(userDTO);
        }

        return existingCompany;
    }
}
