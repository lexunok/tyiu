package com.tyiu.ideas.model.entities.mappers;

import com.tyiu.ideas.model.dto.CompanyDTO;
import io.r2dbc.spi.Row;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.function.BiFunction;
import com.tyiu.client.models.UserDTO;
@Component
public class CompanyMapper implements BiFunction<Row, Object, CompanyDTO> {

    private final Map<String, CompanyDTO> companyDTOMap = new LinkedHashMap<>();


    @Override
    public CompanyDTO apply(Row row, Object o) {
        String companyId = row.get("id", String.class);
        CompanyDTO existingCompany = companyDTOMap.get(companyId);

        if (existingCompany == null) {
            existingCompany = CompanyDTO.builder()
                    .id(row.get("id", String.class))
                    .name(row.get("name", String.class))
                    .owner(UserDTO.builder()
                            .id(row.get("owner_id", String.class))
                            .email(row.get("owner_email", String.class))
                            .firstName(row.get("owner_first_name", String.class))
                            .lastName(row.get("owner_last_name", String.class))
                            .build())
                    .users(new ArrayList<>())
                    .build();
            companyDTOMap.put(companyId, existingCompany);
        }

        String id =row.get("member_id", String.class);
        if(id != null){
            UserDTO member = UserDTO.builder()
                    .id(id)
                    .email(row.get("member_email", String.class))
                    .firstName(row.get("member_first_name", String.class))
                    .lastName(row.get("member_last_name", String.class))
                    .build();

            if(existingCompany.getUsers().stream().noneMatch(m -> m.getId().equals(member.getId()))) {
                existingCompany.getUsers().add(member);
            }
        }

        return existingCompany;
    }
}
