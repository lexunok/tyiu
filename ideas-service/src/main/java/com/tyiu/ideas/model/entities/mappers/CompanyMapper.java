package com.tyiu.ideas.model.entities.mappers;

import com.tyiu.client.models.UserDTO;
import com.tyiu.ideas.model.dto.CompanyDTO;
import io.r2dbc.spi.Row;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.function.BiFunction;

@Slf4j
@Component
public class CompanyMapper implements BiFunction<Row, Object, CompanyDTO> {

    private final Map<String, CompanyDTO> companyDTOMap = new LinkedHashMap<>();


    @Override
    public CompanyDTO apply(Row row, Object o) {
        log.info("Начало mapper");
        String companyId = row.get("c_id", String.class);
        CompanyDTO existingCompany = companyDTOMap.get(companyId);
        log.info("Взялся id");
        log.info("Сборка CompanyDTO");
        if (existingCompany == null) {
            existingCompany = CompanyDTO.builder()
                    .id(companyId)
                    .name(row.get("c_name", String.class))
                    .owner(UserDTO.builder()
                            .id(row.get("o_id", String.class))
                            .email(row.get("o_email", String.class))
                            .firstName(row.get("o_first_name", String.class))
                            .lastName(row.get("o_last_name", String.class))
                            .build())
                    .users(new ArrayList<>())
                    .build();
            companyDTOMap.put(companyId, existingCompany);
        }
        log.info("Сборка CompanyDTO завершена");
        String id =row.get("m_id", String.class);
        if(id != null){
            log.info("Сборка UserDTO участника началась");
            UserDTO member = UserDTO.builder()
                    .id(id)
                    .email(row.get("m_email", String.class))
                    .firstName(row.get("m_first_name", String.class))
                    .lastName(row.get("m_last_name", String.class))
                    .build();
            log.info("Сборка UserDTO участника завершилась");
            if(existingCompany.getUsers().stream().noneMatch(m -> m.getId().equals(member.getId()))) {
                existingCompany.getUsers().add(member);
            }
        }
        log.info("Возврат CompanyDTO");
        return existingCompany;
    }
}
