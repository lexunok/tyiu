package com.tyiu.ideas.service;

import com.tyiu.client.models.UserDTO;
import com.tyiu.ideas.model.dto.CompanyDTO;
import com.tyiu.ideas.model.entities.Company;
import com.tyiu.ideas.model.entities.relations.Company2User;
import lombok.RequiredArgsConstructor;
import org.modelmapper.ModelMapper;
import org.springframework.cache.annotation.CacheConfig;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.data.r2dbc.core.R2dbcEntityTemplate;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.util.ArrayList;
import java.util.concurrent.ConcurrentHashMap;

import static org.springframework.data.relational.core.query.Criteria.where;
import static org.springframework.data.relational.core.query.Query.query;
import static org.springframework.data.relational.core.query.Update.update;

@Service
@RequiredArgsConstructor
@CacheConfig(cacheNames = "companies")
public class CompanyService {

    private final R2dbcEntityTemplate template;
    private final ModelMapper mapper;

    private Mono<CompanyDTO> getCompany(String companyId) {
        String query = """
            SELECT 
                c.id c_id, c.name c_name, c.owner_id c_owner_id,
                o.id o_id, o.email o_email, o.first_name o_first_name, o.last_name o_last_name,
                m.id m_id, m.email m_email, m.first_name m_first_name, m.last_name m_last_name
            FROM company c
                LEFT JOIN users o ON c.owner_id = o.id
                LEFT JOIN company_user cu ON c.id = cu.company_id
                LEFT JOIN users m ON cu.user_id = m.id
            WHERE c.id = :companyId
        """;

        ConcurrentHashMap<String, CompanyDTO> map = new ConcurrentHashMap<>();

        return template.getDatabaseClient()
                .sql(query)
                .bind("companyId", companyId)
                .map((row, rowMetadata) -> {
                    String id = row.get("c_id", String.class);
                    String memberId = row.get("m_id", String.class);
                    CompanyDTO companyDTO = map.getOrDefault(companyId, CompanyDTO.builder()
                            .id(companyId)
                            .name(row.get("c_name", String.class))
                            .owner(UserDTO.builder()
                                    .id(row.get("o_id", String.class))
                                    .email(row.get("o_email", String.class))
                                    .firstName(row.get("o_first_name", String.class))
                                    .lastName(row.get("o_last_name", String.class))
                                    .build())
                            .users(new ArrayList<>())
                            .build());
                    if (memberId != null) {
                        companyDTO.getUsers().add(UserDTO.builder()
                                .id(id)
                                .email(row.get("m_email", String.class))
                                .firstName(row.get("m_first_name", String.class))
                                .lastName(row.get("m_last_name", String.class))
                                .build());
                    }
                    map.put(companyId, companyDTO);
                    return companyDTO;
                })
                .all().then(Flux.fromIterable(map.values()).next());
    }

    public Mono<CompanyDTO> getCompanyById(String companyId){
        return getCompany(companyId);
    }

    public Flux<CompanyDTO> getMembersListCompany(String userId) {
        String QUERY = "SELECT company_user.company_id, company.*, users.id user_id, users.email, users.first_name, users.last_name " +
                "FROM company_user " +
                "LEFT JOIN company ON company.id = company_user.company_id " +
                "LEFT JOIN users ON users.id = company.owner_id " +
                "WHERE company_user.user_id = :userId OR company.owner_id = :userId";
        return template.getDatabaseClient()
                .sql(QUERY)
                .bind("userId",userId)
                .map((row, rowMetadata) -> CompanyDTO.builder()
                        .id(row.get("id", String.class))
                        .name(row.get("name", String.class))
                        .owner(UserDTO.builder()
                                .id(row.get("user_id", String.class))
                                .firstName(row.get("first_name", String.class))
                                .lastName(row.get("last_name", String.class))
                                .email(row.get("email", String.class))
                                .build())
                        .build())
                .all();
    }

    @Cacheable
    public Flux<CompanyDTO> getListCompany() {
        return template.getDatabaseClient()
                .sql("SELECT company.*, users.id user_id, users.email, users.first_name, users.last_name " +
                        "FROM company " +
                        "LEFT JOIN users ON company.owner_id = users.id")
                .map((row, rowMetadata) -> CompanyDTO.builder()
                                .id(row.get("id", String.class))
                                .name(row.get("name", String.class))
                                .owner(UserDTO.builder()
                                        .id(row.get("user_id", String.class))
                                        .firstName(row.get("first_name", String.class))
                                        .lastName(row.get("last_name", String.class))
                                        .email(row.get("email", String.class))
                                        .build())
                                .build()).all();
    }

    public Flux<UserDTO> getListStaff(String id) {
        String QUERY = "SELECT company_user.*, users.id, users.email, users.first_name, users.last_name " +
                "FROM company_user " +
                "LEFT JOIN users ON company_user.user_id = users.id " +
                "WHERE company_user.company_id = :companyId";
        return template.getDatabaseClient()
                .sql(QUERY)
                .bind("companyId",id)
                .map((row, rowMetadata) -> UserDTO.builder()
                        .id(row.get("id", String.class))
                        .email(row.get("email", String.class))
                        .firstName(row.get("first_name", String.class))
                        .lastName(row.get("last_name", String.class))
                        .build()).all();
    }

    @CacheEvict(allEntries = true)
    public Mono<CompanyDTO> createCompany(CompanyDTO companyDTO) {
        Company company = mapper.map(companyDTO, Company.class);
        company.setOwnerId(companyDTO.getOwner().getId());
        return template.insert(company).flatMap(c -> {
            companyDTO.setId(c.getId());
            return Flux.fromIterable(companyDTO.getUsers()).flatMap(u ->
                    template.insert(new Company2User(u.getId(), c.getId())
                    )).next().flatMap(co -> getCompany(c.getId()));
        });
    }

    @CacheEvict(allEntries = true)
    public Mono<Void> deleteCompany(String id) {
        return template.delete(query(where("id").is(id)), Company.class).then();
    }

    @CacheEvict(allEntries = true)
    public Mono<CompanyDTO> updateCompany(String companyId, CompanyDTO companyDTO) {
        return template.delete(query(where("company_id").is(companyId)), Company2User.class)
                .thenMany(Flux.fromIterable(companyDTO.getUsers())
                        .flatMap(u -> template.insert(new Company2User(u.getId(), companyId))))
                .then(template.update(query(where("id").is(companyId)),
                        update("owner_id", companyDTO.getOwner().getId()),
                        Company.class))
                .thenReturn(companyDTO);
    }
}
