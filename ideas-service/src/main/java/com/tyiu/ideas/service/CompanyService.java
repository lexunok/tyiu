package com.tyiu.ideas.service;

import com.tyiu.ideas.model.dto.CompanyDTO;
import com.tyiu.ideas.model.entities.mappers.CompanyMapper;
import com.tyiu.ideas.model.entities.relations.Company2User;
import lombok.RequiredArgsConstructor;
import org.modelmapper.ModelMapper;
import org.springframework.cache.annotation.CacheConfig;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.data.r2dbc.core.R2dbcEntityTemplate;
import org.springframework.stereotype.Service;
import com.tyiu.ideas.model.dto.UserDTO;
import com.tyiu.ideas.model.entities.Company;

import static org.springframework.data.relational.core.query.Criteria.where;
import static org.springframework.data.relational.core.query.Query.query;

import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

@Service
@RequiredArgsConstructor
@CacheConfig(cacheNames = "companies")
public class CompanyService {

    private final R2dbcEntityTemplate template;
    private final ModelMapper mapper;
    private final CompanyMapper companyMapper;

    private Mono<CompanyDTO> getCompany(String companyId) {
        String query = "SELECT company.*, " +
                "owner.id owner_id, owner.email owner_email, owner.first_name owner_first_name, owner.last_name owner_last_name, " +
                "member.id member_id, member.email member_email, member.first_name member_first_name, member.last_name member_last_name " +
                "FROM company " +
                "LEFT JOIN users owner ON company.owner_id = owner.id " +
                "LEFT JOIN company_user ON company.id = company_user.company_id " +
                "LEFT JOIN users member ON company_user.user_id = member.id " +
                "WHERE company.id = :companyId";

        return template.getDatabaseClient()
                .sql(query)
                .bind("companyId", companyId)
                .map(companyMapper::apply)
                .all()
                .collectList()
                .map(c -> c.get(0));
    }

    @Cacheable
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

    @Cacheable
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
        Company company = mapper.map(companyDTO, Company.class);
        company.setId(companyId);
        company.setOwnerId(companyDTO.getOwner().getId());
        return template.update(company).flatMap(c ->
                template.delete(query(where("company_id").is(companyId)), Company2User.class)
                        .thenReturn(companyDTO.getUsers()).mapNotNull(list -> {
                            list.forEach(member -> template.insert(new Company2User(member.getId(), companyId)).subscribe());
                            return list;
                        }).thenReturn(companyDTO));
    }
}
