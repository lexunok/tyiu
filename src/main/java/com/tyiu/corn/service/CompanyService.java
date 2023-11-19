package com.tyiu.corn.service;

import com.tyiu.corn.config.exception.NotFoundException;
import com.tyiu.corn.model.dto.CompanyDTO;
import com.tyiu.corn.model.entities.mappers.CompanyMapper;
import com.tyiu.corn.model.entities.mappers.UserMapper;
import com.tyiu.corn.model.entities.relations.Company2User;
import lombok.RequiredArgsConstructor;
import org.modelmapper.ModelMapper;
import org.springframework.cache.annotation.CacheConfig;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.data.r2dbc.core.R2dbcEntityTemplate;
import org.springframework.stereotype.Service;
import com.tyiu.corn.model.dto.UserDTO;
import com.tyiu.corn.model.entities.Company;

import static org.springframework.data.relational.core.query.Criteria.where;
import static org.springframework.data.relational.core.query.Query.query;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.util.ArrayList;
import java.util.List;

@Service
@RequiredArgsConstructor
@CacheConfig(cacheNames = "companies")
public class CompanyService {

    private final R2dbcEntityTemplate template;
    private final ModelMapper mapper;
    private final UserMapper userMapper;
    private final CompanyMapper companyMapper;

    private Mono<CompanyDTO> getCompany(String companyId) {
        String query = "SELECT company.*, users.id AS user_id, users.email, users.first_name, users.last_name," +
                "owner.id owner_id, owner.email owner_email, owner.first_name owner_first_name, owner.last_name owner_last_name " +
                "FROM company " +
                "LEFT JOIN company_user ON company.id = company_user.company_id " +
                "LEFT JOIN users ON company_user.user_id = users.id " +
                "LEFT JOIN users owner ON company.owner_id = owner.id " +
                "WHERE company.id = :companyId";

        return template.getDatabaseClient()
                .sql(query)
                .bind("companyId", companyId)
                .flatMap(c -> {
                    List<UserDTO> users = new ArrayList<>();
                    return c.map((row, rowMetadata) -> {
                        users.add(userMapper.apply(row,rowMetadata));
                        CompanyDTO companyDTO = companyMapper.apply(row,rowMetadata);
                        companyDTO.setUsers(users);
                        return companyDTO;
                    });
                }).last();
    }

    @Cacheable
    public Mono<CompanyDTO> getCompanyById(String companyId){
        return getCompany(companyId);
    }

    @Cacheable
    public Flux<CompanyDTO> getListCompany() {
        return template.select(Company.class).all()
                .flatMap(c -> Flux.just(mapper.map(c, CompanyDTO.class)));
    }

    @Cacheable
    public Mono<CompanyDTO> getListStaff(String id) {
        return template.selectOne(query(where("id").is(id)), Company.class)
                .flatMap(c -> {
                CompanyDTO companyDTO = mapper.map(c, CompanyDTO.class);
                return template.select(query(where("id").in(c.getId())), Company.class)
                        .flatMap(u -> Flux.just(mapper.map(u, UserDTO.class))).collectList()
                        .flatMap(list -> {
                            companyDTO.setUsers(list);
                            return Mono.just(companyDTO);
                        });
                }).switchIfEmpty(Mono.error(new NotFoundException("Not success!")));
    }

    @CacheEvict(allEntries = true)
    public Mono<CompanyDTO> createCompany(CompanyDTO companyDTO) {

        Company company = mapper.map(companyDTO, Company.class);
        company.setOwnerId(companyDTO.getOwner().getId());

        return template.insert(company).flatMap(c -> {
            companyDTO.setId(c.getId());
            return Flux.fromIterable(companyDTO.getUsers()).flatMap(u ->
                    template.insert(new Company2User(u.getId(), c.getId())
                    )).next().flatMap(co -> getCompany(companyDTO.getId()));
        });
    }

    @CacheEvict(allEntries = true)
    public Mono<Void> deleteCompany(String id) {
        return template.delete(query(where("id").is(id)), Company.class).then();
    }

    @CacheEvict(allEntries = true)
    public Mono<CompanyDTO> updateCompany(String companyId, CompanyDTO companyDTO) {
        return getCompany(companyId)
                .flatMap(c -> {
                    companyDTO.setId(c.getId());
                    companyDTO.setOwner(c.getOwner());
                    return template.delete(query(where("company_id").is(companyId)), Company2User.class)
                            .thenReturn(companyDTO.getUsers())
                            .map(list -> {
                                list.forEach(u -> template.insert(new Company2User(u.getId(), companyId)).subscribe());
                                return list;
                            }).thenReturn(companyDTO);
                });
    }
}
