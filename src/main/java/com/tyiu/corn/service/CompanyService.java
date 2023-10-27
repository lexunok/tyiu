package com.tyiu.corn.service;

import com.tyiu.corn.config.exception.NotFoundException;
import com.tyiu.corn.model.dto.CompanyDTO;
import com.tyiu.corn.model.entities.mappers.CompanyMapper;
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
import java.util.List;

@Service
@RequiredArgsConstructor
@CacheConfig(cacheNames = "companies")
public class CompanyService {

    private final R2dbcEntityTemplate template;
    private final ModelMapper mapper;
    private final CompanyMapper companyMapper = new CompanyMapper();
    private final String Query = "SELECT company.*, users.id AS member_id, users.email, users.first_name, users.last_name " +
            "FROM company " +
            "LEFT JOIN company_user ON company.id = company_user.company_id " +
            "LEFT JOIN users ON company_user.user_id = users.id " +
            "WHERE company.id = :companyId";

    @Cacheable
    public Mono<CompanyDTO> getCompanyById(Long companyId){

        CompanyMapper companyMapper = new CompanyMapper();

        return template.getDatabaseClient()
                .sql(Query)
                .bind("companyId", companyId)
                .map(companyMapper::apply)
                .all()
                .collectList()
                .map(companyDTOMap -> companyDTOMap.get(0));
    }

    @Cacheable
    public Flux<CompanyDTO> getListCompany() {
        return template.select(Company.class).all()
                .flatMap(g -> Flux.just(mapper.map(g, CompanyDTO.class)));
    }

    @Cacheable
    public Mono<CompanyDTO> getListStaff(Long id) {
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

        return template.insert(company).flatMap(c -> {
            companyDTO.setId(c.getId());
            companyDTO.getUsers().forEach(u -> template.insert(new Company2User(u.getId(), c.getId()))
                    .subscribe());
            return Mono.just(companyDTO);
        });
    }

    @CacheEvict(allEntries = true)
    public Mono<Void> deleteCompany(Long id) {
        return template.delete(query(where("id").is(id)), Company.class).then();
    }

    @CacheEvict(allEntries = true)
    public Mono<CompanyDTO> updateCompany(Long companyId,CompanyDTO companyDTO) {
        return template.getDatabaseClient()
                .sql(Query)
                .bind("companyId", companyId)
                .map(companyMapper::apply)
                .all()
                .collectList()
                .map(companyDTOMap -> companyDTOMap.get(0))
                .flatMap(c -> {
                    c.setName(companyDTO.getName());
                    List<UserDTO> newUsers = companyDTO.getUsers();
                    List<UserDTO> oldUsers = c.getUsers();
                    if (!newUsers.equals(oldUsers))
                    {
                        oldUsers.forEach(u -> template.delete(query(where("company_id").is(companyId)
                                .and("user_id").is(u.getId())), Company2User.class).subscribe());
                        newUsers.forEach(u -> template.insert((new Company2User(u.getId(), companyId))).subscribe());
                        c.setUsers(companyDTO.getUsers());
                    }
                    return template.update(mapper.map(c, Company.class)).then(Mono.just(c));
                })
                .onErrorResume(ex -> Mono.error(new NotFoundException("Failed to update a company")));
    }
}
