package com.tyiu.corn.repository;

import com.tyiu.corn.model.entities.Company;
import org.springframework.data.repository.reactive.ReactiveCrudRepository;
import org.springframework.stereotype.Repository;

public interface CompanyRepository extends ReactiveCrudRepository<Company, Long> {
}
