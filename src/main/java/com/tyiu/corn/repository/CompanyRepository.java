package com.tyiu.corn.repository;

import com.tyiu.corn.model.entities.Company;
import org.springframework.data.jpa.repository.JpaRepository;

public interface CompanyRepository extends JpaRepository<Company, Long> {
}
