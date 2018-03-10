package pl.biotronika;

import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.CrudRepository;
import org.springframework.data.repository.query.Param;

import java.util.List;

public interface PatogenRepository extends CrudRepository<Patogen, Long> {

    @Query(value = "select * from PATOGENS f where f.PATOGEN_ID  = :patogen", nativeQuery = true)
    Patogen findByPatogenId(@Param("patogen")Long ID_PATOGEN);

}