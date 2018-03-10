package pl.biotronika;

import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.CrudRepository;
import org.springframework.data.repository.query.Param;

import java.util.List;

public interface FrequencyRepository extends CrudRepository<Frequency, Long> {

    @Query(value = "select * from FREQUENCIES f where f.PATOGEN_ID  = :patogen", nativeQuery = true)
    List<Frequency> findByPatogenId(@Param("patogen")Long ID_PATOGEN);


}