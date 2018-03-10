package pl.biotronika;


import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import org.springframework.boot.CommandLineRunner;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.Bean;
import ravo_zapper.memory.ZapperMemoryRecord;

import java.io.BufferedWriter;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;

@SpringBootApplication
public class Application {

    private static final Logger log = LoggerFactory.getLogger(Application.class);

    public static void main(String[] args) {
        SpringApplication.run(Application.class);
    }

    @Bean
    public CommandLineRunner createZapperManager(ZapperManager zapperManager) {
        return (args) -> {
            List<ZapperMemoryRecord> records = zapperManager.importZapperTable();

            log.info("records: " + records.size());

            Path path = null;
            BufferedWriter writer = null;

            for (ZapperMemoryRecord record: records) {
                String name = record.getCureName().trim();

                if (record.isHeader()) {

                    log.info("name: " + name);
                    if (writer != null)
                        writer.close();

                    path = Paths.get("scripts_zapper/" + name + ".txt");
                    writer = Files.newBufferedWriter(path);

                    writer.write("# " + name + " " + record.getTotalLengthInSeconds()/60 + "m" + System.lineSeparator());
                } else {

                    if (record.getFrequencyFrom() == record.getFrequencyTo() ) {
                        writer.write("freq " + getStartFreq(record) + " " + getTime(record) + System.lineSeparator());
                    } else {
                        writer.write("freq " + getStartFreq(record) + " 1" + System.lineSeparator());
                        writer.write("scan " + getEndFreq(record) + " " + getTime(record) + System.lineSeparator());
                    }


                }
            }

            if (writer != null)
                writer.close();
        };
    }

    @Bean
    public CommandLineRunner createBiozapScripts(PatogenRepository patogenRepository, FrequencyRepository frequencyRepository) {
        return (args) -> {

            log.info("search patogens with findAll():");
            log.info("-------------------------------");
            for (Patogen patogen : patogenRepository.findAll()) {
                log.info(patogen.toString());

                List<Frequency> frequencies = frequencyRepository.findByPatogenId(patogen.getPatogenId());
                Path path = Paths.get("scripts/" + patogen.getSname() + ".txt");
                try (BufferedWriter writer = Files.newBufferedWriter(path)) {
                    writer.write("# " + patogen.getSname() + " " + frequencies.size() * 3 + "m" + System.lineSeparator());
                    writer.write("# " + patogen.getLname() + System.lineSeparator());
                    writer.write("# " + patogen.getDescription() + System.lineSeparator());

                    for (Frequency frequency : frequencies) {

                        if (frequency.getStartf().equals(frequency.getEndf()) ) {
                            writer.write("freq " + getStartFreq(frequency) + " 180" + System.lineSeparator());
                        } else {
                            writer.write("freq " + getStartFreq(frequency) + " 1" + System.lineSeparator());
                            writer.write("scan " + getEndFreq(frequency) + " 179" + System.lineSeparator());
                        }

                    }
                }
            }
            log.info("Done.");

        };
    }

    String getStartFreq(Frequency frequency) {
        if (frequency.getMultiplier() == 1)
            return frequency.getStartf() + ".00";
        else
            return frequency.getStartf() + "000.00";
    }

    String getEndFreq(Frequency frequency) {
        if (frequency.getMultiplier() == 1)
            return frequency.getEndf() + ".00";
        else
            return frequency.getEndf() + "000.00";
    }


    String getStartFreq(ZapperMemoryRecord frequency) {
        if (frequency.isFrequencyHz())
            return frequency.getFrequencyFrom() + ".00";
        else
            return frequency.getFrequencyFrom() + "000.00";
    }

    String getEndFreq(ZapperMemoryRecord frequency) {
        if (frequency.isFrequencyHz())
            return frequency.getFrequencyTo() + ".00";
        else
            return frequency.getFrequencyTo() + "000.00";
    }


    String getTime(ZapperMemoryRecord zapperMemoryRecord) {
        long time = zapperMemoryRecord.getCureLengthMinutes() * 60 + zapperMemoryRecord.getCureLengthSeconds();
        return "" + time;
    }

}