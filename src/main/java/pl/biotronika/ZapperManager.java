package pl.biotronika;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;
import ravo_zapper.EncDec;
import ravo_zapper.RavoRifeProperties;
import ravo_zapper.RavoRifeUtils;
import ravo_zapper.gui.MemoryContent;
import ravo_zapper.languages.Languages;
import ravo_zapper.loggers.RifeLogger;
import ravo_zapper.memory.ZapperMemoryRecord;
import ravo_zapper.packets.PacketReqReadZapperMemory;

import javax.crypto.SecretKey;
import java.io.*;
import java.util.ArrayList;
import java.util.List;

@Component
public class ZapperManager
{

    private static final Logger log = LoggerFactory.getLogger(ZapperManager.class);
    private  RifeLogger logger ;
    //btnLoadFromFileZMActionPerformed

    public  ZapperManager () {
        logger = new RifeLogger();
    }

    private boolean importOK;

    public  List<ZapperMemoryRecord> importZapperTable() {

        File fileFRAM = new File("src/main/resources/CZ2015.fzam");
        RavoRifeProperties.setDirectory("Zapper_Fram_import", new File(fileFRAM.getAbsolutePath()));

        log.info("ZapperManager start! " + fileFRAM.getAbsolutePath());
        File languageFile = new File("src/main/resources/LANGUAGES.csv");
        Languages languages = new Languages(languageFile, "ENGLISH");
        RavoRifeUtils.updateLocalizedOptions();

        List<PacketReqReadZapperMemory> packets = importFRAM(fileFRAM);

        List<ZapperMemoryRecord> newRecords = this.convertPacketsToRecords(packets);

        if (newRecords.size() > 0) {
            ZapperMemoryRecord header = (ZapperMemoryRecord)newRecords.get(0);
            header.setTotalLengthInSeconds(0L);

            for(int i = 1; i < newRecords.size(); ++i) {
                ZapperMemoryRecord record = (ZapperMemoryRecord)newRecords.get(i);
                if (record.isHeader()) {
                    header = record;
                    record.setTotalLengthInSeconds(0L);
                } else {
                    header.setTotalLengthInSeconds(header.getTotalLengthInSeconds() + record.getLengthMinutes() * 60L + record.getLengthSeconds());
                }
            }
        }

        return  newRecords;
    }


    private List<PacketReqReadZapperMemory> importFRAM(File fileFram) {
        FileInputStream fis = null;
        BufferedInputStream bis = null;
        byte[] content = new byte[32];
        List<PacketReqReadZapperMemory> records = new ArrayList();
        SecretKey key = EncDec.getKey(this.logger, "AGyy4_kl66!ye4@.35");

        this.importOK = true;

        try {
            fis = new FileInputStream(fileFram.getAbsoluteFile());
            bis = new BufferedInputStream(fis);
            boolean cont = true;

            while(cont && this.importOK) {
                int readBytes = bis.read(content);
                if (readBytes == 32) {
                    try {
                        records.add(new PacketReqReadZapperMemory(this.decodeByteContent(content, key)));
                    } catch (Exception var10) {
                        this.importOK = false;
                    }
                } else if (readBytes > 0) {
                    this.importOK = false;
                } else {
                    cont = false;
                }
            }
        } catch (FileNotFoundException var11) {
            this.logger.logFramWriter(var11.toString());
            this.importOK = true;
        } catch (IOException var12) {
            this.logger.logFramWriter(var12.toString());
        }

        return records;
    }

    private byte[] decodeByteContent(byte[] content, SecretKey key) throws Exception {
        return EncDec.decryptData(content, key, this.logger, "AGyy4_kl66!ye4@.35");
    }

    public List<ZapperMemoryRecord> convertPacketsToRecords(List<PacketReqReadZapperMemory> packets) {
        List<ZapperMemoryRecord> newRecords = new ArrayList();
        ZapperMemoryRecord newRecord = new ZapperMemoryRecord();
        boolean firstHeaderRecord = true;
        boolean continueReading = true;

        for(int i = 0; i < packets.size() && this.importOK && continueReading; ++i) {
            PacketReqReadZapperMemory packet = (PacketReqReadZapperMemory)packets.get(i);
            if (packet.getStatusByte() != -1) {
                if (packet.isHeaderRecord()) {
                    if (firstHeaderRecord) {
                        newRecord = new ZapperMemoryRecord();
                        newRecord.setIsHeader(true);
                        newRecord.setCureName(packet.getTranslatedCureName());
                        firstHeaderRecord = false;
                    } else {
                        newRecord.setCureName(newRecord.getCureName().concat(packet.getTranslatedCureName()));
                        newRecords.add(newRecord);
                        firstHeaderRecord = true;
                    }
                } else if (firstHeaderRecord) {
                    newRecord = new ZapperMemoryRecord();
                    newRecord.setIsHeader(false);
                    newRecord.setLowerBound(packet.getLowerBound());
                    newRecord.setUpperBound(packet.getUpperBound());
                    newRecord.setShiftSpeed(packet.getShiftSpeed());
                    newRecord.setIsFrequencyHz(packet.getFrequencyRegimeBoolean());
                    newRecord.setLengthMinutes(packet.getCureLengthMinutes());
                    newRecord.setLengthSeconds(packet.getCureLengthSeconds());
                    newRecord.setVoltage(packet.getVoltage());
                    newRecords.add(newRecord);
                } else {
                    this.importOK = false;
                }
            }
        }

        return newRecords;
    }

}
