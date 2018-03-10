package pl.biotronika;

import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;


@Entity
@Table(name = "FREQUENCIES")
public class Frequency {

    @Id
    private Long frequencyId;
    private Long patogenId;
    private Long sweep;
    private Long startf;
    private Long endf;
    private Long shift;
    private Long multiplier;
    private Long hours;
    private Long minutes;
    private Long seconds;

    protected Frequency() {
    }

    public Frequency(Long frequencyId, Long patogenId, Long sweep, Long startf, Long endf,
                     Long shift, Long multiplier, Long hours, Long minutes, Long seconds) {

        this.setFrequencyId(frequencyId);
        this.setPatogenId(patogenId);
        this.setSweep(sweep);
        this.setStartf(startf);
        this.setEndf(endf);
        this.setShift(shift);
        this.setMultiplier(multiplier);
        this.setHours(hours);
        this.setMinutes(minutes);
        this.setSeconds(seconds);

    }

    @Override
    public String toString() {
        return String.format(
                "Frequency[id=%d]",
                getFrequencyId());
    }

    public Long getFrequencyId() {
        return frequencyId;
    }

    public void setFrequencyId(Long frequencyId) {
        this.frequencyId = frequencyId;
    }

    public Long getPatogenId() {
        return patogenId;
    }

    public void setPatogenId(Long patogenId) {
        this.patogenId = patogenId;
    }

    public Long getSweep() {
        return sweep;
    }

    public void setSweep(Long sweep) {
        this.sweep = sweep;
    }

    public Long getStartf() {
        return startf;
    }

    public void setStartf(Long startf) {
        this.startf = startf;
    }

    public Long getEndf() {
        return endf;
    }

    public void setEndf(Long endf) {
        this.endf = endf;
    }

    public Long getShift() {
        return shift;
    }

    public void setShift(Long shift) {
        this.shift = shift;
    }

    public Long getMultiplier() {
        return multiplier;
    }

    public void setMultiplier(Long multiplier) {
        this.multiplier = multiplier;
    }

    public Long getHours() {
        return hours;
    }

    public void setHours(Long hours) {
        this.hours = hours;
    }

    public Long getMinutes() {
        return minutes;
    }

    public void setMinutes(Long minutes) {
        this.minutes = minutes;
    }

    public Long getSeconds() {
        return seconds;
    }

    public void setSeconds(Long seconds) {
        this.seconds = seconds;
    }
}
