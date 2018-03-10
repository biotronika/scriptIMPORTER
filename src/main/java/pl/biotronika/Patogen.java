package pl.biotronika;

import javax.persistence.*;

@Entity
@Table(name = "PATOGENS")
public class Patogen {

    @Id
    private Long patogenId;
    private String sname;
    private String lname;
    private String description;



    protected Patogen() {}

    public Patogen(String sname, String lname) {
        this.setSname(lname);
        this.setLname(lname);
    }

    @Override
    public String toString() {
        return String.format(
                "Patogen[id=%d, sname='%s', lname='%s']",
                getPatogenId(), getSname(), getLname());
    }

    public Long getPatogenId() {
        return patogenId;
    }

    public void setPatogenId(Long patogenId) {
        this.patogenId = patogenId;
    }

    public String getSname() {
        return sname;
    }

    public void setSname(String sname) {
        this.sname = sname;
    }

    public String getLname() {
        return lname;
    }

    public void setLname(String lname) {
        this.lname = lname;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }
}
