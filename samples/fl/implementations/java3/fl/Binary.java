//
// This file was generated by the JavaTM Architecture for XML Binding(JAXB) Reference Implementation, vJAXB 2.1.3 in JDK 1.6 
// See <a href="http://java.sun.com/xml/jaxb">http://java.sun.com/xml/jaxb</a> 
// Any modifications to this file will be lost upon recompilation of the source schema. 
// Generated on: 2008.05.21 at 08:36:03 PM PDT 
//


package fl;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for Binary complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="Binary">
 *   &lt;complexContent>
 *     &lt;extension base="{fl}Expr">
 *       &lt;sequence>
 *         &lt;element name="ops" type="{fl}Ops"/>
 *         &lt;element name="left" type="{fl}Expr"/>
 *         &lt;element name="right" type="{fl}Expr"/>
 *       &lt;/sequence>
 *     &lt;/extension>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "Binary", propOrder = {
    "ops",
    "left",
    "right"
})
public class Binary
    extends Expr
{

    @XmlElement(required = true)
    protected Ops ops;
    @XmlElement(required = true)
    protected Expr left;
    @XmlElement(required = true)
    protected Expr right;

    /**
     * Gets the value of the ops property.
     * 
     * @return
     *     possible object is
     *     {@link Ops }
     *     
     */
    public Ops getOps() {
        return ops;
    }

    /**
     * Sets the value of the ops property.
     * 
     * @param value
     *     allowed object is
     *     {@link Ops }
     *     
     */
    public void setOps(Ops value) {
        this.ops = value;
    }

    /**
     * Gets the value of the left property.
     * 
     * @return
     *     possible object is
     *     {@link Expr }
     *     
     */
    public Expr getLeft() {
        return left;
    }

    /**
     * Sets the value of the left property.
     * 
     * @param value
     *     allowed object is
     *     {@link Expr }
     *     
     */
    public void setLeft(Expr value) {
        this.left = value;
    }

    /**
     * Gets the value of the right property.
     * 
     * @return
     *     possible object is
     *     {@link Expr }
     *     
     */
    public Expr getRight() {
        return right;
    }

    /**
     * Sets the value of the right property.
     * 
     * @param value
     *     allowed object is
     *     {@link Expr }
     *     
     */
    public void setRight(Expr value) {
        this.right = value;
    }

}