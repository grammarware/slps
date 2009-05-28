/**
 * <copyright>
 * </copyright>
 *
 * $Id$
 */
package fl;


/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>If Then Else Exp</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link fl.IfThenElseExp#getIf <em>If</em>}</li>
 *   <li>{@link fl.IfThenElseExp#getThen <em>Then</em>}</li>
 *   <li>{@link fl.IfThenElseExp#getElse <em>Else</em>}</li>
 * </ul>
 * </p>
 *
 * @see fl.FlPackage#getIfThenElseExp()
 * @model
 * @generated
 */
public interface IfThenElseExp extends Exp {
	/**
	 * Returns the value of the '<em><b>If</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>If</em>' containment reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>If</em>' containment reference.
	 * @see #setIf(Exp)
	 * @see fl.FlPackage#getIfThenElseExp_If()
	 * @model containment="true" required="true"
	 * @generated
	 */
	Exp getIf();

	/**
	 * Sets the value of the '{@link fl.IfThenElseExp#getIf <em>If</em>}' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>If</em>' containment reference.
	 * @see #getIf()
	 * @generated
	 */
	void setIf(Exp value);

	/**
	 * Returns the value of the '<em><b>Then</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Then</em>' containment reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Then</em>' containment reference.
	 * @see #setThen(Exp)
	 * @see fl.FlPackage#getIfThenElseExp_Then()
	 * @model containment="true" required="true"
	 * @generated
	 */
	Exp getThen();

	/**
	 * Sets the value of the '{@link fl.IfThenElseExp#getThen <em>Then</em>}' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Then</em>' containment reference.
	 * @see #getThen()
	 * @generated
	 */
	void setThen(Exp value);

	/**
	 * Returns the value of the '<em><b>Else</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Else</em>' containment reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Else</em>' containment reference.
	 * @see #setElse(Exp)
	 * @see fl.FlPackage#getIfThenElseExp_Else()
	 * @model containment="true" required="true"
	 * @generated
	 */
	Exp getElse();

	/**
	 * Sets the value of the '{@link fl.IfThenElseExp#getElse <em>Else</em>}' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Else</em>' containment reference.
	 * @see #getElse()
	 * @generated
	 */
	void setElse(Exp value);

} // IfThenElseExp
