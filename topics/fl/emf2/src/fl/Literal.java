/**
 * <copyright>
 * </copyright>
 *
 * $Id$
 */
package fl;


/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Literal</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link fl.Literal#getInfo <em>Info</em>}</li>
 * </ul>
 * </p>
 *
 * @see fl.FlPackage#getLiteral()
 * @model extendedMetaData="name='Literal' kind='elementOnly'"
 * @generated
 */
public interface Literal extends Expr {
	/**
	 * Returns the value of the '<em><b>Info</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Info</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Info</em>' attribute.
	 * @see #isSetInfo()
	 * @see #unsetInfo()
	 * @see #setInfo(int)
	 * @see fl.FlPackage#getLiteral_Info()
	 * @model unsettable="true" dataType="org.eclipse.emf.ecore.xml.type.Int" required="true"
	 *        extendedMetaData="kind='element' name='info' namespace='##targetNamespace'"
	 * @generated
	 */
	int getInfo();

	/**
	 * Sets the value of the '{@link fl.Literal#getInfo <em>Info</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Info</em>' attribute.
	 * @see #isSetInfo()
	 * @see #unsetInfo()
	 * @see #getInfo()
	 * @generated
	 */
	void setInfo(int value);

	/**
	 * Unsets the value of the '{@link fl.Literal#getInfo <em>Info</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isSetInfo()
	 * @see #getInfo()
	 * @see #setInfo(int)
	 * @generated
	 */
	void unsetInfo();

	/**
	 * Returns whether the value of the '{@link fl.Literal#getInfo <em>Info</em>}' attribute is set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return whether the value of the '<em>Info</em>' attribute is set.
	 * @see #unsetInfo()
	 * @see #getInfo()
	 * @see #setInfo(int)
	 * @generated
	 */
	boolean isSetInfo();

} // Literal
