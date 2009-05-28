/**
 * <copyright>
 * </copyright>
 *
 * $Id$
 */
package fl;


/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Binary</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link fl.Binary#getOps <em>Ops</em>}</li>
 *   <li>{@link fl.Binary#getLeft <em>Left</em>}</li>
 *   <li>{@link fl.Binary#getRight <em>Right</em>}</li>
 * </ul>
 * </p>
 *
 * @see fl.FlPackage#getBinary()
 * @model extendedMetaData="name='Binary' kind='elementOnly'"
 * @generated
 */
public interface Binary extends Expr {
	/**
	 * Returns the value of the '<em><b>Ops</b></em>' attribute.
	 * The literals are from the enumeration {@link fl.Ops}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Ops</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Ops</em>' attribute.
	 * @see fl.Ops
	 * @see #isSetOps()
	 * @see #unsetOps()
	 * @see #setOps(Ops)
	 * @see fl.FlPackage#getBinary_Ops()
	 * @model unsettable="true" required="true"
	 *        extendedMetaData="kind='element' name='ops' namespace='##targetNamespace'"
	 * @generated
	 */
	Ops getOps();

	/**
	 * Sets the value of the '{@link fl.Binary#getOps <em>Ops</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Ops</em>' attribute.
	 * @see fl.Ops
	 * @see #isSetOps()
	 * @see #unsetOps()
	 * @see #getOps()
	 * @generated
	 */
	void setOps(Ops value);

	/**
	 * Unsets the value of the '{@link fl.Binary#getOps <em>Ops</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isSetOps()
	 * @see #getOps()
	 * @see #setOps(Ops)
	 * @generated
	 */
	void unsetOps();

	/**
	 * Returns whether the value of the '{@link fl.Binary#getOps <em>Ops</em>}' attribute is set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return whether the value of the '<em>Ops</em>' attribute is set.
	 * @see #unsetOps()
	 * @see #getOps()
	 * @see #setOps(Ops)
	 * @generated
	 */
	boolean isSetOps();

	/**
	 * Returns the value of the '<em><b>Left</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Left</em>' containment reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Left</em>' containment reference.
	 * @see #setLeft(Expr)
	 * @see fl.FlPackage#getBinary_Left()
	 * @model containment="true" required="true"
	 *        extendedMetaData="kind='element' name='left' namespace='##targetNamespace'"
	 * @generated
	 */
	Expr getLeft();

	/**
	 * Sets the value of the '{@link fl.Binary#getLeft <em>Left</em>}' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Left</em>' containment reference.
	 * @see #getLeft()
	 * @generated
	 */
	void setLeft(Expr value);

	/**
	 * Returns the value of the '<em><b>Right</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Right</em>' containment reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Right</em>' containment reference.
	 * @see #setRight(Expr)
	 * @see fl.FlPackage#getBinary_Right()
	 * @model containment="true" required="true"
	 *        extendedMetaData="kind='element' name='right' namespace='##targetNamespace'"
	 * @generated
	 */
	Expr getRight();

	/**
	 * Sets the value of the '{@link fl.Binary#getRight <em>Right</em>}' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Right</em>' containment reference.
	 * @see #getRight()
	 * @generated
	 */
	void setRight(Expr value);

} // Binary
