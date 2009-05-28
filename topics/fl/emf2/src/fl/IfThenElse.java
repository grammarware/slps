/**
 * <copyright>
 * </copyright>
 *
 * $Id$
 */
package fl;


/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>If Then Else</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link fl.IfThenElse#getIfExpr <em>If Expr</em>}</li>
 *   <li>{@link fl.IfThenElse#getThenExpr <em>Then Expr</em>}</li>
 *   <li>{@link fl.IfThenElse#getElseExpr <em>Else Expr</em>}</li>
 * </ul>
 * </p>
 *
 * @see fl.FlPackage#getIfThenElse()
 * @model extendedMetaData="name='IfThenElse' kind='elementOnly'"
 * @generated
 */
public interface IfThenElse extends Expr {
	/**
	 * Returns the value of the '<em><b>If Expr</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>If Expr</em>' containment reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>If Expr</em>' containment reference.
	 * @see #setIfExpr(Expr)
	 * @see fl.FlPackage#getIfThenElse_IfExpr()
	 * @model containment="true" required="true"
	 *        extendedMetaData="kind='element' name='ifExpr' namespace='##targetNamespace'"
	 * @generated
	 */
	Expr getIfExpr();

	/**
	 * Sets the value of the '{@link fl.IfThenElse#getIfExpr <em>If Expr</em>}' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>If Expr</em>' containment reference.
	 * @see #getIfExpr()
	 * @generated
	 */
	void setIfExpr(Expr value);

	/**
	 * Returns the value of the '<em><b>Then Expr</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Then Expr</em>' containment reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Then Expr</em>' containment reference.
	 * @see #setThenExpr(Expr)
	 * @see fl.FlPackage#getIfThenElse_ThenExpr()
	 * @model containment="true" required="true"
	 *        extendedMetaData="kind='element' name='thenExpr' namespace='##targetNamespace'"
	 * @generated
	 */
	Expr getThenExpr();

	/**
	 * Sets the value of the '{@link fl.IfThenElse#getThenExpr <em>Then Expr</em>}' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Then Expr</em>' containment reference.
	 * @see #getThenExpr()
	 * @generated
	 */
	void setThenExpr(Expr value);

	/**
	 * Returns the value of the '<em><b>Else Expr</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Else Expr</em>' containment reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Else Expr</em>' containment reference.
	 * @see #setElseExpr(Expr)
	 * @see fl.FlPackage#getIfThenElse_ElseExpr()
	 * @model containment="true" required="true"
	 *        extendedMetaData="kind='element' name='elseExpr' namespace='##targetNamespace'"
	 * @generated
	 */
	Expr getElseExpr();

	/**
	 * Sets the value of the '{@link fl.IfThenElse#getElseExpr <em>Else Expr</em>}' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Else Expr</em>' containment reference.
	 * @see #getElseExpr()
	 * @generated
	 */
	void setElseExpr(Expr value);

} // IfThenElse
