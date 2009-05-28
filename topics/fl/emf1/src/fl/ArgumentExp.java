/**
 * <copyright>
 * </copyright>
 *
 * $Id$
 */
package fl;


/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Argument Exp</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link fl.ArgumentExp#getArgument <em>Argument</em>}</li>
 * </ul>
 * </p>
 *
 * @see fl.FlPackage#getArgumentExp()
 * @model
 * @generated
 */
public interface ArgumentExp extends Exp {
	/**
	 * Returns the value of the '<em><b>Argument</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Argument</em>' reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Argument</em>' reference.
	 * @see #setArgument(Argument)
	 * @see fl.FlPackage#getArgumentExp_Argument()
	 * @model required="true"
	 * @generated
	 */
	Argument getArgument();

	/**
	 * Sets the value of the '{@link fl.ArgumentExp#getArgument <em>Argument</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Argument</em>' reference.
	 * @see #getArgument()
	 * @generated
	 */
	void setArgument(Argument value);

} // ArgumentExp
