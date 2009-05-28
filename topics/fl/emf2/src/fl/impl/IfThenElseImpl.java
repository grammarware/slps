/**
 * <copyright>
 * </copyright>
 *
 * $Id$
 */
package fl.impl;

import fl.Expr;
import fl.FlPackage;
import fl.IfThenElse;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.ENotificationImpl;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>If Then Else</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link fl.impl.IfThenElseImpl#getIfExpr <em>If Expr</em>}</li>
 *   <li>{@link fl.impl.IfThenElseImpl#getThenExpr <em>Then Expr</em>}</li>
 *   <li>{@link fl.impl.IfThenElseImpl#getElseExpr <em>Else Expr</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class IfThenElseImpl extends ExprImpl implements IfThenElse {
	/**
	 * The cached value of the '{@link #getIfExpr() <em>If Expr</em>}' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getIfExpr()
	 * @generated
	 * @ordered
	 */
	protected Expr ifExpr;

	/**
	 * The cached value of the '{@link #getThenExpr() <em>Then Expr</em>}' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getThenExpr()
	 * @generated
	 * @ordered
	 */
	protected Expr thenExpr;

	/**
	 * The cached value of the '{@link #getElseExpr() <em>Else Expr</em>}' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getElseExpr()
	 * @generated
	 * @ordered
	 */
	protected Expr elseExpr;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected IfThenElseImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return FlPackage.Literals.IF_THEN_ELSE;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Expr getIfExpr() {
		return ifExpr;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public NotificationChain basicSetIfExpr(Expr newIfExpr, NotificationChain msgs) {
		Expr oldIfExpr = ifExpr;
		ifExpr = newIfExpr;
		if (eNotificationRequired()) {
			ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, FlPackage.IF_THEN_ELSE__IF_EXPR, oldIfExpr, newIfExpr);
			if (msgs == null) msgs = notification; else msgs.add(notification);
		}
		return msgs;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setIfExpr(Expr newIfExpr) {
		if (newIfExpr != ifExpr) {
			NotificationChain msgs = null;
			if (ifExpr != null)
				msgs = ((InternalEObject)ifExpr).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - FlPackage.IF_THEN_ELSE__IF_EXPR, null, msgs);
			if (newIfExpr != null)
				msgs = ((InternalEObject)newIfExpr).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - FlPackage.IF_THEN_ELSE__IF_EXPR, null, msgs);
			msgs = basicSetIfExpr(newIfExpr, msgs);
			if (msgs != null) msgs.dispatch();
		}
		else if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, FlPackage.IF_THEN_ELSE__IF_EXPR, newIfExpr, newIfExpr));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Expr getThenExpr() {
		return thenExpr;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public NotificationChain basicSetThenExpr(Expr newThenExpr, NotificationChain msgs) {
		Expr oldThenExpr = thenExpr;
		thenExpr = newThenExpr;
		if (eNotificationRequired()) {
			ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, FlPackage.IF_THEN_ELSE__THEN_EXPR, oldThenExpr, newThenExpr);
			if (msgs == null) msgs = notification; else msgs.add(notification);
		}
		return msgs;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setThenExpr(Expr newThenExpr) {
		if (newThenExpr != thenExpr) {
			NotificationChain msgs = null;
			if (thenExpr != null)
				msgs = ((InternalEObject)thenExpr).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - FlPackage.IF_THEN_ELSE__THEN_EXPR, null, msgs);
			if (newThenExpr != null)
				msgs = ((InternalEObject)newThenExpr).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - FlPackage.IF_THEN_ELSE__THEN_EXPR, null, msgs);
			msgs = basicSetThenExpr(newThenExpr, msgs);
			if (msgs != null) msgs.dispatch();
		}
		else if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, FlPackage.IF_THEN_ELSE__THEN_EXPR, newThenExpr, newThenExpr));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Expr getElseExpr() {
		return elseExpr;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public NotificationChain basicSetElseExpr(Expr newElseExpr, NotificationChain msgs) {
		Expr oldElseExpr = elseExpr;
		elseExpr = newElseExpr;
		if (eNotificationRequired()) {
			ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, FlPackage.IF_THEN_ELSE__ELSE_EXPR, oldElseExpr, newElseExpr);
			if (msgs == null) msgs = notification; else msgs.add(notification);
		}
		return msgs;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setElseExpr(Expr newElseExpr) {
		if (newElseExpr != elseExpr) {
			NotificationChain msgs = null;
			if (elseExpr != null)
				msgs = ((InternalEObject)elseExpr).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - FlPackage.IF_THEN_ELSE__ELSE_EXPR, null, msgs);
			if (newElseExpr != null)
				msgs = ((InternalEObject)newElseExpr).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - FlPackage.IF_THEN_ELSE__ELSE_EXPR, null, msgs);
			msgs = basicSetElseExpr(newElseExpr, msgs);
			if (msgs != null) msgs.dispatch();
		}
		else if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, FlPackage.IF_THEN_ELSE__ELSE_EXPR, newElseExpr, newElseExpr));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public NotificationChain eInverseRemove(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
		switch (featureID) {
			case FlPackage.IF_THEN_ELSE__IF_EXPR:
				return basicSetIfExpr(null, msgs);
			case FlPackage.IF_THEN_ELSE__THEN_EXPR:
				return basicSetThenExpr(null, msgs);
			case FlPackage.IF_THEN_ELSE__ELSE_EXPR:
				return basicSetElseExpr(null, msgs);
		}
		return super.eInverseRemove(otherEnd, featureID, msgs);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case FlPackage.IF_THEN_ELSE__IF_EXPR:
				return getIfExpr();
			case FlPackage.IF_THEN_ELSE__THEN_EXPR:
				return getThenExpr();
			case FlPackage.IF_THEN_ELSE__ELSE_EXPR:
				return getElseExpr();
		}
		return super.eGet(featureID, resolve, coreType);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void eSet(int featureID, Object newValue) {
		switch (featureID) {
			case FlPackage.IF_THEN_ELSE__IF_EXPR:
				setIfExpr((Expr)newValue);
				return;
			case FlPackage.IF_THEN_ELSE__THEN_EXPR:
				setThenExpr((Expr)newValue);
				return;
			case FlPackage.IF_THEN_ELSE__ELSE_EXPR:
				setElseExpr((Expr)newValue);
				return;
		}
		super.eSet(featureID, newValue);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void eUnset(int featureID) {
		switch (featureID) {
			case FlPackage.IF_THEN_ELSE__IF_EXPR:
				setIfExpr((Expr)null);
				return;
			case FlPackage.IF_THEN_ELSE__THEN_EXPR:
				setThenExpr((Expr)null);
				return;
			case FlPackage.IF_THEN_ELSE__ELSE_EXPR:
				setElseExpr((Expr)null);
				return;
		}
		super.eUnset(featureID);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public boolean eIsSet(int featureID) {
		switch (featureID) {
			case FlPackage.IF_THEN_ELSE__IF_EXPR:
				return ifExpr != null;
			case FlPackage.IF_THEN_ELSE__THEN_EXPR:
				return thenExpr != null;
			case FlPackage.IF_THEN_ELSE__ELSE_EXPR:
				return elseExpr != null;
		}
		return super.eIsSet(featureID);
	}

} //IfThenElseImpl
