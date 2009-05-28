/**
 * <copyright>
 * </copyright>
 *
 * $Id$
 */
package fl.impl;

import fl.*;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EPackage;

import org.eclipse.emf.ecore.impl.EFactoryImpl;

import org.eclipse.emf.ecore.plugin.EcorePlugin;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model <b>Factory</b>.
 * <!-- end-user-doc -->
 * @generated
 */
public class FlFactoryImpl extends EFactoryImpl implements FlFactory {
	/**
	 * Creates the default factory implementation.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public static FlFactory init() {
		try {
			FlFactory theFlFactory = (FlFactory)EPackage.Registry.INSTANCE.getEFactory("http://hub.metrik.lang.fl/1.0"); 
			if (theFlFactory != null) {
				return theFlFactory;
			}
		}
		catch (Exception exception) {
			EcorePlugin.INSTANCE.log(exception);
		}
		return new FlFactoryImpl();
	}

	/**
	 * Creates an instance of the factory.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public FlFactoryImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EObject create(EClass eClass) {
		switch (eClass.getClassifierID()) {
			case FlPackage.PROGRAM: return createProgram();
			case FlPackage.FUNCTION: return createFunction();
			case FlPackage.ARGUMENT: return createArgument();
			case FlPackage.LITERAL_EXP: return createLiteralExp();
			case FlPackage.ARGUMENT_EXP: return createArgumentExp();
			case FlPackage.IF_THEN_ELSE_EXP: return createIfThenElseExp();
			case FlPackage.APPLY_EXP: return createApplyExp();
			case FlPackage.PLUS_EXP: return createPlusExp();
			case FlPackage.MINUS_EXP: return createMinusExp();
			case FlPackage.EQUAL_EXP: return createEqualExp();
			default:
				throw new IllegalArgumentException("The class '" + eClass.getName() + "' is not a valid classifier");
		}
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Program createProgram() {
		ProgramImpl program = new ProgramImpl();
		return program;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Function createFunction() {
		FunctionImpl function = new FunctionImpl();
		return function;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Argument createArgument() {
		ArgumentImpl argument = new ArgumentImpl();
		return argument;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public LiteralExp createLiteralExp() {
		LiteralExpImpl literalExp = new LiteralExpImpl();
		return literalExp;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public ArgumentExp createArgumentExp() {
		ArgumentExpImpl argumentExp = new ArgumentExpImpl();
		return argumentExp;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public IfThenElseExp createIfThenElseExp() {
		IfThenElseExpImpl ifThenElseExp = new IfThenElseExpImpl();
		return ifThenElseExp;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public ApplyExp createApplyExp() {
		ApplyExpImpl applyExp = new ApplyExpImpl();
		return applyExp;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public PlusExp createPlusExp() {
		PlusExpImpl plusExp = new PlusExpImpl();
		return plusExp;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public MinusExp createMinusExp() {
		MinusExpImpl minusExp = new MinusExpImpl();
		return minusExp;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EqualExp createEqualExp() {
		EqualExpImpl equalExp = new EqualExpImpl();
		return equalExp;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public FlPackage getFlPackage() {
		return (FlPackage)getEPackage();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @deprecated
	 * @generated
	 */
	@Deprecated
	public static FlPackage getPackage() {
		return FlPackage.eINSTANCE;
	}

} //FlFactoryImpl
