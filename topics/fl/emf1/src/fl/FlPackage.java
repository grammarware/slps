/**
 * <copyright>
 * </copyright>
 *
 * $Id$
 */
package fl;

import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.EReference;

/**
 * <!-- begin-user-doc -->
 * The <b>Package</b> for the model.
 * It contains accessors for the meta objects to represent
 * <ul>
 *   <li>each class,</li>
 *   <li>each feature of each class,</li>
 *   <li>each enum,</li>
 *   <li>and each data type</li>
 * </ul>
 * <!-- end-user-doc -->
 * @see fl.FlFactory
 * @model kind="package"
 * @generated
 */
public interface FlPackage extends EPackage {
	/**
	 * The package name.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	String eNAME = "fl";

	/**
	 * The package namespace URI.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	String eNS_URI = "http://hub.metrik.lang.fl/1.0";

	/**
	 * The package namespace name.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	String eNS_PREFIX = "hub.metrik.lang.fl";

	/**
	 * The singleton instance of the package.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	FlPackage eINSTANCE = fl.impl.FlPackageImpl.init();

	/**
	 * The meta object id for the '{@link fl.impl.ProgramImpl <em>Program</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see fl.impl.ProgramImpl
	 * @see fl.impl.FlPackageImpl#getProgram()
	 * @generated
	 */
	int PROGRAM = 0;

	/**
	 * The feature id for the '<em><b>Function</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PROGRAM__FUNCTION = 0;

	/**
	 * The number of structural features of the '<em>Program</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PROGRAM_FEATURE_COUNT = 1;

	/**
	 * The meta object id for the '{@link fl.impl.FunctionImpl <em>Function</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see fl.impl.FunctionImpl
	 * @see fl.impl.FlPackageImpl#getFunction()
	 * @generated
	 */
	int FUNCTION = 1;

	/**
	 * The feature id for the '<em><b>Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int FUNCTION__NAME = 0;

	/**
	 * The feature id for the '<em><b>Argument</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int FUNCTION__ARGUMENT = 1;

	/**
	 * The feature id for the '<em><b>Definition</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int FUNCTION__DEFINITION = 2;

	/**
	 * The number of structural features of the '<em>Function</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int FUNCTION_FEATURE_COUNT = 3;

	/**
	 * The meta object id for the '{@link fl.impl.ArgumentImpl <em>Argument</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see fl.impl.ArgumentImpl
	 * @see fl.impl.FlPackageImpl#getArgument()
	 * @generated
	 */
	int ARGUMENT = 2;

	/**
	 * The feature id for the '<em><b>Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ARGUMENT__NAME = 0;

	/**
	 * The number of structural features of the '<em>Argument</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ARGUMENT_FEATURE_COUNT = 1;

	/**
	 * The meta object id for the '{@link fl.impl.ExpImpl <em>Exp</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see fl.impl.ExpImpl
	 * @see fl.impl.FlPackageImpl#getExp()
	 * @generated
	 */
	int EXP = 3;

	/**
	 * The number of structural features of the '<em>Exp</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EXP_FEATURE_COUNT = 0;

	/**
	 * The meta object id for the '{@link fl.impl.LiteralExpImpl <em>Literal Exp</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see fl.impl.LiteralExpImpl
	 * @see fl.impl.FlPackageImpl#getLiteralExp()
	 * @generated
	 */
	int LITERAL_EXP = 4;

	/**
	 * The feature id for the '<em><b>Value</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int LITERAL_EXP__VALUE = EXP_FEATURE_COUNT + 0;

	/**
	 * The number of structural features of the '<em>Literal Exp</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int LITERAL_EXP_FEATURE_COUNT = EXP_FEATURE_COUNT + 1;

	/**
	 * The meta object id for the '{@link fl.impl.ArgumentExpImpl <em>Argument Exp</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see fl.impl.ArgumentExpImpl
	 * @see fl.impl.FlPackageImpl#getArgumentExp()
	 * @generated
	 */
	int ARGUMENT_EXP = 5;

	/**
	 * The feature id for the '<em><b>Argument</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ARGUMENT_EXP__ARGUMENT = EXP_FEATURE_COUNT + 0;

	/**
	 * The number of structural features of the '<em>Argument Exp</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ARGUMENT_EXP_FEATURE_COUNT = EXP_FEATURE_COUNT + 1;

	/**
	 * The meta object id for the '{@link fl.impl.IfThenElseExpImpl <em>If Then Else Exp</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see fl.impl.IfThenElseExpImpl
	 * @see fl.impl.FlPackageImpl#getIfThenElseExp()
	 * @generated
	 */
	int IF_THEN_ELSE_EXP = 6;

	/**
	 * The feature id for the '<em><b>If</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int IF_THEN_ELSE_EXP__IF = EXP_FEATURE_COUNT + 0;

	/**
	 * The feature id for the '<em><b>Then</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int IF_THEN_ELSE_EXP__THEN = EXP_FEATURE_COUNT + 1;

	/**
	 * The feature id for the '<em><b>Else</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int IF_THEN_ELSE_EXP__ELSE = EXP_FEATURE_COUNT + 2;

	/**
	 * The number of structural features of the '<em>If Then Else Exp</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int IF_THEN_ELSE_EXP_FEATURE_COUNT = EXP_FEATURE_COUNT + 3;

	/**
	 * The meta object id for the '{@link fl.impl.ApplyExpImpl <em>Apply Exp</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see fl.impl.ApplyExpImpl
	 * @see fl.impl.FlPackageImpl#getApplyExp()
	 * @generated
	 */
	int APPLY_EXP = 7;

	/**
	 * The feature id for the '<em><b>Function</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int APPLY_EXP__FUNCTION = EXP_FEATURE_COUNT + 0;

	/**
	 * The feature id for the '<em><b>Argument</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int APPLY_EXP__ARGUMENT = EXP_FEATURE_COUNT + 1;

	/**
	 * The number of structural features of the '<em>Apply Exp</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int APPLY_EXP_FEATURE_COUNT = EXP_FEATURE_COUNT + 2;

	/**
	 * The meta object id for the '{@link fl.impl.BinaryExpImpl <em>Binary Exp</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see fl.impl.BinaryExpImpl
	 * @see fl.impl.FlPackageImpl#getBinaryExp()
	 * @generated
	 */
	int BINARY_EXP = 8;

	/**
	 * The feature id for the '<em><b>Left</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int BINARY_EXP__LEFT = EXP_FEATURE_COUNT + 0;

	/**
	 * The feature id for the '<em><b>Right</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int BINARY_EXP__RIGHT = EXP_FEATURE_COUNT + 1;

	/**
	 * The number of structural features of the '<em>Binary Exp</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int BINARY_EXP_FEATURE_COUNT = EXP_FEATURE_COUNT + 2;

	/**
	 * The meta object id for the '{@link fl.impl.PlusExpImpl <em>Plus Exp</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see fl.impl.PlusExpImpl
	 * @see fl.impl.FlPackageImpl#getPlusExp()
	 * @generated
	 */
	int PLUS_EXP = 9;

	/**
	 * The feature id for the '<em><b>Left</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PLUS_EXP__LEFT = BINARY_EXP__LEFT;

	/**
	 * The feature id for the '<em><b>Right</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PLUS_EXP__RIGHT = BINARY_EXP__RIGHT;

	/**
	 * The number of structural features of the '<em>Plus Exp</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PLUS_EXP_FEATURE_COUNT = BINARY_EXP_FEATURE_COUNT + 0;

	/**
	 * The meta object id for the '{@link fl.impl.MinusExpImpl <em>Minus Exp</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see fl.impl.MinusExpImpl
	 * @see fl.impl.FlPackageImpl#getMinusExp()
	 * @generated
	 */
	int MINUS_EXP = 10;

	/**
	 * The feature id for the '<em><b>Left</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int MINUS_EXP__LEFT = BINARY_EXP__LEFT;

	/**
	 * The feature id for the '<em><b>Right</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int MINUS_EXP__RIGHT = BINARY_EXP__RIGHT;

	/**
	 * The number of structural features of the '<em>Minus Exp</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int MINUS_EXP_FEATURE_COUNT = BINARY_EXP_FEATURE_COUNT + 0;

	/**
	 * The meta object id for the '{@link fl.impl.EqualExpImpl <em>Equal Exp</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see fl.impl.EqualExpImpl
	 * @see fl.impl.FlPackageImpl#getEqualExp()
	 * @generated
	 */
	int EQUAL_EXP = 11;

	/**
	 * The feature id for the '<em><b>Left</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EQUAL_EXP__LEFT = BINARY_EXP__LEFT;

	/**
	 * The feature id for the '<em><b>Right</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EQUAL_EXP__RIGHT = BINARY_EXP__RIGHT;

	/**
	 * The number of structural features of the '<em>Equal Exp</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EQUAL_EXP_FEATURE_COUNT = BINARY_EXP_FEATURE_COUNT + 0;


	/**
	 * Returns the meta object for class '{@link fl.Program <em>Program</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Program</em>'.
	 * @see fl.Program
	 * @generated
	 */
	EClass getProgram();

	/**
	 * Returns the meta object for the containment reference list '{@link fl.Program#getFunction <em>Function</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the containment reference list '<em>Function</em>'.
	 * @see fl.Program#getFunction()
	 * @see #getProgram()
	 * @generated
	 */
	EReference getProgram_Function();

	/**
	 * Returns the meta object for class '{@link fl.Function <em>Function</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Function</em>'.
	 * @see fl.Function
	 * @generated
	 */
	EClass getFunction();

	/**
	 * Returns the meta object for the attribute '{@link fl.Function#getName <em>Name</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Name</em>'.
	 * @see fl.Function#getName()
	 * @see #getFunction()
	 * @generated
	 */
	EAttribute getFunction_Name();

	/**
	 * Returns the meta object for the containment reference list '{@link fl.Function#getArgument <em>Argument</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the containment reference list '<em>Argument</em>'.
	 * @see fl.Function#getArgument()
	 * @see #getFunction()
	 * @generated
	 */
	EReference getFunction_Argument();

	/**
	 * Returns the meta object for the containment reference '{@link fl.Function#getDefinition <em>Definition</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the containment reference '<em>Definition</em>'.
	 * @see fl.Function#getDefinition()
	 * @see #getFunction()
	 * @generated
	 */
	EReference getFunction_Definition();

	/**
	 * Returns the meta object for class '{@link fl.Argument <em>Argument</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Argument</em>'.
	 * @see fl.Argument
	 * @generated
	 */
	EClass getArgument();

	/**
	 * Returns the meta object for the attribute '{@link fl.Argument#getName <em>Name</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Name</em>'.
	 * @see fl.Argument#getName()
	 * @see #getArgument()
	 * @generated
	 */
	EAttribute getArgument_Name();

	/**
	 * Returns the meta object for class '{@link fl.Exp <em>Exp</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Exp</em>'.
	 * @see fl.Exp
	 * @generated
	 */
	EClass getExp();

	/**
	 * Returns the meta object for class '{@link fl.LiteralExp <em>Literal Exp</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Literal Exp</em>'.
	 * @see fl.LiteralExp
	 * @generated
	 */
	EClass getLiteralExp();

	/**
	 * Returns the meta object for the attribute '{@link fl.LiteralExp#getValue <em>Value</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Value</em>'.
	 * @see fl.LiteralExp#getValue()
	 * @see #getLiteralExp()
	 * @generated
	 */
	EAttribute getLiteralExp_Value();

	/**
	 * Returns the meta object for class '{@link fl.ArgumentExp <em>Argument Exp</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Argument Exp</em>'.
	 * @see fl.ArgumentExp
	 * @generated
	 */
	EClass getArgumentExp();

	/**
	 * Returns the meta object for the reference '{@link fl.ArgumentExp#getArgument <em>Argument</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Argument</em>'.
	 * @see fl.ArgumentExp#getArgument()
	 * @see #getArgumentExp()
	 * @generated
	 */
	EReference getArgumentExp_Argument();

	/**
	 * Returns the meta object for class '{@link fl.IfThenElseExp <em>If Then Else Exp</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>If Then Else Exp</em>'.
	 * @see fl.IfThenElseExp
	 * @generated
	 */
	EClass getIfThenElseExp();

	/**
	 * Returns the meta object for the containment reference '{@link fl.IfThenElseExp#getIf <em>If</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the containment reference '<em>If</em>'.
	 * @see fl.IfThenElseExp#getIf()
	 * @see #getIfThenElseExp()
	 * @generated
	 */
	EReference getIfThenElseExp_If();

	/**
	 * Returns the meta object for the containment reference '{@link fl.IfThenElseExp#getThen <em>Then</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the containment reference '<em>Then</em>'.
	 * @see fl.IfThenElseExp#getThen()
	 * @see #getIfThenElseExp()
	 * @generated
	 */
	EReference getIfThenElseExp_Then();

	/**
	 * Returns the meta object for the containment reference '{@link fl.IfThenElseExp#getElse <em>Else</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the containment reference '<em>Else</em>'.
	 * @see fl.IfThenElseExp#getElse()
	 * @see #getIfThenElseExp()
	 * @generated
	 */
	EReference getIfThenElseExp_Else();

	/**
	 * Returns the meta object for class '{@link fl.ApplyExp <em>Apply Exp</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Apply Exp</em>'.
	 * @see fl.ApplyExp
	 * @generated
	 */
	EClass getApplyExp();

	/**
	 * Returns the meta object for the reference '{@link fl.ApplyExp#getFunction <em>Function</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Function</em>'.
	 * @see fl.ApplyExp#getFunction()
	 * @see #getApplyExp()
	 * @generated
	 */
	EReference getApplyExp_Function();

	/**
	 * Returns the meta object for the containment reference list '{@link fl.ApplyExp#getArgument <em>Argument</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the containment reference list '<em>Argument</em>'.
	 * @see fl.ApplyExp#getArgument()
	 * @see #getApplyExp()
	 * @generated
	 */
	EReference getApplyExp_Argument();

	/**
	 * Returns the meta object for class '{@link fl.BinaryExp <em>Binary Exp</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Binary Exp</em>'.
	 * @see fl.BinaryExp
	 * @generated
	 */
	EClass getBinaryExp();

	/**
	 * Returns the meta object for the containment reference '{@link fl.BinaryExp#getLeft <em>Left</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the containment reference '<em>Left</em>'.
	 * @see fl.BinaryExp#getLeft()
	 * @see #getBinaryExp()
	 * @generated
	 */
	EReference getBinaryExp_Left();

	/**
	 * Returns the meta object for the containment reference '{@link fl.BinaryExp#getRight <em>Right</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the containment reference '<em>Right</em>'.
	 * @see fl.BinaryExp#getRight()
	 * @see #getBinaryExp()
	 * @generated
	 */
	EReference getBinaryExp_Right();

	/**
	 * Returns the meta object for class '{@link fl.PlusExp <em>Plus Exp</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Plus Exp</em>'.
	 * @see fl.PlusExp
	 * @generated
	 */
	EClass getPlusExp();

	/**
	 * Returns the meta object for class '{@link fl.MinusExp <em>Minus Exp</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Minus Exp</em>'.
	 * @see fl.MinusExp
	 * @generated
	 */
	EClass getMinusExp();

	/**
	 * Returns the meta object for class '{@link fl.EqualExp <em>Equal Exp</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Equal Exp</em>'.
	 * @see fl.EqualExp
	 * @generated
	 */
	EClass getEqualExp();

	/**
	 * Returns the factory that creates the instances of the model.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the factory that creates the instances of the model.
	 * @generated
	 */
	FlFactory getFlFactory();

	/**
	 * <!-- begin-user-doc -->
	 * Defines literals for the meta objects that represent
	 * <ul>
	 *   <li>each class,</li>
	 *   <li>each feature of each class,</li>
	 *   <li>each enum,</li>
	 *   <li>and each data type</li>
	 * </ul>
	 * <!-- end-user-doc -->
	 * @generated
	 */
	interface Literals {
		/**
		 * The meta object literal for the '{@link fl.impl.ProgramImpl <em>Program</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see fl.impl.ProgramImpl
		 * @see fl.impl.FlPackageImpl#getProgram()
		 * @generated
		 */
		EClass PROGRAM = eINSTANCE.getProgram();

		/**
		 * The meta object literal for the '<em><b>Function</b></em>' containment reference list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference PROGRAM__FUNCTION = eINSTANCE.getProgram_Function();

		/**
		 * The meta object literal for the '{@link fl.impl.FunctionImpl <em>Function</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see fl.impl.FunctionImpl
		 * @see fl.impl.FlPackageImpl#getFunction()
		 * @generated
		 */
		EClass FUNCTION = eINSTANCE.getFunction();

		/**
		 * The meta object literal for the '<em><b>Name</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute FUNCTION__NAME = eINSTANCE.getFunction_Name();

		/**
		 * The meta object literal for the '<em><b>Argument</b></em>' containment reference list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference FUNCTION__ARGUMENT = eINSTANCE.getFunction_Argument();

		/**
		 * The meta object literal for the '<em><b>Definition</b></em>' containment reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference FUNCTION__DEFINITION = eINSTANCE.getFunction_Definition();

		/**
		 * The meta object literal for the '{@link fl.impl.ArgumentImpl <em>Argument</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see fl.impl.ArgumentImpl
		 * @see fl.impl.FlPackageImpl#getArgument()
		 * @generated
		 */
		EClass ARGUMENT = eINSTANCE.getArgument();

		/**
		 * The meta object literal for the '<em><b>Name</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute ARGUMENT__NAME = eINSTANCE.getArgument_Name();

		/**
		 * The meta object literal for the '{@link fl.impl.ExpImpl <em>Exp</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see fl.impl.ExpImpl
		 * @see fl.impl.FlPackageImpl#getExp()
		 * @generated
		 */
		EClass EXP = eINSTANCE.getExp();

		/**
		 * The meta object literal for the '{@link fl.impl.LiteralExpImpl <em>Literal Exp</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see fl.impl.LiteralExpImpl
		 * @see fl.impl.FlPackageImpl#getLiteralExp()
		 * @generated
		 */
		EClass LITERAL_EXP = eINSTANCE.getLiteralExp();

		/**
		 * The meta object literal for the '<em><b>Value</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute LITERAL_EXP__VALUE = eINSTANCE.getLiteralExp_Value();

		/**
		 * The meta object literal for the '{@link fl.impl.ArgumentExpImpl <em>Argument Exp</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see fl.impl.ArgumentExpImpl
		 * @see fl.impl.FlPackageImpl#getArgumentExp()
		 * @generated
		 */
		EClass ARGUMENT_EXP = eINSTANCE.getArgumentExp();

		/**
		 * The meta object literal for the '<em><b>Argument</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference ARGUMENT_EXP__ARGUMENT = eINSTANCE.getArgumentExp_Argument();

		/**
		 * The meta object literal for the '{@link fl.impl.IfThenElseExpImpl <em>If Then Else Exp</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see fl.impl.IfThenElseExpImpl
		 * @see fl.impl.FlPackageImpl#getIfThenElseExp()
		 * @generated
		 */
		EClass IF_THEN_ELSE_EXP = eINSTANCE.getIfThenElseExp();

		/**
		 * The meta object literal for the '<em><b>If</b></em>' containment reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference IF_THEN_ELSE_EXP__IF = eINSTANCE.getIfThenElseExp_If();

		/**
		 * The meta object literal for the '<em><b>Then</b></em>' containment reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference IF_THEN_ELSE_EXP__THEN = eINSTANCE.getIfThenElseExp_Then();

		/**
		 * The meta object literal for the '<em><b>Else</b></em>' containment reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference IF_THEN_ELSE_EXP__ELSE = eINSTANCE.getIfThenElseExp_Else();

		/**
		 * The meta object literal for the '{@link fl.impl.ApplyExpImpl <em>Apply Exp</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see fl.impl.ApplyExpImpl
		 * @see fl.impl.FlPackageImpl#getApplyExp()
		 * @generated
		 */
		EClass APPLY_EXP = eINSTANCE.getApplyExp();

		/**
		 * The meta object literal for the '<em><b>Function</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference APPLY_EXP__FUNCTION = eINSTANCE.getApplyExp_Function();

		/**
		 * The meta object literal for the '<em><b>Argument</b></em>' containment reference list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference APPLY_EXP__ARGUMENT = eINSTANCE.getApplyExp_Argument();

		/**
		 * The meta object literal for the '{@link fl.impl.BinaryExpImpl <em>Binary Exp</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see fl.impl.BinaryExpImpl
		 * @see fl.impl.FlPackageImpl#getBinaryExp()
		 * @generated
		 */
		EClass BINARY_EXP = eINSTANCE.getBinaryExp();

		/**
		 * The meta object literal for the '<em><b>Left</b></em>' containment reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference BINARY_EXP__LEFT = eINSTANCE.getBinaryExp_Left();

		/**
		 * The meta object literal for the '<em><b>Right</b></em>' containment reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference BINARY_EXP__RIGHT = eINSTANCE.getBinaryExp_Right();

		/**
		 * The meta object literal for the '{@link fl.impl.PlusExpImpl <em>Plus Exp</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see fl.impl.PlusExpImpl
		 * @see fl.impl.FlPackageImpl#getPlusExp()
		 * @generated
		 */
		EClass PLUS_EXP = eINSTANCE.getPlusExp();

		/**
		 * The meta object literal for the '{@link fl.impl.MinusExpImpl <em>Minus Exp</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see fl.impl.MinusExpImpl
		 * @see fl.impl.FlPackageImpl#getMinusExp()
		 * @generated
		 */
		EClass MINUS_EXP = eINSTANCE.getMinusExp();

		/**
		 * The meta object literal for the '{@link fl.impl.EqualExpImpl <em>Equal Exp</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see fl.impl.EqualExpImpl
		 * @see fl.impl.FlPackageImpl#getEqualExp()
		 * @generated
		 */
		EClass EQUAL_EXP = eINSTANCE.getEqualExp();

	}

} //FlPackage
