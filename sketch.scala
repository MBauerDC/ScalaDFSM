type FSMName = String
type FSMIdentifier = String;
trait FSMState[N <: FSMName]:
  val identifier: FSMIdentifier
  type MachineName = N
trait FSMInput[N <: FSMName]:
  val identifier: FSMIdentifier
  type MachineName = N


trait FiniteStateMachine[N <: FSMName](
  val acceptedInputs: Set[FSMInput[N]],
  val states: Set[FSMState[N]],
  val initialState: FSMState[N],
  val transitionFn: (FSMState[N], FSMInput[N]) => FSMState[N],
  val rejectingStates: Set[FSMState[N]],
  val acceptingStates: Set[FSMState[N]]
):
  type MachineName = N

trait FiniteStateMachineInstance[N <: FSMName] extends FiniteStateMachine[N]:
  val currentState: FSMState[N]
  val transitionHistory: Seq[FSMState[N]]
  def isInAcceptingState: Boolean
  def isInRejectingState: Boolean
  def applyInput(input: FSMInput[N]): FiniteStateMachineInstance[N]

class GenericFiniteStateMachine[N <: FSMName]
(
  override val acceptedInputs: Set[FSMInput[N]],
  override val states: Set[FSMState[N]],
  override val initialState: FSMState[N],
  override val transitionFn: (FSMState[N], FSMInput[N]) => FSMState[N],
  override val rejectingStates: Set[FSMState[N]],
  override val acceptingStates: Set[FSMState[N]],
  val currentState: FSMState[N],
  val transitionHistory: Seq[FSMState[N]]
) extends FiniteStateMachine[N](
  acceptedInputs,
  states,
  initialState,
  transitionFn,
  rejectingStates,
  acceptingStates
) with FiniteStateMachineInstance[N]:
  def applyInput(input:FSMInput[N]) = 
    val newCurrentState = transitionFn(currentState, input)
    new GenericFiniteStateMachine(
      acceptedInputs,
      states,
      initialState,
      transitionFn,
      rejectingStates,
      acceptingStates,
      newCurrentState,
      transitionHistory :+ newCurrentState
    )
  def isInAcceptingState: Boolean = 
    acceptingStates.contains(currentState)
  def isInRejectingState: Boolean = 
    rejectingStates.contains(currentState)

object GenericFiniteStateMachine{
  def apply[N <: FSMName](
    acceptedInputs: Set[FSMInput[N]],
    states: Set[FSMState[N]],
    initialState: FSMState[N],
    transitionFn: (FSMState[N], FSMInput[N]) => FSMState[N],
    rejectingStates: Set[FSMState[N]],
    acceptingStates: Set[FSMState[N]],
  ): GenericFiniteStateMachine[N] = 
    new GenericFiniteStateMachine[N](
      acceptedInputs,
      states,
      initialState,
      transitionFn,
      rejectingStates,
      acceptingStates,
      initialState,
      Seq(initialState)
    )
}


type FSM1Name = "FSM1";
val FSM1InitialState: FSMState[FSM1Name] = new FSMState[FSM1Name]{ 
  val identifier = "FSM1Init"
}
val FSM1InputA: FSMInput[FSM1Name] = new FSMInput[FSM1Name]{
  val identifier = "FSM1InputA"
}
val FSM1InputElse: FSMInput[FSM1Name] = new FSMInput[FSM1Name]{
  val identifier = "FSM1InputElse"
}

val anyToFSM1Input: (Any) => FSMInput[FSM1Name] = 
  (data: Any) => {
    data match
      case 'a' | 'A' => FSM1InputA
      case _ => FSM1InputElse
  }

val FSM1RejectState: FSMState[FSM1Name] = new FSMState[FSM1Name]{
  val identifier = "FSM1Rejecting"
}

val FSM1AcceptState: FSMState[FSM1Name] = new FSMState[FSM1Name]{
  val identifier = "FSM1Accepting"
}
val FSM1States: Set[FSMState[FSM1Name]] = Set(FSM1InitialState, FSM1RejectState, FSM1AcceptState)
val FSM1AcceptingStates: Set[FSMState[FSM1Name]] = Set(FSM1AcceptState)
val FSM1RejectingStates: Set[FSMState[FSM1Name]] = Set(FSM1RejectState)
val FSM1AcceptedInputs: Set[FSMInput[FSM1Name]] = Set(FSM1InputA, FSM1InputElse)
val FSM1TransitionFunction = 
  (state: FSMState[FSM1Name], input: FSMInput[FSM1Name]) => {
    input match
      case FSM1InputA => if FSM1RejectingStates.contains(state) then FSM1RejectState else FSM1AcceptState
      case FSM1InputElse => FSM1RejectState
      case _ => FSM1RejectState
  }

var FSM1Instance: FiniteStateMachineInstance[FSM1Name] = GenericFiniteStateMachine[FSM1Name](
  FSM1AcceptedInputs,
  FSM1States,
  FSM1InitialState,
  FSM1TransitionFunction,
  FSM1RejectingStates,
  FSM1AcceptingStates  
)

FSM1Instance = FSM1Instance.applyInput(anyToFSM1Input('a'))
val FSM1StateAfterTransition1 = FSM1Instance.currentState
FSM1Instance = FSM1Instance.applyInput(anyToFSM1Input('b'))    
val FSM1StateAfterTransition2 = FSM1Instance.currentState
println(FSM1StateAfterTransition1.identifier)
println(FSM1StateAfterTransition2.identifier)
  