package noria

class Noria(var ctx: NoriaRT.Context?,
            val id: Long,
            val updater: (Long, ((Any?) -> Any?)?) -> Unit) {
    @Suppress("NOTHING_TO_INLINE")
    inline operator fun <State, Arg, Value> Reconciler<State, Arg, Value>.invoke(arg: Arg): Thunk<Value> {
        val key = object {}.javaClass
        return invoke(key, arg)
    }

    operator fun <State, Arg, Value> Reconciler<State, Arg, Value>.invoke(key: Any, arg: Arg): Thunk<Value> {
        val thunkId = NoriaRT.reconcile(ctx, this@invoke, key, arg)
        return ComputationThunk(thunkId)
    }

    fun <Value> Thunk<Value>.deref(): Value = this@deref.deref(this@Noria)

    fun <State> Reconciler<State, *, *>.updateState(f: ((State) -> State)? = null) {
        assert(ctx == null) { "Tried to update from reconcile" }
        updater(id, f as ((Any?) -> Any?)?)
    }

    fun update() {
        updater(id, null)
    }
}

interface Thunk<out Value> {
    fun deref(c: Noria): Value
}

internal class CompoundState(val state: Any?,
                             val valueThunk: Thunk<*>,
                             val arg: Any?)

internal class ComputationThunk<Value>(val id: Long) : Thunk<Value> {
    override fun deref(c: Noria): Value {
        val state = NoriaRT.read(c.ctx, id)
        if (state is CompoundState) {
            @Suppress("UNCHECKED_CAST")
            return state.valueThunk.deref(c) as Value
        } else {
            @Suppress("UNCHECKED_CAST")
            return state as Value
        }
    }

    override fun equals(other: Any?): Boolean =
            other is ComputationThunk<*> && other.id == id
}

internal class IdentityThunk<Value>(val value: Value) : Thunk<Value> {
    override fun deref(c: Noria): Value = value
    override fun equals(other: Any?): Boolean =
            other is IdentityThunk<*> && other.value === value
}

internal class ValueThunk<Value>(val value: Value) : Thunk<Value> {
    override fun deref(c: Noria): Value = value
    override fun equals(other: Any?): Boolean =
            other is ValueThunk<*> && other.value == value
}

fun <Value> value(v: Value): Thunk<Value> = ValueThunk(v)
fun <Value> identity(v: Value): Thunk<Value> = IdentityThunk(v)

fun <Arg, Value> fn(f: Noria.(Arg) -> Thunk<Value>): Reconciler<*, Arg, Value> = Fn(f)

internal class Fn<Arg, Value>(private val f: Noria.(Arg) -> Thunk<Value>) : Reconciler<Unit?, Arg, Value> {
    override fun needsReconcile(state: Unit?, oldArg: Arg, arg: Arg): Boolean =
            oldArg != arg

    override fun Noria.reconcile(state: Unit?, arg: Arg): Pair<Unit, Thunk<Value>> =
            Unit to f(arg)
}

class EntityState<Entity, Props>(val props: Props,
                                 val entity: Entity)

interface EntityReconciler<Entity, Props> : Reconciler<EntityState<Entity, Props>?, Props, Entity> {
    override fun Noria.reconcile(state: EntityState<Entity, Props>?, arg: Props): Pair<EntityState<Entity, Props>, Thunk<Entity>> {
        if (state == null) {
            val e = construct(arg)
            return EntityState(arg, e) to identity(e)
        } else {
            reconcile(state.entity, state.props, arg)
            return EntityState(arg, state.entity) to identity(state.entity)
        }
    }

    fun Noria.construct(props: Props): Entity
    fun Noria.reconcile(e: Entity, oldProps: Props, newProps: Props)
}

interface Reconciler<State, Arg, Value> {
    fun Noria.reconcile(state: State, arg: Arg): Pair<State, Thunk<Value>>
    fun needsReconcile(state: State, oldArg: Arg, arg: Arg): Boolean = oldArg != arg
    fun destroy(state: State) {}
}

internal class Middleware(val scheduler: (() -> Unit) -> Unit,
                          root: Reconciler<*, Unit, *>) : java.util.function.Function<Any?, NoriaRT.Reconciler> {
    val dirtySet = hashMapOf<Long, ((Any?) -> Any?)?>()
    var scheduled = false
    var graph: NoriaRT.DAG

    init {
        val result = NoriaRT.evaluate(root, Unit, this)
        graph = result.graph
    }

    override fun apply(r: Any?): NoriaRT.Reconciler {
        return object : NoriaRT.Reconciler {
            override fun destroy(state: Any?) {
                r as Reconciler<Any?, Any?, Any?>
                state as CompoundState
                r.destroy(state.state)
            }

            override fun reconcile(ctx: NoriaRT.Context, state: Any?, arg: Any?): NoriaRT.Propagation {
                r as Reconciler<Any?, Any?, Any?>
                state as CompoundState?
                with(r) {
                    val stateUpdater = dirtySet[ctx.frame.id]
                    val updatedState = if (stateUpdater != null) stateUpdater(state?.state) else state?.state
                    val context = Noria(ctx, ctx.frame.id, ::update)
                    val (privateState, valueThunk) = context.reconcile(updatedState, arg)
                    context.ctx = null // prevent graph from leaking
                    return NoriaRT.Propagation(CompoundState(privateState, valueThunk, arg), valueThunk == state?.valueThunk)
                }
            }

            override fun needsReconcile(state: Any?, arg: Any?): Boolean {
                state as CompoundState
                r as Reconciler<Any?, Any?, Any?>
                return r.needsReconcile(state.state, state.arg, arg)
            }
        }
    }

    fun <State> update(id: Long, f: ((State) -> State)?) {
        dirtySet.put(id, f as ((Any?) -> Any?)?)
        if (!scheduled) {
            scheduled = true
            scheduler {
                val result = NoriaRT.revaluate(graph, dirtySet.keys, this)
                dirtySet.clear()
                graph = result.graph
                scheduled = false
            }
        }
    }
}

fun <Value> noria(scheduler: (() -> Unit) -> Unit, root: Noria.(Unit) -> Thunk<Value>) {
    Middleware(scheduler, fn(root))
}

class Var<T>(x: T) {
    private var t = x
    private val watches = hashSetOf<(T) -> Unit>()
    fun set(v: T) {
        if (t != v) {
            t = v
            watches.forEach { it(v) }
        }
    }

    fun update(u: (T) -> T) {
        set(u(t))
    }

    fun get(): T = t
    fun watch(l: (T) -> Unit) {
        watches.add(l)
    }

    fun unwatch(l: (T) -> Unit) {
        watches.remove(l)
    }
}

class ReadState<T>(val t: T,
                   val v: Var<T>,
                   val w: (T) -> Unit)

class Read<T> : Reconciler<ReadState<T>?, Var<T>, T> {
    override fun Noria.reconcile(state: ReadState<T>?, arg: Var<T>): Pair<ReadState<T>, Thunk<T>> {
        if (state?.v != arg) {
            state?.v?.unwatch(state.w)
            val w = { x: T ->
                updateState { s ->
                    ReadState(x, s!!.v, s.w)
                }
            }
            arg.watch(w)
            return ReadState(arg.get(), arg, w) to value(arg.get())
        } else {
            return state to value(state.t)
        }
    }

    override fun needsReconcile(state: ReadState<T>?, oldArg: Var<T>, arg: Var<T>): Boolean {
        return state?.v != arg
    }

    override fun destroy(state: ReadState<T>?) {
        state!!.v.unwatch(state.w)
    }
}

fun <T> Noria.read(v: Var<T>): Thunk<T> =
        Read<T>().invoke(v)
