package noria;

import gnu.trove.*;
import io.lacuna.bifurcan.IEntry;
import io.lacuna.bifurcan.IntMap;

import java.util.PriorityQueue;
import java.util.Set;
import java.util.function.Function;

public class NoriaRT {

  public static final TObjectLongHashMap<Object> EMPTY_FLASHBACKS = new TObjectLongHashMap<>();
  public static final IntMap<Void> EMPTY_INT_SET = new IntMap<>();
  public static final DAG EMPTY_DAG = new DAG(new IntMap<>(), new IntMap<>(), 0, 1);

  public static class Calc {
    public final Object state;
    public final Object arg;
    public final Object reconciler;
    public final TLongHashSet deps;
    public final long parentId;
    public final TObjectLongHashMap<Object> childrenByKeys;
    public final TLongLongHashMap childrenOrder;

    public Calc(Object state,
                Object arg,
                Object reconciler,
                TLongHashSet deps,
                long parentId,
                TObjectLongHashMap<Object> childrenByKeys,
                TLongLongHashMap childrenOrder) {
      this.state = state;
      this.arg = arg;
      this.reconciler = reconciler;
      this.deps = deps;
      this.parentId = parentId;
      this.childrenByKeys = childrenByKeys;
      this.childrenOrder = childrenOrder;
    }
  }

  public static class Frame {
    public final TObjectLongHashMap<Object> flashbacks;
    public final TLongHashSet deps;
    public final TObjectLongHashMap<Object> childrenByKeys;
    public final TLongLongHashMap childrenOrder;
    public final long id;

    public Frame(TObjectLongHashMap<Object> flashbacks, long id) {
      this.flashbacks = flashbacks;
      this.id = id;
      deps = new TLongHashSet();
      childrenByKeys = new TObjectLongHashMap<>();
      childrenOrder = new TLongLongHashMap();
    }
  }

  public static class DAG {
    public final IntMap<Calc> values;
    public final IntMap<IntMap<Void>> dependants;
    public final long root;
    public final long nextId;


    public DAG(IntMap<Calc> values, IntMap<IntMap<Void>> dependants, long root, long nextId) {
      this.values = values;
      this.dependants = dependants;
      this.root = root;
      this.nextId = nextId;
    }
  }

  public static final Object ROOT_KEY = new Object();

  public static class Context {
    public final Function<Object, Reconciler> middleware;
    public final TLongHashSet dirtySet;
    public final TLongHashSet triggers;
    public final TLongHashSet newTriggers;
    public final TLongHashSet upToDate;
    public Frame frame;

    public IntMap<Calc> values;
    public IntMap<IntMap<Void>> dependants;
    public long root;
    public long nextId;

    public Context(DAG graph, TLongHashSet dirtySet, Function<Object, Reconciler> middleware) {
      this.middleware = middleware;
      this.dirtySet = dirtySet;
      triggers = new TLongHashSet();
      newTriggers = new TLongHashSet();
      upToDate = new TLongHashSet();
      TObjectLongHashMap<Object> flashbacks = new TObjectLongHashMap<>();
      flashbacks.put(ROOT_KEY, graph.root);
      frame = new Frame(flashbacks, -1);
      values = graph.values.linear();
      dependants = graph.dependants.linear();
      root = graph.root;
      nextId = graph.nextId;
    }
  }

  public static boolean intersects(TLongHashSet s1, TLongHashSet s2) {
    if (s1.size() <= s2.size()) {
      TLongIterator it = s1.iterator();
      while (it.hasNext()) {
        if (s2.contains(it.next())) {
          return true;
        }
      }
      return false;
    }
    else {
      return intersects(s2, s1);
    }
  }

  public static void appendChild(Context ctx, Object key, long id) {
    ctx.frame.childrenOrder.put(id, ctx.frame.childrenOrder.size());
    ctx.frame.childrenByKeys.put(key, id);
  }

  public static TLongHashSet difference(TLongHashSet s1, TLongHashSet s2) {
    TLongIterator it = s1.iterator();
    TLongHashSet res = new TLongHashSet();
    while (it.hasNext()) {
      long x = it.next();
      if (!s2.contains(x)) {
        res.add(x);
      }
    }
    return res;
  }

  public static void extinct(Context ctx, long id) {
    Calc calc = ctx.values.get(id, null);
    calc.deps.forEach(dependency -> {
      IntMap<Void> d = ctx.dependants.get(dependency, null).remove(id);
      if (d.size() == 0) {
        ctx.dependants = ctx.dependants.remove(dependency);
      } else {
        ctx.dependants = ctx.dependants.put(dependency, d);
      }
      return true;
    });
    ctx.values = ctx.values.remove(id);
    ctx.middleware.apply(calc.reconciler).destroy(calc.state);
    calc.childrenOrder.forEachKey(child -> {
      extinct(ctx, child);
      return true;
    });
  }


  public static void gc(Context ctx, TLongLongHashMap oldChildren, TLongLongHashMap newChildren) {
    oldChildren.forEachKey(oldChild -> {
      if (!newChildren.contains(oldChild)) {
        extinct(ctx, oldChild);
      }
      return true;
    });
  }

  public static void reconcileById(Context ctx, long id, Object reconciler, Object arg) {
    if (ctx.upToDate.contains(id)) {
      return;
    }
    Calc calc = ctx.values.get(id, null);
    Reconciler reconcilerImpl = ctx.middleware.apply(reconciler);
    if (calc == null ||
        ctx.dirtySet.contains(id) ||
        intersects(ctx.triggers, calc.deps) ||
        reconciler != calc.reconciler ||
        !reconcilerImpl.needsReconcile(calc.state, arg)) {
      Frame currentFrame = ctx.frame;
      Frame newFrame = new Frame(calc == null ? EMPTY_FLASHBACKS : calc.childrenByKeys, id);
      ctx.frame = newFrame;
      Propagation p = reconcilerImpl.reconcile(ctx, calc == null ? null : calc.state, arg);
      ctx.upToDate.add(id);
      if (calc != null && p.propagate) {
        ctx.triggers.add(id);
        ctx.newTriggers.add(id);
      }

      if (calc != null) {
        gc(ctx, calc.childrenOrder, newFrame.childrenOrder);
      }
      Calc newCalc = new Calc(p.state,
                              arg,
                              reconciler,
                              newFrame.deps,
                              currentFrame.id,
                              newFrame.childrenByKeys,
                              newFrame.childrenOrder);
      ctx.values = ctx.values.put(id, newCalc);
      TLongHashSet addedDeps = calc == null ? newCalc.deps : difference(newCalc.deps, calc.deps);
      addedDeps.forEach(dep -> {
        IntMap<Void> d = ctx.dependants.get(dep, EMPTY_INT_SET);
        ctx.dependants = ctx.dependants.put(dep, d.put(id, null));
        return true;
      });
      if (calc != null) {
        TLongHashSet removedDeps = difference(calc.deps, newCalc.deps);
        removedDeps.forEach(dep -> {
          IntMap<Void> d = ctx.dependants.get(dep, null).remove(id);
          if (d.size() == 0) {
            ctx.dependants = ctx.dependants.remove(dep);
          } else {
            ctx.dependants = ctx.dependants.put(dep, d);
          }
          return true;
        });
      }
      ctx.frame = currentFrame;
    }
    else {
      ctx.upToDate.add(id);
    }
  }

  public static TLongArrayList resolvePath(DAG graph, long id, long theId) {
    long pid = graph.values.get(id, null).parentId;
    if (pid == -1) {
      TLongArrayList res = new TLongArrayList();
      res.add(theId);
      return res;
    }
    else {
      Calc pc = graph.values.get(pid, null);
      long idIdx = pc.childrenOrder.get(id);
      TLongArrayList res = resolvePath(graph, pid, theId);
      res.add(idIdx);
      return res;
    }
  }

  private static int comparePaths(long[] p1, long[] p2) {
    for (int i = 1; i < p1.length && i < p2.length; ++i) {
      if (p1[i] != p2[i]) {
        return Long.compare(p1[i], p2[i]);
      }
    }
    return Integer.compare(p1.length, p2.length);
  }

  ////////////////////////////////        API       /////////////////////////////////////////////

  public static class Propagation {
    public final Object state;
    public final boolean propagate;

    Propagation(Object state, boolean propagate) {
      this.state = state;
      this.propagate = propagate;
    }
  }

  public interface Reconciler {
    boolean needsReconcile(Object state, Object newArg);
    Propagation reconcile(NoriaRT.Context context, Object state, Object arg);
    void destroy(Object state);
  }

  public static class Result {
    public final DAG graph;
    public final Object rootState;

    public Result(DAG graph, Object rootState) {
      this.graph = graph;
      this.rootState = rootState;
    }
  }

  @SuppressWarnings("unused")
  public static Object read(Context ctx, long id) {
    ctx.frame.deps.add(id);
    Calc calc = ctx.values.get(id, null);
    return calc != null ? calc.state : null;
  }

  @SuppressWarnings("UnusedReturnValue")
  public static long reconcile(Context ctx, Object reconciler, Object key, Object arg) {
    assert !ctx.frame.childrenByKeys.containsKey(key) : "key " + key + " is not unique";
    long id =  ctx.frame.flashbacks.containsKey(key) ?
               ctx.frame.flashbacks.get(key) : ctx.nextId++;
    appendChild(ctx, key, id);
    reconcileById(ctx, id, reconciler, arg);
    return id;
  }

  @SuppressWarnings("unused")
  public static Result evaluate(Object reconciler, Object arg, Function<Object, Reconciler> middleware) {
    Context context = new Context(EMPTY_DAG, new TLongHashSet(), middleware);
    reconcile(context, reconciler, ROOT_KEY, arg);
    return new Result(new DAG(context.values.forked(),
                              context.dependants.forked(),
                              context.root,
                              context.nextId),
                      context.values.get(context.root, null).state);
  }

  public static void destroyGraph(DAG graph, Function<Object, Reconciler> middleware) {
    extinct(new Context(graph, new TLongHashSet(), middleware), graph.root);
  }

  @SuppressWarnings("unused")
  public static Result evaluate(DAG graph, Set<Long> dirtySet, Function<Object, Reconciler> middleware) {
    TLongHashSet dirty = new TLongHashSet();
    for (Long id : dirtySet) {
      dirty.add(id);
    }
    Context ctx = new Context(graph, dirty, middleware);
    PriorityQueue<long[]> heap = new PriorityQueue<>(NoriaRT::comparePaths);
    dirty.forEach((id) -> {
      if (graph.values.contains(id)) {
        long[] path = resolvePath(graph, id, id).toNativeArray();
        heap.add(path);
      }
      return true;
    });
    while (!heap.isEmpty()) {
      long id = heap.poll()[0];
      Calc calc = ctx.values.get(id, null);
      if (calc != null) {
        ctx.frame = new Frame(EMPTY_FLASHBACKS, calc.parentId);
        reconcileById(ctx, id, calc.reconciler, calc.arg);
        ctx.newTriggers.forEach((trigger) -> {
          IntMap<Void> ds = graph.dependants.get(trigger, null);
          if (ds != null) {
            for (IEntry<Long, Void> entry : ds) {
              Long dependant = entry.key();
              if (!ctx.upToDate.contains(dependant)) {
                long[] path = resolvePath(graph, dependant, dependant).toNativeArray();
                heap.add(path);
              }
            }
          }
          return true;
        });
        ctx.newTriggers.clear();
      }
    }
    return new Result(new DAG(ctx.values.forked(), ctx.dependants.forked(), ctx.root, ctx.nextId),
                      ctx.values.get(ctx.root, null).state);
  }
}
