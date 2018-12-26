package noria;

import gnu.trove.THashSet;
import gnu.trove.TLongHashSet;
import gnu.trove.TObjectHashingStrategy;

import java.util.*;

@SuppressWarnings({"unused", "AssignmentToForLoopParameter", "IfStatementWithIdenticalBranches"})
public class LCS {

  public static long[] lcs(long[] a, long[] b) {
    int[][] lengths = new int[a.length + 1][b.length + 1];

    // row 0 and column 0 are initialized to 0 already
    int len = 0;
    for (int i = 0; i < a.length; i++) {
      for (int j = 0; j < b.length; j++) {
        if (a[i] == b[j]) {
          lengths[i + 1][j + 1] = len = lengths[i][j] + 1;
        }
        else {
          lengths[i + 1][j + 1] = len = Math.max(lengths[i + 1][j], lengths[i][j + 1]);
        }
      }
    }

    // read the substring out from the matrix
    long[] result = new long[len];
    for (int x = a.length, y = b.length, i = 0;
         x != 0 && y != 0; ) {
      if (lengths[x][y] == lengths[x - 1][y]) {
        x--;
      }
      else if (lengths[x][y] == lengths[x][y - 1]) {
        y--;
      }
      else {
        result[i++] = a[x - 1];
        x--;
        y--;
      }
    }
    return result;
  }

  public static Object[] lcsObjects(Object[] a, Object[] b) {
    int[][] lengths = new int[a.length + 1][b.length + 1];

    // row 0 and column 0 are initialized to 0 already
    int len = 0;
    for (int i = 0; i < a.length; i++) {
      for (int j = 0; j < b.length; j++) {
        if (a[i] == b[j]) {
          lengths[i + 1][j + 1] = len = lengths[i][j] + 1;
        }
        else {
          lengths[i + 1][j + 1] = len = Math.max(lengths[i + 1][j], lengths[i][j + 1]);
        }
      }
    }

    // read the substring out from the matrix
    Object[] result = new Object[len];
    for (int x = a.length, y = b.length, i = 0;
         x != 0 && y != 0; ) {
      if (lengths[x][y] == lengths[x - 1][y]) {
        x--;
      }
      else if (lengths[x][y] == lengths[x][y - 1]) {
        y--;
      }
      else {
        result[i++] = a[x - 1];
        x--;
        y--;
      }
    }
    return result;
  }

  interface OrderUpdater {
    void add(int index, Object value);

    void remove(int index);
  }

  public static void updateOrder(long[] oldList, long[] newList, OrderUpdater updater) {
    if (Arrays.equals(oldList, newList)) {
      return;
    }
    if (oldList == null) {
      for (int i = 0; i < newList.length; i++) {
        updater.add(i, newList[i]);
      }
    }
    else if (newList == null) {
      for (int i = 0; i < oldList.length; i++) {
        updater.remove(i);
      }
    }
    else {
      long[] lcs = lcs(oldList, newList);
      TLongHashSet set = new TLongHashSet(lcs);
      for (int i = 0; i < oldList.length; i++) {
        if (!set.contains(oldList[i])) {
          updater.remove(i);
        }
      }
      for (int i = 0; i < newList.length; i++) {
        long o = newList[i];
        if (!set.contains(o)) {
          updater.add(i, o);
        }
      }
    }
  }

  public static void updateOrder(Object[] oldList, Object[] newList, OrderUpdater updater) {
    if (Arrays.equals(oldList, newList)) {
      return;
    }
    if (oldList == null) {
      for (int i = 0; i < newList.length; i++) {
        updater.add(i, newList[i]);
      }
    }
    else if (newList == null) {
      for (int i = 0; i < oldList.length; i++) {
        updater.remove(i);
      }
    }
    else {
      Object[] lcs = lcsObjects(oldList, newList);
      THashSet<Object> set = new THashSet<>(TObjectHashingStrategy.IDENTITY);
      Collections.addAll(set, lcs);
      for (int i = 0; i < oldList.length; i++) {
        if (!set.contains(oldList[i])) {
          updater.remove(i);
        }
      }
      for (int i = 0; i < newList.length; i++) {
        Object o = newList[i];
        if (!set.contains(o)) {
          updater.add(i, o);
        }
      }
    }
  }
}
