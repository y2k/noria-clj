package noria;

import java.util.*;

@SuppressWarnings({"unused", "AssignmentToForLoopParameter", "IfStatementWithIdenticalBranches"})
public class LCS {

    public static long[] lcs(long[] a, long[] b) {
        int[][] lengths = new int[a.length+1][b.length+1];
 
        // row 0 and column 0 are initialized to 0 already
        int len = 0;
        for (int i = 0; i < a.length; i++)
            for (int j = 0; j < b.length; j++)
                if (a[i] == b[j])
                    lengths[i+1][j+1] = len = lengths[i][j] + 1;
                else
                    lengths[i+1][j+1] = len = Math.max(lengths[i+1][j], lengths[i][j+1]);
 
        // read the substring out from the matrix
        long[] result = new long[len];
        for (int x = a.length, y = b.length, i = 0;
             x != 0 && y != 0;) {
            if (lengths[x][y] == lengths[x-1][y])
                x--;
            else if (lengths[x][y] == lengths[x][y-1])
                y--;
            else {
                result[i++] = a[x-1];
                x--;
                y--;
            }
        }
        return result;
    }

    public static Object[] lcsObjects(List<Object> a, List<Object> b) {
        int[][] lengths = new int[a.size()+1][b.size()+1];

        // row 0 and column 0 are initialized to 0 already
        int len = 0;
        for (int i = 0; i < a.size(); i++)
            for (int j = 0; j < b.size(); j++)
                if (a.get(i) == b.get(j))
                    lengths[i+1][j+1] = len = lengths[i][j] + 1;
                else
                    lengths[i+1][j+1] = len = Math.max(lengths[i+1][j], lengths[i][j+1]);

        // read the substring out from the matrix
        Object[] result = new Object[len];
        for (int x = a.size(), y = b.size(), i = 0;
             x != 0 && y != 0;) {
            if (lengths[x][y] == lengths[x-1][y])
                x--;
            else if (lengths[x][y] == lengths[x][y-1])
                y--;
            else {
                result[i++] = a.get(x-1);
                x--;
                y--;
            }
        }
        return result;
    }
}
