package noria;

import java.util.*;

public class LCS {

    public static List<Object> lcs(Object[] a, Object b[]) {
        int[][] lengths = new int[a.length+1][b.length+1];
 
        // row 0 and column 0 are initialized to 0 already
 
        for (int i = 0; i < a.length; i++)
            for (int j = 0; j < b.length; j++)
                if (a[i].equals(b[j]))
                    lengths[i+1][j+1] = lengths[i][j] + 1;
                else
                    lengths[i+1][j+1] =
                        Math.max(lengths[i+1][j], lengths[i][j+1]);
 
        // read the substring out from the matrix
        List<Object> sb = new ArrayList<Object>();
        for (int x = a.length, y = b.length;
             x != 0 && y != 0; ) {
            if (lengths[x][y] == lengths[x-1][y])
                x--;
            else if (lengths[x][y] == lengths[x][y-1])
                y--;
            else {
                sb.add(a[x-1]);
                x--;
                y--;
            }
        }
        return sb;
    }
}

