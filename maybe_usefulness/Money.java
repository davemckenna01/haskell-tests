import java.util.HashMap;
import java.util.Map;

public class Money {
    public static void main(String[] args) {

        Map<String, Integer> map = new HashMap<String, Integer>();

        map.put("dave", 10);
        map.put("gavin", 11);

        // {
        //     "dave": 10,
        //     "gavin": 11
        // }

        Integer davesMoney = map.get("dave");           // 10
        Integer gavinsMoney = map.get("gavin");         // 11
        Integer zorbulonsMoney = map.get("zorbulon");   // null

        Integer total = davesMoney + gavinsMoney + zorbulonsMoney;

        // EXCEPTION!
        // Exception in thread "main" java.lang.NullPointerException

        System.out.println(total);

        depositIntoBankAccount(total) // you can't your program crashed
    }
}