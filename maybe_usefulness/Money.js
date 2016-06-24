var money = {
    'dave': 10,
    'gavin': 11
};

var davesMoney = money['dave'];         // 10
var gavinsMoney = money['gavin'];       // 11
var zorbulonsMoney = money['zorbulon']; // undefined

var total = davesMoney + gavinsMoney + zorbulonsMoney;

console.log(total);
// => NaN

depositIntoBankAccount(total) // you just deposited NaN, weirdo
