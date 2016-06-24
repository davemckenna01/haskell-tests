money = {
    :dave => 10,
    :gavin => 11
}

davesMoney = money[:dave]         # 10
gavinsMoney = money[:gavin]       # 11
zorbulonsMoney = money[:zorbulon] # nil

total = davesMoney + gavinsMoney + zorbulonsMoney

# EXCEPTION!
# nil can't be coerced into Fixnum (TypeError)

puts total

depositIntoBankAccount() # you can't your program crashed