### Çözünürlük

Bu modül, tam genişlik, %50'deki genişlik ve ayırma faktörü yöntemlerini kullanarak kromatografik çözünürlüğü hesaplar. Ayrıntılı bir inceleme için bkz. Meyer ([2010](https://www.doi.org/10.1002/0470032677)).

Kromatografik çözünürlük $R$, hem pik tepeleri hem de genişlikleri hakkında bir şekilde bilgi içeren çeşitli yaklaşımlar kullanılarak iki komşu pik arasında hesaplanabilir. $R = 1$'de yalnızca pik tepeleri ayrılırken, benzer boyuttaki piklerin neredeyse tam çözünürlüğü $R >= 1.5$'te elde edilir. Daha erken çıkan (**Pik 1**) ve **daha geç çıkan** (**Pik 2**) pik çiftleri için, daha basit ve yaygın hesaplama yöntemleri alıkonma sürelerini $t_{R1}$ ve $t_{R2}$ ile ya tabandaki ($W_1$ ve $W_2$) ya da %50 pik yüksekliğindeki ($W_{0.5h1}$ ve $W_{0.5h2}$) karşılık gelen pik genişliklerini kullanır. Bu parametrelerle yaygın olarak kullanılan üç farklı denklem aşağıda sunulmaktadır. Bunların ilki tabandaki tam pik genişliğini kullanır ve bu nedenle Gauss olmayan pik şekillerinden nispeten daha fazla etkilenen en muhafazakâr ölçüdür ve genellikle diğer yöntemlerle elde edilenlerden daha düşük değerlerle sonuçlanır.

$$ R = (t_{R2}-t_{R1})/(0.5\times(W_1+W_2)) $$

$$ R = 1.176\times[(t_{R2}-t_{R1})/(W_{0.5h1}+W_{0.5h2})] $$

$$ R = (t_{R2}-t_{R1})/[1.7\times 0.5\times(W_{0.5h1}+W_{0.5h2})] $$

**İzokratik ayırmalar için**, bitişik pikler arasındaki teorik plaka sayısının büyük ölçüde aynı olması gereken durumlarda, $R$ değeri ayrıca pik tutunma faktörleri ($k_1$ ve $k_2$), ayırma faktörü $\alpha$ ve ortalama teorik plaka sayısı $\overline{N}$ ile aşağıdaki denklemle (bazen **Temel Çözünürlük Denklemi** olarak adlandırılır) ilişkilendirilebilir:

$$ R = (\sqrt{\overline{N}}/4)\times[(\alpha-1)/\alpha]\times[k_2/(1+k_2)] $$
