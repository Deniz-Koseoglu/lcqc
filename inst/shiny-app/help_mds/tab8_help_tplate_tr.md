### Teorik Plakalar

Bu modül, **5-sigma** (**S5**), **Avrupa/İngiliz Farmakopesi** (**EP**), Alan-Yükseklik (**AH**) ve **Üstel Modifiye Gauss** (**EMG**) gibi çeşitli yöntemler kullanarak teorik plaka sayısı $N$, **Teorik Plakaya Eşdeğer Yükseklik ($HETP$)**, indirgenmiş plaka yüksekliği $h$ ve ayırma empedansı $E$'yi hesaplar.

Teorik plaka sayısı $N$, daha yüksek teorik plaka sayısının daha iyi kolon verimliliğini temsil ettiği yaygın olarak kullanılan bir kolon performans indeksidir. LCQC'de birkaç hesaplama yöntemi uygulanmaktadır. **Tam Genişlik (FW)** yöntemi, **%13.4 pik yüksekliğinde** oluşan mükemmel bir Gauss piki için teğet çizgilerinin baz çizgisi kesişimleri olarak tanımlanan Gauss pikin taban genişliği $W_b$'yi kullanır.

$$ N_{FW} = 16(t_R/W_b)^2 $$

**5-sigma (S5)** yöntemi, mükemmel bir Gauss pikin **%4.4 yükseklikte** tam olarak 5-sigma (yani 5 standart sapma) genişliğinde olduğu gerçeğini kullanır (Bidlingmeyer & Warren Jr., [1984](https://www.doi.org/10.1021/ac00278a002); Villalon, [2023](https://doi.org/10.1021/acs.jchemed.2c00588)) ve genişliği belirlemek için 5-sigma ölçüsünü kullanır. Bu, kromatografide sıkça karşılaşılan ve piki Gauss şeklinden bozan tailing ile başa çıkmaya yardımcı olur.

$$ N_{5\sigma} = 25(t_R/W_{5\sigma})^2 $$

**Avrupa Farmakopesi (EP)** yöntemi muhtemelen en popüler olanıdır ve yarı yükseklikteki pik genişliğini ($W_{50}$) alıkonma süresi $t_R$ ile birlikte kullanır. Kullanılan katsayı Alman (DAB), İngiliz (BP) ve Avrupa (EP) Farmakopelerinde **5.55**'ten Japon Farmakopesi'nde (*Rev. 15, Nisan 2006*) **5.54**'e kadar değiştiğinden, burada bunların ortalaması (**5.545**) uygulanmıştır.

$$ N_{EP} = 5.545(t_R/W_{50})^2 $$

Alternatif bir denklem, *yaklaşık* **%60.7** pik yüksekliğindeki teorik Gauss bükülme noktası genişliğini kullanır.

$$ N_{inf} = 4(t_R/w_{60.7})^2 $$

Adından da anlaşılacağı gibi, **Alan-Yükseklik (AH)** yöntemi $N$'yi hesaplamak için pik alanı $A$ ve yükseklik $H$'yi kullanır.

$$ N_{AH} = 2\pi(t_RH/A)^2 $$

Yukarıdaki tüm yöntemler, pratik kromatografide neredeyse hiç karşılaşılmayan gerçek Gauss pik şekline dayanmaktadır. Tailing ve (daha az yaygın olan) fronting olayları pik şeklini bozar ve hesaplamada hatalara neden olur. Foley & Dorsey ([1983](https://www.doi.org/10.1021/ac00255a033)) tarafından önerilen ve Üstel Modifiye Gauss (EMG) modeline dayanan basit bir denklem, "yaklaşık olarak doğru" (Meyer, [2010](https://www.doi.org/10.1002/0470032677)) $N$ değerleri verir. Denklem, pik genişliğini ($W_{10}$) ve %10 yükseklikteki yarı genişlikleri (sırasıyla arka ve ön genişlik için $b$ ve $a$) kullanır. Bu denklemle elde edilen teorik plaka sayısı genellikle diğer yöntemlerden daha düşüktür.

$$ N_{EMG} = (41.7(t_R/W_{10})^2)/(b/a + 1.25) $$

Ayrıca plaka yüksekliği hesaplanır; **Teorik Plakaya Eşdeğer Yükseklik (HETP)** olarak da bilinen bu değer, kromatografik dengenin sağlandığı mm (veya µm) cinsinden mesafedir. Bu basitçe $N$ ve kolon uzunluğu $L$ ile ilişkilidir:

$$ HETP = L/N $$

HETP'den, sabit fazın ortalama partikül boyutu $d_p$ (µm cinsinden) biliniyorsa, indirgenmiş plaka yüksekliği $h$ de hesaplanabilir; $h$ boyutsuz bir parametredir ve farklı uzunluk ve partikül boyutundaki kolonları daha kolay karşılaştırmak için kullanılabilir. $h$ değerleri **2 ile 5 arasında olmalıdır (düşük daha iyidir)**. Örneğin, 3 değerinde, 3 sabit faz katmanı üzerinde tam kromatografik denge elde edilir (Meyer, [2010](https://www.doi.org/10.1002/0470032677)).

$$ h = HETP/d_p $$

Son olarak, Ayırma Empedansı $E$, geri basınç ($\Delta p$), teorik plaka sayısı, hareketli fazın dinamik viskozitesi ($\eta$, mPas cinsinden) ve geçiş süresini içeren bir kolon "kalitesi" (verimlilik) ölçüsüdür. Bir sıvı kromatografi işleminin "yüksek performanslı" kabul edilmesi için **>10000** değerleri zorunludur (Meyer, [2010](https://www.doi.org/10.1002/0470032677)). LCQC'de $E$, **Empedans Yöntemi** **Bireysel** seçeneğini içerdiğinde her bir bireysel $N$ değeri için ayrı ayrı hesaplanabilir ($HETP$ ve $h$ gibi). Bu durumda, aşağıdaki denklem kullanılır:

$$ E = (\Delta p t_0)/(N^2\eta) $$

Ek olarak, yöntem **Evrensel** (varsayılan) olarak ayarlandığında $N$'den bağımsız evrensel bir denklem kullanılabilir.

$$ E = (10^8/5.54^2)\times(\Delta p t_0/\eta)\times(W_{50}/t_R)^4 $$
