### Entegrasyon

Bu modül, bireysel pikler için baz çizgilerini hesaplamak ve yamuk kuralını kullanarak alanlarını entegre etmek 
için tasarlanmıştır. İş akışı, pikleri çözünürlüklerine göre değerlendirir. İlk olarak, tamamen çözülmüş pikler 
için baz çizgileri FastChrom algoritması aracılığıyla hesaplanır (bkz. Johnsen *et al.*, [2013](https://doi.org/10.1039/c3an36276k)). 
Çözülmemiş pik grupları için ortak baz çizgileri de aynı şekilde türetilir.

**Temel** sekmesi, çoğu Waters Corporation ([2017](https://www.waters.com/content/dam/waters/en/library/white-papers/2007/720000494en.pdf)) 
tarafından açıklanan birkaç entegrasyon yöntemi sunar. Her çözülmemiş pik çifti arasındaki sınırlar basit bir 
**Dik İniş** baz çizgisiyle ayrılabilir. Dik İniş, azalan çözünürlükle pik alanlarını giderek daha fazla bozma 
eğilimindedir. Alternatif olarak, pik sınırları üç "kaymalı" baz çizgisi yaklaşımından birine uygunluk açısından 
test edilir: **Teğet Kayma**, **Üstel Kayma** veya **Gauss Kayma**. Bu, bir dizi koşulun değerlendirilmesiyle yapılır:

1. Pik sınırı kaynaşmış veya omuz olarak mı sınıflandırılıyor?
2. Değerlendirilen çiftteki en az bir pik kaynaşmış veya omuz olarak mı sınıflandırılıyor?

Bu başlangıç koşullarından herhangi biri yanlışsa, **kayma denenmez**. Aksi takdirde, ek koşullar değerlendirilir:

3. Daha erken çıkan pik ana pik mi yoksa yavru pik mi (pik maksimumlarının yüksekliğine göre belirlenir)? 
Bu, **ön** veya **kuyruk** kaymanın uygulanıp uygulanmayacağını belirler.

4. Yavru pikin tepesi, ana pikin en yakın (x ekseni boyunca) bükülme noktasından daha mı düşük? 
Bu koşul yanlışsa, **Üstel veya Gauss** kayma denenmez.

5. *Kayma-Vadi oranı* (**yavru pik yüksekliğinin pikler arası sınır/vadi yüksekliğine oranı**) kaymada 
belirlenen eşikten daha mı düşük?

6. Ana pikin yavru pike yükseklik oranı *Dyson Kriterinden* daha mı yüksek? 
Bağımlı koşullardan herhangi biri yanlışsa, **kayma yapılmaz**.

7. Yavru pikin dış sınırı pikler arası sınırdan daha mı yüksek? 
Evetse, teğet kayma mümkün değildir.

Bu koşullar ayrıca her pik çifti için ana ve yavru pikin kimliğini ve uygun kayma türünü 
(ön veya kuyruk) belirler. Böylece, bir sınır yukarıdaki koşullara göre uygun olarak belirlenirse, 
yöntemde belirtilen türde kaymalı baz çizgisi oluşturma ve optimizasyonu denenir.

Ana pikten ön teğet kayma için, pikler arası sınırdan yavru pikin başlangıcı ile maksimumu arasındaki 
her noktaya (bu durumda daha erken gerçekleşen) düz çizgiler çizilir. Kuyruk teğet kayma için, 
çizgiler pikler arası sınır ile yavru pikin maksimumu ile sonu arasındaki her nokta arasında çizilir 
(şimdi daha geç çıkan piktir). Y değerleri herhangi bir noktada maksimum yavru pik sinyalinin **>%2**'si 
olan çizgiler atılır. Geri kalanlar arasında, bitiş noktası yavru pikin dış sınırına en yakın olan 
ve değerlerin **<%40**'ının karşılık gelen kromatografik sinyal değerinin **%1**'i içinde olduğu çizgi seçilir.

**Üstel Kayma** için, ana pikin pikler arası sınıra en yakın bükülme noktasından yavru pikin başlangıcına 
(ön kayma için) veya sonuna (kuyruk kayma için) uzanan üstel bir eğri oluşturmak için aşağıdaki denklem kullanılır.

$$ H_{ex} = H_0\times exp^{(-B\times(t_R-t_0))} + A\times t_R + C $$

Burada $H_{ex}$ üstel eğri değeri, $H_0$ pikler arası sınırdaki (örn. vadi) yükseklik (sinyal), 
$B$ üstel büyüme/azalma fonksiyonu (**kuyruk** kayma için negatif), $A$ ana pik baz çizgisinin eğimi, 
$C$ ana pikin baz çizgisi ofseti ve $t_R$ ile $t_0$ sırasıyla $H_b$ ve pikler arası sınırdaki alıkonma 
süreleridir. Başlangıç üstel eğrisi $B$ ve $C$ değerleri **0** olarak ayarlanarak oluşturulur. 
Ofset sabiti $C$ daha sonra sonuç ile pikler arası sınırdaki sinyal arasındaki farkla belirlenir 
ve üstel uyumun $B$ sabiti, **ana pik bölgesinde** (en yakın bükülme noktasından pikler arası sınıra kadar) 
eğri ile orijinal kromatografik sinyal arasındaki minimum **Öklid Uzaklığı** için optimize edilir. 
Son olarak, pikler arası sınırdan yavru pikin dış sınırına uzanan üstel eğri, optimize edilmiş 
$B$ ve $C$ sabitleri kullanılarak çizilir. Bu, kaymalı baz çizgisi olarak kullanılır.

**Gauss Kayma** için, ana pikin tepesi ile pikler arası sınır arasında bir model oluşturmak için 
Gauss eğrisinin aşağıdaki genel formu kullanılır.

$$ H_{gs} = H_p\times exp^{-(\frac{t_R-t_0}{\sigma})^2} $$

Burada $H_{gs}$ Gauss eğrisi değeri, $H_p$ ana pik maksimum sinyali, $t_0$ karşılık gelen alıkonma süresi, 
$t_R$, $H_{gs}$'deki alıkonma süresi ve $\sigma$ Gauss eğrisinin standart sapmasıdır (burada bükülme 
noktalarındaki pik yarı genişliği olarak tahmin edilir). Gauss eğrisi, ortaya çıkan eğri ile orijinal 
ana pik arasındaki **Öklid Uzaklığı** minimize edilene kadar yinelemeli olarak optimize edilir. 
Son eğri daha sonra optimize edilmiş parametreler kullanılarak **ana** pik tepesi ile **yavru** pikin 
dış sınırı arasında oluşturulur. Model daha sonra iki koşul için kontrol edilir:

1. Gauss eğrisinin en düşük noktası ana pik maksimumunun **%1**'inden daha mı yüksek?
2. **Yavru** pik bölgesindeki eğrinin herhangi bir noktası orijinal sinyalin üzerinde mi 
(yani eğri kromatogramı kesiyor mu)?

Yukarıdakilerden herhangi biri doğruysa, Gauss eğrisi reddedilir, başka kayma denenmez ve bunun yerine 
Dik İniş çizgisi oluşturulur. Aksi takdirde, Gauss eğrisi **ana** pik maksimumundan eğrinin orijinal 
kromatogramdan tutarlı olarak daha düşük sinyal (yani yükseklik) gösterdiği noktaya kadar kesilir.

Tüm baz çizgileri oluşturulduktan sonra, tüm pikleri entegre etmek ve pik alanlarını ($PA$) hesaplamak 
için **Yamuk Kuralı** kullanılır.

$$ PA = \sum{(x_{i+1}-x_i)\times(y_{i+1}+y_i)/2} $$

**Sonuç Tablosu** her pik için entegrasyon türünü **PD** (Dik İniş), **TS** (Teğet Kayma), 
**ES** (Üstel Kayma) veya **GS** (Gauss Kayma) olarak sınıflandırır.
