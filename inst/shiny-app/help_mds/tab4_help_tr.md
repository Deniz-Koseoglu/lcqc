### Pik Tespiti

Bu, LCQC içindeki açık ara en karmaşık modüldür ve 1 boyutlu verileri (yalnızca alıkonma süresi ve sinyal) 
kullanarak kromatografik pikleri otomatik olarak tespit etmek, filtrelemek ve sınıflandırmak için kullanılır. 
Pik tepeleri, bükülme noktaları, yükseliş noktaları ile başlangıç ve bitiş noktaları, özel ve endüstri standardı 
algoritmaların bir kombinasyonu kullanılarak belirlenir.

**Temel** sekmesi aşağıdaki seçenekleri içerir:
<br></br>

#### Genlik Eşik Yöntemi
Pik tespitinin yapılmadığı **genlik eşiğini** tahmin etmek için **bir veya birden fazla** yöntemin seçimi. 
Mevcut yöntemler:

**Z-Skorları**
: Sinyalin hem hareketli ortalamasının hem de hareketli standart sapmasının global ortalama değerlerini hesaplar 
ve genlik limitini hesaplamak için bir hassasiyet parametresi (bkz. **Gelişmiş seçenekler**) kullanır:

$$ AmpLim = \overline{MX} + (1/sens)\times\overline{SD} $$

**Basit Kantiller**
: Genlik limiti basitçe **0.001 ile 50** arasında belirtilen olasılık yüzdesindeki kantil olarak belirlenir.

**Göreli Farklar**
: Önce giriş verilerinin kantil aralığını hesaplar, ardışık kantiller arasındaki maksimum farkı türetir, 
bu farkın **0.001 ile 50** arasındaki bir yüzdesinin aşıldığı en erken (en düşük) kantili bulur 
ve bu kantili genlik limiti olarak kullanır.
<br></br>

#### Pik Yığılmasını Tespit Et ve Azalt
Özel bir algoritma kullanarak yığılmış pik tepelerini tespit eder ve kaldırır.
<br></br>

#### Türev tabanlı pik tespit yöntemi
Pik tespiti için kullanılacak alt (düşük) ve üst (yüksek) birinci/ikinci türev eşiklerinin hesaplanması için yöntem. 
Mevcut yöntemler:

**Gürültü çekirdeği tahmincisi**
: Yaygın olarak uygulanan gürültü çekirdeği yaklaşımını izler (örn. Waters Corporation, [2017](https://www.waters.com/content/dam/waters/en/library/white-papers/2007/720000494en.pdf)). 
Gürültü çekirdeği, türevlerin ortalaması $\overline{X}$'den standart sapma $SD$ uzaklığı olarak belirlenir:

$$ T=\overline{X}\pm sens1\times SD/sens2 $$

Birçok endüstri standardı yaklaşım $sens1$ için 4 değerini önerir ve $\pm 4SD$'nin kromatografik gürültüyü 
en iyi tanımladığını belirtir.

**Vaz *et al.* ([2016](https://doi.org/10.5935/0103-5053.20160076)) yöntemi**
: Türev sinyalinin global medyanı $M_D$ ile türev sinyali arasındaki farklar hesaplanır ve bu sonuçlardan 
yeni bir medyan $M_{new}$ hesaplanır. Son olarak, alt ve üst eşikler $T$ aşağıdaki gibi hesaplanır:

$$T=M_D\pm sens1\times M_{new}/sens2$$

Burada $sens1$, eşik aralığının genişlediği/arttığı ampirik bir faktördür.
Tersine, $sens2$ eşik aralığıyla ters orantılıdır.

**Z-skoru tabanlı**: Yalnızca türevlerin medyanına dayanan hafif bir varyasyon:

$$T=M_D+sens1\times M_D/sens2$$
<br></br>
#### Aykırı Değer Tespit Yöntemi

Bu açılır liste, gürültü sinyalinden aykırı değerleri kaldırmak için çeşitli yöntemler içerir. Bunlar şunlardır:

**Çeyrekler Arası Aralık**
: $Q_1-1.5\times IQR$ ve $Q_3+1.5\times IQR$ dışındaki tüm değerler kaldırılır 
(burada $Q_1$, $Q_3$ ve $IQR$ sırasıyla birinci çeyrek, üçüncü çeyrek ve çeyrekler arası aralıktır).

**Kantil tabanlı**
: %2.5 ve %97.5 kantillerinin dışındaki tüm değerler kaldırılır.

**Standart sapma tabanlı**
: $\overline{X}\pm 2.24\times SD$ dışındaki tüm değerler kaldırılır (burada $\overline{x}$ ve $SD$ 
sırasıyla türevlerin ortalaması ve standart sapmasıdır).
<br></br>
#### Pik Reddetme Filtreleri
**Sinyal/gürültü (S/N) oranı**, pik **yüksekliği**, bükülme noktası **genişliği** ve pik **alanına** dayalı olarak 
ilk tespitden sonra pikleri filtrelemek için dört farklı filtre mevcuttur. Bu filtreler iki aşamada uygulanır. 
Yükseklik ve S/N oranı filtreleri pik sınıflandırmasından önce, pik genişliği ve alan filtreleri ise sonra uygulanır. 
Her iki filtreleme aşaması için mantık geçişleri de mevcuttur (varsayılan olarak **VEYA** mantığı kullanılır).
<br></br>
#### Gelişmiş seçenekler
**Gelişmiş** sekmesi, pik tespitinin ince ayarı için çeşitli parametreler içerir.

**Hassasiyet Parametreleri**
: Bu parametreler, türevlere ve sinyale (genlik limiti) dayalı pik tespitinin hassasiyetini kontrol eder.
**Birinci Türev** ve **İkinci Türev** hassasiyet değerleri yukarıdaki denklemlerdeki $sens1$ ve $sens2$'ye 
eşdeğerdir, **Genlik Limiti Hassasiyeti** ise Z-skorları hesaplanırken kullanılır.

**Z-skoru Parametreleri**
: Z-skorları kullanarak pik bölgelerinin tespitini kontrol eden çeşitli parametreler. 
**Hareketli Ortalama/SS Gecikmesi (tamsayı)** parametresi, hareketli ortalama ve standart sapmanın 
dayandırılacağı gözlem sayısıdır. **Eşik** parametresi, algoritmanın pozitif veya negatif piklerin 
varlığını işaret ettiği z-skorudur (hareketli ortalamanın standart sapmasının çarpılacağı faktör). 
Son olarak, **Ortalama ve SS'ye hassasiyet** parametresi, yeni veri noktalarının hareketli ortalama 
ve standart sapma hesaplamasındaki göreli etkisini/ağırlığını belirten **0 ile 1** arasında bir faktördür.

**FastChrom Baz Çizgisi Düzeltmesi**
: Johnsen *et al.* ([2013](https://doi.org/10.1039/c3an36276k)) tarafından geliştirilen FastChrom algoritmasını 
kullanarak ek baz çizgisi düzeltmesini etkinleştiren veya devre dışı bırakan bir geçiş. İlgili **kritik genişlik** 
parametresi de ayarlanmalıdır.

**Sınır Doğrulama Parametreleri**
: Tespit edilen sinyal ve türev ekstremumlarını (yani maksimum ve minimumları) onaylamak (veya reddetmek) için 
her iki tarafta incelenecek nokta sayısını belirtin.

**ApexTrack Baz Çizgisi Genişletme Parametreleri**
: Burada kullanıcı, Waters Corporation ([2017](https://www.waters.com/content/dam/waters/en/library/white-papers/2007/720000494en.pdf)) 
tarafından geliştirilen ve açıklanan ApexTrack algoritmasının **liftoff** ve **touchdown** parametrelerini belirleyebilir.

**Sıfır Geçiş Parametreleri**
: Sinyal ve türevler için tespit edilen sıfır geçişlerini (aşağı ve yukarı geçişler) 
onaylamak (veya reddetmek) için her iki tarafta incelenecek nokta sayısını belirtin.

#### Pik Sınıflandırması
**Pik Tablosunda** listelenen tespit edilmiş tüm pikler **dört kategoriden birine** sınıflandırılır:

1. **B**: Baz çizgisi ile ayrılmış pikler.
2. **F**: Kaynaşmış pikler.
3. **S**: Omuz pikler.
4. **R**: Yuvarlak pikler. Neredeyse eşit yükseklikte pikler çok düşük (ama sıfır olmayan) çözünürlükte 
birleştiğinde ortaya çıkar.
