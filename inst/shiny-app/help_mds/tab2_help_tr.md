### Baz Çizgisi Düzeltmesi

Bu modül, çeşitli algoritmalar kullanarak içe aktarılan kromatogram üzerinde baz çizgisi düzeltmesi yapar. 
**Temel** sekmesi, çeşitli düzeltme yöntemleri arasında seçim yapmak için bir açılır liste içerir:

**Yinelemeli Asimetrik En Küçük Kareler (ALS)**
: 2. dereceden sonlu fark cezası ile ağırlıklı Eilers (yani Whittaker-Henderson) yumuşatmasına dayanan yinelemeli 
yakınsama (Eilers, [2003](https://doi.org/10.1021/ac034173t)). Ayrıntılı açıklama ve örnekler için örneğin 
Peng *et al.* ([2010](https://www.doi.org/10.1016/j.aca.2010.08.033)) çalışmasına bakınız.

**Chang Yöntemi**
: Chang *et al.* ([2007](https://doi.org/10.1016/j.jmr.2007.05.008)) tarafından geliştirilen, sinyal yoğunluğu 
yüksek kromatogramlar ve spektrumlar için gürbüz bir algoritma.

**Kök Hata Ayarlamalı Yinelemeli Yumuşatma Spline'ları (ISREA)**
: Baz çizgisini tahmin etmek için yumuşatma spline'larını kullanır (Xue *et al.*, [2021](https://www.doi.org/10.1177/0003702820955245)).

**Modifiye Polinom Uyumu (ModPolyFit)**
: Lieber & Mahadevan-Jansen ([2003](https://www.doi.org/10.1366/000370203322554518)) tarafından geliştirilen, 
otomatik polinom uydurmasına dayanan yinelemeli baz çizgisi düzeltme algoritması.

Baz çizgisi düzeltmesi, **Yok** (varsayılan) seçilerek tamamen atlanabilir.
<br></br>
#### Gelişmiş seçenekler

**Gelişmiş** sekmesi, seçilen düzeltme yöntemine göre bağlamsal olarak değişir ve çeşitli yönteme özgü 
seçenekler içerir:

1. **ALS** seçenekleri ikinci türev kısıtlamasını (**Lambda**), pozitif artıkların ağırlıklarını (**p**), 
yakınsamaya ulaşmak için gereken hata toleransını (hassasiyet) (**prec**) ve maksimum yineleme sayısını 
(**Maks. Yineleme**) içerir.

2. **Chang Yöntemi** seçenekleri, baz çizgisinin gürültü bileşenine göre konumunu (**Eşik**), yüksek geçiren 
filtre parametresini (**alpha**), baz çizgisi olarak kabul edilen filtrelenmiş sinyalin düşük yoğunluklu 
parçalarının oranını (**Baz Çizgisi Oranı**), filtrelenmiş sinyalin bölüneceği segment sayısını (**Segmentler**), 
nokta cinsinden sinyal penceresi boyutunu (**Sinyal Penceresi**) ve baz çizgisi uydurma yöntemini 
(doğrusal veya kübik; **Uydurma Yöntemi**) içerir.

3. **ISREA** seçenekleri **0.0001** ile **10** arasında yakınsama kriterini (**Eta**) ve maksimum yineleme 
sayısını (**Maks. Yineleme**) içerir.

4. **ModPolyFit** seçenekleri polinom uydurma derecesini (**Derece**), yakınsama için gereken hassasiyeti 
(**Hassasiyet**) ve maksimum yineleme sayısını (**Maks. Yineleme**) içerir.

Tüm yöntemler ayrıca düzeltilmiş sinyalden negatif değerlerin kaldırılması (**Negatifleri Kaldır**) seçeneğini 
içerir, böylece baz çizgisi sinyal değerleri hiçbir zaman orijinal değerleri aşmaz.
