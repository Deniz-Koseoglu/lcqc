### Pik Simetrisi

Kromatografik veriler için Birleşik Devletler Farmakopesi (USP)/Avrupa Farmakopesi (EP) **Kuyruk Faktörü** ($T_f$) ve **Asimetri Faktörü** ($A_s$) dahil olmak üzere yaygın asimetri metriklerini hesaplar. Ek olarak, **baz çizgisi ile ayrılmış** pikler için Wahab *et al.* ([2017](https://doi.org/10.1016/j.chroma.2017.06.031)) tarafından önerilen **Toplam Pik Analizi (TPA)** iş akışı da uygulanmaktadır.

**Temel** sekmesi, kullanıcıya aşağıdakileri içeren **Modelleme Yöntemi** seçimi sunar:

**Asimetri Faktörü $A_s$**
: Pikin **arka** ($B_{10}$) ve **ön** ($A_{10}$) kenarlarında %10 pik yüksekliğindeki yarı genişliklerin oranı olarak hesaplanır. Bu nedenle, **1'den büyük** değerler tailing'i, 1'den küçük değerler fronting'i ve tam olarak 1 olan kuyruk faktörü mükemmel simetrik bir pikin karakteristiğini gösterir.

$$ A_s = B_{10}/A_{10} $$

**USP Kuyruk Faktörü $T_f$**
: **%5 pik yüksekliğindeki** yarı genişlikleri kullanan başka bir yaygın metriktir.

$$ T_f = \frac{(A_5+B_5)}{2A_5} $$

**Toplam Pik Analizi (TPA)**
: $T_f$ veya $A_s$'nin kullanılıp kullanılmadığına bakılmaksızın, çeşitli farmakopelerde belirtilen kabul kriterleri **0.8-1.8** aralığını kabul edilebilir olarak listeler. Uygulamada, tipik test analitleri için **0.9-1.2** değerleri rutin olarak elde edilir ve tercih edilir. Hem $T_f$ hem de $A_s$ yalnızca tailing veya fronting'in **göreli** miktarı hakkında bilgi sağlar, ancak her ikisinin de mevcut olduğu durumları barındıramaz. Örneğin, hem tailing hem de fronting içeren genişlemiş bir pik (sözde *Eyfel Kulesi* etkisi), Gauss profilinden büyük ölçüde bozulmuş olmasına rağmen 1'e yakın $T_f$ ve $A_s$ değerlerine sahip olabilir. Toplam pik şekline tailing ve fronting'in mutlak ve göreli katkılarının ayrı ölçümlerini sağlamak ve sonuçları etkili bir şekilde görselleştirmek için, bu modül ayrıca Wahab *et al.* ([2017](https://doi.org/10.1016/j.chroma.2017.06.031)) tarafından basit bir görsel ve nicel değerlendirme aracı olarak geliştirilen **Toplam Pik Analizi (TPA)** iş akışını uygular. İlk olarak, Gauss standart sapması $\sigma$, seçilen pik yüksekliği yüzdesindeki ($W_H$) pik genişliği kullanılarak bir pikten (maksimum sinyali **bire normalize edilmiş**) tahmin edilir:

$$ \sigma = W_H/(2\sqrt{2ln(1/H)}) $$

$\sigma$ tahmini, kromatografik piklerin tepelerinin (**%80-85** pik yüksekliğinden sonra) genellikle ağır bozulmuş pikler için bile Gauss dağılımını yakından izlediği gerçeğinden yararlanır. Böylece, tahmin edilen $\sigma$, 1'e eşit bir pik maksimumu ve gerçek pik alıkonma süresi kullanılarak bir Gauss modeli oluşturulur. Ardından, Gauss modelinin üst %15 değerlerinin gerçek kromatografik pike eşit veya onun içinde olmasını sağlamak için doğrusal kısıtlamalı bir çözücü kullanılır. Son olarak, pik fronting ve tailing derecesinin ayrı nicel ölçümlerini sağlayan mutlak ve göreli (%) artıklar, pikin ön ve arka kenarları için ayrı ayrı hesaplanır. Şu anda, TPA prosedürü **yalnızca baz çizgisi ile ayrılmış pikleri değerlendirmekle** sınırlıdır. Her pik ayrıca pik tepesinin solundaki ve sağındaki artık toplamlarına göre TPA için uygunluk açısından değerlendirilir. Gauss modelinin tamamen orijinal kromatografik pikin içinde olması beklenir, ancak bu nadiren böyledir ve modelin pikin alıkonma süresi sınırlarının dışında olduğu yerlerde negatif artıklara neden olur. Pikin herhangi bir tarafında **>%50** artık negatif bulunursa, TPA için uygun olmadığı kabul edilir.
<br></br>
#### Diğer temel seçenekler
**Pikler**
: Kullanıcının işlenmesi gereken pik indekslerini belirtmek için bir veya daha fazla sayısal değer girmesine olanak tanır. Etkileşimli pik seçimi için **Görsel Seçim** seçeneği de mevcuttur.

**Kritik genişliği otomatik belirle**
: Baz çizgisi düzeltmesi için kullanılan kritik genişlik parametresinin otomatik olarak mı tahmin edileceğini yoksa manuel olarak mı (tamsayı olarak) belirtileceğini gösteren bir onay kutusu.

**Genişlikleri göster**
: Algoritma, pik boyunca belirli noktalarda genişlikleri ve yarı genişlikleri hesaplar. Bu anahtar, bunların **Sonuç Tablosuna** dahil edilmesini açar/kapatır.
<br></br>
#### Gelişmiş seçenekler
**Gelişmiş** sekmesinde ek seçenekler mevcuttur.

**Optimizasyon Yöntemi**
: Yinelemeli optimizasyon için kullanılacak yöntemi belirtir. Yalnızca **Toplam Pik Analizi (TPA)** ile ilgilidir. Mevcut yöntemler arasında **Doğrusal Olmayan Programlama** ve **Nelder-Mead** bulunur.

**Çözünürlük**
: Çözünürlüğü artırmak daha ayrıntılı çıktı grafikleriyle sonuçlanacaktır. **Yalnızca TPA ile ilgilidir**.
