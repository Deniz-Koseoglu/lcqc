### Dekonvolüsyon

Bu modül, kromatografik piklerin modellenmesi için **Yinelemeli Eğri Uydurma (ICF)** uygular. ICF, birkaç Gauss ve modifiye Gauss modelini tek bir veya bir grup kromatografik pike uydurur ve yinelemeli optimizasyon yoluyla ortaya çıkan modelin ilişkili **Ortalama Karesel Hata Kökü'nü (RMSE)** minimize eder. Böylece, seçilen modelin kısıtlamaları dahilinde gerçek kromatografik sinyale mümkün olduğunca yakın bir model elde edilir. ICF, orta derecede veya tamamen kaynaşmış pikler için özellikle pik dekonvolüsyon yöntemi olarak kullanışlıdır.

Dahil edilen seçenekler aşağıda açıklanmaktadır.
<br></br>
#### Dekonvolüsyon Yöntemi
İş akışı şu anda **4 popüler ICF modeli** uygulamakta ve incelenen her pik veya **5 kaynaşmış pike** kadar olan grup için en uygun modelin aktif seçimini sunmaktadır. Mevcut modeller aşağıda açıklanmaktadır.

**Basit Gauss**
: Aşağıdaki denkleme dayanan en basit model (Nikitas *et al.*, [2001](https://doi.org/10.1016/S0021-9673(01)00524-6)):

$$ H_{GS} = H_m\times e^{-((t_R-t_m)/2\sigma)^2} $$ 

Burada $H_m$ ve $t_R$ sırasıyla pik maksimumundaki sinyal ve alıkonma süresi, $t_R$ mevcut alıkonma süresi ve $\sigma$ eğrinin standart sapmasıdır; bükülme noktası yüksekliğindeki pik genişliğinin yarısı olarak etkili bir şekilde yaklaştırılır. Böylece Gauss modeli yalnızca 3 optimize edilmesi gereken parametreye sahiptir ve bu nedenle hesaplama açısından ucuzdur, ancak kromatografik verilerde sıkça gözlemlenen fronting veya tailing **olaylarını barındıramaz**.

**Üstel Modifiye Gauss (EMG)**
: EMG modeli (örn. Li, [1995](https://www.doi.org/10.1093/chromsci/33.10.568); Nikitas *et al.*, [2001](https://doi.org/10.1016/S0021-9673(01)00524-6); Caballero *et al.*, [2002](https://doi.org/10.1016/S0021-9673(02)00194-2); Kalambet *et al.*, [2011](https://doi.org/10.1002/cem.1343)) Gauss modeline üstel bir bileşen ekler ve tailing ve fronting içeren kromatografik pikleri modellemek için açık ara en popüler yaklaşımdır:

$$ H_{EMG} = A\times e^{0.5\times(\sigma/\tau)^2 - ((t_R-t_0)/\tau)}\times\mathcal{P}((t_R-t_0)/\sigma - \sigma/\tau)/\tau $$

Bazen şu şekilde de yazılır (Li, [1997](https://www.doi.org/10.1021/ac970481d)):

$$ H_{EMG} = A/2\tau\times e^{(\sigma^2/2\tau^2 + (t_0-t_R)/\tau)}\times(1+erf((t_R-t_0)/\sqrt{2\sigma} - \sigma/\sqrt{2\tau})) $$ 

Burada yeni parametreler $A$ ve $\tau$ sırasıyla pik alanı ve üstel zaman sabitidir; fronting pikler için negatif ve tailing pikler için pozitiftir. EMG modeli GS veya EGH'den hesaplanması daha uzun sürer ve **daha az kararlı** görünmektedir.

**Üstel-Gauss Hibrit**
: EGH modeli (Lan & Jorgenson, [2001](https://doi.org/10.1016/S0021-9673(01)00594-5); Li, [2002](https://doi.org/10.1016/S0021-9673(02)00090-0)), denkleme kesilmiş bir üstel bileşen dahil ederek fronting ve tailing pikleri de barındıran basitleştirilmiş ampirik bir denklemdir:

$$ H_{egh} = H_m\times e^{-(t_{R2}-t_0)^2/(2\sigma^2 + \tau\times(t_{R2}-t_0))} $$

Burada $t_{R2}$, $(2\sigma^2 + \tau\times(t_R-t_0)) > 0$ olan alıkonma süreleridir. EGH, EMG'den **önemli ölçüde daha hızlı yakınsar**, ancak fronting pikleri modellemede (EMG'ye benzer şekilde) biraz esneklikten yoksundur.

**Ampirik Dönüştürülmüş Gauss**
: Son olarak, ETG modeli (Li, [1995](https://www.doi.org/10.1093/chromsci/33.10.568), [1997](https://www.doi.org/10.1021/ac970481d), [2002](https://doi.org/10.1016/S0021-9673(02)00090-0)) sırasıyla **ön** ($k_l$, $\lambda_l$ ve $\alpha$) ve **arka** ($k_r$, $\lambda_r$ ve $\beta$) pik kenarlarını tanımlayan 6 parametre içermesiyle benzersizdir. Pik yüksekliği $H_m$ dahil olmak üzere toplam 7 parametre optimize edilir. Ek olarak, fonksiyon her pik için sol ve sağ bükülme noktası zamanlarının ($t_l$ ve $t_r$) tahminlerini gerektirir; bunlar sabit kalır ve optimize edilmez:

$$ H_{etg} = (2H_me^{0.5})/((1 + \lambda_l e^{k_l(t_l - t)})^\alpha + (1 + \lambda_r e^{k_r(t - t_r)})^\beta - 1) $$

ETG modeli, yalnızca bir pik genliği parametresiyle (pik yüksekliğinden türetilen) ilişkili olan ön ve arka pik kenarlarının açıklamaları arasındaki gevşek bağlantının benzersiz avantajını sunar. Ek olarak, **7 parametrenin** yinelemeli optimizasyonuna rağmen, uydurma prosedürü hızla yakınsar (hesaplama açısından **pahalı değildir**).

Yukarıdaki tüm fonksiyonlar, aşağıdaki gibi temsil edilebilen **Ortalama Karesel Hata Kökü (RMSE)** üzerine dayalı bir ceza fonksiyonu aracılığıyla yinelemeli optimizasyona sunulur:

$$ RMSE = \sum{\sqrt{(y_i - \hat{y}_i)^2/n}} $$

Burada $y_i$, $\hat{y}_i$ ve $n$ sırasıyla orijinal sinyal, modellenmiş eğri ve örneklem boyutudur (orijinal verideki nokta sayısı). Bu hata metriği, model performansını değerlendirmek ve yinelemeli optimizasyon sırasında GS, EMG, EGH ve ETG arasından **en uygun modeli seçmek** için kullanılır.

#### Diğer temel seçenekler

**Kritik genişliği otomatik belirle**
: İş akışı, her pik için baz çizgileri oluşturmak amacıyla FastChrom algoritmasını (Johnsen *et al.*, [2013](https://doi.org/10.1039/c3an36276k)) kullanır. Algoritma, arttıkça daha az katı baz çizgisi düzeltme kurallarıyla sonuçlanan bir kritik genişlik parametresi kullanır. Bu parametre manuel olarak (tamsayı olarak) ayarlanabilir veya onay kutusu değerine göre otomatik olarak belirlenebilir.

**Baz çizgisi ile ayrılmış pikleri modelle**
: Varsayılan olarak, baz çizgisi ile ayrılmış pikler modellenmez çünkü pik alanlarını doğru bir şekilde entegre etmek için bu gerekli değildir. Ancak, bu onay kutusu etkinleştirilerek modellenebilirler.
<br></br>
#### Gelişmiş seçenekler
**Gelişmiş** sekmesi ek seçenekler sunar:

**Optimizasyon Yöntemi**
: Yinelemeli uydurma sırasında yakınsamaya ulaşmak için kullanılan algoritma(lar). Seçenekler arasında **Nelder-Mead**, **Broyden-Fletcher-Goldfarb-Shanno (BFGS)** ve kutu kısıtlamalı varyantı ile **Benzetimli Tavlama** bulunur. Varsayılan olarak tüm algoritmalar kullanılır ve **RMSE**'ye göre her pik için en iyi performans gösteren seçilir.

**EMG gösterimi**
: **EMG1**'in varsayılan ve **EMG2**'nin Lee *et al.* ([1997](https://www.doi.org/10.1021/ac970481d)) tarafından yazılan form olduğu EMG denkleminin biçimini seçer. Daha fazla ayrıntı için yukarıdaki EMG denklemlerine bakın.
