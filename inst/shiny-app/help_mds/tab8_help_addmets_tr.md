### Kolona Özgü Metrikler

Bu modül, çeşitli kolon performans göstergelerini hesaplamak için uzunluk, partikül boyutu, akış hızı, iç çap (diğerlerinin yanı sıra) gibi çeşitli kolon özelliklerini kullanır.

Hareketli fazın **Doğrusal Hızı** $\mu$, kolon uzunluğu $L_c$ ve gerçek geçiş süresi $t_0$ ile basitçe ilişkilidir.

$$ \mu = L_c/t_0 $$

Bu denklemdeki geçiş süresini doğrulamak, yani tutulmayan analitin gerçekten hareketli fazın hızında kolondan geçip geçmediğini belirlemek için **Paketleme Gözenekliliği** $\epsilon$'u hesaplamak faydalı olabilir. **Bağlı** silika bazlı sabit fazların paketleme gözenekliliği *yaklaşık* 0.65'tir. Bu değeri referans olarak kullanarak, $\epsilon$ basitçe $t_0\text{ }(s)$, $L_c\text{ }(mm)$, akış hızı $F\text{ }(mL\text{ }dk^{-1})$ ve iç kolon çapı $d_c^2\text{ }(mm^2)$'den hesaplanabilir.

$$ \epsilon = 21\times[(Ft_0)/(d_c^2L_c)] $$

$\epsilon$'un önemli ölçüde daha yüksek veya daha düşük değerleri (sırasıyla >1 ve <0.5) analitin tutulmasını veya sabit fazın gözeneklerinden dışlanmasını gösterir (Meyer, [2010]()).

Bir diğer yararlı metrik, geri basıncı $\Delta\rho\text{ }(bar)$ içeren **Geçirgenlik** $K$'dır. Büyük bir $K$ değeri zayıf kolon paketlemesini gösterirken, tersi bir sızıntının karakteristiğidir. $1.6\text{ }mL\text{ }dk^{-1}$'lik yüksek akış hızında bağlı fazlar için $15\text{ }mm^2\text{ }s^{-1}\text{ }bar^{-1}$ değerleri tipiktir.

$$ K = L_c^2/(\Delta\rho t_0) $$

**Özgül Geçirgenlik** $K^{\circ}\text{ }(mm^{-2})$ da hesaplanabilir (bağlı fazlar için tipik olarak $4.0\times 10^{-8}\text{ }mm^2$); akış hızı, dinamik viskozite, kolon uzunluğu, iç çap ve geri basınç dahil edilerek.

$$ K^{\circ} = 21\times 10^-8\times[(F\eta L_c)/(d_c^2\Delta\rho)] $$

Son olarak, $K$ farklı kolonların karşılaştırılmasını kolaylaştıran Akış Direnci $\Phi$ adı verilen boyutsuz bir metrik olarak temsil edilebilir. Ek gerekli parametrelerden biri partikül boyutu $d_p\text{ }(\mu m)$'dir.

$$ \Phi = 4.7\times[(\Delta\rho d_p^2 d_c^2)/(L_c \eta F)] $$

Paketlenmiş HPLC kolonları için tipik bir $\Phi$ değeri olan **1000** gözlemlenir. Önemli yukarı ve aşağı sapmalar (örn. **>2000 veya <500**) sırasıyla **bir tıkanmayı veya paketlemedeki boşlukları** gösterir.

Kolon performansını değerlendirmek için teorik plaka sayısı ve indirgenmiş plaka yüksekliği ve Ayırma Empedansı gibi ek boyutsuz metriklerin hesaplanması için **Teorik Plakalar** modülüne bakın.
