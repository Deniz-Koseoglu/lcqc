### Yumuşatma

Bu modül, baz çizgisi düzeltmesi yapıldıktan sonra (yapılmışsa) sinyal ile birinci ve ikinci türevlerin 
yumuşatılması için tasarlanmıştır. **Temel** sekmesi birkaç yumuşatma yöntemi içerir:

Adından da anlaşılacağı gibi, **Dikdörtgen** yumuşatma, her noktayı birkaç komşu noktanın ortalamasıyla 
değiştiren basit bir *dikdörtgen kutu* algoritmasıdır. Örneğin, 3 noktalı yumuşatma için:

$$S_j=\frac{Y_{j-1}+Y_j+Y_{j+1}}{3}$$

**Üçgen** yumuşatma daha karmaşık bir ağırlıklı algoritma uygular. Örneğin, 5 noktalı yumuşatma:

$$S_j=\frac{Y_{j-2}+2Y_{j-1}+3Y_j+2Y_{j+1}+Y_{j+2}}{9}$$

Son olarak, dijital yumuşatma polinom filtreleri olarak da bilinen Savitzky-Golay (Savitsky & Golay, 
[1964](https://doi.org/10.1021/ac60214a047)) yumuşatma yöntemleri, **kuadratik** veya **kuartik** polinom 
regresyonu aracılığıyla hareketli ortalamaya dayalı yumuşatma uygular. İkincisi genellikle dar piklerde 
daha iyi performans gösterir, ancak daha geniş pikleri bozabilir.
<br></br>
### Gelişmiş seçenekler

Burada yumuşatma **nokta** ve **geçiş** sayısı ayarlanabilir (varsayılan 3 ve 3'tür). Alternatif olarak, 
bunlar LCQC için geliştirilen yinelemeli bir algoritma kullanılarak otomatik olarak belirlenebilir. 
Otomatik belirleme seçilirse, optimizasyona başlamak için tahmini bir nokta ve geçiş sayısı sağlanmalıdır.
