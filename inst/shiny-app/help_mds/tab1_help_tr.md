### Dosya Verisi İçe Aktarma

Bu modül, ASCII dosyalarının (örn. .txt) LCQC'ye aktarılmasını sağlar. Başlangıçtaki **Veri Kaynağı** açılır listesi, 
iki içe aktarma modu arasında seçim yapmak için kullanılır:


**CSV Dosyası**
: **Alıkonma süresi** ve **sinyal** olmak üzere iki zorunlu sütun içeren basit bir kromatogram metin dosyası.

**Shimadzu TXT Dosyası**
: Shimadzu kromatograf yazılımı LabSolutions veya GCMSSolutions'dan elde edilen ASCII çıktı dosyası.
<br/><br/>
#### CSV Dosyası içe aktarma

**Gözat** düğmesi kullanılarak bir .CSV dosyası seçildikten sonra, zaman ve sinyal sütunları ilgili açılır listeler 
kullanılarak seçilebilir. Başlangıçta, tespit edilen birinci ve ikinci sütunlar otomatik olarak sırasıyla zaman ve 
sinyal olarak atanır. Alt ve üst **Alıkonma Süresi (RT)** kesim değerleri de belirtilebilir. Son olarak, 
**Gelişmiş** sekmesi ondalık ve sütun ayırıcıları için seçenekler içerir.
<br/><br/>
#### Shimadzu TXT Dosyası içe aktarma

Bu içe aktarma işlevi, Shimadzu Corporation'ın [LabSolutions](https://www.shimadzu.com/an/products/software-informatics/labsolutions-series/index.html) 
yazılımından dışa aktarılan karmaşık ASCII dosyalarıyla özellikle çalışır. **.CSV Dosyası** içe aktarıcısındaki tüm 
seçenekler korunur; bunlar arasında **Zaman** ve **Sinyal** sütunları için seçiciler, **Alıkonma Süresi** kesim 
limitleri ile **ondalık ve sütun ayırıcıları** bulunur. ==Ek Seçenekler== şunları içerir:

#### Temel sekmesi
1. **Kromatogram Modu:** Verilerin alındığı kromatograf türünün seçimi (GC-FID, GC-MS veya HPLC).
2. **Pik Tablosu Seç:** Kullanıcının hangi pik tablosunu görüntüleyeceğini seçmesine olanak tanır (tespit edilenler arasından).

#### Gelişmiş sekmesi
3. **Benzerlik Tablosu Çıkar (yalnızca GC-MS):** MS ile tespit edilen pikler için **benzerlik puanlarını** içeren tablonun 
alınıp alınmayacağını seçin.
4. **Pik Tablosu Çıkar**: Niteliksel pik tablosunun alınıp alınmayacağını seçin.
5. **Pik İsimlerini Al**: Pik isimleri ayrı olarak çıkarılmalı mı?
6. **CAS Numaralarını Al (yalnızca GC-MS)**: CAS numaraları ayrı olarak alınmalı mı?
7. **Meta Verileri Al**: Analiz meta verileri alınmalı mı?
8. **Sütun İsimlerini Düzelt**: Sütun isimleri sözdizimsel olarak doğru hale getirilmeli mi?
9. **Sütunları Filtrele**: **Pik tablosundan** en alakalı olanlar dışındaki tüm sütunları kaldırır.
