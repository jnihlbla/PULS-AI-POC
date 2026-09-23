000001 ID DIVISION.                                                             
000002 PROGRAM-ID.     W4022300.                                                
000003 AUTHOR.         JAN-ERIK FRANTZEN.                                       
000004 DATE-WRITTEN.   AUGUSTI -91.                                             
000005 DATE-COMPILED.                                                           
000006                                                                          
000007*    FUNKTION.                                                            
000008*        PROGRAMMET HANTERAR SVARSBILD TILL ORDERREGISTRERING             
000009*        4221/4222.                                                       
000010*        VISAR AVVIKELSER.                                                
000011*        UPPDATERING AV GODKÄNDA RADER. FLOBOK BLIR = 'J'.                
000012*        ANNULLATION AV HEL ORDER MÖJLIG.                                 
000013*                                                                         
000014*        EFTER AVSLUTAD BEHANDLING SKER UTHOPP TILL ORDERHUVUD            
000015*        4221.                                                            
000016*                                                                         
000017*        PROGRAMMET ÄR ETT UPPDATERINGS-MPP                               
000018*        PROGRAMMET LÄSER      WLORQM (WDQ1)  ORDERBEKR.BAS               
000019*        PROGRAMMET LÄSER      WLORQI (WDQ2)  ORDERHUVUD                  
000020*        PROGRAMMET LÄSER      WLORQA (WDQ4)  ORDERDELAR                  
000021*        PROGRAMMET UPPDATERAR WLORQF (WDQ4)  ORDERRADER                  
000022*        PROGRAMMET LÄSER              WDD6   ARTIKELREGISTER             
000023*        PROGRAMMET LÄSER      WLARTM (WDK9)  ARTIKELREGISTER             
000024*        PROGRAMMET LÄSER      WLBENA (WDD3)  BENÄMNINGSREGISTER          
000025*        PROGRAMMET LÄSER      WLXXKK (WDR1)  TVÅNGSSTYRN.TAB             
000026*        PROGRAMMET LÄSER      WLXXKM (WDR1)  RANSFAKTOR.TAB              
000027*        PROGRAMMET LÄSER      WLXXKN (WDR1)  TEDTIDS.TAB                 
000028*        PROGRAMMET LÄSER      WLXXKB (WDR1)  TRANSPORTAVGÅNGAR           
000029*        PROGRAMMET LÄSER      WLLEVF (WDF2)  STYRREG DIRLEV              
000030*        PROGRAMMET LÄSER      WLLEVG (WDF2)  STYRREG DIRLEV ASEQ         
000031*        PROGRAMMET LÄSER      WLLEVA (WDF1)  LEVERANTÖRSREG              
000032*        PROGRAMMET LÄSER      WLERSA (WDD7)  ERSÄTTNINGSREG.             
000033*        PROGRAMMET LÄSER      WLXXKR (WDR4)  KAMPANJREGISTER             
000034*        PROGRAMMET LÄSER      WLXXKS (WDR4)  MARKN.REG KAMPANJ           
000035*        PROGRAMMET LÄSER      WLXXKT (WDR4)  ANTALSTAB KAMPANJ           
000036*        PROGRAMMET LÄSER      WLORDP (WDA5)  RESTORDERREG.               
000037*        PROGRAMMET LÄSER      WLXXBU (WDR5)  LARMKÖ                      
000038*        PROGRAMMET LÄSER      WLXXKO (WDR1)  CLEARING ARTIKEL            
000039*        PROGRAMMET LÄSER      WL4437 (WDR1)  ARBTIDSKALENDER             
000040*                                                                         
000041*    INDATA.                                                              
000042*        TRANSAKTION: W4T223                                              
000043*                     W4T223U                                             
000044*                     W4T223V                                             
000045*        MID:         W4I22301                                            
000046*                                                                         
000047*    UTDATA.                                                              
000048*        MOD:         W4O22301                                            
000049*                                                                         
000050*    E'TRACKER: 5444132 DATED 2007-08-21                                  
000051*    E'TRACKER: 2218613 DATED 2008-03-11                                  
000052*    E'TRACKER: 7450328 DATED 2008-HÖST   VOHF                            
000053*    E'TRACKER:10254592 DATED 2015        DECOMISSION VOHF                
000054*    E'TRACKER: 10263222 2015      FORCE TO END ORDER REG                 
000055*                                                                         
000056                                                                          
000057 ENVIRONMENT DIVISION.                                                    
000058                                                                          
000059 DATA DIVISION.                                                           
000060                                                                          
000061     EJECT                                                                
000062 WORKING-STORAGE SECTION.                                                 
000063*    -COPY WY2000W1                                                       
000064                                                                          
000065 77  IDPGM                       PIC X(08)   VALUE 'W4022300'.            
000066 77  HOPP                        PIC X(1)   VALUE 'N'.                    
000067                                                                          
000068 77  WS-IDDC                     PIC X(2)   VALUE SPACE.                  
000069                                                                          
000070 77  FELTEXT                     PIC X(64)  VALUE SPACE.                  
000071 77  KDRC-DISPLAY                PIC Z(5)   VALUE ZERO.                   
000072 77  WS-PGM-POSITION             PIC X(24)  VALUE SPACE.                  
000073 77  RKOD-ABEND                  PIC S9(4)  VALUE +33   COMP SYNC.        
000074 77  RKOD-ABEND-MED-DUMP         PIC S9(4)  VALUE +1000 COMP SYNC.        
000075 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
000076 77  INDX                        PIC S9(9)  VALUE +0    COMP SYNC.        
000077 77  MAX-MOD-LAENGD              PIC S9(5)  VALUE +0    COMP SYNC.        
000078 77  4221-MOD-LAENGD             PIC S9(5)  VALUE +0    COMP SYNC.        
000079 77  4221-MOD-LAENGD-ANNULL      PIC S9(5)  VALUE +44   COMP SYNC.        
000080 77  WS-RADER                    PIC 9(2)   VALUE ZERO.                   
000081 77  4222-MID-IX                 PIC S9(9)   COMP SYNC VALUE ZERO.        
000082 77  HFAK-TAB-IX                 PIC S9(9)  VALUE +0    COMP SYNC.        
000083 77  IX-DCCLEAR-MAX              PIC S9(3)   COMP SYNC VALUE +99.         
000084 77  WS-INDEX                    PIC S9(9)   COMP SYNC VALUE ZERO.        
000085 77  WS-INDEX-MID                PIC S9(9)   COMP SYNC VALUE ZERO.        
000086 77  WS-INDEX-MID-MAX            PIC S9(9)   COMP SYNC VALUE +13.         
000087 77  WS-INDEX-MOD                PIC S9(9)   COMP SYNC VALUE ZERO.        
000088 77  WS-INDEX-MOD-MAX            PIC S9(9)   COMP SYNC VALUE +13.         
000089 77  WS-INDEX-SATS               PIC S9(9)   COMP SYNC VALUE ZERO.        
000090 77  WS-INDEX-SATS-MAX           PIC S9(9)   COMP SYNC VALUE +5.          
000091 77  WS-INDEX-WOPS               PIC S9(9)   COMP SYNC VALUE ZERO.        
000092 77  WS-INDEX-WOPS-MAX           PIC S9(9)   COMP SYNC VALUE 100.         
000093 77  WS-INDEX-CL                 PIC S9(9)   COMP SYNC VALUE ZERO.        
000094 77  WS-INDEX-CL-MAX             PIC S9(9)   COMP SYNC VALUE +2.          
000095 77  WS-RESLATT                  PIC S9(3)   VALUE +0  COMP-3.            
000096 77  SPAR-ORAD-KVSLATT           PIC S9(7)   VALUE +0  COMP-3.            
000097 77  SPAR-IDARTNR-TILLK          PIC S9(9)   VALUE +0  COMP-3.            
000098 77  SPAR-KVBEART-TILLK          PIC S9(7)   VALUE +0  COMP-3.            
000099 77  SPAR-REKSIFFR-TILLK         PIC S9(1)   VALUE +0  COMP-3.            
000100 77  SPAR-DIERS-KVOT          PIC S9(4)V9(3) VALUE +0  COMP-3.            
000101 77  SPAR-IDARTNR-40             PIC S9(9)   VALUE +0  COMP-3.            
000102 77  SPAR-IDLOPNR-40             PIC S9(3)   VALUE +0  COMP-3.            
000103 77  SPAR-IDSEKVNR-40            PIC S9(3)   VALUE +0  COMP-3.            
000104 77  WS-IDDISTR                  PIC X(4).                                
000105 77  WS-IDKUNDNR                 PIC X(6).                                
000106 77  WS-IDORDNR                  PIC X(5).                                
000107 77  WS-IDANSK                   PIC 9(3)    VALUE ZERO.                  
000108 77  WS-HFAK-REF-X10             PIC X(10).                               
000109 77  WS-IDDISTR-NUM4             PIC 9(4).                                
000110 77  WS-IDKUNDNR-NUM6            PIC 9(6).                                
000111 01  ORDERNR-TILL-4221.                                                   
000112    03  FILLER                   PIC X(15).                               
000113    03  WS-4221-ORDERNR-TEXT     PIC X(17)  VALUE SPACE.                  
000114    03  WS-IDKUNDRF              PIC X(5).                                
000115     EJECT                                                                
000116                                                                          
000117 01  WS-TIHHMMSS                 PIC 9(6)   VALUE ZERO.                   
000118 01  FILLER REDEFINES WS-TIHHMMSS.                                        
000119     03 WS-TIHHMM                PIC 9(4).                                
000120     03 FILLER                   PIC 9(2).                                
000121     EJECT                                                                
000122                                                                          
000123 77  ALLT-SW                     PIC X       VALUE 'J'.                   
000124     88  ALLT-OK                             VALUE 'J'.                   
000125 77  NYCKEL-SW                   PIC X       VALUE 'J'.                   
000126     88  NYCKEL-OK                           VALUE 'J'.                   
000127 77  AKT-SIDA-SW                 PIC X       VALUE 'N'.                   
000128     88  AKT-SIDA                            VALUE 'J'.                   
000129 77  AVSLUTA-SW                  PIC X       VALUE 'N'.                   
000130     88  AVSLUTA                             VALUE 'J'.                   
000131 77  START-4222-SW               PIC X       VALUE 'N'.                   
000132     88  START-4222                          VALUE 'J'.                   
000133 77  NEXT-SATS-SW                PIC X       VALUE 'N'.                   
000134     88  NEXT-SATS                           VALUE 'J'.                   
000135                                                                          
000136 77  BEVARREF-I-HFAK-TAB-SW      PIC X       VALUE 'N'.                   
000137     88  BEVARREF-I-HFAK-TAB                 VALUE 'J'.                   
000138                                                                          
000139 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
000140     88  EGEN-MID                            VALUE '4223'.                
000141     88  GODK-MID                            VALUE '4221' '4222'          
000142                                                   '4223' '4224'.         
000143                                                                          
000144 01  WS-ALFA-1.                                                           
000145     03  WS-NUM-1                PIC 9(1).                                
000146 01  WS-ALFA-5.                                                           
000147     03  WS-NUM-5                PIC 9(5).                                
000148 01  WS-ALFA-6.                                                           
000149     03  WS-NUM-6                PIC 9(6).                                
000150 01  WS-ALFA-9.                                                           
000151     03  WS-NUM-9                PIC 9(9).                                
000152 01  WS-ALFA-10.                                                          
000153     03  WS-NUM-10               PIC 9(10).                               
000154                                                                          
000155 01  WS-IDARTNR-REKSIFFR.                                                 
000156     03  WS-IDARTNR              PIC 9(9).                                
000157     03  FILLER                  PIC X(1)   VALUE '-'.                    
000158     03  WS-REKSIFFR             PIC 9(1).                                
000159                                                                          
000160 01  WS-AKTUELL-MID-RAD.                                                  
000161     03  WS-AKT-KDORDBEK         PIC 9(2).                                
000162     03  WS-AKT-KDBEHX           PIC X(1).                                
000163     03  WS-AKT-IDARTNR          PIC 9(9).                                
000164     03  WS-AKT-FILLER           PIC X(1).                                
000165     03  WS-AKT-REKSIFFR         PIC 9(1).                                
000166     03  WS-AKT-IDDC             PIC X(2).                                
000167     03  WS-AKT-IDKUNDRF-RO      PIC X(7).                                
000168     03  WS-AKT-KEYS.                                                     
000169         05 WS-AKT-IDLOPNR       PIC 9(3).                                
000170         05 WS-AKT-IDSEKVNR      PIC 9(3).                                
000171         05 WS-AKT-IDARTNR-URS   PIC 9(9).                                
000172         05 WS-AKT-IDLOPNR-RO    PIC 9(3).                                
000173                                                                          
000174 01  WS-IDDC-AKT                 PIC X(2).                                
000175                                                                          
000176 01  WS-IDARTNR-SATS-TAB.                                                 
000177     05 WS-IDARTNR-SATS          PIC X(9)    OCCURS 5.                    
000178                                                                          
000179 01  WS-KEYS-SPAR.                                                        
000180     03 WS-IDLOPNR-SPAR          PIC 9(3).                                
000181     03 WS-IDSEKVNR-SPAR         PIC 9(3).                                
000182     03 WS-IDARTNR-URS-SPAR      PIC 9(9).                                
000183     03 WS-IDARTNR-SPAR          PIC 9(9).                                
000184     03 WS-IDDC-SPAR             PIC X(2).                                
000185     EJECT                                                                
000186*----> TABELL FÖR ATT ÖVERSÄTTA HF-AK-PLOCK                               
000187                                                                          
000188*   -COPY W413WHFA                                                        
000189     EJECT                                                                
000190                                                                          
000191 01  TEST-IDDISTR                PIC  9(5)   COMP-3.                      
000192 01  FILLER  REDEFINES  TEST-IDDISTR.                                     
000193*    03  -COPY WWDIST07                                                   
000194 01  FILLER  REDEFINES  TEST-IDDISTR.                                     
000195*    ----DIST79-DEALER-PRICE-----                                         
000196*    03  -COPY WWDIST79                                                   
000197     EJECT                                                                
000198                                                                          
000199*01  -COPY WWPRODSL                                                       
000200                                                                          
000201*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
000202 01  GENERELLA-SUBPROGRAM.                                                
000203     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
000204     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
000205     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
000206     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
000207     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
000208     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
000209*                                                                         
000210*                                                                         
000211*                                                                         
000212 01  GEMENSAMMA-SUBPROGRAM.                                               
000213     03  W411AREG                PIC X(8)    VALUE 'W411AREG'.            
000214*        LÄSNING ARTIKELREGISTER                                          
000215     03  W411DLEV                PIC X(8)    VALUE 'W411DLEV'.            
000216*        KONTROLL DIREKTLEVERANS                                          
000217     03  W411DNOT                PIC X(8)    VALUE 'W411DNOT'.            
000218*        DATA TILL DELIVERYNOTE NDC                                       
000219     03  W411LAST                PIC X(8)    VALUE 'W411LAST'.            
000220*        KONTROLL ENHETSLAST                                              
000221     EJECT                                                                
000222     03  W411CDCA                PIC X(8)    VALUE 'W411CDCA'.            
000223*        KONTROLL PRELIMINÄRAVBOKNING                                     
000224     03  W411RANS                PIC X(8)    VALUE 'W411RANS'.            
000225*        BERÄKNA RANSONERING                                              
000226     03  W411TPO2                PIC X(8)    VALUE 'W411TPO2'.            
000227*        KONTROLL/UPPDATERING TPO2                                        
000228     03  W411TPO6                PIC X(8)    VALUE 'W411TPO6'.            
000229*        UPPDATERING TPO6                                                 
000230     03  W413ADRS                PIC X(8)    VALUE 'W413ADRS'.            
000231*        JUSTERING LAGERPLATS OCH LAGEROMRÅDE                             
000232     03  W413AVSR                PIC X(8)    VALUE 'W413AVSR'.            
000233*        WOPS PER RAD                                                     
000234     03  W335PRQU                PIC X(8)    VALUE 'W335PRQU'.            
000235*        PRISFRÅGA                                                        
000236     03  W335PRNO                PIC X(8)    VALUE 'W335PRNO'.            
000237*        PRISFRÅGA                                                        
000238     EJECT                                                                
000239*   -COPY W402W001                                                        
000240     EJECT                                                                
000241*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
000242*   -COPY WMSGINIT                                                        
000243     EJECT                                                                
000244*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
000245 01  MESSAGE-CODES.                                                       
000246     03  MED-FLER-SIDOR          PIC X(3)    VALUE '105'.                 
000247     03  MED-UPPLYSN-UPPDAT-PF   PIC X(3)    VALUE '144'.                 
000248     03  MED-EJ-FLER-RADER       PIC X(3)    VALUE '056'.                 
000249     03  MED-ORDER-ANNULLERAD    PIC X(3)    VALUE '052'.                 
000250     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
000251     03  ERR-OBEHORIG            PIC X(3)    VALUE '405'.                 
000252     03  ERR-ORDER-SAKNAS        PIC X(3)    VALUE '701'.                 
000253     03  ERR-ORDER-AVSLUTAD      PIC X(3)    VALUE '057'.                 
000254     03  ERR-UPPLYSTA-FEL        PIC X(3)    VALUE '409'.                 
000255     03  ERR-EJ-ANNULLATION      PIC X(3)    VALUE '066'.                 
000256     03  ERR-FEL-BILDSERIE       PIC X(3)    VALUE '093'.                 
000257     SKIP2                                                                
000258*   -COPY WMEDAREA                                                        
000259     EJECT                                                                
000260*    --- PARAMETRAR TILL GEMENSAMMA SUBPROGRAM                            
000261 01 FILLER                       PIC X(8)    VALUE 'W411AREG'.            
000262*   -COPY W411AREG                                                        
000263     EJECT                                                                
000264 01 FILLER                       PIC X(8)    VALUE 'W411DLEV'.            
000265*   -COPY W411DLEV                                                        
000266     EJECT                                                                
000267 01 FILLER                       PIC X(8)    VALUE 'W411DNOT'.            
000268*   -COPY W411DNOT                                                        
000269     EJECT                                                                
000270 01 FILLER                       PIC X(8)    VALUE 'W411LAST'.            
000271*   -COPY W411LAST                                                        
000272     EJECT                                                                
000273 01 FILLER                       PIC X(8)    VALUE 'W411CDCA'.            
000274*   -COPY W411CDCA                                                        
000275     EJECT                                                                
000276 01 FILLER                       PIC X(8)    VALUE 'W411RANS'.            
000277*   -COPY W411RANS                                                        
000278     EJECT                                                                
000279 01 FILLER                       PIC X(8)    VALUE 'W411TPO2'.            
000280*   -COPY W411TPO2                                                        
000281     EJECT                                                                
000282 01 FILLER                       PIC X(8)    VALUE 'W411TPO6'.            
000283*   -COPY W411TPO6                                                        
000284     EJECT                                                                
000285 01 FILLER                       PIC X(8)    VALUE 'W413AVSR'.            
000286*   -COPY W413AVSR                                                        
000287     EJECT                                                                
000288 01 FILLER                       PIC X(8)    VALUE 'W413ADRS'.            
000289*   -COPY W413ADRS                                                        
000290     EJECT                                                                
000291 01 FILLER                       PIC X(8)    VALUE 'W335PRNO'.            
000292*   -COPY W335PRNO                                                        
000293     EJECT                                                                
000294 01 FILLER                       PIC X(8)    VALUE 'W335PRQU'.            
000295*   -COPY W335PRQU                                                        
000296     EJECT                                                                
000297*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
000298*                                                                         
000299 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
000300     SKIP3                                                                
000301*01  MID -COPY W4I22301                                                   
000302     EJECT                                                                
000303 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
000304     SKIP3                                                                
000305*01  -COPY WMSGAREA                                                       
000306     EJECT                                                                
000307*    03  MOD -COPY W4O22101   -RED MSG-AREA  -PRE 4221-.                  
000308     EJECT                                                                
000309*    03  MOD -COPY W4O22301   -RED MSG-AREA.                              
000310     EJECT                                                                
000311******************************************************************        
000312*    MID-AREA FÖR W2T191                                         *        
000313******************************************************************        
000314*01  -COPY  W2I19101  -PRE 2191-                                          
000315     EJECT                                                                
000316 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
000317     SKIP3                                                                
000318*01  -COPY WMFSAREA                                                       
000319     EJECT                                                                
000320*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
000321 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
000322 01  NYCKLAR-TILL-DLI.                                                    
000323     03  W-WDQ101-KEY-UNIK.                                               
000324         05  W-Q1-IDORDER-UNIK   PIC S9(7)   VALUE ZERO COMP-3.           
000325         05  W-Q1-IDARTNR-UNIK   PIC S9(9)   VALUE ZERO COMP-3.           
000326         05  W-Q1-IDLOPNR-UNIK   PIC S9(3)   VALUE ZERO COMP-3.           
000327         05  W-Q1-IDSEKVNR-UNIK  PIC S9(3)   VALUE ZERO COMP-3.           
000328         05  W-Q1-IDDC-UNIK      PIC  X(2)   VALUE ZERO.                  
000329         05  W-Q1-KDORDBEK-UNIK  PIC 9(2)    VALUE ZERO.                  
000330                                                                          
000331     03  W-WDQ101-KEY-MIN.                                                
000332         05  W-Q1-IDORDER-MIN    PIC S9(7)   VALUE ZERO COMP-3.           
000333         05  W-Q1-IDARTNR-MIN    PIC S9(9)   VALUE ZERO COMP-3.           
000334         05  W-Q1-IDLOPNR-MIN    PIC S9(3)   VALUE ZERO COMP-3.           
000335         05  W-Q1-IDSEKVNR-MIN   PIC S9(3)   VALUE ZERO COMP-3.           
000336         05  W-Q1-IDDC-MIN       PIC  X(2)   VALUE ZERO.                  
000337         05  W-Q1-KDORDBEK-MIN   PIC 9(2)    VALUE ZERO.                  
000338     03  W-WDQ101-KEY-MAX.                                                
000339         05  W-Q1-IDORDER-MAX    PIC S9(7) VALUE 9999999 COMP-3.          
000340         05  W-Q1-IDARTNR-MAX    PIC S9(9) VALUE 999999999 COMP-3.        
000341         05  W-Q1-IDLOPNR-MAX    PIC S9(3) VALUE 999  COMP-3.             
000342         05  W-Q1-IDSEKVNR-MAX   PIC S9(3) VALUE 999  COMP-3.             
000343         05  W-Q1-IDDC-MAX       PIC  X(2) VALUE '99'.                    
000344         05  W-Q1-KDORDBEK-MAX   PIC 9(2)  VALUE 99.                      
000345                                                                          
000346     03  W-WDQ101-KEY-MIN1.                                               
000347         05  W-Q1-IDORDER-MIN1   PIC S9(7)   COMP-3.                      
000348         05  W-Q1-IDARTNR-MIN1   PIC S9(9)   COMP-3.                      
000349         05  W-Q1-IDLOPNR-MIN1   PIC S9(3)   COMP-3.                      
000350         05  W-Q1-IDSEKVNR-MIN1  PIC S9(3)   COMP-3.                      
000351         05  FILLER              PIC X(4)    VALUE LOW-VALUE.             
000352     03  W-WDQ101-KEY-MAX1.                                               
000353         05  W-Q1-IDORDER-MAX1   PIC S9(7)   COMP-3.                      
000354         05  W-Q1-IDARTNR-MAX1   PIC S9(9)   COMP-3.                      
000355         05  W-Q1-IDLOPNR-MAX1   PIC S9(3)   COMP-3.                      
000356         05  W-Q1-IDSEKVNR-MAX1  PIC S9(3)   COMP-3.                      
000357         05  FILLER              PIC X(4)    VALUE HIGH-VALUE.            
000358     EJECT                                                                
000359                                                                          
000360     03  W-WDJ1CSEQ-X.                                                    
000361         05  W-J1-IDLEVNR        PIC X(5)    VALUE SPACE.                 
000362         05  FILLER              PIC X(30)   VALUE SPACE.                 
000363         05  W-J1-IDARTNR        PIC S9(9)   VALUE +0  COMP-3.            
000364                                                                          
000365     03  W-IDLEVNR-X             PIC X(5)    VALUE '1002 '.               
000366                                                                          
000367     03  W-WDA5KEY-X.                                                     
000368         05  W-A5-IDDISTR        PIC S9(5)   VALUE ZERO COMP-3.           
000369         05  W-A5-IDKUNDNR       PIC S9(7)   VALUE ZERO COMP-3.           
000370         05  W-A5-IDKUNDRF       PIC X(10)   VALUE SPACE.                 
000371         05  W-A5-IDARTNR        PIC S9(9)   VALUE ZERO COMP-3.           
000372         05  W-A5-IDLOPNR        PIC S9(3)   VALUE ZERO COMP-3.           
000373     03  W-WDA5KEY-MIN-X.                                                 
000374         05  W-A5-IDDISTR-MIN    PIC S9(5)   VALUE ZERO COMP-3.           
000375         05  W-A5-IDKUNDNR-MIN   PIC S9(7)   VALUE ZERO COMP-3.           
000376         05  W-A5-IDKUNDRF-MIN   PIC X(10)   VALUE SPACE.                 
000377         05  FILLER              PIC X(7)    VALUE LOW-VALUE.             
000378     03  W-WDA5KEY-MAX-X.                                                 
000379         05  W-A5-IDDISTR-MAX    PIC S9(5)   VALUE ZERO COMP-3.           
000380         05  W-A5-IDKUNDNR-MAX   PIC S9(7)   VALUE ZERO COMP-3.           
000381         05  W-A5-IDKUNDRF-MAX   PIC X(10)   VALUE SPACE.                 
000382         05  FILLER              PIC X(7)    VALUE HIGH-VALUE.            
000383                                                                          
000384     03  W-IDGMTREF-X.                                                    
000385         05  W-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.           
000386         05  W-IDKUNDNR          PIC S9(7)   VALUE ZERO COMP-3.           
000387         05  W-IDKUNDRF.                                                  
000388            07  W-IDORDNR        PIC 9(7)    VALUE ZERO.                  
000389            07  FILLER           PIC X(3)    VALUE SPACE.                 
000390     03  W-WDE801KY-X.                                                    
000391         05  W-E8-IDDISTR        PIC S9(5)   VALUE ZERO COMP-3.           
000392         05  W-E8-IDKUNDNR       PIC S9(7)   VALUE ZERO COMP-3.           
000393         05  W-E8-IDKUNDRF.                                               
000394            07  W-E8-IDORDNR     PIC 9(7)    VALUE ZERO.                  
000395            07  FILLER           PIC X(3)    VALUE SPACE.                 
000396     EJECT                                                                
000397                                                                          
000398     03  W-WDB201KEY-X.                                                   
000399         05  W-WDB2-IDDISTR      PIC S9(5) COMP-3 VALUE +0.               
000400         05  W-WDB2-IDKUNDNR     PIC S9(7) COMP-3 VALUE +0.               
000401                                                                          
000402     03  W-IDDC-X.                                                        
000403         05  W-IDDC              PIC  X(2)   VALUE SPACE.                 
000404     03  W-IDLAND-X.                                                      
000405         05  W-IDLAND            PIC  X(2)   VALUE SPACE.                 
000406     03  W-IDARTNR-X.                                                     
000407         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
000408     03  W-IDSKYLT-X.                                                     
000409         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
000410     03  W-WDGXKEY-4541-X.                                                
000411         05  W-IDHTYP            PIC X(04)   VALUE '4541'.                
000412         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
000413     SKIP3                                                                
000414                                                                          
000415     03  W-IDDC-B6-X.                                                     
000416         05 W-IDDC-B6                  PIC X(2).                          
000417     EJECT                                                                
000418*    --- STATUS-KOD FRÅN IMS                                              
000419 01  STATUS-WS                   PIC XX.                                  
000420     88  SEGMENT-FINNS                       VALUE '  '.                  
000421     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
000422     88  BASEN-SLUT                          VALUE 'GB'.                  
000423     SKIP2                                                                
000424 77  STATUS-OBKR-WS              PIC X(2)    VALUE 'GE'.                  
000425     88  OBKR-SEGMENT-FINNS                  VALUE '  '.                  
000426     SKIP2                                                                
000427 01  GODK-STATUSKODER.                                                    
000428     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
000429     SKIP3                                                                
000430 01  SSA1                        PIC X(160).                              
000431 01  SSA2                        PIC X(96).                               
000432 01  SSA3                        PIC X(96).                               
000433                                                                          
000434     EJECT                                                                
000435*    --- IMS FUNKTIONSKODER                                               
000436*01  -COPY W0003                                                          
000437     EJECT                                                                
000438*    ---  DLI INPUT-OUTPUT AREA                                           
000439 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
000440     SKIP3                                                                
000441 01  FILLER                      PIC X(16)   VALUE 'WDQ101-AREA'.         
000442 01  DLI-IO-AREA-ORQM.                                                    
000443     03  WLORQM01.                                                        
000444*        05  -COPY WDQ101                                                 
000445     EJECT                                                                
000446 01  FILLER                      PIC X(16)   VALUE 'WDQ201-AREA'.         
000447 01  DLI-IO-AREA-ORQI01.                                                  
000448     03  WLORQI01.                                                        
000449*        05  -COPY WDQ201                                                 
000450     EJECT                                                                
000451 01  FILLER                      PIC X(16)   VALUE 'WDQ212-AREA'.         
000452 01  DLI-IO-AREA-ORQI12.                                                  
000453     03  WLORQI12.                                                        
000454*        05  -COPY WDQ212                                                 
000455     EJECT                                                                
000456 01  FILLER                      PIC X(16)   VALUE 'WDQ401-AREA'.         
000457 01  DLI-IO-AREA-ORQF.                                                    
000458     03  WLORQF01.                                                        
000459*        05  -COPY WDQ401                                                 
000460     EJECT                                                                
000461 01  FILLER                      PIC X(16)   VALUE 'WDE801-AREA'.         
000462 01  DLI-IO-AREA-PROC.                                                    
000463     03  WLPROC01.                                                        
000464*        05  -COPY WDE801                                                 
000465     EJECT                                                                
000466 01  FILLER                      PIC X(16)   VALUE 'WDK901-AREA'.         
000467 01  DLI-IO-AREA-ARTM.                                                    
000468     03  WLARTM01.                                                        
000469*        05  -COPY WDK901                                                 
000470     EJECT                                                                
000471 01  FILLER                      PIC X(16)   VALUE 'WDK711-AREA'.         
000472 01  DLI-IO-AREA-WDK7.                                                    
000473     03  WDK711.                                                          
000474*        05  -COPY WDK711                                                 
000475     EJECT                                                                
000476 01  FILLER                      PIC X(16)   VALUE 'WDK712-AREA'.         
000477 01  DLI-IO-AREA-WDK712.                                                  
000478     03  WDK712.                                                          
000479*        05  -COPY WDK712                                                 
000480     EJECT                                                                
000481 01  FILLER                      PIC X(16)   VALUE 'WDK722-AREA'.         
000482 01  DLI-IO-WDK722.                                                       
000483     03  WDK722.                                                          
000484*        05  -COPY WDK722                                                 
000485     EJECT                                                                
000486 01  FILLER                      PIC X(16)   VALUE 'WDD311-AREA'.         
000487 01  DLI-IO-AREA-BENA.                                                    
000488     03  WLBENA11.                                                        
000489*        05  -COPY WDD311                                                 
000490     EJECT                                                                
000491 01  FILLER                      PIC X(16)   VALUE 'WDA501-AREA'.         
000492 01  DLI-IO-AREA-ORDP.                                                    
000493     03  WLORDP01.                                                        
000494*        05  -COPY WDA501                                                 
000495     EJECT                                                                
000496 01  FILLER                      PIC X(16)   VALUE 'WDJ1  -AREA'.         
000497 01  DLI-IO-AREA-SATB.                                                    
000498     03  WLSATB11.                                                        
000499*        05  -COPY WDJ111                                                 
000500     03  WLSATB01.                                                        
000501*        05  -COPY WDJ101                                                 
000502     EJECT                                                                
000503 01  FILLER                      PIC X(16)   VALUE 'VOR-KÖ-AREA'.         
000504 01  DLI-IO-AREA-4541.                                                    
000505     03  WL454111.                                                        
000506*        05  -COPY WDGX4542                                               
000507     EJECT                                                                
000508 01  FILLER                      PIC X(16)   VALUE 'WDK611-AREA'.         
000509 01  DLI-IO-AREA-WDK611.                                                  
000510     03  WDK611.                                                          
000511*        05  -COPY WDK611                                                 
000512     EJECT                                                                
000513 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
000514 01   DLI-IO-AREA-B601.                                                   
000515*     03  -COPY WDB601                                                    
000516     EJECT                                                                
000517 01  FILLER               PIC X(16)   VALUE 'WDB201 AREA'.                
000518 01   DLI-IO-AREA-B201.                                                   
000519*     03  -COPY WDB201                                                    
000520     EJECT                                                                
000521 01  FILLER                  PIC X(16)  VALUE 'P-TO-P-SW'.                
000522 01  4223-MSG-IO-AREA.                                                    
000523     03  4223-LL               PIC S9(4)  VALUE +69  COMP SYNC.           
000524     03  4223-Z1               PIC X      VALUE LOW-VALUE.                
000525     03  4223-Z2               PIC X      VALUE LOW-VALUE.                
000526     03  4223-TRANSKOD         PIC X(8)   VALUE 'W4T223V '.               
000527     03  4223-IDTRANS          PIC X(4)   VALUE '4223'.                   
000528     03  4223-SPRAK            PIC X.                                     
000529     03  4223-IDDISTR-IN       PIC X(4).                                  
000530     03  4223-IDKUNDNR-IN      PIC X(6).                                  
000531     03  4223-IDORDNR-IN       PIC X(5).                                  
000532     03  4223-IDDISTR-UT       PIC X(4).                                  
000533     03  4223-IDKUNDNR-UT      PIC X(6).                                  
000534     03  4223-IDORDNR-UT       PIC X(5).                                  
000535     03  4223-KDORDKL-UT       PIC X      VALUE SPACE.                    
000536     03  4223-FLANNULL         PIC X      VALUE 'N'.                      
000537     03  FILLER                PIC X(20)  VALUE ZERO.                     
000538                                                                          
000539 01  4292-MSG-IO-AREA.                                                    
000540     03  4292-LL               PIC S9(4)  VALUE +47  COMP SYNC.           
000541     03  4292-Z1               PIC X.                                     
000542     03  4292-Z2               PIC X.                                     
000543     03  4292-TRANSKOD         PIC X(8)   VALUE 'W4T292X '.               
000544     03  4292-IDTRANS          PIC X(4)   VALUE '4223'.                   
000545     03  4292-SPRAK            PIC X.                                     
000546     03  4292-IDORDER          PIC X(7).                                  
000547     03  4292-IDDISTR          PIC X(4).                                  
000548     03  4292-IDKUNDNR         PIC X(6).                                  
000549     03  4292-IDKUNDRF         PIC X(7).                                  
000550     03  FILLER                PIC X(6)   VALUE SPACE.                    
000551     EJECT                                                                
000552                                                                          
000553 01  FILLER                  PIC X(16)  VALUE '4298-MSG-IO-AREA'.         
000554 01  4298-MSG-IO-AREA.                                                    
000555     03  4298-LL               PIC S9(4)  VALUE +0 COMP SYNC.             
000556     03  4298-Z1               PIC X.                                     
000557     03  4298-Z2               PIC X.                                     
000558     03  4298-TRANSKOD         PIC X(8)   VALUE 'W4T298X '.               
000559     03  4298-IDTRANS          PIC X(4)   VALUE '4223'.                   
000560     03  4298-SPRAK            PIC X      VALUE SPACE.                    
000561*    03  -COPY W4I29801  -PRE 4298-                                       
000562     EJECT                                                                
000563 01  FILLER                  PIC X(16)  VALUE '4222-MSG-IO-AREA'.         
000564 01  4222-MSG-IO-AREA.                                                    
000565     03  4222-LL               PIC S9(4)  VALUE +1080 COMP SYNC.          
000566     03  4222-Z1               PIC X.                                     
000567     03  4222-Z2               PIC X.                                     
000568     03  4222-TRANSKOD         PIC X(8)   VALUE 'W4T222U '.               
000569     03  4222-IDTRANS          PIC X(4)   VALUE '4223'.                   
000570     03  4222-SPRAK            PIC X.                                     
000571     03  -COPY W4I22201  -PRE 4222-                                       
000572     EJECT                                                                
000573 01  FILLER                  PIC X(16)  VALUE '4224-MSG-IO-AREA'.         
000574 01  4224-MSG-IO-AREA.                                                    
000575     03  4224-LL               PIC S9(4)  VALUE +262 COMP SYNC.           
000576     03  4224-Z1               PIC X      VALUE LOW-VALUE.                
000577     03  4224-Z2               PIC X      VALUE LOW-VALUE.                
000578     03  4224-TRANSKOD         PIC X(8)   VALUE 'W4T224  '.               
000579     03  4224-IDTRANS          PIC X(4)   VALUE '4223'.                   
000580     03  4224-SPRAK            PIC X.                                     
000581     03  MID -COPY W4I22401  -PRE 4224-                                   
000582     EJECT                                                                
000583 01  FILLER                  PIC X(16)  VALUE '4225-MSG-IO-AREA'.         
000584 01  4225-MSG-IO-AREA.                                                    
000585     03  4225-LL               PIC S9(4)  VALUE +90  COMP SYNC.           
000586     03  4225-Z1               PIC X      VALUE LOW-VALUE.                
000587     03  4225-Z2               PIC X      VALUE LOW-VALUE.                
000588     03  4225-TRANSKOD         PIC X(8)   VALUE 'W4T225  '.               
000589     03  4225-IDTRANS          PIC X(4)   VALUE '4223'.                   
000590     03  4225-SPRAK            PIC X.                                     
000591     03  MID -COPY W4I22501  -PRE 4225-                                   
000592     EJECT                                                                
000593 01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
000594     SKIP3                                                                
000595 01  -COPY WZ01SEND                                                       
000596     EJECT                                                                
000597 01  FILLER                      PIC X(16)   VALUE 'SEND-AREA'.           
000598     SKIP3                                                                
000599 01  SEND-AREA.                                                           
000600*    03  -COPY WZ01REQU  -PRE 3039-                                       
000601*    03  -COPY W30391I1  -PRE 3039-                                       
000602     EJECT                                                                
000603 LINKAGE SECTION.                                                         
000604                                                                          
000605*01  -COPY W0009   -PRE MSG-                                              
000606                                                                          
000607 01  AVSR-ALT-PCB                PIC X.                                   
000608     EJECT                                                                
000609*01  -COPY W0009   -PRE 4292-                                             
000610     EJECT                                                                
000611*01  -COPY W0009   -PRE 4298-                                             
000612*01  -COPY W0009   -PRE 4222-                                             
000613     EJECT                                                                
000614     -COPY W0009   -PRE 4223-                                             
000615     -COPY W0009   -PRE 4224-                                             
000616     -COPY W0009   -PRE 4225-                                             
000617     EJECT                                                                
000618*    -COPY W0009   -PRE 2191-                                             
000619     EJECT                                                                
000620*01  -COPY W0008   -PRE USEA-                                             
000621     05  FILLER                  PIC X.                                   
000622     EJECT                                                                
000623*01  -COPY W0008   -PRE SATB-                                             
000624     05  FILLER                  PIC X.                                   
000625     EJECT                                                                
000626*01  -COPY W0009   -PRE PRQRY-                                            
000627     05  FILLER                  PIC X.                                   
000628     EJECT                                                                
000629*01  -COPY W0008   -PRE ARTM-                                             
000630     05  FILLER                  PIC X.                                   
000631     EJECT                                                                
000632*01  -COPY W0008   -PRE WDK7-                                             
000633     05  FILLER                  PIC X.                                   
000634     EJECT                                                                
000635*01  -COPY W0008   -PRE BENA-                                             
000636     05  FILLER                  PIC X.                                   
000637     SKIP2                                                                
000638*01  -COPY W0008   -PRE ORDP-                                             
000639     05  FILLER                  PIC X.                                   
000640     EJECT                                                                
000641*01  -COPY W0008   -PRE ORQF-                                             
000642     05  FILLER                  PIC X.                                   
000643     SKIP2                                                                
000644*01  -COPY W0008   -PRE ORQI-                                             
000645     05  FILLER                  PIC X.                                   
000646     EJECT                                                                
000647*01  -COPY W0008   -PRE ORQM-                                             
000648     05  FILLER                  PIC X.                                   
000649     EJECT                                                                
000650*01  -COPY W0008   -PRE 4541-                                             
000651     05  FILLER                  PIC X.                                   
000652     EJECT                                                                
000653*01  -COPY W0008   -PRE WDK6-                                             
000654     05  FILLER                  PIC X.                                   
000655     SKIP2                                                                
000656*01  -COPY W0008   -PRE PROC-                                             
000657     05  FILLER                  PIC X.                                   
000658     EJECT                                                                
000659*01  -COPY W0008   -PRE WDB6-                                             
000660     05  FILLER                  PIC X.                                   
000661*01  -COPY W0008   -PRE WDB2-                                             
000662     05  FILLER                  PIC X.                                   
000663     EJECT                                                                
000664 01  AREG-WDK6-PCB               PIC X.                                   
000665 01  AREG-WDK7-PCB               PIC X.                                   
000666                                                                          
000667 01  DLEV-LEVF-PCB               PIC X.                                   
000668 01  DLEV-LEVG-PCB               PIC X.                                   
000669 01  DLEV-LEVA-PCB               PIC X.                                   
000670 01  DLEV-ARTS-PCB               PIC X.                                   
000671 01  DLEV-WDB6-PCB               PIC X.                                   
000672                                                                          
000673 01  DNOT-ORQP-PCB               PIC X.                                   
000674 01  DNOT-ORQP2-PCB              PIC X.                                   
000675 01  DNOT-ORQP3-PCB              PIC X.                                   
000676 01  DNOT-4013-PCB               PIC X.                                   
000677 01  DNOT-BENA-PCB               PIC X.                                   
000678                                                                          
000679 01  CDCA-ARTM-PCB               PIC X.                                   
000680 01  CDCA-INLB-PCB               PIC X.                                   
000681 01  CDCA-WDB2-PCB               PIC X.                                   
000682 01  CDCA-WDC1-PCB               PIC X.                                   
000683                                                                          
000684 01  RANS-XXKM-PCB               PIC X.                                   
000685 01  RANS-ARTM-PCB               PIC X.                                   
000686 01  RANS-ARTS-PCB               PIC X.                                   
000687                                                                          
000688 01  TPO2-ORDP-PCB               PIC X.                                   
000689 01  TPO2-XXBU-PCB               PIC X.                                   
000690 01  TPO2-XXBV-PCB               PIC X.                                   
000691 01  TPO2-ARTM-PCB               PIC X.                                   
000692 01  TPO2-FILA-PCB               PIC X.                                   
000693 01  TPO2-XXBX-PCB               PIC X.                                   
000694                                                                          
000695 01  2109-PCB                    PIC X.                                   
000696 01  TPO6-ORDP-PCB               PIC X.                                   
000697 01  TPO6-XXBU-PCB               PIC X.                                   
000698 01  TPO6-XXBV-PCB               PIC X.                                   
000699 01  TPO6-XXBX-PCB               PIC X.                                   
000700 01  TPO6-ARTS-PCB               PIC X.                                   
000701                                                                          
000702 01  TIME-4437-PCB               PIC X.                                   
000703                                                                          
000704 01  AVSR-ORQI-PCB               PIC X.                                   
000705 01  AVSR-GMTB-PCB               PIC X.                                   
000706 01  AVSR-GMTC-PCB               PIC X.                                   
000707 01  AVSR-WDB2-PCB               PIC X.                                   
000708 01  AVSR-WDB6-PCB               PIC X.                                   
000709                                                                          
000710 01  TRAN-XXKB-PCB               PIC X.                                   
000711 01  KVAN-WDB2-PCB               PIC X.                                   
000712                                                                          
000713 01  PRNO-3107-PCB               PIC X.                                   
000714 01  PRQU-WDG2-PCB               PIC X.                                   
000715 01  PRQU-WDC7-PCB               PIC X.                                   
000716 01  PRQU-SJKO-WDK6-PCB          PIC X.                                   
000717     EJECT                                                                
000718 PROCEDURE DIVISION  USING MSG-PCB AVSR-ALT-PCB 4292-PCB                  
000719      4298-PCB 4222-PCB 4223-PCB 4224-PCB 4225-PCB                        
000720      2109-PCB 2191-PCB PRQRY-PCB                                         
000721      USEA-PCB SATB-PCB                                                   
000722      ARTM-PCB WDK7-PCB BENA-PCB ORDP-PCB ORQF-PCB ORQI-PCB               
000723      ORQM-PCB 4541-PCB                                                   
000724      WDK6-PCB PROC-PCB WDB6-PCB WDB2-PCB                                 
000725      AREG-WDK6-PCB                                                       
000726      AREG-WDK7-PCB                                                       
000727      DLEV-LEVF-PCB                                                       
000728      DLEV-LEVG-PCB                                                       
000729      DLEV-LEVA-PCB                                                       
000730      DLEV-ARTS-PCB                                                       
000731      DLEV-WDB6-PCB                                                       
000732      DNOT-ORQP-PCB                                                       
000733      DNOT-ORQP2-PCB                                                      
000734      DNOT-ORQP3-PCB                                                      
000735      DNOT-4013-PCB                                                       
000736      DNOT-BENA-PCB                                                       
000737      CDCA-ARTM-PCB CDCA-INLB-PCB CDCA-WDB2-PCB CDCA-WDC1-PCB             
000738      RANS-XXKM-PCB RANS-ARTM-PCB RANS-ARTS-PCB                           
000739      TPO2-ORDP-PCB TPO2-XXBU-PCB TPO2-XXBV-PCB                           
000740      TPO2-ARTM-PCB TPO2-FILA-PCB TPO2-XXBX-PCB                           
000741      TPO6-ORDP-PCB TPO6-XXBU-PCB TPO6-XXBV-PCB                           
000742      TPO6-XXBX-PCB TPO6-ARTS-PCB                                         
000743      TIME-4437-PCB                                                       
000744      AVSR-ORQI-PCB AVSR-GMTB-PCB AVSR-GMTC-PCB                           
000745      AVSR-WDB2-PCB AVSR-WDB6-PCB                                         
000746      TRAN-XXKB-PCB KVAN-WDB2-PCB                                         
000747      PRNO-3107-PCB                                                       
000748      PRQU-WDG2-PCB                                                       
000749      PRQU-WDC7-PCB                                                       
000750      PRQU-SJKO-WDK6-PCB.                                                 
000751 MAIN SECTION.                                                            
000752                                                                          
000753     ENTRY 'DLITCBL' USING MSG-PCB AVSR-ALT-PCB 4292-PCB                  
000754      4298-PCB 4222-PCB 4223-PCB 4224-PCB 4225-PCB                        
000755      2109-PCB 2191-PCB PRQRY-PCB                                         
000756      USEA-PCB SATB-PCB                                                   
000757      ARTM-PCB WDK7-PCB BENA-PCB ORDP-PCB ORQF-PCB ORQI-PCB               
000758      ORQM-PCB 4541-PCB                                                   
000759      WDK6-PCB PROC-PCB WDB6-PCB WDB2-PCB                                 
000760      AREG-WDK6-PCB                                                       
000761      AREG-WDK7-PCB                                                       
000762      DLEV-LEVF-PCB                                                       
000763      DLEV-LEVG-PCB                                                       
000764      DLEV-LEVA-PCB                                                       
000765      DLEV-ARTS-PCB                                                       
000766      DLEV-WDB6-PCB                                                       
000767      DNOT-ORQP-PCB                                                       
000768      DNOT-ORQP2-PCB                                                      
000769      DNOT-ORQP3-PCB                                                      
000770      DNOT-4013-PCB                                                       
000771      DNOT-BENA-PCB                                                       
000772      CDCA-ARTM-PCB CDCA-INLB-PCB CDCA-WDB2-PCB CDCA-WDC1-PCB             
000773      RANS-XXKM-PCB RANS-ARTM-PCB RANS-ARTS-PCB                           
000774      TPO2-ORDP-PCB TPO2-XXBU-PCB TPO2-XXBV-PCB                           
000775      TPO2-ARTM-PCB TPO2-FILA-PCB TPO2-XXBX-PCB                           
000776      TPO6-ORDP-PCB TPO6-XXBU-PCB TPO6-XXBV-PCB                           
000777      TPO6-XXBX-PCB TPO6-ARTS-PCB                                         
000778      TIME-4437-PCB                                                       
000779      AVSR-ORQI-PCB AVSR-GMTB-PCB AVSR-GMTC-PCB                           
000780      AVSR-WDB2-PCB AVSR-WDB6-PCB                                         
000781      TRAN-XXKB-PCB KVAN-WDB2-PCB                                         
000782      PRNO-3107-PCB                                                       
000783      PRQU-WDG2-PCB                                                       
000784      PRQU-WDC7-PCB                                                       
000785      PRQU-SJKO-WDK6-PCB.                                                 
000786                                                                          
000787     EJECT                                                                
000788     PERFORM IMS-GET-MSG                                                  
000789     IF SEGMENT-FINNS                                                     
000790        PERFORM A-INIT                                                    
000791        PERFORM B-KOLLA-NYCKLAR                                           
000792        IF NYCKEL-OK                                                      
000793           IF MFS-NEXT                                                    
000794              PERFORM C-NAESTA-SIDA                                       
000795           ELSE                                                           
000796              PERFORM D-FOERSTA-SIDA                                      
000797           END-IF                                                         
000798           IF ALLT-OK                                                     
000799              PERFORM F-KOLLA-ATT-ORDER-FINNS                             
000800              IF ALLT-OK                                                  
000801                 PERFORM E-SKRIVSKYDDA-NYCKLAR                            
000802                 IF (MFS-KDTRTYP = 'U' OR 'V')  OR MFS-ENTER              
000803                    PERFORM G-KONTROLLERA-BILDEN                          
000804                 END-IF                                                   
000805                 IF ALLT-OK                                               
000806                    PERFORM H-BEHANDLA-RADER                              
000807                    IF  AVSLUTA                                           
000808                       PERFORM I-STARTA-ORDERAVSLUT                       
000809                       IF W-IDTRANS = 'V412'                              
000810                         CONTINUE                                         
000811                       ELSE                                               
000812                         PERFORM M-HOPPA-TILL-NY-BILD                     
000813                      END-IF                                              
000814                    END-IF                                                
000815                 END-IF                                                   
000816              END-IF                                                      
000817           END-IF                                                         
000818        END-IF                                                            
000819*                                                                         
000820        IF W-IDTRANS = 'V412'                                             
000821          CONTINUE                                                        
000822        ELSE                                                              
000823          IF HOPP = NEJ                                                   
000824             PERFORM Z-FINIT                                              
000825          END-IF                                                          
000826        END-IF                                                            
000827*                                                                         
000828     END-IF                                                               
000829     MOVE +0 TO RETURN-CODE                                               
000830     GOBACK                                                               
000831     .                                                                    
000832     EJECT                                                                
000833                                                                          
000834 A-INIT SECTION.                                                          
000835                                                                          
000836     MOVE SPACE                TO MED-IDMFSFEL                            
000837                                  MED-IDMFSINF                            
000838     MOVE JA                   TO ALLT-SW                                 
000839                                  NYCKEL-SW                               
000840                                                                          
000841     IF MSG-DUBBLA-TRANSKODER                                             
000842       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I22301                 
000843       MOVE MSG-IDTRANS-2      TO MFS-IDTRANS                             
000844       MOVE MSG-KDMFSFOR-2     TO MFS-KDMFSFOR                            
000845     ELSE                                                                 
000846       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I22301                  
000847       MOVE MSG-IDTRANS-1      TO MFS-IDTRANS                             
000848       MOVE MSG-KDMFSFOR-1     TO MFS-KDMFSFOR                            
000849     END-IF                                                               
000850     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
000851     MOVE MSG-IDPFK            TO MFS-IDPFK                               
000852     MOVE MFS-IDTRANS          TO W-IDTRANS                               
000853     MOVE LOW-VALUE            TO MSG-AREA                                
000854     MOVE 'W4O22301'           TO MFS-IDMOD                               
000855     MOVE '4'                  TO MOD-IDTRANS1                            
000856     MOVE '2'                  TO MOD-IDTRANS2                            
000857     MOVE '2'                  TO MOD-IDTRANS3                            
000858     MOVE '3'                  TO MOD-IDTRANS4                            
000859     MOVE MFS-RENSA-FAELT      TO MOD-TEMFSFEL MOD-TEMFSINF               
000860                                                                          
000861     COMPUTE MAX-MOD-LAENGD = LENGTH OF MOD-W4O22301 + 4                  
000862                                                                          
000863     IF NOT EGEN-MID                                                      
000864       IF (W-IDTRANS = '4222' AND MFS-UPD-V) OR                           
000865           W-IDTRANS = 'V412'                                             
000866         CONTINUE                                                         
000867       ELSE                                                               
000868         MOVE SPACE            TO MFS-KDTRTYP                             
000869         MOVE '7'              TO MFS-IDPFK                               
000870         IF W-IDTRANS = '0813'                                            
000871            MOVE ALL '+'           TO MSGI-WMSGINIT                       
000872            MOVE '001'             TO MSGI-KDCALL                         
000873            MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                         
000874            MOVE '4223'            TO MSGI-IDTRANS                        
000875            MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                   
000876                                                                          
000877            CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                    
000878         END-IF                                                           
000879       END-IF                                                             
000880     END-IF                                                               
000881     IF ENGLISH-TEXT                                                      
000882       MOVE +2                 TO SPRAK-IX                                
000883       MOVE 'GB '              TO MED-IDSKYLT                             
000884     ELSE                                                                 
000885       MOVE +1                 TO SPRAK-IX                                
000886       MOVE 'S  '              TO MED-IDSKYLT                             
000887     END-IF                                                               
000888                                                                          
000889     PERFORM AA-NOLLA-TABELLER                                            
000890                                                                          
000891     MOVE SPACE                TO 2191-MID-W2I19101                       
000892     .                                                                    
000893                                                                          
000894 AA-NOLLA-TABELLER SECTION.                                               
000895                                                                          
000896     MOVE +1                   TO WS-INDEX-WOPS                           
000897     PERFORM UNTIL WS-INDEX-WOPS > WS-INDEX-WOPS-MAX                      
000898        MOVE +0                TO AVSR-ADLAGOMR(WS-INDEX-WOPS)            
000899        MOVE SPACE             TO AVSR-IDLEVNR(WS-INDEX-WOPS)             
000900        MOVE ZERO              TO AVSR-IDDC(WS-INDEX-WOPS)                
000901        MOVE ZERO              TO AVSR-KDSPEEMB(WS-INDEX-WOPS)            
000902        MOVE +0                TO AVSR-KVANNANT(WS-INDEX-WOPS)            
000903        MOVE +0                TO AVSR-KVBEART-Q(WS-INDEX-WOPS)           
000904        MOVE +0                TO AVSR-PRARTNTO(WS-INDEX-WOPS)            
000905        MOVE +0                TO AVSR-PRAVCOST(WS-INDEX-WOPS)            
000906        INITIALIZE             AVSR-DEAL-PR-LINE(WS-INDEX-WOPS)           
000907        MOVE +0                TO AVSR-VKART(WS-INDEX-WOPS)               
000908        MOVE +0                TO AVSR-VLARTNTO(WS-INDEX-WOPS)            
000909        MOVE SPACE             TO AVSR-KDORDSTA(WS-INDEX-WOPS)            
000910        MOVE +0                TO AVSR-KDVIA   (WS-INDEX-WOPS)            
000911        MOVE +0                TO AVSR-KVDAGAR-DIFF(WS-INDEX-WOPS)        
000912        MOVE +0                TO AVSR-TISKEPPN-DDC(WS-INDEX-WOPS)        
000913        MOVE +0                TO AVSR-KDVSOP(WS-INDEX-WOPS)              
000914                                  AVSR-KDFARLIG(WS-INDEX-WOPS)            
000915        ADD +1                 TO WS-INDEX-WOPS                           
000916     END-PERFORM                                                          
000917     MOVE +1                   TO WS-INDEX-WOPS                           
000918                                                                          
000919     MOVE +1                   TO WS-INDEX                                
000920     PERFORM UNTIL WS-INDEX > +14                                         
000921        MOVE ALL '+'           TO                                         
000922        4222-MID-W4I22201-001-GRP(WS-INDEX)                               
000923        ADD +1                 TO WS-INDEX                                
000924     END-PERFORM                                                          
000925     .                                                                    
000926     EJECT                                                                
000927 B-KOLLA-NYCKLAR SECTION.                                                 
000928                                                                          
000929     MOVE MFS-RENSA-FAELT      TO MOD-IDDISTR-IN                          
000930                                  MOD-IDKUNDNR-IN                         
000931                                  MOD-IDORDNR-IN                          
000932     IF W-IDTRANS = '0813'                                                
000933        MOVE MSGI-IDDISTR       TO MID-IDDISTR-IN                         
000934        MOVE MSGI-IDKUNDNR      TO MID-IDKUNDNR-IN                        
000935        MOVE MSGI-IDKUNDRF(3:5) TO MID-IDORDNR-IN                         
000936     END-IF                                                               
000937                                                                          
000938     IF MID-IDDISTR-IN = ALL '+'                                          
000939        MOVE MID-IDDISTR-UT    TO WS-IDDISTR                              
000940        INSPECT WS-IDDISTR REPLACING LEADING SPACE BY ZERO                
000941     ELSE                                                                 
000942        MOVE MID-IDDISTR-IN    TO WS-IDDISTR                              
000943        MOVE '7'               TO MFS-IDPFK                               
000944        MOVE SPACE             TO MFS-KDTRTYP                             
000945     END-IF                                                               
000946                                                                          
000947     IF WS-IDDISTR NUMERIC  AND  WS-IDDISTR > ZERO                        
000948        MOVE WS-IDDISTR        TO W-IDDISTR                               
000949     ELSE                                                                 
000950        MOVE NEJ               TO NYCKEL-SW                               
000951        MOVE ZERO              TO W-IDDISTR                               
000952     END-IF                                                               
000953                                                                          
000954     MOVE W-IDDISTR            TO TEST-IDDISTR                            
000955     IF DIST79-DEALER-PRICE                                               
000956        IF ENGLISH-TEXT                                                   
000957           MOVE 'DEALERPRICE'   TO MOD-TEDDI                              
000958        ELSE                                                              
000959           MOVE '    ÅF PRIS'   TO MOD-TEDDI                              
000960        END-IF                                                            
000961     ELSE                                                                 
000962        MOVE SPACES            TO MOD-TEDDI                               
000963     END-IF                                                               
000964                                                                          
000965     IF MID-IDKUNDNR-IN = ALL '+'                                         
000966        MOVE MID-IDKUNDNR-UT   TO WS-IDKUNDNR                             
000967        INSPECT WS-IDKUNDNR REPLACING LEADING SPACE BY ZERO               
000968     ELSE                                                                 
000969        MOVE MID-IDKUNDNR-IN   TO WS-IDKUNDNR                             
000970        MOVE '7'               TO MFS-IDPFK                               
000971        MOVE SPACE             TO MFS-KDTRTYP                             
000972     END-IF                                                               
000973                                                                          
000974     IF WS-IDKUNDNR NUMERIC                                               
000975        MOVE WS-IDKUNDNR       TO W-IDKUNDNR                              
000976     ELSE                                                                 
000977        MOVE NEJ               TO NYCKEL-SW                               
000978     END-IF                                                               
000979     EJECT                                                                
000980     IF MID-IDORDNR-IN = ALL '+'                                          
000981        MOVE MID-IDORDNR-UT    TO WS-IDORDNR                              
000982        INSPECT WS-IDORDNR REPLACING LEADING SPACE BY ZERO                
000983     ELSE                                                                 
000984        MOVE MID-IDORDNR-IN    TO WS-IDORDNR                              
000985        MOVE '7'               TO MFS-IDPFK                               
000986        MOVE SPACE             TO MFS-KDTRTYP                             
000987     END-IF                                                               
000988     IF WS-IDORDNR NUMERIC  AND WS-IDORDNR > ZERO                         
000989        MOVE WS-IDORDNR        TO W-IDORDNR                               
000990     ELSE                                                                 
000991        MOVE NEJ               TO NYCKEL-SW                               
000992     END-IF                                                               
000993                                                                          
000994     IF GODK-MID OR NYCKEL-OK                                             
000995        MOVE WS-IDKUNDNR          TO MOD-IDKUNDNR-UT                      
000996        INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE           
000997        IF WS-IDKUNDNR = ZERO                                             
000998           MOVE '     0'          TO MOD-IDKUNDNR-UT                      
000999        END-IF                                                            
001000        MOVE WS-IDDISTR           TO MOD-IDDISTR-UT                       
001001        INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE            
001002        MOVE WS-IDORDNR           TO MOD-IDORDNR-UT                       
001003        INSPECT MOD-IDORDNR-UT REPLACING LEADING ZERO BY SPACE            
001004     ELSE                                                                 
001005        MOVE MFS-RENSA-FAELT   TO MOD-IDDISTR-UT                          
001006                                  MOD-IDKUNDNR-UT                         
001007                                  MOD-IDORDNR-UT                          
001008     END-IF                                                               
001009                                                                          
001010     IF NOT NYCKEL-OK                                                     
001011        MOVE ERR-WRONG-KEY     TO MED-IDMFSFEL                            
001012        PERFORM MFS-RENSA-ALLA-FAELT                                      
001013     END-IF                                                               
001014     .                                                                    
001015     EJECT                                                                
001016                                                                          
001017 C-NAESTA-SIDA SECTION.                                                   
001018                                                                          
001019     IF MID-IDARTNR-NEXT NUMERIC AND                                      
001020        MID-IDARTNR-NEXT > ZERO                                           
001021        MOVE MID-IDARTNR-NEXT  TO W-Q1-IDARTNR-MIN                        
001022        MOVE MID-IDLOPNR-NEXT  TO W-Q1-IDLOPNR-MIN                        
001023        MOVE MID-IDSEKVNR-NEXT TO W-Q1-IDSEKVNR-MIN                       
001024        MOVE MID-IDDC-NEXT     TO W-Q1-IDDC-MIN                           
001025        MOVE MID-KDORDBEK-NEXT TO W-Q1-KDORDBEK-MIN                       
001026     ELSE                                                                 
001027        MOVE MED-EJ-FLER-RADER TO MED-IDMFSFEL                            
001028        MOVE NEJ               TO ALLT-SW                                 
001029     END-IF                                                               
001030     .                                                                    
001031                                                                          
001032 D-FOERSTA-SIDA SECTION.                                                  
001033                                                                          
001034     MOVE ZERO                 TO W-Q1-IDARTNR-MIN                        
001035                                  W-Q1-IDLOPNR-MIN                        
001036                                  W-Q1-IDSEKVNR-MIN                       
001037                                  W-Q1-KDORDBEK-MIN                       
001038     MOVE SPACE                TO W-Q1-IDDC-MIN                           
001039     IF MID-IDARTNR-NEXT NUMERIC AND                                      
001040        MID-IDARTNR-NEXT > ZERO                                           
001041        MOVE MID-IDARTNR-NEXT  TO MOD-IDARTNR-NEXT                        
001042        MOVE MID-IDLOPNR-NEXT  TO MOD-IDLOPNR-NEXT                        
001043        MOVE MID-IDSEKVNR-NEXT TO MOD-IDSEKVNR-NEXT                       
001044        MOVE MID-IDDC-NEXT     TO MOD-IDDC-NEXT                           
001045        MOVE MID-KDORDBEK-NEXT TO MOD-KDORDBEK-NEXT                       
001046     END-IF                                                               
001047     .                                                                    
001048     EJECT                                                                
001049                                                                          
001050 E-SKRIVSKYDDA-NYCKLAR SECTION.                                           
001051                                                                          
001052     MOVE MFS-STAENG-FAELT     TO MOD-IDTRANS1-ATTR                       
001053                                  MOD-IDTRANS2-ATTR                       
001054                                  MOD-IDTRANS3-ATTR                       
001055                                  MOD-IDTRANS4-ATTR                       
001056                                  MOD-IDDISTR-IN-ATTR                     
001057                                  MOD-IDKUNDNR-IN-ATTR                    
001058                                  MOD-IDORDNR-IN-ATTR                     
001059     MOVE MFS-ADD-SAETT-CURSOR TO MOD-FLANNULL-ATTR                       
001060     .                                                                    
001061     EJECT                                                                
001062                                                                          
001063 F-KOLLA-ATT-ORDER-FINNS SECTION.                                         
001064                                                                          
001065     PERFORM IMS-09-GU-ORQI-WDQ201                                        
001066     IF SEGMENT-FINNS                                                     
001067                                                                          
001068        IF OHUV-FLKLAR = JA                                               
001069           MOVE ERR-ORDER-AVSLUTAD                                        
001070                               TO MED-IDMFSFEL                            
001071           MOVE NEJ            TO ALLT-SW                                 
001072           MOVE NEJ            TO NYCKEL-SW                               
001073           PERFORM MFS-RENSA-ALLA-FAELT                                   
001074        ELSE                                                              
001075           IF OHUV-IDSYSTEM NOT  = '4221' AND                             
001076              OHUV-IDSYSTEM NOT  = 'LYNV'                                 
001078              MOVE ERR-FEL-BILDSERIE                                      
001079                                  TO MED-IDMFSFEL                         
001080              MOVE NEJ            TO ALLT-SW                              
001081              MOVE NEJ            TO NYCKEL-SW                            
001082              PERFORM MFS-RENSA-ALLA-FAELT                                
001083           ELSE                                                           
001084              MOVE OHUV-KDORDKL TO MOD-KDORDKL-UT                         
001085              MOVE OHUV-IDORDER TO W-Q1-IDORDER-UNIK                      
001086                                   W-Q1-IDORDER-MIN                       
001087                                   W-Q1-IDORDER-MAX                       
001088                                   W-Q1-IDORDER-MIN1                      
001089                                   W-Q1-IDORDER-MAX1                      
001090              PERFORM FA-FIXA-LOKAL-TID                                   
001091           END-IF                                                         
001092        END-IF                                                            
001093     ELSE                                                                 
001094        MOVE ERR-ORDER-SAKNAS  TO MED-IDMFSFEL                            
001095        MOVE NEJ               TO ALLT-SW                                 
001096        MOVE NEJ               TO NYCKEL-SW                               
001097        PERFORM MFS-RENSA-ALLA-FAELT                                      
001098     END-IF                                                               
001099     .                                                                    
001100     EJECT                                                                
001101                                                                          
001102 FA-FIXA-LOKAL-TID SECTION.                                               
001103                                                                          
001104     MOVE ALL '+'           TO MSGI-WMSGINIT                              
001105     MOVE '001'             TO MSGI-KDCALL                                
001106     MOVE 'WIDDC   '        TO MSGI-IDUSER                                
001107     MOVE OHUV-IDDC-TVS     TO MSGI-IDUSER(6:2)                           
001108     MOVE '4223'            TO MSGI-IDTRANS                               
001109     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
001110                                                                          
001111     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
001112     .                                                                    
001113     EJECT                                                                
001114                                                                          
001115 G-KONTROLLERA-BILDEN SECTION.                                            
001116                                                                          
001117     MOVE JA                   TO ALLT-SW                                 
001118                                                                          
001119     IF MID-FLANNULL NOT = '+'                                            
001120        IF MID-FLANNULL = 'J' OR 'Y' OR 'N'                               
001121           IF MID-FLANNULL = 'J' OR 'Y'                                   
001122              IF OHUV-KDTPOTYP > +0                                       
001123                 MOVE NEJ                TO ALLT-SW                       
001124                 MOVE MFS-ALFA-FAELT-FEL TO MOD-FLANNULL-ATTR             
001125                 MOVE ERR-EJ-ANNULLATION TO MED-IDMFSFEL                  
001126              ELSE                                                        
001127                 MOVE W-IDDISTR  TO W-A5-IDDISTR-MIN                      
001128                                    W-A5-IDDISTR-MAX                      
001129                 MOVE W-IDKUNDNR TO W-A5-IDKUNDNR-MIN                     
001130                                    W-A5-IDKUNDNR-MAX                     
001131                 MOVE WS-IDORDNR TO W-A5-IDKUNDRF-MIN                     
001132                                    W-A5-IDKUNDRF-MAX                     
001133                 PERFORM IMS-24-GU-ORDP-WDA501                            
001134                 IF SEGMENT-FINNS                                         
001135                    MOVE NEJ                TO ALLT-SW                    
001136                    MOVE MFS-ALFA-FAELT-FEL TO MOD-FLANNULL-ATTR          
001137                    MOVE ERR-EJ-ANNULLATION TO MED-IDMFSFEL               
001138                 END-IF                                                   
001139              END-IF                                                      
001140              IF MID-FLANNULL = 'Y'                                       
001141                 MOVE JA          TO MID-FLANNULL                         
001142              END-IF                                                      
001143           END-IF                                                         
001144        ELSE                                                              
001145           MOVE NEJ                TO ALLT-SW                             
001146           MOVE MFS-ALFA-FAELT-FEL TO MOD-FLANNULL-ATTR                   
001147           MOVE ERR-UPPLYSTA-FEL   TO MED-IDMFSFEL                        
001148        END-IF                                                            
001149     ELSE                                                                 
001150        MOVE NEJ               TO MID-FLANNULL                            
001151     END-IF                                                               
001152     EJECT                                                                
001153                                                                          
001154     PERFORM GA-KONTROLLERA-KDBEHX                                        
001155     IF ALLT-OK                                                           
001156        PERFORM GB-KONTROLLERA-SAMBAND                                    
001157        IF ALLT-OK                                                        
001158           PERFORM GC-JUSTERA-KDBEHX                                      
001159        END-IF                                                            
001160     END-IF                                                               
001161     .                                                                    
001162     EJECT                                                                
001163                                                                          
001164 GA-KONTROLLERA-KDBEHX SECTION.                                           
001165                                                                          
001166     MOVE +1                   TO WS-INDEX-MID                            
001167     PERFORM UNTIL WS-INDEX-MID > WS-INDEX-MID-MAX  OR                    
001168                   MID-KDORDBEK(WS-INDEX-MID) = ZERO                      
001169                                                                          
001170        IF MID-KDBEHX(WS-INDEX-MID) = '+'                                 
001171           MOVE SPACE              TO MID-KDBEHX(WS-INDEX-MID)            
001172        END-IF                                                            
001173                                                                          
001174        MOVE MID-RAD(WS-INDEX-MID) TO WS-AKTUELL-MID-RAD                  
001175        IF WS-AKT-KDBEHX = 'B' OR 'D' OR 'A' OR 'X'                       
001176                               OR '1' OR '2' OR ' '                       
001177           EVALUATE WS-AKT-KDBEHX                                         
001178              WHEN 'A'                                                    
001179                 IF WS-AKT-KDORDBEK = 61                                  
001180                    CONTINUE                                              
001181                 ELSE                                                     
001182                    MOVE MFS-ALFA-FAELT-FEL                               
001183                               TO MOD-KDBEHX-ATTR(WS-INDEX-MID)           
001184                    MOVE NEJ TO ALLT-SW                                   
001185                 END-IF                                                   
001186              WHEN 'B'                                                    
001187                 IF WS-AKT-KDORDBEK = 21  OR 51  OR 52  OR 53  OR         
001188                        54  OR 55  OR 57  OR 58  OR 59  OR 66  OR         
001189                        67  OR 72  OR 73  OR 74  OR 75  OR                
001190                        76  OR 80  OR 81  OR 82  OR 85  OR                
001191                     ((WS-AKT-KDORDBEK = 41 OR 61) AND                    
001192                      (WS-AKT-IDARTNR  = WS-AKT-IDARTNR-URS))             
001193                    CONTINUE                                              
001194                 ELSE                                                     
001195                    MOVE MFS-ALFA-FAELT-FEL                               
001196                               TO MOD-KDBEHX-ATTR(WS-INDEX-MID)           
001197                    MOVE NEJ   TO ALLT-SW                                 
001198                 END-IF                                                   
001199              WHEN 'D'                                                    
001200                 IF WS-AKT-IDLOPNR-RO = +0                                
001201                    IF WS-AKT-KDORDBEK = 15  OR 16  OR 43                 
001202                               OR 44  OR 70  OR 95  OR 98                 
001203                               OR 99  OR 41                               
001204                       CONTINUE                                           
001205                    ELSE                                                  
001206                      MOVE MFS-ALFA-FAELT-FEL                             
001207                               TO MOD-KDBEHX-ATTR(WS-INDEX-MID)           
001208                      MOVE NEJ TO ALLT-SW                                 
001209                    END-IF                                                
001210                 ELSE                                                     
001211                    IF WS-AKT-KDORDBEK = 43  OR                           
001212                           44  OR 70  OR 98  OR 99  OR                    
001213                           41  OR 56                                      
001214                       CONTINUE                                           
001215                    ELSE                                                  
001216                      MOVE MFS-ALFA-FAELT-FEL                             
001217                               TO MOD-KDBEHX-ATTR(WS-INDEX-MID)           
001218                      MOVE NEJ TO ALLT-SW                                 
001219                    END-IF                                                
001220                 END-IF                                                   
001221              WHEN 'X'                                                    
001222                 IF WS-AKT-KDORDBEK = 10                                  
001223                    CONTINUE                                              
001224                 ELSE                                                     
001225                    MOVE MFS-ALFA-FAELT-FEL                               
001226                               TO MOD-KDBEHX-ATTR(WS-INDEX-MID)           
001227                    MOVE NEJ   TO ALLT-SW                                 
001228                 END-IF                                                   
001229              WHEN '1'                                                    
001230                 IF WS-AKT-KDORDBEK = 43  OR 44                           
001231                    CONTINUE                                              
001232                 ELSE                                                     
001233                    MOVE MFS-ALFA-FAELT-FEL                               
001234                               TO MOD-KDBEHX-ATTR(WS-INDEX-MID)           
001235                    MOVE NEJ   TO ALLT-SW                                 
001236                 END-IF                                                   
001237              WHEN '2'                                                    
001238                 IF WS-AKT-KDORDBEK = 43  OR 44                           
001239                    CONTINUE                                              
001240                 ELSE                                                     
001241                    MOVE MFS-ALFA-FAELT-FEL                               
001242                               TO MOD-KDBEHX-ATTR(WS-INDEX-MID)           
001243                    MOVE NEJ   TO ALLT-SW                                 
001244                 END-IF                                                   
001245              WHEN OTHER                                                  
001246                 IF WS-AKT-KDORDBEK = 10  OR 15  OR 16  OR                
001247                        43  OR 44  OR 70  OR 74  OR 95  OR                
001248                        98  OR 99  OR 41  OR 61  OR                       
001249                        92  OR 56  OR 26                                  
001250                    CONTINUE                                              
001251                 ELSE                                                     
001252                    MOVE MFS-ALFA-FAELT-FEL                               
001253                               TO MOD-KDBEHX-ATTR(WS-INDEX-MID)           
001254                    MOVE NEJ   TO ALLT-SW                                 
001255                 END-IF                                                   
001256           END-EVALUATE                                                   
001257        ELSE                                                              
001258           MOVE MFS-ALFA-FAELT-FEL                                        
001259                               TO MOD-KDBEHX-ATTR(WS-INDEX-MID)           
001260           MOVE NEJ            TO ALLT-SW                                 
001261        END-IF                                                            
001262                                                                          
001263        ADD +1                 TO WS-INDEX-MID                            
001264     END-PERFORM                                                          
001265     IF NOT ALLT-OK                                                       
001266        MOVE ERR-UPPLYSTA-FEL  TO MED-IDMFSFEL                            
001267     END-IF                                                               
001268     .                                                                    
001269     EJECT                                                                
001270                                                                          
001271 GB-KONTROLLERA-SAMBAND SECTION.                                          
001272                                                                          
001273     MOVE +1                   TO WS-INDEX-MID                            
001274     PERFORM UNTIL WS-INDEX-MID > WS-INDEX-MID-MAX  OR                    
001275                   MID-KDBEHX(WS-INDEX-MID) = '+'                         
001276        MOVE MID-RAD(WS-INDEX-MID)                                        
001277                               TO WS-AKTUELL-MID-RAD                      
001278        IF WS-AKT-KDBEHX = 'D'                                            
001279           PERFORM S13-HITTA-FORSTA-I-GRUPPEN                             
001280           ADD +1              TO WS-INDEX-MID                            
001281           IF WS-INDEX-MID NOT > WS-INDEX-MID-MAX                         
001282              MOVE MID-RAD(WS-INDEX-MID)                                  
001283                               TO WS-AKTUELL-MID-RAD                      
001284           END-IF                                                         
001285           PERFORM UNTIL WS-INDEX-MID > WS-INDEX-MID-MAX OR               
001286                         WS-AKT-IDARTNR NOT = WS-IDARTNR-SPAR OR          
001287                         WS-AKT-IDLOPNR NOT = WS-IDLOPNR-SPAR OR          
001288                     WS-AKT-IDARTNR-URS NOT = WS-IDARTNR-URS-SPAR         
001289              IF WS-AKT-KDBEHX = '1' OR '2'                               
001290                 MOVE NEJ      TO ALLT-SW                                 
001291                 MOVE MFS-ALFA-FAELT-FEL                                  
001292                               TO MOD-KDBEHX-ATTR(WS-INDEX-MID)           
001293                 MOVE ERR-UPPLYSTA-FEL                                    
001294                               TO MED-IDMFSFEL                            
001295              END-IF                                                      
001296              ADD +1           TO WS-INDEX-MID                            
001297              IF WS-INDEX-MID NOT > WS-INDEX-MID-MAX                      
001298                 MOVE MID-RAD(WS-INDEX-MID)                               
001299                               TO WS-AKTUELL-MID-RAD                      
001300              END-IF                                                      
001301           END-PERFORM                                                    
001302        ELSE                                                              
001303           ADD +1              TO WS-INDEX-MID                            
001304        END-IF                                                            
001305     END-PERFORM                                                          
001306     .                                                                    
001307     EJECT                                                                
001308                                                                          
001309 GC-JUSTERA-KDBEHX SECTION.                                               
001310                                                                          
001311*    JUSTERINGEN GÖRS FÖR ATT VARJE RAD SENARE I PROGRAMMET               
001312*    SKALL KUNNA BEHANDLAS VAR FÖR SIG.                                   
001313*    BORTTAG AV ORDERBEKRÄFTELSER SKULLE ANNARS BEHÖVA GÖRAS              
001314*    I MÅNGA SEKTIONER. PÅ DETTA SÄTT KOMMER ALLA BORTTAG                 
001315*    ATT GÖRAS I HEA-ANNULLERA-RAD.                                       
001316                                                                          
001317     MOVE +1                   TO WS-INDEX-MID                            
001318     PERFORM UNTIL WS-INDEX-MID > WS-INDEX-MID-MAX   OR                   
001319                   MID-KDBEHX(WS-INDEX-MID) = '+'                         
001320        MOVE MID-RAD(WS-INDEX-MID)                                        
001321                               TO WS-AKTUELL-MID-RAD                      
001322        IF WS-AKT-KDBEHX = '1' OR '2'                                     
001323           PERFORM S13-HITTA-FORSTA-I-GRUPPEN                             
001324           PERFORM GCA-D-MARKERA-1-2-RADER                                
001325        ELSE                                                              
001326           IF WS-AKT-KDBEHX = 'X'                                         
001327              IF OHUV-KDTPOTYP > +0                                       
001328                 PERFORM S13-HITTA-FORSTA-I-GRUPPEN                       
001329                 PERFORM GCB-D-MARKERA-RADER                              
001330              ELSE                                                        
001331                 ADD +1        TO WS-INDEX-MID                            
001332              END-IF                                                      
001333           ELSE                                                           
001334              IF WS-AKT-KDBEHX = 'D'                                      
001335                 PERFORM S13-HITTA-FORSTA-I-GRUPPEN                       
001336                 PERFORM GCB-D-MARKERA-RADER                              
001337              ELSE                                                        
001338                 ADD +1        TO WS-INDEX-MID                            
001339              END-IF                                                      
001340           END-IF                                                         
001341        END-IF                                                            
001342     END-PERFORM                                                          
001343     .                                                                    
001344     EJECT                                                                
001345                                                                          
001346 GCA-D-MARKERA-1-2-RADER SECTION.                                         
001347                                                                          
001348     ADD +1                    TO WS-INDEX-MID                            
001349     IF WS-INDEX-MID NOT > WS-INDEX-MID-MAX                               
001350        MOVE MID-RAD(WS-INDEX-MID)                                        
001351                               TO WS-AKTUELL-MID-RAD                      
001352     END-IF                                                               
001353     PERFORM UNTIL WS-INDEX-MID > WS-INDEX-MID-MAX OR                     
001354                   WS-AKT-IDARTNR NOT = WS-IDARTNR-SPAR OR                
001355                   WS-AKT-IDLOPNR NOT = WS-IDLOPNR-SPAR OR                
001356               WS-AKT-IDARTNR-URS NOT = WS-IDARTNR-URS-SPAR               
001357                                                                          
001358        IF WS-AKT-KDBEHX = ' '                                            
001359           IF WS-AKT-KDORDBEK = 41                                        
001360              CONTINUE                                                    
001361           ELSE                                                           
001362              MOVE 'D'         TO MID-KDBEHX(WS-INDEX-MID)                
001363           END-IF                                                         
001364        END-IF                                                            
001365        ADD +1                 TO WS-INDEX-MID                            
001366        IF WS-INDEX-MID NOT > WS-INDEX-MID-MAX                            
001367           MOVE MID-RAD(WS-INDEX-MID)                                     
001368                               TO WS-AKTUELL-MID-RAD                      
001369        END-IF                                                            
001370     END-PERFORM                                                          
001371     .                                                                    
001372     EJECT                                                                
001373                                                                          
001374 GCB-D-MARKERA-RADER SECTION.                                             
001375                                                                          
001376     ADD +1                    TO WS-INDEX-MID                            
001377     IF WS-INDEX-MID NOT > WS-INDEX-MID-MAX                               
001378        MOVE MID-RAD(WS-INDEX-MID)                                        
001379                               TO WS-AKTUELL-MID-RAD                      
001380     END-IF                                                               
001381     PERFORM UNTIL WS-INDEX-MID > WS-INDEX-MID-MAX OR                     
001382                   WS-AKT-IDARTNR NOT = WS-IDARTNR-SPAR OR                
001383                   WS-AKT-IDLOPNR NOT = WS-IDLOPNR-SPAR OR                
001384               WS-AKT-IDARTNR-URS NOT = WS-IDARTNR-URS-SPAR               
001385                                                                          
001386        IF WS-AKT-KDBEHX = ' '  AND                                       
001387                 (WS-AKT-IDDC = WS-IDDC-SPAR)                             
001388                                                                          
001389           MOVE 'D'            TO MID-KDBEHX(WS-INDEX-MID)                
001390        END-IF                                                            
001391        ADD +1                 TO WS-INDEX-MID                            
001392        IF WS-INDEX-MID NOT > WS-INDEX-MID-MAX                            
001393           MOVE MID-RAD(WS-INDEX-MID)                                     
001394                               TO WS-AKTUELL-MID-RAD                      
001395        END-IF                                                            
001396     END-PERFORM                                                          
001397     .                                                                    
001398     EJECT                                                                
001399 H-BEHANDLA-RADER SECTION.                                                
001400                                                                          
001401     PERFORM HA-LAES-ARBTABELL                                            
001402                                                                          
001403     IF MFS-FIRST  OR  MFS-NEXT                                           
001404                                                                          
001405        PERFORM HB-LAS-IN-13-RADER                                        
001406        IF  WS-INDEX-MOD = +1  AND (W-IDTRANS = '4222' OR '4297')         
001407           MOVE JA             TO AVSLUTA-SW                              
001408        END-IF                                                            
001409     ELSE                                                                 
001410        IF MID-FLANNULL NOT = JA                                          
001411           IF MFS-UPDATE  OR  MFS-QUERY                                   
001412              PERFORM HE-UPPDATERA-AKT-SIDA                               
001413              PERFORM HD-UPPDATERA-OBEH-RADER                             
001414              IF START-4222                                               
001415                MOVE 'U'         TO 4222-MID-KDTRTYP                      
001416                PERFORM S03-STARTA-RADBEHANDLINGEN                        
001417              ELSE                                                        
001418                PERFORM HB-LAS-IN-13-RADER                                
001419                IF  WS-INDEX-MOD = +1                                     
001420                   MOVE JA       TO AVSLUTA-SW                            
001421                END-IF                                                    
001422              END-IF                                                      
001423           ELSE                                                           
001424              IF MFS-UPD-V                                                
001425                IF W-IDTRANS = 'V412'                                     
001426                  CONTINUE                                                
001427                ELSE                                                      
001428                  PERFORM HE-UPPDATERA-AKT-SIDA                           
001429                END-IF                                                    
001430                PERFORM HG-UPPDATERA-RESTERANDE-RADER                     
001431              END-IF                                                      
001432           END-IF                                                         
001433        ELSE                                                              
001434           PERFORM HH-ANNULLERA-ORDER                                     
001435        END-IF                                                            
001436     END-IF                                                               
001437     .                                                                    
001438     EJECT                                                                
001439                                                                          
001440 HA-LAES-ARBTABELL SECTION.                                               
001441                                                                          
001442     MOVE OHUV-IDDC-TVS        TO W-IDDC                                  
001443                                                                          
001444     PERFORM IMS-13-GHNP-ORQI-WDQ212                                      
001445     .                                                                    
001446     EJECT                                                                
001447 HB-LAS-IN-13-RADER SECTION.                                              
001448                                                                          
001449     PERFORM MFS-RENSA-ALLA-FAELT                                         
001450     MOVE +0                   TO WS-IDARTNR-SPAR                         
001451                                  WS-IDLOPNR-SPAR                         
001452                                                                          
001453     PERFORM HBA-VISA-OBKR-OCH-SATS-RADER                                 
001454                                                                          
001455     PERFORM HBB-FIXA-BLADDRINGS-VARDEN                                   
001456                                                                          
001457     IF WS-INDEX-MOD > WS-INDEX-MOD-MAX AND                               
001458           OBKR-SEGMENT-FINNS   AND                                       
001459          (OBKR-KDORDBEK = 41 OR 61)                                      
001460                                                                          
001461        PERFORM HBC-KONTROLLERA-SIDSLUT                                   
001462     END-IF                                                               
001463     .                                                                    
001464     EJECT                                                                
001465                                                                          
001466 HBA-VISA-OBKR-OCH-SATS-RADER SECTION.                                    
001467                                                                          
001468     MOVE +1                   TO WS-INDEX-MOD                            
001469                                                                          
001470     PERFORM IMS-03-GHU-ORQM-WDQ101-MIN-MAX                               
001471     PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                           
001472              OR   WS-INDEX-MOD > WS-INDEX-MOD-MAX                        
001473                                                                          
001474        PERFORM HBAA-REDIGERA-ORDERBEKR-RAD                               
001475                                                                          
001476        IF OBKR-KDORDBEK = 57                                             
001477           PERFORM HBAB-VISA-SATS-ARTIKLAR                                
001478        END-IF                                                            
001479        MOVE OBKR-IDARTNR   TO WS-IDARTNR-SPAR                            
001480        MOVE OBKR-IDLOPNR   TO WS-IDLOPNR-SPAR                            
001481                                                                          
001482        ADD +1              TO WS-INDEX-MOD                               
001483                                                                          
001484        IF NOT NEXT-SATS                                                  
001485           PERFORM IMS-04-GHN-ORQM-WDQ101-MIN-MAX                         
001486*----------SPARA STATUSKODEN FRÅN ORDERBEKRÄFTELSE LÄSNINGEN              
001487           MOVE STATUS-WS      TO STATUS-OBKR-WS                          
001488        END-IF                                                            
001489     END-PERFORM                                                          
001490     .                                                                    
001491     EJECT                                                                
001492                                                                          
001493 HBAA-REDIGERA-ORDERBEKR-RAD SECTION.                                     
001494                                                                          
001495     MOVE OBKR-KDORDBEK        TO MOD-KDORDBEK(WS-INDEX-MOD)              
001496     IF OBKR-KDORDBEK = 41 OR 61                                          
001497        MOVE '*'               TO MOD-ASTERIX(WS-INDEX-MOD)               
001498     END-IF                                                               
001499                                                                          
001500     MOVE SPACE                TO MOD-KDBEHX(WS-INDEX-MOD)                
001501     IF OBKR-KDORDBEK = 21 OR 51 OR                                       
001502                        52 OR 53 OR 54 OR 55 OR 57 OR 58 OR 59            
001503                     OR 66 OR 67 OR 72 OR 73 OR 74 OR 75 OR 76            
001504                     OR 80 OR 81 OR 82 OR 85                              
001505        MOVE 'B'               TO MOD-KDBEHX(WS-INDEX-MOD)                
001506        MOVE MFS-STAENG-FAELT  TO MOD-KDBEHX-ATTR(WS-INDEX-MOD)           
001507     END-IF                                                               
001508     IF OBKR-KDORDBEK = 41 OR 61                                          
001509        IF OBKR-IDARTNR = WS-IDARTNR-SPAR AND                             
001510                 OBKR-IDLOPNR = WS-IDLOPNR-SPAR                           
001511           MOVE SPACE          TO MOD-KDBEHX(WS-INDEX-MOD)                
001512           MOVE MFS-STAENG-FAELT-OSYNLIGT                                 
001513                               TO MOD-KDORDBEK-ATTR(WS-INDEX-MOD)         
001514        ELSE                                                              
001515           MOVE 'B'              TO MOD-KDBEHX(WS-INDEX-MOD)              
001516           MOVE MFS-STAENG-FAELT TO MOD-KDBEHX-ATTR(WS-INDEX-MOD)         
001517        END-IF                                                            
001518     END-IF                                                               
001519     IF (OBKR-KDORDBEK = 61)  AND                                         
001520                         OBKR-IDARTNR-TILLK > +0                          
001521        MOVE 'A'               TO MOD-KDBEHX(WS-INDEX-MOD)                
001522     END-IF                                                               
001523     MOVE OBKR-IDARTNR         TO MOD-IDARTNR-1--9(WS-INDEX-MOD)          
001524     MOVE '-'                  TO MOD-STRAEK(WS-INDEX-MOD)                
001525     MOVE OBKR-REKSIFFR        TO MOD-REKSIFFR(WS-INDEX-MOD)              
001526                                                                          
001527     PERFORM S22-HAMTA-BENAMNING                                          
001528                                                                          
001529     MOVE OBKR-IDDC            TO MOD-IDDC-RAD(WS-INDEX-MOD)              
001530                                                                          
001531     IF OBKR-KDORDBEK = 10 OR 15 OR 16 OR 43 OR 44 OR 70 OR               
001532                        74 OR 95                                          
001533        MOVE OBKR-KVBEART-Q    TO MOD-KVANTAL(WS-INDEX-MOD)               
001534     ELSE                                                                 
001535        IF OBKR-KDORDBEK = 80 OR 85                                       
001536           MOVE OBKR-KVANNANT  TO MOD-KVANTAL(WS-INDEX-MOD)               
001537        ELSE                                                              
001538           IF OBKR-KDORDBEK = 92 OR 98 OR 99                              
001539              MOVE OBKR-KVPRERO TO MOD-KVANTAL(WS-INDEX-MOD)              
001540           ELSE                                                           
001541              MOVE OBKR-KVBEART TO MOD-KVANTAL(WS-INDEX-MOD)              
001542           END-IF                                                         
001543        END-IF                                                            
001544     END-IF                                                               
001545                                                                          
001546     IF OBKR-KDORDBEK = 43 OR 44                                          
001547        MOVE OBKR-KVQPACK      TO MOD-KVQPACK(WS-INDEX-MOD)               
001548     ELSE                                                                 
001549        MOVE +0                TO MOD-KVQPACK(WS-INDEX-MOD)               
001550     END-IF                                                               
001551     IF OBKR-KDORDBEK = 10                                                
001552        MOVE OBKR-IDKUNDRF-RO (1:7)                                       
001553                               TO MOD-IDKUNDRF-RO(WS-INDEX-MOD)           
001554     ELSE                                                                 
001555        MOVE +0                TO MOD-IDKUNDRF-RO(WS-INDEX-MOD)           
001556     END-IF                                                               
001557                                                                          
001558     IF OBKR-KDORDBEK = 15 OR 16 OR 21                                    
001559                     OR 43 OR 44 OR 52 OR 53 OR 54                        
001560                     OR 55 OR 57 OR 70 OR 72 OR 73 OR 74                  
001561                     OR 75 OR 76 OR 80 OR 95 OR 99 OR 58 OR 92            
001562                     OR 66 OR 98                                          
001563        IF OBKR-IDARTNR-TILLK > 0                                         
001564           MOVE OBKR-IDARTNR-TILLK                                        
001565                               TO MOD-IDARTNR-1--9(WS-INDEX-MOD)          
001566           MOVE '-'            TO MOD-STRAEK(WS-INDEX-MOD)                
001567           MOVE OBKR-REKSIFFR-TILLK                                       
001568                               TO MOD-REKSIFFR(WS-INDEX-MOD)              
001569        END-IF                                                            
001570     ELSE                                                                 
001571        IF OBKR-KDORDBEK = 41 OR 61                                       
001572*------ ERSATT ARTIKEL                                                    
001573                                                                          
001574           PERFORM HBAAA-FIXA-ERSATNING-RAD                               
001575        END-IF                                                            
001576     END-IF                                                               
001577                                                                          
001578     MOVE OBKR-IDLOPNR         TO WS-AKT-IDLOPNR                          
001579     MOVE OBKR-IDSEKVNR        TO WS-AKT-IDSEKVNR                         
001580     MOVE OBKR-IDARTNR         TO WS-AKT-IDARTNR-URS                      
001581     MOVE OBKR-IDLOPNR-RO      TO WS-AKT-IDLOPNR-RO                       
001582                                                                          
001583     MOVE WS-AKT-KEYS          TO MOD-KEYS(WS-INDEX-MOD)                  
001584     .                                                                    
001585     EJECT                                                                
001586 HBAAA-FIXA-ERSATNING-RAD SECTION.                                        
001587                                                                          
001588     IF (OBKR-IDARTNR NOT = WS-IDARTNR-SPAR)     OR                       
001589           (OBKR-IDARTNR = WS-IDARTNR-SPAR  AND                           
001590              OBKR-IDLOPNR NOT = WS-IDLOPNR-SPAR)                         
001591        PERFORM S22-HAMTA-BENAMNING                                       
001592        MOVE MFS-STAENG-FAELT-OSYNLIGT                                    
001593                      TO MOD-IDDC-ATTR(WS-INDEX-MOD)                      
001594                         MOD-KVANTAL-ATTR(WS-INDEX-MOD)                   
001595                         MOD-KVQPACK-ATTR(WS-INDEX-MOD)                   
001596                         MOD-IDKUNDRF-RO-ATTR(WS-INDEX-MOD)               
001597     ELSE                                                                 
001598        IF OBKR-IDARTNR-TILLK = +0                                        
001599*------ TILLKOMMANDE TEXT                                                 
001600           MOVE MFS-RENSA-FAELT                                           
001601                           TO MOD-IDARTNR(WS-INDEX-MOD)                   
001602           MOVE OBKR-BEERS TO MOD-BEART(WS-INDEX-MOD)                     
001603           MOVE MFS-STAENG-FAELT-OSYNLIGT                                 
001604                      TO MOD-KDBEHX-ATTR(WS-INDEX-MOD)                    
001605                         MOD-IDDC-ATTR(WS-INDEX-MOD)                      
001606                         MOD-KVANTAL-ATTR(WS-INDEX-MOD)                   
001607                         MOD-KVQPACK-ATTR(WS-INDEX-MOD)                   
001608                         MOD-IDKUNDRF-RO-ATTR(WS-INDEX-MOD)               
001609        ELSE                                                              
001610*-------TILLKOMMANDE ARTIKEL                                              
001611           MOVE OBKR-IDARTNR-TILLK                                        
001612                         TO MOD-IDARTNR-1--9(WS-INDEX-MOD)                
001613           MOVE '-'      TO MOD-STRAEK(WS-INDEX-MOD)                      
001614           MOVE OBKR-REKSIFFR-TILLK                                       
001615                         TO MOD-REKSIFFR(WS-INDEX-MOD)                    
001616           PERFORM S22-HAMTA-BENAMNING                                    
001617           MOVE OBKR-KVBEART-TILLK                                        
001618                         TO MOD-KVANTAL(WS-INDEX-MOD)                     
001619           MOVE OBKR-DIERS-KVOT                                           
001620                         TO MOD-KVQPACK(WS-INDEX-MOD)                     
001621        END-IF                                                            
001622     END-IF                                                               
001623     .                                                                    
001624     EJECT                                                                
001625 HBAB-VISA-SATS-ARTIKLAR SECTION.                                         
001626                                                                          
001627     MOVE NEJ                  TO NEXT-SATS-SW                            
001628                                                                          
001629     MOVE +1                   TO WS-INDEX-SATS                           
001630     PERFORM UNTIL WS-INDEX-SATS > WS-INDEX-SATS-MAX                      
001631        MOVE SPACE             TO WS-IDARTNR-SATS(WS-INDEX-SATS)          
001632        ADD +1                 TO WS-INDEX-SATS                           
001633     END-PERFORM                                                          
001634                                                                          
001635     MOVE +1                   TO WS-INDEX-SATS                           
001636                                  WS-INDEX                                
001637     MOVE OBKR-IDARTNR         TO W-J1-IDARTNR                            
001638                                                                          
001639     PERFORM IMS-18A-GU-SATB-WDJ111-01                                    
001640     PERFORM UNTIL SEGMENT-SAKNAS                                         
001641             OR    WS-INDEX-SATS > WS-INDEX-SATS-MAX                      
001642             OR    WS-INDEX  > +10                                        
001643                                                                          
001644        MOVE RAD-TISTADAT   TO TMP1-YYMMDD                                
001645        MOVE RAD-TISTODAT   TO TMP2-YYMMDD                                
001646        MOVE MSGI-TILOKDAT  TO TMP3-YYMMDD                                
001647        PERFORM WY2000Q1                                                  
001648        IF STR-IDARTNR < +100000000 AND STR-TIBORT = +0 AND               
001649           TMP1-YYMMDD  NOT > TMP3-YYMMDD   AND                           
001650           TMP2-YYMMDD  NOT < TMP3-YYMMDD                                 
001651                                                                          
001652           IF WS-INDEX-MOD = WS-INDEX-MOD-MAX                             
001653              MOVE JA             TO NEXT-SATS-SW                         
001654              MOVE +6             TO WS-INDEX-SATS                        
001655           ELSE                                                           
001656              MOVE STR-IDARTNR TO WS-NUM-9                                
001657              MOVE WS-ALFA-9   TO WS-IDARTNR-SATS(WS-INDEX-SATS)          
001658              ADD +1           TO WS-INDEX-SATS                           
001659           END-IF                                                         
001660        END-IF                                                            
001661        IF WS-INDEX-SATS NOT > WS-INDEX-SATS-MAX                          
001662           PERFORM IMS-18-GN-SATB-WDJ111-01                               
001663           ADD  +1             TO WS-INDEX                                
001664        END-IF                                                            
001665     END-PERFORM                                                          
001666     IF WS-INDEX-SATS NOT > WS-INDEX-SATS-MAX                             
001667                                                                          
001668        COMPUTE WS-INDEX = WS-INDEX-MOD + WS-INDEX-SATS - 1               
001669        IF WS-INDEX NOT > WS-INDEX-MOD-MAX                                
001670                                                                          
001671           MOVE +1       TO WS-INDEX-SATS                                 
001672           PERFORM UNTIL WS-INDEX-SATS > WS-INDEX-SATS-MAX                
001673                 OR WS-IDARTNR-SATS(WS-INDEX-SATS) = SPACE                
001674                                                                          
001675              ADD +1     TO WS-INDEX-MOD                                  
001676              PERFORM HBABA-REDIGERA-SATS-RAD                             
001677              ADD +1     TO WS-INDEX-SATS                                 
001678           END-PERFORM                                                    
001679        ELSE                                                              
001680           MOVE JA       TO NEXT-SATS-SW                                  
001681        END-IF                                                            
001682     END-IF                                                               
001683     IF NEXT-SATS                                                         
001684*-----OM EJ ALLA SATS-ART FÅR PLATS PÅ SIDAN RADERAS RAD MED              
001685*-----ORDBEK = 57 OCH DEN SPARAS FÖR NÄSTA SIDA(I HBB-SECTIONEN)          
001686        MOVE MFS-RENSA-FAELT   TO MOD-KDORDBEK(WS-INDEX-MOD)              
001687                                  MOD-KDBEHX(WS-INDEX-MOD)                
001688                                  MOD-IDARTNR(WS-INDEX-MOD)               
001689                                  MOD-BEART(WS-INDEX-MOD)                 
001690                                  MOD-IDDC-RAD(WS-INDEX-MOD)              
001691                                  MOD-KVANTAL(WS-INDEX-MOD)               
001692                                  MOD-KVQPACK(WS-INDEX-MOD)               
001693                                  MOD-IDKUNDRF-RO(WS-INDEX-MOD)           
001694        MOVE MED-FLER-SIDOR    TO MED-IDMFSINF                            
001695        MOVE MED-UPPLYSN-UPPDAT-PF                                        
001696                               TO MED-IDMFSFEL                            
001697     END-IF                                                               
001698     .                                                                    
001699     EJECT                                                                
001700                                                                          
001701 HBABA-REDIGERA-SATS-RAD SECTION.                                         
001702                                                                          
001703     MOVE OBKR-KDORDBEK        TO MOD-KDORDBEK(WS-INDEX-MOD)              
001704     MOVE MFS-STAENG-FAELT-OSYNLIGT                                       
001705                               TO MOD-KDORDBEK-ATTR(WS-INDEX-MOD)         
001706                                                                          
001707     MOVE SPACE                TO MOD-ASTERIX(WS-INDEX-MOD)               
001708     MOVE 'B'                  TO MOD-KDBEHX(WS-INDEX-MOD)                
001709     MOVE MFS-RENSA-FAELT      TO MOD-IDARTNR(WS-INDEX-MOD)               
001710                                                                          
001711     MOVE WS-IDARTNR-SATS(WS-INDEX-SATS)                                  
001712                               TO MOD-BEART(WS-INDEX-MOD)                 
001713     INSPECT MOD-BEART(WS-INDEX-MOD) REPLACING LEADING ZERO BY            
001714                                                         SPACE            
001715     MOVE MFS-STAENG-FAELT-OSYNLIGT                                       
001716                           TO MOD-IDDC-ATTR(WS-INDEX-MOD)                 
001717                              MOD-KVANTAL-ATTR(WS-INDEX-MOD)              
001718                              MOD-KVQPACK-ATTR(WS-INDEX-MOD)              
001719                              MOD-IDKUNDRF-RO-ATTR(WS-INDEX-MOD)          
001720                                                                          
001721     MOVE WS-AKT-KEYS          TO MOD-KEYS(WS-INDEX-MOD)                  
001722     .                                                                    
001723     EJECT                                                                
001724                                                                          
001725 HBB-FIXA-BLADDRINGS-VARDEN SECTION.                                      
001726                                                                          
001727     IF OBKR-SEGMENT-FINNS                                                
001728        MOVE OBKR-IDARTNR      TO MOD-IDARTNR-NEXT                        
001729        MOVE OBKR-IDLOPNR      TO MOD-IDLOPNR-NEXT                        
001730        MOVE OBKR-IDSEKVNR     TO MOD-IDSEKVNR-NEXT                       
001731        MOVE OBKR-IDDC         TO MOD-IDDC-NEXT                           
001732        MOVE OBKR-KDORDBEK     TO MOD-KDORDBEK-NEXT                       
001733                                                                          
001734        MOVE MED-FLER-SIDOR    TO MED-IDMFSINF                            
001735        MOVE MED-UPPLYSN-UPPDAT-PF                                        
001736                               TO MED-IDMFSFEL                            
001737     ELSE                                                                 
001738        MOVE ZERO              TO MOD-IDARTNR-NEXT                        
001739                                  MOD-IDLOPNR-NEXT                        
001740                                  MOD-IDSEKVNR-NEXT                       
001741                                  MOD-IDDC-NEXT                           
001742                                  MOD-KDORDBEK-NEXT                       
001743     END-IF                                                               
001744     .                                                                    
001745     EJECT                                                                
001746                                                                          
001747 HBC-KONTROLLERA-SIDSLUT SECTION.                                         
001748                                                                          
001749     MOVE WS-INDEX-MOD-MAX     TO WS-INDEX-MOD                            
001750     MOVE MOD-KEYS(WS-INDEX-MOD)                                          
001751                               TO WS-AKT-KEYS                             
001752     IF OBKR-IDARTNR = WS-AKT-IDARTNR-URS AND                             
001753        OBKR-IDLOPNR = WS-AKT-IDLOPNR                                     
001754                                                                          
001755        PERFORM UNTIL WS-INDEX-MOD = +1 OR                                
001756                     (OBKR-IDARTNR NOT = WS-AKT-IDARTNR-URS OR            
001757                      OBKR-IDLOPNR NOT = WS-AKT-IDLOPNR)                  
001758                                                                          
001759           SUBTRACT 1 FROM WS-INDEX-MOD                                   
001760           MOVE MOD-KEYS(WS-INDEX-MOD)                                    
001761                               TO WS-AKT-KEYS                             
001762        END-PERFORM                                                       
001763                                                                          
001764        ADD +1 TO WS-INDEX-MOD                                            
001765                                                                          
001766*---- BLÄDDRINGSVÄRDENA MÅSTE JUSTERAS OM NÄR VI BACKAR RADER             
001767                                                                          
001768        MOVE MOD-KEYS(WS-INDEX-MOD)                                       
001769                               TO WS-AKT-KEYS                             
001770        MOVE WS-AKT-IDARTNR-URS TO MOD-IDARTNR-NEXT                       
001771        MOVE WS-AKT-IDLOPNR    TO MOD-IDLOPNR-NEXT                        
001772        MOVE WS-AKT-IDSEKVNR   TO MOD-IDSEKVNR-NEXT                       
001773        MOVE MOD-IDDC-RAD(WS-INDEX-MOD)                                   
001774                               TO MOD-IDDC-NEXT                           
001775        MOVE MOD-KDORDBEK(WS-INDEX-MOD)                                   
001776                               TO MOD-KDORDBEK-NEXT                       
001777        MOVE WS-INDEX-MOD      TO WS-INDEX                                
001778        PERFORM UNTIL WS-INDEX > WS-INDEX-MOD-MAX                         
001779          MOVE MFS-RENSA-FAELT TO MOD-KDORDBEK(WS-INDEX)                  
001780                                  MOD-ASTERIX(WS-INDEX)                   
001781                                  MOD-KDBEHX(WS-INDEX)                    
001782                                  MOD-IDARTNR(WS-INDEX)                   
001783                                  MOD-BEART(WS-INDEX)                     
001784                                  MOD-IDDC-RAD(WS-INDEX)                  
001785                                  MOD-KVANTAL(WS-INDEX)                   
001786                                  MOD-KVQPACK(WS-INDEX)                   
001787                                  MOD-IDKUNDRF-RO(WS-INDEX)               
001788                                  MOD-KEYS(WS-INDEX)                      
001789          ADD +1               TO WS-INDEX                                
001790        END-PERFORM                                                       
001791     ELSE                                                                 
001792        ADD +1                 TO WS-INDEX-MOD                            
001793     END-IF                                                               
001794                                                                          
001795     MOVE MED-FLER-SIDOR       TO MED-IDMFSINF                            
001796     MOVE MED-UPPLYSN-UPPDAT-PF                                           
001797                               TO MED-IDMFSFEL                            
001798     .                                                                    
001799     EJECT                                                                
001800 HD-UPPDATERA-OBEH-RADER SECTION.                                         
001801                                                                          
001802     MOVE NEJ                  TO AKT-SIDA-SW                             
001803                                                                          
001804     MOVE MID-RAD(1)           TO WS-AKTUELL-MID-RAD                      
001805     MOVE WS-AKT-IDARTNR-URS   TO W-Q1-IDARTNR-MAX                        
001806     MOVE WS-AKT-IDLOPNR       TO W-Q1-IDLOPNR-MAX                        
001807     MOVE WS-AKT-IDSEKVNR      TO W-Q1-IDSEKVNR-MAX                       
001808     MOVE WS-AKT-IDDC          TO W-Q1-IDDC-MAX                           
001809     MOVE WS-AKT-KDORDBEK      TO W-Q1-KDORDBEK-MAX                       
001810                                                                          
001811     PERFORM IMS-01-GHU-ORQM-WDQ101-FOERE                                 
001812     PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                           
001813                                                                          
001814        PERFORM S02-GODKANN-RAD                                           
001815                                                                          
001816        PERFORM IMS-02-GHN-ORQM-WDQ101-FOERE                              
001817     END-PERFORM                                                          
001818                                                                          
001819     MOVE ALL '9'              TO W-Q1-IDARTNR-MAX                        
001820                                  W-Q1-IDLOPNR-MAX                        
001821                                  W-Q1-IDSEKVNR-MAX                       
001822                                  W-Q1-IDDC-MAX                           
001823                                  W-Q1-KDORDBEK-MAX                       
001824     .                                                                    
001825     EJECT                                                                
001826 HE-UPPDATERA-AKT-SIDA SECTION.                                           
001827                                                                          
001828     MOVE JA                   TO AKT-SIDA-SW                             
001829     MOVE +1 TO WS-INDEX-MID                                              
001830     PERFORM UNTIL WS-INDEX-MID > WS-INDEX-MID-MAX  OR                    
001831                   MID-KDORDBEK(WS-INDEX-MID) = ZERO                      
001832       MOVE MID-RAD(WS-INDEX-MID)                                         
001833                             TO WS-AKTUELL-MID-RAD                        
001834       MOVE WS-AKT-IDARTNR-URS   TO W-Q1-IDARTNR-UNIK                     
001835       MOVE WS-AKT-IDLOPNR       TO W-Q1-IDLOPNR-UNIK                     
001836       MOVE WS-AKT-IDSEKVNR      TO W-Q1-IDSEKVNR-UNIK                    
001837       MOVE WS-AKT-IDDC          TO W-Q1-IDDC-UNIK                        
001838       MOVE WS-AKT-KDORDBEK      TO W-Q1-KDORDBEK-UNIK                    
001839                                                                          
001840       PERFORM IMS-05-GHU-ORQM-WDQ101-UNIK                                
001841       IF SEGMENT-FINNS                                                   
001842         IF MID-IDARTNR(WS-INDEX-MID) = ZERO                              
001843           MOVE JA       TO OBKR-FLOBOK                                   
001844           PERFORM IMS-06-REPL-ORQM-WDQ101                                
001845         ELSE                                                             
001846           IF WS-AKT-KDBEHX = SPACE                                       
001847             PERFORM S02-GODKANN-RAD                                      
001848           ELSE                                                           
001849             IF WS-AKT-KDBEHX = 'A'                                       
001850               MOVE JA       TO OBKR-FLOBOK                               
001851               PERFORM IMS-06-REPL-ORQM-WDQ101                            
001852             ELSE                                                         
001853               IF WS-AKT-KDBEHX = 'D'                                     
001854                 PERFORM HEA-ANNULLERA-RAD                                
001855               ELSE                                                       
001856                 IF WS-AKT-KDBEHX = '1' OR '2'                            
001857                   PERFORM HEA-ANNULLERA-RAD                              
001858                   IF WS-AKT-KDBEHX = '1'                                 
001859                     MOVE +1       TO OBKR-KDKVBRYT                       
001860                   ELSE                                                   
001861                     MOVE +2       TO OBKR-KDKVBRYT                       
001862                   END-IF                                                 
001863                   MOVE +0         TO OBKR-KVPREAVB                       
001864                                      OBKR-KVPRERO                        
001865                   PERFORM S01-SKRIV-MID-TILL-4222                        
001866                 ELSE                                                     
001867                   IF WS-AKT-KDBEHX = 'X'                                 
001868                     PERFORM HEB-BACKA-WDA5-STATUS                        
001869                                                                          
001870                     PERFORM HEA-ANNULLERA-RAD                            
001871                   ELSE                                                   
001872                     IF WS-AKT-KDBEHX = 'B' AND OBKR-KDORDKL = +0         
001873*DDGS                  AND (OBKR-KDORDBEK = 21 OR 52 OR 53 OR 54          
001874                       AND (OBKR-KDORDBEK = 51 OR 52 OR 53 OR 54          
001875                                               OR 55 OR 57 OR 67)         
001876                       PERFORM S11-SKRIV-VOR-RAD                          
001877                       MOVE JA TO OBKR-FLOBOK                             
001878                       PERFORM IMS-06-REPL-ORQM-WDQ101                    
001879                     ELSE                                                 
001880                       MOVE JA TO OBKR-FLOBOK                             
001881                       PERFORM IMS-06-REPL-ORQM-WDQ101                    
001882                     END-IF                                               
001883                   END-IF                                                 
001884                 END-IF                                                   
001885               END-IF                                                     
001886             END-IF                                                       
001887           END-IF                                                         
001888         END-IF                                                           
001889       END-IF                                                             
001890       ADD +1                  TO WS-INDEX-MID                            
001891     END-PERFORM                                                          
001892     .                                                                    
001893     EJECT                                                                
001894                                                                          
001895 HEA-ANNULLERA-RAD SECTION.                                               
001896                                                                          
001897     IF OBKR-KVPREAVB > +0 OR OBKR-KVPRERO > +0                           
001898        IF OBKR-IDLEVNR = SPACE                                           
001899                                                                          
001900           IF OBKR-IDARTNR-TILLK > +0                                     
001901              MOVE OBKR-IDARTNR-TILLK TO W-IDARTNR                        
001902           ELSE                                                           
001903              MOVE OBKR-IDARTNR       TO W-IDARTNR                        
001904           END-IF                                                         
001905                                                                          
001906           MOVE OBKR-IDDC       TO WS-IDDC                                
001907           IF WS-IDDC NOT = W-IDDC-B6                                     
001908              MOVE WS-IDDC TO W-IDDC-B6                                   
001909              PERFORM IMS-GU-WDB601                                       
001910           END-IF                                                         
001911                                                                          
001912           IF DCS-CDC                                                     
001913             PERFORM IMS-21-GHU-ARTM-WDK901                               
001914                                                                          
001915             PERFORM HEAA-BACKA-WDK9-SALDON                               
001916                                                                          
001917             PERFORM IMS-22-REPL-ARTM-WDK901                              
001918           ELSE                                                           
001919             MOVE OBKR-IDDC TO W-IDDC                                     
001920             PERFORM IMS-11-GHU-WDK711                                    
001921             IF OBKR-KDORDKL = +0 OR +1                                   
001922               SUBTRACT OBKR-KVBEART-Q FROM SLAG-KVOKS-DAG                
001923             ELSE                                                         
001924               SUBTRACT OBKR-KVBEART-Q FROM SLAG-KVOKS-BULK               
001925             END-IF                                                       
001926             PERFORM IMS-12-REPL-WDK711                                   
001927           END-IF                                                         
001928        END-IF                                                            
001929     END-IF                                                               
001930     PERFORM IMS-08-DLET-ORQM-WDQ101                                      
001931     PERFORM S25-DELETE-PRICE-Q-LINE                                      
001932     .                                                                    
001933     EJECT                                                                
001934                                                                          
001935 HEAA-BACKA-WDK9-SALDON SECTION.                                          
001936                                                                          
001937     IF OBKR-KDORDKL = +0                                                 
001938        IF OBKR-KVPREAVB > +0                                             
001939           COMPUTE ART-KVPREAVB-VOR =                                     
001940           ART-KVPREAVB-VOR - OBKR-KVPREAVB                               
001941        END-IF                                                            
001942        IF OBKR-KVPRERO > +0                                              
001943           COMPUTE ART-KVPRERO-DAG =                                      
001944           ART-KVPRERO-DAG - OBKR-KVPRERO                                 
001945        END-IF                                                            
001946        IF OHUV-IDKAMPRF = +0 AND                                         
001947           OBKR-IDKUNDRF-RO = '0000000   '                                
001948           COMPUTE ART-KVOKS-VOR =                                        
001949           ART-KVOKS-VOR - OBKR-KVBEART-Q                                 
001950        END-IF                                                            
001951     ELSE                                                                 
001952        IF OBKR-KDORDKL = +1                                              
001953           IF OBKR-KVPREAVB > +0                                          
001954              COMPUTE ART-KVPREAVB-DAG =                                  
001955              ART-KVPREAVB-DAG - OBKR-KVPREAVB                            
001956           END-IF                                                         
001957           IF OBKR-KVPRERO > +0                                           
001958              COMPUTE ART-KVPRERO-DAG =                                   
001959              ART-KVPRERO-DAG - OBKR-KVPRERO                              
001960           END-IF                                                         
001961           IF OHUV-IDKAMPRF = +0 AND                                      
001962              OBKR-IDKUNDRF-RO = '0000000   '                             
001963              COMPUTE ART-KVOKS-DAG =                                     
001964              ART-KVOKS-DAG - OBKR-KVBEART-Q                              
001965           END-IF                                                         
001966        ELSE                                                              
001967           IF OBKR-KVPREAVB > +0                                          
001968              COMPUTE ART-KVPREAVB-BULK =                                 
001969              ART-KVPREAVB-BULK - OBKR-KVPREAVB                           
001970           END-IF                                                         
001971           IF OBKR-KVPRERO > +0                                           
001972              COMPUTE ART-KVPRERO-BULK =                                  
001973              ART-KVPRERO-BULK - OBKR-KVPRERO                             
001974           END-IF                                                         
001975           IF OHUV-IDKAMPRF = +0 AND                                      
001976              OBKR-IDKUNDRF-RO = '0000000   '                             
001977              COMPUTE ART-KVOKS-BULK =                                    
001978              ART-KVOKS-BULK - OBKR-KVBEART-Q                             
001979           END-IF                                                         
001980        END-IF                                                            
001981     END-IF                                                               
001982     .                                                                    
001983     EJECT                                                                
001984 HEB-BACKA-WDA5-STATUS SECTION.                                           
001985                                                                          
001986     MOVE OBKR-IDDISTR       TO W-A5-IDDISTR                              
001987     MOVE OBKR-IDKUNDNR      TO W-A5-IDKUNDNR                             
001988     MOVE WS-AKT-IDKUNDRF-RO (3:5)                                        
001989                             TO W-A5-IDKUNDRF                             
001990     INSPECT W-A5-IDKUNDRF REPLACING LEADING                              
001991                                 SPACE BY ZERO                            
001992     MOVE WS-AKT-IDARTNR     TO W-A5-IDARTNR                              
001993     MOVE WS-AKT-IDLOPNR-RO  TO W-A5-IDLOPNR                              
001994                                                                          
001995     PERFORM IMS-26-GHU-ORDP-WDA501                                       
001996                                                                          
001997     MOVE '3'             TO RAD-KDSTARAD                                 
001998     MOVE '00000     ' TO RAD-IDKUNDRF-LEV                                
001999     PERFORM IMS-27-REPL-ORDP-WDA501                                      
002000                                                                          
002001     IF ARB-KDROPACK > ZERO                                               
002002                                                                          
002003        MOVE ZERO              TO ARB-KDROPACK                            
002004        PERFORM IMS-15-REPL-ORQI-WDQ212                                   
002005     END-IF                                                               
002006     .                                                                    
002007     EJECT                                                                
002008                                                                          
002009 HG-UPPDATERA-RESTERANDE-RADER SECTION.                                   
002010                                                                          
002011     MOVE NEJ                  TO AKT-SIDA-SW                             
002012*--- BEHANDLA RESTERANDE OBKR PÅ ORDERN                                   
002013                                                                          
002014     PERFORM IMS-03-GHU-ORQM-WDQ101-MIN-MAX                               
002015     MOVE +1                   TO WS-RADER                                
002016     PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT OR WS-RADER > +13         
002017                                                                          
002018        PERFORM S02-GODKANN-RAD                                           
002019        ADD  +1               TO WS-RADER                                 
002020                                                                          
002021        PERFORM IMS-04-GHN-ORQM-WDQ101-MIN-MAX                            
002022     END-PERFORM                                                          
002023                                                                          
002024     IF START-4222                                                        
002025        MOVE 'V'              TO 4222-MID-KDTRTYP                         
002026        PERFORM S03-STARTA-RADBEHANDLINGEN                                
002027     ELSE                                                                 
002028        IF WS-RADER = 14 AND SEGMENT-FINNS                                
002029           PERFORM HGA-OMSKEDULERA                                        
002030        ELSE                                                              
002031           MOVE JA             TO AVSLUTA-SW                              
002032        END-IF                                                            
002033     END-IF                                                               
002034     .                                                                    
002035     EJECT                                                                
002036 HGA-OMSKEDULERA SECTION.                                                 
002037                                                                          
002038     MOVE MFS-KDMFSFOR         TO 4223-SPRAK                              
002039     MOVE ALL '+'              TO 4223-IDDISTR-IN                         
002040                                  4223-IDKUNDNR-IN                        
002041                                  4223-IDORDNR-IN                         
002042     MOVE WS-IDDISTR           TO 4223-IDDISTR-UT                         
002043     MOVE WS-IDKUNDNR          TO 4223-IDKUNDNR-UT                        
002044     MOVE WS-IDORDNR           TO 4223-IDORDNR-UT                         
002045                                                                          
002046     PERFORM IMS-INSERT-4223-MSG                                          
002047     MOVE JA                   TO HOPP                                    
002048     .                                                                    
002049     EJECT                                                                
002050                                                                          
002051 HH-ANNULLERA-ORDER SECTION.                                              
002052                                                                          
002053     MOVE MFS-KDMFSFOR         TO 4292-SPRAK                              
002054     MOVE OHUV-IDORDER         TO 4292-IDORDER                            
002055     MOVE WS-IDDISTR           TO 4292-IDDISTR                            
002056     MOVE WS-IDKUNDNR          TO 4292-IDKUNDNR                           
002057     MOVE W-IDORDNR            TO 4292-IDKUNDRF                           
002058                                                                          
002059     PERFORM IMS-INSERT-4292-MSG                                          
002060                                                                          
002061     MOVE OHUV-IDDC-PRIM       TO WS-IDDC                                 
002062     IF WS-IDDC NOT = W-IDDC-B6                                           
002063        MOVE WS-IDDC TO W-IDDC-B6                                         
002064        PERFORM IMS-GU-WDB601                                             
002065     END-IF                                                               
002066                                                                          
002067     IF DCS-NDC-NA                                                        
002068        PERFORM S23-DATA-TILL-DEL-NOTE                                    
002069     END-IF                                                               
002070                                                                          
002071     MOVE 'W4O22101'           TO MFS-IDMOD                               
002072                                                                          
002073     MOVE '4221'               TO MOD-W4O22301(1:4)                       
002074                                                                          
002075     MOVE MED-ORDER-ANNULLERAD TO MED-IDMFSFEL                            
002076     CALL WMEDKONV USING MED-WMEDAREA                                     
002077     MOVE MED-MFSFEL           TO MOD-W4O22301(5:40)                      
002078                                                                          
002079     MOVE 4221-MOD-LAENGD-ANNULL                                          
002080                               TO MSG-KVLL                                
002081     PERFORM IMS-INSERT-MSG                                               
002082     MOVE JA                   TO HOPP                                    
002083     .                                                                    
002084     EJECT                                                                
002085                                                                          
002086 I-STARTA-ORDERAVSLUT SECTION.                                            
002087                                                                          
002088     MOVE W-IDDISTR            TO 4298-MID-IDDISTR                        
002089     MOVE W-IDKUNDNR           TO 4298-MID-IDKUNDNR                       
002090     MOVE W-IDKUNDRF           TO 4298-MID-IDKUNDRF                       
002091     MOVE OHUV-IDORDER         TO 4298-MID-IDORDER                        
002092                                                                          
002093     COMPUTE 4298-LL = LENGTH OF 4298-MID-W4I29801 + 17                   
002094     PERFORM IMS-INSERT-4298-MSG                                          
002095       .                                                                  
002096     EJECT                                                                
002097                                                                          
002098 M-HOPPA-TILL-NY-BILD SECTION.                                            
002099                                                                          
002100     IF OHUV-FLVORKO = NEJ                                                
002101       MOVE 'W4O22101'           TO MFS-IDMOD                             
002102       MOVE '4221'               TO MOD-W4O22301(1:4)                     
002103                                                                          
002104       MOVE ERR-ORDER-AVSLUTAD   TO MED-IDMFSFEL                          
002105       CALL WMEDKONV USING MED-WMEDAREA                                   
002106       MOVE MED-MFSFEL           TO MOD-W4O22301(5:53)                    
002107       PERFORM MFS-RENSA-4221-MOD                                         
002108       MOVE WS-IDORDNR           TO WS-IDKUNDRF                           
002109       INSPECT WS-IDKUNDRF REPLACING LEADING ZERO BY SPACE                
002110       IF ENGLISH-TEXT                                                    
002111          MOVE 'ORDER NUMBER WAS' TO WS-4221-ORDERNR-TEXT                 
002112       ELSE                                                               
002113          MOVE 'ORDERNUMRET VAR ' TO WS-4221-ORDERNR-TEXT                 
002114       END-IF                                                             
002115       MOVE ORDERNR-TILL-4221    TO MOD-W4O22301(527:55)                  
002116       COMPUTE 4221-MOD-LAENGD = LENGTH OF 4221-W4O22101 + 4              
002117       MOVE 4221-MOD-LAENGD      TO MSG-KVLL                              
002118       PERFORM IMS-INSERT-MSG                                             
002119     ELSE                                                                 
002120       IF  OHUV-FLVORKO = JA                                              
002121         MOVE MFS-KDMFSFOR       TO 4225-SPRAK                            
002122         MOVE MFS-RENSA-FAELT    TO 4225-MID                              
002123*        MOVE WS-IDDISTR         TO 4225-MID-IDDISTR-IN                   
002124         MOVE WS-IDORDNR         TO 4225-MID-IDORDNR-VOR                  
002125         PERFORM IMS-INSERT-4225-MSG                                      
002126       ELSE                                                               
002127         MOVE MFS-KDMFSFOR       TO 4224-SPRAK                            
002128         MOVE MFS-RENSA-FAELT    TO 4224-MID                              
002129         MOVE WS-IDDISTR         TO 4224-MID-IDDISTR-1-IN                 
002130         MOVE WS-IDORDNR         TO 4224-MID-IDORDNR-VOR                  
002131         PERFORM IMS-INSERT-4224-MSG                                      
002132       END-IF                                                             
002133     END-IF                                                               
002134     MOVE JA                   TO HOPP                                    
002135     .                                                                    
002136     EJECT                                                                
002137                                                                          
002138 Z-FINIT SECTION.                                                         
002139                                                                          
002140     IF MED-IDMFSFEL NOT = SPACE OR MED-IDMFSINF NOT = SPACE              
002141         CALL WMEDKONV USING MED-WMEDAREA                                 
002142         MOVE MED-MFSFEL       TO MOD-TEMFSFEL                            
002143         MOVE MED-MFSINF       TO MOD-TEMFSINF                            
002144     END-IF                                                               
002145     IF NOT ALLT-OK AND NYCKEL-OK                                         
002146        PERFORM MFS-ROER-EJ-BILD                                          
002147     END-IF                                                               
002148                                                                          
002149     MOVE MAX-MOD-LAENGD       TO MSG-KVLL                                
002150     PERFORM IMS-INSERT-MSG                                               
002151     .                                                                    
002152     EJECT                                                                
002153 S01-SKRIV-MID-TILL-4222 SECTION.                                         
002154                                                                          
002155     MOVE JA                   TO START-4222-SW                           
002156     ADD +1                    TO 4222-MID-IX                             
002157                                                                          
002158     IF OBKR-IDARTNR-TILLK > +0                                           
002159       MOVE OBKR-IDARTNR-TILLK TO WS-IDARTNR                              
002160      MOVE OBKR-REKSIFFR-TILLK TO WS-REKSIFFR                             
002161     ELSE                                                                 
002162       MOVE OBKR-IDARTNR       TO WS-IDARTNR                              
002163       MOVE OBKR-REKSIFFR      TO WS-REKSIFFR                             
002164     END-IF                                                               
002165     MOVE WS-IDARTNR-REKSIFFR  TO                                         
002166     4222-MID-IDARTNR-006(4222-MID-IX)                                    
002167                                                                          
002168     IF OBKR-KVBEART-TILLK > +0                                           
002169        MOVE OBKR-KVBEART-TILLK TO WS-NUM-6                               
002170        MOVE WS-ALFA-6         TO 4222-MID-KVBEART(4222-MID-IX)           
002171     ELSE                                                                 
002172        IF OBKR-KVPREAVB > +0 OR OBKR-KVPRERO > +0                        
002173           COMPUTE WS-NUM-6 = OBKR-KVPREAVB + OBKR-KVPRERO                
002174           MOVE WS-ALFA-6      TO 4222-MID-KVBEART(4222-MID-IX)           
002175        ELSE                                                              
002176           MOVE OBKR-KVBEART   TO WS-NUM-6                                
002177           MOVE WS-ALFA-6      TO 4222-MID-KVBEART(4222-MID-IX)           
002178        END-IF                                                            
002179     END-IF                                                               
002180                                                                          
002181     MOVE ALL '+'              TO 4222-MID-PRARTNTO(4222-MID-IX)          
002182     EJECT                                                                
002183     MOVE OBKR-FLINVEST        TO 4222-MID-FLINVEST(4222-MID-IX)          
002184     MOVE OBKR-KDVRINFO        TO 4222-MID-KDVRINFO(4222-MID-IX)          
002185     MOVE OBKR-BERADREF        TO 4222-MID-BERADREF(4222-MID-IX)          
002186     .                                                                    
002187     EJECT                                                                
002188 S02-GODKANN-RAD SECTION.                                                 
002189                                                                          
002190     MOVE OBKR-IDDC            TO WS-IDDC                                 
002191     MOVE JA                   TO OBKR-FLOBOK                             
002192     PERFORM IMS-06-REPL-ORQM-WDQ101                                      
002193                                                                          
002194     IF ((OBKR-KVPREAVB > +0 OR OBKR-KVPRERO > +0)                        
002195            AND OBKR-KDORDBEK NOT = 92 AND 98 AND 26) OR                  
002196               (OBKR-KDORDBEK = 92 AND OBKR-KVPREAVB > +0) OR             
002197               (OBKR-KDORDBEK = 98 AND OBKR-KVPREAVB > +0)                
002198                                                                          
002199        PERFORM S14-LAS-ARTIKELREG                                        
002200        PERFORM S12-BYGG-UPP-ORDERRAD                                     
002201                                                                          
002202        PERFORM S09-KONTROLLERA-ENHETSLAST                                
002203        PERFORM S10-BERAKNA-WOPS-SKRIV-ORAD                               
002204     END-IF                                                               
002205                                                                          
002206     IF OBKR-KDORDBEK = 10                                                
002207                                                                          
002208       PERFORM S14-LAS-ARTIKELREG                                         
002209       PERFORM S02A-VALD-BIPACKNING                                       
002210     ELSE                                                                 
002211       IF (OBKR-KDORDBEK =  61) AND AKT-SIDA                              
002212                                                                          
002213         PERFORM S14-LAS-ARTIKELREG                                       
002214         PERFORM S01-SKRIV-MID-TILL-4222                                  
002215         PERFORM S02B-SKRIV-ORDERBEKR-40                                  
002216       ELSE                                                               
002217         IF OBKR-KDORDBEK =  70  OR 74                                    
002218           IF OBKR-KDTPOTYP = +2                                          
002219                                                                          
002220             PERFORM S14-LAS-ARTIKELREG                                   
002221             PERFORM S02C-UPPDATERA-TPO2                                  
002222           ELSE                                                           
002223             IF OBKR-KDTPOTYP = +6                                        
002224                                                                          
002225                PERFORM S14-LAS-ARTIKELREG                                
002226                PERFORM S02D-UPPDATERA-TPO6                               
002227             END-IF                                                       
002228           END-IF                                                         
002229         ELSE                                                             
002230           IF (OBKR-KDORDBEK = 51 OR 52 OR 53 OR 54 OR                    
002231                               55 OR 57 OR 67 OR 92 OR 98)                
002232                          AND  OHUV-KDORDKL = +0                          
002233                                                                          
002234              PERFORM S11-SKRIV-VOR-RAD                                   
002235           END-IF                                                         
002236         END-IF                                                           
002237       END-IF                                                             
002238     END-IF                                                               
002239     .                                                                    
002240     EJECT                                                                
002241                                                                          
002242 S02A-VALD-BIPACKNING SECTION.                                            
002243                                                                          
002244     PERFORM S12-BYGG-UPP-ORDERRAD                                        
002245                                                                          
002246*** BIPACKNING RESTORDER ***                                              
002247     IF OBKR-TIRODAT > +0                                                 
002248        PERFORM S09-KONTROLLERA-ENHETSLAST                                
002249        PERFORM S10-BERAKNA-WOPS-SKRIV-ORAD                               
002250        IF ARB-KDROPACK = 'L'                                             
002251           PERFORM S02AB-UPPDATERA-WDE8                                   
002252        END-IF                                                            
002253     ELSE                                                                 
002254*** BIPACKNING TPO ***                                                    
002255       IF WS-IDDC NOT = W-IDDC-B6                                         
002256          MOVE WS-IDDC TO W-IDDC-B6                                       
002257          PERFORM IMS-GU-WDB601                                           
002258       END-IF                                                             
002259       IF DCS-CDC OR DCS-SDC                                              
002260         PERFORM S02AA-KOLLA-DIREKTLEVERANS                               
002261                                                                          
002262         IF DLEV-KDORDBEK-UT = +0                                         
002263            PERFORM S02AC-KOMPLETTERA-RANSONERING                         
002264            PERFORM S02AE-PREL-AVBOKNING                                  
002265            PERFORM S02AF-SKRIV-Q1-OCH-Q4-RADER                           
002266                                                                          
002267            IF ARB-KDROPACK = 'L'                                         
002268               PERFORM S02AB-UPPDATERA-WDE8                               
002269            END-IF                                                        
002270         ELSE                                                             
002271            MOVE DLEV-KDORDBEK-UT TO OBKR-KDORDBEK                        
002272            MOVE '4223DLEV'       TO OBKR-IDPGM                           
002273            MOVE DLEV-IDLEVNR-UT TO OBKR-IDLEVNR                          
002274            MOVE NEJ             TO OBKR-FLOBOK                           
002275                                                                          
002276            ADD +1               TO OBKR-IDSEKVNR                         
002277            PERFORM IMS-07-ISRT-ORQM-WDQ101                               
002278                                                                          
002279            IF DLEV-KDORDBEK-UT = 95                                      
002280               PERFORM S02AC-KOMPLETTERA-RANSONERING                      
002281               PERFORM S02AE-PREL-AVBOKNING                               
002282               PERFORM S02AF-SKRIV-Q1-OCH-Q4-RADER                        
002283                                                                          
002284               IF ARB-KDROPACK = 'L'                                      
002285                  PERFORM S02AB-UPPDATERA-WDE8                            
002286               END-IF                                                     
002287            ELSE                                                          
002288              MOVE OBKR-IDARTNR TO W-IDARTNR                              
002289              PERFORM IMS-21-GHU-ARTM-WDK901                              
002290              IF OBKR-KDORDKL = +0                                        
002291                 SUBTRACT OBKR-KVBEART-Q FROM                             
002292                          ART-KVOKS-VOR                                   
002293              ELSE                                                        
002294                IF OBKR-KDORDKL = +1                                      
002295                   SUBTRACT OBKR-KVBEART-Q FROM                           
002296                            ART-KVOKS-DAG                                 
002297                ELSE                                                      
002298                   SUBTRACT OBKR-KVBEART-Q FROM                           
002299                            ART-KVOKS-BULK                                
002300                END-IF                                                    
002301              END-IF                                                      
002302              PERFORM IMS-22-REPL-ARTM-WDK901                             
002303           END-IF                                                         
002304         END-IF                                                           
002305       END-IF                                                             
002306     END-IF                                                               
002307     .                                                                    
002308     EJECT                                                                
002309                                                                          
002310 S02AA-KOLLA-DIREKTLEVERANS SECTION.                                      
002311                                                                          
002312     MOVE ORAD-IDDISTR         TO DLEV-IDDISTR-IN                         
002313     MOVE ORAD-IDKUNDNR        TO DLEV-IDKUNDNR-IN                        
002314     MOVE OHUV-KDORDKL         TO DLEV-KDORDKL-IN                         
002315     MOVE ORAD-IDARTNR         TO DLEV-IDARTNR-IN                         
002316     MOVE ORAD-IDLEVNR         TO DLEV-IDLEVNR-IN                         
002317     MOVE ORAD-KVBEART-Q       TO DLEV-KVBEART-Q-IN                       
002318     MOVE AREG-REDIRLEV        TO DLEV-REDIRLEV-IN                        
002319     MOVE ORAD-IDDC            TO DLEV-IDDC-IN                            
002320     MOVE ORAD-IDDC            TO DLEV-IDDC-ORD-IN                        
002321     MOVE ORAD-IDKAMPRF        TO DLEV-IDKAMPRF-IN                        
002322     MOVE ORAD-KDTPOTYP        TO DLEV-KDTPOTYP-IN                        
002323     MOVE AREG-KDUART          TO DLEV-KDUART-IN                          
002324     MOVE OHUV-FLFORBI         TO DLEV-FLFORBI-IN                         
002325     MOVE ORAD-FLRESTN         TO DLEV-FLRESTN-IN                         
002326     MOVE AREG-FLREFILL        TO DLEV-FLREFILL-IN                        
002327     MOVE ORAD-KDORDING        TO DLEV-KDORDING-IN                        
002328     MOVE ORAD-CLEARGROUP      TO DLEV-CLEARGROUP                         
002329     MOVE ORAD-KDOI            TO DLEV-KDOI-UT                            
002330*    TO GET CLEARING DC LIST FROM WDB2                                    
002331     MOVE ORAD-IDDISTR         TO W-WDB2-IDDISTR                          
002332     MOVE ORAD-IDKUNDNR        TO W-WDB2-IDKUNDNR                         
002333     PERFORM IMS-GU-WDB201                                                
002334*                                                                         
002335     IF OHUV-KDORDKL > 1                                                  
002336                                                                          
002337        MOVE +1 TO WS-INDEX                                               
002338        PERFORM UNTIL WS-INDEX > IX-DCCLEAR-MAX                           
002339           MOVE GMT-IDDC-BULK(WS-INDEX) TO                                
002340                                  DLEV-IDDC-CLEAR-IN(WS-INDEX)            
002341           ADD +1 TO WS-INDEX                                             
002342        END-PERFORM                                                       
002343                                                                          
002344     ELSE                                                                 
002345       IF OHUV-KDORDKL = 1                                                
002346                                                                          
002347          MOVE +1 TO WS-INDEX                                             
002348          PERFORM UNTIL WS-INDEX > IX-DCCLEAR-MAX                         
002349             MOVE GMT-IDDC-DAY(WS-INDEX) TO                               
002350                                  DLEV-IDDC-CLEAR-IN(WS-INDEX)            
002351             ADD +1 TO WS-INDEX                                           
002352          END-PERFORM                                                     
002353                                                                          
002354       ELSE                                                               
002355         IF OHUV-KDORDKL = 0                                              
002356                                                                          
002357            MOVE +1 TO WS-INDEX                                           
002358            PERFORM UNTIL WS-INDEX > IX-DCCLEAR-MAX                       
002359               MOVE GMT-IDDC-VOR(WS-INDEX) TO                             
002360                                  DLEV-IDDC-CLEAR-IN(WS-INDEX)            
002361               ADD +1 TO WS-INDEX                                         
002362            END-PERFORM                                                   
002363                                                                          
002364         END-IF                                                           
002365       END-IF                                                             
002366     END-IF                                                               
002367     MOVE OHUV-IDKUNDRF        TO DLEV-IDKUNDRF-IN                        
002368**   MOVE 1                    TO DLEV-KDCALL                             
002369**   THE ABOVE LINE IS COMMENTED AND ZERO IS MOVED TO KDCALL              
002370**   TO AVOID THE INSERT INTO WDR6 IN W411DLEV                            
002371     MOVE ZERO                 TO DLEV-KDCALL                             
002372                                                                          
002373     CALL W411DLEV USING DLEV-W411DLEV DLEV-LEVF-PCB                      
002374                                       DLEV-LEVG-PCB                      
002375                                       DLEV-LEVA-PCB                      
002376                                       DLEV-ARTS-PCB                      
002377                                       DLEV-WDB6-PCB                      
002378                                       TPO2-FILA-PCB                      
002379                                                                          
002380     IF DLEV-IDLEVNR-UT NOT = SPACE                                       
002381       IF DLEV-IDDC-UT NOT = DLEV-IDDC-IN                                 
002382         MOVE DLEV-IDDC-UT     TO ORAD-IDDC                               
002383       END-IF                                                             
002384     END-IF                                                               
002385                                                                          
002386     MOVE DLEV-KDORDSTA-UT     TO AVSR-KDORDSTA (WS-INDEX-WOPS)           
002387     MOVE DLEV-KDVIA-UT        TO AVSR-KDVIA    (WS-INDEX-WOPS)           
002388     MOVE DLEV-KVDAGAR-DIFF-UT TO                                         
002389                               AVSR-KVDAGAR-DIFF(WS-INDEX-WOPS)           
002390     MOVE DLEV-TISKEPPN-DDC-UT TO                                         
002391                               AVSR-TISKEPPN-DDC(WS-INDEX-WOPS)           
002392                                                                          
002393     MOVE DLEV-IDLEVNR-UT      TO ORAD-IDLEVNR                            
002394     MOVE DLEV-FLRESTN-UT      TO ORAD-FLRESTN                            
002395     MOVE DLEV-FLSDCLEV-UT     TO ORAD-FLSDCLEV                           
002396     IF DLEV-FLSDCLEV-UT = JA                                             
002397       MOVE DLEV-IDDC-UT       TO ORAD-IDDC                               
002398                                  WS-IDDC                                 
002399     ELSE                                                                 
002400       MOVE DLEV-KDOI-UT       TO ORAD-KDOI                               
002401       MOVE DLEV-CLEARGROUP    TO ORAD-CLEARGROUP                         
002402     END-IF                                                               
002403     .                                                                    
002404     EJECT                                                                
002405 S02AB-UPPDATERA-WDE8 SECTION.                                            
002406                                                                          
002407     MOVE W-IDDISTR        TO W-E8-IDDISTR                                
002408     MOVE W-IDKUNDNR       TO W-E8-IDKUNDNR                               
002409     MOVE OBKR-IDKUNDRF-RO TO W-E8-IDKUNDRF                               
002410     PERFORM IMS-35-GHU-PROC-WDE801                                       
002411                                                                          
002412     MOVE +1                   TO WS-INDEX                                
002413     PERFORM UNTIL WS-INDEX > 10                                          
002414        IF PHUV-IDKUNDRF-ING(WS-INDEX) = W-IDKUNDRF                       
002415           MOVE +10            TO WS-INDEX                                
002416        ELSE                                                              
002417           IF PHUV-IDKUNDRF-ING(WS-INDEX) = '0000000   '                  
002418              MOVE W-IDKUNDRF  TO PHUV-IDKUNDRF-ING(WS-INDEX)             
002419                                                                          
002420              PERFORM IMS-36-REPL-PROC-WDE801                             
002421              MOVE +10         TO WS-INDEX                                
002422           END-IF                                                         
002423        END-IF                                                            
002424        ADD  +1                TO WS-INDEX                                
002425     END-PERFORM                                                          
002426     .                                                                    
002427     EJECT                                                                
002428                                                                          
002429 S02AC-KOMPLETTERA-RANSONERING SECTION.                                   
002430                                                                          
002431     MOVE ORAD-BERADREF        TO RANS-BERADREF                           
002432     MOVE OHUV-FLEMBORD        TO RANS-FLEMBORD                           
002433     MOVE OHUV-FLFORBI         TO RANS-FLFORBI                            
002434     MOVE OHUV-FLORDSPE        TO RANS-FLORDSPE                           
002435     MOVE OHUV-FLOVRLEV        TO RANS-FLOVRLEV                           
002436     MOVE ORAD-IDKAMPRF        TO RANS-IDKAMPRF                           
002437     MOVE ORAD-IDARTNR         TO RANS-IDARTNR                            
002438     MOVE ORAD-IDLEVNR         TO RANS-IDLEVNR                            
002439     MOVE OHUV-IDRFTAB         TO RANS-IDRFTAB                            
002440     MOVE ORAD-TIRODAT         TO RANS-TIRODAT                            
002441     MOVE +2                   TO RANS-KDORDBEH                           
002442     MOVE OHUV-KDORDKL         TO RANS-KDORDKL                            
002443     MOVE ORAD-KVBEART-Q       TO RANS-KVBEART-Q                          
002444     MOVE ORAD-KDTPOTYP        TO RANS-KDTPOTYP                           
002445     MOVE AREG-KDERS           TO RANS-KDERS                              
002446     MOVE AREG-KVLS            TO RANS-KVLS                               
002447     MOVE AREG-KVPB-SATS       TO RANS-KVPB-SATS                          
002448     MOVE AREG-KVPB-SEP        TO RANS-KVPB-SEP                           
002449     MOVE AREG-REDIRLEV        TO RANS-REDIRLEV                           
002450     MOVE AREG-KVRESS          TO RANS-KVRESS                             
002451     MOVE AREG-KVSPANT         TO RANS-KVSPANT                            
002452     MOVE AREG-KVUTRS          TO RANS-KVUTRS                             
002453     MOVE AREG-TIDISPIN        TO RANS-TIDISPIN                           
002454     MOVE AREG-KDPRODSL        TO TEST-KDPRODSL                           
002455     IF KDPRODSL-BIMA                                                     
002456       MOVE 1                  TO ORAD-RERF-RAD                           
002457                                  RANS-RERF-RAD-UT                        
002458       MOVE ZERO               TO RANS-SUTPO-PB-UT                        
002459                                  RANS-SUTPO-EJPB-UT                      
002460                                  RANS-RERF-ART-UT                        
002461     ELSE                                                                 
002462       CALL W411RANS USING RANS-W411RANS RANS-XXKM-PCB                    
002463                           RANS-ARTM-PCB RANS-ARTS-PCB                    
002464                                                                          
002465       MOVE RANS-RERF-RAD-UT TO ORAD-RERF-RAD                             
002466     END-IF                                                               
002467     .                                                                    
002468     EJECT                                                                
002469                                                                          
002470 S02AE-PREL-AVBOKNING SECTION.                                            
002471                                                                          
002472     MOVE JA                   TO CDCA-FLRESTN-IN                         
002473     MOVE OHUV-FLFORBI         TO CDCA-FLFORBI-IN                         
002474     MOVE OHUV-FLORDSPE        TO CDCA-FLORDSPE-IN                        
002475     MOVE OHUV-FLOVRLEV        TO CDCA-FLOVRLEV-IN                        
002476     MOVE OHUV-FLPRELRO        TO CDCA-FLPRELRO-IN                        
002477     MOVE ORAD-FLRESTN         TO CDCA-FLRESTN-IN                         
002478     MOVE OBKR-FLSLATT         TO CDCA-FLSLATT-IN                         
002479     MOVE ORAD-IDARTNR         TO CDCA-IDARTNR-IN                         
002480     MOVE OHUV-IDDISTR         TO CDCA-IDDISTR-IN                         
002481     MOVE ORAD-IDDC            TO CDCA-IDDC-IN                            
002482     MOVE ORAD-IDKAMPRF        TO CDCA-IDKAMPRF-IN                        
002483     MOVE ORAD-IDKUNDRF-RO     TO CDCA-IDKUNDRF-RO-IN                     
002484     MOVE ORAD-IDSYSTEM        TO CDCA-IDSYSTEM-IN                        
002485     MOVE ORAD-IDLEVNR         TO CDCA-IDLEVNR-IN                         
002486     MOVE +0                   TO CDCA-KDERS-IN                           
002487     MOVE ORAD-KDKVBRYT        TO CDCA-KDKVBRYT-IN                        
002488     MOVE ORAD-KDORDKL         TO CDCA-KDORDKL-IN                         
002489     MOVE ORAD-KDPRODSL        TO CDCA-KDPRODSL-IN                        
002490     MOVE AREG-KDSORT          TO CDCA-KDSORT-IN                          
002491     MOVE ORAD-KDTPOTYP        TO CDCA-KDTPOTYP-IN                        
002492     MOVE AREG-KDUART          TO CDCA-KDUART-IN                          
002493     MOVE ORAD-KVBEART         TO CDCA-KVBEART-IN                         
002494     MOVE ORAD-KVBEART-Q       TO CDCA-KVBEART-Q-IN                       
002495     MOVE AREG-KVQPACK-0       TO CDCA-KVQPACK-0-IN                       
002496     MOVE AREG-KVQPACK-1       TO CDCA-KVQPACK-1-IN                       
002497     MOVE AREG-IDFKNGRP        TO CDCA-IDFKNGRP-IN                        
002498     MOVE RANS-RERF-RAD-UT     TO CDCA-RERF-RAD-IN                        
002499     MOVE RANS-RERF-ART-UT     TO CDCA-RERF-ART-IN                        
002500     MOVE OHUV-RESLATT         TO CDCA-RESLATT-IN                         
002501     MOVE SPACE                TO CDCA-KDPROTYP-IN                        
002502     MOVE AREG-KVAKS-CDC       TO CDCA-KVAKS-CDC-IN                       
002503     MOVE AREG-KVAKS-PAV       TO CDCA-KVAKS-PAV-IN                       
002504     MOVE AREG-KVLS            TO CDCA-KVLS-IN                            
002505     MOVE AREG-KVRESS          TO CDCA-KVRESS-IN                          
002506     MOVE AREG-KVSPANT         TO CDCA-KVSPANT-IN                         
002507     MOVE AREG-KVUTRS          TO CDCA-KVUTRS-IN                          
002508     MOVE AREG-KVSPARR-KVAL    TO CDCA-KVSPARR-KVAL-IN                    
002509     MOVE ORAD-IDKUNDNR        TO CDCA-IDKUNDNR-IN                        
002510     MOVE ORAD-BERADREF        TO CDCA-BERADREF-IN                        
002511     MOVE +1                   TO CDCA-KDCALL                             
002512                                                                          
002513     CALL W411CDCA USING CDCA-W411CDCA CDCA-ARTM-PCB                      
002514                                       CDCA-INLB-PCB                      
002515                                       CDCA-WDB2-PCB                      
002516                                       CDCA-WDC1-PCB                      
002517     EJECT                                                                
002518     MOVE CDCA-FLAKPLOC-UT     TO ORAD-FLAKPLOC                           
002519     MOVE CDCA-KVBEART-UT      TO ORAD-KVBEART                            
002520     MOVE CDCA-KVBEART-Q-UT    TO ORAD-KVBEART-Q                          
002521     MOVE CDCA-KVPREAVB-UT     TO ORAD-KVPREAVB                           
002522     MOVE CDCA-KVPRERO-UT      TO ORAD-KVPRERO                            
002523     MOVE CDCA-KVSLATT-UT      TO ORAD-KVSLATT                            
002524     MOVE CDCA-RERF-RAD-UT     TO ORAD-RERF-RAD                           
002525     .                                                                    
002526     EJECT                                                                
002527 S02AF-SKRIV-Q1-OCH-Q4-RADER SECTION.                                     
002528                                                                          
002529     MOVE AREG-ADLAGOMR     TO ORAD-ADLAGOMR                              
002530     MOVE AREG-ADGANG       TO ORAD-ADGANG                                
002531     MOVE AREG-ADPLATS      TO ORAD-ADPLATS                               
002532                                                                          
002533     IF ORAD-KVPREAVB > +0 OR ORAD-KVPRERO > +0                           
002534       PERFORM S09-KONTROLLERA-ENHETSLAST                                 
002535       PERFORM S10-BERAKNA-WOPS-SKRIV-ORAD                                
002536     END-IF                                                               
002537                                                                          
002538     IF CDCA-KDORDBEK-UT >  0                                             
002539       PERFORM S02AFA-SKRIV-ORDERBEKR                                     
002540     END-IF                                                               
002541     .                                                                    
002542     EJECT                                                                
002543 S02AFA-SKRIV-ORDERBEKR SECTION.                                          
002544                                                                          
002545     MOVE ORAD-FLAKPLOC        TO OBKR-FLAKPLOC                           
002546     MOVE ORAD-IDDC            TO OBKR-IDDC                               
002547     MOVE ORAD-KVBEART         TO OBKR-KVBEART                            
002548     MOVE ORAD-KVBEART-Q       TO OBKR-KVBEART-Q                          
002549     MOVE ORAD-KVPREAVB        TO OBKR-KVPREAVB                           
002550     MOVE ORAD-KVPRERO         TO OBKR-KVPRERO                            
002551     MOVE ORAD-KVSLATT         TO OBKR-KVSLATT                            
002552                                                                          
002553     MOVE OHUV-KDORDTYP-LDC   TO OBKR-KDORDTYP-LDC                        
002554     MOVE OHUV-TIREPDAT       TO OBKR-TIREPDAT                            
002555     MOVE ORAD-IDKUNDRF-WIP   TO OBKR-IDKUNDRF-WIP                        
002556                                                                          
002557     IF CDCA-KDORDBEK-UT > +0                                             
002558*----(KOD 80, 92, 99)                                                     
002559        IF CDCA-KDORDBEK-UT =  80                                         
002560          MOVE CDCA-KVANNANT-UT TO OBKR-KVANNANT                          
002561        END-IF                                                            
002562                                                                          
002563        IF DLEV-IDLEVNR-UT NOT = SPACE                                    
002564           MOVE ORAD-KVBEART-Q TO OBKR-KVPREAVB                           
002565        END-IF                                                            
002566        MOVE CDCA-KDORDBEK-UT  TO OBKR-KDORDBEK                           
002567        MOVE '4223CDCA'        TO OBKR-IDPGM                              
002568        ADD +1                 TO OBKR-IDSEKVNR                           
002569        PERFORM  IMS-07-ISRT-ORQM-WDQ101                                  
002570     END-IF                                                               
002571     .                                                                    
002572     EJECT                                                                
002573                                                                          
002574 S02B-SKRIV-ORDERBEKR-40 SECTION.                                         
002575                                                                          
002576     MOVE OBKR-KVBEART-TILLK   TO SPAR-KVBEART-TILLK                      
002577     MOVE OBKR-IDARTNR-TILLK   TO SPAR-IDARTNR-TILLK                      
002578     MOVE OBKR-REKSIFFR-TILLK  TO SPAR-REKSIFFR-TILLK                     
002579     MOVE OBKR-DIERS-KVOT      TO SPAR-DIERS-KVOT                         
002580                                                                          
002581     IF OBKR-IDARTNR NOT = SPAR-IDARTNR-40  OR                            
002582         (OBKR-IDARTNR = SPAR-IDARTNR-40   AND                            
002583          OBKR-IDLOPNR NOT = SPAR-IDLOPNR-40)                             
002584                                                                          
002585        PERFORM S02BA-SKRIV-KOD40-ERSATT-ART                              
002586        COMPUTE SPAR-IDSEKVNR-40 = OBKR-IDSEKVNR + 1                      
002587     END-IF                                                               
002588                                                                          
002589     MOVE SPAR-KVBEART-TILLK   TO OBKR-KVBEART-TILLK                      
002590     MOVE SPAR-IDARTNR-TILLK   TO OBKR-IDARTNR-TILLK                      
002591     MOVE SPAR-REKSIFFR-TILLK  TO OBKR-REKSIFFR-TILLK                     
002592     MOVE SPAR-DIERS-KVOT      TO OBKR-DIERS-KVOT                         
002593                                                                          
002594     MOVE JA                   TO OBKR-FLOBOK                             
002595     MOVE 40                   TO OBKR-KDORDBEK                           
002596     MOVE IDPGM                TO OBKR-IDPGM                              
002597     MOVE SPAR-IDSEKVNR-40     TO OBKR-IDSEKVNR                           
002598                                                                          
002599     PERFORM IMS-07-ISRT-ORQM-WDQ101                                      
002600     ADD +1                 TO OBKR-IDSEKVNR                              
002601                               SPAR-IDSEKVNR-40                           
002602     MOVE OBKR-IDARTNR         TO SPAR-IDARTNR-40                         
002603     MOVE OBKR-IDLOPNR         TO SPAR-IDLOPNR-40                         
002604                                                                          
002605     PERFORM S02BB-UPPDAT-ERSATT-ART                                      
002606     .                                                                    
002607     EJECT                                                                
002608 S02BA-SKRIV-KOD40-ERSATT-ART SECTION.                                    
002609                                                                          
002610     MOVE OBKR-IDARTNR      TO W-Q1-IDARTNR-MIN1                          
002611                               W-Q1-IDARTNR-MAX1                          
002612     MOVE OBKR-IDLOPNR      TO W-Q1-IDLOPNR-MIN1                          
002613                               W-Q1-IDLOPNR-MAX1                          
002614     MOVE +3                TO W-Q1-IDSEKVNR-MIN1                         
002615     MOVE +999              TO W-Q1-IDSEKVNR-MAX1                         
002616                                                                          
002617     PERFORM IMS-33-GN-ORQM-WDQ101                                        
002618     PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                           
002619                                                                          
002620        PERFORM IMS-33-GN-ORQM-WDQ101                                     
002621     END-PERFORM                                                          
002622                                                                          
002623     MOVE +0                TO OBKR-IDARTNR-TILLK                         
002624                               OBKR-KVBEART-TILLK                         
002625                               OBKR-REKSIFFR-TILLK                        
002626                               OBKR-DIERS-KVOT                            
002627     MOVE JA                TO OBKR-FLOBOK                                
002628     MOVE 40                TO OBKR-KDORDBEK                              
002629     MOVE IDPGM             TO OBKR-IDPGM                                 
002630     ADD +1                 TO OBKR-IDSEKVNR                              
002631                                                                          
002632     PERFORM IMS-07-ISRT-ORQM-WDQ101                                      
002633     .                                                                    
002634     EJECT                                                                
002635 S02BB-UPPDAT-ERSATT-ART SECTION.                                         
002636                                                                          
002637*--VID VAL AV ERSÄTTNING SÄTTS FLOBTRAN TILL 'N' FÖR ATT FÖRHINDRA        
002638*--ATT ORDERBEKRÄFTELSETRANS SKICKAS FRÅN W4029300 TILL VR/VIPS           
002639                                                                          
002640     MOVE OBKR-IDARTNR      TO W-Q1-IDARTNR-MIN1                          
002641                               W-Q1-IDARTNR-MAX1                          
002642     MOVE OBKR-IDLOPNR      TO W-Q1-IDLOPNR-MIN1                          
002643                               W-Q1-IDLOPNR-MAX1                          
002644     MOVE +1                TO W-Q1-IDSEKVNR-MIN1                         
002645                               W-Q1-IDSEKVNR-MAX1                         
002646                                                                          
002647     PERFORM IMS-34-GHU-ORQM-WDQ101                                       
002648     MOVE NEJ              TO OBKR-FLOBTRAN                               
002649     PERFORM IMS-06-REPL-ORQM-WDQ101                                      
002650     .                                                                    
002651     EJECT                                                                
002652 S02C-UPPDATERA-TPO2 SECTION.                                             
002653                                                                          
002654     IF WS-IDDC NOT = W-IDDC-B6                                           
002655        MOVE WS-IDDC TO W-IDDC-B6                                         
002656        PERFORM IMS-GU-WDB601                                             
002657     END-IF                                                               
002658                                                                          
002659     IF DCS-CDC OR DCS-SDC                                                
002660       PERFORM S12-BYGG-UPP-ORDERRAD                                      
002661                                                                          
002662       MOVE ORAD-IDDISTR       TO TPO2-IDDISTR                            
002663       MOVE ORAD-IDKUNDNR      TO TPO2-IDKUNDNR                           
002664       MOVE ORAD-IDKUNDRF      TO TPO2-IDKUNDRF                           
002665       MOVE ORAD-IDARTNR       TO TPO2-IDARTNR                            
002666       MOVE ORAD-BERADREF      TO TPO2-BERADREF                           
002667       MOVE AREG-IDANSK        TO TPO2-IDANSK                             
002668                                                                          
002669       MOVE OHUV-IDKONTO       TO TPO2-IDKONTO                            
002670       MOVE OHUV-IDKST         TO TPO2-IDKST                              
002671       MOVE OHUV-IDANALYS      TO TPO2-IDANALYS                           
002672       MOVE ORAD-KDDSP         TO TPO2-KDDSP                              
002673       MOVE OHUV-KDFAKTYP      TO TPO2-KDFAKTYP                           
002674       MOVE ARB-KDFRAKT        TO TPO2-KDFRAKT                            
002675       MOVE ORAD-KDKVBRYT      TO TPO2-KDKVBRYT                           
002676       MOVE ORAD-KDORDING      TO TPO2-KDORDING                           
002677       MOVE OHUV-KDORDKL       TO TPO2-KDORDKL                            
002678       MOVE ORAD-KDPRODSL      TO TPO2-KDPRODSL                           
002679       MOVE +2                 TO TPO2-KDTPOTYP                           
002680       MOVE ORAD-KDVRINFO      TO TPO2-KDVRINFO                           
002681       MOVE ORAD-KVBEART-Q     TO TPO2-KVBEART-Q                          
002682       MOVE ORAD-PRARTNTO      TO TPO2-PRARTNTO                           
002683       MOVE ORAD-DEAL-PR-LINE  TO TPO2-DEAL-PR-LINE                       
002684       MOVE ORAD-REKSIFFR      TO TPO2-REKSIFFR                           
002685       MOVE ORAD-TITPO         TO TPO2-TITPO                              
002686       MOVE ORAD-KDPRTYP       TO TPO2-KDPRTYP                            
002687       MOVE ORAD-BEVOLREF      TO TPO2-BEVOLREF                           
002688       MOVE ORAD-FLINVEST      TO TPO2-FLINVEST                           
002689       MOVE OHUV-FLORDSPE      TO TPO2-FLORDSPE                           
002690       MOVE OHUV-FLOVRLEV      TO TPO2-FLOVRLEV                           
002691       MOVE OHUV-FLFORBI       TO TPO2-FLFORBI                            
002692       MOVE ORAD-FLPRTILL      TO TPO2-FLPRTILL                           
002693       MOVE OHUV-BEKUNDRF      TO TPO2-BEKUNDRF                           
002694       MOVE ORAD-IDKAMPRF      TO TPO2-IDKAMPRF                           
002695       MOVE ORAD-IDLEVNR       TO TPO2-IDLEVNR                            
002696       MOVE ORAD-IDSYSTEM      TO TPO2-IDSYSTEM                           
002697       MOVE AREG-KDUART        TO TPO2-KDUART                             
002698       MOVE AREG-KVFRYSTI      TO TPO2-KVFRYSTI                           
002699       MOVE +2                 TO TPO2-KDORDBEH                           
002700       MOVE ORAD-FLTILLK       TO TPO2-FLTILLK                            
002701       MOVE AREG-TIDISPIN      TO TPO2-TIDISPIN                           
002702       MOVE OHUV-BEVARREF      TO TPO2-BEVARREF                           
002703       MOVE OBKR-KVQPACK       TO TPO2-KVQPACK-1                          
002704       MOVE ORAD-KVBEART       TO TPO2-KVBEART                            
002705                                                                          
002706       MOVE OHUV-KDORDTYP-LDC TO TPO2-KDORDTYP-LDC                        
002707       MOVE OHUV-TIREPDAT     TO TPO2-TIREPDAT                            
002708       MOVE ORAD-IDKUNDRF-WIP TO TPO2-IDKUNDRF-WIP                        
002709                                                                          
002710       CALL W411TPO2 USING TPO2-W411TPO2 TPO2-ORDP-PCB                    
002711                                       TPO2-XXBU-PCB TPO2-XXBV-PCB        
002712                         TPO2-ARTM-PCB TPO2-FILA-PCB TPO2-XXBX-PCB        
002713                           TIME-4437-PCB                                  
002714     END-IF                                                               
002715     .                                                                    
002716     EJECT                                                                
002717                                                                          
002718 S02D-UPPDATERA-TPO6 SECTION.                                             
002719                                                                          
002720     IF WS-IDDC NOT = W-IDDC-B6                                           
002721        MOVE WS-IDDC TO W-IDDC-B6                                         
002722        PERFORM IMS-GU-WDB601                                             
002723     END-IF                                                               
002724                                                                          
002725     IF DCS-CDC OR DCS-SDC                                                
002726       PERFORM S12-BYGG-UPP-ORDERRAD                                      
002727                                                                          
002728       MOVE ORAD-IDDISTR       TO TPO6-IDDISTR                            
002729       MOVE ORAD-IDKUNDNR      TO TPO6-IDKUNDNR                           
002730       MOVE ORAD-IDKUNDRF      TO TPO6-IDKUNDRF                           
002731       MOVE ORAD-IDARTNR       TO TPO6-IDARTNR                            
002732       MOVE OHUV-IDDC-PRIM     TO TPO6-IDDC-DAY                           
002733       MOVE AREG-FLREFILL      TO TPO6-FLREFILL                           
002734       MOVE AREG-KDUART        TO TPO6-KDUART                             
002735       MOVE AREG-REDIRLEV      TO TPO6-REDIRLEV                           
002736       MOVE ORAD-BERADREF      TO TPO6-BERADREF                           
002737       MOVE AREG-IDANSK        TO TPO6-IDANSK                             
002738                                                                          
002739       MOVE OHUV-IDKONTO       TO TPO6-IDKONTO                            
002740       MOVE OHUV-IDKST         TO TPO6-IDKST                              
002741       MOVE ORAD-KDDSP         TO TPO6-KDDSP                              
002742       MOVE OHUV-KDFAKTYP      TO TPO6-KDFAKTYP                           
002743       MOVE ARB-KDFRAKT        TO TPO6-KDFRAKT                            
002744       MOVE ORAD-KDKVBRYT      TO TPO6-KDKVBRYT                           
002745       MOVE ORAD-KDORDING      TO TPO6-KDORDING                           
002746       MOVE OHUV-KDORDKL       TO TPO6-KDORDKL                            
002747       MOVE ORAD-KDPRODSL      TO TPO6-KDPRODSL                           
002748       MOVE +6                 TO TPO6-KDTPOTYP                           
002749       MOVE ORAD-KDVRINFO      TO TPO6-KDVRINFO                           
002750       MOVE ORAD-KVBEART-Q     TO TPO6-KVBEART-Q                          
002751       MOVE ORAD-PRARTNTO      TO TPO6-PRARTNTO                           
002752       MOVE ORAD-DEAL-PR-LINE  TO TPO6-DEAL-PR-LINE                       
002753       MOVE AREG-REKSIFFR      TO TPO6-REKSIFFR                           
002754       MOVE ORAD-KDPRTYP       TO TPO6-KDPRTYP                            
002755       MOVE ORAD-BEVOLREF      TO TPO6-BEVOLREF                           
002756       MOVE ORAD-FLINVEST      TO TPO6-FLINVEST                           
002757       MOVE ORAD-FLPRTILL      TO TPO6-FLPRTILL                           
002758       MOVE OHUV-BEKUNDRF      TO TPO6-BEKUNDRF                           
002759       MOVE ORAD-IDKAMPRF      TO TPO6-IDKAMPRF                           
002760       MOVE ORAD-IDLEVNR       TO TPO6-IDLEVNR                            
002761       MOVE ORAD-IDSYSTEM      TO TPO6-IDSYSTEM                           
002762       MOVE OHUV-FLFORBI       TO TPO6-FLFORBI                            
002763                                                                          
002764       MOVE OHUV-KDORDTYP-LDC TO TPO6-KDORDTYP-LDC                        
002765       MOVE OHUV-TIREPDAT     TO TPO6-TIREPDAT                            
002766       MOVE ORAD-IDKUNDRF-WIP TO TPO6-IDKUNDRF-WIP                        
002767                                                                          
002768       MOVE ORAD-KDOI         TO TPO6-KDOI                                
002769       MOVE ORAD-CLEARGROUP   TO TPO6-CLEARGROUP                          
002770                                                                          
002771       CALL W411TPO6 USING TPO6-W411TPO6 2109-PCB TPO6-ORDP-PCB           
002772           TPO6-XXBU-PCB TPO6-XXBV-PCB TPO6-XXBX-PCB                      
002773             TPO6-ARTS-PCB TIME-4437-PCB                                  
002774     END-IF                                                               
002775     .                                                                    
002776     EJECT                                                                
002777 S03-STARTA-RADBEHANDLINGEN SECTION.                                      
002778                                                                          
002779     MOVE MFS-KDMFSFOR         TO 4222-SPRAK                              
002780                                                                          
002781     MOVE WS-IDDISTR           TO 4222-MID-IDDISTR                        
002782     MOVE WS-IDKUNDNR          TO 4222-MID-IDKUNDNR                       
002783     MOVE WS-IDORDNR           TO 4222-MID-IDORDNR5                       
002784                                                                          
002785     PERFORM IMS-INSERT-4222-MSG                                          
002786     MOVE JA                   TO HOPP                                    
002787     .                                                                    
002788     EJECT                                                                
002789 S09-KONTROLLERA-ENHETSLAST SECTION.                                      
002790                                                                          
002791     MOVE ORAD-ADLAGOMR        TO LAST-ADLAGOMR                           
002792     MOVE OHUV-FLFORBI         TO LAST-FLFORBI                            
002793     MOVE OHUV-FLOVRLEV        TO LAST-FLOVRLEV                           
002794     MOVE OHUV-FLORDSPE        TO LAST-FLORDSPE                           
002795     MOVE ORAD-IDLEVNR         TO LAST-IDLEVNR                            
002796     MOVE ORAD-IDDC            TO LAST-IDDC                               
002797     MOVE ARB-KDFDKRAV         TO LAST-KDFDKRAV                           
002798     MOVE ORAD-KVPREAVB        TO LAST-KVPREAVB                           
002799     MOVE AREG-KVQPACK-3       TO LAST-KVQPACK-3                          
002800     MOVE AREG-KVQPACK-4       TO LAST-KVQPACK-4                          
002801                                                                          
002802     CALL W411LAST USING LAST-W411LAST                                    
002803     .                                                                    
002804     EJECT                                                                
002805 S10-BERAKNA-WOPS-SKRIV-ORAD SECTION.                                     
002806                                                                          
002807     IF LAST-ADLAGOMR-UT = +0 AND                                         
002808        LAST-KVANTAL-UT  = +0 AND                                         
002809        LAST-KVBEART-UT  = +0                                             
002810*------------------------------------------------------------*            
002811*-----ALLT MOT VANLIGT LAGEROMRÅDE ( 'VANLIG' ORDERRAD)------*            
002812*------------------------------------------------------------*            
002813        PERFORM S10A-FIXA-LAGEROMR-PLATS                                  
002814        PERFORM S10B-REDIGERA-WOPS-AREA                                   
002815        PERFORM IMS-17-ISRT-ORQF-WDQ401                                   
002816        PERFORM UNTIL SEGMENT-FINNS                                       
002817           ADD +1                    TO ORAD-IDLOPNR                      
002818           PERFORM IMS-17-ISRT-ORQF-WDQ401                                
002819        END-PERFORM                                                       
002820     ELSE                                                                 
002821                                                                          
002822        IF LAST-KVANTAL-UT > +0 AND LAST-KVBEART-UT > +0                  
002823*------------------------------------------------------------*            
002824*--------- RADEN MOT VANLIGT LAGEROMRÅDE (KVBEART)-----------*            
002825*------------------------------------------------------------*            
002826                                                                          
002827           MOVE LAST-KVBEART-UT      TO ORAD-KVPREAVB                     
002828           COMPUTE ORAD-KVBEART-Q = ORAD-KVPREAVB +                       
002829                                    ORAD-KVPRERO                          
002830           MOVE ORAD-KVSLATT         TO SPAR-ORAD-KVSLATT                 
002831           PERFORM S10C-BERAEKNA-KVSLATT                                  
002832           PERFORM S10A-FIXA-LAGEROMR-PLATS                               
002833           PERFORM S10B-REDIGERA-WOPS-AREA                                
002834           PERFORM IMS-17-ISRT-ORQF-WDQ401                                
002835           PERFORM UNTIL SEGMENT-FINNS                                    
002836              ADD +1                 TO ORAD-IDLOPNR                      
002837              PERFORM IMS-17-ISRT-ORQF-WDQ401                             
002838           END-PERFORM                                                    
002839*------------------------------------------------------------*            
002840*--------- RADEN MOT 'ENHETS' LAGEROMRÅDE (KVANTAL)----------*            
002841*------------------------------------------------------------*            
002842           MOVE +0                   TO ORAD-KVBEART                      
002843           MOVE LAST-KVANTAL-UT      TO ORAD-KVBEART-Q                    
002844                                        ORAD-KVPREAVB                     
002845           MOVE LAST-ADLAGOMR-UT     TO ORAD-ADLAGOMR                     
002846           IF LAST-ADLAGOMR-UT = 62 OR 63 OR 64                           
002847             CONTINUE                                                     
002848           ELSE                                                           
002849             IF LAST-ADGANG-UT > ZERO                                     
002850               MOVE LAST-ADGANG-UT   TO ORAD-ADGANG                       
002851             END-IF                                                       
002852           END-IF                                                         
002853           MOVE 1.0000               TO ORAD-RERF-RAD                     
002854           COMPUTE ORAD-KVSLATT = SPAR-ORAD-KVSLATT - ORAD-KVSLATT        
002855           PERFORM S10A-FIXA-LAGEROMR-PLATS                               
002856           PERFORM S10B-REDIGERA-WOPS-AREA                                
002857           PERFORM IMS-17-ISRT-ORQF-WDQ401                                
002858           PERFORM UNTIL SEGMENT-FINNS                                    
002859              ADD +1                 TO ORAD-IDLOPNR                      
002860              PERFORM IMS-17-ISRT-ORQF-WDQ401                             
002861           END-PERFORM                                                    
002862        ELSE                                                              
002863*------------------------------------------------------------*            
002864*-----ALLT MOT 'ENHETS' LAGEROMRÅDE -------------------------*            
002865*------------------------------------------------------------*            
002866           MOVE LAST-ADLAGOMR-UT  TO ORAD-ADLAGOMR                        
002867           IF LAST-ADLAGOMR-UT = 62 OR 63 OR 64                           
002868             CONTINUE                                                     
002869           ELSE                                                           
002870             IF LAST-ADGANG-UT > ZERO                                     
002871               MOVE LAST-ADGANG-UT   TO ORAD-ADGANG                       
002872             END-IF                                                       
002873           END-IF                                                         
002874           MOVE 1.0000            TO ORAD-RERF-RAD                        
002875           PERFORM S10A-FIXA-LAGEROMR-PLATS                               
002876           PERFORM S10B-REDIGERA-WOPS-AREA                                
002877           PERFORM IMS-17-ISRT-ORQF-WDQ401                                
002878           PERFORM UNTIL SEGMENT-FINNS                                    
002879              ADD +1              TO ORAD-IDLOPNR                         
002880              PERFORM IMS-17-ISRT-ORQF-WDQ401                             
002881           END-PERFORM                                                    
002882        END-IF                                                            
002883     END-IF                                                               
002884     .                                                                    
002885     EJECT                                                                
002886 S10A-FIXA-LAGEROMR-PLATS SECTION.                                        
002887                                                                          
002888     MOVE ORAD-ADLAGOMR        TO ADRS-ADLAGOMR-IN                        
002889     MOVE ORAD-ADPLATS         TO ADRS-ADPLATS-IN                         
002890                                                                          
002891     IF OHUV-BEVARREF = SPACE                                             
002892       MOVE ORAD-BERADREF   TO ADRS-BEVARREF-IN                           
002893     ELSE                                                                 
002894       MOVE OHUV-BEVARREF   TO WS-HFAK-REF-X10                            
002895       PERFORM S10AA-KOLLA-I-HFAK-TAB                                     
002896       IF BEVARREF-I-HFAK-TAB                                             
002897         MOVE OHUV-BEVARREF TO ADRS-BEVARREF-IN                           
002898       ELSE                                                               
002899         MOVE ORAD-BERADREF TO ADRS-BEVARREF-IN                           
002900       END-IF                                                             
002901     END-IF                                                               
002902     MOVE OHUV-FLFORBI         TO ADRS-FLFORBI-IN                         
002903     MOVE OHUV-IDDISTR         TO ADRS-IDDISTR-IN                         
002904     MOVE 1                    TO ADRS-KDCALL-IN                          
002905     MOVE ORAD-IDDC            TO ADRS-IDDC-IN                            
002906     MOVE OHUV-KDORDKL         TO ADRS-KDORDKL-IN                         
002907     MOVE ORAD-KVBEART-Q       TO ADRS-KVBEART-Q-IN                       
002908     MOVE ORAD-VLARTNTO        TO ADRS-VLARTNTO-IN                        
002909                                                                          
002910     CALL W413ADRS USING ADRS-W413ADRS                                    
002911                                                                          
002912*- KAMPANJORDRAR FÅR ETT FIKTIVT LAGEROMRÅDE.E'TRACKER 2218613.           
002913     IF ORAD-IDKAMPRF > 0                                                 
002914       MOVE 8                  TO ORAD-ADLAGOMR                           
002915     ELSE                                                                 
002916       MOVE ADRS-ADLAGOMR-UT   TO ORAD-ADLAGOMR                           
002917     END-IF                                                               
002918                                                                          
002919*- CHINESE COMPULSORY CERTIF. = FIKTIVT ORDEROMR. E'TR.6605105.           
002920     IF WS-IDDC NOT = W-IDDC-B6                                           
002921        MOVE WS-IDDC TO W-IDDC-B6                                         
002922        PERFORM IMS-GU-WDB601                                             
002923     END-IF                                                               
002924     MOVE OHUV-IDDISTR         TO TEST-IDDISTR                            
002925     MOVE ADRS-ADLAGOMR-UT     TO ORAD-ADLAGOMR                           
002926     MOVE ADRS-ADPLATS-UT      TO ORAD-ADPLATS                            
002927                                                                          
002928     .                                                                    
002929     EJECT                                                                
002930 S10AA-KOLLA-I-HFAK-TAB   SECTION.                                        
002931                                                                          
002932     IF WS-HFAK-REF-X10 NOT = SPACE                                       
002933        MOVE 1 TO HFAK-TAB-IX                                             
002934        PERFORM UNTIL HFAK-TAB-IX > MAX-HFAK-IX                           
002935           IF WS-HFAK-REF-X10 (1:3) = HFAK-BERADREF (HFAK-TAB-IX)         
002936           OR (WS-HFAK-REF-X10 (1:1) = '#'                                
002937           AND WS-HFAK-REF-X10 (2:2) NOT = SPACE                          
002938           AND HFAK-BERADREF-1 (HFAK-TAB-IX) = '#')                       
002939              MOVE JA TO BEVARREF-I-HFAK-TAB-SW                           
002940              MOVE 99 TO HFAK-TAB-IX                                      
002941           END-IF                                                         
002942           ADD 1 TO HFAK-TAB-IX                                           
002943        END-PERFORM                                                       
002944     END-IF                                                               
002945     .                                                                    
002946     EJECT                                                                
002947                                                                          
002948 S10B-REDIGERA-WOPS-AREA SECTION.                                         
002949                                                                          
002950     MOVE +1                   TO AVSR-KDCALL                             
002951     MOVE OHUV-IDORDER         TO AVSR-IDORDER                            
002952     MOVE OHUV-KDORDKL         TO AVSR-KDORDKL                            
002953     MOVE ARB-KDFRAKT          TO AVSR-KDFRAKT                            
002954     MOVE ARB-KDROPACK         TO AVSR-KDROPACK                           
002955     MOVE MSGI-TILOKDAT        TO AVSR-TIREGDAT                           
002956     MOVE MSGI-TILOKTID        TO AVSR-TIHHMM                             
002957                                                                          
002958     MOVE ORAD-ADLAGOMR        TO AVSR-ADLAGOMR(WS-INDEX-WOPS)            
002959     MOVE ORAD-IDLEVNR         TO AVSR-IDLEVNR(WS-INDEX-WOPS)             
002960     MOVE ORAD-IDDC            TO AVSR-IDDC(WS-INDEX-WOPS)                
002961     MOVE ORAD-KDSPEEMB        TO AVSR-KDSPEEMB(WS-INDEX-WOPS)            
002962     MOVE +0                   TO AVSR-KVANNANT(WS-INDEX-WOPS)            
002963     MOVE ORAD-KVBEART-Q       TO AVSR-KVBEART-Q(WS-INDEX-WOPS)           
002964     MOVE ORAD-PRARTNTO        TO AVSR-PRARTNTO(WS-INDEX-WOPS)            
002965     MOVE ORAD-PRAVCOST        TO AVSR-PRAVCOST(WS-INDEX-WOPS)            
002966     MOVE ORAD-DEAL-PR-LINE    TO AVSR-DEAL-PR-LINE(WS-INDEX-WOPS)        
002967     MOVE ORAD-VKART           TO AVSR-VKART(WS-INDEX-WOPS)               
002968     MOVE ORAD-VLARTNTO        TO AVSR-VLARTNTO(WS-INDEX-WOPS)            
002969     MOVE AREG-KDVSOP          TO AVSR-KDVSOP(WS-INDEX-WOPS)              
002970     MOVE AREG-KDFARLIG        TO AVSR-KDFARLIG(WS-INDEX-WOPS)            
002971                                                                          
002972     IF WS-IDDC NOT = W-IDDC-B6                                           
002973        MOVE WS-IDDC TO W-IDDC-B6                                         
002974        PERFORM IMS-GU-WDB601                                             
002975     END-IF                                                               
002976                                                                          
002977     IF DCS-DDC                                                           
002978       PERFORM S02AA-KOLLA-DIREKTLEVERANS                                 
002979     END-IF                                                               
002980                                                                          
002981     CALL W413AVSR USING AVSR-W413AVSR AVSR-ALT-PCB AVSR-ORQI-PCB         
002982                         AVSR-GMTB-PCB AVSR-GMTC-PCB                      
002983                         AVSR-WDB2-PCB AVSR-WDB6-PCB TRAN-XXKB-PCB        
002984     .                                                                    
002985     EJECT                                                                
002986 S10C-BERAEKNA-KVSLATT SECTION.                                           
002987                                                                          
002988     IF ORAD-FLRESTN = JA AND OHUV-RESLATT > +0                           
002989                                                                          
002990        COMPUTE WS-RESLATT = OHUV-RESLATT / 100                           
002991                                                                          
002992        COMPUTE ORAD-KVSLATT ROUNDED =                                    
002993               (ORAD-KVPREAVB + ORAD-KVPRERO) * WS-RESLATT                
002994     END-IF                                                               
002995     .                                                                    
002996     EJECT                                                                
002997 S11-SKRIV-VOR-RAD SECTION.                                               
002998                                                                          
002999     MOVE OBKR-IDDISTR         TO 4542-IDDISTR                            
003000     MOVE OBKR-IDDC            TO WS-IDDC                                 
003001                                                                          
003002     IF OBKR-IDARTNR-TILLK > +0                                           
003003       MOVE OBKR-IDARTNR-TILLK TO W-IDARTNR                               
003004                                  4542-IDARTNR                            
003005     ELSE                                                                 
003006       MOVE OBKR-IDARTNR       TO W-IDARTNR                               
003007                                  4542-IDARTNR                            
003008     END-IF                                                               
003009     MOVE OBKR-IDDC            TO W-IDDC                                  
003010                                  4542-IDDC                               
003011                                                                          
003012     PERFORM IMS-GU-WDK722                                                
003013     IF SEGMENT-FINNS AND XLAG-IDANSK > 0                                 
003014        MOVE XLAG-IDANSK       TO WS-IDANSK                               
003015     ELSE                                                                 
003016        PERFORM IMS-10-GU-WDK611                                          
003017        MOVE CLAG-IDANSK       TO WS-IDANSK                               
003018     END-IF                                                               
003019     MOVE WS-IDANSK            TO 4542-IDANSK                             
003020     MOVE +1                   TO 4542-IDLOPNR                            
003021     MOVE OBKR-IDORDER         TO 4542-IDORDER                            
003022     MOVE OBKR-BERADREF        TO 4542-BERADREF                           
003023     MOVE OBKR-IDKUNDNR        TO 4542-IDKUNDNR                           
003024     MOVE OBKR-IDKUNDRF        TO 4542-IDKUNDRF                           
003025     MOVE OHUV-IDUSER          TO 4542-IDUSER                             
003026     MOVE OBKR-KDORDBEK        TO 4542-KDORDBEK                           
003027     MOVE OBKR-KDPRTYP         TO 4542-KDPRTYP                            
003028     MOVE ZERO                 TO 4542-KDVORATG                           
003029     IF 4542-KDORDBEK = 92 OR 98                                          
003030       COMPUTE 4542-KVBEART = OBKR-KVPREAVB + OBKR-KVPRERO                
003031       MOVE 4542-KVBEART       TO 4542-KVBEART-Q                          
003032       MOVE OBKR-KVPREAVB      TO 4542-KVPREAVB                           
003033     ELSE                                                                 
003034       MOVE OBKR-KVBEART       TO 4542-KVBEART-Q                          
003035       MOVE +0                 TO 4542-KVPREAVB                           
003036     END-IF                                                               
003037     EJECT                                                                
003038     MOVE OBKR-PRARTNTO        TO 4542-PRARTNTO                           
003039     MOVE OBKR-DEAL-PR-LINE    TO 4542-DEAL-PR-LINE                       
003040     MOVE SPACE                TO 4542-TEVORMRK                           
003041     MOVE MSGI-TILOKDAT        TO 4542-TIREGDAT                           
003042     MOVE MSGI-TILOKTID        TO WS-TIHHMM                               
003043     MOVE WS-TIHHMMSS          TO 4542-TIREGTID                           
003044     MOVE +0                   TO 4542-TIUPPDAT                           
003045     MOVE +0                   TO 4542-TIUPPTID                           
003046     MOVE OBKR-IDLEVNR         TO 4542-IDLEVNR                            
003047                                                                          
003048*---------------------------------------UPPLÄGG TILL NY VORKÖ             
003049*                                       SKER I W40293                     
003050     IF WS-IDDC NOT = W-IDDC-B6                                           
003051        MOVE WS-IDDC TO W-IDDC-B6                                         
003052        PERFORM IMS-GU-WDB601                                             
003053     END-IF                                                               
003054                                                                          
003055     IF  DCS-NDC                                                          
003056         PERFORM IMS-19-ISRT-4541-WDR411                                  
003057         PERFORM UNTIL SEGMENT-FINNS                                      
003058                                                                          
003059            ADD +1             TO 4542-IDLOPNR                            
003060            PERFORM IMS-19-ISRT-4541-WDR411                               
003061         END-PERFORM                                                      
003062                                                                          
003063         IF  DCS-NDC-CN                                                   
003064         OR (DCS-NDC-NA AND DCS-USA)                                      
003065            PERFORM IMS-GU-WDK711                                         
003066            IF SEGMENT-FINNS AND SLAG-IDDC-REF = SPACE                    
003067               PERFORM S11A-STARTA-W2T191X                                
003068            END-IF                                                        
003069         END-IF                                                           
003070     END-IF                                                               
003071                                                                          
003072     IF (4542-KDORDBEK NOT = 92 AND 98) AND 4542-IDLEVNR = SPACE          
003073                                                                          
003074       IF DCS-CDC                                                         
003075         MOVE 4542-IDARTNR     TO W-IDARTNR                               
003076         PERFORM IMS-21-GHU-ARTM-WDK901                                   
003077         COMPUTE ART-KVOKS-VOR =                                          
003078                 ART-KVOKS-VOR +                                          
003079                (4542-KVBEART-Q - 4542-KVPREAVB)                          
003080         PERFORM IMS-22-REPL-ARTM-WDK901                                  
003081       END-IF                                                             
003082     END-IF                                                               
003083     PERFORM S24-CHANGE-PRICE-Q-LINE                                      
003084     .                                                                    
003085     EJECT                                                                
003086                                                                          
003087 S11A-STARTA-W2T191X   SECTION.                                           
003088                                                                          
003089     MOVE +1                    TO 2191-MID-KDCLAGER                      
003090     MOVE 4542-IDARTNR          TO 2191-MID-IDARTNR                       
003091     MOVE ZERO                  TO 2191-MID-TISENBEK-DAG                  
003092                                   2191-MID-TISENBEK-KL                   
003093     MOVE SPACE                 TO 2191-MID-IDKR                          
003094     MOVE WS-IDANSK             TO 2191-MID-IDANSK                        
003095     MOVE '500'                 TO 2191-MID-KDLARM                        
003096     MOVE 4542-IDDISTR          TO WS-IDDISTR-NUM4                        
003097     MOVE WS-IDDISTR-NUM4       TO 2191-MID-IDDISTR                       
003098     MOVE 4542-IDKUNDNR         TO WS-IDKUNDNR-NUM6                       
003099     MOVE WS-IDKUNDNR-NUM6      TO 2191-MID-IDKUNDNR                      
003100     MOVE 4542-IDKUNDRF         TO 2191-MID-IDKUNDRF                      
003101     MOVE 'J'                   TO 2191-MID-FLNYLARM                      
003102     MOVE SLAG-IDDC             TO 2191-MID-IDDC                          
003103     MOVE SLAG-IDLEVNR          TO 2191-MID-IDLEVNR                       
003104                                                                          
003105     COMPUTE MSG-KVLL = LENGTH OF 2191-MID-W2I19101 + 17                  
003106     MOVE 'W2T191X '            TO MSG-KDTRANS-1                          
003107     MOVE '4223'                TO MSG-IDTRANS-1                          
003108     MOVE '1'                   TO MSG-KDMFSFOR-1                         
003109     MOVE 2191-MID-W2I19101     TO MSG-INDATA-MINUS-1-TRANSKOD            
003110                                                                          
003111     PERFORM IMS-PURG-MSG-2191                                            
003112                                                                          
003113     MOVE SPACE                 TO 2191-MID-W2I19101                      
003114     .                                                                    
003115     EJECT                                                                
003116                                                                          
003117 S12-BYGG-UPP-ORDERRAD SECTION.                                           
003118                                                                          
003119     MOVE OBKR-IDORDER         TO ORAD-IDORDER                            
003120     MOVE OBKR-IDDC            TO ORAD-IDDC                               
003121                                  WS-IDDC                                 
003122     IF WS-IDDC NOT = W-IDDC-B6                                           
003123        MOVE WS-IDDC TO W-IDDC-B6                                         
003124        PERFORM IMS-GU-WDB601                                             
003125     END-IF                                                               
003126                                                                          
003127     IF DCS-SDC OR DCS-NDC                                                
003128       MOVE AREG-IDARTNR       TO W-IDARTNR                               
003129       MOVE ORAD-IDDC          TO W-IDDC                                  
003130       PERFORM IMS-11-GHU-WDK711                                          
003131       MOVE SLAG-ADLAGOMR      TO ORAD-ADLAGOMR                           
003132       MOVE SLAG-ADGANG        TO ORAD-ADGANG                             
003133       MOVE SLAG-ADPLATS       TO ORAD-ADPLATS                            
003134       MOVE DCS-IDLANDX2       TO W-IDLAND                                
003135       IF DCS-NDC                                                         
003136         PERFORM IMS-GU-WDK712                                            
003137         IF SEGMENT-FINNS                                                 
003138            IF LART-KDARTURS > SPACE                                      
003139               MOVE LART-KDARTURS TO ORAD-KDARTURS                        
003140            ELSE                                                          
003141               MOVE AREG-KDARTURS TO ORAD-KDARTURS                        
003142            END-IF                                                        
003143            IF LART-VKART > ZERO AND                                      
003144               LART-VKART NOT = AREG-VKART                                
003145               MOVE LART-VKART TO ORAD-VKART                              
003146                                  ORAD-VKART-NTO                          
003147            ELSE                                                          
003148               MOVE AREG-VKART     TO ORAD-VKART                          
003149               MOVE AREG-VKART-NTO TO ORAD-VKART-NTO                      
003150            END-IF                                                        
003151            IF LART-VLARTNTO > 0                                          
003152               MOVE LART-VLARTNTO TO ORAD-VLARTNTO                        
003153            ELSE                                                          
003154               MOVE AREG-VLARTNTO TO ORAD-VLARTNTO                        
003155            END-IF                                                        
003156         ELSE                                                             
003157            MOVE AREG-KDARTURS  TO ORAD-KDARTURS                          
003158            MOVE AREG-VKART     TO ORAD-VKART                             
003159            MOVE AREG-VKART-NTO TO ORAD-VKART-NTO                         
003160            MOVE AREG-VLARTNTO  TO ORAD-VLARTNTO                          
003161         END-IF                                                           
003162       ELSE                                                               
003163          MOVE AREG-KDARTURS   TO ORAD-KDARTURS                           
003164          MOVE AREG-VKART      TO ORAD-VKART                              
003165          MOVE AREG-VKART-NTO  TO ORAD-VKART-NTO                          
003166          MOVE AREG-VLARTNTO   TO ORAD-VLARTNTO                           
003167       END-IF                                                             
003168     ELSE                                                                 
003169       MOVE AREG-ADLAGOMR      TO ORAD-ADLAGOMR                           
003170       MOVE AREG-ADGANG        TO ORAD-ADGANG                             
003171       MOVE AREG-ADPLATS       TO ORAD-ADPLATS                            
003172       MOVE AREG-KDARTURS      TO ORAD-KDARTURS                           
003173       MOVE AREG-VKART         TO ORAD-VKART                              
003174       MOVE AREG-VKART-NTO     TO ORAD-VKART-NTO                          
003175       MOVE AREG-VLARTNTO      TO ORAD-VLARTNTO                           
003176     END-IF                                                               
003177     MOVE AREG-IDARTNR         TO ORAD-IDARTNR                            
003178     MOVE +1                   TO ORAD-IDLOPNR                            
003179     MOVE OBKR-BERADREF        TO ORAD-BERADREF                           
003180     MOVE OBKR-BEVOLREF        TO ORAD-BEVOLREF                           
003181     MOVE OBKR-FLAKPLOC        TO ORAD-FLAKPLOC                           
003182     MOVE OBKR-FLINVEST        TO ORAD-FLINVEST                           
003183     MOVE OBKR-FLOBTRAN        TO ORAD-FLOBTRAN                           
003184     MOVE OBKR-FLPRTILL        TO ORAD-FLPRTILL                           
003185     MOVE OBKR-FLRESTN         TO ORAD-FLRESTN                            
003186     MOVE OBKR-FLTILLK         TO ORAD-FLTILLK                            
003187     MOVE 'N'                  TO ORAD-FLSDCLEV                           
003188     MOVE OBKR-IDDC-RO         TO ORAD-IDDC-RO                            
003189     MOVE OBKR-IDDISTR         TO ORAD-IDDISTR                            
003190     MOVE OBKR-IDKUNDNR        TO ORAD-IDKUNDNR                           
003191     MOVE OBKR-IDKUNDRF        TO ORAD-IDKUNDRF                           
003192     MOVE OBKR-IDKAMPRF        TO ORAD-IDKAMPRF                           
003193     MOVE OBKR-IDLEVNR         TO ORAD-IDLEVNR                            
003194     MOVE OBKR-IDLOPNR-RO      TO ORAD-IDLOPNR-RO                         
003195     MOVE OBKR-IDKUNDRF-RO     TO ORAD-IDKUNDRF-RO                        
003196     MOVE +0                   TO ORAD-IDSPECEMB                          
003197     MOVE OBKR-IDSYSTEM        TO ORAD-IDSYSTEM                           
003198     MOVE OBKR-KDDSP           TO ORAD-KDDSP                              
003199     MOVE AREG-KDFARLIG        TO ORAD-KDFARLIG                           
003200     MOVE OBKR-KDKVBRYT        TO ORAD-KDKVBRYT                           
003201     MOVE OBKR-KDOI            TO ORAD-KDOI                               
003202     MOVE OBKR-CLEARGROUP      TO ORAD-CLEARGROUP                         
003203     MOVE OHUV-KDORDING        TO ORAD-KDORDING                           
003204     MOVE JA                   TO ORAD-FLORDING                           
003205     MOVE OBKR-KDORDKL         TO ORAD-KDORDKL                            
003206     MOVE AREG-KDPRODSL        TO ORAD-KDPRODSL                           
003207     MOVE OBKR-KDPRTYP         TO ORAD-KDPRTYP                            
003208     MOVE AREG-KDSPEEMB        TO ORAD-KDSPEEMB                           
003209     MOVE OBKR-KDTPOTYP        TO ORAD-KDTPOTYP                           
003210     MOVE OBKR-KDVRINFO        TO ORAD-KDVRINFO                           
003211        MOVE OBKR-KVPREAVB     TO ORAD-KVPREAVB                           
003212     MOVE OBKR-KVPRERO         TO ORAD-KVPRERO                            
003213     MOVE +0                   TO ORAD-KVOKS-PREL                         
003214     MOVE OBKR-KVBEART         TO ORAD-KVBEART                            
003215     IF OBKR-KDORDBEK = 92 OR 98                                          
003216       COMPUTE ORAD-KVBEART = ORAD-KVBEART + ORAD-KVPRERO                 
003217       MOVE OBKR-KVPREAVB      TO ORAD-KVBEART-Q                          
003218       MOVE +0                 TO ORAD-KVPRERO                            
003219     ELSE                                                                 
003220       IF OBKR-KVPREAVB > +0  OR  OBKR-KVPRERO > +0                       
003221          COMPUTE ORAD-KVBEART-Q = OBKR-KVPREAVB + OBKR-KVPRERO           
003222       ELSE                                                               
003223          MOVE OBKR-KVBEART-Q  TO ORAD-KVBEART-Q                          
003224       END-IF                                                             
003225     END-IF                                                               
003226     MOVE OBKR-KVSLATT         TO ORAD-KVSLATT                            
003227     MOVE OBKR-PRARTNTO        TO ORAD-PRARTNTO                           
003228     MOVE OBKR-DEAL-PR-LINE    TO ORAD-DEAL-PR-LINE                       
003229***VID T.EX ERSATTA BIPACKNINGSRADER, TPO6'OR ETC.                        
003230*** DE HAR INGEN PRISFRÅGA ÄNNU.                                          
003231     IF DIST79-DEALER-PRICE AND                                           
003232        (OBKR-PRARTNTO-LOC = +0 AND OBKR-PRARTNTO-LOCPREL = +0)           
003233        PERFORM S26-ADD-PRICE-Q-LINE                                      
003234     END-IF                                                               
003235     MOVE OBKR-PRBPRIS         TO ORAD-PRBPRIS                            
003236     MOVE AREG-REKSIFFR        TO ORAD-REKSIFFR                           
003237     MOVE OBKR-RERF-RAD        TO ORAD-RERF-RAD                           
003238     MOVE OBKR-TIPRIS          TO ORAD-TIPRIS                             
003239     MOVE MSGI-TILOKDAT        TO ORAD-TIREGDAT                           
003240     MOVE MSGI-TILOKTID        TO WS-TIHHMM                               
003241     MOVE WS-TIHHMMSS          TO ORAD-TIREGTID                           
003242     IF OBKR-KDORDBEK = 10                                                
003243        MOVE OBKR-TIRODAT      TO ORAD-TIRODAT                            
003244     ELSE                                                                 
003245        MOVE +0                TO ORAD-TIRODAT                            
003246     END-IF                                                               
003247     MOVE OBKR-TITPO           TO ORAD-TITPO                              
003248     MOVE SPACE                TO ORAD-IDBIL                              
003249                                  ORAD-IDKLIENT                           
003250                                  ORAD-IDARBREF                           
003251                                  ORAD-IDVIN                              
003252                                                                          
003253     MOVE OBKR-IDKUNDRF-WIP    TO ORAD-IDKUNDRF-WIP                       
003254     MOVE OBKR-PRAVCOST        TO ORAD-PRAVCOST                           
003255     MOVE OBKR-KDVALISO        TO ORAD-KDVALISO                           
003256     .                                                                    
003257     EJECT                                                                
003258                                                                          
003259 S13-HITTA-FORSTA-I-GRUPPEN SECTION.                                      
003260                                                                          
003261     MOVE WS-AKT-IDARTNR       TO WS-IDARTNR-SPAR                         
003262     MOVE WS-AKT-IDDC          TO WS-IDDC-SPAR                            
003263     MOVE WS-AKT-IDLOPNR       TO WS-IDLOPNR-SPAR                         
003264     MOVE WS-AKT-IDARTNR-URS   TO WS-IDARTNR-URS-SPAR                     
003265                                                                          
003266     SUBTRACT 1 FROM WS-INDEX-MID                                         
003267     IF WS-INDEX-MID NOT = +0                                             
003268        MOVE MID-RAD(WS-INDEX-MID)                                        
003269                               TO WS-AKTUELL-MID-RAD                      
003270        PERFORM UNTIL WS-INDEX-MID = +0 OR                                
003271                      WS-AKT-IDARTNR NOT = WS-IDARTNR-SPAR OR             
003272                      WS-AKT-IDLOPNR NOT = WS-IDLOPNR-SPAR OR             
003273                      WS-AKT-IDARTNR-URS NOT =                            
003274                      WS-IDARTNR-URS-SPAR                                 
003275           SUBTRACT 1        FROM WS-INDEX-MID                            
003276           IF WS-INDEX-MID NOT = +0                                       
003277              MOVE MID-RAD(WS-INDEX-MID)                                  
003278                               TO WS-AKTUELL-MID-RAD                      
003279           END-IF                                                         
003280        END-PERFORM                                                       
003281     END-IF                                                               
003282     .                                                                    
003283     EJECT                                                                
003284                                                                          
003285 S14-LAS-ARTIKELREG SECTION.                                              
003286                                                                          
003287     IF OBKR-IDARTNR-TILLK > +0                                           
003288       MOVE OBKR-IDARTNR-TILLK TO AREG-IDARTNR                            
003289     ELSE                                                                 
003290       MOVE OBKR-IDARTNR       TO AREG-IDARTNR                            
003291     END-IF                                                               
003292                                                                          
003293     CALL W411AREG USING AREG-W411AREG                                    
003294                         AREG-WDK6-PCB                                    
003295                         AREG-WDK7-PCB                                    
003296     .                                                                    
003297     EJECT                                                                
003298                                                                          
003299 S22-HAMTA-BENAMNING SECTION.                                             
003300                                                                          
003301     IF OBKR-IDARTNR-TILLK > +0                                           
003302        MOVE OBKR-IDARTNR-TILLK                                           
003303                               TO W-IDARTNR                               
003304     ELSE                                                                 
003305        MOVE OBKR-IDARTNR      TO W-IDARTNR                               
003306     END-IF                                                               
003307                                                                          
003308     MOVE OHUV-IDSKYLT         TO W-IDSKYLT                               
003309                                                                          
003310     PERFORM IMS-23-GU-BENA-WDD311                                        
003311     IF SEGMENT-FINNS                                                     
003312        MOVE TEXT-BEART        TO MOD-BEART(WS-INDEX-MOD)                 
003313     ELSE                                                                 
003314        MOVE SPACE             TO MOD-BEART(WS-INDEX-MOD)                 
003315     END-IF                                                               
003316     .                                                                    
003317     EJECT                                                                
003318                                                                          
003319                                                                          
003320 S23-DATA-TILL-DEL-NOTE SECTION.                                          
003321                                                                          
003322     MOVE OHUV-IDDISTR             TO TEST-IDDISTR                        
003323     IF DIST07-USA-RETAILER-DNOTE                                         
003324     OR DIST07-CAN-RETAILER                                               
003325                                                                          
003326        INITIALIZE DNOT-ORDER-INFO                                        
003327                                                                          
003328        MOVE IDPGM                    TO DNOT-IDPGM                       
003329        MOVE OHUV-IDORDER             TO DNOT-IDORDER                     
003330                                                                          
003331        CALL W411DNOT USING DNOT-W411DNOT                                 
003332                            DNOT-ORQP-PCB                                 
003333                            DNOT-ORQP2-PCB                                
003334                            DNOT-ORQP3-PCB                                
003335                            DNOT-4013-PCB                                 
003336                            DNOT-BENA-PCB                                 
003337     END-IF                                                               
003338     .                                                                    
003339     EJECT                                                                
003340 S24-CHANGE-PRICE-Q-LINE SECTION.                                         
003341                                                                          
003342     IF DIST79-DEALER-PRICE                                               
003343       IF OBKR-IDPRQUES > ZERO                                            
003344         INITIALIZE PRQU-W335PRQU                                         
003345         MOVE OBKR-IDDISTR       TO PRQU-IDDISTR                          
003346         MOVE OBKR-IDKUNDNR      TO PRQU-IDKUNDNR                         
003347         MOVE OBKR-IDKUNDRF      TO PRQU-IDKUNDRF                         
003348         MOVE OBKR-IDPRQUES      TO PRQU-IDPRQUES                         
003349         MOVE OBKR-KVBEART-Q     TO PRQU-KVBEART-Q                        
003350         MOVE 5                  TO PRQU-KDCALL                           
003351         CALL W335PRQU USING PRQU-W335PRQU  PRQU-WDG2-PCB                 
003352                                            PRQU-WDC7-PCB                 
003353                                            PRQU-SJKO-WDK6-PCB            
003354       END-IF                                                             
003355     END-IF                                                               
003356     .                                                                    
003357     EJECT                                                                
003358 S25-DELETE-PRICE-Q-LINE SECTION.                                         
003359                                                                          
003360     IF DIST79-DEALER-PRICE                                               
003361       IF ORAD-IDPRQUES > ZERO                                            
003362         INITIALIZE PRQU-W335PRQU                                         
003363         MOVE OHUV-IDDISTR       TO PRQU-IDDISTR                          
003364         MOVE OHUV-IDKUNDNR      TO PRQU-IDKUNDNR                         
003365         MOVE OHUV-IDKUNDRF      TO PRQU-IDKUNDRF                         
003366         MOVE ORAD-IDPRQUES      TO PRQU-IDPRQUES                         
003367         MOVE 4                  TO PRQU-KDCALL                           
003368         CALL W335PRQU USING PRQU-W335PRQU  PRQU-WDG2-PCB                 
003369                                            PRQU-WDC7-PCB                 
003370                                            PRQU-SJKO-WDK6-PCB            
003371       END-IF                                                             
003372     END-IF                                                               
003373     .                                                                    
003374     EJECT                                                                
003375                                                                          
003376 S26-ADD-PRICE-Q-LINE SECTION.                                            
003377                                                                          
003378     IF DIST79-DEALER-PRICE                                               
003379       IF OBKR-PRARTNTO-LOC = +0    AND                                   
003380          OBKR-PRARTNTO-LOCPREL = +0                                      
003381         IF OBKR-IDPRQUES       = +0                                      
003382********* HÄMTAR NÄSTA LEDIGA PRISFRÅGENR                                 
003383           MOVE +0                    TO PRNO-IDPRQUES-IN                 
003384           MOVE +1                    TO PRNO-KDCALL                      
003385                                                                          
003386           CALL W335PRNO USING PRNO-W335PRNO PRNO-3107-PCB                
003387                                                                          
003388**********UPPDATERAR WDC7 MED EN PRISFRÅGA                                
003389           MOVE PRNO-IDPRQUES-UT      TO PRQU-IDPRQUES                    
003390           MOVE +1                    TO PRQU-KDCALL                      
003391                                                                          
003392           MOVE W-IDDISTR             TO PRQU-IDDISTR                     
003393           MOVE W-IDKUNDNR            TO PRQU-IDKUNDNR                    
003394           MOVE W-IDKUNDRF            TO PRQU-IDKUNDRF                    
003395           MOVE OBKR-IDORDER          TO PRQU-IDORDER                     
003396           MOVE OBKR-KDORDKL          TO PRQU-KDORDKL                     
003397           MOVE 'N'                   TO PRQU-KDPRSTA                     
003398           MOVE OBKR-IDARTNR          TO PRQU-IDARTNR                     
003399           MOVE OBKR-KVBEART-Q        TO PRQU-KVBEART-Q                   
003400           MOVE OBKR-PRARTNTO-LOC     TO PRQU-PRARTNTO-LOC                
003401           MOVE +0                    TO PRQU-PRARTNTO-LOCPREL            
003402           MOVE OBKR-IDSYSTEM         TO PRQU-IDSYSTEM                    
003403                                                                          
003404           MOVE OBKR-KDVALISO          TO PRQU-KDVALISO                   
003405                                                                          
003406                                                                          
003407           CALL W335PRQU USING PRQU-W335PRQU PRQU-WDG2-PCB                
003408                                           PRQU-WDC7-PCB                  
003409                                           PRQU-SJKO-WDK6-PCB             
003410                                                                          
003411           MOVE PRQU-IDPRQUES           TO  ORAD-IDPRQUES                 
003412           MOVE PRQU-FLPRTILL           TO  ORAD-FLPRTILL                 
003413                                                                          
003414           IF OBKR-PRARTNTO-LOC = +0                                      
003415             MOVE PRQU-PRARTNTO-LOCPREL TO  ORAD-PRARTNTO-LOCPREL         
003416           ELSE                                                           
003417             MOVE OBKR-PRARTNTO-LOC     TO  ORAD-PRARTNTO-LOC             
003418           END-IF                                                         
003419                                                                          
003420*** UPPDATERA NÄSTA LEDIGA PRISFRÅGENR                                    
003421                                                                          
003422           MOVE PRQU-IDPRQUES           TO PRNO-IDPRQUES-IN               
003423           MOVE +3                      TO PRNO-KDCALL                    
003424                                                                          
003425           CALL W335PRNO USING PRNO-W335PRNO PRNO-3107-PCB                
003426***SKICKA PRISFRÅGA                                                       
003427           PERFORM S26C-SKICKA-PRISFRAGA                                  
003428         END-IF                                                           
003429       END-IF                                                             
003430     END-IF                                                               
003431     .                                                                    
003432                                                                          
003433 S26C-SKICKA-PRISFRAGA SECTION.                                           
003434                                                                          
003435     MOVE 1                      TO 3039-REQU-IDMSGVER                    
003436     MOVE SPACE                  TO 3039-REQU-KDPGMACT                    
003437     MOVE 'W4022300'             TO 3039-REQU-IDUSER                      
003438                                                                          
003439     MOVE SPACE                  TO 3039-MID-IDBUNDLE                     
003440     MOVE OBKR-IDDISTR           TO 3039-MID-IDDISTR                      
003441     MOVE OBKR-IDKUNDNR          TO 3039-MID-IDKUNDNR                     
003442     MOVE OBKR-IDORDNR7          TO 3039-MID-IDBUNDLE                     
003443     MOVE PRQU-IDPRQUES          TO 3039-MID-IDPRQUES                     
003444                                                                          
003445     PERFORM S27-SKICKA-OPEN                                              
003446     PERFORM S27-SKICKA-MEDDELANDE                                        
003447     PERFORM S27-SKICKA-CLOSE                                             
003448     .                                                                    
003449                                                                          
003450 S27-SKICKA-OPEN SECTION.                                                 
003451                                                                          
003452     MOVE 'OPEN'                     TO SEND-KDFUNC                       
003453     MOVE 'CARPARTS.PULS.PRQRY' TO SEND-ADDISPABS                         
003454     CALL WZ01SEND USING SEND-CONTROL-AREA SEND-OPEN-AREA                 
003455                                                                          
003456     IF SEND-KDRC > 0                                                     
003457       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
003458       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
003459       DELIMITED BY SIZE INTO FELTEXT                                     
003460       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
003461     END-IF                                                               
003462     .                                                                    
003463     SKIP3                                                                
003464 S27-SKICKA-MEDDELANDE SECTION.                                           
003465                                                                          
003466     MOVE 'PUT'                      TO SEND-KDFUNC                       
003467     MOVE LENGTH OF SEND-AREA        TO SEND-KVDLEN                       
003468     CALL WZ01SEND USING SEND-CONTROL-AREA SEND-KVDLEN SEND-AREA          
003469                                                                          
003470     IF SEND-KDRC > 0                                                     
003471       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
003472       STRING 'WZ01SEND GET ERROR RC=' KDRC-DISPLAY                       
003473       DELIMITED BY SIZE INTO FELTEXT                                     
003474       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
003475     END-IF                                                               
003476     .                                                                    
003477     SKIP3                                                                
003478 S27-SKICKA-CLOSE SECTION.                                                
003479                                                                          
003480     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
003481     CALL WZ01SEND USING SEND-CONTROL-AREA                                
003482                                                                          
003483     IF SEND-KDRC > 0                                                     
003484       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
003485       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
003486       DELIMITED BY SIZE INTO FELTEXT                                     
003487       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
003488     END-IF                                                               
003489     .                                                                    
003490     EJECT                                                                
003491 MFS-ROER-EJ-BILD SECTION.                                                
003492                                                                          
003493     MOVE MFS-ROER-EJ-FAELT    TO MOD-FLANNULL                            
003494                                  MOD-KDORDKL-UT                          
003495                                                                          
003496     MOVE +1                   TO WS-INDEX                                
003497     PERFORM UNTIL WS-INDEX > WS-INDEX-MOD-MAX                            
003498                                                                          
003499        PERFORM MFS-SAETT-ATTRIBUT                                        
003500                                                                          
003501        MOVE MFS-ROER-EJ-FAELT TO MOD-KDORDBEK(WS-INDEX)                  
003502                                  MOD-ASTERIX(WS-INDEX)                   
003503                                  MOD-KDBEHX(WS-INDEX)                    
003504                                  MOD-IDARTNR(WS-INDEX)                   
003505                                  MOD-BEART(WS-INDEX)                     
003506                                  MOD-IDDC-RAD(WS-INDEX)                  
003507                                  MOD-KVANTAL(WS-INDEX)                   
003508                                  MOD-KVQPACK(WS-INDEX)                   
003509                                  MOD-IDKUNDRF-RO(WS-INDEX)               
003510                                  MOD-KEYS(WS-INDEX)                      
003511        ADD +1                 TO WS-INDEX                                
003512     END-PERFORM                                                          
003513     .                                                                    
003514     EJECT                                                                
003515 MFS-SAETT-ATTRIBUT SECTION.                                              
003516                                                                          
003517     MOVE MID-RAD(WS-INDEX) TO WS-AKTUELL-MID-RAD                         
003518                                                                          
003519     IF WS-AKT-KDBEHX = 'B'                                               
003520       IF (WS-AKT-KDORDBEK = 41 OR 61) AND                                
003521                    (WS-AKT-IDARTNR NOT = WS-AKT-IDARTNR-URS)             
003522         MOVE MFS-STAENG-FAELT-OSYNLIGT                                   
003523                           TO MOD-KDORDBEK-ATTR(WS-INDEX)                 
003524       ELSE                                                               
003525         IF WS-AKT-KDORDBEK      = 21 OR 51 OR                            
003526                                   41 OR 52 OR 53 OR 54 OR                
003527                                   55 OR 57 OR 58 OR 59 OR 66 OR          
003528                                   67 OR 72 OR 73 OR 74 OR 75 OR          
003529                                   76 OR 80 OR 81 OR 82 OR 85 OR          
003530                                   61                                     
003531           MOVE MFS-STAENG-FAELT TO MOD-KDBEHX-ATTR(WS-INDEX)             
003532         END-IF                                                           
003533                                                                          
003534         IF WS-AKT-KDORDBEK = 41 OR 61                                    
003535           MOVE MFS-STAENG-FAELT-OSYNLIGT                                 
003536                             TO MOD-IDDC-ATTR(WS-INDEX)                   
003537                                MOD-KVANTAL-ATTR(WS-INDEX)                
003538                                MOD-KVQPACK-ATTR(WS-INDEX)                
003539                                MOD-IDKUNDRF-RO-ATTR(WS-INDEX)            
003540         END-IF                                                           
003541         IF WS-AKT-KDORDBEK = 57 AND  WS-AKT-IDARTNR = ZERO               
003542            MOVE MFS-STAENG-FAELT-OSYNLIGT                                
003543                             TO MOD-KDORDBEK-ATTR(WS-INDEX)               
003544                                MOD-IDDC-ATTR(WS-INDEX)                   
003545                                MOD-KVANTAL-ATTR(WS-INDEX)                
003546                                MOD-KVQPACK-ATTR(WS-INDEX)                
003547                                MOD-IDKUNDRF-RO-ATTR(WS-INDEX)            
003548         END-IF                                                           
003549       END-IF                                                             
003550     EJECT                                                                
003551     ELSE                                                                 
003552       IF WS-AKT-KDORDBEK = 41 OR 61                                      
003553         MOVE MFS-STAENG-FAELT-OSYNLIGT                                   
003554                           TO MOD-KDORDBEK-ATTR(WS-INDEX)                 
003555         IF (WS-AKT-KDORDBEK = 61) AND                                    
003556                  WS-AKT-IDARTNR = ZERO                                   
003557           MOVE MFS-STAENG-FAELT-OSYNLIGT                                 
003558                           TO MOD-KDBEHX-ATTR(WS-INDEX)                   
003559                              MOD-IDDC-ATTR(WS-INDEX)                     
003560                              MOD-KVANTAL-ATTR(WS-INDEX)                  
003561                              MOD-KVQPACK-ATTR(WS-INDEX)                  
003562                              MOD-IDKUNDRF-RO-ATTR(WS-INDEX)              
003563         END-IF                                                           
003564       END-IF                                                             
003565     END-IF                                                               
003566     .                                                                    
003567     EJECT                                                                
003568                                                                          
003569 MFS-RENSA-MOD-RADER SECTION.                                             
003570                                                                          
003571     MOVE +1                 TO WS-INDEX                                  
003572     PERFORM UNTIL WS-INDEX > WS-INDEX-MOD-MAX                            
003573        MOVE MFS-RENSA-FAELT TO MOD-KDORDBEK(WS-INDEX)                    
003574                                MOD-ASTERIX(WS-INDEX)                     
003575                                MOD-KDBEHX(WS-INDEX)                      
003576                                MOD-IDARTNR(WS-INDEX)                     
003577                                MOD-BEART(WS-INDEX)                       
003578                                MOD-IDDC-RAD(WS-INDEX)                    
003579                                MOD-KVANTAL(WS-INDEX)                     
003580                                MOD-KVQPACK(WS-INDEX)                     
003581                                MOD-IDKUNDRF-RO(WS-INDEX)                 
003582                                MOD-KEYS(WS-INDEX)                        
003583        ADD 1                TO WS-INDEX                                  
003584     END-PERFORM                                                          
003585     .                                                                    
003586                                                                          
003587 MFS-RENSA-ALLA-FAELT SECTION.                                            
003588                                                                          
003589     MOVE MFS-RENSA-FAELT    TO MOD-FLANNULL                              
003590                                                                          
003591     MOVE MFS-RENSA-FAELT    TO MOD-IDARTNR-NEXT                          
003592                                MOD-IDLOPNR-NEXT                          
003593                                MOD-IDSEKVNR-NEXT                         
003594                                MOD-IDDC-NEXT                             
003595                                MOD-KDORDBEK-NEXT                         
003596                                                                          
003597     PERFORM MFS-RENSA-MOD-RADER                                          
003598     .                                                                    
003599     EJECT                                                                
003600                                                                          
003601 MFS-RENSA-4221-MOD SECTION.                                              
003602                                                                          
003603     MOVE MFS-RENSA-FAELT    TO 4221-FLVORKO                              
003604                                4221-FLFORBI                              
003605                                4221-IDDISTR-1-IN                         
003606                                4221-IDKUNDNR                             
003607                                4221-IDORDNR5                             
003608                                4221-KDORDKL                              
003609                                4221-KDFRAKT                              
003610                                4221-IDDC-TVS                             
003611                                4221-KDFAKTYP                             
003612                                4221-TIRFS-DAT                            
003613                                4221-TIRFS-TID                            
003614                                4221-BEKUNDRF                             
003615                                4221-BELAGINS-GRP                         
003616                                4221-BEGMT-RAD1                           
003617                                4221-BEGMT-RAD2                           
003618                                4221-ADGMT-GATA                           
003619                                4221-ADGMT-PADR                           
003620                                4221-ADGMT-LAND                           
003621                                4221-BEGMRK-RAD1                          
003622                                4221-BEGMRK-RAD2                          
003623                                4221-IDFTG                                
003624                                4221-IDKONTO                              
003625                                4221-IDANALYS                             
003626                                4221-IDKST                                
003627                                4221-BEVARREF                             
003628     MOVE NEJ               TO  4221-NORMALORDER-JA-NEJ                   
003629                                                                          
003630                                                                          
003631     MOVE MFS-FORMATETS-ATTR TO 4221-IDDISTR-ATTR                         
003632                                4221-IDKUNDNR-ATTR                        
003633                                4221-IDORDNR7-ATTR                        
003634                                4221-KDORDKL-ATTR                         
003635                                4221-KDFRAKT-ATTR                         
003636                                4221-IDDC-ATTR                            
003637                                4221-KDFAKTYP-ATTR                        
003638                                4221-NORMALORDER-ATTR                     
003639                                4221-IDFTG-ATTR                           
003640                                4221-IDKONTO-ATTR                         
003641                                4221-IDANALYS-ATTR                        
003642                                4221-IDKST-ATTR                           
003643     .                                                                    
003644     EJECT                                                                
003645 IMS-GET-MSG SECTION.                                                     
003646                                                                          
003647     MOVE '  QC' TO GODK-STATUSKODER                                      
003648     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
003649     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
003650     PERFORM IMS-STATUSKONTROLL                                           
003651     .                                                                    
003652     SKIP3                                                                
003653 IMS-INSERT-MSG SECTION.                                                  
003654                                                                          
003655     IF ENGLISH-TEXT                                                      
003656       MOVE 'N' TO MFS-KDHUVOMR                                           
003657     END-IF                                                               
003658     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
003659     MOVE SPACE TO GODK-STATUSKODER                                       
003660     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
003661     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
003662     PERFORM IMS-STATUSKONTROLL                                           
003663     .                                                                    
003664                                                                          
003665 IMS-INSERT-4222-MSG SECTION.                                             
003666                                                                          
003667     IF ENGLISH-TEXT                                                      
003668       MOVE 'N' TO MFS-KDHUVOMR                                           
003669     END-IF                                                               
003670     MOVE LOW-VALUE TO 4222-Z1 4222-Z2                                    
003671     MOVE SPACE TO GODK-STATUSKODER                                       
003672     CALL CBLTDLI USING ISRT 4222-PCB 4222-MSG-IO-AREA                    
003673     MOVE 4222-STATUS-CODE TO STATUS-WS                                   
003674     PERFORM IMS-STATUSKONTROLL                                           
003675     .                                                                    
003676     EJECT                                                                
003677 IMS-INSERT-4292-MSG SECTION.                                             
003678                                                                          
003679     IF ENGLISH-TEXT                                                      
003680       MOVE 'N' TO MFS-KDHUVOMR                                           
003681     END-IF                                                               
003682     MOVE LOW-VALUE TO 4292-Z1 4292-Z2                                    
003683     MOVE SPACE TO GODK-STATUSKODER                                       
003684     CALL CBLTDLI USING ISRT 4292-PCB 4292-MSG-IO-AREA                    
003685     MOVE 4292-STATUS-CODE TO STATUS-WS                                   
003686     PERFORM IMS-STATUSKONTROLL                                           
003687     .                                                                    
003688     EJECT                                                                
003689                                                                          
003690 IMS-INSERT-4298-MSG SECTION.                                             
003691                                                                          
003692     MOVE LOW-VALUE TO 4298-Z1 4298-Z2                                    
003693     MOVE SPACE TO GODK-STATUSKODER                                       
003694     CALL CBLTDLI USING ISRT 4298-PCB 4298-MSG-IO-AREA                    
003695     MOVE 4298-STATUS-CODE TO STATUS-WS                                   
003696     PERFORM IMS-STATUSKONTROLL                                           
003697     .                                                                    
003698     SKIP2                                                                
003699 IMS-INSERT-4223-MSG SECTION.                                             
003700                                                                          
003701     IF ENGLISH-TEXT                                                      
003702       MOVE 'N' TO MFS-KDHUVOMR                                           
003703     END-IF                                                               
003704     MOVE SPACE TO GODK-STATUSKODER                                       
003705     CALL CBLTDLI USING ISRT 4223-PCB 4223-MSG-IO-AREA                    
003706     MOVE 4223-STATUS-CODE TO STATUS-WS                                   
003707     PERFORM IMS-STATUSKONTROLL                                           
003708     .                                                                    
003709     SKIP2                                                                
003710 IMS-INSERT-4224-MSG SECTION.                                             
003711                                                                          
003712     MOVE SPACE TO GODK-STATUSKODER                                       
003713     CALL CBLTDLI USING ISRT 4224-PCB 4224-MSG-IO-AREA                    
003714     MOVE 4224-STATUS-CODE TO STATUS-WS                                   
003715     PERFORM IMS-STATUSKONTROLL                                           
003716     .                                                                    
003717     SKIP2                                                                
003718 IMS-INSERT-4225-MSG SECTION.                                             
003719                                                                          
003720     MOVE SPACE TO GODK-STATUSKODER                                       
003721     CALL CBLTDLI USING ISRT 4225-PCB 4225-MSG-IO-AREA                    
003722     MOVE 4225-STATUS-CODE TO STATUS-WS                                   
003723     PERFORM IMS-STATUSKONTROLL                                           
003724     .                                                                    
003725     EJECT                                                                
003726                                                                          
003727 IMS-PURG-MSG-2191  SECTION.                                              
003728     MOVE SPACE TO GODK-STATUSKODER                                       
003729     CALL  CBLTDLI  USING PURG 2191-PCB MSG-IO-AREA                       
003730     MOVE 2191-STATUS-CODE TO STATUS-WS                                   
003731     PERFORM IMS-STATUSKONTROLL                                           
003732     .                                                                    
003733     EJECT                                                                
003734                                                                          
003735 IMS-01-GHU-ORQM-WDQ101-FOERE SECTION.                                    
003736                                                                          
003737     STRING 'WLORQM01(WDQ101KY=>' W-WDQ101-KEY-MIN                        
003738                    '&WDQ101KY <' W-WDQ101-KEY-MAX                        
003739                    '&FLOBOK   =' NEJ ')'                                 
003740          DELIMITED BY SIZE INTO SSA1                                     
003741     MOVE '  GE'               TO GODK-STATUSKODER                        
003742     CALL CBLTDLI USING GHU ORQM-PCB DLI-IO-AREA-ORQM SSA1                
003743     MOVE ORQM-STATUS-CODE     TO STATUS-WS                               
003744     PERFORM IMS-STATUSKONTROLL                                           
003745     .                                                                    
003746     SKIP2                                                                
003747 IMS-02-GHN-ORQM-WDQ101-FOERE SECTION.                                    
003748                                                                          
003749     STRING 'WLORQM01(WDQ101KY=>' W-WDQ101-KEY-MIN                        
003750                    '&WDQ101KY <' W-WDQ101-KEY-MAX                        
003751                    '&FLOBOK   =' NEJ ')'                                 
003752          DELIMITED BY SIZE INTO SSA1                                     
003753     MOVE '  GEGB'             TO GODK-STATUSKODER                        
003754     CALL CBLTDLI USING GHN ORQM-PCB DLI-IO-AREA-ORQM SSA1                
003755     MOVE ORQM-STATUS-CODE     TO STATUS-WS                               
003756     PERFORM IMS-STATUSKONTROLL                                           
003757     .                                                                    
003758     SKIP2                                                                
003759 IMS-03-GHU-ORQM-WDQ101-MIN-MAX SECTION.                                  
003760                                                                          
003761     STRING 'WLORQM01(WDQ101KY>=' W-WDQ101-KEY-MIN                        
003762                    '&WDQ101KY<=' W-WDQ101-KEY-MAX                        
003763                    '&FLOBOK   =' NEJ ')'                                 
003764          DELIMITED BY SIZE INTO SSA1                                     
003765     MOVE '  GE'               TO GODK-STATUSKODER                        
003766     CALL CBLTDLI USING GHU ORQM-PCB DLI-IO-AREA-ORQM SSA1                
003767     MOVE ORQM-STATUS-CODE     TO STATUS-WS                               
003768     PERFORM IMS-STATUSKONTROLL                                           
003769     .                                                                    
003770     EJECT                                                                
003771 IMS-04-GHN-ORQM-WDQ101-MIN-MAX SECTION.                                  
003772                                                                          
003773     STRING 'WLORQM01(WDQ101KY>=' W-WDQ101-KEY-MIN                        
003774                    '&WDQ101KY<=' W-WDQ101-KEY-MAX                        
003775                    '&FLOBOK   =' NEJ ')'                                 
003776          DELIMITED BY SIZE INTO SSA1                                     
003777     MOVE '  GEGB'             TO GODK-STATUSKODER                        
003778     CALL CBLTDLI USING GHN ORQM-PCB DLI-IO-AREA-ORQM SSA1                
003779     MOVE ORQM-STATUS-CODE     TO STATUS-WS                               
003780     PERFORM IMS-STATUSKONTROLL                                           
003781     .                                                                    
003782     SKIP2                                                                
003783 IMS-05-GHU-ORQM-WDQ101-UNIK SECTION.                                     
003784                                                                          
003785     STRING 'WLORQM01(WDQ101KY =' W-WDQ101-KEY-UNIK                       
003786                    '&FLOBOK   =' NEJ ')'                                 
003787          DELIMITED BY SIZE  INTO SSA1                                    
003788     MOVE '  GE'               TO GODK-STATUSKODER                        
003789     CALL CBLTDLI USING GHU ORQM-PCB DLI-IO-AREA-ORQM SSA1                
003790     MOVE ORQM-STATUS-CODE     TO STATUS-WS                               
003791     PERFORM IMS-STATUSKONTROLL                                           
003792     .                                                                    
003793     SKIP2                                                                
003794                                                                          
003795 IMS-06-REPL-ORQM-WDQ101 SECTION.                                         
003796                                                                          
003797     MOVE '    '               TO GODK-STATUSKODER                        
003798     CALL CBLTDLI USING REPL ORQM-PCB DLI-IO-AREA-ORQM                    
003799     MOVE ORQM-STATUS-CODE     TO STATUS-WS                               
003800     PERFORM IMS-STATUSKONTROLL                                           
003801     .                                                                    
003802     EJECT                                                                
003803 IMS-07-ISRT-ORQM-WDQ101 SECTION.                                         
003804                                                                          
003805     MOVE 'WLORQM01 '          TO SSA1                                    
003806     MOVE '  '                 TO GODK-STATUSKODER                        
003807     CALL CBLTDLI USING ISRT ORQM-PCB DLI-IO-AREA-ORQM SSA1               
003808     MOVE ORQM-STATUS-CODE     TO STATUS-WS                               
003809     PERFORM IMS-STATUSKONTROLL                                           
003810     .                                                                    
003811     SKIP3                                                                
003812 IMS-08-DLET-ORQM-WDQ101 SECTION.                                         
003813                                                                          
003814     MOVE '    '               TO GODK-STATUSKODER                        
003815     CALL CBLTDLI USING DLET ORQM-PCB DLI-IO-AREA-ORQM                    
003816     MOVE ORQM-STATUS-CODE     TO STATUS-WS                               
003817     PERFORM IMS-STATUSKONTROLL                                           
003818     .                                                                    
003819     EJECT                                                                
003820                                                                          
003821 IMS-09-GU-ORQI-WDQ201 SECTION.                                           
003822                                                                          
003823     STRING 'WLORQI01(WDQ2CSEQ =' W-IDGMTREF-X ')'                        
003824          DELIMITED BY SIZE INTO SSA1                                     
003825     MOVE '  GE'               TO GODK-STATUSKODER                        
003826     CALL CBLTDLI USING GU ORQI-PCB DLI-IO-AREA-ORQI01 SSA1               
003827     MOVE ORQI-STATUS-CODE     TO STATUS-WS                               
003828     PERFORM IMS-STATUSKONTROLL                                           
003829     .                                                                    
003830     SKIP3                                                                
003831 IMS-10-GU-WDK611 SECTION.                                                
003832                                                                          
003833     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
003834          DELIMITED BY SIZE INTO SSA1                                     
003835     MOVE 'WDK611  '           TO SSA2                                    
003836     MOVE '  '                 TO GODK-STATUSKODER                        
003837     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-AREA-WDK611 SSA1 SSA2          
003838     MOVE WDK6-STATUS-CODE     TO STATUS-WS                               
003839     PERFORM IMS-STATUSKONTROLL                                           
003840     .                                                                    
003841     EJECT                                                                
003842 IMS-11-GHU-WDK711 SECTION.                                               
003843                                                                          
003844     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
003845          DELIMITED BY SIZE  INTO SSA1                                    
003846     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
003847          DELIMITED BY SIZE  INTO SSA2                                    
003848     MOVE '    '               TO GODK-STATUSKODER                        
003849     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-AREA-WDK7 SSA1 SSA2           
003850     MOVE WDK7-STATUS-CODE     TO STATUS-WS                               
003851     PERFORM IMS-STATUSKONTROLL                                           
003852     .                                                                    
003853     SKIP3                                                                
003854 IMS-GU-WDK711 SECTION.                                                   
003855                                                                          
003856     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
003857          DELIMITED BY SIZE  INTO SSA1                                    
003858     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
003859          DELIMITED BY SIZE  INTO SSA2                                    
003860     MOVE '  GE'               TO GODK-STATUSKODER                        
003861     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-AREA-WDK7 SSA1 SSA2            
003862     MOVE WDK7-STATUS-CODE     TO STATUS-WS                               
003863     PERFORM IMS-STATUSKONTROLL                                           
003864     .                                                                    
003865     SKIP3                                                                
003866 IMS-GU-WDK712           SECTION.                                         
003867     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
003868            DELIMITED BY SIZE INTO SSA1                                   
003869     STRING 'WDK712  (IDLAND   =' W-IDLAND-X ')'                          
003870            DELIMITED BY SIZE INTO SSA2                                   
003871     MOVE '  GE' TO GODK-STATUSKODER                                      
003872     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-AREA-WDK712 SSA1 SSA2          
003873     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
003874     PERFORM IMS-STATUSKONTROLL                                           
003875     .                                                                    
003876     SKIP3                                                                
003877 IMS-GU-WDK722 SECTION.                                                   
003878                                                                          
003879     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
003880          DELIMITED BY SIZE  INTO SSA1                                    
003881     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
003882          DELIMITED BY SIZE  INTO SSA2                                    
003883     MOVE 'WDK722 '            TO SSA3                                    
003884     MOVE '  GE'               TO GODK-STATUSKODER                        
003885     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK722 SSA1 SSA2 SSA3          
003886     MOVE WDK7-STATUS-CODE     TO STATUS-WS                               
003887     PERFORM IMS-STATUSKONTROLL                                           
003888     .                                                                    
003889     SKIP3                                                                
003890 IMS-12-REPL-WDK711 SECTION.                                              
003891                                                                          
003892     MOVE '    '               TO GODK-STATUSKODER                        
003893     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-AREA-WDK7                    
003894     MOVE WDK7-STATUS-CODE     TO STATUS-WS                               
003895     PERFORM IMS-STATUSKONTROLL                                           
003896     .                                                                    
003897     EJECT                                                                
003898                                                                          
003899 IMS-13-GHNP-ORQI-WDQ212 SECTION.                                         
003900                                                                          
003901     STRING 'WLORQI12(IDDC     =' W-IDDC-X ')'                            
003902          DELIMITED BY SIZE  INTO SSA1                                    
003903     MOVE '    '               TO GODK-STATUSKODER                        
003904     CALL CBLTDLI USING GHNP ORQI-PCB DLI-IO-AREA-ORQI12 SSA1             
003905     MOVE ORQI-STATUS-CODE     TO STATUS-WS                               
003906     PERFORM IMS-STATUSKONTROLL                                           
003907     .                                                                    
003908     SKIP3                                                                
003909                                                                          
003910 IMS-15-REPL-ORQI-WDQ212 SECTION.                                         
003911                                                                          
003912     MOVE '    '               TO GODK-STATUSKODER                        
003913     CALL CBLTDLI USING REPL ORQI-PCB DLI-IO-AREA-ORQI12                  
003914     MOVE ORQI-STATUS-CODE     TO STATUS-WS                               
003915     PERFORM IMS-STATUSKONTROLL                                           
003916     .                                                                    
003917     SKIP3                                                                
003918 IMS-17-ISRT-ORQF-WDQ401 SECTION.                                         
003919                                                                          
003920     MOVE 'WLORQF01 '          TO SSA1                                    
003921     MOVE '  II'               TO GODK-STATUSKODER                        
003922     CALL CBLTDLI USING ISRT ORQF-PCB DLI-IO-AREA-ORQF SSA1               
003923     MOVE ORQF-STATUS-CODE     TO STATUS-WS                               
003924     PERFORM IMS-STATUSKONTROLL                                           
003925     .                                                                    
003926     EJECT                                                                
003927                                                                          
003928 IMS-18A-GU-SATB-WDJ111-01 SECTION.                                       
003929                                                                          
003930     STRING 'WLSATB11*D(WDJ1CSEQ =' W-WDJ1CSEQ-X ')'                      
003931          DELIMITED BY SIZE INTO SSA1                                     
003932     STRING 'WLSATB01(IDLEVNR  =' W-IDLEVNR-X ')'                         
003933          DELIMITED BY SIZE INTO SSA2                                     
003934     MOVE '  GE'               TO GODK-STATUSKODER                        
003935     CALL CBLTDLI USING GU SATB-PCB DLI-IO-AREA-SATB SSA1 SSA2            
003936     MOVE SATB-STATUS-CODE     TO STATUS-WS                               
003937     PERFORM IMS-STATUSKONTROLL                                           
003938     .                                                                    
003939     SKIP2                                                                
003940 IMS-18-GN-SATB-WDJ111-01 SECTION.                                        
003941                                                                          
003942     STRING 'WLSATB11*D(WDJ1CSEQ =' W-WDJ1CSEQ-X ')'                      
003943          DELIMITED BY SIZE INTO SSA1                                     
003944     STRING 'WLSATB01(IDLEVNR  =' W-IDLEVNR-X ')'                         
003945          DELIMITED BY SIZE INTO SSA2                                     
003946     MOVE '  GE'               TO GODK-STATUSKODER                        
003947     CALL CBLTDLI USING GN SATB-PCB DLI-IO-AREA-SATB SSA1 SSA2            
003948     MOVE SATB-STATUS-CODE     TO STATUS-WS                               
003949     PERFORM IMS-STATUSKONTROLL                                           
003950     .                                                                    
003951     SKIP2                                                                
003952 IMS-19-ISRT-4541-WDR411 SECTION.                                         
003953                                                                          
003954     STRING 'WDR401  (WDGXKEY  =' W-WDGXKEY-4541-X ')'                    
003955          DELIMITED BY SIZE INTO SSA1                                     
003956     MOVE 'WDGX4542 '          TO SSA2                                    
003957     MOVE '  II'               TO GODK-STATUSKODER                        
003958     CALL CBLTDLI USING ISRT 4541-PCB DLI-IO-AREA-4541 SSA1 SSA2          
003959     MOVE 4541-STATUS-CODE     TO STATUS-WS                               
003960     PERFORM IMS-STATUSKONTROLL                                           
003961     .                                                                    
003962     EJECT                                                                
003963                                                                          
003964 IMS-21-GHU-ARTM-WDK901 SECTION.                                          
003965                                                                          
003966     STRING 'WLARTM01(IDARTNR  =' W-IDARTNR-X ')'                         
003967          DELIMITED BY SIZE  INTO SSA1                                    
003968     MOVE '    '               TO GODK-STATUSKODER                        
003969     CALL CBLTDLI USING GHU ARTM-PCB DLI-IO-AREA-ARTM SSA1                
003970     MOVE ARTM-STATUS-CODE     TO STATUS-WS                               
003971     PERFORM IMS-STATUSKONTROLL                                           
003972     .                                                                    
003973                                                                          
003974 IMS-22-REPL-ARTM-WDK901 SECTION.                                         
003975                                                                          
003976     MOVE '    '               TO GODK-STATUSKODER                        
003977     CALL CBLTDLI USING REPL ARTM-PCB DLI-IO-AREA-ARTM                    
003978     MOVE ARTM-STATUS-CODE     TO STATUS-WS                               
003979     PERFORM IMS-STATUSKONTROLL                                           
003980     .                                                                    
003981     EJECT                                                                
003982 IMS-23-GU-BENA-WDD311 SECTION.                                           
003983                                                                          
003984     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
003985          DELIMITED BY SIZE INTO SSA1                                     
003986     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
003987          DELIMITED BY SIZE INTO SSA2                                     
003988     MOVE '  GE'               TO GODK-STATUSKODER                        
003989     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA-BENA SSA1 SSA2            
003990     MOVE BENA-STATUS-CODE     TO STATUS-WS                               
003991     PERFORM IMS-STATUSKONTROLL                                           
003992     .                                                                    
003993     SKIP2                                                                
003994 IMS-24-GU-ORDP-WDA501 SECTION.                                           
003995                                                                          
003996     STRING 'WLORDP01(WDA501KY=>' W-WDA5KEY-MIN-X                         
003997                    '&WDA501KY=<' W-WDA5KEY-MAX-X ')'                     
003998          DELIMITED BY SIZE  INTO SSA1                                    
003999     MOVE '  GEGB'             TO GODK-STATUSKODER                        
004000     CALL CBLTDLI USING GU  ORDP-PCB DLI-IO-AREA-ORDP SSA1                
004001     MOVE ORDP-STATUS-CODE     TO STATUS-WS                               
004002     PERFORM IMS-STATUSKONTROLL                                           
004003     .                                                                    
004004     SKIP2                                                                
004005                                                                          
004006 IMS-26-GHU-ORDP-WDA501 SECTION.                                          
004007                                                                          
004008     STRING 'WLORDP01(WDA501KY =' W-WDA5KEY-X ')'                         
004009          DELIMITED BY SIZE  INTO SSA1                                    
004010     MOVE '    '               TO GODK-STATUSKODER                        
004011     CALL CBLTDLI USING GHU ORDP-PCB DLI-IO-AREA-ORDP SSA1                
004012     MOVE ORDP-STATUS-CODE     TO STATUS-WS                               
004013     PERFORM IMS-STATUSKONTROLL                                           
004014     .                                                                    
004015     SKIP3                                                                
004016 IMS-27-REPL-ORDP-WDA501 SECTION.                                         
004017                                                                          
004018     MOVE '    '               TO GODK-STATUSKODER                        
004019     CALL CBLTDLI USING REPL ORDP-PCB DLI-IO-AREA-ORDP                    
004020     MOVE ORDP-STATUS-CODE     TO STATUS-WS                               
004021     PERFORM IMS-STATUSKONTROLL                                           
004022     .                                                                    
004023     EJECT                                                                
004024 IMS-33-GN-ORQM-WDQ101 SECTION.                                           
004025                                                                          
004026     STRING 'WLORQM01(WDQ101KY=>' W-WDQ101-KEY-MIN1                       
004027                    '&WDQ101KY=<' W-WDQ101-KEY-MAX1 ')'                   
004028          DELIMITED BY SIZE INTO SSA1                                     
004029     MOVE '  GEGB'             TO GODK-STATUSKODER                        
004030     CALL CBLTDLI USING GN   ORQM-PCB DLI-IO-AREA-ORQM SSA1               
004031     MOVE ORQM-STATUS-CODE     TO STATUS-WS                               
004032     PERFORM IMS-STATUSKONTROLL                                           
004033     .                                                                    
004034     SKIP2                                                                
004035 IMS-34-GHU-ORQM-WDQ101 SECTION.                                          
004036                                                                          
004037     STRING 'WLORQM01(WDQ101KY=>' W-WDQ101-KEY-MIN1                       
004038                    '&WDQ101KY=<' W-WDQ101-KEY-MAX1 ')'                   
004039          DELIMITED BY SIZE INTO SSA1                                     
004040     MOVE '  '                 TO GODK-STATUSKODER                        
004041     CALL CBLTDLI USING GHU  ORQM-PCB DLI-IO-AREA-ORQM SSA1               
004042     MOVE ORQM-STATUS-CODE     TO STATUS-WS                               
004043     PERFORM IMS-STATUSKONTROLL                                           
004044     .                                                                    
004045     EJECT                                                                
004046 IMS-35-GHU-PROC-WDE801 SECTION.                                          
004047                                                                          
004048     STRING 'WLPROC01(WDE801KY =' W-WDE801KY-X ')'                        
004049          DELIMITED BY SIZE  INTO SSA1                                    
004050     MOVE '  '                 TO GODK-STATUSKODER                        
004051     CALL CBLTDLI USING GHU PROC-PCB DLI-IO-AREA-PROC SSA1                
004052     MOVE PROC-STATUS-CODE     TO STATUS-WS                               
004053     PERFORM IMS-STATUSKONTROLL                                           
004054     .                                                                    
004055     EJECT                                                                
004056 IMS-36-REPL-PROC-WDE801 SECTION.                                         
004057                                                                          
004058     MOVE '    '               TO GODK-STATUSKODER                        
004059     CALL CBLTDLI USING REPL PROC-PCB DLI-IO-AREA-PROC                    
004060     MOVE PROC-STATUS-CODE     TO STATUS-WS                               
004061     PERFORM IMS-STATUSKONTROLL                                           
004062     .                                                                    
004063     EJECT                                                                
004064                                                                          
004065 IMS-GU-WDB601    SECTION.                                                
004066     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
004067          DELIMITED BY SIZE INTO SSA1                                     
004068     MOVE '  '   TO GODK-STATUSKODER                                      
004069     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
004070     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
004071     PERFORM IMS-STATUSKONTROLL                                           
004072     IF SEGMENT-SAKNAS                                                    
004073        MOVE SPACE TO DCS-KDDC                                            
004074     END-IF                                                               
004075     .                                                                    
004076     SKIP2                                                                
004077 IMS-GU-WDB201 SECTION.                                                   
004078                                                                          
004079     STRING  'WDB201  (IDGMT    =' W-WDB201KEY-X ')'                      
004080            DELIMITED BY SIZE INTO SSA1                                   
004081     MOVE    '    '             TO GODK-STATUSKODER                       
004082     CALL    CBLTDLI USING GU WDB2-PCB DLI-IO-AREA-B201 SSA1              
004083     MOVE    WDB2-STATUS-CODE   TO STATUS-WS                              
004084     PERFORM IMS-STATUSKONTROLL                                           
004085     .                                                                    
004086     EJECT                                                                
004087 IMS-STATUSKONTROLL SECTION.                                              
004088                                                                          
004089     SET STATUS-IX TO 1                                                   
004090     SEARCH GODK-STATUS                                                   
004091       AT END CALL FELLOG                                                 
004092       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
004093     END-SEARCH                                                           
004094     .                                                                    
004095     EJECT                                                                
004100*    -COPY WY2000Q1                                                       
