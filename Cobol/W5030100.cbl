000001 ID DIVISION.                                                             
000002 PROGRAM-ID.     W5030100.                                                
000003 AUTHOR.         CHRISTINA BRUHN                                          
000004 DATE-WRITTEN.   JUNI  86.                                                
000005                                                                          
000006*    FUNKTION.                                                            
000007*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!             
000008*     THIS PROGRAM ALSO HAS A WEB-LDC VERSION CALLED WL0173               
000009*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!             
000010*        INVENTERING, SUBPROGRAM TILL BILD 5302.                          
000011*        BEGÄRAN OM UTSKRIFT AV INVENTERINGSANMODAN                       
000012*        UTSKRIFT KAN VÄLJAS PÅ FÖLJANDE SÄTT:                            
000013*        - ETT VISST ARTIKELNUMMER                                        
000014*        - ETT VISST ANTAL                                                
000015*        - ETT VISST ANTAL INOM ETT LAGEROMRÅDE                           
000016*        OM ANTAL KOMBINERAS MED OMRÅDE LÄSES BASEN FRÅN BÖRJAN.          
000017*        PRIORITERADE ARTIKLAR (PRIORITET = 1) SKRIVS UT I FÖRSTA         
000018*        HAND OBEROENDE AV OMRÅDE.                                        
000019*                                                                         
000020*        FÖRSÖK HAR GJORTS ATT INDELA PROGRAMMET I SEPARATA DELAR         
000021*        FÖR ATT LÄTTARE HITTA DELAR MED LIKARTAT UTSEENDE                
000022*        FÖLJANDE OMRÅDEN ÄR GRUPPERADE:                                  
000023*        - CDC          DC 11                                             
000024*        - SDC          DC 21 - 26/LDC DC 1A-Z, 2A-Z, 4A-Z, 6A-Z          
000025*        - NDC-NA       DC 41 - 43 OCH 51                                 
000026*        - NDC-PACIFIC  DC 61 OCH 62: KALLAS HÄR FÖRENKLAT ADC            
000027*                                                                         
000028*    SUBPROGRAM:                                                          
000029*        W006PRS1   - SKÖTER ALL SKRIVNING MOT IMS-PRINTER                
000030*                                                                         
000031*    INDATA.                                                              
000032*        TRANSAKTION: W5T301                                              
000033*        TRANSAKTION: W5T302                                              
000034*        MID:         W5I30101                                            
000035*                                                                         
000036*    UTDATA.                                                              
000037*        MOD:         W5O30101                                            
000038*                                                                         
000039*    E-TRACKER: 7450319  2008-HÖST  VOHF                                  
000040*    E-TRACKER:10254592  2015       DECOMISSION VOHF                      
000041                                                                          
000042 ENVIRONMENT DIVISION.                                                    
000043                                                                          
000044 DATA DIVISION.                                                           
000045     EJECT                                                                
000046 WORKING-STORAGE SECTION.                                                 
000047*    -COPY WY2000W1                                                       
000048     SKIP3                                                                
000049 77    IDPGM                     PIC X(8)    VALUE 'W5030200'.            
000050 77    WS-SECTION                PIC X(32)   VALUE SPACE.                 
000051 77    WS-IMS                    PIC X(32)   VALUE SPACE.                 
000052                                                                          
000053                                                                          
000054 77    JA                        PIC X       VALUE 'J'.                   
000055 77    NEJ                       PIC X       VALUE 'N'.                   
000056 77    WS-RAD-RAEKNARE2          PIC S9(3)   VALUE  +0  COMP-3.           
000057 77    WS-KVLEVART-ADC           PIC S9(11)  VALUE  +0  COMP-3.           
000058 77    WS-KVLEVART-NDC           PIC S9(11)  VALUE  +0  COMP-3.           
000059 77    DUMMY-AREA                PIC X(50)   VALUE SPACE.                 
000060 77    SPRAK-IX                  PIC S9(9)   VALUE +0   COMP SYNC.        
000061 77    TAB-IX                    PIC S9(9)   VALUE +0   COMP SYNC.        
000062 77    IX                        PIC S9(9)   VALUE +0   COMP SYNC.        
000063 77    IND                       PIC S9(9)   VALUE +0   COMP SYNC.        
000064 77    INDX                      PIC S9(9)   VALUE +0   COMP SYNC.        
000065 77    WS-ANT                    PIC S9(3)   VALUE +4   COMP SYNC.        
000066 77    MAX-IX-7                  PIC S9(9)   VALUE +7   COMP SYNC.        
000067 77    MAX-IX-16                 PIC S9(9)   VALUE +16  COMP SYNC.        
000068 77    MAX-IX-20                 PIC S9(9)   VALUE +20  COMP SYNC.        
000069 77    SALDO-IX                  PIC S9(9)   VALUE +0   COMP SYNC.        
000070 77    PRINT-ANT                 PIC S9(9)   VALUE +0   COMP SYNC.        
000071 77    INV-ANM1-3-BJ             PIC X(8)    VALUE '301     '.            
000072 77    INV-ANM1-3-CDC            PIC X(8)    VALUE '302     '.            
000073 77    INV-ANM-DC21ET            PIC X(8)    VALUE '303     '.            
000074 77    INV-ANM-DC21              PIC X(8)    VALUE '304     '.            
000075 77    RAD-IX                    PIC S9(3)   VALUE ZERO COMP-3.           
000076 77    CD-IX                     PIC S9(3)   VALUE ZERO COMP-3.           
000077 77    WS-SUM-BUFFR              PIC S9(3)   VALUE ZERO COMP-3.           
000078                                                                          
000079*01  -COPY WWDCKONS                                                       
000080                                                                          
000081 01    WS-QTY-CDC                PIC S9(7)   VALUE  +0.                   
000082 01    WS-QTY-CDC-TOT            PIC S9(7)   VALUE  +0.                   
000083 01    WS-QTY-ADC                PIC S9(7)   VALUE  +0.                   
000084 01    WS-QTY-ADC-TOT            PIC S9(7)   VALUE  +0.                   
000085 01    WS-QTY-NDC                PIC S9(7)   VALUE  +0.                   
000086 01    WS-QTY-NDC-TOT            PIC S9(7)   VALUE  +0.                   
000087                                                                          
000088 01    WS-IDUSER                 PIC X(8)    VALUE SPACE.                 
000089 01    WS-DAGENS-DATUM           PIC 9(8)    VALUE ZERO.                  
000090                                                                          
000091 01    W-BLANKRAD                PIC X(132)  VALUE SPACE.                 
000092 01    WS-TISEGKEY               PIC 9(9).                                
000093                                                                          
000094 01    WS-IDPRTINV.                                                       
000095    03 WS-IDPRTOMG               PIC S9     COMP-3.                       
000096    03 WS-IDLOPNR                PIC S9(5)  COMP-3.                       
000097                                                                          
000098 01    W-IDPRTOMG-ALFA           PIC X.                                   
000099                                                                          
000100 01    WS-IDPRTINV-NUM           PIC 9(6).                                
000101 01    WS-IDLOPNR-5              PIC 9(5).                                
000102     EJECT                                                                
000103 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
000104   88  EGEN-TRANS                            VALUE '5301'.                
000105   88  GODK-TRANS                            VALUE '5301' '530B'.         
000106 77  WS-SATS                     PIC X(1)    VALUE 'N'.                   
000107   88  SATS-FINNS                            VALUE 'J'.                   
000108 77    INDATA-SW                 PIC X.                                   
000109   88  INDATA-OK                             VALUE 'J'.                   
000110 77    PRINT-SW                  PIC X.                                   
000111   88  PRINT-OK                              VALUE 'J'.                   
000112 77    BUFFERT-SW                PIC X.                                   
000113   88  BUFFERT-PRINT                         VALUE 'J'.                   
000114                                                                          
000115 77    WDB6-A-SW         PIC X       VALUE 'J'.                           
000116       88  WDB6-A-FINNS              VALUE 'J'.                           
000117       88  WDB6-A-SAKNAS             VALUE 'N'.                           
000118                                                                          
000119 01  WS-KVEFRS-OLD               PIC S9(7) COMP-3.                        
000120 01  WS-KVEFRS-OLD-N             PIC 9(7) COMP-3.                         
000121 01  WS-ANTAL-RADER-EFR          PIC 9(7).                                
000122 01  WS-BELEV                    PIC X(30).                               
000123 01  WS-IDBENR                   PIC S9   COMP-3.                         
000124 01  WS-IDLEVNR                  PIC X(5).                                
000125     EJECT                                                                
000126 01  DYNAMISKA-SUBPROGRAM.                                                
000127     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
000128     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
000129     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
000130     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
000131     03  W006PRS1                PIC X(8)    VALUE 'W006PRS1'.            
000132     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
000133     EJECT                                                                
000134 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
000135*01  -COPY  WMSGINIT                                                      
000136     EJECT                                                                
000137 01  FILLER                      PIC X(16)   VALUE 'WDATAREA'.            
000138*01  -COPY  WDATAREA                                                      
000139     EJECT                                                                
000140 01  WS-FAELT.                                                            
000141     03  WS-TIAAVVD              PIC 9(5).                                
000142     03  W-SDC-ANTAL             PIC S9(9)   VALUE  ZERO COMP-3.          
000143     03  W-ADC-ANTAL             PIC S9(9)   VALUE  ZERO COMP-3.          
000144     03  W-NDC-ANTAL             PIC S9(9)   VALUE  ZERO COMP-3.          
000145                                                                          
000146 01  CDC-TAB-EFR.                                                         
000147   03  WCDC-KVLEVART-TOT         PIC 9(9)    VALUE ZERO.                  
000148   03  WCDC-TAB OCCURS 20 TIMES.                                          
000149     05  WCDC-IDKUNDRF           PIC 9(5)    VALUE ZERO.                  
000150     05  WCDC-IDKUNDNR           PIC 9(5)    VALUE ZERO.                  
000151     05  WCDC-IDDISTR            PIC 9(5)    VALUE ZERO.                  
000152     05  WCDC-KVLEVART           PIC 9(7)    VALUE ZERO.                  
000153                                                                          
000154 01  SDC-TAB-EFR.                                                         
000155   03  WSDC-TAB OCCURS 7  TIMES.                                          
000156     05  WSDC-IDKUNDRF           PIC 9(5)    VALUE ZERO.                  
000157     05  WSDC-IDKUNDNR           PIC 9(5)    VALUE ZERO.                  
000158     05  WSDC-IDDISTR            PIC 9(5)    VALUE ZERO.                  
000159     05  WSDC-KVLEVART           PIC 9(7)    VALUE ZERO.                  
000160                                                                          
000161 01  ADC-TAB-EFR.                                                         
000162   03  WADC-TAB OCCURS 16 TIMES.                                          
000163     05  WADC-IDKUNDRF           PIC 9(5)    VALUE ZERO.                  
000164     05  WADC-IDKUNDNR           PIC 9(5)    VALUE ZERO.                  
000165     05  WADC-IDDISTR            PIC 9(5)    VALUE ZERO.                  
000166     05  WADC-KVLEVART           PIC 9(7)    VALUE ZERO.                  
000167                                                                          
000168 01  NDC-TAB-EFR.                                                         
000169   03  WNDC-TAB OCCURS 16 TIMES.                                          
000170     05  WNDC-IDKUNDRF           PIC 9(5)    VALUE ZERO.                  
000171     05  WNDC-IDKUNDNR           PIC 9(5)    VALUE ZERO.                  
000172     05  WNDC-IDDISTR            PIC 9(5)    VALUE ZERO.                  
000173     05  WNDC-KVLEVART           PIC 9(7)    VALUE ZERO.                  
000174                                                                          
000175 01  WSATS-TABELL.                                                        
000176   03  WSATS-KVLEVART-TOT         PIC 9(9)   VALUE ZERO.                  
000177   03  FILLER OCCURS 6.                                                   
000178     05  WSATS-IDORDNR            PIC 9(5)   VALUE ZERO.                  
000179     05  WSATS-KVLEVART           PIC 9(9)   VALUE ZERO.                  
000180     05  WSATS-SATSNR             PIC 9(10)  VALUE ZERO.                  
000181     05  WSATS-IDDISTR            PIC 9(5)   VALUE ZERO.                  
000182     EJECT                                                                
000183 01  WS-PRINTERDEST.                                                      
000184     03  WS-PRT1                 PIC X(8).                                
000185                                                                          
000186 01  WS-IDKUNDRF.                                                         
000187     03  WS-IDORDNR              PIC 9(5).                                
000188     03  FILLER                  PIC X(5).                                
000189 01  WS-IDARTNR-SATS             PIC S9(9) COMP-3.                        
000190                                                                          
000191 01  SATS-TABELL.                                                         
000192   03  TAB-SATS OCCURS 10 TIMES.                                          
000193     05  TAB-SATS-IDARTNR        PIC S9(9)       COMP-3.                  
000194     EJECT                                                                
000195 01  FILLER                      PIC X(16)   VALUE 'NYC-TILL-DLI'.        
000196 01    NYCKLAR-TILL-DLI.                                                  
000197     03  W-WDJ1CSEQ-X.                                                    
000198         05  W-IDLEVNR            PIC X(5)  VALUE SPACE.                  
000199         05  W-BELEVART           PIC X(30) VALUE SPACE.                  
000200         05  W-IDARTNR-WDJ1C      PIC S9(9) VALUE ZERO    COMP-3.         
000201   03  W-ROT-WDG3KEY-X.                                                   
000202     05  FILLER                  PIC X(4)    VALUE '5113'.                
000203     05  W-IDDC-G3               PIC X(2)    VALUE SPACE.                 
000204     05  FILLER                  PIC X(24)   VALUE LOW-VALUE.             
000205   03  W-IDLEVNR-SATS-X.                                                  
000206     05  W-IDLEVNR-SATS          PIC X(5)    VALUE '1002 '.               
000207   03  W-IDARTNR-100000000-X.                                             
000208     05  W-IDARTNR-100000000     PIC S9(9)   COMP-3                       
000209                                 VALUE +100000000.                        
000210   03  W-IDARTNR-X.                                                       
000211     05  W-IDARTNR               PIC S9(9)   COMP-3.                      
000212                                                                          
000213   03  W-WDE401-X.                                                        
000214     05  W-401-IDDISTR           PIC S9(5) VALUE ZERO COMP-3.             
000215     05  W-401-IDKUNDNR          PIC S9(7) VALUE ZERO COMP-3.             
000216     05  W-401-IDKUNDRF.                                                  
000217       07  W-401-IDORDNR         PIC 9(5)  VALUE ZERO.                    
000218       07  FILLER                PIC X(5)  VALUE SPACE.                   
000219     05  W-401-IDPRODNR          PIC S9(7) VALUE ZERO COMP-3.             
000220     05  W-401-IDPLKLST          PIC S9(3) VALUE ZERO COMP-3.             
000221*                                                                         
000222   03  W-WDE411-X.                                                        
000223     05  W-411-IDPURAD           PIC S9(5) VALUE ZERO COMP-3.             
000224                                                                          
000225   03  W-ORDSTA-X.                                                        
000226     05  W-ORDSTA                PIC S9      COMP-3 VALUE +4.             
000227   03  W-IDSKYLT-X.                                                       
000228     05  W-IDSKYLT               PIC X(3)    VALUE SPACE.                 
000229                                                                          
000230   03  W-IDDC                    PIC X(2)    VALUE SPACE.                 
000231                                                                          
000232   03  W-TISEGKEY-X.                                                      
000233     05  W-TISEGKEY         PIC S9(9)   VALUE +999999999 COMP-3.          
000234                                                                          
000235   03  W-WDD8B1KY-MIN-X.                                                  
000236     05 W-IDDC-D8-MIN            PIC X(2)         VALUE SPACE.            
000237     05 W-IDARTNR-D8-MIN         PIC S9(9) COMP-3 VALUE ZERO.             
000238     05 FILLER                   PIC X(15)   VALUE LOW-VALUE.             
000239                                                                          
000240   03 W-WDD8B1KY-MAX-X.                                                   
000241     05 W-IDDC-D8-MAX            PIC X(2)          VALUE SPACE.           
000242     05 W-IDARTNR-D8-MAX         PIC S9(9) COMP-3 VALUE ZERO.             
000243     05 FILLER                   PIC X(15)   VALUE HIGH-VALUE.            
000244                                                                          
000260   03  W-WDH111KY-MIN-X.                                                  
000270     05  W-IDDC-WDH1-MIN      PIC X(2)  VALUE SPACE.                      
000280     05  W-KDINVKAT-WDH1-MIN  PIC S9(3)                  COMP-3.          
000290     05  W-TISEGKEY-WDH1-MIN  PIC S9(9) VALUE ZERO       COMP-3.          
000300     05  W-DAREGDAT-SORT-MIN    PIC  9(8) VALUE ZERO.                     
000400   03  W-WDH111KY-MAX-X.                                                  
000500     05  W-IDDC-WDH1-MAX      PIC X(2)  VALUE SPACE.                      
000501     05  W-KDINVKAT-WDH1-MAX  PIC S9(3)                  COMP-3.          
000502     05  W-TISEGKEY-WDH1-MAX  PIC S9(9) VALUE +999999999 COMP-3.          
000503     05  W-DAREGDAT-SORT-MAX    PIC  9(8) VALUE 99999999.                 
000504     EJECT                                                                
000505   03  W-WDE4C1KY-LOW.                                                    
000506     05  W-IDARTNR-LOW           PIC S9(9)   COMP-3.                      
000507     05  FILLER                  PIC X(7)    VALUE  LOW-VALUE.            
000508   03  W-WDE4C1KY-HIGH.                                                   
000509     05  W-IDARTNR-HIGH          PIC S9(9)   COMP-3.                      
000510     05  FILLER                  PIC X(7)    VALUE  HIGH-VALUE.           
000511   03  W-IDDISTR-X.                                                       
000512     05  W-IDDISTR               PIC S9(5)   COMP-3.                      
000513   03  W-WDE4KEY-X.                                                       
000514     05  W-IDDISTR-WDE4          PIC S9(5)   COMP-3.                      
000515     05  W-IDKUNDNR              PIC S9(7)   COMP-3.                      
000516     05  W-IDKUNDRF              PIC X(10).                               
000517     05  W-IDPRODNR              PIC S9(7)   COMP-3.                      
000518     05  W-IDPLKLST              PIC S9(3)   COMP-3.                      
000519   03  W-IDPURAD-X.                                                       
000520     05  W-IDPURAD               PIC S9(5)   COMP-3.                      
000521   03  W-WDG3KEY01-X.                                                     
000522     05  W-IDHTYP                PIC X(4)    VALUE '5101'.                
000523     05  W-FILLER                PIC X(26)   VALUE LOW-VALUE.             
000524   03  W-WDGX5102-X.                                                      
000525     05  W-KDSEGKEY              PIC X       VALUE '1'.                   
000526     05  W-IDLOPNR               PIC S9(5)   COMP-3.                      
000527                                                                          
000528   03  W-IDDC-B6-X.                                                       
000529     05 W-IDDC-B6                PIC X(2).                                
000530                                                                          
000531     EJECT                                                                
000532 01  ANT-MEDDELANDE.                                                      
000533   03  SV-ANT-MED.                                                        
000534     05  FILLER                  PIC X(12)                                
000535         VALUE 'BEGÄRT ANT  '.                                            
000536     05  SV-BEG-ANT              PIC 9(2).                                
000537     05  FILLER                  PIC X(9)                                 
000538         VALUE ' PRINTAT '.                                               
000539     05  SV-PRINT-ANT            PIC 9(2).                                
000540   03  BG-ANT-MED.                                                        
000541     05  FILLER                  PIC X(12)                                
000542         VALUE 'AANGEVRAAGD '.                                            
000543     05  BG-BEG-ANT              PIC 9(2).                                
000544     05  FILLER                  PIC X(9)                                 
000545         VALUE ' GEDRUKT '.                                               
000546     05  BG-PRINT-ANT            PIC 9(2).                                
000547     SKIP3                                                                
000548 01  ANTAL-MED REDEFINES ANT-MEDDELANDE.                                  
000549   03  ANTALS-RAD OCCURS 2.                                               
000550     05  FILLER                  PIC X(12).                               
000551     05  BEGAERT-ANT             PIC 9(2).                                
000552     05  FILLER                  PIC X(9).                                
000553     05  PRINTAT-ANT             PIC 9(2).                                
000554     EJECT                                                                
000555 01  ART-MEDDELANDE.                                                      
000556   03  SV-ART-MED.                                                        
000557     05  SV-PRINT-ARTIKEL        PIC Z(9).                                
000558     05  FILLER                  PIC X(10)                                
000559         VALUE ' UTSKRIVEN'.                                              
000560   03  BG-ART-MED.                                                        
000561     05  BG-PRINT-ARTIKEL        PIC Z(9).                                
000562     05  FILLER                  PIC X(10)                                
000563         VALUE ' GEDRUKT'.                                                
000564     SKIP2                                                                
000565 01  ARTIKEL-MED REDEFINES ART-MEDDELANDE.                                
000566   03  ARTIKEL-RAD OCCURS 2.                                              
000567     05  PRINT-ARTIKEL           PIC Z(9).                                
000568     05  FILLER                  PIC X(10).                               
000569     EJECT                                                                
000570 01    MEDDELANDE.                                                        
000571   03  FEL1.                                                              
000572     05  FILLER                  PIC X(40)                                
000573           VALUE 'UPPDATERING ENBART FRÅN EGEN BILD'.                     
000574     05  FILLER                  PIC X(40)                                
000575           VALUE 'UPPDATERING ENBART FRÅN EGEN BILD'.                     
000576   03  FILLER REDEFINES FEL1.                                             
000577     05  FEL-1                   PIC X(40)  OCCURS 2.                     
000578     EJECT                                                                
000579*************************************************                         
000580*****    LISTTRANSAR FÖR CDC-STOCKTAKING   ******                         
000581*****    BUFFERT-LISTAN                    ******                         
000582*************************************************                         
000583 01  BU-RUB-CDC.                                                          
000584   03  FILLER                       PIC X(10)  VALUE ' CAR PARTS'.        
000585   03  FILLER                       PIC X(9)   VALUE ' LISTA.  '.         
000586   03  FILLER                       PIC X(10)  VALUE 'W50302-002'.        
000587   03  FILLER                       PIC X(32)  VALUE                      
000588                             '     INVENTERINGSUNDERLAG  DC 11'.          
000589   03  FILLER                       PIC X(5)   VALUE ' KAT '.             
000590   03  BU-KDINVKAT-CDC              PIC 9(2)   VALUE ZERO.                
000591   03  FILLER                       PIC X(8)   VALUE '  DATUM '.          
000592   03  BU-TIREGDAT-CDC              PIC 9(6)   VALUE ZERO.                
000593   03  FILLER                       PIC X(11) VALUE ' UTSKRIVEN '.        
000594   03  BU-DATUM-CDC                 PIC 9(6)   VALUE ZERO.                
000595   03  FILLER                       PIC X(1)   VALUE '-'.                 
000596   03  BU-TID-CDC                   PIC 9(4)   VALUE ZERO.                
000597   03  FILLER                       PIC X(28)  VALUE SPACE.               
000598 01  BU-RUB2-CDC.                                                         
000599   03  FILLER                       PIC X(10)  VALUE ' ARTIKELNR'.        
000600   03  BU-IDARTNR-CDC               PIC Z(9)   VALUE ZERO.                
000601   03  FILLER                       PIC X(3)   VALUE SPACE.               
000602   03  FILLER                       PIC X(14)  VALUE 'BENÄMNING:'.        
000603   03  BU-BEART-CDC                 PIC X(31)  VALUE SPACE.               
000604   03  FILLER                       PIC X(65)  VALUE SPACE.               
000605*************** SIDA 2, CDC  BUFFERTRUBRIK ************                   
000606 01  LIST-BUFFERT-SIDA2-CDC.                                              
000607   03  BU-RUB3-CDC.                                                       
000608     05  FILLER                     PIC X(50)  VALUE                      
000609                                ' BUFFERT           ANTAL'.               
000610     05  FILLER                     PIC X(50)  VALUE                      
000611                                ' BUFFERT           ANTAL'.               
000612     05  FILLER                     PIC X(32)  VALUE SPACE.               
000613**************  SIDA 2, CDC BUFFERTRAD ***************                    
000614 01  BU-RAD-CDC.                                                          
000615   03  FILLER OCCURS 2.                                                   
000616     05  BU-ADBUFFOMR-CDC           PIC Z(3)   VALUE ZERO.                
000617     05  BU-ADBUFFGANG-CDC          PIC Z(3)   VALUE ZERO.                
000618     05  BU-ADBUFFPL-CDC            PIC Z(6)   VALUE ZERO.                
000619     05  FILLER                     PIC X(7)   VALUE SPACE.               
000620     05  BU-QTY-CDC                 PIC Z(5)   VALUE ZERO.                
000621     05  FILLER                     PIC X(7)   VALUE SPACE.               
000622     05  BU-PUNKT-CDC               PIC X(7)   VALUE SPACE.               
000623     05  FILLER                     PIC X(12)  VALUE SPACE.               
000624   03  FILLER                       PIC X(32)  VALUE SPACE.               
000625*************************************************                         
000626*****    LISTTRANSAR FÖR CDC-STOCKTAKING   ******                         
000627*************************************************                         
000628 01  CDC-RUBRAD.                                                          
000629   03  FILLER                       PIC X(10)  VALUE ' CAR PARTS'.        
000630   03  FILLER                       PIC X(9)   VALUE ' LISTA.  '.         
000631   03  FILLER                       PIC X(10)  VALUE 'W50302-001'.        
000632   03  FILLER                       PIC X(32)  VALUE                      
000633                             '     INVENTERINGSUNDERLAG  DC 11'.          
000634   03  FILLER                       PIC X(5)   VALUE ' KAT '.             
000635   03  RUB-KDINVKAT-CDC             PIC 9(2)   VALUE ZERO.                
000636   03  FILLER                       PIC X(8)   VALUE '  DATUM '.          
000637   03  RUB-TIREGDAT-CDC             PIC 9(6)   VALUE ZERO.                
000638   03  FILLER                       PIC X(11) VALUE ' UTSKRIVEN '.        
000639   03  RUB-DATUM-CDC                PIC 9(6)   VALUE ZERO.                
000640   03  FILLER                       PIC X(1)   VALUE '-'.                 
000641   03  RUB-TID-CDC                  PIC 9(4)   VALUE ZERO.                
000642   03  FILLER                       PIC X(2)   VALUE SPACE.               
000643   03  FILLER                       PIC X(8)   VALUE 'LISTNR '.           
000644   03  RUB-PRINTNR-CDC              PIC Z(6)   VALUE ZERO.                
000645   03  FILLER                       PIC X(12)  VALUE SPACE.               
000646                                                                          
000647 01  L1-RAD-1-CDC.                                                        
000648   03  FILLER                       PIC X(10)  VALUE ' ARTIKELNR'.        
000649   03  L1-IDARTNR-CDC               PIC Z(9)   VALUE ZERO.                
000650   03  FILLER                       PIC X(3)   VALUE SPACE.               
000651   03  FILLER                       PIC X(5)   VALUE 'PKOD '.             
000652   03  L1-KDPRODSL-CDC              PIC Z(2)   VALUE ZERO.                
000653   03  FILLER                       PIC X(7)   VALUE ' F-GRP '.           
000654   03  L1-IDFKNGRP-CDC              PIC Z(4)   VALUE ZERO.                
000655   03  FILLER                       PIC X(9)   VALUE SPACE.               
000656   03  FILLER                       PIC X(14)  VALUE                      
000657                                        ' UTREDN.SALDO '.                 
000658   03  L1-KVUTRS-CDC                PIC Z(6)9- VALUE ZERO.                
000659   03  FILLER                       PIC X(2)   VALUE SPACE.               
000660   03  FILLER                       PIC X(20)  VALUE                      
000661                                        'INGÅR I SATS (1221):'.           
000662   03  L1-KDIART-CDC                PIC X(3)   VALUE '   '.               
000663   03  FILLER                       PIC X(15)  VALUE                      
000664                                        '   ERSÄTTN.KOD '.                
000665   03  L1-KDERS-CDC                 PIC Z99    VALUE ZERO.                
000666   03  FILLER                       PIC X(18)  VALUE SPACE.               
000667 01  L1-RAD-3-CDC.                                                        
000668   03  FILLER                       PIC X(14)  VALUE                      
000669                                               ' BENÄMNING:  '.           
000670   03  L1-BEART-CDC                 PIC X(34)  VALUE SPACE.               
000671   03  FILLER                       PIC X(5)   VALUE '  AK '.             
000672   03  L1-KVAKS-CDC                 PIC Z(6)9  VALUE SPACE.               
000673   03  FILLER                       PIC X(20)  VALUE                      
000674                                           '  (5107)(6123)(6126)'.        
000675   03  FILLER                       PIC X(84)  VALUE SPACE.               
000676 01  L1-RAD-4-CDC.                                                        
000677   03  FILLER                       PIC X(50)  VALUE SPACE.               
000678   03  FILLER                       PIC X(16)  VALUE 'QSALDO'.            
000679   03  L1-KVQS-CDC                  PIC Z(6)9  VALUE ZERO.                
000680   03  FILLER                       PIC X(24)  VALUE SPACE.               
000681   03  FILLER                       PIC X(17)  VALUE                      
000682                                              'EJ PACKN.RAPP EFR'.        
000683   03  FILLER                       PIC X(18)  VALUE SPACE.               
000684 01  L1-RAD-5-CDC.                                                        
000685   03  FILLER                       PIC X      VALUE SPACE.               
000686   03  FILLER                       PIC X(10)  VALUE 'LAGERPLATS'.        
000687   03  FILLER                       PIC X(3)   VALUE SPACE.               
000688   03  FILLER                       PIC X(3)   VALUE 'OMR'.               
000689   03  FILLER                       PIC X(3)   VALUE SPACE.               
000690   03  FILLER                       PIC X(5)   VALUE 'ANTAL'.             
000691   03  FILLER                       PIC X(62)  VALUE SPACE.               
000692   03  FILLER                       PIC X(32)  VALUE                      
000693                               'DISTR     KUND  ORDERNR    ANTAL'.        
000694   03  FILLER                       PIC X(11)  VALUE SPACE.               
000695 01  L1-RAD-6-CDC.                                                        
000696   03  L1-ADLAGOMR-CDC              PIC Z(3)   VALUE ZERO.                
000697   03  FILLER                       PIC X      VALUE SPACE.               
000698   03  L1-ADGANG-CDC                PIC Z(3)   VALUE ZERO.                
000699   03  FILLER                       PIC X      VALUE SPACE.               
000700   03  L1-ADPLATS-CDC               PIC Z(5)   VALUE ZERO.                
000701   03  FILLER                       PIC X      VALUE SPACE.               
000702   03  L1-OMR-CDC                   PIC X(3)   VALUE SPACE.               
000703   03  FILLER                       PIC X(10)  VALUE SPACE.               
000704   03  FILLER                       PIC X(7)   VALUE 'PLATS  '.           
000705   03  FILLER                       PIC X(3)   VALUE SPACE.               
000706   03  FILLER                       PIC X(7)   VALUE '.......'.           
000707   03  FILLER                       PIC X(6)   VALUE SPACE.               
000708   03  FILLER                       PIC X(13)  VALUE                      
000709                                                 'LAGERSALDO   '.         
000710   03  FILLER                       PIC X(3)   VALUE SPACE.               
000711   03  L1-KVLS-CDC                  PIC Z(6)9- VALUE ZERO.                
000712   03  FILLER                       PIC X(13)  VALUE SPACE.               
000713   03  L1-IDDISTR-CDC-1             PIC Z(5)   VALUE ZERO.                
000714   03  FILLER                       PIC X(4)   VALUE SPACE.               
000715   03  L1-IDKUNDNR-CDC-1            PIC Z(5)   VALUE ZERO.                
000716   03  FILLER                       PIC X(4)   VALUE SPACE.               
000717   03  L1-IDKUNDRF-CDC-1            PIC Z(5)   VALUE ZERO.                
000718   03  FILLER                       PIC X(2)   VALUE SPACE.               
000719   03  L1-KVLEVART-CDC-1            PIC Z(7)   VALUE ZERO.                
000720   03  FILLER                       PIC X(13)  VALUE SPACE.               
000721 01  L1-RAD-7-CDC.                                                        
000722   03  FILLER                       PIC X(50)  VALUE SPACE.               
000723   03  FILLER                       PIC X(23)  VALUE ALL '*'.             
000724   03  FILLER                       PIC X(14)  VALUE SPACE.               
000725   03  L1-IDDISTR-CDC-2             PIC Z(5)   VALUE ZERO.                
000726   03  FILLER                       PIC X(4)   VALUE SPACE.               
000727   03  L1-IDKUNDNR-CDC-2            PIC Z(5)   VALUE ZERO.                
000728   03  FILLER                       PIC X(4)   VALUE SPACE.               
000729   03  L1-IDKUNDRF-CDC-2            PIC Z(5)   VALUE ZERO.                
000730   03  FILLER                       PIC X(2)   VALUE SPACE.               
000731   03  L1-KVLEVART-CDC-2            PIC Z(7)   VALUE ZERO.                
000732   03  FILLER                       PIC X(13)  VALUE SPACE.               
000733 01  L1-RAD-8-CDC.                                                        
000734   03  L1-ADLAGOMR-SVS              PIC Z(3)   VALUE ZERO.                
000735   03  FILLER                       PIC X      VALUE SPACE.               
000736   03  L1-ADGANG-SVS                PIC Z(3)   VALUE ZERO.                
000737   03  FILLER                       PIC X      VALUE SPACE.               
000738   03  L1-ADPLATS-SVS               PIC Z(5)   VALUE ZERO.                
000739   03  FILLER                       PIC X      VALUE SPACE.               
000740   03  L1-OMR-SVS                   PIC X(3)   VALUE SPACE.               
000741   03  FILLER                       PIC X      VALUE SPACE.               
000742   03  L1-KVLS-SVS                  PIC Z(7)-  VALUE SPACE.               
000743   03  FILLER                       PIC X      VALUE SPACE.               
000744   03  FILLER                       PIC X(17)  VALUE ALL '.'.             
000745   03  FILLER                       PIC X(6)   VALUE SPACE.               
000746   03  FILLER                       PIC X(27)  VALUE                      
000747                                    'EFR EJ PLOCKADE+.......    '.        
000748   03  FILLER                       PIC X(10)  VALUE SPACE.               
000749   03  L1-IDDISTR-CDC-3             PIC Z(5)   VALUE ZERO.                
000750   03  FILLER                       PIC X(4)   VALUE SPACE.               
000751   03  L1-IDKUNDNR-CDC-3            PIC Z(5)   VALUE ZERO.                
000752   03  FILLER                       PIC X(4)   VALUE SPACE.               
000753   03  L1-IDKUNDRF-CDC-3            PIC Z(5)   VALUE ZERO.                
000754   03  FILLER                       PIC X(2)   VALUE SPACE.               
000755   03  L1-KVLEVART-CDC-3            PIC Z(7)   VALUE ZERO.                
000756   03  FILLER                       PIC X(13)  VALUE SPACE.               
000757 01  L1-RAD-9-CDC.                                                        
000758   03  FILLER                       PIC X(87)  VALUE SPACE.               
000759   03  L1-IDDISTR-CDC-4             PIC Z(5)   VALUE ZERO.                
000760   03  FILLER                       PIC X(4)   VALUE SPACE.               
000761   03  L1-IDKUNDNR-CDC-4            PIC Z(5)   VALUE ZERO.                
000762   03  FILLER                       PIC X(4)   VALUE SPACE.               
000763   03  L1-IDKUNDRF-CDC-4            PIC Z(5)   VALUE ZERO.                
000764   03  FILLER                       PIC X(2)   VALUE SPACE.               
000765   03  L1-KVLEVART-CDC-4            PIC Z(7)   VALUE ZERO.                
000766   03  FILLER                       PIC X(13)  VALUE SPACE.               
000767 01  L1-RAD-10-CDC.                                                       
000768   03  L1-ADBUFFOMR-CDC-1           PIC Z(3)   VALUE ZERO.                
000769   03  FILLER                       PIC X      VALUE SPACE.               
000770   03  L1-ADBUFFGANG-CDC-1          PIC Z(3)   VALUE ZERO.                
000771   03  FILLER                       PIC X      VALUE SPACE.               
000772   03  L1-ADBUFFPLATS-CDC-1         PIC Z(5)   VALUE ZERO.                
000773   03  FILLER                       PIC X      VALUE SPACE.               
000774   03  L1-OMR-CDC-1                 PIC X(3)   VALUE SPACE.               
000775   03  FILLER                       PIC X      VALUE SPACE.               
000776   03  L1-QTY-CDC-1                 PIC Z(7)   VALUE ZERO.                
000777   03  FILLER                       PIC X(2)   VALUE SPACE.               
000778   03  FILLER                       PIC X(17)  VALUE ALL '.'.             
000779   03  FILLER                       PIC X(6)   VALUE SPACE.               
000780   03  FILLER                       PIC X(15)  VALUE                      
000781                                                'FYSISKT LAGER  '.        
000782   03  FILLER                       PIC X(10)  VALUE                      
000783                                               '-.......  '.              
000784   03  FILLER                       PIC X(12)  VALUE SPACE.               
000785   03  L1-IDDISTR-CDC-5             PIC Z(5)   VALUE ZERO.                
000786   03  FILLER                       PIC X(4)   VALUE SPACE.               
000787   03  L1-IDKUNDNR-CDC-5            PIC Z(5)   VALUE ZERO.                
000788   03  FILLER                       PIC X(4)   VALUE SPACE.               
000789   03  L1-IDKUNDRF-CDC-5            PIC Z(5)   VALUE ZERO.                
000790   03  FILLER                       PIC X(2)   VALUE SPACE.               
000791   03  L1-KVLEVART-CDC-5            PIC Z(7)   VALUE ZERO.                
000792   03  FILLER                       PIC X(13)  VALUE SPACE.               
000793 01  L1-RAD-11-CDC.                                                       
000794   03  FILLER                       PIC X(87)  VALUE SPACE.               
000795   03  L1-IDDISTR-CDC-6             PIC Z(5)   VALUE ZERO.                
000796   03  FILLER                       PIC X(4)   VALUE SPACE.               
000797   03  L1-IDKUNDNR-CDC-6            PIC Z(5)   VALUE ZERO.                
000798   03  FILLER                       PIC X(4)   VALUE SPACE.               
000799   03  L1-IDKUNDRF-CDC-6            PIC Z(5)   VALUE ZERO.                
000800   03  FILLER                       PIC X(2)   VALUE SPACE.               
000801   03  L1-KVLEVART-CDC-6            PIC Z(7)   VALUE ZERO.                
000802   03  FILLER                       PIC X(13)  VALUE SPACE.               
000803 01  L1-RAD-12-CDC.                                                       
000804   03  L1-ADBUFFOMR-CDC-2           PIC Z(3)   VALUE ZERO.                
000805   03  FILLER                       PIC X      VALUE SPACE.               
000806   03  L1-ADBUFFGANG-CDC-2          PIC Z(3)   VALUE ZERO.                
000807   03  FILLER                       PIC X      VALUE SPACE.               
000808   03  L1-ADBUFFPLATS-CDC-2         PIC Z(5)   VALUE ZERO.                
000809   03  FILLER                       PIC X      VALUE SPACE.               
000810   03  L1-OMR-CDC-2                 PIC X(3)   VALUE SPACE.               
000811   03  FILLER                       PIC X      VALUE SPACE.               
000812   03  L1-QTY-CDC-2                 PIC Z(7)   VALUE ZERO.                
000813   03  FILLER                       PIC X(2)   VALUE SPACE.               
000814   03  FILLER                       PIC X(17)  VALUE ALL '.'.             
000815   03  FILLER                       PIC X(6)   VALUE SPACE.               
000816   03  FILLER                       PIC X(15)  VALUE                      
000817                                              'DIFFERENS      '.          
000818   03  FILLER                       PIC X(10)  VALUE                      
000819                                         '=.......  '.                    
000820   03  FILLER                       PIC X(12)  VALUE SPACE.               
000821   03  L1-IDDISTR-CDC-7             PIC Z(5)   VALUE ZERO.                
000822   03  FILLER                       PIC X(4)   VALUE SPACE.               
000823   03  L1-IDKUNDNR-CDC-7            PIC Z(5)   VALUE ZERO.                
000824   03  FILLER                       PIC X(4)   VALUE SPACE.               
000825   03  L1-IDKUNDRF-CDC-7            PIC Z(5)   VALUE ZERO.                
000826   03  FILLER                       PIC X(2)   VALUE SPACE.               
000827   03  L1-KVLEVART-CDC-7            PIC Z(7)   VALUE ZERO.                
000828   03  FILLER                       PIC X(13)  VALUE SPACE.               
000829 01  L1-RAD-13-CDC.                                                       
000830   03  FILLER                       PIC X(87)  VALUE SPACE.               
000831   03  L1-IDDISTR-CDC-8             PIC Z(5)   VALUE ZERO.                
000832   03  FILLER                       PIC X(4)   VALUE SPACE.               
000833   03  L1-IDKUNDNR-CDC-8            PIC Z(5)   VALUE ZERO.                
000834   03  FILLER                       PIC X(4)   VALUE SPACE.               
000835   03  L1-IDKUNDRF-CDC-8            PIC Z(5)   VALUE ZERO.                
000836   03  FILLER                       PIC X(2)   VALUE SPACE.               
000837   03  L1-KVLEVART-CDC-8            PIC Z(7)   VALUE ZERO.                
000838   03  FILLER                       PIC X(13)  VALUE SPACE.               
000839 01  L1-RAD-14-CDC.                                                       
000840   03  L1-ADBUFFOMR-CDC-3           PIC Z(3)   VALUE ZERO.                
000841   03  FILLER                       PIC X      VALUE SPACE.               
000842   03  L1-ADBUFFGANG-CDC-3          PIC Z(3)   VALUE ZERO.                
000843   03  FILLER                       PIC X      VALUE SPACE.               
000844   03  L1-ADBUFFPLATS-CDC-3         PIC Z(5)   VALUE ZERO.                
000845   03  FILLER                       PIC X      VALUE SPACE.               
000846   03  L1-OMR-CDC-3                 PIC X(3)   VALUE SPACE.               
000847   03  FILLER                       PIC X      VALUE SPACE.               
000848   03  L1-QTY-CDC-3                 PIC Z(7)   VALUE ZERO.                
000849   03  FILLER                       PIC X(2)   VALUE SPACE.               
000850   03  FILLER                       PIC X(17)  VALUE ALL '.'.             
000851   03  FILLER                       PIC X(43)  VALUE SPACE.               
000852   03  L1-IDDISTR-CDC-9             PIC Z(5)   VALUE ZERO.                
000853   03  FILLER                       PIC X(4)   VALUE SPACE.               
000854   03  L1-IDKUNDNR-CDC-9            PIC Z(5)   VALUE ZERO.                
000855   03  FILLER                       PIC X(4)   VALUE SPACE.               
000856   03  L1-IDKUNDRF-CDC-9            PIC Z(5)   VALUE ZERO.                
000857   03  FILLER                       PIC X(2)   VALUE SPACE.               
000858   03  L1-KVLEVART-CDC-9            PIC Z(7)   VALUE ZERO.                
000859   03  FILLER                       PIC X(13)  VALUE SPACE.               
000860 01  L1-RAD-15-CDC.                                                       
000861   03  FILLER                       PIC X(87)  VALUE SPACE.               
000862   03  L1-IDDISTR-CDC-10            PIC Z(5)   VALUE ZERO.                
000863   03  FILLER                       PIC X(4)   VALUE SPACE.               
000864   03  L1-IDKUNDNR-CDC-10           PIC Z(5)   VALUE ZERO.                
000865   03  FILLER                       PIC X(4)   VALUE SPACE.               
000866   03  L1-IDKUNDRF-CDC-10           PIC Z(5)   VALUE ZERO.                
000867   03  FILLER                       PIC X(2)   VALUE SPACE.               
000868   03  L1-KVLEVART-CDC-10           PIC Z(7)   VALUE ZERO.                
000869   03  FILLER                       PIC X(13)  VALUE SPACE.               
000870 01  L1-RAD-16-CDC.                                                       
000871   03  L1-ADBUFFOMR-CDC-4           PIC Z(3)   VALUE ZERO.                
000872   03  FILLER                       PIC X      VALUE SPACE.               
000873   03  L1-ADBUFFGANG-CDC-4          PIC Z(3)   VALUE ZERO.                
000874   03  FILLER                       PIC X      VALUE SPACE.               
000875   03  L1-ADBUFFPLATS-CDC-4         PIC Z(5)   VALUE ZERO.                
000876   03  FILLER                       PIC X      VALUE SPACE.               
000877   03  L1-OMR-CDC-4                 PIC X(3)   VALUE SPACE.               
000878   03  FILLER                       PIC X      VALUE SPACE.               
000879   03  L1-QTY-CDC-4                 PIC Z(7)   VALUE ZERO.                
000880   03  FILLER                       PIC X(2)   VALUE SPACE.               
000881   03  FILLER                       PIC X(17)  VALUE ALL '.'.             
000882   03  FILLER                       PIC X(43)  VALUE SPACE.               
000883   03  L1-IDDISTR-CDC-11            PIC Z(5)   VALUE ZERO.                
000884   03  FILLER                       PIC X(4)   VALUE SPACE.               
000885   03  L1-IDKUNDNR-CDC-11           PIC Z(5)   VALUE ZERO.                
000886   03  FILLER                       PIC X(4)   VALUE SPACE.               
000887   03  L1-IDKUNDRF-CDC-11           PIC Z(5)   VALUE ZERO.                
000888   03  FILLER                       PIC X(2)   VALUE SPACE.               
000889   03  L1-KVLEVART-CDC-11           PIC Z(7)   VALUE ZERO.                
000890   03  FILLER                       PIC X(13)  VALUE SPACE.               
000891 01  L1-RAD-17-CDC.                                                       
000892   03  FILLER                       PIC X(50)  VALUE SPACE.               
000893   03  FILLER                       PIC X(6)   VALUE 'VÄRDE '.            
000894   03  FILLER                       PIC X(9)   VALUE SPACE.               
000895   03  FILLER                       PIC X(10)  VALUE '=.......  '.        
000896   03  FILLER                       PIC X(12)  VALUE SPACE.               
000897   03  L1-IDDISTR-CDC-12            PIC Z(5)   VALUE ZERO.                
000898   03  FILLER                       PIC X(4)   VALUE SPACE.               
000899   03  L1-IDKUNDNR-CDC-12           PIC Z(5)   VALUE ZERO.                
000900   03  FILLER                       PIC X(4)   VALUE SPACE.               
000901   03  L1-IDKUNDRF-CDC-12           PIC Z(5)   VALUE ZERO.                
000902   03  FILLER                       PIC X(2)   VALUE SPACE.               
000903   03  L1-KVLEVART-CDC-12           PIC Z(7)   VALUE ZERO.                
000904   03  FILLER                       PIC X(13)  VALUE SPACE.               
000905 01  L1-RAD-18-CDC.                                                       
000906   03  FILLER                       PIC X(18)  VALUE                      
000907                                            ' FLER BUFF (4108):'.         
000908   03  FILLER                       PIC X(3)   VALUE SPACE.               
000909   03  L1-FLBUFF-CDC                PIC X(3).                             
000910   03  FILLER                       PIC X(63)  VALUE SPACE.               
000911   03  L1-IDDISTR-CDC-13            PIC Z(5)   VALUE ZERO.                
000912   03  FILLER                       PIC X(4)   VALUE SPACE.               
000913   03  L1-IDKUNDNR-CDC-13           PIC Z(5)   VALUE ZERO.                
000914   03  FILLER                       PIC X(4)   VALUE SPACE.               
000915   03  L1-IDKUNDRF-CDC-13           PIC Z(5)   VALUE ZERO.                
000916   03  FILLER                       PIC X(2)   VALUE SPACE.               
000917   03  L1-KVLEVART-CDC-13           PIC Z(7)   VALUE ZERO.                
000918   03  FILLER                       PIC X(13)  VALUE SPACE.               
000919 01  L1-RAD-19-CDC.                                                       
000920   03  FILLER                       PIC X(87)  VALUE SPACE.               
000921   03  L1-IDDISTR-CDC-14            PIC Z(5)   VALUE ZERO.                
000922   03  FILLER                       PIC X(4)   VALUE SPACE.               
000923   03  L1-IDKUNDNR-CDC-14           PIC Z(5)   VALUE ZERO.                
000924   03  FILLER                       PIC X(4)   VALUE SPACE.               
000925   03  L1-IDKUNDRF-CDC-14           PIC Z(5)   VALUE ZERO.                
000926   03  FILLER                       PIC X(2)   VALUE SPACE.               
000927   03  L1-KVLEVART-CDC-14           PIC Z(7)   VALUE ZERO.                
000928   03  FILLER                       PIC X(13)  VALUE SPACE.               
000929 01  L1-RAD-20-CDC.                                                       
000930   03  FILLER                       PIC X(18)  VALUE                      
000931                                             ' TOT. BUFF.SALDO  '.        
000932   03  L1-TOT-QTY-CDC               PIC Z(6)9  VALUE ZERO.                
000933   03  FILLER                       PIC X(62)  VALUE SPACE.               
000934   03  L1-IDDISTR-CDC-15            PIC Z(5)   VALUE ZERO.                
000935   03  FILLER                       PIC X(4)   VALUE SPACE.               
000936   03  L1-IDKUNDNR-CDC-15           PIC Z(5)   VALUE ZERO.                
000937   03  FILLER                       PIC X(4)   VALUE SPACE.               
000938   03  L1-IDKUNDRF-CDC-15           PIC Z(5)   VALUE ZERO.                
000939   03  FILLER                       PIC X(2)   VALUE SPACE.               
000940   03  L1-KVLEVART-CDC-15           PIC Z(7)   VALUE ZERO.                
000941   03  FILLER                       PIC X(13)  VALUE SPACE.               
000942 01  L1-RAD-21-CDC.                                                       
000943   03  FILLER                       PIC X(50)  VALUE SPACE.               
000944   03  FILLER                       PIC X(9)   VALUE 'JUST.ANT:'.         
000945   03  FILLER                       PIC X(14)  VALUE ALL '.'.             
000946   03  FILLER                       PIC X(14)  VALUE SPACE.               
000947   03  L1-IDDISTR-CDC-16            PIC Z(5)   VALUE ZERO.                
000948   03  FILLER                       PIC X(4)   VALUE SPACE.               
000949   03  L1-IDKUNDNR-CDC-16           PIC Z(5)   VALUE ZERO.                
000950   03  FILLER                       PIC X(4)   VALUE SPACE.               
000951   03  L1-IDKUNDRF-CDC-16           PIC Z(5)   VALUE ZERO.                
000952   03  FILLER                       PIC X(2)   VALUE SPACE.               
000953   03  L1-KVLEVART-CDC-16           PIC Z(7)   VALUE ZERO.                
000954   03  FILLER                       PIC X(13)  VALUE SPACE.               
000955 01  L1-RAD-22-CDC.                                                       
000956   03  FILLER                       PIC X(20)  VALUE                      
000957                                       ' TOT. FYS.ANTAL     '.            
000958   03  FILLER                       PIC X(24)  VALUE ALL '.'.             
000959   03  FILLER                       PIC X(43)  VALUE SPACE.               
000960   03  L1-IDDISTR-CDC-17            PIC Z(5)   VALUE ZERO.                
000961   03  FILLER                       PIC X(4)   VALUE SPACE.               
000962   03  L1-IDKUNDNR-CDC-17           PIC Z(5)   VALUE ZERO.                
000963   03  FILLER                       PIC X(4)   VALUE SPACE.               
000964   03  L1-IDKUNDRF-CDC-17           PIC Z(5)   VALUE ZERO.                
000965   03  FILLER                       PIC X(2)   VALUE SPACE.               
000966   03  L1-KVLEVART-CDC-17           PIC Z(7)   VALUE ZERO.                
000967   03  FILLER                       PIC X(13)  VALUE SPACE.               
000968 01  L1-RAD-23-CDC.                                                       
000969   03  FILLER                       PIC X(50)  VALUE SPACE.               
000970   03  FILLER                       PIC X(9)   VALUE 'VÄRDE   :'.         
000971   03  FILLER                       PIC X(14)  VALUE ALL '.'.             
000972   03  FILLER                       PIC X(14)  VALUE SPACE.               
000973   03  L1-IDDISTR-CDC-18            PIC Z(5)   VALUE ZERO.                
000974   03  FILLER                       PIC X(4)   VALUE SPACE.               
000975   03  L1-IDKUNDNR-CDC-18           PIC Z(5)   VALUE ZERO.                
000976   03  FILLER                       PIC X(4)   VALUE SPACE.               
000977   03  L1-IDKUNDRF-CDC-18           PIC Z(5)   VALUE ZERO.                
000978   03  FILLER                       PIC X(2)   VALUE SPACE.               
000979   03  L1-KVLEVART-CDC-18           PIC Z(7)   VALUE ZERO.                
000980   03  FILLER                       PIC X(13)  VALUE SPACE.               
000981 01  L1-RAD-24-CDC.                                                       
000982   03  FILLER                       PIC X(10)  VALUE ' DIFF.(LS-'.        
000983   03  FILLER                       PIC X(10)  VALUE 'FYS.ANT) ='.        
000984   03  FILLER                       PIC X(8)   VALUE ALL '.'.             
000985   03  FILLER                       PIC X(7)   VALUE ' /SUM: '.           
000986   03  FILLER                       PIC X(9)   VALUE ALL '.'.             
000987   03  FILLER                       PIC X(43)  VALUE SPACE.               
000988   03  L1-IDDISTR-CDC-19            PIC Z(5)   VALUE ZERO.                
000989   03  FILLER                       PIC X(4)   VALUE SPACE.               
000990   03  L1-IDKUNDNR-CDC-19           PIC Z(5)   VALUE ZERO.                
000991   03  FILLER                       PIC X(4)   VALUE SPACE.               
000992   03  L1-IDKUNDRF-CDC-19           PIC Z(5)   VALUE ZERO.                
000993   03  FILLER                       PIC X(2)   VALUE SPACE.               
000994   03  L1-KVLEVART-CDC-19           PIC Z(7)   VALUE ZERO.                
000995   03  FILLER                       PIC X(13)  VALUE SPACE.               
000996 01  L1-RAD-25-CDC.                                                       
000997   03  FILLER                       PIC X(50)  VALUE SPACE.               
000998   03  FILLER                       PIC X(9)   VALUE 'ATTEST  :'.         
000999   03  FILLER                       PIC X(14)  VALUE                      
001000                                              '..............'.           
001001   03  FILLER                       PIC X(14)  VALUE SPACE.               
001002   03  L1-IDDISTR-CDC-20            PIC Z(5)   VALUE ZERO.                
001003   03  FILLER                       PIC X(4)   VALUE SPACE.               
001004   03  L1-IDKUNDNR-CDC-20           PIC Z(5)   VALUE ZERO.                
001005   03  FILLER                       PIC X(4)   VALUE SPACE.               
001006   03  L1-IDKUNDRF-CDC-20           PIC Z(5)   VALUE ZERO.                
001007   03  FILLER                       PIC X(2)   VALUE SPACE.               
001008   03  L1-KVLEVART-CDC-20           PIC Z(7)   VALUE ZERO.                
001009   03  FILLER                       PIC X(13)  VALUE SPACE.               
001010 01  L1-RAD-26-CDC.                                                       
001011   03  FILLER                       PIC X(10)  VALUE ' RÄKNAT AV'.        
001012   03  FILLER                       PIC X(10)  VALUE ' / DATUM  '.        
001013   03  FILLER                       PIC X(24)  VALUE ALL '.'.             
001014   03  FILLER                       PIC X(48)  VALUE SPACE.               
001015   03  FILLER                       PIC X(10)  VALUE 'FLER EFR: '.        
001016   03  L1-FLEFR-CDC                 PIC X(3)   VALUE SPACE.               
001017   03  FILLER                       PIC X(27)  VALUE SPACE.               
001018 01  L1-RAD-27-CDC.                                                       
001019   03  FILLER                       PIC X(88)  VALUE SPACE.               
001020   03  FILLER                       PIC X(12)  VALUE 'TOTAL EFR'.         
001021   03  FILLER                       PIC X(8)   VALUE SPACE.               
001022   03  L1-TOTQTY-EFR-CDC            PIC Z(10)9 VALUE ZERO.                
001023   03  FILLER                       PIC X(13)  VALUE SPACE.               
001024 01  L1-RAD-28-CDC.                                                       
001025   03  FILLER                       PIC X(132) VALUE SPACE.               
001026 01  L1-RAD-29-CDC.                                                       
001027   03  FILLER                       PIC X(96)  VALUE SPACE.               
001028   03  FILLER                       PIC X(9)   VALUE 'SATSORDER'.         
001029   03  FILLER                       PIC X(27)  VALUE SPACE.               
001030 01  L1-RAD-30-CDC.                                                       
001031   03  FILLER                       PIC X(12)  VALUE ' KOMMENTAR'.        
001032   03  L1-TEINVANM-CDC              PIC X(32)  VALUE SPACE.               
001033   03  FILLER                       PIC X(6)   VALUE SPACE.               
001034   03  FILLER                       PIC X(6)   VALUE 'SORT  '.            
001035   03  FILLER                       PIC X(15)  VALUE SPACE.               
001036   03  L1-KDSORT-CDC                PIC X(2)   VALUE SPACE.               
001037   03  FILLER                       PIC X(14)  VALUE SPACE.               
001038   03  FILLER                       PIC X(27)  VALUE                      
001039                                    'DISTR   SATSNR  ORDERNR    '.        
001040   03  FILLER                       PIC X(5)   VALUE 'ANTAL'.             
001041   03  FILLER                       PIC X(13)  VALUE SPACE.               
001042 01  L1-RAD-31-CDC.                                                       
001043   03  FILLER                       PIC X(49)  VALUE SPACE.               
001044   03  FILLER                       PIC X(9)   VALUE ' KVANT Q1'.         
001045   03  FILLER                       PIC X(8)   VALUE ' (4108) '.          
001046   03  L1-KVQPACK-1-CDC             PIC Z(6)9  VALUE ZERO.                
001047   03  FILLER                       PIC X(14)  VALUE SPACE.               
001048   03  L1-IDDISTR-CDC-SO-1          PIC Z(5).                             
001049   03  L1-SATSNR-CDC-SO-1           PIC Z(9)   VALUE ZERO.                
001050   03  FILLER                       PIC X(4)   VALUE SPACE.               
001051   03  L1-IDORDNR-CDC-SO-1          PIC Z(5)   VALUE ZERO.                
001052   03  FILLER                       PIC X(2)   VALUE SPACE.               
001053   03  L1-KVLEVART-CDC-SO-1         PIC Z(7)   VALUE ZERO.                
001054   03  FILLER                       PIC X(13)  VALUE SPACE.               
001055 01  L1-RAD-32-CDC.                                                       
001056   03  FILLER                       PIC X(49)  VALUE SPACE.               
001057   03  FILLER                       PIC X(10)  VALUE ' VIKT (G) '.        
001058   03  FILLER                       PIC X(7)   VALUE SPACE.               
001059   03  L1-VKART-CDC                 PIC Z(7)   VALUE ZERO.                
001060   03  FILLER                       PIC X(14)  VALUE SPACE.               
001061   03  L1-IDDISTR-CDC-SO-2          PIC Z(5).                             
001062   03  L1-SATSNR-CDC-SO-2           PIC Z(9)   VALUE ZERO.                
001063   03  FILLER                       PIC X(4)   VALUE SPACE.               
001064   03  L1-IDORDNR-CDC-SO-2          PIC Z(5)   VALUE ZERO.                
001065   03  FILLER                       PIC X(2)   VALUE SPACE.               
001066   03  L1-KVLEVART-CDC-SO-2         PIC Z(7)   VALUE ZERO.                
001067   03  FILLER                       PIC X(13)  VALUE SPACE.               
001068 01  L1-RAD-33-CDC.                                                       
001069   03  FILLER                       PIC X(26)  VALUE                      
001070                                    ' SENASTE JUSTERINGAR(5105)'.         
001071   03  FILLER                       PIC X(6)   VALUE SPACE.               
001072   03  L1-DUBBLETT-CDC              PIC X(8)   VALUE SPACE.               
001073   03  FILLER                       PIC X(47)  VALUE SPACE.               
001074   03  L1-IDDISTR-CDC-SO-3          PIC Z(5).                             
001075   03  L1-SATSNR-CDC-SO-3           PIC Z(9)   VALUE ZERO.                
001076   03  FILLER                       PIC X(4)   VALUE SPACE.               
001077   03  L1-IDORDNR-CDC-SO-3          PIC Z(5)   VALUE ZERO.                
001078   03  FILLER                       PIC X(2)   VALUE SPACE.               
001079   03  L1-KVLEVART-CDC-SO-3         PIC Z(7)   VALUE ZERO.                
001080   03  FILLER                       PIC X(13)  VALUE SPACE.               
001081 01  L1-RAD-34-CDC.                                                       
001082   03  FILLER                       PIC X(27)  VALUE                      
001083                                   '  DATUM     ANTAL    TYP   '.         
001084   03  FILLER                       PIC X(22)  VALUE SPACE.               
001085   03  FILLER                       PIC X(14)  VALUE                      
001086                                              ' STD.PRIS     '.           
001087   03  L1-PRARTSTD-CDC              PIC Z(7).99 VALUE ZERO.               
001088   03  FILLER                       PIC X(14)  VALUE SPACE.               
001089   03  L1-IDDISTR-CDC-SO-4          PIC Z(5).                             
001090   03  L1-SATSNR-CDC-SO-4           PIC Z(9)   VALUE ZERO.                
001091   03  FILLER                       PIC X(4)   VALUE SPACE.               
001092   03  L1-IDORDNR-CDC-SO-4          PIC Z(5)   VALUE ZERO.                
001093   03  FILLER                       PIC X(2)   VALUE SPACE.               
001094   03  L1-KVLEVART-CDC-SO-4         PIC Z(7)   VALUE ZERO.                
001095   03  FILLER                       PIC X(13)  VALUE SPACE.               
001096 01  L1-RAD-35-CDC.                                                       
001097   03  FILLER                       PIC X(2)   VALUE SPACE.               
001098   03  L1-TIJUSTDA-1-CDC            PIC 9(5).                             
001099   03  FILLER                       PIC X(2)   VALUE SPACE.               
001100   03  L1-KVJUSTKV-1-CDC            PIC -(8).                             
001101   03  FILLER                       PIC X(5)   VALUE SPACE.               
001102   03  L1-KDJUSTYP-1-CDC            PIC Z.                                
001103   03  L1-KDJUSTYP-1A-CDC           PIC X.                                
001104   03  FILLER                       PIC X(25)  VALUE SPACE.               
001105   03  FILLER                       PIC X(21)  VALUE                      
001106                                              ' FÖRP.TYP(4108)'.          
001107   03  L1-BEFT-CDC                  PIC Z(2)9  VALUE ZERO.                
001108   03  FILLER                       PIC X(14)  VALUE SPACE.               
001109   03  L1-IDDISTR-CDC-SO-5          PIC Z(5).                             
001110   03  L1-SATSNR-CDC-SO-5           PIC Z(9)   VALUE ZERO.                
001111   03  FILLER                       PIC X(4)   VALUE SPACE.               
001112   03  L1-IDORDNR-CDC-SO-5          PIC Z(5)   VALUE ZERO.                
001113   03  FILLER                       PIC X(2)   VALUE SPACE.               
001114   03  L1-KVLEVART-CDC-SO-5         PIC Z(7)   VALUE ZERO.                
001115   03  FILLER                       PIC X(13)  VALUE SPACE.               
001116 01  L1-RAD-36-CDC.                                                       
001117   03  FILLER                       PIC X(2)   VALUE SPACE.               
001118   03  L1-TIJUSTDA-2-CDC            PIC 9(5).                             
001119   03  FILLER                       PIC X(2)   VALUE SPACE.               
001120   03  L1-KVJUSTKV-2-CDC            PIC -(8).                             
001121   03  FILLER                       PIC X(5)   VALUE SPACE.               
001122   03  L1-KDJUSTYP-2-CDC            PIC Z.                                
001123   03  L1-KDJUSTYP-2A-CDC           PIC X.                                
001124   03  FILLER                       PIC X(63)  VALUE SPACE.               
001125   03  L1-IDDISTR-CDC-SO-6          PIC Z(5).                             
001126   03  L1-SATSNR-CDC-SO-6           PIC Z(9)   VALUE ZERO.                
001127   03  FILLER                       PIC X(4)   VALUE SPACE.               
001128   03  L1-IDORDNR-CDC-SO-6          PIC Z(5)   VALUE ZERO.                
001129   03  FILLER                       PIC X(2)   VALUE SPACE.               
001130   03  L1-KVLEVART-CDC-SO-6         PIC Z(7)   VALUE ZERO.                
001131   03  FILLER                       PIC X(13)  VALUE SPACE.               
001132 01  L1-RAD-37-CDC.                                                       
001133   03  FILLER                       PIC X(2)   VALUE SPACE.               
001134   03  L1-TIJUSTDA-3-CDC            PIC 9(5).                             
001135   03  FILLER                       PIC X(2)   VALUE SPACE.               
001136   03  L1-KVJUSTKV-3-CDC            PIC -(8).                             
001137   03  FILLER                       PIC X(5)   VALUE SPACE.               
001138   03  L1-KDJUSTYP-3-CDC            PIC Z.                                
001139   03  L1-KDJUSTYP-3A-CDC           PIC X.                                
001140   03  FILLER                       PIC X(26)  VALUE SPACE.               
001141   03  FILLER                       PIC X(19)  VALUE                      
001142                                    'GAM.PL.(6319)(6165)'.                
001143   03  FILLER                       PIC X(36)  VALUE SPACE.               
001144   03  FILLER                       PIC X(7)   VALUE 'TOTALT '.           
001145   03  L1-KVLEVART-CDC-SO-TOT       PIC Z(7)   VALUE ZERO.                
001146   03  FILLER                       PIC X(17)  VALUE SPACE.               
001147 01  L1-RAD-38-CDC.                                                       
001148   03  FILLER                       PIC X(50)  VALUE SPACE.               
001149   03  FILLER                       PIC X(6)   VALUE 'LEV.NR'.            
001150   03  FILLER                       PIC X(12)  VALUE SPACE.               
001151   03  L1-IDLEVNR-CDC               PIC X(5).                             
001152   03  FILLER                       PIC X(59)  VALUE SPACE.               
001153     EJECT                                                                
001154*************************************************                         
001155*****    LISTTRANSAR FÖR SDC-STOCKTAKING   ******                         
001156*************************************************                         
001157 01  L1-RUB-SDC.                                                          
001158     03  FILLER                     PIC X(19)  VALUE                      
001159                                          ' VOLVO CAR PARTS   '.          
001160     03  FILLER                     PIC X(15)  VALUE                      
001161                                                 'W50302-001'.            
001162     03  FILLER                     PIC X(13)  VALUE                      
001163                                                 'STOCKTAKING '.          
001164     03  FILLER                     PIC X(12)  VALUE                      
001165                                                 'DOCUMENT DC '.          
001166     03  RUB-IDDC-SDC               PIC X(2)   VALUE SPACE.               
001167     03  FILLER                     PIC X(6)   VALUE '  CAT '.            
001168     03  RUB-KDINVKAT-SDC           PIC 9(2)   VALUE ZERO.                
001169     03  FILLER                     PIC X(9)   VALUE '  ISSUE '.          
001170     03  RUB-TIREGDAT-SDC           PIC 9(6)   VALUE ZERO.                
001171     03  FILLER                     PIC X(9)   VALUE '   PRINT'.          
001172     03  RUB-DATUM-SDC              PIC 9(6)   VALUE ZERO.                
001173     03  FILLER                     PIC X      VALUE '-'.                 
001174     03  RUB-TID-SDC                PIC 9(4)   VALUE ZERO.                
001175     03  FILLER                     PIC X(3)   VALUE SPACE.               
001176     03  RUB-IDARTNR-SDC            PIC Z(9)   VALUE ZERO.                
001177     03  FILLER                     PIC X(16)  VALUE SPACE.               
001178 01  L1-LISTNR-RAD-SDC.                                                   
001179     03  FILLER                     PIC X(102) VALUE SPACE.               
001180     03  FILLER                     PIC X(8)   VALUE                      
001181                'LISTNR: '.                                               
001182     03 RUB-PRINTNR-SDC             PIC Z(6).                             
001183     03 FILLER                      PIC X(15)  VALUE SPACE.               
001184 01  L1-RAD-1-SDC.                                                        
001185     03  FILLER                     PIC X(12)  VALUE                      
001186               ' PARTNO.'.                                                
001187     03  L1-IDARTNR-SDC             PIC Z(9)   VALUE ZERO.                
001188     03  FILLER                     PIC X(5)   VALUE SPACE.               
001189     03  FILLER                     PIC X(6)   VALUE 'PCODE '.            
001190     03  L1-KDPRODSL-SDC            PIC Z(2)   VALUE ZERO.                
001191     03  FILLER                     PIC X(11)  VALUE SPACE.               
001192     03  FILLER                     PIC X(20)  VALUE                      
001193                'INVESTIG BALANCE'.                                       
001194     03  L1-KVUTRS-SDC              PIC Z(7)-  VALUE ZERO.                
001195     03  FILLER                     PIC X(15)  VALUE SPACE.               
001196     03  FILLER                     PIC X(29)  VALUE                      
001197               'UNPACKED EFR           '.                                 
001198     03  FILLER                     PIC X(16)  VALUE SPACE.               
001199 01  L1-RAD-2-SDC.                                                        
001200     03  FILLER                     PIC X(14) VALUE                       
001201               ' DESCRIPTION:'.                                           
001202     03  L1-BEART-SDC               PIC X(15) VALUE SPACE.                
001203     03  FILLER                     PIC X(5)  VALUE SPACE.                
001204     03  FILLER                     PIC X(11) VALUE 'ADRESS'.             
001205     03  FILLER                     PIC X(20) VALUE                       
001206              'AKS '.                                                     
001207     03  L1-KVAKS-SDC               PIC Z(7)- VALUE ZERO.                 
001208     03  FILLER                     PIC X(7)  VALUE SPACE.                
001209     03  FILLER                     PIC X(5)  VALUE 'DISTR'.              
001210     03  FILLER                     PIC X(2)  VALUE SPACE.                
001211     03  FILLER                     PIC X(8)  VALUE 'CUSTOMER'.           
001212     03  FILLER                     PIC X(3)  VALUE SPACE.                
001213     03  FILLER                     PIC X(10) VALUE 'ORDERNO'.            
001214     03  FILLER                     PIC X(3)  VALUE SPACE.                
001215     03  FILLER                     PIC X(5)  VALUE 'QUANT'.              
001216     03  FILLER                     PIC X(18) VALUE SPACE.                
001217 01  L1-RAD-3-SDC.                                                        
001218     03  FILLER                     PIC X(12)  VALUE                      
001219               ' PLACE   '.                                               
001220     03  FILLER                     PIC X(7)   VALUE '.......'.           
001221     03  FILLER                     PIC X(11)  VALUE SPACE.               
001222     03  L1-ADLAGOMR-SDC            PIC Z(3)   VALUE ZERO.                
001223     03  FILLER                     PIC X      VALUE SPACE.               
001224     03  L1-ADGANG-SDC              PIC Z(3)   VALUE ZERO.                
001225     03  FILLER                     PIC X      VALUE SPACE.               
001226     03  L1-ADPLATS-SDC             PIC Z(5)   VALUE ZERO.                
001227     03  FILLER                     PIC X(2)   VALUE SPACE.               
001228     03  FILLER                     PIC X(20)  VALUE                      
001229               'STOCK BALANCE'.                                           
001230     03  L1-KVLS-SDC                PIC Z(7)-  VALUE ZERO.                
001231     03  FILLER                     PIC X(7)   VALUE SPACE.               
001232     03  L1-IDDISTR-SDC-1           PIC Z(5)   VALUE ZERO.                
001233     03  FILLER                     PIC X(4)   VALUE SPACE.               
001234     03  L1-IDKUNDNR-SDC-1          PIC Z(5)   VALUE ZERO.                
001235     03  FILLER                     PIC X(5)   VALUE SPACE.               
001236     03  L1-IDKUNDRF-SDC-1          PIC Z(5)   VALUE ZERO.                
001237     03  FILLER                     PIC X(6)   VALUE SPACE.               
001238     03  L1-KVLEVART-SDC-1          PIC Z(7)   VALUE ZERO.                
001239     03  FILLER                     PIC X(16)  VALUE SPACE.               
001240 01  L1-RAD-4-SDC.                                                        
001241     03  FILLER                     PIC X(12)  VALUE                      
001242               ' BUFFER 1'.                                               
001243     03  FILLER                     PIC X(7)   VALUE '.......'.           
001244     03  FILLER                     PIC X(2)   VALUE SPACE.               
001245     03  L1-ADBUFFOMR-SDC-1         PIC Z(3)   VALUE ZERO.                
001246     03  FILLER                     PIC X      VALUE SPACE.               
001247     03  L1-ADBUFFGANG-SDC-1        PIC Z(3)   VALUE ZERO.                
001248     03  FILLER                     PIC X      VALUE SPACE.               
001249     03  L1-ADBUFFPLATS-SDC-1       PIC Z(5)   VALUE ZERO.                
001250     03  FILLER                     PIC X(11)  VALUE SPACE.               
001251     03  FILLER                     PIC X(33)  VALUE                      
001252               'EFR NOT PICKED  +'.                                       
001253     03  FILLER                     PIC X(2)   VALUE SPACE.               
001254     03  L1-IDDISTR-SDC-2           PIC Z(5)   VALUE ZERO.                
001255     03  FILLER                     PIC X(4)   VALUE SPACE.               
001256     03  L1-IDKUNDNR-SDC-2          PIC Z(5)   VALUE ZERO.                
001257     03  FILLER                     PIC X(5)   VALUE SPACE.               
001258     03  L1-IDKUNDRF-SDC-2          PIC Z(5)   VALUE ZERO.                
001259     03  FILLER                     PIC X(6)   VALUE SPACE.               
001260     03  L1-KVLEVART-SDC-2          PIC Z(7)   VALUE ZERO.                
001261     03  FILLER                     PIC X(16)  VALUE SPACE.               
001262 01  L1-RAD-5-SDC.                                                        
001263     03  FILLER                     PIC X(12)  VALUE                      
001264               ' BUFFER 2'.                                               
001265     03  FILLER                     PIC X(7)   VALUE '.......'.           
001266     03  FILLER                     PIC X(2)   VALUE SPACE.               
001267     03  L1-ADBUFFOMR-SDC-2         PIC Z(3)   VALUE ZERO.                
001268     03  FILLER                     PIC X      VALUE SPACE.               
001269     03  L1-ADBUFFGANG-SDC-2        PIC Z(3)   VALUE ZERO.                
001270     03  FILLER                     PIC X      VALUE SPACE.               
001271     03  L1-ADBUFFPLATS-SDC-2       PIC Z(5)   VALUE ZERO.                
001272     03  FILLER                     PIC X(11)  VALUE SPACE.               
001273     03  FILLER                     PIC X(33)  VALUE                      
001274               'PHYSICAL STOCK  -'.                                       
001275     03  FILLER                     PIC X(2)   VALUE SPACE.               
001276     03  L1-IDDISTR-SDC-3           PIC Z(5)   VALUE ZERO.                
001277     03  FILLER                     PIC X(4)   VALUE SPACE.               
001278     03  L1-IDKUNDNR-SDC-3          PIC Z(5)   VALUE ZERO.                
001279     03  FILLER                     PIC X(5)   VALUE SPACE.               
001280     03  L1-IDKUNDRF-SDC-3          PIC Z(5)   VALUE ZERO.                
001281     03  FILLER                     PIC X(6)   VALUE SPACE.               
001282     03  L1-KVLEVART-SDC-3          PIC Z(7)   VALUE ZERO.                
001283     03  FILLER                     PIC X(16)  VALUE SPACE.               
001284 01  L1-RAD-6-SDC.                                                        
001285     03  FILLER                     PIC X(12)  VALUE                      
001286               ' BUFFER 3'.                                               
001287     03  FILLER                     PIC X(7)   VALUE '.......'.           
001288     03  FILLER                     PIC X(2)   VALUE SPACE.               
001289     03  L1-ADBUFFOMR-SDC-3         PIC Z(3)   VALUE ZERO.                
001290     03  FILLER                     PIC X      VALUE SPACE.               
001291     03  L1-ADBUFFGANG-SDC-3        PIC Z(3)   VALUE ZERO.                
001292     03  FILLER                     PIC X      VALUE SPACE.               
001293     03  L1-ADBUFFPLATS-SDC-3       PIC Z(5)   VALUE ZERO.                
001294     03  FILLER                     PIC X(46)  VALUE SPACE.               
001295     03  L1-IDDISTR-SDC-4           PIC Z(5)   VALUE ZERO.                
001296     03  FILLER                     PIC X(4)   VALUE SPACE.               
001297     03  L1-IDKUNDNR-SDC-4          PIC Z(5)   VALUE ZERO.                
001298     03  FILLER                     PIC X(5)   VALUE SPACE.               
001299     03  L1-IDKUNDRF-SDC-4          PIC Z(5)   VALUE ZERO.                
001300     03  FILLER                     PIC X(6)   VALUE SPACE.               
001301     03  L1-KVLEVART-SDC-4          PIC Z(7)   VALUE ZERO.                
001302     03  FILLER                     PIC X(16)  VALUE SPACE.               
001303 01  L1-RAD-7-SDC.                                                        
001304     03  FILLER                     PIC X(12)  VALUE                      
001305               ' BUFFER 4'.                                               
001306     03  FILLER                     PIC X(7)   VALUE '.......'.           
001307     03  FILLER                     PIC X(2)   VALUE SPACE.               
001308     03  L1-ADBUFFOMR-SDC-4         PIC Z(3)   VALUE ZERO.                
001309     03  FILLER                     PIC X      VALUE SPACE.               
001310     03  L1-ADBUFFGANG-SDC-4        PIC Z(3)   VALUE ZERO.                
001311     03  FILLER                     PIC X      VALUE SPACE.               
001312     03  L1-ADBUFFPLATS-SDC-4       PIC Z(5)   VALUE ZERO.                
001313     03  FILLER                     PIC X(11)  VALUE SPACE.               
001314     03  FILLER                     PIC X(33)  VALUE                      
001315                                            'DIFFERENCE      ='.          
001316     03  FILLER                     PIC X(2)   VALUE SPACE.               
001317     03  L1-IDDISTR-SDC-5           PIC Z(5)   VALUE ZERO.                
001318     03  FILLER                     PIC X(4)   VALUE SPACE.               
001319     03  L1-IDKUNDNR-SDC-5          PIC Z(5)   VALUE ZERO.                
001320     03  FILLER                     PIC X(5)   VALUE SPACE.               
001321     03  L1-IDKUNDRF-SDC-5          PIC Z(5)   VALUE ZERO.                
001322     03  FILLER                     PIC X(6)   VALUE SPACE.               
001323     03  L1-KVLEVART-SDC-5          PIC Z(7)   VALUE ZERO.                
001324     03  FILLER                     PIC X(16)  VALUE SPACE.               
001325 01  L1-RAD-8-SDC.                                                        
001326     03  FILLER                     PIC X(12)  VALUE                      
001327                                                 ' TOTAL QTY  '.          
001328     03  FILLER                     PIC X(7)   VALUE '.......'.           
001329     03  FILLER                     PIC X(26)  VALUE SPACE.               
001330     03  FILLER                     PIC X(33)  VALUE                      
001331                                                 'SUM SEK'.               
001332     03  FILLER                     PIC X(2)   VALUE SPACE.               
001333     03  L1-IDDISTR-SDC-6           PIC Z(5)   VALUE ZERO.                
001334     03  FILLER                     PIC X(4)   VALUE SPACE.               
001335     03  L1-IDKUNDNR-SDC-6          PIC Z(5)   VALUE ZERO.                
001336     03  FILLER                     PIC X(5)   VALUE SPACE.               
001337     03  L1-IDKUNDRF-SDC-6          PIC Z(5)   VALUE ZERO.                
001338     03  FILLER                     PIC X(6)   VALUE SPACE.               
001339     03  L1-KVLEVART-SDC-6          PIC Z(7)   VALUE ZERO.                
001340     03  FILLER                     PIC X(16)  VALUE SPACE.               
001341                                                                          
001342 01  L1-RAD-9-SDC.                                                        
001343     03  FILLER                     PIC X(24)  VALUE                      
001344                                                 ' SORT '.                
001345     03  L1-KDSORT-SDC              PIC X(2)   VALUE SPACE.               
001346     03  FILLER                     PIC X(19)  VALUE SPACE.               
001347     03  FILLER                     PIC X(33)  VALUE                      
001348                                                 'ATTEST '.               
001349     03  FILLER                     PIC X(2)   VALUE SPACE.               
001350     03  L1-IDDISTR-SDC-7           PIC Z(5)   VALUE ZERO.                
001351     03  FILLER                     PIC X(4)   VALUE SPACE.               
001352     03  L1-IDKUNDNR-SDC-7          PIC Z(5)   VALUE ZERO.                
001353     03  FILLER                     PIC X(5)   VALUE SPACE.               
001354     03  L1-IDKUNDRF-SDC-7          PIC Z(5)   VALUE ZERO.                
001355     03  FILLER                     PIC X(6)   VALUE SPACE.               
001356     03  L1-KVLEVART-SDC-7          PIC Z(7)   VALUE ZERO.                
001357     03  FILLER                     PIC X(16)  VALUE SPACE.               
001358                                                                          
001359 01  L1-RAD-10-SDC.                                                       
001360     03  FILLER                     PIC X(19)  VALUE ' WEIGHT'.           
001361     03  L1-VKART-SDC               PIC Z(7)   VALUE ZERO.                
001362     03  FILLER                     PIC X(106) VALUE SPACE.               
001363                                                                          
001364 01  L1-RAD-11-SDC.                                                       
001365     03  FILLER                     PIC X(19)  VALUE                      
001366               ' STD PRICE '.                                             
001367     03  L1-PRARTSTD-SDC            PIC Z(7).99 VALUE ZERO.               
001368     03  FILLER                     PIC X(16)  VALUE SPACE.               
001369     03  FILLER                     PIC X(71)  VALUE                      
001370                                             '----------------'.          
001371     03  FILLER                     PIC X(16)  VALUE SPACE.               
001372                                                                          
001373 01  L1-RAD-12-SDC.                                                       
001374     03  FILLER                     PIC X(26)  VALUE                      
001375                                       ' LATEST ADJUSTMENTS '.            
001376     03  FILLER                     PIC X(19)  VALUE SPACE.               
001377     03  FILLER                     PIC X(71)  VALUE                      
001378                                             'ADJ. TRANSACTION '.         
001379     03  FILLER                     PIC X(16)  VALUE SPACE.               
001380                                                                          
001381 01  L1-RAD-13-SDC.                                                       
001382     03  FILLER                     PIC X(10)  VALUE '  DATE'.            
001383     03  FILLER                     PIC X(8)   VALUE '    QTY'.           
001384     03  FILLER                     PIC X(10)  VALUE '  TYPE'.            
001385     03  FILLER                     PIC X(21)  VALUE SPACE.               
001386     03  FILLER                     PIC X(48)  VALUE                      
001387               'TT   WH    PARTNO    QTY         U/D ONLY QTY   '.        
001388     03  FILLER                     PIC X(12)  VALUE                      
001389                                                   'DELETE  MOVE'.        
001390     03  FILLER                     PIC X(23)  VALUE SPACE.               
001391                                                                          
001392 01  L1-RAD-14-SDC.                                                       
001393     03  FILLER                     PIC X(2)   VALUE SPACE.               
001394     03  L1-TIJUSTDA-SDC            PIC 9(5)   VALUE ZERO.                
001395     03  FILLER                     PIC X(2)   VALUE SPACE.               
001396     03  L1-KVJUSTKV-SDC            PIC -(8)   VALUE ZERO.                
001397     03  FILLER                     PIC X(5)   VALUE SPACE.               
001398     03  L1-KDJUSTYP-SDC            PIC Z      VALUE ZERO.                
001399     03  L1-KDJUSTYP-A-SDC          PIC X      VALUE ZERO.                
001400     03  FILLER                     PIC X(20)  VALUE SPACE.               
001401     03  FILLER                     PIC X(24)  VALUE                      
001402               'POS 1-3   4      5-12'.                                   
001403     03  FILLER                     PIC X(47)  VALUE                      
001404               '  13-19       20        21      22     23'.               
001405     03  FILLER                     PIC X(16)  VALUE SPACE.               
001406                                                                          
001407 01  L1-RAD-15-SDC.                                                       
001408     03  FILLER                     PIC X(48)  VALUE SPACE.               
001409     03  FILLER                     PIC X(8)   VALUE                      
001410                                                 'R62   1'.               
001411     03  L1-IDARTNR-SDC-1           PIC Z(9)   VALUE ZERO.                
001412                                                                          
001413 01  L1-RAD-STRECK-SDC.                                                   
001414     03  FILLER                     PIC X(68)  VALUE SPACE.               
001415     03  FILLER                     PIC X(48)  VALUE                      
001416                 '-------        -         -       -      -'.             
001417     03  FILLER                     PIC X(16)  VALUE SPACE.               
001418                                                                          
001419 01  L1-RAD-17-SDC.                                                       
001420     03  FILLER                     PIC X(45)  VALUE SPACE.               
001421     03  L1-DUBBLETT-SDC            PIC X(9)   VALUE SPACE.               
001422     03  FILLER                     PIC X(78)  VALUE SPACE.               
001423                                                                          
001424 01  L1-RAD-18-SDC.                                                       
001425     03  FILLER                     PIC X(20)  VALUE SPACE.               
001426     03  L1-KVROS-SDC               PIC Z(7)-.                            
001427     03  FILLER                     PIC X(89).                            
001428     03  FILLER                     PIC X(16).                            
001429                                                                          
001430 01  L1-RAD-19-SDC.                                                       
001431     03  FILLER                     PIC X(14)  VALUE                      
001432                                          ' COMMENT .'.                   
001433     03  L1-TEINVANM-SDC            PIC X(25)  VALUE SPACE.               
001434     03  FILLER                     PIC X(94)  VALUE SPACE.               
001435                                                                          
001436*************** SIDA 2, SDC  BUFFERTRUBRIK SDC ********                   
001437 01  LIST-BUFFERT-SIDA2-SDC.                                              
001438   03  L-BUFFERT-RUB-SDC.                                                 
001439     05  FILLER                     PIC X(50)  VALUE                      
001440            ' BUFFER                                           '.         
001441     05  FILLER                     PIC X(50)  VALUE                      
001442            ' BUFFER                                           '.         
001443     05  FILLER                     PIC X(32)  VALUE SPACE.               
001444     EJECT                                                                
001445**************  SIDA 2, SDC BUFFERTRAD SDC ***********                    
001446 01  BU-RAD-SDC.                                                          
001447   03  FILLER OCCURS 2.                                                   
001448     05  FILLER                     PIC X(1)   VALUE SPACE.               
001449     05  BU-ADBUFFOMR-SDC           PIC Z(2)   VALUE ZERO.                
001450     05  FILLER                     PIC X(1)   VALUE SPACE.               
001451     05  BU-ADBUFFGANG-SDC          PIC Z(2)   VALUE ZERO.                
001452     05  FILLER                     PIC X(1)   VALUE SPACE.               
001453     05  BU-ADBUFFPL-SDC            PIC Z(5)   VALUE ZERO.                
001454     05  FILLER                     PIC X(7)   VALUE SPACE.               
001455     05  BU-PUNKT-SDC               PIC X(7)   VALUE SPACE.               
001456     05  FILLER                     PIC X(24)  VALUE SPACE.               
001457   03  FILLER                       PIC X(32)  VALUE SPACE.               
001458     EJECT                                                                
001459*************************************************                         
001460*****    LISTTRANSAR FÖR ADC-STOCKTAKING   ******                         
001461*************************************************                         
001462 01  ADC-RUBRAD.                                                          
001463   03  FILLER                       PIC X(19)  VALUE                      
001464                                        ' VCNA     LISTNO.  '.            
001465   03  FILLER                       PIC X(10)  VALUE 'W50302-001'.        
001466   03  LISTNR-ADC                   PIC X(2)   VALUE SPACE.               
001467   03  FILLER                       PIC X(28)  VALUE                      
001468                                 '   STOCK TAKING DOCUMENT DC '.          
001469   03  RUB-IDDC-ADC                 PIC X(2)   VALUE SPACE.               
001470   03  FILLER                       PIC X(5)   VALUE ' CAT '.             
001471   03  RUB-KDINVKAT-ADC             PIC 9(2)   VALUE ZERO.                
001472   03  FILLER                       PIC X(8)   VALUE '   DATE '.          
001473   03  RUB-TIREGDAT-ADC             PIC 9(6)   VALUE ZERO.                
001474   03  FILLER                       PIC X(9)   VALUE '   PRINT '.         
001475   03  RUB-DATUM-ADC                PIC 9(6)   VALUE ZERO.                
001476   03  FILLER                       PIC X(1)   VALUE '-'.                 
001477   03  RUB-TID-ADC                  PIC 9(4)   VALUE ZERO.                
001478   03  FILLER                       PIC X(30)  VALUE SPACE.               
001479     EJECT                                                                
001480 01  L1-LISTNR-RAD-ADC.                                                   
001481     03  FILLER                     PIC X(102) VALUE SPACE.               
001482     03  FILLER                     PIC X(8)   VALUE                      
001483                'LISTNR: '.                                               
001484     03 RUB-PRINTNR-ADC             PIC Z(6).                             
001485     03 FILLER                      PIC X(15)  VALUE SPACE.               
001486 01  L1-RAD-1-ADC.                                                        
001487   03  FILLER                       PIC X(12)  VALUE ' PARTNO.'.          
001488   03  L1-IDARTNR-ADC               PIC Z(9)   VALUE ZERO.                
001489   03  FILLER                       PIC X(5)   VALUE SPACE.               
001490   03  FILLER                       PIC X(3)   VALUE 'PG '.               
001491   03  L1-KDPRODSL-ADC              PIC Z(2)   VALUE ZERO.                
001492   03  FILLER                       PIC X(2)   VALUE SPACE.               
001493   03  FILLER                       PIC X(4)   VALUE 'LPC '.              
001494   03  L1-KDPSLLOC-ADC              PIC Z(2)   VALUE ZERO.                
001495   03  FILLER                       PIC X(2)   VALUE SPACE.               
001496   03  FILLER                       PIC X(21)  VALUE                      
001497                                        'INVESTIGATION BALANCE'.          
001498   03  L1-KVUTRS-ADC                PIC Z(6)9- VALUE ZERO.                
001499   03  FILLER                       PIC X(3)   VALUE SPACE.               
001500   03  FILLER                       PIC X(16)  VALUE                      
001501                                        'SUPRESSION CODE '.               
001502   03  L1-KDERS-ADC                 PIC Z99    VALUE ZERO.                
001503   03  FILLER                       PIC X(40)  VALUE SPACE.               
001504   EJECT                                                                  
001505 01  L1-RAD-3-ADC.                                                        
001506   03  FILLER                       PIC X(14)  VALUE                      
001507                                               ' DESCRIPTION:'.           
001508   03  L1-BEART-ADC                 PIC X(15)  VALUE SPACE.               
001509   03  FILLER                       PIC X(30)  VALUE SPACE.               
001510   03  FILLER                       PIC X(3)   VALUE ' AK'.               
001511   03  L1-KVAKS-ADC                 PIC Z(6)9- VALUE ZERO.                
001512   03  FILLER                       PIC X(62)  VALUE SPACE.               
001513   EJECT                                                                  
001514 01  L1-RAD-4-ADC.                                                        
001515   03  FILLER                       PIC X(84)  VALUE SPACE.               
001516   03  FILLER                       PIC X(12)  VALUE                      
001517                                               'UNPACKED EFR'.            
001518   03  FILLER                       PIC X(36)  VALUE SPACE.               
001519     EJECT                                                                
001520 01  L1-RAD-5-ADC.                                                        
001521   03  FILLER                       PIC X(14)  VALUE                      
001522                                               '  LOCATION    '.          
001523   03  FILLER                       PIC X(18)  VALUE                      
001524                                               '    QTY'.                 
001525   03  FILLER                       PIC X(42)  VALUE SPACE.               
001526   03  FILLER                       PIC X(6)   VALUE 'DISTR '.            
001527   03  FILLER                       PIC X(10)  VALUE 'CUSTOMER  '.        
001528   03  FILLER                       PIC X(7)   VALUE 'ORDERNO'.           
001529   03  FILLER                       PIC X(6)   VALUE SPACE.               
001530   03  FILLER                       PIC X(3)   VALUE 'QTY'.               
001531   03  FILLER                       PIC X(25)  VALUE SPACE.               
001532   EJECT                                                                  
001533 01  L1-RAD-6-ADC.                                                        
001534   03  FILLER                       PIC X      VALUE SPACE.               
001535   03  L1-ADLAGOMR-ADC              PIC Z99    VALUE ZERO.                
001536   03  FILLER                       PIC X      VALUE SPACE.               
001537   03  L1-ADGANG-ADC                PIC Z(3)   VALUE ZERO.                
001538   03  FILLER                       PIC X      VALUE SPACE.               
001539   03  L1-ADPLATS-ADC               PIC 9(5)   VALUE ZERO.                
001540   03  FILLER                       PIC X(12)  VALUE SPACE.               
001541   03  FILLER                       PIC X(7)   VALUE 'PRIME  '.           
001542   03  FILLER                       PIC X(4)   VALUE SPACE.               
001543   03  FILLER                       PIC X(7)   VALUE '.......'.           
001544   03  FILLER                       PIC X(3)   VALUE SPACE.               
001545   03  FILLER                       PIC X(13)  VALUE                      
001546                                                 'STOCK BALANCE'.         
001547   03  FILLER                       PIC X(3)   VALUE SPACE.               
001548   03  L1-KVLS-ADC                  PIC Z(7)-  VALUE ZERO.                
001549   03  FILLER                       PIC X(3)   VALUE SPACE.               
001550   03  L1-IDDISTR-ADC-1             PIC Z(5)   VALUE ZERO.                
001551   03  FILLER                       PIC X(4)   VALUE SPACE.               
001552   03  L1-IDKUNDNR-ADC-1            PIC Z(5)   VALUE ZERO.                
001553   03  FILLER                       PIC X(4)   VALUE SPACE.               
001554   03  L1-IDKUNDRF-ADC-1            PIC Z(5)   VALUE ZERO.                
001555   03  FILLER                       PIC X(2)   VALUE SPACE.               
001556   03  L1-KVLEVART-ADC-1            PIC Z(7)   VALUE ZERO.                
001557   03  FILLER                       PIC X(26)  VALUE SPACE.               
001558   EJECT                                                                  
001559 01  L1-RAD-7-ADC.                                                        
001560   03  FILLER                       PIC X      VALUE SPACE.               
001561   03  L1-ADBUFFOMR-ADC-1           PIC Z(3)   VALUE ZERO.                
001562   03  FILLER                       PIC X      VALUE SPACE.               
001563   03  L1-ADBUFFGANG-ADC-1          PIC Z(3)   VALUE ZERO.                
001564   03  FILLER                       PIC X      VALUE SPACE.               
001565   03  L1-ADBUFFPLATS-ADC-1         PIC Z(5)   VALUE ZERO.                
001566   03  FILLER                       PIC X(4)   VALUE SPACE.               
001567   03  L1-QTY-ADC-1                 PIC Z(7)   VALUE ZERO.                
001568   03  FILLER                       PIC X(1)   VALUE SPACE.               
001569   03  FILLER                       PIC X(9)   VALUE 'RESERVE 1'.         
001570   03  FILLER                       PIC X(2)   VALUE SPACE.               
001571   03  FILLER                       PIC X(7)   VALUE '.......'.           
001572   03  FILLER                       PIC X(1)   VALUE SPACE.               
001573   03  FILLER                       PIC X(17)  VALUE                      
001574                                             '  EFR NOT PICKED '.         
001575   03  FILLER                       PIC X(10)  VALUE '+.......  '.        
001576   03  FILLER                       PIC X(2)   VALUE SPACE.               
001577   03  L1-IDDISTR-ADC-2             PIC Z(5)   VALUE ZERO.                
001578   03  FILLER                       PIC X(4)   VALUE SPACE.               
001579   03  L1-IDKUNDNR-ADC-2            PIC Z(5)   VALUE ZERO.                
001580   03  FILLER                       PIC X(4)   VALUE SPACE.               
001581   03  L1-IDKUNDRF-ADC-2            PIC Z(5)   VALUE ZERO.                
001582   03  FILLER                       PIC X(2)   VALUE SPACE.               
001583   03  L1-KVLEVART-ADC-2            PIC Z(7)   VALUE ZERO.                
001584   03  FILLER                       PIC X(26)  VALUE SPACE.               
001585   EJECT                                                                  
001586 01  L1-RAD-8-ADC.                                                        
001587   03  FILLER                       PIC X      VALUE SPACE.               
001588   03  L1-ADBUFFOMR-ADC-2           PIC Z(3)   VALUE ZERO.                
001589   03  FILLER                       PIC X      VALUE SPACE.               
001590   03  L1-ADBUFFGANG-ADC-2          PIC Z(3)   VALUE ZERO.                
001591   03  FILLER                       PIC X      VALUE SPACE.               
001592   03  L1-ADBUFFPLATS-ADC-2         PIC Z(5)   VALUE ZERO.                
001593   03  FILLER                       PIC X(4)   VALUE SPACE.               
001594   03  L1-QTY-ADC-2                 PIC Z(7)   VALUE ZERO.                
001595   03  FILLER                       PIC X(12)  VALUE ' RESERVE 2'.        
001596   03  FILLER                       PIC X(7)   VALUE '.......'.           
001597   03  FILLER                       PIC X      VALUE SPACE.               
001598   03  FILLER                       PIC X(17)  VALUE                      
001599                                              '  STOCK          '.        
001600   03  FILLER                       PIC X(10)  VALUE                      
001601                                               '-.......  '.              
001602   03  FILLER                       PIC X(2)   VALUE SPACE.               
001603   03  L1-IDDISTR-ADC-3             PIC Z(5)   VALUE ZERO.                
001604   03  FILLER                       PIC X(4)   VALUE SPACE.               
001605   03  L1-IDKUNDNR-ADC-3            PIC Z(5)   VALUE ZERO.                
001606   03  FILLER                       PIC X(4)   VALUE SPACE.               
001607   03  L1-IDKUNDRF-ADC-3            PIC Z(5)   VALUE ZERO.                
001608   03  FILLER                       PIC X(2)   VALUE SPACE.               
001609   03  L1-KVLEVART-ADC-3            PIC Z(7)   VALUE ZERO.                
001610   03  FILLER                       PIC X(26)  VALUE SPACE.               
001611   EJECT                                                                  
001612 01  L1-RAD-9-ADC.                                                        
001613   03  FILLER                       PIC X      VALUE SPACE.               
001614   03  L1-ADBUFFOMR-ADC-3           PIC Z(3)   VALUE ZERO.                
001615   03  FILLER                       PIC X      VALUE SPACE.               
001616   03  L1-ADBUFFGANG-ADC-3          PIC Z(3)   VALUE ZERO.                
001617   03  FILLER                       PIC X      VALUE SPACE.               
001618   03  L1-ADBUFFPLATS-ADC-3         PIC Z(5)   VALUE ZERO.                
001619   03  FILLER                       PIC X(4)   VALUE SPACE.               
001620   03  L1-QTY-ADC-3                 PIC Z(7)   VALUE ZERO.                
001621   03  FILLER                       PIC X(12)  VALUE ' RESERVE 3'.        
001622   03  FILLER                       PIC X(7)   VALUE '.......'.           
001623   03  FILLER                       PIC X(18)  VALUE                      
001624                                           '   DIFFERENCE     '.          
001625   03  FILLER                       PIC X(10)  VALUE                      
001626                                         '=.......  '.                    
001627   03  FILLER                       PIC X(2)   VALUE SPACE.               
001628   03  L1-IDDISTR-ADC-4             PIC Z(5)   VALUE ZERO.                
001629   03  FILLER                       PIC X(4)   VALUE SPACE.               
001630   03  L1-IDKUNDNR-ADC-4            PIC Z(5)   VALUE ZERO.                
001631   03  FILLER                       PIC X(4)   VALUE SPACE.               
001632   03  L1-IDKUNDRF-ADC-4            PIC Z(5)   VALUE ZERO.                
001633   03  FILLER                       PIC X(2)   VALUE SPACE.               
001634   03  L1-KVLEVART-ADC-4            PIC Z(7)   VALUE ZERO.                
001635   03  FILLER                       PIC X(26)  VALUE SPACE.               
001636   EJECT                                                                  
001637 01  L1-RAD-10-ADC.                                                       
001638   03  FILLER                       PIC X      VALUE SPACE.               
001639   03  L1-ADBUFFOMR-ADC-4           PIC Z(3)   VALUE ZERO.                
001640   03  FILLER                       PIC X      VALUE SPACE.               
001641   03  L1-ADBUFFGANG-ADC-4          PIC Z(3)   VALUE ZERO.                
001642   03  FILLER                       PIC X      VALUE SPACE.               
001643   03  L1-ADBUFFPLATS-ADC-4         PIC Z(5)   VALUE ZERO.                
001644   03  FILLER                       PIC X(4)   VALUE SPACE.               
001645   03  L1-QTY-ADC-4                 PIC Z(7)   VALUE ZERO.                
001646   03  FILLER                       PIC X(12)  VALUE                      
001647             ' RESERVE 4'.                                                
001648   03  FILLER                       PIC X(7)   VALUE '.......'.           
001649   03  FILLER                       PIC X(30)  VALUE SPACE.               
001650   03  L1-IDDISTR-ADC-5             PIC Z(5)   VALUE ZERO.                
001651   03  FILLER                       PIC X(4)   VALUE SPACE.               
001652   03  L1-IDKUNDNR-ADC-5            PIC Z(5)   VALUE ZERO.                
001653   03  FILLER                       PIC X(4)   VALUE SPACE.               
001654   03  L1-IDKUNDRF-ADC-5            PIC Z(5)   VALUE ZERO.                
001655   03  FILLER                       PIC X(2)   VALUE SPACE.               
001656   03  L1-KVLEVART-ADC-5            PIC Z(7)   VALUE ZERO.                
001657   03  FILLER                       PIC X(26)  VALUE SPACE.               
001658   EJECT                                                                  
001659 01  L1-RAD-11-ADC.                                                       
001660   03  FILLER                       PIC X      VALUE SPACE.               
001661   03  L1-ADBUFFOMR-ADC-5           PIC Z(3)   VALUE ZERO.                
001662   03  FILLER                       PIC X      VALUE SPACE.               
001663   03  L1-ADBUFFGANG-ADC-5          PIC Z(3)   VALUE ZERO.                
001664   03  FILLER                       PIC X      VALUE SPACE.               
001665   03  L1-ADBUFFPLATS-ADC-5         PIC Z(5)   VALUE ZERO.                
001666   03  FILLER                       PIC X(4)   VALUE SPACE.               
001667   03  L1-QTY-ADC-5                 PIC Z(7)   VALUE ZERO.                
001668   03  FILLER                       PIC X(12)  VALUE ' RESERVE 5'.        
001669   03  FILLER                       PIC X(7)   VALUE '.......'.           
001670   03  FILLER                       PIC X(30)  VALUE SPACE.               
001671   03  L1-IDDISTR-ADC-6             PIC Z(5)   VALUE ZERO.                
001672   03  FILLER                       PIC X(4)   VALUE SPACE.               
001673   03  L1-IDKUNDNR-ADC-6            PIC Z(5)   VALUE ZERO.                
001674   03  FILLER                       PIC X(4)   VALUE SPACE.               
001675   03  L1-IDKUNDRF-ADC-6            PIC Z(5)   VALUE ZERO.                
001676   03  FILLER                       PIC X(2)   VALUE SPACE.               
001677   03  L1-KVLEVART-ADC-6            PIC Z(7)   VALUE ZERO.                
001678   03  FILLER                       PIC X(26)  VALUE SPACE.               
001679   EJECT                                                                  
001680 01  L1-RAD-12-ADC.                                                       
001681   03  FILLER                       PIC X(74)  VALUE SPACE.               
001682   03  L1-IDDISTR-ADC-7             PIC Z(5)   VALUE ZERO.                
001683   03  FILLER                       PIC X(4)   VALUE SPACE.               
001684   03  L1-IDKUNDNR-ADC-7            PIC Z(5)   VALUE ZERO.                
001685   03  FILLER                       PIC X(4)   VALUE SPACE.               
001686   03  L1-IDKUNDRF-ADC-7            PIC Z(5)   VALUE ZERO.                
001687   03  FILLER                       PIC X(2)   VALUE SPACE.               
001688   03  L1-KVLEVART-ADC-7            PIC Z(7)   VALUE ZERO.                
001689   03  FILLER                       PIC X(26)  VALUE SPACE.               
001690   EJECT                                                                  
001691 01  L1-RAD-13-ADC.                                                       
001692   03  FILLER                       PIC X(15)  VALUE                      
001693                                              ' MORE RESERVE ?'.          
001694   03  FILLER                       PIC X(8)   VALUE SPACE.               
001695   03  L1-BUFFER-ADC-7D             PIC X(3)   VALUE SPACE.               
001696   03  FILLER                       PIC X(21)  VALUE SPACE.               
001697   03  FILLER                       PIC X(6)   VALUE 'DOLLAR'.            
001698   03  FILLER                       PIC X(9)   VALUE SPACE.               
001699   03  FILLER                       PIC X(10)  VALUE                      
001700                                          '=.......  '.                   
001701   03  FILLER                       PIC X(2)   VALUE SPACE.               
001702   03  L1-IDDISTR-ADC-8             PIC Z(5)   VALUE ZERO.                
001703   03  FILLER                       PIC X(4)   VALUE SPACE.               
001704   03  L1-IDKUNDNR-ADC-8            PIC Z(5)   VALUE ZERO.                
001705   03  FILLER                       PIC X(4)   VALUE SPACE.               
001706   03  L1-IDKUNDRF-ADC-8            PIC Z(5)   VALUE ZERO.                
001707   03  FILLER                       PIC X(2)   VALUE SPACE.               
001708   03  L1-KVLEVART-ADC-8            PIC Z(7)   VALUE ZERO.                
001709   03  FILLER                       PIC X(26)  VALUE SPACE.               
001710   EJECT                                                                  
001711 01  L1-RAD-14-ADC.                                                       
001712   03  FILLER                       PIC X(74)  VALUE SPACE.               
001713   03  L1-IDDISTR-ADC-9             PIC Z(5)   VALUE ZERO.                
001714   03  FILLER                       PIC X(4)   VALUE SPACE.               
001715   03  L1-IDKUNDNR-ADC-9            PIC Z(5)   VALUE ZERO.                
001716   03  FILLER                       PIC X(4)   VALUE SPACE.               
001717   03  L1-IDKUNDRF-ADC-9            PIC Z(5)   VALUE ZERO.                
001718   03  FILLER                       PIC X(2)   VALUE SPACE.               
001719   03  L1-KVLEVART-ADC-9            PIC Z(7)   VALUE ZERO.                
001720   03  FILLER                       PIC X(26)  VALUE SPACE.               
001721   EJECT                                                                  
001722 01  L1-RAD-15-ADC.                                                       
001723   03  FILLER                       PIC X(17)  VALUE                      
001724                                             ' TOT. RESERVE QTY'.         
001725   03  FILLER                       PIC X(2)   VALUE SPACE.               
001726   03  L1-TOT-QTY-ADC-10            PIC Z(6)9  VALUE ZERO.                
001727   03  FILLER                       PIC X(48)  VALUE SPACE.               
001728   03  L1-IDDISTR-ADC-10            PIC Z(5)   VALUE ZERO.                
001729   03  FILLER                       PIC X(4)   VALUE SPACE.               
001730   03  L1-IDKUNDNR-ADC-10           PIC Z(5)   VALUE ZERO.                
001731   03  FILLER                       PIC X(4)   VALUE SPACE.               
001732   03  L1-IDKUNDRF-ADC-10           PIC Z(5)   VALUE ZERO.                
001733   03  FILLER                       PIC X(2)   VALUE SPACE.               
001734   03  L1-KVLEVART-ADC-10           PIC Z(7)   VALUE ZERO.                
001735   03  FILLER                       PIC X(26)  VALUE SPACE.               
001736   EJECT                                                                  
001737 01  L1-RAD-16-ADC.                                                       
001738   03  FILLER                       PIC X(74)  VALUE SPACE.               
001739   03  L1-IDDISTR-ADC-11            PIC Z(5)   VALUE ZERO.                
001740   03  FILLER                       PIC X(4)   VALUE SPACE.               
001741   03  L1-IDKUNDNR-ADC-11           PIC Z(5)   VALUE ZERO.                
001742   03  FILLER                       PIC X(4)   VALUE SPACE.               
001743   03  L1-IDKUNDRF-ADC-11           PIC Z(5)   VALUE ZERO.                
001744   03  FILLER                       PIC X(2)   VALUE SPACE.               
001745   03  L1-KVLEVART-ADC-11           PIC Z(7)   VALUE ZERO.                
001746   03  FILLER                       PIC X(26)  VALUE SPACE.               
001747   EJECT                                                                  
001748 01  L1-RAD-17-ADC.                                                       
001749   03  FILLER                       PIC X(6)   VALUE ' SORT '.            
001750   03  FILLER                       PIC X(8)   VALUE SPACE.               
001751   03  L1-KDSORT-ADC                PIC X(12)  VALUE SPACE.               
001752   03  FILLER                       PIC X(48)  VALUE SPACE.               
001753   03  L1-IDDISTR-ADC-12            PIC Z(5)   VALUE ZERO.                
001754   03  FILLER                       PIC X(4)   VALUE SPACE.               
001755   03  L1-IDKUNDNR-ADC-12           PIC Z(5)   VALUE ZERO.                
001756   03  FILLER                       PIC X(4)   VALUE SPACE.               
001757   03  L1-IDKUNDRF-ADC-12           PIC Z(5)   VALUE ZERO.                
001758   03  FILLER                       PIC X(2)   VALUE SPACE.               
001759   03  L1-KVLEVART-ADC-12           PIC Z(7)   VALUE ZERO.                
001760   03  FILLER                       PIC X(26)  VALUE SPACE.               
001761   EJECT                                                                  
001762 01  L1-RAD-18-ADC.                                                       
001763   03  FILLER                       PIC X(74)  VALUE SPACE.               
001764   03  L1-IDDISTR-ADC-13            PIC Z(5)   VALUE ZERO.                
001765   03  FILLER                       PIC X(4)   VALUE SPACE.               
001766   03  L1-IDKUNDNR-ADC-13           PIC Z(5)   VALUE ZERO.                
001767   03  FILLER                       PIC X(4)   VALUE SPACE.               
001768   03  L1-IDKUNDRF-ADC-13           PIC Z(5)   VALUE ZERO.                
001769   03  FILLER                       PIC X(2)   VALUE SPACE.               
001770   03  L1-KVLEVART-ADC-13           PIC Z(7)   VALUE ZERO.                
001771   03  FILLER                       PIC X(26)  VALUE SPACE.               
001772   SKIP2                                                                  
001773 01  L1-RAD-19-ADC.                                                       
001774   03  FILLER                       PIC X(10)  VALUE ' WEIGHT(G)'.        
001775   03  FILLER                       PIC X(9)   VALUE SPACE.               
001776   03  L1-VKART-ADC                 PIC Z(7)   VALUE ZERO.                
001777   03  FILLER                       PIC X(48)  VALUE SPACE.               
001778   03  L1-IDDISTR-ADC-14            PIC Z(5)   VALUE ZERO.                
001779   03  FILLER                       PIC X(4)   VALUE SPACE.               
001780   03  L1-IDKUNDNR-ADC-14           PIC Z(5)   VALUE ZERO.                
001781   03  FILLER                       PIC X(4)   VALUE SPACE.               
001782   03  L1-IDKUNDRF-ADC-14           PIC Z(5)   VALUE ZERO.                
001783   03  FILLER                       PIC X(2)   VALUE SPACE.               
001784   03  L1-KVLEVART-ADC-14           PIC Z(7)   VALUE ZERO.                
001785   03  FILLER                       PIC X(26)  VALUE SPACE.               
001786   SKIP2                                                                  
001787 01  L1-RAD-20-ADC.                                                       
001788   03  FILLER                       PIC X(74)  VALUE SPACE.               
001789   03  L1-IDDISTR-ADC-15            PIC Z(5)   VALUE ZERO.                
001790   03  FILLER                       PIC X(4)   VALUE SPACE.               
001791   03  L1-IDKUNDNR-ADC-15           PIC Z(5)   VALUE ZERO.                
001792   03  FILLER                       PIC X(4)   VALUE SPACE.               
001793   03  L1-IDKUNDRF-ADC-15           PIC Z(5)   VALUE ZERO.                
001794   03  FILLER                       PIC X(2)   VALUE SPACE.               
001795   03  L1-KVLEVART-ADC-15           PIC Z(7)   VALUE ZERO.                
001796   03  FILLER                       PIC X(26)  VALUE SPACE.               
001797   SKIP2                                                                  
001798 01  L1-RAD-21-ADC.                                                       
001799   03  FILLER                       PIC X(15)  VALUE                      
001800                                              ' STANDARD PRICE'.          
001801   03  FILLER                       PIC X(1)   VALUE SPACE.               
001802   03  L1-PRARTSTD-ADC              PIC Z(7).99 VALUE ZERO.               
001803   03  FILLER                       PIC X(48)  VALUE SPACE.               
001804   03  L1-IDDISTR-ADC-16            PIC Z(5)   VALUE ZERO.                
001805   03  FILLER                       PIC X(4)   VALUE SPACE.               
001806   03  L1-IDKUNDNR-ADC-16           PIC Z(5)   VALUE ZERO.                
001807   03  FILLER                       PIC X(4)   VALUE SPACE.               
001808   03  L1-IDKUNDRF-ADC-16           PIC Z(5)   VALUE ZERO.                
001809   03  FILLER                       PIC X(2)   VALUE SPACE.               
001810   03  L1-KVLEVART-ADC-16           PIC Z(7)   VALUE ZERO.                
001811   03  FILLER                       PIC X(26)  VALUE SPACE.               
001812   SKIP2                                                                  
001813 01  L1-RAD-23-ADC.                                                       
001814   03  FILLER                       PIC X(26)  VALUE                      
001815                                           ' LATEST ADJUSTMENTS '.        
001816   03  FILLER                       PIC X(33)  VALUE SPACE.               
001817   03  L1-TOTQTY-ADC-KOMM           PIC X(20)  VALUE SPACE.               
001818   03  FILLER                       PIC X(9)   VALUE 'TOTAL ALL'.         
001819   03  FILLER                       PIC X(7)   VALUE ' EFR   '.           
001820   03  L1-TOTQTY-ADC                PIC Z(10)9 VALUE ZERO.                
001821   03  FILLER                       PIC X(26)  VALUE SPACE.               
001822   SKIP2                                                                  
001823 01  L1-RAD-25-ADC.                                                       
001824   03  FILLER                       PIC X(10)  VALUE '  DATE'.            
001825   03  FILLER                       PIC X(8)   VALUE '    QTY'.           
001826   03  FILLER                       PIC X(10)  VALUE '  TYPE'.            
001827   03  FILLER                       PIC X(104) VALUE SPACE.               
001828   EJECT                                                                  
001829 01  L1-RAD-27-ADC.                                                       
001830   03  FILLER                       PIC X(2)   VALUE SPACE.               
001831   03  L1-TIJUSTDA-ADC              PIC 9(5)   VALUE ZERO.                
001832   03  FILLER                       PIC X(2)   VALUE SPACE.               
001833   03  L1-KVJUSTKV-ADC              PIC -(7)9  VALUE ZERO.                
001834   03  FILLER                       PIC X(5)   VALUE SPACE.               
001835   03  L1-KDJUSTYP-ADC              PIC Z      VALUE ZERO.                
001836   03  L1-KDJUSTYP-A-ADC            PIC X      VALUE ZERO.                
001837   03  FILLER                       PIC X(16)  VALUE SPACE.               
001838   03  FILLER                       PIC X(9)   VALUE 'COMMENT  '.         
001839   03  L1-TEINVANM-ADC              PIC X(28)  VALUE SPACE.               
001840   03  FILLER                       PIC X(55)  VALUE SPACE.               
001841   SKIP2                                                                  
001842 01  L1-RAD-29-ADC.                                                       
001843   03  FILLER                       PIC X(2)   VALUE SPACE.               
001844   03  L1-TIJUSTDA-1-ADC            PIC 9(5)   VALUE ZERO.                
001845   03  FILLER                       PIC X(2)   VALUE SPACE.               
001846   03  L1-KVJUSTKV-1-ADC            PIC -(8)   VALUE ZERO.                
001847   03  FILLER                       PIC X(5)   VALUE SPACE.               
001848   03  L1-KDJUSTYP-1-ADC            PIC Z      VALUE ZERO.                
001849   03  L1-KDJUSTYP-1A-ADC           PIC X      VALUE ZERO.                
001850   03  FILLER                       PIC X(108) VALUE SPACE.               
001851   SKIP2                                                                  
001852 01  L1-RAD-31-ADC.                                                       
001853   03  FILLER                       PIC X(2)   VALUE SPACE.               
001854   03  L1-TIJUSTDA-2-ADC            PIC 9(5)   VALUE ZERO.                
001855   03  FILLER                       PIC X(2)   VALUE SPACE.               
001856   03  L1-KVJUSTKV-2-ADC            PIC -(8)   VALUE ZERO.                
001857   03  FILLER                       PIC X(5)   VALUE SPACE.               
001858   03  L1-KDJUSTYP-2-ADC            PIC Z      VALUE ZERO.                
001859   03  L1-KDJUSTYP-2A-ADC           PIC X      VALUE ZERO.                
001860   03  FILLER                       PIC X(15)  VALUE SPACE.               
001861   03  FILLER                       PIC X(22)  VALUE                      
001862                                        ' ADJUSTED:............'.         
001863   03  FILLER                       PIC X(2)   VALUE SPACE.               
001864   03  FILLER                       PIC X(21)  VALUE                      
001865                                        '  ATTEST:............'.          
001866   03  FILLER                       PIC X(58)  VALUE SPACE.               
001867     EJECT                                                                
001868*************************************************                         
001869*****    LISTTRANSAR FÖR NDC-STOCKTAKING   ******                         
001870*************************************************                         
001871 01  NDC-RUBRAD.                                                          
001872   03  FILLER                       PIC X(15)  VALUE                      
001873                                        ' VCNA  LISTNO. '.                
001874   03  FILLER                       PIC X(10)  VALUE 'W50302-001'.        
001875   03  LISTNR-NDC                   PIC X(2)   VALUE SPACE.               
001876   03  FILLER                       PIC X(28)  VALUE                      
001877                                 '   STOCK TAKING DOCUMENT DC '.          
001878   03  RUB-IDDC-NDC                 PIC X(2)   VALUE SPACE.               
001879   03  FILLER                       PIC X(5)   VALUE ' CAT '.             
001880   03  RUB-KDINVKAT-NDC             PIC 9(2)   VALUE ZERO.                
001881   03  FILLER                       PIC X(6)   VALUE ' DATE '.            
001882   03  RUB-TIREGDAT-NDC             PIC 9(6)   VALUE ZERO.                
001883   03  FILLER                       PIC X(7)   VALUE ' PRINT '.           
001884   03  RUB-DATUM-NDC                PIC 9(6)   VALUE ZERO.                
001885   03  FILLER                       PIC X(1)   VALUE '-'.                 
001886   03  RUB-TID-NDC                  PIC 9(4)   VALUE ZERO.                
001887   03  FILLER                       PIC X(1)   VALUE SPACE.               
001888   03  FILLER                       PIC X(8)   VALUE 'LISTNR '.           
001889   03  RUB-PRINTNR-NDC              PIC Z(6)   VALUE ZERO.                
001890   03  FILLER                       PIC X(23)  VALUE SPACE.               
001891     EJECT                                                                
001892 01  L1-RAD-1-NDC.                                                        
001893   03  FILLER                       PIC X(12)  VALUE ' PARTNO.'.          
001894   03  L1-IDARTNR-NDC               PIC Z(9)   VALUE ZERO.                
001895   03  FILLER                       PIC X(5)   VALUE SPACE.               
001896   03  FILLER                       PIC X(3)   VALUE 'PG '.               
001897   03  L1-KDPRODSL-NDC              PIC Z(2)   VALUE ZERO.                
001898   03  FILLER                       PIC X(2)   VALUE SPACE.               
001899   03  FILLER                       PIC X(4)   VALUE 'LPC '.              
001900   03  L1-KDPSLLOC-NDC              PIC Z(2)   VALUE ZERO.                
001901   03  FILLER                       PIC X(2)   VALUE SPACE.               
001902   03  FILLER                       PIC X(21)  VALUE                      
001903                                        'INVESTIGATION BALANCE'.          
001904   03  L1-KVUTRS-NDC                PIC Z(6)9- VALUE ZERO.                
001905   03  FILLER                       PIC X(3)   VALUE SPACE.               
001906   03  FILLER                       PIC X(16)  VALUE                      
001907                                        'SUPRESSION CODE '.               
001908   03  L1-KDERS-NDC                 PIC Z99    VALUE ZERO.                
001909   03  FILLER                       PIC X(40)  VALUE SPACE.               
001910   EJECT                                                                  
001911 01  L1-RAD-3-NDC.                                                        
001912   03  FILLER                       PIC X(14)  VALUE                      
001913                                               ' DESCRIPTION:'.           
001914   03  L1-BEART-NDC                 PIC X(15)  VALUE SPACE.               
001915   03  FILLER                       PIC X(30)  VALUE SPACE.               
001916   03  FILLER                       PIC X(3)   VALUE ' AK'.               
001917   03  L1-KVAKS-NDC                 PIC Z(6)9- VALUE ZERO.                
001918   03  FILLER                       PIC X(62)  VALUE SPACE.               
001919   EJECT                                                                  
001920 01  L1-RAD-4-NDC.                                                        
001921   03  FILLER                       PIC X(84)  VALUE SPACE.               
001922   03  FILLER                       PIC X(12)  VALUE                      
001923                                               'UNPACKED EFR'.            
001924   03  FILLER                       PIC X(36)  VALUE SPACE.               
001925     EJECT                                                                
001926 01  L1-RAD-5-NDC.                                                        
001927   03  FILLER                       PIC X(14)  VALUE                      
001928                                               '  LOCATION    '.          
001929   03  FILLER                       PIC X(18)  VALUE                      
001930                                               '    QTY'.                 
001931   03  FILLER                       PIC X(42)  VALUE SPACE.               
001932   03  FILLER                       PIC X(6)   VALUE 'DISTR '.            
001933   03  FILLER                       PIC X(10)  VALUE 'CUSTOMER  '.        
001934   03  FILLER                       PIC X(7)   VALUE 'ORDERNO'.           
001935   03  FILLER                       PIC X(6)   VALUE SPACE.               
001936   03  FILLER                       PIC X(3)   VALUE 'QTY'.               
001937   03  FILLER                       PIC X(25)  VALUE SPACE.               
001938   EJECT                                                                  
001939 01  L1-RAD-6-NDC.                                                        
001940   03  FILLER                       PIC X      VALUE SPACE.               
001941   03  L1-ADLAGOMR-NDC              PIC Z99    VALUE ZERO.                
001942   03  FILLER                       PIC X      VALUE SPACE.               
001943   03  L1-ADGANG-NDC                PIC Z(3)   VALUE ZERO.                
001944   03  FILLER                       PIC X      VALUE SPACE.               
001945   03  L1-ADPLATS-NDC               PIC 9(5)   VALUE ZERO.                
001946   03  FILLER                       PIC X(12)  VALUE SPACE.               
001947   03  FILLER                       PIC X(7)   VALUE 'PRIME  '.           
001948   03  FILLER                       PIC X(4)   VALUE SPACE.               
001949   03  FILLER                       PIC X(7)   VALUE '.......'.           
001950   03  FILLER                       PIC X(3)   VALUE SPACE.               
001951   03  FILLER                       PIC X(13)  VALUE                      
001952                                                 'STOCK BALANCE'.         
001953   03  FILLER                       PIC X(3)   VALUE SPACE.               
001954   03  L1-KVLS-NDC                  PIC Z(7)-  VALUE ZERO.                
001955   03  FILLER                       PIC X(3)   VALUE SPACE.               
001956   03  L1-IDDISTR-NDC-1             PIC Z(5)   VALUE ZERO.                
001957   03  FILLER                       PIC X(4)   VALUE SPACE.               
001958   03  L1-IDKUNDNR-NDC-1            PIC Z(5)   VALUE ZERO.                
001959   03  FILLER                       PIC X(4)   VALUE SPACE.               
001960   03  L1-IDKUNDRF-NDC-1            PIC Z(5)   VALUE ZERO.                
001961   03  FILLER                       PIC X(2)   VALUE SPACE.               
001962   03  L1-KVLEVART-NDC-1            PIC Z(7)   VALUE ZERO.                
001963   03  FILLER                       PIC X(26)  VALUE SPACE.               
001964   EJECT                                                                  
001965 01  L1-RAD-7-NDC.                                                        
001966   03  FILLER                       PIC X      VALUE SPACE.               
001967   03  L1-ADBUFFOMR-NDC-1           PIC Z(3)   VALUE ZERO.                
001968   03  FILLER                       PIC X      VALUE SPACE.               
001969   03  L1-ADBUFFGANG-NDC-1          PIC Z(3)   VALUE ZERO.                
001970   03  FILLER                       PIC X      VALUE SPACE.               
001971   03  L1-ADBUFFPLATS-NDC-1         PIC Z(5)   VALUE ZERO.                
001972   03  FILLER                       PIC X(4)   VALUE SPACE.               
001973   03  L1-QTY-NDC-1                 PIC Z(7)   VALUE ZERO.                
001974   03  FILLER                       PIC X(1)   VALUE SPACE.               
001975   03  FILLER                       PIC X(9)   VALUE 'RESERVE 1'.         
001976   03  FILLER                       PIC X(2)   VALUE SPACE.               
001977   03  FILLER                       PIC X(7)   VALUE '.......'.           
001978   03  FILLER                       PIC X(1)   VALUE SPACE.               
001979   03  FILLER                       PIC X(17)  VALUE                      
001980                                             '  EFR NOT PICKED '.         
001981   03  FILLER                       PIC X(10)  VALUE '+.......  '.        
001982   03  FILLER                       PIC X(2)   VALUE SPACE.               
001983   03  L1-IDDISTR-NDC-2             PIC Z(5)   VALUE ZERO.                
001984   03  FILLER                       PIC X(4)   VALUE SPACE.               
001985   03  L1-IDKUNDNR-NDC-2            PIC Z(5)   VALUE ZERO.                
001986   03  FILLER                       PIC X(4)   VALUE SPACE.               
001987   03  L1-IDKUNDRF-NDC-2            PIC Z(5)   VALUE ZERO.                
001988   03  FILLER                       PIC X(2)   VALUE SPACE.               
001989   03  L1-KVLEVART-NDC-2            PIC Z(7)   VALUE ZERO.                
001990   03  FILLER                       PIC X(26)  VALUE SPACE.               
001991   EJECT                                                                  
001992 01  L1-RAD-8-NDC.                                                        
001993   03  FILLER                       PIC X      VALUE SPACE.               
001994   03  L1-ADBUFFOMR-NDC-2           PIC Z(3)   VALUE ZERO.                
001995   03  FILLER                       PIC X      VALUE SPACE.               
001996   03  L1-ADBUFFGANG-NDC-2          PIC Z(3)   VALUE ZERO.                
001997   03  FILLER                       PIC X      VALUE SPACE.               
001998   03  L1-ADBUFFPLATS-NDC-2         PIC Z(5)   VALUE ZERO.                
001999   03  FILLER                       PIC X(4)   VALUE SPACE.               
002000   03  L1-QTY-NDC-2                 PIC Z(7)   VALUE ZERO.                
002001   03  FILLER                       PIC X(12)  VALUE ' RESERVE 2'.        
002002   03  FILLER                       PIC X(7)   VALUE '.......'.           
002003   03  FILLER                       PIC X      VALUE SPACE.               
002004   03  FILLER                       PIC X(17)  VALUE                      
002005                                              '  STOCK          '.        
002006   03  FILLER                       PIC X(10)  VALUE                      
002007                                               '-.......  '.              
002008   03  FILLER                       PIC X(2)   VALUE SPACE.               
002009   03  L1-IDDISTR-NDC-3             PIC Z(5)   VALUE ZERO.                
002010   03  FILLER                       PIC X(4)   VALUE SPACE.               
002011   03  L1-IDKUNDNR-NDC-3            PIC Z(5)   VALUE ZERO.                
002012   03  FILLER                       PIC X(4)   VALUE SPACE.               
002013   03  L1-IDKUNDRF-NDC-3            PIC Z(5)   VALUE ZERO.                
002014   03  FILLER                       PIC X(2)   VALUE SPACE.               
002015   03  L1-KVLEVART-NDC-3            PIC Z(7)   VALUE ZERO.                
002016   03  FILLER                       PIC X(26)  VALUE SPACE.               
002017   EJECT                                                                  
002018 01  L1-RAD-9-NDC.                                                        
002019   03  FILLER                       PIC X      VALUE SPACE.               
002020   03  L1-ADBUFFOMR-NDC-3           PIC Z(3)   VALUE ZERO.                
002021   03  FILLER                       PIC X      VALUE SPACE.               
002022   03  L1-ADBUFFGANG-NDC-3          PIC Z(3)   VALUE ZERO.                
002023   03  FILLER                       PIC X      VALUE SPACE.               
002024   03  L1-ADBUFFPLATS-NDC-3         PIC Z(5)   VALUE ZERO.                
002025   03  FILLER                       PIC X(4)   VALUE SPACE.               
002026   03  L1-QTY-NDC-3                 PIC Z(7)   VALUE ZERO.                
002027   03  FILLER                       PIC X(12)  VALUE ' RESERVE 3'.        
002028   03  FILLER                       PIC X(7)   VALUE '.......'.           
002029   03  FILLER                       PIC X(18)  VALUE                      
002030                                           '   DIFFERENCE     '.          
002031   03  FILLER                       PIC X(10)  VALUE                      
002032                                         '=.......  '.                    
002033   03  FILLER                       PIC X(2)   VALUE SPACE.               
002034   03  L1-IDDISTR-NDC-4             PIC Z(5)   VALUE ZERO.                
002035   03  FILLER                       PIC X(4)   VALUE SPACE.               
002036   03  L1-IDKUNDNR-NDC-4            PIC Z(5)   VALUE ZERO.                
002037   03  FILLER                       PIC X(4)   VALUE SPACE.               
002038   03  L1-IDKUNDRF-NDC-4            PIC Z(5)   VALUE ZERO.                
002039   03  FILLER                       PIC X(2)   VALUE SPACE.               
002040   03  L1-KVLEVART-NDC-4            PIC Z(7)   VALUE ZERO.                
002041   03  FILLER                       PIC X(26)  VALUE SPACE.               
002042   EJECT                                                                  
002043 01  L1-RAD-10-NDC.                                                       
002044   03  FILLER                       PIC X      VALUE SPACE.               
002045   03  L1-ADBUFFOMR-NDC-4           PIC Z(3)   VALUE ZERO.                
002046   03  FILLER                       PIC X      VALUE SPACE.               
002047   03  L1-ADBUFFGANG-NDC-4          PIC Z(3)   VALUE ZERO.                
002048   03  FILLER                       PIC X      VALUE SPACE.               
002049   03  L1-ADBUFFPLATS-NDC-4         PIC Z(5)   VALUE ZERO.                
002050   03  FILLER                       PIC X(4)   VALUE SPACE.               
002051   03  L1-QTY-NDC-4                 PIC Z(7)   VALUE ZERO.                
002052   03  FILLER                       PIC X(12)  VALUE                      
002053             ' RESERVE 4'.                                                
002054   03  FILLER                       PIC X(7)   VALUE '.......'.           
002055   03  FILLER                       PIC X(30)  VALUE SPACE.               
002056   03  L1-IDDISTR-NDC-5             PIC Z(5)   VALUE ZERO.                
002057   03  FILLER                       PIC X(4)   VALUE SPACE.               
002058   03  L1-IDKUNDNR-NDC-5            PIC Z(5)   VALUE ZERO.                
002059   03  FILLER                       PIC X(4)   VALUE SPACE.               
002060   03  L1-IDKUNDRF-NDC-5            PIC Z(5)   VALUE ZERO.                
002061   03  FILLER                       PIC X(2)   VALUE SPACE.               
002062   03  L1-KVLEVART-NDC-5            PIC Z(7)   VALUE ZERO.                
002063   03  FILLER                       PIC X(26)  VALUE SPACE.               
002064   EJECT                                                                  
002065 01  L1-RAD-11-NDC.                                                       
002066   03  FILLER                       PIC X      VALUE SPACE.               
002067   03  L1-ADBUFFOMR-NDC-5           PIC Z(3)   VALUE ZERO.                
002068   03  FILLER                       PIC X      VALUE SPACE.               
002069   03  L1-ADBUFFGANG-NDC-5          PIC Z(3)   VALUE ZERO.                
002070   03  FILLER                       PIC X      VALUE SPACE.               
002071   03  L1-ADBUFFPLATS-NDC-5         PIC Z(5)   VALUE ZERO.                
002072   03  FILLER                       PIC X(4)   VALUE SPACE.               
002073   03  L1-QTY-NDC-5                 PIC Z(7)   VALUE ZERO.                
002074   03  FILLER                       PIC X(12)  VALUE ' RESERVE 5'.        
002075   03  FILLER                       PIC X(7)   VALUE '.......'.           
002076   03  FILLER                       PIC X(30)  VALUE SPACE.               
002077   03  L1-IDDISTR-NDC-6             PIC Z(5)   VALUE ZERO.                
002078   03  FILLER                       PIC X(4)   VALUE SPACE.               
002079   03  L1-IDKUNDNR-NDC-6            PIC Z(5)   VALUE ZERO.                
002080   03  FILLER                       PIC X(4)   VALUE SPACE.               
002081   03  L1-IDKUNDRF-NDC-6            PIC Z(5)   VALUE ZERO.                
002082   03  FILLER                       PIC X(2)   VALUE SPACE.               
002083   03  L1-KVLEVART-NDC-6            PIC Z(7)   VALUE ZERO.                
002084   03  FILLER                       PIC X(26)  VALUE SPACE.               
002085   EJECT                                                                  
002086 01  L1-RAD-12-NDC.                                                       
002087   03  FILLER                       PIC X(74)  VALUE SPACE.               
002088   03  L1-IDDISTR-NDC-7             PIC Z(5)   VALUE ZERO.                
002089   03  FILLER                       PIC X(4)   VALUE SPACE.               
002090   03  L1-IDKUNDNR-NDC-7            PIC Z(5)   VALUE ZERO.                
002091   03  FILLER                       PIC X(4)   VALUE SPACE.               
002092   03  L1-IDKUNDRF-NDC-7            PIC Z(5)   VALUE ZERO.                
002093   03  FILLER                       PIC X(2)   VALUE SPACE.               
002094   03  L1-KVLEVART-NDC-7            PIC Z(7)   VALUE ZERO.                
002095   03  FILLER                       PIC X(26)  VALUE SPACE.               
002096   EJECT                                                                  
002097 01  L1-RAD-13-NDC.                                                       
002098   03  FILLER                       PIC X(15)  VALUE                      
002099                                              ' MORE RESERVE ?'.          
002100   03  FILLER                       PIC X(8)   VALUE SPACE.               
002101   03  L1-BUFFER-NDC-7D             PIC X(3)   VALUE SPACE.               
002102   03  FILLER                       PIC X(21)  VALUE SPACE.               
002103   03  FILLER                       PIC X(6)   VALUE 'DOLLAR'.            
002104   03  FILLER                       PIC X(9)   VALUE SPACE.               
002105   03  FILLER                       PIC X(10)  VALUE                      
002106                                          '=.......  '.                   
002107   03  FILLER                       PIC X(2)   VALUE SPACE.               
002108   03  L1-IDDISTR-NDC-8             PIC Z(5)   VALUE ZERO.                
002109   03  FILLER                       PIC X(4)   VALUE SPACE.               
002110   03  L1-IDKUNDNR-NDC-8            PIC Z(5)   VALUE ZERO.                
002111   03  FILLER                       PIC X(4)   VALUE SPACE.               
002112   03  L1-IDKUNDRF-NDC-8            PIC Z(5)   VALUE ZERO.                
002113   03  FILLER                       PIC X(2)   VALUE SPACE.               
002114   03  L1-KVLEVART-NDC-8            PIC Z(7)   VALUE ZERO.                
002115   03  FILLER                       PIC X(26)  VALUE SPACE.               
002116   EJECT                                                                  
002117 01  L1-RAD-14-NDC.                                                       
002118   03  FILLER                       PIC X(74)  VALUE SPACE.               
002119   03  L1-IDDISTR-NDC-9             PIC Z(5)   VALUE ZERO.                
002120   03  FILLER                       PIC X(4)   VALUE SPACE.               
002121   03  L1-IDKUNDNR-NDC-9            PIC Z(5)   VALUE ZERO.                
002122   03  FILLER                       PIC X(4)   VALUE SPACE.               
002123   03  L1-IDKUNDRF-NDC-9            PIC Z(5)   VALUE ZERO.                
002124   03  FILLER                       PIC X(2)   VALUE SPACE.               
002125   03  L1-KVLEVART-NDC-9            PIC Z(7)   VALUE ZERO.                
002126   03  FILLER                       PIC X(26)  VALUE SPACE.               
002127   EJECT                                                                  
002128 01  L1-RAD-15-NDC.                                                       
002129   03  FILLER                       PIC X(17)  VALUE                      
002130                                             ' TOT. RESERVE QTY'.         
002131   03  FILLER                       PIC X(2)   VALUE SPACE.               
002132   03  L1-TOT-QTY-NDC-10            PIC Z(6)9  VALUE ZERO.                
002133   03  FILLER                       PIC X(48)  VALUE SPACE.               
002134   03  L1-IDDISTR-NDC-10            PIC Z(5)   VALUE ZERO.                
002135   03  FILLER                       PIC X(4)   VALUE SPACE.               
002136   03  L1-IDKUNDNR-NDC-10           PIC Z(5)   VALUE ZERO.                
002137   03  FILLER                       PIC X(4)   VALUE SPACE.               
002138   03  L1-IDKUNDRF-NDC-10           PIC Z(5)   VALUE ZERO.                
002139   03  FILLER                       PIC X(2)   VALUE SPACE.               
002140   03  L1-KVLEVART-NDC-10           PIC Z(7)   VALUE ZERO.                
002141   03  FILLER                       PIC X(26)  VALUE SPACE.               
002142   EJECT                                                                  
002143 01  L1-RAD-16-NDC.                                                       
002144   03  FILLER                       PIC X(74)  VALUE SPACE.               
002145   03  L1-IDDISTR-NDC-11            PIC Z(5)   VALUE ZERO.                
002146   03  FILLER                       PIC X(4)   VALUE SPACE.               
002147   03  L1-IDKUNDNR-NDC-11           PIC Z(5)   VALUE ZERO.                
002148   03  FILLER                       PIC X(4)   VALUE SPACE.               
002149   03  L1-IDKUNDRF-NDC-11           PIC Z(5)   VALUE ZERO.                
002150   03  FILLER                       PIC X(2)   VALUE SPACE.               
002151   03  L1-KVLEVART-NDC-11           PIC Z(7)   VALUE ZERO.                
002152   03  FILLER                       PIC X(26)  VALUE SPACE.               
002153   EJECT                                                                  
002154 01  L1-RAD-17-NDC.                                                       
002155   03  FILLER                       PIC X(6)   VALUE ' SORT '.            
002156   03  FILLER                       PIC X(8)   VALUE SPACE.               
002157   03  L1-KDSORT-NDC                PIC X(12)  VALUE SPACE.               
002158   03  FILLER                       PIC X(48)  VALUE SPACE.               
002159   03  L1-IDDISTR-NDC-12            PIC Z(5)   VALUE ZERO.                
002160   03  FILLER                       PIC X(4)   VALUE SPACE.               
002161   03  L1-IDKUNDNR-NDC-12           PIC Z(5)   VALUE ZERO.                
002162   03  FILLER                       PIC X(4)   VALUE SPACE.               
002163   03  L1-IDKUNDRF-NDC-12           PIC Z(5)   VALUE ZERO.                
002164   03  FILLER                       PIC X(2)   VALUE SPACE.               
002165   03  L1-KVLEVART-NDC-12           PIC Z(7)   VALUE ZERO.                
002166   03  FILLER                       PIC X(26)  VALUE SPACE.               
002167   EJECT                                                                  
002168 01  L1-RAD-18-NDC.                                                       
002169   03  FILLER                       PIC X(74)  VALUE SPACE.               
002170   03  L1-IDDISTR-NDC-13            PIC Z(5)   VALUE ZERO.                
002171   03  FILLER                       PIC X(4)   VALUE SPACE.               
002172   03  L1-IDKUNDNR-NDC-13           PIC Z(5)   VALUE ZERO.                
002173   03  FILLER                       PIC X(4)   VALUE SPACE.               
002174   03  L1-IDKUNDRF-NDC-13           PIC Z(5)   VALUE ZERO.                
002175   03  FILLER                       PIC X(2)   VALUE SPACE.               
002176   03  L1-KVLEVART-NDC-13           PIC Z(7)   VALUE ZERO.                
002177   03  FILLER                       PIC X(26)  VALUE SPACE.               
002178   SKIP2                                                                  
002179 01  L1-RAD-19-NDC.                                                       
002180   03  FILLER                       PIC X(10)  VALUE ' WEIGHT(G)'.        
002181   03  FILLER                       PIC X(9)   VALUE SPACE.               
002182   03  L1-VKART-NDC                 PIC Z(7)   VALUE ZERO.                
002183   03  FILLER                       PIC X(48)  VALUE SPACE.               
002184   03  L1-IDDISTR-NDC-14            PIC Z(5)   VALUE ZERO.                
002185   03  FILLER                       PIC X(4)   VALUE SPACE.               
002186   03  L1-IDKUNDNR-NDC-14           PIC Z(5)   VALUE ZERO.                
002187   03  FILLER                       PIC X(4)   VALUE SPACE.               
002188   03  L1-IDKUNDRF-NDC-14           PIC Z(5)   VALUE ZERO.                
002189   03  FILLER                       PIC X(2)   VALUE SPACE.               
002190   03  L1-KVLEVART-NDC-14           PIC Z(7)   VALUE ZERO.                
002191   03  FILLER                       PIC X(26)  VALUE SPACE.               
002192   SKIP2                                                                  
002193 01  L1-RAD-20-NDC.                                                       
002194   03  FILLER                       PIC X(74)  VALUE SPACE.               
002195   03  L1-IDDISTR-NDC-15            PIC Z(5)   VALUE ZERO.                
002196   03  FILLER                       PIC X(4)   VALUE SPACE.               
002197   03  L1-IDKUNDNR-NDC-15           PIC Z(5)   VALUE ZERO.                
002198   03  FILLER                       PIC X(4)   VALUE SPACE.               
002199   03  L1-IDKUNDRF-NDC-15           PIC Z(5)   VALUE ZERO.                
002200   03  FILLER                       PIC X(2)   VALUE SPACE.               
002201   03  L1-KVLEVART-NDC-15           PIC Z(7)   VALUE ZERO.                
002202   03  FILLER                       PIC X(26)  VALUE SPACE.               
002203   SKIP2                                                                  
002204 01  L1-RAD-21-NDC.                                                       
002205   03  FILLER                       PIC X(15)  VALUE                      
002206                                              ' AVERAGE COST  '.          
002207   03  FILLER                       PIC X(1)   VALUE SPACE.               
002208   03  L1-PRAVCOST-NDC              PIC Z(7).99 VALUE ZERO.               
002209   03  FILLER                       PIC X(48)  VALUE SPACE.               
002210   03  L1-IDDISTR-NDC-16            PIC Z(5)   VALUE ZERO.                
002211   03  FILLER                       PIC X(4)   VALUE SPACE.               
002212   03  L1-IDKUNDNR-NDC-16           PIC Z(5)   VALUE ZERO.                
002213   03  FILLER                       PIC X(4)   VALUE SPACE.               
002214   03  L1-IDKUNDRF-NDC-16           PIC Z(5)   VALUE ZERO.                
002215   03  FILLER                       PIC X(2)   VALUE SPACE.               
002216   03  L1-KVLEVART-NDC-16           PIC Z(7)   VALUE ZERO.                
002217   03  FILLER                       PIC X(26)  VALUE SPACE.               
002218   SKIP2                                                                  
002219 01  L1-RAD-23-NDC.                                                       
002220   03  FILLER                       PIC X(26)  VALUE                      
002221                                           ' LATEST ADJUSTMENTS '.        
002222   03  FILLER                       PIC X(33)  VALUE SPACE.               
002223   03  L1-TOTQTY-NDC-KOMM           PIC X(20)  VALUE SPACE.               
002224   03  FILLER                       PIC X(9)   VALUE 'TOTAL ALL'.         
002225   03  FILLER                       PIC X(7)   VALUE ' EFR   '.           
002226   03  L1-TOTQTY-NDC                PIC Z(10)9 VALUE ZERO.                
002227   03  FILLER                       PIC X(26)  VALUE SPACE.               
002228   SKIP2                                                                  
002229 01  L1-RAD-25-NDC.                                                       
002230   03  FILLER                       PIC X(10)  VALUE '  DATE'.            
002231   03  FILLER                       PIC X(8)   VALUE '    QTY'.           
002232   03  FILLER                       PIC X(10)  VALUE '  TYPE'.            
002233   03  FILLER                       PIC X(104) VALUE SPACE.               
002234   EJECT                                                                  
002235 01  L1-RAD-27-NDC.                                                       
002236   03  FILLER                       PIC X(2)   VALUE SPACE.               
002237   03  L1-TIJUSTDA-NDC              PIC 9(5)   VALUE ZERO.                
002238   03  FILLER                       PIC X(2)   VALUE SPACE.               
002239   03  L1-KVJUSTKV-NDC              PIC -(7)9  VALUE ZERO.                
002240   03  FILLER                       PIC X(5)   VALUE SPACE.               
002241   03  L1-KDJUSTYP-NDC              PIC Z      VALUE ZERO.                
002242   03  FILLER                       PIC X(17)  VALUE SPACE.               
002243   03  FILLER                       PIC X(9)   VALUE 'COMMENT  '.         
002244   03  L1-TEINVANM-NDC              PIC X(28)  VALUE SPACE.               
002245   03  FILLER                       PIC X(55)  VALUE SPACE.               
002246   SKIP2                                                                  
002247 01  L1-RAD-29-NDC.                                                       
002248   03  FILLER                       PIC X(2)   VALUE SPACE.               
002249   03  L1-TIJUSTDA-1-NDC            PIC 9(5)   VALUE ZERO.                
002250   03  FILLER                       PIC X(2)   VALUE SPACE.               
002251   03  L1-KVJUSTKV-1-NDC            PIC -(8)   VALUE ZERO.                
002252   03  FILLER                       PIC X(5)   VALUE SPACE.               
002253   03  L1-KDJUSTYP-1-NDC            PIC Z      VALUE ZERO.                
002254   03  FILLER                       PIC X(109) VALUE SPACE.               
002255   SKIP2                                                                  
002256 01  L1-RAD-31-NDC.                                                       
002257   03  FILLER                       PIC X(2)   VALUE SPACE.               
002258   03  L1-TIJUSTDA-2-NDC            PIC 9(5)   VALUE ZERO.                
002259   03  FILLER                       PIC X(2)   VALUE SPACE.               
002260   03  L1-KVJUSTKV-2-NDC            PIC -(8)   VALUE ZERO.                
002261   03  FILLER                       PIC X(5)   VALUE SPACE.               
002262   03  L1-KDJUSTYP-2-NDC            PIC Z      VALUE ZERO.                
002263   03  FILLER                       PIC X(16)  VALUE SPACE.               
002264   03  FILLER                       PIC X(22)  VALUE                      
002265                                        ' ADJUSTED:............'.         
002266   03  FILLER                       PIC X(2)   VALUE SPACE.               
002267   03  FILLER                       PIC X(21)  VALUE                      
002268                                        '  ATTEST:............'.          
002269   03  FILLER                       PIC X(58)  VALUE SPACE.               
002270     EJECT                                                                
002271 01    FILLER                    PIC X(16)     VALUE 'W006PRAR'.          
002272                                                                          
002273*01  -COPY W006PRAR                                                       
002274     EJECT                                                                
002275***************************************************************           
002276* SPAR WDH11 AREA                                                         
002277                                                                          
002278 01  SPAR-WDH111-AREA.                                                    
002279     03  WDH111.                                                          
002280*        05  -COPY WDH111 -PRE SPAR-                                      
002281                                                                          
002282******************************************************************        
002283*                                                                         
002284*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
002285*                                                                         
002286 01    FILLER                    PIC X(16)   VALUE 'MFS-WS'.              
002287     SKIP3                                                                
002288*01    MID -COPY W5I30101                                                 
002289     EJECT                                                                
002290* - - - - - - - - - - - - - - - MSG-AREA                                  
002291*01    -COPY WMSGAREA                                                     
002292     EJECT                                                                
002293*  03  MOD -COPY W5O30101    -RED MSG-AREA.                               
002294     EJECT                                                                
002295*01    -COPY WMFSAREA                                                     
002296     EJECT                                                                
002297******************************************************************        
002298*                                                                         
002299*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
002300*                                                                         
002301 01    IMS-WS.                                                            
002302   03  FILLER                    PIC X(16)   VALUE 'IMS-WS     '.         
002303     SKIP3                                                                
002304*                        **** STATUS-KOD FRÅN IMS                         
002305   03    STATUS-WS               PIC XX.                                  
002306     88    SEGMENT-FINNS                     VALUE '  '.                  
002307     88    SEGMENT-FINNS-REDAN               VALUE 'II'.                  
002308     88    SEGMENT-SAKNAS                    VALUE 'GE' 'GB'.             
002309     88    BASEN-SLUT                        VALUE 'GB'.                  
002310     SKIP3                                                                
002311   03    GODK-STATUSKODER.                                                
002312     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
002313     SKIP3                                                                
002314 01  ALL-SSA.                                                             
002315     03 SSA1                     PIC X(128).                              
002316     03 SSA2                     PIC X(128).                              
002317     EJECT                                                                
002318*                            IMS FUNKTIONSKODER                           
002319*01    -COPY W0003                                                        
002320     EJECT                                                                
002321*                            DL1 INPUT-OUTPUT AREA                        
002322 01    DLI-IO-AREA.                                                       
002323   03    IO-AREA                 PIC X(900)  VALUE SPACE.                 
002324     SKIP3                                                                
002325*  03    WLARTC01 -COPY WDK601              -RED IO-AREA.                 
002326     EJECT                                                                
002327*  03    WLARTC11 -COPY WDK611              -RED IO-AREA.                 
002328     EJECT                                                                
002329*  03  WLBENA11 -COPY WDD311 -PRE BEN-      -RED IO-AREA.                 
002330     EJECT                                                                
002331 01  FILLER                      PIC X(16) VALUE 'DLI-IO-E4C1'.           
002332 01  DLI-IO-E4C1.                                                         
002333*  03  -COPY WDE4C1                                                       
002334     EJECT                                                                
002335 01  FILLER                      PIC X(16) VALUE 'DLI-IO-E401-11'.        
002336 01  DLI-IO-E401-11.                                                      
002337   03 DLI-IO-E401.                                                        
002338*    05  -COPY WDE401                                                     
002339   03 DLI-IO-E411.                                                        
002340*    05  -COPY WDE411                                                     
002341     EJECT                                                                
002342 01  FILLER                   PIC X(16) VALUE 'DLI-IO-AREA3    '.         
002343                                                                          
002344 01  DLI-IO-AREA3.                                                        
002345    03  IO-AREA3              PIC X(350)  VALUE SPACE.                    
002346     SKIP2                                                                
002347*    03 WLSATE -COPY WDJ1C1      -PRE SATE01-  -RED IO-AREA3.             
002348     EJECT                                                                
002349   03   WLSATB-IN   REDEFINES IO-AREA3.                                   
002350*    05 WLSATB -COPY WDJ111      -PRE SATB11-.                            
002351     SKIP3                                                                
002352*    05 WLSATB -COPY WDJ101      -PRE SATB01-.                            
002353     EJECT                                                                
002354 01    DLI-IO-AREA4.                                                      
002355   03  IO-AREA4                  PIC X(600)  VALUE SPACE.                 
002356     SKIP3                                                                
002357*  03  WLARTS01 -COPY WDK701               -RED IO-AREA4.                 
002358     EJECT                                                                
002359*  03  WLARTS11 -COPY WDK711               -RED IO-AREA4.                 
002360     EJECT                                                                
002361 01    DLI-IO-AREA5.                                                      
002362   03  IO-AREA5                  PIC X(300)  VALUE SPACE.                 
002363     SKIP3                                                                
002364*  03  WLINVC01 -COPY WDH701               -RED IO-AREA5.                 
002365     EJECT                                                                
002366*  03  WLINVC11 -COPY WDH711               -RED IO-AREA5.                 
002367     EJECT                                                                
002368 01  FILLER                      PIC X(16)   VALUE 'WDH101-AREA'.         
002369 01  DLI-IO-AREA-INVA01.                                                  
002370     03  WDH101.                                                          
002371*        05  -COPY WDH101                                                 
002372 01  FILLER                      PIC X(16)   VALUE 'WDH111-AREA'.         
002373 01  DLI-IO-AREA-INVA11.                                                  
002374     03  WDH111.                                                          
002375*        05  -COPY WDH111                                                 
002376     EJECT                                                                
002377 01  FILLER                      PIC X(16)   VALUE 'WDH121-AREA'.         
002378 01  DLI-IO-AREA-INVA21.                                                  
002379     03  WDH121.                                                          
002380*        05  -COPY WDH121                                                 
002381     EJECT                                                                
002382 01  FILLER                      PIC X(16)   VALUE 'WDGX5102AREA'.        
002383 01  DLI-IO-AREA-WDGX5102.                                                
002384     03  WDGX5102.                                                        
002385*        05  -COPY WDGX5102                                               
002386     EJECT                                                                
002387 01  FILLER               PIC X(16)   VALUE 'WDB601-A AREA'.              
002388 01   DLI-IO-AREA-B601-A.                                                 
002389*     03  -COPY WDB601 -PRE A-                                            
002390     EJECT                                                                
002391 01  FILLER               PIC X(16)   VALUE 'WDB601-B AREA'.              
002392 01   DLI-IO-AREA-B601-B.                                                 
002393*     03  -COPY WDB601 -PRE B-                                            
002394     EJECT                                                                
002398 01  FILLER               PIC X(16)   VALUE 'WDD8B1 AREA'.                
002399 01   DLI-IO-WDD8B1.                                                      
002400*     03  -COPY WDD8B1                                                    
002401     EJECT                                                                
002402 LINKAGE SECTION.                                                         
002403                                                                          
002404*01    -COPY W0009     -PRE MSG-                                          
002405                                                                          
002406 01  ALT-PCB                     PIC X(32).                               
002407     EJECT                                                                
002408*01    -COPY W0008     -PRE USEA-                                         
002409     05  FILLER                  PIC X.                                   
002410                                                                          
002411*01    -COPY W0008     -PRE ARTC-                                         
002412     05  FILLER                  PIC X.                                   
002413     EJECT                                                                
002414*01    -COPY W0008     -PRE WDD8B-                                        
002415     05  FILLER                  PIC X.                                   
002416                                                                          
002417*01    -COPY W0008     -PRE WDE4C-                                        
002418     05  FILLER                  PIC X.                                   
002419     EJECT                                                                
002420*01    -COPY W0008     -PRE WDE4-                                         
002421     05  FILLER                  PIC X.                                   
002422                                                                          
002423*01    -COPY W0008     -PRE INV-                                          
002424     05  FILLER                  PIC X.                                   
002425                                                                          
002426*01    -COPY W0008     -PRE BEN-                                          
002427     05  FILLER                  PIC X.                                   
002428                                                                          
002429*    -COPY W0008       -PRE SATB-.                                        
002430      05    FILLER           PIC X.                                       
002431     EJECT                                                                
002432                                                                          
002433*    -COPY W0008       -PRE ARTS-.                                        
002434      05    FILLER           PIC X.                                       
002435                                                                          
002436                                                                          
002437*    -COPY W0008       -PRE INVC-.                                        
002438      05    FILLER           PIC X.                                       
002439                                                                          
002440*    -COPY W0008       -PRE WDG3-.                                        
002441      05    FILLER           PIC X.                                       
002442                                                                          
002443*01  -COPY W0008       -PRE WDB6-                                         
002444     05  FILLER              PIC X.                                       
002448                                                                          
002449     EJECT                                                                
002450 PROCEDURE DIVISION USING MSG-PCB ALT-PCB USEA-PCB                        
002451         ARTC-PCB WDD8B-PCB WDE4C-PCB WDE4-PCB                            
002452         INV-PCB BEN-PCB SATB-PCB ARTS-PCB                                
002453         INVC-PCB WDG3-PCB WDB6-PCB.                                      
002454 MAIN SECTION.                                                            
002455     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB USEA-PCB                       
002456         ARTC-PCB WDD8B-PCB WDE4C-PCB WDE4-PCB                            
002457         INV-PCB BEN-PCB SATB-PCB ARTS-PCB                                
002458         INVC-PCB WDG3-PCB WDB6-PCB.                                      
002459                                                                          
002460                                                                          
002461     PERFORM IMS-GET-MSG                                                  
002462     IF SEGMENT-FINNS                                                     
002463        PERFORM A-INIT-SPARA-INPUT                                        
002464        IF GODK-TRANS                                                     
002465           IF MFS-UPD-X                                                   
002466              PERFORM E-BEARBETNING                                       
002467           END-IF                                                         
002468        ELSE                                                              
002469           MOVE FEL-1 (SPRAK-IX) TO MOD-TEMFSFEL                          
002470           PERFORM MFS-RENSA-BILD                                         
002471        END-IF                                                            
002472        IF NOT MFS-UPD-X                                                  
002473           COMPUTE MSG-KVLL = LENGTH OF MOD-W5O30101 + 4                  
002474           PERFORM IMS-INSERT-MSG                                         
002475        END-IF                                                            
002476     END-IF                                                               
002477     MOVE ZERO TO RETURN-CODE                                             
002478     GOBACK                                                               
002479     .                                                                    
002480     EJECT                                                                
002481 A-INIT-SPARA-INPUT SECTION.                                              
002482     MOVE 'A-INITSPARA-INPUT'  TO WS-SECTION                              
002483                                                                          
002484     IF MSG-DUBBLA-TRANSKODER                                             
002485       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W5I30101                 
002486       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
002487       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
002488       MOVE MSG-KDTRTYP                   TO MFS-KDTRTYP                  
002489     ELSE                                                                 
002490       MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W5I30101                 
002491       MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                  
002492       MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                 
002493     END-IF                                                               
002494                                                                          
002495     MOVE MSG-KDTRTYP                     TO MFS-KDTRTYP                  
002496     MOVE MSG-IDPFK                       TO MFS-IDPFK                    
002497     MOVE MFS-IDTRANS                     TO W-IDTRANS                    
002498                                                                          
002499     MOVE LOW-VALUE                       TO MSG-AREA                     
002500     MOVE 'W5O30101'                      TO MFS-IDMOD                    
002501     MOVE '5301'                          TO MOD-IDTRANS                  
002502     IF SWEDISH-TEXT                                                      
002503       MOVE +1                            TO SPRAK-IX                     
002504     ELSE                                                                 
002505       MOVE +2                            TO SPRAK-IX                     
002506     END-IF                                                               
002507     MOVE MFS-RENSA-FAELT                 TO MOD-TEMFSFEL                 
002508                                             MOD-TEMFSINF                 
002509     MOVE SPACE                           TO L1-TOTQTY-ADC-KOMM           
002510                                             L1-TOTQTY-NDC-KOMM           
002511     MOVE FUNCTION CURRENT-DATE(1:8)  TO WS-DAGENS-DATUM                  
002512     .                                                                    
002513     EJECT                                                                
002514 E-BEARBETNING SECTION.                                                   
002515     MOVE 'E-BEARBETNING' TO WS-SECTION                                   
002516                                                                          
002517     MOVE MID-IDDC                   TO W-IDDC                            
002518                                        W-IDDC-G3                         
002519                                        W-IDDC-WDH1-MIN                   
002520                                        W-IDDC-WDH1-MAX                   
002521                                        W-IDDC-B6                         
002522     MOVE JA       TO WDB6-A-SW                                           
002523     PERFORM IMS-GU-WDB601-A                                              
002524     IF SEGMENT-SAKNAS                                                    
002525        MOVE NEJ   TO WDB6-A-SW                                           
002526     END-IF                                                               
002527                                                                          
002528     PERFORM EA-OEPPNA-PRINTER                                            
002529     PERFORM EB-HAMTA-DATUM-OCH-TID                                       
002530                                                                          
002531     MOVE +1 TO IX                                                        
002532     PERFORM UNTIL IX > 10 OR MID-IDARTNR-PRINT(IX) = ALL '+'             
002534       MOVE MID-IDARTNR-PRINT(IX)    TO W-IDARTNR                         
002535                                        L1-IDARTNR-CDC                    
002536                                        BU-IDARTNR-CDC                    
002537                                        L1-IDARTNR-SDC                    
002538                                        L1-IDARTNR-SDC-1                  
002539                                        RUB-IDARTNR-SDC                   
002540                                        L1-IDARTNR-ADC                    
002541                                        L1-IDARTNR-NDC                    
002542                                        W-IDARTNR-LOW                     
002543                                        W-IDARTNR-HIGH                    
002544                                        W-IDARTNR-D8-MIN                  
002545                                        W-IDARTNR-D8-MAX                  
002546       MOVE MID-KDINVKAT-PRINT (IX)  TO W-KDINVKAT-WDH1-MIN               
002547                                        W-KDINVKAT-WDH1-MAX               
002548       PERFORM EC-HAEMTA-ANM-WDH1                                         
002549       IF MFS-UPD-X AND SEGMENT-SAKNAS                                    
002550         MOVE +99 TO IX                                                   
002551       ELSE                                                               
002552         PERFORM ED-NOLLA-TABELLER                                        
002553         EVALUATE TRUE                                                    
002554         WHEN WDB6-A-FINNS AND A-DCS-CDC                                  
002555           PERFORM EE-HAEMTA-UPPG-WDK6-CDC                                
002556           PERFORM EF-HAEMTA-UPPG-INLEV                                   
002557           PERFORM EG-HAEMTA-UPPG-SATS                                    
002558           PERFORM EH-HAEMTA-UPPG-WDD8-CDC                                
002559           PERFORM EI-HAEMTA-UPPG-WDD3-CDC                                
002560           PERFORM EJ-HAEMTA-CDC-WDE4-OVR-DISTR                           
002561           PERFORM EK-HAMTA-UPPG-WDE4-DISTR-98                            
002562           PERFORM EL-CDC-SKRIV-LISTA1                                    
002563           IF BUFFERT-PRINT                                               
002564             PERFORM EM-SKRIV-EXTRA-BUFFRADER-CDC                         
002565           END-IF                                                         
002566         WHEN WDB6-A-FINNS AND A-DCS-SDC                                  
002567           PERFORM EN-HAEMTA-UPPG-WDK6-WDK7-SDC                           
002568           PERFORM EO-HAEMTA-UPPG-WDD8-SDC                                
002569           PERFORM EP-HAEMTA-UPPG-WDD3-SDC                                
002570           PERFORM EQ-HAEMTA-SDC-WDE4-OVR-DISTR                           
002571           PERFORM ER-SDC-SKRIV-LISTA1                                    
002572           IF BUFFERT-PRINT                                               
002573             PERFORM ES-SKRIV-EXTRA-BUFFRADER-SDC                         
002574           END-IF                                                         
002575         WHEN WDB6-A-FINNS AND A-DCS-NDC-NA                               
002576           PERFORM ET-HAEMTA-UPPG-WDK6-WDK7-NDC                           
002577           PERFORM EU-HAEMTA-UPPG-WDD8-NDC                                
002578           PERFORM EV-HAEMTA-UPPG-WDD3-NDC                                
002579           PERFORM EX-HAEMTA-NDC-WDE4-DISTR                               
002580           PERFORM EY-NDC-SKRIV-LISTA1                                    
002581         WHEN WDB6-A-FINNS AND A-DCS-NDC-PF                               
002582           PERFORM AT-HAEMTA-UPPG-WDK6-WDK7-ADC                           
002583           PERFORM AU-HAEMTA-UPPG-WDD8-ADC                                
002584           PERFORM AV-HAEMTA-UPPG-WDD3-ADC                                
002585           PERFORM AX-HAEMTA-ADC-WDE4-DISTR                               
002586           PERFORM AY-ADC-SKRIV-LISTA1                                    
002587         END-EVALUATE                                                     
002588                                                                          
002589         ADD +1 TO PRINT-ANT                                              
002590         ADD +1 TO IX                                                     
002591       END-IF                                                             
002592     END-PERFORM                                                          
002593     PERFORM EZ-STAENG-PRINTER                                            
002594     .                                                                    
002595     EJECT                                                                
002596 EA-OEPPNA-PRINTER SECTION.                                               
002597     MOVE 'EA-OEPPNA-PRINTER' TO WS-SECTION                               
002598                                                                          
002599     MOVE ALL '+'           TO MSGI-WMSGINIT                              
002600     MOVE '001'             TO MSGI-KDCALL                                
002601     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
002602     MOVE '5301'            TO MSGI-IDTRANS                               
002603     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
002604     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
002605                                                                          
002606     IF WDB6-A-FINNS                                                      
002607        IF W-IDDC = WC-SDC-NL AND MSGI-IDRT-KEY = 'ET'                    
002608           MOVE A-DCS-IDPRTLST-INVAB   TO WS-PRT1                         
002609        ELSE                                                              
002610           MOVE A-DCS-IDPRTLST-INVA    TO WS-PRT1                         
002611        END-IF                                                            
002612     ELSE                                                                 
002613        CALL FELLOG                                                       
002614     END-IF                                                               
002615     IF MID-IDPRTLST = SPACE OR LOW-VALUE                                 
002616        CONTINUE                                                          
002617     ELSE                                                                 
002618        MOVE SPACE             TO WS-PRT1                                 
002619        MOVE MID-IDPRTLST      TO WS-PRT1                                 
002620     END-IF                                                               
002621                                                                          
002622     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-OPEN WS-PRT1 ALT-PCB           
002623                                   DUMMY-AREA DUMMY-AREA                  
002624     .                                                                    
002625     EJECT                                                                
002626 EB-HAMTA-DATUM-OCH-TID SECTION.                                          
002627     MOVE 'EB-HAMTA-DATUM-OCH-TID' TO WS-SECTION                          
002628                                                                          
002629     MOVE FUNCTION CURRENT-DATE(3:6)  TO RUB-DATUM-CDC                    
002630                                         BU-DATUM-CDC                     
002631                                         RUB-DATUM-SDC                    
002632     MOVE FUNCTION CURRENT-DATE(9:4)  TO RUB-TID-CDC                      
002633                                         BU-TID-CDC                       
002634                                         RUB-TID-SDC                      
002635                                                                          
002636**  BERÄKNA DATUM OCH TID FÖR LAB                                         
002637     IF A-DCS-NDC-NA OR A-DCS-NDC-PF                                      
002638     MOVE '011'                       TO MSGI-KDCALL                      
002639     MOVE 'WIDDCXX '                  TO MSGI-IDUSER                      
002640     MOVE MSGI-IDDC                   TO MSGI-IDUSER(6:2)                 
002641     MOVE FUNCTION CURRENT-DATE(3:6)  TO MSGI-TILOKDAT                    
002642     MOVE FUNCTION CURRENT-DATE(9:4)  TO MSGI-TILOKTID                    
002643     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
002644                                                                          
002645     IF A-DCS-NDC-NA                                                      
002646       MOVE MSGI-TILOKDAT             TO RUB-DATUM-NDC                    
002647       MOVE MSGI-TILOKTID             TO RUB-TID-NDC                      
002648     END-IF                                                               
002649                                                                          
002650     IF A-DCS-NDC-PF                                                      
002651       MOVE MSGI-TILOKDAT             TO RUB-DATUM-ADC                    
002652       MOVE MSGI-TILOKTID             TO RUB-TID-ADC                      
002653     END-IF                                                               
002654     END-IF                                                               
002655     .                                                                    
002656     EJECT                                                                
002657 EC-HAEMTA-ANM-WDH1 SECTION.                                              
002658     MOVE 'EC-HAEMTA-ANM-WDH1    ' TO WS-SECTION                          
002659                                                                          
002660     MOVE SPACE                    TO L1-TEINVANM-CDC                     
002661                                      L1-TEINVANM-SDC                     
002662                                      L1-TEINVANM-ADC                     
002663                                      L1-TEINVANM-NDC                     
002664     PERFORM IMS-GHU-WDH101                                               
002665     IF SEGMENT-FINNS                                                     
002666       PERFORM IMS-GHNP-WDH111                                            
002667       IF SEGMENT-FINNS                                                   
002668****      START, FIXA TILL FÖRSTA ARTIKELNS PRINT-ID,                     
002669***       BLIR SAMMA PRINT-ID FÖR ALLA RADERNA                            
002670***       GÄLLER CDC SDC OCH NDC-PACIFIC                                  
002671           IF IX = 1                                                      
002672*--- EN DUBBLETT NÄR INV-FLINVSKR = J                                     
002673            IF INV-IDLOPNR > 0 AND INV-FLINVSKR = 'J'                     
002674              MOVE INV-IDPRTOMG   TO WS-IDPRTINV-NUM(1:1)                 
002675                                     WS-IDPRTOMG                          
002676              MOVE INV-IDLOPNR    TO WS-IDLOPNR-5                         
002677                                     WS-IDLOPNR                           
002678                                     WS-IDPRTINV-NUM(2:5)                 
002679            ELSE                                                          
002680             IF INV-IDPRTOMG = +0 OR +1 OR +2                             
002681               EVALUATE INV-IDPRTOMG                                      
002682                 WHEN +0 MOVE 1    TO WS-IDPRTINV-NUM(1:1)                
002683                                      WS-IDPRTOMG                         
002684                 WHEN +1 MOVE 2    TO WS-IDPRTINV-NUM(1:1)                
002685                                      WS-IDPRTOMG                         
002686                 WHEN +2 MOVE 3    TO WS-IDPRTINV-NUM(1:1)                
002687                                      WS-IDPRTOMG                         
002688                 WHEN +3 MOVE 4    TO WS-IDPRTINV-NUM(1:1)                
002689                                      WS-IDPRTOMG                         
002690               END-EVALUATE                                               
002691               MOVE WS-IDPRTOMG     TO W-IDPRTOMG-ALFA                    
002692                                                                          
002693****  HÄMTA NÄSTA INV-IDLOPNR FRÅN HÄNDELSEBASEN.                         
002694               PERFORM IMS-GHU-WDGX5102                                   
002695               IF SEGMENT-SAKNAS                                          
002696                 PERFORM IMS-GU-WDG301                                    
002697                 MOVE +1           TO 5102-KDSEGKEY                       
002698                 MOVE +1           TO 5102-IDLOPNR                        
002699                 MOVE 5102-IDLOPNR TO WS-IDPRTINV-NUM(2:5)                
002700                 PERFORM IMS-ISRT-WDGX5102                                
002701               ELSE                                                       
002702                 ADD +1 TO 5102-IDLOPNR                                   
002703                 MOVE 5102-IDLOPNR   TO WS-IDLOPNR-5                      
002704                                        WS-IDLOPNR                        
002705                 MOVE WS-IDLOPNR-5   TO WS-IDPRTINV-NUM(2:5)              
002706                 PERFORM IMS-REPL-WDGX5102                                
002707               END-IF                                                     
002708             ELSE                                                         
002709               MOVE ZERO           TO WS-IDPRTOMG                         
002710                                      WS-IDLOPNR                          
002711             END-IF                                                       
002712            END-IF                                                        
002713           END-IF                                                         
002714           MOVE WS-IDPRTINV-NUM    TO RUB-PRINTNR-CDC                     
002715                                      RUB-PRINTNR-SDC                     
002716                                      RUB-PRINTNR-ADC                     
002717                                      RUB-PRINTNR-NDC                     
002718****   PRINTID SLUT                                                       
002719                                                                          
002720         MOVE INV-KDINVKAT         TO RUB-KDINVKAT-SDC                    
002721                                      RUB-KDINVKAT-CDC                    
002722                                      RUB-KDINVKAT-ADC                    
002723                                      RUB-KDINVKAT-NDC                    
002724                                      BU-KDINVKAT-CDC                     
002725         MOVE INV-TISEGKEY         TO WS-TISEGKEY                         
002726         MOVE WS-TISEGKEY(3:6)     TO RUB-TIREGDAT-SDC                    
002727                                      RUB-TIREGDAT-ADC                    
002728                                      RUB-TIREGDAT-NDC                    
002729                                      RUB-TIREGDAT-CDC                    
002730                                      BU-TIREGDAT-CDC                     
002731         MOVE INV-TEINVANM         TO L1-TEINVANM-CDC                     
002732                                      L1-TEINVANM-SDC                     
002733                                      L1-TEINVANM-ADC                     
002734                                      L1-TEINVANM-NDC                     
002735         MOVE MSGI-IDDC            TO RUB-IDDC-SDC                        
002736                                      RUB-IDDC-ADC                        
002737                                      RUB-IDDC-NDC                        
002738         MOVE ZERO                 TO INV-IDLOPNR                         
002739                                      INV-KVAKS-OLD                       
002740                                      INV-KVEFRS-OLD                      
002741                                      INV-KVLS-OLD                        
002742         IF INV-FLINVSKR = 'N'                                            
002743           MOVE 'J'                TO INV-FLINVSKR                        
002744           IF A-DCS-NDC-NA                                                
002745             PERFORM IMS-REPLACE                                          
002746           END-IF                                                         
002747           MOVE SPACE              TO L1-DUBBLETT-CDC                     
002748                                      L1-DUBBLETT-SDC                     
002749         ELSE                                                             
002750           MOVE 'DUBBLETT'         TO L1-DUBBLETT-CDC                     
002751           MOVE 'DUPLICATE'        TO L1-DUBBLETT-SDC                     
002752         END-IF                                                           
002753       END-IF                                                             
002754     END-IF                                                               
002755     .                                                                    
002756     EJECT                                                                
002757 ED-NOLLA-TABELLER     SECTION.                                           
002758     MOVE 'ED-NOLLA-TABELLER     ' TO WS-SECTION                          
002759                                                                          
002760     MOVE +1 TO INDX                                                      
002761     PERFORM UNTIL INDX > MAX-IX-20                                       
002762       MOVE ZERO TO WCDC-IDKUNDRF (INDX)                                  
002763                    WCDC-IDKUNDNR (INDX)                                  
002764                    WCDC-IDDISTR  (INDX)                                  
002765                    WCDC-KVLEVART (INDX)                                  
002766       ADD +1 TO INDX                                                     
002767     END-PERFORM                                                          
002768                                                                          
002769     MOVE +1 TO INDX                                                      
002770     PERFORM UNTIL INDX > MAX-IX-7                                        
002771       MOVE ZERO TO WSDC-IDKUNDRF (INDX)                                  
002772                    WSDC-IDKUNDNR (INDX)                                  
002773                    WSDC-IDDISTR  (INDX)                                  
002774                    WSDC-KVLEVART (INDX)                                  
002775       ADD +1 TO INDX                                                     
002776     END-PERFORM                                                          
002777                                                                          
002778     MOVE +1 TO INDX                                                      
002779     PERFORM UNTIL INDX > MAX-IX-16                                       
002780       MOVE ZERO TO WADC-IDKUNDRF (INDX)                                  
002781                    WADC-IDKUNDNR (INDX)                                  
002782                    WADC-IDDISTR  (INDX)                                  
002783                    WADC-KVLEVART (INDX)                                  
002784       ADD +1 TO INDX                                                     
002785     END-PERFORM                                                          
002786                                                                          
002787     MOVE +1 TO INDX                                                      
002788     PERFORM UNTIL INDX > MAX-IX-16                                       
002789       MOVE ZERO TO WNDC-IDKUNDRF (INDX)                                  
002790                    WNDC-IDKUNDNR (INDX)                                  
002791                    WNDC-IDDISTR  (INDX)                                  
002792                    WNDC-KVLEVART (INDX)                                  
002793       ADD +1 TO INDX                                                     
002794     END-PERFORM                                                          
002795     .                                                                    
002796     EJECT                                                                
002797 EE-HAEMTA-UPPG-WDK6-CDC SECTION.                                         
002798     MOVE 'EE-HAMTA-UPPG-WDK6'     TO WS-SECTION                          
002799     MOVE +0 TO L1-ADBUFFOMR-CDC-1                                        
002800                L1-ADBUFFOMR-CDC-2                                        
002801                L1-ADBUFFOMR-CDC-3                                        
002802                L1-ADBUFFOMR-CDC-4                                        
002803                L1-ADBUFFGANG-CDC-1                                       
002804                L1-ADBUFFGANG-CDC-2                                       
002805                L1-ADBUFFGANG-CDC-3                                       
002806                L1-ADBUFFGANG-CDC-4                                       
002807                L1-ADBUFFPLATS-CDC-1                                      
002808                L1-ADBUFFPLATS-CDC-2                                      
002809                L1-ADBUFFPLATS-CDC-3                                      
002810                L1-ADBUFFPLATS-CDC-4                                      
002811                L1-QTY-CDC-1                                              
002812                L1-QTY-CDC-2                                              
002813                L1-QTY-CDC-3                                              
002814                L1-QTY-CDC-4                                              
002815     PERFORM IMS-GET-ART-WDK6                                             
002816                                                                          
002817     IF ART-FLIART = JA                                                   
002818       MOVE JA                        TO WS-SATS                          
002819     ELSE                                                                 
002820       MOVE NEJ                       TO WS-SATS                          
002821     END-IF                                                               
002822     MOVE ART-IDFKNGRP                TO L1-IDFKNGRP-CDC                  
002823     MOVE ART-KDPRODSL                TO L1-KDPRODSL-CDC                  
002824     MOVE ART-KDSORT                  TO L1-KDSORT-CDC                    
002825     MOVE ART-IDLEVNR                 TO L1-IDLEVNR-CDC                   
002826                                                                          
002827     PERFORM IMS-GNP-CLAGERINFO                                           
002828     MOVE CLAG-VKART                  TO L1-VKART-CDC                     
002829     MOVE CLAG-PRARTSTD               TO L1-PRARTSTD-CDC                  
002830     MOVE CLAG-BEFT                   TO L1-BEFT-CDC                      
002831     MOVE CLAG-KVQPACK-1              TO L1-KVQPACK-1-CDC                 
002832     MOVE CLAG-KDERS                  TO L1-KDERS-CDC                     
002833     MOVE CLAG-KVAKS-CDC              TO L1-KVAKS-CDC                     
002834                                         INV-KVAKS-OLD                    
002835     MOVE CLAG-KVLS                   TO L1-KVLS-CDC                      
002836                                         INV-KVLS-OLD                     
002837     MOVE CLAG-KVUTRS                 TO L1-KVUTRS-CDC                    
002838     MOVE CLAG-KVROS                  TO L1-KVROS-SDC                     
002839     MOVE CLAG-ADLAGOMR               TO L1-ADLAGOMR-CDC                  
002840     MOVE CLAG-ADGANG                 TO L1-ADGANG-CDC                    
002841     MOVE CLAG-ADPLATS                TO L1-ADPLATS-CDC                   
002842     IF CLAG-ADLAGOMR NOT = ZERO                                          
002843        MOVE 'CDC'                    TO L1-OMR-CDC                       
002844     END-IF                                                               
002845     MOVE CLAG-ADLAGOMR-SVS           TO L1-ADLAGOMR-SVS                  
002846     MOVE CLAG-ADGANG-SVS             TO L1-ADGANG-SVS                    
002847     MOVE CLAG-ADPLATS-SVS            TO L1-ADPLATS-SVS                   
002848     IF CLAG-ADLAGOMR-SVS NOT = ZERO                                      
002849        MOVE 'SVS'                    TO L1-OMR-SVS                       
002850     END-IF                                                               
002851     MOVE CLAG-KVLS-SVS               TO L1-KVLS-SVS                      
002852     MOVE +1               TO CD-IX                                       
002853                              RAD-IX                                      
002854     PERFORM UNTIL CD-IX > WS-ANT                                         
002855        IF CLAG-ADLAGOMR-CD(CD-IX) NOT = ZERO                             
002856           IF RAD-IX = 1                                                  
002857              MOVE 'CD '                   TO L1-OMR-CDC-1                
002858              MOVE CLAG-ADLAGOMR-CD(CD-IX) TO L1-ADBUFFOMR-CDC-1          
002859              MOVE CLAG-ADGANG-CD(CD-IX)   TO L1-ADBUFFGANG-CDC-1         
002860              MOVE CLAG-ADPLATS-CD(CD-IX)  TO L1-ADBUFFPLATS-CDC-1        
002861              MOVE CLAG-KVLS-CD(CD-IX)     TO L1-QTY-CDC-1                
002862           END-IF                                                         
002863           IF RAD-IX = 2                                                  
002864              MOVE 'CD '                   TO L1-OMR-CDC-2                
002865              MOVE CLAG-ADLAGOMR-CD(CD-IX) TO L1-ADBUFFOMR-CDC-2          
002866              MOVE CLAG-ADGANG-CD(CD-IX)   TO L1-ADBUFFGANG-CDC-2         
002867              MOVE CLAG-ADPLATS-CD(CD-IX)  TO L1-ADBUFFPLATS-CDC-2        
002868              MOVE CLAG-KVLS-CD(CD-IX)     TO L1-QTY-CDC-2                
002869           END-IF                                                         
002870           IF RAD-IX = 3                                                  
002871              MOVE 'CD '                   TO L1-OMR-CDC-3                
002872              MOVE CLAG-ADLAGOMR-CD(CD-IX) TO L1-ADBUFFOMR-CDC-3          
002873              MOVE CLAG-ADGANG-CD(CD-IX)   TO L1-ADBUFFGANG-CDC-3         
002874              MOVE CLAG-ADPLATS-CD(CD-IX)  TO L1-ADBUFFPLATS-CDC-3        
002875              MOVE CLAG-KVLS-CD(CD-IX)     TO L1-QTY-CDC-3                
002876           END-IF                                                         
002877           IF RAD-IX = 4                                                  
002878              MOVE 'CD '                   TO L1-OMR-CDC-4                
002879              MOVE CLAG-ADLAGOMR-CD(CD-IX) TO L1-ADBUFFOMR-CDC-4          
002880              MOVE CLAG-ADGANG-CD(CD-IX)   TO L1-ADBUFFGANG-CDC-4         
002881              MOVE CLAG-ADPLATS-CD(CD-IX)  TO L1-ADBUFFPLATS-CDC-4        
002882              MOVE CLAG-KVLS-CD(CD-IX)     TO L1-QTY-CDC-4                
002883           END-IF                                                         
002884           ADD +1 TO RAD-IX                                               
002885       END-IF                                                             
002886       ADD +1 TO CD-IX                                                    
002887     END-PERFORM                                                          
002888                                                                          
002889     MOVE CLAG-KVAKS-CDC                   TO INV-KVAKS-OLD               
002890     MOVE CLAG-KVLS                        TO INV-KVLS-OLD                
002891     MOVE WS-IDLOPNR                       TO INV-IDLOPNR                 
002892     IF WS-IDPRTOMG NOT = INV-IDPRTOMG                                    
002893       MOVE FUNCTION CURRENT-DATE(1:8) TO INV-DAREGDAT                    
002894       EVALUATE WS-IDPRTOMG                                               
002895         WHEN 1                                                           
002896           MOVE FUNCTION CURRENT-DATE(1:8) TO INV-DAREGDAT-PR1            
002897         WHEN 2                                                           
002898           MOVE FUNCTION CURRENT-DATE(1:8) TO INV-DAREGDAT-PR2            
002899         WHEN 3                                                           
002900           MOVE FUNCTION CURRENT-DATE(1:8) TO INV-DAREGDAT-PR3            
002901       END-EVALUATE                                                       
002902     END-IF                                                               
002903     MOVE WS-IDPRTOMG                      TO INV-IDPRTOMG                
002904     MOVE ZERO                             TO WS-IDBENR                   
002905     .                                                                    
002906     EJECT                                                                
002907 EF-HAEMTA-UPPG-INLEV SECTION.                                            
002908     MOVE 'EF-HAMTA-UPPG-INLEV'    TO WS-SECTION                          
002909     MOVE ZERO                        TO L1-TIJUSTDA-1-CDC                
002910                                         L1-TIJUSTDA-2-CDC                
002911                                         L1-TIJUSTDA-3-CDC                
002912                                         L1-KVJUSTKV-1-CDC                
002913                                         L1-KVJUSTKV-2-CDC                
002914                                         L1-KVJUSTKV-3-CDC                
002915                                         L1-KDJUSTYP-1-CDC                
002916                                         L1-KDJUSTYP-2-CDC                
002917                                         L1-KDJUSTYP-3-CDC                
002918     MOVE SPACE                       TO L1-KDJUSTYP-1A-CDC               
002919                                         L1-KDJUSTYP-2A-CDC               
002920                                         L1-KDJUSTYP-3A-CDC               
002921     PERFORM IMS-GET-INVHIST-ROT                                          
002922     IF SEGMENT-FINNS                                                     
002923       MOVE +1 TO IND                                                     
002924       PERFORM IMS-GET-INVHIST-CDCFIRST                                   
002925       PERFORM UNTIL IND > +3 OR SEGMENT-SAKNAS                           
002926         PERFORM S10-KONV-DATUM                                           
002927         MOVE DAT-TIAAVVD             TO WS-TIAAVVD                       
002928                                                                          
002929         EVALUATE IND                                                     
002930           WHEN +1                                                        
002931             MOVE WS-TIAAVVD          TO L1-TIJUSTDA-1-CDC                
002932             MOVE INVH-KVJUSTKV       TO L1-KVJUSTKV-1-CDC                
002933             MOVE INVH-KDJUSTYP       TO L1-KDJUSTYP-1-CDC                
002934             IF INVH-FLAUTLSJ = 'J'                                       
002935               MOVE 'A'               TO L1-KDJUSTYP-1A-CDC               
002936             ELSE                                                         
002937               MOVE ' '               TO L1-KDJUSTYP-1A-CDC               
002938             END-IF                                                       
002939           WHEN +2                                                        
002940             MOVE WS-TIAAVVD          TO L1-TIJUSTDA-2-CDC                
002941             MOVE INVH-KVJUSTKV       TO L1-KVJUSTKV-2-CDC                
002942             MOVE INVH-KDJUSTYP       TO L1-KDJUSTYP-2-CDC                
002943             IF INVH-FLAUTLSJ = 'J'                                       
002944               MOVE 'A'               TO L1-KDJUSTYP-2A-CDC               
002945             ELSE                                                         
002946               MOVE ' '               TO L1-KDJUSTYP-2A-CDC               
002947             END-IF                                                       
002948           WHEN +3                                                        
002949             MOVE WS-TIAAVVD          TO L1-TIJUSTDA-3-CDC                
002950             MOVE INVH-KVJUSTKV       TO L1-KVJUSTKV-3-CDC                
002951             MOVE INVH-KDJUSTYP       TO L1-KDJUSTYP-3-CDC                
002952             IF INVH-FLAUTLSJ = 'J'                                       
002953               MOVE 'A'               TO L1-KDJUSTYP-3A-CDC               
002954             ELSE                                                         
002955               MOVE ' '               TO L1-KDJUSTYP-3A-CDC               
002956             END-IF                                                       
002957         END-EVALUATE                                                     
002958                                                                          
002959         PERFORM IMS-GET-INVHIST-CDC                                      
002960         ADD +1 TO IND                                                    
002961       END-PERFORM                                                        
002962     END-IF                                                               
002963     .                                                                    
002964 EG-HAEMTA-UPPG-SATS SECTION.                                             
002965     MOVE 'EG-HAMTA-UPP-SATS'      TO WS-SECTION                          
002966     MOVE +1 TO TAB-IX                                                    
002967     IF SATS-FINNS                                                        
002968       MOVE 'JA'                         TO L1-KDIART-CDC                 
002969       MOVE W-IDARTNR                    TO W-IDARTNR-WDJ1C               
002970       PERFORM IMS-GU-SATB11-SATB01                                       
002971     ELSE                                                                 
002972       MOVE 'NEJ'                        TO L1-KDIART-CDC                 
002973       MOVE 'GE'                         TO STATUS-WS                     
002974     END-IF                                                               
002975     PERFORM UNTIL TAB-IX > +10                                           
002976       IF SEGMENT-FINNS                                                   
002977         MOVE SATB11-RAD-TISTODAT        TO TMP1-YYMMDD                   
002978         MOVE FUNCTION CURRENT-DATE(3:6) TO TMP2-YYMMDD                   
002979         PERFORM WY2000P1                                                 
002980         IF SATB01-STR-TIBORT = ZERO AND                                  
002981            TMP1-YYMMDD       > TMP2-YYMMDD                               
002982            MOVE SATB01-STR-IDARTNR    TO TAB-SATS-IDARTNR(TAB-IX)        
002983            ADD +1 TO TAB-IX                                              
002984         END-IF                                                           
002985         PERFORM IMS-GN-SATB11-SATB01                                     
002986       ELSE                                                               
002987         MOVE ZERO                     TO TAB-SATS-IDARTNR(TAB-IX)        
002988         ADD +1 TO TAB-IX                                                 
002989       END-IF                                                             
002990     END-PERFORM                                                          
002991     .                                                                    
002992     EJECT                                                                
002993 EH-HAEMTA-UPPG-WDD8-CDC SECTION.                                         
002994     MOVE 'EH-HAMTA-UPPG-WDD8'     TO WS-SECTION                          
002995     PERFORM EHA-NOLLSTAELL-CDC                                           
002996     MOVE ZERO                    TO WS-SUM-BUFFR                         
002997     MOVE NEJ                     TO BUFFERT-SW                           
002998     MOVE 'NEJ'                   TO L1-FLBUFF-CDC                        
002999     MOVE MSGI-IDDC               TO W-IDDC-D8-MIN                        
003000                                     W-IDDC-D8-MAX                        
003001     MOVE RAD-IX TO SALDO-IX                                              
003002     PERFORM IMS-GU-WDD8B1                                                
003003     PERFORM UNTIL SEGMENT-SAKNAS                                         
003010       IF SALDO-IX <= WS-ANT                                              
003017                                                                          
003018         EVALUATE SALDO-IX                                                
003019            WHEN +1                                                       
003020             MOVE SPACE                 TO L1-OMR-CDC-1                   
003021             MOVE SEQB-ADBUFFOMR        TO L1-ADBUFFOMR-CDC-1             
003022             MOVE SEQB-ADBUFFGANG       TO L1-ADBUFFGANG-CDC-1            
003023             MOVE SEQB-ADBUFFPL         TO L1-ADBUFFPLATS-CDC-1           
003024             COMPUTE WS-QTY-CDC = SEQB-KVBUFF-F +                         
003025                                  SEQB-KVBUFF-OF                          
003026             MOVE WS-QTY-CDC            TO L1-QTY-CDC-1                   
003027             ADD WS-QTY-CDC             TO WS-QTY-CDC-TOT                 
003028             ADD +1 TO WS-SUM-BUFFR                                       
003029                                                                          
003030            WHEN +2                                                       
003031             MOVE SPACE                 TO L1-OMR-CDC-2                   
003032             MOVE SEQB-ADBUFFOMR        TO L1-ADBUFFOMR-CDC-2             
003033             MOVE SEQB-ADBUFFGANG       TO L1-ADBUFFGANG-CDC-2            
003034             MOVE SEQB-ADBUFFPL         TO L1-ADBUFFPLATS-CDC-2           
003035             COMPUTE WS-QTY-CDC = SEQB-KVBUFF-F +                         
003036                                  SEQB-KVBUFF-OF                          
003037             MOVE WS-QTY-CDC            TO L1-QTY-CDC-2                   
003038             ADD WS-QTY-CDC             TO WS-QTY-CDC-TOT                 
003039             ADD +1 TO WS-SUM-BUFFR                                       
003040                                                                          
003041            WHEN +3                                                       
003042             MOVE SPACE                 TO L1-OMR-CDC-3                   
003043             MOVE SEQB-ADBUFFOMR        TO L1-ADBUFFOMR-CDC-3             
003044             MOVE SEQB-ADBUFFGANG       TO L1-ADBUFFGANG-CDC-3            
003045             MOVE SEQB-ADBUFFPL         TO L1-ADBUFFPLATS-CDC-3           
003046             COMPUTE WS-QTY-CDC = SEQB-KVBUFF-F +                         
003047                                  SEQB-KVBUFF-OF                          
003048             MOVE WS-QTY-CDC            TO L1-QTY-CDC-3                   
003049             ADD WS-QTY-CDC             TO WS-QTY-CDC-TOT                 
003050             ADD +1 TO WS-SUM-BUFFR                                       
003051                                                                          
003052            WHEN +4                                                       
003053             MOVE SPACE                 TO L1-OMR-CDC-4                   
003054             MOVE SEQB-ADBUFFOMR        TO L1-ADBUFFOMR-CDC-4             
003055             MOVE SEQB-ADBUFFGANG       TO L1-ADBUFFGANG-CDC-4            
003056             MOVE SEQB-ADBUFFPL         TO L1-ADBUFFPLATS-CDC-4           
003057             COMPUTE WS-QTY-CDC = SEQB-KVBUFF-F +                         
003058                                  SEQB-KVBUFF-OF                          
003059             MOVE WS-QTY-CDC            TO L1-QTY-CDC-4                   
003060             ADD WS-QTY-CDC             TO WS-QTY-CDC-TOT                 
003061             ADD +1 TO WS-SUM-BUFFR                                       
003062                                                                          
003063         END-EVALUATE                                                     
003064       ELSE                                                               
003072         COMPUTE WS-QTY-CDC-TOT = WS-QTY-CDC-TOT +                        
003073           SEQB-KVBUFF-F + SEQB-KVBUFF-OF                                 
003074         MOVE JA                TO BUFFERT-SW                             
003075         MOVE 'JA '             TO L1-FLBUFF-CDC                          
003076       END-IF                                                             
003077       ADD +1 TO SALDO-IX                                                 
003078       PERFORM IMS-GN-WDD8B1                                              
003079     END-PERFORM                                                          
003080     MOVE WS-QTY-CDC-TOT        TO L1-TOT-QTY-CDC                         
003081     .                                                                    
003082     EJECT                                                                
003083 EHA-NOLLSTAELL-CDC     SECTION.                                          
003084     MOVE 'EHA-NOLLSTAÄLL-CDC'     TO WS-SECTION                          
003085     MOVE +0 TO  WS-QTY-CDC                                               
003086                WS-QTY-CDC-TOT                                            
003087                L1-TOT-QTY-CDC                                            
003088     .                                                                    
003089     EJECT                                                                
003090 EI-HAEMTA-UPPG-WDD3-CDC SECTION.                                         
003091     MOVE 'EI-HAMTA-UPPG-WDD3-CDC' TO WS-SECTION                          
003092     MOVE 'S  '                TO W-IDSKYLT                               
003093     PERFORM IMS-GU-BEN-SEQ                                               
003094     IF SEGMENT-FINNS                                                     
003095       MOVE BEN-TEXT-BEART     TO L1-BEART-CDC                            
003096                                  BU-BEART-CDC                            
003097     ELSE                                                                 
003098       MOVE SPACE              TO L1-BEART-CDC                            
003099                                  BU-BEART-CDC                            
003100     END-IF                                                               
003101     .                                                                    
003102     SKIP2                                                                
003103 EJ-HAEMTA-CDC-WDE4-OVR-DISTR SECTION.                                    
003104     MOVE 'EJ-HAMTA-CDC-WDE4OVR' TO WS-SECTION                            
003105     MOVE 'NEJ'                             TO L1-FLEFR-CDC               
003106     MOVE +0                                TO WCDC-KVLEVART-TOT          
003107     MOVE +0                                TO WS-ANTAL-RADER-EFR         
003108     MOVE +0                                TO WS-KVEFRS-OLD              
003109     MOVE +1 TO INDX                                                      
003110     PERFORM IMS-GU-WDE4C1                                                
003111                                                                          
003112     PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                           
003113       MOVE SEQC-IDDISTR   TO W-401-IDDISTR                               
003114       MOVE SEQC-IDKUNDNR  TO W-401-IDKUNDNR                              
003115       MOVE SEQC-IDORDNR5  TO W-401-IDORDNR                               
003116       MOVE SEQC-IDPRODNR  TO W-401-IDPRODNR                              
003117       MOVE SEQC-IDPLKLST  TO W-401-IDPLKLST                              
003118       MOVE SEQC-IDPURAD   TO W-411-IDPURAD                               
003119       PERFORM IMS-GU-WDE401-11                                           
003120                                                                          
003121       IF ORAD-FLDIRLEV NOT = 'J'                                         
003122        IF KORD-IDDC = MID-IDDC                                           
003123         IF KORD-IDDISTR NOT = +98                                        
003124          IF INDX <= MAX-IX-20                                            
003125             MOVE KORD-IDKUNDRF        TO WS-IDKUNDRF                     
003126             MOVE WS-IDORDNR           TO WCDC-IDKUNDRF(INDX)             
003127             MOVE ORAD-KVAVBART        TO WCDC-KVLEVART(INDX)             
003128             SUBTRACT ORAD-KVLEVART FROM  WCDC-KVLEVART(INDX)             
003129             MOVE KORD-IDDISTR         TO WCDC-IDDISTR (INDX)             
003130             MOVE KORD-IDKUNDNR        TO WCDC-IDKUNDNR(INDX)             
003131             ADD +1  TO INDX                                              
003132          ELSE                                                            
003133            MOVE 'JA '                      TO L1-FLEFR-CDC               
003134          END-IF                                                          
003135          COMPUTE WCDC-KVLEVART-TOT = WCDC-KVLEVART-TOT +                 
003136                       ORAD-KVAVBART - ORAD-KVLEVART                      
003137          ADD +1 TO WS-ANTAL-RADER-EFR                                    
003138         END-IF                                                           
003139        END-IF                                                            
003140       END-IF                                                             
003141       PERFORM IMS-GN-WDE4C1                                              
003142     END-PERFORM                                                          
003143                                                                          
003144     MOVE WCDC-KVLEVART-TOT TO WS-KVEFRS-OLD                              
003145     PERFORM EJA-FLYTTA-TILL-CDC-LISTA                                    
003146     .                                                                    
003147     EJECT                                                                
003148 EJA-FLYTTA-TILL-CDC-LISTA SECTION.                                       
003149     MOVE 'EJA-FLYTTA-TILL-CDC   ' TO WS-SECTION                          
003150     MOVE WCDC-IDKUNDRF(01)  TO L1-IDKUNDRF-CDC-1                         
003151     MOVE WCDC-IDKUNDRF(02)  TO L1-IDKUNDRF-CDC-2                         
003152     MOVE WCDC-IDKUNDRF(03)  TO L1-IDKUNDRF-CDC-3                         
003153     MOVE WCDC-IDKUNDRF(04)  TO L1-IDKUNDRF-CDC-4                         
003154     MOVE WCDC-IDKUNDRF(05)  TO L1-IDKUNDRF-CDC-5                         
003155     MOVE WCDC-IDKUNDRF(06)  TO L1-IDKUNDRF-CDC-6                         
003156     MOVE WCDC-IDKUNDRF(07)  TO L1-IDKUNDRF-CDC-7                         
003157     MOVE WCDC-IDKUNDRF(08)  TO L1-IDKUNDRF-CDC-8                         
003158     MOVE WCDC-IDKUNDRF(09)  TO L1-IDKUNDRF-CDC-9                         
003159     MOVE WCDC-IDKUNDRF(10)  TO L1-IDKUNDRF-CDC-10                        
003160     MOVE WCDC-IDKUNDRF(11)  TO L1-IDKUNDRF-CDC-11                        
003161     MOVE WCDC-IDKUNDRF(12)  TO L1-IDKUNDRF-CDC-12                        
003162     MOVE WCDC-IDKUNDRF(13)  TO L1-IDKUNDRF-CDC-13                        
003163     MOVE WCDC-IDKUNDRF(14)  TO L1-IDKUNDRF-CDC-14                        
003164     MOVE WCDC-IDKUNDRF(15)  TO L1-IDKUNDRF-CDC-15                        
003165     MOVE WCDC-IDKUNDRF(16)  TO L1-IDKUNDRF-CDC-16                        
003166     MOVE WCDC-IDKUNDRF(17)  TO L1-IDKUNDRF-CDC-17                        
003167     MOVE WCDC-IDKUNDRF(18)  TO L1-IDKUNDRF-CDC-18                        
003168     MOVE WCDC-IDKUNDRF(19)  TO L1-IDKUNDRF-CDC-19                        
003169     MOVE WCDC-IDKUNDRF(20)  TO L1-IDKUNDRF-CDC-20                        
003170                                                                          
003171     MOVE WCDC-IDKUNDNR(01)  TO L1-IDKUNDNR-CDC-1                         
003172     MOVE WCDC-IDKUNDNR(02)  TO L1-IDKUNDNR-CDC-2                         
003173     MOVE WCDC-IDKUNDNR(03)  TO L1-IDKUNDNR-CDC-3                         
003174     MOVE WCDC-IDKUNDNR(04)  TO L1-IDKUNDNR-CDC-4                         
003175     MOVE WCDC-IDKUNDNR(05)  TO L1-IDKUNDNR-CDC-5                         
003176     MOVE WCDC-IDKUNDNR(06)  TO L1-IDKUNDNR-CDC-6                         
003177     MOVE WCDC-IDKUNDNR(07)  TO L1-IDKUNDNR-CDC-7                         
003178     MOVE WCDC-IDKUNDNR(08)  TO L1-IDKUNDNR-CDC-8                         
003179     MOVE WCDC-IDKUNDNR(09)  TO L1-IDKUNDNR-CDC-9                         
003180     MOVE WCDC-IDKUNDNR(10)  TO L1-IDKUNDNR-CDC-10                        
003181     MOVE WCDC-IDKUNDNR(11)  TO L1-IDKUNDNR-CDC-11                        
003182     MOVE WCDC-IDKUNDNR(12)  TO L1-IDKUNDNR-CDC-12                        
003183     MOVE WCDC-IDKUNDNR(13)  TO L1-IDKUNDNR-CDC-13                        
003184     MOVE WCDC-IDKUNDNR(14)  TO L1-IDKUNDNR-CDC-14                        
003185     MOVE WCDC-IDKUNDNR(15)  TO L1-IDKUNDNR-CDC-15                        
003186     MOVE WCDC-IDKUNDNR(16)  TO L1-IDKUNDNR-CDC-16                        
003187     MOVE WCDC-IDKUNDNR(17)  TO L1-IDKUNDNR-CDC-17                        
003188     MOVE WCDC-IDKUNDNR(18)  TO L1-IDKUNDNR-CDC-18                        
003189     MOVE WCDC-IDKUNDNR(19)  TO L1-IDKUNDNR-CDC-19                        
003190     MOVE WCDC-IDKUNDNR(20)  TO L1-IDKUNDNR-CDC-20                        
003191                                                                          
003192     MOVE WCDC-IDDISTR(01)   TO L1-IDDISTR-CDC-1                          
003193     MOVE WCDC-IDDISTR(02)   TO L1-IDDISTR-CDC-2                          
003194     MOVE WCDC-IDDISTR(03)   TO L1-IDDISTR-CDC-3                          
003195     MOVE WCDC-IDDISTR(04)   TO L1-IDDISTR-CDC-4                          
003196     MOVE WCDC-IDDISTR(05)   TO L1-IDDISTR-CDC-5                          
003197     MOVE WCDC-IDDISTR(06)   TO L1-IDDISTR-CDC-6                          
003198     MOVE WCDC-IDDISTR(07)   TO L1-IDDISTR-CDC-7                          
003199     MOVE WCDC-IDDISTR(08)   TO L1-IDDISTR-CDC-8                          
003200     MOVE WCDC-IDDISTR(09)   TO L1-IDDISTR-CDC-9                          
003201     MOVE WCDC-IDDISTR(10)   TO L1-IDDISTR-CDC-10                         
003202     MOVE WCDC-IDDISTR(11)   TO L1-IDDISTR-CDC-11                         
003203     MOVE WCDC-IDDISTR(12)   TO L1-IDDISTR-CDC-12                         
003204     MOVE WCDC-IDDISTR(13)   TO L1-IDDISTR-CDC-13                         
003205     MOVE WCDC-IDDISTR(14)   TO L1-IDDISTR-CDC-14                         
003206     MOVE WCDC-IDDISTR(15)   TO L1-IDDISTR-CDC-15                         
003207     MOVE WCDC-IDDISTR(16)   TO L1-IDDISTR-CDC-16                         
003208     MOVE WCDC-IDDISTR(17)   TO L1-IDDISTR-CDC-17                         
003209     MOVE WCDC-IDDISTR(18)   TO L1-IDDISTR-CDC-18                         
003210     MOVE WCDC-IDDISTR(19)   TO L1-IDDISTR-CDC-19                         
003211     MOVE WCDC-IDDISTR(20)   TO L1-IDDISTR-CDC-20                         
003212                                                                          
003213     MOVE WCDC-KVLEVART(01)  TO L1-KVLEVART-CDC-1                         
003214     MOVE WCDC-KVLEVART(02)  TO L1-KVLEVART-CDC-2                         
003215     MOVE WCDC-KVLEVART(03)  TO L1-KVLEVART-CDC-3                         
003216     MOVE WCDC-KVLEVART(04)  TO L1-KVLEVART-CDC-4                         
003217     MOVE WCDC-KVLEVART(05)  TO L1-KVLEVART-CDC-5                         
003218     MOVE WCDC-KVLEVART(06)  TO L1-KVLEVART-CDC-6                         
003219     MOVE WCDC-KVLEVART(07)  TO L1-KVLEVART-CDC-7                         
003220     MOVE WCDC-KVLEVART(08)  TO L1-KVLEVART-CDC-8                         
003221     MOVE WCDC-KVLEVART(09)  TO L1-KVLEVART-CDC-9                         
003222     MOVE WCDC-KVLEVART(10)  TO L1-KVLEVART-CDC-10                        
003223     MOVE WCDC-KVLEVART(11)  TO L1-KVLEVART-CDC-11                        
003224     MOVE WCDC-KVLEVART(12)  TO L1-KVLEVART-CDC-12                        
003225     MOVE WCDC-KVLEVART(13)  TO L1-KVLEVART-CDC-13                        
003226     MOVE WCDC-KVLEVART(14)  TO L1-KVLEVART-CDC-14                        
003227     MOVE WCDC-KVLEVART(15)  TO L1-KVLEVART-CDC-15                        
003228     MOVE WCDC-KVLEVART(16)  TO L1-KVLEVART-CDC-16                        
003229     MOVE WCDC-KVLEVART(17)  TO L1-KVLEVART-CDC-17                        
003230     MOVE WCDC-KVLEVART(18)  TO L1-KVLEVART-CDC-18                        
003231     MOVE WCDC-KVLEVART(19)  TO L1-KVLEVART-CDC-19                        
003232     MOVE WCDC-KVLEVART(20)  TO L1-KVLEVART-CDC-20                        
003233     MOVE WCDC-KVLEVART-TOT  TO L1-TOTQTY-EFR-CDC                         
003234     .                                                                    
003235 EK-HAMTA-UPPG-WDE4-DISTR-98 SECTION.                                     
003236****    SATSORDER-RADER                                                   
003237     MOVE 'EK-HAMTA-UPPG-WDE4    ' TO WS-SECTION                          
003238     PERFORM EKA-INITIERA-SATSRAD                                         
003239     MOVE +98                              TO W-IDDISTR                   
003240                                              W-IDDISTR-WDE4              
003241     PERFORM IMS-GU-WDE4C-DISTR-98                                        
003242     MOVE +1 TO IND                                                       
003243     PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                           
003244       MOVE SEQC-IDKUNDNR                  TO W-IDKUNDNR                  
003245       MOVE SEQC-IDKUNDRF                  TO W-IDKUNDRF                  
003246                                              WS-IDKUNDRF                 
003247       MOVE SEQC-IDPRODNR                  TO W-IDPRODNR                  
003248       MOVE SEQC-IDPLKLST                  TO W-IDPLKLST                  
003249       MOVE SEQC-IDPURAD                   TO W-IDPURAD                   
003250       PERFORM IMS-GU-WDE4-ROT                                            
003251       MOVE KORD-IDDC                      TO W-IDDC-B6                   
003252                                                                          
003253       MOVE KORD-IDARTNR-SATS              TO WS-IDARTNR-SATS             
003254       PERFORM IMS-GNP-KUNDORDERRAD                                       
003255       IF ORAD-FLDIRLEV NOT = 'J'                                         
003256         PERFORM IMS-GU-WDB601-B                                          
003257         IF B-DCS-CDC OR B-DCS-CDC-TR                                     
003258           IF ORAD-KDRADSTA < +4                                          
003259            IF IND < 7                                                    
003260             MOVE WS-IDORDNR               TO WSATS-IDORDNR(IND)          
003261             MOVE +98                      TO WSATS-IDDISTR(IND)          
003262             MOVE WS-IDARTNR-SATS          TO WSATS-SATSNR (IND)          
003263             MOVE ORAD-KVAVBART            TO WSATS-KVLEVART(IND)         
003264             SUBTRACT ORAD-KVLEVART FROM WSATS-KVLEVART(IND)              
003265            END-IF                                                        
003266            ADD +1 TO IND                                                 
003267           END-IF                                                         
003268           COMPUTE WSATS-KVLEVART-TOT = WSATS-KVLEVART-TOT +              
003269                        ORAD-KVAVBART - ORAD-KVLEVART                     
003270           ADD +1 TO WS-ANTAL-RADER-EFR                                   
003271         END-IF                                                           
003272       END-IF                                                             
003273       PERFORM IMS-GN-WDE4C-DISTR-98                                      
003274     END-PERFORM                                                          
003275*--- NU RAKNAR VI UT DEN TOTALA  EFR SOM FINNS MED STATUS N               
003276*    FÖR ATT SEDAN UPPDATERA WDH1 BASEN                                   
003277     COMPUTE WS-KVEFRS-OLD   = WS-KVEFRS-OLD   +                          
003278                               WSATS-KVLEVART-TOT                         
003279     IF WS-ANTAL-RADER-EFR > 1                                            
003280       COMPUTE INV-KVEFRS-OLD = WS-KVEFRS-OLD / 2                         
003281     ELSE                                                                 
003282       MOVE WS-KVEFRS-OLD   TO INV-KVEFRS-OLD                             
003283     END-IF                                                               
003284                                                                          
003285**** TA BORT POSTEN PÅ WDH1 OCH SKAPA SEDAN ETT NYTT MED                  
003286**** DAGENS DATUM I DAREGDAT-SORT OM DET ÄR FÖRSTA PRINTNINGEN            
003287     IF WS-IDPRTOMG = 1                                                   
003288       PERFORM IMS-GNP-WDH121                                             
003289       IF SEGMENT-FINNS                                                   
003290         IF INVL-KDSEGKEY = 0                                             
003291           MOVE INVL-IDUSER     TO WS-IDUSER                              
003292         END-IF                                                           
003293       END-IF                                                             
003294       PERFORM IMS-GHU-WDH101                                             
003295       MOVE DLI-IO-AREA-INVA11  TO SPAR-WDH111-AREA                       
003296       PERFORM IMS-GHNP-WDH111                                            
003297                                                                          
003298       PERFORM IMS-DELETE-WDH111                                          
003299       MOVE WS-DAGENS-DATUM     TO  SPAR-INV-DAREGDAT-SORT                
003300                                    SPAR-INV-DAREGDAT-PR1                 
003301       PERFORM IMS-GHU-WDH101                                             
003302       MOVE SPAR-WDH111-AREA TO DLI-IO-AREA-INVA11                        
003303       PERFORM IMS-INSERT-WDH111                                          
003304       IF SEGMENT-FINNS-REDAN                                             
003305         PERFORM UNTIL SEGMENT-FINNS                                      
003306           ADD +1 TO INV-TISEGKEY                                         
003307           PERFORM IMS-INSERT-WDH111                                      
003308         END-PERFORM                                                      
003309       END-IF                                                             
003310       MOVE '0'            TO INVL-KDSEGKEY                               
003311       MOVE WS-IDUSER      TO INVL-IDUSER                                 
003312       PERFORM IMS-INSERT-WDH121                                          
003313       MOVE '1'            TO INVL-KDSEGKEY                               
003314       MOVE MID-IDUSER     TO INVL-IDUSER                                 
003315       PERFORM IMS-INSERT-WDH121                                          
003316       MOVE '2'            TO INVL-KDSEGKEY                               
003317       MOVE SPACE          TO INVL-IDUSER                                 
003318       PERFORM IMS-INSERT-WDH121                                          
003319       MOVE '3'            TO INVL-KDSEGKEY                               
003320       MOVE SPACE          TO INVL-IDUSER                                 
003321       PERFORM IMS-INSERT-WDH121                                          
003322     ELSE                                                                 
003323****** REPLACE PÅ WDH111 ****                                             
003324       PERFORM IMS-REPLACE                                                
003325       MOVE 'GJORT REPL PÅ WDH1 EK ' TO WS-SECTION                        
003326       PERFORM IMS-GNP-WDH121                                             
003327       MOVE 'GNP  PÅ WDH121 EK' TO WS-SECTION                             
003328       IF SEGMENT-FINNS                                                   
003329         PERFORM UNTIL SEGMENT-SAKNAS                                     
003330           IF W-IDPRTOMG-ALFA  = INVL-KDSEGKEY                            
003331             MOVE MID-IDUSER         TO INVL-IDUSER                       
003332             PERFORM IMS-REPLACE-WDH121                                   
003333           END-IF                                                         
003334           PERFORM IMS-GNP-WDH121                                         
003335         END-PERFORM                                                      
003336       END-IF                                                             
003337                                                                          
003338***---                                                                    
003339     END-IF                                                               
003340                                                                          
003341                                                                          
003342                                                                          
003343     MOVE WSATS-IDDISTR(1)       TO L1-IDDISTR-CDC-SO-1                   
003344     MOVE WSATS-IDDISTR(2)       TO L1-IDDISTR-CDC-SO-2                   
003345     MOVE WSATS-IDDISTR(3)       TO L1-IDDISTR-CDC-SO-3                   
003346     MOVE WSATS-IDDISTR(4)       TO L1-IDDISTR-CDC-SO-4                   
003347     MOVE WSATS-IDDISTR(5)       TO L1-IDDISTR-CDC-SO-5                   
003348     MOVE WSATS-IDDISTR(6)       TO L1-IDDISTR-CDC-SO-6                   
003349     MOVE WSATS-SATSNR(1)        TO L1-SATSNR-CDC-SO-1                    
003350     MOVE WSATS-SATSNR(2)        TO L1-SATSNR-CDC-SO-2                    
003351     MOVE WSATS-SATSNR(3)        TO L1-SATSNR-CDC-SO-3                    
003352     MOVE WSATS-SATSNR(4)        TO L1-SATSNR-CDC-SO-4                    
003353     MOVE WSATS-SATSNR(5)        TO L1-SATSNR-CDC-SO-5                    
003354     MOVE WSATS-SATSNR(6)        TO L1-SATSNR-CDC-SO-6                    
003355     MOVE WSATS-KVLEVART(1)      TO L1-KVLEVART-CDC-SO-1                  
003356     MOVE WSATS-KVLEVART(2)      TO L1-KVLEVART-CDC-SO-2                  
003357     MOVE WSATS-KVLEVART(3)      TO L1-KVLEVART-CDC-SO-3                  
003358     MOVE WSATS-KVLEVART(4)      TO L1-KVLEVART-CDC-SO-4                  
003359     MOVE WSATS-KVLEVART(5)      TO L1-KVLEVART-CDC-SO-5                  
003360     MOVE WSATS-KVLEVART(6)      TO L1-KVLEVART-CDC-SO-6                  
003361     MOVE WSATS-KVLEVART-TOT     TO L1-KVLEVART-CDC-SO-TOT                
003362     MOVE WSATS-IDORDNR(1)       TO L1-IDORDNR-CDC-SO-1                   
003363     MOVE WSATS-IDORDNR(2)       TO L1-IDORDNR-CDC-SO-2                   
003364     MOVE WSATS-IDORDNR(3)       TO L1-IDORDNR-CDC-SO-3                   
003365     MOVE WSATS-IDORDNR(4)       TO L1-IDORDNR-CDC-SO-4                   
003366     MOVE WSATS-IDORDNR(5)       TO L1-IDORDNR-CDC-SO-5                   
003367     MOVE WSATS-IDORDNR(6)       TO L1-IDORDNR-CDC-SO-6                   
003368     .                                                                    
003369     EJECT                                                                
003370 EKA-INITIERA-SATSRAD SECTION.                                            
003371     MOVE 'EKA-INIT-SATS        '  TO WS-SECTION                          
003372     MOVE +1 TO IND                                                       
003373                                                                          
003374     PERFORM UNTIL IND > +6                                               
003375        MOVE +0    TO WSATS-IDORDNR  (IND)                                
003376                      WSATS-KVLEVART (IND)                                
003377                      WSATS-SATSNR   (IND)                                
003378                      WSATS-IDDISTR  (IND)                                
003379        ADD +1 TO IND                                                     
003380     END-PERFORM                                                          
003381     MOVE +0       TO WSATS-KVLEVART-TOT                                  
003382     .                                                                    
003383     EJECT                                                                
003384 EL-CDC-SKRIV-LISTA1 SECTION.                                             
003385      MOVE 'EL-SKRIVLISTA         ' TO WS-SECTION                         
003386**********************  SKRIVER CDC-LISTAN   *******************          
003387                                                                          
003388* SKRIVER RUBRIKER                                                        
003389                                                                          
003390     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
003391                         PRT-NYSIDA-RAD2 CDC-RUBRAD                       
003392                                                                          
003393* SKRIVER RADER                                                           
003394                                                                          
003395     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
003396                         PRT-AFTER-2 L1-RAD-1-CDC                         
003397     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
003398                         PRT-AFTER-1 L1-RAD-3-CDC                         
003399     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
003400                         PRT-AFTER-1 L1-RAD-4-CDC                         
003401     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
003402                         PRT-AFTER-1 L1-RAD-5-CDC                         
003403     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
003404                         PRT-AFTER-1 L1-RAD-6-CDC                         
003405     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
003406                         PRT-AFTER-1 L1-RAD-7-CDC                         
003407     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
003408                         PRT-AFTER-1 L1-RAD-8-CDC                         
003409     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
003410                         PRT-AFTER-1 L1-RAD-9-CDC                         
003411     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
003412                         PRT-AFTER-1 L1-RAD-10-CDC                        
003413     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
003414                         PRT-AFTER-1 L1-RAD-11-CDC                        
003415     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
003416                         PRT-AFTER-1 L1-RAD-12-CDC                        
003417     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
003418                         PRT-AFTER-1 L1-RAD-13-CDC                        
003419     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
003420                         PRT-AFTER-1 L1-RAD-14-CDC                        
003421     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
003422                         PRT-AFTER-1 L1-RAD-15-CDC                        
003423     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
003424                         PRT-AFTER-1 L1-RAD-16-CDC                        
003425     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
003426                         PRT-AFTER-1 L1-RAD-17-CDC                        
003427     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
003428                         PRT-AFTER-1 L1-RAD-18-CDC                        
003429     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
003430                         PRT-AFTER-1 L1-RAD-19-CDC                        
003431     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
003432                         PRT-AFTER-1 L1-RAD-20-CDC                        
003433     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
003434                         PRT-AFTER-1 L1-RAD-21-CDC                        
003435     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
003436                         PRT-AFTER-1 L1-RAD-22-CDC                        
003437     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
003438                         PRT-AFTER-1 L1-RAD-23-CDC                        
003439     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
003440                         PRT-AFTER-1 L1-RAD-24-CDC                        
003441     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
003442                         PRT-AFTER-1 L1-RAD-25-CDC                        
003443     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
003444                         PRT-AFTER-1 L1-RAD-26-CDC                        
003445     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
003446                         PRT-AFTER-1 L1-RAD-27-CDC                        
003447     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
003448                         PRT-AFTER-1 L1-RAD-28-CDC                        
003449     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
003450                         PRT-AFTER-1 L1-RAD-29-CDC                        
003451     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
003452                         PRT-AFTER-1 L1-RAD-30-CDC                        
003453     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
003454                         PRT-AFTER-1 L1-RAD-31-CDC                        
003455     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
003456                         PRT-AFTER-1 L1-RAD-32-CDC                        
003457     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
003458                         PRT-AFTER-1 L1-RAD-33-CDC                        
003459     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
003460                         PRT-AFTER-1 L1-RAD-34-CDC                        
003461     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
003462                         PRT-AFTER-1 L1-RAD-35-CDC                        
003463     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
003464                         PRT-AFTER-1 L1-RAD-36-CDC                        
003465     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
003466                         PRT-AFTER-1 L1-RAD-37-CDC                        
003467     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
003468                         PRT-AFTER-1 L1-RAD-38-CDC                        
003469     .                                                                    
003470     EJECT                                                                
003471 EM-SKRIV-EXTRA-BUFFRADER-CDC SECTION.                                    
003472     MOVE 'EM-SKRIV-EXTRA-BUFF   ' TO WS-SECTION                          
003473     PERFORM EMA-SKRIV-BUFF-RUBRIKER                                      
003474     MOVE SPACE                    TO STATUS-WS                           
003475     MOVE +1                       TO SALDO-IX                            
003476                                                                          
003477     MOVE +0                       TO SALDO-IX                            
003478     PERFORM IMS-GU-WDD8B1                                                
003479     PERFORM UNTIL NOT SEGMENT-FINNS OR SALDO-IX = WS-SUM-BUFFR           
003480       ADD +1                      TO SALDO-IX                            
003481       PERFORM IMS-GN-WDD8B1                                              
003482     END-PERFORM                                                          
003483                                                                          
003484     MOVE +1                       TO SALDO-IX                            
003485     PERFORM UNTIL SEGMENT-SAKNAS                                         
003492                                                                          
003493       MOVE SEQB-ADBUFFOMR        TO BU-ADBUFFOMR-CDC (SALDO-IX)          
003494       MOVE SEQB-ADBUFFGANG       TO BU-ADBUFFGANG-CDC(SALDO-IX)          
003495       MOVE SEQB-ADBUFFPL         TO BU-ADBUFFPL-CDC  (SALDO-IX)          
003496       COMPUTE WS-QTY-CDC = SEQB-KVBUFF-F +                               
003497                            SEQB-KVBUFF-OF                                
003498       MOVE WS-QTY-CDC             TO BU-QTY-CDC       (SALDO-IX)         
003499       MOVE ALL '.'                TO BU-PUNKT-CDC     (SALDO-IX)         
003500       PERFORM IMS-GN-WDD8B1                                              
003501       ADD +1 TO SALDO-IX                                                 
003502       IF SALDO-IX > 2                                                    
003503         IF WS-RAD-RAEKNARE2 > 41                                         
003504           PERFORM EMA-SKRIV-BUFF-RUBRIKER                                
003505         END-IF                                                           
003506         CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1              
003507                           ALT-PCB PRT-AFTER-2 BU-RAD-CDC                 
003508                                                                          
003509         ADD +2  TO WS-RAD-RAEKNARE2                                      
003510         MOVE +1 TO SALDO-IX                                              
003511       END-IF                                                             
003512     END-PERFORM                                                          
003513                                                                          
003514     IF SALDO-IX > +1                                                     
003515       MOVE ZERO              TO BU-ADBUFFOMR-CDC (SALDO-IX)              
003516                                 BU-ADBUFFGANG-CDC(SALDO-IX)              
003517                                 BU-ADBUFFPL-CDC  (SALDO-IX)              
003518                                 BU-QTY-CDC       (SALDO-IX)              
003519       MOVE SPACE             TO BU-PUNKT-CDC     (SALDO-IX)              
003520       IF WS-RAD-RAEKNARE2 > 41                                           
003521         PERFORM EMA-SKRIV-BUFF-RUBRIKER                                  
003522       END-IF                                                             
003523       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB        
003524                           PRT-AFTER-2 BU-RAD-CDC                         
003525                                                                          
003526       ADD +2  TO WS-RAD-RAEKNARE2                                        
003527       MOVE +1 TO SALDO-IX                                                
003528     END-IF                                                               
003529     .                                                                    
003530     EJECT                                                                
003531 EMA-SKRIV-BUFF-RUBRIKER SECTION.                                         
003532      MOVE 'EMA-SKRIV-BUFF        ' TO WS-SECTION                         
003533     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
003534                         PRT-NYSIDA-RAD2 BU-RUB-CDC                       
003535     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
003536                         PRT-AFTER-2 BU-RUB2-CDC                          
003537     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
003538                         PRT-AFTER-3 BU-RUB3-CDC                          
003539     MOVE +6                          TO WS-RAD-RAEKNARE2                 
003540     .                                                                    
003541     EJECT                                                                
003542 EN-HAEMTA-UPPG-WDK6-WDK7-SDC SECTION.                                    
003543     MOVE 'EN-HAMTA-WDK6-WDK7    ' TO WS-SECTION                          
003544     MOVE ZERO                        TO L1-KVLS-SDC                      
003545                                         L1-KVAKS-SDC                     
003546                                         L1-KVUTRS-SDC                    
003547                                         L1-TIJUSTDA-SDC                  
003548                                         L1-KVJUSTKV-SDC                  
003549                                         L1-KDJUSTYP-SDC                  
003550     MOVE SPACE                       TO L1-KDJUSTYP-A-SDC                
003551     PERFORM IMS-GET-ART-WDK6                                             
003552     MOVE ART-KDPRODSL                TO L1-KDPRODSL-SDC                  
003553     MOVE ART-KDSORT                  TO L1-KDSORT-SDC                    
003554     MOVE ART-IDLEVNR                 TO L1-IDLEVNR-CDC                   
003555                                                                          
003556     PERFORM IMS-GNP-CLAGERINFO                                           
003557     MOVE CLAG-KVROS                  TO L1-KVROS-SDC                     
003558                                                                          
003559     IF WDB6-A-SAKNAS                                                     
003560     OR A-DCS-FLPRISSPR = JA                                              
003561       MOVE ZERO                      TO L1-PRARTSTD-SDC                  
003562     ELSE                                                                 
003563       MOVE CLAG-PRARTSTD             TO L1-PRARTSTD-SDC                  
003564     END-IF                                                               
003565     MOVE CLAG-VKART                  TO L1-VKART-SDC                     
003566                                                                          
003567     PERFORM IMS-GET-ART-WDK7                                             
003568     IF SEGMENT-FINNS                                                     
003569       MOVE SLAG-KVLS                 TO L1-KVLS-SDC                      
003570       MOVE SLAG-KVAKS-SDC            TO L1-KVAKS-SDC                     
003571       MOVE SLAG-KVUTRS               TO L1-KVUTRS-SDC                    
003572       MOVE SLAG-ADLAGOMR             TO L1-ADLAGOMR-SDC                  
003573       MOVE SLAG-ADGANG               TO L1-ADGANG-SDC                    
003574       MOVE SLAG-ADPLATS              TO L1-ADPLATS-SDC                   
003575                                                                          
003576       MOVE SLAG-KVLS                        TO INV-KVLS-OLD              
003577       MOVE SLAG-KVAKS-SDC                   TO INV-KVAKS-OLD             
003578       MOVE WS-IDLOPNR                       TO INV-IDLOPNR               
003579       IF WS-IDPRTOMG NOT = INV-IDPRTOMG                                  
003580         MOVE FUNCTION CURRENT-DATE(1:8) TO INV-DAREGDAT                  
003581         EVALUATE WS-IDPRTOMG                                             
003582           WHEN 1                                                         
003583             MOVE FUNCTION CURRENT-DATE(1:8) TO INV-DAREGDAT-PR1          
003584           WHEN 2                                                         
003585             MOVE FUNCTION CURRENT-DATE(1:8) TO INV-DAREGDAT-PR2          
003586           WHEN 3                                                         
003587             MOVE FUNCTION CURRENT-DATE(1:8) TO INV-DAREGDAT-PR3          
003588         END-EVALUATE                                                     
003589       END-IF                                                             
003590       MOVE WS-IDPRTOMG                      TO INV-IDPRTOMG              
003591                                                                          
003592       PERFORM IMS-GET-INVHIST-ROT                                        
003593       IF SEGMENT-FINNS                                                   
003594         PERFORM IMS-GET-INVHIST-SDC                                      
003595         IF SEGMENT-FINNS                                                 
003596           PERFORM S10-KONV-DATUM                                         
003597           MOVE DAT-TIAAVVD           TO L1-TIJUSTDA-SDC                  
003598           MOVE INVH-KVJUSTKV         TO L1-KVJUSTKV-SDC                  
003599           MOVE INVH-KDJUSTYP         TO L1-KDJUSTYP-SDC                  
003600           IF INVH-FLAUTLSJ = 'J'                                         
003601             MOVE 'A'                 TO L1-KDJUSTYP-A-SDC                
003602           ELSE                                                           
003603             MOVE ' '                 TO L1-KDJUSTYP-A-SDC                
003604           END-IF                                                         
003605         END-IF                                                           
003606       END-IF                                                             
003607     END-IF                                                               
003608     .                                                                    
003609     EJECT                                                                
003610 EO-HAEMTA-UPPG-WDD8-SDC SECTION.                                         
003611     MOVE 'EO-HAMTA-UPPG-WDD8    ' TO WS-SECTION                          
003612     PERFORM EOB-NOLLSTAELL-SDC                                           
003613     MOVE NEJ                   TO BUFFERT-SW                             
003614     MOVE MID-IDDC              TO W-IDDC-D8-MIN                          
003615                                   W-IDDC-D8-MAX                          
003616     MOVE +1 TO SALDO-IX                                                  
003617     PERFORM IMS-GU-WDD8B1                                                
003618     PERFORM UNTIL SEGMENT-SAKNAS OR SALDO-IX > +4                        
003625                                                                          
003626       EVALUATE SALDO-IX                                                  
003627         WHEN +1                                                          
003628           MOVE SEQB-ADBUFFOMR        TO L1-ADBUFFOMR-SDC-1               
003629           MOVE SEQB-ADBUFFGANG       TO L1-ADBUFFGANG-SDC-1              
003630           MOVE SEQB-ADBUFFPL         TO L1-ADBUFFPLATS-SDC-1             
003631         WHEN +2                                                          
003632           MOVE SEQB-ADBUFFOMR        TO L1-ADBUFFOMR-SDC-2               
003633           MOVE SEQB-ADBUFFGANG       TO L1-ADBUFFGANG-SDC-2              
003634           MOVE SEQB-ADBUFFPL         TO L1-ADBUFFPLATS-SDC-2             
003635         WHEN +3                                                          
003636           MOVE SEQB-ADBUFFOMR        TO L1-ADBUFFOMR-SDC-3               
003637           MOVE SEQB-ADBUFFGANG       TO L1-ADBUFFGANG-SDC-3              
003638           MOVE SEQB-ADBUFFPL         TO L1-ADBUFFPLATS-SDC-3             
003639         WHEN +4                                                          
003640           MOVE SEQB-ADBUFFOMR        TO L1-ADBUFFOMR-SDC-4               
003641           MOVE SEQB-ADBUFFGANG       TO L1-ADBUFFGANG-SDC-4              
003642           MOVE SEQB-ADBUFFPL         TO L1-ADBUFFPLATS-SDC-4             
003643        END-EVALUATE                                                      
003644        ADD +1 TO SALDO-IX                                                
003645        PERFORM IMS-GN-WDD8B1                                             
003646     END-PERFORM                                                          
003647     IF SEGMENT-FINNS                                                     
003648        MOVE JA TO BUFFERT-SW                                             
003649     END-IF                                                               
003650     .                                                                    
003651     EJECT                                                                
003652 EOB-NOLLSTAELL-SDC     SECTION.                                          
003653     MOVE 'EOB-NOLLSTAELL-SDC    ' TO WS-SECTION                          
003654                                                                          
003655     MOVE +0 TO L1-ADBUFFOMR-SDC-1                                        
003656                L1-ADBUFFOMR-SDC-2                                        
003657                L1-ADBUFFOMR-SDC-3                                        
003658                L1-ADBUFFOMR-SDC-4                                        
003659                L1-ADBUFFGANG-SDC-1                                       
003660                L1-ADBUFFGANG-SDC-2                                       
003661                L1-ADBUFFGANG-SDC-3                                       
003662                L1-ADBUFFGANG-SDC-4                                       
003663                L1-ADBUFFPLATS-SDC-1                                      
003664                L1-ADBUFFPLATS-SDC-2                                      
003665                L1-ADBUFFPLATS-SDC-3                                      
003666                L1-ADBUFFPLATS-SDC-4                                      
003667     .                                                                    
003668     EJECT                                                                
003669 EP-HAEMTA-UPPG-WDD3-SDC SECTION.                                         
003670     MOVE 'EP-HAEMTA-UPPG-WDD3-SDC' TO WS-SECTION                         
003671                                                                          
003672     MOVE 'GB '                TO W-IDSKYLT                               
003673     PERFORM IMS-GU-BEN-SEQ                                               
003674     IF SEGMENT-FINNS                                                     
003675       MOVE BEN-TEXT-BEART     TO L1-BEART-SDC                            
003676     ELSE                                                                 
003677       MOVE SPACE              TO L1-BEART-SDC                            
003678     END-IF                                                               
003679     .                                                                    
003680     SKIP2                                                                
003681 EQ-HAEMTA-SDC-WDE4-OVR-DISTR SECTION.                                    
003682     MOVE 'EQ-HAMTA-SDC-WDE4     ' TO WS-SECTION                          
003683     MOVE +1 TO INDX                                                      
003684     MOVE +0 TO WS-KVEFRS-OLD                                             
003685     MOVE +0 TO WS-ANTAL-RADER-EFR                                        
003686     MOVE +0 TO W-SDC-ANTAL                                               
003687     PERFORM IMS-GU-WDE4C1                                                
003688                                                                          
003689     PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                           
003690                     OR INDX > MAX-IX-7                                   
003691        MOVE SEQC-IDDISTR  TO W-401-IDDISTR                               
003692        MOVE SEQC-IDKUNDNR TO W-401-IDKUNDNR                              
003693        MOVE SEQC-IDORDNR5 TO W-401-IDORDNR                               
003694        MOVE SEQC-IDPRODNR TO W-401-IDPRODNR                              
003695        MOVE SEQC-IDPLKLST TO W-401-IDPLKLST                              
003696        MOVE SEQC-IDPURAD  TO W-411-IDPURAD                               
003697        PERFORM IMS-GU-WDE401-11                                          
003698        IF (ORAD-FLDIRLEV NOT = 'J')                                      
003699           IF KORD-IDDC = MID-IDDC                                        
003700             IF KORD-IDDISTR NOT = +98                                    
003701               MOVE KORD-IDKUNDRF TO WS-IDKUNDRF                          
003702                                                                          
003703               MOVE WS-IDORDNR      TO WSDC-IDKUNDRF (INDX)               
003704               COMPUTE W-SDC-ANTAL =                                      
003705                 (ORAD-KVAVBART - ORAD-KVLEVART)                          
003706               MOVE W-SDC-ANTAL     TO WSDC-KVLEVART (INDX)               
003707               COMPUTE WS-KVEFRS-OLD = WS-KVEFRS-OLD + W-SDC-ANTAL        
003708               MOVE KORD-IDDISTR    TO WSDC-IDDISTR  (INDX)               
003709               MOVE KORD-IDKUNDNR   TO WSDC-IDKUNDNR (INDX)               
003710               ADD +1 TO INDX                                             
003711               ADD +1 TO WS-ANTAL-RADER-EFR                               
003712             END-IF                                                       
003713           END-IF                                                         
003714        END-IF                                                            
003715        PERFORM IMS-GN-WDE4C1                                             
003716     END-PERFORM                                                          
003717                                                                          
003718* NU HAR VI ALLA SALDO, GÖR REPLACE PÅ WDH1 MED AKTUELLA VÄRDEN           
003719* OM EFR SKALL DELAS MED 2 LÄGG TILL EN RÄKNARE I SLINGAN OVAN            
003720     MOVE +0                               TO INV-KVEFRS-OLD              
003721     IF WS-ANTAL-RADER-EFR < 2                                            
003722       MOVE WS-KVEFRS-OLD                  TO INV-KVEFRS-OLD              
003723     ELSE                                                                 
003724       COMPUTE INV-KVEFRS-OLD = WS-KVEFRS-OLD / 2                         
003725     END-IF                                                               
003726                                                                          
003727**** TA BORT POSTEN PÅ WDH1 OCH SKAPA SEDAN ETT NYTT MED                  
003728**** DAGENS DATUM I DAREGDAT-SORT OM DET ÄR FÖRSTA PRINTNINGEN            
003729     IF WS-IDPRTOMG = 1                                                   
003730       PERFORM IMS-GNP-WDH121                                             
003731       IF SEGMENT-FINNS                                                   
003732         IF INVL-KDSEGKEY = 0                                             
003733           MOVE INVL-IDUSER     TO WS-IDUSER                              
003734         END-IF                                                           
003735       END-IF                                                             
003736       PERFORM IMS-GHU-WDH101                                             
003737       MOVE DLI-IO-AREA-INVA11  TO SPAR-WDH111-AREA                       
003738       PERFORM IMS-GHNP-WDH111                                            
003739                                                                          
003740       PERFORM IMS-DELETE-WDH111                                          
003741       MOVE WS-DAGENS-DATUM     TO SPAR-INV-DAREGDAT-SORT                 
003742                                                                          
003743       PERFORM IMS-GHU-WDH101                                             
003744       MOVE SPAR-WDH111-AREA TO DLI-IO-AREA-INVA11                        
003745       PERFORM IMS-INSERT-WDH111                                          
003746                                                                          
003747       IF SEGMENT-FINNS-REDAN                                             
003748         PERFORM UNTIL SEGMENT-FINNS                                      
003749           ADD +1 TO INV-TISEGKEY                                         
003750           PERFORM IMS-INSERT-WDH111                                      
003751         END-PERFORM                                                      
003752       END-IF                                                             
003753                                                                          
003754       MOVE '0'            TO INVL-KDSEGKEY                               
003755       MOVE WS-IDUSER      TO INVL-IDUSER                                 
003756       PERFORM IMS-INSERT-WDH121                                          
003757       MOVE '1'            TO INVL-KDSEGKEY                               
003758       MOVE MID-IDUSER     TO INVL-IDUSER                                 
003759       PERFORM IMS-INSERT-WDH121                                          
003760       MOVE '2'            TO INVL-KDSEGKEY                               
003761       MOVE SPACE          TO INVL-IDUSER                                 
003762       PERFORM IMS-INSERT-WDH121                                          
003763       MOVE '3'            TO INVL-KDSEGKEY                               
003764       MOVE SPACE          TO INVL-IDUSER                                 
003765       PERFORM IMS-INSERT-WDH121                                          
003766     ELSE                                                                 
003767       PERFORM IMS-REPLACE                                                
003768       MOVE 'REPLACE PÅ WDH111     ' TO WS-SECTION                        
003769       PERFORM IMS-GNP-WDH121                                             
003770       MOVE 'GJORT GNP FÖR WDH121  ' TO WS-SECTION                        
003771       IF SEGMENT-FINNS                                                   
003772         PERFORM UNTIL SEGMENT-SAKNAS                                     
003773           IF W-IDPRTOMG-ALFA = INVL-KDSEGKEY                             
003774             MOVE MID-IDUSER         TO INVL-IDUSER                       
003775             MOVE 'SKA GÖRA REPLACE PÅ 21' TO WS-SECTION                  
003776             PERFORM IMS-REPLACE-WDH121                                   
003777             MOVE 'GJORT REPL PÅ 21      ' TO WS-SECTION                  
003778           END-IF                                                         
003779           PERFORM IMS-GNP-WDH121                                         
003780         END-PERFORM                                                      
003781       END-IF                                                             
003782     END-IF                                                               
003783**                                                                        
003784     PERFORM EQB-FLYTTA-TILL-SDC-LISTA                                    
003785     .                                                                    
003786     EJECT                                                                
003787 EQB-FLYTTA-TILL-SDC-LISTA  SECTION.                                      
003788     MOVE 'EQB-FLYTTA-TILL-SDC-LISTA' TO WS-SECTION                       
003789                                                                          
003790     MOVE WSDC-IDKUNDRF (1) TO L1-IDKUNDRF-SDC-1                          
003791     MOVE WSDC-IDKUNDRF (2) TO L1-IDKUNDRF-SDC-2                          
003792     MOVE WSDC-IDKUNDRF (3) TO L1-IDKUNDRF-SDC-3                          
003793     MOVE WSDC-IDKUNDRF (4) TO L1-IDKUNDRF-SDC-4                          
003794     MOVE WSDC-IDKUNDRF (5) TO L1-IDKUNDRF-SDC-5                          
003795     MOVE WSDC-IDKUNDRF (6) TO L1-IDKUNDRF-SDC-6                          
003796     MOVE WSDC-IDKUNDRF (7) TO L1-IDKUNDRF-SDC-7                          
003797                                                                          
003798     MOVE WSDC-IDKUNDNR (1) TO L1-IDKUNDNR-SDC-1                          
003799     MOVE WSDC-IDKUNDNR (2) TO L1-IDKUNDNR-SDC-2                          
003800     MOVE WSDC-IDKUNDNR (3) TO L1-IDKUNDNR-SDC-3                          
003801     MOVE WSDC-IDKUNDNR (4) TO L1-IDKUNDNR-SDC-4                          
003802     MOVE WSDC-IDKUNDNR (5) TO L1-IDKUNDNR-SDC-5                          
003803     MOVE WSDC-IDKUNDNR (6) TO L1-IDKUNDNR-SDC-6                          
003804     MOVE WSDC-IDKUNDNR (7) TO L1-IDKUNDNR-SDC-7                          
003805                                                                          
003806     MOVE WSDC-IDDISTR  (1) TO L1-IDDISTR-SDC-1                           
003807     MOVE WSDC-IDDISTR  (2) TO L1-IDDISTR-SDC-2                           
003808     MOVE WSDC-IDDISTR  (3) TO L1-IDDISTR-SDC-3                           
003809     MOVE WSDC-IDDISTR  (4) TO L1-IDDISTR-SDC-4                           
003810     MOVE WSDC-IDDISTR  (5) TO L1-IDDISTR-SDC-5                           
003811     MOVE WSDC-IDDISTR  (6) TO L1-IDDISTR-SDC-6                           
003812     MOVE WSDC-IDDISTR  (7) TO L1-IDDISTR-SDC-7                           
003813                                                                          
003814     MOVE WSDC-KVLEVART (1) TO L1-KVLEVART-SDC-1                          
003815     MOVE WSDC-KVLEVART (2) TO L1-KVLEVART-SDC-2                          
003816     MOVE WSDC-KVLEVART (3) TO L1-KVLEVART-SDC-3                          
003817     MOVE WSDC-KVLEVART (4) TO L1-KVLEVART-SDC-4                          
003818     MOVE WSDC-KVLEVART (5) TO L1-KVLEVART-SDC-5                          
003819     MOVE WSDC-KVLEVART (6) TO L1-KVLEVART-SDC-6                          
003820     MOVE WSDC-KVLEVART (7) TO L1-KVLEVART-SDC-7                          
003821     .                                                                    
003822     EJECT                                                                
003823 ER-SDC-SKRIV-LISTA1 SECTION.                                             
003824     MOVE 'ER-SDC-SKRIV-LISTA1      ' TO WS-SECTION                       
003825                                                                          
003826**********************  SKRIVER SDC-LISTAN   *******************          
003827* SKRIVER RUBRIK                                                          
003828                                                                          
003829     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
003830                         PRT-NYSIDA-RAD4 L1-RUB-SDC                       
003831                                                                          
003832* SKRIVER RADER                                                           
003833                                                                          
003834     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
003835                         PRT-AFTER-1 L1-LISTNR-RAD-SDC                    
003836     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
003837                         PRT-AFTER-2 L1-RAD-1-SDC                         
003838     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
003839                         PRT-AFTER-2 L1-RAD-2-SDC                         
003840     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
003841                         PRT-AFTER-2 L1-RAD-3-SDC                         
003842     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
003843                         PRT-AFTER-2 L1-RAD-4-SDC                         
003844     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
003845                         PRT-AFTER-2 L1-RAD-5-SDC                         
003846     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
003847                         PRT-AFTER-2 L1-RAD-6-SDC                         
003848     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
003849                         PRT-AFTER-2 L1-RAD-7-SDC                         
003850     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
003851                         PRT-AFTER-2 L1-RAD-8-SDC                         
003852     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
003853                         PRT-AFTER-2 L1-RAD-9-SDC                         
003854     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
003855                         PRT-AFTER-2 L1-RAD-10-SDC                        
003856     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
003857                         PRT-AFTER-2 L1-RAD-11-SDC                        
003858     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
003859                         PRT-AFTER-2 L1-RAD-12-SDC                        
003860     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
003861                         PRT-AFTER-2 L1-RAD-13-SDC                        
003862     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
003863                         PRT-AFTER-2 L1-RAD-14-SDC                        
003864     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
003865                         PRT-AFTER-2 L1-RAD-15-SDC                        
003866     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
003867                         PRT-AFTER-1 L1-RAD-STRECK-SDC                    
003868     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
003869                         PRT-AFTER-2 L1-RAD-17-SDC                        
003870     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
003871                         PRT-AFTER-2 L1-RAD-18-SDC                        
003872     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
003873                         PRT-AFTER-2 L1-RAD-19-SDC                        
003874     .                                                                    
003875     EJECT                                                                
003876 ES-SKRIV-EXTRA-BUFFRADER-SDC SECTION.                                    
003877     MOVE 'ES-SKRIV-EXTRA-BUFFRADER-SDC' TO WS-SECTION                    
003878                                                                          
003879     PERFORM ESA-SKRIV-BUFF-RUB-SDC                                       
003880     ADD  +3 TO WS-RAD-RAEKNARE2                                          
003881                                                                          
003882     MOVE +1 TO SALDO-IX                                                  
003883     MOVE SPACE TO STATUS-WS                                              
003884     PERFORM UNTIL SEGMENT-SAKNAS                                         
003891                                                                          
003892      MOVE SEQB-ADBUFFOMR         TO BU-ADBUFFOMR-SDC (SALDO-IX)          
003893      MOVE SEQB-ADBUFFGANG        TO BU-ADBUFFGANG-SDC(SALDO-IX)          
003894      MOVE SEQB-ADBUFFPL          TO BU-ADBUFFPL-SDC  (SALDO-IX)          
003895      MOVE ALL '.'                TO BU-PUNKT-SDC     (SALDO-IX)          
003896      PERFORM IMS-GN-WDD8B1                                               
003897      ADD +1 TO SALDO-IX                                                  
003898      IF SALDO-IX > 2                                                     
003899         IF WS-RAD-RAEKNARE2 > 41                                         
003900           PERFORM ESA-SKRIV-BUFF-RUB-SDC                                 
003901           MOVE +6 TO WS-RAD-RAEKNARE2                                    
003902         END-IF                                                           
003903         CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1              
003904                           ALT-PCB PRT-AFTER-2 BU-RAD-SDC                 
003905                                                                          
003906         ADD +2  TO WS-RAD-RAEKNARE2                                      
003907         MOVE +1 TO SALDO-IX                                              
003908      END-IF                                                              
003909     END-PERFORM                                                          
003910     IF SALDO-IX > +1                                                     
003911       MOVE ZERO              TO BU-ADBUFFOMR-SDC (SALDO-IX)              
003912                                 BU-ADBUFFGANG-SDC(SALDO-IX)              
003913                                 BU-ADBUFFPL-SDC  (SALDO-IX)              
003914       MOVE SPACE             TO BU-PUNKT-SDC     (SALDO-IX)              
003915       IF WS-RAD-RAEKNARE2 > 41                                           
003916         PERFORM ESA-SKRIV-BUFF-RUB-SDC                                   
003917         MOVE +6 TO WS-RAD-RAEKNARE2                                      
003918       END-IF                                                             
003919       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB        
003920                           PRT-AFTER-2 BU-RAD-SDC                         
003921                                                                          
003922       ADD +2  TO WS-RAD-RAEKNARE2                                        
003923       MOVE +1 TO SALDO-IX                                                
003924     END-IF                                                               
003925     .                                                                    
003926     SKIP2                                                                
003927 ESA-SKRIV-BUFF-RUB-SDC SECTION.                                          
003928     MOVE 'ESA-SKRIV-BUFF-RUB-SDC      ' TO WS-SECTION                    
003929                                                                          
003930     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
003931                         PRT-NYSIDA-RAD4 L1-RUB-SDC                       
003932     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
003933                         PRT-AFTER-3 L-BUFFERT-RUB-SDC                    
003934     .                                                                    
003935     EJECT                                                                
003936 AT-HAEMTA-UPPG-WDK6-WDK7-ADC SECTION.                                    
003937     MOVE 'AT-HAEMTA-UPPG-WDK6-WDK7-ADC' TO WS-SECTION                    
003938                                                                          
003939     PERFORM ATA-NOLLA-ADC                                                
003940                                                                          
003941     PERFORM IMS-GET-ART-WDK6                                             
003942                                                                          
003943     MOVE ART-KDPRODSL                TO L1-KDPRODSL-ADC                  
003944     MOVE '      PIECES'              TO L1-KDSORT-ADC                    
003945     EVALUATE ART-KDSORT                                                  
003946       WHEN 'KG'  MOVE '    KILOGRAM' TO L1-KDSORT-ADC                    
003947       WHEN ' L'  MOVE '       LITRE' TO L1-KDSORT-ADC                    
003948       WHEN 'L '  MOVE '       LITRE' TO L1-KDSORT-ADC                    
003949       WHEN 'M '  MOVE '       METRE' TO L1-KDSORT-ADC                    
003950       WHEN ' M'  MOVE '       METRE' TO L1-KDSORT-ADC                    
003951       WHEN 'MM'  MOVE '  MILLIMETRE' TO L1-KDSORT-ADC                    
003952       WHEN 'SA'  MOVE '         KIT' TO L1-KDSORT-ADC                    
003953       WHEN 'C2'  MOVE '   SQUARE-CM' TO L1-KDSORT-ADC                    
003954       WHEN 'M2'  MOVE 'SQUARE-METER' TO L1-KDSORT-ADC                    
003955       WHEN 'ML'  MOVE '  MILLILITRE' TO L1-KDSORT-ADC                    
003956       WHEN 'SW'  MOVE '    SOFTWARE' TO L1-KDSORT-ADC                    
003957     END-EVALUATE                                                         
003958                                                                          
003959     PERFORM IMS-GNP-CLAGERINFO                                           
003960     MOVE CLAG-KDPSLLOC               TO L1-KDPSLLOC-ADC                  
003961     MOVE CLAG-KDERS                  TO L1-KDERS-ADC                     
003962     MOVE CLAG-VKART                  TO L1-VKART-ADC                     
003963     MOVE CLAG-PRARTSTD               TO L1-PRARTSTD-ADC                  
003964                                                                          
003965     PERFORM IMS-GET-ART-WDK7                                             
003966     IF SEGMENT-FINNS                                                     
003967       MOVE SLAG-KVLS                 TO L1-KVLS-ADC                      
003968       MOVE SLAG-KVAKS-SDC            TO L1-KVAKS-ADC                     
003969       MOVE SLAG-KVUTRS               TO L1-KVUTRS-ADC                    
003970       MOVE SLAG-ADLAGOMR             TO L1-ADLAGOMR-ADC                  
003971       MOVE SLAG-ADGANG               TO L1-ADGANG-ADC                    
003972       MOVE SLAG-ADPLATS              TO L1-ADPLATS-ADC                   
003973                                                                          
003974       MOVE SLAG-KVLS                        TO INV-KVLS-OLD              
003975       MOVE SLAG-KVAKS-SDC                   TO INV-KVAKS-OLD             
003976       MOVE WS-IDLOPNR                       TO INV-IDLOPNR               
003977       IF WS-IDPRTOMG NOT = INV-IDPRTOMG                                  
003978         MOVE FUNCTION CURRENT-DATE(1:8)     TO INV-DAREGDAT              
003979         EVALUATE WS-IDPRTOMG                                             
003980           WHEN 1                                                         
003981             MOVE FUNCTION CURRENT-DATE(1:8) TO INV-DAREGDAT-PR1          
003982           WHEN 2                                                         
003983             MOVE FUNCTION CURRENT-DATE(1:8) TO INV-DAREGDAT-PR2          
003984           WHEN 3                                                         
003985             MOVE FUNCTION CURRENT-DATE(1:8) TO INV-DAREGDAT-PR3          
003986         END-EVALUATE                                                     
003987       END-IF                                                             
003988       MOVE WS-IDPRTOMG                      TO INV-IDPRTOMG              
003989                                                                          
003990       PERFORM IMS-GET-INVHIST-ROT                                        
003991       IF SEGMENT-FINNS                                                   
003992         PERFORM IMS-GET-INVHIST-SDC                                      
003993         IF SEGMENT-FINNS                                                 
003994           PERFORM S10-KONV-DATUM                                         
003995           MOVE DAT-TIAAVVD           TO L1-TIJUSTDA-ADC                  
003996           MOVE INVH-KVJUSTKV         TO L1-KVJUSTKV-ADC                  
003997           MOVE INVH-KDJUSTYP         TO L1-KDJUSTYP-ADC                  
003998           IF INVH-FLAUTLSJ = 'J'                                         
003999             MOVE 'A'                 TO L1-KDJUSTYP-A-ADC                
004000           ELSE                                                           
004001             MOVE ' '                 TO L1-KDJUSTYP-A-ADC                
004002           END-IF                                                         
004003                                                                          
004004           PERFORM IMS-GET-INVHIST-SDCNASTA                               
004005           IF SEGMENT-FINNS                                               
004006             PERFORM S10-KONV-DATUM                                       
004007             MOVE DAT-TIAAVVD         TO L1-TIJUSTDA-1-ADC                
004008             MOVE INVH-KVJUSTKV       TO L1-KVJUSTKV-1-ADC                
004009             MOVE INVH-KDJUSTYP       TO L1-KDJUSTYP-1-ADC                
004010             IF INVH-FLAUTLSJ = 'J'                                       
004011               MOVE 'A'                 TO L1-KDJUSTYP-1A-ADC             
004012             ELSE                                                         
004013               MOVE ' '                 TO L1-KDJUSTYP-1A-ADC             
004014             END-IF                                                       
004015             PERFORM IMS-GET-INVHIST-SDCNASTA                             
004016             IF SEGMENT-FINNS                                             
004017               PERFORM S10-KONV-DATUM                                     
004018               MOVE DAT-TIAAVVD       TO L1-TIJUSTDA-2-ADC                
004019               MOVE INVH-KVJUSTKV     TO L1-KVJUSTKV-2-ADC                
004020               MOVE INVH-KDJUSTYP     TO L1-KDJUSTYP-2-ADC                
004021               IF INVH-FLAUTLSJ = 'J'                                     
004022                 MOVE 'A'                 TO L1-KDJUSTYP-2A-ADC           
004023               ELSE                                                       
004024                 MOVE ' '                 TO L1-KDJUSTYP-2A-ADC           
004025               END-IF                                                     
004026             END-IF                                                       
004027           END-IF                                                         
004028         END-IF                                                           
004029       END-IF                                                             
004030     END-IF                                                               
004031     .                                                                    
004032     EJECT                                                                
004033 ATA-NOLLA-ADC SECTION.                                                   
004034                                                                          
004035     MOVE ZERO             TO L1-KVLS-ADC                                 
004036                              L1-KVUTRS-ADC                               
004037                              L1-PRARTSTD-ADC                             
004038                              L1-TIJUSTDA-ADC                             
004039                              L1-KVJUSTKV-ADC                             
004040                              L1-KDJUSTYP-ADC                             
004041                              L1-TIJUSTDA-1-ADC                           
004042                              L1-KVJUSTKV-1-ADC                           
004043                              L1-KDJUSTYP-1-ADC                           
004044                              L1-TIJUSTDA-2-ADC                           
004045                              L1-KVJUSTKV-2-ADC                           
004046                              L1-KDJUSTYP-2-ADC                           
004047     MOVE SPACE            TO L1-KDJUSTYP-A-ADC                           
004048                              L1-KDJUSTYP-1A-ADC                          
004049                              L1-KDJUSTYP-2A-ADC                          
004050     .                                                                    
004051     EJECT                                                                
004052 AU-HAEMTA-UPPG-WDD8-ADC SECTION.                                         
004053     MOVE 'AU-HAEMTA-UPPG-WDD8-ADC     ' TO WS-SECTION                    
004054                                                                          
004055     PERFORM AUA-NOLLSTAELL-ADC                                           
004056     MOVE ' NO'                          TO L1-BUFFER-ADC-7D              
004057     MOVE MID-IDDC                       TO W-IDDC-D8-MIN                 
004058                                            W-IDDC-D8-MAX                 
004059     MOVE +1 TO SALDO-IX                                                  
004060     PERFORM IMS-GU-WDD8B1                                                
004070     PERFORM UNTIL SEGMENT-SAKNAS OR SALDO-IX > +5                        
004077                                                                          
004078       EVALUATE SALDO-IX                                                  
004079         WHEN +1                                                          
004080           MOVE SEQB-ADBUFFOMR        TO L1-ADBUFFOMR-ADC-1               
004081           MOVE SEQB-ADBUFFGANG       TO L1-ADBUFFGANG-ADC-1              
004082           MOVE SEQB-ADBUFFPL         TO L1-ADBUFFPLATS-ADC-1             
004083           COMPUTE WS-QTY-ADC =                                           
004084                  SEQB-KVBUFF-F + SEQB-KVBUFF-OF                          
004085           MOVE WS-QTY-ADC            TO L1-QTY-ADC-1                     
004086           ADD WS-QTY-ADC             TO WS-QTY-ADC-TOT                   
004087         WHEN +2                                                          
004088           MOVE SEQB-ADBUFFOMR        TO L1-ADBUFFOMR-ADC-2               
004089           MOVE SEQB-ADBUFFGANG       TO L1-ADBUFFGANG-ADC-2              
004090           MOVE SEQB-ADBUFFPL         TO L1-ADBUFFPLATS-ADC-2             
004091           COMPUTE WS-QTY-ADC =                                           
004092                  SEQB-KVBUFF-F + SEQB-KVBUFF-OF                          
004093           MOVE WS-QTY-ADC            TO L1-QTY-ADC-2                     
004094           ADD WS-QTY-ADC             TO WS-QTY-ADC-TOT                   
004095         WHEN +3                                                          
004096           MOVE SEQB-ADBUFFOMR        TO L1-ADBUFFOMR-ADC-3               
004097           MOVE SEQB-ADBUFFGANG       TO L1-ADBUFFGANG-ADC-3              
004098           MOVE SEQB-ADBUFFPL         TO L1-ADBUFFPLATS-ADC-3             
004099           COMPUTE WS-QTY-ADC =                                           
004100                  SEQB-KVBUFF-F + SEQB-KVBUFF-OF                          
004101           MOVE WS-QTY-ADC            TO L1-QTY-ADC-3                     
004102           ADD WS-QTY-ADC             TO WS-QTY-ADC-TOT                   
004103         WHEN +4                                                          
004104           MOVE SEQB-ADBUFFOMR        TO L1-ADBUFFOMR-ADC-4               
004105           MOVE SEQB-ADBUFFGANG       TO L1-ADBUFFGANG-ADC-4              
004106           MOVE SEQB-ADBUFFPL         TO L1-ADBUFFPLATS-ADC-4             
004107           COMPUTE WS-QTY-ADC =                                           
004108                  SEQB-KVBUFF-F + SEQB-KVBUFF-OF                          
004109           MOVE WS-QTY-ADC            TO L1-QTY-ADC-4                     
004110           ADD WS-QTY-ADC             TO WS-QTY-ADC-TOT                   
004111         WHEN +5                                                          
004112           MOVE SEQB-ADBUFFOMR        TO L1-ADBUFFOMR-ADC-5               
004113           MOVE SEQB-ADBUFFGANG       TO L1-ADBUFFGANG-ADC-5              
004114           MOVE SEQB-ADBUFFPL         TO L1-ADBUFFPLATS-ADC-5             
004115           COMPUTE WS-QTY-ADC =                                           
004116                  SEQB-KVBUFF-F + SEQB-KVBUFF-OF                          
004117           MOVE WS-QTY-ADC            TO L1-QTY-ADC-5                     
004118           ADD WS-QTY-ADC             TO WS-QTY-ADC-TOT                   
004119         END-EVALUATE                                                     
004120         ADD +1 TO SALDO-IX                                               
004121         PERFORM IMS-GN-WDD8B1                                            
004122     END-PERFORM                                                          
004123     IF SEGMENT-FINNS                                                     
004124       MOVE 'YES'                      TO L1-BUFFER-ADC-7D                
004125       PERFORM UNTIL SEGMENT-SAKNAS                                       
004132                                                                          
004133         COMPUTE WS-QTY-ADC = SEQB-KVBUFF-F +                             
004134                              SEQB-KVBUFF-OF                              
004135         ADD WS-QTY-ADC                TO WS-QTY-ADC-TOT                  
004136         PERFORM IMS-GN-WDD8B1                                            
004137       END-PERFORM                                                        
004138     END-IF                                                               
004139     MOVE WS-QTY-ADC-TOT               TO L1-TOT-QTY-ADC-10               
004140     .                                                                    
004141     EJECT                                                                
004142 AUA-NOLLSTAELL-ADC      SECTION.                                         
004143                                                                          
004144     MOVE +0 TO L1-ADBUFFOMR-ADC-1                                        
004145                L1-ADBUFFOMR-ADC-2                                        
004146                L1-ADBUFFOMR-ADC-3                                        
004147                L1-ADBUFFOMR-ADC-4                                        
004148                L1-ADBUFFOMR-ADC-5                                        
004149                L1-ADBUFFGANG-ADC-1                                       
004150                L1-ADBUFFGANG-ADC-2                                       
004151                L1-ADBUFFGANG-ADC-3                                       
004152                L1-ADBUFFGANG-ADC-4                                       
004153                L1-ADBUFFGANG-ADC-5                                       
004154                L1-ADBUFFPLATS-ADC-1                                      
004155                L1-ADBUFFPLATS-ADC-2                                      
004156                L1-ADBUFFPLATS-ADC-3                                      
004157                L1-ADBUFFPLATS-ADC-4                                      
004158                L1-ADBUFFPLATS-ADC-5                                      
004159                WS-QTY-ADC                                                
004160                WS-QTY-ADC-TOT                                            
004161                L1-QTY-ADC-1                                              
004162                L1-QTY-ADC-2                                              
004163                L1-QTY-ADC-3                                              
004164                L1-QTY-ADC-4                                              
004165                L1-QTY-ADC-5                                              
004166                L1-TOT-QTY-ADC-10                                         
004167     .                                                                    
004168     SKIP2                                                                
004169 AV-HAEMTA-UPPG-WDD3-ADC SECTION.                                         
004170     MOVE 'AV-HAEMTA-UPPG-WDD3-ADC     ' TO WS-SECTION                    
004171                                                                          
004172     MOVE 'US '                TO W-IDSKYLT                               
004173                                                                          
004174     PERFORM IMS-GU-BEN-SEQ                                               
004175     IF SEGMENT-FINNS                                                     
004176       MOVE BEN-TEXT-BEART     TO L1-BEART-ADC                            
004177     ELSE                                                                 
004178       MOVE 'GB '              TO W-IDSKYLT                               
004179       PERFORM IMS-GU-BEN-SEQ                                             
004180       IF SEGMENT-FINNS                                                   
004181         MOVE BEN-TEXT-BEART   TO L1-BEART-ADC                            
004182       ELSE                                                               
004183         MOVE SPACE            TO L1-BEART-ADC                            
004184       END-IF                                                             
004185     END-IF                                                               
004186     .                                                                    
004187     EJECT                                                                
004188 AX-HAEMTA-ADC-WDE4-DISTR SECTION.                                        
004189     MOVE 'AX-HAEMTA-ADC-WDE4-DISTR    ' TO WS-SECTION                    
004190                                                                          
004191     MOVE ZERO           TO WS-KVLEVART-ADC                               
004192     MOVE +0             TO WS-KVEFRS-OLD                                 
004193     MOVE +0             TO WS-ANTAL-RADER-EFR                            
004194     MOVE +0             TO W-ADC-ANTAL                                   
004195     MOVE +1             TO INDX                                          
004196     PERFORM IMS-GU-WDE4C1                                                
004197                                                                          
004198     PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                           
004199        MOVE SEQC-IDDISTR  TO W-401-IDDISTR                               
004200        MOVE SEQC-IDKUNDNR TO W-401-IDKUNDNR                              
004201        MOVE SEQC-IDORDNR5 TO W-401-IDORDNR                               
004202        MOVE SEQC-IDPRODNR TO W-401-IDPRODNR                              
004203        MOVE SEQC-IDPLKLST TO W-401-IDPLKLST                              
004204        MOVE SEQC-IDPURAD  TO W-411-IDPURAD                               
004205        PERFORM IMS-GU-WDE401-11                                          
004206                                                                          
004207        IF ORAD-FLDIRLEV NOT = 'J'                                        
004208           IF KORD-IDDC = MID-IDDC                                        
004209             IF KORD-IDDISTR NOT = +98                                    
004210               COMPUTE W-ADC-ANTAL =                                      
004211                    (ORAD-KVAVBART - ORAD-KVLEVART)                       
004212               ADD W-ADC-ANTAL          TO WS-KVLEVART-ADC                
004213               COMPUTE WS-KVEFRS-OLD = WS-KVEFRS-OLD + W-ADC-ANTAL        
004214               ADD +1 TO WS-ANTAL-RADER-EFR                               
004215               IF INDX <= MAX-IX-16                                       
004216                 MOVE KORD-IDKUNDRF TO WS-IDKUNDRF                        
004217                 MOVE WS-IDORDNR    TO WADC-IDKUNDRF(INDX)                
004218                 MOVE W-ADC-ANTAL   TO WADC-KVLEVART(INDX)                
004219                 MOVE KORD-IDDISTR  TO WADC-IDDISTR (INDX)                
004220                 MOVE KORD-IDKUNDNR TO WADC-IDKUNDNR(INDX)                
004221                 MOVE SPACE         TO L1-TOTQTY-ADC-KOMM                 
004222                 ADD +1 TO INDX                                           
004223               ELSE                                                       
004224                 MOVE 'MORE EFR EXIST,'   TO L1-TOTQTY-ADC-KOMM           
004225               END-IF                                                     
004226             END-IF                                                       
004227           END-IF                                                         
004228        END-IF                                                            
004229        PERFORM IMS-GN-WDE4C1                                             
004230     END-PERFORM                                                          
004231                                                                          
004232* NU HAR VI ALLA SALDO, GÖR REPLACE PÅ WDH1 MED AKTUELLA VÄRDEN           
004233* OM EFR SKALL DELAS MED 2 LÄGG TILL EN RÄKNARE I SLINGAN OVAN            
004234     MOVE +0                               TO INV-KVEFRS-OLD              
004235     IF WS-ANTAL-RADER-EFR < 2                                            
004236       MOVE WS-KVEFRS-OLD                  TO INV-KVEFRS-OLD              
004237     ELSE                                                                 
004238       COMPUTE INV-KVEFRS-OLD = WS-KVEFRS-OLD / 2                         
004239     END-IF                                                               
004240                                                                          
004241**** TA BORT POSTEN PÅ WDH1 OCH SKAPA SEDAN ETT NYTT MED                  
004242**** DAGENS DATUM I DAREGDAT-SORT OM DET ÄR FÖRSTA PRINTNINGEN            
004243     IF WS-IDPRTOMG = 1                                                   
004244       PERFORM IMS-GNP-WDH121                                             
004245       IF SEGMENT-FINNS                                                   
004246         IF INVL-KDSEGKEY = 0                                             
004247           MOVE INVL-IDUSER     TO WS-IDUSER                              
004248         END-IF                                                           
004249       END-IF                                                             
004250       PERFORM IMS-GHU-WDH101                                             
004251       MOVE DLI-IO-AREA-INVA11  TO SPAR-WDH111-AREA                       
004252       PERFORM IMS-GHNP-WDH111                                            
004253                                                                          
004254       PERFORM IMS-DELETE-WDH111                                          
004255       MOVE WS-DAGENS-DATUM     TO SPAR-INV-DAREGDAT-SORT                 
004256                                                                          
004257       PERFORM IMS-GHU-WDH101                                             
004258       MOVE SPAR-WDH111-AREA TO DLI-IO-AREA-INVA11                        
004259       PERFORM IMS-INSERT-WDH111                                          
004260                                                                          
004261       IF SEGMENT-FINNS-REDAN                                             
004262         PERFORM UNTIL SEGMENT-FINNS                                      
004263           ADD +1 TO INV-TISEGKEY                                         
004264           PERFORM IMS-INSERT-WDH111                                      
004265         END-PERFORM                                                      
004266       END-IF                                                             
004267                                                                          
004268       MOVE '0'            TO INVL-KDSEGKEY                               
004269       MOVE WS-IDUSER      TO INVL-IDUSER                                 
004270       PERFORM IMS-INSERT-WDH121                                          
004271       MOVE '1'            TO INVL-KDSEGKEY                               
004272       MOVE MID-IDUSER     TO INVL-IDUSER                                 
004273       PERFORM IMS-INSERT-WDH121                                          
004274       MOVE '2'            TO INVL-KDSEGKEY                               
004275       MOVE SPACE          TO INVL-IDUSER                                 
004276       PERFORM IMS-INSERT-WDH121                                          
004277       MOVE '3'            TO INVL-KDSEGKEY                               
004278       MOVE SPACE          TO INVL-IDUSER                                 
004279       PERFORM IMS-INSERT-WDH121                                          
004280     ELSE                                                                 
004281       PERFORM IMS-REPLACE                                                
004282                                                                          
004283       PERFORM IMS-GNP-WDH121                                             
004284       IF SEGMENT-FINNS                                                   
004285         PERFORM UNTIL SEGMENT-SAKNAS                                     
004286           IF W-IDPRTOMG-ALFA = INVL-KDSEGKEY                             
004287             MOVE MID-IDUSER         TO INVL-IDUSER                       
004288             PERFORM IMS-REPLACE-WDH121                                   
004289           END-IF                                                         
004290           PERFORM IMS-GNP-WDH121                                         
004291         END-PERFORM                                                      
004292       END-IF                                                             
004293     END-IF                                                               
004294     PERFORM AXB-FLYTTA-TILL-LISTA                                        
004295     .                                                                    
004296     EJECT                                                                
004297 AXB-FLYTTA-TILL-LISTA  SECTION.                                          
004298     MOVE 'AXB-FLYTTA-TILL-LISTA       ' TO WS-SECTION                    
004299                                                                          
004300     MOVE WADC-IDKUNDRF (1) TO L1-IDKUNDRF-ADC-1                          
004301     MOVE WADC-IDKUNDRF (2) TO L1-IDKUNDRF-ADC-2                          
004302     MOVE WADC-IDKUNDRF (3) TO L1-IDKUNDRF-ADC-3                          
004303     MOVE WADC-IDKUNDRF (4) TO L1-IDKUNDRF-ADC-4                          
004304     MOVE WADC-IDKUNDRF (5) TO L1-IDKUNDRF-ADC-5                          
004305     MOVE WADC-IDKUNDRF (6) TO L1-IDKUNDRF-ADC-6                          
004306     MOVE WADC-IDKUNDRF (7) TO L1-IDKUNDRF-ADC-7                          
004307     MOVE WADC-IDKUNDRF (8) TO L1-IDKUNDRF-ADC-8                          
004308     MOVE WADC-IDKUNDRF (9) TO L1-IDKUNDRF-ADC-9                          
004309     MOVE WADC-IDKUNDRF(10) TO L1-IDKUNDRF-ADC-10                         
004310     MOVE WADC-IDKUNDRF(11) TO L1-IDKUNDRF-ADC-11                         
004311     MOVE WADC-IDKUNDRF(12) TO L1-IDKUNDRF-ADC-12                         
004312     MOVE WADC-IDKUNDRF(13) TO L1-IDKUNDRF-ADC-13                         
004313     MOVE WADC-IDKUNDRF(14) TO L1-IDKUNDRF-ADC-14                         
004314     MOVE WADC-IDKUNDRF(15) TO L1-IDKUNDRF-ADC-15                         
004315     MOVE WADC-IDKUNDRF(16) TO L1-IDKUNDRF-ADC-16                         
004316                                                                          
004317     MOVE WADC-IDKUNDNR (1) TO L1-IDKUNDNR-ADC-1                          
004318     MOVE WADC-IDKUNDNR (2) TO L1-IDKUNDNR-ADC-2                          
004319     MOVE WADC-IDKUNDNR (3) TO L1-IDKUNDNR-ADC-3                          
004320     MOVE WADC-IDKUNDNR (4) TO L1-IDKUNDNR-ADC-4                          
004321     MOVE WADC-IDKUNDNR (5) TO L1-IDKUNDNR-ADC-5                          
004322     MOVE WADC-IDKUNDNR (6) TO L1-IDKUNDNR-ADC-6                          
004323     MOVE WADC-IDKUNDNR (7) TO L1-IDKUNDNR-ADC-7                          
004324     MOVE WADC-IDKUNDNR (8) TO L1-IDKUNDNR-ADC-8                          
004325     MOVE WADC-IDKUNDNR (9) TO L1-IDKUNDNR-ADC-9                          
004326     MOVE WADC-IDKUNDNR(10) TO L1-IDKUNDNR-ADC-10                         
004327     MOVE WADC-IDKUNDNR(11) TO L1-IDKUNDNR-ADC-11                         
004328     MOVE WADC-IDKUNDNR(12) TO L1-IDKUNDNR-ADC-12                         
004329     MOVE WADC-IDKUNDNR(13) TO L1-IDKUNDNR-ADC-13                         
004330     MOVE WADC-IDKUNDNR(14) TO L1-IDKUNDNR-ADC-14                         
004331     MOVE WADC-IDKUNDNR(15) TO L1-IDKUNDNR-ADC-15                         
004332     MOVE WADC-IDKUNDNR(16) TO L1-IDKUNDNR-ADC-16                         
004333                                                                          
004334     MOVE WADC-IDDISTR  (1) TO L1-IDDISTR-ADC-1                           
004335     MOVE WADC-IDDISTR  (2) TO L1-IDDISTR-ADC-2                           
004336     MOVE WADC-IDDISTR  (3) TO L1-IDDISTR-ADC-3                           
004337     MOVE WADC-IDDISTR  (4) TO L1-IDDISTR-ADC-4                           
004338     MOVE WADC-IDDISTR  (5) TO L1-IDDISTR-ADC-5                           
004339     MOVE WADC-IDDISTR  (6) TO L1-IDDISTR-ADC-6                           
004340     MOVE WADC-IDDISTR  (7) TO L1-IDDISTR-ADC-7                           
004341     MOVE WADC-IDDISTR  (8) TO L1-IDDISTR-ADC-8                           
004342     MOVE WADC-IDDISTR  (9) TO L1-IDDISTR-ADC-9                           
004343     MOVE WADC-IDDISTR (10) TO L1-IDDISTR-ADC-10                          
004344     MOVE WADC-IDDISTR (11) TO L1-IDDISTR-ADC-11                          
004345     MOVE WADC-IDDISTR (12) TO L1-IDDISTR-ADC-12                          
004346     MOVE WADC-IDDISTR (13) TO L1-IDDISTR-ADC-13                          
004347     MOVE WADC-IDDISTR (14) TO L1-IDDISTR-ADC-14                          
004348     MOVE WADC-IDDISTR (15) TO L1-IDDISTR-ADC-15                          
004349     MOVE WADC-IDDISTR (16) TO L1-IDDISTR-ADC-16                          
004350                                                                          
004351     MOVE WADC-KVLEVART (1) TO L1-KVLEVART-ADC-1                          
004352     MOVE WADC-KVLEVART (2) TO L1-KVLEVART-ADC-2                          
004353     MOVE WADC-KVLEVART (3) TO L1-KVLEVART-ADC-3                          
004354     MOVE WADC-KVLEVART (4) TO L1-KVLEVART-ADC-4                          
004355     MOVE WADC-KVLEVART (5) TO L1-KVLEVART-ADC-5                          
004356     MOVE WADC-KVLEVART (6) TO L1-KVLEVART-ADC-6                          
004357     MOVE WADC-KVLEVART (7) TO L1-KVLEVART-ADC-7                          
004358     MOVE WADC-KVLEVART (8) TO L1-KVLEVART-ADC-8                          
004359     MOVE WADC-KVLEVART (9) TO L1-KVLEVART-ADC-9                          
004360     MOVE WADC-KVLEVART(10) TO L1-KVLEVART-ADC-10                         
004361     MOVE WADC-KVLEVART(11) TO L1-KVLEVART-ADC-11                         
004362     MOVE WADC-KVLEVART(12) TO L1-KVLEVART-ADC-12                         
004363     MOVE WADC-KVLEVART(13) TO L1-KVLEVART-ADC-13                         
004364     MOVE WADC-KVLEVART(14) TO L1-KVLEVART-ADC-14                         
004365     MOVE WADC-KVLEVART(15) TO L1-KVLEVART-ADC-15                         
004366     MOVE WADC-KVLEVART(16) TO L1-KVLEVART-ADC-16                         
004367     MOVE WS-KVLEVART-ADC   TO L1-TOTQTY-ADC                              
004368     .                                                                    
004369     EJECT                                                                
004370 AY-ADC-SKRIV-LISTA1 SECTION.                                             
004371     MOVE 'AY-SKRIV-LISTA1             ' TO WS-SECTION                    
004372                                                                          
004373**********************  SKRIVER LAB-LISTAN   *******************          
004374                                                                          
004375* SKRIVER RUBRIKER                                                        
004376                                                                          
004377     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
004378                         PRT-NYSIDA-RAD7 ADC-RUBRAD                       
004379                                                                          
004380* SKRIVER RADER                                                           
004381                                                                          
004382     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
004383                         PRT-AFTER-1 L1-LISTNR-RAD-ADC                    
004384     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
004385                         PRT-AFTER-2 L1-RAD-1-ADC                         
004386     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
004387                         PRT-AFTER-2 L1-RAD-3-ADC                         
004388     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
004389                         PRT-AFTER-1 L1-RAD-4-ADC                         
004390     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
004391                         PRT-AFTER-1 L1-RAD-5-ADC                         
004392     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
004393                         PRT-AFTER-1 L1-RAD-6-ADC                         
004394     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
004395                         PRT-AFTER-1 L1-RAD-7-ADC                         
004396     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
004397                         PRT-AFTER-1 L1-RAD-8-ADC                         
004398     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
004399                         PRT-AFTER-1 L1-RAD-9-ADC                         
004400     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
004401                         PRT-AFTER-1 L1-RAD-10-ADC                        
004402     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
004403                         PRT-AFTER-1 L1-RAD-11-ADC                        
004404     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
004405                         PRT-AFTER-1 L1-RAD-12-ADC                        
004406     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
004407                         PRT-AFTER-1 L1-RAD-13-ADC                        
004408     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
004409                         PRT-AFTER-1 L1-RAD-14-ADC                        
004410     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
004411                         PRT-AFTER-1 L1-RAD-15-ADC                        
004412     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
004413                         PRT-AFTER-1 L1-RAD-16-ADC                        
004414     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
004415                         PRT-AFTER-1 L1-RAD-17-ADC                        
004416     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
004417                         PRT-AFTER-1 L1-RAD-18-ADC                        
004418     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
004419                         PRT-AFTER-1 L1-RAD-19-ADC                        
004420     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
004421                         PRT-AFTER-1 L1-RAD-20-ADC                        
004422     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
004423                         PRT-AFTER-1 L1-RAD-21-ADC                        
004424     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
004425                         PRT-AFTER-2 L1-RAD-23-ADC                        
004426     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
004427                         PRT-AFTER-2 L1-RAD-25-ADC                        
004428     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
004429                         PRT-AFTER-2 L1-RAD-27-ADC                        
004430     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
004431                         PRT-AFTER-2 L1-RAD-29-ADC                        
004432     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
004433                         PRT-AFTER-2 L1-RAD-31-ADC                        
004434     .                                                                    
004435     EJECT                                                                
004436 ET-HAEMTA-UPPG-WDK6-WDK7-NDC SECTION.                                    
004437     MOVE 'ET-HAEMTA-UPPG-WDK6-WDK7-NDC' TO WS-SECTION                    
004438                                                                          
004439     PERFORM ETA-NOLLA-NDC                                                
004440                                                                          
004441     PERFORM IMS-GET-ART-WDK6                                             
004442                                                                          
004443     MOVE ART-KDPRODSL                TO L1-KDPRODSL-NDC                  
004444     MOVE '      PIECES'              TO L1-KDSORT-NDC                    
004445     EVALUATE ART-KDSORT                                                  
004446       WHEN 'KG'  MOVE '    KILOGRAM' TO L1-KDSORT-NDC                    
004447       WHEN ' L'  MOVE '       LITRE' TO L1-KDSORT-NDC                    
004448       WHEN 'L '  MOVE '       LITRE' TO L1-KDSORT-NDC                    
004449       WHEN 'M '  MOVE '       METRE' TO L1-KDSORT-NDC                    
004450       WHEN ' M'  MOVE '       METRE' TO L1-KDSORT-NDC                    
004451       WHEN 'MM'  MOVE '  MILLIMETRE' TO L1-KDSORT-NDC                    
004452       WHEN 'SA'  MOVE '         KIT' TO L1-KDSORT-NDC                    
004453       WHEN 'C2'  MOVE '   SQUARE-CM' TO L1-KDSORT-NDC                    
004454       WHEN 'M2'  MOVE 'SQUARE-METER' TO L1-KDSORT-NDC                    
004455       WHEN 'ML'  MOVE '  MILLILITRE' TO L1-KDSORT-NDC                    
004456       WHEN 'SW'  MOVE '    SOFTWARE' TO L1-KDSORT-NDC                    
004457     END-EVALUATE                                                         
004458                                                                          
004459     PERFORM IMS-GNP-CLAGERINFO                                           
004460     MOVE CLAG-KDPSLLOC               TO L1-KDPSLLOC-NDC                  
004461     MOVE CLAG-KDERS                  TO L1-KDERS-NDC                     
004462     MOVE CLAG-VKART                  TO L1-VKART-NDC                     
004463                                                                          
004464     PERFORM IMS-GET-ART-WDK7                                             
004465     IF SEGMENT-FINNS                                                     
004466       MOVE SLAG-KVLS                 TO L1-KVLS-NDC                      
004467       MOVE SLAG-KVAKS-SDC            TO L1-KVAKS-NDC                     
004468       MOVE SLAG-KVUTRS               TO L1-KVUTRS-NDC                    
004469       MOVE SLAG-ADLAGOMR             TO L1-ADLAGOMR-NDC                  
004470       MOVE SLAG-ADGANG               TO L1-ADGANG-NDC                    
004471       MOVE SLAG-ADPLATS              TO L1-ADPLATS-NDC                   
004472       MOVE SLAG-PRAVCOST             TO L1-PRAVCOST-NDC                  
004473                                                                          
004474       MOVE WS-IDLOPNR                       TO INV-IDLOPNR               
004475       IF WS-IDPRTOMG NOT = INV-IDPRTOMG                                  
004476         MOVE FUNCTION CURRENT-DATE(1:8)     TO INV-DAREGDAT              
004477         EVALUATE WS-IDPRTOMG                                             
004478           WHEN 1                                                         
004479             MOVE FUNCTION CURRENT-DATE(1:8) TO INV-DAREGDAT-PR1          
004480           WHEN 2                                                         
004481             MOVE FUNCTION CURRENT-DATE(1:8) TO INV-DAREGDAT-PR2          
004482           WHEN 3                                                         
004483             MOVE FUNCTION CURRENT-DATE(1:8) TO INV-DAREGDAT-PR3          
004484         END-EVALUATE                                                     
004485       END-IF                                                             
004486       MOVE WS-IDPRTOMG                      TO INV-IDPRTOMG              
004487                                                                          
004488       PERFORM IMS-GET-INVHIST-ROT                                        
004489       IF SEGMENT-FINNS                                                   
004490         PERFORM IMS-GET-INVHIST-SDC                                      
004491         IF SEGMENT-FINNS                                                 
004492           PERFORM S10-KONV-DATUM                                         
004493           MOVE DAT-TIAAVVD           TO L1-TIJUSTDA-NDC                  
004494           MOVE INVH-KVJUSTKV         TO L1-KVJUSTKV-NDC                  
004495           MOVE INVH-KDJUSTYP         TO L1-KDJUSTYP-NDC                  
004496           PERFORM IMS-GET-INVHIST-SDCNASTA                               
004497           IF SEGMENT-FINNS                                               
004498             PERFORM S10-KONV-DATUM                                       
004499             MOVE DAT-TIAAVVD         TO L1-TIJUSTDA-1-NDC                
004500             MOVE INVH-KVJUSTKV       TO L1-KVJUSTKV-1-NDC                
004501             MOVE INVH-KDJUSTYP       TO L1-KDJUSTYP-1-NDC                
004502             PERFORM IMS-GET-INVHIST-SDCNASTA                             
004503             IF SEGMENT-FINNS                                             
004504               PERFORM S10-KONV-DATUM                                     
004505               MOVE DAT-TIAAVVD       TO L1-TIJUSTDA-2-NDC                
004506               MOVE INVH-KVJUSTKV     TO L1-KVJUSTKV-2-NDC                
004507               MOVE INVH-KDJUSTYP     TO L1-KDJUSTYP-2-NDC                
004508             END-IF                                                       
004509           END-IF                                                         
004510         END-IF                                                           
004511       END-IF                                                             
004512     END-IF                                                               
004513     .                                                                    
004514     EJECT                                                                
004515 ETA-NOLLA-NDC SECTION.                                                   
004516                                                                          
004517     MOVE ZERO             TO L1-KVLS-NDC                                 
004518                              L1-KVUTRS-NDC                               
004519                              L1-PRAVCOST-NDC                             
004520                              L1-TIJUSTDA-NDC                             
004521                              L1-KVJUSTKV-NDC                             
004522                              L1-KDJUSTYP-NDC                             
004523                              L1-TIJUSTDA-1-NDC                           
004524                              L1-KVJUSTKV-1-NDC                           
004525                              L1-KDJUSTYP-1-NDC                           
004526                              L1-TIJUSTDA-2-NDC                           
004527                              L1-KVJUSTKV-2-NDC                           
004528                              L1-KDJUSTYP-2-NDC                           
004529     .                                                                    
004530     EJECT                                                                
004531 EU-HAEMTA-UPPG-WDD8-NDC SECTION.                                         
004532     MOVE 'EU-HAEMTA-UPPG-WDD8-NDC     ' TO WS-SECTION                    
004533                                                                          
004534     PERFORM EUA-NOLLSTAELL-NDC                                           
004535     MOVE ' NO'                          TO L1-BUFFER-NDC-7D              
004536     MOVE MID-IDDC                       TO W-IDDC-D8-MIN                 
004537                                            W-IDDC-D8-MAX                 
004538     MOVE +1 TO SALDO-IX                                                  
004539     PERFORM IMS-GU-WDD8B1                                                
004540     PERFORM UNTIL SEGMENT-SAKNAS OR SALDO-IX > +5                        
004547                                                                          
004548       EVALUATE SALDO-IX                                                  
004549         WHEN +1                                                          
004550           MOVE SEQB-ADBUFFOMR         TO L1-ADBUFFOMR-NDC-1              
004551           MOVE SEQB-ADBUFFGANG        TO L1-ADBUFFGANG-NDC-1             
004552           MOVE SEQB-ADBUFFPL          TO L1-ADBUFFPLATS-NDC-1            
004553           COMPUTE WS-QTY-NDC =                                           
004554                  SEQB-KVBUFF-F + SEQB-KVBUFF-OF                          
004555           MOVE WS-QTY-NDC             TO L1-QTY-NDC-1                    
004556           ADD WS-QTY-NDC              TO WS-QTY-NDC-TOT                  
004557         WHEN +2                                                          
004558           MOVE SEQB-ADBUFFOMR         TO L1-ADBUFFOMR-NDC-2              
004559           MOVE SEQB-ADBUFFGANG        TO L1-ADBUFFGANG-NDC-2             
004560           MOVE SEQB-ADBUFFPL          TO L1-ADBUFFPLATS-NDC-2            
004561           COMPUTE WS-QTY-NDC =                                           
004562                  SEQB-KVBUFF-F + SEQB-KVBUFF-OF                          
004563           MOVE WS-QTY-NDC             TO L1-QTY-NDC-2                    
004564           ADD WS-QTY-NDC              TO WS-QTY-NDC-TOT                  
004565         WHEN +3                                                          
004566           MOVE SEQB-ADBUFFOMR         TO L1-ADBUFFOMR-NDC-3              
004567           MOVE SEQB-ADBUFFGANG        TO L1-ADBUFFGANG-NDC-3             
004568           MOVE SEQB-ADBUFFPL          TO L1-ADBUFFPLATS-NDC-3            
004569           COMPUTE WS-QTY-NDC =                                           
004570                  SEQB-KVBUFF-F + SEQB-KVBUFF-OF                          
004571           MOVE WS-QTY-NDC             TO L1-QTY-NDC-3                    
004572           ADD WS-QTY-NDC              TO WS-QTY-NDC-TOT                  
004573         WHEN +4                                                          
004574           MOVE SEQB-ADBUFFOMR         TO L1-ADBUFFOMR-NDC-4              
004575           MOVE SEQB-ADBUFFGANG        TO L1-ADBUFFGANG-NDC-4             
004576           MOVE SEQB-ADBUFFPL          TO L1-ADBUFFPLATS-NDC-4            
004577           COMPUTE WS-QTY-NDC =                                           
004578                  SEQB-KVBUFF-F + SEQB-KVBUFF-OF                          
004579           MOVE WS-QTY-NDC             TO L1-QTY-NDC-4                    
004580           ADD WS-QTY-NDC              TO WS-QTY-NDC-TOT                  
004581         WHEN +5                                                          
004582           MOVE SEQB-ADBUFFOMR         TO L1-ADBUFFOMR-NDC-5              
004583           MOVE SEQB-ADBUFFGANG        TO L1-ADBUFFGANG-NDC-5             
004584           MOVE SEQB-ADBUFFPL          TO L1-ADBUFFPLATS-NDC-5            
004585           COMPUTE WS-QTY-NDC =                                           
004586                  SEQB-KVBUFF-F + SEQB-KVBUFF-OF                          
004587           MOVE WS-QTY-NDC             TO L1-QTY-NDC-5                    
004588           ADD WS-QTY-NDC              TO WS-QTY-NDC-TOT                  
004589         END-EVALUATE                                                     
004590         ADD +1 TO SALDO-IX                                               
004591         PERFORM IMS-GN-WDD8B1                                            
004592     END-PERFORM                                                          
004593     IF SEGMENT-FINNS                                                     
004594       MOVE 'YES'                      TO L1-BUFFER-NDC-7D                
004595       PERFORM UNTIL SEGMENT-SAKNAS                                       
004602                                                                          
004603         COMPUTE WS-QTY-NDC = SEQB-KVBUFF-F +                             
004604                              SEQB-KVBUFF-OF                              
004605         ADD WS-QTY-NDC                TO WS-QTY-NDC-TOT                  
004606         PERFORM IMS-GN-WDD8B1                                            
004607       END-PERFORM                                                        
004608     END-IF                                                               
004609     MOVE WS-QTY-NDC-TOT               TO L1-TOT-QTY-NDC-10               
004610     .                                                                    
004611     EJECT                                                                
004612 EUA-NOLLSTAELL-NDC      SECTION.                                         
004613                                                                          
004614     MOVE +0 TO L1-ADBUFFOMR-NDC-1                                        
004615                L1-ADBUFFOMR-NDC-2                                        
004616                L1-ADBUFFOMR-NDC-3                                        
004617                L1-ADBUFFOMR-NDC-4                                        
004618                L1-ADBUFFOMR-NDC-5                                        
004619                L1-ADBUFFGANG-NDC-1                                       
004620                L1-ADBUFFGANG-NDC-2                                       
004621                L1-ADBUFFGANG-NDC-3                                       
004622                L1-ADBUFFGANG-NDC-4                                       
004623                L1-ADBUFFGANG-NDC-5                                       
004624                L1-ADBUFFPLATS-NDC-1                                      
004625                L1-ADBUFFPLATS-NDC-2                                      
004626                L1-ADBUFFPLATS-NDC-3                                      
004627                L1-ADBUFFPLATS-NDC-4                                      
004628                L1-ADBUFFPLATS-NDC-5                                      
004629                WS-QTY-NDC                                                
004630                WS-QTY-NDC-TOT                                            
004631                L1-QTY-NDC-1                                              
004632                L1-QTY-NDC-2                                              
004633                L1-QTY-NDC-3                                              
004634                L1-QTY-NDC-4                                              
004635                L1-QTY-NDC-5                                              
004636                L1-TOT-QTY-NDC-10                                         
004637     .                                                                    
004638     SKIP2                                                                
004639 EV-HAEMTA-UPPG-WDD3-NDC SECTION.                                         
004640     MOVE 'EV-HAEMTA-UPPG-WDD3-NDC     ' TO WS-SECTION                    
004641                                                                          
004642     MOVE 'US '                TO W-IDSKYLT                               
004643                                                                          
004644     PERFORM IMS-GU-BEN-SEQ                                               
004645     IF SEGMENT-FINNS                                                     
004646       MOVE BEN-TEXT-BEART     TO L1-BEART-NDC                            
004647     ELSE                                                                 
004648       MOVE 'GB '              TO W-IDSKYLT                               
004649       PERFORM IMS-GU-BEN-SEQ                                             
004650       IF SEGMENT-FINNS                                                   
004651         MOVE BEN-TEXT-BEART   TO L1-BEART-NDC                            
004652       ELSE                                                               
004653         MOVE SPACE            TO L1-BEART-NDC                            
004654       END-IF                                                             
004655     END-IF                                                               
004656     .                                                                    
004657     EJECT                                                                
004658 EX-HAEMTA-NDC-WDE4-DISTR SECTION.                                        
004659     MOVE 'EX-HAEMTA-NDC-WDE4-DISTR    ' TO WS-SECTION                    
004660                                                                          
004661     MOVE ZERO           TO WS-KVLEVART-NDC                               
004662     MOVE +0                                TO WCDC-KVLEVART-TOT          
004663     MOVE +0                                TO WS-ANTAL-RADER-EFR         
004664     MOVE +0                                TO WS-KVEFRS-OLD              
004665     MOVE +1 TO INDX                                                      
004666     PERFORM IMS-GU-WDE4C1                                                
004667                                                                          
004668     PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                           
004669        MOVE SEQC-IDDISTR  TO W-401-IDDISTR                               
004670        MOVE SEQC-IDKUNDNR TO W-401-IDKUNDNR                              
004671        MOVE SEQC-IDORDNR5 TO W-401-IDORDNR                               
004672        MOVE SEQC-IDPRODNR TO W-401-IDPRODNR                              
004673        MOVE SEQC-IDPLKLST TO W-401-IDPLKLST                              
004674        MOVE SEQC-IDPURAD  TO W-411-IDPURAD                               
004675        PERFORM IMS-GU-WDE401-11                                          
004676        IF ORAD-FLDIRLEV NOT = 'J'                                        
004677           IF KORD-IDDC = MID-IDDC                                        
004678             IF KORD-IDDISTR NOT = +98                                    
004679               COMPUTE W-NDC-ANTAL =                                      
004680                    (ORAD-KVAVBART - ORAD-KVLEVART)                       
004681               ADD W-NDC-ANTAL          TO WS-KVLEVART-NDC                
004682               IF INDX <= MAX-IX-16                                       
004683                 MOVE KORD-IDKUNDRF TO WS-IDKUNDRF                        
004684                 MOVE WS-IDORDNR    TO WNDC-IDKUNDRF(INDX)                
004685                 MOVE W-NDC-ANTAL   TO WNDC-KVLEVART(INDX)                
004686                 MOVE KORD-IDDISTR  TO WNDC-IDDISTR (INDX)                
004687                 MOVE KORD-IDKUNDNR TO WNDC-IDKUNDNR(INDX)                
004688                 MOVE SPACE         TO L1-TOTQTY-NDC-KOMM                 
004689                 ADD +1 TO INDX                                           
004690               ELSE                                                       
004691                 MOVE 'MORE EFR EXIST,'   TO L1-TOTQTY-NDC-KOMM           
004692               END-IF                                                     
004693             END-IF                                                       
004694           END-IF                                                         
004695        END-IF                                                            
004696        PERFORM IMS-GN-WDE4C1                                             
004697     END-PERFORM                                                          
004698                                                                          
004699* NU HAR VI ALLA SALDO, GÖR REPLACE PÅ WDH1 MED AKTUELLA VÄRDEN           
004700* OM EFR SKALL DELAS MED 2 LÄGG TILL EN RÄKNARE I SLINGAN OVAN            
004701     MOVE +0                               TO INV-KVEFRS-OLD              
004702     IF WS-ANTAL-RADER-EFR < 2                                            
004703       MOVE WS-KVEFRS-OLD                  TO INV-KVEFRS-OLD              
004704     ELSE                                                                 
004705       COMPUTE INV-KVEFRS-OLD = WS-KVEFRS-OLD / 2                         
004706     END-IF                                                               
004707                                                                          
004708**** TA BORT POSTEN PÅ WDH1 OCH SKAPA SEDAN ETT NYTT MED                  
004709**** DAGENS DATUM I DAREGDAT-SORT OM DET ÄR FÖRSTA PRINTNINGEN            
004710     IF WS-IDPRTOMG = 1                                                   
004711       PERFORM IMS-GNP-WDH121                                             
004712       IF SEGMENT-FINNS                                                   
004713         IF INVL-KDSEGKEY = 0                                             
004714           MOVE INVL-IDUSER     TO WS-IDUSER                              
004715         END-IF                                                           
004716       END-IF                                                             
004717       PERFORM IMS-GHU-WDH101                                             
004718       MOVE DLI-IO-AREA-INVA11  TO SPAR-WDH111-AREA                       
004719       PERFORM IMS-GHNP-WDH111                                            
004720                                                                          
004721       PERFORM IMS-DELETE-WDH111                                          
004722       MOVE WS-DAGENS-DATUM     TO SPAR-INV-DAREGDAT-SORT                 
004723                                                                          
004724       PERFORM IMS-GHU-WDH101                                             
004725       MOVE SPAR-WDH111-AREA TO DLI-IO-AREA-INVA11                        
004726       PERFORM IMS-INSERT-WDH111                                          
004727                                                                          
004728       IF SEGMENT-FINNS-REDAN                                             
004729         PERFORM UNTIL SEGMENT-FINNS                                      
004730           ADD +1 TO INV-TISEGKEY                                         
004731           PERFORM IMS-INSERT-WDH111                                      
004732         END-PERFORM                                                      
004733       END-IF                                                             
004734                                                                          
004735       MOVE '0'            TO INVL-KDSEGKEY                               
004736       MOVE WS-IDUSER      TO INVL-IDUSER                                 
004737       PERFORM IMS-INSERT-WDH121                                          
004738       MOVE '1'            TO INVL-KDSEGKEY                               
004739       MOVE MID-IDUSER     TO INVL-IDUSER                                 
004740       PERFORM IMS-INSERT-WDH121                                          
004741       MOVE '2'            TO INVL-KDSEGKEY                               
004742       MOVE SPACE          TO INVL-IDUSER                                 
004743       PERFORM IMS-INSERT-WDH121                                          
004744       MOVE '3'            TO INVL-KDSEGKEY                               
004745       MOVE SPACE          TO INVL-IDUSER                                 
004746       PERFORM IMS-INSERT-WDH121                                          
004747     ELSE                                                                 
004748       PERFORM IMS-REPLACE                                                
004749       MOVE 'REPLACE PÅ WDH111     ' TO WS-SECTION                        
004750       PERFORM IMS-GNP-WDH121                                             
004751       MOVE 'GJORT GNP FÖR WDH121  ' TO WS-SECTION                        
004752       IF SEGMENT-FINNS                                                   
004753         PERFORM UNTIL SEGMENT-SAKNAS                                     
004754           IF W-IDPRTOMG-ALFA = INVL-KDSEGKEY                             
004755             MOVE MID-IDUSER         TO INVL-IDUSER                       
004756             MOVE 'SKA GÖRA REPLACE PÅ 21' TO WS-SECTION                  
004757             PERFORM IMS-REPLACE-WDH121                                   
004758             MOVE 'GJORT REPL PÅ 21      ' TO WS-SECTION                  
004759           END-IF                                                         
004760           PERFORM IMS-GNP-WDH121                                         
004761         END-PERFORM                                                      
004762       END-IF                                                             
004763     END-IF                                                               
004764**                                                                        
004765     PERFORM EXB-FLYTTA-TILL-LISTA                                        
004766     .                                                                    
004767     EJECT                                                                
004768 EXB-FLYTTA-TILL-LISTA  SECTION.                                          
004769     MOVE 'EXB-FLYTTA-TILL-LISTA       ' TO WS-SECTION                    
004770                                                                          
004771     MOVE WNDC-IDKUNDRF (1) TO L1-IDKUNDRF-NDC-1                          
004772     MOVE WNDC-IDKUNDRF (2) TO L1-IDKUNDRF-NDC-2                          
004773     MOVE WNDC-IDKUNDRF (3) TO L1-IDKUNDRF-NDC-3                          
004774     MOVE WNDC-IDKUNDRF (4) TO L1-IDKUNDRF-NDC-4                          
004775     MOVE WNDC-IDKUNDRF (5) TO L1-IDKUNDRF-NDC-5                          
004776     MOVE WNDC-IDKUNDRF (6) TO L1-IDKUNDRF-NDC-6                          
004777     MOVE WNDC-IDKUNDRF (7) TO L1-IDKUNDRF-NDC-7                          
004778     MOVE WNDC-IDKUNDRF (8) TO L1-IDKUNDRF-NDC-8                          
004779     MOVE WNDC-IDKUNDRF (9) TO L1-IDKUNDRF-NDC-9                          
004780     MOVE WNDC-IDKUNDRF(10) TO L1-IDKUNDRF-NDC-10                         
004781     MOVE WNDC-IDKUNDRF(11) TO L1-IDKUNDRF-NDC-11                         
004782     MOVE WNDC-IDKUNDRF(12) TO L1-IDKUNDRF-NDC-12                         
004783     MOVE WNDC-IDKUNDRF(13) TO L1-IDKUNDRF-NDC-13                         
004784     MOVE WNDC-IDKUNDRF(14) TO L1-IDKUNDRF-NDC-14                         
004785     MOVE WNDC-IDKUNDRF(15) TO L1-IDKUNDRF-NDC-15                         
004786     MOVE WNDC-IDKUNDRF(16) TO L1-IDKUNDRF-NDC-16                         
004787                                                                          
004788     MOVE WNDC-IDKUNDNR (1) TO L1-IDKUNDNR-NDC-1                          
004789     MOVE WNDC-IDKUNDNR (2) TO L1-IDKUNDNR-NDC-2                          
004790     MOVE WNDC-IDKUNDNR (3) TO L1-IDKUNDNR-NDC-3                          
004791     MOVE WNDC-IDKUNDNR (4) TO L1-IDKUNDNR-NDC-4                          
004792     MOVE WNDC-IDKUNDNR (5) TO L1-IDKUNDNR-NDC-5                          
004793     MOVE WNDC-IDKUNDNR (6) TO L1-IDKUNDNR-NDC-6                          
004794     MOVE WNDC-IDKUNDNR (7) TO L1-IDKUNDNR-NDC-7                          
004795     MOVE WNDC-IDKUNDNR (8) TO L1-IDKUNDNR-NDC-8                          
004796     MOVE WNDC-IDKUNDNR (9) TO L1-IDKUNDNR-NDC-9                          
004797     MOVE WNDC-IDKUNDNR(10) TO L1-IDKUNDNR-NDC-10                         
004798     MOVE WNDC-IDKUNDNR(11) TO L1-IDKUNDNR-NDC-11                         
004799     MOVE WNDC-IDKUNDNR(12) TO L1-IDKUNDNR-NDC-12                         
004800     MOVE WNDC-IDKUNDNR(13) TO L1-IDKUNDNR-NDC-13                         
004801     MOVE WNDC-IDKUNDNR(14) TO L1-IDKUNDNR-NDC-14                         
004802     MOVE WNDC-IDKUNDNR(15) TO L1-IDKUNDNR-NDC-15                         
004803     MOVE WNDC-IDKUNDNR(16) TO L1-IDKUNDNR-NDC-16                         
004804                                                                          
004805     MOVE WNDC-IDDISTR  (1) TO L1-IDDISTR-NDC-1                           
004806     MOVE WNDC-IDDISTR  (2) TO L1-IDDISTR-NDC-2                           
004807     MOVE WNDC-IDDISTR  (3) TO L1-IDDISTR-NDC-3                           
004808     MOVE WNDC-IDDISTR  (4) TO L1-IDDISTR-NDC-4                           
004809     MOVE WNDC-IDDISTR  (5) TO L1-IDDISTR-NDC-5                           
004810     MOVE WNDC-IDDISTR  (6) TO L1-IDDISTR-NDC-6                           
004811     MOVE WNDC-IDDISTR  (7) TO L1-IDDISTR-NDC-7                           
004812     MOVE WNDC-IDDISTR  (8) TO L1-IDDISTR-NDC-8                           
004813     MOVE WNDC-IDDISTR  (9) TO L1-IDDISTR-NDC-9                           
004814     MOVE WNDC-IDDISTR (10) TO L1-IDDISTR-NDC-10                          
004815     MOVE WNDC-IDDISTR (11) TO L1-IDDISTR-NDC-11                          
004816     MOVE WNDC-IDDISTR (12) TO L1-IDDISTR-NDC-12                          
004817     MOVE WNDC-IDDISTR (13) TO L1-IDDISTR-NDC-13                          
004818     MOVE WNDC-IDDISTR (14) TO L1-IDDISTR-NDC-14                          
004819     MOVE WNDC-IDDISTR (15) TO L1-IDDISTR-NDC-15                          
004820     MOVE WNDC-IDDISTR (16) TO L1-IDDISTR-NDC-16                          
004821                                                                          
004822     MOVE WNDC-KVLEVART (1) TO L1-KVLEVART-NDC-1                          
004823     MOVE WNDC-KVLEVART (2) TO L1-KVLEVART-NDC-2                          
004824     MOVE WNDC-KVLEVART (3) TO L1-KVLEVART-NDC-3                          
004825     MOVE WNDC-KVLEVART (4) TO L1-KVLEVART-NDC-4                          
004826     MOVE WNDC-KVLEVART (5) TO L1-KVLEVART-NDC-5                          
004827     MOVE WNDC-KVLEVART (6) TO L1-KVLEVART-NDC-6                          
004828     MOVE WNDC-KVLEVART (7) TO L1-KVLEVART-NDC-7                          
004829     MOVE WNDC-KVLEVART (8) TO L1-KVLEVART-NDC-8                          
004830     MOVE WNDC-KVLEVART (9) TO L1-KVLEVART-NDC-9                          
004831     MOVE WNDC-KVLEVART(10) TO L1-KVLEVART-NDC-10                         
004832     MOVE WNDC-KVLEVART(11) TO L1-KVLEVART-NDC-11                         
004833     MOVE WNDC-KVLEVART(12) TO L1-KVLEVART-NDC-12                         
004834     MOVE WNDC-KVLEVART(13) TO L1-KVLEVART-NDC-13                         
004835     MOVE WNDC-KVLEVART(14) TO L1-KVLEVART-NDC-14                         
004836     MOVE WNDC-KVLEVART(15) TO L1-KVLEVART-NDC-15                         
004837     MOVE WNDC-KVLEVART(16) TO L1-KVLEVART-NDC-16                         
004838     MOVE WS-KVLEVART-NDC   TO L1-TOTQTY-NDC                              
004839     .                                                                    
004840     EJECT                                                                
004841 EY-NDC-SKRIV-LISTA1 SECTION.                                             
004842     MOVE 'EY-NDC-SKRIV-LISTA1         ' TO WS-SECTION                    
004843                                                                          
004844**********************  SKRIVER LAB-LISTAN   *******************          
004845                                                                          
004846* SKRIVER RUBRIKER                                                        
004847                                                                          
004848     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
004849                         PRT-NYSIDA-RAD7 NDC-RUBRAD                       
004850                                                                          
004851* SKRIVER RADER                                                           
004852                                                                          
004853     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
004854                         PRT-AFTER-2 L1-RAD-1-NDC                         
004855     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
004856                         PRT-AFTER-2 L1-RAD-3-NDC                         
004857     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
004858                         PRT-AFTER-1 L1-RAD-4-NDC                         
004859     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
004860                         PRT-AFTER-1 L1-RAD-5-NDC                         
004861     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
004862                         PRT-AFTER-1 L1-RAD-6-NDC                         
004863     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
004864                         PRT-AFTER-1 L1-RAD-7-NDC                         
004865     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
004866                         PRT-AFTER-1 L1-RAD-8-NDC                         
004867     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
004868                         PRT-AFTER-1 L1-RAD-9-NDC                         
004869     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
004870                         PRT-AFTER-1 L1-RAD-10-NDC                        
004871     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
004872                         PRT-AFTER-1 L1-RAD-11-NDC                        
004873     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
004874                         PRT-AFTER-1 L1-RAD-12-NDC                        
004875     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
004876                         PRT-AFTER-1 L1-RAD-13-NDC                        
004877     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
004878                         PRT-AFTER-1 L1-RAD-14-NDC                        
004879     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
004880                         PRT-AFTER-1 L1-RAD-15-NDC                        
004881     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
004882                         PRT-AFTER-1 L1-RAD-16-NDC                        
004883     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
004884                         PRT-AFTER-1 L1-RAD-17-NDC                        
004885     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
004886                         PRT-AFTER-1 L1-RAD-18-NDC                        
004887     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
004888                         PRT-AFTER-1 L1-RAD-19-NDC                        
004889     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
004890                         PRT-AFTER-1 L1-RAD-20-NDC                        
004891     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
004892                         PRT-AFTER-1 L1-RAD-21-NDC                        
004893     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
004894                         PRT-AFTER-2 L1-RAD-23-NDC                        
004895     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
004896                         PRT-AFTER-2 L1-RAD-25-NDC                        
004897     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
004898                         PRT-AFTER-2 L1-RAD-27-NDC                        
004899     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
004900                         PRT-AFTER-2 L1-RAD-29-NDC                        
004901     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE WS-PRT1 ALT-PCB          
004902                         PRT-AFTER-2 L1-RAD-31-NDC                        
004903     .                                                                    
004904     EJECT                                                                
004905 EZ-STAENG-PRINTER SECTION.                                               
004906     MOVE 'EZ-STAENG-PRINTER           ' TO WS-SECTION                    
004907                                                                          
004908     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-CLOSE WS-PRT1 ALT-PCB          
004909                                          DUMMY-AREA DUMMY-AREA           
004910     .                                                                    
004911     EJECT                                                                
004912 S10-KONV-DATUM SECTION.                                                  
004913                                                                          
004914     MOVE INVH-DAREGDAT-CLO(3:6)  TO DAT-I-TIDATUM                        
004915     MOVE 'AAMMDD'                TO DAT-KDDATFORM                        
004916                                                                          
004917     CALL WDATKONV USING             DAT-KDDATFORM                        
004918                                     DAT-I-TIDATUM                        
004919                                     DAT-O-TIDATUM                        
004920                                     DAT-KDSVAR                           
004921     IF NOT DAT-KDSVAR-OK                                                 
004922       MOVE ZERO                  TO DAT-TIAAVVD                          
004923     END-IF                                                               
004924     .                                                                    
004925     EJECT                                                                
004926 MFS-RENSA-BILD SECTION.                                                  
004927                                                                          
004928     MOVE MFS-RENSA-FAELT TO MOD-KVINVSKR                                 
004929                             MOD-ADLAGOMR                                 
004930                             MOD-IDARTNR                                  
004931     .                                                                    
004932     EJECT                                                                
004933* IMS SEKTIONER                                                           
004934     SKIP3                                                                
004935 IMS-GET-MSG SECTION.                                                     
004936     MOVE '  QC' TO GODK-STATUSKODER                                      
004937     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
004938     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
004939     PERFORM IMS-STATUSKONTROLL                                           
004940     .                                                                    
004941     SKIP3                                                                
004942 IMS-INSERT-MSG SECTION.                                                  
004943     IF ENGLISH-TEXT                                                      
004944       MOVE 'N' TO MFS-KDHUVOMR                                           
004945     END-IF                                                               
004946     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
004947     MOVE SPACE TO GODK-STATUSKODER                                       
004948     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
004949     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
004950     PERFORM IMS-STATUSKONTROLL                                           
004951     .                                                                    
004952     EJECT                                                                
004953******* OPERATIONER MOT WDK6                                              
004954                                                                          
004955 IMS-GET-ART-WDK6 SECTION.                                                
004956     MOVE 'IMS-GET-ART-WDK61     ' TO WS-IMS                              
004957                                                                          
004958     MOVE SPACE                 TO ALL-SSA                                
004959     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
004960            DELIMITED BY SIZE INTO SSA1                                   
004961     MOVE '  '                  TO GODK-STATUSKODER                       
004962     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA SSA1                      
004963     MOVE ARTC-STATUS-CODE      TO STATUS-WS                              
004964     PERFORM IMS-STATUSKONTROLL                                           
004965     .                                                                    
004966     SKIP3                                                                
004967 IMS-GNP-CLAGERINFO SECTION.                                              
004968     MOVE 'IMS-GNP-CLAGERINFO    ' TO WS-IMS                              
004969                                                                          
004970     MOVE SPACE            TO ALL-SSA                                     
004971     MOVE 'WLARTC11 ' TO SSA1                                             
004972     MOVE '  '             TO GODK-STATUSKODER                            
004973     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA SSA1                     
004974     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
004975     PERFORM IMS-STATUSKONTROLL                                           
004976     .                                                                    
004977     EJECT                                                                
004978******* OPERATIONER MOT WDK7                                              
004979     SKIP2                                                                
004980 IMS-GET-ART-WDK7 SECTION.                                                
004981     MOVE 'IMS-GET-ART-WDK7   ' TO WS-IMS                                 
004982                                                                          
004983     MOVE SPACE                 TO ALL-SSA                                
004984     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
004985            DELIMITED BY SIZE INTO SSA1                                   
004986     STRING 'WLARTS11(IDDC     =' W-IDDC ')'                              
004987            DELIMITED BY SIZE INTO SSA2                                   
004988     MOVE '  GE'                TO GODK-STATUSKODER                       
004989     CALL CBLTDLI USING GU ARTS-PCB DLI-IO-AREA4 SSA1 SSA2                
004990     MOVE ARTS-STATUS-CODE      TO STATUS-WS                              
004991     PERFORM IMS-STATUSKONTROLL                                           
004992     .                                                                    
004993     SKIP3                                                                
004994****** INVENTERINGSHISTORIK WDH7                                          
004995                                                                          
004996 IMS-GET-INVHIST-ROT SECTION.                                             
004997     MOVE 'IMS-GET-ART-WDK7   ' TO WS-IMS                                 
004998                                                                          
004999     MOVE SPACE                 TO ALL-SSA                                
005000     STRING 'WLINVC01(IDARTNR  =' W-IDARTNR-X ')'                         
005001            DELIMITED BY SIZE INTO SSA1                                   
005002     MOVE '  GE'                TO GODK-STATUSKODER                       
005003     CALL CBLTDLI USING GU INVC-PCB DLI-IO-AREA5 SSA1                     
005004     MOVE INVC-STATUS-CODE      TO STATUS-WS                              
005005     PERFORM IMS-STATUSKONTROLL                                           
005006     .                                                                    
005007     SKIP2                                                                
005008 IMS-GET-INVHIST-SDC SECTION.                                             
005009     MOVE 'IMS-GET-INVHIST-SDC' TO WS-IMS                                 
005010                                                                          
005011     MOVE SPACE                 TO ALL-SSA                                
005012     STRING 'WLINVC11*F(TISEGKEY<=' W-TISEGKEY-X                          
005013                    '&IDDC     =' W-IDDC ')'                              
005014            DELIMITED BY SIZE INTO SSA1                                   
005015     MOVE '  GE'                TO GODK-STATUSKODER                       
005016     CALL CBLTDLI USING GNP INVC-PCB DLI-IO-AREA5 SSA1                    
005017     MOVE INVC-STATUS-CODE      TO STATUS-WS                              
005018     PERFORM IMS-STATUSKONTROLL                                           
005019     .                                                                    
005020     SKIP2                                                                
005021 IMS-GET-INVHIST-SDCNASTA SECTION.                                        
005022     MOVE 'IMS-GET-INVHIST-SDCNASTA' TO WS-IMS                            
005023                                                                          
005024     MOVE SPACE                 TO ALL-SSA                                
005025     STRING 'WLINVC11(TISEGKEY<=' W-TISEGKEY-X                            
005026                    '&IDDC     =' W-IDDC ')'                              
005027            DELIMITED BY SIZE INTO SSA1                                   
005028     MOVE '  GE'                TO GODK-STATUSKODER                       
005029     CALL CBLTDLI USING GNP INVC-PCB DLI-IO-AREA5 SSA1                    
005030     MOVE INVC-STATUS-CODE      TO STATUS-WS                              
005031     PERFORM IMS-STATUSKONTROLL                                           
005032     .                                                                    
005033     SKIP2                                                                
005034 IMS-GET-INVHIST-CDCFIRST SECTION.                                        
005035     MOVE 'IMS-GET-INVHIST-CDCFIRST' TO WS-IMS                            
005036                                                                          
005037     MOVE SPACE                 TO ALL-SSA                                
005038     STRING 'WLINVC11*F(TISEGKEY<=' W-TISEGKEY-X                          
005039                    '&IDDC     =' W-IDDC ')'                              
005040            DELIMITED BY SIZE INTO SSA1                                   
005041     MOVE '  GE'                TO GODK-STATUSKODER                       
005042     CALL CBLTDLI USING GNP INVC-PCB DLI-IO-AREA5 SSA1                    
005043     MOVE INVC-STATUS-CODE      TO STATUS-WS                              
005044     PERFORM IMS-STATUSKONTROLL                                           
005045     .                                                                    
005046     EJECT                                                                
005047 IMS-GET-INVHIST-CDC SECTION.                                             
005048     MOVE 'IMS-GET-INVHIST-CDC' TO WS-IMS                                 
005049                                                                          
005050     MOVE SPACE                 TO ALL-SSA                                
005051     STRING 'WLINVC11(TISEGKEY<=' W-TISEGKEY-X                            
005052                    '&IDDC     =' W-IDDC ')'                              
005053            DELIMITED BY SIZE INTO SSA1                                   
005054     MOVE '  GE'                TO GODK-STATUSKODER                       
005055     CALL CBLTDLI USING GNP INVC-PCB DLI-IO-AREA5 SSA1                    
005056     MOVE INVC-STATUS-CODE      TO STATUS-WS                              
005057     PERFORM IMS-STATUSKONTROLL                                           
005058     .                                                                    
005059     EJECT                                                                
005060******* OPERATIONER MOT WDD8                                              
005061                                                                          
005075 IMS-GN-WDD8B1 SECTION.                                                   
005076                                                                          
005077     STRING 'WDD8B1  (WDD8B1KY=>' W-WDD8B1KY-MIN-X                        
005078                    '&WDD8B1KY=<' W-WDD8B1KY-MAX-X ')'                    
005080            DELIMITED BY SIZE INTO SSA1                                   
005081     MOVE '  GEGB' TO GODK-STATUSKODER                                    
005082     CALL CBLTDLI USING GN WDD8B-PCB DLI-IO-WDD8B1 SSA1                   
005083     MOVE WDD8B-STATUS-CODE TO STATUS-WS                                  
005084     PERFORM IMS-STATUSKONTROLL                                           
005085     .                                                                    
005086     EJECT                                                                
005087 IMS-GU-WDD8B1 SECTION.                                                   
005088                                                                          
005089     STRING 'WDD8B1  (WDD8B1KY=>' W-WDD8B1KY-MIN-X                        
005090                    '&WDD8B1KY=<' W-WDD8B1KY-MAX-X ')'                    
005092            DELIMITED BY SIZE INTO SSA1                                   
005093     MOVE '  GE' TO GODK-STATUSKODER                                      
005094     CALL CBLTDLI USING GU WDD8B-PCB DLI-IO-WDD8B1 SSA1                   
005095     MOVE WDD8B-STATUS-CODE TO STATUS-WS                                  
005096     PERFORM IMS-STATUSKONTROLL                                           
005097     .                                                                    
005098     EJECT                                                                
005099******* OPERATIONER MOT WDH1                                              
005100     SKIP2                                                                
005110 IMS-GHU-WDH101 SECTION.                                                  
005111     MOVE 'IMS-GHU-WDH101      ' TO WS-IMS                                
005112                                                                          
005113     MOVE SPACE                  TO ALL-SSA                               
005114     STRING 'WDH101  (IDARTNR  =' W-IDARTNR-X ')'                         
005115             DELIMITED BY SIZE INTO SSA1                                  
005116     MOVE '  GE'                 TO GODK-STATUSKODER                      
005117     CALL CBLTDLI USING GU INV-PCB DLI-IO-AREA-INVA01 SSA1                
005118     MOVE INV-STATUS-CODE        TO STATUS-WS                             
005119     PERFORM IMS-STATUSKONTROLL                                           
005120     .                                                                    
005121     SKIP2                                                                
005122 IMS-GHNP-WDH111 SECTION.                                                 
005123     MOVE 'IMS-GHNP-WDH111     ' TO WS-IMS                                
005124                                                                          
005125     MOVE SPACE                  TO ALL-SSA                               
005126     STRING 'WDH111  (WDH111KY>=' W-WDH111KY-MIN-X                        
005127                    '&WDH111KY<=' W-WDH111KY-MAX-X                        
005128                    '&FLINVBEH =' NEJ ')'                                 
005129             DELIMITED BY SIZE INTO SSA1                                  
005130     MOVE '  GE'                 TO GODK-STATUSKODER                      
005131     CALL CBLTDLI USING GHNP INV-PCB DLI-IO-AREA-INVA11 SSA1              
005132     MOVE INV-STATUS-CODE        TO STATUS-WS                             
005133     PERFORM IMS-STATUSKONTROLL                                           
005134     .                                                                    
005135     SKIP2                                                                
005136 IMS-DELETE-WDH111 SECTION.                                               
005137     MOVE 'IMS-DELETE-WDH111     ' TO WS-IMS                              
005138                                                                          
005139     MOVE SPACE                    TO ALL-SSA                             
005140     MOVE 'IMS-DELETE-WDH111     ' TO WS-IMS                              
005141     MOVE 'WDH111 ' TO SSA1                                               
005142     MOVE '  '                     TO GODK-STATUSKODER                    
005143     CALL CBLTDLI USING DLET INV-PCB DLI-IO-AREA-INVA11                   
005144     MOVE INV-STATUS-CODE          TO STATUS-WS                           
005145     PERFORM IMS-STATUSKONTROLL                                           
005146     .                                                                    
005147     SKIP2                                                                
005148 IMS-INSERT-WDH111 SECTION.                                               
005149     MOVE 'IMS-INSERT-WDH111    ' TO WS-IMS                               
005150                                                                          
005151     MOVE SPACE           TO ALL-SSA                                      
005152     MOVE 'WDH111 '       TO SSA1                                         
005153     MOVE '  II' TO GODK-STATUSKODER                                      
005154     CALL CBLTDLI USING ISRT INV-PCB DLI-IO-AREA-INVA11 SSA1              
005155     MOVE INV-STATUS-CODE TO STATUS-WS                                    
005156     PERFORM IMS-STATUSKONTROLL                                           
005157     .                                                                    
005158     SKIP3                                                                
005159 IMS-GNP-WDH121 SECTION.                                                  
005160     MOVE 'IMS-GNP-WDH121        ' TO WS-IMS                              
005161                                                                          
005162     MOVE SPACE           TO ALL-SSA                                      
005163     MOVE 'WDH121 '       TO SSA1                                         
005164     MOVE '  GE'          TO GODK-STATUSKODER                             
005165     CALL CBLTDLI USING GHNP INV-PCB DLI-IO-AREA-INVA21 SSA1              
005166     MOVE INV-STATUS-CODE TO STATUS-WS                                    
005167     PERFORM IMS-STATUSKONTROLL                                           
005168     .                                                                    
005169     SKIP2                                                                
005170 IMS-REPLACE SECTION.                                                     
005171     MOVE 'IMS-REPLACE           ' TO WS-IMS                              
005172                                                                          
005173     MOVE SPACE           TO ALL-SSA                                      
005174     MOVE '  '            TO GODK-STATUSKODER                             
005175     CALL CBLTDLI USING REPL INV-PCB DLI-IO-AREA-INVA11                   
005176     MOVE INV-STATUS-CODE TO STATUS-WS                                    
005177     PERFORM IMS-STATUSKONTROLL                                           
005178     .                                                                    
005179     EJECT                                                                
005180 IMS-REPLACE-WDH121 SECTION.                                              
005181     MOVE 'IMS-REPLACE           ' TO WS-IMS                              
005182                                                                          
005183     MOVE SPACE           TO ALL-SSA                                      
005184     MOVE '  '            TO GODK-STATUSKODER                             
005185     CALL CBLTDLI USING REPL INV-PCB DLI-IO-AREA-INVA21                   
005186     MOVE INV-STATUS-CODE TO STATUS-WS                                    
005187     PERFORM IMS-STATUSKONTROLL                                           
005188     .                                                                    
005189     EJECT                                                                
005190 IMS-INSERT-WDH121 SECTION.                                               
005191     MOVE 'IMS-INSERT-WDH121     ' TO WS-IMS                              
005192                                                                          
005193     MOVE SPACE           TO ALL-SSA                                      
005194     MOVE 'WDH121 '       TO SSA1                                         
005195     MOVE '  '            TO GODK-STATUSKODER                             
005196     CALL CBLTDLI USING ISRT INV-PCB DLI-IO-AREA-INVA21 SSA1              
005197     MOVE INV-STATUS-CODE TO STATUS-WS                                    
005198     PERFORM IMS-STATUSKONTROLL                                           
005199     .                                                                    
005200     EJECT                                                                
005201******* OPERATIONER MOT WDE4                                              
005202     SKIP2                                                                
005203 IMS-GU-WDE4C1 SECTION.                                                   
005204     MOVE 'IMS-GU-WDE4C1     ' TO WS-IMS                                  
005205                                                                          
005206     MOVE SPACE                  TO ALL-SSA                               
005207     STRING 'WDE4C1  (WDE4C1KY>=' W-WDE4C1KY-LOW                          
005208                    '&WDE4C1KY<=' W-WDE4C1KY-HIGH                         
005209                    '&KDRADSTA <' W-ORDSTA-X ')'                          
005210             DELIMITED BY SIZE INTO SSA1                                  
005211     MOVE '  GE'                 TO GODK-STATUSKODER                      
005212     CALL CBLTDLI USING GU WDE4C-PCB DLI-IO-E4C1 SSA1                     
005213     MOVE WDE4C-STATUS-CODE      TO STATUS-WS                             
005214     PERFORM IMS-STATUSKONTROLL                                           
005215     .                                                                    
005216     SKIP2                                                                
005217 IMS-GN-WDE4C1         SECTION.                                           
005218     MOVE 'IMS-GN-WDE4C1       ' TO WS-IMS                                
005219                                                                          
005220     STRING 'WDE4C1  (WDE4C1KY>=' W-WDE4C1KY-LOW                          
005221                    '&WDE4C1KY<=' W-WDE4C1KY-HIGH                         
005222                    '&KDRADSTA <' W-ORDSTA-X ')'                          
005223             DELIMITED BY SIZE INTO SSA1                                  
005224     MOVE '  GEGB'               TO GODK-STATUSKODER                      
005225     CALL CBLTDLI USING GN WDE4C-PCB DLI-IO-E4C1 SSA1                     
005226     MOVE WDE4C-STATUS-CODE      TO STATUS-WS                             
005227     PERFORM IMS-STATUSKONTROLL                                           
005228     .                                                                    
005229     EJECT                                                                
005230 IMS-GU-WDE401-11   SECTION.                                              
005231     MOVE 'IMS-GU-E401-11'  TO WS-IMS                                     
005232                                                                          
005233     MOVE SPACE                  TO ALL-SSA                               
005234     STRING 'WDE401  *D(WDE401KY =' W-WDE401-X ')'                        
005235             DELIMITED BY SIZE INTO SSA1                                  
005236     STRING 'WDE411  (IDPURAD  =' W-WDE411-X ')'                          
005237            DELIMITED BY SIZE INTO SSA2                                   
005238     MOVE '    '                 TO GODK-STATUSKODER                      
005239     CALL CBLTDLI USING GU WDE4-PCB DLI-IO-E401-11 SSA1 SSA2              
005240     MOVE WDE4-STATUS-CODE      TO STATUS-WS                              
005241     PERFORM IMS-STATUSKONTROLL                                           
005242     .                                                                    
005243     EJECT                                                                
005244 IMS-GU-WDE4C-DISTR-98 SECTION.                                           
005245     MOVE 'IMS-GU-WDE4C-DISTR-98' TO WS-IMS                               
005246                                                                          
005247     MOVE SPACE                  TO ALL-SSA                               
005248     STRING 'WDE4C1  (WDE4C1KY=>' W-WDE4C1KY-LOW                          
005249             '&WDE4C1KY=<' W-WDE4C1KY-HIGH                                
005250             '&IDDISTR  =' W-IDDISTR-X ')'                                
005251             DELIMITED BY SIZE INTO SSA1                                  
005252     MOVE '  GEGB'               TO GODK-STATUSKODER                      
005253     CALL CBLTDLI USING GU WDE4C-PCB DLI-IO-E4C1 SSA1                     
005254     MOVE WDE4C-STATUS-CODE      TO STATUS-WS                             
005255     PERFORM IMS-STATUSKONTROLL                                           
005256     .                                                                    
005257     SKIP2                                                                
005258 IMS-GN-WDE4C-DISTR-98 SECTION.                                           
005259     MOVE 'IMS-GN-WDE4C-DISTR-98' TO WS-IMS                               
005260                                                                          
005261     MOVE SPACE                  TO ALL-SSA                               
005262     STRING 'WDE4C1  (WDE4C1KY=>' W-WDE4C1KY-LOW                          
005263             '&WDE4C1KY=<' W-WDE4C1KY-HIGH                                
005264             '&IDDISTR  =' W-IDDISTR-X ')'                                
005265             DELIMITED BY SIZE INTO SSA1                                  
005266     MOVE '  GEGB'               TO GODK-STATUSKODER                      
005267     CALL CBLTDLI USING GN WDE4C-PCB DLI-IO-E4C1 SSA1                     
005268     MOVE WDE4C-STATUS-CODE      TO STATUS-WS                             
005269     PERFORM IMS-STATUSKONTROLL                                           
005270     .                                                                    
005271     EJECT                                                                
005272 IMS-GU-WDE4-ROT SECTION.                                                 
005273     MOVE 'IMS-GU-WDE4-ROT     ' TO WS-IMS                                
005274                                                                          
005275     MOVE SPACE                  TO ALL-SSA                               
005276     STRING 'WDE401  (WDE401KY =' W-WDE4KEY-X ')'                         
005277             DELIMITED BY SIZE INTO SSA1                                  
005278     MOVE '    '                 TO GODK-STATUSKODER                      
005279     CALL CBLTDLI USING GU WDE4-PCB DLI-IO-E401 SSA1                      
005280     MOVE WDE4-STATUS-CODE       TO STATUS-WS                             
005281     PERFORM IMS-STATUSKONTROLL                                           
005282     .                                                                    
005283     EJECT                                                                
005284 IMS-GNP-KUNDORDERRAD SECTION.                                            
005285     MOVE 'IMS-GNP-KUNDORDERRAD' TO WS-IMS                                
005286                                                                          
005287     MOVE SPACE                  TO ALL-SSA                               
005288     STRING 'WDE411  (IDPURAD  =' W-IDPURAD-X ')'                         
005289             DELIMITED BY SIZE INTO SSA1                                  
005290     MOVE '  '                   TO GODK-STATUSKODER                      
005291     CALL CBLTDLI USING GNP WDE4-PCB DLI-IO-E411 SSA1                     
005292     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
005293     PERFORM IMS-STATUSKONTROLL                                           
005294     .                                                                    
005295     EJECT                                                                
005296 IMS-GU-BEN-SEQ SECTION.                                                  
005297     MOVE 'IMS-GU-BEN-SEQ '      TO WS-IMS                                
005298                                                                          
005299     MOVE SPACE                  TO ALL-SSA                               
005300     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
005301             DELIMITED BY SIZE INTO SSA1                                  
005302     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
005303             DELIMITED BY SIZE INTO SSA2                                  
005304     MOVE '  GE'                 TO GODK-STATUSKODER                      
005305     CALL CBLTDLI USING GU BEN-PCB DLI-IO-AREA SSA1 SSA2                  
005306     MOVE BEN-STATUS-CODE        TO STATUS-WS                             
005307     PERFORM IMS-STATUSKONTROLL                                           
005308     .                                                                    
005309     EJECT                                                                
005310 IMS-GU-SATB11-SATB01 SECTION.                                            
005311     MOVE 'IMS-GU-SATB11-SATB01' TO WS-IMS                                
005312                                                                          
005313     MOVE SPACE                 TO ALL-SSA                                
005314     STRING 'WLSATB11*D(WDJ1CSEQ =' W-WDJ1CSEQ-X ')'                      
005315            DELIMITED BY SIZE INTO SSA1                                   
005316     STRING 'WLSATB01(IDLEVNR  =' W-IDLEVNR-SATS-X                        
005317            '&IDARTNR  <' W-IDARTNR-100000000-X  ')'                      
005318            DELIMITED BY SIZE INTO SSA2                                   
005319     MOVE '  GE'                TO GODK-STATUSKODER                       
005320     CALL CBLTDLI USING GU SATB-PCB DLI-IO-AREA3 SSA1 SSA2                
005321     MOVE SATB-STATUS-CODE      TO STATUS-WS                              
005322     PERFORM IMS-STATUSKONTROLL                                           
005323     .                                                                    
005324     SKIP2                                                                
005325 IMS-GN-SATB11-SATB01 SECTION.                                            
005326     MOVE 'IMS-GN-SATB11-SATB01' TO WS-IMS                                
005327                                                                          
005328     MOVE SPACE                 TO ALL-SSA                                
005329     STRING 'WLSATB11*D(WDJ1CSEQ =' W-WDJ1CSEQ-X ')'                      
005330            DELIMITED BY SIZE INTO SSA1                                   
005331     STRING 'WLSATB01(IDLEVNR  =' W-IDLEVNR-SATS-X                        
005332            '&IDARTNR  <' W-IDARTNR-100000000-X  ')'                      
005333            DELIMITED BY SIZE INTO SSA2                                   
005334     MOVE '  GE'                TO GODK-STATUSKODER                       
005335     CALL CBLTDLI USING GN SATB-PCB DLI-IO-AREA3 SSA1 SSA2                
005336     MOVE SATB-STATUS-CODE      TO STATUS-WS                              
005337     PERFORM IMS-STATUSKONTROLL                                           
005338     .                                                                    
005339     EJECT                                                                
005340 IMS-GU-WDG301 SECTION.                                                   
005341     MOVE 'IMS-GU-WDG301      ' TO WS-IMS                                 
005342                                                                          
005343     MOVE SPACE                 TO ALL-SSA                                
005344     STRING 'WDG301  (WDG3KEY  =' W-WDG3KEY01-X ')'                       
005345            DELIMITED BY SIZE INTO SSA1                                   
005346     MOVE '  '                  TO GODK-STATUSKODER                       
005347     CALL CBLTDLI USING GU WDG3-PCB DLI-IO-AREA-WDGX5102 SSA1             
005348     MOVE WDG3-STATUS-CODE      TO STATUS-WS                              
005349     PERFORM IMS-STATUSKONTROLL                                           
005350     .                                                                    
005351     EJECT                                                                
005352 IMS-GHU-WDGX5102 SECTION.                                                
005353     MOVE 'IMS-GHU-WDGX5102   ' TO WS-IMS                                 
005354                                                                          
005355     MOVE SPACE                 TO ALL-SSA                                
005356     STRING 'WDG301  (WDG3KEY  =' W-WDG3KEY01-X ')'                       
005357            DELIMITED BY SIZE INTO SSA1                                   
005358     STRING 'WDGX5102 '                                                   
005359            DELIMITED BY SIZE INTO SSA2                                   
005360     MOVE '  GE'                TO GODK-STATUSKODER                       
005361     CALL CBLTDLI USING GHU WDG3-PCB DLI-IO-AREA-WDGX5102                 
005362                                               SSA1 SSA2                  
005363     MOVE WDG3-STATUS-CODE TO STATUS-WS                                   
005364     PERFORM IMS-STATUSKONTROLL                                           
005365     .                                                                    
005366     EJECT                                                                
005367 IMS-REPL-WDGX5102 SECTION.                                               
005368     MOVE 'IMS-REPL-WDGX5102  ' TO WS-IMS                                 
005369                                                                          
005370     MOVE SPACE            TO ALL-SSA                                     
005371     MOVE '  '             TO GODK-STATUSKODER                            
005372     CALL CBLTDLI USING REPL WDG3-PCB DLI-IO-AREA-WDGX5102                
005373     MOVE WDG3-STATUS-CODE TO STATUS-WS                                   
005374     PERFORM IMS-STATUSKONTROLL                                           
005375     .                                                                    
005376     EJECT                                                                
005377 IMS-ISRT-WDGX5102 SECTION.                                               
005378     MOVE 'IMS-ISRT-WDGX5102  ' TO WS-IMS                                 
005379                                                                          
005380     MOVE SPACE            TO ALL-SSA                                     
005381     MOVE 'WDGX5102 '      TO SSA1                                        
005382     MOVE '  '             TO GODK-STATUSKODER                            
005383     CALL CBLTDLI USING ISRT WDG3-PCB DLI-IO-AREA-WDGX5102                
005384                                  SSA1                                    
005385     MOVE WDG3-STATUS-CODE TO STATUS-WS                                   
005386     PERFORM IMS-STATUSKONTROLL                                           
005387     .                                                                    
005388     EJECT                                                                
005389 IMS-GU-WDB601-A  SECTION.                                                
005390     MOVE 'IMS-GU-WDB601-A    ' TO WS-IMS                                 
005391                                                                          
005392     MOVE SPACE               TO ALL-SSA                                  
005393     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
005394          DELIMITED BY SIZE INTO SSA1                                     
005395     MOVE '  GE'              TO GODK-STATUSKODER                         
005396     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601-A SSA1               
005397     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
005398     PERFORM IMS-STATUSKONTROLL                                           
005399     .                                                                    
005400     SKIP2                                                                
005401                                                                          
005402 IMS-GU-WDB601-B  SECTION.                                                
005403     MOVE 'IMS-GU-WDB601-B    ' TO WS-IMS                                 
005404                                                                          
005405     MOVE SPACE               TO ALL-SSA                                  
005406     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
005407          DELIMITED BY SIZE INTO SSA1                                     
005408     MOVE '  '                TO GODK-STATUSKODER                         
005409     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601-B SSA1               
005410     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
005411     PERFORM IMS-STATUSKONTROLL                                           
005412     .                                                                    
005413     SKIP2                                                                
005414 IMS-STATUSKONTROLL SECTION.                                              
005415     SET STATUS-IX TO 1                                                   
005416     SEARCH GODK-STATUS                                                   
005417       AT END                                                             
005418         CALL FELLOG                                                      
005419       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
005420         CONTINUE                                                         
005421     END-SEARCH                                                           
005422     .                                                                    
005423*    -COPY WY2000P1                                                       
