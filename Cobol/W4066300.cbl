000001 ID DIVISION.                                                             
000002                                                                          
000003 PROGRAM-ID.     W4066300.                                                
000004 AUTHOR.         LARS THELL.                                              
000005 DATE-WRITTEN.   94/09/05.                                                
000006 DATE-COMPILED.                                                           
000007                                                                          
000008*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!             
000009*     THIS PROGRAM ALSO HAS A WEB-LDC VERSION CALLED WL0186               
000010*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!             
000011*                                                                         
000012*    FUNKTION:                                                            
000013*        DE KOLLIN PÅ EN VISS TRANSPORT SOM ÄR VALBARA FÖR                
000014*        FAKT/LAST VISAS.                                                 
000015*        GENOM ATT VÄLJA KOLLI ELLER REGISTRERA KOLLI LASTNINGS-          
000016*        RELEASAS DE FÖR ATT SEDAN FAKTURARELEASAS PÅ BILD 4664.          
000017*                                                                         
000018*        NY BEHANDLING AV SAMLINGSKOLLI, DEC. 2016.                       
000019*                                                                         
000020*        PROGRAMMET UPPDATERAR WDE6                                       
000021*        PROGRAMMET UPPDATERAR 4495/6/8 (WDR4)                            
000022*                                                                         
000023*                                                                         
000024*    INDATA.                                                              
000025*        TRANSAKTION: W4T663                                              
000026*        MID:         W4I66301                                            
000027*                                                                         
000028*    UTDATA.                                                              
000029*        MOD:         W4O66301                                            
000030                                                                          
000031     SKIP3                                                                
000032 ENVIRONMENT DIVISION.                                                    
000033     EJECT                                                                
000034 DATA DIVISION.                                                           
000035 WORKING-STORAGE SECTION.                                                 
000036                                                                          
000037*    -- CHECKED BY WY2000                                                 
000038 77  IDPGM                       PIC X(08)   VALUE 'W4066300'.            
000039                                                                          
000040*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
000041 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
000042                                                                          
000043 77  JA                          PIC X       VALUE 'J'.                   
000044 77  YES                         PIC X       VALUE 'Y'.                   
000045 77  NEJ                         PIC X       VALUE 'N'.                   
000046 77  MIXED                       PIC X       VALUE 'M'.                   
000047 77  CLOSED                      PIC X       VALUE 'C'.                   
000048 77  W-DATE-AAMM                 PIC 9(4)    VALUE ZERO.                  
000049 77  WS-KDVALISO-HUV             PIC X(3)    VALUE 'SEK'.                 
000050                                                                          
000051*    --- INDEX FÖR BLÄDDRINGSRADER                                        
000052 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
000053 77  MAX-INDX                    PIC S9(4)  VALUE +10   COMP SYNC.        
000054 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
000055*    --- DET RÄTTA VÄRDET PÅ NEDANSTÅENDE FÄLT SÄTTS I A-INIT             
000056                                                                          
000057*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
000058 77  WS-IDTRPTNR                 PIC X(3)    VALUE SPACE.                 
000059 77  WS-IDLBBET                  PIC X(12)   VALUE SPACE.                 
000060 77  WS-FLFARLIG                 PIC X(1)    VALUE SPACE.                 
000061                                                                          
000062 77  W-UPD-RAKNARE           PIC S9(3)   VALUE ZERO.                      
000063 77  W-UPD-MAX               PIC S9(3)   VALUE +100.                      
000064*      --- VALID IDDC CODES                                               
000065*                                                                         
000066*01    -COPY WWDC99                                                       
000067       EJECT                                                              
000068 01  WS-IDTIDZON                 PIC 9(2).                                
000069 01  WS-KDMATT                   PIC X.                                   
000070                                                                          
000071 77  INDATA-SW                   PIC X       VALUE 'J'.                   
000072     88  INDATA-OK                           VALUE 'J'.                   
000073     88  INDATA-FEL                          VALUE 'N'.                   
000074                                                                          
000075 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
000076     88  NYCKLAR-OK                          VALUE 'J'.                   
000077     88  NYCKLAR-FEL                         VALUE 'N'.                   
000078                                                                          
000079 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
000080     88  EGEN-MID                            VALUE '4663'.                
000081     88  GODK-MID                            VALUE '4663' '4664'          
000082                                                   '4665'.                
000083     88  HELP-MID                            VALUE '0551'.                
000084 77  SW-OMSTART                  PIC  X(1)   VALUE 'N'.                   
000085     88 OMSTART                              VALUE 'J'.                   
000086                                                                          
000087 77  SW-TRAEFF                   PIC  X(1)   VALUE 'N'.                   
000088     88 TRAEFF                               VALUE 'J'.                   
000089                                                                          
000090 77  DDGS-REFILL-SW          PIC X           VALUE 'N'.                   
000091     88  DDGS-REFILL                         VALUE 'J'.                   
000092     EJECT                                                                
000093 01  FILLER                  PIC X(16) VALUE 'SWITCHAR'.                  
000094 77  W-FUNKTION              PIC S9(1) COMP-3 VALUE +0.                   
000095     88  LASTA-HELA-SIDAN                     VALUE +1.                   
000096     88  LASTA-VALDA-RADER                    VALUE +2.                   
000097                                                                          
000098 77  SW-TRPT                 PIC  X(1)        VALUE 'N'.                  
000099     88  TRPT-FINNS                           VALUE 'J'.                  
000100                                                                          
000101 77  SW-RAD-INPUT            PIC  X(1)        VALUE 'N'.                  
000102                                                                          
000103 77  WS-SLINGA-KLAR          PIC  X(1)        VALUE 'N'.                  
000104     88  SLINGA-KLAR                          VALUE 'J'.                  
000105                                                                          
000106 77  TRAFF-SW                PIC  X(1)        VALUE 'N'.                  
000107     88  TRAFF                                VALUE 'J'.                  
000108                                                                          
000109 77  SW-SAMKOLLI                 PIC  X(1)   VALUE 'N'.                   
000110     88 SAMKOLLI-FINNS                       VALUE 'J'.                   
000111                                                                          
000112 EJECT                                                                    
000113                                                                          
000114 01  SPAR-AREA.                                                           
000115   03    SPAR-IDTRANS             PIC X(4).                               
000116   03    SPAR-IDSEGM-ENTER        PIC X(4).                               
000117   03    SPAR-IDSEGM-NEXT         PIC X(4).                               
000118   03    SPAR-WDE6C1KY-ENTER      PIC X(42).                              
000119   03    SPAR-WDE6C1KY-NEXT       PIC X(42).                              
000120   03    SPAR-WDE6E1KY-ENTER      PIC X(12).                              
000121   03    SPAR-WDE6E1KY-NEXT       PIC X(12).                              
000122                                                                          
000123 01  TEST-IDDISTR            PIC  9(5) COMP-3 VALUE ZERO.                 
000124*01  FILLER  -COPY WWDIST79 -RED TEST-IDDISTR.                            
000125     EJECT                                                                
000126 01  FILLER                  PIC X(16) VALUE 'KONSTANTER'.                
000127 01  KONSTANTER.                                                          
000128     03  WC-IDDC-SE          PIC  X(2)        VALUE 'SE'.                 
000129     03  KLI-PACK            PIC S9(1) COMP-3 VALUE +1.                   
000130     03  KLI-PACK-FAKT       PIC S9(1) COMP-3 VALUE +6.                   
000131     03  SK-SHIPPED          PIC  X(1)        VALUE 'S'.                  
000132     03  W-LASTA-HELA-SIDAN  PIC S9(1) COMP-3 VALUE +1.                   
000133     03  W-LASTA-VALDA-RADER PIC S9(1) COMP-3 VALUE +2.                   
000134     03  W-VALD              PIC  X(1)        VALUE 'X'.                  
000135     03  W-FEL-721           PIC  X(3)        VALUE '721'.                
000136     03  W-FEL-758           PIC  X(3)        VALUE '758'.                
000137     03  W-FEL-795           PIC  X(3)        VALUE '795'.                
000138                                                                          
000139 01      TIDSUPPGIFTER.                                                   
000140   03    DAGENS-DATUM        PIC 9(6).                                    
000141   03    TIAAMMDD            PIC 9(6).                                    
000142   03    TIKLOCK             PIC 9(8).                                    
000143   03    FILLER              REDEFINES TIKLOCK.                           
000144     05  TIKLOCK-MIN         PIC 9(4).                                    
000145     05  FILLER              PIC X(4).                                    
000146 77  LNG-P-TO-P-PREFIX           PIC S9(4)  VALUE +17   COMP SYNC.        
000147 77  W-KVKOLLI-SIDA              PIC S9(3)   VALUE ZERO COMP-3.           
000148 77  W-VLORDBTO-SIDA       PIC S9(4)V9(3)    VALUE ZERO COMP-3.           
000149 77  W-VKORDBTO-SIDA       PIC S9(6)V9(1)    VALUE ZERO COMP-3.           
000150 77  W-KVKOLLI-TOT               PIC S9(3)   VALUE ZERO COMP-3.           
000151 77  W-VLORDBTO-TOT        PIC S9(5)V9(3)    VALUE ZERO COMP-3.           
000152 77  W-VKORDBTO-TOT        PIC S9(7)V9(1)    VALUE ZERO COMP-3.           
000153 77  W-KVKOLLI                   PIC S9(3)   VALUE ZERO COMP-3.           
000154 77  W-KVKOLLI-SAMP              PIC S9(3)   VALUE ZERO COMP-3.           
000155 77  W-VLORDBTO            PIC S9(4)V9(3)    VALUE ZERO COMP-3.           
000156 77  W-VKORDBTO            PIC S9(6)V9(1)    VALUE ZERO COMP-3.           
000157 77  W-SUORDV              PIC S9(9)V9(2)    VALUE ZERO COMP-3.           
000158 77  W-SUORDV-TOT          PIC S9(9)V9(2)    VALUE ZERO COMP-3.           
000159 77  W-SUORDV-EXP          PIC S9(9)V9(2)    VALUE ZERO COMP-3.           
000160 77  W-SUORDV-TOT-EXP      PIC S9(9)V9(2)    VALUE ZERO COMP-3.           
000161 77  W-REG-IDPRODNR              PIC S9(7)   VALUE ZERO COMP-3.           
000162 77  W-SPAR-IDDISTR              PIC S9(5)   VALUE ZERO COMP-3.           
000163 77  W-IDDISTR-NUM               PIC S9(4)   VALUE ZERO COMP-3.           
000164 77  W-IDKUNDNR-NUM              PIC S9(6)   VALUE ZERO COMP-3.           
000165 77  W-IDPSN-NUM                 PIC  9(3)   VALUE ZERO.                  
000166 77  W-MED-IDMFSFEL              PIC  X(3)   VALUE SPACE.                 
000167 77  SPAR-KDFAKTYP               PIC  X(1)   VALUE SPACE.                 
000168 77  JFR-IDKOLLI-SAMP            PIC S9(5)   VALUE ZERO COMP-3.           
000169                                                                          
000170 77  WS-VLORDBTO           PIC S9(4)V9(3)    VALUE ZERO COMP-3.           
000171 77  WS-VKORDBTO           PIC S9(6)V9(1)    VALUE ZERO COMP-3.           
000172 77  WS-VLORDBTO-TOT       PIC S9(5)V9(3)    VALUE ZERO COMP-3.           
000173 77  WS-VKORDBTO-TOT       PIC S9(7)V9(1)    VALUE ZERO COMP-3.           
000174                                                                          
000175 77  WS-IDDC-IN                  PIC  X(2)   VALUE SPACE.                 
000176                                                                          
000177                                                                          
000178 01  WS-TILASTID                 PIC 9(6)    VALUE ZERO.                  
000179 01  WS-TILASTID-GRP             REDEFINES WS-TILASTID.                   
000180     03 WS-TILASTID-HHMM         PIC 9(4).                                
000181     03 WS-TILASTID-SS           PIC 9(2).                                
000182                                                                          
000183*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
000184 01  GENERELLA-SUBPROGRAM.                                                
000185     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
000186     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
000187     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
000188     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
000189     03  W411EXCH                PIC X(8)    VALUE 'W411EXCH'.            
000190     03  W510CURR                PIC X(8)    VALUE 'W510CURR'.            
000191     EJECT                                                                
000192*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
000193*01 -COPY WMSGINIT                                                        
000194     EJECT                                                                
000195*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
000196*01 -COPY WMEDAREA                                                        
000197     EJECT                                                                
000198*    --- PARAMETRAR TILL COPYTEXT   WWOMVAND                              
000199*01 -COPY WWOMVAND                                                        
000200     SKIP3                                                                
000201*    --- PARAMETRAR TILL COPYTEXT   W411EXCH                              
000202 01  FILLER         PIC X(16)   VALUE 'OMRÄKNA VALUTA'.                   
000203*    -COPY W411EXCH                                                       
000204     SKIP3                                                                
000205*    --- PARAMETRAR TILL COPYTEXT   W510CURR                              
000206*01 -COPY W510CURR                                                        
000207     SKIP3                                                                
000208 01  MESSAGE-CODES.                                                       
000209     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
000210     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
000211     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
000212     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
000213     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
000214     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
000215     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
000216     EJECT                                                                
000217 01      MEDDELANDE1.                                                     
000218    04   FILLER.                                                          
000219     05  FILLER              PIC X(15)      VALUE                         
000220         'DISTR LÅST AV '.                                                
000221     05  FILLER              PIC X(8).                                    
000222     05  FILLER              PIC X(8)       VALUE                         
000223         ' DATUM: '.                                                      
000224     05  FILLER              PIC X(6).                                    
000225     05  FILLER              PIC X(6)       VALUE                         
000226         ' TID: '.                                                        
000227     05  FILLER              PIC X(5).                                    
000228     05  FILLER              PIC X(1).                                    
000229     05  FILLER              PIC X(4).                                    
000230    04   FILLER.                                                          
000231     05  FILLER              PIC X(15)      VALUE                         
000232         'DISTR LÅST AV '.                                                
000233     05  FILLER              PIC X(8).                                    
000234     05  FILLER              PIC X(8)       VALUE                         
000235         ' DATUM: '.                                                      
000236     05  FILLER              PIC X(6).                                    
000237     05  FILLER              PIC X(6)       VALUE                         
000238         ' TYD: '.                                                        
000239     05  FILLER              PIC X(5).                                    
000240     05  FILLER              PIC X(1).                                    
000241     05  FILLER              PIC X(4).                                    
000242     EJECT                                                                
000243 01  MEDDELANDE2.                                                         
000244     03  FILLER              PIC X(40)                                    
000245         VALUE 'DC-SE GODS FINNS TILL DENNA TRANSPORT   '.                
000246     03  FILLER              PIC X(40)                                    
000247         VALUE 'DC-SE GOODS FOR THIS TRANSPORT EXISTS   '.                
000248 01  FILLER REDEFINES MEDDELANDE2.                                        
000249     03  DDC-SE-MED  OCCURS 2  PIC X(40).                                 
000250     EJECT                                                                
000251 01  MEDDELANDE3.                                                         
000252     03  FILLER              PIC X(40)                                    
000253         VALUE 'MER INFO TRYCK PF8.   DC-SE GODS FINNS  '.                
000254     03  FILLER              PIC X(40)                                    
000255         VALUE 'MORE INFO PRESS PF8.  DC-SE GOODS EXISTS'.                
000256 01  FILLER REDEFINES MEDDELANDE3.                                        
000257     03  MORE-INFO-DDC-SE-MED OCCURS 2  PIC X(40).                        
000258     EJECT                                                                
000259                                                                          
000260 01  FILLER                      PIC X(16)   VALUE 'P-TO-P-AREA'.         
000261 01      P-TO-P-SW.                                                       
000262                                                                          
000263  02     P-TO-P-KVLL             PIC S9(4)           COMP SYNC.           
000264  02     P-TO-P-KDZ1             PIC X(1)  VALUE LOW-VALUE.               
000265  02     P-TO-P-KDZ2             PIC X(1)  VALUE LOW-VALUE.               
000266  02     P-TO-P-KDTRANS          PIC X(8).                                
000267  02     P-TO-P-IDTRANS          PIC X(4).                                
000268  02     P-TO-P-KDMFSFOR         PIC X(1).                                
000269  02     P-TO-P-DATA             PIC X(1000).                             
000270     EJECT                                                                
000271*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
000272*                                                                         
000273 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
000274     SKIP3                                                                
000275*01  MID -COPY W4I66301                                                   
000276     EJECT                                                                
000277 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
000278     SKIP3                                                                
000279*01  -COPY WMSGAREA                                                       
000280     EJECT                                                                
000281     03  MOD REDEFINES MSG-AREA.                                          
000282*      05  -COPY W4O66301     -PRE MOD-                                   
000283     EJECT                                                                
000284 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
000285     SKIP3                                                                
000286*01  -COPY WMFSAREA                                                       
000287     EJECT                                                                
000288*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
000289*                                                                         
000290     EJECT                                                                
000291 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
000292     SKIP3                                                                
000293 01  NYCKLAR-TILL-DLI.                                                    
000294   03  W-IDDC-X.                                                          
000295       05  W-IDDC              PIC  X(2)   VALUE SPACE.                   
000296   03  W-IDPRODNR-X.                                                      
000297       05  W-IDPRODNR          PIC S9(7)   VALUE ZERO  COMP-3.            
000298   03  W-IDKOLLI-X.                                                       
000299       05  W-IDKOLLI           PIC S9(5)   VALUE ZERO  COMP-3.            
000300   03  W-WDGXKEY-X.                                                       
000301     05  W-IDHTYP                PIC  X(4)   VALUE '4495'.                
000302     05  W-IDDC-4495             PIC  X(2)   VALUE SPACE.                 
000303     05  W-IDTRPTNR              PIC S9(3)   VALUE ZERO  COMP-3.          
000304     05  W-IDLBBET               PIC X(12)   VALUE SPACE.                 
000305     05  FILLER                  PIC X(10)   VALUE LOW-VALUE.             
000306   03  W-WDE4ASEQ-X.                                                      
000307     05  W-4A1-IDDISTR           PIC S9(5)   VALUE ZERO  COMP-3.          
000308     05  W-4A1-IDKUNDNR          PIC S9(7)   VALUE ZERO  COMP-3.          
000309     05  W-4A1-IDKUNDRF.                                                  
000310       07  W-4A1-IDORDNR         PIC X(5).                                
000311       07  FILLER                PIC X(5).                                
000312     SKIP2                                                                
000313   03    W-WDE6C1KY-STA-X.                                                
000314     05  W-IDTRPTNR-STA        PIC S9(3)   VALUE ZERO  COMP-3.            
000315     05  FILLER                PIC X(40)   VALUE LOW-VALUE.               
000316                                                                          
000317   03    W-WDE6C1KY-MIN-X.                                                
000318     05  W-IDTRPTNR-MIN        PIC S9(3)   VALUE ZERO  COMP-3.            
000319     05  W-DARFS-MIN           PIC 9(12)   VALUE ZERO.                    
000320     05  W-ADCLGEO-MIN.                                                   
000321       07 W-IDDC-MIN           PIC X(2)    VALUE SPACE.                   
000322       07 W-ADFLGEO-MIN        PIC X(3)    VALUE SPACE.                   
000323     05  W-ADFLOMR-MIN         PIC S9(3)   VALUE ZERO  COMP-3.            
000324     05  W-ADRUTNIV-MIN        PIC S9(3)   VALUE ZERO  COMP-3.            
000325     05  W-ADVMODUL-MIN        PIC S9(3)   VALUE ZERO  COMP-3.            
000326     05  W-IDDISTR-MIN         PIC S9(5)   VALUE ZERO  COMP-3.            
000327     05  W-IDKUNDNR-MIN        PIC S9(7)   VALUE ZERO  COMP-3.            
000328     05  W-IDPRODNR-KOLLI-MIN  PIC S9(7)   VALUE ZERO  COMP-3.            
000329     05  W-IDKOLLI-FLER-MIN    PIC S9(5)   VALUE ZERO  COMP-3.            
000330     05  W-IDKOLLI-MIN         PIC S9(5)   VALUE ZERO  COMP-3.            
000331                                                                          
000332   03    W-WDE6C1KY-MAX-X.                                                
000333     05  W-IDTRPTNR-MAX        PIC S9(3)   VALUE ZERO  COMP-3.            
000334     05  W-DARFS-MAX           PIC 9(12)   VALUE ZERO.                    
000335     05  W-ADCLGEO-MAX.                                                   
000336       07 W-IDDC-MAX           PIC X(2)    VALUE SPACE.                   
000337       07 W-ADFLGEO-MAX        PIC X(3)    VALUE SPACE.                   
000338     05  W-ADFLOMR-MAX         PIC S9(3)   VALUE ZERO  COMP-3.            
000339     05  W-ADRUTNIV-MAX        PIC S9(3)   VALUE ZERO  COMP-3.            
000340     05  W-ADVMODUL-MAX        PIC S9(3)   VALUE ZERO  COMP-3.            
000341     05  W-IDDISTR-MAX         PIC S9(5)   VALUE ZERO  COMP-3.            
000342     05  W-IDKUNDNR-MAX        PIC S9(7)   VALUE ZERO  COMP-3.            
000343     05  W-IDPRODNR-KOLLI-MAX  PIC S9(7)   VALUE ZERO  COMP-3.            
000344     05  W-IDKOLLI-FLER-MAX    PIC S9(5)   VALUE ZERO  COMP-3.            
000345     05  W-IDKOLLI-MAX         PIC S9(5)   VALUE ZERO  COMP-3.            
000346     SKIP2                                                                
000347   03    W-WDE6E1KY-STA-X.                                                
000348     05  W-E6E-IDDC-STA        PIC  X(2)   VALUE SPACE.                   
000349     05  W-E6E-IDKOLLIS-STA    PIC S9(5)   VALUE ZERO  COMP-3.            
000350     05  FILLER                PIC  X(7)   VALUE LOW-VALUE.               
000351     SKIP2                                                                
000352   03    W-WDE6E1KY-MIN-X.                                                
000353     05  W-E6E-IDDC-MIN        PIC  X(2)   VALUE SPACE.                   
000354     05  W-E6E-IDKOLLIS-MIN    PIC S9(5)   VALUE ZERO  COMP-3.            
000355     05  FILLER                PIC  X(7)   VALUE LOW-VALUE.               
000356   03    W-WDE6E1KY-MAX-X.                                                
000357     05  W-E6E-IDDC-MAX        PIC  X(2)   VALUE 'ZZ'.                    
000358     05  W-E6E-IDKOLLIS-MAX    PIC S9(5)   VALUE 99999 COMP-3.            
000359     05  FILLER                PIC  X(7)   VALUE HIGH-VALUE.              
000360     SKIP2                                                                
000361   03  W-E6E-IDTRPTNR-X.                                                  
000362     05  W-E6E-IDTRPTNR        PIC S9(3)   VALUE ZERO  COMP-3.            
000363*CO                                                                       
000364   03  W-KDKOLSTX-X.                                                      
000365     05  W-KDKOLSTA-CROSS      PIC S9(1)   VALUE +0    COMP-3.            
000366     SKIP2                                                                
000367   03    W-WDE7ASEQ-X.                                                    
000368     05  W-E7A-IDDC            PIC  X(2)   VALUE SPACE.                   
000369     05  W-E7A-IDKOLLIS        PIC S9(5)   VALUE ZERO  COMP-3.            
000370     SKIP2                                                                
000371   03    W-WDE4F1KY-MIN-X.                                                
000372     05  W-E4F-IDPRODNR-MIN    PIC S9(7)   VALUE ZERO  COMP-3.            
000373     05  W-E4F-IDKOLLI-MIN     PIC S9(5)   VALUE ZERO  COMP-3.            
000374     05  FILLER                PIC  X(22)  VALUE LOW-VALUE.               
000375   03    W-WDE4F1KY-MAX-X.                                                
000376     05  W-E4F-IDPRODNR-MAX    PIC S9(7)   VALUE ZERO  COMP-3.            
000377     05  W-E4F-IDKOLLI-MAX     PIC S9(5)   VALUE ZERO  COMP-3.            
000378     05  FILLER                PIC  X(22)  VALUE HIGH-VALUE.              
000379     SKIP2                                                                
000380   03    W-IDGMT-X.                                                       
000381     05  W-IDDISTR-WDB2        PIC S9(5)   VALUE ZERO  COMP-3.            
000382     05  W-IDKUNDNR-WDB2       PIC S9(7)   VALUE ZERO  COMP-3.            
000383                                                                          
000384   03  W-WDB101KY-X.                                                      
000385     05  W-WDB1-IDPARTNR       PIC X(9)     VALUE SPACE.                  
000386     05  W-WDB1-IDFTG          PIC 9(2)     VALUE ZERO.                   
000387                                                                          
000388     SKIP2                                                                
000389*    --- STATUS-KOD FRÅN IMS                                              
000390 01  STATUS-WS                   PIC XX.                                  
000391     88  SEGMENT-FINNS                       VALUE '  '.                  
000392     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
000393     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
000394     88  BASEN-SLUT                          VALUE 'GB'.                  
000395     SKIP2                                                                
000396 01  GODK-STATUSKODER.                                                    
000397     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
000398     SKIP3                                                                
000399 01  SSA1                        PIC X(144).                              
000400 01  SSA2                        PIC X(64).                               
000401 01  SSA3                        PIC X(64).                               
000402     EJECT                                                                
000403*    --- IMS FUNKTIONSKODER                                               
000404*01  -COPY W0003                                                          
000405     EJECT                                                                
000406*    ---  DLI INPUT-OUTPUT AREA                                           
000407 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
000408     SKIP3                                                                
000409 01  FILLER         PIC X(16) VALUE 'DLI-IO-E401'.                        
000410 01  DLI-IO-E401.                                                         
000411*    03  -COPY WDE401                                                     
000412     EJECT                                                                
000413 01  FILLER         PIC X(16) VALUE 'DLI-IO-E4F1'.                        
000414 01  DLI-IO-E4F1.                                                         
000415*    03  -COPY WDE4F1                                                     
000416     EJECT                                                                
000417 01  FILLER         PIC X(16) VALUE 'DLI-IO-E601'.                        
000418 01  DLI-IO-E601.                                                         
000419*    03  -COPY WDE601                                                     
000420     EJECT                                                                
000421 01  FILLER         PIC X(16) VALUE 'DLI-IO-E611'.                        
000422 01  DLI-IO-E611.                                                         
000423*    03  -COPY WDE611                                                     
000424     EJECT                                                                
000425 01  FILLER         PIC X(16) VALUE 'DLI-IO-E621'.                        
000426 01  DLI-IO-E621.                                                         
000427*    03  -COPY WDE621                                                     
000428     EJECT                                                                
000429 01  FILLER         PIC X(16) VALUE 'DLI-IO-E6C1'.                        
000430 01  DLI-IO-E6C1.                                                         
000431*    03  -COPY WDE6C1                                                     
000432     EJECT                                                                
000433 01  FILLER         PIC X(16) VALUE 'DLI-IO-E6E1'.                        
000434 01  DLI-IO-E6E1.                                                         
000435*    03  -COPY WDE6E1                                                     
000436     EJECT                                                                
000437 01  FILLER         PIC X(16) VALUE 'DLI-IO-E711'.                        
000438 01  DLI-IO-E711.                                                         
000439*    03  -COPY WDE711                                                     
000440     EJECT                                                                
000441 01  FILLER         PIC X(16) VALUE 'DLI-IO-4495'.                        
000442 01  DLI-IO-4495.                                                         
000443*    03  -COPY WDGX4495                                                   
000444     EJECT                                                                
000445 01  FILLER         PIC X(16) VALUE 'DLI-IO-4496'.                        
000446 01  DLI-IO-4496.                                                         
000447*    03  -COPY WDGX4496                                                   
000448     EJECT                                                                
000449 01  FILLER         PIC X(16) VALUE 'DLI-IO-4497'.                        
000450 01  DLI-IO-4497.                                                         
000451*    03  -COPY WDGX4497                                                   
000452     EJECT                                                                
000453 01  FILLER         PIC X(16) VALUE 'DLI-IO-4498'.                        
000454 01  DLI-IO-4498.                                                         
000455*    03  -COPY WDGX4498                                                   
000456     EJECT                                                                
000457 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB101'.                      
000458 01  DLI-IO-WDB101.                                                       
000459*    03  -COPY WDB101                                                     
000460     EJECT                                                                
000461 01  FILLER         PIC X(16) VALUE 'DLI-IO-B201'.                        
000462 01  DLI-IO-B201.                                                         
000463*    03  -COPY WDB201                                                     
000464                                                                          
000465     EJECT                                                                
000466 LINKAGE SECTION.                                                         
000467                                                                          
000468*01  -COPY W0009   -PRE MSG-                                              
000469     EJECT                                                                
000470*01  -COPY W0009  -PRE ALT-                                               
000471     EJECT                                                                
000472*01  -COPY W0008  -PRE WDP7-                                              
000473     05  FILLER                  PIC X.                                   
000474     EJECT                                                                
000475*01  -COPY W0008  -PRE WDE6C-                                             
000476     05  FILLER                  PIC X.                                   
000477     EJECT                                                                
000478*01  -COPY W0008  -PRE WDE6-                                              
000479     05  FILLER                  PIC X.                                   
000480     EJECT                                                                
000481*01  -COPY W0008  -PRE WDE4-                                              
000482     05  FILLER                  PIC X.                                   
000483     EJECT                                                                
000484*01  -COPY W0008  -PRE WDE4F-                                             
000485     05  FILLER                  PIC X.                                   
000486     EJECT                                                                
000487*01  -COPY W0008  -PRE 4495-                                              
000488     05  FILLER                  PIC X.                                   
000489     EJECT                                                                
000490*01  -COPY W0008  -PRE WDE6E-                                             
000491     05  FILLER                  PIC X.                                   
000492     EJECT                                                                
000493*01  -COPY W0008  -PRE WDB2-                                              
000494     05  FILLER                  PIC X.                                   
000495     EJECT                                                                
000496*01  -COPY W0008  -PRE WDB1-                                              
000497     05  FILLER                  PIC X.                                   
000498     EJECT                                                                
000499*01  -COPY W0008  -PRE WDG2-                                              
000500     05  FILLER                  PIC X.                                   
000501     EJECT                                                                
000502*01  -COPY W0008  -PRE WDE7-                                              
000503     05  FILLER                  PIC X.                                   
000504     EJECT                                                                
000505 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB WDP7-PCB                       
000506                           WDE6C-PCB WDE6-PCB WDE4-PCB WDE4F-PCB          
000507                           4495-PCB WDE6E-PCB WDB2-PCB                    
000508                           WDB1-PCB WDG2-PCB WDE7-PCB.                    
000509     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB WDP7-PCB                       
000510                           WDE6C-PCB WDE6-PCB WDE4-PCB WDE4F-PCB          
000511                           4495-PCB WDE6E-PCB WDB2-PCB                    
000512                           WDB1-PCB WDG2-PCB WDE7-PCB.                    
000513                                                                          
000514     PERFORM IMS-GET-MSG                                                  
000515     IF SEGMENT-FINNS                                                     
000516       PERFORM A-INIT                                                     
000517       PERFORM B-KOLLA-NYCKLAR                                            
000518       IF NYCKLAR-OK                                                      
000519         IF MFS-UPDATE                                                    
000520           IF NOT OMSTART                                                 
000521              PERFORM G-KOLLA-INPUT                                       
000522           END-IF                                                         
000523           IF INDATA-OK                                                   
000524             PERFORM H-UPPDATERA                                          
000525           END-IF                                                         
000526         ELSE                                                             
000527           IF MFS-FIRST                                                   
000528             PERFORM C-FOERSTA-SIDA                                       
000529           ELSE                                                           
000530             IF MFS-NEXT                                                  
000531               PERFORM D-NAESTA-SIDA                                      
000532             ELSE                                                         
000533               PERFORM E-SAMMA-SIDA                                       
000534             END-IF                                                       
000535           END-IF                                                         
000536         END-IF                                                           
000537         IF INDATA-OK AND NOT OMSTART                                     
000538           PERFORM F-LAES-VISA-INFO                                       
000539         END-IF                                                           
000540       END-IF                                                             
000541       IF OMSTART                                                         
000542          PERFORM I-STARTA-W40663                                         
000543       ELSE                                                               
000544          COMPUTE MSG-KVLL         = LENGTH OF MOD-W4O66301 + 4           
000545          PERFORM IMS-INSERT-MSG                                          
000546       END-IF                                                             
000547     END-IF                                                               
000548                                                                          
000549     MOVE ZERO TO RETURN-CODE                                             
000550     GOBACK                                                               
000551     .                                                                    
000552     EJECT                                                                
000553 A-INIT SECTION.                                                          
000554                                                                          
000555     IF MSG-DUBBLA-TRANSKODER                                             
000556       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I66301                 
000557       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
000558       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
000559     ELSE                                                                 
000560       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I66301                  
000561       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
000562       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
000563     END-IF                                                               
000564                                                                          
000565     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
000566     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
000567     MOVE MFS-IDTRANS TO W-IDTRANS                                        
000568                                                                          
000569     MOVE LOW-VALUE TO MSG-AREA                                           
000570     MOVE 'W4O663N1' TO MFS-IDMOD                                         
000571     MOVE '4663' TO MOD-IDTRANS                                           
000572     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
000573                                                                          
000574                                                                          
000575     IF EGEN-MID OR HELP-MID                                              
000576       CONTINUE                                                           
000577     ELSE                                                                 
000578       MOVE SPACE TO MFS-KDTRTYP                                          
000579       MOVE '7' TO MFS-IDPFK                                              
000580     END-IF                                                               
000581                                                                          
000582     MOVE LOW-VALUE            TO W-WDE6C1KY-MIN-X                        
000583     MOVE HIGH-VALUE           TO W-WDE6C1KY-MAX-X                        
000584                                                                          
000585     MOVE FUNCTION CURRENT-DATE(3:2) TO W-DATE-AAMM(1:2)                  
000586     MOVE FUNCTION CURRENT-DATE(5:2) TO W-DATE-AAMM(3:2)                  
000587     .                                                                    
000588     EJECT                                                                
000589 B-KOLLA-NYCKLAR SECTION.                                                 
000590                                                                          
000591     MOVE ALL '+'           TO MSGI-WMSGINIT                              
000592     MOVE '001'             TO MSGI-KDCALL                                
000593     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
000594     MOVE '4663'            TO MSGI-IDTRANS                               
000595     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
000596     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
000597     MOVE MSGI-SPAR-AREA    TO SPAR-AREA                                  
000598                                                                          
000599     IF MSGI-IDLAND-SPR = 'GB'                                            
000600       MOVE +2 TO SPRAK-IX                                                
000601     ELSE                                                                 
000602       MOVE +1 TO SPRAK-IX                                                
000603     END-IF                                                               
000604                                                                          
000605     MOVE MSGI-IDLAND-SPR   TO MED-IDSKYLT                                
000606     MOVE MSGI-IDTIDZON     TO WS-IDTIDZON                                
000607     MOVE MSGI-KDMATT       TO WS-KDMATT                                  
000608                                                                          
000609     MOVE JA TO NYCKLAR-SW                                                
000610                                                                          
000611*    -- KONTROLL AV IDTRPTNR                                              
000612     MOVE MFS-RENSA-FAELT TO MOD-IDTRPTNR-IN                              
000613                             MOD-IDLBBET-IN                               
000614                             MOD-FLFARLIG-IN                              
000615                             MOD-IDDC-IN                                  
000616                                                                          
000617     IF MID-IDTRPTNR-IN     =  ALL '+'                                    
000618       MOVE MID-IDTRPTNR-UT TO WS-IDTRPTNR                                
000619       INSPECT WS-IDTRPTNR REPLACING LEADING SPACE BY ZERO                
000620     ELSE                                                                 
000621       MOVE MID-IDTRPTNR-IN TO WS-IDTRPTNR                                
000622       MOVE '7'             TO MFS-IDPFK                                  
000623       MOVE SPACE           TO MFS-KDTRTYP                                
000624     END-IF                                                               
000625                                                                          
000626     IF WS-IDTRPTNR NUMERIC AND WS-IDTRPTNR > ZERO                        
000627       MOVE WS-IDTRPTNR     TO W-IDTRPTNR-MIN                             
000628                               W-IDTRPTNR-MAX                             
000629                               W-IDTRPTNR-STA                             
000630                               W-E6E-IDTRPTNR                             
000631                               W-IDTRPTNR                                 
000632     ELSE                                                                 
000633       MOVE NEJ             TO NYCKLAR-SW                                 
000634     END-IF                                                               
000635                                                                          
000636     IF MID-IDLBBET-IN       = ALL '+'                                    
000637       MOVE MID-IDLBBET-UT   TO WS-IDLBBET                                
000638     ELSE                                                                 
000639       MOVE MID-IDLBBET-IN   TO WS-IDLBBET                                
000640       MOVE '7'              TO MFS-IDPFK                                 
000641       MOVE SPACE            TO MFS-KDTRTYP                               
000642     END-IF                                                               
000643     IF WS-IDLBBET           NOT = SPACE                                  
000644       MOVE WS-IDLBBET       TO W-IDLBBET                                 
000645     ELSE                                                                 
000646       MOVE NEJ              TO NYCKLAR-SW                                
000647     END-IF                                                               
000648                                                                          
000649     MOVE MSGI-IDDC           TO WS-IDDC                                  
000650                                                                          
000651     IF CDC-SE                                                            
000652        IF MID-IDDC-IN NOT = ALL '+'                                      
000653          MOVE MID-IDDC-IN      TO WS-IDDC                                
000654          IF CDC-SE OR DDC-SE                                             
000655             MOVE MID-IDDC-IN   TO WS-IDDC-IN                             
000656             MOVE '7'           TO MFS-IDPFK                              
000657             MOVE SPACE         TO MFS-KDTRTYP                            
000658          ELSE                                                            
000659             MOVE MSGI-IDDC     TO WS-IDDC-IN                             
000660          END-IF                                                          
000661        ELSE                                                              
000662          MOVE MID-IDDC-UT      TO WS-IDDC                                
000663          IF DDC-SE                                                       
000664             MOVE MID-IDDC-UT   TO WS-IDDC-IN                             
000665          ELSE                                                            
000666             MOVE MSGI-IDDC     TO WS-IDDC-IN                             
000667          END-IF                                                          
000668        END-IF                                                            
000669     ELSE                                                                 
000670        MOVE MSGI-IDDC          TO WS-IDDC-IN                             
000671     END-IF                                                               
000672                                                                          
000673     MOVE WS-IDDC-IN            TO W-IDDC                                 
000674                                   W-IDDC-4495                            
000675                                   W-E6E-IDDC-STA                         
000676                                   W-E6E-IDDC-MIN                         
000677                                   W-E6E-IDDC-MAX                         
000678                                   W-E7A-IDDC                             
000679                                   MOD-IDDC-UT                            
000680                                                                          
000681                                                                          
000682     IF MID-FLFARLIG-IN      = ALL '+'                                    
000683       IF NDC-JP OR NDC-AU                                                
000684         MOVE MIXED          TO WS-FLFARLIG                               
000685       ELSE                                                               
000686         MOVE MID-FLFARLIG-UT TO WS-FLFARLIG                              
000687       END-IF                                                             
000688     ELSE                                                                 
000689       MOVE MID-FLFARLIG-IN  TO WS-FLFARLIG                               
000690       MOVE '7'              TO MFS-IDPFK                                 
000691       MOVE SPACE            TO MFS-KDTRTYP                               
000692     END-IF                                                               
000693                                                                          
000694     IF WS-FLFARLIG          = JA OR YES OR NEJ OR MIXED                  
000695       IF WS-FLFARLIG = YES                                               
000696         MOVE JA TO WS-FLFARLIG                                           
000697       END-IF                                                             
000698     ELSE                                                                 
000699       MOVE NEJ              TO NYCKLAR-SW                                
000700     END-IF                                                               
000701                                                                          
000702     IF GODK-MID OR NYCKLAR-OK                                            
000703       MOVE WS-IDTRPTNR      TO MOD-IDTRPTNR-UT                           
000704       INSPECT MOD-IDTRPTNR-UT REPLACING LEADING ZERO BY SPACE            
000705       MOVE WS-IDLBBET       TO MOD-IDLBBET-UT                            
000706       MOVE WS-FLFARLIG      TO MOD-FLFARLIG-UT                           
000707       INSPECT MOD-IDDC-UT   REPLACING LEADING ZERO BY SPACE              
000708     ELSE                                                                 
000709       MOVE MFS-RENSA-FAELT  TO MOD-IDTRPTNR-UT                           
000710                                MOD-IDLBBET-UT                            
000711                                MOD-FLFARLIG-UT                           
000712     END-IF                                                               
000713                                                                          
000714     IF NYCKLAR-FEL                                                       
000715       MOVE ERR-WRONG-KEY    TO MED-IDMFSFEL                              
000716       CALL WMEDKONV USING MED-WMEDAREA                                   
000717       MOVE MED-MFSFEL       TO MOD-TEMFSFEL                              
000718       PERFORM MFS-RENSA-FAELT-IN                                         
000719       PERFORM MFS-RENSA-FAELT-UT                                         
000720     END-IF                                                               
000721     .                                                                    
000722     EJECT                                                                
000723 C-FOERSTA-SIDA SECTION.                                                  
000724                                                                          
000725     MOVE INF-FIRST-PAGE     TO MED-IDMFSINF                              
000726     CALL WMEDKONV USING MED-WMEDAREA                                     
000727     MOVE MED-MFSINF         TO MOD-TEMFSFEL                              
000728                                                                          
000729     PERFORM MFS-RENSA-FAELT-IN                                           
000730     .                                                                    
000731     EJECT                                                                
000732 D-NAESTA-SIDA SECTION.                                                   
000733                                                                          
000734     IF SPAR-IDTRANS = '4663'                                             
000735        IF SPAR-IDSEGM-NEXT = 'E6C1'                                      
000736          MOVE SPAR-WDE6C1KY-NEXT TO W-WDE6C1KY-MIN-X                     
000737          MOVE WS-IDTRPTNR        TO W-IDTRPTNR-MIN                       
000738        END-IF                                                            
000739        IF SPAR-IDSEGM-NEXT = 'E6E1'                                      
000740          MOVE SPAR-WDE6E1KY-NEXT TO W-WDE6E1KY-MIN-X                     
000741        END-IF                                                            
000742     END-IF                                                               
000743                                                                          
000744     PERFORM MFS-RENSA-FAELT-IN                                           
000745     .                                                                    
000746     EJECT                                                                
000747 E-SAMMA-SIDA SECTION.                                                    
000748                                                                          
000749     IF SPAR-IDTRANS = '4663'                                             
000750        IF SPAR-IDSEGM-ENTER = 'E6C1'                                     
000751          MOVE SPAR-WDE6C1KY-ENTER TO W-WDE6C1KY-MIN-X                    
000752          MOVE WS-IDTRPTNR         TO W-IDTRPTNR-MIN                      
000753        ELSE                                                              
000754          IF W-IDDC = SPAR-WDE6E1KY-ENTER(1:2)                            
000755            MOVE SPAR-WDE6E1KY-ENTER TO W-WDE6E1KY-MIN-X                  
000756          END-IF                                                          
000757          MOVE LOW-VALUES          TO                                     
000758                                 W-WDE6E1KY-MIN-X(6:7)                    
000759        END-IF                                                            
000760                                                                          
000761        MOVE NEJ                      TO SW-RAD-INPUT                     
000762        MOVE +1                       TO INDX                             
000763        PERFORM UNTIL (INDX > MAX-INDX)                                   
000764                                                                          
000765          IF MID-FLLASTA-RAD (INDX)   = ALL '+'                           
000766             CONTINUE                                                     
000767          ELSE                                                            
000768             MOVE JA                  TO SW-RAD-INPUT                     
000769          END-IF                                                          
000770                                                                          
000771         ADD +1                    TO INDX                                
000772        END-PERFORM                                                       
000773                                                                          
000774        IF MID-FLSIDLAST = ALL '+' AND SW-RAD-INPUT  = NEJ                
000775          PERFORM MFS-RENSA-FAELT-IN                                      
000776        ELSE                                                              
000777          MOVE INF-PRESS-PF11 TO MED-IDMFSINF                             
000778          CALL WMEDKONV USING MED-WMEDAREA                                
000779          MOVE MED-MFSINF TO MOD-TEMFSFEL                                 
000780          PERFORM EA-MID-INDATA-TILL-MOD                                  
000781        END-IF                                                            
000782     ELSE                                                                 
000783        PERFORM MFS-RENSA-FAELT-IN                                        
000784     END-IF                                                               
000785     .                                                                    
000786     EJECT                                                                
000787 EA-MID-INDATA-TILL-MOD SECTION.                                          
000788                                                                          
000789     IF  MID-FLSIDLAST            =  ALL '+'                              
000790       MOVE MFS-RENSA-FAELT       TO MOD-FLSIDLAST                        
000791     ELSE                                                                 
000792       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLSIDLAST-ATTR                   
000793       MOVE MID-FLSIDLAST         TO MOD-FLSIDLAST                        
000794     END-IF                                                               
000795                                                                          
000796     MOVE +1                     TO INDX                                  
000797                                                                          
000798     PERFORM UNTIL (INDX > MAX-INDX)                                      
000799                                                                          
000800       IF  MID-FLLASTA-RAD (INDX)   = ALL '+'                             
000801         MOVE MFS-RENSA-FAELT       TO MOD-FLLASTA-RAD (INDX)             
000802       ELSE                                                               
000803         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLLASTA-RAD-ATTR (INDX)        
000804         MOVE MID-FLLASTA-RAD (INDX) TO MOD-FLLASTA-RAD (INDX)            
000805       END-IF                                                             
000806                                                                          
000807       ADD +1                    TO INDX                                  
000808     END-PERFORM                                                          
000809                                                                          
000810     .                                                                    
000811     EJECT                                                                
000812 F-LAES-VISA-INFO SECTION.                                                
000813                                                                          
000814     MOVE ZERO                 TO W-KVKOLLI-SIDA                          
000815                                  W-VKORDBTO-SIDA                         
000816                                  W-VLORDBTO-SIDA                         
000817                                  W-KVKOLLI-TOT                           
000818                                  W-VKORDBTO-TOT                          
000819                                  W-VLORDBTO-TOT                          
000820                                                                          
000821     PERFORM FG-RAKNA-TOTALER                                             
000822                                                                          
000823     PERFORM FA-LAES-LASTAT-HITTILLS                                      
000824                                                                          
000825     MOVE +1                   TO INDX                                    
000826                                                                          
000827     IF MFS-FIRST OR                                                      
000828       (MFS-ENTER AND SPAR-IDSEGM-ENTER = 'E6E1') OR                      
000829        SPAR-IDSEGM-NEXT = 'E6E1'                                         
000830        PERFORM FB-LAES-VISA-SAMKOLLI                                     
000831     END-IF                                                               
000832                                                                          
000833     IF INDX NOT > MAX-INDX                                               
000834       PERFORM FC-LAES-VISA-KOLLI                                         
000835     END-IF                                                               
000836                                                                          
000837     IF W-KVKOLLI-SIDA > ZERO                                             
000838       MOVE MFS-OEPPNA-ALFA-FAELT TO                                      
000839                   MOD-FLSIDLAST-ATTR                                     
000840     END-IF                                                               
000841                                                                          
000842     MOVE W-KVKOLLI-SIDA          TO MOD-KVKOLLI-VALB                     
000843                                                                          
000844     IF WS-KDMATT = 'U'                                                   
000845       COMPUTE WS-VKORDBTO     =                                          
000846               W-VKORDBTO-SIDA *                                          
000847               CONV-KG-TO-LB                                              
000848       COMPUTE WS-VLORDBTO     =                                          
000849               W-VLORDBTO-SIDA *                                          
000850               CONV-M3-TO-YD3                                             
000851       MOVE WS-VKORDBTO           TO MOD-VKORDBTO-VALB                    
000852       MOVE WS-VLORDBTO           TO MOD-VLORDBTO-VALB                    
000853     ELSE                                                                 
000854       MOVE W-VKORDBTO-SIDA       TO MOD-VKORDBTO-VALB                    
000855       MOVE W-VLORDBTO-SIDA       TO MOD-VLORDBTO-VALB                    
000856     END-IF                                                               
000857                                                                          
000858     MOVE WS-IDDC-IN TO WS-IDDC                                           
000859     IF CDC-SE                                                            
000860        PERFORM FE-KOLLA-DC-SE-GODS                                       
000861     END-IF                                                               
000862* SPARA ENTER/NEXT NYCKLAR                                                
000863     MOVE '002'       TO MSGI-KDCALL                                      
000864     MOVE '4663'      TO SPAR-IDTRANS                                     
000865     MOVE SPAR-AREA   TO MSGI-SPAR-AREA                                   
000866     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
000867     .                                                                    
000868     EJECT                                                                
000869 FA-LAES-LASTAT-HITTILLS SECTION.                                         
000870                                                                          
000871     PERFORM IMS-GHU-WDGX4496                                             
000872     IF SEGMENT-FINNS                                                     
000873       MOVE 4496-KVKOLLI-LAST      TO MOD-KVKOLLI-VALD                    
000874                                                                          
000875       IF WS-KDMATT = 'U'                                                 
000876         COMPUTE WS-VKORDBTO = 4496-VKORDBTO-LASTB *                      
000877                               CONV-KG-TO-LB                              
000878         COMPUTE WS-VLORDBTO = 4496-VLORDBTO-LASTB *                      
000879                               CONV-M3-TO-YD3                             
000880                                                                          
000881         MOVE WS-VKORDBTO          TO MOD-VKORDBTO-VALD                   
000882         MOVE WS-VLORDBTO          TO MOD-VLORDBTO-VALD                   
000883       ELSE                                                               
000884         MOVE 4496-VLORDBTO-LASTB  TO MOD-VLORDBTO-VALD                   
000885         MOVE 4496-VKORDBTO-LASTB  TO MOD-VKORDBTO-VALD                   
000886       END-IF                                                             
000887     ELSE                                                                 
000888       MOVE ZERO                   TO MOD-KVKOLLI-VALD                    
000889                                      MOD-VLORDBTO-VALD                   
000890                                      MOD-VKORDBTO-VALD                   
000891     END-IF                                                               
000892     .                                                                    
000893     EJECT                                                                
000894 FB-LAES-VISA-SAMKOLLI SECTION.                                           
000895                                                                          
000896     MOVE SPACE                      TO MOD-IDPSN(INDX)                   
000897     PERFORM IMS-GU-WDE6E                                                 
000898     IF SEGMENT-FINNS                                                     
000899       MOVE SEQE-WDE6E1 (1:12)       TO SPAR-WDE6E1KY-ENTER               
000900       MOVE 'E6E1' TO SPAR-IDSEGM-ENTER                                   
000901     END-IF                                                               
000902                                                                          
000903     PERFORM UNTIL SEGMENT-SAKNAS  OR                                     
000904                   BASEN-SLUT      OR                                     
000905                   INDX > MAX-INDX                                        
000906                                                                          
000907      IF SEQE-IDDC = W-E7A-IDDC                                           
000908       MOVE SEQE-IDKOLLI-SAMP TO W-E7A-IDKOLLIS                           
000909       PERFORM IMS-GU-WDE711-ASEQ                                         
000910                                                                          
000911       IF WS-FLFARLIG   = JA                                              
000912         IF SKLI-FLFARLIG = JA                                            
000913           PERFORM FBA-SKRIV-MOD-RAD                                      
000914           MOVE '*'               TO MOD-IDPSN(INDX) (1:1)                
000915           ADD +1                    TO INDX                              
000916         END-IF                                                           
000917       END-IF                                                             
000918                                                                          
000919       IF WS-FLFARLIG   = MIXED                                           
000920         PERFORM FBA-SKRIV-MOD-RAD                                        
000921         IF SKLI-FLFARLIG = JA                                            
000922           MOVE '*'               TO MOD-IDPSN(INDX) (1:1)                
000923         END-IF                                                           
000924         ADD +1                      TO INDX                              
000925       END-IF                                                             
000926                                                                          
000927       IF WS-FLFARLIG   = NEJ                                             
000928         IF SKLI-FLFARLIG = NEJ                                           
000929           PERFORM FBA-SKRIV-MOD-RAD                                      
000930           ADD +1                         TO INDX                         
000931         END-IF                                                           
000932       END-IF                                                             
000933* MAN BEHÖVER BARA LÄSA 1 FÖREKOMST PER S-KOLLI.                          
000934* LÄSER NÄSTA SK SOM ÄR > ÄN FÖREGÅENDE INLÄSTA SK                        
000935       ADD +1 TO SEQE-IDKOLLI-SAMP                                        
000936       MOVE SEQE-IDKOLLI-SAMP   TO W-E6E-IDKOLLIS-MIN                     
000937       PERFORM IMS-GN-WDE6E                                               
000938      ELSE                                                                
000939       ADD +1 TO SEQE-IDKOLLI-SAMP                                        
000940       MOVE SEQE-IDKOLLI-SAMP   TO W-E6E-IDKOLLIS-MIN                     
000941       PERFORM IMS-GN-WDE6E                                               
000942      END-IF                                                              
000943     END-PERFORM                                                          
000944                                                                          
000945     IF INDX > MAX-INDX                                                   
000946       IF SEGMENT-FINNS                                                   
000947         MOVE NEJ                   TO SW-TRAEFF                          
000948         PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                       
000949                                      OR TRAEFF                           
000950***        JAG MÅSTE KOLLA OM DET ÄR FARLIGT ELLER EJ...                  
000951           MOVE SEQE-IDKOLLI-SAMP TO W-E7A-IDKOLLIS                       
000952           PERFORM IMS-GU-WDE711-ASEQ                                     
000953           IF SKLI-FLFARLIG = JA AND                                      
000954             (WS-FLFARLIG = JA OR WS-FLFARLIG = MIXED)                    
000955             MOVE 'E6E1' TO SPAR-IDSEGM-NEXT                              
000956             MOVE SEQE-WDE6E1 (1:12)       TO SPAR-WDE6E1KY-NEXT          
000957             MOVE JA                TO SW-TRAEFF                          
000958           ELSE                                                           
000959             IF SKLI-FLFARLIG = NEJ AND                                   
000960                WS-FLFARLIG = MIXED                                       
000961               MOVE 'E6E1' TO SPAR-IDSEGM-NEXT                            
000962               MOVE SEQE-WDE6E1 (1:12)     TO SPAR-WDE6E1KY-NEXT          
000963               MOVE JA              TO SW-TRAEFF                          
000964             ELSE                                                         
000965               IF SKLI-FLFARLIG = NEJ AND                                 
000966                  WS-FLFARLIG = NEJ                                       
000967                 MOVE 'E6E1' TO SPAR-IDSEGM-NEXT                          
000968                 MOVE SEQE-WDE6E1 (1:12)     TO SPAR-WDE6E1KY-NEXT        
000969                 MOVE JA            TO SW-TRAEFF                          
000970               ELSE                                                       
000971                 MOVE SPACE TO SPAR-IDSEGM-NEXT                           
000972                 ADD +1 TO SEQE-IDKOLLI-SAMP                              
000973                 MOVE SEQE-IDKOLLI-SAMP TO W-E6E-IDKOLLIS-MIN             
000974                 PERFORM IMS-GN-WDE6E                                     
000975               END-IF                                                     
000976             END-IF                                                       
000977           END-IF                                                         
000978         END-PERFORM                                                      
000979       ELSE                                                               
000980         MOVE SPACE  TO SPAR-IDSEGM-NEXT                                  
000981       END-IF                                                             
000982     END-IF                                                               
000983     .                                                                    
000984     EJECT                                                                
000985 FBA-SKRIV-MOD-RAD SECTION.                                               
000986                                                                          
000987     ADD +1                      TO W-KVKOLLI-SIDA                        
000988     ADD SKLI-VKKOLLIB-SAMP      TO W-VKORDBTO-SIDA                       
000989     ADD SKLI-VLKOLLIB-SAMP      TO W-VLORDBTO-SIDA                       
000990     MOVE MFS-RENSA-FAELT        TO MOD-IDDISTR  (INDX)                   
000991                                    MOD-IDKUNDNR (INDX)                   
000992                                    MOD-IDORDNR7 (INDX)                   
000993     MOVE SEQE-IDKOLLI-SAMP      TO MOD-IDKOLLI  (INDX)                   
000994     MOVE MFS-RENSA-FAELT        TO MOD-TIRFS    (INDX)                   
000995                                    MOD-IDPRODNR (INDX)                   
000996                                    MOD-IDPSN    (INDX)                   
000997     MOVE SKLI-KDKOLLI-SAMP      TO MOD-KDKOLLI  (INDX)                   
000998     IF WS-KDMATT = 'U'                                                   
000999       COMPUTE WS-VKORDBTO             =                                  
001000               SKLI-VKKOLLIB-SAMP      *                                  
001001               CONV-KG-TO-LB                                              
001002                                                                          
001003       COMPUTE WS-VLORDBTO             =                                  
001004               SKLI-VLKOLLIB-SAMP      *                                  
001005               CONV-M3-TO-YD3                                             
001006                                                                          
001007       MOVE WS-VKORDBTO             TO MOD-VKORDBTO (INDX)                
001008       MOVE WS-VLORDBTO             TO MOD-VLORDBTO (INDX)                
001009     ELSE                                                                 
001010       MOVE SKLI-VKKOLLIB-SAMP      TO MOD-VKORDBTO   (INDX)              
001011       MOVE SKLI-VLKOLLIB-SAMP      TO MOD-VLORDBTO   (INDX)              
001012     END-IF                                                               
001013     .                                                                    
001014     EJECT                                                                
001015 FC-LAES-VISA-KOLLI SECTION.                                              
001016                                                                          
001017     PERFORM IMS-GU-WDE6C                                                 
001018     PERFORM UNTIL INDX > MAX-INDX                                        
001019       IF SEGMENT-FINNS                                                   
001020         IF ((WS-FLFARLIG = JA AND SEQC-IDPSN(1) > ZERO)     OR           
001021            (WS-FLFARLIG = NEJ AND SEQC-IDPSN(1) = ZERO)     OR           
001022            (WS-FLFARLIG = MIXED))                           AND          
001023            SEQC-KDKOLSTA         =  KLI-PACK                             
001024                                                                          
001025           IF INDX = 1                                                    
001026             MOVE SEQC-WDE6C1 (1:42) TO SPAR-WDE6C1KY-ENTER               
001027             MOVE 'E6C1' TO SPAR-IDSEGM-ENTER                             
001028           END-IF                                                         
001029           ADD +1                   TO W-KVKOLLI-SIDA                     
001030           ADD SEQC-VKORDBTO-KOLLI  TO W-VKORDBTO-SIDA                    
001031           ADD SEQC-VLORDBTO-KOLLI  TO W-VLORDBTO-SIDA                    
001032           MOVE SEQC-IDDISTR        TO MOD-IDDISTR (INDX)                 
001033           MOVE SEQC-IDKUNDNR       TO MOD-IDKUNDNR (INDX)                
001034           MOVE SEQC-IDKOLLI        TO MOD-IDKOLLI (INDX)                 
001035                                                                          
001036           MOVE SEQC-DARFS (3:10)   TO MOD-TIRFS (INDX)                   
001037                                                                          
001038           MOVE SEQC-KDKOLLI        TO MOD-KDKOLLI (INDX)                 
001039                                                                          
001040           IF WS-KDMATT = 'U'                                             
001041                                                                          
001042             COMPUTE WS-VKORDBTO            =                             
001043                     SEQC-VKORDBTO-KOLLI *                                
001044                     CONV-KG-TO-LB                                        
001045                                                                          
001046             COMPUTE WS-VLORDBTO            =                             
001047                     SEQC-VLORDBTO-KOLLI *                                
001048                     CONV-M3-TO-YD3                                       
001049                                                                          
001050             MOVE WS-VKORDBTO         TO MOD-VKORDBTO (INDX)              
001051             MOVE WS-VLORDBTO         TO MOD-VLORDBTO (INDX)              
001052                                                                          
001053           ELSE                                                           
001054             MOVE SEQC-VKORDBTO-KOLLI TO MOD-VKORDBTO    (INDX)           
001055             MOVE SEQC-VLORDBTO-KOLLI TO MOD-VLORDBTO    (INDX)           
001056           END-IF                                                         
001057                                                                          
001058           IF SEQC-IDPSN(1)    >  ZERO                                    
001059              MOVE SEQC-IDPSN(1)  TO W-IDPSN-NUM                          
001060              IF SEQC-IDPSN(2) > ZERO                                     
001061                 MOVE '*'         TO MOD-IDPSN(INDX) (1:1)                
001062                 MOVE W-IDPSN-NUM TO MOD-IDPSN(INDX) (2:3)                
001063              ELSE                                                        
001064                 MOVE W-IDPSN-NUM TO MOD-IDPSN(INDX) (2:3)                
001065              END-IF                                                      
001066           ELSE                                                           
001067              MOVE SPACE         TO MOD-IDPSN(INDX)                       
001068           END-IF                                                         
001069                                                                          
001070           MOVE SEQC-IDPRODNR    TO W-IDPRODNR                            
001071                                      MOD-IDPRODNR(INDX)                  
001072           MOVE SEQC-IDKOLLI     TO W-IDKOLLI                             
001073                                                                          
001074           PERFORM IMS-GHU-WDE601                                         
001075                                                                          
001076           IF VORD-KVORDRAD = VORD-KVORDRAD-PACK AND                      
001077              VORD-KVKOLLI  = VORD-KVKOLPAC                               
001078              CONTINUE                                                    
001079           ELSE                                                           
001080              MOVE MFS-ADD-LYS-UPP-FAELT                                  
001081                           TO MOD-IDORDNR7-ATTR (INDX)                    
001082           END-IF                                                         
001083           MOVE SEQC-IDPRODNR    TO W-E4F-IDPRODNR-MIN                    
001084                                      W-E4F-IDPRODNR-MAX                  
001085           MOVE SEQC-IDKOLLI     TO W-E4F-IDKOLLI-MIN                     
001086                                    W-E4F-IDKOLLI-MAX                     
001087           PERFORM IMS-GU-WDE4F                                           
001088           MOVE SEQF-IDORDNR5    TO MOD-IDORDNR7 (INDX)                   
001089           ADD 1 TO INDX                                                  
001090         END-IF                                                           
001091                                                                          
001092         PERFORM IMS-GN-WDE6C                                             
001093       ELSE                                                               
001094         MOVE MFS-STAENG-FAELT TO MOD-FLLASTA-RAD-ATTR (INDX)             
001095         PERFORM MFS-RENSA-RAD-FAELT-UT                                   
001096         ADD 1 TO INDX                                                    
001097       END-IF                                                             
001098     END-PERFORM                                                          
001099                                                                          
001100     PERFORM FCA-SKAPA-NEXT-NYCKLAR                                       
001101     .                                                                    
001102     EJECT                                                                
001103 FCA-SKAPA-NEXT-NYCKLAR SECTION.                                          
001104                                                                          
001105* ATT KOLLA HUR MAN GÖR MED DESSA NYCKLAR                                 
001106*                                                                         
001107     MOVE NEJ                     TO SW-TRAEFF                            
001108     PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT OR                        
001109                  TRAEFF                                                  
001110       IF ((WS-FLFARLIG = JA  AND SEQC-IDPSN(1) > ZERO) OR                
001111          (WS-FLFARLIG = NEJ AND SEQC-IDPSN(1) = ZERO)       OR           
001112          (WS-FLFARLIG = MIXED))                             AND          
001113           SEQC-KDKOLSTA           =  KLI-PACK                            
001114                                                                          
001115          MOVE SEQC-WDE6C1 (1:42) TO SPAR-WDE6C1KY-NEXT                   
001116          MOVE 'E6C1' TO SPAR-IDSEGM-NEXT                                 
001117          MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                       
001118          CALL WMEDKONV USING MED-WMEDAREA                                
001119          MOVE MED-TEMFSINF       TO MOD-TEMFSINF                         
001120          MOVE JA                 TO SW-TRAEFF                            
001121       END-IF                                                             
001122       IF NOT TRAEFF                                                      
001123          PERFORM IMS-GN-WDE6C                                            
001124       END-IF                                                             
001125     END-PERFORM                                                          
001126                                                                          
001127     IF SW-TRAEFF = JA                                                    
001128       IF SEGMENT-SAKNAS OR BASEN-SLUT                                    
001129* INGA DB-SEGM ATT BLÄDDRA FÖR. SPARA ENTER-KEY I NEXT ISTÄLLET!          
001130         IF SPAR-IDSEGM-ENTER = 'E6C1'                                    
001131           MOVE SPAR-WDE6C1KY-ENTER TO SPAR-WDE6C1KY-NEXT                 
001132           MOVE 'E6C1' TO SPAR-IDSEGM-NEXT                                
001133         ELSE                                                             
001134           MOVE SPAR-WDE6E1KY-ENTER TO SPAR-WDE6E1KY-NEXT                 
001135           MOVE 'E6E1' TO SPAR-IDSEGM-NEXT                                
001136         END-IF                                                           
001137       END-IF                                                             
001138     ELSE                                                                 
001139       MOVE SPACES                  TO SPAR-WDE6C1KY-NEXT                 
001140                                       SPAR-WDE6E1KY-NEXT                 
001141**                                     SPAR-WDE6C1KY-ENTER                
001142**                                     SPAR-WDE6E1KY-ENTER                
001143     END-IF                                                               
001144     .                                                                    
001145     EJECT                                                                
001146                                                                          
001147 FE-KOLLA-DC-SE-GODS SECTION.                                             
001148                                                                          
001149     MOVE NEJ                  TO DDGS-REFILL-SW                          
001150     MOVE WC-IDDC-SE           TO W-IDDC                                  
001151                                                                          
001152     PERFORM IMS-GU-WDE6C                                                 
001153     IF SEGMENT-FINNS                                                     
001154       IF MOD-TEMFSINF = MFS-RENSA-FAELT                                  
001155         MOVE DDC-SE-MED(SPRAK-IX)TO MOD-TEMFSINF                         
001156       ELSE                                                               
001157         MOVE MORE-INFO-DDC-SE-MED(SPRAK-IX) TO                           
001158                                     MOD-TEMFSINF                         
001159       END-IF                                                             
001160     END-IF                                                               
001161     .                                                                    
001162     EJECT                                                                
001163 FG-RAKNA-TOTALER SECTION.                                                
001164                                                                          
001165* HÄR LÄSER MAN SAMTLIGA 'VANLIGA' KOLLIN FRÅN FÖRSTA SEGMENT             
001166* FÖR ATT RÄKNA TOTALER                                                   
001167     PERFORM IMS-GU-WDE6C-STA                                             
001168     PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                           
001169                                                                          
001170       IF ((WS-FLFARLIG = JA  AND SEQC-IDPSN(1) > ZERO) OR                
001171          (WS-FLFARLIG = NEJ AND SEQC-IDPSN(1) = ZERO)       OR           
001172          (WS-FLFARLIG = MIXED))                             AND          
001173          SEQC-KDKOLSTA           =  KLI-PACK                             
001174                                                                          
001175          ADD 1                   TO W-KVKOLLI-TOT                        
001176          ADD SEQC-VKORDBTO-KOLLI TO W-VKORDBTO-TOT                       
001177          ADD SEQC-VLORDBTO-KOLLI TO W-VLORDBTO-TOT                       
001178       END-IF                                                             
001179                                                                          
001180       PERFORM IMS-GN-WDE6C-STA                                           
001181                                                                          
001182     END-PERFORM                                                          
001183                                                                          
001184* HÄR LÄSER MAN SAMTLIGA SAMLINGSKOLLIN FÖR ATT RÄKNA                     
001185* TOTALER                                                                 
001186*    LÄSER FÖRSTA FÖREKOMSTEN AV SK                                       
001187     PERFORM IMS-GU-WDE6E-STA                                             
001188                                                                          
001189     PERFORM UNTIL SEGMENT-SAKNAS  OR                                     
001190                   BASEN-SLUT                                             
001191                                                                          
001192       IF WS-FLFARLIG    = JA                                             
001193         MOVE SEQE-IDKOLLI-SAMP TO W-E7A-IDKOLLIS                         
001194         PERFORM IMS-GU-WDE711-ASEQ                                       
001195         IF SKLI-FLFARLIG = JA                                            
001196           ADD 1                  TO W-KVKOLLI-TOT                        
001197           ADD SKLI-VKKOLLIB-SAMP TO W-VKORDBTO-TOT                       
001198           ADD SKLI-VLKOLLIB-SAMP TO W-VLORDBTO-TOT                       
001199           MOVE JA                TO SW-SAMKOLLI                          
001200         END-IF                                                           
001201       ELSE                                                               
001202         IF WS-FLFARLIG  = MIXED                                          
001203           MOVE SEQE-IDKOLLI-SAMP TO W-E7A-IDKOLLIS                       
001204           PERFORM IMS-GU-WDE711-ASEQ                                     
001205           ADD 1                  TO W-KVKOLLI-TOT                        
001206           ADD SKLI-VKKOLLIB-SAMP TO W-VKORDBTO-TOT                       
001207           ADD SKLI-VLKOLLIB-SAMP TO W-VLORDBTO-TOT                       
001208           MOVE JA                TO SW-SAMKOLLI                          
001209         ELSE                                                             
001210           MOVE SEQE-IDKOLLI-SAMP TO W-E7A-IDKOLLIS                       
001211           PERFORM IMS-GU-WDE711-ASEQ                                     
001212           IF SKLI-FLFARLIG = NEJ                                         
001213             ADD 1                TO W-KVKOLLI-TOT                        
001214             MOVE JA              TO SW-SAMKOLLI                          
001215             ADD SKLI-VKKOLLIB-SAMP TO W-VKORDBTO-TOT                     
001216             ADD SKLI-VLKOLLIB-SAMP TO W-VLORDBTO-TOT                     
001217             MOVE JA            TO SW-SAMKOLLI                            
001218           END-IF                                                         
001219         END-IF                                                           
001220       END-IF                                                             
001221                                                                          
001222* MAN BEHÖVER BARA LÄSA 1 FÖREKOMST PER S-KOLLI.                          
001223* LÄSER NÄSTA SK SOM ÄR > ÄN FÖREGÅENDE INLÄSTA SK                        
001224       ADD +1 TO  SEQE-IDKOLLI-SAMP                                       
001225       MOVE SEQE-IDKOLLI-SAMP  TO W-E6E-IDKOLLIS-STA                      
001226       PERFORM IMS-GN-WDE6E-STA                                           
001227     END-PERFORM                                                          
001228                                                                          
001229* ÅTERSTÄLL KOLLIS-MAX FÖR SENARE E6E-LÄSNINGAR                           
001230     MOVE W-KVKOLLI-TOT           TO MOD-KVKOLLI-TOT                      
001231                                                                          
001232     IF WS-KDMATT = 'U'                                                   
001233                                                                          
001234       COMPUTE WS-VKORDBTO-TOT = W-VKORDBTO-TOT  *                        
001235                                 CONV-KG-TO-LB                            
001236       COMPUTE WS-VLORDBTO-TOT = W-VLORDBTO-TOT  *                        
001237                                 CONV-M3-TO-YD3                           
001238       MOVE WS-VKORDBTO-TOT      TO MOD-VKORDBTO-TOT                      
001239       MOVE WS-VLORDBTO-TOT      TO MOD-VLORDBTO-TOT                      
001240     ELSE                                                                 
001241                                                                          
001242       MOVE W-VKORDBTO-TOT       TO MOD-VKORDBTO-TOT                      
001243       MOVE W-VLORDBTO-TOT       TO MOD-VLORDBTO-TOT                      
001244     END-IF                                                               
001245     .                                                                    
001246     EJECT                                                                
001247                                                                          
001248 G-KOLLA-INPUT SECTION.                                                   
001249                                                                          
001250     MOVE JA                   TO INDATA-SW                               
001251     MOVE NEJ                  TO SW-RAD-INPUT                            
001252     MOVE +1                   TO INDX                                    
001253     MOVE SPACE                TO W-MED-IDMFSFEL                          
001254     PERFORM UNTIL (INDX > MAX-INDX)                                      
001255                                                                          
001256       IF MID-FLLASTA-RAD (INDX) = ALL '+' OR SPACE                       
001257          CONTINUE                                                        
001258       ELSE                                                               
001259          MOVE JA                TO SW-RAD-INPUT                          
001260       END-IF                                                             
001261                                                                          
001262      ADD +1                     TO INDX                                  
001263     END-PERFORM                                                          
001264                                                                          
001265     IF MID-FLSIDLAST = ALL '+' AND SW-RAD-INPUT = NEJ                    
001266       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
001267       CALL WMEDKONV USING MED-WMEDAREA                                   
001268       MOVE MED-MFSFEL           TO MOD-TEMFSFEL                          
001269       PERFORM MFS-ROER-EJ-FAELT-IN                                       
001270       PERFORM MFS-ROER-EJ-FAELT-UT                                       
001271       MOVE NEJ                  TO INDATA-SW                             
001272     ELSE                                                                 
001273                                                                          
001274       IF MID-FLSIDLAST            =  JA OR YES                           
001275         IF MID-IDKOLLI(1)         = ZERO OR ALL '+'                      
001276            MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLSIDLAST-ATTR               
001277            MOVE NEJ                  TO INDATA-SW                        
001278         ELSE                                                             
001279            MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLSIDLAST-ATTR               
001280            MOVE W-LASTA-HELA-SIDAN   TO W-FUNKTION                       
001281            MOVE +1                   TO INDX                             
001282            PERFORM UNTIL (INDX       > MAX-INDX) OR                      
001283                           MID-IDKOLLI(INDX) NOT NUMERIC                  
001284               PERFORM S01-KOLLA-KOLLI                                    
001285               MOVE W-VALD            TO MID-FLLASTA-RAD(INDX)            
001286               ADD +1                 TO INDX                             
001287            END-PERFORM                                                   
001288         END-IF                                                           
001289       ELSE                                                               
001290         IF MID-FLSIDLAST             = ALL '+' OR SPACE                  
001291            MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLSIDLAST-ATTR               
001292         ELSE                                                             
001293            MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLSIDLAST-ATTR               
001294            MOVE NEJ                  TO INDATA-SW                        
001295         END-IF                                                           
001296       END-IF                                                             
001297                                                                          
001298       MOVE +1                      TO  INDX                              
001299       PERFORM UNTIL INDX           >   MAX-INDX                          
001300          IF MID-FLLASTA-RAD(INDX)  NOT = ALL '+'                         
001301             IF MID-FLLASTA-RAD(INDX) = W-VALD AND                        
001302                MID-IDKOLLI(INDX)     NUMERIC                             
001303               MOVE MFS-ALFA-FAELT-RAETT                                  
001304                                  TO MOD-FLLASTA-RAD-ATTR(INDX)           
001305               MOVE W-LASTA-VALDA-RADER                                   
001306                                  TO W-FUNKTION                           
001307               PERFORM S01-KOLLA-KOLLI                                    
001308             ELSE                                                         
001309               IF MID-FLLASTA-RAD(INDX) = SPACE                           
001310                 MOVE MFS-ALFA-FAELT-RAETT                                
001311                                 TO MOD-FLLASTA-RAD-ATTR(INDX)            
001312               ELSE                                                       
001313                 MOVE MFS-ALFA-FAELT-FEL                                  
001314                                 TO MOD-FLLASTA-RAD-ATTR(INDX)            
001315                 MOVE NEJ         TO INDATA-SW                            
001316               END-IF                                                     
001317             END-IF                                                       
001318          END-IF                                                          
001319          ADD +1                    TO INDX                               
001320       END-PERFORM                                                        
001321                                                                          
001322       IF INDATA-FEL                                                      
001323          IF W-MED-IDMFSFEL            = SPACE                            
001324             MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                    
001325          ELSE                                                            
001326             MOVE W-MED-IDMFSFEL       TO MED-IDMFSFEL                    
001327          END-IF                                                          
001328          CALL WMEDKONV USING MED-WMEDAREA                                
001329          MOVE MED-MFSFEL          TO MOD-TEMFSFEL                        
001330          PERFORM MFS-ROER-EJ-FAELT-UT                                    
001331          PERFORM MFS-ROER-EJ-FAELT-IN                                    
001332       END-IF                                                             
001333     END-IF                                                               
001334     .                                                                    
001335     EJECT                                                                
001336 H-UPPDATERA SECTION.                                                     
001337                                                                          
001338     MOVE ZERO           TO W-KVKOLLI                                     
001339                            W-VKORDBTO                                    
001340                            W-VLORDBTO                                    
001341                            W-SUORDV                                      
001342                            W-SUORDV-TOT                                  
001343                            W-SUORDV-EXP                                  
001344                            W-SUORDV-TOT-EXP                              
001345                            W-UPD-RAKNARE                                 
001346                                                                          
001347     MOVE NEJ            TO SW-OMSTART                                    
001348                                                                          
001349     PERFORM IMS-GU-4495                                                  
001350     IF SEGMENT-FINNS                                                     
001351       MOVE JA                     TO SW-TRPT                             
001352     ELSE                                                                 
001353       MOVE NEJ                    TO SW-TRPT                             
001354       MOVE LOW-VALUE              TO 4495-LOW-VALUE                      
001355       MOVE '4495'                 TO 4495-IDHTYP                         
001356       MOVE W-IDDC                 TO 4495-IDDC                           
001357       MOVE W-IDTRPTNR             TO 4495-IDTRPTNR                       
001358       MOVE W-IDLBBET              TO 4495-IDLBBET                        
001359                                                                          
001360       PERFORM IMS-ISRT-4495                                              
001361                                                                          
001362     END-IF                                                               
001363                                                                          
001364     EVALUATE TRUE                                                        
001365                                                                          
001366     WHEN LASTA-HELA-SIDAN                                                
001367       PERFORM HA-UPPDATERA-HELA-SIDAN                                    
001368                                                                          
001369     WHEN LASTA-VALDA-RADER                                               
001370         PERFORM HB-UPPDATERA-RADER                                       
001371                                                                          
001372     END-EVALUATE                                                         
001373                                                                          
001374     IF W-FUNKTION            NOT = ZERO                                  
001375        MOVE INF-UPDATE-DONE  TO MED-IDMFSINF                             
001376        CALL WMEDKONV USING MED-WMEDAREA                                  
001377        MOVE MED-MFSINF       TO MOD-TEMFSINF                             
001378     END-IF                                                               
001379     PERFORM MFS-FORM-ATTR                                                
001380     PERFORM MFS-RENSA-FAELT-IN                                           
001381     .                                                                    
001382     EJECT                                                                
001383 HA-UPPDATERA-HELA-SIDAN SECTION.                                         
001384                                                                          
001385     MOVE +1                         TO INDX                              
001386     PERFORM UNTIL INDX              >  MAX-INDX OR OMSTART               
001387       IF MID-IDKOLLI(INDX) NUMERIC AND                                   
001388          MID-FLLASTA-RAD(INDX)     =  W-VALD                             
001389         IF MID-IDDISTR(INDX) NUMERIC                                     
001390            MOVE MID-IDPRODNR(INDX)  TO W-IDPRODNR                        
001391            MOVE MID-IDKOLLI(INDX)   TO W-IDKOLLI                         
001392                                                                          
001393            MOVE MID-IDDISTR(INDX)    TO 4498-IDDISTR                     
001394                                         TEST-IDDISTR                     
001395            MOVE MID-IDKUNDNR(INDX)   TO 4498-IDKUNDNR                    
001396                                         4498-IDDEALER                    
001397            PERFORM S02-UPPDATERA-WDE6                                    
001398                                                                          
001399            MOVE SPACE                TO 4498-IDKUNDRF                    
001400            INSPECT MID-IDORDNR7(INDX)                                    
001401            REPLACING LEADING SPACE BY ZERO                               
001402            MOVE MID-IDORDNR7(INDX)   TO 4498-IDORDNR7                    
001403            MOVE MID-IDPRODNR(INDX)   TO 4498-IDPRODNR                    
001404            PERFORM S03-UPPDATERA-ARBREG-RAD                              
001405         ELSE                                                             
001406            PERFORM S06-UPPDATERA-SAMKOLLI                                
001407         END-IF                                                           
001408         IF NOT OMSTART                                                   
001409            MOVE SPACE                 TO MID-FLLASTA-RAD(INDX)           
001410         END-IF                                                           
001411       END-IF                                                             
001412       ADD +1                          TO INDX                            
001413     END-PERFORM                                                          
001414     PERFORM S04-UPPDATERA-ARBREG-TOT                                     
001415     .                                                                    
001416     EJECT                                                                
001417 HB-UPPDATERA-RADER SECTION.                                              
001418                                                                          
001419     MOVE +1                 TO INDX                                      
001420     PERFORM UNTIL INDX      >  MAX-INDX OR OMSTART                       
001421       IF MID-FLLASTA-RAD(INDX) = W-VALD                                  
001422         IF MID-IDDISTR(INDX)   NUMERIC                                   
001423            MOVE MID-IDPRODNR(INDX)   TO W-IDPRODNR                       
001424            MOVE MID-IDKOLLI(INDX)    TO W-IDKOLLI                        
001425                                                                          
001426            MOVE MID-IDDISTR(INDX)     TO 4498-IDDISTR                    
001427                                          TEST-IDDISTR                    
001428            MOVE MID-IDKUNDNR(INDX)    TO 4498-IDKUNDNR                   
001429                                          4498-IDDEALER                   
001430            PERFORM S02-UPPDATERA-WDE6                                    
001431                                                                          
001432            MOVE SPACE                 TO 4498-IDKUNDRF                   
001433            INSPECT MID-IDORDNR7(INDX)                                    
001434            REPLACING LEADING SPACE BY ZERO                               
001435            MOVE MID-IDORDNR7(INDX)    TO 4498-IDORDNR7                   
001436            MOVE MID-IDPRODNR(INDX)    TO 4498-IDPRODNR                   
001437            PERFORM S03-UPPDATERA-ARBREG-RAD                              
001438         ELSE                                                             
001439            PERFORM S06-UPPDATERA-SAMKOLLI                                
001440         END-IF                                                           
001441         IF NOT OMSTART                                                   
001442            MOVE SPACE                TO MID-FLLASTA-RAD(INDX)            
001443         END-IF                                                           
001444       END-IF                                                             
001445       ADD +1                TO INDX                                      
001446     END-PERFORM                                                          
001447     PERFORM S04-UPPDATERA-ARBREG-TOT                                     
001448     .                                                                    
001449     EJECT                                                                
001450 I-STARTA-W40663          SECTION.                                        
001451                                                                          
001452     COMPUTE P-TO-P-KVLL       =   LNG-P-TO-P-PREFIX +                    
001453                                   LENGTH OF MID-W4I66301                 
001454                                                                          
001455     MOVE LOW-VALUE            TO P-TO-P-KDZ1                             
001456     MOVE LOW-VALUE            TO P-TO-P-KDZ2                             
001457     MOVE 'W4T663U '           TO P-TO-P-KDTRANS                          
001458     MOVE '4663'               TO P-TO-P-IDTRANS                          
001459     MOVE MFS-KDMFSFOR         TO P-TO-P-KDMFSFOR                         
001460                                                                          
001461     MOVE MID-W4I66301         TO P-TO-P-DATA                             
001462     PERFORM IMS-ISRT-ALT-MSG-4663                                        
001463     .                                                                    
001464     EJECT                                                                
001465 S01-KOLLA-KOLLI   SECTION.                                               
001466                                                                          
001467     IF MID-IDDISTR (INDX) NOT NUMERIC                                    
001468* KONTROLL AV SK                                                          
001469       IF MID-IDKOLLI (INDX) > ZERO                                       
001470         CONTINUE                                                         
001471       ELSE                                                               
001472         MOVE W-FEL-795             TO W-MED-IDMFSFEL                     
001473         MOVE NEJ                   TO INDATA-SW                          
001474         MOVE MFS-ALFA-FAELT-FEL    TO MOD-FLLASTA-RAD-ATTR(INDX)         
001475       END-IF                                                             
001476     ELSE                                                                 
001477       IF MID-IDDISTR(INDX) NUMERIC                                       
001478                                                                          
001479         MOVE MID-IDPRODNR(INDX)    TO W-IDPRODNR                         
001480         MOVE MID-IDKOLLI(INDX)     TO W-IDKOLLI                          
001481                                                                          
001482         PERFORM IMS-GU-WDE611                                            
001483         IF SEGMENT-FINNS                                                 
001484           IF KOLLI-KDKOLSTA = KLI-PACK AND                               
001485              KOLLI-FLUTLAST = JA                                         
001486               CONTINUE                                                   
001487           ELSE                                                           
001488               MOVE W-FEL-721       TO W-MED-IDMFSFEL                     
001489               MOVE NEJ             TO INDATA-SW                          
001490               MOVE MFS-ALFA-FAELT-FEL                                    
001491                                    TO MOD-FLLASTA-RAD-ATTR(INDX)         
001492           END-IF                                                         
001493         ELSE                                                             
001494           MOVE W-FEL-758           TO W-MED-IDMFSFEL                     
001495           MOVE NEJ                 TO INDATA-SW                          
001496           MOVE MFS-ALFA-FAELT-FEL  TO MOD-FLLASTA-RAD-ATTR(INDX)         
001497         END-IF                                                           
001498       END-IF                                                             
001499     END-IF                                                               
001500     .                                                                    
001501     EJECT                                                                
001502 S02-UPPDATERA-WDE6    SECTION.                                           
001503                                                                          
001504     PERFORM IMS-GHU-WDE611                                               
001505                                                                          
001506*    SAMLINGSKOLLI SPECIAL - ADD BARA 1 / SK TILL KVKOLLI                 
001507     IF MID-IDDISTR(INDX) > ZERO                                          
001508       ADD 1                     TO W-KVKOLLI                             
001509       ADD KOLLI-VKORDBTO-KOLLI  TO W-VKORDBTO                            
001510       ADD KOLLI-VLORDBTO-KOLLI  TO W-VLORDBTO                            
001511     ELSE                                                                 
001512       MOVE SK-SHIPPED           TO KOLLI-KDSTASKLI                       
001513       IF KOLLI-IDKOLLI-SAMP NOT = JFR-IDKOLLI-SAMP                       
001514         ADD 1                   TO W-KVKOLLI                             
001515       END-IF                                                             
001516     END-IF                                                               
001517                                                                          
001518     MOVE NEJ                    TO KOLLI-FLUTLAST                        
001519     MOVE W-IDLBBET              TO KOLLI-IDLBBET                         
001520     MOVE KLI-PACK-FAKT          TO KOLLI-KDKOLSTA                        
001521     MOVE ZERO                   TO W-SUORDV                              
001522                                    W-SUORDV-EXP                          
001523     MOVE MSGI-TILOKDAT          TO KOLLI-TILASTN                         
001524                                                                          
001525     MOVE MSGI-TILOKTID          TO WS-TILASTID-HHMM                      
001526     MOVE FUNCTION CURRENT-DATE (13:2) TO                                 
001527                                 WS-TILASTID-SS                           
001528     MOVE WS-TILASTID            TO KOLLI-TILASTID                        
001529                                                                          
001530     PERFORM S02A-FIXA-NYCKLAR                                            
001531                                                                          
001532     PERFORM IMS-REPL-WDE611                                              
001533                                                                          
001534     PERFORM IMS-GHU-WDE601                                               
001535     MOVE VORD-KDFAKTYP           TO SPAR-KDFAKTYP                        
001536     ADD  +1                      TO VORD-KVKOLLI-FL                      
001537     IF DIST79-DEALER-PRICE                                               
001538        ADD KOLLI-SUORDV-LOC      TO VORD-SUORDV-FL-LOC                   
001539        ADD KOLLI-SUORDV-LOCPREL  TO VORD-SUORDV-FL-LOCPREL               
001540                                                                          
001541        ADD KOLLI-SUORDV-LOC      TO W-SUORDV                             
001542        ADD KOLLI-SUORDV-LOCPREL  TO W-SUORDV                             
001543        PERFORM S02B-OMRAKNING-LOC-TO-SEK                                 
001544        MOVE EXCH-SUORDV-UT       TO W-SUORDV                             
001545     ELSE                                                                 
001546       IF DIST79-ECOM-PRICE                                               
001547          ADD KOLLI-SUORDV-LOC    TO VORD-SUORDV-FL-LOC                   
001548                                     W-SUORDV                             
001549       ELSE                                                               
001550          IF KOLLI-SUORDV-KLI-EXP > ZERO                                  
001551            ADD KOLLI-SUORDV-KLI-EXP TO VORD-SUORDV-FL                    
001552          ELSE                                                            
001553            ADD KOLLI-SUORDV-KOLLI TO VORD-SUORDV-FL                      
001554          END-IF                                                          
001555          ADD KOLLI-SUORDV-KOLLI  TO W-SUORDV                             
001556          ADD KOLLI-SUORDV-KLI-EXP TO W-SUORDV-EXP                        
001557       END-IF                                                             
001558     END-IF                                                               
001559     ADD  KOLLI-VKORDBTO-KOLLI    TO VORD-VKORDBTO-FL                     
001560     ADD  KOLLI-VLORDBTO-KOLLI    TO VORD-VLORDBTO-FL                     
001561     PERFORM IMS-REPL-WDE601                                              
001562     ADD W-SUORDV                 TO W-SUORDV-TOT                         
001563     ADD W-SUORDV-EXP             TO W-SUORDV-TOT-EXP                     
001564     MOVE ZERO                    TO W-SUORDV                             
001565                                     W-SUORDV-EXP                         
001566     .                                                                    
001567     EJECT                                                                
001568 S02A-FIXA-NYCKLAR SECTION.                                               
001569                                                                          
001570*    --- FIXA BLÄDDRINGSNYCKEL                                            
001571      MOVE KOLLI-ADFLGEO        TO W-ADFLGEO-MIN                          
001572      MOVE KOLLI-ADFLOMR        TO W-ADFLOMR-MIN                          
001573      MOVE KOLLI-ADRUTNIV       TO W-ADRUTNIV-MIN                         
001574      MOVE KOLLI-ADVMODUL       TO W-ADVMODUL-MIN                         
001575      MOVE KOLLI-DARFS          TO W-DARFS-MIN                            
001576      MOVE KOLLI-IDDISTR        TO W-IDDISTR-MIN                          
001577      MOVE KOLLI-IDKUNDNR       TO W-IDKUNDNR-MIN                         
001578      MOVE W-IDPRODNR           TO W-IDPRODNR-KOLLI-MIN                   
001579      MOVE KOLLI-IDKOLLI-FLER   TO W-IDKOLLI-FLER-MIN                     
001580      MOVE KOLLI-IDKOLLI        TO W-IDKOLLI-MIN                          
001581     .                                                                    
001582     EJECT                                                                
001583 S02B-OMRAKNING-LOC-TO-SEK SECTION.                                       
001584                                                                          
001585     MOVE KOLLI-KDVALISO   TO CURR-KDVALISO-ROW                           
001586                                                                          
001587     IF CURR-KDVALISO-ROW =  'EUR'  OR 'GBP' OR 'SEK'                     
001588       OR 'CHF' OR 'NOK' OR 'DKK'                                         
001589       OR 'USD' OR 'CAD'                                                  
001590       CONTINUE                                                           
001591     ELSE                                                                 
001592       IF DIST79-DEALER-PRICE                                             
001593          MOVE KOLLI-IDDISTR    TO W-IDDISTR-WDB2                         
001594          MOVE KOLLI-IDKUNDNR   TO W-IDKUNDNR-WDB2                        
001595          PERFORM IMS-GU-WDB201                                           
001596          MOVE GMT-IDPARTNR     TO W-WDB1-IDPARTNR                        
001597          MOVE GMT-IDFTG        TO W-WDB1-IDFTG                           
001598          PERFORM IMS-GU-WDB101                                           
001599          MOVE BET-KDVALISO     TO CURR-KDVALISO-ROW                      
001600       ELSE                                                               
001601          MOVE 'SEK'            TO CURR-KDVALISO-ROW                      
001602       END-IF                                                             
001603     END-IF                                                               
001604                                                                          
001605     CALL W510CURR USING CURR-W510CURR WDG2-PCB                           
001606     IF CURR-KDSVAR = ' '                                                 
001607       MOVE CURR-PRKURS-NEW  TO EXCH-PRKURS                               
001608     ELSE                                                                 
001609       MOVE 1                TO EXCH-PRKURS                               
001610     END-IF                                                               
001611*      +1 KDCALL = LOKAL VALUTA TILL SEK                                  
001612     MOVE +1                 TO EXCH-KDCALL                               
001613     MOVE W-SUORDV           TO EXCH-SUORDV-IN                            
001614     MOVE +0                 TO EXCH-PRARTNTO-IN                          
001615     CALL W411EXCH USING    EXCH-W411EXCH                                 
001616     .                                                                    
001617     EJECT                                                                
001618 S03-UPPDATERA-ARBREG-RAD  SECTION.                                       
001619                                                                          
001620     MOVE SPAR-KDFAKTYP            TO 4498-KDFAKTYP                       
001621     MOVE KOLLI-IDKOLLI            TO 4498-IDKOLLI                        
001622     MOVE KOLLI-IDKOLLI-SAMP       TO 4498-IDKOLLI-SAMP                   
001623     MOVE KOLLI-IDPSN(1)           TO 4498-IDPSN(1)                       
001624     MOVE KOLLI-IDPSN(2)           TO 4498-IDPSN(2)                       
001625     MOVE KOLLI-KDKOLLI            TO 4498-KDKOLLI                        
001626     MOVE KOLLI-DARFS (3:10)       TO 4498-TIRFS                          
001627     MOVE KOLLI-VLORDBTO-KOLLI     TO 4498-VLORDBTO                       
001628     MOVE KOLLI-VKORDBTO-KOLLI     TO 4498-VKORDBTO                       
001629     MOVE NEJ                      TO 4498-FLCROSS                        
001630                                                                          
001631*CO-CD                                                                    
001632     PERFORM IMS-GNP-WDE621-STA1                                          
001633     IF SEGMENT-FINNS                                                     
001634       MOVE JA                     TO 4498-FLCROSS                        
001635     END-IF                                                               
001636*CO-CD                                                                    
001637                                                                          
001638     PERFORM IMS-ISRT-WDGX4498                                            
001639     .                                                                    
001640     EJECT                                                                
001641 S04-UPPDATERA-ARBREG-TOT  SECTION.                                       
001642                                                                          
001643     PERFORM IMS-GHU-WDGX4496                                             
001644                                                                          
001645     IF SEGMENT-FINNS                                                     
001646        ADD W-KVKOLLI         TO 4496-KVKOLLI-LAST                        
001647        ADD W-VKORDBTO        TO 4496-VKORDBTO-LASTB                      
001648        ADD W-VLORDBTO        TO 4496-VLORDBTO-LASTB                      
001649        IF W-SUORDV-TOT-EXP > ZERO                                        
001650          ADD W-SUORDV-TOT-EXP TO 4496-SUORDV-LASTB                       
001651        ELSE                                                              
001652          ADD W-SUORDV-TOT    TO 4496-SUORDV-LASTB                        
001653        END-IF                                                            
001654                                                                          
001655        PERFORM IMS-REPL-WDGX4496                                         
001656     ELSE                                                                 
001657        MOVE '1'              TO 4496-KDSEGKEY                            
001658        MOVE W-KVKOLLI        TO 4496-KVKOLLI-LAST                        
001659        MOVE W-VKORDBTO       TO 4496-VKORDBTO-LASTB                      
001660        MOVE W-VLORDBTO       TO 4496-VLORDBTO-LASTB                      
001661        MOVE W-SUORDV-TOT     TO 4496-SUORDV-LASTB                        
001662        IF W-SUORDV-TOT-EXP > ZERO                                        
001663          MOVE W-SUORDV-TOT-EXP TO 4496-SUORDV-LASTB                      
001664        ELSE                                                              
001665          MOVE W-SUORDV-TOT     TO 4496-SUORDV-LASTB                      
001666        END-IF                                                            
001667                                                                          
001668        PERFORM IMS-ISRT-WDGX4496                                         
001669     END-IF                                                               
001670     .                                                                    
001671     EJECT                                                                
001672 S06-UPPDATERA-SAMKOLLI    SECTION.                                       
001673                                                                          
001674     MOVE ZERO                 TO JFR-IDKOLLI-SAMP                        
001675                                                                          
001676     MOVE MID-IDKOLLI(INDX)    TO W-E6E-IDKOLLIS-MIN                      
001677                                  W-E6E-IDKOLLIS-MAX                      
001678                                                                          
001679     PERFORM IMS-GU-WDE6E                                                 
001680     PERFORM UNTIL (SEGMENT-SAKNAS OR BASEN-SLUT) OR OMSTART              
001681        MOVE SEQE-IDPRODNR         TO W-IDPRODNR                          
001682        MOVE SEQE-IDKOLLI          TO W-IDKOLLI                           
001683                                                                          
001684        PERFORM S02-UPPDATERA-WDE6                                        
001685        ADD +2                     TO W-UPD-RAKNARE                       
001686                                                                          
001687        MOVE KOLLI-IDDISTR         TO 4498-IDDISTR                        
001688                                      TEST-IDDISTR                        
001689        MOVE KOLLI-IDKUNDNR        TO 4498-IDKUNDNR                       
001690                                      4498-IDDEALER                       
001691                                                                          
001692        MOVE SPACE                 TO 4498-IDKUNDRF                       
001693        MOVE SEQE-IDPRODNR         TO 4498-IDPRODNR                       
001694                                                                          
001695        PERFORM S03-UPPDATERA-ARBREG-RAD                                  
001696        ADD +1                     TO W-UPD-RAKNARE                       
001697                                                                          
001698        PERFORM S06A-UPPDATERA-WDE7                                       
001699        ADD +1                     TO W-UPD-RAKNARE                       
001700                                                                          
001701        PERFORM S06B-UPPDAT-ARBREG-SK-TOT                                 
001702        ADD +1                     TO W-UPD-RAKNARE                       
001703                                                                          
001704        PERFORM IMS-GN-WDE6E                                              
001705        IF SEGMENT-FINNS                                                  
001706          MOVE SEQE-IDKOLLI-SAMP   TO JFR-IDKOLLI-SAMP                    
001707        END-IF                                                            
001708                                                                          
001709        IF W-UPD-RAKNARE       > W-UPD-MAX                                
001710           MOVE JA                 TO SW-OMSTART                          
001711        END-IF                                                            
001712     END-PERFORM                                                          
001713     .                                                                    
001714     EJECT                                                                
001715 S06A-UPPDATERA-WDE7 SECTION.                                             
001716                                                                          
001717     MOVE SEQE-IDKOLLI-SAMP        TO W-E7A-IDKOLLIS                      
001718     PERFORM IMS-GHU-WDE711-ASEQ                                          
001719     MOVE SK-SHIPPED               TO SKLI-KDSTASKLI                      
001720     PERFORM IMS-REPL-WDE711                                              
001721     .                                                                    
001722     EJECT                                                                
001723 S06B-UPPDAT-ARBREG-SK-TOT SECTION.                                       
001724                                                                          
001725     IF SKLI-IDKOLLI-SAMP NOT = JFR-IDKOLLI-SAMP                          
001726       MOVE SKLI-IDKOLLI-SAMP       TO 4497-IDKOLLI-SAMP                  
001727       MOVE SKLI-VKKOLLIB-SAMP      TO 4497-VKKOLLIB-SAMP                 
001728       MOVE SKLI-VLKOLLIB-SAMP      TO 4497-VLKOLLIB-SAMP                 
001729       MOVE SKLI-FLFARLIG           TO 4497-FLFARLIG                      
001730       MOVE SKLI-KDKOLLI-SAMP       TO 4497-KDKOLLI-SAMP                  
001731       PERFORM IMS-ISRT-WDGX4497                                          
001732                                                                          
001733       ADD SKLI-VKKOLLIB-SAMP       TO W-VKORDBTO                         
001734       ADD SKLI-VLKOLLIB-SAMP       TO W-VLORDBTO                         
001735     END-IF                                                               
001736     .                                                                    
001737     EJECT                                                                
001738 MFS-RENSA-FAELT-UT SECTION.                                              
001739                                                                          
001740*    --- ALLA UTDATA-FÄLT                                                 
001741*    --- INKL. BLÄDDRINGSNYCKLAR                                          
001742     MOVE MFS-RENSA-FAELT TO MOD-KVKOLLI-VALB                             
001743                             MOD-VKORDBTO-VALB                            
001744                             MOD-VLORDBTO-VALB                            
001745                             MOD-KVKOLLI-TOT                              
001746                             MOD-VKORDBTO-TOT                             
001747                             MOD-VLORDBTO-TOT                             
001748                             MOD-KVKOLLI-VALD                             
001749                             MOD-VKORDBTO-VALD                            
001750                             MOD-VLORDBTO-VALD                            
001751                                                                          
001752     MOVE +1              TO INDX                                         
001753     PERFORM  UNTIL INDX  > MAX-INDX                                      
001754       PERFORM MFS-RENSA-RAD-FAELT-UT                                     
001755       ADD +1             TO INDX                                         
001756     END-PERFORM                                                          
001757     .                                                                    
001758     SKIP3                                                                
001759 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
001760                                                                          
001761*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
001762     MOVE MFS-RENSA-FAELT TO MOD-IDDISTR  (INDX)                          
001763                             MOD-IDKUNDNR (INDX)                          
001764                             MOD-IDORDNR7 (INDX)                          
001765                             MOD-IDKOLLI  (INDX)                          
001766                             MOD-TIRFS    (INDX)                          
001767                             MOD-KDKOLLI  (INDX)                          
001768                             MOD-VKORDBTO (INDX)                          
001769                             MOD-VLORDBTO (INDX)                          
001770                             MOD-IDPSN    (INDX)                          
001771                             MOD-IDPRODNR (INDX)                          
001772     .                                                                    
001773     SKIP3                                                                
001774 MFS-RENSA-FAELT-IN SECTION.                                              
001775                                                                          
001776*    --- ALLA INDATA-FÄLT                                                 
001777     MOVE MFS-RENSA-FAELT   TO MOD-FLSIDLAST                              
001778     MOVE +1                TO INDX                                       
001779     PERFORM  UNTIL INDX    > MAX-INDX                                    
001780       MOVE MFS-RENSA-FAELT TO MOD-FLLASTA-RAD(INDX)                      
001781       ADD +1               TO INDX                                       
001782     END-PERFORM                                                          
001783     .                                                                    
001784     EJECT                                                                
001785 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
001786                                                                          
001787*    --- ALLA UTDATA-FÄLT                                                 
001788*    --- INKL BLÄDDRINGSNYCKLAR OCH RAD-DATA                              
001789     MOVE MFS-ROER-EJ-FAELT TO MOD-KVKOLLI-VALB                           
001790                               MOD-VKORDBTO-VALB                          
001791                               MOD-VLORDBTO-VALB                          
001792                               MOD-KVKOLLI-VALD                           
001793                               MOD-VKORDBTO-VALD                          
001794                               MOD-VLORDBTO-VALD                          
001795                                                                          
001796     MOVE +1                TO INDX                                       
001797     PERFORM UNTIL INDX     > MAX-INDX                                    
001798       PERFORM MFS-ROER-EJ-RAD-FAELT-UT                                   
001799       ADD +1               TO INDX                                       
001800     END-PERFORM                                                          
001801     .                                                                    
001802     SKIP2                                                                
001803 MFS-ROER-EJ-RAD-FAELT-UT  SECTION.                                       
001804                                                                          
001805*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
001806                                                                          
001807     MOVE MFS-ROER-EJ-FAELT TO MOD-IDDISTR  (INDX)                        
001808                               MOD-IDKUNDNR (INDX)                        
001809                               MOD-IDORDNR7 (INDX)                        
001810                               MOD-IDKOLLI  (INDX)                        
001811                               MOD-TIRFS    (INDX)                        
001812                               MOD-KDKOLLI  (INDX)                        
001813                               MOD-VKORDBTO (INDX)                        
001814                               MOD-VLORDBTO (INDX)                        
001815                               MOD-IDPSN    (INDX)                        
001816                               MOD-IDPRODNR (INDX)                        
001817     .                                                                    
001818     SKIP3                                                                
001819 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
001820                                                                          
001821*    --- ALLA INDATA-FÄLT                                                 
001822     MOVE MFS-ROER-EJ-FAELT   TO MOD-FLSIDLAST                            
001823     MOVE +1                  TO INDX                                     
001824     PERFORM  UNTIL INDX      > MAX-INDX                                  
001825       MOVE MFS-ROER-EJ-FAELT TO MOD-FLLASTA-RAD(INDX)                    
001826       ADD +1                 TO INDX                                     
001827     END-PERFORM                                                          
001828     .                                                                    
001829     EJECT                                                                
001830 MFS-FORM-ATTR SECTION.                                                   
001831                                                                          
001832*    --- ALLA INDATA-FÄLT                                                 
001833     MOVE MFS-FORMATETS-ATTR    TO MOD-FLSIDLAST-ATTR                     
001834     MOVE +1                    TO INDX                                   
001835     PERFORM  UNTIL INDX        > MAX-INDX                                
001836       MOVE MFS-FORMATETS-ATTR  TO MOD-FLLASTA-RAD-ATTR(INDX)             
001837       ADD +1                   TO INDX                                   
001838     END-PERFORM                                                          
001839     .                                                                    
001840     EJECT                                                                
001841* --- IMS SEKTIONER ---                                                   
001842     SKIP3                                                                
001843 IMS-GET-MSG SECTION.                                                     
001844                                                                          
001845     MOVE '  QC' TO GODK-STATUSKODER                                      
001846     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
001847     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
001848     PERFORM IMS-STATUSKONTROLL                                           
001849     .                                                                    
001850     SKIP3                                                                
001851 IMS-INSERT-MSG SECTION.                                                  
001852                                                                          
001853     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
001854       MOVE '0' TO MFS-KDHUVOMR                                           
001855     END-IF                                                               
001856     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
001857     MOVE SPACE TO GODK-STATUSKODER                                       
001858     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
001859     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
001860     PERFORM IMS-STATUSKONTROLL                                           
001861     .                                                                    
001862     EJECT                                                                
001863 IMS-ISRT-ALT-MSG-4663 SECTION.                                           
001864                                                                          
001865     MOVE SPACE TO GODK-STATUSKODER                                       
001866     CALL CBLTDLI USING ISRT ALT-PCB P-TO-P-SW                            
001867     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
001868     PERFORM IMS-STATUSKONTROLL                                           
001869     .                                                                    
001870     SKIP2                                                                
001871 IMS-GU-WDE4F                  SECTION.                                   
001872     STRING 'WDE4F1  (WDE4F1KY>=' W-WDE4F1KY-MIN-X                        
001873                    '&WDE4F1KY<=' W-WDE4F1KY-MAX-X ')'                    
001874            DELIMITED BY SIZE INTO SSA1                                   
001875     MOVE '  GE' TO GODK-STATUSKODER                                      
001876     CALL CBLTDLI USING GU WDE4F-PCB DLI-IO-E4F1 SSA1                     
001877     MOVE WDE4F-STATUS-CODE TO STATUS-WS                                  
001878     PERFORM IMS-STATUSKONTROLL                                           
001879     .                                                                    
001880     SKIP3                                                                
001881 IMS-GU-WDE6C-STA              SECTION.                                   
001882     STRING 'WDE6C1  (WDE6C1KY>=' W-WDE6C1KY-STA-X                        
001883                    '&WDE6C1KY<=' W-WDE6C1KY-MAX-X                        
001884                    '&IDDC     =' W-IDDC-X ')'                            
001885            DELIMITED BY SIZE INTO SSA1                                   
001886     MOVE '  GE' TO GODK-STATUSKODER                                      
001887     CALL CBLTDLI USING GU WDE6C-PCB DLI-IO-E6C1 SSA1                     
001888     MOVE WDE6C-STATUS-CODE TO STATUS-WS                                  
001889     PERFORM IMS-STATUSKONTROLL                                           
001890     .                                                                    
001891     SKIP3                                                                
001892 IMS-GN-WDE6C-STA              SECTION.                                   
001893     STRING 'WDE6C1  (WDE6C1KY>=' W-WDE6C1KY-STA-X                        
001894                    '&WDE6C1KY<=' W-WDE6C1KY-MAX-X                        
001895                    '&IDDC     =' W-IDDC-X ')'                            
001896            DELIMITED BY SIZE INTO SSA1                                   
001897     MOVE '  GEGB' TO GODK-STATUSKODER                                    
001898     CALL CBLTDLI USING GN WDE6C-PCB DLI-IO-E6C1 SSA1                     
001899     MOVE WDE6C-STATUS-CODE TO STATUS-WS                                  
001900     PERFORM IMS-STATUSKONTROLL                                           
001901     .                                                                    
001902     SKIP3                                                                
001903 IMS-GU-WDE6C                  SECTION.                                   
001904     STRING 'WDE6C1  (WDE6C1KY>=' W-WDE6C1KY-MIN-X                        
001905                    '&WDE6C1KY<=' W-WDE6C1KY-MAX-X                        
001906                    '&IDDC     =' W-IDDC-X ')'                            
001907            DELIMITED BY SIZE INTO SSA1                                   
001908     MOVE '  GE' TO GODK-STATUSKODER                                      
001909     CALL CBLTDLI USING GU WDE6C-PCB DLI-IO-E6C1 SSA1                     
001910     MOVE WDE6C-STATUS-CODE TO STATUS-WS                                  
001911     PERFORM IMS-STATUSKONTROLL                                           
001912     .                                                                    
001913     SKIP3                                                                
001914 IMS-GN-WDE6C            SECTION.                                         
001915                                                                          
001916     STRING 'WDE6C1  (WDE6C1KY>=' W-WDE6C1KY-MIN-X                        
001917                    '&WDE6C1KY<=' W-WDE6C1KY-MAX-X                        
001918                    '&IDDC     =' W-IDDC-X ')'                            
001919            DELIMITED BY SIZE INTO SSA1                                   
001920     MOVE '  GEGB' TO GODK-STATUSKODER                                    
001921     CALL CBLTDLI USING GN WDE6C-PCB DLI-IO-E6C1 SSA1                     
001922     MOVE WDE6C-STATUS-CODE TO STATUS-WS                                  
001923     PERFORM IMS-STATUSKONTROLL                                           
001924     .                                                                    
001925     EJECT                                                                
001926 IMS-GU-WDE6E-STA          SECTION.                                       
001927     STRING 'WDE6E1  (WDE6E1KY>=' W-WDE6E1KY-STA-X                        
001928                    '&WDE6E1KY<=' W-WDE6E1KY-MAX-X                        
001929                    '&IDTRPTNR =' W-E6E-IDTRPTNR-X ')'                    
001930            DELIMITED BY SIZE INTO SSA1                                   
001931     MOVE '  GE' TO GODK-STATUSKODER                                      
001932     CALL CBLTDLI USING GU WDE6E-PCB DLI-IO-E6E1 SSA1                     
001933     MOVE WDE6E-STATUS-CODE TO STATUS-WS                                  
001934     PERFORM IMS-STATUSKONTROLL                                           
001935     .                                                                    
001936     SKIP3                                                                
001937 IMS-GN-WDE6E-STA          SECTION.                                       
001938     STRING 'WDE6E1  (WDE6E1KY>=' W-WDE6E1KY-STA-X                        
001939                    '&WDE6E1KY<=' W-WDE6E1KY-MAX-X                        
001940                    '&IDTRPTNR =' W-E6E-IDTRPTNR-X ')'                    
001941            DELIMITED BY SIZE INTO SSA1                                   
001942     MOVE '  GEGB' TO GODK-STATUSKODER                                    
001943     CALL CBLTDLI USING GN WDE6E-PCB DLI-IO-E6E1 SSA1                     
001944     MOVE WDE6E-STATUS-CODE TO STATUS-WS                                  
001945     PERFORM IMS-STATUSKONTROLL                                           
001946     .                                                                    
001947     SKIP3                                                                
001948 IMS-GU-WDE6E              SECTION.                                       
001949     STRING 'WDE6E1  (WDE6E1KY>=' W-WDE6E1KY-MIN-X                        
001950                    '&WDE6E1KY<=' W-WDE6E1KY-MAX-X                        
001951                    '&IDTRPTNR =' W-E6E-IDTRPTNR-X ')'                    
001952            DELIMITED BY SIZE INTO SSA1                                   
001953     MOVE '  GE' TO GODK-STATUSKODER                                      
001954     CALL CBLTDLI USING GU WDE6E-PCB DLI-IO-E6E1 SSA1                     
001955     MOVE WDE6E-STATUS-CODE TO STATUS-WS                                  
001956     PERFORM IMS-STATUSKONTROLL                                           
001957     .                                                                    
001958     SKIP3                                                                
001959 IMS-GN-WDE6E        SECTION.                                             
001960                                                                          
001961     STRING 'WDE6E1  (WDE6E1KY>=' W-WDE6E1KY-MIN-X                        
001962                    '&WDE6E1KY<=' W-WDE6E1KY-MAX-X                        
001963                    '&IDTRPTNR =' W-E6E-IDTRPTNR-X ')'                    
001964            DELIMITED BY SIZE INTO SSA1                                   
001965     MOVE '  GEGB' TO GODK-STATUSKODER                                    
001966     CALL CBLTDLI USING GN WDE6E-PCB DLI-IO-E6E1 SSA1                     
001967     MOVE WDE6E-STATUS-CODE TO STATUS-WS                                  
001968     PERFORM IMS-STATUSKONTROLL                                           
001969     .                                                                    
001970     EJECT                                                                
001971 IMS-GHU-WDE601 SECTION.                                                  
001972                                                                          
001973     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
001974            DELIMITED BY SIZE INTO SSA1                                   
001975     MOVE '  ' TO GODK-STATUSKODER                                        
001976     CALL CBLTDLI USING GHU WDE6-PCB DLI-IO-E601 SSA1                     
001977     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
001978     PERFORM IMS-STATUSKONTROLL                                           
001979     .                                                                    
001980     EJECT                                                                
001981 IMS-GU-WDE611 SECTION.                                                   
001982                                                                          
001983     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
001984            DELIMITED BY SIZE INTO SSA1                                   
001985     STRING 'WDE611  (IDKOLLI  =' W-IDKOLLI-X ')'                         
001986            DELIMITED BY SIZE INTO SSA2                                   
001987     MOVE '  GE' TO GODK-STATUSKODER                                      
001988     CALL CBLTDLI USING GU WDE6-PCB DLI-IO-E611 SSA1 SSA2                 
001989     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
001990     PERFORM IMS-STATUSKONTROLL                                           
001991     .                                                                    
001992     EJECT                                                                
001993 IMS-GHU-WDE611 SECTION.                                                  
001994                                                                          
001995     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
001996            DELIMITED BY SIZE INTO SSA1                                   
001997     STRING 'WDE611  (IDKOLLI  =' W-IDKOLLI-X ')'                         
001998            DELIMITED BY SIZE INTO SSA2                                   
001999     MOVE '  ' TO GODK-STATUSKODER                                        
002000     CALL CBLTDLI USING GHU WDE6-PCB DLI-IO-E611 SSA1 SSA2                
002001     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
002002     PERFORM IMS-STATUSKONTROLL                                           
002003     .                                                                    
002004     EJECT                                                                
002005 IMS-REPL-WDE601 SECTION.                                                 
002006                                                                          
002007     MOVE '  ' TO GODK-STATUSKODER                                        
002008     CALL CBLTDLI USING REPL WDE6-PCB DLI-IO-E601                         
002009     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
002010     PERFORM IMS-STATUSKONTROLL                                           
002011     .                                                                    
002012                                                                          
002013 IMS-REPL-WDE611 SECTION.                                                 
002014                                                                          
002015     MOVE '  ' TO GODK-STATUSKODER                                        
002016     CALL CBLTDLI USING REPL WDE6-PCB DLI-IO-E611                         
002017     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
002018     PERFORM IMS-STATUSKONTROLL                                           
002019     .                                                                    
002020     EJECT                                                                
002021 IMS-GNP-WDE621-STA1  SECTION.                                            
002022                                                                          
002023     STRING 'WDE621  (KDKOLSTX =' W-KDKOLSTX-X ')'                        
002024     DELIMITED BY SIZE INTO SSA1                                          
002025     MOVE '  GE'      TO GODK-STATUSKODER                                 
002026     CALL CBLTDLI USING GNP  WDE6-PCB DLI-IO-E621 SSA1                    
002027     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
002028     PERFORM IMS-STATUSKONTROLL                                           
002029     .                                                                    
002030     SKIP3                                                                
002031 IMS-GU-WDE711-ASEQ       SECTION.                                        
002032                                                                          
002033     STRING 'WDE711  (WDE7ASEQ =' W-WDE7ASEQ-X ')'                        
002034     DELIMITED BY SIZE INTO SSA1                                          
002035     MOVE '  GE' TO GODK-STATUSKODER                                      
002036     CALL CBLTDLI USING GU WDE7-PCB DLI-IO-E711 SSA1                      
002037     MOVE WDE7-STATUS-CODE TO STATUS-WS                                   
002038     PERFORM IMS-STATUSKONTROLL                                           
002039     .                                                                    
002040     SKIP3                                                                
002041 IMS-GHU-WDE711-ASEQ SECTION.                                             
002042                                                                          
002043     STRING 'WDE711  (WDE7ASEQ =' W-WDE7ASEQ-X ')'                        
002044            DELIMITED BY SIZE INTO SSA1                                   
002045     MOVE '  ' TO GODK-STATUSKODER                                        
002046     CALL CBLTDLI USING GHU WDE7-PCB DLI-IO-E711 SSA1                     
002047     MOVE WDE7-STATUS-CODE TO STATUS-WS                                   
002048                                                                          
002049     PERFORM IMS-STATUSKONTROLL                                           
002050     .                                                                    
002051     SKIP2                                                                
002052 IMS-REPL-WDE711 SECTION.                                                 
002053                                                                          
002054     MOVE '  ' TO GODK-STATUSKODER                                        
002055     CALL CBLTDLI USING REPL WDE7-PCB DLI-IO-E711                         
002056     MOVE WDE7-STATUS-CODE TO STATUS-WS                                   
002057     PERFORM IMS-STATUSKONTROLL                                           
002058     .                                                                    
002059     SKIP2                                                                
002060 IMS-GU-WDE401-ASEK       SECTION.                                        
002061                                                                          
002062     STRING 'WDE401  (WDE4ASEQ =' W-WDE4ASEQ-X ')'                        
002063     DELIMITED BY SIZE INTO SSA1                                          
002064     MOVE '  GE' TO GODK-STATUSKODER                                      
002065     CALL CBLTDLI USING GU WDE4-PCB DLI-IO-E401 SSA1                      
002066     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
002067     PERFORM IMS-STATUSKONTROLL                                           
002068     .                                                                    
002069     SKIP3                                                                
002070 IMS-GN-WDE401-ASEK       SECTION.                                        
002071                                                                          
002072     STRING 'WDE401  (WDE4ASEQ =' W-WDE4ASEQ-X ')'                        
002073     DELIMITED BY SIZE INTO SSA1                                          
002074     MOVE '  GEGB' TO GODK-STATUSKODER                                    
002075     CALL CBLTDLI USING GN WDE4-PCB DLI-IO-E401 SSA1                      
002076     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
002077     PERFORM IMS-STATUSKONTROLL                                           
002078     .                                                                    
002079     EJECT                                                                
002080 IMS-GU-4495  SECTION.                                                    
002081     STRING 'WDR401  (WDGXKEY  =' W-WDGXKEY-X ')'                         
002082                      DELIMITED BY SIZE INTO SSA1                         
002083     MOVE '  GE' TO GODK-STATUSKODER                                      
002084     CALL CBLTDLI USING GU 4495-PCB DLI-IO-4495 SSA1                      
002085     MOVE 4495-STATUS-CODE TO STATUS-WS                                   
002086     PERFORM IMS-STATUSKONTROLL                                           
002087     .                                                                    
002088     SKIP2                                                                
002089 IMS-ISRT-4495  SECTION.                                                  
002090     MOVE  'WDR401 ' TO SSA1                                              
002091     MOVE '  ' TO GODK-STATUSKODER                                        
002092     CALL CBLTDLI USING ISRT 4495-PCB DLI-IO-4495 SSA1                    
002093     MOVE 4495-STATUS-CODE TO STATUS-WS                                   
002094     PERFORM IMS-STATUSKONTROLL                                           
002095     .                                                                    
002096     SKIP2                                                                
002097 IMS-GHU-WDGX4496 SECTION.                                                
002098     STRING 'WDR401  (WDGXKEY  =' W-WDGXKEY-X ')'                         
002099                      DELIMITED BY SIZE INTO SSA1                         
002100     MOVE  'WDGX4496 ' TO SSA2                                            
002101     MOVE '  GE' TO GODK-STATUSKODER                                      
002102     CALL CBLTDLI USING GHU 4495-PCB DLI-IO-4496 SSA1 SSA2                
002103     MOVE 4495-STATUS-CODE TO STATUS-WS                                   
002104     PERFORM IMS-STATUSKONTROLL                                           
002105     .                                                                    
002106     SKIP2                                                                
002107 IMS-ISRT-WDGX4496 SECTION.                                               
002108     STRING 'WDR401  (WDGXKEY  =' W-WDGXKEY-X ')'                         
002109                      DELIMITED BY SIZE INTO SSA1                         
002110     MOVE  'WDGX4496' TO SSA2                                             
002111     MOVE '  ' TO GODK-STATUSKODER                                        
002112     CALL CBLTDLI USING ISRT 4495-PCB DLI-IO-4496 SSA1 SSA2               
002113     MOVE 4495-STATUS-CODE TO STATUS-WS                                   
002114     PERFORM IMS-STATUSKONTROLL                                           
002115     .                                                                    
002116     SKIP2                                                                
002117 IMS-REPL-WDGX4496 SECTION.                                               
002118     MOVE '  ' TO GODK-STATUSKODER                                        
002119     CALL CBLTDLI USING REPL 4495-PCB DLI-IO-4496                         
002120     MOVE 4495-STATUS-CODE TO STATUS-WS                                   
002121     PERFORM IMS-STATUSKONTROLL                                           
002122     .                                                                    
002123     SKIP2                                                                
002124 IMS-ISRT-WDGX4497    SECTION.                                            
002125     STRING 'WDR401  (WDGXKEY  =' W-WDGXKEY-X ')'                         
002126                      DELIMITED BY SIZE INTO SSA1                         
002127     MOVE  'WDGX4497 ' TO SSA2                                            
002128     MOVE '  II' TO GODK-STATUSKODER                                      
002129     CALL CBLTDLI USING ISRT 4495-PCB DLI-IO-4497 SSA1 SSA2               
002130     MOVE 4495-STATUS-CODE TO STATUS-WS                                   
002131     PERFORM IMS-STATUSKONTROLL                                           
002132     .                                                                    
002133     SKIP2                                                                
002134 IMS-ISRT-WDGX4498 SECTION.                                               
002135     STRING 'WDR401  (WDGXKEY  =' W-WDGXKEY-X ')'                         
002136                      DELIMITED BY SIZE INTO SSA1                         
002137     MOVE  'WDGX4498' TO SSA2                                             
002138     MOVE '  ' TO GODK-STATUSKODER                                        
002139     CALL CBLTDLI USING ISRT 4495-PCB DLI-IO-4498 SSA1 SSA2               
002140     MOVE 4495-STATUS-CODE TO STATUS-WS                                   
002141     PERFORM IMS-STATUSKONTROLL                                           
002142     .                                                                    
002143     SKIP2                                                                
002144 IMS-GU-WDB201 SECTION.                                                   
002145     STRING 'WDB201  (IDGMT   >=' W-IDGMT-X ')'                           
002146                      DELIMITED BY SIZE INTO SSA1                         
002147     MOVE '    ' TO GODK-STATUSKODER                                      
002148     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-B201 SSA1                      
002149     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
002150     PERFORM IMS-STATUSKONTROLL                                           
002151     .                                                                    
002152     SKIP2                                                                
002153 IMS-GU-WDB101 SECTION.                                                   
002154                                                                          
002155     STRING 'WDB101  (WDB101KY =' W-WDB101KY-X ')'                        
002156          DELIMITED BY SIZE INTO SSA1                                     
002157     MOVE '  ' TO GODK-STATUSKODER                                        
002158     CALL CBLTDLI USING GU WDB1-PCB DLI-IO-WDB101 SSA1                    
002159     MOVE WDB1-STATUS-CODE TO STATUS-WS                                   
002160     PERFORM IMS-STATUSKONTROLL                                           
002161     .                                                                    
002162     EJECT                                                                
002163 IMS-STATUSKONTROLL SECTION.                                              
002164                                                                          
002165     SET STATUS-IX TO 1                                                   
002166     SEARCH GODK-STATUS                                                   
002167       AT END                                                             
002168         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
002169         DELIMITED BY SIZE INTO FELTEXT                                   
002170         CALL FELLOG                                                      
002171       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
002172         CONTINUE                                                         
002173     END-SEARCH                                                           
002180     .                                                                    
