000001 ID DIVISION.                                                             
000002 PROGRAM-ID.     W4063600.                                                
000003 AUTHOR.         MOGREN STINA.                                            
000004 DATE-WRITTEN.   02/04/04.                                                
000005 DATE-COMPILED.                                                           
000006                                                                          
000007*    FUNKTION:                                                            
000008*        PROGRAMMET ÄR EN BAKGRUNDS-MPP I ADD-IT MODULEN                  
000009*        SOM KOMPLETTERAR WDE1- / WDE2- SEGMENT FÖR SKEPPNINGEN           
000010*        RAD-SEGMENT SKAPAS                                               
000011*                                                                         
000012*        PROGRAMMET STARTAS AV WZ01 FRÅN    W4675 / W4676                 
000013*                                       EL. W4390 / 4399                  
000014*                                           W4677 / W4678                 
000015*                                           W47605                        
000016*                                           W4639                         
000017*                                           WL0188                        
000018*        SAMT STARTAR UPP W4637 I BUILD-IT                                
000019*                         W4631 / W4632 I SHIP-IT                         
000020*                         W4674 LASTIT                                    
000021*        PROGRAMMET STARTAR OM SIG SJÄLV EFTER ETT ANTAL KOLLI'N          
000022*                                                                         
000023*        KDTRPINF  = P  BEHANDLA BARA WDE2                                
000024*                  = J  BEHANDLA WDE1 + RADER + TRANS-31/32               
000025*                  = N  BEHANDLA WDE1 UTAN RADER O TRANS 31/32            
000026*                  = L  BEHANDLA WDE1 + TRANS W4T674                      
000027*                                                                         
000028*        PROGRAMMET SKRIVER    WDE2  TRANSPORTRELREG                      
000029*                                    FAKTURA - BILL-IT                    
000030*                              WDE1  TRANSPORTRELEASEREG.                 
000031*                              WDE4  ORDERRADER                           
000032*                              WDE6  KOLLIREG                             
000033*                              WDL9  SALDOFÖRÄNDRINGAR                    
000034*                              WDC7  DDI PRIS FRÅGA                       
000035*        PROGRAMMET LÄSER      WDB1  KUNDREG                              
000036*                              WDB2  KUNDREG                              
000037*                              WDB3  KUNDREG                              
000038*                              WDQ2  ORDERHUVUD                           
000039*                                                                         
000040*    INDATA.                                                              
000041*        MID:         W40636I1                                            
000042*                                                                         
000043*    UTDATA.                                                              
000044*        TRANSAR      W40631X                                             
000045*                     W40632X                                             
000046*                     W40637X                                             
000047*                     W40636X                                             
000048*                     W4T674                                              
000049*                                                                         
000050*    E'TRACKER: 3894923  2006-09                                          
000051*    E'TRACKER: 10196973 2013-08 EJ P&H FÖR BYTES ADM.ART.                
000052*                                                                         
000053                                                                          
000054     SKIP3                                                                
000055 ENVIRONMENT DIVISION.                                                    
000056                                                                          
000057 DATA DIVISION.                                                           
000058     EJECT                                                                
000059 WORKING-STORAGE SECTION.                                                 
000060 77  IDPGM                       PIC X(08)   VALUE 'W4063600'.            
000061                                                                          
000062*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
000063 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
000064                                                                          
000065                                                                          
000066 77  JA                          PIC X       VALUE 'J'.                   
000067 77  YES                         PIC X       VALUE 'Y'.                   
000068 77  NEJ                         PIC X       VALUE 'N'.                   
000069*77  WS-IDDC                     PIC X(2)    VALUE SPACE.                 
000070 77  KODP                        PIC X       VALUE 'P'.                   
000071 77  KODB                        PIC X       VALUE 'B'.                   
000072 77  KODL                        PIC X       VALUE 'L'.                   
000073 77  WS-DEALER                   PIC 9(2)    VALUE 1.                     
000074 77  WS-IMPORTER                 PIC 9(2)    VALUE 4.                     
000075                                                                          
000076*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
000077                                                                          
000078                                                                          
000079 77  NO-MESSAGE-SW               PIC X       VALUE 'N'.                   
000080     88  NO-MESSAGE                          VALUE 'J'.                   
000081     88  MESSAGE-OK                          VALUE 'N'.                   
000082                                                                          
000083 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
000084     88  NYCKLAR-OK                          VALUE 'J'.                   
000085     88  NYCKLAR-FEL                         VALUE 'N'.                   
000086                                                                          
000087 77  ALLT-SW                     PIC X       VALUE 'J'.                   
000088     88  ALLT-OK                             VALUE 'J'.                   
000089                                                                          
000090 77  WDE4-SW                     PIC X       VALUE 'N'.                   
000091     88  UPDE4-OK                            VALUE 'J'.                   
000092                                                                          
000093 77  DELA-SW                     PIC X       VALUE 'N'.                   
000094     88  DELA-JA                             VALUE 'J'.                   
000095     88  DELA-NEJ                            VALUE 'N'.                   
000096                                                                          
000097 77  ATERSTART-SW                PIC X       VALUE 'N'.                   
000098     88  ATERSTART                           VALUE 'J'.                   
000099                                                                          
000100 77  OMSTART-SW                  PIC X       VALUE 'N'.                   
000101     88  OMSTART                             VALUE 'J'.                   
000102                                                                          
000103 77  KOLLI-SW                    PIC X       VALUE 'J'.                   
000104     88  KOLLI-OK                            VALUE 'J'.                   
000105     88  KOLLI-EJPRIS                        VALUE 'N'.                   
000106                                                                          
000107 77  SOFT-SPEC-SW                PIC X       VALUE 'N'.                   
000108     88  SOFT-SPEC                           VALUE 'J'.                   
000109     88  SOFT-SPEC-NEJ                       VALUE 'N'.                   
000110                                                                          
000111 77  UPPDAT-IDDC-EXP-SW          PIC X       VALUE 'N'.                   
000112     88  UPPDAT-IDDC-EXP                     VALUE 'J'.                   
000113                                                                          
000114 77  SW-LYNK-NON-API             PIC X       VALUE 'N'.                   
000115     88  LYNK-NON-API                        VALUE 'J'.                   
000116                                                                          
000117 77  SW-VOR                      PIC X       VALUE 'N'.                   
000118     88  VOR                                 VALUE 'J'.                   
000119                                                                          
000120 77  SW-INITIALIZE-TOT           PIC X       VALUE 'J'.                   
000121     88  INITIALIZE-TOT                      VALUE 'N'.                   
000122                                                                          
000123 77  SW-CALC-AVGCOST             PIC X       VALUE 'N'.                   
000124     88 CALC-AVGCOST                         VALUE 'J'.                   
000125                                                                          
000126 77  MAX-CHKP                    PIC S9(5)   VALUE +6   COMP-3.           
000127 77  W-CHKP                      PIC S9(5)   VALUE ZERO COMP-3.           
000128 77  CHKP-ID                     PIC X(8)    VALUE 'W40636  '.            
000129 77  DUMMY-AREA                  PIC X(1)    VALUE SPACE.                 
000130                                                                          
000131 77  W-SVAR                      PIC X       VALUE SPACE.                 
000132                                                                          
000133 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   VALUE +33  COMP-3.           
000134 77  RKOD-ABEND-UTAN-DUMP        PIC S9(3)   VALUE +16  COMP-3.           
000135 77  KDRC-DISP                   PIC 9(4)    VALUE ZERO.                  
000136                                                                          
000137 77  WS-DAGENS-DATUM             PIC 9(6)    VALUE ZERO.                  
000138 77  WS-VOR-TID-KLAR             PIC 9(9)    VALUE ZERO.                  
000139 77  WS-PARTNER                  PIC X(1)    VALUE SPACE.                 
000140                                                                          
000141 77  IB                          PIC S9(3)  VALUE ZERO COMP-3.            
000142 77  IC                          PIC S9(3)  VALUE ZERO COMP-3.            
000143 77  IX                          PIC S9(3)  VALUE ZERO COMP-3.            
000144 77  MAX-IX                      PIC S9(3)  VALUE +7   COMP-3.            
000145 77  WS-IDMARKBO                 PIC X      VALUE SPACE.                  
000146 77  WS-IDDC-EXP                 PIC X(2)   VALUE SPACE.                  
000147 77  W-DATE-AAMM                 PIC 9(4)    VALUE ZERO.                  
000148 77  WS-KDVALISO-HUV             PIC X(3)    VALUE 'SEK'.                 
000149                                                                          
000150 77  FAKT-KDVALISO              PIC X(3)       VALUE SPACE.               
000151 77  FAKT-KDVALISO-DC           PIC X(3)       VALUE SPACE.               
000152 77  FAKT-PRKURS                PIC S9(6)V9(5) VALUE ZERO COMP-3.         
000153 77  KUND-PRKURS                PIC S9(6)V9(5) VALUE ZERO COMP-3.         
000154 77  WS-IDPARTNR-EXP            PIC X(9)     VALUE SPACE.                 
000155*                                                                         
000156 77  WS-IDKUNDNR-999            PIC S9(7)    VALUE 999  COMP-3.           
000157*    OBS! OBS! OBS!                                                       
000158*    DETTA KUNDNR ANVÄNDS BARA NÄR MAN SKAPAR EN TILLÄGGSORDER            
000159*    OCH ÄVEN OM DISTRIKTET ÄR ETT EXPORT DISTRIKT BLIR DET               
000160*    INGET STUDS FLÖDE.                                                   
000161*    OBS!                                                                 
000162                                                                          
000163 01  WS-IDSYSTEM.                                                         
000164     03 WS-IDSYST-1-3            PIC X(3)    VALUE SPACE.                 
000165     03 WS-IDSYST-4              PIC X(1)    VALUE SPACE.                 
000166                                                                          
000167 01  WS-IDSYST-K                 PIC X(1)    VALUE 'K'.                   
000168 01  WS-IDSYST-E                 PIC X(1)    VALUE 'E'.                   
000169 01  WS-IDSYST-M                 PIC X(1)    VALUE 'M'.                   
000170                                                                          
000171 01  E4-STATUS                   PIC XX      VALUE SPACE.                 
000172                                                                          
000173 01  WC-KDMOMSIN                 PIC S9(1)   VALUE ZERO COMP-3.           
000174 01  WC-IDDC                     PIC X(2)    VALUE SPACE.                 
000175                                                                          
000176*    PARAMETRAR FÖR EVENT                                                 
000177 77  EVENT-SW                     PIC X(4)   VALUE SPACE.                 
000178     88 EVENT-OK                             VALUE 'LYNK'                 
000179                                                   'POLE'                 
000180                                                   'ECOM'                 
000181                                                   'ACC '                 
000182                                                   'APA '                 
000183                                                   'APB '                 
000184                                                   'APC '                 
000185                                                   'APD '                 
000186                                                   'APE '                 
000187                                                   'APF '                 
000188                                                   'APG '                 
000189                                                   'APH '                 
000190                                                   'API '                 
000191                                                   'APJ '                 
000192                                                   'TAD '.                
000193                                                                          
000194 77  SKAPA-EVENT-SW               PIC X(1)   VALUE 'N'.                   
000195     88 SKAPA-EVENT                          VALUE 'J'.                   
000196                                                                          
000197 77  REFILL-FLOW-SW               PIC X(1)   VALUE 'N'.                   
000198     88 REFILL-FLOW                          VALUE 'J'.                   
000199                                                                          
000200 01  WS-IDEVENTORDREF.                                                    
000201     03 WS-IDDISTR-EVENT         PIC 9(4).                                
000202     03 WS-IDKUNDNR-EVENT        PIC 9(6).                                
000203     03 WS-IDORDNR7-EVENT        PIC 9(7).                                
000204     03 WS-TIREGDAT-EVENT        PIC 9(6).                                
000205                                                                          
000206 01  WS-IDEVENT-SPAR.                                                     
000207     03 WS-IDDISTR-SPAR          PIC 9(4)    VALUE ZERO.                  
000208     03 WS-IDKUNDNR-SPAR         PIC 9(6)    VALUE ZERO.                  
000209     03 WS-IDORDNR7-SPAR         PIC 9(7)    VALUE ZERO.                  
000210                                                                          
000211 01  WS-CASE-TOTALS.                                                      
000212     03 WS-SUORDV-FAKT           PIC S9(9)V9(2) VALUE ZERO COMP-3.        
000213     03 WS-SUORDV-EXP            PIC S9(9)V9(2) VALUE ZERO COMP-3.        
000214     03 WS-VKORDBTO-FAKT         PIC S9(6)V9(1) VALUE ZERO COMP-3.        
000215     03 WS-VLORDBTO-FAKT         PIC S9(4)V9(3) VALUE ZERO COMP-3.        
000216                                                                          
000217 01  WS-CASE-KDVALISO-DGFLAG.                                             
000218     03 WS-KDVALISO-EXP          PIC X(3)    VALUE SPACES.                
000219     03 WS-FLFARLIG-FLAG         PIC X(1)    VALUE SPACES.                
000220                                                                          
000221 01  KONSTANTER.                                                          
000222     03   GEN-IDSTATNR           PIC S9(9) COMP-3 VALUE 87089997.         
000223 01  ITABELL.                                                             
000224*    FÖR FELLETNING                                                       
000225   03  IA   PIC S9(5)   VALUE ZERO COMP-3.                                
000226   03  IATAB   OCCURS 100.                                                
000227     05  IANR    PIC S9(3)  COMP-3.                                       
000228     05  IADISTR PIC S9(5)  COMP-3.                                       
000229     05  IAKUNDNR PIC S9(7) COMP-3.                                       
000230     05  IAPRODNR PIC S9(7)  COMP-3.                                      
000231     05  IAKOLLI  PIC S9(5)  COMP-3.                                      
000232     05  IAPURAD  PIC S9(5)  COMP-3.                                      
000233                                                                          
000234 01  WS-IDLEVNR                  PIC X(5)    VALUE SPACE.                 
000235                                                                          
000236 01  WS-B                        PIC X(30)   VALUE SPACE.                 
000237 01  FILLER                      REDEFINES WS-B.                          
000238 03  WS-BETEXT                   PIC X(25).                               
000239 03  FILLER                      PIC X(5).                                
000240                                                                          
000241 01  FILLER                      PIC X(16)   VALUE 'WS-SEKTION'.          
000242 01  WS-SEKTION                  PIC X(32)   VALUE SPACE.                 
000243                                                                          
000244 01  WS-PGM                      PIC 9(1)    VALUE ZERO.                  
000245                                                                          
000246 01  WS-IDCOM                    PIC S9(9)   VALUE ZERO COMP-3.           
000247 01  WS-IDKOLLI                  PIC S9(5)   COMP-3.                      
000248 01  WS-KDORDKL-MAX              PIC S9(1)   VALUE ZERO COMP-3.           
000249                                                                          
000250 01  WS-TIKLOCK                  PIC S9(9)   VALUE ZERO COMP-3.           
000251 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
000252                                                                          
000253 01  WS-DATUM                    PIC 9(8).                                
000254 01  FILLER                      REDEFINES WS-DATUM.                      
000255     03  WS-SEKEL                PIC 9(2).                                
000256     03  WS-AAMMDD               PIC 9(6).                                
000257                                                                          
000258*    --- ARBETSFÄLT FÖR BERÄKNING AV DAT./TID                             
000259 77  WS-AAAAMMDD                 PIC 9(8)    VALUE ZERO.                  
000260 77  WS-TTMMSSTH                 PIC 9(8)    VALUE ZERO.                  
000261                                                                          
000262 01  WX-IDTRPTNR                 PIC 9(3)   VALUE ZERO.                   
000263 01  FILLER                      REDEFINES WX-IDTRPTNR.                   
000264   03  W-IDTRPTNR                PIC X(3).                                
000265                                                                          
000266 01  WS-SUVAT                    PIC S9(7)V9(2) VALUE ZERO COMP-3.        
000267                                                                          
000268 01  WS-PRARTNTO-LOC             PIC S9(7)V9(2) VALUE ZERO COMP-3.        
000269 01  WS-PRARTNTO-LOCPREL         PIC S9(7)V9(2) VALUE ZERO COMP-3.        
000270 01  WS-SUORDV-LOC               PIC S9(9)V9(2) VALUE ZERO COMP-3.        
000271 01  WS-SUORDV-LOCPREL           PIC S9(9)V9(2) VALUE ZERO COMP-3.        
000272 01  WS-SUORDV-BEST-LOC          PIC S9(9)V9(2) VALUE ZERO COMP-3.        
000273 01  WS-SUORDV-BEST-LOCPREL      PIC S9(9)V9(2) VALUE ZERO COMP-3.        
000274 01  WS-SUORDV-LEV-LOC           PIC S9(9)V9(2) VALUE ZERO COMP-3.        
000275 01  WS-SUORDV-LEV-LOCPREL       PIC S9(9)V9(2) VALUE ZERO COMP-3.        
000276                                                                          
000277 01  WY-PRARTNTO-LOC             PIC S9(7)V9(2) VALUE ZERO COMP-3.        
000278 01  WY-PRARTNTO-LOCPREL         PIC S9(7)V9(2) VALUE ZERO COMP-3.        
000279                                                                          
000280 01  W-SUORDV-TOT                PIC S9(9)V9(2) VALUE ZERO COMP-3.        
000281 01  W-SUORDV-NOLL               PIC S9(9)V9(2) VALUE ZERO COMP-3.        
000282 01  W-SUORDV-NOLL-LOC           PIC S9(9)V9(2) VALUE ZERO COMP-3.        
000283 01  W-SUORDV-NOLL-LOCPREL       PIC S9(9)V9(2) VALUE ZERO COMP-3.        
000284 01  W-SUORDV-DIST               PIC S9(9)V9(2) VALUE ZERO COMP-3.        
000285 01  W-SUORDV-DIST-LOC           PIC S9(9)V9(2) VALUE ZERO COMP-3.        
000286 01  W-SUORDV-DIST-LOCPREL       PIC S9(9)V9(2) VALUE ZERO COMP-3.        
000287                                                                          
000288 01  WS-VKORDNTO                 PIC S9(6)V9(3) VALUE ZERO COMP-3.        
000289                                                                          
000290 01  FILLER                      PIC X(16) VALUE 'TILLÄGG-AVDRAG'.        
000291 01  W-TILLAGG-AVDRAG.                                                    
000292     03  W-PRLEGKST              PIC S9(07)V9(02).                        
000293     03  W-RELEGKST              PIC S9(02)V9(01).                        
000294     03  W-RELEGKST-2            REDEFINES W-RELEGKST                     
000295                                 PIC SV9(3).                              
000296                                                                          
000297     03  W-PREMBHNT              PIC S9(07)V9(02).                        
000298     03  W-REEMBHNT              PIC S9(02)V9(01).                        
000299     03  W-REEMBHNT-2            REDEFINES  W-REEMBHNT                    
000300                                 PIC SV9(3).                              
000301                                                                          
000302     03  W-PRFRAKT               PIC S9(07)V9(02).                        
000303                                                                          
000304     03  W-PRFOERS               PIC S9(07)V9(02).                        
000305     03  W-REFOERS               PIC S9(02)V9(05).                        
000306                                                                          
000307                                                                          
000308     03  W-REOVKOFF              PIC S9(02)V9(05).                        
000309                                                                          
000310     03  W-PRAVDRAG              PIC S9(07)V9(02).                        
000311     03  W-REAVDRAG              PIC S9(02)V9(01).                        
000312     03  W-REAVDRAG-2            REDEFINES  W-REAVDRAG                    
000313                                 PIC SV9(3).                              
000314                                                                          
000315 01  FILLER                      PIC X(16) VALUE 'SPAR-WDE211'.           
000316 01  SPAR-WDE211.                                                         
000317*    03   -COPY WDE211   -PRE SPAR-                                       
000318                                                                          
000319 01  SPAR-NYCKEL.                                                         
000320     03  X-WDE111KY-X.                                                    
000321         05  X-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.           
000322         05  X-IDKUNDNR          PIC S9(7)   VALUE ZERO COMP-3.           
000323                                                                          
000324 01  TEST-ARTIKEL                PIC 9(9)    COMP-3.                      
000325*01  FILLER  -COPY WWART04       -RED  TEST-ARTIKEL.                      
000326*    -BYTESARTIKLAR----                                                   
000327*01  FILLER  -COPY WWBYT02       -RED  TEST-ARTIKEL.                      
000328     EJECT                                                                
000329*01  FILLER  -COPY WWBYT19       -RED  TEST-ARTIKEL.                      
000330     EJECT                                                                
000331*01  -COPY  WWDC99                                                        
000332                                                                          
000333*01  -COPY  WWIDFTG                                                       
000334                                                                          
000335*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
000336 01  GENERELLA-SUBPROGRAM.                                                
000337     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
000338     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
000339     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
000340     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
000341     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
000342     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
000343     03  W335PRIS                PIC X(8)    VALUE 'W335PRIS'.            
000344     03  W403PLAT                PIC X(8)    VALUE 'W403PLAT'.            
000345     03  W411EXCH                PIC X(8)    VALUE 'W411EXCH'.            
000346     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
000347     03  WZ01RECV                PIC X(8)    VALUE 'WZ01RECV'.            
000348     03  W510CURR                PIC X(8)    VALUE 'W510CURR'.            
000349     03  W006KOM                 PIC X(8)    VALUE 'W006KOM '.            
000350     03  W009WAIT                PIC X(8)    VALUE 'W009WAIT'.            
000351     EJECT                                                                
000352                                                                          
000353*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
000354*01 -COPY WMEDAREA                                                        
000355     SKIP3                                                                
000356*    --- PARAMETRAR TILL SUBPROGRAM W510CURR                              
000357*01 -COPY W510CURR                                                        
000358     SKIP3                                                                
000359*    --- PARAMETERS TO W009WAIT                                           
000360 77  2-SECONDS                   PIC S9(9)   COMP VALUE +200.             
000361     EJECT                                                                
000362 01  MESSAGE-CODES.                                                       
000363     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
000364     EJECT                                                                
000365*01  -COPY WWDCKONS                                                       
000366*01  -COPY WWDCLAND                                                       
000367*01  -COPY WWLANDX2                                                       
000368                                                                          
000369 01  TEST-IDDISTR                PIC 9(5)    COMP-3 VALUE ZERO.           
000370*01  FILLER   -COPY WWDIST03    -RED TEST-IDDISTR.                        
000371     EJECT                                                                
000372*01  FILLER   -COPY WWDIST18    -RED TEST-IDDISTR.                        
000373     EJECT                                                                
000374*01  FILLER   -COPY WWDIST20    -RED TEST-IDDISTR.                        
000375     EJECT                                                                
000376*01  FILLER   -COPY WWDIST28    -RED TEST-IDDISTR.                        
000377     EJECT                                                                
000378*01  FILLER   -COPY WWDIST34    -RED TEST-IDDISTR.                        
000379     EJECT                                                                
000380*01  FILLER   -COPY WWDIST35    -RED TEST-IDDISTR.                        
000381     EJECT                                                                
000382*01  FILLER   -COPY WWDIST38    -RED TEST-IDDISTR.                        
000383     EJECT                                                                
000384*01  FILLER   -COPY WWDIST42    -RED TEST-IDDISTR.                        
000385     EJECT                                                                
000386*01  FILLER   -COPY WWDIST79    -RED TEST-IDDISTR.                        
000387     EJECT                                                                
000388*01  FILLER   -COPY WWDIST92    -RED TEST-IDDISTR.                        
000389     EJECT                                                                
000390*01  FILLER   -COPY WWDIS105    -RED TEST-IDDISTR.                        
000391     EJECT                                                                
000392*01  FILLER   -COPY WWDIS107    -RED TEST-IDDISTR.                        
000393     EJECT                                                                
000394                                                                          
000395*01  -COPY WDATAREA                                                       
000396     EJECT                                                                
000397*    --- AREOR TILL GEMENSAMMA SUBPROGRAM                                 
000398*                                                                         
000399 01  FILLER                      PIC X(16)  VALUE 'W403PLAT '.            
000400*01 -COPY W403PLAT                                                        
000401                                                                          
000402 01 FILLER                       PIC X(8)   VALUE  'W335PRIS'.            
000403*   -COPY W335PRIS                                                        
000404     EJECT                                                                
000405 01 FILLER                       PIC X(8)   VALUE  'W411EXCH'.            
000406*   -COPY W411EXCH                                                        
000407     EJECT                                                                
000408*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
000409*                                                                         
000410 01  FILLER                      PIC X(16)  VALUE 'WMSGINIT'.             
000411     SKIP3                                                                
000412*01 -COPY WMSGINIT                                                        
000413     EJECT                                                                
000414                                                                          
000415*                                                                         
000416*    --- AREOR FÖR ANROP TILL WZ01  ------                                
000417 01  FILLER                      PIC X(16)   VALUE 'WZ01-AREAUT'.         
000418*01  -COPY WZ01SEND                                                       
000419                                                                          
000420                                                                          
000421*    --- AREOR FÖR ANROP FRÅN WZ01                                        
000422 01  FILLER                      PIC X(16)   VALUE 'WZ01-AREAIN'.         
000423*01  -COPY WZ01RECV                                                       
000424                                                                          
000425*    --- AREOR FÖR HANTERING AV API                                       
000426*01  FILLER                      PIC X(16)  VALUE 'MSG-KOM-WMSG'.         
000427 01  KOM-IO-AREA.                                                         
000428     03  -COPY WMSGKOM                                                    
000429                                                                          
000430 01  FILLER                    PIC X(16)   VALUE 'MSG-IO-AREA'.           
000431*01  -COPY WMSGAREA                                                       
000432*01  FILLER                      PIC X(16)   VALUE 'Z430-REQU-A '.        
000433*01  -COPY WZ0430I1  -PRE Z430-                                           
000434*    03  -COPY WAPIORD  -RED Z430-REQU-EVENT-DATA -PRE Z430-              
000435                                                                          
000436*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
000437*                                                                         
000438 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
000439     SKIP3                                                                
000440*01  MID -COPY W40636I1                                                   
000441*01  MOD -COPY W40636I1 -PRE 4636-                                        
000442 01  FILLER                      PIC X(16)  VALUE 'MID-4637-SEND'.        
000443*01  MOD -COPY W40637I1 -PRE MOD-                                         
000444*01  MOD -COPY W40632I1 -PRE 4632-                                        
000445*01  MOD -COPY W4I63101 -PRE 4631-                                        
000446 01  FILLER                      PIC X(16)   VALUE 'P-TO-P-AREA'.         
000447 01      P-TO-P-SW.                                                       
000448                                                                          
000449  02     P-TO-P-KVLL             PIC S9(4)           COMP SYNC.           
000450  02     P-TO-P-KDZ1             PIC X(1)  VALUE LOW-VALUE.               
000451  02     P-TO-P-KDZ2             PIC X(1)  VALUE LOW-VALUE.               
000452  02     P-TO-P-KDTRANS          PIC X(8).                                
000453  02     P-TO-P-IDTRANS          PIC X(4).                                
000454  02     P-TO-P-KDMFSFOR         PIC X(1).                                
000455  02     P-TO-P-DATA             PIC X(1000).                             
000456     EJECT                                                                
000457*02  MOD -COPY W4I67401 -PRE 4674- -RED P-TO-P-DATA                       
000458     EJECT                                                                
000459                                                                          
000460*    --- ARBETS-AREOR TILL -SEKTIONERNA                                   
000461*                                                                         
000462 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
000463     SKIP3                                                                
000464 01  NYCKLAR-TILL-DLI.                                                    
000465     03  W-IDSHIPM-X.                                                     
000466         05  W-IDSHIPM           PIC  9(7)   VALUE ZERO.                  
000467     03  W-WDE111KY-X.                                                    
000468         05  W-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.           
000469         05  W-IDKUNDNR          PIC S9(7)   VALUE ZERO COMP-3.           
000470     03  W-WDE121KY-X.                                                    
000471         05  W-IDPRODNR          PIC S9(7)   VALUE ZERO COMP-3.           
000472         05  W-IDKOLLI           PIC S9(5)   VALUE ZERO COMP-3.           
000473     03  W-IDPURAD-X.                                                     
000474         05  W-IDPURAD           PIC S9(5)   VALUE ZERO COMP-3.           
000475     03  W-IDARTNR-X.                                                     
000476         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
000477     03  W-IDDC-X.                                                        
000478         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
000479     03  W-IDLAND-X.                                                      
000480         05  W-IDLAND            PIC X(2)    VALUE SPACE.                 
000481     03  W-IDSKYLT-X.                                                     
000482         05  W-IDSKYLT           PIC X(3)    VALUE 'S  '.                 
000483*                                                                         
000484     03  W-WDC701KY-X.                                                    
000485         05  W-IDDISTR-C7        PIC 9(4)    VALUE ZERO.                  
000486         05  W-IDKUNDNR-C7       PIC 9(7)    VALUE ZERO.                  
000487         05  W-IDBUNDLE-C7       PIC X(15).                               
000488         05  FILLER              REDEFINES W-IDBUNDLE-C7.                 
000489           07  W-IDORDER-C7      PIC 9(7).                                
000490           07  FILLER            PIC X(8).                                
000491     03  W-IDPRQUES-X.                                                    
000492         05  W-IDPRQUES          PIC 9(7)    VALUE ZERO.                  
000493*                                                                         
000494     03  W-WDE401KY-X.                                                    
000495         05  W-IDDISTR-E4        PIC S9(5)   VALUE ZERO COMP-3.           
000496         05  W-IDKUNDNR-E4       PIC S9(7)   VALUE ZERO COMP-3.           
000497         05  W-IDKUNDRF-E4       PIC X(10)   VALUE SPACE.                 
000498         05  W-IDPRODNR-E4       PIC S9(7)   VALUE ZERO COMP-3.           
000499         05  W-IDPLKLST-E4       PIC S9(3)   VALUE ZERO COMP-3.           
000500*                                                                         
000501     03  W-WDE4ESEQ-X.                                                    
000502         05  W-IDPRODNR-ESEQ     PIC S9(7)   VALUE ZERO COMP-3.           
000503     03  W-WDE4FSEQ-X.                                                    
000504         05  W-IDPRODNR-FSEQ     PIC S9(7)   VALUE ZERO COMP-3.           
000505         05  W-IDKOLLI-FSEQ      PIC S9(5)   VALUE ZERO COMP-3.           
000506     03  W-IDPURAD-WDE4-X.                                                
000507         05  W-IDPURAD-FSEQ      PIC S9(5)   VALUE ZERO COMP-3.           
000508                                                                          
000509     03  W-WDE401KY-MIN-X.                                                
000510         05  W-IDDISTR-MINE4     PIC S9(5)   VALUE ZERO COMP-3.           
000511         05  W-IDKUNDNR-MINE4    PIC S9(7)   VALUE ZERO COMP-3.           
000512         05  W-IDKUNDRF-MINE4    PIC X(10)   VALUE SPACE.                 
000513     03  W-IDPRODNR-MINE4-X.                                              
000514         05  W-IDPRODNR-MINE4    PIC S9(7)   VALUE ZERO COMP-3.           
000515     03  W-IDPLKLST-MINE4-X.                                              
000516         05  W-IDPLKLST-MINE4    PIC S9(3)   VALUE ZERO COMP-3.           
000517     03  W-WDE401KY-MAX-X.                                                
000518         05  W-IDDISTR-MAXE4     PIC S9(5)   VALUE ZERO COMP-3.           
000519         05  W-IDKUNDNR-MAXE4    PIC S9(7)   VALUE ZERO COMP-3.           
000520         05  W-IDKUNDRF-MAXE4    PIC X(10)   VALUE SPACE.                 
000521     03  W-IDPRODNR-MAXE4-X.                                              
000522         05  W-IDPRODNR-MAXE4    PIC S9(7)   VALUE ZERO COMP-3.           
000523     03  W-IDPLKLST-MAXE4-X.                                              
000524         05  W-IDPLKLST-MAXE4    PIC S9(3)   VALUE ZERO COMP-3.           
000525     03  W-WDE421KY-X.                                                    
000526         05  W-IDPRODNR-E421     PIC S9(7)   VALUE ZERO COMP-3.           
000527         05  W-IDKOLLI-E421      PIC S9(5)   VALUE ZERO COMP-3.           
000528*                                                                         
000529     03  W-IDPRODNR-E6-X.                                                 
000530         05  W-IDPRODNR-E6       PIC S9(7)   VALUE ZERO COMP-3.           
000531     03  W-IDKOLLI-X.                                                     
000532         05  W-IDKOLLI-E6        PIC S9(5)   VALUE ZERO COMP-3.           
000533*                                                                         
000534     03  W-IDDC-CROSS-X.                                                  
000535         05  W-IDDC-CROSS        PIC  X(2).                               
000536*                                                                         
000537     03  W-KDKOLSTX-X.                                                    
000538         05 W-KDKOLSTA-CROSS     PIC S9      VALUE 6     COMP-3.          
000539                                                                          
000540     03  W-TIRECXDAT-X.                                                   
000541         05 W-TIRECXDAT          PIC 9(6)    VALUE 0.                     
000542                                                                          
000543     03  W-IDTRPTNC-X.                                                    
000544         05 W-IDTRPTNR-CROSS     PIC S9(3)   VALUE 0     COMP-3.          
000545                                                                          
000546*    -NYCKLAR TIL WDB201                                                  
000547     03  W-IDGMT-X.                                                       
000548       05  W-IDDISTR-WDB2        PIC S9(5)   VALUE ZERO COMP-3.           
000549       05  W-IDKUNDNR-WDB2       PIC S9(7)   VALUE ZERO COMP-3.           
000550                                                                          
000551     03  W-WDB101KY-X.                                                    
000552       05  W-WDB1-IDPARTNR       PIC X(9)    VALUE SPACE.                 
000553       05  W-WDB1-IDFTG          PIC 9(2)    VALUE ZERO.                  
000554                                                                          
000555     03   W-WDB301KY-X.                                                   
000556         05  W-IDDC-B3           PIC X(2)    VALUE SPACE.                 
000557         05  W-IDDISTR-B3        PIC S9(5)   VALUE ZERO COMP-3.           
000558         05  W-IDKUNDNR-B3       PIC S9(7)   VALUE ZERO COMP-3.           
000559    03  W-WDB301KY-DEF-X.                                                 
000560        05  W-IDDC-B3-DEF        PIC X(2)    VALUE SPACE.                 
000561        05  W-IDDISTR-B3-DEF     PIC S9(5)   VALUE ZERO COMP-3.           
000562        05  W-IDKUNDNR-B3-DEF    PIC S9(7) VALUE +9999999 COMP-3.         
000563                                                                          
000564     03  W-IDORDER-X.                                                     
000565         05  W-IDORDER           PIC S9(7)   VALUE ZERO COMP-3.           
000566*                                                                         
000567     03  W-WDQ211KY-X.                                                    
000568         05  W-IDDC-Q2           PIC X(2)    VALUE SPACE.                 
000569         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
000570*                                                                         
000571     03  W-IDDC-Q212-X.                                                   
000572         05  W-IDDC-Q212         PIC X(2)    VALUE SPACE.                 
000573*                                                                         
000574   03  W-WDQ2CSEQ.                                                        
000575     05  W-IDDISTR-CSEQ          PIC S9(5)   VALUE ZERO COMP-3.           
000576     05  W-IDKUNDNR-CSEQ         PIC S9(7)   VALUE ZERO COMP-3.           
000577     05  FILLER                  PIC 9(2)    VALUE ZERO.                  
000578     05  W-IDORDNR5-CSEQ         PIC 9(5).                                
000579     05  FILLER                  PIC X(3)    VALUE SPACE.                 
000580*                                                                         
000581   03    W-WDA601KY-MIN-X.                                                
000582     05    W-A601KY-MIN-IDDISTR      PIC S9(5) VALUE ZERO COMP-3.         
000583     05    W-A601KY-MIN-IDKUNDNR     PIC S9(7) VALUE ZERO COMP-3.         
000584     05    W-A601KY-MIN-IDKUNDRF.                                         
000585       07  W-A601KY-MIN-IDORDNR-7    PIC 9(7)  VALUE ZERO.                
000586       07  FILLER                    PIC X(03) VALUE SPACE.               
000587     05    W-A601KY-MIN-TIREGDAT     PIC S9(7) VALUE ZERO COMP-3.         
000588     05    W-A601KY-MIN-IDARTNR      PIC S9(9) VALUE ZERO COMP-3.         
000589     05    W-A601KY-MIN-TIREGTID     PIC S9(9) VALUE ZERO COMP-3.         
000590     05    W-A601KY-MIN-TIREGDAT-AVV PIC S9(7) VALUE ZERO COMP-3.         
000591     05    W-A601KY-MIN-TIREGTID-AVV PIC S9(9) VALUE ZERO COMP-3.         
000592                                                                          
000593   03    W-WDA601KY-MAX-X.                                                
000594     05    W-A601KY-MAX-IDDISTR      PIC S9(5) VALUE ZERO COMP-3.         
000595     05    W-A601KY-MAX-IDKUNDNR     PIC S9(7) VALUE ZERO COMP-3.         
000596     05    W-A601KY-MAX-IDKUNDRF.                                         
000597       07  W-A601KY-MAX-IDORDNR-7    PIC 9(7)  VALUE ZERO.                
000598       07  FILLER                    PIC X(03) VALUE SPACE.               
000599     05    W-A601KY-MAX-TIREGDAT     PIC S9(7) VALUE ZERO COMP-3.         
000600     05    W-A601KY-MAX-IDARTNR      PIC S9(9) VALUE ZERO COMP-3.         
000601     05    W-A601KY-MAX-TIREGTID     PIC S9(9) VALUE ZERO COMP-3.         
000602     05    W-A601KY-MAX-TIREGDAT-AVV PIC S9(7) VALUE ZERO COMP-3.         
000603     05    W-A601KY-MAX-TIREGTID-AVV PIC S9(9) VALUE ZERO COMP-3.         
000604     SKIP2                                                                
000605   03    W-WDA6BSEQ-MIN-X.                                                
000606     05    W-A6BSEQ-MIN-IDDISTR      PIC S9(5) VALUE ZERO COMP-3.         
000607     05    W-A6BSEQ-MIN-IDKUNDNR     PIC S9(7) VALUE ZERO COMP-3.         
000608     05    W-A6BSEQ-MIN-IDKUNDRF-LEV PIC X(10) VALUE SPACE.               
000609     05    W-A6BSEQ-MIN-TIREGDAT-LEV PIC S9(7) VALUE ZERO COMP-3.         
000610     05    W-A6BSEQ-MIN-IDARTNR      PIC S9(9) VALUE ZERO COMP-3.         
000611     05    FILLER                    PIC X(14) VALUE SPACE.               
000612     SKIP2                                                                
000613   03    W-WDA6BSEQ-MAX-X.                                                
000614     05    W-A6BSEQ-MAX-IDDISTR      PIC S9(5) VALUE ZERO COMP-3.         
000615     05    W-A6BSEQ-MAX-IDKUNDNR     PIC S9(7) VALUE ZERO COMP-3.         
000616     05    W-A6BSEQ-MAX-IDKUNDRF-LEV PIC X(10) VALUE SPACE.               
000617     05    W-A6BSEQ-MAX-TIREGDAT-LEV PIC S9(7) VALUE ZERO COMP-3.         
000618     05    W-A6BSEQ-MAX-IDARTNR      PIC S9(9) VALUE ZERO COMP-3.         
000619     05    FILLER                    PIC X(14) VALUE SPACE.               
000620     SKIP2                                                                
000621   03  W-WDA501KY-A5-MIN-X.                                               
000622       05  W-IDDISTR-A5-MIN          PIC S9(5) VALUE ZERO COMP-3.         
000623       05  W-IDKUNDNR-A5-MIN         PIC S9(7) VALUE ZERO COMP-3.         
000624       05  FILLER                    PIC X(17) VALUE LOW-VALUE.           
000625                                                                          
000626   03  W-WDA501KY-A5-MAX-X.                                               
000627       05  W-IDDISTR-A5-MAX          PIC S9(5) VALUE ZERO COMP-3.         
000628       05  W-IDKUNDNR-A5-MAX         PIC S9(7) VALUE ZERO COMP-3.         
000629       05  FILLER                    PIC X(17) VALUE HIGH-VALUE.          
000630                                                                          
000631   03  W-KDORDKL-X.                                                       
000632       05  W-KDORDKL                 PIC S9    VALUE ZERO COMP-3.         
000633                                                                          
000634   03    W-IDKUNDRF-LEV-X.                                                
000635     05  W-IDKUNDRF-LEV              PIC X(10)   VALUE SPACE.             
000636                                                                          
000637    03  W-IDDC-B6-X.                                                      
000638        05 W-IDDC-B6                 PIC X(2).                            
000639                                                                          
000640    03  W-KDSEGKEY-X.                                                     
000641        05  W-KDSEGKEY               PIC X(1)  VALUE '1'.                 
000642                                                                          
000643     SKIP2                                                                
000644                                                                          
000645*    --- STATUS-KOD FRÅN IMS                                              
000646 01  STATUS-WS                   PIC XX.                                  
000647     88  SEGMENT-FINNS                       VALUE '  '.                  
000648     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
000649     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
000650     88  SEGMENT-SLUT                        VALUE 'GB'.                  
000651     88  IMS-EJ-OK                           VALUE 'XD'.                  
000652     88  IMS-DUBBEL-INDEX                    VALUE 'NI'.                  
000653     SKIP2                                                                
000654 01  GODK-STATUSKODER.                                                    
000655     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
000656     SKIP3                                                                
000657 01  SSA1                        PIC X(200).                              
000658 01  SSA2                        PIC X(64).                               
000659 01  SSA3                        PIC X(64).                               
000660     EJECT                                                                
000661*    --- IMS FUNKTIONSKODER                                               
000662*01  -COPY W0003                                                          
000663     EJECT                                                                
000664                                                                          
000665*    ---  DLI INPUT-OUTPUT AREA                                           
000666                                                                          
000667 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDE101'.             
000668 01  DLI-IO-WDE101.                                                       
000669*    03  -COPY WDE101                                                     
000670     EJECT                                                                
000671 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDE111'.             
000672 01  DLI-IO-WDE111.                                                       
000673*    03  -COPY WDE111                                                     
000674     EJECT                                                                
000675 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDE121'.             
000676 01  DLI-IO-WDE121.                                                       
000677*    03  -COPY WDE121                                                     
000678     EJECT                                                                
000679 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDE122'.             
000680 01  DLI-IO-WDE122.                                                       
000681*    03  -COPY WDE122                                                     
000682     EJECT                                                                
000683 01  FILLER                   PIC X(16) VALUE 'DLI-IO-WDE131'.            
000684 01  DLI-IO-WDE131.                                                       
000685*    03  -COPY WDE131                                                     
000686     EJECT                                                                
000687                                                                          
000688 01  FILLER                   PIC X(16) VALUE 'DLI-IO-WDE201'.            
000689 01  DLI-IO-WDE201.                                                       
000690*    03  -COPY WDE201                                                     
000691     EJECT                                                                
000692 01  FILLER                   PIC X(16) VALUE 'DLI-IO-WDE211'.            
000693 01  DLI-IO-WDE211.                                                       
000694*    03  -COPY WDE211                                                     
000695     EJECT                                                                
000696 01  FILLER                   PIC X(16) VALUE 'DLI-IO-WDE221'.            
000697 01  DLI-IO-WDE221.                                                       
000698*    03  -COPY WDE221                                                     
000699     EJECT                                                                
000700 01  FILLER                   PIC X(16) VALUE 'DLI-IO-WDE231'.            
000701 01  DLI-IO-WDE231.                                                       
000702*    03  -COPY WDE231                                                     
000703     EJECT                                                                
000704                                                                          
000705 01  FILLER                   PIC X(16) VALUE 'DLI-IO-WDE401'.            
000706 01  DLI-IO-WDE401.                                                       
000707*    03  -COPY WDE401                                                     
000708     EJECT                                                                
000709 01  FILLER                   PIC X(16) VALUE 'DLI-IO-WDE411'.            
000710 01  DLI-IO-WDE411-21.                                                    
000711     03 DLI-IO-WDE411.                                                    
000712*     05  -COPY WDE411                                                    
000713     03 DLI-IO-WDE421.                                                    
000714*     05  -COPY WDE421                                                    
000715     EJECT                                                                
000716                                                                          
000717 01  FILLER                   PIC X(16) VALUE 'DLI-IO-WDE601'.            
000718 01  DLI-IO-WDE601.                                                       
000719*    03  -COPY WDE601                                                     
000720     EJECT                                                                
000721 01  FILLER                   PIC X(16) VALUE 'DLI-IO-WDE611'.            
000722 01  DLI-IO-WDE611.                                                       
000723*    03  -COPY WDE611                                                     
000724     EJECT                                                                
000725 01  FILLER                   PIC X(16) VALUE 'DLI-IO-WDE621'.            
000726 01  DLI-IO-WDE621.                                                       
000727*    03  -COPY WDE621                                                     
000728     EJECT                                                                
000729 01  FILLER                   PIC X(16) VALUE 'DLI-IO-WDC701'.            
000730 01  DLI-IO-WDC701.                                                       
000731*    03  -COPY WDC701                                                     
000732     EJECT                                                                
000733 01  FILLER                   PIC X(16) VALUE 'DLI-IO-WDC711'.            
000734 01  DLI-IO-WDC711.                                                       
000735*    03  -COPY WDC711                                                     
000736     EJECT                                                                
000737 01  FILLER                   PIC X(16)   VALUE 'DLI-IO-WDK601'.          
000738 01  DLI-IO-WDK601.                                                       
000739*    03  -COPY WDK601                                                     
000740 01  FILLER                   PIC X(16)   VALUE 'DLI-IO-WDK611'.          
000741 01  DLI-IO-WDK611.                                                       
000742*    03  -COPY WDK611                                                     
000743     EJECT                                                                
000744 01  FILLER                   PIC X(16) VALUE 'DLI-IO-WDK701'.            
000745 01  DLI-IO-WDK701.                                                       
000746*    03  -COPY WDK701                                                     
000747     EJECT                                                                
000748 01  FILLER                   PIC X(16) VALUE 'DLI-IO-WDK711'.            
000749 01  DLI-IO-WDK711.                                                       
000750*    03  -COPY WDK711                                                     
000751     EJECT                                                                
000752 01  FILLER                   PIC X(16) VALUE 'DLI-IO-WDK712'.            
000753 01  DLI-IO-WDK712.                                                       
000754*    03  -COPY WDK712                                                     
000755     EJECT                                                                
000756 01  FILLER                   PIC X(16) VALUE 'DLI-IO-WDB101'.            
000757 01  DLI-IO-WDB101.                                                       
000758*     03  -COPY WDB101.                                                   
000759     EJECT                                                                
000760 01  FILLER                   PIC X(16) VALUE 'DLI-IO-WDB201'.            
000761 01  DLI-IO-WDB201.                                                       
000762*     03  -COPY WDB201.                                                   
000763     EJECT                                                                
000764 01  FILLER                   PIC X(16) VALUE 'DLI-IO-WDB301'.            
000765 01  DLI-IO-WDB301.                                                       
000766*     03  -COPY WDB301.                                                   
000767     EJECT                                                                
000768 01  FILLER                   PIC X(16) VALUE  'DLI-IO-WDD311'.           
000769 01  DLI-IO-WDD311.                                                       
000770*    03  -COPY WDD311                                                     
000771     EJECT                                                                
000772 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDL901'.           
000773 01  DLI-IO-WDL901.                                                       
000774*    03  -COPY WDL901                                                     
000775 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDQ201'.           
000776 01  DLI-IO-WDQ201.                                                       
000777*    03  -COPY WDQ201                                                     
000778     EJECT                                                                
000779 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDQ201'.           
000780 01  DLI-IO-WDQ201-C.                                                     
000781*    03  -COPY WDQ201  -PRE CSEQ-                                         
000782     EJECT                                                                
000783 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDA601'.           
000784 01  DLI-IO-WDA601.                                                       
000785*    03  -COPY WDA601                                                     
000786                                                                          
000787 01  FILLER                    PIC X(16) VALUE 'WDB601 AREA'.             
000788 01   DLI-IO-AREA-B601.                                                   
000789*     03  -COPY WDB601                                                    
000790                                                                          
000791 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDB617'.           
000792 01  DLI-IO-WDB617.                                                       
000793*    03  -COPY WDB617                                                     
000794     EJECT                                                                
000795 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDA501'.           
000796 01  DLI-IO-WDA501.                                                       
000797*    03  -COPY WDA501                                                     
000798                                                                          
000799     EJECT                                                                
000800                                                                          
000801 LINKAGE SECTION.                                                         
000802                                                                          
000803 01  IO-PCB       PIC X.                                                  
000804                                                                          
000805*01  -COPY W0009  -PRE MSG-                                               
000806     EJECT                                                                
000807 01  0693-PCB     PIC X.                                                  
000808     EJECT                                                                
000809*01  -COPY W0009  -PRE  SH31-                                             
000810     EJECT                                                                
000811*01  -COPY W0009  -PRE  SH32-                                             
000812     EJECT                                                                
000813*01  -COPY W0009  -PRE  BU37-                                             
000814     EJECT                                                                
000815*01  -COPY W0009  -PRE  AD36-                                             
000816     EJECT                                                                
000817*01  -COPY W0009  -PRE LA74-                                              
000818     EJECT                                                                
000819*01  -COPY W0008  -PRE WDE1-                                              
000820     05  FILLER                  PIC X.                                   
000821*01  -COPY W0008  -PRE WDE1X-                                             
000822     05  FILLER                  PIC X.                                   
000823*01  -COPY W0008  -PRE WDE2-                                              
000824     05  FILLER                  PIC X.                                   
000825*01  -COPY W0008  -PRE WDE2X-                                             
000826     05  FILLER                  PIC X.                                   
000827*01  -COPY W0008  -PRE WDE4-                                              
000828     05  FILLER                  PIC X.                                   
000829*01  -COPY W0008  -PRE WDE4X-                                             
000830     05  FILLER                  PIC X.                                   
000831*01  -COPY W0008  -PRE WDE6-                                              
000832     05  FILLER                  PIC X.                                   
000833*01  -COPY W0008  -PRE WDC7-                                              
000834     05  FILLER                  PIC X.                                   
000835*01  -COPY W0008  -PRE WDK6-                                              
000836     05  FILLER                  PIC X.                                   
000837*01  -COPY W0008  -PRE WDK7-                                              
000838     05  FILLER                  PIC X.                                   
000839*01  -COPY W0008  -PRE WDG2-                                              
000840     05  FILLER                  PIC X.                                   
000841*01  -COPY W0008  -PRE WDB1-                                              
000842     05  FILLER                  PIC X.                                   
000843*01  -COPY W0008  -PRE WDB2-                                              
000844     05  FILLER                  PIC X.                                   
000845*01  -COPY W0008  -PRE WDB3-                                              
000846     05  FILLER                  PIC X.                                   
000847*01  -COPY W0008  -PRE WDD3-                                              
000848     05  FILLER                  PIC X.                                   
000849*01  -COPY W0008  -PRE WDL9-                                              
000850     05  FILLER                  PIC X.                                   
000851*01  -COPY W0008  -PRE WDQ2-                                              
000852     05  FILLER                  PIC X.                                   
000853     SKIP2                                                                
000854*01  -COPY W0008  -PRE WDA6B-                                             
000855     05  FILLER                  PIC X.                                   
000856     SKIP2                                                                
000857*01  -COPY W0008  -PRE WDB6-                                              
000858     05  FILLER                  PIC X.                                   
000859     EJECT                                                                
000860*01  -COPY W0008  -PRE WDQ2C-                                             
000861     05  FILLER                  PIC X.                                   
000862     EJECT                                                                
000863*01  -COPY W0008  -PRE WDA5-                                              
000864     05  FILLER                  PIC X.                                   
000865     SKIP2                                                                
000866 01  PRIS-ARTC-PCB               PIC X.                                   
000867 01  PRIS-WDK7-PCB               PIC X.                                   
000868 01  PRIS-GMTA-PCB               PIC X.                                   
000869 01  PRIS-BETA-PCB               PIC X.                                   
000870 01  PRIS-GPRIA-PCB              PIC X.                                   
000871 01  PRIS-GPRIB-PCB              PIC X.                                   
000872 01  PRIS-COST-WDK6-PCB          PIC X.                                   
000873 01  PRIS-COST-WDK7-PCB          PIC X.                                   
000874 01  PRIS-COST-WDF1-PCB          PIC X.                                   
000875 01  PRIS-COST-9305-PCB          PIC X.                                   
000876 01  PRIS-COST-WDK72-PCB         PIC X.                                   
000877 01  PRIS-COST-WDB6-PCB          PIC X.                                   
000878*                                                                         
000879 01  PLATS-DM-PCB                PIC X.                                   
000880 01  PLATS-DN-PCB                PIC X.                                   
000881 01  PLATS-DP-PCB                PIC X.                                   
000882 01  PLATS-DO-PCB                PIC X.                                   
000883 01  PLATS-WDE6C-PCB             PIC X.                                   
000884 01  PLATS-GMTC-PCB              PIC X.                                   
000885 01  PLATS-WDB6-PCB              PIC X.                                   
000886*                                                                         
000887 01  KOM-WDP8-PCB                PIC X.                                   
000888*                                                                         
000889 PROCEDURE DIVISION  USING          IO-PCB  0693-PCB                      
000890                                   SH32-PCB SH31-PCB BU37-PCB             
000891                                   AD36-PCB LA74-PCB                      
000892                                   WDE1-PCB WDE1X-PCB                     
000893                                   WDE2-PCB WDE2X-PCB                     
000894                                   WDE4-PCB WDE4X-PCB                     
000895                                   WDE6-PCB                               
000896                                   WDC7-PCB                               
000897                                   WDK6-PCB                               
000898                                   WDK7-PCB                               
000899                                   WDG2-PCB                               
000900                                   WDB1-PCB                               
000901                                   WDB2-PCB                               
000902                                   WDB3-PCB                               
000903                                   WDD3-PCB                               
000904                                   WDL9-PCB                               
000905                                   WDQ2-PCB                               
000906                                   WDA6B-PCB                              
000907                                   WDB6-PCB                               
000908                                   WDQ2C-PCB                              
000909                                   WDA5-PCB                               
000910                                   PRIS-ARTC-PCB                          
000911                                   PRIS-WDK7-PCB                          
000912                                   PRIS-GMTA-PCB                          
000913                                   PRIS-BETA-PCB                          
000914                                   PRIS-GPRIA-PCB                         
000915                                   PRIS-GPRIB-PCB                         
000916                                   PRIS-COST-WDK6-PCB                     
000917                                   PRIS-COST-WDK7-PCB                     
000918                                   PRIS-COST-WDF1-PCB                     
000919                                   PRIS-COST-9305-PCB                     
000920                                   PRIS-COST-WDK72-PCB                    
000921                                   PRIS-COST-WDB6-PCB                     
000922                                   PLATS-DM-PCB                           
000923                                   PLATS-DN-PCB                           
000924                                   PLATS-DP-PCB                           
000925                                   PLATS-DO-PCB                           
000926                                   PLATS-WDE6C-PCB                        
000927                                   PLATS-GMTC-PCB                         
000928                                   PLATS-WDB6-PCB                         
000929                                   KOM-WDP8-PCB.                          
000930 MAIN SECTION.                                                            
000931     ENTRY 'DLITCBL' USING          IO-PCB  0693-PCB                      
000932                                   SH32-PCB SH31-PCB BU37-PCB             
000933                                   AD36-PCB LA74-PCB                      
000934                                   WDE1-PCB WDE1X-PCB                     
000935                                   WDE2-PCB WDE2X-PCB                     
000936                                   WDE4-PCB WDE4X-PCB                     
000937                                   WDE6-PCB                               
000938                                   WDC7-PCB                               
000939                                   WDK6-PCB                               
000940                                   WDK7-PCB                               
000941                                   WDG2-PCB                               
000942                                   WDB1-PCB                               
000943                                   WDB2-PCB                               
000944                                   WDB3-PCB                               
000945                                   WDD3-PCB                               
000946                                   WDL9-PCB                               
000947                                   WDQ2-PCB                               
000948                                   WDA6B-PCB                              
000949                                   WDB6-PCB                               
000950                                   WDQ2C-PCB                              
000951                                   WDA5-PCB                               
000952                                   PRIS-ARTC-PCB                          
000953                                   PRIS-WDK7-PCB                          
000954                                   PRIS-GMTA-PCB                          
000955                                   PRIS-BETA-PCB                          
000956                                   PRIS-GPRIA-PCB                         
000957                                   PRIS-GPRIB-PCB                         
000958                                   PRIS-COST-WDK6-PCB                     
000959                                   PRIS-COST-WDK7-PCB                     
000960                                   PRIS-COST-WDF1-PCB                     
000961                                   PRIS-COST-9305-PCB                     
000962                                   PRIS-COST-WDK72-PCB                    
000963                                   PRIS-COST-WDB6-PCB                     
000964                                   PLATS-DM-PCB                           
000965                                   PLATS-DN-PCB                           
000966                                   PLATS-DP-PCB                           
000967                                   PLATS-DO-PCB                           
000968                                   PLATS-WDE6C-PCB                        
000969                                   PLATS-GMTC-PCB                         
000970                                   PLATS-WDB6-PCB                         
000971                                   KOM-WDP8-PCB.                          
000972                                                                          
000973     PERFORM A-INIT                                                       
000974     IF MESSAGE-OK                                                        
000975     PERFORM B-KOLLA-NYCKLAR                                              
000976                                                                          
000977     IF NYCKLAR-OK                                                        
000978                                                                          
000979       IF MID-KDTRPINF = JA                                               
000980          OR = NEJ OR = KODL                                              
000981         PERFORM IMS-GU-WDE101                                            
000982         PERFORM IMS-GHNP-WDE111                                          
000983         MOVE SGMT-IDDISTR          TO TEST-IDDISTR                       
000984*        IF DIS107-BYGG-SHIP                                              
000985*            FÅR EJ FÖRÄNDRA MID-KDTRPINF                                 
000986*          MOVE JA                  TO MID-KDTRPINF                       
000987*        END-IF                                                           
000988       END-IF                                                             
000989       PERFORM IMS-GU-WDE201                                              
000990       PERFORM IMS-GHNP-WDE211                                            
000991       IF BGMT-IDKUNDNR = ZERO AND SGMT-IDKUNDNR NOT = ZERO               
000992         MOVE BGMT-WDE211   TO SPAR-BGMT-WDE211                           
000993         PERFORM IMS-GHNP-WDE211                                          
000994         IF BGMT-IDKUNDNR NOT = SGMT-IDKUNDNR                             
000995           MOVE 'WDE2-WDE1 I OTAKT'  TO FELTEXT                           
000996           CALL FELLOG                                                    
000997         END-IF                                                           
000998         MOVE SGMT-IDDISTR          TO X-IDDISTR                          
000999         MOVE SGMT-IDKUNDNR         TO X-IDKUNDNR                         
001000       END-IF                                                             
001001       IF ATERSTART                                                       
001002         IF MID-KDTRPINF = JA OR NEJ OR KODL                              
001003           OR DIS107-BYGG-SHIP                                            
001004           PERFORM IMS-GHNP-WDE111-KVAL                                   
001005         END-IF                                                           
001006         PERFORM IMS-GHNP-WDE211-KVAL                                     
001007       END-IF                                                             
001008*211                                                                      
001009       PERFORM UNTIL SEGMENT-SAKNAS OR OMSTART                            
001010         PERFORM C-LAS-WDB3-B2-B1-B6                                      
001011         IF NOT ATERSTART                                                 
001012           IF MID-KDTRPINF = JA                                           
001013             OR = NEJ OR KODL                                             
001014             OR DIS107-BYGG-SHIP                                          
001015             PERFORM D-KOMPLETTERA-WDE111                                 
001016           END-IF                                                         
001017           PERFORM E-KOMPLETTERA-WDE211                                   
001018         END-IF                                                           
001019         PERFORM C-SOFTWARE                                               
001020                                                                          
001021         IF ATERSTART                                                     
001022           IF MID-KDTRPINF = JA OR NEJ OR KODL                            
001023             OR DIS107-BYGG-SHIP                                          
001024             PERFORM IMS-GHNP-WDE121-KVAL                                 
001025           END-IF                                                         
001026           PERFORM IMS-GHNP-WDE221-KVAL                                   
001027           MOVE NEJ                   TO ATERSTART-SW                     
001028         ELSE                                                             
001029           IF MID-KDTRPINF = JA                                           
001030             OR = NEJ OR KODL                                             
001031             OR DIS107-BYGG-SHIP                                          
001032             PERFORM IMS-GHNP-WDE121                                      
001033           END-IF                                                         
001034           PERFORM IMS-GHNP-WDE221                                        
001035*SW-INITIALIZE-TOT WILL BE N IN CASE OF CHANGE OF DIST OR CUST,           
001036*BECAUSE THE ACCUMULATED CASE TOTALS SHOULD BE RETAINED                   
001037           IF SW-INITIALIZE-TOT = JA                                      
001038              INITIALIZE WS-CASE-TOTALS                                   
001039                         WS-CASE-KDVALISO-DGFLAG                          
001040              MOVE NEJ                   TO REFILL-FLOW-SW                
001041           END-IF                                                         
001042         END-IF                                                           
001043                                                                          
001044         IF SEGMENT-FINNS                                                 
001045           ADD +1                     TO W-CHKP                           
001046           IF W-CHKP NOT < MAX-CHKP                                       
001047             MOVE JA                  TO OMSTART-SW                       
001048           END-IF                                                         
001049         END-IF                                                           
001050                                                                          
001051         PERFORM S13-SET-AVGCOST-FLAG                                     
001052*221                                                                      
001053         PERFORM UNTIL SEGMENT-SAKNAS OR OMSTART                          
001054           PERFORM F-LAS-WDE6-E4                                          
001055           IF MID-KDTRPINF = JA                                           
001056             OR = NEJ OR KODL                                             
001057             OR DIS107-BYGG-SHIP                                          
001058             PERFORM F-KOMPLETTERA-WDE121                                 
001059           END-IF                                                         
001060           PERFORM G-KOMPLETTERA-WDE221                                   
001061           MOVE ZERO                  TO WS-SUORDV-LOC                    
001062                                         WS-SUORDV-LOCPREL                
001063                                         WS-SUORDV-BEST-LOC               
001064                                         WS-SUORDV-BEST-LOCPREL           
001065                                         WS-SUORDV-LEV-LOC                
001066                                         WS-SUORDV-LEV-LOCPREL            
001067           MOVE W-IDDISTR             TO TEST-IDDISTR                     
001068           MOVE JA                    TO KOLLI-SW                         
001069           MOVE NEJ                   TO WDE4-SW                          
001070           MOVE SPACE                 TO W-SVAR                           
001071*231                                                                      
001072          IF MID-KDTRPINF NOT = KODL                                      
001073           MOVE E4-STATUS             TO STATUS-WS                        
001074           PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                   
001075             PERFORM IMS-GNP-WDE421                                       
001076                                                                          
001077             PERFORM H-LAS-WDK6                                           
001078             IF DIST79-DEALER-PRICE                                       
001079               PERFORM K-KOLLA-PRISFRAGA                                  
001080             ELSE                                                         
001081               MOVE ' '           TO LPRQ-KDPRSTA                         
001082             END-IF                                                       
001083                                                                          
001084*-- ALL DELIVERIES FROM DC 87 (INCL.THE AUTO.SHIP.THAT HAVE               
001085*-- MID-KDTRPINF = N) MUST HAVE SEG. WDE131 IN ORDER TO BE                
001086*-- ABLE TO CREATE THE TRACKING NO. SEG. WDE141 WHICH IS DONE             
001087*-- IN THE NEXT STEP IN PGM. W4063700.                                    
001088*-- SAME LOGIC IS VALID FOR DELIVERIES FROM DC. 53 (MEXICO)               
001089*                                                                         
001090             IF MID-KDTRPINF = JA      OR                                 
001091                DIS107-BYGG-SHIP       OR                                 
001092                SHIP-IDDC = WC-NDC-AE  OR                                 
001093                SHIP-IDDC = WC-NDC-MX                                     
001094               PERFORM I-SKAPA-RADPOSTER-WDE1                             
001095             END-IF                                                       
001096                                                                          
001097             PERFORM J-SKAPA-RADPOSTER-WDE2                               
001098                                                                          
001099*-------------------------------------- KLARMARKERA VOR                   
001100             MOVE VORD-IDDC        TO WS-IDDC                             
001101                                      W-IDDC-B6                           
001102             PERFORM IMS-GU-WDB601                                        
001103                                                                          
001104             IF  VORD-KDORDKL = 0                                         
001105             AND NOT (DCS-NDC OR (DCS-SDC AND                             
001106                                  DCS-CHINA))                             
001107                PERFORM S-SET-VOR-TO-READY                                
001108             END-IF                                                       
001109                                                                          
001110             IF KORD-FLLSBOK = JA                                         
001111               IF SHIP-KDFAKSTA-EXP = 0 OR SPACE                          
001112                 PERFORM N-SKAPA-SALDOLOGG                                
001113               END-IF                                                     
001114             END-IF                                                       
001115                                                                          
001116             IF UPDE4-OK                                                  
001117               PERFORM IMS-REPL-WDE411                                    
001118               MOVE NEJ                 TO WDE4-SW                        
001119             END-IF                                                       
001120                                                                          
001121***          HANTERING AV EVENTUELL 'EVENT'                               
001122             PERFORM V-EVENT-HANTERING                                    
001123*                                                                         
001124             PERFORM IMS-GHN-WDE411-FSEQ                                  
001125           END-PERFORM                                                    
001126          END-IF                                                          
001127           PERFORM L-UPPDAT-SUORDV-KOLLI                                  
001128           PERFORM M-UPPDATERA-STATUS-KOLLI                               
001129*231                                                                      
001130           IF MID-KDTRPINF = JA                                           
001131             OR = NEJ OR KODL                                             
001132             OR DIS107-BYGG-SHIP                                          
001133             PERFORM IMS-GHNP-WDE121                                      
001134           END-IF                                                         
001135           PERFORM IMS-GHNP-WDE221                                        
001136           IF SEGMENT-FINNS                                               
001137             ADD +1                    TO W-CHKP                          
001138             IF W-CHKP NOT < MAX-CHKP                                     
001139               MOVE JA                 TO OMSTART-SW                      
001140             END-IF                                                       
001141           END-IF                                                         
001142         END-PERFORM                                                      
001143                                                                          
001144*IN CASE OF RESTART, WE UPDATE THE CASE TOTALS CALCULATED TILL            
001145*THIS POINT IN WDE1                                                       
001146         IF OMSTART AND                                                   
001147           (MID-KDTRPINF = JA                                             
001148         OR MID-KDTRPINF = NEJ                                            
001149         OR MID-KDTRPINF = KODL                                           
001150         OR DIS107-BYGG-SHIP)                                             
001151            PERFORM S12-UPD-TOTAL-WDE101                                  
001152         END-IF                                                           
001153*221                                                                      
001154         IF NOT OMSTART                                                   
001155          IF DIST79-DEALER-PRICE                                          
001156            COMPUTE W-SUORDV-TOT = W-SUORDV-DIST-LOC +                    
001157                                   W-SUORDV-DIST-LOCPREL                  
001158          ELSE                                                            
001159            IF DIST79-ECOM-PRICE                                          
001160              MOVE W-SUORDV-DIST-LOC    TO W-SUORDV-TOT                   
001161            ELSE                                                          
001162              MOVE W-SUORDV-DIST        TO W-SUORDV-TOT                   
001163            END-IF                                                        
001164          END-IF                                                          
001165          MOVE BGMT-IDDISTR             TO W-IDDISTR                      
001166          MOVE BGMT-IDKUNDNR            TO W-IDKUNDNR                     
001167          PERFORM IMS-GHU-WDE211X                                         
001168          IF SEGMENT-FINNS                                                
001169            PERFORM S06-RAKNA-TILLAGG-AVDRAG                              
001170            PERFORM IMS-REPL-WDE211X                                      
001171            PERFORM S07-TILLAGG-WDE122                                    
001172          END-IF                                                          
001173******                                                                    
001174                                                                          
001175          IF MID-KDTRPINF = JA                                            
001176            OR = NEJ OR KODL                                              
001177            OR DIS107-BYGG-SHIP                                           
001178            PERFORM P-ORDKL-WDE111X                                       
001179            PERFORM IMS-GHNP-WDE111                                       
001180          END-IF                                                          
001181          PERFORM IMS-GHNP-WDE211                                         
001182          IF SEGMENT-FINNS                                                
001183           IF BGMT-IDKUNDNR = ZERO AND SGMT-IDKUNDNR NOT = ZERO           
001184             IF SPAR-BGMT-IDDISTR NOT = ZERO                              
001185               PERFORM R-TILLAGG-NOLL                                     
001186             END-IF                                                       
001187             MOVE BGMT-WDE211   TO SPAR-BGMT-WDE211                       
001188             MOVE ZERO               TO W-SUORDV-NOLL                     
001189             MOVE ZERO               TO W-SUORDV-NOLL-LOC                 
001190             MOVE ZERO               TO W-SUORDV-NOLL-LOCPREL             
001191             PERFORM IMS-GHNP-WDE211                                      
001192             IF BGMT-IDKUNDNR NOT = SGMT-IDKUNDNR                         
001193               MOVE 'WDE2-WDE1 I OTAKT-2'  TO FELTEXT                     
001194               CALL FELLOG                                                
001195             END-IF                                                       
001196           END-IF                                                         
001197          END-IF                                                          
001198                                                                          
001199          IF (BGMT-IDDISTR NOT = W-IDDISTR) OR                            
001200             (BGMT-IDKUNDNR NOT = W-IDKUNDNR)                             
001201             MOVE NEJ TO SW-INITIALIZE-TOT                                
001202          END-IF                                                          
001203                                                                          
001204          MOVE BGMT-IDDISTR             TO W-IDDISTR                      
001205          MOVE BGMT-IDKUNDNR            TO W-IDKUNDNR                     
001206         END-IF                                                           
001207       END-PERFORM                                                        
001208*211                                                                      
001209       IF NOT OMSTART                                                     
001210         IF SPAR-BGMT-IDDISTR NOT = ZERO                                  
001211           PERFORM R-TILLAGG-NOLL                                         
001212         END-IF                                                           
001213                                                                          
001214         IF MID-KDTRPINF = JA                                             
001215           OR = NEJ OR KODL                                               
001216           OR DIS107-BYGG-SHIP                                            
001217           PERFORM Q-AVSLUTA-WDE101                                       
001218         END-IF                                                           
001219       END-IF                                                             
001220                                                                          
001221     ELSE                                                                 
001222        MOVE 'SEGMENT SAKNAS'   TO FELTEXT                                
001223        CALL ABEND USING RKOD-ABEND-MED-DUMP                              
001224     END-IF                                                               
001225                                                                          
001226     IF OMSTART                                                           
001227       PERFORM X-STARTA-OM-4636                                           
001228     ELSE                                                                 
001229       PERFORM Z-FINIT                                                    
001230     END-IF                                                               
001231     END-IF                                                               
001232                                                                          
001233     MOVE ZERO TO RETURN-CODE                                             
001234     GOBACK                                                               
001235     .                                                                    
001236     EJECT                                                                
001237 A-INIT SECTION.                                                          
001238     MOVE 'A-INIT'              TO WS-SEKTION                             
001239                                                                          
001240     PERFORM S11-RECV-OPEN                                                
001241     IF MESSAGE-OK                                                        
001242       PERFORM S11-RECV-MESSAGE                                           
001243       PERFORM S11-RECV-CLOSE                                             
001244     END-IF                                                               
001245                                                                          
001246     MOVE NEJ                   TO SW-LYNK-NON-API                        
001247                                   SW-VOR                                 
001248                                                                          
001249     ACCEPT DAGENS-DATUM        FROM DATE                                 
001250     MOVE DAGENS-DATUM          TO WS-AAMMDD                              
001251     MOVE DAGENS-DATUM(1:2)     TO W-DATE-AAMM(1:2)                       
001252     MOVE DAGENS-DATUM(3:2)     TO W-DATE-AAMM(3:2)                       
001253     ACCEPT WS-TIKLOCK          FROM TIME                                 
001254     ACCEPT WS-DAGENS-DATUM     FROM DATE                                 
001255     ACCEPT WS-VOR-TID-KLAR     FROM TIME                                 
001256     MOVE ZERO                  TO GMT-IDDISTR                            
001257                                   GMT-IDKUNDNR                           
001258                                   SGMT-IDKUNDNR                          
001259                                   SPAR-BGMT-IDKUNDNR                     
001260     MOVE ZERO                  TO SPAR-BGMT-IDDISTR                      
001261     MOVE SPACE                 TO GMT-IDPARTNR                           
001262                                   GMT-FLCOD                              
001263                                   W-IDBUNDLE-C7                          
001264     MOVE ZERO                  TO GMT-IDFTG                              
001265     MOVE HIGH-VALUE            TO W-IDPLKLST-MAXE4-X                     
001266     MOVE LOW-VALUE             TO W-IDPLKLST-MINE4-X                     
001267                                                                          
001268     MOVE W-DATE-AAMM           TO CURR-TIAAMM                            
001269     MOVE WS-KDVALISO-HUV       TO CURR-KDVALISO-HUV                      
001270     MOVE 'M'                   TO CURR-KDVALTYP                          
001271*                                                                         
001272     MOVE SPACE                 TO EVENT-SW                               
001273*                                                                         
001274*    -- INITIALIZE W006KOM AREAS WITH FIXED VALUES                        
001275*    -- FIELDS WITH VARYING CONTENT ARE SET LATER                         
001276     MOVE SPACE                      TO MSG-KOM-WMSGKOM                   
001277     MOVE LENGTH OF MSG-KOM-WMSGKOM  TO MSG-KOM-KVLL                      
001278     MOVE LOW-VALUE                  TO MSG-KOM-KDZ1                      
001279     MOVE LOW-VALUE                  TO MSG-KOM-KDZ2                      
001280     MOVE SPACE                      TO MSG-KOM-KDTRANS                   
001281     MOVE 'W4063600'                 TO MSG-KOM-IDSNDJOB                  
001282                                                                          
001283     MOVE FUNCTION CURRENT-DATE(3:6) TO MSG-KOM-TIREGDAT                  
001284*    -- THIS IS THE START VALUE                                           
001285     MOVE FUNCTION CURRENT-DATE(9:8) TO MSG-KOM-TIKLOCK                   
001286                                                                          
001287*    -- INITIALIZE TARGET TRANSACTION AREA WITH FIXED VALUES              
001288     MOVE LOW-VALUE                  TO MSG-KDZ1                          
001289     MOVE LOW-VALUE                  TO MSG-KDZ2                          
001290     .                                                                    
001291     EJECT                                                                
001292 B-KOLLA-NYCKLAR SECTION.                                                 
001293     MOVE 'B-KOLLA-NYCKLAR'     TO WS-SEKTION                             
001294                                                                          
001295                                                                          
001296     MOVE JA TO NYCKLAR-SW                                                
001297                                                                          
001298     IF MID-IDSHIPM         = ALL '+'                                     
001299       MOVE NEJ TO NYCKLAR-SW                                             
001300     ELSE                                                                 
001301       IF MID-IDSHIPM NUMERIC                                             
001302         MOVE MID-IDSHIPM     TO W-IDSHIPM                                
001303       ELSE                                                               
001304         IF MID-KDTRPINF = ALL '+'                                        
001305           MOVE NEJ           TO MID-KDTRPINF                             
001306         END-IF                                                           
001307         IF MID-KDTRPINF = JA OR NEJ OR KODP OR KODL                      
001308           CONTINUE                                                       
001309         ELSE                                                             
001310           MOVE NEJ TO NYCKLAR-SW                                         
001311         END-IF                                                           
001312       END-IF                                                             
001313     END-IF                                                               
001314                                                                          
001315     IF MID-IDDISTR NOT = ZERO                                            
001316       MOVE JA                    TO ATERSTART-SW                         
001317       MOVE MID-IDDISTR           TO W-IDDISTR                            
001318       MOVE MID-IDKUNDNR          TO W-IDKUNDNR                           
001319       MOVE MID-IDPRODNR          TO W-IDPRODNR                           
001320       MOVE MID-IDKOLLI           TO W-IDKOLLI                            
001321       MOVE MID-SUORDV-DIST       TO W-SUORDV-DIST                        
001322       MOVE MID-SUORDV-DIST-LOC   TO W-SUORDV-DIST-LOC                    
001323       MOVE MID-SUORDV-DIST-PREL  TO W-SUORDV-DIST-LOCPREL                
001324       MOVE MID-SUORDV-NOLL       TO W-SUORDV-NOLL                        
001325       MOVE MID-SUORDV-NOLL-LOC   TO W-SUORDV-NOLL-LOC                    
001326       MOVE MID-SUORDV-NOLL-PREL  TO W-SUORDV-NOLL-LOCPREL                
001327       MOVE MID-KDORDKL           TO WS-KDORDKL-MAX                       
001328     END-IF                                                               
001329                                                                          
001330     IF NYCKLAR-FEL                                                       
001331       STRING 'SKEPPN.NR SAKNAS '                                         
001332            DELIMITED BY SIZE INTO FELTEXT                                
001333       CALL FELLOG                                                        
001334                                                                          
001335     END-IF                                                               
001336     .                                                                    
001337     EJECT                                                                
001338 C-LAS-WDB3-B2-B1-B6 SECTION.                                             
001339     MOVE 'C-LAS-WDB3-B2-B1-B6' TO WS-SEKTION.                            
001340                                                                          
001341     MOVE BGMT-IDDISTR         TO W-IDDISTR                               
001342                                  W-IDDISTR-B3                            
001343                                  W-IDDISTR-B3-DEF                        
001344                                  TEST-IDDISTR                            
001345     MOVE BGMT-IDKUNDNR        TO W-IDKUNDNR                              
001346                                  W-IDKUNDNR-B3                           
001347     MOVE BILL-IDDC            TO W-IDDC-B3                               
001348                                  W-IDDC-B3-DEF                           
001349     PERFORM IMS-GU-WDB301                                                
001350     IF SEGMENT-SAKNAS                                                    
001351        MOVE ZERO              TO DC-KDFORSKN                             
001352                                  DC-KDSPFKTK                             
001353                                  DC-KDMOMSIN                             
001354        MOVE BILL-IDDC         TO WC-IDDC                                 
001355                                  WS-IDDC                                 
001356        IF WS-IDDC NOT = W-IDDC-B6                                        
001357           MOVE WS-IDDC TO W-IDDC-B6                                      
001358           PERFORM IMS-GU-WDB601                                          
001359        END-IF                                                            
001360                                                                          
001361        IF DCS-DDC                                                        
001362          MOVE WC-CDC-SE       TO W-IDDC-B3                               
001363                                  W-IDDC-B3-DEF                           
001364          PERFORM IMS-GU-WDB301                                           
001365        END-IF                                                            
001366        PERFORM S20-KDMOMSIN                                              
001367     ELSE                                                                 
001368        MOVE DC-IDDC           TO WC-IDDC                                 
001369        MOVE DC-KDMOMSIN       TO WC-KDMOMSIN                             
001370     END-IF                                                               
001371     IF BGMT-IDDISTR  = GMT-IDDISTR  AND                                  
001372        BGMT-IDKUNDNR = GMT-IDKUNDNR AND                                  
001373        GMT-IDDISTR NOT = ZERO                                            
001374        CONTINUE                                                          
001375     ELSE                                                                 
001376        MOVE BGMT-IDDISTR       TO W-IDDISTR-WDB2                         
001377                                   TEST-IDDISTR                           
001378        MOVE BGMT-IDKUNDNR      TO W-IDKUNDNR-WDB2                        
001379        PERFORM IMS-GU-WDB201                                             
001380        IF SEGMENT-SAKNAS                                                 
001381          IF BGMT-IDKUNDNR = ZERO                                         
001382            MOVE SPACE          TO GMT-IDPARTNR                           
001383            MOVE ZERO           TO GMT-IDFTG                              
001384            MOVE SPACE          TO GMT-FLCOD                              
001385            MOVE ZERO           TO GMT-REAVDRAG                           
001386                                   GMT-REEMBHNT                           
001387          ELSE                                                            
001388            MOVE 'KUND SAKNAS I WDB2' TO FELTEXT                          
001389            CALL FELLOG                                                   
001390          END-IF                                                          
001391        END-IF                                                            
001392        IF GMT-IDPARTNR NOT = SPACE                                       
001393          MOVE GMT-IDPARTNR       TO W-WDB1-IDPARTNR                      
001394          MOVE GMT-IDFTG          TO W-WDB1-IDFTG                         
001395*                                                                         
001396*---      FÖR REFILL-STUDSDISTRIKTEN I FÖRSTA FLÖDET                      
001397*---      GÄLLER: PARMAID'T FRÅN STUDS DC'T -> IDDC=11                    
001398*---      OCH FÖRETAGSKODEN FRÅN SÄNDANDE DC'T (FTG EJ=57)                
001399*---      MEN: IDKUNDNR = 999 --> EJ STUDS                                
001400*                                                                         
001401*---      FÖR FLÖDET MELLAN ETT NDC (EJ FTG=57) TILL                      
001402*---      EN IMPORTÖR (FTG=57) BLIR DET OCKSÅ STUDSFLÖDE:                 
001403*---      I FÖRSTA FLÖDET GÄLLER OCKSÅ ATT PARMAID'T ÄR                   
001404*---      FRÅN STUDS DC'T -> IDDC=11 OCH FÖRETAGSKODEN                    
001405*---      FRÅN SÄNDANDE DC'T                                              
001406*---                                                                      
001407          IF (DIST35-NONVCC-NONVCC-REFILL     AND                         
001408              BGMT-IDKUNDNR NOT = WS-IDKUNDNR-999)                        
001409                            OR                                            
001410             (SHIP-IDDC-EXP = WC-CDC-SE AND                               
001411              GMT-KDKUNDKAT = WS-IMPORTER)                                
001412            MOVE WC-CDC-SE        TO W-IDDC-B6                            
001413            PERFORM IMS-GU-WDB601                                         
001414            MOVE DCS-IDPARTNR     TO W-WDB1-IDPARTNR                      
001415                                     WS-IDPARTNR-EXP                      
001416                                                                          
001417            MOVE WC-IDDC          TO W-IDDC-B6                            
001418            PERFORM IMS-GU-WDB601                                         
001419            MOVE DCS-IDFTG        TO W-WDB1-IDFTG                         
001420            MOVE DCS-KDVALISO     TO FAKT-KDVALISO-DC                     
001421          END-IF                                                          
001422*                                                                         
001423*---      FÖR VOR FLÖDET, DC 11 --> ETT ANNAT FÖRETAG                     
001424*         (T.EX. KINA, INDIEN) BLIR FLÖDET ETT STUDSFLÖDE.                
001425*         DÅ GÄLLER SAMMA REGLER SOM FÖR REFILL GE:                       
001426*         FÖR STUDSDISTRIKTEN I FÖRSTA FLÖDET GÄLLER                      
001427*         PARMAID'T FRÅN STUDS DC'T -> IDDC=XX OCH                        
001428*         FÖRETAGSKODEN FRÅN SÄNDANDE DC'T                                
001429*                                                                         
001430          IF BILL-KDFINDOC = 'PROF' OR MID-KDTRPINF = KODL                
001431            CONTINUE                                                      
001432          ELSE                                                            
001433            IF SHIP-IDDC-EXP NOT = SPACE                                  
001434              IF SHIP-IDDC-EXP NOT = WC-CDC-SE                            
001435                MOVE SHIP-IDDC-EXP TO W-IDDC-B6                           
001436                PERFORM IMS-GU-WDB601                                     
001437                MOVE DCS-IDPARTNR TO W-WDB1-IDPARTNR                      
001438                                       WS-IDPARTNR-EXP                    
001439                                                                          
001440                MOVE WC-IDDC      TO W-IDDC-B6                            
001441                PERFORM IMS-GU-WDB601                                     
001442                MOVE DCS-IDFTG    TO W-WDB1-IDFTG                         
001443                MOVE DCS-KDVALISO TO FAKT-KDVALISO-DC                     
001444              END-IF                                                      
001445            END-IF                                                        
001446          END-IF                                                          
001447*                                                                         
001448          PERFORM IMS-GU-WDB101                                           
001449          IF SEGMENT-FINNS                                                
001450            MOVE BET-KDVALISO        TO CURR-KDVALISO-ROW                 
001451*---                                                                      
001452*           VALUTAKOD FRÅN SÄNDANDE DC ('STUDS' FLÖDE) GÄLLER:            
001453*           *STUDSREFILLDISTRIKTEN                                        
001454*           *NDC (EJ FTG=57) -> IMPORTÖR                                  
001455*---                                                                      
001456            IF (DIST35-NONVCC-NONVCC-REFILL     AND                       
001457                BGMT-IDKUNDNR NOT = WS-IDKUNDNR-999)                      
001458                             OR                                           
001459               (SHIP-IDDC-EXP = WC-CDC-SE AND                             
001460                GMT-KDKUNDKAT = WS-IMPORTER)                              
001461              MOVE FAKT-KDVALISO-DC  TO FAKT-KDVALISO                     
001462                                        CURR-KDVALISO-ROW                 
001463            END-IF                                                        
001464            MOVE BET-IDMARKBO        TO WS-IDMARKBO                       
001465            CALL W510CURR USING CURR-W510CURR WDG2-PCB                    
001466            IF CURR-KDSVAR = ' '                                          
001467              CONTINUE                                                    
001468            ELSE                                                          
001469              MOVE 1                 TO CURR-PRKURS-NEW                   
001470            END-IF                                                        
001471            MOVE CURR-PRKURS-NEW     TO FAKT-PRKURS                       
001472*---                                                                      
001473*           FÖR VOR 'STUDS'FLÖDE STÄMMER 'SEK' I KDVALISO                 
001474*           EFTERSOM SÄNDANDE DC:T ÄR = 11                                
001475*           OBS--> ATT REVIDERA OM VOR FRÅN ANNAT DC <--OBS               
001476*---                                                                      
001477*---        OM NDC (EJ FTG=57) --> IMPORTÖR GÄLLER SÄNDANDE               
001478*---        VALUTA FRÅN SÄNDANDE DC (REDAN INITIERAT)                     
001479*---                                                                      
001480            IF NOT DIST79-DEALER-PRICE AND                                
001481               NOT DIST79-ECOM-PRICE                                      
001482              IF NOT DIST35-NONVCC-NONVCC-REFILL                          
001483                IF (SHIP-IDDC-EXP = WC-CDC-SE AND                         
001484                    GMT-KDKUNDKAT = WS-IMPORTER)                          
001485                  CONTINUE                                                
001486                ELSE                                                      
001487                  MOVE 'SEK'           TO FAKT-KDVALISO                   
001488                  MOVE 1.0             TO FAKT-PRKURS                     
001489                END-IF                                                    
001490              END-IF                                                      
001491                                                                          
001492*             SPECIELLT FALL FÖR TILLÄGGSFAKT.->EXPORT REF.DISTR.         
001493              IF DIST35-NONVCC-NONVCC-REFILL    AND                       
001494                 BGMT-IDKUNDNR = WS-IDKUNDNR-999                          
001495                MOVE 'SEK'           TO FAKT-KDVALISO                     
001496                MOVE 1.0             TO FAKT-PRKURS                       
001497              END-IF                                                      
001498            END-IF                                                        
001499          END-IF                                                          
001500        END-IF                                                            
001501     END-IF                                                               
001502     .                                                                    
001503     EJECT                                                                
001504 C-SOFTWARE  SECTION.                                                     
001505     MOVE NEJ                     TO SOFT-SPEC-SW                         
001506     MOVE SGMT-IDDISTR            TO TEST-IDDISTR                         
001507     IF DIST42-SOFT AND                                                   
001508        (SHIP-IDLBBET = 'SOFTAUTF' OR 'SOFTW   ')                         
001509        MOVE JA                   TO SOFT-SPEC-SW                         
001510     END-IF                                                               
001511     .                                                                    
001512     EJECT                                                                
001513                                                                          
001514 D-KOMPLETTERA-WDE111   SECTION.                                          
001515     MOVE 'D-KOMPLETTERA-WDE111'  TO WS-SEKTION                           
001516                                                                          
001517     MOVE SGMT-IDDISTR            TO TEST-IDDISTR                         
001518     IF DIS105-NA-BYT                                                     
001519       IF BILL-IDDC = WC-NDC-CA                                           
001520         MOVE '112029   '         TO GMT-IDPARTNR                         
001521       END-IF                                                             
001522     END-IF                                                               
001523     MOVE DC-KDFORSKN             TO SGMT-KDFORSKN                        
001524     MOVE DC-KDSPFKTK             TO SGMT-KDFKBIL                         
001525     MOVE GMT-IDPARTNR            TO SGMT-IDPARTNR                        
001526*                                                                         
001527*--- STUDS:REFILLDISTRIKT                                                 
001528*    MEN IDKUNDNR = 999 --> EJ STUDS (TILLÄGGSFAKT.KUND)                  
001529*--- STUDS:NDC (EJ FTG 57) --> IMPORTÖR                                   
001530*                                                                         
001531     IF (DIST35-NONVCC-NONVCC-REFILL     AND                              
001532         SGMT-IDKUNDNR NOT = WS-IDKUNDNR-999)                             
001533                  OR                                                      
001534        (SHIP-IDDC-EXP = WC-CDC-SE AND                                    
001535         GMT-KDKUNDKAT = WS-IMPORTER)                                     
001536                                                                          
001537       MOVE WS-IDPARTNR-EXP       TO SGMT-IDPARTNR                        
001538     END-IF                                                               
001539*                                                                         
001540*--- VOR FRÅN DC 11                                                       
001541     IF BILL-KDFINDOC = 'PROF' OR MID-KDTRPINF = KODL                     
001542       CONTINUE                                                           
001543     ELSE                                                                 
001544       IF SHIP-IDDC-EXP NOT = SPACE                                       
001545         IF SHIP-IDDC-EXP NOT = WC-CDC-SE                                 
001546           MOVE WS-IDPARTNR-EXP   TO SGMT-IDPARTNR                        
001547         END-IF                                                           
001548       END-IF                                                             
001549     END-IF                                                               
001550*                                                                         
001551     MOVE GMT-FLCOD               TO SGMT-FLCOD                           
001552     IF DIST79-DEALER-PRICE OR                                            
001553        DIST79-ECOM-PRICE                                                 
001554       MOVE BET-KDVALISO          TO SGMT-KDVALISO                        
001555       MOVE CURR-PRKURS-NEW       TO SGMT-PRKURS                          
001556     ELSE                                                                 
001557*      MOVE BET-KDVALISO          TO SGMT-KDVALISO                        
001558       MOVE FAKT-KDVALISO         TO SGMT-KDVALISO                        
001559       MOVE FAKT-PRKURS           TO SGMT-PRKURS                          
001560                                     KUND-PRKURS                          
001561     END-IF                                                               
001562     IF SGMT-KDVALISO NOT = CURR-KDVALISO-ROW                             
001563       MOVE SGMT-KDVALISO         TO CURR-KDVALISO-ROW                    
001564       CALL W510CURR USING CURR-W510CURR WDG2-PCB                         
001565       IF CURR-KDSVAR = ' '                                               
001566         CONTINUE                                                         
001567       ELSE                                                               
001568         MOVE 1                   TO CURR-PRKURS-NEW                      
001569       END-IF                                                             
001570       MOVE CURR-PRKURS-NEW       TO SGMT-PRKURS                          
001571     END-IF                                                               
001572     IF SGMT-KDVALISO = BET-KDVALISO                                      
001573       MOVE CURR-PRKURS-NEW       TO KUND-PRKURS                          
001574     END-IF                                                               
001575*  PRIS SKA RÄKNAS OM FÖR USA, TAIWAN OCH THAILAND                        
001576                                                                          
001577     MOVE SGMT-KDORDKL-MAX        TO WS-KDORDKL-MAX                       
001578     IF MID-KDTRPINF = JA                                                 
001579       OR = NEJ OR KODL                                                   
001580       OR DIS107-BYGG-SHIP                                                
001581        PERFORM IMS-REPL-WDE111                                           
001582     END-IF                                                               
001583     .                                                                    
001584     EJECT                                                                
001585 E-KOMPLETTERA-WDE211   SECTION.                                          
001586     MOVE 'E-KOMPLETTERA-WDE211'  TO WS-SEKTION.                          
001587                                                                          
001588     MOVE ZERO                    TO W-SUORDV-DIST                        
001589                                     W-SUORDV-DIST-LOC                    
001590                                     W-SUORDV-DIST-LOCPREL                
001591                                                                          
001592     MOVE BGMT-IDDISTR            TO TEST-IDDISTR                         
001593     IF DIS105-NA-BYT                                                     
001594       IF BILL-IDDC = WC-NDC-CA                                           
001595         MOVE '112029   '         TO GMT-IDPARTNR                         
001596       END-IF                                                             
001597     END-IF                                                               
001598     MOVE GMT-IDPARTNR            TO BGMT-IDPARTNR                        
001599*                                                                         
001600*--- STUDS:REFILLDISTRIKT                                                 
001601*    MEN IDKUNDNR = 999 --> EJ STUDS (TILLÄGGSFAKT.KUND)                  
001602*--- STUDS:NDC (EJ FTG 57) --> IMPORTÖR                                   
001603*                                                                         
001604     IF (DIST35-NONVCC-NONVCC-REFILL     AND                              
001605         BGMT-IDKUNDNR NOT = WS-IDKUNDNR-999)                             
001606                  OR                                                      
001607        (SHIP-IDDC-EXP = WC-CDC-SE AND                                    
001608         GMT-KDKUNDKAT = WS-IMPORTER)                                     
001609                                                                          
001610       MOVE WS-IDPARTNR-EXP       TO BGMT-IDPARTNR                        
001611     END-IF                                                               
001612*                                                                         
001613*--- VOR FRÅN DC 11                                                       
001614     IF BILL-KDFINDOC = 'PROF' OR MID-KDTRPINF = KODL                     
001615       CONTINUE                                                           
001616     ELSE                                                                 
001617       IF SHIP-IDDC-EXP NOT = SPACE                                       
001618         IF SHIP-IDDC-EXP NOT = WC-CDC-SE                                 
001619           MOVE WS-IDPARTNR-EXP   TO BGMT-IDPARTNR                        
001620         END-IF                                                           
001621       END-IF                                                             
001622     END-IF                                                               
001623*                                                                         
001624     MOVE GMT-FLCOD               TO BGMT-FLCOD                           
001625     PERFORM IMS-REPL-WDE211                                              
001626     .                                                                    
001627     EJECT                                                                
001628 F-LAS-WDE6-E4    SECTION.                                                
001629     MOVE 'F-LAS-WDE6-E4'         TO WS-SEKTION.                          
001630                                                                          
001631     MOVE BKOLLI-IDPRODNR         TO W-IDPRODNR-E6                        
001632                                     W-IDPRODNR                           
001633     MOVE BKOLLI-IDKOLLI          TO W-IDKOLLI-E6                         
001634                                     W-IDKOLLI                            
001635     PERFORM IMS-GHU-WDE601                                               
001636*                                                                         
001637     IF VORD-IDDC-EXP > SPACE                                             
001638       MOVE VORD-IDDC-EXP         TO WS-IDDC-EXP                          
001639       MOVE JA                    TO UPPDAT-IDDC-EXP-SW                   
001640     END-IF                                                               
001641*                                                                         
001642     IF DIST35-NDCCN-NONVCC-REFILL OR                                     
001643        DIST35-NDCUS-NONVCC-REFILL OR                                     
001644        DIST35-NDCKR-NDC-REFILL    OR                                     
001645        DIST35-NDCIN-NDC-REFILL    OR                                     
001646        DIST35-NDCMY-NDC-REFILL    OR                                     
001647        DIST35-NDCTH-NDC-REFILL    OR                                     
001648        DIST35-NDCTW-NDC-REFILL                                           
001649       SET REFILL-FLOW            TO TRUE                                 
001650     END-IF                                                               
001651*                                                                         
001652     MOVE WS-AAMMDD               TO VORD-TILASTN-SK                      
001653     IF VORD-KDORDSTA > 2                                                 
001654       IF VORD-KVKOLLI  = VORD-KVKOLLI-FL    AND                          
001655          VORD-KVORDRAD = VORD-KVORDRAD-PACK                              
001656         MOVE +4                    TO VORD-KDORDSTA                      
001657       END-IF                                                             
001658     END-IF                                                               
001659**(IW40634)  ADD +1               TO VORD-KVKOLLI-LAST                    
001660     IF BILL-KDFINDOC = 'PROF' OR MID-KDTRPINF = KODL                     
001661       CONTINUE                                                           
001662     ELSE                                                                 
001663       PERFORM IMS-REPL-WDE601                                            
001664     END-IF                                                               
001665                                                                          
001666     PERFORM IMS-GHNP-WDE611                                              
001667     IF SEGMENT-FINNS                                                     
001668       IF KOLLI-KDKOLSTA > 6                                              
001669         CONTINUE                                                         
001670*        THE CASE IS A CROSS-DOCK AND ALREADY INVOICED                    
001671       ELSE                                                               
001672         MOVE +7                  TO KOLLI-KDKOLSTA                       
001673         MOVE WS-AAMMDD           TO KOLLI-TILASTN                        
001674         MOVE BILL-IDSHIPM        TO KOLLI-IDSHIPM                        
001675         MOVE NEJ                 TO KOLLI-FLUTLAST                       
001676         IF BILL-KDFINDOC = 'PROF' OR MID-KDTRPINF = KODL                 
001677           CONTINUE                                                       
001678         ELSE                                                             
001679           PERFORM IMS-REPL-WDE611                                        
001680         END-IF                                                           
001681       END-IF                                                             
001682*CO-CD                                                                    
001683       IF BILL-KDFINDOC = 'PROF' OR MID-KDTRPINF = KODL                   
001684         CONTINUE                                                         
001685       ELSE                                                               
001686         MOVE SHIP-IDDC             TO W-IDDC-CROSS-X                     
001687         MOVE SHIP-IDTRPTNR         TO W-IDTRPTNR-CROSS                   
001688         PERFORM IMS-GHNP-WDE621                                          
001689         IF SEGMENT-FINNS                                                 
001690           MOVE 9                   TO CROSS-KDKOLSTA-CROSS               
001691           MOVE WS-AAMMDD           TO CROSS-TISKEPPN                     
001692           MOVE W-IDSHIPM           TO CROSS-IDSHIPM-CROSS                
001693           MOVE SHIP-IDLBBET        TO CROSS-IDLBBET-CROSS                
001694           PERFORM IMS-REPL-WDE621                                        
001695         END-IF                                                           
001696       END-IF                                                             
001697     END-IF                                                               
001698                                                                          
001699     PERFORM S05-SKAPA-PLATSAVBOKN                                        
001700     MOVE BKOLLI-IDPRODNR         TO W-IDPRODNR-FSEQ                      
001701     MOVE BKOLLI-IDKOLLI          TO W-IDKOLLI-FSEQ                       
001702     PERFORM IMS-GHU-WDE411-FSEQ                                          
001703     MOVE STATUS-WS               TO E4-STATUS                            
001704                                                                          
001705     PERFORM IMS-GHNP-WDE401                                              
001706     MOVE KORD-IDDISTR            TO W-IDDISTR-E4                         
001707     MOVE KORD-IDKUNDNR           TO W-IDKUNDNR-E4                        
001708     MOVE KORD-IDPRODNR           TO W-IDPRODNR-E4                        
001709     MOVE KORD-IDKUNDRF           TO W-IDKUNDRF-E4                        
001710     MOVE KORD-IDPLKLST           TO W-IDPLKLST-E4                        
001711     .                                                                    
001712     EJECT                                                                
001713 F-KOMPLETTERA-WDE121   SECTION.                                          
001714     MOVE 'F-KOMPLETTERA-WDE121'  TO WS-SEKTION.                          
001715     MOVE VORD-FLDIRLEV        TO SKOLLI-FLDIRLEV                         
001716     MOVE VORD-KDFRAKT         TO SKOLLI-KDFRAKT                          
001717                                                                          
001718     MOVE KOLLI-DIKOLLIB       TO SKOLLI-DIKOLLIB                         
001719     MOVE KOLLI-DIKOLLIH       TO SKOLLI-DIKOLLIH                         
001720     MOVE KOLLI-DIKOLLIL       TO SKOLLI-DIKOLLIL                         
001721     MOVE KOLLI-KDEMBTYP       TO SKOLLI-KDEMBTYP                         
001722     MOVE KOLLI-KDFARLIG-KOLLI                                            
001723                               TO SKOLLI-KDFARLIG-KOLLI                   
001724     MOVE KOLLI-KDKOLLI        TO SKOLLI-KDKOLLI                          
001725     MOVE KOLLI-KVFLAMP-KOLLI                                             
001726                               TO SKOLLI-KVFLAMP-KOLLI                    
001727     MOVE KOLLI-DEAL-PR-SUM    TO SKOLLI-DEAL-PR-SUM                      
001728     IF SKOLLI-KDVALISO = SPACE                                           
001729       IF SKOLLI-SUORDV-LOC = ZERO AND                                    
001730          SKOLLI-SUORDV-LOCPREL = ZERO                                    
001731          CONTINUE                                                        
001732       ELSE                                                               
001733          MOVE FAKT-KDVALISO   TO SKOLLI-KDVALISO                         
001734       END-IF                                                             
001735     END-IF                                                               
001736     MOVE KOLLI-SUORDV-KOLLI   TO SKOLLI-SUORDV                           
001737     MOVE KOLLI-SUORDV-KLI-EXP TO SKOLLI-SUORDV-EXP                       
001738     MOVE KOLLI-KDVALISO-EXP   TO SKOLLI-KDVALISO-EXP                     
001739     IF DIST79-DEALER-PRICE OR                                            
001740        DIST79-ECOM-PRICE                                                 
001741       MOVE ZERO               TO SKOLLI-SUORDV                           
001742     END-IF                                                               
001743     MOVE KOLLI-TIPACKN        TO SKOLLI-TIPACKN                          
001744     MOVE KOLLI-VKORDBTO-KOLLI                                            
001745                               TO SKOLLI-VKORDBTO-KOLLI                   
001746     MOVE KOLLI-VKORDNTO-KOLLI                                            
001747                               TO SKOLLI-VKORDNTO-KOLLI                   
001748     MOVE KOLLI-VLORDBTO-KOLLI                                            
001749                               TO SKOLLI-VLORDBTO-KOLLI                   
001750                                                                          
001751     IF REFILL-FLOW                                                       
001752       PERFORM FA-CALC-CASE-TOTALS                                        
001753     END-IF                                                               
001754                                                                          
001755     MOVE KORD-IDGMTREF        TO SKOLLI-IDGMTREF                         
001756     MOVE KORD-IDORDNR5        TO SKOLLI-IDORDNR7(3:5)                    
001757     MOVE ZERO                 TO SKOLLI-IDORDNR7(1:2)                    
001758     MOVE KORD-IDORDER         TO SKOLLI-IDORDER                          
001759     MOVE KORD-KDFAKTYP        TO SKOLLI-KDFAKTYP                         
001760     MOVE KORD-KDORDKL         TO SKOLLI-KDORDKL                          
001761     IF WS-KDORDKL-MAX = +9  OR                                           
001762          KORD-KDORDKL > WS-KDORDKL-MAX                                   
001763          MOVE KORD-KDORDKL    TO WS-KDORDKL-MAX                          
001764     END-IF                                                               
001765     MOVE KORD-TIORDREG        TO SKOLLI-TIORDREG                         
001766     PERFORM IMS-REPL-WDE121                                              
001767                                                                          
001768     MOVE ZERO                 TO WS-VKORDNTO                             
001769                                                                          
001770     .                                                                    
001771     EJECT                                                                
001772*************************************************************             
001773*FOR REFILL FLOW,ACCUMULATE THE TOTAL ORDER VALUE,TOTAL ORDER             
001774*VALUE BOUNCE,TOTAL WEIGHT,VOLUME FOR EACH CASE.                          
001775*IF ANY CASE HAS DG, SET WS-FLFARLIG-FLAG TO TRUE.                        
001776*AT THE END IN SECTION Q-AVSLUTA-WDE101,UPDATE TOTALS,CURRENCY,           
001777*DG FLAG IN WDE101.                                                       
001778*************************************************************             
001779 FA-CALC-CASE-TOTALS    SECTION.                                          
001780     MOVE 'FA-CALC-CASE-TOTALS'   TO WS-SEKTION.                          
001781                                                                          
001782     MOVE VORD-IDDISTR            TO TEST-IDDISTR                         
001783                                                                          
001784     IF KOLLI-KDFARLIG-KOLLI = 4 OR                                       
001785        KOLLI-KDFARLIG-KOLLI = 7                                          
001786        MOVE JA                   TO WS-FLFARLIG-FLAG                     
001787     END-IF                                                               
001788                                                                          
001789     IF WS-KDVALISO-EXP  = SPACE                                          
001790        MOVE SKOLLI-KDVALISO-EXP  TO WS-KDVALISO-EXP                      
001791     END-IF                                                               
001792                                                                          
001793     COMPUTE WS-SUORDV-FAKT   = WS-SUORDV-FAKT   +                        
001794                                          SKOLLI-SUORDV                   
001795     COMPUTE WS-SUORDV-EXP    = WS-SUORDV-EXP    +                        
001796                                          SKOLLI-SUORDV-EXP               
001797     COMPUTE WS-VKORDBTO-FAKT = WS-VKORDBTO-FAKT +                        
001798                                          SKOLLI-VKORDBTO-KOLLI           
001799     COMPUTE WS-VLORDBTO-FAKT = WS-VLORDBTO-FAKT +                        
001800                                          SKOLLI-VLORDBTO-KOLLI           
001801     .                                                                    
001802     EJECT                                                                
001803 G-KOMPLETTERA-WDE221   SECTION.                                          
001804     MOVE 'G-KOMPLETTERA-WDE221'  TO WS-SEKTION.                          
001805                                                                          
001806     MOVE KORD-IDGMTREF           TO BKOLLI-IDGMTREF                      
001807     MOVE KORD-IDORDNR5           TO BKOLLI-IDORDNR7(3:5)                 
001808     MOVE ZERO                    TO BKOLLI-IDORDNR7(1:2)                 
001809     MOVE KORD-FLOVRLEV           TO BKOLLI-FLOVRLEV                      
001810     MOVE KORD-KDFAKTYP           TO BKOLLI-KDFAKTYP                      
001811     MOVE KORD-KDORDKL            TO BKOLLI-KDORDKL                       
001812     MOVE 'A'                     TO BKOLLI-KDPRSTA                       
001813     MOVE KOLLI-VKORDBTO-KOLLI    TO BKOLLI-VKORDBTO-KOLLI                
001814     MOVE KORD-IDORDER            TO W-IDORDER                            
001815     PERFORM IMS-GU-WDQ201                                                
001816                                                                          
001817     IF SEGMENT-FINNS                                                     
001818       PERFORM S09-CHK-LYN-NON-API                                        
001819       MOVE OHUV-BEKUNDRF         TO BKOLLI-BEKUNDRF                      
001820*                                                                         
001821       IF OHUV-IDSYSTEM(1:3) = 'LYN'                                      
001822         MOVE OHUV-IDSYSTEM(1:3)  TO WS-IDSYST-1-3                        
001823         MOVE WS-IDSYST-K         TO WS-IDSYST-4                          
001824       ELSE                                                               
001825         IF OHUV-IDSYSTEM(1:3) = 'POL'                                    
001826           MOVE OHUV-IDSYSTEM(1:3) TO WS-IDSYST-1-3                       
001827           MOVE WS-IDSYST-E        TO WS-IDSYST-4                         
001828         ELSE                                                             
001829           IF OHUV-IDSYSTEM(1:3) = 'ECO'                                  
001830             MOVE OHUV-IDSYSTEM(1:3) TO WS-IDSYST-1-3                     
001831             MOVE WS-IDSYST-M        TO WS-IDSYST-4                       
001832           ELSE                                                           
001833             IF OHUV-IDSYSTEM(1:3) = 'TAD'                                
001834               MOVE OHUV-IDSYSTEM(1:3) TO WS-IDSYST-1-3                   
001835               MOVE SPACE         TO WS-IDSYST-4                          
001836             ELSE                                                         
001837               IF OHUV-IDSYSTEM(1:3) = 'ACC'                              
001838                 MOVE OHUV-IDSYSTEM(1:3) TO WS-IDSYST-1-3                 
001839                 MOVE SPACE         TO WS-IDSYST-4                        
001840               ELSE                                                       
001841                 IF OHUV-IDSYSTEM(1:3) = 'APA'                            
001842                   MOVE OHUV-IDSYSTEM(1:3) TO WS-IDSYST-1-3               
001843                   MOVE SPACE         TO WS-IDSYST-4                      
001844                 ELSE                                                     
001845                   IF OHUV-IDSYSTEM(1:3) = 'APB'                          
001846                     MOVE OHUV-IDSYSTEM(1:3) TO WS-IDSYST-1-3             
001847                     MOVE SPACE         TO WS-IDSYST-4                    
001848                   ELSE                                                   
001849                     IF OHUV-IDSYSTEM(1:3) = 'APC'                        
001850                       MOVE OHUV-IDSYSTEM(1:3) TO WS-IDSYST-1-3           
001851                       MOVE SPACE         TO WS-IDSYST-4                  
001852                     ELSE                                                 
001853                       IF OHUV-IDSYSTEM(1:3) = 'APD'                      
001854                         MOVE OHUV-IDSYSTEM(1:3) TO WS-IDSYST-1-3         
001855                         MOVE SPACE         TO WS-IDSYST-4                
001856                       ELSE                                               
001857                         IF OHUV-IDSYSTEM(1:3) = 'APE'                    
001858                          MOVE OHUV-IDSYSTEM(1:3)                         
001859                                                  TO WS-IDSYST-1-3        
001860                           MOVE SPACE         TO WS-IDSYST-4              
001861                         ELSE                                             
001862                           IF OHUV-IDSYSTEM(1:3) = 'APF'                  
001863                             MOVE OHUV-IDSYSTEM(1:3)                      
001864                                                  TO WS-IDSYST-1-3        
001865                             MOVE SPACE         TO WS-IDSYST-4            
001866                           ELSE                                           
001867                             IF OHUV-IDSYSTEM(1:3) = 'APG'                
001868                               MOVE OHUV-IDSYSTEM(1:3)                    
001869                                                  TO WS-IDSYST-1-3        
001870                               MOVE SPACE         TO WS-IDSYST-4          
001871                             ELSE                                         
001872                               IF OHUV-IDSYSTEM(1:3) = 'APH'              
001873                                 MOVE OHUV-IDSYSTEM(1:3)                  
001874                                                  TO WS-IDSYST-1-3        
001875                                 MOVE SPACE       TO WS-IDSYST-4          
001876                               ELSE                                       
001877                                 IF OHUV-IDSYSTEM(1:3) = 'API'            
001878                                   MOVE OHUV-IDSYSTEM(1:3)                
001879                                                  TO WS-IDSYST-1-3        
001880                                   MOVE SPACE     TO WS-IDSYST-4          
001881                                 ELSE                                     
001882                                   IF OHUV-IDSYSTEM(1:3) = 'APJ'          
001883                                     MOVE OHUV-IDSYSTEM(1:3)              
001884                                                  TO WS-IDSYST-1-3        
001885                                     MOVE SPACE                           
001886                                                  TO WS-IDSYST-4          
001887                                   ELSE                                   
001888                                     MOVE SPACE                           
001889                                                  TO WS-IDSYSTEM          
001890                                   END-IF                                 
001891                                 END-IF                                   
001892                               END-IF                                     
001893                             END-IF                                       
001894                           END-IF                                         
001895                         END-IF                                           
001896                       END-IF                                             
001897                     END-IF                                               
001898                  END-IF                                                  
001899               END-IF                                                     
001900            END-IF                                                        
001901          END-IF                                                          
001902        END-IF                                                            
001903      END-IF                                                              
001904     END-IF                                                               
001905*                                                                         
001906     ELSE                                                                 
001907       MOVE SPACE                 TO WS-IDSYSTEM                          
001908     END-IF                                                               
001909                                                                          
001910     PERFORM IMS-REPL-WDE221                                              
001911                                                                          
001912     IF BKOLLI-IDKUNDNR NOT = BGMT-IDKUNDNR OR                            
001913        BKOLLI-IDDISTR NOT = BGMT-IDDISTR                                 
001914        MOVE 'KUNDNR STÄMMER INTE UNDER ROT' TO FELTEXT                   
001915        CALL FELLOG                                                       
001916     END-IF                                                               
001917                                                                          
001918     .                                                                    
001919     EJECT                                                                
001920 H-LAS-WDK6   SECTION.                                                    
001921                                                                          
001922     MOVE ORAD-IDARTNR            TO W-IDARTNR                            
001923     PERFORM IMS-GU-WDK601                                                
001924     PERFORM IMS-GNP-WDK611                                               
001925     .                                                                    
001926     EJECT                                                                
001927 I-SKAPA-RADPOSTER-WDE1   SECTION.                                        
001928     MOVE 'I-SKAPA-RADPOSTER-WDE1'  TO WS-SEKTION                         
001929                                                                          
001930       INITIALIZE                    SRAD-WDE131                          
001931       MOVE ORAD-IDPURAD          TO SRAD-IDPURAD                         
001932                                     W-IDPURAD                            
001933       MOVE ORAD-IDARTNR          TO SRAD-IDARTNR                         
001934       MOVE ORAD-REKSIFFR         TO SRAD-REKSIFFR                        
001935       MOVE KKOLLI-KVLEVART       TO SRAD-KVLEVART                        
001936       MOVE ORAD-PRARTNTO         TO SRAD-PRARTNTO                        
001937       MOVE ORAD-PRARTNTO-LOC     TO SRAD-PRARTNTO-LOC                    
001938       MOVE ORAD-PRARTNTO-LOCPREL TO SRAD-PRARTNTO-LOCPREL                
001939       IF SRAD-PRARTNTO-LOC     NOT = ZERO   AND                          
001940          SRAD-PRARTNTO-LOCPREL NOT = ZERO                                
001941          MOVE ZERO               TO SRAD-PRARTNTO-LOCPREL                
001942       END-IF                                                             
001943       IF DIST79-DEALER-PRICE OR                                          
001944          DIST79-ECOM-PRICE                                               
001945         MOVE ZERO                TO SRAD-PRARTNTO                        
001946       END-IF                                                             
001947       MOVE ORAD-KDVALISO         TO SRAD-KDVALISO                        
001948       MOVE ORAD-VKARTNTO         TO SRAD-VKARTNTO                        
001949       MOVE ORAD-VKART-NTO-KG     TO SRAD-VKART-NTO-KG                    
001950       MOVE ART-KDPRODSL          TO SRAD-KDPRODSL                        
001951       MOVE ART-IDFKNGRP          TO SRAD-IDFKNGRP                        
001952       MOVE ORAD-KDARTURS         TO SRAD-KDARTURS                        
001953       IF SRAD-KDARTURS = SPACE                                           
001954         MOVE CLAG-KDARTURS       TO SRAD-KDARTURS                        
001955         SEARCH ALL DC-LAND                                               
001956            AT END                                                        
001957               MOVE SPACE         TO W-IDLAND                             
001958            WHEN DCLAND-IDDC (DCLAND-IX) = VORD-IDDC                      
001959               MOVE DCLAND-IDLANDX2 (DCLAND-IX)                           
001960                                  TO W-IDLAND                             
001961         END-SEARCH                                                       
001962         PERFORM IMS-GU-WDK712                                            
001963         IF SEGMENT-FINNS                                                 
001964           IF LART-KDARTURS > SPACE                                       
001965             MOVE LART-KDARTURS    TO SRAD-KDARTURS                       
001966           END-IF                                                         
001967         END-IF                                                           
001968       END-IF                                                             
001969       IF GMT-KDSTATNR > 0 AND < 7                                        
001970          MOVE CLAG-IDSTATNR(GMT-KDSTATNR)                                
001971                                  TO SRAD-IDSTATNR                        
001972       END-IF                                                             
001973       MOVE ORAD-IDKUNDRF-RO      TO SRAD-IDKUNDRF-RO                     
001974                                                                          
001975       MOVE ART-IDLEVNR           TO SRAD-IDLEVNR-ART                     
001976       MOVE VORD-IDDC             TO WS-IDDC                              
001977       IF ART-IDLEVNR = '9998' AND NDC-CA                                 
001978*--       '9998' BETYDER ATT DET FINNS EN LOKAL LEV. FÖR ART.             
001979*         OCH USA BEHÖVER HA DEN PÅ SINA DOKUMENT IN TILL LANDET.         
001980         MOVE ORAD-IDARTNR        TO W-IDARTNR                            
001981         MOVE VORD-IDDC           TO W-IDDC                               
001982         PERFORM IMS-GU-WDK711                                            
001983         IF SEGMENT-FINNS                                                 
001984           MOVE SLAG-IDLEVNR      TO SRAD-IDLEVNR-ART                     
001985         END-IF                                                           
001986       END-IF                                                             
001987                                                                          
001988       MOVE ORAD-PRAVCOST         TO SRAD-PRAVCOST                        
001989       MOVE ORAD-KDVALISO-EXP     TO SRAD-KDVALISO-EXP                    
001990                                                                          
001991       IF NDC-AE                                                          
001992*--       FÖR LEV. FRÅN DUBAI BEHÖVER VI HA SENAST UPPDATERAD             
001993*         PRAVCOST PÅ ART.REG                                             
001994*         OBS: I W4063700 GÖR MAN EN UPPDAT. MED DEN SENASTE              
001995*         PRAVCOST MEN BARA PÅ ORDERRADEN.VI BEHÖVER ÄVEN HA SAMMA        
001996*         PÅ WDE131 PGA DOK. W476CUST SOM SKAPAS BARA FÖR DUBAI.          
001997         MOVE ORAD-IDARTNR        TO W-IDARTNR                            
001998         MOVE VORD-IDDC           TO W-IDDC                               
001999         PERFORM IMS-GU-WDK711                                            
002000         IF SEGMENT-FINNS                                                 
002001           MOVE SLAG-PRAVCOST     TO SRAD-PRAVCOST                        
002002         END-IF                                                           
002003       END-IF                                                             
002004       PERFORM IMS-ISRT-WDE131                                            
002005                                                                          
002006     .                                                                    
002007     EJECT                                                                
002008 J-SKAPA-RADPOSTER-WDE2   SECTION.                                        
002009     MOVE 'J-SKAPA-RADPOSTER-WDE2'  TO WS-SEKTION                         
002010                                                                          
002011       INITIALIZE                    BRAD-WDE231                          
002012       MOVE ORAD-IDPURAD          TO BRAD-IDPURAD                         
002013                                     W-IDPURAD                            
002014       MOVE ORAD-IDARTNR          TO BRAD-IDARTNR                         
002015       MOVE ORAD-REKSIFFR         TO BRAD-REKSIFFR                        
002016       MOVE ORAD-DEAL-PR-LINE     TO BRAD-DEAL-PR-LINE                    
002017       MOVE ORAD-KDPRTYP          TO BRAD-KDPRTYP                         
002018       IF DIST79-DEALER-PRICE                                             
002019        IF LPRQ-KDPRSTA = 'A' OR 'M'                                      
002020         IF BRAD-PRARTNTO-LOC     NOT = ZERO   AND                        
002021            BRAD-PRARTNTO-LOCPREL NOT = ZERO                              
002022            MOVE ZERO             TO BRAD-PRARTNTO-LOCPREL                
002023         END-IF                                                           
002024        ELSE                                                              
002025         IF DIST42-EJ-EU AND NOT SOFT-SPEC                                
002026           IF ORAD-KDPRTYP = 'P'                                          
002027             MOVE 'P'             TO BRAD-KDPRTYP                         
002028           ELSE                                                           
002029             IF BRAD-PRARTNTO-LOC = ZERO                                  
002030               MOVE 'T'           TO BRAD-KDPRTYP                         
002031               MOVE BRAD-PRARTNTO-LOCPREL                                 
002032                                  TO BRAD-PRARTNTO-LOC                    
002033             END-IF                                                       
002034             MOVE ZERO            TO BRAD-PRARTNTO-LOCPREL                
002035                                                                          
002036*2006-09-14 UPPDATERA PRISFRÅGAN NÄR ORDERN AVSLUTAS MED LOCPREL.         
002037             IF ORAD-IDPRQUES NOT = ZERO                                  
002038               MOVE KORD-IDDISTR         TO W-IDDISTR-C7                  
002039               MOVE KORD-IDKUNDNR        TO W-IDKUNDNR-C7                 
002040               MOVE SPACE                TO W-IDBUNDLE-C7                 
002041               MOVE KORD-IDORDNR5        TO W-IDORDER-C7(3:5)             
002042               MOVE ZERO                 TO W-IDORDER-C7(1:2)             
002043               MOVE ORAD-IDPRQUES        TO W-IDPRQUES                    
002044               IF ORAD-IDKUNDRF-RO NOT = '00000     '                     
002045                 MOVE ORAD-IDKUNDRF-RO(1:5) TO W-IDORDER-C7(3:5)          
002046               END-IF                                                     
002047               PERFORM IMS-GHU-WDC711                                     
002048               IF SEGMENT-FINNS                                           
002049                 ADD KKOLLI-KVLEVART    TO LPRQ-KVANTAL-AVBOK             
002050                 IF LPRQ-KVANTAL-AVBOK >=  LPRQ-KVBEART                   
002051                    MOVE 'Y'            TO LPRQ-FLALL                     
002052                 END-IF                                                   
002053                 PERFORM IMS-REPL-WDC711                                  
002054               END-IF                                                     
002055             END-IF                                                       
002056           END-IF                                                         
002057         END-IF                                                           
002058        END-IF                                                            
002059       END-IF                                                             
002060       PERFORM S10-VATCODE                                                
002061       MOVE ORAD-KDARTURS         TO BRAD-KDARTURS                        
002062       IF ORAD-IDBIL > SPACE AND  ORAD-IDSYSTEM  = 'VDI '                 
002063         MOVE +1                  TO BRAD-KDSOFT                          
002064       ELSE                                                               
002065         IF ORAD-IDSYSTEM = 'SOFT'                                        
002066           MOVE +2                TO BRAD-KDSOFT                          
002067         ELSE                                                             
002068           MOVE ORAD-IDARTNR      TO TEST-ARTIKEL                         
002069           IF ART04-SOFTWARE                                              
002070             MOVE +3              TO BRAD-KDSOFT                          
002071           ELSE                                                           
002072             IF ORAD-IDSYSTEM = 'W371'                                    
002073               MOVE +4            TO BRAD-KDSOFT                          
002074             ELSE                                                         
002075               MOVE +0            TO BRAD-KDSOFT                          
002076             END-IF                                                       
002077           END-IF                                                         
002078         END-IF                                                           
002079       END-IF                                                             
002080       MOVE ART-KDPRODSL          TO BRAD-KDPRODSL                        
002081       MOVE ORAD-KVBEART          TO BRAD-KVBEART                         
002082       MOVE KKOLLI-KVLEVART       TO BRAD-KVLEVART                        
002083       MOVE ORAD-PRARTNTO         TO BRAD-PRARTNTO                        
002084       MOVE ORAD-VKARTNTO         TO BRAD-VKARTNTO                        
002085       MOVE ORAD-VKART-NTO-KG     TO BRAD-VKART-NTO-KG                    
002086*                                                                         
002087       MOVE ART-IDLEVNR           TO BRAD-IDLEVNR-ART                     
002088       MOVE VORD-IDDC             TO WS-IDDC                              
002089       IF ART-IDLEVNR = '9998' AND NDC-CA                                 
002090*         '9998' BETYDER ATT DET FINNS EN LOKAL LEV. FÖR ART.             
002091*         OCH USA BEHÖVER HA DEN PÅ SINA DOKUMENT IN TILL LANDET.         
002092         MOVE ORAD-IDARTNR        TO W-IDARTNR                            
002093         MOVE VORD-IDDC           TO W-IDDC                               
002094         PERFORM IMS-GU-WDK711                                            
002095         IF SEGMENT-FINNS                                                 
002096           MOVE SLAG-IDLEVNR      TO BRAD-IDLEVNR-ART                     
002097         END-IF                                                           
002098       END-IF                                                             
002099*                                                                         
002100       MOVE ORAD-IDANALYS         TO BRAD-IDANALYS                        
002101       MOVE ORAD-IDKONTO          TO BRAD-IDKONTO                         
002102       MOVE ORAD-IDKST            TO BRAD-IDKST                           
002103       MOVE ORAD-BERADREF         TO BRAD-BERADREF                        
002104*         IDSTATNR FRÅN K611                                              
002105       IF GMT-KDSTATNR > 0 AND < 7                                        
002106          MOVE CLAG-IDSTATNR(GMT-KDSTATNR)                                
002107                                  TO BRAD-IDSTATNR                        
002108       END-IF                                                             
002109       IF BRAD-KDARTURS = SPACE                                           
002110           MOVE CLAG-KDARTURS     TO BRAD-KDARTURS                        
002111           SEARCH ALL DC-LAND                                             
002112              AT END                                                      
002113                 MOVE SPACE       TO W-IDLAND                             
002114              WHEN DCLAND-IDDC (DCLAND-IX) = VORD-IDDC                    
002115                 MOVE DCLAND-IDLANDX2 (DCLAND-IX)                         
002116                                  TO W-IDLAND                             
002117           END-SEARCH                                                     
002118           PERFORM IMS-GU-WDK712                                          
002119           IF SEGMENT-FINNS                                               
002120             IF LART-KDARTURS > SPACE                                     
002121               MOVE LART-KDARTURS  TO SRAD-KDARTURS                       
002122             END-IF                                                       
002123           END-IF                                                         
002124       END-IF                                                             
002125       IF BRAD-IDSTATNR = ZERO                                            
002126         MOVE GEN-IDSTATNR       TO BRAD-IDSTATNR                         
002127       END-IF                                                             
002128*         W335PRIS EFTER BRUTTO-FOB-PRIS                                  
002129       PERFORM S04-PRIS-W335PRIS                                          
002130                                                                          
002131       IF NOT DIST79-DEALER-PRICE OR                                      
002132          BRAD-BEART-VIPS = SPACES                                        
002133         PERFORM JA-HAMTA-ARTTEXT                                         
002134       END-IF                                                             
002135*                                                                         
002136       PERFORM IMS-ISRT-WDE231                                            
002137                                                                          
002138       IF ORAD-IDLEVNR = SPACE OR '00000'                                 
002139         CONTINUE                                                         
002140       ELSE                                                               
002141         MOVE ORAD-IDLEVNR        TO WS-IDLEVNR                           
002142       END-IF                                                             
002143                                                                          
002144*      BERÄKNA SUORDV FÖR TILLÄGG/AVDRAG                                  
002145                                                                          
002146       MOVE ORAD-PRARTNTO-LOC     TO WY-PRARTNTO-LOC                      
002147       MOVE ORAD-PRARTNTO-LOCPREL TO WY-PRARTNTO-LOCPREL                  
002148       IF WY-PRARTNTO-LOC NOT = ZERO AND                                  
002149          WY-PRARTNTO-LOCPREL NOT = ZERO                                  
002150          MOVE ZERO               TO WY-PRARTNTO-LOCPREL                  
002151       END-IF                                                             
002152                                                                          
002153       IF ORAD-PRAVCOST > ZERO                                            
002154         COMPUTE W-SUORDV-NOLL = W-SUORDV-NOLL +                          
002155                 BRAD-KVLEVART * ORAD-PRAVCOST                            
002156       ELSE                                                               
002157         COMPUTE W-SUORDV-NOLL = W-SUORDV-NOLL +                          
002158                 BRAD-KVLEVART * ORAD-PRARTNTO                            
002159       END-IF                                                             
002160*                                                                         
002161*--    FÖR VOR FRÅN DC 11 TILL ETT ANNAT FÖRETAG (CN, IN)                 
002162       IF BILL-KDFINDOC = 'PROF' OR MID-KDTRPINF = KODL                   
002163         CONTINUE                                                         
002164       ELSE                                                               
002165         IF SHIP-IDDC-EXP NOT = SPACE                                     
002166           IF SHIP-IDDC-EXP NOT = WC-CDC-SE                               
002167             COMPUTE W-SUORDV-NOLL = W-SUORDV-NOLL +                      
002168                     BRAD-KVLEVART * ORAD-PRARTNTO                        
002169           END-IF                                                         
002170         END-IF                                                           
002171       END-IF                                                             
002172*--                                                                       
002173       COMPUTE W-SUORDV-NOLL-LOC = W-SUORDV-NOLL-LOC +                    
002174               BRAD-KVLEVART * WY-PRARTNTO-LOC                            
002175       COMPUTE W-SUORDV-NOLL-LOCPREL = W-SUORDV-NOLL-LOCPREL +            
002176               BRAD-KVLEVART * WY-PRARTNTO-LOCPREL                        
002177*                                                                         
002178       IF ORAD-PRAVCOST > ZERO                                            
002179       COMPUTE W-SUORDV-NOLL-LOCPREL = W-SUORDV-NOLL-LOCPREL +            
002180               BRAD-KVLEVART * WY-PRARTNTO-LOCPREL                        
002181         IF CALC-AVGCOST                                                  
002182           MOVE ORAD-IDARTNR      TO W-IDARTNR                            
002183           MOVE BILL-IDDC         TO W-IDDC                               
002184           PERFORM IMS-GU-WDK711                                          
002185           IF SEGMENT-FINNS                                               
002186              COMPUTE W-SUORDV-DIST = W-SUORDV-DIST +                     
002187                      BRAD-KVLEVART * SLAG-PRAVCOST                       
002188           ELSE                                                           
002189              COMPUTE W-SUORDV-DIST = W-SUORDV-DIST +                     
002190                      BRAD-KVLEVART * ORAD-PRAVCOST                       
002191           END-IF                                                         
002192         ELSE                                                             
002193           COMPUTE W-SUORDV-DIST = W-SUORDV-DIST +                        
002194                   BRAD-KVLEVART * ORAD-PRAVCOST                          
002195         END-IF                                                           
002196       ELSE                                                               
002197         COMPUTE W-SUORDV-DIST = W-SUORDV-DIST +                          
002198                 BRAD-KVLEVART * ORAD-PRARTNTO                            
002199       END-IF                                                             
002200*                                                                         
002201*--    FÖR VOR FRÅN DC 11 TILL ETT ANNAT FÖRETAG (CN, IN)                 
002202       IF BILL-KDFINDOC = 'PROF' OR MID-KDTRPINF = KODL                   
002203         CONTINUE                                                         
002204       ELSE                                                               
002205         IF SHIP-IDDC-EXP NOT = SPACE                                     
002206           IF SHIP-IDDC-EXP NOT = WC-CDC-SE                               
002207             COMPUTE W-SUORDV-DIST = W-SUORDV-DIST +                      
002208                     BRAD-KVLEVART * ORAD-PRARTNTO                        
002209           END-IF                                                         
002210         END-IF                                                           
002211       END-IF                                                             
002212*--                                                                       
002213       COMPUTE W-SUORDV-DIST-LOC = W-SUORDV-DIST-LOC +                    
002214               BRAD-KVLEVART * WY-PRARTNTO-LOC                            
002215       COMPUTE W-SUORDV-DIST-LOCPREL = W-SUORDV-DIST-LOCPREL +            
002216               BRAD-KVLEVART * WY-PRARTNTO-LOCPREL                        
002217     .                                                                    
002218     EJECT                                                                
002219 JA-HAMTA-ARTTEXT SECTION.                                                
002220     MOVE 'JA-HAMTA-ARTTEXT'   TO WS-SEKTION                              
002221                                                                          
002222     MOVE BRAD-IDARTNR          TO W-IDARTNR                              
002223     MOVE 'GB '                 TO W-IDSKYLT                              
002224     IF NOT DIST79-DEALER-PRICE                                           
002225       MOVE GMT-IDSKYLT         TO W-IDSKYLT                              
002226     END-IF                                                               
002227                                                                          
002228     PERFORM IMS-GU-WDD311                                                
002229     IF SEGMENT-FINNS                                                     
002230       MOVE TEXT-BEART          TO BRAD-BEART-VIPS                        
002231     END-IF                                                               
002232*                                                                         
002233*    OM ARGENTINA OCH BYTES-ARTIKEL TA BORT 'BYTES' FRÅN TEXTEN           
002234*    FAST PÅ SPANSKA,REACONDI ELLER  EXCH PÅ ENGELSKA                     
002235     IF DIST38-EJ-EXCH                                                    
002236       MOVE BRAD-IDARTNR         TO TEST-ARTIKEL                          
002237       IF BYT02-RENOV OR BYT19-BYTES                                      
002238         PERFORM JAA-EXCH-BORT                                            
002239       END-IF                                                             
002240     END-IF                                                               
002241*                                                                         
002242*    OM TAIWAN TAG BORT 'KIT' FRÅN TEXTEN PÅ ENGELSKA                     
002243*    GÄLLER ALLA ARTIKLAR!                                                
002244     IF DIST38-EJ-KIT-TW                                                  
002245       PERFORM JAB-KIT-BORT                                               
002246     END-IF                                                               
002247     .                                                                    
002248     EJECT                                                                
002249 JAA-EXCH-BORT  SECTION.                                                  
002250     MOVE BRAD-BEART-VIPS        TO WS-BETEXT                             
002251*     FÖRSÖK TA BORT EXCH                                                 
002252     IF W-IDSKYLT = 'GB '                                                 
002253       MOVE 1                      TO IB                                  
002254       MOVE ZERO                   TO IC                                  
002255       PERFORM UNTIL IB > 22                                              
002256         IF WS-BETEXT(IB:4) = 'EXCH'                                      
002257           MOVE SPACE              TO WS-BETEXT(IB:4)                     
002258           ADD 4 TO IB     GIVING IC                                      
002259           MOVE 25                 TO IB                                  
002260         END-IF                                                           
002261         ADD 1                     TO IB                                  
002262       END-PERFORM                                                        
002263     END-IF                                                               
002264*     FÖRSÖK TA BORT REACONDI                                             
002265     IF W-IDSKYLT = 'E  '                                                 
002266       MOVE 1                      TO IB                                  
002267       MOVE ZERO                   TO IC                                  
002268       PERFORM UNTIL IB > 22                                              
002269         IF WS-BETEXT(IB:4) = 'REAC'                                      
002270           MOVE SPACE              TO WS-BETEXT(IB:4)                     
002271           ADD 4 TO IB     GIVING IC                                      
002272           MOVE 25                 TO IB                                  
002273         END-IF                                                           
002274         ADD 1                     TO IB                                  
002275       END-PERFORM                                                        
002276     END-IF                                                               
002277*     FÖRSÖK TA BORT ÉCHANGE                                              
002278     IF W-IDSKYLT = 'F  '                                                 
002279       MOVE 1                      TO IB                                  
002280       MOVE ZERO                   TO IC                                  
002281       PERFORM UNTIL IB > 22                                              
002282         IF WS-BETEXT(IB:4) = 'ÉCHA'                                      
002283           MOVE SPACE              TO WS-BETEXT(IB:4)                     
002284           ADD 4 TO IB     GIVING IC                                      
002285           MOVE 25                 TO IB                                  
002286         END-IF                                                           
002287         ADD 1                     TO IB                                  
002288       END-PERFORM                                                        
002289     END-IF                                                               
002290     IF IC < 25 AND NOT = ZERO                                            
002291       PERFORM UNTIL IC > 25                                              
002292         MOVE SPACE              TO WS-BETEXT(IC:1)                       
002293         ADD 1                   TO IC                                    
002294       END-PERFORM                                                        
002295     END-IF                                                               
002296     MOVE WS-BETEXT              TO BRAD-BEART-VIPS                       
002297     .                                                                    
002298     EJECT                                                                
002299 JAB-KIT-BORT  SECTION.                                                   
002300     MOVE BRAD-BEART-VIPS        TO WS-BETEXT                             
002301*     FÖRSÖK TA BORT KIT                                                  
002302     IF W-IDSKYLT = 'GB '                                                 
002303       MOVE 1                      TO IB                                  
002304       MOVE ZERO                   TO IC                                  
002305       PERFORM UNTIL IB > 22                                              
002306         IF WS-BETEXT(IB:3) = 'KIT'                                       
002307           MOVE SPACE              TO WS-BETEXT(IB:3)                     
002308           ADD 3 TO IB     GIVING IC                                      
002309           MOVE 25                 TO IB                                  
002310         END-IF                                                           
002311         ADD 1                     TO IB                                  
002312       END-PERFORM                                                        
002313     END-IF                                                               
002314     IF IC < 25 AND NOT = ZERO                                            
002315       PERFORM UNTIL IC > 25                                              
002316         MOVE SPACE              TO WS-BETEXT(IC:1)                       
002317         ADD 1                   TO IC                                    
002318       END-PERFORM                                                        
002319     END-IF                                                               
002320     MOVE WS-BETEXT              TO BRAD-BEART-VIPS                       
002321     .                                                                    
002322     EJECT                                                                
002323 K-KOLLA-PRISFRAGA  SECTION.                                              
002324     MOVE 'K-KOLLA-PRISFRAGA'   TO WS-SEKTION                             
002325                                                                          
002326     MOVE NEJ                    TO DELA-SW                               
002327     IF ORAD-PRARTNTO-LOC = ZERO OR                                       
002328       (ORAD-PRARTNTO-LOC NOT = ZERO AND                                  
002329        ORAD-PRARTNTO-LOCPREL NOT = ZERO)                                 
002330      IF ORAD-IDPRQUES NOT = ZERO                                         
002331       MOVE KORD-IDDISTR         TO W-IDDISTR-C7                          
002332       MOVE KORD-IDKUNDNR        TO W-IDKUNDNR-C7                         
002333       MOVE SPACE                TO W-IDBUNDLE-C7                         
002334       MOVE KORD-IDORDNR5        TO W-IDORDER-C7(3:5)                     
002335       MOVE ZERO                 TO W-IDORDER-C7(1:2)                     
002336       MOVE ORAD-IDPRQUES        TO W-IDPRQUES                            
002337       IF ORAD-IDKUNDRF-RO NOT = '00000     '                             
002338         MOVE ORAD-IDKUNDRF-RO(1:5) TO W-IDORDER-C7(3:5)                  
002339       END-IF                                                             
002340       PERFORM IMS-GHU-WDC711                                             
002341       IF SEGMENT-FINNS                                                   
002342         IF  LPRQ-KDPRSTA = 'A' OR 'M'                                    
002343           MOVE ORAD-PRARTNTO-LOC     TO WS-PRARTNTO-LOC                  
002344           MOVE ORAD-PRARTNTO-LOCPREL TO WS-PRARTNTO-LOCPREL              
002345           IF WS-PRARTNTO-LOC NOT = ZERO AND                              
002346              WS-PRARTNTO-LOCPREL NOT = ZERO                              
002347              MOVE ZERO               TO WS-PRARTNTO-LOC                  
002348              MOVE JA                 TO DELA-SW                          
002349           END-IF                                                         
002350*                                                                         
002351           ADD KKOLLI-KVLEVART        TO LPRQ-KVANTAL-AVBOK               
002352           IF LPRQ-KVANTAL-AVBOK >=  LPRQ-KVBEART                         
002353             MOVE 'Y'                 TO LPRQ-FLALL                       
002354           END-IF                                                         
002355           PERFORM IMS-REPL-WDC711                                        
002356                                                                          
002357           MOVE ORAD-IDPURAD        TO W-IDPURAD-FSEQ                     
002358           PERFORM IMS-GHU-WDE411X-FSEQ                                   
002359*------- UPDATERA PRARTBTO/NTO FRÅN C711 TILL E411                        
002360           MOVE LPRQ-PRARTBTO-LOC   TO ORAD-PRARTBTO-LOC                  
002361           IF ORAD-KDPRTYP NOT = 'P'                                      
002362            MOVE LPRQ-PRARTNTO-LOC  TO ORAD-PRARTNTO-LOC                  
002363           END-IF                                                         
002364           IF ORAD-KDPRTYP = 'P' AND                                      
002365              ORAD-PRARTBTO-LOC = ZERO                                    
002366              MOVE ORAD-PRARTNTO-LOC TO ORAD-PRARTBTO-LOC                 
002367           END-IF                                                         
002368           MOVE LPRQ-KDVALISO       TO ORAD-KDVALISO                      
002369           MOVE LPRQ-KDVAT          TO ORAD-KDVAT                         
002370           MOVE LPRQ-REARTRAB       TO ORAD-RERAB                         
002371           MOVE LPRQ-KDRAB          TO ORAD-KDRAB                         
002372           MOVE LPRQ-BEART-VIPS     TO ORAD-BEART-VIPS                    
002373                                                                          
002374           MOVE ORAD-PRARTNTO-LOC     TO WY-PRARTNTO-LOC                  
002375           MOVE ORAD-PRARTNTO-LOCPREL TO WY-PRARTNTO-LOCPREL              
002376           IF WY-PRARTNTO-LOC NOT = ZERO AND                              
002377              WY-PRARTNTO-LOCPREL NOT = ZERO                              
002378              MOVE ZERO               TO WY-PRARTNTO-LOCPREL              
002379           END-IF                                                         
002380*                                                                         
002381                                                                          
002382           IF ORAD-PRARTNTO-LOC > 0 AND                                   
002383              ORAD-PRARTNTO-LOCPREL > 0 AND LPRQ-FLALL = 'Y'              
002384             MOVE ZERO           TO ORAD-PRARTNTO-LOCPREL                 
002385           END-IF                                                         
002386           PERFORM IMS-REPL-WDE411X                                       
002387                                                                          
002388*-------  SUORDV FÖR E401, E121, E601, E611                               
002389           COMPUTE WS-SUORDV-LOC = WS-SUORDV-LOC +                        
002390                 KKOLLI-KVLEVART * WY-PRARTNTO-LOC -                      
002391                 KKOLLI-KVLEVART * WS-PRARTNTO-LOC                        
002392           COMPUTE WS-SUORDV-LOCPREL = WS-SUORDV-LOCPREL +                
002393                 KKOLLI-KVLEVART * WY-PRARTNTO-LOCPREL -                  
002394                 KKOLLI-KVLEVART * WS-PRARTNTO-LOCPREL                    
002395                                                                          
002396             COMPUTE WS-SUORDV-BEST-LOC =                                 
002397                 WS-SUORDV-BEST-LOC +                                     
002398                 KKOLLI-KVLEVART * WY-PRARTNTO-LOC -                      
002399                 KKOLLI-KVLEVART * WS-PRARTNTO-LOC                        
002400             COMPUTE WS-SUORDV-BEST-LOCPREL =                             
002401                 WS-SUORDV-BEST-LOCPREL +                                 
002402                 KKOLLI-KVLEVART * WY-PRARTNTO-LOCPREL -                  
002403                 KKOLLI-KVLEVART * WS-PRARTNTO-LOCPREL                    
002404                                                                          
002405             COMPUTE WS-SUORDV-LEV-LOC =                                  
002406                 WS-SUORDV-LEV-LOC +                                      
002407                 KKOLLI-KVLEVART * WY-PRARTNTO-LOC -                      
002408                 KKOLLI-KVLEVART * WS-PRARTNTO-LOC                        
002409             COMPUTE WS-SUORDV-LEV-LOCPREL =                              
002410                 WS-SUORDV-LEV-LOCPREL +                                  
002411                 KKOLLI-KVLEVART * WY-PRARTNTO-LOCPREL -                  
002412                 KKOLLI-KVLEVART * WS-PRARTNTO-LOCPREL                    
002413         ELSE                                                             
002414           IF DIST42-EU  OR                                               
002415             (DIST42-EJ-EU AND SOFT-SPEC)                                 
002416             MOVE NEJ               TO KOLLI-SW                           
002417           END-IF                                                         
002418         END-IF                                                           
002419       ELSE                                                               
002420         MOVE NEJ                 TO KOLLI-SW                             
002421       END-IF                                                             
002422      ELSE                                                                
002423       MOVE NEJ                   TO KOLLI-SW                             
002424      END-IF                                                              
002425     END-IF                                                               
002426     IF KORD-FLOVRLEV = JA OR ORAD-PRARTBTO-LOC = ZERO                    
002427       PERFORM KB-TEXT-TILL-E4                                            
002428     END-IF                                                               
002429     .                                                                    
002430     EJECT                                                                
002431 KB-TEXT-TILL-E4   SECTION.                                               
002432     MOVE 'KB-TEXT-TILL-E4'        TO WS-SEKTION                          
002433     IF ORAD-BEART-VIPS = SPACE OR                                        
002434        ORAD-KDVALISO = SPACE   OR                                        
002435        ORAD-KDVAT = SPACE      OR                                        
002436        (ORAD-PRARTNTO-LOC NOT = ZERO AND                                 
002437         ORAD-PRARTBTO-LOC = ZERO)                                        
002438       IF ORAD-IDPRQUES NOT = ZERO                                        
002439         MOVE KORD-IDDISTR           TO W-IDDISTR-C7                      
002440         MOVE KORD-IDKUNDNR          TO W-IDKUNDNR-C7                     
002441         MOVE SPACE                  TO W-IDBUNDLE-C7                     
002442         MOVE KORD-IDORDNR5          TO W-IDORDER-C7(3:5)                 
002443         MOVE ZERO                   TO W-IDORDER-C7(1:2)                 
002444         MOVE ORAD-IDPRQUES          TO W-IDPRQUES                        
002445         IF ORAD-IDKUNDRF-RO NOT = '00000     '                           
002446           MOVE ORAD-IDKUNDRF-RO(1:5) TO W-IDORDER-C7(3:5)                
002447         END-IF                                                           
002448         PERFORM IMS-GHU-WDC711                                           
002449         IF SEGMENT-FINNS                                                 
002450            MOVE ORAD-IDPURAD        TO W-IDPURAD-FSEQ                    
002451            PERFORM IMS-GHU-WDE411X-FSEQ                                  
002452            IF ORAD-KDVALISO = SPACE                                      
002453             IF LPRQ-KDVALISO NOT = SPACE                                 
002454              MOVE LPRQ-KDVALISO     TO ORAD-KDVALISO                     
002455             ELSE                                                         
002456              MOVE BET-KDVALISO      TO ORAD-KDVALISO                     
002457             END-IF                                                       
002458                                                                          
002459            END-IF                                                        
002460            IF ORAD-KDVAT = SPACE                                         
002461             IF LPRQ-KDVAT NOT = SPACE                                    
002462               MOVE LPRQ-KDVAT       TO ORAD-KDVAT                        
002463             ELSE                                                         
002464               PERFORM S10-VATCODE                                        
002465               MOVE BRAD-KDVAT       TO ORAD-KDVAT                        
002466             END-IF                                                       
002467            END-IF                                                        
002468            IF ORAD-BEART-VIPS = SPACE                                    
002469             MOVE LPRQ-BEART-VIPS     TO ORAD-BEART-VIPS                  
002470            END-IF                                                        
002471            IF ORAD-PRARTBTO-LOC = ZERO  AND                              
002472               ORAD-PRARTNTO-LOC NOT = ZERO                               
002473               MOVE ORAD-PRARTNTO-LOC TO ORAD-PRARTBTO-LOC                
002474               MOVE ZERO              TO ORAD-RERAB                       
002475            END-IF                                                        
002476            PERFORM IMS-REPL-WDE411X                                      
002477         END-IF                                                           
002478       ELSE                                                               
002479         IF ORAD-PRARTNTO-LOC NOT = ZERO AND                              
002480            ORAD-PRARTBTO-LOC = ZERO                                      
002481            MOVE ORAD-IDPURAD        TO W-IDPURAD-FSEQ                    
002482            PERFORM IMS-GHU-WDE411X-FSEQ                                  
002483            MOVE ORAD-PRARTNTO-LOC   TO ORAD-PRARTBTO-LOC                 
002484            MOVE ZERO                TO ORAD-RERAB                        
002485            PERFORM IMS-REPL-WDE411X                                      
002486         END-IF                                                           
002487       END-IF                                                             
002488     END-IF                                                               
002489     .                                                                    
002490     EJECT                                                                
002491 L-UPPDAT-SUORDV-KOLLI SECTION.                                           
002492     MOVE 'L-UPPDAT-SUORDV-KOLLI'    TO WS-SEKTION                        
002493                                                                          
002494     IF WS-SUORDV-LOC = +0 AND WS-SUORDV-LOCPREL = +0 OR                  
002495        BILL-KDFINDOC = 'PROF'                                            
002496       CONTINUE                                                           
002497     ELSE                                                                 
002498*------- UPPDATERA SUORDV PÅ E401, E121                                   
002499       MOVE BILL-IDDC                TO WS-IDDC                           
002500                                        W-IDDC-B6                         
002501       PERFORM IMS-GU-WDB601                                              
002502       PERFORM IMS-GHU-WDE401-KVAL                                        
002503       IF SEGMENT-FINNS                                                   
002504         IF DCS-DDC AND ORAD-FLDIRLEV = JA                                
002505           COMPUTE KORD-SUORDV-LEVPL-LOC  =                               
002506                   KORD-SUORDV-LEVPL-LOC  +                               
002507                   WS-SUORDV-BEST-LOC                                     
002508           COMPUTE KORD-SUORDV-LEVPL-LOCPREL =                            
002509                   KORD-SUORDV-LEVPL-LOCPREL +                            
002510                   WS-SUORDV-BEST-LOCPREL                                 
002511         ELSE                                                             
002512           COMPUTE KORD-SUORDV-LOC  =                                     
002513                   KORD-SUORDV-LOC  +                                     
002514                   WS-SUORDV-BEST-LOC                                     
002515           COMPUTE KORD-SUORDV-LOCPREL  =                                 
002516                   KORD-SUORDV-LOCPREL  +                                 
002517                   WS-SUORDV-BEST-LOCPREL                                 
002518         END-IF                                                           
002519         IF MID-KDTRPINF NOT = KODL                                       
002520           PERFORM IMS-REPL-WDE401                                        
002521         END-IF                                                           
002522       END-IF                                                             
002523*                                                                         
002524       IF MID-KDTRPINF = JA                                               
002525         OR = NEJ OR KODL                                                 
002526         OR DIS107-BYGG-SHIP                                              
002527         MOVE SKOLLI-IDPRODNR        TO W-IDPRODNR                        
002528         MOVE SKOLLI-IDKOLLI         TO W-IDKOLLI                         
002529         PERFORM IMS-GHNP-WDE121-KVAL                                     
002530         IF SEGMENT-FINNS                                                 
002531          IF DIST79-DEALER-PRICE                                          
002532           COMPUTE SKOLLI-SUORDV-LOC     = SKOLLI-SUORDV-LOC +            
002533                                           WS-SUORDV-LOC                  
002534           COMPUTE SKOLLI-SUORDV-LOCPREL = SKOLLI-SUORDV-LOCPREL +        
002535                                           WS-SUORDV-LOCPREL              
002536           MOVE SRAD-KDVALISO            TO SKOLLI-KDVALISO               
002537                                                                          
002538           PERFORM IMS-REPL-WDE121                                        
002539*                                                                         
002540          ELSE                                                            
002541            IF DIST79-ECOM-PRICE                                          
002542             COMPUTE SKOLLI-SUORDV-LOC   = SKOLLI-SUORDV-LOC +            
002543                                           WS-SUORDV-LOC                  
002544             MOVE SRAD-KDVALISO          TO SKOLLI-KDVALISO               
002545                                                                          
002546             PERFORM IMS-REPL-WDE121                                      
002547            END-IF                                                        
002548          END-IF                                                          
002549         END-IF                                                           
002550       END-IF                                                             
002551*                                                                         
002552*------- UPPDATERA SUORDV PÅ E6,                                          
002553       IF MID-KDTRPINF NOT = KODL                                         
002554         PERFORM IMS-GHU-WDE601                                           
002555         IF SEGMENT-FINNS                                                 
002556           COMPUTE VORD-SUORDV-LOC     =                                  
002557                   VORD-SUORDV-LOC     +                                  
002558                   WS-SUORDV-BEST-LOC                                     
002559           COMPUTE VORD-SUORDV-LOCPREL    =                               
002560                   VORD-SUORDV-LOCPREL    +                               
002561                   WS-SUORDV-BEST-LOCPREL                                 
002562           MOVE LPRQ-KDVALISO          TO VORD-KDVALISO                   
002563           COMPUTE VORD-SUORDV-PACK-LOC   =                               
002564                   VORD-SUORDV-PACK-LOC   +                               
002565                   WS-SUORDV-LEV-LOC                                      
002566           COMPUTE VORD-SUORDV-PACK-LOCPREL =                             
002567                   VORD-SUORDV-PACK-LOCPREL +                             
002568                   WS-SUORDV-LEV-LOCPREL                                  
002569           COMPUTE VORD-SUORDV-FL-LOC     =                               
002570                   VORD-SUORDV-FL-LOC     +                               
002571                   WS-SUORDV-LEV-LOC                                      
002572           COMPUTE VORD-SUORDV-FL-LOCPREL =                               
002573                   VORD-SUORDV-FL-LOCPREL +                               
002574                   WS-SUORDV-LEV-LOCPREL                                  
002575                                                                          
002576           IF DIST79-ECOM-PRICE                                           
002577             MOVE SRAD-KDVALISO        TO VORD-KDVALISO                   
002578           END-IF                                                         
002579*                                                                         
002580           PERFORM IMS-REPL-WDE601                                        
002581         END-IF                                                           
002582*                                                                         
002583         PERFORM IMS-GHNP-WDE611                                          
002584         IF SEGMENT-FINNS                                                 
002585           IF DIST79-DEALER-PRICE                                         
002586             COMPUTE KOLLI-SUORDV-LOC   =                                 
002587                     KOLLI-SUORDV-LOC   +                                 
002588                     WS-SUORDV-LOC                                        
002589             COMPUTE KOLLI-SUORDV-LOCPREL =                               
002590                     KOLLI-SUORDV-LOCPREL +                               
002591                     WS-SUORDV-LOCPREL                                    
002592             MOVE LPRQ-KDVALISO        TO KOLLI-KDVALISO                  
002593*                                                                         
002594             PERFORM IMS-REPL-WDE611                                      
002595           ELSE                                                           
002596             IF DIST79-ECOM-PRICE                                         
002597               COMPUTE KOLLI-SUORDV-LOC =                                 
002598                       KOLLI-SUORDV-LOC +                                 
002599                       WS-SUORDV-LOC                                      
002600             MOVE VORD-KDVALISO        TO KOLLI-KDVALISO                  
002601*                                                                         
002602             PERFORM IMS-REPL-WDE611                                      
002603             END-IF                                                       
002604           END-IF                                                         
002605                                                                          
002606         END-IF                                                           
002607       END-IF                                                             
002608*                                                                         
002609       MOVE KORD-IDORDER             TO W-IDORDER                         
002610       MOVE ORAD-IDLEVNR             TO W-IDLEVNR                         
002611*                                                                         
002612     END-IF                                                               
002613     .                                                                    
002614     EJECT                                                                
002615 M-UPPDATERA-STATUS-KOLLI SECTION.                                        
002616     MOVE 'M-UPPDATERA-STATUS-KOLLI' TO WS-SEKTION                        
002617                                                                          
002618     IF (KOLLI-EJPRIS AND (DIST42-EU OR SOFT-SPEC))                       
002619        AND (SKOLLI-FLCROSS = SPACE OR NEJ)                               
002620       IF NOT DIST92-ITALY                                                
002621         OR (DIST92-ITALY-SEP AND SOFT-SPEC)                              
002622*       UPPDATERA KDPRSTA  I WDE221                                       
002623         PERFORM IMS-GHNP-WDE221-KVAL                                     
002624         MOVE 'R'                  TO BKOLLI-KDPRSTA                      
002625         PERFORM IMS-REPL-WDE221                                          
002626       END-IF                                                             
002627     END-IF                                                               
002628     .                                                                    
002629     EJECT                                                                
002630 P-ORDKL-WDE111X  SECTION.                                                
002631     MOVE 'P-ORDKL-WDE111X'      TO WS-SEKTION                            
002632                                                                          
002633     PERFORM IMS-GHU-WDE111X-KVAL                                         
002634     IF SEGMENT-FINNS                                                     
002635        MOVE WS-KDORDKL-MAX     TO SGMT-KDORDKL-MAX                       
002636        PERFORM IMS-REPL-WDE111X                                          
002637     END-IF                                                               
002638     .                                                                    
002639     EJECT                                                                
002640 N-SKAPA-SALDOLOGG SECTION.                                               
002641     MOVE 'N-SKAPA-SALDOLOGG'      TO WS-SEKTION                          
002642                                                                          
002643     MOVE BILL-IDDC                  TO WS-IDDC                           
002644     IF WS-IDDC NOT = W-IDDC-B6                                           
002645        MOVE WS-IDDC TO W-IDDC-B6                                         
002646        PERFORM IMS-GU-WDB601                                             
002647     END-IF                                                               
002648     PERFORM IMS-GU-WDB601                                                
002649     IF DCS-CDC                                                           
002650       MOVE W-IDARTNR                TO LOGG-IDARTNR                      
002651                                                                          
002652       MOVE FUNCTION CURRENT-DATE (1:8) TO WS-AAAAMMDD                    
002653       COMPUTE LOGG-DAREGDAT-9KOMPL  = 99999999                           
002654                                     - WS-AAAAMMDD                        
002655       MOVE FUNCTION CURRENT-DATE (9:8) TO WS-TTMMSSTH                    
002656       COMPUTE LOGG-TIKLOCK-9KOMPL   = 999999999                          
002657                                     - WS-TTMMSSTH                        
002658       MOVE 9                        TO LOGG-IDSEKVNR                     
002659       MOVE WS-IDDC                  TO LOGG-IDDC                         
002660       MOVE 'OUTB'                   TO LOGG-IDHUVTYP                     
002661       MOVE 'SHP'                    TO LOGG-IDSUBTYP                     
002662       MOVE 'W4063600'               TO LOGG-IDPGM                        
002663       MOVE SPACE                    TO LOGG-IDTRANS                      
002664       MOVE 'W4063600'               TO LOGG-IDUSER                       
002665       MOVE SPACE                    TO LOGG-REF                          
002666       MOVE ZERO                     TO LOGG-IDFAKT                       
002667       MOVE BKOLLI-IDDISTR           TO LOGG-IDDISTR                      
002668       MOVE BKOLLI-IDKUNDNR          TO LOGG-IDKUNDNR                     
002669       MOVE BKOLLI-IDKUNDRF          TO LOGG-IDKUNDRF                     
002670       MOVE BKOLLI-IDPRODNR          TO LOGG-IDPRODNR                     
002671                                                                          
002672       MOVE ' '                      TO LOGG-IDTECKEN-KVAKS               
002673       MOVE ' '                      TO LOGG-IDTECKEN-KVAKS-PAV           
002674       MOVE ' '                      TO LOGG-IDTECKEN-KVEFRS              
002675       MOVE ' '                      TO LOGG-IDTECKEN-KVLS                
002676       MOVE ZERO                     TO LOGG-KVART-SALDO                  
002677                                                                          
002678       COMPUTE LOGG-KVAKS = CLAG-KVAKS-CDC                                
002679                          + CLAG-KVAKS-T                                  
002680                                                                          
002681       MOVE CLAG-KVAKS-PAV           TO LOGG-KVAKS-PAV                    
002682       MOVE CLAG-KVEFRS              TO LOGG-KVEFRS                       
002683       MOVE CLAG-KVLS                TO LOGG-KVLS                         
002684       MOVE 000000                   TO LOGG-DAREGDAT-LADD                
002685                                                                          
002686       PERFORM S08-IMS-ISRT-WDL901                                        
002687     ELSE                                                                 
002688       MOVE BILL-IDDC                TO W-IDDC                            
002689       PERFORM IMS-GU-WDK711                                              
002690       IF SEGMENT-FINNS                                                   
002691         MOVE W-IDARTNR              TO LOGG-IDARTNR                      
002692                                                                          
002693         MOVE FUNCTION CURRENT-DATE (1:8) TO WS-AAAAMMDD                  
002694         COMPUTE LOGG-DAREGDAT-9KOMPL  = 99999999                         
002695                                       - WS-AAAAMMDD                      
002696         MOVE FUNCTION CURRENT-DATE (9:8) TO WS-TTMMSSTH                  
002697         COMPUTE LOGG-TIKLOCK-9KOMPL   = 999999999                        
002698                                       - WS-TTMMSSTH                      
002699         MOVE 9                      TO LOGG-IDSEKVNR                     
002700         MOVE WS-IDDC                TO LOGG-IDDC                         
002701         MOVE 'OUTB'                 TO LOGG-IDHUVTYP                     
002702         MOVE 'SHP'                  TO LOGG-IDSUBTYP                     
002703         MOVE 'W4063600'             TO LOGG-IDPGM                        
002704         MOVE SPACE                  TO LOGG-IDTRANS                      
002705         MOVE 'W4063600'             TO LOGG-IDUSER                       
002706         MOVE SPACE                  TO LOGG-REF                          
002707         MOVE ZERO                   TO LOGG-IDFAKT                       
002708         MOVE BKOLLI-IDDISTR         TO LOGG-IDDISTR                      
002709         MOVE BKOLLI-IDKUNDNR        TO LOGG-IDKUNDNR                     
002710         MOVE BKOLLI-IDKUNDRF        TO LOGG-IDKUNDRF                     
002711         MOVE BKOLLI-IDPRODNR        TO LOGG-IDPRODNR                     
002712                                                                          
002713         MOVE ' '                    TO LOGG-IDTECKEN-KVAKS               
002714         MOVE ' '                    TO LOGG-IDTECKEN-KVAKS-PAV           
002715         MOVE ' '                    TO LOGG-IDTECKEN-KVEFRS              
002716         MOVE ' '                    TO LOGG-IDTECKEN-KVLS                
002717         MOVE ZERO                   TO LOGG-KVART-SALDO                  
002718                                                                          
002719                                                                          
002720         MOVE SLAG-KVAKS-SDC         TO LOGG-KVAKS                        
002721         MOVE SLAG-KVAKS-PAV         TO LOGG-KVAKS-PAV                    
002722         MOVE SLAG-KVEFRS            TO LOGG-KVEFRS                       
002723         MOVE SLAG-KVLS              TO LOGG-KVLS                         
002724         MOVE 000000                 TO LOGG-DAREGDAT-LADD                
002725                                                                          
002726         PERFORM S08-IMS-ISRT-WDL901                                      
002727       END-IF                                                             
002728     END-IF                                                               
002729     .                                                                    
002730     EJECT                                                                
002731 Q-AVSLUTA-WDE101 SECTION.                                                
002732     MOVE 'J-AVSLUTA-WDE101'    TO WS-SEKTION                             
002733                                                                          
002734     PERFORM IMS-GHU-WDE101                                               
002735     IF MID-KDTRPINF = NEJ                                                
002736       MOVE KODB                TO SHIP-KDKLAR                            
002737     ELSE                                                                 
002738       IF MID-KDTRPINF = KODL                                             
002739         MOVE KODL              TO SHIP-KDKLAR                            
002740       ELSE                                                               
002741         MOVE JA                TO SHIP-KDKLAR                            
002742       END-IF                                                             
002743     END-IF                                                               
002744                                                                          
002745     IF DIS107-BYGG-SHIP                                                  
002746       IF MID-KDTRPINF NOT = KODL                                         
002747         MOVE JA                  TO SHIP-KDKLAR                          
002748       END-IF                                                             
002749       MOVE JA                  TO SHIP-FLSKRIV-NU                        
002750     END-IF                                                               
002751                                                                          
002752     IF UPPDAT-IDDC-EXP                                                   
002753       MOVE WS-IDDC-EXP         TO SHIP-IDDC-EXP                          
002754       MOVE 1                   TO SHIP-KDFAKSTA-EXP                      
002755     END-IF                                                               
002756                                                                          
002757     MOVE WS-IDSYSTEM           TO SHIP-IDSYSTEM                          
002758                                                                          
002759     IF REFILL-FLOW                                                       
002760                                                                          
002761        COMPUTE SHIP-SUORDV-FAKT   = SHIP-SUORDV-FAKT   +                 
002762                                     WS-SUORDV-FAKT                       
002763        COMPUTE SHIP-SUORDV-EXP    = SHIP-SUORDV-EXP    +                 
002764                                     WS-SUORDV-EXP                        
002765        COMPUTE SHIP-VKORDBTO-FAKT = SHIP-VKORDBTO-FAKT +                 
002766                                     WS-VKORDBTO-FAKT                     
002767        COMPUTE SHIP-VLORDBTO-FAKT = SHIP-VLORDBTO-FAKT +                 
002768                                     WS-VLORDBTO-FAKT                     
002769        IF SHIP-KDVALISO-EXP = SPACE                                      
002770           MOVE WS-KDVALISO-EXP    TO SHIP-KDVALISO-EXP                   
002771        END-IF                                                            
002772                                                                          
002773        IF SHIP-FLFARLIG = SPACE OR                                       
002774           SHIP-FLFARLIG = NEJ                                            
002775           MOVE WS-FLFARLIG-FLAG   TO SHIP-FLFARLIG                       
002776        END-IF                                                            
002777                                                                          
002778     END-IF                                                               
002779                                                                          
002780     PERFORM IMS-REPL-WDE101                                              
002781                                                                          
002782     IF IMS-DUBBEL-INDEX                                                  
002783       PERFORM UNTIL SEGMENT-FINNS                                        
002784         ADD +1                 TO SHIP-TISKPTID                          
002785         PERFORM IMS-REPL-WDE101                                          
002786       END-PERFORM                                                        
002787     END-IF                                                               
002788     .                                                                    
002789     EJECT                                                                
002790 R-TILLAGG-NOLL  SECTION.                                                 
002791     MOVE 'K-TILLAGG-NOLL'    TO WS-SEKTION                               
002792                                                                          
002793     IF SPAR-BGMT-REAVDRAG = ZERO  AND                                    
002794        SPAR-BGMT-REEMBHNT = ZERO  AND                                    
002795        SPAR-BGMT-REFOERS  = ZERO  AND                                    
002796        SPAR-BGMT-RELEGKST = ZERO  AND                                    
002797        SPAR-BGMT-REOVKOFF = ZERO                                         
002798        CONTINUE                                                          
002799     ELSE                                                                 
002800        MOVE ZERO               TO W-IDKUNDNR                             
002801        MOVE SPAR-BGMT-IDDISTR  TO W-IDDISTR                              
002802        PERFORM IMS-GHU-WDE211X                                           
002803                                                                          
002804        IF SEGMENT-FINNS                                                  
002805         IF DIST79-DEALER-PRICE                                           
002806           COMPUTE W-SUORDV-TOT = W-SUORDV-NOLL-LOC +                     
002807                                  W-SUORDV-NOLL-LOCPREL                   
002808         ELSE                                                             
002809           IF DIST79-ECOM-PRICE                                           
002810             MOVE W-SUORDV-NOLL-LOC TO W-SUORDV-TOT                       
002811           ELSE                                                           
002812             MOVE W-SUORDV-NOLL     TO W-SUORDV-TOT                       
002813           END-IF                                                         
002814         END-IF                                                           
002815         IF SPAR-BGMT-REEMBHNT NOT = BGMT-REEMBHNT                        
002816           MOVE SPAR-BGMT-REEMBHNT  TO BGMT-REEMBHNT                      
002817         END-IF                                                           
002818                                                                          
002819         PERFORM S06-RAKNA-TILLAGG-AVDRAG                                 
002820         PERFORM IMS-REPL-WDE211X                                         
002821         MOVE SPAR-NYCKEL         TO W-WDE111KY-X                         
002822         PERFORM S07-TILLAGG-WDE122                                       
002823        END-IF                                                            
002824     END-IF                                                               
002825     MOVE ZERO TO SPAR-BGMT-IDDISTR                                       
002826     .                                                                    
002827     EJECT                                                                
002828 S-SET-VOR-TO-READY SECTION.                                              
002829     SKIP2                                                                
002830                                                                          
002831     MOVE HIGH-VALUE        TO W-WDA601KY-MAX-X                           
002832     MOVE LOW-VALUE         TO W-WDA601KY-MIN-X                           
002833     MOVE KORD-IDDISTR      TO W-A601KY-MIN-IDDISTR                       
002834                               W-A601KY-MAX-IDDISTR                       
002835     MOVE KORD-IDKUNDNR     TO W-A601KY-MIN-IDKUNDNR                      
002836                               W-A601KY-MAX-IDKUNDNR                      
002837     MOVE SPACE             TO W-A601KY-MIN-IDKUNDRF                      
002838                               W-A601KY-MAX-IDKUNDRF                      
002839     MOVE KORD-IDORDNR5     TO W-A601KY-MIN-IDORDNR-7                     
002840                               W-A601KY-MAX-IDORDNR-7                     
002841     MOVE ORAD-IDARTNR      TO W-A601KY-MIN-IDARTNR                       
002842                               W-A601KY-MAX-IDARTNR                       
002843     MOVE KORD-TIORDREG     TO W-A601KY-MIN-TIREGDAT                      
002844                               W-A601KY-MAX-TIREGDAT                      
002845                                                                          
002846     PERFORM IMS-GHU-SEQB-WDA601                                          
002847     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
002848       IF VOR-KDVORATG = '2'                                              
002849       OR VOR-KDVORATG = '3'                                              
002850         MOVE WS-DAGENS-DATUM      TO VOR-TIKLAR                          
002851         COMPUTE VOR-TIKLATID      = WS-VOR-TID-KLAR                      
002852                               / 100                                      
002853         END-COMPUTE                                                      
002854                                                                          
002855         PERFORM IMS-REPL-SEQB-WDA601                                     
002856       END-IF                                                             
002857       PERFORM IMS-GHN-SEQB-WDA601                                        
002858     END-PERFORM                                                          
002859     .                                                                    
002860     EJECT                                                                
002861 V-EVENT-HANTERING SECTION.                                               
002862     MOVE 'V-EVENT-HANTERING' TO WS-SEKTION                               
002863                                                                          
002864     MOVE WS-IDSYSTEM                TO EVENT-SW                          
002865     IF EVENT-OK OR LYNK-NON-API                                          
002866                                                                          
002867       IF LYNK-NON-API                                                    
002868         PERFORM VB-CR-NON-API-EVENT                                      
002869       ELSE                                                               
002870***** TYP AV ORDER NÄR DET SKAPAS EVENT:                                  
002871                                                                          
002872**     'DÖSKALLAR'                                                        
002873*      -RO ANVÄNTS FÖR ATT LÄSA SEQC ->Q2-TIREGDAT FRÅN GAMLA ORD.        
002874                                                                          
002875         IF OHUV-IDSYSTEM(4:1) = 'D'                                      
002876                                                                          
002877           MOVE ORAD-IDKUNDRF-RO(1:5)  TO W-IDORDNR5-CSEQ                 
002878           MOVE OHUV-IDDISTR           TO W-IDDISTR-CSEQ                  
002879           MOVE OHUV-IDKUNDNR          TO W-IDKUNDNR-CSEQ                 
002880           PERFORM IMS-GU-WDQ201-CSEQ                                     
002881           IF SEGMENT-FINNS                                               
002882             MOVE CSEQ-OHUV-IDDISTR    TO WS-IDDISTR-EVENT                
002883             MOVE CSEQ-OHUV-IDKUNDNR   TO WS-IDKUNDNR-EVENT               
002884             MOVE CSEQ-OHUV-IDORDNR7   TO WS-IDORDNR7-EVENT               
002885             MOVE CSEQ-OHUV-TIREGDAT   TO WS-TIREGDAT-EVENT               
002886                                                                          
002887             MOVE JA                   TO SKAPA-EVENT-SW                  
002888                                                                          
002889           END-IF                                                         
002890         ELSE                                                             
002891**       VOR-ORDER                                                        
002892* *      LÄS WDA6B -> HÄMTA URSP. IDKUNDRF(1:7)+ TIREGDAT-URSP            
002893                                                                          
002894           IF OHUV-IDSYSTEM(4:1) = 'V'                                    
002895                                                                          
002896             MOVE LOW-VALUE            TO W-WDA6BSEQ-MIN-X                
002897             MOVE HIGH-VALUE           TO W-WDA6BSEQ-MAX-X                
002898                                                                          
002899             MOVE OHUV-IDDISTR         TO W-A6BSEQ-MIN-IDDISTR            
002900                                          W-A6BSEQ-MAX-IDDISTR            
002901             MOVE OHUV-IDKUNDNR        TO W-A6BSEQ-MIN-IDKUNDNR           
002902                                          W-A6BSEQ-MAX-IDKUNDNR           
002903             MOVE OHUV-IDKUNDRF       TO W-A6BSEQ-MIN-IDKUNDRF-LEV        
002904                                         W-A6BSEQ-MAX-IDKUNDRF-LEV        
002905             PERFORM IMS-GU-SEQB-WDA601                                   
002906             IF SEGMENT-FINNS                                             
002907               MOVE OHUV-IDDISTR       TO WS-IDDISTR-EVENT                
002908               MOVE OHUV-IDKUNDNR      TO WS-IDKUNDNR-EVENT               
002909               MOVE VOR-IDKUNDRF(1:7)  TO WS-IDORDNR7-EVENT               
002910               MOVE VOR-TIREGDAT-URSP  TO WS-TIREGDAT-EVENT               
002911                                                                          
002912               MOVE JA                 TO SKAPA-EVENT-SW                  
002913             END-IF                                                       
002914                                                                          
002915           ELSE                                                           
002916**         VERKSTADSORDER                                                 
002917*          LÄS WDA5 -> HÄMTA RAD-IDORDNR5 + RAD-TIREGDAT                  
002918                                                                          
002919             IF OHUV-IDSYSTEM(4:1) = 'B'                                  
002920                                                                          
002921               MOVE OHUV-IDDISTR       TO W-IDDISTR-A5-MIN                
002922                                          W-IDDISTR-A5-MAX                
002923               MOVE OHUV-IDKUNDNR      TO W-IDKUNDNR-A5-MIN               
002924                                          W-IDKUNDNR-A5-MAX               
002925               MOVE OHUV-KDORDKL       TO W-KDORDKL                       
002926               MOVE OHUV-IDORDNR7(3:5) TO W-IDKUNDRF-LEV                  
002927                                                                          
002928               PERFORM IMS-GU-WDA501                                      
002929               IF SEGMENT-FINNS                                           
002930                 MOVE OHUV-IDDISTR  TO WS-IDDISTR-EVENT                   
002931                 MOVE OHUV-IDKUNDNR TO WS-IDKUNDNR-EVENT                  
002932                 MOVE RAD-IDORDNR5  TO WS-IDORDNR7-EVENT                  
002933                 MOVE RAD-TIREGDAT  TO WS-TIREGDAT-EVENT                  
002934                                                                          
002935                 MOVE JA            TO SKAPA-EVENT-SW                     
002936               END-IF                                                     
002937                                                                          
002938             ELSE                                                         
002939**.          'VANLIGA' ORDER SOM SKALL SKAPA EVENT                        
002940                                                                          
002941               MOVE SKOLLI-IDDISTR  TO WS-IDDISTR-EVENT                   
002942               MOVE SKOLLI-IDKUNDNR TO WS-IDKUNDNR-EVENT                  
002943               MOVE SKOLLI-IDORDNR7 TO WS-IDORDNR7-EVENT                  
002944               MOVE SKOLLI-TIORDREG TO WS-TIREGDAT-EVENT                  
002945                                                                          
002946               MOVE JA              TO SKAPA-EVENT-SW                     
002947             END-IF                                                       
002948           END-IF                                                         
002949         END-IF                                                           
002950       END-IF                                                             
002951                                                                          
002952                                                                          
002953       IF SKAPA-EVENT OR LYNK-NON-API                                     
002954                                                                          
002955         IF (WS-IDSYST-1-3 = 'LYN') OR LYNK-NON-API                       
002956           MOVE 'L'               TO WS-PARTNER                           
002957         END-IF                                                           
002958         IF WS-IDSYST-1-3 = 'POL'                                         
002959           MOVE 'P'               TO WS-PARTNER                           
002960         END-IF                                                           
002961         IF WS-IDSYST-1-3 = 'ECO'                                         
002962           MOVE 'E'               TO WS-PARTNER                           
002963         END-IF                                                           
002964                                                                          
002965         IF WS-IDSYST-1-3 = 'TAD'                                         
002966           MOVE 'T'               TO WS-PARTNER                           
002967         END-IF                                                           
002968                                                                          
002969         IF WS-IDSYST-1-3 = 'ACC'                                         
002970           MOVE 'A'               TO WS-PARTNER                           
002971         END-IF                                                           
002972         IF WS-IDSYST-1-3 = 'APA'                                         
002973           MOVE 'K'            TO WS-PARTNER                              
002974         END-IF                                                           
002975         IF WS-IDSYST-1-3 = 'APB'                                         
002976           MOVE 'B'            TO WS-PARTNER                              
002977         END-IF                                                           
002978         IF WS-IDSYST-1-3 = 'APC'                                         
002979           MOVE 'C'            TO WS-PARTNER                              
002980         END-IF                                                           
002981         IF WS-IDSYST-1-3 = 'APD'                                         
002982           MOVE 'D'            TO WS-PARTNER                              
002983         END-IF                                                           
002984         IF WS-IDSYST-1-3 = 'APE'                                         
002985           MOVE 'M'            TO WS-PARTNER                              
002986         END-IF                                                           
002987         IF WS-IDSYST-1-3 = 'APF'                                         
002988           MOVE 'F'            TO WS-PARTNER                              
002989         END-IF                                                           
002990         IF WS-IDSYST-1-3 = 'APG'                                         
002991           MOVE 'G'            TO WS-PARTNER                              
002992         END-IF                                                           
002993         IF WS-IDSYST-1-3 = 'APH'                                         
002994           MOVE 'H'            TO WS-PARTNER                              
002995         END-IF                                                           
002996         IF WS-IDSYST-1-3 = 'API'                                         
002997           MOVE 'I'            TO WS-PARTNER                              
002998         END-IF                                                           
002999         IF WS-IDSYST-1-3 = 'APJ'                                         
003000           MOVE 'J'            TO WS-PARTNER                              
003001         END-IF                                                           
003002                                                                          
003003* KOLLA HUR VI GÖR MED INFO NEDAN, ÄR DET OK?                             
003004         IF WS-IDDISTR-EVENT  = WS-IDDISTR-SPAR  AND                      
003005            WS-IDKUNDNR-EVENT = WS-IDKUNDNR-SPAR AND                      
003006            WS-IDORDNR7-EVENT = WS-IDORDNR7-SPAR                          
003007*  VI SKICKAR EN EVENT PER ORDER->OM DET FINNS FLER RADER                 
003008           CONTINUE                                                       
003009                                                                          
003010         ELSE                                                             
003011           MOVE SPACE             TO Z430-REQU-TIMESTAMP                  
003012                                     Z430-REQU-IDEVENTREC                 
003013           PERFORM VA-SKAPA-EVENT                                         
003014                                                                          
003015           MOVE NEJ               TO SKAPA-EVENT-SW                       
003016           MOVE WS-IDDISTR-EVENT  TO WS-IDDISTR-SPAR                      
003017           MOVE WS-IDKUNDNR-EVENT TO WS-IDKUNDNR-SPAR                     
003018           MOVE WS-IDORDNR7-EVENT TO WS-IDORDNR7-SPAR                     
003019         END-IF                                                           
003020       END-IF                                                             
003021     END-IF                                                               
003022     .                                                                    
003023     EJECT                                                                
003024 VA-SKAPA-EVENT SECTION.                                                  
003025     MOVE 'VA-SKAPA-EVENT'      TO WS-SEKTION                             
003026                                                                          
003027     MOVE '001'                 TO Z430-REQU-IDMSGVER                     
003028                                                                          
003029     IF LYNK-NON-API                                                      
003030        MOVE 'LYNK'             TO Z430-REQU-IDEVENTREC                   
003031     ELSE                                                                 
003032        MOVE WS-IDSYSTEM        TO Z430-REQU-IDEVENTREC                   
003033     END-IF                                                               
003034                                                                          
003035     MOVE 'PURCHASEORDER'       TO Z430-REQU-IDEVENT                      
003036     MOVE 'UPDATE'              TO Z430-REQU-IDEVENTTYP                   
003037     MOVE FUNCTION CURRENT-DATE TO Z430-REQU-TIMESTAMP                    
003038     MOVE 'WAPIORD'             TO Z430-REQU-IDCPYTXT                     
003039     MOVE WS-IDEVENTORDREF      TO Z430-IDAPIORDREF                       
003040     MOVE '154'                 TO Z430-IDMSG                             
003041     MOVE 'SOME PART/ALL HAS BEEN SHIPPED'                                
003042                                TO Z430-TEMFSINF                          
003043                                                                          
003044*    -- INITIALIZE W006KOM FIELDS WITH VARIABLE CONTENT                   
003045*    -- FIXED DATA HAS BEEN SET IN A-INIT                                 
003046     MOVE 'WZ0430X '           TO MSG-KDTRANS-1                           
003047     MOVE 'Z430'               TO MSG-IDTRANS-1                           
003048     MOVE '1'                  TO MSG-KDMFSFOR-1                          
003049     MOVE 'WZ0430I1'           TO MSG-KOM-IDCPYTXT                        
003050     STRING 'EVE' WS-PARTNER WS-IDDISTR-EVENT                             
003051          DELIMITED BY SIZE INTO MSG-KOM-IDSNDNOD                         
003052                                                                          
003053     ADD  1                    TO MSG-KOM-TIKLOCK                         
003054     COMPUTE MSG-KVLL = LENGTH OF Z430-REQU-WZ0430I1 + 17                 
003055     MOVE Z430-REQU-WZ0430I1     TO MSG-INDATA-MINUS-1-TRANSKOD           
003056                                                                          
003057     CALL W006KOM USING IO-PCB                                            
003058                        0693-PCB                                          
003059                        KOM-WDP8-PCB                                      
003060                        MSG-KOM-WMSGKOM                                   
003061                        MSG-IO-AREA                                       
003062                                                                          
003063     IF MSG-KOM-IDMFSMED NOT = SPACE                                      
003064        MOVE                                                              
003065        'FELAKTIG UPPDATERING AV PÅ KOMMUNIKATIONS DB'                    
003066                                     TO FELTEXT                           
003067        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
003068     END-IF                                                               
003069     .                                                                    
003070     EJECT                                                                
003071 VB-CR-NON-API-EVENT SECTION.                                             
003072     MOVE 'VB-CR-NON-API-EV'    TO WS-SEKTION                             
003073                                                                          
003074     MOVE NEJ                   TO SKAPA-EVENT-SW                         
003075     IF VOR                                                               
003076       MOVE LOW-VALUE           TO W-WDA6BSEQ-MIN-X                       
003077       MOVE HIGH-VALUE          TO W-WDA6BSEQ-MAX-X                       
003078                                                                          
003079       MOVE OHUV-IDDISTR        TO W-A6BSEQ-MIN-IDDISTR                   
003080                                   W-A6BSEQ-MAX-IDDISTR                   
003081       MOVE OHUV-IDKUNDNR       TO W-A6BSEQ-MIN-IDKUNDNR                  
003082                                   W-A6BSEQ-MAX-IDKUNDNR                  
003083       MOVE OHUV-IDKUNDRF       TO W-A6BSEQ-MIN-IDKUNDRF-LEV              
003084                                   W-A6BSEQ-MAX-IDKUNDRF-LEV              
003085       PERFORM IMS-GU-SEQB-WDA601                                         
003086       IF SEGMENT-FINNS                                                   
003087         MOVE OHUV-IDDISTR      TO WS-IDDISTR-EVENT                       
003088         MOVE OHUV-IDKUNDNR     TO WS-IDKUNDNR-EVENT                      
003089         MOVE VOR-IDKUNDRF(1:7) TO WS-IDORDNR7-EVENT                      
003090         MOVE VOR-TIREGDAT-URSP TO WS-TIREGDAT-EVENT                      
003091                                                                          
003092         MOVE JA                TO SKAPA-EVENT-SW                         
003093       END-IF                                                             
003094     ELSE                                                                 
003095       IF (ORAD-IDKUNDRF-RO NOT = '0000000   ') AND                       
003096          (ORAD-IDKUNDRF-RO NOT = '00000     ')                           
003097          MOVE ORAD-IDKUNDRF-RO(1:5)  TO W-IDORDNR5-CSEQ                  
003098          MOVE OHUV-IDDISTR           TO W-IDDISTR-CSEQ                   
003099          MOVE OHUV-IDKUNDNR          TO W-IDKUNDNR-CSEQ                  
003100          PERFORM IMS-GU-WDQ201-CSEQ                                      
003101          IF SEGMENT-FINNS                                                
003102            MOVE CSEQ-OHUV-IDDISTR    TO WS-IDDISTR-EVENT                 
003103            MOVE CSEQ-OHUV-IDKUNDNR   TO WS-IDKUNDNR-EVENT                
003104            MOVE CSEQ-OHUV-IDORDNR7   TO WS-IDORDNR7-EVENT                
003105            MOVE CSEQ-OHUV-TIREGDAT   TO WS-TIREGDAT-EVENT                
003106                                                                          
003107            MOVE JA                   TO SKAPA-EVENT-SW                   
003108                                                                          
003109          END-IF                                                          
003110       ELSE                                                               
003111          MOVE OHUV-IDDISTR           TO W-IDDISTR-A5-MIN                 
003112                                         W-IDDISTR-A5-MAX                 
003113          MOVE OHUV-IDKUNDNR          TO W-IDKUNDNR-A5-MIN                
003114                                         W-IDKUNDNR-A5-MAX                
003115          MOVE OHUV-KDORDKL           TO W-KDORDKL                        
003116          MOVE OHUV-IDORDNR7(3:5)     TO W-IDKUNDRF-LEV                   
003117                                                                          
003118          PERFORM IMS-GU-WDA501                                           
003119          IF SEGMENT-FINNS                                                
003120            MOVE OHUV-IDDISTR         TO WS-IDDISTR-EVENT                 
003121            MOVE OHUV-IDKUNDNR        TO WS-IDKUNDNR-EVENT                
003122            MOVE RAD-IDORDNR5         TO WS-IDORDNR7-EVENT                
003123            MOVE RAD-TIREGDAT         TO WS-TIREGDAT-EVENT                
003124                                                                          
003125            MOVE JA                   TO SKAPA-EVENT-SW                   
003126          END-IF                                                          
003127       END-IF                                                             
003128     END-IF                                                               
003129     IF SKAPA-EVENT-SW = NEJ                                              
003130       MOVE SKOLLI-IDDISTR            TO WS-IDDISTR-EVENT                 
003131       MOVE SKOLLI-IDKUNDNR           TO WS-IDKUNDNR-EVENT                
003132       MOVE SKOLLI-IDORDNR7           TO WS-IDORDNR7-EVENT                
003133       MOVE SKOLLI-TIORDREG           TO WS-TIREGDAT-EVENT                
003134                                                                          
003135       MOVE JA                        TO SKAPA-EVENT-SW                   
003136     END-IF                                                               
003137     .                                                                    
003138     EJECT                                                                
003139 X-STARTA-OM-4636   SECTION.                                              
003140     MOVE 'X-STARTA-OM-4636'    TO WS-SEKTION                             
003141                                                                          
003142*           STARTAR UPP W4636 (ADDIT)                                     
003143     MOVE ZERO                  TO 4636-MID-W40636I1                      
003144     MOVE MID-IDSHIPM           TO 4636-MID-IDSHIPM                       
003145     MOVE MID-KDTRPINF          TO 4636-MID-KDTRPINF                      
003146     MOVE BGMT-IDDISTR          TO 4636-MID-IDDISTR                       
003147     MOVE BGMT-IDKUNDNR         TO 4636-MID-IDKUNDNR                      
003148     MOVE BKOLLI-IDPRODNR       TO 4636-MID-IDPRODNR                      
003149     MOVE BKOLLI-IDKOLLI        TO 4636-MID-IDKOLLI                       
003150     MOVE W-SUORDV-DIST         TO 4636-MID-SUORDV-DIST                   
003151     MOVE W-SUORDV-DIST-LOC     TO 4636-MID-SUORDV-DIST-LOC               
003152     MOVE W-SUORDV-DIST-LOCPREL TO 4636-MID-SUORDV-DIST-PREL              
003153     MOVE W-SUORDV-NOLL         TO 4636-MID-SUORDV-NOLL                   
003154     MOVE W-SUORDV-NOLL-LOC     TO 4636-MID-SUORDV-NOLL-LOC               
003155     MOVE W-SUORDV-NOLL-LOCPREL TO 4636-MID-SUORDV-NOLL-PREL              
003156     MOVE WS-KDORDKL-MAX        TO 4636-MID-KDORDKL                       
003157                                                                          
003158     MOVE 4  TO WS-PGM                                                    
003159     PERFORM S01-OPEN-WZ01                                                
003160     PERFORM S02B-SEND-WZ01                                               
003161     PERFORM S03-CLOSE-WZ01                                               
003162                                                                          
003163     MOVE ZERO                      TO W-CHKP                             
003164                                                                          
003165* BEFORE RESTARTING OWN TRANS MAKE A 2 SECONDS WAIT TO AVOID              
003166* CONFLICT OF TIME STAMP FOR WDP8 THAT CAN OVERRIDE ALREADY USED          
003167* TIME IN PREVIOUS TRANS FOR SAME SHIPMENT                                
003168     IF EVENT-OK OR LYNK-NON-API                                          
003169       CALL W009WAIT USING 2-SECONDS                                      
003170     END-IF                                                               
003171     .                                                                    
003172     EJECT                                                                
003173 Z-FINIT  SECTION.                                                        
003174     MOVE 'Z-FINIT'             TO WS-SEKTION                             
003175                                                                          
003176     IF WS-IDLEVNR = SPACE OR = '00000'                                   
003177       IF UPPDAT-IDDC-EXP                                                 
003178         PERFORM IMS-GHU-WDE201                                           
003179         IF SEGMENT-FINNS                                                 
003180           MOVE WS-IDDC-EXP     TO BILL-IDDC-EXP                          
003181           PERFORM IMS-REPL-WDE201                                        
003182         END-IF                                                           
003183       ELSE                                                               
003184         CONTINUE                                                         
003185       END-IF                                                             
003186     ELSE                                                                 
003187       PERFORM IMS-GHU-WDE201                                             
003188       IF SEGMENT-FINNS                                                   
003189         MOVE WS-IDLEVNR        TO BILL-IDLEVNR                           
003190         IF UPPDAT-IDDC-EXP                                               
003191           MOVE WS-IDDC-EXP     TO BILL-IDDC-EXP                          
003192         END-IF                                                           
003193         PERFORM IMS-REPL-WDE201                                          
003194       END-IF                                                             
003195     END-IF                                                               
003196                                                                          
003197*           STARTAR UPP W4637 (BUILDIT)                                   
003198     MOVE ZERO                  TO MOD-MID-W40637I1                       
003199     MOVE MID-IDSHIPM           TO MOD-MID-IDSHIPM                        
003200                                                                          
003201     IF MID-KDTRPINF NOT = KODL                                           
003202       MOVE 1  TO WS-PGM                                                  
003203       PERFORM S01-OPEN-WZ01                                              
003204       PERFORM S02A-SEND-WZ01                                             
003205       PERFORM S03-CLOSE-WZ01                                             
003206     END-IF                                                               
003207                                                                          
003208     IF MID-KDTRPINF = KODL                                               
003209       CONTINUE                                                           
003210     ELSE                                                                 
003211       IF MID-KDTRPINF = JA OR                                            
003212         (DIS107-BYGG-SHIP AND NOT DIS105-NA-BYT)                         
003213                                                                          
003214*---      STARTAR UPP 4631 (SHIPDOK):                                     
003215*         * OM VANLIGT FLÖDE:                                             
003216*---        * SHIP-KDFAKSTA-EXP = 0 ELLER SPACE                           
003217*                                                                         
003218         IF BILL-KDFINDOC = 'PROF' OR MID-KDTRPINF = KODL                 
003219           CONTINUE                                                       
003220         ELSE                                                             
003221           IF SHIP-KDFAKSTA-EXP = 0 OR SPACE                              
003222             IF ((SHIP-IDDC = WC-NDC-AE)      AND                         
003223                   (DIST18-SCRAP-NDC-SC       OR                          
003224                    DIST18-SCRAP-NDC-QUAL     OR                          
003225                    DIST18-SCRAP-NDC-SC-LOCAL OR                          
003226                    DIST20-EMBALLAGE-SDC ))                               
003227                                              OR                          
003228                    DIST35-AE-CDC-RETURNS     OR                          
003229                    DIST35-CDC-AE-REFILL                                  
003230*---           FÖR REFILL TILL DUBAI OCH RETURER FRÅN DUBAI,              
003231*---           STARTAR MAN INTE UPP 4631 EFTERSOM MAN STARTAR             
003232*---           STARTAR UPP DET EFTER FAKT!                                
003233*---           * FOR REFILL TO DUBAI AND RETURNS FROM DUBAI,              
003234*---           * 4631 WILL NOT START UNTIL AFTER THE INVOICING!           
003235*                                                                         
003236               CONTINUE                                                   
003237             ELSE                                                         
003238               MOVE 2 TO WS-PGM                                           
003239               PERFORM S01-OPEN-WZ01                                      
003240               PERFORM S02-SEND-WZ01                                      
003241               PERFORM S03-CLOSE-WZ01                                     
003242             END-IF                                                       
003243           END-IF                                                         
003244         END-IF                                                           
003245*                                                                         
003246*---      STARTAR INTE UPP W4631 (SHIPDOK):                               
003247*         * OM STUDS-FLÖDE (DUBBELFAKT.) FÖR GLOBAL EXPORT                
003248*           REFILL. (4631 STARTAS I DETTA FALL EFTER ANDRA                
003249*           FAKT. I PGM. 4634):                                           
003250*---        * SHIP-KDFAKSTA-EXP = 1                                       
003251*                                                                         
003252*---      UNDANTAG FRÅN DENNA REGEL --> STARTAR ÄNDÅ UPP 4631             
003253*         * OM VOR SOM GÅR FRÅN DC 11 TILL ETT ANNAT FÖRETAG              
003254*           (T.EX. KINA, INDIEN) OCH SOM I DETTA FALL ÄR ETT              
003255*           STUDS (DUBBELFAKT.) FLÖDE:                                    
003256*---        * SHIP-KDSTA-EXP = 1 OCH STUDSEN SKER INTE I DC 11            
003257*                                                                         
003258         IF BILL-KDFINDOC = 'PROF' OR MID-KDTRPINF = KODL                 
003259           CONTINUE                                                       
003260         ELSE                                                             
003261           IF SHIP-KDFAKSTA-EXP = 1 AND                                   
003262             (SHIP-IDDC-EXP NOT = WC-CDC-SE)                              
003263             MOVE 2 TO WS-PGM                                             
003264             PERFORM S01-OPEN-WZ01                                        
003265             PERFORM S02-SEND-WZ01                                        
003266             PERFORM S03-CLOSE-WZ01                                       
003267           END-IF                                                         
003268         END-IF                                                           
003269*                                                                         
003270       END-IF                                                             
003271     END-IF                                                               
003272                                                                          
003273     IF MID-KDTRPINF = JA                                                 
003274*           STARTAR UPP W4632 (SHIPIT )                                   
003275       MOVE 3  TO WS-PGM                                                  
003276       PERFORM S01-OPEN-WZ01                                              
003277       PERFORM S02-SEND-WZ01                                              
003278       PERFORM S03-CLOSE-WZ01                                             
003279     END-IF                                                               
003280                                                                          
003281     IF MID-KDTRPINF = KODL                                               
003282*           STARTAR UPP W4674                                             
003283       MOVE 5  TO WS-PGM                                                  
003284       PERFORM S03A-P2P-4674                                              
003285     END-IF                                                               
003286                                                                          
003287     .                                                                    
003288     EJECT                                                                
003289 S01-OPEN-WZ01 SECTION.                                                   
003290     MOVE 'S01-OPEN-WZ01'       TO WS-SEKTION                             
003291                                                                          
003292     MOVE 'OPEN'                     TO SEND-KDFUNC                       
003293     EVALUATE WS-PGM                                                      
003294       WHEN 1                                                             
003295         MOVE 'CARPARTS.PULS.BUILDIT '   TO SEND-ADDISPABS                
003296       WHEN 2                                                             
003297         MOVE 'CARPARTS.PULS.SHIPDOK '   TO SEND-ADDISPABS                
003298       WHEN 3                                                             
003299         MOVE 'CARPARTS.PULS.SHIPIT'     TO SEND-ADDISPABS                
003300       WHEN 4                                                             
003301         MOVE 'CARPARTS.PULS.ADDIT '     TO SEND-ADDISPABS                
003302       WHEN OTHER                                                         
003303         MOVE 'CARPARTS.PULS.BUILDIT '   TO SEND-ADDISPABS                
003304     END-EVALUATE                                                         
003305     MOVE SPACE                      TO SEND-ADDISPABS-RETURN             
003306                                                                          
003307     CALL WZ01SEND   USING      SEND-CONTROL-AREA                         
003308                                SEND-OPEN-AREA                            
003309     IF SEND-KDRC > 0                                                     
003310       MOVE SEND-KDRC           TO KDRC-DISP                              
003311       STRING 'WZ01SEND-OPEN RC-ERR = ' KDRC-DISP                         
003312            DELIMITED BY SIZE INTO FELTEXT                                
003313       CALL FELLOG                                                        
003314     ELSE                                                                 
003315       MOVE SEND-IDCOM               TO WS-IDCOM                          
003316     END-IF                                                               
003317     .                                                                    
003318     EJECT                                                                
003319 S02-SEND-WZ01 SECTION.                                                   
003320     MOVE 'S02-SEND-WZ01'    TO WS-SEKTION                                
003321                                                                          
003322     MOVE 'PUT'                      TO SEND-KDFUNC                       
003323     EVALUATE WS-PGM                                                      
003324       WHEN 2                                                             
003325        COMPUTE SEND-KVDLEN = LENGTH OF 4631-MID-W4I63101                 
003326        MOVE MID-IDSHIPM        TO 4631-MID-IDSHIPM                       
003327        MOVE SPACE              TO 4631-MID-IDPRTLST                      
003328        MOVE SPACE              TO 4631-MID-IDDC-REC                      
003329        MOVE IDPGM              TO 4631-MID-IDPGM                         
003330                                                                          
003331        CALL WZ01SEND   USING      SEND-CONTROL-AREA                      
003332                                   SEND-KVDLEN                            
003333                                   4631-MID-W4I63101                      
003334       WHEN 3                                                             
003335         COMPUTE SEND-KVDLEN = LENGTH OF 4632-MID-W40632I1                
003336         MOVE MID-IDSHIPM       TO 4632-MID-IDSHIPM                       
003337                                                                          
003338         CALL WZ01SEND   USING      SEND-CONTROL-AREA                     
003339                                    SEND-KVDLEN                           
003340                                    4632-MID-W40632I1                     
003341       WHEN 5                                                             
003342         COMPUTE SEND-KVDLEN = LENGTH OF 4674-MID-W4I67401                
003343         MOVE SPACE             TO 4674-MID-W4I67401                      
003344         MOVE MID-IDSHIPM       TO 4674-MID-IDSHIPM                       
003345         MOVE SHIP-IDTRPTNR     TO WX-IDTRPTNR                            
003346         MOVE W-IDTRPTNR        TO 4674-MID-IDTRPTNR                      
003347         MOVE SHIP-IDLBBET      TO 4674-MID-IDLBBET-HUV                   
003348         MOVE SHIP-IDDC         TO 4674-MID-IDDC                          
003349         MOVE '4636'            TO 4674-MID-FRAN-IDTRANS                  
003350                                                                          
003351     END-EVALUATE                                                         
003352     IF SEND-KDRC > 0                                                     
003353       MOVE SEND-KDRC           TO KDRC-DISP                              
003354       STRING 'WZ01SEND-PUT RC-ERR = ' KDRC-DISP                          
003355            DELIMITED BY SIZE INTO FELTEXT                                
003356       CALL FELLOG                                                        
003357     END-IF                                                               
003358     .                                                                    
003359     EJECT                                                                
003360 S02A-SEND-WZ01 SECTION.                                                  
003361     MOVE 'S02A-SEND-WZ01'      TO WS-SEKTION                             
003362                                                                          
003363     MOVE 'PUT'                 TO SEND-KDFUNC                            
003364     COMPUTE SEND-KVDLEN = LENGTH OF MOD-MID-W40637I1                     
003365                                                                          
003366     CALL WZ01SEND   USING      SEND-CONTROL-AREA                         
003367                                SEND-KVDLEN                               
003368                                MOD-MID-W40637I1                          
003369     IF SEND-KDRC > 0                                                     
003370       MOVE SEND-KDRC           TO KDRC-DISP                              
003371       STRING 'WZ01SEND-PUT RC-ERR = ' KDRC-DISP                          
003372            DELIMITED BY SIZE INTO FELTEXT                                
003373       CALL FELLOG                                                        
003374     END-IF                                                               
003375     .                                                                    
003376     EJECT                                                                
003377 S02B-SEND-WZ01 SECTION.                                                  
003378     MOVE 'S02B-SEND-WZ01'      TO WS-SEKTION                             
003379                                                                          
003380     MOVE 'PUT'                 TO SEND-KDFUNC                            
003381     COMPUTE SEND-KVDLEN = LENGTH OF 4636-MID-W40636I1                    
003382                                                                          
003383     CALL WZ01SEND   USING      SEND-CONTROL-AREA                         
003384                                SEND-KVDLEN                               
003385                                4636-MID-W40636I1                         
003386     IF SEND-KDRC > 0                                                     
003387       MOVE SEND-KDRC           TO KDRC-DISP                              
003388       STRING 'WZ01SEND-PUT RC-ERR = ' KDRC-DISP                          
003389            DELIMITED BY SIZE INTO FELTEXT                                
003390       CALL FELLOG                                                        
003391     END-IF                                                               
003392     .                                                                    
003393     EJECT                                                                
003394 S03-CLOSE-WZ01  SECTION.                                                 
003395     MOVE 'S03-CLOSE-WZ01'      TO WS-SEKTION                             
003396                                                                          
003397     MOVE 'CLOSE'               TO SEND-KDFUNC                            
003398                                                                          
003399     CALL WZ01SEND   USING      SEND-CONTROL-AREA                         
003400     IF SEND-KDRC > 0                                                     
003401       MOVE SEND-KDRC           TO KDRC-DISP                              
003402       STRING 'WZ01SEND-CLOSE RC-ERR = ' KDRC-DISP                        
003403            DELIMITED BY SIZE INTO FELTEXT                                
003404       CALL FELLOG                                                        
003405     END-IF                                                               
003406     .                                                                    
003407     EJECT                                                                
003408 S03A-P2P-4674  SECTION.                                                  
003409     MOVE LOW-VALUE            TO P-TO-P-KDZ1                             
003410     MOVE LOW-VALUE            TO P-TO-P-KDZ2                             
003411     MOVE SPACE                TO 4674-MID-W4I67401                       
003412     MOVE MID-IDSHIPM          TO 4674-MID-IDSHIPM                        
003413     MOVE SHIP-IDTRPTNR        TO WX-IDTRPTNR                             
003414     MOVE W-IDTRPTNR           TO 4674-MID-IDTRPTNR                       
003415     MOVE SHIP-IDLBBET         TO 4674-MID-IDLBBET-HUV                    
003416     MOVE SHIP-IDDC            TO 4674-MID-IDDC                           
003417     MOVE '4636'               TO 4674-MID-FRAN-IDTRANS                   
003418                                                                          
003419     MOVE 'W4T674  '           TO P-TO-P-KDTRANS                          
003420     MOVE '4636'               TO P-TO-P-IDTRANS                          
003421     MOVE '1'                  TO P-TO-P-KDMFSFOR                         
003422     COMPUTE P-TO-P-KVLL =                                                
003423        LENGTH OF 4674-MID-W4I67401 + 17                                  
003424                                                                          
003425     PERFORM IMS-ISRT-ALT-MSG-4674                                        
003426     .                                                                    
003427     EJECT                                                                
003428 S04-PRIS-W335PRIS  SECTION.                                              
003429     MOVE 'S04-PRIS-W335PRIS'   TO WS-SEKTION                             
003430                                                                          
003431     IF NOT DIST79-DEALER-PRICE AND                                       
003432        NOT DIST79-ECOM-PRICE                                             
003433       MOVE 1                    TO PRIS-KDCALL                           
003434       MOVE IDPGM                TO PRIS-IDPGM                            
003435       MOVE ORAD-IDARTNR         TO PRIS-IDARTNR                          
003436       MOVE BKOLLI-IDDISTR       TO PRIS-IDDISTR                          
003437       MOVE BKOLLI-IDKUNDNR      TO PRIS-IDKUNDNR                         
003438       MOVE BILL-IDDC            TO PRIS-IDDC                             
003439       MOVE ORAD-KDORDKL         TO PRIS-KDORDKL                          
003440       MOVE ORAD-KVBEART         TO PRIS-KVBEART                          
003441       MOVE ORAD-FLINVEST        TO PRIS-FLINVEST                         
003442                                                                          
003443       CALL W335PRIS USING PRIS-W335PRIS  PRIS-ARTC-PCB                   
003444                           PRIS-WDK7-PCB                                  
003445                           PRIS-GMTA-PCB  PRIS-BETA-PCB                   
003446                           PRIS-GPRIA-PCB PRIS-GPRIB-PCB                  
003447                           PRIS-COST-WDK6-PCB                             
003448                           PRIS-COST-WDK7-PCB                             
003449                           PRIS-COST-WDF1-PCB                             
003450                           PRIS-COST-9305-PCB                             
003451                           PRIS-COST-WDK72-PCB                            
003452                           PRIS-COST-WDB6-PCB                             
003453                                                                          
003454       IF PRIS-KDSVAR = '2'                                               
003455          MOVE 'DIST/KUND SAKNAS NÄR W335PRIS LÄSER KUNDREG'              
003456                              TO FELTEXT                                  
003457          CALL ABEND USING RKOD-ABEND-MED-DUMP                            
003458       END-IF                                                             
003459                                                                          
003460       MOVE PRIS-KDARTRAB        TO BRAD-KDARTRAB                         
003461       MOVE PRIS-PRARTBTO-MARK   TO BRAD-PRARTBTO-EXP                     
003462     END-IF                                                               
003463     .                                                                    
003464     EJECT                                                                
003465 S05-SKAPA-PLATSAVBOKN SECTION.                                           
003466     MOVE 'S05-SKAPA-PLATSAV'   TO WS-SEKTION                             
003467*    (KOLLI-SEGM FRÅN E611,  3=AVBOKA)                                    
003468                                                                          
003469     IF BILL-KDFINDOC = 'PROF' OR MID-KDTRPINF = KODL                     
003470       CONTINUE                                                           
003471     ELSE                                                                 
003472       MOVE +3                    TO PLATS-KDCALL                         
003473       MOVE KOLLI-IDTRPTNR        TO PLATS-IDTRPTNR                       
003474       MOVE ZERO                  TO PLATS-TIRFS                          
003475       MOVE KOLLI-ADCLGEO         TO PLATS-ADCLGEO                        
003476       MOVE KOLLI-ADFLOMR         TO PLATS-ADFLOMR                        
003477       MOVE KOLLI-ADRUTNIV        TO PLATS-ADRUTNIV                       
003478       MOVE KOLLI-ADVMODUL        TO PLATS-ADVMODUL                       
003479       MOVE KOLLI-ADHMODUL        TO PLATS-ADHMODUL                       
003480       MOVE KOLLI-DIDMODUL        TO PLATS-DIDMODUL                       
003481       MOVE KOLLI-DIHMODUL        TO PLATS-DIHMODUL                       
003482                                                                          
003483       CALL W403PLAT USING PLATS-W403PLAT                                 
003484                           PLATS-DM-PCB                                   
003485                           PLATS-DN-PCB                                   
003486                           PLATS-DP-PCB                                   
003487                           PLATS-DO-PCB                                   
003488                           PLATS-WDE6C-PCB                                
003489                           PLATS-GMTC-PCB                                 
003490                           PLATS-WDB6-PCB                                 
003491                                                                          
003492*      (PCB OVAN-- WDR1,WDR1,WDR1,WDR1,WDE6,WDB5)                         
003493     END-IF                                                               
003494     .                                                                    
003495     EJECT                                                                
003496 S06-RAKNA-TILLAGG-AVDRAG  SECTION.                                       
003497     MOVE 'S06-RAKNA-TILLAGG'   TO WS-SEKTION                             
003498                                                                          
003499     IF BGMT-PRLEGKST = -1                                                
003500       MOVE ZERO TO BGMT-PRLEGKST                                         
003501     END-IF                                                               
003502     IF BGMT-PRFRAKT = -1                                                 
003503       MOVE ZERO TO BGMT-PRFRAKT                                          
003504     END-IF                                                               
003505     MOVE BGMT-PRFRAKT             TO W-PRFRAKT                           
003506                                                                          
003507     MOVE BGMT-PRLEGKST            TO W-PRLEGKST                          
003508     MOVE BGMT-RELEGKST            TO W-RELEGKST                          
003509     IF W-PRLEGKST = ZERO AND W-RELEGKST NOT = ZERO                       
003510        COMPUTE W-PRLEGKST ROUNDED =                                      
003511             W-SUORDV-TOT * W-RELEGKST-2                                  
003512     END-IF                                                               
003513                                                                          
003514     IF SHIP-IDLBBET = 'SOFTW       '  OR                                 
003515                       'SOFTAUTF    '                                     
003516       MOVE ZERO                   TO GMT-REAVDRAG                        
003517                                      GMT-REEMBHNT                        
003518     END-IF                                                               
003519                                                                          
003520*    KONTROLL FÖR ADMINISTRATIVA BYTESARTIKLAR SOM EJ SKALL HA P&H        
003521     IF OHUV-IDSYSTEM = 'W37A'                                            
003522       MOVE ZERO                   TO GMT-REAVDRAG                        
003523                                      GMT-REEMBHNT                        
003524     END-IF                                                               
003525                                                                          
003526     IF BGMT-PRAVDRAG = -1                                                
003527       MOVE ZERO TO BGMT-PRAVDRAG                                         
003528     END-IF                                                               
003529     IF BGMT-REAVDRAG NOT > ZERO                                          
003530       IF GMT-REAVDRAG > ZERO                                             
003531         MOVE GMT-REAVDRAG         TO BGMT-REAVDRAG                       
003532       END-IF                                                             
003533     END-IF                                                               
003534     MOVE BGMT-PRAVDRAG            TO W-PRAVDRAG                          
003535     MOVE BGMT-REAVDRAG            TO W-REAVDRAG                          
003536     IF W-PRAVDRAG = ZERO AND W-REAVDRAG NOT = ZERO                       
003537        COMPUTE W-PRAVDRAG ROUNDED =                                      
003538             W-SUORDV-TOT * W-REAVDRAG-2                                  
003539     END-IF                                                               
003540                                                                          
003541     IF BGMT-PREMBHNT = -1                                                
003542       MOVE ZERO TO BGMT-PREMBHNT                                         
003543     END-IF                                                               
003544     IF BGMT-REEMBHNT NOT > ZERO                                          
003545       IF GMT-REEMBHNT > ZERO                                             
003546         IF SPAR-BGMT-IDDISTR NOT = ZERO                                  
003547           MOVE GMT-REEMBHNT       TO SPAR-BGMT-REEMBHNT                  
003548         ELSE                                                             
003549           MOVE GMT-REEMBHNT       TO BGMT-REEMBHNT                       
003550         END-IF                                                           
003551       END-IF                                                             
003552     END-IF                                                               
003553     MOVE BGMT-PREMBHNT            TO W-PREMBHNT                          
003554     MOVE BGMT-REEMBHNT            TO W-REEMBHNT                          
003555                                                                          
003556****   MAN VILL STYRA SÅ ATT BARA KINA-VOR FRÅN DC 11 FÅR                 
003557****   ANVÄNDA P&H SOM FINNS PÅ KUNDREG.                                  
003558****   OBS! DET SKALL INTE PÅVERKA KINESISKA DEALER-DISTRIKT              
003559****   => DE SKALL INTE BELASTAS MED P&H                                  
003560*                                                                         
003561     MOVE SHIP-IDDC                TO WS-IDDC                             
003562                                                                          
003563     IF NDC-CN AND GMT-KDKUNDKAT = WS-DEALER                              
003564                                                                          
003565       MOVE ZERO                   TO W-REEMBHNT                          
003566                                      SPAR-BGMT-REEMBHNT                  
003567                                      BGMT-REEMBHNT                       
003568     END-IF                                                               
003569                                                                          
003570                                                                          
003571     IF W-REEMBHNT > ZERO AND W-PREMBHNT = ZERO                           
003572*                                                                         
003573****   FÅ FRAM EXPORTPÅLÄGG TILL PACKING OCH HANDLING                     
003574****   EFTERSOM SUORDV-TOT KAN VARA = 0, PGA ATT PRAVCOST = 0             
003575****   I DE FALLEN DÅ DET SAKNAS PRAVCOST PÅ WDK7 PÅ DE INGÅENDE          
003576****   ARTIKLARNA. I DETTA FALL GÖRS INTE BERÄKNINGEN NEDAN,              
003577****   UTAN MAN SKICKAR NOLL I P&H TILL BILLIT.                           
003578*                                                                         
003579*     IMPORTANT NOT TO USE 'DIST35-NDCCN-NONVCC-REFILL'                   
003580*     BECAUSE DC 87 IS NOT INCLUDED BELOW:                                
003581       IF DIST35-NONVCC-CDC-REFILL     OR                                 
003582          DIST35-NONVCC-VCC-REFILL     OR                                 
003583          DIST35-NONVCC-VCC-TRANSFER   OR                                 
003584          DIST35-NDCCN-NONVCC-REFILL   OR                                 
003585          DIST35-NDCUS-NONVCC-REFILL   OR                                 
003586          DIST35-NDCKR-NDC-REFILL      OR                                 
003587          DIST35-NDCIN-NDC-REFILL      OR                                 
003588          DIST35-NDCMY-NDC-REFILL      OR                                 
003589          DIST35-NDCTH-NDC-REFILL      OR                                 
003590          DIST35-NDCTW-NDC-REFILL      OR                                 
003591         (SHIP-IDDC-EXP = WC-CDC-SE                                       
003592                       AND                                                
003593          GMT-KDKUNDKAT = WS-IMPORTER)                                    
003594                                                                          
003595         IF W-SUORDV-TOT > ZERO                                           
003596            COMPUTE W-PREMBHNT ROUNDED =                                  
003597                    ((W-SUORDV-TOT -                                      
003598                      W-PRAVDRAG) *                                       
003599                      W-REEMBHNT) / 100                                   
003600*                                                                         
003601****   ADDERA EV. EXPORTPÅLÄGG TILL PACKNING OCH HANDLING                 
003602*                                                                         
003603            PERFORM IMS-GU-WDB617                                         
003604            IF SEGMENT-FINNS                                              
003605              IF PROC-RELANDCO-EXP > 1                                    
003606                COMPUTE W-PREMBHNT ROUNDED =                              
003607                        W-PREMBHNT + (W-PREMBHNT *                        
003608                        (PROC-RELANDCO-EXP - 1))                          
003609              ELSE                                                        
003610                COMPUTE W-PREMBHNT ROUNDED =                              
003611                        W-PREMBHNT                                        
003612              END-IF                                                      
003613            ELSE                                                          
003614              MOVE ZERO TO W-PREMBHNT                                     
003615            END-IF                                                        
003616         ELSE                                                             
003617           MOVE ZERO TO W-PREMBHNT                                        
003618         END-IF                                                           
003619       ELSE                                                               
003620          COMPUTE W-PREMBHNT ROUNDED =                                    
003621                  ((W-SUORDV-TOT -                                        
003622                    W-PRAVDRAG) *                                         
003623                    W-REEMBHNT) / 100                                     
003624       END-IF                                                             
003625     ELSE                                                                 
003626**** LYNK SERVICE FEE TO BE ADDED TO THE EMB FIELD                        
003627       IF BET-RESERAVG > 0                                                
003628         COMPUTE W-PREMBHNT ROUNDED =                                     
003629                 (W-SUORDV-TOT *                                          
003630                  BET-RESERAVG) / 100                                     
003631       END-IF                                                             
003632     END-IF                                                               
003633                                                                          
003634     COMPUTE W-REFOERS =                                                  
003635             BGMT-REFOERS / 100                                           
003636     COMPUTE W-REOVKOFF =                                                 
003637             BGMT-REOVKOFF / 100                                          
003638     MOVE BGMT-PRFOERS             TO W-PRFOERS                           
003639     IF W-PRFOERS = ZERO AND W-REFOERS NOT = ZERO                         
003640         COMPUTE W-PRFOERS ROUNDED                                        
003641               = (W-REFOERS )                                             
003642               * (1 + (W-REOVKOFF))                                       
003643               * (W-SUORDV-TOT                                            
003644                + W-PREMBHNT                                              
003645                + W-PRLEGKST                                              
003646                + W-PRFRAKT                                               
003647                - W-PRAVDRAG)                                             
003648               / (1 - (W-REFOERS))                                        
003649     END-IF                                                               
003650                                                                          
003651     MOVE W-PRLEGKST               TO BGMT-PRLEGKST                       
003652     MOVE W-PRAVDRAG               TO BGMT-PRAVDRAG                       
003653     MOVE W-PREMBHNT               TO BGMT-PREMBHNT                       
003654     MOVE W-PRFOERS                TO BGMT-PRFOERS                        
003655                                                                          
003656     .                                                                    
003657     EJECT                                                                
003658 S07-TILLAGG-WDE122  SECTION.                                             
003659     MOVE 'S07-TILLAGG-WDE122'   TO WS-SEKTION                            
003660                                                                          
003661     IF MID-KDTRPINF = JA                                                 
003662        OR = NEJ OR KODL                                                  
003663        OR DIS107-BYGG-SHIP                                               
003664        PERFORM IMS-GHU-WDE101X                                           
003665        PERFORM IMS-GHU-WDE111X-KVAL                                      
003666        IF SEGMENT-FINNS                                                  
003667          MOVE SGMT-IDDISTR        TO W-IDDISTR                           
003668          MOVE SGMT-IDKUNDNR       TO W-IDKUNDNR                          
003669          PERFORM IMS-GHU-WDE122X                                         
003670          IF SEGMENT-FINNS                                                
003671            IF TILL-PRFRAKT  = ZERO                                       
003672              MOVE W-PRFRAKT           TO TILL-PRFRAKT                    
003673            END-IF                                                        
003674            IF TILL-PRLEGKST = ZERO                                       
003675              MOVE W-PRLEGKST          TO TILL-PRLEGKST                   
003676            END-IF                                                        
003677            IF TILL-PRAVDRAG = ZERO                                       
003678              MOVE W-PRAVDRAG          TO TILL-PRAVDRAG                   
003679            END-IF                                                        
003680            IF TILL-PREMBHNT = ZERO                                       
003681              MOVE W-PREMBHNT          TO TILL-PREMBHNT                   
003682            END-IF                                                        
003683            IF TILL-PRFOERS  = ZERO                                       
003684              MOVE W-PRFOERS           TO TILL-PRFOERS                    
003685            END-IF                                                        
003686              PERFORM IMS-REPL-WDE122X                                    
003687          ELSE                                                            
003688            INITIALIZE                 TILL-WDE122                        
003689            MOVE W-PRFRAKT             TO TILL-PRFRAKT                    
003690            MOVE W-PRLEGKST            TO TILL-PRLEGKST                   
003691            MOVE W-PRAVDRAG            TO TILL-PRAVDRAG                   
003692            MOVE W-PREMBHNT            TO TILL-PREMBHNT                   
003693            MOVE W-PRFOERS             TO TILL-PRFOERS                    
003694            PERFORM IMS-ISRT-WDE122X                                      
003695          END-IF                                                          
003696        END-IF                                                            
003697     END-IF                                                               
003698     .                                                                    
003699     EJECT                                                                
003700 S08-IMS-ISRT-WDL901 SECTION.                                             
003701     MOVE 'S08-IMS-ISRT-WDL901'  TO WS-SEKTION                            
003702                                                                          
003703     PERFORM IMS-ISRT-WDL901                                              
003704     IF SEGMENT-FINNS-REDAN                                               
003705       PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                              
003706         SUBTRACT 1       FROM LOGG-IDSEKVNR                              
003707         PERFORM IMS-ISRT-WDL901                                          
003708       END-PERFORM                                                        
003709     END-IF                                                               
003710     .                                                                    
003711     EJECT                                                                
003712                                                                          
003713 S09-CHK-LYN-NON-API SECTION.                                             
003714                                                                          
003715     MOVE OHUV-IDDISTR         TO W-IDDISTR-WDB2                          
003716     MOVE OHUV-IDKUNDNR        TO W-IDKUNDNR-WDB2                         
003717     MOVE NEJ                  TO SW-LYNK-NON-API                         
003718                                  SW-VOR                                  
003719     PERFORM IMS-GU-WDB201                                                
003720                                                                          
003721     IF SEGMENT-FINNS                                                     
003722       IF GMT-KDKUNDKAT = 03                                              
003723         IF OHUV-IDSYSTEM(1:3) NOT = 'LYN'                                
003724           MOVE JA             TO SW-LYNK-NON-API                         
003725         END-IF                                                           
003726         IF OHUV-KDORDKL = 0                                              
003727            MOVE JA            TO SW-VOR                                  
003728         END-IF                                                           
003729       END-IF                                                             
003730     END-IF                                                               
003731     .                                                                    
003732     EJECT                                                                
003733                                                                          
003734 S10-VATCODE  SECTION.                                                    
003735     MOVE WC-IDDC              TO WS-IDDC                                 
003736     IF WS-IDDC NOT = W-IDDC-B6                                           
003737        MOVE WS-IDDC TO W-IDDC-B6                                         
003738        PERFORM IMS-GU-WDB601                                             
003739     END-IF                                                               
003740     IF WC-KDMOMSIN    = +2                                               
003741**** NO VAT / MOMSFRITT                                                   
003742       MOVE BGMT-IDDISTR       TO TEST-IDDISTR                            
003743       IF DCS-SDC AND DCS-ITALY                                           
003744         IF BGMT-IDDISTR = 1558                                           
003745         OR BGMT-IDDISTR = 1578                                           
003746           MOVE 'ID'           TO BRAD-KDVAT                              
003747         ELSE                                                             
003748           MOVE 'IC'           TO BRAD-KDVAT                              
003749         END-IF                                                           
003750       ELSE                                                               
003751        IF DCS-SDC AND DCS-HOLLAND                                        
003752         IF BGMT-IDDISTR    =  1619 OR 1620 OR 1622 OR 1628 OR            
003753                             1678                                         
003754           MOVE 'NZ'           TO BRAD-KDVAT                              
003755         ELSE                                                             
003756           IF DIST42-EU                                                   
003757             MOVE 'NI'         TO BRAD-KDVAT                              
003758           ELSE                                                           
003759             MOVE 'NJ'         TO BRAD-KDVAT                              
003760           END-IF                                                         
003761         END-IF                                                           
003762        ELSE                                                              
003763         IF DCS-DDC AND DCS-GERMANY                                       
003764         OR DCS-SDC AND DCS-GERMANY                                       
003765           IF DIST42-EU                                                   
003766             MOVE 'VI'         TO BRAD-KDVAT                              
003767           ELSE                                                           
003768             MOVE 'VJ'         TO BRAD-KDVAT                              
003769           END-IF                                                         
003770         ELSE                                                             
003771           IF DCS-DDC AND DCS-BELGIUM                                     
003772           OR DCS-SDC AND DCS-BELGIUM                                     
003773             IF DIST34-BELGIEN-DDC                                        
003774               MOVE 'BD'       TO BRAD-KDVAT                              
003775             ELSE                                                         
003776               IF DIST42-EU                                               
003777                 MOVE 'BQ'     TO BRAD-KDVAT                              
003778               ELSE                                                       
003779                 MOVE 'BX'     TO BRAD-KDVAT                              
003780               END-IF                                                     
003781             END-IF                                                       
003782           ELSE                                                           
003783            IF DCS-DDC AND DCS-FRANCE                                     
003784              IF DIST34-FRANKRIKE-DDC                                     
003785                MOVE 'F3'       TO BRAD-KDVAT                             
003786              ELSE                                                        
003787                IF DIST42-EU                                              
003788                  MOVE 'F4'     TO BRAD-KDVAT                             
003789                ELSE                                                      
003790                  MOVE 'F5'     TO BRAD-KDVAT                             
003791                END-IF                                                    
003792              END-IF                                                      
003793            ELSE                                                          
003794             IF LDC-CH                                                    
003795               MOVE 'XX'       TO BRAD-KDVAT                              
003796             ELSE                                                         
003797              IF DCS-DDC AND DCS-FINLAND                                  
003798              OR DCS-SDC AND DCS-FINLAND                                  
003799                IF DIST34-FINLAND-DDC                                     
003800                OR DIST34-FINLAND-LDC                                     
003801                  MOVE '48'      TO BRAD-KDVAT                            
003802                ELSE                                                      
003803                  IF DIST42-EU                                            
003804                    MOVE '70'    TO BRAD-KDVAT                            
003805                  ELSE                                                    
003806                    MOVE '90'    TO BRAD-KDVAT                            
003807                  END-IF                                                  
003808                END-IF                                                    
003809              ELSE                                                        
003810               IF DCS-DDC AND DCS-POLAND                                  
003811               OR DCS-SDC AND DCS-POLAND                                  
003812                IF DIST34-POLAND-DDC                                      
003813                OR DIST34-POLAND-LDC                                      
003814                  MOVE 'P2'      TO BRAD-KDVAT                            
003815                ELSE                                                      
003816                  IF DIST42-EU                                            
003817                    MOVE 'PC'    TO BRAD-KDVAT                            
003818                  ELSE                                                    
003819                    MOVE 'PD'    TO BRAD-KDVAT                            
003820                  END-IF                                                  
003821                END-IF                                                    
003822               ELSE                                                       
003823                IF DCS-DDC AND DCS-AUSTRALIA                              
003824                 IF DIST34-AUSTRALIA-DDC                                  
003825                   MOVE '90'     TO BRAD-KDVAT                            
003826                 ELSE                                                     
003827                   IF DIST42-EU                                           
003828                     MOVE '90' TO BRAD-KDVAT                              
003829                   ELSE                                                   
003830                     MOVE '90' TO BRAD-KDVAT                              
003831                   END-IF                                                 
003832                 END-IF                                                   
003833                ELSE                                                      
003834                  IF DCS-IDLANDX2 = 'GB'                                  
003835                    IF DCS-DDC AND DCS-ENGLAND                            
003836                    OR DCS-SDC AND DCS-ENGLAND                            
003837                      IF DIST34-ENGLAND-DDC                               
003838                      OR DIST34-ENGLAND-LDC                               
003839                      OR DIST34-ENGLAND-SDC                               
003840                        MOVE 'G7' TO BRAD-KDVAT                           
003841*** SENDING FROM GB TO EU                                                 
003842                      ELSE                                                
003843                        IF DIST42-EU                                      
003844                          MOVE 'G9' TO BRAD-KDVAT                         
003845                        ELSE                                              
003846                          MOVE '90' TO BRAD-KDVAT                         
003847                        END-IF                                            
003848                      END-IF                                              
003849                    END-IF                                                
003850                  ELSE                                                    
003851                   IF DIST42-EU                                           
003852                    IF DIST03-SVERIGE                                     
003853                      MOVE '21' TO BRAD-KDVAT                             
003854                    ELSE                                                  
003855                      MOVE '70' TO BRAD-KDVAT                             
003856                    END-IF                                                
003857                   ELSE                                                   
003858                     MOVE '90'     TO BRAD-KDVAT                          
003859                     IF DCS-NDC-PF                                        
003860                       MOVE 'XX' TO BRAD-KDVAT                            
003861                       IF DCS-AUSTRALIA AND DIST34-JAPAN-NDC OR           
003862                          DCS-JAPAN AND DIST34-AUSTRALIA-NDC OR           
003863                          DIST35-PACIFIC-TRANSFER            OR           
003864                          DIST35-REFILL-INOM-JP                           
003865                        MOVE '90' TO BRAD-KDVAT                           
003866                       END-IF                                             
003867                       IF DCS-THAILAND                                    
003868                         MOVE 'XZ' TO BRAD-KDVAT                          
003869                       END-IF                                             
003870                     END-IF                                               
003871                   END-IF                                                 
003872                  END-IF                                                  
003873                END-IF                                                    
003874               END-IF                                                     
003875              END-IF                                                      
003876             END-IF                                                       
003877            END-IF                                                        
003878           END-IF                                                         
003879         END-IF                                                           
003880       END-IF                                                             
003881      END-IF                                                              
003882     ELSE                                                                 
003883       EVALUATE TRUE                                                      
003884       WHEN DCS-CDC OR DCS-SDC AND DCS-SWEDEN                             
003885          MOVE '21'          TO BRAD-KDVAT                                
003886       WHEN DCS-SDC AND DCS-HOLLAND                                       
003887          MOVE 'NZ'          TO BRAD-KDVAT                                
003888       WHEN DCS-SDC AND DCS-ENGLAND                                       
003889          MOVE 'G7'          TO BRAD-KDVAT                                
003890       WHEN DCS-DDC AND DCS-ENGLAND                                       
003891          MOVE 'G7'          TO BRAD-KDVAT                                
003892       WHEN DCS-SDC AND DCS-SPAIN                                         
003893          MOVE 'S4'          TO BRAD-KDVAT                                
003894       WHEN DCS-SDC AND DCS-ITALY                                         
003895          MOVE 'IC'          TO BRAD-KDVAT                                
003896       WHEN DCS-SDC AND DCS-AUSTRIA                                       
003897          MOVE 'A2'          TO BRAD-KDVAT                                
003898       WHEN DCS-NDC-PF AND DCS-JAPAN                                      
003899          MOVE 'J4'          TO BRAD-KDVAT                                
003900       WHEN DCS-NDC-PF AND DCS-AUSTRALIA                                  
003901          MOVE '90'          TO BRAD-KDVAT                                
003902       WHEN DCS-DDC AND DCS-SWEDEN                                        
003903          MOVE '21'          TO BRAD-KDVAT                                
003904       WHEN DCS-DDC AND DCS-NORWAY                                        
003905          MOVE 'Y1'          TO BRAD-KDVAT                                
003906       WHEN DCS-SDC AND DCS-NORWAY                                        
003907          MOVE 'Y1'          TO BRAD-KDVAT                                
003908       WHEN DCS-DDC AND DCS-BELGIUM                                       
003909          MOVE 'BD'          TO BRAD-KDVAT                                
003910       WHEN DCS-SDC AND DCS-BELGIUM                                       
003911          MOVE 'BD'          TO BRAD-KDVAT                                
003912       WHEN DCS-DDC AND DCS-GERMANY                                       
003913          MOVE 'VE'          TO BRAD-KDVAT                                
003914       WHEN DCS-SDC AND DCS-GERMANY                                       
003915          MOVE 'VE'          TO BRAD-KDVAT                                
003916       WHEN DCS-DDC AND DCS-FRANCE                                        
003917          MOVE 'F3'          TO BRAD-KDVAT                                
003918       WHEN DCS-SDC AND DCS-FRANCE                                        
003919          MOVE 'F3'          TO BRAD-KDVAT                                
003920       WHEN DCS-DDC AND DCS-FINLAND                                       
003921          MOVE '48'          TO BRAD-KDVAT                                
003922       WHEN DCS-SDC AND DCS-FINLAND                                       
003923          MOVE '48'          TO BRAD-KDVAT                                
003924       WHEN DCS-DDC AND DCS-AUSTRALIA                                     
003925          MOVE '90'          TO BRAD-KDVAT                                
003926*      WHEN DCS-DDC AND DCS-HUNGARY                                       
003927*         MOVE '??'          TO BRAD-KDVAT                                
003928       WHEN DCS-DDC AND DCS-POLAND                                        
003929          MOVE 'P2'          TO BRAD-KDVAT                                
003930       WHEN DCS-SDC AND DCS-POLAND                                        
003931          MOVE 'P2'          TO BRAD-KDVAT                                
003932       WHEN LDC-CH                                                        
003933          MOVE 'XX'          TO BRAD-KDVAT                                
003934       END-EVALUATE                                                       
003935       IF BGMT-IDDISTR = 1558                                             
003936       OR BGMT-IDDISTR = 1578                                             
003937        MOVE 'ID'            TO BRAD-KDVAT                                
003938       END-IF                                                             
003939     END-IF                                                               
003940     PERFORM S10A-VAT-ADAPTION                                            
003941     .                                                                    
003942     EJECT                                                                
003943                                                                          
003944 S10A-VAT-ADAPTION  SECTION.                                              
003945**** TEMPORARY SOLN FOR D25 COMPOUND - GB, BE, ES                         
003946     MOVE BGMT-IDDISTR       TO TEST-IDDISTR                              
003947     IF DIST28-CDC-GB25 OR DIST28-CDC-NO25                                
003948      MOVE '90'              TO BRAD-KDVAT                                
003949     END-IF                                                               
003950     IF DIST28-CDC-BE25 OR DIST28-CDC-ES25                                
003951      MOVE '70'              TO BRAD-KDVAT                                
003952     END-IF                                                               
003953**** SHOULD ONLY BE FOR DDGS                                              
003954     IF DCS-DDC                                                           
003955**** RECEIVER COUNTRY SHOULD BE IN EUROPE                                 
003956       MOVE BET-IDLANDX2       TO LANDX2-IDLANDX2                         
003957       IF LANDX2-EU-IDLANDX2                                              
003958**** SENDING COUNTRY SHOULD BE IN EUROPE                                  
003959         MOVE DCS-IDLANDX2     TO LANDX2-IDLANDX2                         
003960         IF LANDX2-EU-IDLANDX2                                            
003961           IF BET-FLDIRVAT = 'J'                                          
003962             IF BET-IDLANDX2 = 'AT'                                       
003963               MOVE 'A2' TO BRAD-KDVAT                                    
003964             END-IF                                                       
003965             IF BET-IDLANDX2 = 'BE'                                       
003966               MOVE 'BD' TO BRAD-KDVAT                                    
003967             END-IF                                                       
003968             IF BET-IDLANDX2 = 'DE'                                       
003969               MOVE 'VE' TO BRAD-KDVAT                                    
003970             END-IF                                                       
003971             IF BET-IDLANDX2 = 'ES'                                       
003972               MOVE 'S4' TO BRAD-KDVAT                                    
003973             END-IF                                                       
003974             IF BET-IDLANDX2 = 'FI'                                       
003975               MOVE '48' TO BRAD-KDVAT                                    
003976             END-IF                                                       
003977             IF BET-IDLANDX2 = 'FR'                                       
003978               MOVE 'F3' TO BRAD-KDVAT                                    
003979             END-IF                                                       
003980             IF BET-IDLANDX2 = 'IT'                                       
003981               MOVE 'IC' TO BRAD-KDVAT                                    
003982             END-IF                                                       
003983             IF BET-IDLANDX2 = 'NL'                                       
003984               MOVE 'NZ' TO BRAD-KDVAT                                    
003985             END-IF                                                       
003986             IF BET-IDLANDX2 = 'PL'                                       
003987               MOVE 'P2' TO BRAD-KDVAT                                    
003988             END-IF                                                       
003989           END-IF                                                         
003990         END-IF                                                           
003991       END-IF                                                             
003992     END-IF                                                               
003993     .                                                                    
003994     EJECT                                                                
003995                                                                          
003996     .                                                                    
003997     EJECT                                                                
003998                                                                          
003999 S11-RECV-OPEN SECTION.                                                   
004000     MOVE 'S11-RECV-OPEN'       TO WS-SEKTION                             
004001                                                                          
004002     MOVE 'OPEN'                   TO RECV-KDFUNC                         
004003     MOVE 'CARPARTS.PULS.ADDIT  '  TO RECV-ADDISPABS                      
004004                                                                          
004005     CALL WZ01RECV USING           RECV-CONTROL-AREA                      
004006                                   RECV-OPEN-AREA                         
004007                                                                          
004008     IF RECV-KDRC = 20                                                    
004009       MOVE JA TO NO-MESSAGE-SW                                           
004010     ELSE                                                                 
004011       IF RECV-KDRC > 0                                                   
004012        MOVE RECV-KDRC               TO KDRC-DISP                         
004013        STRING 'WZ01RECV OPEN ERROR RC= ' KDRC-DISP                       
004014          DELIMITED BY SIZE INTO FELTEXT                                  
004015        CALL FELLOG                                                       
004016       END-IF                                                             
004017     END-IF                                                               
004018     .                                                                    
004019     EJECT                                                                
004020 S11-RECV-MESSAGE SECTION.                                                
004021     MOVE 'S11-RECV-MESSAGE'      TO WS-SEKTION                           
004022                                                                          
004023     MOVE 'GET'                    TO RECV-KDFUNC                         
004024     MOVE LENGTH OF MID-W40636I1   TO RECV-KVDLEN                         
004025     CALL WZ01RECV USING RECV-CONTROL-AREA                                
004026                         RECV-KVDLEN                                      
004027                         MID-W40636I1                                     
004028*                                                                         
004029     IF RECV-KDRC > 1                                                     
004030       MOVE RECV-KDRC            TO KDRC-DISP                             
004031       STRING 'WZ01RECV GET  ERROR RC= ' KDRC-DISP                        
004032         DELIMITED BY SIZE INTO FELTEXT                                   
004033       CALL FELLOG                                                        
004034     END-IF                                                               
004035     .                                                                    
004036     EJECT                                                                
004037 S11-RECV-CLOSE SECTION.                                                  
004038     MOVE 'S11-RECV-CLOSE'      TO WS-SEKTION                             
004039                                                                          
004040     MOVE 'CLOSE'                TO RECV-KDFUNC                           
004041     CALL WZ01RECV     USING        RECV-CONTROL-AREA                     
004042*                                                                         
004043     IF RECV-KDRC > 0                                                     
004044       MOVE RECV-KDRC            TO KDRC-DISP                             
004045       STRING 'WZ01RECV CLOSE ERROR RC= ' KDRC-DISP                       
004046         DELIMITED BY SIZE INTO FELTEXT                                   
004047       CALL FELLOG                                                        
004048     END-IF                                                               
004049     .                                                                    
004050     EJECT                                                                
004051                                                                          
004052 S12-UPD-TOTAL-WDE101 SECTION.                                            
004053     MOVE 'S12-UPD-TOTA'        TO WS-SEKTION                             
004054                                                                          
004055     PERFORM IMS-GHU-WDE101                                               
004056                                                                          
004057     COMPUTE SHIP-SUORDV-FAKT   = SHIP-SUORDV-FAKT   +                    
004058                                  WS-SUORDV-FAKT                          
004059     COMPUTE SHIP-SUORDV-EXP    = SHIP-SUORDV-EXP    +                    
004060                                  WS-SUORDV-EXP                           
004061     COMPUTE SHIP-VKORDBTO-FAKT = SHIP-VKORDBTO-FAKT +                    
004062                                  WS-VKORDBTO-FAKT                        
004063     COMPUTE SHIP-VLORDBTO-FAKT = SHIP-VLORDBTO-FAKT +                    
004064                                  WS-VLORDBTO-FAKT                        
004065                                                                          
004066     IF SHIP-KDVALISO-EXP = SPACE                                         
004067        MOVE WS-KDVALISO-EXP    TO SHIP-KDVALISO-EXP                      
004068     END-IF                                                               
004069*IF DG FLAG IS ALREADY Y IN WDE01, DO NOT OVERWRITE THE DG FLAG           
004070     IF SHIP-FLFARLIG = SPACE OR                                          
004071        SHIP-FLFARLIG = NEJ                                               
004072        MOVE WS-FLFARLIG-FLAG   TO SHIP-FLFARLIG                          
004073     END-IF                                                               
004074                                                                          
004075     PERFORM IMS-REPL-WDE101                                              
004076                                                                          
004077     IF IMS-DUBBEL-INDEX                                                  
004078       PERFORM UNTIL SEGMENT-FINNS                                        
004079         ADD +1                 TO SHIP-TISKPTID                          
004080         PERFORM IMS-REPL-WDE101                                          
004081       END-PERFORM                                                        
004082     END-IF                                                               
004083     .                                                                    
004084     EJECT                                                                
004085                                                                          
004086 S13-SET-AVGCOST-FLAG  SECTION.                                           
004087     MOVE 'S13-SET-AVGC'        TO WS-SEKTION                             
004088                                                                          
004089     MOVE NEJ                         TO SW-CALC-AVGCOST                  
004090                                                                          
004091     IF DIST35-NONVCC-CDC-REFILL     OR                                   
004092        DIST35-NONVCC-VCC-REFILL     OR                                   
004093        DIST35-NONVCC-VCC-TRANSFER   OR                                   
004094        DIST35-NDCCN-NONVCC-REFILL   OR                                   
004095        DIST35-NDCUS-NONVCC-REFILL   OR                                   
004096        DIST35-NDCKR-NDC-REFILL      OR                                   
004097        DIST35-NDCIN-NDC-REFILL      OR                                   
004098        DIST35-NDCMY-NDC-REFILL      OR                                   
004099        DIST35-NDCTH-NDC-REFILL      OR                                   
004100        DIST35-NDCTW-NDC-REFILL      OR                                   
004101       (SHIP-IDDC-EXP = WC-CDC-SE                                         
004102                     AND                                                  
004103        GMT-KDKUNDKAT = WS-IMPORTER)                                      
004104                                                                          
004105         MOVE JA                      TO SW-CALC-AVGCOST                  
004106                                                                          
004107     END-IF                                                               
004108     .                                                                    
004109     EJECT                                                                
004110 S20-KDMOMSIN   SECTION.                                                  
004111     MOVE 'S20-KDMOMSIN'        TO WS-SEKTION                             
004112                                                                          
004113     MOVE BILL-IDDC                  TO WS-IDDC                           
004114     MOVE BGMT-IDDISTR               TO TEST-IDDISTR                      
004115     MOVE +2                         TO WC-KDMOMSIN                       
004116     EVALUATE TRUE                                                        
004117        WHEN DCS-DDC AND DCS-SWEDEN                                       
004118           IF DIST34-SVERIGE-DDC                                          
004119              MOVE +1                TO WC-KDMOMSIN                       
004120           END-IF                                                         
004121        WHEN DCS-DDC AND DCS-NORWAY                                       
004122           IF DIST34-NORGE-DDC                                            
004123              MOVE +1                TO WC-KDMOMSIN                       
004124           END-IF                                                         
004125        WHEN DCS-DDC AND DCS-FINLAND                                      
004126           IF DIST34-FINLAND-DDC                                          
004127              MOVE +1                TO WC-KDMOMSIN                       
004128           END-IF                                                         
004129        WHEN DCS-DDC AND DCS-KOREA                                        
004130           IF DIST34-KOREA-DDC                                            
004131              MOVE +1                TO WC-KDMOMSIN                       
004132           END-IF                                                         
004133        WHEN DCS-DDC AND DCS-BELGIUM                                      
004134           IF DIST34-BELGIEN-DDC                                          
004135              MOVE +1                TO WC-KDMOMSIN                       
004136           END-IF                                                         
004137        WHEN DCS-DDC AND DCS-GERMANY                                      
004138           IF DIST34-TYSKLAND-DDC                                         
004139              MOVE +1                TO WC-KDMOMSIN                       
004140           END-IF                                                         
004141        WHEN DCS-DDC AND DCS-FRANCE                                       
004142           IF DIST34-FRANKRIKE-DDC                                        
004143              MOVE +1                TO WC-KDMOMSIN                       
004144           END-IF                                                         
004145        WHEN DCS-DDC AND DCS-POLAND                                       
004146           IF DIST34-POLAND-DDC                                           
004147              MOVE +1                TO WC-KDMOMSIN                       
004148           END-IF                                                         
004149        WHEN DCS-DDC AND DCS-ENGLAND                                      
004150           IF DIST34-ENGLAND-DDC                                          
004151              MOVE +1                TO WC-KDMOMSIN                       
004152           END-IF                                                         
004153*       WHEN DCS-DDC AND DCS-TURKEY                                       
004154*          IF DIST34-TURKEY-DDC                                           
004155*             MOVE +1                TO WC-KDMOMSIN                       
004156*          END-IF                                                         
004157*       WHEN DCS-DDC AND DCS-HUNGARY                                      
004158*          IF DIST34-HUNGARY-DDC                                          
004159*             MOVE +1                TO WC-KDMOMSIN                       
004160*          END-IF                                                         
004161*       WHEN DCS-DDC AND DCS-MAROCKO                                      
004162*          IF DIST34-MAROCKO-DDC                                          
004163*             MOVE +1                TO WC-KDMOMSIN                       
004164*          END-IF                                                         
004165     END-EVALUATE                                                         
004166     .                                                                    
004167     EJECT                                                                
004168                                                                          
004169* --- IMS SEKTIONER ---                                                   
004170     SKIP3                                                                
004171 IMS-ISRT-ALT-MSG-4674 SECTION.                                           
004172                                                                          
004173     MOVE SPACE TO GODK-STATUSKODER                                       
004174     CALL CBLTDLI USING ISRT LA74-PCB P-TO-P-SW                           
004175     MOVE LA74-STATUS-CODE TO STATUS-WS                                   
004176     PERFORM IMS-STATUSKONTROLL                                           
004177     .                                                                    
004178     SKIP2                                                                
004179 IMS-GU-WDE101 SECTION.                                                   
004180     MOVE 'IMS-GU-WDE101'      TO WS-SEKTION                              
004181                                                                          
004182     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
004183          DELIMITED BY SIZE INTO SSA1                                     
004184     MOVE '    ' TO GODK-STATUSKODER                                      
004185     CALL CBLTDLI USING GU WDE1-PCB DLI-IO-WDE101 SSA1                    
004186     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
004187     PERFORM IMS-STATUSKONTROLL                                           
004188     .                                                                    
004189     EJECT                                                                
004190 IMS-GHU-WDE101 SECTION.                                                  
004191     MOVE 'IMS-GHU-WDE101'      TO WS-SEKTION                             
004192                                                                          
004193     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
004194          DELIMITED BY SIZE INTO SSA1                                     
004195     MOVE '    ' TO GODK-STATUSKODER                                      
004196     CALL CBLTDLI USING GHU WDE1-PCB DLI-IO-WDE101 SSA1                   
004197     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
004198     PERFORM IMS-STATUSKONTROLL                                           
004199     .                                                                    
004200     EJECT                                                                
004201 IMS-GHU-WDE101X SECTION.                                                 
004202     MOVE 'IMS-GHU-WDE101X'      TO WS-SEKTION                            
004203                                                                          
004204     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
004205          DELIMITED BY SIZE INTO SSA1                                     
004206     MOVE '    ' TO GODK-STATUSKODER                                      
004207     CALL CBLTDLI USING GHU WDE1X-PCB DLI-IO-WDE101 SSA1                  
004208     MOVE WDE1X-STATUS-CODE TO STATUS-WS                                  
004209     PERFORM IMS-STATUSKONTROLL                                           
004210     .                                                                    
004211     EJECT                                                                
004212 IMS-REPL-WDE101 SECTION.                                                 
004213     MOVE 'IMS-REPL-WDE101'      TO WS-SEKTION                            
004214                                                                          
004215     MOVE '  NI' TO GODK-STATUSKODER                                      
004216     CALL CBLTDLI USING REPL WDE1-PCB DLI-IO-WDE101                       
004217     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
004218     PERFORM IMS-STATUSKONTROLL                                           
004219     .                                                                    
004220     EJECT                                                                
004221 IMS-GHNP-WDE111 SECTION.                                                 
004222     MOVE 'IMS-GHNP-WDE111'      TO WS-SEKTION                            
004223                                                                          
004224     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
004225          DELIMITED BY SIZE INTO SSA1                                     
004226     MOVE 'WDE111  '            TO SSA2                                   
004227     MOVE '  GE' TO GODK-STATUSKODER                                      
004228     CALL CBLTDLI USING GHNP WDE1-PCB DLI-IO-WDE111 SSA1 SSA2             
004229     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
004230     PERFORM IMS-STATUSKONTROLL                                           
004231     .                                                                    
004232     EJECT                                                                
004233 IMS-GHNP-WDE111-KVAL SECTION.                                            
004234     MOVE 'IMS-GHNP-WDE111-KVAL'   TO WS-SEKTION                          
004235                                                                          
004236     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
004237          DELIMITED BY SIZE INTO SSA1                                     
004238     STRING 'WDE111  *F(WDE111KY =' W-WDE111KY-X ')'                      
004239          DELIMITED BY SIZE INTO SSA2                                     
004240     MOVE '  GE' TO GODK-STATUSKODER                                      
004241     CALL CBLTDLI USING GHNP WDE1-PCB DLI-IO-WDE111 SSA1 SSA2             
004242     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
004243     PERFORM IMS-STATUSKONTROLL                                           
004244     .                                                                    
004245     EJECT                                                                
004246 IMS-GHU-WDE111X-KVAL SECTION.                                            
004247     MOVE 'IMS-GHU-WDE111X-KVAL'      TO WS-SEKTION                       
004248                                                                          
004249     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
004250          DELIMITED BY SIZE INTO SSA1                                     
004251     STRING 'WDE111  *F(WDE111KY =' W-WDE111KY-X ')'                      
004252          DELIMITED BY SIZE INTO SSA2                                     
004253     MOVE '  GE' TO GODK-STATUSKODER                                      
004254     CALL CBLTDLI USING GHU WDE1X-PCB DLI-IO-WDE111 SSA1 SSA2             
004255     MOVE WDE1X-STATUS-CODE TO STATUS-WS                                  
004256     PERFORM IMS-STATUSKONTROLL                                           
004257     .                                                                    
004258     EJECT                                                                
004259 IMS-REPL-WDE111X SECTION.                                                
004260     MOVE 'IMS-REPL-WDE111X'      TO WS-SEKTION                           
004261                                                                          
004262     MOVE '    ' TO GODK-STATUSKODER                                      
004263     CALL CBLTDLI USING REPL WDE1X-PCB DLI-IO-WDE111                      
004264     MOVE WDE1X-STATUS-CODE TO STATUS-WS                                  
004265     PERFORM IMS-STATUSKONTROLL                                           
004266     .                                                                    
004267     EJECT                                                                
004268 IMS-REPL-WDE111 SECTION.                                                 
004269     MOVE 'IMS-REPL-WDE111'      TO WS-SEKTION                            
004270                                                                          
004271     MOVE '    ' TO GODK-STATUSKODER                                      
004272     CALL CBLTDLI USING REPL WDE1-PCB DLI-IO-WDE111                       
004273     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
004274     PERFORM IMS-STATUSKONTROLL                                           
004275     .                                                                    
004276     EJECT                                                                
004277 IMS-GHNP-WDE121 SECTION.                                                 
004278     MOVE 'IMS-GHNP-WDE121'      TO WS-SEKTION                            
004279                                                                          
004280     STRING 'WDE111  (WDE111KY =' W-WDE111KY-X ')'                        
004281          DELIMITED BY SIZE INTO SSA1                                     
004282     MOVE 'WDE121  '            TO SSA2                                   
004283     MOVE '  GE' TO GODK-STATUSKODER                                      
004284     CALL CBLTDLI USING GHNP WDE1-PCB DLI-IO-WDE121 SSA1 SSA2             
004285     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
004286     PERFORM IMS-STATUSKONTROLL                                           
004287     .                                                                    
004288     EJECT                                                                
004289 IMS-GHNP-WDE121-KVAL SECTION.                                            
004290     MOVE 'IMS-GHNP-WDE121-KVAL'  TO WS-SEKTION                           
004291                                                                          
004292     STRING 'WDE111  (WDE111KY =' W-WDE111KY-X ')'                        
004293          DELIMITED BY SIZE INTO SSA1                                     
004294     STRING 'WDE121  *F(WDE121KY =' W-WDE121KY-X ')'                      
004295          DELIMITED BY SIZE INTO SSA2                                     
004296     MOVE '    ' TO GODK-STATUSKODER                                      
004297     CALL CBLTDLI USING GHNP WDE1-PCB DLI-IO-WDE121 SSA1 SSA2             
004298     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
004299     PERFORM IMS-STATUSKONTROLL                                           
004300     .                                                                    
004301     EJECT                                                                
004302 IMS-REPL-WDE121 SECTION.                                                 
004303     MOVE 'IMS-REPL-WDE121'      TO WS-SEKTION                            
004304                                                                          
004305     MOVE '    '   TO GODK-STATUSKODER                                    
004306     CALL CBLTDLI USING REPL WDE1-PCB DLI-IO-WDE121                       
004307     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
004308     PERFORM IMS-STATUSKONTROLL                                           
004309     .                                                                    
004310     EJECT                                                                
004311 IMS-GHU-WDE122X SECTION.                                                 
004312     MOVE 'IMS-GHU-WDE122X'      TO WS-SEKTION                            
004313                                                                          
004314     MOVE 'WDE122'   TO SSA1                                              
004315     MOVE '  GE' TO GODK-STATUSKODER                                      
004316     CALL CBLTDLI USING GHU WDE1X-PCB DLI-IO-WDE122 SSA1                  
004317     MOVE WDE1X-STATUS-CODE TO STATUS-WS                                  
004318     PERFORM IMS-STATUSKONTROLL                                           
004319     .                                                                    
004320     EJECT                                                                
004321 IMS-REPL-WDE122X SECTION.                                                
004322     MOVE 'IMS-REPL-WDE122X'     TO WS-SEKTION                            
004323                                                                          
004324     MOVE '    '   TO GODK-STATUSKODER                                    
004325     CALL CBLTDLI USING REPL WDE1X-PCB DLI-IO-WDE122                      
004326     MOVE WDE1X-STATUS-CODE TO STATUS-WS                                  
004327     PERFORM IMS-STATUSKONTROLL                                           
004328     .                                                                    
004329     EJECT                                                                
004330 IMS-ISRT-WDE122X  SECTION.                                               
004331     MOVE 'IMS-ISRT-WDE122X'     TO WS-SEKTION                            
004332                                                                          
004333     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
004334          DELIMITED BY SIZE INTO SSA1                                     
004335     STRING 'WDE111  (WDE111KY =' W-WDE111KY-X ')'                        
004336          DELIMITED BY SIZE INTO SSA2                                     
004337     MOVE 'WDE122 ' TO SSA3                                               
004338     MOVE '  II' TO GODK-STATUSKODER                                      
004339     CALL CBLTDLI USING ISRT WDE1X-PCB DLI-IO-WDE122 SSA1                 
004340                                                     SSA2  SSA3           
004341     MOVE WDE1X-STATUS-CODE TO STATUS-WS                                  
004342     PERFORM IMS-STATUSKONTROLL                                           
004343     .                                                                    
004344     SKIP3                                                                
004345 IMS-ISRT-WDE131 SECTION.                                                 
004346     MOVE 'IMS-ISRT-WDE131'      TO WS-SEKTION                            
004347                                                                          
004348     MOVE 'WDE131'  TO SSA1                                               
004349     MOVE '  II'    TO GODK-STATUSKODER                                   
004350     CALL CBLTDLI USING ISRT WDE1-PCB DLI-IO-WDE131 SSA1                  
004351     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
004352     PERFORM IMS-STATUSKONTROLL                                           
004353     .                                                                    
004354     EJECT                                                                
004355 IMS-GU-WDE201 SECTION.                                                   
004356     MOVE 'IMS-GU-WDE201'      TO WS-SEKTION                              
004357                                                                          
004358     STRING 'WDE201  (IDSHIPM  =' W-IDSHIPM-X ')'                         
004359          DELIMITED BY SIZE INTO SSA1                                     
004360     MOVE '    ' TO GODK-STATUSKODER                                      
004361     CALL CBLTDLI USING GU WDE2-PCB DLI-IO-WDE201 SSA1                    
004362     MOVE WDE2-STATUS-CODE TO STATUS-WS                                   
004363     PERFORM IMS-STATUSKONTROLL                                           
004364     .                                                                    
004365     EJECT                                                                
004366 IMS-GHU-WDE201 SECTION.                                                  
004367     MOVE 'IMS-GU-WDE201'      TO WS-SEKTION                              
004368                                                                          
004369     STRING 'WDE201  (IDSHIPM  =' W-IDSHIPM-X ')'                         
004370          DELIMITED BY SIZE INTO SSA1                                     
004371     MOVE '    ' TO GODK-STATUSKODER                                      
004372     CALL CBLTDLI USING GHU WDE2X-PCB DLI-IO-WDE201 SSA1                  
004373     MOVE WDE2X-STATUS-CODE TO STATUS-WS                                  
004374     PERFORM IMS-STATUSKONTROLL                                           
004375     .                                                                    
004376     EJECT                                                                
004377 IMS-REPL-WDE201 SECTION.                                                 
004378     MOVE 'IMS-REPL-WDE201'      TO WS-SEKTION                            
004379                                                                          
004380     MOVE '    ' TO GODK-STATUSKODER                                      
004381     CALL CBLTDLI USING REPL WDE2X-PCB DLI-IO-WDE201                      
004382     MOVE WDE2X-STATUS-CODE TO STATUS-WS                                  
004383     PERFORM IMS-STATUSKONTROLL                                           
004384     .                                                                    
004385     EJECT                                                                
004386 IMS-GHU-WDE211X SECTION.                                                 
004387     MOVE 'IMS-GHU-WDE211X'      TO WS-SEKTION                            
004388                                                                          
004389     STRING 'WDE201  (IDSHIPM  =' W-IDSHIPM-X ')'                         
004390          DELIMITED BY SIZE INTO SSA1                                     
004391     STRING 'WDE211  (WDE211KY =' W-WDE111KY-X ')'                        
004392          DELIMITED BY SIZE INTO SSA2                                     
004393     MOVE '    ' TO GODK-STATUSKODER                                      
004394     CALL CBLTDLI USING GHU WDE2X-PCB DLI-IO-WDE211 SSA1 SSA2             
004395     MOVE WDE2X-STATUS-CODE TO STATUS-WS                                  
004396     PERFORM IMS-STATUSKONTROLL                                           
004397     .                                                                    
004398     EJECT                                                                
004399 IMS-REPL-WDE211X SECTION.                                                
004400     MOVE 'IMS-REPL-WDE211X'      TO WS-SEKTION                           
004401                                                                          
004402     MOVE '    ' TO GODK-STATUSKODER                                      
004403     CALL CBLTDLI USING REPL WDE2X-PCB DLI-IO-WDE211                      
004404     MOVE WDE2X-STATUS-CODE TO STATUS-WS                                  
004405     PERFORM IMS-STATUSKONTROLL                                           
004406     .                                                                    
004407     EJECT                                                                
004408 IMS-GHNP-WDE211 SECTION.                                                 
004409     MOVE 'IMS-GHNP-WDE211'      TO WS-SEKTION                            
004410                                                                          
004411     STRING 'WDE201  (IDSHIPM  =' W-IDSHIPM-X ')'                         
004412          DELIMITED BY SIZE INTO SSA1                                     
004413     MOVE 'WDE211  '            TO SSA2                                   
004414     MOVE '  GE' TO GODK-STATUSKODER                                      
004415     CALL CBLTDLI USING GHNP WDE2-PCB DLI-IO-WDE211 SSA1 SSA2             
004416     MOVE WDE2-STATUS-CODE TO STATUS-WS                                   
004417     PERFORM IMS-STATUSKONTROLL                                           
004418     .                                                                    
004419     EJECT                                                                
004420 IMS-GHNP-WDE211-KVAL SECTION.                                            
004421     MOVE 'IMS-GHNP-WDE211-KVAL' TO WS-SEKTION                            
004422                                                                          
004423     STRING 'WDE201  (IDSHIPM  =' W-IDSHIPM-X ')'                         
004424          DELIMITED BY SIZE INTO SSA1                                     
004425     STRING 'WDE211  *F(WDE211KY =' W-WDE111KY-X ')'                      
004426          DELIMITED BY SIZE INTO SSA2                                     
004427     MOVE '  GE' TO GODK-STATUSKODER                                      
004428     CALL CBLTDLI USING GHNP WDE2-PCB DLI-IO-WDE211 SSA1 SSA2             
004429     MOVE WDE2-STATUS-CODE TO STATUS-WS                                   
004430     PERFORM IMS-STATUSKONTROLL                                           
004431     .                                                                    
004432     EJECT                                                                
004433 IMS-REPL-WDE211 SECTION.                                                 
004434     MOVE 'IMS-REPL-WDE211'      TO WS-SEKTION                            
004435                                                                          
004436     MOVE '    ' TO GODK-STATUSKODER                                      
004437     CALL CBLTDLI USING REPL WDE2-PCB DLI-IO-WDE211                       
004438     MOVE WDE2-STATUS-CODE TO STATUS-WS                                   
004439     PERFORM IMS-STATUSKONTROLL                                           
004440     .                                                                    
004441     EJECT                                                                
004442 IMS-GHNP-WDE221 SECTION.                                                 
004443     MOVE 'IMS-GHNP-WDE221'      TO WS-SEKTION                            
004444                                                                          
004445     STRING 'WDE211  (WDE211KY =' W-WDE111KY-X ')'                        
004446          DELIMITED BY SIZE INTO SSA1                                     
004447     MOVE 'WDE221  '            TO SSA2                                   
004448     MOVE '  GE' TO GODK-STATUSKODER                                      
004449     CALL CBLTDLI USING GHNP WDE2-PCB DLI-IO-WDE221 SSA1 SSA2             
004450     MOVE WDE2-STATUS-CODE TO STATUS-WS                                   
004451     PERFORM IMS-STATUSKONTROLL                                           
004452     .                                                                    
004453     EJECT                                                                
004454 IMS-GHNP-WDE221-KVAL SECTION.                                            
004455     MOVE 'IMS-GHNP-WDE221-KVAL'  TO WS-SEKTION                           
004456                                                                          
004457     STRING 'WDE211  (WDE211KY =' W-WDE111KY-X ')'                        
004458          DELIMITED BY SIZE INTO SSA1                                     
004459     STRING 'WDE221  *F(WDE221KY =' W-WDE121KY-X ')'                      
004460          DELIMITED BY SIZE INTO SSA2                                     
004461     MOVE '  GE' TO GODK-STATUSKODER                                      
004462     CALL CBLTDLI USING GHNP WDE2-PCB DLI-IO-WDE221 SSA1 SSA2             
004463     MOVE WDE2-STATUS-CODE TO STATUS-WS                                   
004464     PERFORM IMS-STATUSKONTROLL                                           
004465     .                                                                    
004466     EJECT                                                                
004467 IMS-REPL-WDE221 SECTION.                                                 
004468     MOVE 'IMS-REPL-WDE221'      TO WS-SEKTION                            
004469                                                                          
004470     MOVE '    '   TO GODK-STATUSKODER                                    
004471     CALL CBLTDLI USING REPL WDE2-PCB DLI-IO-WDE221                       
004472     MOVE WDE2-STATUS-CODE TO STATUS-WS                                   
004473     PERFORM IMS-STATUSKONTROLL                                           
004474     .                                                                    
004475     EJECT                                                                
004476 IMS-ISRT-WDE231 SECTION.                                                 
004477     MOVE 'IMS-ISRT-WDE231'      TO WS-SEKTION                            
004478                                                                          
004479     MOVE 'WDE231'  TO SSA1                                               
004480     MOVE '  II'    TO GODK-STATUSKODER                                   
004481     CALL CBLTDLI USING ISRT WDE2-PCB DLI-IO-WDE231 SSA1                  
004482     MOVE WDE2-STATUS-CODE TO STATUS-WS                                   
004483     PERFORM IMS-STATUSKONTROLL                                           
004484     .                                                                    
004485     EJECT                                                                
004486 IMS-GHU-WDE401-KVAL  SECTION.                                            
004487     MOVE 'IMS-GHU-WDE401-KVAL'  TO WS-SEKTION                            
004488                                                                          
004489     STRING 'WDE411  *F(WDE4FSEQ =' W-WDE4FSEQ-X ')'                      
004490          DELIMITED BY SIZE INTO SSA1                                     
004491     STRING 'WDE401  (WDE401KY =' W-WDE401KY-X ')'                        
004492          DELIMITED BY SIZE INTO SSA2                                     
004493     MOVE '    ' TO GODK-STATUSKODER                                      
004494     CALL CBLTDLI USING GHU WDE4-PCB DLI-IO-WDE401 SSA1 SSA2              
004495     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
004496     PERFORM IMS-STATUSKONTROLL                                           
004497     .                                                                    
004498     SKIP3                                                                
004499 IMS-GHNP-WDE401  SECTION.                                                
004500     MOVE 'IMS-GHNP-WDE401'   TO WS-SEKTION                               
004501                                                                          
004502     MOVE 'WDE401'      TO SSA1                                           
004503     MOVE '  GE' TO GODK-STATUSKODER                                      
004504     CALL CBLTDLI USING GHNP WDE4-PCB DLI-IO-WDE401 SSA1                  
004505     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
004506     PERFORM IMS-STATUSKONTROLL                                           
004507     .                                                                    
004508     SKIP3                                                                
004509 IMS-REPL-WDE401 SECTION.                                                 
004510     MOVE 'IMS-REPL-WDE401'      TO WS-SEKTION                            
004511                                                                          
004512     MOVE '    '   TO GODK-STATUSKODER                                    
004513     CALL CBLTDLI USING REPL WDE4-PCB DLI-IO-WDE401                       
004514     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
004515     PERFORM IMS-STATUSKONTROLL                                           
004516     .                                                                    
004517     EJECT                                                                
004518 IMS-GHU-WDE411-FSEQ  SECTION.                                            
004519     MOVE 'IMS-GHU-WDE411-FSEQ'  TO WS-SEKTION                            
004520                                                                          
004521     STRING 'WDE411  (WDE4FSEQ =' W-WDE4FSEQ-X ')'                        
004522          DELIMITED BY SIZE INTO SSA1                                     
004523     MOVE '    ' TO GODK-STATUSKODER                                      
004524     CALL CBLTDLI USING GHU WDE4-PCB DLI-IO-WDE411 SSA1                   
004525     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
004526     PERFORM IMS-STATUSKONTROLL                                           
004527     .                                                                    
004528     SKIP3                                                                
004529 IMS-GHN-WDE411-FSEQ  SECTION.                                            
004530     MOVE 'IMS-GHN-WDE411-FSEQ'  TO WS-SEKTION                            
004531                                                                          
004532     STRING 'WDE411  (WDE4FSEQ =' W-WDE4FSEQ-X ')'                        
004533          DELIMITED BY SIZE INTO SSA1                                     
004534     MOVE '  GEGB' TO GODK-STATUSKODER                                    
004535     CALL CBLTDLI USING GHN WDE4-PCB DLI-IO-WDE411 SSA1                   
004536     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
004537     PERFORM IMS-STATUSKONTROLL                                           
004538     .                                                                    
004539     SKIP3                                                                
004540 IMS-GHU-WDE411X-FSEQ  SECTION.                                           
004541     MOVE 'IMS-GHU-WDE411X-FSEQ'  TO WS-SEKTION                           
004542                                                                          
004543     STRING 'WDE411  (WDE4FSEQ =' W-WDE4FSEQ-X                            
004544                    '&IDPURAD  =' W-IDPURAD-WDE4-X ')'                    
004545          DELIMITED BY SIZE INTO SSA1                                     
004546     MOVE '    ' TO GODK-STATUSKODER                                      
004547     CALL CBLTDLI USING GHU WDE4X-PCB DLI-IO-WDE411 SSA1                  
004548     MOVE WDE4X-STATUS-CODE TO STATUS-WS                                  
004549     PERFORM IMS-STATUSKONTROLL                                           
004550     .                                                                    
004551     SKIP3                                                                
004552 IMS-REPL-WDE411 SECTION.                                                 
004553     MOVE 'IMS-REPL-WDE411'      TO WS-SEKTION                            
004554                                                                          
004555     MOVE '    '   TO GODK-STATUSKODER                                    
004556     CALL CBLTDLI USING REPL WDE4-PCB DLI-IO-WDE411                       
004557     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
004558     PERFORM IMS-STATUSKONTROLL                                           
004559     .                                                                    
004560     EJECT                                                                
004561 IMS-REPL-WDE411X SECTION.                                                
004562     MOVE 'IMS-REPL-WDE411X'      TO WS-SEKTION                           
004563                                                                          
004564     MOVE '    '   TO GODK-STATUSKODER                                    
004565     CALL CBLTDLI USING REPL WDE4X-PCB DLI-IO-WDE411                      
004566     MOVE WDE4X-STATUS-CODE TO STATUS-WS                                  
004567     PERFORM IMS-STATUSKONTROLL                                           
004568     .                                                                    
004569     EJECT                                                                
004570 IMS-GNP-WDE421  SECTION.                                                 
004571                                                                          
004572     STRING 'WDE421  (WDE421KY =' W-WDE4FSEQ-X ')'                        
004573          DELIMITED BY SIZE INTO SSA1                                     
004574     MOVE '    ' TO GODK-STATUSKODER                                      
004575     CALL CBLTDLI USING GNP WDE4-PCB DLI-IO-WDE421 SSA1                   
004576     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
004577     PERFORM IMS-STATUSKONTROLL                                           
004578     .                                                                    
004579     SKIP3                                                                
004580 IMS-GHU-WDE601  SECTION.                                                 
004581     MOVE 'IMS-GHU-WDE601'    TO WS-SEKTION                               
004582                                                                          
004583     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-E6-X ')'                     
004584          DELIMITED BY SIZE INTO SSA1                                     
004585     MOVE '    ' TO GODK-STATUSKODER                                      
004586     CALL CBLTDLI USING GHU WDE6-PCB DLI-IO-WDE601 SSA1                   
004587     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
004588     PERFORM IMS-STATUSKONTROLL                                           
004589     .                                                                    
004590     SKIP3                                                                
004591 IMS-REPL-WDE601 SECTION.                                                 
004592     MOVE 'IMS-REPL-WDE601'      TO WS-SEKTION                            
004593                                                                          
004594     MOVE '    '   TO GODK-STATUSKODER                                    
004595     CALL CBLTDLI USING REPL WDE6-PCB DLI-IO-WDE601                       
004596     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
004597     PERFORM IMS-STATUSKONTROLL                                           
004598     .                                                                    
004599     EJECT                                                                
004600 IMS-GHNP-WDE611  SECTION.                                                
004601     MOVE 'IMS-GHNP-WDE611'     TO WS-SEKTION                             
004602                                                                          
004603     STRING 'WDE611  (IDKOLLI  =' W-IDKOLLI-X ')'                         
004604          DELIMITED BY SIZE INTO SSA1                                     
004605     MOVE '  GE' TO GODK-STATUSKODER                                      
004606     CALL CBLTDLI USING GHNP WDE6-PCB DLI-IO-WDE611 SSA1                  
004607     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
004608     PERFORM IMS-STATUSKONTROLL                                           
004609     .                                                                    
004610     SKIP3                                                                
004611 IMS-REPL-WDE611  SECTION.                                                
004612     MOVE 'IMS-REPL-WDE611'    TO WS-SEKTION                              
004613                                                                          
004614     MOVE '    ' TO GODK-STATUSKODER                                      
004615     CALL CBLTDLI USING REPL WDE6-PCB DLI-IO-WDE611                       
004616     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
004617     PERFORM IMS-STATUSKONTROLL                                           
004618     .                                                                    
004619     SKIP3                                                                
004620 IMS-GHNP-WDE621 SECTION.                                                 
004621     MOVE 'IMS-GHNP-WDE621'    TO WS-SEKTION                              
004622                                                                          
004623     STRING 'WDE621  (KDKOLSTX =' W-KDKOLSTX-X                            
004624                    '&IDDCCROS =' SHIP-IDDC                               
004625                    '&IDTRPTNC =' W-IDTRPTNC-X                            
004626                    '&TIRECDAT >' W-TIRECXDAT-X ')'                       
004627     DELIMITED BY SIZE INTO SSA1                                          
004628     MOVE '  GE'      TO GODK-STATUSKODER                                 
004629     CALL CBLTDLI USING GHNP WDE6-PCB DLI-IO-WDE621 SSA1                  
004630     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
004631     PERFORM IMS-STATUSKONTROLL                                           
004632     .                                                                    
004633     SKIP3                                                                
004634 IMS-REPL-WDE621 SECTION.                                                 
004635     MOVE 'IMS-REPL-WDE621'    TO WS-SEKTION                              
004636                                                                          
004637     MOVE 'WDE621 ' TO SSA1                                               
004638     MOVE '  ' TO GODK-STATUSKODER                                        
004639     CALL CBLTDLI USING REPL WDE6-PCB DLI-IO-WDE621 SSA1                  
004640     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
004641     PERFORM IMS-STATUSKONTROLL                                           
004642     .                                                                    
004643     EJECT                                                                
004644 IMS-GHU-WDC711 SECTION.                                                  
004645     MOVE 'IMS-GHU-WDC711'     TO WS-SEKTION                              
004646                                                                          
004647     STRING 'WDC701  (WDC701KY =' W-WDC701KY-X ')'                        
004648          DELIMITED BY SIZE INTO SSA1                                     
004649     STRING 'WDC711  (IDPRQUES =' W-IDPRQUES-X ')'                        
004650          DELIMITED BY SIZE INTO SSA2                                     
004651     MOVE '  GE' TO GODK-STATUSKODER                                      
004652     CALL CBLTDLI USING GHU WDC7-PCB DLI-IO-WDC711 SSA1 SSA2              
004653     MOVE WDC7-STATUS-CODE TO STATUS-WS                                   
004654     PERFORM IMS-STATUSKONTROLL                                           
004655     .                                                                    
004656     SKIP3                                                                
004657 IMS-REPL-WDC711  SECTION.                                                
004658     MOVE 'IMS-REPL-WDC711'    TO WS-SEKTION                              
004659                                                                          
004660     MOVE '    ' TO GODK-STATUSKODER                                      
004661     CALL CBLTDLI USING REPL WDC7-PCB DLI-IO-WDC711                       
004662     MOVE WDC7-STATUS-CODE TO STATUS-WS                                   
004663     PERFORM IMS-STATUSKONTROLL                                           
004664     .                                                                    
004665     SKIP3                                                                
004666 IMS-GU-WDK601 SECTION.                                                   
004667     MOVE 'IMS-GU-WDK601'     TO WS-SEKTION                               
004668                                                                          
004669     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
004670          DELIMITED BY SIZE INTO SSA1                                     
004671     MOVE '    ' TO GODK-STATUSKODER                                      
004672     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
004673     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
004674     PERFORM IMS-STATUSKONTROLL                                           
004675     .                                                                    
004676 IMS-GNP-WDK611 SECTION.                                                  
004677     MOVE 'IMS-GNP-WDK611'     TO WS-SEKTION                              
004678                                                                          
004679     MOVE    'WDK611  '           TO    SSA1                              
004680     MOVE '    ' TO GODK-STATUSKODER                                      
004681     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
004682     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
004683     PERFORM IMS-STATUSKONTROLL                                           
004684     .                                                                    
004685     SKIP3                                                                
004686 IMS-GU-WDK711  SECTION.                                                  
004687     MOVE 'IMS-GU-WDK711'        TO WS-SEKTION                            
004688                                                                          
004689     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
004690          DELIMITED BY SIZE INTO SSA1                                     
004691     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
004692          DELIMITED BY SIZE INTO SSA2                                     
004693     MOVE '  GE' TO GODK-STATUSKODER                                      
004694     CALL CBLTDLI USING GU  WDK7-PCB DLI-IO-WDK711 SSA1 SSA2              
004695     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
004696     PERFORM IMS-STATUSKONTROLL                                           
004697     .                                                                    
004698     SKIP3                                                                
004699 IMS-GU-WDK712  SECTION.                                                  
004700     MOVE 'IMS-GU-WDK712'        TO WS-SEKTION                            
004701                                                                          
004702     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
004703          DELIMITED BY SIZE INTO SSA1                                     
004704     STRING 'WDK712  (IDLAND   =' W-IDLAND-X ')'                          
004705          DELIMITED BY SIZE INTO SSA2                                     
004706     MOVE '  GE' TO GODK-STATUSKODER                                      
004707     CALL CBLTDLI USING GU  WDK7-PCB DLI-IO-WDK712 SSA1 SSA2              
004708     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
004709     PERFORM IMS-STATUSKONTROLL                                           
004710     .                                                                    
004711     SKIP3                                                                
004712 IMS-GU-WDB101              SECTION.                                      
004713     MOVE 'IMS-GU-WDB101'   TO WS-SEKTION                                 
004714                                                                          
004715     STRING 'WDB101  (WDB101KY =' W-WDB101KY-X ')'                        
004716          DELIMITED BY SIZE INTO SSA1                                     
004717     MOVE '    '              TO GODK-STATUSKODER                         
004718                                                                          
004719     CALL CBLTDLI USING GU WDB1-PCB DLI-IO-WDB101 SSA1                    
004720     MOVE WDB1-STATUS-CODE    TO STATUS-WS                                
004721     PERFORM IMS-STATUSKONTROLL                                           
004722     .                                                                    
004723     EJECT                                                                
004724 IMS-GU-WDB201              SECTION.                                      
004725     MOVE 'IMS-GU-WDB201'   TO WS-SEKTION                                 
004726                                                                          
004727     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
004728          DELIMITED BY SIZE INTO SSA1                                     
004729     MOVE '  GE'              TO GODK-STATUSKODER                         
004730                                                                          
004731     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
004732     MOVE WDB2-STATUS-CODE    TO STATUS-WS                                
004733     PERFORM IMS-STATUSKONTROLL                                           
004734     .                                                                    
004735     EJECT                                                                
004736 IMS-GU-WDB301 SECTION.                                                   
004737     MOVE 'IMS-GU-WDB301'   TO WS-SEKTION                                 
004738                                                                          
004739     STRING 'WDB301  (WDB301KY =' W-WDB301KY-X                            
004740                    '+WDB301KY =' W-WDB301KY-DEF-X ')'                    
004741          DELIMITED BY SIZE INTO SSA1                                     
004742     MOVE '  GE' TO GODK-STATUSKODER                                      
004743     CALL CBLTDLI USING GU WDB3-PCB DLI-IO-WDB301 SSA1                    
004744     MOVE WDB3-STATUS-CODE TO STATUS-WS                                   
004745     PERFORM IMS-STATUSKONTROLL                                           
004746     .                                                                    
004747     SKIP3                                                                
004748 IMS-GU-WDD311    SECTION.                                                
004749     MOVE 'IMS-GU-WDD311'  TO WS-SEKTION                                  
004750                                                                          
004751     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
004752            DELIMITED BY SIZE INTO SSA1                                   
004753     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
004754            DELIMITED BY SIZE INTO SSA2                                   
004755     MOVE '  GE' TO GODK-STATUSKODER                                      
004756     CALL CBLTDLI USING GU  WDD3-PCB DLI-IO-WDD311 SSA1 SSA2              
004757     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
004758     PERFORM IMS-STATUSKONTROLL                                           
004759     .                                                                    
004760     EJECT                                                                
004761 IMS-ISRT-WDL901 SECTION.                                                 
004762     MOVE 'IMS-ISRT-WDL901'      TO WS-SEKTION                            
004763                                                                          
004764     MOVE 'WDL901 ' TO SSA1                                               
004765     MOVE '  II' TO GODK-STATUSKODER                                      
004766     CALL CBLTDLI USING ISRT WDL9-PCB DLI-IO-WDL901 SSA1                  
004767     MOVE WDL9-STATUS-CODE TO STATUS-WS                                   
004768     PERFORM IMS-STATUSKONTROLL                                           
004769     .                                                                    
004770     SKIP3                                                                
004771 IMS-GU-WDQ201 SECTION.                                                   
004772     MOVE 'IMS-GU-WDQ201'      TO WS-SEKTION                              
004773                                                                          
004774     STRING 'WDQ201  (IDORDER  =' W-IDORDER-X ')'                         
004775          DELIMITED BY SIZE INTO SSA1                                     
004776     MOVE '  GE' TO GODK-STATUSKODER                                      
004777     CALL CBLTDLI USING GU WDQ2-PCB DLI-IO-WDQ201 SSA1                    
004778     MOVE WDQ2-STATUS-CODE TO STATUS-WS                                   
004779     PERFORM IMS-STATUSKONTROLL                                           
004780     .                                                                    
004781     SKIP2                                                                
004782 IMS-GU-WDQ201-CSEQ SECTION.                                              
004783                                                                          
004784     STRING 'WDQ201  (WDQ2CSEQ =' W-WDQ2CSEQ ')'                          
004785          DELIMITED BY SIZE INTO SSA1                                     
004786     MOVE '  GE'              TO GODK-STATUSKODER                         
004787     CALL CBLTDLI USING GU WDQ2C-PCB DLI-IO-WDQ201-C SSA1                 
004788     MOVE WDQ2C-STATUS-CODE    TO STATUS-WS                               
004789     PERFORM IMS-STATUSKONTROLL                                           
004790     .                                                                    
004791 IMS-GHU-SEQB-WDA601 SECTION.                                             
004792     STRING 'WDA601  (WDA6BSEQ>=' W-WDA601KY-MIN-X                        
004793                    '&WDA6BSEQ<=' W-WDA601KY-MAX-X ')'                    
004794            DELIMITED BY SIZE INTO SSA1                                   
004795     MOVE '  GEGB'               TO GODK-STATUSKODER                      
004796     CALL  CBLTDLI  USING GHU   WDA6B-PCB DLI-IO-WDA601 SSA1              
004797     MOVE WDA6B-STATUS-CODE TO STATUS-WS                                  
004798     PERFORM IMS-STATUSKONTROLL                                           
004799     .                                                                    
004800     SKIP2                                                                
004801 IMS-GHN-SEQB-WDA601 SECTION.                                             
004802     STRING 'WDA601  (WDA6BSEQ>=' W-WDA601KY-MIN-X                        
004803                    '&WDA6BSEQ<=' W-WDA601KY-MAX-X ')'                    
004804            DELIMITED BY SIZE INTO SSA1                                   
004805     MOVE '  GEGB'               TO GODK-STATUSKODER                      
004806     CALL  CBLTDLI  USING GHN   WDA6B-PCB DLI-IO-WDA601 SSA1              
004807     MOVE WDA6B-STATUS-CODE TO STATUS-WS                                  
004808     PERFORM IMS-STATUSKONTROLL                                           
004809     .                                                                    
004810     SKIP2                                                                
004811 IMS-REPL-SEQB-WDA601 SECTION.                                            
004812     MOVE 'WDA601  '           TO SSA1                                    
004813     MOVE '    '               TO GODK-STATUSKODER                        
004814     CALL  CBLTDLI  USING REPL WDA6B-PCB DLI-IO-WDA601 SSA1               
004815     MOVE WDA6B-STATUS-CODE TO STATUS-WS                                  
004816     PERFORM IMS-STATUSKONTROLL                                           
004817     .                                                                    
004818     SKIP3                                                                
004819 IMS-GU-SEQB-WDA601 SECTION.                                              
004820     STRING 'WDA601  (WDA6BSEQ>=' W-WDA6BSEQ-MIN-X                        
004821                    '&WDA6BSEQ<=' W-WDA6BSEQ-MAX-X ')'                    
004822            DELIMITED BY SIZE INTO SSA1                                   
004823     MOVE '  GE'                 TO GODK-STATUSKODER                      
004824     CALL  CBLTDLI  USING GHU  WDA6B-PCB DLI-IO-WDA601 SSA1               
004825     MOVE WDA6B-STATUS-CODE TO STATUS-WS                                  
004826     PERFORM IMS-STATUSKONTROLL                                           
004827     .                                                                    
004828     SKIP2                                                                
004829 IMS-GU-WDB601    SECTION.                                                
004830     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
004831          DELIMITED BY SIZE INTO SSA1                                     
004832     MOVE '  GE' TO GODK-STATUSKODER                                      
004833     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
004834     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
004835     PERFORM IMS-STATUSKONTROLL                                           
004836     IF SEGMENT-SAKNAS                                                    
004837        MOVE SPACE TO DCS-KDDC                                            
004838     END-IF                                                               
004839     .                                                                    
004840 IMS-GU-WDB617 SECTION.                                                   
004841                                                                          
004842     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
004843          DELIMITED BY SIZE INTO SSA1                                     
004844     STRING 'WDB617  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
004845          DELIMITED BY SIZE INTO SSA2                                     
004846     MOVE '  GE' TO GODK-STATUSKODER                                      
004847     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB617 SSA1 SSA2               
004848     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
004849     PERFORM IMS-STATUSKONTROLL                                           
004850     .                                                                    
004851     SKIP3                                                                
004852 IMS-GU-WDA501 SECTION.                                                   
004853                                                                          
004854     STRING 'WDA501  (WDA501KY>=' W-WDA501KY-A5-MIN-X                     
004855                    '&WDA501KY<=' W-WDA501KY-A5-MAX-X                     
004856                    '&KDORDKL  =' W-KDORDKL-X                             
004857                    '&IDKNDRFL =' W-IDKUNDRF-LEV-X ')'                    
004858          DELIMITED BY SIZE INTO SSA1                                     
004859     MOVE '  GE'              TO GODK-STATUSKODER                         
004860     CALL CBLTDLI USING GU WDA5-PCB DLI-IO-WDA501  SSA1                   
004861     MOVE WDA5-STATUS-CODE    TO STATUS-WS                                
004862     PERFORM IMS-STATUSKONTROLL                                           
004863     .                                                                    
004864 IMS-STATUSKONTROLL SECTION.                                              
004865                                                                          
004866     SET STATUS-IX TO 1                                                   
004867     SEARCH GODK-STATUS                                                   
004868       AT END                                                             
004869         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
004870         DELIMITED BY SIZE INTO FELTEXT                                   
004871         CALL FELLOG                                                      
004872       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
004873         CONTINUE                                                         
004874     END-SEARCH                                                           
004880     .                                                                    
