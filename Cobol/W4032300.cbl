000001 ID DIVISION.                                                             
000002     SKIP2                                                                
000003 PROGRAM-ID.     W4032300.                                                
000004 AUTHOR.         SVANTE BJÖRKBERG.                                        
000005 DATE-WRITTEN.   JULI 86.                                                 
000006                                                                          
000007     REMARKS.                                                             
000008*    FUNKTION.                                                            
000009*                                                                         
000010*        SALDOBILD:                                                       
000011*                                                                         
000012*           INDATA            : DISTR, KUND, KUNDRF EL. PRODNR,           
000013*                               SAMT RADNR.                               
000014*                                                                         
000015*           BILDEN SVARAR MED : PACK-UNDERLAG, RADER                      
000016*                                                                         
000017*    INDATA.                                                              
000018*        TRANSAKTION: W4T323                                              
000019*                     W4T323 7                                            
000020*                     W4T323 8                                            
000021*        MID:         W4I32301                                            
000022*                                                                         
000023*    UTDATA.                                                              
000024*        MOD:         W4O32301                                            
000025*    SKIP3                                                                
000026 ENVIRONMENT DIVISION.                                                    
000027     SKIP3                                                                
000028 DATA DIVISION.                                                           
000029     EJECT                                                                
000030 WORKING-STORAGE SECTION.                                                 
000031                                                                          
000032*    -- CHECKED BY WY2000                                                 
000033 77   PROGRAM-NAMN           VALUE 'W4032300'                             
000034                                 PIC X(8).                                
000035 77  JA                          PIC X       VALUE 'J'.                   
000036 77  NEJ                         PIC X       VALUE 'N'.                   
000037 77  IDSKYLT-ENG                 PIC X(3)    VALUE 'GB '.                 
000038 77  IDSKYLT-SVE                 PIC X(3)    VALUE 'S  '.                 
000039 77  KUNDORDER-AER-NYCKEL        PIC X.                                   
000040 01  FILLER          PIC X(8) VALUE 'AAAAAAAA'.                           
000041 01  WS-KVBEART      PIC 9(8)V9(5).                                       
000042 01  WS-KVBEART-REST PIC 9(8).                                            
000043 01  FILLER          PIC X(8) VALUE 'AAAAAAAA'.                           
000045 77  NYCKLAR-OK                  PIC X.                                   
000046 77  RADER-SLUT                  PIC X.                                   
000047 77  KOLLIN-SLUT                 PIC X.                                   
000048 77  SPRAK-IX                    PIC S9(9)   VALUE ZERO COMP SYNC.        
000049 77  CD-IX                       PIC S9(9)   VALUE ZERO COMP SYNC.        
000052 77  ACCRAD                      PIC S9(3)   COMP-3.                      
000053 77  TEMP-IDKOLLI                PIC Z(4)9.                               
000054 77  TEMP-FORSTA-IDPURAD         PIC S9(4).                               
000055 77  TEMP-FORSTA-IDKOLLI         PIC S9(5).                               
000056 77  TEMP-IDDISTR                PIC S9(4).                               
000057 77  TEMP-IDKUNDNR               PIC S9(6).                               
000058 77  TEMP-IDORDNR                PIC S9(5).                               
000059 77  SPAR-FORSTA-Q4-IDORDER      PIC S9(7).                               
000060 77  SPAR-FORSTA-Q4-IDARTNR      PIC S9(9).                               
000061 77  SPAR-FORSTA-Q4-IDLOPNR      PIC S9(3).                               
000062 77  WS-SPAR-KVORDRAD-LEVPL      PIC S9(5)   COMP-3 VALUE ZERO.           
000063 77  WS-EGEN-TRANS               PIC X(4)    VALUE '4323'.                
000064 77  WS-IDTRANS                  PIC X(4).                                
000065     88  WS-GODKAEND-BILD                    VALUE '4321' '4322'          
000066                                            '4323' '4324' '4325'.         
000067     88  EGEN-MID                            VALUE '4323'.                
000068 77  WS-SLINGA-KLAR              PIC X(1).                                
000069     88  SLINGA-KLAR                         VALUE 'J'.                   
000070                                                                          
000074 77  IDPURAD-HITTAD-SW           PIC X(1).                                
000075     88 IDPURAD-HITTAD                       VALUE 'J'.                   
000076                                                                          
000077 77  FEL-PLKLST-SW              PIC X(1).                                 
000078     88  FEL-PLKLST                          VALUE 'J'.                   
000079                                                                          
000080 01  W-SPAR-IDKUNDRF.                                                     
000081     03  FILLER                  PIC X(2)    VALUE '00'.                  
000082     03  W-SPAR-IDORDNR5         PIC X(5)    VALUE '+++++'.               
000083     03  FILLER                  PIC X(3)    VALUE '+++'.                 
000084 01  IDANSTNR-WS                 PIC X(5).                                
000085 01  IDDISTR-WS                  PIC X(4).                                
000086 01  IDKUNDNR-WS                 PIC X(6).                                
000087 01  IDKUNDRF-WS                 PIC X(5).                                
000088 01  IDPURAD-WS                  PIC X(5).                                
000089 01  IDPRODNR-WS                 PIC X(7).                                
000090 01  IDPRODNR-WS-JFR             PIC X(7).                                
000091*                                                                         
000092*      --- VALID IDDD CODES                                               
000093*                                                                         
000094*01    -COPY WWDC99                                                       
000095       EJECT                                                              
000096*                                                                         
000097 01  TRANSFER-KUND               PIC 9(7).                                
000098     88 TRANSFER-KUNDNR          VALUE 0000511                            
000099                                       0000512                            
000100                                       0000513.                           
000101     88  RETUR-KUNDNR            VALUE 0000051.                           
000102*                                                                         
000103 01  DYNAMISKA-SUBPROGRAM.                                                
000104     03  CBLTDLI                 PIC X(8) VALUE 'CBLTDLI '.               
000105     03  FELLOG                  PIC X(8) VALUE 'FELLOG  '.               
000106     03  W005INIT                PIC X(8) VALUE 'W005INIT'.               
000107     EJECT                                                                
000108*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
000109*01 -COPY WMSGINIT                                                        
000110     EJECT                                                                
000111*01  -COPY WWDIST08.                                                      
000112     EJECT                                                                
000113 01    NYCKLAR-TILL-DLI.                                                  
000114   03    W-WDE4E1KY-MAX-X.                                                
000115     05    W-IDPRODNR-WDE4E-MAX  PIC S9(7)   VALUE ZERO  COMP-3.          
000116     05    W-WDE4E1-MAX          PIC X(19)   VALUE HIGH-VALUE.            
000117                                                                          
000118   03    W-WDE4E1KY-MIN-X.                                                
000119     05    W-IDPRODNR-WDE4E-MIN  PIC S9(7)   VALUE ZERO  COMP-3.          
000120     05    W-WDE4E1-MIN          PIC X(19)   VALUE LOW-VALUE.             
000121                                                                          
000122     EJECT                                                                
000123   03    W-WDE4AKEY-X.                                                    
000124     05    W-E4A1-IDDISTR        PIC S9(5)   VALUE ZERO  COMP-3.          
000125     05    W-E4A1-IDKUNDNR       PIC S9(7)   VALUE ZERO  COMP-3.          
000126     05    W-E4A1-IDKUNDRF       PIC X(10).                               
000127*                                                                         
000128   03    W-WDE4KEY-X.                                                     
000129     05    W-E401-IDDISTR        PIC S9(5)   VALUE ZERO  COMP-3.          
000130     05    W-E401-IDKUNDNR       PIC S9(7)   VALUE ZERO  COMP-3.          
000131     05    W-E401-IDKUNDRF.                                               
000132       07    W-E401-IDORDNR      PIC 9(5)    VALUE ZERO.                  
000133       07    FILLER              PIC X(5)    VALUE SPACE.                 
000134     05    W-E401-IDPRODNR       PIC S9(7)   VALUE ZERO  COMP-3.          
000135     05    W-E401-IDPLKLST       PIC S9(3)   VALUE ZERO  COMP-3.          
000136   03    W-WDE4KEY-MAX-X.                                                 
000137     05    W-E401-IDDISTR-MAX    PIC S9(5)   VALUE ZERO  COMP-3.          
000138     05    W-E401-IDKUNDNR-MAX   PIC S9(7)   VALUE ZERO  COMP-3.          
000139     05    W-E401-IDKUNDRF-MAX.                                           
000140       07    W-E401-IDORDNR-MAX  PIC 9(5)    VALUE ZERO.                  
000141       07    FILLER              PIC X(5)    VALUE SPACE.                 
000142     05    W-E401-IDPRODNR-MAX   PIC S9(7)   VALUE ZERO  COMP-3.          
000143     05    W-E401-IDPLKLST-MAX   PIC S9(3)   VALUE 999   COMP-3.          
000144   03    W-IDPRODNR-X.                                                    
000145     05    W-IDPRODNR            PIC S9(7)   VALUE ZERO  COMP-3.          
000146*                                                                         
000147   03    W-IDPURAD-X.                                                     
000148     05    WS-IDPURAD            PIC S9(5)   VALUE ZERO  COMP-3.          
000149*                                                                         
000150   03    W-WDE4BSEQ-X.                                                    
000151     05    W-WDE4B-IDPRODNR      PIC S9(7)   VALUE ZERO  COMP-3.          
000152     05    W-IDPURAD             PIC S9(5)   VALUE ZERO  COMP-3.          
000153*                                                                         
000154   03    W-WDE4BSEQ-MIN-X.                                                
000155     05    W-IDPRODNR-MIN        PIC S9(7)   VALUE ZERO  COMP-3.          
000156     05    W-IDPURAD-MIN         PIC S9(5)   VALUE ZERO  COMP-3.          
000157   03    W-WDE4BSEQ-MAX-X.                                                
000158     05    W-IDPRODNR-MAX        PIC S9(7)   VALUE ZERO  COMP-3.          
000159     05    W-IDPURAD-MAX         PIC S9(5)   VALUE ZERO  COMP-3.          
000160*                                                                         
000161   03    W-SAMMANLAGD-KOLLI-NYCKEL-X.                                     
000162     05    W-K-IDPRODNR          PIC S9(7)   VALUE ZERO  COMP-3.          
000163     05    W-K-IDKOLLI           PIC S9(5)   VALUE ZERO  COMP-3.          
000164*                                                                         
000165   03    W-IDARTNR-X.                                                     
000166     05    W-IDARTNR             PIC S9(9)   VALUE ZERO  COMP-3.          
000167   03    W-IDORDER-X.                                                     
000168     05    W-IDORDER             PIC S9(7)   VALUE ZERO  COMP-3.          
000169   03    W-KDSEGKEY-X.                                                    
000170     05    W-KDSEGKEY            PIC X       VALUE '1'.                   
000171   03    W-IDDC-X.                                                        
000172     05    W-IDDC                PIC X(02).                               
000173   03    W-IDSKYLT-X.                                                     
000174     05    W-IDSKYLT             PIC X(3).                                
000175   03    W-IDPRC-X.                                                       
000176     05    W-IDPRC               PIC X(4).                                
000177   03    W-KDODELST-X.                                                    
000178     05    W-KDODELST            PIC X       VALUE 'R'.                   
000179*                                                                         
000180   03    W-WDQ4ASEQ-MIN-X.                                                
000181     05    W-IDORDER-MIN         PIC S9(7) VALUE ZERO COMP-3.             
000182     05    W-IDARTNR-MIN         PIC S9(9) VALUE ZERO COMP-3.             
000183     05    W-IDLOPNR-MIN         PIC S9(3) VALUE ZERO COMP-3.             
000184   03    W-WDQ4ASEQ-MAX-X.                                                
000185     05    W-IDORDER-MAX         PIC S9(7) VALUE ZERO COMP-3.             
000186     05    W-IDARTNR-MAX       PIC S9(9) VALUE 999999999 COMP-3.          
000187     05    W-IDLOPNR-MAX         PIC S9(3) VALUE 999     COMP-3.          
000188*                                                                         
000189   03    W-WDQ301KY-MIN-X.                                                
000190     05    W-301-IDORDER-MIN     PIC S9(7)   VALUE ZERO  COMP-3.          
000191     05    W-301-IDDC-MIN        PIC X(02)   VALUE SPACE.                 
000192     05    W-301-IDPRODNR-MIN    PIC S9(7)   VALUE ZERO  COMP-3.          
000193     05    W-301-IDPLKLST-MIN    PIC S9(3)   VALUE ZERO  COMP-3.          
000194   03    W-WDQ301KY-MAX-X.                                                
000195     05    W-301-IDORDER-MAX     PIC S9(7)   VALUE ZERO  COMP-3.          
000196     05    W-301-IDDC-MAX        PIC X(02)   VALUE SPACE.                 
000197     05    W-301-IDPRODNR-MAX    PIC S9(7)   VALUE ZERO  COMP-3.          
000198     05    W-301-IDPLKLST-MAX    PIC S9(3)   VALUE 999   COMP-3.          
000199*                                                                         
000200   03    W-WDQ4A1KY-MIN-X.                                                
000201     05    W-Q4A1-IDORDER-MIN     PIC S9(7)   VALUE ZERO  COMP-3.         
000202     05    W-Q4A1-IDARTNR-MIN     PIC S9(9)   VALUE ZERO  COMP-3.         
000203     05    W-Q4A1-IDLOPNR-MIN     PIC S9(3)   VALUE ZERO  COMP-3.         
000204     05    W-Q4A1-IDDC-MIN        PIC X(02)   VALUE SPACE.                
000205     05    W-Q4A1-ADLAGOMR-MIN    PIC S9(3)   VALUE ZERO  COMP-3.         
000206     05    W-Q4A1-ADGANG-MIN      PIC S9(3)   VALUE ZERO  COMP-3.         
000207     05    W-Q4A1-ADPLATS-MIN     PIC S9(5)   VALUE ZERO  COMP-3.         
000208   03    W-WDQ4A1KY-MAX-X.                                                
000209     05    W-Q4A1-IDORDER-MAX     PIC S9(7)   VALUE ZERO  COMP-3.         
000210     05    W-Q4A1-IDARTNR-MAX   PIC S9(9) VALUE 999999999 COMP-3.         
000211     05    W-Q4A1-IDLOPNR-MAX     PIC S9(3)   VALUE 999   COMP-3.         
000212     05    W-Q4A1-IDDC-MAX        PIC X(02)   VALUE SPACE.                
000213     05    W-Q4A1-ADLAGOMR-MAX    PIC S9(3)   VALUE 999   COMP-3.         
000214     05    W-Q4A1-ADGANG-MAX      PIC S9(3)   VALUE 999   COMP-3.         
000215     05    W-Q4A1-ADPLATS-MAX     PIC S9(5)   VALUE 99999 COMP-3.         
000216*                                                                         
000217   03    W-WDQ401KY-MIN-X.                                                
000218     05    W-Q4-IDORDER-MIN       PIC S9(7)   VALUE ZERO  COMP-3.         
000219     05    W-Q4-IDDC-MIN          PIC X(02)   VALUE SPACE.                
000220     05    W-Q4-ADLAGOMR-MIN      PIC S9(3)   VALUE ZERO  COMP-3.         
000221     05    W-Q4-ADGANG-MIN        PIC S9(3)   VALUE ZERO  COMP-3.         
000222     05    W-Q4-ADPLATS-MIN       PIC S9(5)   VALUE ZERO  COMP-3.         
000223     05    W-Q4-IDARTNR-MIN       PIC S9(9)   VALUE ZERO  COMP-3.         
000224     05    W-Q4-IDLOPNR-MIN       PIC S9(3)   VALUE ZERO  COMP-3.         
000225   03    W-WDQ401KY-MAX-X.                                                
000226     05    W-Q4-IDORDER-MAX       PIC S9(7)   VALUE ZERO  COMP-3.         
000227     05    W-Q4-IDDC-MAX          PIC X(02)   VALUE SPACE.                
000228     05    W-Q4-ADLAGOMR-MAX      PIC S9(3)   VALUE 999   COMP-3.         
000229     05    W-Q4-ADGANG-MAX        PIC S9(3)   VALUE 999   COMP-3.         
000230     05    W-Q4-ADPLATS-MAX       PIC S9(5)   VALUE 99999 COMP-3.         
000231     05    W-Q4-IDARTNR-MAX     PIC S9(9) VALUE 999999999 COMP-3.         
000232     05    W-Q4-IDLOPNR-MAX       PIC S9(3)   VALUE 999   COMP-3.         
000233     EJECT                                                                
000234 01    MEDDELANDE.                                                        
000235   03    FEL1.                                                            
000236     05    FILLER                PIC X(40)   VALUE                        
000237           '749 FEL NYCKEL                          '.                    
000238     05    FILLER                PIC X(40)   VALUE                        
000239           '749 WRONG KEY                           '.                    
000240   03 FILLER                     REDEFINES FEL1.                          
000241     05    FEL-1                 PIC X(40)   OCCURS 2.                    
000242                                                                          
000243   03    FEL2.                                                            
000244     05    FILLER                PIC X(40)   VALUE                        
000245           '701 ORDERN SAKNAS                       '.                    
000246     05    FILLER                PIC X(40)   VALUE                        
000247           '701 ORDER MISSING                       '.                    
000248   03 FILLER                     REDEFINES FEL2.                          
000249     05    FEL-2                 PIC X(40)   OCCURS 2.                    
000250                                                                          
000251   03    MED11.                                                           
000252     05    FILLER                PIC X(40)   VALUE                        
000253           '    INGA FLER RADER FINNS               '.                    
000254     05    FILLER                PIC X(40)   VALUE                        
000255           '    NO MORE LINES                       '.                    
000256   03 FILLER                     REDEFINES MED11.                         
000257     05    MED-11                 PIC X(40)   OCCURS 2.                   
000258                                                                          
000259   03    MED12.                                                           
000260     05    FILLER                PIC X(40)   VALUE                        
000261           '    INGA FLER RADER FINNS               '.                    
000262     05    FILLER                PIC X(40)   VALUE                        
000263           '    NO MORE LINES                       '.                    
000264   03 FILLER                     REDEFINES MED12.                         
000265     05    MED-12                 PIC X(40)   OCCURS 2.                   
000266                                                                          
000267   03    MED13.                                                           
000268     05    FILLER                PIC X(40)   VALUE                        
000269           '    INGA FLER RADER FINNS               '.                    
000270     05    FILLER                PIC X(40)   VALUE                        
000271           '    NO MORE LINES                       '.                    
000272   03 FILLER                     REDEFINES MED13.                         
000273     05    MED-13                 PIC X(40)   OCCURS 2.                   
000274                                                                          
000275   03    MED21.                                                           
000276     05    FILLER                PIC X(40)   VALUE                        
000277           '    FLER RADER FINNS                    '.                    
000278     05    FILLER                PIC X(40)   VALUE                        
000279           '    MORE LINES                          '.                    
000280   03 FILLER                     REDEFINES MED21.                         
000281     05    MED-21                PIC X(40)   OCCURS 2.                    
000282                                                                          
000283   03    MED22.                                                           
000284     05    FILLER                PIC X(40)   VALUE                        
000285           '    FLER RADER FINNS                    '.                    
000286     05    FILLER                PIC X(40)   VALUE                        
000287           '    MORE LINES                          '.                    
000288   03 FILLER                     REDEFINES MED22.                         
000289     05    MED-22                PIC X(40)   OCCURS 2.                    
000290                                                                          
000291   03    MED23.                                                           
000292     05    FILLER                PIC X(40)   VALUE                        
000293           '    FLER RADER FINNS                    '.                    
000294     05    FILLER                PIC X(40)   VALUE                        
000295           '    MORE LINES                          '.                    
000296   03 FILLER                     REDEFINES MED23.                         
000297     05    MED-23                PIC X(40)   OCCURS 2.                    
000298     EJECT                                                                
000299                                                                          
000300******************************************************************        
000301*                                                                         
000302*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
000303*                                                                         
000304 01    FILLER                    PIC X(16)   VALUE 'MFS-WS'.              
000305     SKIP3                                                                
000306*01  MID -COPY W4I32301.                                                  
000307     EJECT                                                                
000308*01    -COPY WMSGAREA                                                     
000309     EJECT                                                                
000310*  03  MOD -COPY W4O32301 -RED MSG-AREA.                                  
000311     EJECT                                                                
000312*01    -COPY WMFSAREA                                                     
000313     EJECT                                                                
000314******************************************************************        
000315*                                                                         
000316*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
000317*                                                                         
000318 01    IMS-WS.                                                            
000319   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
000320     SKIP3                                                                
000321*                        **** STATUS-KOD FRÅN IMS                         
000322   03    STATUS-WS               PIC XX.                                  
000323     88    SEGMENT-FINNS                     VALUE '  '.                  
000324     88    SEGMENT-SAKNAS                    VALUE 'GE'.                  
000325     88    END-OF-DATA                       VALUE 'GB'.                  
000326   03    STATUS-WDE401-SEK-WS    PIC XX.                                  
000327     88    WDE401-SEK-FINNS                  VALUE '  '.                  
000328     88    WDE401-SEK-SAKNAS                 VALUE 'GE' 'GB'.             
000329     SKIP3                                                                
000330   03    GODK-STATUSKODER.                                                
000331     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
000332     SKIP3                                                                
000333 01    SSA1                      PIC X(128).                              
000334 01    SSA2                      PIC X(160).                              
000335 01    SSA3                      PIC X(96).                               
000336     EJECT                                                                
000337*                            IMS FUNKTIONSKODER                           
000338*01    -COPY W0003                                                        
000339     EJECT                                                                
000340*    ---  DLI INPUT-OUTPUT AREA                                           
000341 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
000342     SKIP2                                                                
000343 01    DLI-IO-AREA.                                                       
000344   03    IO-AREA                 PIC X(900)  VALUE SPACE.                 
000345     SKIP3                                                                
000346*  03       -COPY WDE401  -RED IO-AREA.                                   
000347     EJECT                                                                
000348*  03  ORAD-AREA   -COPY WDE411  -RED IO-AREA.                            
000349     EJECT                                                                
000350*  03  KKOLLI-AREA -COPY WDE421  -RED IO-AREA.                            
000351     EJECT                                                                
000352*  03       -COPY WDE4A1  -RED IO-AREA.                                   
000353     EJECT                                                                
000354*  03       -COPY WDK611  -RED IO-AREA.                                   
000355     EJECT                                                                
000356*  03       -COPY WDK711  -RED IO-AREA.                                   
000357     EJECT                                                                
000358*  03       -COPY WDD311  -RED IO-AREA -PRE BENA-.                        
000359     EJECT                                                                
000360*  03       -COPY WDQ301  -RED IO-AREA.                                   
000361     EJECT                                                                
000362*  03       -COPY WDQ401  -RED IO-AREA -PRE Q4-.                          
000363     EJECT                                                                
000364*  03       -COPY WDQ4A1  -RED IO-AREA.                                   
000365     EJECT                                                                
000366*  03  VORD-AREA -COPY WDE601  -RED IO-AREA.                              
000367     EJECT                                                                
000368*  03  ORAD-AREA -COPY WDE411  -PRE ARB-.                                 
000369     EJECT                                                                
000370*  03  KKOLLI-AREA -COPY WDE421  -PRE ARB-.                               
000371     EJECT                                                                
000372*  03  WDE4E-AREA -COPY WDE4E1                                            
000373     EJECT                                                                
000374 01  FILLER                      PIC X(16) VALUE 'DLI-IO-Q212'.           
000375     SKIP2                                                                
000376 01    DLI-IO-Q212.                                                       
000377*    03  -COPY WDQ212                                                     
000378 01  FILLER                      PIC X(16) VALUE 'DLI-IO-Q221'.           
000379     SKIP2                                                                
000380 01    DLI-IO-Q221.                                                       
000381*    03  -COPY WDQ221                                                     
000382     EJECT                                                                
000392 LINKAGE SECTION.                                                         
000393*01    -COPY W0009     -PRE MSG-                                          
000394     EJECT                                                                
000395*01    -COPY W0008     -PRE USEA-                                         
000396     05  FILLER                  PIC X.                                   
000397     SKIP3                                                                
000398*01    -COPY W0008     -PRE WDE4A-                                        
000399     05  FILLER                  PIC X.                                   
000400     SKIP3                                                                
000401*01    -COPY W0008     -PRE WDE4-                                         
000402     05  FILLER                  PIC X(30).                               
000403     SKIP3                                                                
000404*01    -COPY W0008     -PRE WDE4E-                                        
000405     05  FILLER                  PIC X.                                   
000406     EJECT                                                                
000407*01    -COPY W0008     -PRE WDE6-                                         
000408     05  FILLER                  PIC X.                                   
000409     EJECT                                                                
000410*01    -COPY W0008     -PRE ARTC-                                         
000411     05  FILLER                  PIC X.                                   
000412     EJECT                                                                
000413*01    -COPY W0008     -PRE BENA-                                         
000414     05  FILLER                  PIC X.                                   
000415     EJECT                                                                
000416*01    -COPY W0008     -PRE WDE42-                                        
000417     05  FILLER                  PIC X.                                   
000418     SKIP2                                                                
000419*01    -COPY W0008     -PRE WDE41-                                        
000420     05  FILLER                  PIC X.                                   
000421     SKIP2                                                                
000422*01    -COPY W0008     -PRE ORQG-                                         
000423     05  FILLER                  PIC X.                                   
000424     SKIP2                                                                
000425*01    -COPY W0008     -PRE ORQF-                                         
000426     05  FILLER                  PIC X.                                   
000427     SKIP2                                                                
000428*01    -COPY W0008     -PRE ORQA-                                         
000429     05  FILLER                  PIC X.                                   
000430     SKIP2                                                                
000431*01    -COPY W0008     -PRE ORQI-                                         
000432     05  FILLER                  PIC X.                                   
000433     SKIP3                                                                
000434*01    -COPY W0008     -PRE WDE4B-                                        
000435     05  FILLER                  PIC X.                                   
000436     SKIP3                                                                
000437*01    -COPY W0008     -PRE ARTS-                                         
000438     05  FILLER                  PIC X.                                   
000439     EJECT                                                                
000440 PROCEDURE DIVISION USING MSG-PCB USEA-PCB                                
000441                 WDE4A-PCB WDE4-PCB WDE4E-PCB WDE6-PCB                    
000442                 ARTC-PCB BENA-PCB WDE42-PCB WDE41-PCB                    
000443                 ORQG-PCB ORQF-PCB ORQA-PCB ORQI-PCB WDE4B-PCB            
000444                 ARTS-PCB.                                                
000445                                                                          
000446     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB                               
000447                 WDE4A-PCB WDE4-PCB WDE4E-PCB WDE6-PCB                    
000448                 ARTC-PCB BENA-PCB WDE42-PCB WDE41-PCB                    
000449                 ORQG-PCB ORQF-PCB ORQA-PCB ORQI-PCB WDE4B-PCB            
000450                 ARTS-PCB.                                                
000451                                                                          
000452 STYR SECTION.                                                            
000453     PERFORM IMS-GET-MSG                                                  
000454                                                                          
000455     IF SEGMENT-FINNS                                                     
000456       PERFORM A-INIT-SPARA-INPUT                                         
000457       PERFORM B-VILKA-NYCKLAR-ANVANDS                                    
000458       IF KUNDORDER-AER-NYCKEL = JA                                       
000459        PERFORM C-HAMTA-VOLVOORDER                                        
000460       ELSE                                                               
000461        PERFORM D-HAMTA-KUNDORDER                                         
000462       END-IF                                                             
000463                                                                          
000464       IF NYCKLAR-OK = JA                                                 
000465                                                                          
000466        EVALUATE TRUE                                                     
000467         WHEN MFS-IDPFK = ' '                                             
000468          PERFORM E-ENTER-TRANS                                           
000469                                                                          
000470         WHEN MFS-IDPFK = '7'                                             
000471          PERFORM F-PF7-TRANS                                             
000472                                                                          
000473         WHEN MFS-IDPFK = '8'                                             
000474          PERFORM G-PF8-TRANS                                             
000475        END-EVALUATE                                                      
000476       ELSE                                                               
000477           IF NOT WS-GODKAEND-BILD                                        
000478              PERFORM H-RENSA-NYCKLAR                                     
000479           END-IF                                                         
000480       END-IF                                                             
000481                                                                          
000482       COMPUTE MSG-KVLL = LENGTH OF MOD-W4O32301 + 4                      
000483       PERFORM IMS-INSERT-MSG                                             
000484     END-IF                                                               
000485                                                                          
000486     MOVE ZERO TO RETURN-CODE                                             
000487     GOBACK                                                               
000488     .                                                                    
000489     EJECT                                                                
000490 A-INIT-SPARA-INPUT SECTION.                                              
000491                                                                          
000492     IF MSG-DUBBLA-TRANSKODER                                             
000493       MOVE MSG-INDATA-MINUS-2-TRANSKODER  TO MID-W4I32301                
000494       MOVE MSG-IDTRANS-2                  TO MFS-IDTRANS                 
000495       MOVE MSG-KDMFSFOR-2                 TO MFS-KDMFSFOR                
000496       MOVE MSG-KDTRTYP                    TO MFS-KDTRTYP                 
000497     ELSE                                                                 
000498       MOVE MSG-INDATA-MINUS-1-TRANSKOD    TO MID-W4I32301                
000499       MOVE MSG-IDTRANS-1                  TO MFS-IDTRANS                 
000500       MOVE MSG-KDMFSFOR-1                 TO MFS-KDMFSFOR                
000501       MOVE ' '                            TO MFS-KDTRTYP                 
000502     END-IF                                                               
000503                                                                          
000504     MOVE MSG-IDPFK                        TO MFS-IDPFK                   
000505     MOVE MFS-IDTRANS                      TO WS-IDTRANS                  
000506     MOVE ZERO                             TO ACCRAD                      
000507                                                                          
000508     MOVE LOW-VALUE                        TO MSG-AREA                    
000509     MOVE 'W4O323N1'                       TO MFS-IDMOD                   
000510     MOVE '4323'                           TO MOD-IDTRANS                 
000511                                                                          
000512     PERFORM AA-SPARA-NYCKLAR                                             
000513     PERFORM AB-RENSA-MOD                                                 
000514     EJECT                                                                
000515                                                                          
000516     .                                                                    
000517 AA-SPARA-NYCKLAR        SECTION.                                         
000518                                                                          
000519     MOVE ALL '+'           TO MSGI-WMSGINIT                              
000520     MOVE '001'             TO MSGI-KDCALL                                
000521     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
000522     MOVE '4323'            TO MSGI-IDTRANS                               
000523     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
000524     IF EGEN-MID                                                          
000525        MOVE MID-IDPRODNR-IN    TO MSGI-IDPRODNR                          
000526        MOVE MID-IDDISTR-IN     TO MSGI-IDDISTR                           
000527        MOVE MID-IDKUNDNR-IN    TO MSGI-IDKUNDNR                          
000528        IF MID-IDORDNR-IN       NOT = ALL '+'                             
000529           MOVE MID-IDORDNR-IN  TO W-SPAR-IDORDNR5                        
000530           MOVE W-SPAR-IDKUNDRF TO MSGI-IDKUNDRF                          
000531        END-IF                                                            
000532     END-IF                                                               
000533     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
000534                                                                          
000535     IF MSGI-IDLAND-SPR = 'GB'                                            
000536       MOVE +2 TO SPRAK-IX                                                
000537     ELSE                                                                 
000538       MOVE +1 TO SPRAK-IX                                                
000539     END-IF                                                               
000540                                                                          
000541     IF WS-GODKAEND-BILD                                                  
000542        IF MID-IDANSTNR-IN = ALL '+'                                      
000543          MOVE MID-IDANSTNR-UT TO IDANSTNR-WS                             
000544          INSPECT IDANSTNR-WS REPLACING ALL SPACE BY ZERO                 
000545        ELSE                                                              
000546          MOVE MID-IDANSTNR-IN TO IDANSTNR-WS                             
000547        END-IF                                                            
000548        MOVE IDANSTNR-WS  TO MOD-IDANSTNR-UT                              
000549        INSPECT MOD-IDANSTNR-UT REPLACING LEADING ZERO BY SPACE           
000550                                                                          
000551        IF MID-IDDISTR-IN = ALL '+'                                       
000552          MOVE MID-IDDISTR-UT TO IDDISTR-WS                               
000553          INSPECT IDDISTR-WS REPLACING ALL SPACE BY ZERO                  
000554        ELSE                                                              
000555          MOVE MID-IDDISTR-IN TO IDDISTR-WS                               
000556        END-IF                                                            
000557        MOVE IDDISTR-WS  TO MOD-IDDISTR-UT                                
000558        INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE            
000559                                                                          
000560        IF MID-IDKUNDNR-IN = ALL '+'                                      
000561          MOVE MID-IDKUNDNR-UT TO IDKUNDNR-WS                             
000562          INSPECT IDKUNDNR-WS REPLACING ALL SPACE BY ZERO                 
000563        ELSE                                                              
000564          MOVE MID-IDKUNDNR-IN TO IDKUNDNR-WS                             
000565        END-IF                                                            
000566        MOVE IDKUNDNR-WS  TO MOD-IDKUNDNR-UT                              
000567        INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE           
000568                                                                          
000569        IF MID-IDORDNR-IN = ALL '+'                                       
000570          MOVE MID-IDORDNR-UT TO IDKUNDRF-WS                              
000571          INSPECT IDKUNDRF-WS REPLACING ALL SPACE BY ZERO                 
000572        ELSE                                                              
000573          MOVE MID-IDORDNR-IN TO IDKUNDRF-WS                              
000574        END-IF                                                            
000575        MOVE IDKUNDRF-WS TO  MOD-IDORDNR-UT                               
000576        INSPECT MOD-IDORDNR-UT REPLACING LEADING ZERO BY SPACE            
000577                                                                          
000578        IF MID-IDPURAD-IN = ALL '+'                                       
000579          MOVE MID-IDPURAD-UT TO IDPURAD-WS                               
000580          INSPECT IDPURAD-WS REPLACING ALL SPACE BY ZERO                  
000581        ELSE                                                              
000582          MOVE MID-IDPURAD-IN TO IDPURAD-WS                               
000583        END-IF                                                            
000584        MOVE IDPURAD-WS  TO MOD-IDPURAD-UT                                
000585        INSPECT MOD-IDPURAD-UT REPLACING LEADING ZERO BY SPACE            
000586     ELSE                                                                 
000587        MOVE '00000'      TO IDPURAD-WS                                   
000588        MOVE IDPURAD-WS  TO MOD-IDPURAD-UT                                
000589        INSPECT MOD-IDPURAD-UT REPLACING LEADING ZERO BY SPACE            
000590     END-IF                                                               
000591                                                                          
000592     MOVE MSGI-IDPRODNR   TO IDPRODNR-WS                                  
000593                             MOD-IDPRODNR-UT                              
000594     INSPECT MOD-IDPRODNR-UT REPLACING LEADING ZERO BY SPACE              
000595                                                                          
000596     IF WS-GODKAEND-BILD                                                  
000597        IF MID-IDDC-IN = ALL '+'                                          
000598          MOVE MID-IDDC-UT                TO WS-IDDC                      
000599        ELSE                                                              
000600          MOVE MID-IDDC-IN                TO WS-IDDC                      
000601        END-IF                                                            
000602     ELSE                                                                 
000603        MOVE MSGI-IDDC                    TO WS-IDDC                      
000604     END-IF                                                               
000605                                                                          
000606     MOVE WS-IDDC           TO MOD-IDDC-UT                                
000607                               W-Q4-IDDC-MIN                              
000608                               W-Q4-IDDC-MAX                              
000609                               W-Q4A1-IDDC-MIN                            
000610                                                                          
000611     IF WS-IDTRANS NOT = WS-EGEN-TRANS                                    
000612       MOVE ZERO          TO MID-FORSTA-IDPURAD                           
000613                             MID-FORSTA-IDKOLLI                           
000614                             MID-FORSTA-IDPLKLST                          
000615                             MID-FORSTA-KOLLI-IDPRODNR                    
000616                             MID-FORSTA-Q4-IDORDER                        
000617                             MID-FORSTA-Q4-IDARTNR                        
000618                             MID-FORSTA-Q4-IDLOPNR                        
000619     END-IF                                                               
000620     .                                                                    
000621 AB-RENSA-MOD        SECTION.                                             
000622                                                                          
000623     MOVE MFS-RENSA-FAELT       TO MOD-TEMFSFEL                           
000624                                   MOD-IDANSTNR-IN                        
000625                                   MOD-IDDISTR-IN                         
000626                                   MOD-IDKUNDNR-IN                        
000627                                   MOD-IDORDNR-IN                         
000628                                   MOD-IDPURAD-IN                         
000629                                   MOD-IDPRODNR-IN                        
000630                                   MOD-IDDC-IN                            
000631                                   MOD-FORSTA-IDPURAD                     
000632                                   MOD-FORSTA-IDKOLLI                     
000633                                   MOD-TEMFSINF                           
000634     EJECT                                                                
000635                                                                          
000636     .                                                                    
000637 B-VILKA-NYCKLAR-ANVANDS SECTION.                                         
000638                                                                          
000639     IF MID-IDPRODNR-IN NOT = ALL '+' OR NOT WS-GODKAEND-BILD             
000640       MOVE NEJ   TO KUNDORDER-AER-NYCKEL                                 
000641                                                                          
000642       IF IDPRODNR-WS NUMERIC AND IDPRODNR-WS = ZERO                      
000643         MOVE JA     TO KUNDORDER-AER-NYCKEL                              
000644       END-IF                                                             
000645     ELSE                                                                 
000646       IF  MID-IDDISTR-IN  = ALL '+'                                      
000647       AND MID-IDKUNDNR-IN = ALL '+'                                      
000648       AND MID-IDORDNR-IN = ALL '+'                                       
000649                                                                          
000650         IF MID-IDPRODNR-UT NUMERIC AND MID-IDPRODNR-UT > ZERO            
000651           MOVE NEJ    TO KUNDORDER-AER-NYCKEL                            
000652                                                                          
000653         ELSE                                                             
000654           MOVE JA     TO KUNDORDER-AER-NYCKEL                            
000655         END-IF                                                           
000656       ELSE                                                               
000657                                                                          
000658           MOVE JA     TO KUNDORDER-AER-NYCKEL                            
000659           MOVE ZERO            TO IDPRODNR-WS                            
000660           MOVE IDPRODNR-WS     TO MOD-IDPRODNR-UT                        
000661       END-IF                                                             
000662     END-IF                                                               
000663     .                                                                    
000664     EJECT                                                                
000665 C-HAMTA-VOLVOORDER        SECTION.                                       
000666                                                                          
000667     IF IDDISTR-WS    NUMERIC AND                                         
000668        IDKUNDNR-WS   NUMERIC AND                                         
000669        IDKUNDRF-WS   NUMERIC                                             
000670       MOVE IDDISTR-WS                 TO W-E4A1-IDDISTR                  
000671       MOVE IDKUNDNR-WS                TO W-E4A1-IDKUNDNR                 
000672       MOVE IDKUNDRF-WS                TO W-E4A1-IDKUNDRF                 
000673       PERFORM IMS-GU-WDE401-ASEQ                                         
000674*                                                                         
000675       IF WDE401-SEK-FINNS                                                
000676         MOVE KORD-IDORDER            TO W-IDORDER-MIN                    
000677                                         W-IDORDER-MAX                    
000678                                         W-Q4A1-IDORDER-MIN               
000679                                         W-Q4A1-IDORDER-MAX               
000680                                         W-Q4-IDORDER-MIN                 
000681                                         W-Q4-IDORDER-MAX                 
000682         MOVE KORD-IDDISTR            TO W-E401-IDDISTR                   
000683                                         W-E401-IDDISTR-MAX               
000684         MOVE KORD-IDKUNDNR           TO W-E401-IDKUNDNR                  
000685                                         W-E401-IDKUNDNR-MAX              
000686         MOVE KORD-IDORDNR5           TO W-E401-IDORDNR                   
000687         MOVE KORD-IDKUNDRF           TO W-E401-IDKUNDRF-MAX              
000688                                                                          
000689         IF MID-FORSTA-KOLLI-IDPRODNR > ZERO    AND                       
000690            MID-FORSTA-KOLLI-IDPRODNR < 9999999 AND                       
000691            MID-IDPURAD-IN = ALL '+'                                      
000692           MOVE MID-FORSTA-KOLLI-IDPRODNR TO W-E401-IDPRODNR              
000693                                             W-E401-IDPRODNR-MAX          
000694           MOVE MID-FORSTA-IDPLKLST       TO W-E401-IDPLKLST              
000695           MOVE 999                       TO W-E401-IDPLKLST-MAX          
000696         ELSE                                                             
000697           MOVE KORD-IDPRODNR             TO W-E401-IDPRODNR              
000698                                             W-E401-IDPRODNR-MAX          
000699           MOVE KORD-IDPLKLST             TO W-E401-IDPLKLST              
000700           MOVE 999                       TO W-E401-IDPLKLST-MAX          
000701         END-IF                                                           
000702*                                                                         
000703         MOVE KORD-IDDC               TO W-IDDC                           
000704                                                                          
000705           PERFORM CA-LAS-ORAD                                            
000706                                                                          
000707           IF SEGMENT-FINNS                                               
000708               PERFORM UNTIL SEGMENT-SAKNAS OR                            
000709                             (ORAD-FLDIRLEV = NEJ AND                     
000710                              W-IDDC = WS-IDDC)                           
000711                  PERFORM CB-LAS-NASTA-PRODNR                             
000712                  IF SEGMENT-FINNS                                        
000713                    PERFORM CA-LAS-ORAD                                   
000714                  END-IF                                                  
000715               END-PERFORM                                                
000716           END-IF                                                         
000717                                                                          
000718           IF SEGMENT-FINNS                                               
000719             MOVE JA                  TO NYCKLAR-OK                       
000720             INSPECT MOD-IDPRODNR-UT REPLACING                            
000721                                    LEADING ZERO BY SPACE                 
000722           ELSE                                                           
000723             MOVE NEJ                 TO NYCKLAR-OK                       
000724             MOVE FEL-2 (SPRAK-IX)    TO MOD-TEMFSFEL                     
000725           END-IF                                                         
000726      ELSE                                                                
000727       MOVE NEJ                     TO NYCKLAR-OK                         
000728       MOVE FEL-2 (SPRAK-IX)        TO MOD-TEMFSFEL                       
000729      END-IF                                                              
000730     ELSE                                                                 
000731       MOVE NEJ                     TO NYCKLAR-OK                         
000732       MOVE FEL-1 (SPRAK-IX)        TO MOD-TEMFSFEL                       
000733     END-IF                                                               
000734     .                                                                    
000735     EJECT                                                                
000736 CA-LAS-ORAD SECTION.                                                     
000737                                                                          
000738     MOVE KORD-IDPRODNR           TO W-E401-IDPRODNR                      
000739                                     IDPRODNR-WS                          
000740     MOVE KORD-IDPLKLST           TO W-E401-IDPLKLST                      
000741     PERFORM IMS-GU-WDE401-WDE4                                           
000742     IF SEGMENT-FINNS                                                     
000743       PERFORM IMS-GNP-WDE411                                             
000744     END-IF                                                               
000745     .                                                                    
000746     SKIP2                                                                
000747 CB-LAS-NASTA-PRODNR SECTION.                                             
000748                                                                          
000749*    LÄS NÄSTA PRODNR MED FORMATETS CLAGER                                
000750*    LOOK IF THERE ARE MORE PRODUCTIONNR FOR THIS ORDER                   
000751*                                                                         
000752     PERFORM IMS-GN-WDE401-ASEQ                                           
000753*                                                                         
000754     IF SEGMENT-FINNS                                                     
000755        MOVE KORD-IDDC            TO W-IDDC                               
000756     END-IF                                                               
000757*                                                                         
000758     PERFORM UNTIL SEGMENT-SAKNAS                   OR                    
000759                   W-IDDC            = WS-IDDC                            
000760*                                                                         
000761          PERFORM IMS-GN-WDE401-ASEQ                                      
000762          IF SEGMENT-FINNS                                                
000763             MOVE KORD-IDDC               TO W-IDDC                       
000764          END-IF                                                          
000765*                                                                         
000766     END-PERFORM                                                          
000767     .                                                                    
000768     SKIP2                                                                
000769 D-HAMTA-KUNDORDER  SECTION.                                              
000770                                                                          
000771     IF IDPRODNR-WS NUMERIC                                               
000772       PERFORM IMS-GU-WDE601                                              
000773                                                                          
000774       IF SEGMENT-FINNS                                                   
000775         MOVE VORD-IDDC            TO W-IDDC                              
000776         IF VORD-IDDC     = WS-IDDC                                       
000777          MOVE VORD-IDPRODNR        TO W-IDPRODNR-WDE4E-MAX               
000778          MOVE VORD-IDPRODNR        TO W-IDPRODNR-WDE4E-MIN               
000779                                                                          
000780           PERFORM IMS-GU-WDE4E1                                          
000781                                                                          
000782           MOVE SEQE-IDDISTR TO TEMP-IDDISTR                              
000783                                      W-E4A1-IDDISTR                      
000784                                      W-E401-IDDISTR                      
000785                                      W-E401-IDDISTR-MAX                  
000786           MOVE TEMP-IDDISTR       TO MOD-IDDISTR-UT                      
000787           INSPECT MOD-IDDISTR-UT  REPLACING LEADING ZERO BY SPACE        
000788           MOVE TEMP-IDDISTR       TO IDDISTR-WS                          
000789                                                                          
000790           MOVE SEQE-IDKUNDNR TO TEMP-IDKUNDNR                            
000791                                      W-E4A1-IDKUNDNR                     
000792                                      W-E401-IDKUNDNR                     
000793                                      W-E401-IDKUNDNR-MAX                 
000794           MOVE TEMP-IDKUNDNR      TO MOD-IDKUNDNR-UT                     
000795           INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE        
000796                                                                          
000797           MOVE SEQE-IDKUNDRF TO MOD-IDORDNR-UT                           
000798                                      W-E4A1-IDKUNDRF                     
000799                                      W-E401-IDKUNDRF                     
000800                                      W-E401-IDKUNDRF-MAX                 
000801           INSPECT MOD-IDORDNR-UT REPLACING LEADING ZERO BY SPACE         
000802           IF MID-FORSTA-KOLLI-IDPRODNR > ZERO     AND                    
000803              MID-FORSTA-KOLLI-IDPRODNR < 9999999  AND                    
000804              MID-IDPRODNR-IN = ALL '+'            AND                    
000805              MID-IDPURAD-IN = ALL '+'                                    
000806             MOVE MID-FORSTA-KOLLI-IDPRODNR TO W-E401-IDPRODNR            
000807             MOVE MID-FORSTA-IDPLKLST       TO W-E401-IDPLKLST            
000808           ELSE                                                           
000809             MOVE SEQE-IDPRODNR TO W-E401-IDPRODNR                        
000810             MOVE SEQE-IDPLKLST TO W-E401-IDPLKLST                        
000811           END-IF                                                         
000812                                                                          
000813           PERFORM IMS-GU-WDE401                                          
000814           IF SEGMENT-FINNS                                               
000815             MOVE KORD-IDORDER     TO W-IDORDER-MIN                       
000816                                      W-IDORDER-MAX                       
000817                                      W-Q4A1-IDORDER-MIN                  
000818                                      W-Q4A1-IDORDER-MAX                  
000819                                      W-Q4-IDORDER-MIN                    
000820                                      W-Q4-IDORDER-MAX                    
000821           ELSE                                                           
000822             MOVE NEJ TO NYCKLAR-OK                                       
000823             MOVE FEL-2 (SPRAK-IX) TO MOD-TEMFSFEL                        
000824           END-IF                                                         
000825                                                                          
000826           MOVE JA  TO NYCKLAR-OK                                         
000827         ELSE                                                             
000828           MOVE NEJ TO NYCKLAR-OK                                         
000829           MOVE FEL-2 (SPRAK-IX)   TO MOD-TEMFSFEL                        
000830         END-IF                                                           
000831                                                                          
000832       ELSE                                                               
000833         MOVE NEJ TO NYCKLAR-OK                                           
000834         MOVE FEL-1 (SPRAK-IX)   TO MOD-TEMFSFEL                          
000835       END-IF                                                             
000836                                                                          
000837     ELSE                                                                 
000838       MOVE NEJ TO NYCKLAR-OK                                             
000839       MOVE FEL-1 (SPRAK-IX)    TO MOD-TEMFSFEL                           
000840     END-IF                                                               
000841     .                                                                    
000842     EJECT                                                                
000843 E-ENTER-TRANS SECTION.                                                   
000844                                                                          
000845     IF MID-IDANSTNR-IN        = ALL '+'     AND                          
000846        MID-IDDISTR-IN         = ALL '+'     AND                          
000847        MID-IDKUNDNR-IN        = ALL '+'     AND                          
000848        MID-IDORDNR-IN         = ALL '+'     AND                          
000849        MID-IDPURAD-IN         = ALL '+'     AND                          
000850        MID-IDPRODNR-IN        = ALL '+'     AND                          
000851        MFS-IDTRANS            = '4323'      AND                          
000852        MID-FORSTA-IDPURAD NOT = '9999'      AND                          
000853        MID-FORSTA-IDKOLLI NOT = '99999'                                  
000854       IF MID-FORSTA-Q4-IDARTNR > ZERO                                    
000855         MOVE '8'                   TO MFS-IDPFK                          
000856         MOVE MID-FORSTA-Q4-IDORDER TO W-Q4-IDORDER-MIN                   
000857         MOVE MID-FORSTA-Q4-IDARTNR TO W-Q4-IDARTNR-MIN                   
000858         MOVE MID-FORSTA-Q4-IDLOPNR TO W-Q4-IDLOPNR-MIN                   
000859       END-IF                                                             
000860       PERFORM S30-INIT-VID-PF8-TRYCKNING                                 
000861     ELSE                                                                 
000862       PERFORM S01-INIT-VID-E-TRYCKNING                                   
000863     END-IF                                                               
000864     .                                                                    
000865     EJECT                                                                
000866 F-PF7-TRANS SECTION.                                                     
000867                                                                          
000868     IF MID-IDANSTNR-IN = ALL '+'     AND                                 
000869        MID-IDDISTR-IN  = ALL '+'     AND                                 
000870        MID-IDKUNDNR-IN = ALL '+'     AND                                 
000871        MID-IDORDNR-IN  = ALL '+'     AND                                 
000872        MID-IDPURAD-IN  = ALL '+'     AND                                 
000873        MID-IDPRODNR-IN = ALL '+'                                         
000874       PERFORM S41-LAS-RAD-SEGM-MED-GN                                    
000875                                                                          
000876       IF SEGMENT-FINNS                                                   
000877        MOVE NEJ             TO RADER-SLUT                                
000878       ELSE                                                               
000879        MOVE JA              TO RADER-SLUT                                
000880       END-IF                                                             
000881       MOVE 1                TO ACCRAD                                    
000882       PERFORM S20-VISA-BILD                                              
000883       PERFORM S50-EV-VISA-RADER-Q4                                       
000884                                                                          
000885     ELSE                                                                 
000886       PERFORM S01-INIT-VID-E-TRYCKNING                                   
000887     END-IF                                                               
000888     .                                                                    
000889     EJECT                                                                
000890 G-PF8-TRANS SECTION.                                                     
000891                                                                          
000892     IF MID-IDANSTNR-IN = ALL '+'            AND                          
000893        MID-IDDISTR-IN  = ALL '+'            AND                          
000894        MID-IDKUNDNR-IN = ALL '+'            AND                          
000895        MID-IDORDNR-IN  = ALL '+'            AND                          
000896        MID-IDPURAD-IN  = ALL '+'            AND                          
000897        MID-IDPRODNR-IN = ALL '+'            AND                          
000898        MID-FORSTA-IDPURAD NOT = '9999'      AND                          
000899        MID-FORSTA-IDKOLLI NOT = '99999'                                  
000900       IF MID-FORSTA-Q4-IDARTNR = ALL SPACE                               
000901         PERFORM S30-INIT-VID-PF8-TRYCKNING                               
000902       ELSE                                                               
000903         MOVE 1                TO ACCRAD                                  
000904         PERFORM S50-EV-VISA-RADER-Q4                                     
000905       END-IF                                                             
000906     ELSE                                                                 
000907       IF MID-FORSTA-Q4-IDARTNR = ALL SPACE                               
000908         PERFORM S01-INIT-VID-E-TRYCKNING                                 
000909       ELSE                                                               
000910         MOVE 1                TO ACCRAD                                  
000911         PERFORM S50-EV-VISA-RADER-Q4                                     
000912       END-IF                                                             
000913     END-IF                                                               
000914     .                                                                    
000915     SKIP2                                                                
000916 H-RENSA-NYCKLAR SECTION.                                                 
000917     MOVE MFS-RENSA-FAELT              TO  MOD-IDDISTR-UT                 
000918                                           MOD-IDANSTNR-UT                
000919                                           MOD-IDKUNDNR-UT                
000920                                           MOD-IDORDNR-UT                 
000921                                           MOD-IDPURAD-UT                 
000922*                                          MOD-IDPRODNR-UT                
000923*                                          MOD-IDDC-UT                    
000924     .                                                                    
000925     SKIP2                                                                
000926 S01-INIT-VID-E-TRYCKNING      SECTION.                                   
000927                                                                          
000928     IF IDPURAD-WS NOT NUMERIC                                            
000929       MOVE FEL-1 (SPRAK-IX)       TO MOD-TEMFSFEL                        
000930     ELSE                                                                 
000931                                                                          
000932      IF IDPURAD-WS = ALL '0'                                             
000933         PERFORM S41-LAS-RAD-SEGM-MED-GN                                  
000934                                                                          
000935         MOVE 1                      TO ACCRAD                            
000936         IF SEGMENT-FINNS                                                 
000937            MOVE NEJ                  TO RADER-SLUT                       
000938            MOVE ARB-ORAD-IDPURAD     TO TEMP-FORSTA-IDPURAD              
000939            MOVE TEMP-FORSTA-IDPURAD  TO MID-FORSTA-IDPURAD               
000940            PERFORM S20-VISA-BILD                                         
000941            PERFORM S50-EV-VISA-RADER-Q4                                  
000942         ELSE                                                             
000943            PERFORM S50-EV-VISA-RADER-Q4                                  
000944                                                                          
000945*           IF SEGMENT-SAKNAS                                             
000946*           MOVE FEL-1(SPRAK-IX)      TO MOD-TEMFSFEL                     
000947*           MOVE JA                   TO RADER-SLUT                       
000948*           END-IF                                                        
000949         END-IF                                                           
000950      ELSE                                                                
000951         MOVE IDPRODNR-WS           TO W-WDE4B-IDPRODNR                   
000952         MOVE IDPURAD-WS            TO W-IDPURAD WS-IDPURAD               
000953         PERFORM S40-LAS-RAD-SEGM-MED-GU                                  
000954                                                                          
000955         IF SEGMENT-FINNS                                                 
000956            MOVE 1                    TO ACCRAD                           
000957            MOVE NEJ                  TO RADER-SLUT                       
000958            MOVE ARB-ORAD-IDPURAD     TO TEMP-FORSTA-IDPURAD              
000959            MOVE TEMP-FORSTA-IDPURAD  TO MID-FORSTA-IDPURAD               
000960            PERFORM S20-VISA-BILD                                         
000961            PERFORM S50-EV-VISA-RADER-Q4                                  
000962         ELSE                                                             
000963            MOVE FEL-1(SPRAK-IX)    TO MOD-TEMFSFEL                       
000964            MOVE JA                 TO RADER-SLUT                         
000965         END-IF                                                           
000966      END-IF                                                              
000967     END-IF                                                               
000968     EJECT                                                                
000969     .                                                                    
000970 S20-VISA-BILD SECTION.                                                   
000971                                                                          
000972     IF SEGMENT-FINNS                                                     
000973       PERFORM UNTIL ACCRAD > 14 OR RADER-SLUT = JA                       
000974         PERFORM S20A-LAS-LAGERINFO-TILL-MOD                              
000975         PERFORM S51-LAS-BENA-TILL-MOD                                    
000976         PERFORM S20C-FLYTTA-RAD-SEGM-TILL-MOD                            
000977                                                                          
000978         PERFORM IMS-GNP-WDE421                                           
000979         MOVE KKOLLI-AREA             TO ARB-KKOLLI-AREA                  
000980                                                                          
000981         IF SEGMENT-FINNS                                                 
000982           IF ARB-ORAD-KVLEVART = ARB-KKOLLI-KVLEVART                     
000983             PERFORM S20D-INGA-FLER-KOLLIN-FINNS                          
000984           ELSE                                                           
000985             PERFORM S20E-FLER-KOLLIN-FINNS                               
000986           END-IF                                                         
000987         ELSE                                                             
000988           PERFORM S20F-INGET-KOLLI-FANNS                                 
000989         END-IF                                                           
000990                                                                          
000991         PERFORM S20G-BRYT-ORSAK                                          
000992       END-PERFORM                                                        
000993                                                                          
000994       IF RADER-SLUT = JA                                                 
000995         PERFORM S20H-BRYT-ORSAK-RADER-SLUT                               
000996       ELSE                                                               
000997         PERFORM S20I-BRYT-ORSAK-SIDA-SLUT                                
000998       END-IF                                                             
000999                                                                          
001000       IF RADER-SLUT = JA    AND                                          
001001          KOLLIN-SLUT = JA                                                
001002         MOVE MID-FORSTA-IDPURAD      TO MOD-FORSTA-IDPURAD               
001003       END-IF                                                             
001004     ELSE                                                                 
001005       MOVE ZERO                      TO ACCRAD                           
001006     END-IF                                                               
001007                                                                          
001008     .                                                                    
001009     EJECT                                                                
001010 S20A-LAS-LAGERINFO-TILL-MOD   SECTION.                                   
001011                                                                          
001012     MOVE ARB-ORAD-IDARTNR        TO W-IDARTNR                            
001013     MOVE WS-IDDC                 TO W-IDDC                               
001014     IF CDC                                                               
001015       PERFORM IMS-GU-ARTC11                                              
001016                                                                          
001017       IF SEGMENT-FINNS                                                   
001018                                                                          
001019         MOVE 1              TO CD-IX                                     
001020         PERFORM UNTIL CD-IX > 4                                          
001021         OR ARB-ORAD-ADLAGOMR = CLAG-ADLAGOMR-CD (CD-IX)                  
001022           ADD 1             TO CD-IX                                     
001023         END-PERFORM                                                      
001024                                                                          
001025         IF CD-IX > 4                                                     
001026                                                                          
001027           MOVE CLAG-ADLAGOMR       TO MOD-ADLAGOMR (ACCRAD)              
001028           MOVE CLAG-ADGANG         TO MOD-ADGANG (ACCRAD)                
001029           MOVE CLAG-ADPLATS        TO MOD-ADPLATS (ACCRAD)               
001030         ELSE                                                             
001031           MOVE CLAG-ADLAGOMR-CD (CD-IX)                                  
001032                                    TO MOD-ADLAGOMR (ACCRAD)              
001033           MOVE CLAG-ADGANG-CD (CD-IX) TO MOD-ADGANG (ACCRAD)             
001034           MOVE CLAG-ADPLATS-CD (CD-IX)                                   
001035                                    TO MOD-ADPLATS (ACCRAD)               
001036         END-IF                                                           
001037       END-IF                                                             
001038     ELSE                                                                 
001039       PERFORM IMS-GU-ARTS11                                              
001040                                                                          
001041       IF SEGMENT-FINNS                                                   
001042           MOVE SLAG-ADLAGOMR       TO MOD-ADLAGOMR (ACCRAD)              
001043           MOVE SLAG-ADGANG         TO MOD-ADGANG (ACCRAD)                
001044           MOVE SLAG-ADPLATS        TO MOD-ADPLATS (ACCRAD)               
001045       END-IF                                                             
001046     END-IF                                                               
001047     SKIP2                                                                
001048     .                                                                    
001049 S20C-FLYTTA-RAD-SEGM-TILL-MOD    SECTION.                                
001050                                                                          
001051     MOVE ARB-ORAD-IDKUNDRF-RO      TO MOD-IDRONR (ACCRAD)                
001052     INSPECT MOD-IDRONR (ACCRAD)                                          
001053             REPLACING LEADING ZERO BY SPACE                              
001054     MOVE ARB-ORAD-IDPURAD     TO MOD-IDPURAD (ACCRAD)                    
001055     MOVE ARB-ORAD-IDARTNR     TO MOD-IDARTNR (ACCRAD)                    
001056     MOVE '-'                  TO MOD-STRACK (ACCRAD)                     
001057     MOVE ARB-ORAD-REKSIFFR    TO MOD-REKSIFFR (ACCRAD)                   
001058                                                                          
001059     MOVE IDDISTR-WS           TO DIST08-IDDISTR                          
001060     IF DIST08-URSP-RAPP                                                  
001061     OR                                                                   
001062     (CDC AND DIST08-URSP-RAPP-CDC)                                       
001063     OR                                                                   
001064     (CDC AND DIST08-URSP-SPX)                                            
001065     OR                                                                   
001066     (NDC AND TRANSFER-KUNDNR                                             
001067     AND DIST08-URSP-TRANSFER-NDC)                                        
001068     OR                                                                   
001069     (NDC-CA AND DIST08-URSP-RAPP-CDC)                                    
001070     OR                                                                   
001071     (NDC AND RETUR-KUNDNR                                                
001072     AND DIST08-URSP-RETUR-NDC)                                           
001073       MOVE ARB-ORAD-KDARTURS  TO MOD-KDARTURS (ACCRAD)                   
001074     ELSE                                                                 
001075       MOVE MFS-RENSA-FAELT    TO MOD-KDARTURS (ACCRAD)                   
001076     END-IF                                                               
001077                                                                          
001078     MOVE ARB-ORAD-KVAVBART    TO MOD-KVAVBART (ACCRAD)                   
001079     MOVE ARB-ORAD-KVLEVART    TO MOD-KVLEVART (ACCRAD)                   
001080     EJECT                                                                
001081                                                                          
001082     .                                                                    
001083 S20D-INGA-FLER-KOLLIN-FINNS    SECTION.                                  
001084                                                                          
001085     MOVE KKOLLI-IDKOLLI     TO TEMP-IDKOLLI                              
001086     MOVE TEMP-IDKOLLI       TO MOD-IDKOLLI (ACCRAD)                      
001087     ADD +1                  TO ACCRAD                                    
001088     MOVE JA                 TO KOLLIN-SLUT                               
001089     SKIP3                                                                
001090                                                                          
001091     .                                                                    
001092 S20E-FLER-KOLLIN-FINNS    SECTION.                                       
001093                                                                          
001094     MOVE '  X  '            TO MOD-IDKOLLI (ACCRAD)                      
001095     ADD +1                  TO ACCRAD                                    
001096     MOVE NEJ                TO KOLLIN-SLUT                               
001097                                                                          
001098     PERFORM UNTIL KOLLIN-SLUT = JA OR ACCRAD > 14                        
001099       MOVE KKOLLI-KVLEVART  TO MOD-KVLEVART (ACCRAD)                     
001100       MOVE KKOLLI-IDKOLLI   TO TEMP-IDKOLLI                              
001101       MOVE TEMP-IDKOLLI     TO MOD-IDKOLLI (ACCRAD)                      
001102       ADD +1                TO ACCRAD                                    
001103       PERFORM IMS-GNP-WDE421                                             
001104       MOVE KKOLLI-AREA             TO ARB-KKOLLI-AREA                    
001105                                                                          
001106       IF SEGMENT-FINNS                                                   
001107         MOVE NEJ            TO KOLLIN-SLUT                               
001108       ELSE                                                               
001109         MOVE JA             TO KOLLIN-SLUT                               
001110       END-IF                                                             
001111     END-PERFORM                                                          
001112                                                                          
001113     .                                                                    
001114 S20F-INGET-KOLLI-FANNS         SECTION.                                  
001115                                                                          
001116     MOVE SPACE              TO MOD-IDKOLLI (ACCRAD)                      
001117     ADD +1                  TO ACCRAD                                    
001118     MOVE JA                 TO KOLLIN-SLUT                               
001119     SKIP3                                                                
001120                                                                          
001121     .                                                                    
001122 S20G-BRYT-ORSAK                 SECTION.                                 
001123     MOVE W-E401-IDPLKLST         TO MOD-FORSTA-KOLLI-IDPLKLST            
001124     IF KOLLIN-SLUT = JA                                                  
001125       PERFORM S20GA-BRYT-ORSAK-KOLLIN-SLUT                               
001126     ELSE                                                                 
001127       MOVE 9999999                  TO W-E401-IDPRODNR-MAX               
001128       MOVE 999                      TO W-E401-IDPLKLST-MAX               
001129       PERFORM IMS-GU-WDE401-WDE4                                         
001130       IF SEGMENT-FINNS                                                   
001131         PERFORM IMS-GNP-WDE411                                           
001132         IF SEGMENT-FINNS AND ACCRAD < 15                                 
001133           MOVE ORAD-AREA              TO ARB-ORAD-AREA                   
001134           MOVE ARB-ORAD-IDPURAD       TO IDPURAD-WS                      
001135           MOVE ORAD-IDPRODNR          TO IDPRODNR-WS                     
001136                                        W-IDPRODNR-MAX                    
001137                                        W-IDPRODNR-MIN                    
001138                                        W-E401-IDPRODNR                   
001139                                        W-E401-IDPRODNR-MAX               
001140           MOVE 000                    TO W-E401-IDPLKLST                 
001141           MOVE 999                    TO W-E401-IDPLKLST-MAX             
001142           PERFORM IMS-GU-WDE4A1                                          
001143         ELSE                                                             
001144           PERFORM S20GB-BRYT-ORSAK-SIDA-SLUT                             
001145         END-IF                                                           
001146       END-IF                                                             
001147     END-IF                                                               
001148     EJECT                                                                
001149     .                                                                    
001150 S20GA-BRYT-ORSAK-KOLLIN-SLUT    SECTION.                                 
001151                                                                          
001152     MOVE 0                      TO MOD-FORSTA-IDKOLLI                    
001153                                                                          
001154     PERFORM S41-LAS-RAD-SEGM-MED-GN                                      
001155                                                                          
001156     IF SEGMENT-FINNS                                                     
001157       MOVE NEJ            TO RADER-SLUT                                  
001158     ELSE                                                                 
001159       MOVE JA             TO RADER-SLUT                                  
001160     END-IF                                                               
001161     SKIP2                                                                
001162     .                                                                    
001163 S20GB-BRYT-ORSAK-SIDA-SLUT      SECTION.                                 
001164                                                                          
001165     MOVE NEJ                     TO RADER-SLUT                           
001166     MOVE ARB-KKOLLI-IDKOLLI      TO MOD-FORSTA-IDKOLLI                   
001167     MOVE ARB-KKOLLI-IDPRODNR     TO MOD-FORSTA-KOLLI-IDPRODNR            
001168     EJECT                                                                
001169     .                                                                    
001170 S20H-BRYT-ORSAK-RADER-SLUT    SECTION.                                   
001171     MOVE 0                         TO MOD-FORSTA-IDPURAD                 
001172     MOVE MED-11 (SPRAK-IX)          TO MOD-TEMFSINF                      
001173     MOVE W-IDORDER-MIN             TO W-Q4A1-IDORDER-MIN                 
001174                                       W-Q4A1-IDORDER-MAX                 
001175                                       W-Q4-IDORDER-MIN                   
001176                                       W-Q4-IDORDER-MAX                   
001177     .                                                                    
001178 S20I-BRYT-ORSAK-SIDA-SLUT      SECTION.                                  
001179     MOVE ARB-ORAD-IDPURAD          TO MOD-FORSTA-IDPURAD                 
001180     MOVE ARB-ORAD-IDPRODNR         TO MOD-FORSTA-KOLLI-IDPRODNR          
001181     MOVE W-E401-IDPLKLST           TO MOD-FORSTA-KOLLI-IDPLKLST          
001182     MOVE MED-21 (SPRAK-IX)          TO MOD-TEMFSINF                      
001183     EJECT                                                                
001184                                                                          
001185     .                                                                    
001186 S30-INIT-VID-PF8-TRYCKNING    SECTION.                                   
001187                                                                          
001188     MOVE MID-FORSTA-IDPURAD        TO TEMP-FORSTA-IDPURAD                
001189     MOVE MID-FORSTA-IDKOLLI        TO TEMP-FORSTA-IDKOLLI                
001190                                                                          
001191     MOVE TEMP-FORSTA-IDPURAD       TO W-IDPURAD                          
001192                                       WS-IDPURAD                         
001193     MOVE IDPRODNR-WS               TO W-WDE4B-IDPRODNR                   
001194     PERFORM S40-LAS-RAD-SEGM-MED-GU                                      
001195                                                                          
001196*LN TEST                                                                  
001197     IF SEGMENT-FINNS                                                     
001198*LN                                                                       
001199       EVALUATE TRUE                                                      
001200         WHEN TEMP-FORSTA-IDPURAD NOT = 0   AND                           
001201              TEMP-FORSTA-IDKOLLI NOT = 0                                 
001202                                                                          
001203           MOVE MID-FORSTA-IDKOLLI        TO W-K-IDKOLLI                  
001204           IF MID-FORSTA-KOLLI-IDPRODNR > ZERO                            
001205             MOVE MID-FORSTA-KOLLI-IDPRODNR TO W-K-IDPRODNR               
001206           END-IF                                                         
001207           PERFORM IMS-GNP-WDE421-KOLLI                                   
001208           IF SEGMENT-FINNS                                               
001209                                                                          
001210             MOVE NEJ                     TO KOLLIN-SLUT                  
001211             MOVE 1                       TO ACCRAD                       
001212                                                                          
001213             PERFORM UNTIL KOLLIN-SLUT = JA OR ACCRAD > 14                
001214                 MOVE KKOLLI-IDKOLLI        TO TEMP-IDKOLLI               
001215                 MOVE TEMP-IDKOLLI       TO MOD-IDKOLLI (ACCRAD)          
001216                 MOVE KKOLLI-KVLEVART    TO MOD-KVLEVART (ACCRAD)         
001217                 ADD +1                     TO ACCRAD                     
001218                                                                          
001219                 PERFORM IMS-GNP-WDE421                                   
001220                                                                          
001221                 IF SEGMENT-FINNS                                         
001222                   MOVE NEJ                 TO KOLLIN-SLUT                
001223                 ELSE                                                     
001224                   MOVE JA                  TO KOLLIN-SLUT                
001225                 END-IF                                                   
001226             END-PERFORM                                                  
001227                                                                          
001228             IF KOLLIN-SLUT = JA                                          
001229               MOVE 0               TO MOD-FORSTA-IDKOLLI                 
001230               PERFORM S41-LAS-RAD-SEGM-MED-GN                            
001231                                                                          
001232               IF SEGMENT-FINNS                                           
001233                 MOVE NEJ           TO RADER-SLUT                         
001234               ELSE                                                       
001235                 MOVE JA            TO RADER-SLUT                         
001236               END-IF                                                     
001237             ELSE                                                         
001238               MOVE KKOLLI-IDKOLLI  TO MOD-FORSTA-IDKOLLI                 
001239               MOVE KKOLLI-IDPRODNR TO MOD-FORSTA-KOLLI-IDPRODNR          
001240               MOVE W-E401-IDPLKLST TO MOD-FORSTA-KOLLI-IDPLKLST          
001241               MOVE NEJ             TO RADER-SLUT                         
001242             END-IF                                                       
001243           ELSE                                                           
001244             MOVE MED-12 (SPRAK-IX) TO MOD-TEMFSINF                       
001245           END-IF                                                         
001246                                                                          
001247         WHEN TEMP-FORSTA-IDPURAD NOT = 0 AND                             
001248              TEMP-FORSTA-IDKOLLI = 0                                     
001249           MOVE NEJ                  TO RADER-SLUT                        
001250           MOVE 1                    TO ACCRAD                            
001251           IF MID-FORSTA-KOLLI-IDPRODNR > ZERO                            
001252             MOVE MID-FORSTA-KOLLI-IDPRODNR TO W-K-IDPRODNR               
001253           END-IF                                                         
001254       END-EVALUATE                                                       
001255*LN                                                                       
001256     ELSE                                                                 
001257       MOVE  MED-12 (SPRAK-IX) TO MOD-TEMFSINF                            
001258     END-IF                                                               
001259*LN END-TEST                                                              
001260                                                                          
001261     IF ACCRAD > 0                                                        
001262       CONTINUE                                                           
001263     ELSE                                                                 
001264       MOVE +1          TO ACCRAD                                         
001265     END-IF                                                               
001266                                                                          
001267     IF MFS-IDPFK = '8' AND MID-FORSTA-Q4-IDARTNR > ZERO                  
001268       PERFORM S50-EV-VISA-RADER-Q4                                       
001269     ELSE                                                                 
001270       PERFORM S20-VISA-BILD                                              
001271       PERFORM S50-EV-VISA-RADER-Q4                                       
001272     END-IF                                                               
001273     EJECT                                                                
001274     .                                                                    
001275 S40-LAS-RAD-SEGM-MED-GU       SECTION.                                   
001276                                                                          
001277     PERFORM IMS-GU-WDE411-BSEQ                                           
001278     IF SEGMENT-FINNS                                                     
001279       MOVE ORAD-AREA          TO ARB-ORAD-AREA                           
001280       MOVE ORAD-IDPURAD       TO WS-IDPURAD                              
001281     END-IF                                                               
001282     .                                                                    
001283     SKIP2                                                                
001284 S41-LAS-RAD-SEGM-MED-GN   SECTION.                                       
001285                                                                          
001286     MOVE LOW-VALUE              TO W-WDE4BSEQ-MIN-X                      
001287     MOVE HIGH-VALUE             TO W-WDE4BSEQ-MAX-X                      
001288     MOVE IDPRODNR-WS            TO W-IDPRODNR-MIN                        
001289     MOVE IDPRODNR-WS            TO W-IDPRODNR-MAX                        
001290     PERFORM IMS-GN-WDE411-BSEQ                                           
001291     IF SEGMENT-FINNS                                                     
001292       MOVE ORAD-AREA            TO ARB-ORAD-AREA                         
001293       MOVE ARB-ORAD-IDPURAD     TO IDPURAD-WS                            
001294                                    WS-IDPURAD                            
001295     END-IF                                                               
001296     .                                                                    
001297     EJECT                                                                
001298 S50-EV-VISA-RADER-Q4 SECTION.                                            
001299                                                                          
001300     IF ACCRAD < 15                                                       
001301       PERFORM S50D-EV-MID-TILL-NYCKL                                     
001302       IF KUNDORDER-AER-NYCKEL = JA                                       
001303                                                                          
001304         IF MFS-IDPFK = '8' AND MID-FORSTA-Q4-IDORDER > ZERO              
001305           PERFORM IMS-GU-ORQG01-Q4A1                                     
001306           IF SEGMENT-FINNS                                               
001307             MOVE SEQA-ADLAGOMR      TO W-Q4-ADLAGOMR-MIN                 
001308             MOVE SEQA-ADGANG        TO W-Q4-ADGANG-MIN                   
001309             MOVE SEQA-ADPLATS       TO W-Q4-ADPLATS-MIN                  
001310             MOVE MID-FORSTA-Q4-IDARTNR TO W-Q4-IDARTNR-MIN               
001311             MOVE MID-FORSTA-Q4-IDLOPNR TO W-Q4-IDLOPNR-MIN               
001312             PERFORM IMS-GU-ORQF01-KVAL                                   
001313           END-IF                                                         
001314         ELSE                                                             
001315           PERFORM IMS-GU-ORQF01                                          
001316         END-IF                                                           
001317         IF SEGMENT-FINNS                                                 
001318           PERFORM S55-PF8-NYCKL-TO-SPAR                                  
001319                                                                          
001320           IF Q4-ORAD-IDORDER = W-IDORDER-MIN                             
001321             PERFORM UNTIL ACCRAD > 14 OR SEGMENT-SAKNAS OR               
001322               END-OF-DATA                                                
001323               PERFORM S50A-FLYTTA-Q4INFO-TILL-MOD                        
001324               MOVE Q4-ORAD-IDARTNR   TO W-IDARTNR                        
001325               PERFORM S54-LAS-LAGERINFO-TILL-MOD                         
001326               PERFORM S51-LAS-BENA-TILL-MOD                              
001327               PERFORM IMS-GN-ORQF01                                      
001328               PERFORM S52-EV-PF8-NYCKL-MOD                               
001329               ADD 1                 TO ACCRAD                            
001330             END-PERFORM                                                  
001331                                                                          
001332             IF SEGMENT-FINNS                                             
001333               MOVE  MED-22 (SPRAK-IX) TO MOD-TEMFSINF                    
001334             ELSE                                                         
001335               MOVE  MED-12 (SPRAK-IX) TO MOD-TEMFSINF                    
001336               PERFORM S56-SPAR-NYCKL-TO-MOD                              
001337             END-IF                                                       
001338           END-IF                                                         
001339         END-IF                                                           
001340       ELSE                                                               
001341         PERFORM S50B-FLYTTA-TILL-NYCKLAR                                 
001342                                                                          
001343         PERFORM IMS-GU-ORQA01-R                                          
001344         IF SEGMENT-FINNS                                                 
001345           PERFORM IMS-GU-ORQI12                                          
001346           MOVE ODEL-IDPRC        TO W-IDPRC                              
001347           PERFORM IMS-GNP-ORQI21-PRC                                     
001348           PERFORM UNTIL SEGMENT-SAKNAS OR                                
001349                         ACCRAD > 14                                      
001350             MOVE LOR-ADLAGOMR    TO W-Q4-ADLAGOMR-MIN                    
001351                                     W-Q4A1-ADLAGOMR-MIN                  
001352                                     W-Q4A1-ADLAGOMR-MAX                  
001353             PERFORM S50C-MOVE-Q4-D3-INFO-TO-MOD                          
001354             PERFORM S56-SPAR-NYCKL-TO-MOD                                
001355             PERFORM IMS-GN-ORQA01-R                                      
001356             IF SEGMENT-FINNS                                             
001357               MOVE ODEL-IDPRC        TO W-IDPRC                          
001358               PERFORM IMS-GNP-ORQI21-PRC                                 
001359             ELSE                                                         
001360               SET SEGMENT-SAKNAS TO TRUE                                 
001361             END-IF                                                       
001362           END-PERFORM                                                    
001363           IF SEGMENT-FINNS                                               
001364             MOVE MED-23 (SPRAK-IX)  TO MOD-TEMFSINF                      
001365           ELSE                                                           
001366             MOVE MED-13 (SPRAK-IX)  TO MOD-TEMFSINF                      
001367           END-IF                                                         
001368         END-IF                                                           
001416       END-IF                                                             
001417     END-IF                                                               
001418     .                                                                    
001419     EJECT                                                                
001420 S50A-FLYTTA-Q4INFO-TILL-MOD   SECTION.                                   
001421                                                                          
001422     IF ACCRAD = ZERO                                                     
001423       MOVE +1                    TO ACCRAD                               
001424     END-IF                                                               
001425                                                                          
001426     MOVE Q4-ORAD-ADLAGOMR        TO MOD-ADLAGOMR (ACCRAD)                
001427     MOVE Q4-ORAD-ADGANG          TO MOD-ADGANG   (ACCRAD)                
001428     MOVE Q4-ORAD-ADPLATS         TO MOD-ADPLATS  (ACCRAD)                
001429     MOVE Q4-ORAD-IDARTNR         TO MOD-IDARTNR  (ACCRAD)                
001430     MOVE '-'                     TO MOD-STRACK   (ACCRAD)                
001431     MOVE Q4-ORAD-REKSIFFR        TO MOD-REKSIFFR (ACCRAD)                
001432     MOVE Q4-ORAD-IDKUNDRF-RO      TO MOD-IDRONR   (ACCRAD)               
001433     INSPECT MOD-IDRONR (ACCRAD)                                          
001434             REPLACING LEADING ZERO BY SPACE                              
001435                                                                          
001436     MOVE IDDISTR-WS              TO DIST08-IDDISTR                       
001437     IF DIST08-URSP-RAPP                                                  
001438     OR DIST08-URSP-SPX                                                   
001439       MOVE Q4-ORAD-KDARTURS      TO MOD-KDARTURS (ACCRAD)                
001440     ELSE                                                                 
001441       MOVE MFS-RENSA-FAELT       TO MOD-KDARTURS (ACCRAD)                
001442     END-IF                                                               
001443     .                                                                    
001444     SKIP2                                                                
001445 S50B-FLYTTA-TILL-NYCKLAR SECTION.                                        
001446                                                                          
001447     MOVE W-IDORDER-MIN           TO W-301-IDORDER-MIN                    
001448                                     W-301-IDORDER-MAX                    
001449                                     W-Q4-IDORDER-MIN                     
001450                                     W-Q4-IDORDER-MAX                     
001451                                     W-IDORDER                            
001452     MOVE IDPRODNR-WS             TO W-301-IDPRODNR-MIN                   
001453                                     W-301-IDPRODNR-MAX                   
001454     MOVE W-IDDC                  TO W-301-IDDC-MIN                       
001455                                     W-301-IDDC-MAX                       
001456                                     W-Q4-IDDC-MIN                        
001457                                     W-Q4-IDDC-MAX                        
001458     MOVE ZERO                    TO W-301-IDPLKLST-MIN                   
001459     MOVE 999                     TO W-301-IDPLKLST-MAX                   
001460     .                                                                    
001461     SKIP2                                                                
001462 S50C-MOVE-Q4-D3-INFO-TO-MOD SECTION.                                     
001463                                                                          
001464     PERFORM IMS-GU-ORQF01                                                
001465     IF SEGMENT-FINNS                                                     
001466       PERFORM S55-PF8-NYCKL-TO-SPAR                                      
001467       IF Q4-ORAD-IDORDER = W-Q4-IDORDER-MIN                              
001468         MOVE 'N'             TO WS-SLINGA-KLAR                           
001469                                                                          
001470         PERFORM UNTIL SEGMENT-SAKNAS OR END-OF-DATA OR                   
001471                       SLINGA-KLAR                                        
001472                                                                          
001473           IF (Q4-ORAD-ADLAGOMR = W-Q4A1-ADLAGOMR-MIN) AND                
001474               ACCRAD < 15                                                
001475             PERFORM S53-FLYTTA-Q4INFO-TILL-MOD                           
001476             MOVE Q4-ORAD-IDARTNR   TO W-IDARTNR                          
001477             PERFORM S54-LAS-LAGERINFO-TILL-MOD                           
001478             PERFORM S51-LAS-BENA-TILL-MOD                                
001479             PERFORM IMS-GN-ORQF01                                        
001480             PERFORM S52-EV-PF8-NYCKL-MOD                                 
001481             ADD +1                 TO ACCRAD                             
001482           ELSE                                                           
001483             MOVE 'J'             TO WS-SLINGA-KLAR                       
001484           END-IF                                                         
001485         END-PERFORM                                                      
001486       END-IF                                                             
001487     END-IF                                                               
001488     .                                                                    
001489     SKIP2                                                                
001490 S50D-EV-MID-TILL-NYCKL SECTION.                                          
001491                                                                          
001492     IF MID-FORSTA-Q4-IDORDER > ZERO                                      
001493       IF MFS-IDPFK = '8'                                                 
001494         MOVE JA                    TO KUNDORDER-AER-NYCKEL               
001495         MOVE MID-FORSTA-Q4-IDORDER TO W-IDORDER-MIN                      
001496                                       W-IDORDER-MAX                      
001497                                       W-Q4A1-IDORDER-MIN                 
001498                                       W-Q4A1-IDORDER-MAX                 
001499                                       W-Q4-IDORDER-MIN                   
001500                                       W-Q4-IDORDER-MAX                   
001501         MOVE MID-FORSTA-Q4-IDARTNR TO W-IDARTNR-MIN                      
001502                                       W-Q4A1-IDARTNR-MIN                 
001503                                       W-Q4-IDARTNR-MIN                   
001504         MOVE MID-FORSTA-Q4-IDLOPNR TO W-IDLOPNR-MIN                      
001505                                       W-Q4A1-IDLOPNR-MIN                 
001506                                       W-Q4-IDLOPNR-MIN                   
001507       END-IF                                                             
001508     END-IF                                                               
001509     .                                                                    
001510     SKIP2                                                                
001511 S51-LAS-BENA-TILL-MOD SECTION.                                           
001512                                                                          
001513     PERFORM IMS-BENA01-LASGU-ROTSEG                                      
001514     IF SEGMENT-FINNS                                                     
001515        IF SWEDISH-TEXT                                                   
001516            MOVE 'S  ' TO        W-IDSKYLT                                
001517            PERFORM IMS-BENA11-LASGNP-TEXTSEG                             
001518            IF SEGMENT-FINNS                                              
001519                MOVE BENA-TEXT-BEART TO MOD-BEART (ACCRAD)                
001520            END-IF                                                        
001521        ELSE                                                              
001522            MOVE 'GB ' TO        W-IDSKYLT                                
001523            PERFORM IMS-BENA11-LASGNP-TEXTSEG                             
001524            IF SEGMENT-FINNS                                              
001525                MOVE BENA-TEXT-BEART TO MOD-BEART (ACCRAD)                
001526            ELSE                                                          
001527                MOVE SPACE TO MOD-BEART (ACCRAD)                          
001528            END-IF                                                        
001529        END-IF                                                            
001530     ELSE                                                                 
001531        MOVE SPACE TO MOD-BEART (ACCRAD)                                  
001532     END-IF                                                               
001533     .                                                                    
001534     EJECT                                                                
001535 S52-EV-PF8-NYCKL-MOD SECTION.                                            
001536                                                                          
001537     IF SEGMENT-FINNS                                                     
001538       MOVE Q4-ORAD-IDORDER       TO MOD-FORSTA-Q4-IDORDER                
001539       MOVE Q4-ORAD-IDARTNR       TO MOD-FORSTA-Q4-IDARTNR                
001540       MOVE Q4-ORAD-IDLOPNR       TO MOD-FORSTA-Q4-IDLOPNR                
001541     ELSE                                                                 
001542       MOVE ZERO                  TO MOD-FORSTA-Q4-IDORDER                
001543                                     MOD-FORSTA-Q4-IDARTNR                
001544                                     MOD-FORSTA-Q4-IDLOPNR                
001545     END-IF                                                               
001546     .                                                                    
001547     EJECT                                                                
001548 S53-FLYTTA-Q4INFO-TILL-MOD   SECTION.                                    
001549                                                                          
001550     IF ACCRAD = ZERO                                                     
001551       MOVE +1     TO ACCRAD                                              
001552     END-IF                                                               
001553     MOVE Q4-ORAD-ADLAGOMR        TO MOD-ADLAGOMR (ACCRAD)                
001554     MOVE Q4-ORAD-ADGANG          TO MOD-ADGANG   (ACCRAD)                
001555     MOVE Q4-ORAD-ADPLATS         TO MOD-ADPLATS  (ACCRAD)                
001556     MOVE Q4-ORAD-IDARTNR         TO MOD-IDARTNR  (ACCRAD)                
001557     MOVE '-'                     TO MOD-STRACK   (ACCRAD)                
001558     MOVE Q4-ORAD-REKSIFFR        TO MOD-REKSIFFR (ACCRAD)                
001559     MOVE Q4-ORAD-IDKUNDRF-RO      TO MOD-IDRONR   (ACCRAD)               
001560     INSPECT MOD-IDRONR (ACCRAD)                                          
001561             REPLACING LEADING ZERO BY SPACE                              
001562                                                                          
001563     MOVE IDDISTR-WS              TO DIST08-IDDISTR                       
001564     IF DIST08-URSP-RAPP                                                  
001565     OR DIST08-URSP-SPX                                                   
001566       MOVE Q4-ORAD-KDARTURS      TO MOD-KDARTURS (ACCRAD)                
001567     ELSE                                                                 
001568       MOVE MFS-RENSA-FAELT       TO MOD-KDARTURS (ACCRAD)                
001569     END-IF                                                               
001570     .                                                                    
001571     SKIP2                                                                
001572 S54-LAS-LAGERINFO-TILL-MOD SECTION.                                      
001573                                                                          
001574     MOVE WS-IDDC               TO W-IDDC                                 
001575     IF CDC                                                               
001576       PERFORM IMS-GU-ARTC11                                              
001577                                                                          
001578       IF SEGMENT-FINNS                                                   
001579         MOVE 1              TO CD-IX                                     
001580         PERFORM UNTIL CD-IX > 4                                          
001581         OR Q4-ORAD-ADLAGOMR = CLAG-ADLAGOMR-CD (CD-IX)                   
001582           ADD 1             TO CD-IX                                     
001583         END-PERFORM                                                      
001584                                                                          
001585         IF CD-IX > 4                                                     
001586                                                                          
001587           MOVE CLAG-ADLAGOMR       TO MOD-ADLAGOMR (ACCRAD)              
001588           MOVE CLAG-ADGANG         TO MOD-ADGANG (ACCRAD)                
001589           MOVE CLAG-ADPLATS        TO MOD-ADPLATS (ACCRAD)               
001590         ELSE                                                             
001591           MOVE CLAG-ADLAGOMR-CD (CD-IX)                                  
001592                                    TO MOD-ADLAGOMR (ACCRAD)              
001593           MOVE CLAG-ADGANG-CD (CD-IX) TO MOD-ADGANG (ACCRAD)             
001594           MOVE CLAG-ADPLATS-CD (CD-IX)                                   
001595                                    TO MOD-ADPLATS (ACCRAD)               
001596         END-IF                                                           
001597       END-IF                                                             
001598     ELSE                                                                 
001599       PERFORM IMS-GU-ARTS11                                              
001600                                                                          
001601       IF SEGMENT-FINNS                                                   
001602           MOVE SLAG-ADLAGOMR       TO MOD-ADLAGOMR (ACCRAD)              
001603           MOVE SLAG-ADGANG         TO MOD-ADGANG (ACCRAD)                
001604           MOVE SLAG-ADPLATS        TO MOD-ADPLATS (ACCRAD)               
001605       END-IF                                                             
001606     END-IF                                                               
001607     .                                                                    
001608     SKIP2                                                                
001609 S55-PF8-NYCKL-TO-SPAR SECTION.                                           
001610                                                                          
001611     IF SEGMENT-FINNS                                                     
001612       MOVE Q4-ORAD-IDORDER       TO SPAR-FORSTA-Q4-IDORDER               
001613       MOVE Q4-ORAD-IDARTNR       TO SPAR-FORSTA-Q4-IDARTNR               
001614       MOVE Q4-ORAD-IDLOPNR       TO SPAR-FORSTA-Q4-IDLOPNR               
001615     END-IF                                                               
001616     .                                                                    
001617     SKIP2                                                                
001618 S56-SPAR-NYCKL-TO-MOD SECTION.                                           
001619                                                                          
001620     IF SEGMENT-SAKNAS AND ACCRAD < 15                                    
001621       MOVE SPAR-FORSTA-Q4-IDORDER TO MOD-FORSTA-Q4-IDORDER               
001622       MOVE SPAR-FORSTA-Q4-IDARTNR TO MOD-FORSTA-Q4-IDARTNR               
001623       MOVE SPAR-FORSTA-Q4-IDLOPNR TO MOD-FORSTA-Q4-IDLOPNR               
001624     END-IF                                                               
001625     .                                                                    
001626     SKIP2                                                                
001627 IMS-GET-MSG SECTION.                                                     
001628     MOVE '  QC' TO GODK-STATUSKODER                                      
001629     CALL CBLTDLI USING GU                                                
001630                          MSG-PCB                                         
001631                          MSG-IO-AREA                                     
001632     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
001633     PERFORM IMS-STATUSKONTROLL                                           
001634     SKIP3                                                                
001635     .                                                                    
001636 IMS-INSERT-MSG SECTION.                                                  
001637     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
001638       MOVE '0' TO MFS-KDHUVOMR                                           
001639     END-IF                                                               
001640     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
001641     MOVE SPACE TO GODK-STATUSKODER                                       
001642     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
001643     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
001644     PERFORM IMS-STATUSKONTROLL                                           
001645     EJECT                                                                
001646                                                                          
001647     .                                                                    
001648 IMS-GU-WDE401-WDE4 SECTION.                                              
001649                                                                          
001650     STRING 'WDE401  (WDE401KY =' W-WDE4KEY-X ')'                         
001651                    DELIMITED BY SIZE INTO SSA1                           
001652     MOVE '  GE' TO GODK-STATUSKODER                                      
001653     CALL CBLTDLI USING GU WDE4-PCB DLI-IO-AREA SSA1                      
001654     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
001655     PERFORM IMS-STATUSKONTROLL                                           
001656     .                                                                    
001657 IMS-GNP-WDE411 SECTION.                                                  
001658                                                                          
001659     STRING 'WDE411  (IDPURAD  >' W-IDPURAD-X ')'                         
001660                    DELIMITED BY SIZE INTO SSA1                           
001661     MOVE '  GE' TO GODK-STATUSKODER                                      
001662     CALL CBLTDLI USING GNP WDE4-PCB DLI-IO-AREA SSA1                     
001663     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
001664     PERFORM IMS-STATUSKONTROLL                                           
001665     SKIP2                                                                
001666     .                                                                    
001667 IMS-GU-WDE411-BSEQ SECTION.                                              
001668                                                                          
001669     MOVE '  GE' TO GODK-STATUSKODER                                      
001670     STRING 'WDE411  (WDE4BSEQ =' W-WDE4BSEQ-X ')'                        
001671            DELIMITED BY SIZE INTO SSA1                                   
001672     CALL CBLTDLI USING GU WDE4B-PCB DLI-IO-AREA SSA1                     
001673     MOVE WDE4B-STATUS-CODE TO STATUS-WS                                  
001674     PERFORM IMS-STATUSKONTROLL                                           
001675     .                                                                    
001676 IMS-GN-WDE411-BSEQ          SECTION.                                     
001677                                                                          
001678     MOVE '  GEGB' TO GODK-STATUSKODER                                    
001679     STRING 'WDE411  (WDE4BSEQ>=' W-WDE4BSEQ-MIN-X                        
001680                    '&WDE4BSEQ<=' W-WDE4BSEQ-MAX-X ')'                    
001681                    DELIMITED BY SIZE INTO SSA1                           
001682     CALL CBLTDLI USING GN WDE4B-PCB DLI-IO-AREA SSA1                     
001683     MOVE WDE4B-STATUS-CODE TO STATUS-WS                                  
001684     PERFORM IMS-STATUSKONTROLL                                           
001685     EJECT                                                                
001686     .                                                                    
001687 IMS-GNP-WDE421-KOLLI SECTION.                                            
001688                                                                          
001689     MOVE '    ' TO GODK-STATUSKODER                                      
001690     STRING 'WDE421  (WDE421KY =' W-SAMMANLAGD-KOLLI-NYCKEL-X ')'         
001691            DELIMITED BY SIZE INTO SSA1                                   
001692     MOVE '  GE' TO GODK-STATUSKODER                                      
001693     CALL CBLTDLI USING GNP WDE4B-PCB DLI-IO-AREA SSA1                    
001694     MOVE WDE4B-STATUS-CODE TO STATUS-WS                                  
001695     PERFORM IMS-STATUSKONTROLL                                           
001696                                                                          
001697     .                                                                    
001698 IMS-GNP-WDE421              SECTION.                                     
001699                                                                          
001700     STRING 'WDE421   ' DELIMITED BY SIZE INTO SSA1                       
001701     MOVE '  GE' TO GODK-STATUSKODER                                      
001702     CALL CBLTDLI USING GNP WDE4B-PCB DLI-IO-AREA SSA1                    
001703     MOVE WDE4B-STATUS-CODE TO STATUS-WS                                  
001704     PERFORM IMS-STATUSKONTROLL                                           
001705     EJECT                                                                
001706     .                                                                    
001707 IMS-GU-ARTC11 SECTION.                                                   
001708     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
001709          DELIMITED BY SIZE INTO SSA1                                     
001710     STRING 'WLARTC11(KDSEGKEY =' W-KDSEGKEY-X ')'                        
001711          DELIMITED BY SIZE INTO SSA2                                     
001712     MOVE '  GE' TO GODK-STATUSKODER                                      
001713     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA SSA1 SSA2                 
001714     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
001715     PERFORM IMS-STATUSKONTROLL                                           
001716     .                                                                    
001717     SKIP3                                                                
001718 IMS-GU-ARTS11 SECTION.                                                   
001719     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
001720          DELIMITED BY SIZE INTO SSA1                                     
001721     STRING 'WLARTS11(IDDC     =' W-IDDC-X ')'                            
001722          DELIMITED BY SIZE INTO SSA2                                     
001723     MOVE '  GE' TO GODK-STATUSKODER                                      
001724     CALL CBLTDLI USING GU ARTS-PCB DLI-IO-AREA SSA1 SSA2                 
001725     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
001726     PERFORM IMS-STATUSKONTROLL                                           
001727     .                                                                    
001728     SKIP3                                                                
001729 IMS-BENA01-LASGU-ROTSEG SECTION.                                         
001730     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
001731             DELIMITED BY SIZE INTO SSA1                                  
001732     MOVE '  GE' TO GODK-STATUSKODER                                      
001733     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA SSA1                      
001734     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
001735     PERFORM IMS-STATUSKONTROLL                                           
001736     SKIP2                                                                
001737     .                                                                    
001738 IMS-BENA11-LASGNP-TEXTSEG SECTION.                                       
001739     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
001740             DELIMITED BY SIZE INTO SSA1                                  
001741     MOVE '  GE' TO GODK-STATUSKODER                                      
001742     CALL CBLTDLI USING GNP BENA-PCB DLI-IO-AREA SSA1                     
001743     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
001744     PERFORM IMS-STATUSKONTROLL                                           
001745     EJECT                                                                
001746                                                                          
001747     .                                                                    
001748 IMS-GU-WDE601 SECTION.                                                   
001749                                                                          
001750     MOVE IDPRODNR-WS     TO W-IDPRODNR                                   
001751     MOVE '  GE' TO GODK-STATUSKODER                                      
001752     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
001753            DELIMITED BY SIZE INTO SSA1                                   
001754     CALL CBLTDLI USING GU WDE6-PCB DLI-IO-AREA SSA1                      
001755     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
001756     PERFORM IMS-STATUSKONTROLL                                           
001757     SKIP3                                                                
001758     .                                                                    
001759 IMS-GU-WDE4E1 SECTION.                                                   
001760                                                                          
001761     STRING 'WDE4E1  (WDE4E1KY=>' W-WDE4E1KY-MIN-X ')'                    
001762                    '&WDE4E1KY=<' W-WDE4E1KY-MAX-X ')'                    
001763            DELIMITED BY SIZE INTO SSA1                                   
001764     MOVE '    ' TO GODK-STATUSKODER                                      
001765     CALL CBLTDLI USING GU WDE4E-PCB WDE4E-AREA SSA1                      
001766     MOVE WDE4E-STATUS-CODE TO STATUS-WS                                  
001767     PERFORM IMS-STATUSKONTROLL                                           
001768     EJECT                                                                
001769     .                                                                    
001770 IMS-GU-WDE401 SECTION.                                                   
001771                                                                          
001772     MOVE '  GE' TO GODK-STATUSKODER                                      
001773     STRING 'WDE401  (WDE401KY =' W-WDE4KEY-X ')'                         
001774            DELIMITED BY SIZE INTO SSA1                                   
001775     CALL CBLTDLI USING GU WDE42-PCB DLI-IO-AREA SSA1                     
001776     MOVE WDE42-STATUS-CODE TO STATUS-WS                                  
001777     PERFORM IMS-STATUSKONTROLL                                           
001778     SKIP3                                                                
001779                                                                          
001780     .                                                                    
001781 IMS-GU-WDE4A1 SECTION.                                                   
001782                                                                          
001783     STRING 'WDE4A1  (WDE4A1KY=>' W-WDE4KEY-X                             
001784                    '&WDE4A1KY=<' W-WDE4KEY-MAX-X ')'                     
001785            DELIMITED BY SIZE INTO SSA1                                   
001786     MOVE '  GEGB' TO GODK-STATUSKODER                                    
001787     CALL CBLTDLI USING GU WDE4A-PCB DLI-IO-AREA SSA1                     
001788     MOVE WDE4A-STATUS-CODE TO STATUS-WS                                  
001789                               STATUS-WDE401-SEK-WS                       
001790     PERFORM IMS-STATUSKONTROLL                                           
001791     SKIP3                                                                
001792     .                                                                    
001793 IMS-GU-WDE401-ASEQ SECTION.                                              
001794                                                                          
001795     STRING 'WDE401  (WDE4ASEQ =' W-WDE4AKEY-X ')'                        
001796            DELIMITED BY SIZE INTO SSA1                                   
001797     MOVE '  GEGB' TO GODK-STATUSKODER                                    
001798     CALL CBLTDLI USING GU WDE41-PCB DLI-IO-AREA SSA1                     
001799     MOVE WDE41-STATUS-CODE TO STATUS-WS                                  
001800                               STATUS-WDE401-SEK-WS                       
001801     PERFORM IMS-STATUSKONTROLL                                           
001802     SKIP3                                                                
001803     .                                                                    
001804 IMS-GN-WDE401-ASEQ SECTION.                                              
001805                                                                          
001806     STRING 'WDE401  (WDE4ASEQ =' W-WDE4AKEY-X ')'                        
001807            DELIMITED BY SIZE INTO SSA1                                   
001808     MOVE '  GEGB' TO GODK-STATUSKODER                                    
001809     CALL CBLTDLI USING GN WDE41-PCB DLI-IO-AREA SSA1                     
001810     MOVE WDE41-STATUS-CODE TO STATUS-WS                                  
001811                               STATUS-WDE401-SEK-WS                       
001812     PERFORM IMS-STATUSKONTROLL                                           
001813     SKIP3                                                                
001814     .                                                                    
001815 IMS-GU-ORQG01-Q4A1 SECTION.                                              
001816                                                                          
001817     STRING 'WLORQG01(WDQ4A1KY=>' W-WDQ4A1KY-MIN-X                        
001818                    '&WDQ4A1KY=<' W-WDQ4A1KY-MAX-X ')'                    
001819            DELIMITED BY SIZE INTO SSA1                                   
001820     MOVE '  GE' TO GODK-STATUSKODER                                      
001821     CALL CBLTDLI USING GU ORQG-PCB DLI-IO-AREA SSA1                      
001822     MOVE ORQG-STATUS-CODE     TO STATUS-WS                               
001823     PERFORM IMS-STATUSKONTROLL                                           
001824     .                                                                    
001825     SKIP2                                                                
001826 IMS-GU-ORQF01 SECTION.                                                   
001827                                                                          
001828     STRING 'WLORQF01(WDQ401KY=>' W-WDQ401KY-MIN-X                        
001829                    '&WDQ401KY=<' W-WDQ401KY-MAX-X ')'                    
001830            DELIMITED BY SIZE INTO SSA1                                   
001831     MOVE '  GBGE' TO GODK-STATUSKODER                                    
001832     CALL CBLTDLI USING GU ORQF-PCB DLI-IO-AREA SSA1                      
001833     MOVE ORQF-STATUS-CODE      TO STATUS-WS                              
001834     PERFORM IMS-STATUSKONTROLL                                           
001835     .                                                                    
001836     SKIP2                                                                
001837 IMS-GU-ORQF01-KVAL SECTION.                                              
001838                                                                          
001839     STRING 'WLORQF01(WDQ401KY =' W-WDQ401KY-MIN-X ')'                    
001840            DELIMITED BY SIZE INTO SSA1                                   
001841     MOVE '  GBGE' TO GODK-STATUSKODER                                    
001842     CALL CBLTDLI USING GU ORQF-PCB DLI-IO-AREA SSA1                      
001843     MOVE ORQF-STATUS-CODE      TO STATUS-WS                              
001844     PERFORM IMS-STATUSKONTROLL                                           
001845     .                                                                    
001846     SKIP2                                                                
001847 IMS-GN-ORQF01 SECTION.                                                   
001848                                                                          
001849     STRING 'WLORQF01(WDQ401KY=>' W-WDQ401KY-MIN-X                        
001850                    '&WDQ401KY=<' W-WDQ401KY-MAX-X ')'                    
001851            DELIMITED BY SIZE INTO SSA1                                   
001852     MOVE '  GBGE' TO GODK-STATUSKODER                                    
001853     CALL CBLTDLI USING GN ORQF-PCB DLI-IO-AREA SSA1                      
001854     MOVE ORQF-STATUS-CODE      TO STATUS-WS                              
001855     PERFORM IMS-STATUSKONTROLL                                           
001856     .                                                                    
001857     SKIP2                                                                
001858 IMS-GU-ORQA01-R SECTION.                                                 
001859                                                                          
001860     STRING 'WLORQA01(WDQ301KY=>' W-WDQ301KY-MIN-X                        
001870                    '&WDQ301KY=<' W-WDQ301KY-MAX-X                        
001880                    '&KDODELST =' W-KDODELST-X ')'                        
001881            DELIMITED BY SIZE INTO SSA1                                   
001882     MOVE '  GE' TO GODK-STATUSKODER                                      
001883     CALL CBLTDLI USING GU ORQA-PCB DLI-IO-AREA SSA1                      
001884     MOVE ORQA-STATUS-CODE     TO STATUS-WS                               
001885     PERFORM IMS-STATUSKONTROLL                                           
001886     .                                                                    
001887 IMS-GN-ORQA01-R SECTION.                                                 
001888                                                                          
001889     STRING 'WLORQA01(WDQ301KY=>' W-WDQ301KY-MIN-X                        
001890                    '&WDQ301KY=<' W-WDQ301KY-MAX-X ')'                    
001891                    '&KDODELST =' W-KDODELST-X ')'                        
001892            DELIMITED BY SIZE INTO SSA1                                   
001893     MOVE '  GEGB' TO GODK-STATUSKODER                                    
001894     CALL CBLTDLI USING GN ORQA-PCB DLI-IO-AREA SSA1                      
001895     MOVE ORQA-STATUS-CODE     TO STATUS-WS                               
001896     PERFORM IMS-STATUSKONTROLL                                           
001897     .                                                                    
001898 IMS-GU-ORQI12 SECTION.                                                   
001899                                                                          
001900     STRING 'WLORQI01(IDORDER  =' W-IDORDER-X ')'                         
001901            DELIMITED BY SIZE INTO SSA1                                   
001902     STRING 'WLORQI12(IDDC     =' W-IDDC-X ')'                            
001903            DELIMITED BY SIZE INTO SSA2                                   
001904     MOVE '  GE' TO GODK-STATUSKODER                                      
001905     CALL CBLTDLI USING GU ORQI-PCB DLI-IO-Q212 SSA1 SSA2                 
001906     MOVE ORQI-STATUS-CODE     TO STATUS-WS                               
001907     PERFORM IMS-STATUSKONTROLL                                           
001908     .                                                                    
001909 IMS-GNP-ORQI21-PRC SECTION.                                              
001910                                                                          
001911     STRING 'WLORQI21*F(IDPRC    =' W-IDPRC-X ')'                         
001912            DELIMITED BY SIZE INTO SSA1                                   
001913     MOVE '  GE' TO GODK-STATUSKODER                                      
001914     CALL CBLTDLI USING GNP ORQI-PCB DLI-IO-Q221 SSA1                     
001915     MOVE ORQI-STATUS-CODE     TO STATUS-WS                               
001916     PERFORM IMS-STATUSKONTROLL                                           
001917     .                                                                    
001918 IMS-STATUSKONTROLL SECTION.                                              
001919     SET STATUS-IX TO 1                                                   
001920     SEARCH GODK-STATUS AT END CALL FELLOG                                
001921       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
001922     END-SEARCH                                                           
001930     CONTINUE                                                             
002000     .                                                                    
