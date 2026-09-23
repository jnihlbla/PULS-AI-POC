000001 ID DIVISION.                                                             
000002     SKIP2                                                                
000003 PROGRAM-ID.     WL011800.                                                
000004 AUTHOR.         BERT ANDERSSON.                                          
000005 DATE-WRITTEN.   JUNI 2005.                                               
000006                                                                          
000007     REMARKS.                                                             
000008* WL011800 PROGRAM IS A REPLICA OF W4032300 PROGRAM                       
000009* AND CUSTOMIZED FOR WEB-LDC REQUIREMENTS.                                
000010*                                                                         
000011*    NAMN:       CARPARTS.LDC.PACKINGSPECLINES                            
000012*                                                                         
000013                                                                          
000014*    FUNKTION.                                                            
000015*                                                                         
000016*        SALDOBILD:                                                       
000017*                                                                         
000018*           INDATA            : DISTR, KUND, KUNDRF EL. PRODNR,           
000019*                               SAMT RADNR.                               
000020*                                                                         
000021*           BILDEN SVARAR MED : PACK-UNDERLAG, RADER                      
000022*                                                                         
000023*    INDATA.                                                              
000024*        TRANSAKTION: WL0118U                                             
000025*        REQU:        WL0118I1                                            
000026*                                                                         
000027*    UTDATA.                                                              
000028*        RESP:        WLI118O1                                            
000029*    SKIP3                                                                
000030 ENVIRONMENT DIVISION.                                                    
000031     SKIP3                                                                
000032 DATA DIVISION.                                                           
000033     EJECT                                                                
000034 WORKING-STORAGE SECTION.                                                 
000035                                                                          
000036*    -- CHECKED BY WY2000                                                 
000037 77   PROGRAM-NAMN           VALUE 'WL011800'                             
000038                                 PIC X(8).                                
000039 77  FILLER                      PIC X(8)    VALUE 'FELTEXT'.             
000040 77  FELTEXT                     PIC X(64)   VALUE SPACE.                 
000041                                                                          
000042 77  FILLER                      PIC X(8)    VALUE 'PGM-POS'.             
000043 77  PGM-POS                     PIC X(64)   VALUE SPACE.                 
000044                                                                          
000045 77  JA                          PIC X       VALUE 'J'.                   
000046 77  NEJ                         PIC X       VALUE 'N'.                   
000047 77  IDSKYLT-ENG                 PIC X(3)    VALUE 'GB '.                 
000048 77  IDSKYLT-SVE                 PIC X(3)    VALUE 'S  '.                 
000049 77  KUNDORDER-AER-NYCKEL        PIC X.                                   
000050 77  KDRC-DISPLAY                PIC Z(5)   VALUE ZERO.                   
000051 01  FILLER          PIC X(8) VALUE 'AAAAAAAA'.                           
000052 01  WS-KVBEART      PIC 9(8)V9(5).                                       
000053 01  WS-KVBEART-REST PIC 9(8).                                            
000054 01  FILLER          PIC X(8) VALUE 'BBBBBBBB'.                           
000056 77  NYCKLAR-OK                  PIC X.                                   
000057 77  RADER-SLUT                  PIC X.                                   
000058 77  KOLLIN-SLUT                 PIC X.                                   
000059 77  SPRAK-IX                    PIC S9(9)   VALUE ZERO COMP SYNC.        
000060 77  CD-IX                       PIC S9(9)   VALUE ZERO COMP SYNC.        
000063 77  ACCRAD                      PIC S9(3)   COMP-3.                      
000064 77  WS-MAX-500                  PIC S9(3)   COMP-3 VALUE 500.            
000065 77  WS-MAX-501                  PIC S9(3)   COMP-3 VALUE 501.            
000066 77  TEMP-IDKOLLI                PIC Z(4)9.                               
000067 77  TEMP-FORSTA-IDPURAD         PIC S9(4).                               
000068 77  TEMP-FORSTA-IDKOLLI         PIC S9(5).                               
000069 77  TEMP-IDDISTR                PIC S9(4).                               
000070 77  TEMP-IDKUNDNR               PIC S9(6).                               
000071 77  TEMP-IDORDNR                PIC S9(5).                               
000072 01  FILLER          PIC X(8) VALUE 'CCCCCCCC'.                           
000076 77  WS-SPAR-KVORDRAD-LEVPL      PIC S9(5)   COMP-3 VALUE ZERO.           
000077 77  WS-EGEN-TRANS               PIC X(4)    VALUE '4323'.                
000078 77  WS-IDTRANS                  PIC X(4).                                
000079     88  WS-GODKAEND-BILD                    VALUE '4321' '4322'          
000080                                            '4323' '4324' '4325'.         
000081     88  EGEN-MID                            VALUE '4323'.                
000082 01  FILLER          PIC X(8) VALUE 'DDDDDDDD'.                           
000083 77  WS-SLINGA-KLAR              PIC X(1).                                
000084     88  SLINGA-KLAR                         VALUE 'J'.                   
000085                                                                          
000089 77  IDPURAD-HITTAD-SW           PIC X(1).                                
000090     88 IDPURAD-HITTAD                       VALUE 'J'.                   
000091                                                                          
000092 77  FEL-PLKLST-SW              PIC X(1).                                 
000093     88  FEL-PLKLST                          VALUE 'J'.                   
000094                                                                          
000095 77  WS-IDSKYLT-CN               PIC X(3)  VALUE 'RCN'.                   
000096 77  WS-IDSKYLT-GB               PIC X(3)  VALUE 'GB '.                   
000097 77  WS-CP-UTF8                  PIC X(4)  VALUE 'UTF8'.                  
000098 77  WS-CP-278                   PIC X(3)  VALUE '278'.                   
000099                                                                          
000100 01  W-SPAR-IDKUNDRF.                                                     
000101     03  FILLER                  PIC X(2)    VALUE '00'.                  
000102     03  W-SPAR-IDORDNR5         PIC X(5)    VALUE '+++++'.               
000103     03  FILLER                  PIC X(3)    VALUE '+++'.                 
000104 01  FILLER          PIC X(8) VALUE 'EEEEEEEE'.                           
000105 01  IDANSTNR-WS                 PIC X(5).                                
000106 01  IDDISTR-WS                  PIC X(4).                                
000107 01  IDKUNDNR-WS                 PIC X(6).                                
000108 01  IDKUNDRF-WS                 PIC X(5).                                
000109 01  IDPURAD-WS                  PIC X(4).                                
000110 01  IDPRODNR-WS                 PIC X(7).                                
000111 01  IDPRODNR-WS-JFR             PIC X(7).                                
000112 01  FILLER          PIC X(8) VALUE 'FFFFFFFF'.                           
000113*                                                                         
000114 01  TRANSFER-KUND               PIC 9(7).                                
000115     88 TRANSFER-KUNDNR          VALUE 0000511                            
000116                                       0000512                            
000117                                       0000513.                           
000118     88  RETUR-KUNDNR            VALUE 0000051.                           
000119                                                                          
000120*    --- VALID DC CODES                                                   
000121*01  -COPY WWDC99                                                         
000122     EJECT                                                                
000123                                                                          
000124*                                                                         
000125 01  DYNAMISKA-SUBPROGRAM.                                                
000126     03  CBLTDLI                 PIC X(8) VALUE 'CBLTDLI '.               
000127     03  FELLOG                  PIC X(8) VALUE 'FELLOG  '.               
000128     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
000129     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
000130     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
000131     03  WTRAUTF8                PIC X(8)    VALUE 'WTRAUTF8'.            
000132     EJECT                                                                
000133*                                                                         
000134*    --- PARAMETERS TO ABEND                                              
000135                                                                          
000136 77  FILLER                      PIC X(08)   VALUE 'ABENDARE'.            
000137 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +33.              
000138 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
000139 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
000140     SKIP2                                                                
000141 77  FILLER                      PIC X(08)   VALUE 'MESSAGES'.            
000142 01  MESSAGE-CODES.                                                       
000143     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
000144     03  MUST-BE-NUMERIC         PIC X(3)    VALUE '024'.                 
000145     03  ERR-KEY-MISSING         PIC X(3)    VALUE '041'.                 
000146     03  ERR-CORR-FIELDS         PIC X(3)    VALUE '023'.                 
000147     03  ERR-ARTIKEL-SAKNAS      PIC X(3)    VALUE '017'.                 
000148     03  INF-NO-MORE-LINES       PIC X(3)    VALUE '316'.                 
000149     SKIP3                                                                
000150 01  FILLER                      PIC X(16) VALUE 'WTRAUTF8-AREA'.         
000151*01  -COPY WTRAUTF8                                                       
000152     EJECT                                                                
000153 77  FILLER                      PIC X(08)   VALUE 'WDIST08 '.            
000154*01  -COPY WWDIST08.                                                      
000155     EJECT                                                                
000156 01  FILLER          PIC X(8) VALUE 'GGGGGGGG'.                           
000157 01    NYCKLAR-TILL-DLI.                                                  
000158   03    W-WDE4E1KY-MAX-X.                                                
000159     05    W-IDPRODNR-WDE4E-MAX  PIC S9(7)   VALUE ZERO  COMP-3.          
000160     05    W-WDE4E1-MAX          PIC X(19)   VALUE HIGH-VALUE.            
000161                                                                          
000162   03    W-WDE4E1KY-MIN-X.                                                
000163     05    W-IDPRODNR-WDE4E-MIN  PIC S9(7)   VALUE ZERO  COMP-3.          
000164     05    W-WDE4E1-MIN          PIC X(19)   VALUE LOW-VALUE.             
000165                                                                          
000166     EJECT                                                                
000167   03    W-WDE4AKEY-X.                                                    
000168     05    W-E4A1-IDDISTR        PIC S9(5)   VALUE ZERO  COMP-3.          
000169     05    W-E4A1-IDKUNDNR       PIC S9(7)   VALUE ZERO  COMP-3.          
000170     05    W-E4A1-IDKUNDRF       PIC X(10).                               
000171*                                                                         
000172   03    W-WDE4KEY-X.                                                     
000173     05    W-E401-IDDISTR        PIC S9(5)   VALUE ZERO  COMP-3.          
000174     05    W-E401-IDKUNDNR       PIC S9(7)   VALUE ZERO  COMP-3.          
000175     05    W-E401-IDKUNDRF.                                               
000176       07    W-E401-IDORDNR      PIC 9(5)    VALUE ZERO.                  
000177       07    FILLER              PIC X(5)    VALUE SPACE.                 
000178     05    W-E401-IDPRODNR       PIC S9(7)   VALUE ZERO  COMP-3.          
000179     05    W-E401-IDPLKLST       PIC S9(3)   VALUE ZERO  COMP-3.          
000180   03    W-WDE4KEY-MAX-X.                                                 
000181     05    W-E401-IDDISTR-MAX    PIC S9(5)   VALUE ZERO  COMP-3.          
000182     05    W-E401-IDKUNDNR-MAX   PIC S9(7)   VALUE ZERO  COMP-3.          
000183     05    W-E401-IDKUNDRF-MAX.                                           
000184       07    W-E401-IDORDNR-MAX  PIC 9(5)    VALUE ZERO.                  
000185       07    FILLER              PIC X(5)    VALUE SPACE.                 
000186     05    W-E401-IDPRODNR-MAX   PIC S9(7)   VALUE ZERO  COMP-3.          
000187     05    W-E401-IDPLKLST-MAX   PIC S9(3)   VALUE 999   COMP-3.          
000188   03    W-IDPRODNR-X.                                                    
000189     05    W-IDPRODNR            PIC S9(7)   VALUE ZERO  COMP-3.          
000190*                                                                         
000191   03    W-IDPURAD-X.                                                     
000192     05    WS-IDPURAD            PIC S9(5)   VALUE ZERO  COMP-3.          
000193*                                                                         
000194   03    W-WDE4BSEQ-X.                                                    
000195     05    W-WDE4B-IDPRODNR      PIC S9(7)   VALUE ZERO  COMP-3.          
000196     05    W-IDPURAD             PIC S9(5)   VALUE ZERO  COMP-3.          
000197*                                                                         
000198   03    W-WDE4BSEQ-MIN-X.                                                
000199     05    W-IDPRODNR-MIN        PIC S9(7)   VALUE ZERO  COMP-3.          
000200     05    W-IDPURAD-MIN         PIC S9(5)   VALUE ZERO  COMP-3.          
000201   03    W-WDE4BSEQ-MAX-X.                                                
000202     05    W-IDPRODNR-MAX        PIC S9(7)   VALUE ZERO  COMP-3.          
000203     05    W-IDPURAD-MAX         PIC S9(5)   VALUE ZERO  COMP-3.          
000204*                                                                         
000205   03    W-SAMMANLAGD-KOLLI-NYCKEL-X.                                     
000206     05    W-K-IDPRODNR          PIC S9(7)   VALUE ZERO  COMP-3.          
000207     05    W-K-IDKOLLI           PIC S9(5)   VALUE ZERO  COMP-3.          
000208*                                                                         
000209   03    W-IDARTNR-X.                                                     
000210     05    W-IDARTNR             PIC S9(9)   VALUE ZERO  COMP-3.          
000211   03    W-IDORDER-X.                                                     
000212     05    W-IDORDER             PIC S9(7)   VALUE ZERO  COMP-3.          
000213   03    W-KDSEGKEY-X.                                                    
000214     05    W-KDSEGKEY            PIC X       VALUE '1'.                   
000215   03    W-IDDC-X.                                                        
000216     05    W-IDDC                PIC X(02).                               
000217   03    W-IDSKYLT-X.                                                     
000218     05    W-IDSKYLT             PIC X(3).                                
000219   03    W-IDPRC-X.                                                       
000220     05    W-IDPRC               PIC X(4).                                
000221   03    W-KDODELST-X.                                                    
000222     05    W-KDODELST            PIC X       VALUE 'R'.                   
000223*                                                                         
000224   03    W-WDQ4ASEQ-MIN-X.                                                
000225     05    W-IDORDER-MIN         PIC S9(7) VALUE ZERO COMP-3.             
000226     05    W-IDARTNR-MIN         PIC S9(9) VALUE ZERO COMP-3.             
000227     05    W-IDLOPNR-MIN         PIC S9(3) VALUE ZERO COMP-3.             
000228   03    W-WDQ4ASEQ-MAX-X.                                                
000229     05    W-IDORDER-MAX         PIC S9(7) VALUE ZERO COMP-3.             
000230     05    W-IDARTNR-MAX       PIC S9(9) VALUE 999999999 COMP-3.          
000231     05    W-IDLOPNR-MAX         PIC S9(3) VALUE 999     COMP-3.          
000232*                                                                         
000233   03    W-WDQ301KY-MIN-X.                                                
000234     05    W-301-IDORDER-MIN     PIC S9(7)   VALUE ZERO  COMP-3.          
000235     05    W-301-IDDC-MIN        PIC X(02)   VALUE SPACE.                 
000236     05    W-301-IDPRODNR-MIN    PIC S9(7)   VALUE ZERO  COMP-3.          
000237     05    W-301-IDPLKLST-MIN    PIC S9(3)   VALUE ZERO  COMP-3.          
000238   03    W-WDQ301KY-MAX-X.                                                
000239     05    W-301-IDORDER-MAX     PIC S9(7)   VALUE ZERO  COMP-3.          
000240     05    W-301-IDDC-MAX        PIC X(02)   VALUE SPACE.                 
000241     05    W-301-IDPRODNR-MAX    PIC S9(7)   VALUE ZERO  COMP-3.          
000242     05    W-301-IDPLKLST-MAX    PIC S9(3)   VALUE 999   COMP-3.          
000243*                                                                         
000244   03    W-WDQ4A1KY-MIN-X.                                                
000245     05    W-Q4A1-IDORDER-MIN     PIC S9(7)   VALUE ZERO  COMP-3.         
000246     05    W-Q4A1-IDARTNR-MIN     PIC S9(9)   VALUE ZERO  COMP-3.         
000247     05    W-Q4A1-IDLOPNR-MIN     PIC S9(3)   VALUE ZERO  COMP-3.         
000248     05    W-Q4A1-IDDC-MIN        PIC X(02)   VALUE SPACE.                
000249     05    W-Q4A1-ADLAGOMR-MIN    PIC S9(3)   VALUE ZERO  COMP-3.         
000250     05    W-Q4A1-ADGANG-MIN      PIC S9(3)   VALUE ZERO  COMP-3.         
000251     05    W-Q4A1-ADPLATS-MIN     PIC S9(5)   VALUE ZERO  COMP-3.         
000252   03    W-WDQ4A1KY-MAX-X.                                                
000253     05    W-Q4A1-IDORDER-MAX     PIC S9(7)   VALUE ZERO  COMP-3.         
000254     05    W-Q4A1-IDARTNR-MAX   PIC S9(9) VALUE 999999999 COMP-3.         
000255     05    W-Q4A1-IDLOPNR-MAX     PIC S9(3)   VALUE 999   COMP-3.         
000256     05    W-Q4A1-IDDC-MAX        PIC X(02)   VALUE SPACE.                
000257     05    W-Q4A1-ADLAGOMR-MAX    PIC S9(3)   VALUE 999   COMP-3.         
000258     05    W-Q4A1-ADGANG-MAX      PIC S9(3)   VALUE 999   COMP-3.         
000259     05    W-Q4A1-ADPLATS-MAX     PIC S9(5)   VALUE 99999 COMP-3.         
000260*                                                                         
000261   03    W-WDQ401KY-MIN-X.                                                
000262     05    W-Q4-IDORDER-MIN       PIC S9(7)   VALUE ZERO  COMP-3.         
000263     05    W-Q4-IDDC-MIN          PIC X(02)   VALUE SPACE.                
000264     05    W-Q4-ADLAGOMR-MIN      PIC S9(3)   VALUE ZERO  COMP-3.         
000265     05    W-Q4-ADGANG-MIN        PIC S9(3)   VALUE ZERO  COMP-3.         
000266     05    W-Q4-ADPLATS-MIN       PIC S9(5)   VALUE ZERO  COMP-3.         
000267     05    W-Q4-IDARTNR-MIN       PIC S9(9)   VALUE ZERO  COMP-3.         
000268     05    W-Q4-IDLOPNR-MIN       PIC S9(3)   VALUE ZERO  COMP-3.         
000269   03    W-WDQ401KY-MAX-X.                                                
000270     05    W-Q4-IDORDER-MAX       PIC S9(7)   VALUE ZERO  COMP-3.         
000271     05    W-Q4-IDDC-MAX          PIC X(02)   VALUE SPACE.                
000272     05    W-Q4-ADLAGOMR-MAX      PIC S9(3)   VALUE 999   COMP-3.         
000273     05    W-Q4-ADGANG-MAX        PIC S9(3)   VALUE 999   COMP-3.         
000274     05    W-Q4-ADPLATS-MAX       PIC S9(5)   VALUE 99999 COMP-3.         
000275     05    W-Q4-IDARTNR-MAX     PIC S9(9) VALUE 999999999 COMP-3.         
000276     05    W-Q4-IDLOPNR-MAX       PIC S9(3)   VALUE 999   COMP-3.         
000277   03  W-IDDC-B6-X.                                                       
000278       05  W-IDDC-B6           PIC X(2)    VALUE SPACE.                   
000279     EJECT                                                                
000280 01  FILLER          PIC X(8) VALUE 'HHHHHHHH'.                           
000281 01    MEDDELANDE.                                                        
000282   03    FEL1.                                                            
000283     05    FILLER                PIC X(40)   VALUE                        
000284           '749 FEL NYCKEL                          '.                    
000285     05    FILLER                PIC X(40)   VALUE                        
000286           '749 WRONG KEY                           '.                    
000287   03 FILLER                     REDEFINES FEL1.                          
000288     05    FEL-1                 PIC X(40)   OCCURS 2.                    
000289                                                                          
000290   03    FEL2.                                                            
000291     05    FILLER                PIC X(40)   VALUE                        
000292           '701 ORDERN SAKNAS                       '.                    
000293     05    FILLER                PIC X(40)   VALUE                        
000294           '701 ORDER MISSING                       '.                    
000295   03 FILLER                     REDEFINES FEL2.                          
000296     05    FEL-2                 PIC X(40)   OCCURS 2.                    
000297                                                                          
000298   03    MED11.                                                           
000299     05    FILLER                PIC X(40)   VALUE                        
000300           '    INGA FLER RADER FINNS               '.                    
000301     05    FILLER                PIC X(40)   VALUE                        
000302           '    NO MORE LINES                       '.                    
000303   03 FILLER                     REDEFINES MED11.                         
000304     05    MED-11                 PIC X(40)   OCCURS 2.                   
000305                                                                          
000306   03    MED12.                                                           
000307     05    FILLER                PIC X(40)   VALUE                        
000308           '    INGA FLER RADER FINNS               '.                    
000309     05    FILLER                PIC X(40)   VALUE                        
000310           '    NO MORE LINES                       '.                    
000311   03 FILLER                     REDEFINES MED12.                         
000312     05    MED-12                 PIC X(40)   OCCURS 2.                   
000313                                                                          
000314   03    MED13.                                                           
000315     05    FILLER                PIC X(40)   VALUE                        
000316           '    INGA FLER RADER FINNS               '.                    
000317     05    FILLER                PIC X(40)   VALUE                        
000318           '    NO MORE LINES                       '.                    
000319   03 FILLER                     REDEFINES MED13.                         
000320     05    MED-13                 PIC X(40)   OCCURS 2.                   
000321                                                                          
000322   03    MED21.                                                           
000323     05    FILLER                PIC X(40)   VALUE                        
000324           '    FLER RADER FINNS                    '.                    
000325     05    FILLER                PIC X(40)   VALUE                        
000326           '    MORE LINES                          '.                    
000327   03 FILLER                     REDEFINES MED21.                         
000328     05    MED-21                PIC X(40)   OCCURS 2.                    
000329                                                                          
000330   03    MED22.                                                           
000331     05    FILLER                PIC X(40)   VALUE                        
000332           '    FLER RADER FINNS                    '.                    
000333     05    FILLER                PIC X(40)   VALUE                        
000334           '    MORE LINES                          '.                    
000335   03 FILLER                     REDEFINES MED22.                         
000336     05    MED-22                PIC X(40)   OCCURS 2.                    
000337                                                                          
000338   03    MED23.                                                           
000339     05    FILLER                PIC X(40)   VALUE                        
000340           '    FLER RADER FINNS                    '.                    
000341     05    FILLER                PIC X(40)   VALUE                        
000342           '    MORE LINES                          '.                    
000343   03 FILLER                     REDEFINES MED23.                         
000344     05    MED-23                PIC X(40)   OCCURS 2.                    
000345     EJECT                                                                
000346                                                                          
000347******************************************************************        
000348*                                                                         
000349*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
000350*                                                                         
000351 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
000352     SKIP3                                                                
000353*01  -COPY WZ01SUB                                                        
000354     EJECT                                                                
000355 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
000356     SKIP3                                                                
000357 01  REQU-AREA.                                                           
000358*    03  -COPY WZ01REQU                                                   
000359*    03  -COPY WL0118I1                                                   
000360     EJECT                                                                
000361 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
000362     SKIP3                                                                
000363 01  RESP-AREA.                                                           
000364*    03  -COPY WZ01RESP                                                   
000365*    03  -COPY WL0118O1                                                   
000366     SKIP3                                                                
000367******************************************************************        
000368*                                                                         
000369*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
000370*                                                                         
000371 01    IMS-WS.                                                            
000372   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
000373     SKIP3                                                                
000374*                        **** STATUS-KOD FRÅN IMS                         
000375   03    STATUS-WS               PIC XX.                                  
000376     88    SEGMENT-FINNS                     VALUE '  '.                  
000377     88    SEGMENT-SAKNAS                    VALUE 'GE'.                  
000378     88    END-OF-DATA                       VALUE 'GB'.                  
000379   03    STATUS-WDE401-SEK-WS    PIC XX.                                  
000380     88    WDE401-SEK-FINNS                  VALUE '  '.                  
000381     88    WDE401-SEK-SAKNAS                 VALUE 'GE' 'GB'.             
000382     SKIP3                                                                
000383   03    GODK-STATUSKODER.                                                
000384     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
000385     SKIP3                                                                
000386 01    SSA1                      PIC X(128).                              
000387 01    SSA2                      PIC X(160).                              
000388 01    SSA3                      PIC X(96).                               
000389     EJECT                                                                
000390*                            IMS FUNKTIONSKODER                           
000391*01    -COPY W0003                                                        
000392     EJECT                                                                
000393*    ---  DLI INPUT-OUTPUT AREA                                           
000394 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
000395     SKIP2                                                                
000396 01    DLI-IO-AREA.                                                       
000397   03    IO-AREA                 PIC X(900)  VALUE SPACE.                 
000398     SKIP3                                                                
000399*  03       -COPY WDE401  -RED IO-AREA.                                   
000400     EJECT                                                                
000401*  03  ORAD-AREA   -COPY WDE411  -RED IO-AREA.                            
000402     EJECT                                                                
000403*  03  KKOLLI-AREA -COPY WDE421  -RED IO-AREA.                            
000404     EJECT                                                                
000405*  03       -COPY WDE4A1  -RED IO-AREA.                                   
000406     EJECT                                                                
000407*  03       -COPY WDK611  -RED IO-AREA.                                   
000408     EJECT                                                                
000409*  03       -COPY WDK711  -RED IO-AREA.                                   
000410     EJECT                                                                
000411*  03       -COPY WDD311  -RED IO-AREA -PRE BENA-.                        
000412     EJECT                                                                
000413*  03       -COPY WDQ301  -RED IO-AREA.                                   
000414     EJECT                                                                
000415*  03       -COPY WDQ401  -RED IO-AREA -PRE Q4-.                          
000416     EJECT                                                                
000417*  03       -COPY WDQ4A1  -RED IO-AREA.                                   
000418     EJECT                                                                
000419*  03  VORD-AREA -COPY WDE601  -RED IO-AREA.                              
000420     EJECT                                                                
000421*  03  ORAD-AREA -COPY WDE411  -PRE ARB-.                                 
000422     EJECT                                                                
000423*  03  KKOLLI-AREA -COPY WDE421  -PRE ARB-.                               
000424     EJECT                                                                
000425*  03  WDE4E-AREA -COPY WDE4E1                                            
000426     EJECT                                                                
000427 01  FILLER                      PIC X(16) VALUE 'DLI-IO-Q212'.           
000428     SKIP2                                                                
000429 01    DLI-IO-Q212.                                                       
000434*    03  -COPY WDQ212                                                     
000436 01  FILLER                      PIC X(16) VALUE 'DLI-IO-Q221'.           
000437     SKIP2                                                                
000438 01    DLI-IO-Q221.                                                       
000439*    03  -COPY WDQ221                                                     
000440     EJECT                                                                
000441 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
000442 01  DLI-IO-AREA-B601.                                                    
000443*    03  -COPY WDB601                                                     
000444     EJECT                                                                
000445 01  FILLER          PIC X(8) VALUE 'LINKAGE:'.                           
000446 LINKAGE SECTION.                                                         
000447*01    -COPY W0009     -PRE MSG-                                          
000448     SKIP3                                                                
000449*01    -COPY W0008     -PRE WDE4A-                                        
000450     05  FILLER                  PIC X.                                   
000451     SKIP3                                                                
000452*01    -COPY W0008     -PRE WDE4-                                         
000453     05  FILLER                  PIC X(30).                               
000454     SKIP3                                                                
000455*01    -COPY W0008     -PRE WDE4E-                                        
000456     05  FILLER                  PIC X.                                   
000457     EJECT                                                                
000458*01    -COPY W0008     -PRE WDE6-                                         
000459     05  FILLER                  PIC X.                                   
000460     EJECT                                                                
000461*01    -COPY W0008     -PRE ARTC-                                         
000462     05  FILLER                  PIC X.                                   
000463     EJECT                                                                
000464*01    -COPY W0008     -PRE BENA-                                         
000465     05  FILLER                  PIC X.                                   
000466     EJECT                                                                
000467*01    -COPY W0008     -PRE WDE42-                                        
000468     05  FILLER                  PIC X.                                   
000469     SKIP2                                                                
000470*01    -COPY W0008     -PRE WDE41-                                        
000471     05  FILLER                  PIC X.                                   
000472     SKIP2                                                                
000473*01    -COPY W0008     -PRE ORQG-                                         
000474     05  FILLER                  PIC X.                                   
000475     SKIP2                                                                
000476*01    -COPY W0008     -PRE ORQF-                                         
000477     05  FILLER                  PIC X.                                   
000478     SKIP2                                                                
000479*01    -COPY W0008     -PRE ORQA-                                         
000480     05  FILLER                  PIC X.                                   
000481     SKIP2                                                                
000482*01    -COPY W0008     -PRE ORQI-                                         
000483     05  FILLER                  PIC X.                                   
000484     SKIP3                                                                
000485*01    -COPY W0008     -PRE WDE4B-                                        
000486     05  FILLER                  PIC X.                                   
000487     SKIP3                                                                
000488*01    -COPY W0008     -PRE ARTS-                                         
000489     05  FILLER                  PIC X.                                   
000490*01  -COPY W0008  -PRE WDB6-                                              
000491     05  FILLER                  PIC X.                                   
000492     EJECT                                                                
000493 PROCEDURE DIVISION USING MSG-PCB                                         
000494                 WDE4A-PCB WDE4-PCB WDE4E-PCB WDE6-PCB                    
000495                 ARTC-PCB BENA-PCB WDE42-PCB WDE41-PCB                    
000496                 ORQG-PCB ORQF-PCB ORQA-PCB ORQI-PCB WDE4B-PCB            
000497                 ARTS-PCB WDB6-PCB.                                       
000498                                                                          
000499     ENTRY 'DLITCBL' USING MSG-PCB                                        
000500                 WDE4A-PCB WDE4-PCB WDE4E-PCB WDE6-PCB                    
000501                 ARTC-PCB BENA-PCB WDE42-PCB WDE41-PCB                    
000502                 ORQG-PCB ORQF-PCB ORQA-PCB ORQI-PCB WDE4B-PCB            
000503                 ARTS-PCB WDB6-PCB.                                       
000504                                                                          
000505 MAIN SECTION.                                                            
000506                                                                          
000507     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
000508     IF SUB-KDRC = 0                                                      
000509       PERFORM A-INIT-SPARA-INPUT                                         
000510       PERFORM B-VILKA-NYCKLAR-ANVANDS                                    
000511       IF KUNDORDER-AER-NYCKEL = JA                                       
000512        PERFORM C-HAMTA-VOLVOORDER                                        
000513       ELSE                                                               
000514        PERFORM D-HAMTA-KUNDORDER                                         
000515       END-IF                                                             
000516                                                                          
000517       IF NYCKLAR-OK = JA                                                 
000518                                                                          
000519          PERFORM F-PF7-TRANS                                             
000520                                                                          
000521       ELSE                                                               
000522         MOVE ALL '+'                 TO RESP-WL0118O1                    
000523         MOVE ZERO TO RESP-KVRADER                                        
000524       END-IF                                                             
000525                                                                          
000526       PERFORM S02-RETURN-RESPONSE                                        
000527     END-IF                                                               
000528                                                                          
000529     MOVE ZERO TO RETURN-CODE                                             
000530     GOBACK                                                               
000531     .                                                                    
000532     EJECT                                                                
000533 A-INIT-SPARA-INPUT SECTION.                                              
000534                                                                          
000535     MOVE 'STA A-INIT        ' TO PGM-POS                                 
000536                                                                          
000537     MOVE ALL '+'              TO RESP-WL0118O1                           
000538     MOVE 001                  TO RESP-IDMSGVER                           
000539     MOVE SPACE                TO RESP-IDMSG-ERROR                        
000540                                  RESP-IDMSG-INFO                         
000541                                  RESP-IDELMT-ERROR                       
000542                                                                          
000543     MOVE ZERO                 TO RESP-KVRADER                            
000544     PERFORM AA-SPARA-NYCKLAR                                             
000545     .                                                                    
000546     EJECT                                                                
000547 AA-SPARA-NYCKLAR        SECTION.                                         
000548     MOVE 'STA AA-SPARA-NYCKLAR' TO PGM-POS                               
000549                                                                          
000550       MOVE +2 TO SPRAK-IX                                                
000551                                                                          
000552        IF REQU-IDANSTNR-KEY = ALL '+'                                    
000553          CONTINUE                                                        
000554        ELSE                                                              
000555          MOVE REQU-IDANSTNR-KEY TO IDANSTNR-WS                           
000556          MOVE IDANSTNR-WS       TO RESP-IDANSTNR-KEY                     
000557         INSPECT RESP-IDANSTNR-KEY REPLACING LEADING ZERO BY SPACE        
000558        END-IF                                                            
000559                                                                          
000560        IF REQU-IDDISTR-KEY = ALL '+'                                     
000561          CONTINUE                                                        
000562        ELSE                                                              
000563          MOVE REQU-IDDISTR-KEY TO IDDISTR-WS                             
000564        END-IF                                                            
000565        MOVE IDDISTR-WS  TO RESP-IDDISTR-KEY                              
000566        INSPECT RESP-IDDISTR-KEY REPLACING LEADING ZERO BY SPACE          
000567                                                                          
000568        IF REQU-IDKUNDNR-KEY = ALL '+'                                    
000569          CONTINUE                                                        
000570        ELSE                                                              
000571          IF REQU-IDKUNDNR-KEY NUMERIC                                    
000572            MOVE REQU-IDKUNDNR-KEY TO IDKUNDNR-WS                         
000573            MOVE IDKUNDNR-WS TO RESP-IDKUNDNR-KEY                         
000574         INSPECT RESP-IDKUNDNR-KEY REPLACING LEADING ZERO BY SPACE        
000575          ELSE                                                            
000576            MOVE NEJ                  TO NYCKLAR-OK                       
000577            MOVE ERR-WRONG-KEY        TO RESP-IDMSG-ERROR                 
000578            MOVE 'IDKUNDNR'           TO RESP-IDELMT-ERROR                
000579          END-IF                                                          
000580        END-IF                                                            
000581                                                                          
000582        IF REQU-IDORDNR-KEY = ALL '+'                                     
000583          CONTINUE                                                        
000584        ELSE                                                              
000585          IF REQU-IDORDNR-KEY NUMERIC                                     
000586          MOVE REQU-IDORDNR-KEY TO IDKUNDRF-WS                            
000587          MOVE IDKUNDRF-WS      TO  RESP-IDORDNR-KEY                      
000588          INSPECT RESP-IDORDNR-KEY REPLACING LEADING ZERO BY SPACE        
000589          ELSE                                                            
000590            MOVE NEJ                  TO NYCKLAR-OK                       
000591            MOVE ERR-WRONG-KEY        TO RESP-IDMSG-ERROR                 
000592            MOVE 'IDORDNR'            TO RESP-IDELMT-ERROR                
000593          END-IF                                                          
000594        END-IF                                                            
000595                                                                          
000596        IF REQU-IDPURAD-KEY = ALL '+'                                     
000597          CONTINUE                                                        
000598        ELSE                                                              
000599          IF REQU-IDPURAD-KEY NUMERIC                                     
000600            MOVE REQU-IDPURAD-KEY TO RESP-IDPURAD-KEY                     
000601          INSPECT RESP-IDPURAD-KEY REPLACING LEADING ZERO BY SPACE        
000602            MOVE REQU-IDPURAD-KEY TO IDPURAD-WS                           
000603          ELSE                                                            
000604            MOVE NEJ                  TO NYCKLAR-OK                       
000605            MOVE ERR-WRONG-KEY        TO RESP-IDMSG-ERROR                 
000606            MOVE 'IDPURAD'            TO RESP-IDELMT-ERROR                
000607          END-IF                                                          
000608        END-IF                                                            
000609                                                                          
000610        IF REQU-IDPRODNR-KEY = ALL '+'                                    
000611          CONTINUE                                                        
000612        ELSE                                                              
000613          IF REQU-IDPRODNR-KEY NUMERIC                                    
000614            MOVE REQU-IDPRODNR-KEY TO IDPRODNR-WS                         
000615            MOVE IDPRODNR-WS     TO RESP-IDPRODNR-KEY                     
000616         INSPECT RESP-IDPRODNR-KEY REPLACING LEADING SPACE BY ZERO        
000617          ELSE                                                            
000618            MOVE NEJ                  TO NYCKLAR-OK                       
000619            MOVE ERR-WRONG-KEY        TO RESP-IDMSG-ERROR                 
000620            MOVE 'IDPRODNR'           TO RESP-IDELMT-ERROR                
000621          END-IF                                                          
000622        END-IF                                                            
000623                                                                          
000624        IF REQU-IDDC-KEY = ALL '+'                                        
000625          MOVE NEJ                    TO NYCKLAR-OK                       
000626          MOVE ERR-WRONG-KEY          TO RESP-IDMSG-ERROR                 
000627          MOVE 'IDDC'                 TO RESP-IDELMT-ERROR                
000628        END-IF                                                            
000629                                                                          
000630     MOVE REQU-IDDC-KEY     TO RESP-IDDC-KEY                              
000631                               W-Q4-IDDC-MIN                              
000632                               W-Q4-IDDC-MAX                              
000633                               W-Q4A1-IDDC-MIN                            
000634                               WS-IDDC                                    
000635                               W-IDDC-B6                                  
000636     .                                                                    
000637 B-VILKA-NYCKLAR-ANVANDS SECTION.                                         
000638                                                                          
000639     IF REQU-IDPRODNR-KEY NOT = ALL '+'                                   
000640       MOVE NEJ   TO KUNDORDER-AER-NYCKEL                                 
000641                                                                          
000642       IF IDPRODNR-WS NUMERIC AND IDPRODNR-WS = ZERO                      
000643         MOVE JA     TO KUNDORDER-AER-NYCKEL                              
000644       END-IF                                                             
000645     ELSE                                                                 
000646       IF  REQU-IDDISTR-KEY = ALL '+'                                     
000647       AND REQU-IDKUNDNR-KEY = ALL '+'                                    
000648       AND REQU-IDORDNR-KEY = ALL '+'                                     
000649                                                                          
000650         IF REQU-IDPRODNR-KEY NUMERIC AND REQU-IDPRODNR-KEY > ZERO        
000651           MOVE NEJ    TO KUNDORDER-AER-NYCKEL                            
000652                                                                          
000653         ELSE                                                             
000654           MOVE JA     TO KUNDORDER-AER-NYCKEL                            
000655         END-IF                                                           
000656       ELSE                                                               
000657                                                                          
000658           MOVE JA     TO KUNDORDER-AER-NYCKEL                            
000659       END-IF                                                             
000660     END-IF                                                               
000661     .                                                                    
000662     EJECT                                                                
000663 C-HAMTA-VOLVOORDER        SECTION.                                       
000664                                                                          
000665     IF IDDISTR-WS    NUMERIC AND                                         
000666        IDKUNDNR-WS   NUMERIC AND                                         
000667        IDKUNDRF-WS   NUMERIC                                             
000668       MOVE IDDISTR-WS                 TO W-E4A1-IDDISTR                  
000669       MOVE IDKUNDNR-WS                TO W-E4A1-IDKUNDNR                 
000670       MOVE IDKUNDRF-WS                TO W-E4A1-IDKUNDRF                 
000671       PERFORM IMS-GU-WDE401-ASEQ                                         
000672       IF WDE401-SEK-FINNS                                                
000673         MOVE KORD-IDORDER            TO W-IDORDER-MIN                    
000674                                         W-IDORDER-MAX                    
000675                                         W-Q4A1-IDORDER-MIN               
000676                                         W-Q4A1-IDORDER-MAX               
000677                                         W-Q4-IDORDER-MIN                 
000678                                         W-Q4-IDORDER-MAX                 
000679         MOVE KORD-IDDISTR            TO W-E401-IDDISTR                   
000680                                         W-E401-IDDISTR-MAX               
000681         MOVE KORD-IDKUNDNR           TO W-E401-IDKUNDNR                  
000682                                         W-E401-IDKUNDNR-MAX              
000683         MOVE KORD-IDORDNR5           TO W-E401-IDORDNR                   
000684         MOVE KORD-IDKUNDRF           TO W-E401-IDKUNDRF-MAX              
000685                                                                          
000686           MOVE KORD-IDPRODNR             TO W-E401-IDPRODNR              
000687                                             W-E401-IDPRODNR-MAX          
000688           MOVE KORD-IDPLKLST             TO W-E401-IDPLKLST              
000689           MOVE 999                       TO W-E401-IDPLKLST-MAX          
000690*                                                                         
000691         MOVE KORD-IDDC               TO W-IDDC                           
000692                                                                          
000693           PERFORM CA-LAS-ORAD                                            
000694                                                                          
000695           IF SEGMENT-FINNS                                               
000696               PERFORM UNTIL SEGMENT-SAKNAS OR                            
000697                             (ORAD-FLDIRLEV = NEJ AND                     
000698                              W-IDDC = REQU-IDDC-KEY)                     
000699                  PERFORM CB-LAS-NASTA-PRODNR                             
000700                  IF SEGMENT-FINNS                                        
000701                    PERFORM CA-LAS-ORAD                                   
000702                  END-IF                                                  
000703               END-PERFORM                                                
000704           END-IF                                                         
000705                                                                          
000706           IF SEGMENT-FINNS                                               
000707             MOVE JA                  TO NYCKLAR-OK                       
000708             INSPECT RESP-IDPRODNR-KEY REPLACING                          
000709                                    LEADING ZERO BY SPACE                 
000710           ELSE                                                           
000711             MOVE 'IDORDNR'        TO RESP-IDELMT-ERROR                   
000712             MOVE ERR-WRONG-KEY    TO RESP-IDMSG-ERROR                    
000713             MOVE NEJ              TO NYCKLAR-OK                          
000714           END-IF                                                         
000715      ELSE                                                                
000716       MOVE NEJ                    TO NYCKLAR-OK                          
000717       MOVE ERR-KEY-MISSING        TO RESP-IDMSG-ERROR                    
000718       MOVE 'KEY'                  TO RESP-IDELMT-ERROR                   
000719      END-IF                                                              
000720     ELSE                                                                 
000721       MOVE NEJ                    TO NYCKLAR-OK                          
000722       MOVE MUST-BE-NUMERIC        TO RESP-IDMSG-ERROR                    
000723       MOVE 'KEY'                  TO RESP-IDELMT-ERROR                   
000724     END-IF                                                               
000725     .                                                                    
000726     EJECT                                                                
000727 CA-LAS-ORAD SECTION.                                                     
000728                                                                          
000729     MOVE KORD-IDPRODNR           TO W-E401-IDPRODNR                      
000730                                     IDPRODNR-WS                          
000731     MOVE KORD-IDPLKLST           TO W-E401-IDPLKLST                      
000732     PERFORM IMS-GU-WDE401-WDE4                                           
000733     IF SEGMENT-FINNS                                                     
000734       PERFORM IMS-GNP-WDE411                                             
000735     END-IF                                                               
000736     .                                                                    
000737     SKIP2                                                                
000738 CB-LAS-NASTA-PRODNR SECTION.                                             
000739                                                                          
000740*    LÄS NÄSTA PRODNR MED FORMATETS CLAGER                                
000741*    LOOK IF THERE ARE MORE PRODUCTIONNR FOR THIS ORDER                   
000742*                                                                         
000743     PERFORM IMS-GN-WDE401-ASEQ                                           
000744*                                                                         
000745     IF SEGMENT-FINNS                                                     
000746        MOVE KORD-IDDC            TO W-IDDC                               
000747     END-IF                                                               
000748*                                                                         
000749     PERFORM UNTIL SEGMENT-SAKNAS                   OR                    
000750                   W-IDDC            = REQU-IDDC-KEY                      
000751*                                                                         
000752          PERFORM IMS-GN-WDE401-ASEQ                                      
000753          IF SEGMENT-FINNS                                                
000754             MOVE KORD-IDDC               TO W-IDDC                       
000755          END-IF                                                          
000756*                                                                         
000757     END-PERFORM                                                          
000758     .                                                                    
000759     SKIP2                                                                
000760 D-HAMTA-KUNDORDER  SECTION.                                              
000761                                                                          
000762     IF IDPRODNR-WS NUMERIC                                               
000763       PERFORM IMS-GU-WDE601                                              
000764                                                                          
000765       IF SEGMENT-FINNS                                                   
000766         MOVE VORD-IDDC            TO W-IDDC                              
000767         IF VORD-IDDC     = REQU-IDDC-KEY                                 
000768          MOVE VORD-IDPRODNR        TO W-IDPRODNR-WDE4E-MAX               
000769          MOVE VORD-IDPRODNR        TO W-IDPRODNR-WDE4E-MIN               
000770                                                                          
000771           PERFORM IMS-GU-WDE4E1                                          
000772                                                                          
000773           MOVE SEQE-IDDISTR TO TEMP-IDDISTR                              
000774                                      W-E4A1-IDDISTR                      
000775                                      W-E401-IDDISTR                      
000776                                      W-E401-IDDISTR-MAX                  
000777           MOVE TEMP-IDDISTR       TO RESP-IDDISTR-KEY                    
000778           INSPECT RESP-IDDISTR-KEY                                       
000779           REPLACING LEADING ZERO BY SPACE                                
000780           MOVE TEMP-IDDISTR       TO IDDISTR-WS                          
000781                                                                          
000782           MOVE SEQE-IDKUNDNR TO TEMP-IDKUNDNR                            
000783                                      W-E4A1-IDKUNDNR                     
000784                                      W-E401-IDKUNDNR                     
000785                                      W-E401-IDKUNDNR-MAX                 
000786           MOVE TEMP-IDKUNDNR      TO RESP-IDKUNDNR-KEY                   
000787           INSPECT RESP-IDKUNDNR-KEY                                      
000788           REPLACING LEADING ZERO BY SPACE                                
000789                                                                          
000790           MOVE SEQE-IDORDNR5     TO RESP-IDORDNR-KEY                     
000791                                      W-E4A1-IDKUNDRF                     
000792                                      W-E401-IDKUNDRF                     
000793                                      W-E401-IDKUNDRF-MAX                 
000794           INSPECT RESP-IDORDNR-KEY                                       
000795           REPLACING LEADING ZERO BY SPACE                                
000796             MOVE SEQE-IDPRODNR TO W-E401-IDPRODNR                        
000797             MOVE SEQE-IDPLKLST TO W-E401-IDPLKLST                        
000798                                                                          
000799           PERFORM IMS-GU-WDE401                                          
000800           IF SEGMENT-FINNS                                               
000801             MOVE KORD-IDORDER     TO W-IDORDER-MIN                       
000802                                      W-IDORDER-MAX                       
000803                                      W-Q4A1-IDORDER-MIN                  
000804                                      W-Q4A1-IDORDER-MAX                  
000805                                      W-Q4-IDORDER-MIN                    
000806                                      W-Q4-IDORDER-MAX                    
000807           END-IF                                                         
000808                                                                          
000809           MOVE JA  TO NYCKLAR-OK                                         
000810         ELSE                                                             
000811           MOVE NEJ TO NYCKLAR-OK                                         
000812           MOVE 'D-HAMTA-KUNDORDER2'   TO RESP-IDELMT-ERROR               
000813           MOVE ERR-WRONG-KEY          TO RESP-IDMSG-ERROR                
000814         END-IF                                                           
000815                                                                          
000816       ELSE                                                               
000817         MOVE NEJ TO NYCKLAR-OK                                           
000818         MOVE 'IDPRODN2'           TO RESP-IDELMT-ERROR                   
000819         MOVE ERR-WRONG-KEY        TO RESP-IDMSG-ERROR                    
000820       END-IF                                                             
000821                                                                          
000822     ELSE                                                                 
000823       MOVE NEJ TO NYCKLAR-OK                                             
000824       MOVE 'IDPRODN3'             TO RESP-IDELMT-ERROR                   
000825       MOVE ERR-WRONG-KEY          TO RESP-IDMSG-ERROR                    
000826     END-IF                                                               
000827     .                                                                    
000828     EJECT                                                                
000829 F-PF7-TRANS SECTION.                                                     
000830      MOVE 'STA F-PF7-TRANS'  TO PGM-POS                                  
000831                                                                          
000832      IF REQU-IDPURAD-KEY = ALL '+'                                       
000833         MOVE 'F-PF7-TRANS POS2' TO PGM-POS                               
000834         PERFORM S41-LAS-RAD-SEGM-MED-GN                                  
000835                                                                          
000836         IF SEGMENT-FINNS                                                 
000837           MOVE NEJ          TO RADER-SLUT                                
000838           MOVE 'F-PF7-TRANS POS6' TO PGM-POS                             
000839         ELSE                                                             
000840            MOVE JA          TO RADER-SLUT                                
000841            MOVE 'F-PF7-TRANS POS7' TO PGM-POS                            
000842         END-IF                                                           
000843         MOVE 1              TO ACCRAD                                    
000844         PERFORM S20-VISA-BILD                                            
000845         PERFORM S50-EV-VISA-RADER-Q4                                     
000846      ELSE                                                                
000847         MOVE 'F-PF7-TRANS POS3' TO PGM-POS                               
000848         MOVE IDPRODNR-WS         TO W-WDE4B-IDPRODNR                     
000849         MOVE IDPURAD-WS          TO W-IDPURAD WS-IDPURAD                 
000850         PERFORM S40-LAS-RAD-SEGM-MED-GU                                  
000851                                                                          
000852         IF SEGMENT-FINNS                                                 
000853            MOVE 'F-PF7-TRANS POS4' TO PGM-POS                            
000854            MOVE 1                    TO ACCRAD                           
000855            MOVE NEJ                  TO RADER-SLUT                       
000856            PERFORM S20-VISA-BILD                                         
000857            PERFORM S50-EV-VISA-RADER-Q4                                  
000858         ELSE                                                             
000859            MOVE 'F-PF7-TRANS POS5' TO PGM-POS                            
000860            MOVE JA                 TO RADER-SLUT                         
000861            MOVE +000               TO ACCRAD                             
000862         END-IF                                                           
000863      END-IF                                                              
000864       IF ACCRAD > 0                                                      
000865         COMPUTE RESP-KVRADER = ACCRAD - 1                                
000866         END-COMPUTE                                                      
000867       END-IF                                                             
000868      MOVE 'END F-PF7-TRANS'  TO PGM-POS                                  
000869     .                                                                    
000870     EJECT                                                                
000871 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
000872     MOVE 'STA S01-FETCH-REQUEST          ' TO PGM-POS                    
000873                                                                          
000874     MOVE 'GETARG'               TO SUB-KDFUNC                            
000875     MOVE 'CARPARTS.LDC.PACKINGSPECLINES'   TO SUB-ADDISPABS              
000876     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
000877                                                                          
000878     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
000879                                                                          
000880     IF SUB-KDRC > 0                                                      
000881       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
000882       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
000883       DELIMITED BY SIZE INTO FELTEXT                                     
000884       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
000885     END-IF                                                               
000886     MOVE 'END S01-FETCH-REQUEST          ' TO PGM-POS                    
000887     .                                                                    
000888     SKIP3                                                                
000889 S02-RETURN-RESPONSE SECTION.                                             
000890     MOVE 'STA S02-RETURN-RESPONSE        ' TO PGM-POS                    
000891                                                                          
000892     MOVE 'RETURN'                   TO SUB-KDFUNC                        
000893     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
000894                                                                          
000895     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
000896                                                                          
000897     IF SUB-KDRC > 0                                                      
000898       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
000899       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
000900       DELIMITED BY SIZE INTO FELTEXT                                     
000901       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
000902     END-IF                                                               
000903     MOVE 'END S02-RETURN-RESPONSE        ' TO PGM-POS                    
000904     .                                                                    
000905     EJECT                                                                
000906 S20-VISA-BILD SECTION.                                                   
000907     MOVE 'STA S20-VISA-BILD '           TO PGM-POS                       
000908                                                                          
000909     IF SEGMENT-FINNS                                                     
000910       PERFORM UNTIL ACCRAD > WS-MAX-500 OR RADER-SLUT = JA               
000911     MOVE 'S20-VISA-BILD POS2'       TO PGM-POS                           
000912         PERFORM S20A-LAS-LAGERINFO-TILL-MOD                              
000913         PERFORM S51-LAS-BENA-TILL-MOD                                    
000914         PERFORM S20C-FLYTTA-RAD-SEGM-TILL-MOD                            
000915                                                                          
000916         PERFORM IMS-GNP-WDE421                                           
000917         MOVE KKOLLI-AREA             TO ARB-KKOLLI-AREA                  
000918                                                                          
000919         IF SEGMENT-FINNS                                                 
000920           IF ARB-ORAD-KVLEVART = ARB-KKOLLI-KVLEVART                     
000921             PERFORM S20D-INGA-FLER-KOLLIN-FINNS                          
000922           ELSE                                                           
000923             PERFORM S20E-FLER-KOLLIN-FINNS                               
000924           END-IF                                                         
000925         ELSE                                                             
000926           PERFORM S20F-INGET-KOLLI-FANNS                                 
000927         END-IF                                                           
000928                                                                          
000929         PERFORM S20G-BRYT-ORSAK                                          
000930       END-PERFORM                                                        
000931                                                                          
000932       IF RADER-SLUT = JA                                                 
000933         PERFORM S20H-BRYT-ORSAK-RADER-SLUT                               
000934       END-IF                                                             
000935                                                                          
000936     ELSE                                                                 
000937       MOVE ZERO                      TO ACCRAD                           
000938     END-IF                                                               
000939     MOVE 'END S20-VISA-BILD '              TO PGM-POS                    
000940     .                                                                    
000941     EJECT                                                                
000942 S20A-LAS-LAGERINFO-TILL-MOD   SECTION.                                   
000943     MOVE 'STA S20A-LAS-LAGERINFO-'         TO PGM-POS                    
000944                                                                          
000945     MOVE ARB-ORAD-IDARTNR       TO W-IDARTNR                             
000946     MOVE REQU-IDDC-KEY          TO W-IDDC                                
000947     PERFORM IMS-GU-ARTS11                                                
000948                                                                          
000949     IF SEGMENT-FINNS                                                     
000950        MOVE SLAG-ADLAGOMR       TO RESP-ADLAGOMR (ACCRAD)                
000951        MOVE SLAG-ADGANG         TO RESP-ADGANG (ACCRAD)                  
000952        MOVE SLAG-ADPLATS        TO RESP-ADPLATS (ACCRAD)                 
000953     END-IF                                                               
000954     MOVE 'END S20A-LAS-LAGERINFO-'         TO PGM-POS                    
000955     SKIP2                                                                
000956     .                                                                    
000957 S20C-FLYTTA-RAD-SEGM-TILL-MOD    SECTION.                                
000958     MOVE 'STA S20C-FLYTTA-RAD-SEG'         TO PGM-POS                    
000959                                                                          
000960     MOVE ARB-ORAD-IDKUNDRF-RO      TO RESP-IDRONR (ACCRAD)               
000961     INSPECT RESP-IDRONR (ACCRAD)                                         
000962             REPLACING LEADING ZERO BY SPACE                              
000963     MOVE ARB-ORAD-IDPURAD     TO RESP-IDPURAD (ACCRAD)                   
000964     MOVE ARB-ORAD-IDARTNR     TO RESP-IDARTNR (ACCRAD)                   
000965                                                                          
000966     MOVE IDDISTR-WS           TO DIST08-IDDISTR                          
000967     IF DIST08-URSP-RAPP                                                  
000968       MOVE ARB-ORAD-KDARTURS  TO RESP-KDARTURS (ACCRAD)                  
000969     END-IF                                                               
000970                                                                          
000971     MOVE ARB-ORAD-KVAVBART    TO RESP-KVAVBART (ACCRAD)                  
000972     MOVE ARB-ORAD-KVLEVART    TO RESP-KVLEVART (ACCRAD)                  
000973     MOVE 'END S20C-FLYTTA-RAD-SEG'         TO PGM-POS                    
000974     EJECT                                                                
000975     .                                                                    
000976 S20D-INGA-FLER-KOLLIN-FINNS    SECTION.                                  
000977     MOVE 'STA S20D-INGA-FLER-KOLLI'        TO PGM-POS                    
000978                                                                          
000979     MOVE KKOLLI-IDKOLLI     TO TEMP-IDKOLLI                              
000980     MOVE TEMP-IDKOLLI       TO RESP-IDKOLLI (ACCRAD)                     
000981     ADD +1                  TO ACCRAD                                    
000982     MOVE JA                 TO KOLLIN-SLUT                               
000983     SKIP3                                                                
000984                                                                          
000985     .                                                                    
000986 S20E-FLER-KOLLIN-FINNS    SECTION.                                       
000987                                                                          
000988     ADD +1                  TO ACCRAD                                    
000989     MOVE NEJ                TO KOLLIN-SLUT                               
000990                                                                          
000991     PERFORM UNTIL KOLLIN-SLUT = JA OR ACCRAD > WS-MAX-500                
000992       MOVE ALL X'20'        TO RESP-BEART    (ACCRAD)                    
000993                                                                          
000994       MOVE KKOLLI-KVLEVART  TO RESP-KVLEVART (ACCRAD)                    
000995       MOVE KKOLLI-IDKOLLI   TO TEMP-IDKOLLI                              
000996       MOVE TEMP-IDKOLLI     TO RESP-IDKOLLI (ACCRAD)                     
000997       ADD +1                TO ACCRAD                                    
000998       PERFORM IMS-GNP-WDE421                                             
000999       MOVE KKOLLI-AREA             TO ARB-KKOLLI-AREA                    
001000                                                                          
001001       IF SEGMENT-FINNS                                                   
001002         MOVE NEJ            TO KOLLIN-SLUT                               
001003       ELSE                                                               
001004         MOVE JA             TO KOLLIN-SLUT                               
001005       END-IF                                                             
001006     END-PERFORM                                                          
001007                                                                          
001008     .                                                                    
001009 S20F-INGET-KOLLI-FANNS         SECTION.                                  
001010                                                                          
001011     MOVE ZERO               TO RESP-IDKOLLI (ACCRAD)                     
001012     ADD +1                  TO ACCRAD                                    
001013     MOVE JA                 TO KOLLIN-SLUT                               
001014     SKIP3                                                                
001015                                                                          
001016     .                                                                    
001017 S20G-BRYT-ORSAK                 SECTION.                                 
001018     IF KOLLIN-SLUT = JA                                                  
001019       PERFORM S20GA-BRYT-ORSAK-KOLLIN-SLUT                               
001020     ELSE                                                                 
001021       MOVE 9999999                  TO W-E401-IDPRODNR-MAX               
001022       MOVE 999                      TO W-E401-IDPLKLST-MAX               
001023       PERFORM IMS-GU-WDE401-WDE4                                         
001024       PERFORM IMS-GNP-WDE411                                             
001025       IF SEGMENT-FINNS AND ACCRAD < WS-MAX-501                           
001026         MOVE ORAD-AREA              TO ARB-ORAD-AREA                     
001027         MOVE ARB-ORAD-IDPURAD       TO IDPURAD-WS                        
001028         MOVE ORAD-IDPRODNR          TO IDPRODNR-WS                       
001029                                      W-IDPRODNR-MAX                      
001030                                      W-IDPRODNR-MIN                      
001031                                      W-E401-IDPRODNR                     
001032                                      W-E401-IDPRODNR-MAX                 
001033         MOVE 000                    TO W-E401-IDPLKLST                   
001034         MOVE 999                    TO W-E401-IDPLKLST-MAX               
001035         PERFORM IMS-GU-WDE4A1                                            
001036       ELSE                                                               
001037         PERFORM S20GB-BRYT-ORSAK-SIDA-SLUT                               
001038       END-IF                                                             
001039     END-IF                                                               
001040     EJECT                                                                
001041     .                                                                    
001042 S20GA-BRYT-ORSAK-KOLLIN-SLUT    SECTION.                                 
001043                                                                          
001044                                                                          
001045     PERFORM S41-LAS-RAD-SEGM-MED-GN                                      
001046                                                                          
001047     IF SEGMENT-FINNS                                                     
001048       MOVE NEJ            TO RADER-SLUT                                  
001049     ELSE                                                                 
001050       MOVE JA             TO RADER-SLUT                                  
001051     END-IF                                                               
001052     SKIP2                                                                
001053     .                                                                    
001054 S20GB-BRYT-ORSAK-SIDA-SLUT      SECTION.                                 
001055                                                                          
001056     MOVE NEJ                     TO RADER-SLUT                           
001057     EJECT                                                                
001058     .                                                                    
001059 S20H-BRYT-ORSAK-RADER-SLUT    SECTION.                                   
001060     MOVE 'IDORDER3'                TO RESP-IDELMT-ERROR                  
001061     MOVE INF-NO-MORE-LINES         TO RESP-IDMSG-INFO                    
001062     MOVE W-IDORDER-MIN             TO W-Q4A1-IDORDER-MIN                 
001063                                       W-Q4A1-IDORDER-MAX                 
001064                                       W-Q4-IDORDER-MIN                   
001065                                       W-Q4-IDORDER-MAX                   
001066     .                                                                    
001067 S40-LAS-RAD-SEGM-MED-GU       SECTION.                                   
001068                                                                          
001069     PERFORM IMS-GU-WDE411-BSEQ                                           
001070     IF SEGMENT-FINNS                                                     
001071       MOVE ORAD-AREA          TO ARB-ORAD-AREA                           
001072       MOVE ORAD-IDPURAD       TO WS-IDPURAD                              
001073     END-IF                                                               
001074     .                                                                    
001075     SKIP2                                                                
001076 S41-LAS-RAD-SEGM-MED-GN   SECTION.                                       
001077                                                                          
001078     MOVE LOW-VALUE              TO W-WDE4BSEQ-MIN-X                      
001079     MOVE HIGH-VALUE             TO W-WDE4BSEQ-MAX-X                      
001080     MOVE IDPRODNR-WS            TO W-IDPRODNR-MIN                        
001081     MOVE IDPRODNR-WS            TO W-IDPRODNR-MAX                        
001082     PERFORM IMS-GN-WDE411-BSEQ                                           
001083     IF SEGMENT-FINNS                                                     
001084       MOVE ORAD-AREA            TO ARB-ORAD-AREA                         
001085       MOVE ARB-ORAD-IDPURAD     TO IDPURAD-WS                            
001086                                    WS-IDPURAD                            
001087     END-IF                                                               
001088     .                                                                    
001089     EJECT                                                                
001090 S50-EV-VISA-RADER-Q4 SECTION.                                            
001091                                                                          
001092     IF ACCRAD < WS-MAX-501                                               
001093       IF KUNDORDER-AER-NYCKEL = JA                                       
001094                                                                          
001095           PERFORM IMS-GU-ORQF01                                          
001096         IF SEGMENT-FINNS                                                 
001097                                                                          
001098           IF Q4-ORAD-IDORDER = W-IDORDER-MIN                             
001099             PERFORM UNTIL ACCRAD > WS-MAX-500                            
001100                        OR SEGMENT-SAKNAS OR                              
001101               END-OF-DATA                                                
001102               PERFORM S50A-FLYTTA-Q4INFO-TILL-MOD                        
001103               MOVE Q4-ORAD-IDARTNR   TO W-IDARTNR                        
001104               PERFORM S54-LAS-LAGERINFO-TILL-MOD                         
001105               PERFORM S51-LAS-BENA-TILL-MOD                              
001106               PERFORM IMS-GN-ORQF01                                      
001107               ADD 1                 TO ACCRAD                            
001108             END-PERFORM                                                  
001109                                                                          
001110             IF SEGMENT-SAKNAS                                            
001111               MOVE INF-NO-MORE-LINES  TO RESP-IDMSG-INFO                 
001112               MOVE 'IDORDER1'        TO RESP-IDELMT-ERROR                
001113             END-IF                                                       
001114           END-IF                                                         
001115         END-IF                                                           
001116       ELSE                                                               
001117         PERFORM S50B-FLYTTA-TILL-NYCKLAR                                 
001118                                                                          
001119         PERFORM IMS-GU-ORQA01-R                                          
001127         IF SEGMENT-FINNS                                                 
001128           PERFORM IMS-GU-ORQI12                                          
001130           MOVE ODEL-IDPRC        TO W-IDPRC                              
001131           PERFORM IMS-GNP-ORQI21-PRC                                     
001132           PERFORM UNTIL SEGMENT-SAKNAS OR                                
001133                         ACCRAD > WS-MAX-500                              
001134             MOVE LOR-ADLAGOMR    TO W-Q4-ADLAGOMR-MIN                    
001135                                     W-Q4A1-ADLAGOMR-MIN                  
001136                                     W-Q4A1-ADLAGOMR-MAX                  
001137             PERFORM S50C-MOVE-Q4-D3-INFO-TO-MOD                          
001147             PERFORM IMS-GN-ORQA01-R                                      
001149             IF SEGMENT-FINNS                                             
001150               MOVE ODEL-IDPRC        TO W-IDPRC                          
001151               PERFORM IMS-GNP-ORQI21-PRC                                 
001159             ELSE                                                         
001160               SET SEGMENT-SAKNAS TO TRUE                                 
001161             END-IF                                                       
001165           END-PERFORM                                                    
001166           IF SEGMENT-SAKNAS                                              
001167             MOVE INF-NO-MORE-LINES  TO RESP-IDMSG-INFO                   
001168             MOVE 'IDORDER2'        TO RESP-IDELMT-ERROR                  
001169           END-IF                                                         
001170         END-IF                                                           
001171       END-IF                                                             
001172     END-IF                                                               
001173     .                                                                    
001174     EJECT                                                                
001175 S50A-FLYTTA-Q4INFO-TILL-MOD   SECTION.                                   
001176                                                                          
001177     MOVE Q4-ORAD-ADLAGOMR        TO RESP-ADLAGOMR (ACCRAD)               
001178     MOVE Q4-ORAD-ADGANG          TO RESP-ADGANG  (ACCRAD)                
001179     MOVE Q4-ORAD-ADPLATS         TO RESP-ADPLATS (ACCRAD)                
001180     MOVE Q4-ORAD-IDARTNR         TO RESP-IDARTNR (ACCRAD)                
001181     MOVE Q4-ORAD-IDKUNDRF-RO      TO RESP-IDRONR  (ACCRAD)               
001182     INSPECT RESP-IDRONR (ACCRAD)                                         
001183             REPLACING LEADING ZERO BY SPACE                              
001184                                                                          
001185     MOVE IDDISTR-WS              TO DIST08-IDDISTR                       
001186     IF DIST08-URSP-RAPP                                                  
001187     OR DIST08-URSP-SPX                                                   
001188       MOVE Q4-ORAD-KDARTURS      TO RESP-KDARTURS (ACCRAD)               
001189     END-IF                                                               
001190     .                                                                    
001191     SKIP2                                                                
001192 S50B-FLYTTA-TILL-NYCKLAR SECTION.                                        
001193                                                                          
001194     MOVE W-IDORDER-MIN           TO W-301-IDORDER-MIN                    
001195                                     W-301-IDORDER-MAX                    
001196                                     W-Q4-IDORDER-MIN                     
001197                                     W-Q4-IDORDER-MAX                     
001198                                     W-IDORDER                            
001199     MOVE IDPRODNR-WS             TO W-301-IDPRODNR-MIN                   
001200                                     W-301-IDPRODNR-MAX                   
001201     MOVE W-IDDC                  TO W-301-IDDC-MIN                       
001202                                     W-301-IDDC-MAX                       
001203                                     W-Q4-IDDC-MIN                        
001204                                     W-Q4-IDDC-MAX                        
001205     MOVE ZERO                    TO W-301-IDPLKLST-MIN                   
001206     MOVE 999                     TO W-301-IDPLKLST-MAX                   
001207     .                                                                    
001208     SKIP2                                                                
001209 S50C-MOVE-Q4-D3-INFO-TO-MOD SECTION.                                     
001210                                                                          
001211     PERFORM IMS-GU-ORQF01                                                
001212     IF SEGMENT-FINNS                                                     
001213       IF Q4-ORAD-IDORDER = W-Q4-IDORDER-MIN                              
001214         MOVE 'N'             TO WS-SLINGA-KLAR                           
001215                                                                          
001216         PERFORM UNTIL SEGMENT-SAKNAS OR END-OF-DATA OR                   
001217                       SLINGA-KLAR                                        
001218                                                                          
001219           IF (Q4-ORAD-ADLAGOMR = W-Q4A1-ADLAGOMR-MIN) AND                
001220               ACCRAD < WS-MAX-501                                        
001221             PERFORM S53-FLYTTA-Q4INFO-TILL-MOD                           
001222             MOVE Q4-ORAD-IDARTNR   TO W-IDARTNR                          
001223             PERFORM S54-LAS-LAGERINFO-TILL-MOD                           
001224             PERFORM S51-LAS-BENA-TILL-MOD                                
001225             PERFORM IMS-GN-ORQF01                                        
001226             ADD +1                 TO ACCRAD                             
001227           ELSE                                                           
001228             MOVE 'J'             TO WS-SLINGA-KLAR                       
001229           END-IF                                                         
001230         END-PERFORM                                                      
001231       END-IF                                                             
001232     END-IF                                                               
001233     .                                                                    
001234     SKIP2                                                                
001235 S50D-EV-REQU-TILL-NYCKL SECTION.                                         
001236     CONTINUE                                                             
001237     .                                                                    
001238     SKIP2                                                                
001239 S51-LAS-BENA-TILL-MOD SECTION.                                           
001240                                                                          
001241     PERFORM IMS-GU-WDB601                                                
001242     PERFORM IMS-BENA01-LASGU-ROTSEG                                      
001243     IF SEGMENT-FINNS                                                     
001244            MOVE DCS-IDSKYLT-DB        TO W-IDSKYLT                       
001245            IF DCS-UNICODE-IDSKYLT                                        
001246              MOVE 'UTF8'             TO TRAUTF8-KDCP                     
001247            ELSE                                                          
001248              MOVE '278 '             TO TRAUTF8-KDCP                     
001249            END-IF                                                        
001250            PERFORM IMS-BENA11-LASGNP-TEXTSEG                             
001251            IF SEGMENT-FINNS                                              
001252                MOVE BENA-TEXT-BEART TO TRAUTF8-TECONV-FROM               
001253            ELSE                                                          
001254                MOVE SPACE TO TRAUTF8-TECONV-FROM                         
001255            END-IF                                                        
001256     ELSE                                                                 
001257        MOVE SPACE TO TRAUTF8-TECONV-FROM                                 
001258     END-IF                                                               
001259     IF TRAUTF8-TECONV-FROM = SPACES                                      
001260      MOVE 'GB'  TO W-IDSKYLT                                             
001261      MOVE '278' TO TRAUTF8-KDCP                                          
001262      PERFORM IMS-BENA11-LASGNP-TEXTSEG                                   
001263      MOVE BENA-TEXT-BEART    TO TRAUTF8-TECONV-FROM                      
001264     END-IF                                                               
001265*    -- STRIP SPACE OR CONVERT TO UNICODE                                 
001266     CALL WTRAUTF8 USING TRAUTF8-AREA                                     
001267                                                                          
001268*    -- MOVE CONVERTED DESCRIPTION TO THE RESPONSE                        
001269     MOVE TRAUTF8-TECONV-TO   TO RESP-BEART (ACCRAD)                      
001270                                                                          
001271     .                                                                    
001272     EJECT                                                                
001273 S53-FLYTTA-Q4INFO-TILL-MOD   SECTION.                                    
001274                                                                          
001275     MOVE Q4-ORAD-ADLAGOMR        TO RESP-ADLAGOMR (ACCRAD)               
001276     MOVE Q4-ORAD-ADGANG          TO RESP-ADGANG  (ACCRAD)                
001277     MOVE Q4-ORAD-ADPLATS         TO RESP-ADPLATS (ACCRAD)                
001278     MOVE Q4-ORAD-IDARTNR         TO RESP-IDARTNR (ACCRAD)                
001279     MOVE Q4-ORAD-IDKUNDRF-RO      TO RESP-IDRONR  (ACCRAD)               
001280     INSPECT RESP-IDRONR (ACCRAD)                                         
001281             REPLACING LEADING ZERO BY SPACE                              
001282                                                                          
001283     MOVE IDDISTR-WS              TO DIST08-IDDISTR                       
001284     IF DIST08-URSP-RAPP                                                  
001285     OR DIST08-URSP-SPX                                                   
001286       MOVE Q4-ORAD-KDARTURS      TO RESP-KDARTURS (ACCRAD)               
001287     END-IF                                                               
001288     .                                                                    
001289     SKIP2                                                                
001290 S54-LAS-LAGERINFO-TILL-MOD SECTION.                                      
001291                                                                          
001292     MOVE REQU-IDDC-KEY         TO W-IDDC                                 
001293     PERFORM IMS-GU-ARTS11                                                
001294                                                                          
001295     IF SEGMENT-FINNS                                                     
001296        MOVE SLAG-ADLAGOMR      TO RESP-ADLAGOMR (ACCRAD)                 
001297        MOVE SLAG-ADGANG        TO RESP-ADGANG (ACCRAD)                   
001298        MOVE SLAG-ADPLATS       TO RESP-ADPLATS (ACCRAD)                  
001299     END-IF                                                               
001300     .                                                                    
001301     SKIP2                                                                
001302 IMS-GU-WDE401-WDE4 SECTION.                                              
001303                                                                          
001304     STRING 'WDE401  (WDE401KY =' W-WDE4KEY-X ')'                         
001305                    DELIMITED BY SIZE INTO SSA1                           
001306     MOVE '  GE' TO GODK-STATUSKODER                                      
001307     CALL CBLTDLI USING GU WDE4-PCB DLI-IO-AREA SSA1                      
001308     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
001309     PERFORM IMS-STATUSKONTROLL                                           
001310     .                                                                    
001311 IMS-GNP-WDE411 SECTION.                                                  
001312                                                                          
001313     STRING 'WDE411  (IDPURAD  >' W-IDPURAD-X ')'                         
001314                    DELIMITED BY SIZE INTO SSA1                           
001315     MOVE '  GE' TO GODK-STATUSKODER                                      
001316     CALL CBLTDLI USING GNP WDE4-PCB DLI-IO-AREA SSA1                     
001317     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
001318     PERFORM IMS-STATUSKONTROLL                                           
001319     SKIP2                                                                
001320     .                                                                    
001321 IMS-GU-WDE411-BSEQ SECTION.                                              
001322                                                                          
001323     MOVE '  GE' TO GODK-STATUSKODER                                      
001324     STRING 'WDE411  (WDE4BSEQ =' W-WDE4BSEQ-X ')'                        
001325            DELIMITED BY SIZE INTO SSA1                                   
001326     CALL CBLTDLI USING GU WDE4B-PCB DLI-IO-AREA SSA1                     
001327     MOVE WDE4B-STATUS-CODE TO STATUS-WS                                  
001328     PERFORM IMS-STATUSKONTROLL                                           
001329     .                                                                    
001330 IMS-GN-WDE411-BSEQ          SECTION.                                     
001331                                                                          
001332     MOVE '  GEGB' TO GODK-STATUSKODER                                    
001333     STRING 'WDE411  (WDE4BSEQ>=' W-WDE4BSEQ-MIN-X                        
001334                    '&WDE4BSEQ<=' W-WDE4BSEQ-MAX-X ')'                    
001335                    DELIMITED BY SIZE INTO SSA1                           
001336     CALL CBLTDLI USING GN WDE4B-PCB DLI-IO-AREA SSA1                     
001337     MOVE WDE4B-STATUS-CODE TO STATUS-WS                                  
001338     PERFORM IMS-STATUSKONTROLL                                           
001339     EJECT                                                                
001340     .                                                                    
001341 IMS-GNP-WDE421              SECTION.                                     
001342                                                                          
001343     STRING 'WDE421   ' DELIMITED BY SIZE INTO SSA1                       
001344     MOVE '  GE' TO GODK-STATUSKODER                                      
001345     CALL CBLTDLI USING GNP WDE4B-PCB DLI-IO-AREA SSA1                    
001346     MOVE WDE4B-STATUS-CODE TO STATUS-WS                                  
001347     PERFORM IMS-STATUSKONTROLL                                           
001348     EJECT                                                                
001349     .                                                                    
001350 IMS-GU-ARTS11 SECTION.                                                   
001351     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
001352          DELIMITED BY SIZE INTO SSA1                                     
001353     STRING 'WLARTS11(IDDC     =' W-IDDC-X ')'                            
001354          DELIMITED BY SIZE INTO SSA2                                     
001355     MOVE '  GE' TO GODK-STATUSKODER                                      
001356     CALL CBLTDLI USING GU ARTS-PCB DLI-IO-AREA SSA1 SSA2                 
001357     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
001358     PERFORM IMS-STATUSKONTROLL                                           
001359     .                                                                    
001360     SKIP3                                                                
001361 IMS-BENA01-LASGU-ROTSEG SECTION.                                         
001362     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
001363             DELIMITED BY SIZE INTO SSA1                                  
001364     MOVE '  GE' TO GODK-STATUSKODER                                      
001365     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA SSA1                      
001366     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
001367     PERFORM IMS-STATUSKONTROLL                                           
001368     SKIP2                                                                
001369     .                                                                    
001370 IMS-BENA11-LASGNP-TEXTSEG SECTION.                                       
001371     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
001372             DELIMITED BY SIZE INTO SSA1                                  
001373     MOVE '  GE' TO GODK-STATUSKODER                                      
001374     CALL CBLTDLI USING GNP BENA-PCB DLI-IO-AREA SSA1                     
001375     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
001376     PERFORM IMS-STATUSKONTROLL                                           
001377     EJECT                                                                
001378                                                                          
001379     .                                                                    
001380 IMS-GU-WDE601 SECTION.                                                   
001381                                                                          
001382     MOVE IDPRODNR-WS     TO W-IDPRODNR                                   
001383     MOVE '  GE' TO GODK-STATUSKODER                                      
001384     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
001385            DELIMITED BY SIZE INTO SSA1                                   
001386     CALL CBLTDLI USING GU WDE6-PCB DLI-IO-AREA SSA1                      
001387     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
001388     PERFORM IMS-STATUSKONTROLL                                           
001389     SKIP3                                                                
001390     .                                                                    
001391 IMS-GU-WDE4E1 SECTION.                                                   
001392                                                                          
001393     STRING 'WDE4E1  (WDE4E1KY=>' W-WDE4E1KY-MIN-X ')'                    
001394                    '&WDE4E1KY=<' W-WDE4E1KY-MAX-X ')'                    
001395            DELIMITED BY SIZE INTO SSA1                                   
001396     MOVE '    ' TO GODK-STATUSKODER                                      
001397     CALL CBLTDLI USING GU WDE4E-PCB WDE4E-AREA SSA1                      
001398     MOVE WDE4E-STATUS-CODE TO STATUS-WS                                  
001399     PERFORM IMS-STATUSKONTROLL                                           
001400     EJECT                                                                
001401     .                                                                    
001402 IMS-GU-WDE401 SECTION.                                                   
001403                                                                          
001404     MOVE '    ' TO GODK-STATUSKODER                                      
001405     STRING 'WDE401  (WDE401KY =' W-WDE4KEY-X ')'                         
001406            DELIMITED BY SIZE INTO SSA1                                   
001407     CALL CBLTDLI USING GU WDE42-PCB DLI-IO-AREA SSA1                     
001408     MOVE WDE42-STATUS-CODE TO STATUS-WS                                  
001409     PERFORM IMS-STATUSKONTROLL                                           
001410     SKIP3                                                                
001411                                                                          
001412     .                                                                    
001413 IMS-GU-WDE4A1 SECTION.                                                   
001414                                                                          
001415     STRING 'WDE4A1  (WDE4A1KY=>' W-WDE4KEY-X                             
001416                    '&WDE4A1KY=<' W-WDE4KEY-MAX-X ')'                     
001417            DELIMITED BY SIZE INTO SSA1                                   
001418     MOVE '  GEGB' TO GODK-STATUSKODER                                    
001419     CALL CBLTDLI USING GU WDE4A-PCB DLI-IO-AREA SSA1                     
001420     MOVE WDE4A-STATUS-CODE TO STATUS-WS                                  
001421                               STATUS-WDE401-SEK-WS                       
001422     PERFORM IMS-STATUSKONTROLL                                           
001423     SKIP3                                                                
001424     .                                                                    
001425 IMS-GU-WDE401-ASEQ SECTION.                                              
001426                                                                          
001427     STRING 'WDE401  (WDE4ASEQ =' W-WDE4AKEY-X ')'                        
001428            DELIMITED BY SIZE INTO SSA1                                   
001429     MOVE '  GEGB' TO GODK-STATUSKODER                                    
001430     CALL CBLTDLI USING GU WDE41-PCB DLI-IO-AREA SSA1                     
001431     MOVE WDE41-STATUS-CODE TO STATUS-WS                                  
001432                               STATUS-WDE401-SEK-WS                       
001433     PERFORM IMS-STATUSKONTROLL                                           
001434     SKIP3                                                                
001435     .                                                                    
001436 IMS-GN-WDE401-ASEQ SECTION.                                              
001437                                                                          
001438     STRING 'WDE401  (WDE4ASEQ =' W-WDE4AKEY-X ')'                        
001439            DELIMITED BY SIZE INTO SSA1                                   
001440     MOVE '  GEGB' TO GODK-STATUSKODER                                    
001441     CALL CBLTDLI USING GN WDE41-PCB DLI-IO-AREA SSA1                     
001442     MOVE WDE41-STATUS-CODE TO STATUS-WS                                  
001443                               STATUS-WDE401-SEK-WS                       
001444     PERFORM IMS-STATUSKONTROLL                                           
001445     SKIP3                                                                
001446     .                                                                    
001447     SKIP2                                                                
001448 IMS-GU-ORQF01 SECTION.                                                   
001449                                                                          
001450     STRING 'WLORQF01(WDQ401KY=>' W-WDQ401KY-MIN-X                        
001451                    '&WDQ401KY=<' W-WDQ401KY-MAX-X ')'                    
001452            DELIMITED BY SIZE INTO SSA1                                   
001453     MOVE '  GBGE' TO GODK-STATUSKODER                                    
001454     CALL CBLTDLI USING GU ORQF-PCB DLI-IO-AREA SSA1                      
001455     MOVE ORQF-STATUS-CODE      TO STATUS-WS                              
001456     PERFORM IMS-STATUSKONTROLL                                           
001457     .                                                                    
001458     SKIP2                                                                
001459 IMS-GN-ORQF01 SECTION.                                                   
001460                                                                          
001461     STRING 'WLORQF01(WDQ401KY=>' W-WDQ401KY-MIN-X                        
001462                    '&WDQ401KY=<' W-WDQ401KY-MAX-X ')'                    
001463            DELIMITED BY SIZE INTO SSA1                                   
001464     MOVE '  GBGE' TO GODK-STATUSKODER                                    
001465     CALL CBLTDLI USING GN ORQF-PCB DLI-IO-AREA SSA1                      
001466     MOVE ORQF-STATUS-CODE      TO STATUS-WS                              
001467     PERFORM IMS-STATUSKONTROLL                                           
001468     .                                                                    
001469     SKIP2                                                                
001470 IMS-GU-ORQA01-R SECTION.                                                 
001471                                                                          
001472     STRING 'WLORQA01(WDQ301KY=>' W-WDQ301KY-MIN-X                        
001473                    '&WDQ301KY=<' W-WDQ301KY-MAX-X                        
001474                    '&KDODELST =' W-KDODELST-X ')'                        
001475            DELIMITED BY SIZE INTO SSA1                                   
001476     MOVE '  GE' TO GODK-STATUSKODER                                      
001477     CALL CBLTDLI USING GU ORQA-PCB DLI-IO-AREA SSA1                      
001478     MOVE ORQA-STATUS-CODE     TO STATUS-WS                               
001479     PERFORM IMS-STATUSKONTROLL                                           
001480     .                                                                    
001481 IMS-GN-ORQA01-R SECTION.                                                 
001482                                                                          
001483     STRING 'WLORQA01(WDQ301KY=>' W-WDQ301KY-MIN-X                        
001484                    '&WDQ301KY=<' W-WDQ301KY-MAX-X ')'                    
001485                    '&KDODELST =' W-KDODELST-X ')'                        
001486            DELIMITED BY SIZE INTO SSA1                                   
001487     MOVE '  GEGB' TO GODK-STATUSKODER                                    
001488     CALL CBLTDLI USING GN ORQA-PCB DLI-IO-AREA SSA1                      
001489     MOVE ORQA-STATUS-CODE     TO STATUS-WS                               
001490     PERFORM IMS-STATUSKONTROLL                                           
001491     .                                                                    
001504 IMS-GU-ORQI12 SECTION.                                                   
001505                                                                          
001506     STRING 'WLORQI01(IDORDER  =' W-IDORDER-X ')'                         
001507            DELIMITED BY SIZE INTO SSA1                                   
001508     STRING 'WLORQI12(IDDC     =' W-IDDC-X ')'                            
001509            DELIMITED BY SIZE INTO SSA2                                   
001510     MOVE '  GE' TO GODK-STATUSKODER                                      
001511     CALL CBLTDLI USING GU ORQI-PCB DLI-IO-Q212 SSA1 SSA2                 
001512     MOVE ORQI-STATUS-CODE     TO STATUS-WS                               
001513     PERFORM IMS-STATUSKONTROLL                                           
001514     .                                                                    
001515 IMS-GNP-ORQI21-PRC SECTION.                                              
001516                                                                          
001517     STRING 'WLORQI21*F(IDPRC    =' W-IDPRC-X ')'                         
001518            DELIMITED BY SIZE INTO SSA1                                   
001519     MOVE '  GE' TO GODK-STATUSKODER                                      
001520     CALL CBLTDLI USING GNP ORQI-PCB DLI-IO-Q221 SSA1                     
001521     MOVE ORQI-STATUS-CODE     TO STATUS-WS                               
001522     PERFORM IMS-STATUSKONTROLL                                           
001523     .                                                                    
001524 IMS-GU-WDB601 SECTION.                                                   
001525                                                                          
001526     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
001527          DELIMITED BY SIZE INTO SSA1                                     
001528     MOVE '  ' TO GODK-STATUSKODER                                        
001529     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
001530     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
001531     PERFORM IMS-STATUSKONTROLL                                           
001532     .                                                                    
001533     SKIP3                                                                
001534 IMS-STATUSKONTROLL SECTION.                                              
001535     SET STATUS-IX TO 1                                                   
001536     SEARCH GODK-STATUS AT END CALL FELLOG                                
001537       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
001538     END-SEARCH                                                           
001539     CONTINUE                                                             
001540     .                                                                    
