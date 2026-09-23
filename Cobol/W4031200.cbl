000001*                                                                         
000002******************************************************************        
000003*     THIS PROGRAM ALSO HAS A WEB-LDC VERSION CALLED WL0120      *        
000004******************************************************************        
000005*                                                                         
000006 ID DIVISION.                                                             
000007     SKIP2                                                                
000008 PROGRAM-ID.     W4031200.                                                
000009 AUTHOR.         M LUNDBERG.                                              
000010     DATE-WRITTEN.   NOV  1985.                                           
000011                                                                          
000012     REMARKS.                                                             
000013                                                                          
000014     FUNKTION.                                                            
000015         ANNULLERA/DELA ORDER FRÅGA/UPPDATERA.                            
000016                                                                          
000017     INDATA.                                                              
000018         TRANSAKTION: W4T312                                              
000019         MID:         W4I31201                                            
000020                                                                          
000021     UTDATA.                                                              
000022         MOD:         W4O31201                                            
000023*    CHANGE LOG                                                           
000024*                                                                         
000025*    DIGAMBAR/021011                                                      
000026*    STRUCTURE OF ACTION TRANSACTION 4487 IS CHANGED TO IMPROVE           
000027*    THE RESPONSE TIME OF THE SCREEN 4312.                                
000028*                                                                         
000029     SKIP3                                                                
000030 ENVIRONMENT DIVISION.                                                    
000031     SKIP3                                                                
000032 DATA DIVISION.                                                           
000033     EJECT                                                                
000034 WORKING-STORAGE SECTION.                                                 
000035                                                                          
000036*    -- CHECKED BY WY2000                                                 
000037 77    PROGRAM-NAMN              VALUE 'W4031200'                         
000038                                 PIC X(8).                                
000039 77    PGM-POS                   PIC X(16)   VALUE SPACE.                 
000040 77    JA                        PIC X       VALUE 'J'.                   
000041 77    NEJ                       PIC X       VALUE 'N'.                   
000042 77    FL-SAMTL-PLKLST-HAR-USER-NOLL PIC X.                               
000043 77    BORTTAG                   PIC X       VALUE 'B'.                   
000044 77    FILLER                    PIC X(8)    VALUE 'AAAAAAAA'.            
000045 77    INDX                      PIC S9(9)   VALUE +0   COMP SYNC.        
000046 77    SPRAK-INDX                PIC S9(9)   VALUE +0   COMP SYNC.        
000047 77    MID-RAD-IND               PIC S9(9)   VALUE +0   COMP SYNC.        
000048 77    MOD-RAD-IND               PIC S9(9)   VALUE +0   COMP SYNC.        
000049 77    WS-RAD-IND                PIC  9(9)   VALUE  0.                    
000050 77    MAX-LINE                  PIC S9(9)   VALUE +13  COMP SYNC.        
000051 77    MAX-MOD-LAENGD            PIC S9(4)  VALUE +920  COMP SYNC.        
000052 77    FILLER                    PIC X(8)    VALUE 'BBBBBBBB'.            
000053 77    DAGENS-DATUM              PIC 9(6).                                
000054 77    WS-JFR-IDPRODNR           PIC S9(7)              COMP-3.           
000055 77    WS-4487-IDDC              PIC X(2).                                
000056*                                                                         
000057*      --- VALID IDDD CODES                                               
000058*                                                                         
000059*01    -COPY WWDC99                                                       
000060       EJECT                                                              
000070*                                                                         
000071 77    INDATA-SW                 PIC  X(1).                               
000072    88 INDATA-OK                 VALUE 'J'.                               
000073    88 INDATA-FEL                VALUE 'N'.                               
000074     SKIP2                                                                
000075*                                                                         
000076 77    WS-NY-RADNR-FROM          PIC 9(5)    VALUE ZERO COMP-3.           
000077 77    WS-NY-RADNR-TOM           PIC 9(5)    VALUE ZERO COMP-3.           
000078 77    WS-KDFEL                  PIC 9(2)    VALUE ZERO COMP-3.           
000079     SKIP2                                                                
000080 01    TESTMED.                                                           
000081       03  FILLER                PIC X(5) VALUE 'USER='.                  
000082       03  TEST-1                PIC X(5).                                
000083       03  FILLER                PIC X(5) VALUE '4488='.                  
000084       03  TEST-2                PIC 9(11).                               
000085       03  TEST-3                PIC 9(07).                               
000086       03  TEST-4                PIC 9(03).                               
000087 77    FILLER                    PIC X(8)    VALUE 'CCCCCCCC'.            
000088 01    WS-IDANSTNR                           PIC X(5).                    
000089 01    IDANSTNR-WS REDEFINES WS-IDANSTNR     PIC 9(5).                    
000090 01    WS-IDDISTR                            PIC X(4).                    
000091 01    IDDISTR-WS REDEFINES WS-IDDISTR       PIC 9(4).                    
000092 01    WS-IDKUNDNR                           PIC X(6).                    
000093 01    IDKUNDNR-WS REDEFINES WS-IDKUNDNR     PIC 9(6).                    
000094 01    WS-IDORDNR                            PIC X(5).                    
000095 01    IDORDNR-WS REDEFINES WS-IDORDNR       PIC 9(5).                    
000096 01    WS-IDPRODNR                           PIC X(7).                    
000097 01    IDPRODNR-WS REDEFINES WS-IDPRODNR     PIC 9(7).                    
000098     SKIP2                                                                
000099 77    FILLER                    PIC X(8)    VALUE 'DDDDDDDD'.            
000100 77    WS-SPARA-IDDISTR          PIC 9(4)    VALUE ZERO COMP-3.           
000101 77    WS-SPARA-IDKUNDNR         PIC 9(6)    VALUE ZERO COMP-3.           
000102 77    WS-SPARA-IDORDER          PIC 9(7)    VALUE ZERO COMP-3.           
000103 77    WS-SPARA-IDORDNR          PIC 9(5)    VALUE ZERO COMP-3.           
000104 77    WS-SPARA-IDPRODNR         PIC 9(7)    VALUE ZERO.                  
000105 77    WS-SPARA-IDPLKLST         PIC 9(3)    VALUE ZERO.                  
000106 77    WS-SPARA-IDPLKLST-2POS    PIC 9(2).                                
000107 77    WS-SPAR-KVORDRAD-LEVPL    PIC S9(5)   VALUE ZERO COMP-3.           
000108     EJECT                                                                
000109 77    FILLER                    PIC X(8)    VALUE 'EEEEEEEE'.            
000110 01    WS-NYCKELFALT.                                                     
000111   03  WS-DARFS-NYCKEL           PIC 9(12).                               
000112   03  WS-MID-KDPRCGRP-NEXT      PIC X(5).                                
000113   03  WS-MID-IDPRODNR-NEXT      PIC S9(7)   COMP-3.                      
000114   03  WS-MID-IDPLKLST-NEXT      PIC S9(3)   COMP-3.                      
000115   03  WS-MID-KDPRCGRP-ENTER     PIC X(5).                                
000116   03  WS-MID-IDPRODNR-ENTER     PIC S9(7)   COMP-3.                      
000117   03  WS-MID-IDPLKLST-ENTER     PIC S9(3)   COMP-3.                      
000118     SKIP2                                                                
000119 01    WS-SPARA-IDKUNDRF.                                                 
000120   03  WS-SPARA-IDORDNR-ALFA     PIC X(5).                                
000121   03  FILLER                    PIC X(5)    VALUE SPACE.                 
000122     SKIP2                                                                
000123 01    WS-START-STOPP-IDPURAD.                                            
000124   03  WS-START-IDPURAD          PIC S9(5)   VALUE ZERO COMP-3.           
000125   03  WS-STOPP-IDPURAD          PIC S9(5)   VALUE ZERO COMP-3.           
000126     SKIP2                                                                
000127 01    WS-JFR-IDANSTNR.                                                   
000128   03  FILLER                    PIC X(3).                                
000129   03  WS-JFR-IDANSTNR-5         PIC X(5).                                
000130     EJECT                                                                
000131 77    FILLER                    PIC X(8)    VALUE 'FFFFFFFF'.            
000132 77    WS-KVRADER                PIC 9(5)    VALUE ZERO COMP-3.           
000133 77    WS-SUM-KVRADER            PIC 9(7)    VALUE ZERO COMP-3.           
000134 77    WS-KVORDRAD-KVAR          PIC 9(7)    VALUE ZERO COMP-3.           
000135 77    WS-DATUM                  PIC 9(6)    VALUE ZERO COMP-3.           
000136 77    WS-TID                    PIC 9(6)    VALUE ZERO COMP-3.           
000137 77    WS-ANST-KVORDRAD-KVAR     PIC 9(5)    VALUE ZERO COMP-3.           
000138     EJECT                                                                
000139*      - - - - - - - - - - - - - *****    SWITCHAR                        
000140 77    FILLER                    PIC X(8)    VALUE 'GGGGGGGG'.            
000141 77    SW-AENDRA                 PIC X       VALUE 'N'.                   
000142 77    SW-ORDERID                PIC X       VALUE 'N'.                   
000143 77    SW-IDPRODNR               PIC X       VALUE 'N'.                   
000144 77    SW-IDANSTNR               PIC X       VALUE 'N'.                   
000145     EJECT                                                                
000146 01    DYNAMISKA-SUBPROGRAM.                                              
000147   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
000148   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
000149   03  W005INIT                  PIC X(8)    VALUE 'W005INIT'.            
000150     EJECT                                                                
000151*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
000152*01 -COPY WMSGINIT                                                        
000153     SKIP2                                                                
000154*    --- AREAS FOR MFS AND SCREEN MANAGEMENT                              
000155*                                                                         
000156     SKIP3                                                                
000157 01  FILLER                      PIC X(16)   VALUE 'P-TO-P-SW'.           
000158 01  P-TO-P-SW.                                                           
000159     03  PTOP-LL                 PIC S9(4)   VALUE 0 COMP SYNC.           
000160     03  PTOP-Z1                 PIC  X(1)   VALUE LOW-VALUE.             
000161     03  PTOP-Z2                 PIC  X(1)   VALUE LOW-VALUE.             
000162     03  PTOP-TRANSKOD           PIC  X(7)   VALUE 'W4T324 '.             
000163     03  FILLER                  PIC  X(1)   VALUE SPACE.                 
000164     03  FILLER                  PIC  X(4)   VALUE '4312'.                
000165     03  PTOP-KDMFSFOR           PIC  X(1).                               
000166*    03  -COPY W4I32401   -PRE PTOP-                                      
000167     SKIP3                                                                
000168*                                                                         
000169*    --- AREA CONTAINING DATA TO BE SAVED BETWEEN DIALOG STEPS            
000170 01  SAVE-AREA.                                                           
000171     03  SAVE-IDTRANS           PIC X(4)     VALUE '4312'.                
000172     03  SAVE-IDPRODNR-ENTER    PIC S9(7)    VALUE ZERO  COMP-3.          
000173     03  SAVE-IDPRODNR-NEXT     PIC S9(7)    VALUE ZERO  COMP-3.          
000174     03  SAVE-IDPLKLST-ENTER    PIC S9(3)    VALUE ZERO  COMP-3.          
000175     03  SAVE-IDPLKLST-NEXT     PIC S9(3)    VALUE ZERO  COMP-3.          
000176     03  SAVE-KDPRCGRP-ENTER    PIC  X(5).                                
000177     03  SAVE-KDPRCGRP-NEXT     PIC  X(5).                                
000178     03  SAVE-TIRFS-ENTER       PIC 9(12).                                
000179     03  SAVE-TIRFS-NEXT        PIC 9(12).                                
000180*                                                                         
000181     SKIP2                                                                
000182 01    NYCKEL-KOMBINATION        PIC S9(2)   COMP-3.                      
000183   88  ENDAST-ANSTNR             VALUE +1.                                
000184   88  ANSTNR-ORDERID            VALUE +2.                                
000185   88  ENDAST-ORDERID            VALUE +3.                                
000186     SKIP2                                                                
000187 01    WS-TRAEFF-PACKARE         PIC X(1).                                
000188   88  TRAEFF-PACKARE            VALUE 'J'.                               
000189     SKIP2                                                                
000190 01    NYCKEL-TYP                PIC S9(2)   COMP-3.                      
000191   88  GAMLA-NYCKLAR             VALUE +1.                                
000192   88  NYA-NYCKLAR               VALUE +2.                                
000193     SKIP2                                                                
000194 01    WS-ORDERDEL-KLAR          PIC X(1).                                
000195   88  ORDERDEL-KLAR             VALUE 'J'.                               
000196     SKIP2                                                                
000197 01    WS-ORDERDEL-STARTAD       PIC X(1).                                
000198   88  ORDERDEL-STARTAD          VALUE 'J'.                               
000199     SKIP2                                                                
000200 01    WS-ORDERDEL-STATUS        PIC X(1).                                
000201   88  ORDERDEL-STATUS-PACKAD    VALUE 'J'.                               
000202     SKIP2                                                                
000203 01    WS-VISA-RAD               PIC X(1).                                
000204   88  VISA-EJ-RAD-PA-BILD       VALUE 'N'.                               
000205   88  VISA-RAD-PA-BILD          VALUE 'J'.                               
000206     SKIP2                                                                
000207 01    WS-SLINGA-KLAR            PIC X(1).                                
000208   88  SLINGA-KLAR               VALUE 'J'.                               
000209     SKIP2                                                                
000210 01    FRAN-BILD                 PIC 9(4).                                
000211   88  FRAN-BILD-OK              VALUE 4312 4324.                         
000212   88  FRAN-EGEN-BILD            VALUE 4312.                              
000213   88  FRAN-4324-BILD            VALUE 4324.                              
000214                                                                          
000215     EJECT                                                                
000216 01    FILLER                    PIC X(8)    VALUE 'HHHHHHHH'.            
000217 01    NYCKLAR-TILL-DLI.                                                  
000218   03    W-WDE4A1-KUNDORDER-X.                                            
000219     05    W-4A1-IDDISTR         PIC S9(5)            COMP-3.             
000220     05    W-4A1-IDKUNDNR        PIC S9(7)            COMP-3.             
000221     05    W-4A1-IDKUNDRF.                                                
000222       07  W-4A1-IDORDNR         PIC X(5).                                
000223       07  FILLER                PIC X(5)        VALUE SPACE.             
000224     SKIP2                                                                
000225   03    W-WDE401-KUNDORDER-X.                                            
000226     05    W-401-IDDISTR         PIC S9(5)            COMP-3.             
000227     05    W-401-IDKUNDNR        PIC S9(7)            COMP-3.             
000228     05    W-401-IDKUNDRF.                                                
000229       07  W-401-IDORDNR         PIC X(5).                                
000230       07  FILLER                PIC X(5)        VALUE SPACE.             
000231     05    W-401-IDPRODNR        PIC S9(7)            COMP-3.             
000232     05    W-401-IDPLKLST        PIC S9(3)            COMP-3.             
000233     SKIP2                                                                
000234   03    W-WDE411-KEYSEQ-MIN-X.                                           
000235     05    W-411-IDPRODNR-MIN    PIC S9(7)            COMP-3.             
000236     05    W-411-IDPURAD-MIN     PIC S9(5)            COMP-3.             
000237     SKIP2                                                                
000238   03    W-WDE411-KEYSEQ-MAX-X.                                           
000239     05    W-411-IDPRODNR-MAX    PIC S9(7)            COMP-3.             
000240     05    W-411-IDPURAD-MAX     PIC S9(5)            COMP-3.             
000241     SKIP2                                                                
000242   03    W-WDE601-VOLVOORDER-X.                                           
000243     05    W-601-IDPRODNR        PIC S9(7) COMP-3.                        
000244     SKIP2                                                                
000245   03    W-4305-X.                                                        
000246     05    FILLER                PIC X(4)  VALUE '4305'.                  
000247     05    W-4305-IDDC           PIC X(2).                                
000248     05    FILLER                PIC X(24) VALUE LOW-VALUE.               
000249     SKIP2                                                                
000250   03    W-4306-X.                                                        
000251     05    W-4306-IDPRODNR       PIC S9(7) COMP-3.                        
000252     05    FILLER                PIC X(6)  VALUE LOW-VALUE.               
000253     SKIP2                                                                
000254   03    W-4447-X.                                                        
000255     05    W-4447-IDHTYP         PIC X(4)   VALUE '4447'.                 
000256     05    W-4447-IDDC           PIC X(2).                                
000257     05    W-4447-LOW-VALUE      PIC X(24)  VALUE LOW-VALUE.              
000258     SKIP2                                                                
000259   03    W-4448-X.                                                        
000260     05    W-4448-IDPRC          PIC X(4).                                
000261     05    FILLER                PIC X(1)   VALUE LOW-VALUE.              
000262     SKIP2                                                                
000263   03    W-4487-X.                                                        
000264     05    W-4487-IDHTYP         PIC X(4)   VALUE '4487'.                 
000265     05    W-4487-IDDC           PIC X(2).                                
000266     05    W-4487-LOW-VALUE      PIC X(24)  VALUE LOW-VALUE.              
000267     SKIP2                                                                
000268   03    W-4488-X.                                                        
000269     05    W-4488-KDPRCGRP       PIC X(5).                                
000270     SKIP2                                                                
000271   03    W-4490-X.                                                        
000272     05    W-4490-DARFS          PIC 9(12).                               
000273     05    W-4490-IDPRODNR       PIC S9(7)  COMP-3.                       
000274     05    W-4490-IDPLKLST       PIC S9(3)  COMP-3.                       
000275     SKIP2                                                                
000276   03  W-WDQ301KY-X.                                                      
000277     05  W-IDORDER-WDQ3          PIC S9(7)   VALUE ZERO COMP-3.           
000278     05  W-IDDC-WDQ3             PIC X(2).                                
000279     05  W-IDPRODNR-WDQ3         PIC S9(7)   VALUE ZERO COMP-3.           
000280     05  W-IDPLKLST-WDQ3         PIC S9(3)   VALUE ZERO COMP-3.           
000281                                                                          
000282   03  W-WDQ301KY-MIN-X.                                                  
000283     05  W-IDORDER-WDQ3-MIN      PIC S9(7)   VALUE ZERO COMP-3.           
000284     05  W-IDDC-WDQ3-MIN         PIC X(2).                                
000285     05  W-IDPRODNR-WDQ3-MIN     PIC S9(7)   VALUE ZERO COMP-3.           
000286     05  FILLER                  PIC  X(2)   VALUE LOW-VALUE.             
000287                                                                          
000288   03  W-WDQ301KY-MAX-X.                                                  
000289     05  W-IDORDER-WDQ3-MAX      PIC S9(7)   VALUE ZERO COMP-3.           
000290     05  W-IDDC-WDQ3-MAX         PIC X(2).                                
000291     05  W-IDPRODNR-WDQ3-MAX     PIC S9(7)   VALUE ZERO COMP-3.           
000292     05  FILLER                  PIC  X(2)   VALUE HIGH-VALUE.            
000293                                                                          
000294   03  W-IDDC-B6-X.                                                       
000295     05 W-IDDC-B6                PIC X(2).                                
000296     EJECT                                                                
000297 01    MEDDELANDE.                                                        
000298   03    FEL1.                                                            
000299     05    FILLER                PIC X(40)   VALUE                        
000300             '708. ORDERN SAKNAS ELLER KLAR          '.                   
000301     05    FILLER                PIC X(40)   VALUE                        
000302             '708 ORDER MISSING OR READY             '.                   
000303   03    FILLER REDEFINES FEL1.                                           
000304     05    FEL-1 OCCURS 2        PIC X(40).                               
000305     SKIP2                                                                
000306   03    FEL2.                                                            
000307     05    FILLER                PIC X(40)   VALUE                        
000308             '702 ORDERVIS PACKNING PÅGÅR.           '.                   
000309     05    FILLER                PIC X(40)   VALUE                        
000310             '702 REPORTING PER ORDER IN PROGRESS    '.                   
000311   03    FILLER REDEFINES FEL2.                                           
000312     05    FEL-2 OCCURS 2        PIC X(40).                               
000313     SKIP2                                                                
000314   03    FEL3.                                                            
000315     05    FILLER                PIC X(40)   VALUE                        
000316             'SOFTWARE ORDER                         '.                   
000317     05    FILLER                PIC X(40)   VALUE                        
000318             'SOFTWARE ORDER                         '.                   
000319   03    FILLER REDEFINES FEL3.                                           
000320     05    FEL-3 OCCURS 2        PIC X(40).                               
000321     SKIP2                                                                
000322   03    FEL4.                                                            
000323     05    FILLER                PIC X(40)   VALUE                        
000324             '711. ANGIVEN PACKARE SAKNAS PÅ ORDERN  '.                   
000325     05    FILLER                PIC X(40)   VALUE                        
000326             '711 PACKER AND ORDER DO NOT MATCH      '.                   
000327   03    FILLER REDEFINES FEL4.                                           
000328     05    FEL-4 OCCURS 2        PIC X(40).                               
000329     EJECT                                                                
000330   03    FEL5.                                                            
000331     05    FILLER                PIC X(40)   VALUE                        
000332             '712. INGA UTEST. ORDERDELAR FÖR PACKARE'.                   
000333     05    FILLER                PIC X(40)   VALUE                        
000334             'NO REMAIN. ORDER PARTS FOR THIS PACKER '.                   
000335   03    FILLER REDEFINES FEL5.                                           
000336     05    FEL-5 OCCURS 2        PIC X(40).                               
000337     SKIP2                                                                
000338   03    FEL8.                                                            
000339     05    FILLER                PIC X(40)   VALUE                        
000340             '714. RAPPORTERING PÅBÖRJAD ELLER KLAR  '.                   
000341     05    FILLER                PIC X(40)   VALUE                        
000342             '714  REPORTING IN PROGRESS OR READY    '.                   
000343   03    FILLER REDEFINES FEL8.                                           
000344     05    FEL-8 OCCURS 2        PIC X(40).                               
000345     EJECT                                                                
000346   03    FEL9.                                                            
000347     05    FILLER                PIC X(40)   VALUE                        
000348             '719. PACKARE SAKNAS                    '.                   
000349     05    FILLER                PIC X(40)   VALUE                        
000350             '719 PACKER MISSING                     '.                   
000351   03    FILLER REDEFINES FEL9.                                           
000352     05    FEL-9 OCCURS 2        PIC X(40).                               
000353     SKIP2                                                                
000354   03    FEL10.                                                           
000355     05    FILLER                PIC X(40)   VALUE                        
000356             '720. ANG. PACKARES ORDERDEL REDAN KLAR '.                   
000357     05    FILLER                PIC X(40)   VALUE                        
000358             '720 ORDER PART OF PACKER READY         '.                   
000359   03    FILLER REDEFINES FEL10.                                          
000360     05    FEL-10 OCCURS 2       PIC X(40).                               
000361     SKIP2                                                                
000362   03    FEL11.                                                           
000363     05    FILLER                PIC X(40)   VALUE                        
000364             '777. UPPDATERING EJ TILLÅTEN           '.                   
000365     05    FILLER                PIC X(40)   VALUE                        
000366             '777. UPDATING NOT ALLOWED              '.                   
000367   03    FILLER REDEFINES FEL11.                                          
000368     05    FEL-11 OCCURS 2       PIC X(40).                               
000369     SKIP2                                                                
000370   03    FEL12.                                                           
000371     05    FILLER                PIC X(40)   VALUE                        
000372             '748. UPPLYSTA FÄLT FEL                 '.                   
000373     05    FILLER                PIC X(40)   VALUE                        
000374             '748. HIGHLIT FIELDS WRONG              '.                   
000375   03    FILLER REDEFINES FEL12.                                          
000376     05    FEL-12 OCCURS 2       PIC X(40).                               
000377     EJECT                                                                
000378   03    FEL13.                                                           
000379     05    FILLER                PIC X(40)   VALUE                        
000380             '760. UPPGIFTER SAKNAS                  '.                   
000381     05    FILLER                PIC X(40)   VALUE                        
000382             '760. INFORMATION MISSING               '.                   
000383   03    FILLER REDEFINES FEL13.                                          
000384     05    FEL-13 OCCURS 2       PIC X(40).                               
000385     SKIP2                                                                
000386   03    FEL14.                                                           
000387     05    FILLER                PIC X(40)   VALUE                        
000388             '749 FEL NYCKEL                         '.                   
000389     05    FILLER                PIC X(40)   VALUE                        
000390             '749 WRONG KEY                          '.                   
000391   03    FILLER REDEFINES FEL14.                                          
000392     05    FEL-14 OCCURS 2       PIC X(40).                               
000393     SKIP2                                                                
000394   03    FEL16.                                                           
000395     05    FILLER                PIC X(40)   VALUE                        
000396             '799. KONTROLL FÖR PACKAREN PÅGÅR       '.                   
000397     05    FILLER                PIC X(40)   VALUE                        
000398             '799 CONTROL FOR PACKER IN PROGRESS     '.                   
000399   03    FILLER REDEFINES FEL16.                                          
000400     05    FEL-16 OCCURS 2       PIC X(40).                               
000401     EJECT                                                                
000402   03    FEL17.                                                           
000403     05    FILLER                PIC X(40)   VALUE                        
000404             '800 AVVIKELSE UPPDAT FÖR PACKAREN PÅGÅR'.                   
000405     05    FILLER                PIC X(40)   VALUE                        
000406             'DEVIATION UPDAT. FOR PACKER IN PROGRESS'.                   
000407   03    FILLER REDEFINES FEL17.                                          
000408     05    FEL-17 OCCURS 2       PIC X(40).                               
000409     SKIP2                                                                
000410   03    FEL18.                                                           
000411     05    FILLER                PIC X(40)   VALUE                        
000412             '801. MANUELL ORDER EJ KOLLIVIS         '.                   
000413     05    FILLER                PIC X(40)   VALUE                        
000414             '801 MANUAL ORDER - NOT PER CASE        '.                   
000415   03    FILLER REDEFINES FEL18.                                          
000416     05    FEL-18 OCCURS 2       PIC X(40).                               
000417     SKIP2                                                                
000418   03    FEL19.                                                           
000419     05    FILLER                PIC X(40)   VALUE                        
000420             '003 TRYCK PF11 VID UPPDATERING         '.                   
000421     05    FILLER                PIC X(40)   VALUE                        
000422             '003 PRESS PF11 TO UPDATE               '.                   
000423   03    FILLER REDEFINES FEL19.                                          
000424     05    FEL-19 OCCURS 2       PIC X(40).                               
000425     SKIP3                                                                
000426   03    MED1.                                                            
000427     05    FILLER                PIC X(40)   VALUE                        
000428             '778. FLER RADER FINNS                  '.                   
000429     05    FILLER                PIC X(40)   VALUE                        
000430             '778. MORE LINES                        '.                   
000431   03    FILLER REDEFINES MED1.                                           
000432     05    MED-1 OCCURS 2        PIC X(40).                               
000433     SKIP2                                                                
000434   03    MED2.                                                            
000435     05    FILLER                PIC X(40)   VALUE                        
000436             '779. ÄNDRING KLAR                      '.                   
000437     05    FILLER                PIC X(40)   VALUE                        
000438             '779. CHANGE READY                      '.                   
000439   03    FILLER REDEFINES MED2.                                           
000440     05    MED-2 OCCURS 2        PIC X(40).                               
000441     SKIP2                                                                
000442   03    MED4.                                                            
000443     05    FILLER                PIC X(40)   VALUE                        
000444             '006 DETTA ÄR FÖRSTA SIDAN              '.                   
000445     05    FILLER                PIC X(40)   VALUE                        
000446             '006 THIS IS THE FIRST PAGE             '.                   
000447   03    FILLER REDEFINES MED4.                                           
000448     05    MED-4 OCCURS 2        PIC X(40).                               
000449     EJECT                                                                
000450   03    MED5.                                                            
000451     05    FILLER                PIC X(40)   VALUE                        
000452             '106 DETTA ÄR SISTA SIDAN               '.                   
000453     05    FILLER                PIC X(40)   VALUE                        
000454             '106 THIS IS THE LAST PAGE              '.                   
000455   03    FILLER REDEFINES MED5.                                           
000456     05    MED-5 OCCURS 2        PIC X(40).                               
000457     EJECT                                                                
000458******************************************************************        
000459*                                                                         
000460*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
000461*                                                                         
000462 01    FILLER                    PIC X(16)   VALUE 'MFS-WS'.              
000463     SKIP3                                                                
000464*01    MID -COPY W4I31201.                                                
000465     EJECT                                                                
000466*01    -COPY WMSGAREA                                                     
000467     EJECT                                                                
000468*  03    MOD -COPY W4O31201  -RED MSG-AREA.                               
000469     EJECT                                                                
000470*01    -COPY WMFSAREA                                                     
000471     EJECT                                                                
000472******************************************************************        
000473*                                                                         
000474*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
000475*                                                                         
000476 01    IMS-WS.                                                            
000477   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
000478     SKIP3                                                                
000479*                        **** STATUS-KOD FRÅN IMS                         
000480   03    STATUS-4488-WS          PIC XX.                                  
000481     88    SEG-4488-FINNS                   VALUE '  '.                   
000482     88    SEG-4488-SAKNAS                  VALUE 'GE' 'GB'.              
000483   03    STATUS-4490-WS          PIC XX.                                  
000484     88    SEG-4490-FINNS                   VALUE '  '.                   
000485     88    SEG-4490-SAKNAS                  VALUE 'GE'.                   
000486   03    STATUS-KUNDORDER-SEK-WS PIC XX.                                  
000487     88    KUNDORDER-SEK-FINNS              VALUE '  '.                   
000488     88    KUNDORDER-SEK-SAKNAS             VALUE 'GE' 'GB'.              
000489   03    STATUS-ORAD-WS          PIC XX.                                  
000490     88    ORAD-FINNS                       VALUE '  '.                   
000491     88    ORAD-SAKNAS                      VALUE 'GE' 'GP'.              
000492   03    STATUS-WS               PIC XX.                                  
000493     88    SEGMENT-FINNS                    VALUE '  '.                   
000494     88    SEGMENT-SAKNAS                   VALUE 'GE'.                   
000495     88    SEGMENT-FINNS-REDAN              VALUE 'II'.                   
000496     88    SLUT-PA-BASEN                    VALUE 'GB'.                   
000497     SKIP3                                                                
000498   03    GODK-STATUSKODER.                                                
000499     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
000500     SKIP3                                                                
000501 01    SSA1                      PIC X(128).                              
000502 01    SSA2                      PIC X(64).                               
000503 01    SSA3                      PIC X(64).                               
000504 01    SSA4                      PIC X(64).                               
000505     EJECT                                                                
000506*                            IMS FUNKTIONSKODER                           
000507*01    -COPY W0003                                                        
000508     EJECT                                                                
000509*                            DLI INPUT-OUTPUT AREA                        
000510 01    FILLER                    PIC X(16) VALUE 'DLI-IO-AREA1'.          
000511 01    DLI-IO-AREA.                                                       
000512   03    IO-AREA                 PIC X(510)  VALUE SPACE.                 
000513     SKIP3                                                                
000514*  03    WLXXDJ01 -COPY WDGX4305            -RED IO-AREA.                 
000515     EJECT                                                                
000516*  03    WLXXDJ12 -COPY WDGX4306              -RED IO-AREA.               
000517     EJECT                                                                
000518*                            DLI INPUT-OUTPUT AREA -3                     
000519 01    FILLER                    PIC X(16) VALUE 'DLI-IO-AREA3'.          
000520 01    DLI-IO-AREA3.                                                      
000521*  03    WDGX4487 -COPY WDGX4487                                          
000522     EJECT                                                                
000523*  03    WDGX4488 -COPY WDGX4488                                          
000524     EJECT                                                                
000525*                            DLI INPUT-OUTPUT AREA WDGX4490               
000526 01    FILLER                PIC X(16) VALUE 'DLI-IO-WDGX4490'.           
000527 01    DLI-IO-WDGX4490.                                                   
000528*  03    -COPY WDGX4490                                                   
000529     EJECT                                                                
000530 01    FILLER                PIC X(16) VALUE 'DLI-IO-WDQ301'.             
000531 01    DLI-IO-WDQ301.                                                     
000532*  03    -COPY WDQ301                                                     
000533     SKIP3                                                                
000534 01    FILLER                PIC X(16) VALUE 'DLI-IO-WDGX4447'.           
000535 01    DLI-IO-WDGX4447.                                                   
000536*  03    -COPY WDGX4447                                                   
000537     SKIP3                                                                
000538 01    FILLER                PIC X(16) VALUE 'DLI-IO-WDGX4448'.           
000539 01    DLI-IO-WDGX4448.                                                   
000540*  03    -COPY WDGX4448                                                   
000541     SKIP3                                                                
000542 01    FILLER                    PIC X(16) VALUE 'DLI-IO-E601'.           
000543 01    DLI-IO-E601.                                                       
000544*  03    -COPY WDE601                                                     
000545     SKIP3                                                                
000546 01    FILLER                    PIC X(16) VALUE 'DLI-IO-E401'.           
000547 01    DLI-IO-E401.                                                       
000548*  03    -COPY WDE401                                                     
000549     SKIP3                                                                
000550 01    FILLER                    PIC X(16) VALUE 'DLI-IO-E411'.           
000551 01    DLI-IO-E411.                                                       
000552*  03    -COPY WDE411                                                     
000553                                                                          
000554 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
000555 01   DLI-IO-AREA-B6.                                                     
000556*     03  -COPY WDB601                                                    
000557***********************************************************               
000558     EJECT                                                                
000559 LINKAGE SECTION.                                                         
000560*01    -COPY W0009     -PRE MSG-                                          
000561     EJECT                                                                
000562*01    -COPY W0009     -PRE ALT-                                          
000563     EJECT                                                                
000564*01    -COPY W0008     -PRE USEA-                                         
000565     05  FILLER                  PIC X.                                   
000566     EJECT                                                                
000567*01    -COPY W0008     -PRE WDE4-                                         
000568     05  FILLER                  PIC X.                                   
000569     EJECT                                                                
000570*01    -COPY W0008     -PRE WDE41-                                        
000571     05  FILLER                  PIC X.                                   
000572     EJECT                                                                
000573*01    -COPY W0008     -PRE WDE42-                                        
000574     05  FILLER                  PIC X.                                   
000575     EJECT                                                                
000576*01    -COPY W0008     -PRE WDE43-                                        
000577     05  FILLER                  PIC X.                                   
000578     EJECT                                                                
000579*01    -COPY W0008     -PRE WDE6-                                         
000580     05  FILLER                  PIC X.                                   
000581     EJECT                                                                
000582*01    -COPY W0008     -PRE XXDJ-                                         
000583     05  FILLER                  PIC X.                                   
000584     EJECT                                                                
000585*01    -COPY W0008     -PRE 4487-                                         
000586     05  FILLER                  PIC X.                                   
000587     EJECT                                                                
000588*01    -COPY W0008     -PRE ORQA-                                         
000589     05  FILLER                  PIC X.                                   
000590     EJECT                                                                
000591*01    -COPY W0008     -PRE XXKH-                                         
000592     05  FILLER                  PIC X.                                   
000593     EJECT                                                                
000594*01    -COPY W0008     -PRE WDB6-                                         
000595     05  FILLER                  PIC X.                                   
000596     EJECT                                                                
000597 PROCEDURE DIVISION USING MSG-PCB  ALT-PCB   USEA-PCB                     
000598                          WDE4-PCB WDE41-PCB WDE42-PCB WDE43-PCB          
000599                          WDE6-PCB XXDJ-PCB  4487-PCB                     
000600                          ORQA-PCB XXKH-PCB  WDB6-PCB.                    
000601 MAIN SECTION.                                                            
000602                                                                          
000603     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB   USEA-PCB                     
000604                          WDE4-PCB WDE41-PCB WDE42-PCB WDE43-PCB          
000605                          WDE6-PCB XXDJ-PCB  4487-PCB                     
000606                          ORQA-PCB XXKH-PCB  WDB6-PCB.                    
000607     SKIP2                                                                
000608     PERFORM IMS-GET-MSG                                                  
000609                                                                          
000610     IF SEGMENT-FINNS                                                     
000611       PERFORM A-INIT-SPARA-INPUT                                         
000612*** FIX FÖR ATT FIXA TILL IDUSER PÅ WDE401                                
000613*      IF MSG-SIGNON-USERID = 'W0HELP1 '                                  
000614*         MOVE +1398         TO W-401-IDDISTR                             
000615*         MOVE +99754        TO W-401-IDKUNDNR                            
000616*         MOVE '37433     '  TO W-401-IDKUNDRF                            
000617*         MOVE +58033        TO W-401-IDPRODNR                            
000618*         MOVE +2            TO W-401-IDPLKLST                            
000619*         PERFORM IMS-GHU-WDE401                                          
000620*         MOVE '00000000'    TO  KORD-IDUSER                              
000621*         PERFORM IMS-REPL-WDE401                                         
000622*      ELSE                                                               
000623*******************************                                           
000624          IF FRAN-BILD-OK                                                 
000625            PERFORM B-KOLLA-INPUT                                         
000626                                                                          
000627           IF MFS-RETURN                                                  
000628              PERFORM M-RETURN-TO-4324                                    
000629           ELSE                                                           
000630            IF SW-ORDERID = JA AND SW-IDPRODNR = NEJ                      
000631              PERFORM C-LAS-IDPRODNR                                      
000632            END-IF                                                        
000633                                                                          
000634            IF WS-KDFEL = ZERO                                            
000635               IF MFS-UPDATE                                              
000636                  IF GAMLA-NYCKLAR AND                                    
000637                     WS-KDFEL = ZERO                                      
000638                     PERFORM D-UPPDATERA                                  
000639                     IF WS-KDFEL = ZERO                                   
000640                        MOVE 21 TO WS-KDFEL                               
000641                     END-IF                                               
000642                  END-IF                                                  
000643               ELSE                                                       
000644                  IF WS-KDFEL = ZERO                                      
000645                     PERFORM H-KOLLA-OM-FELTRYCK                          
000646                     IF INDATA-OK                                         
000647                        PERFORM S20-RENSA-MOD-RADER                       
000648                        IF ENDAST-ANSTNR                                  
000649                           PERFORM E-BEHANDLA-ENDAST-ANSTNR               
000650                        ELSE                                              
000651                           IF ANSTNR-ORDERID                              
000652                              PERFORM F-BEHANDLA-ANSTNR-ORDERID           
000653                           ELSE                                           
000654                              IF ENDAST-ORDERID                           
000655                                 PERFORM G-BEHANDLA-ENDAST-ORDERID        
000656                              ELSE                                        
000657                                 MOVE 13 TO WS-KDFEL                      
000658                              END-IF                                      
000659                           END-IF                                         
000660                        END-IF                                            
000661                     END-IF                                               
000662                  END-IF                                                  
000663               END-IF                                                     
000664            END-IF                                                        
000665                                                                          
000666            IF WS-KDFEL > ZERO                                            
000667              PERFORM J-HAMTA-MEDDELANDE                                  
000668                                                                          
000669              IF WS-KDFEL < 20                                            
000670                PERFORM K-VISA-BILD-IGEN                                  
000671                                                                          
000672                IF WS-KDFEL < 11 AND                                      
000673                   WS-KDFEL NOT = 8 AND                                   
000674                   WS-KDFEL NOT = 4                                       
000675                  PERFORM S20-RENSA-MOD-RADER                             
000676                END-IF                                                    
000677              END-IF                                                      
000678            END-IF                                                        
000679           END-IF                                                         
000680          ELSE                                                            
000681                                                                          
000682            PERFORM L-TOM-SKAERM                                          
000683          END-IF                                                          
000684*      END-IF                                                             
000685****************************************                                  
000686       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
000687                                                                          
000688       IF WS-KDFEL = ZERO                                                 
000689           IF MFS-ENTER                                                   
000690               MOVE MED-4 (SPRAK-INDX) TO MOD-TEMFSFEL                    
000691            ELSE                                                          
000692               IF MFS-NEXT                                                
000693                   MOVE MED-5 (SPRAK-INDX) TO MOD-TEMFSFEL                
000694               END-IF                                                     
000695           END-IF                                                         
000696       END-IF                                                             
000697                                                                          
000698       IF MFS-RETURN                                                      
000699*        IF KEYS-WRONG                                                    
000700         IF WS-KDFEL > ZERO                                               
000701           PERFORM M-RETURN-TO-4324                                       
000702         END-IF                                                           
000703         PERFORM IMS-ISRT-ALT-MSG                                         
000704       ELSE                                                               
000705         PERFORM IMS-INSERT-MSG                                           
000706       END-IF                                                             
000707     END-IF                                                               
000708                                                                          
000709     MOVE ZERO TO RETURN-CODE                                             
000710     GOBACK.                                                              
000711     EJECT                                                                
000712 A-INIT-SPARA-INPUT SECTION.                                              
000713                                                                          
000714     MOVE 'STA A-INIT   '  TO PGM-POS                                     
000715     IF MSG-DUBBLA-TRANSKODER                                             
000716       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I31201                 
000717       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
000718       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
000719       MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                           
000720       MOVE MSG-IDPFK TO MFS-IDPFK                                        
000721     ELSE                                                                 
000722       MOVE MSG-INDATA-MINUS-1-TRANSKOD TO MID-W4I31201                   
000723       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
000724       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
000725       MOVE ' ' TO MFS-KDTRTYP                                            
000726     END-IF                                                               
000727*                                                                         
000728     MOVE ZERO                           TO WS-KDFEL                      
000729     MOVE LOW-VALUE                      TO MSG-AREA                      
000730     MOVE MFS-IDTRANS                    TO FRAN-BILD                     
000731                                                                          
000732     PERFORM AB-INIT-NYCKLAR                                              
000733                                                                          
000734     MOVE 'W4O312N1'                     TO MFS-IDMOD                     
000735     MOVE '4312'                         TO MOD-IDTRANS                   
000736     ACCEPT DAGENS-DATUM FROM DATE                                        
000737                                                                          
000738     MOVE MFS-RENSA-FAELT TO MOD-IDPRODNR-IN                              
000739                             MOD-IDANSTNR-IN                              
000740                             MOD-IDKOLLI-IN                               
000741                             MOD-IDDISTR-IN                               
000742                             MOD-IDKUNDNR-IN                              
000743                             MOD-IDORDNR-IN                               
000744                             MOD-TEMFSFEL                                 
000745                             MOD-TEMFSINF                                 
000746                                                                          
000747     IF MSGI-IDLAND-SPR = 'GB'                                            
000748       MOVE +2 TO SPRAK-INDX                                              
000749     ELSE                                                                 
000750       MOVE +1 TO SPRAK-INDX                                              
000751     END-IF                                                               
000752                                                                          
000753     MOVE 1 TO MOD-RAD-IND                                                
000754     PERFORM UNTIL MOD-RAD-IND = 14                                       
000755       MOVE MFS-ROER-EJ-FAELT TO MOD-IDDISTR      (MOD-RAD-IND)           
000756                                 MOD-IDKUNDNR     (MOD-RAD-IND)           
000757                                 MOD-IDORDNR      (MOD-RAD-IND)           
000758                                 MOD-IDPRODNR     (MOD-RAD-IND)           
000759                                 MOD-IDPLKLST     (MOD-RAD-IND)           
000760                            MOD-IDRADNR-ORD-FROM  (MOD-RAD-IND)           
000761                            MOD-IDRADNR-ORD-TOM   (MOD-RAD-IND)           
000762                                 MOD-IDANSTNR     (MOD-RAD-IND)           
000763                                 MOD-IDANSTNR-NEW (MOD-RAD-IND)           
000764       ADD 1 TO MOD-RAD-IND                                               
000765     END-PERFORM                                                          
000766                                                                          
000767     IF NOT FRAN-EGEN-BILD                                                
000768*      MOVE '+++++'         TO MID-IDANSTNR-IN                            
000769*      MOVE '+++++++'       TO MID-IDPRODNR-IN                            
000770*      MOVE '++++++'        TO MID-IDKUNDNR-IN                            
000771*      MOVE '+++++'         TO MID-IDDISTR-IN                             
000772*      MOVE '+++++'         TO MID-IDORDNR-IN                             
000773       IF NOT FRAN-4324-BILD                                              
000774         MOVE '+++++'       TO MID-IDANSTNR-IN                            
000775         MOVE '+++++++'     TO MID-IDPRODNR-IN                            
000776         MOVE '++++++'      TO MID-IDKUNDNR-IN                            
000777         MOVE '+++++'       TO MID-IDDISTR-IN                             
000778         MOVE '+++++'       TO MID-IDORDNR-IN                             
000779       END-IF                                                             
000780     END-IF                                                               
000781                                                                          
000782     IF FRAN-4324-BILD                                                    
000783       MOVE MID-KDPRCGRP-4324  TO MOD-KDPRCGRP-4324                       
000784     END-IF                                                               
000785     .                                                                    
000786     EJECT                                                                
000787 AB-INIT-NYCKLAR SECTION.                                                 
000788                                                                          
000789     MOVE 'STA AB-INIT  '  TO PGM-POS                                     
000790     MOVE ALL '+'           TO MSGI-WMSGINIT                              
000791     MOVE '001'             TO MSGI-KDCALL                                
000792     MOVE '4312'            TO MSGI-IDTRANS                               
000793     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
000794     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
000795     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
000796                                                                          
000797     IF MID-IDANSTNR-IN = ALL '+'                                         
000798       MOVE SPACE TO WS-IDANSTNR                                          
000799     ELSE                                                                 
000800       MOVE MID-IDANSTNR-IN TO WS-IDANSTNR                                
000801     END-IF                                                               
000802                                                                          
000803     IF MID-IDPRODNR-IN NOT = ALL '+'                                     
000804       MOVE MID-IDPRODNR-IN TO WS-IDPRODNR                                
000805     ELSE                                                                 
000806       MOVE ZERO TO WS-IDPRODNR                                           
000807     END-IF                                                               
000808                                                                          
000809     IF MID-IDKUNDNR-IN NOT = ALL '+'                                     
000810       MOVE MID-IDKUNDNR-IN TO WS-IDKUNDNR                                
000811     ELSE                                                                 
000812       INSPECT MID-IDKUNDNR-UT REPLACING LEADING SPACE                    
000813       BY ZERO                                                            
000814       MOVE MID-IDKUNDNR-UT TO WS-IDKUNDNR                                
000815     END-IF                                                               
000816                                                                          
000817     IF MID-IDDISTR-IN  NOT = ALL '+'                                     
000818       MOVE MID-IDDISTR-IN  TO WS-IDDISTR                                 
000819     ELSE                                                                 
000820       INSPECT MID-IDDISTR-UT  REPLACING LEADING SPACE                    
000821       BY ZERO                                                            
000822       MOVE MID-IDDISTR-UT  TO WS-IDDISTR                                 
000823     END-IF                                                               
000824                                                                          
000825     IF MID-IDORDNR-IN  NOT = ALL '+'                                     
000826       MOVE MID-IDORDNR-IN  TO WS-IDORDNR                                 
000827     ELSE                                                                 
000828       INSPECT MID-IDORDNR-UT  REPLACING LEADING SPACE                    
000829       BY ZERO                                                            
000830       MOVE MID-IDORDNR-UT  TO WS-IDORDNR                                 
000831     END-IF                                                               
000832                                                                          
000833     MOVE MSGI-IDDC                       TO WS-IDDC                      
000834     MOVE WS-IDDC                         TO MOD-IDDC-UT                  
000835     IF WS-IDDC IS > SPACE                                                
000836*      CONTINUE                                                           
000837        MOVE WS-IDDC TO W-IDDC-B6                                         
000838        PERFORM IMS-GU-WDB6                                               
000839     ELSE                                                                 
000840       MOVE 14    TO WS-KDFEL                                             
000841     END-IF                                                               
000842     .                                                                    
000843     EJECT                                                                
000844 B-KOLLA-INPUT SECTION.                                                   
000845                                                                          
000846     MOVE 'STA B-KOLLA  '  TO PGM-POS                                     
000847     MOVE ZERO   TO NYCKEL-KOMBINATION                                    
000848     MOVE +1     TO NYCKEL-TYP                                            
000849     MOVE NEJ    TO SW-IDPRODNR                                           
000850                    SW-ORDERID                                            
000851                    SW-IDANSTNR                                           
000852                                                                          
000853     IF MID-IDANSTNR-IN = ALL '+'                                         
000854                                                                          
000855       IF NOT FRAN-EGEN-BILD                                              
000856         IF NOT FRAN-4324-BILD                                            
000857           MOVE 9   TO WS-KDFEL                                           
000858         END-IF                                                           
000859       END-IF                                                             
000860     ELSE                                                                 
000861                                                                          
000862       IF MID-IDANSTNR-IN NUMERIC                                         
000863         MOVE +2 TO NYCKEL-TYP                                            
000864         MOVE JA TO SW-IDANSTNR                                           
000865       ELSE                                                               
000866         MOVE SPACE TO WS-IDANSTNR                                        
000867       END-IF                                                             
000868     END-IF                                                               
000869                                                                          
000870     IF MID-IDDISTR-IN NOT = ALL '+'                                      
000871                                                                          
000872       IF MID-IDDISTR-IN NUMERIC                                          
000873         MOVE +2 TO NYCKEL-TYP                                            
000874         MOVE JA TO SW-ORDERID                                            
000875       ELSE                                                               
000876         MOVE ZERO TO WS-IDDISTR                                          
000877       END-IF                                                             
000878     END-IF                                                               
000879                                                                          
000880     SKIP2                                                                
000881     IF MID-IDKUNDNR-IN NOT = ALL '+'                                     
000882                                                                          
000883       IF MID-IDKUNDNR-IN NUMERIC                                         
000884         MOVE +2 TO NYCKEL-TYP                                            
000885         MOVE JA TO SW-ORDERID                                            
000886       ELSE                                                               
000887         MOVE ZERO TO WS-IDKUNDNR                                         
000888       END-IF                                                             
000889     END-IF                                                               
000890     EJECT                                                                
000891                                                                          
000892     IF MID-IDORDNR-IN NOT = ALL '+'                                      
000893                                                                          
000894       IF MID-IDORDNR-IN NUMERIC                                          
000895         MOVE +2 TO NYCKEL-TYP                                            
000896         MOVE JA TO SW-ORDERID                                            
000897       ELSE                                                               
000898         MOVE ZERO TO WS-IDORDNR                                          
000899       END-IF                                                             
000900     END-IF                                                               
000901                                                                          
000902     SKIP2                                                                
000903     IF MID-IDPRODNR-IN NOT = ALL '+'                                     
000904                                                                          
000905       IF MID-IDPRODNR-IN NUMERIC                                         
000906         MOVE +2 TO NYCKEL-TYP                                            
000907         MOVE JA TO SW-IDPRODNR                                           
000908       ELSE                                                               
000909         MOVE ZERO TO WS-IDPRODNR                                         
000910       END-IF                                                             
000911     END-IF                                                               
000912     SKIP2                                                                
000913                                                                          
000914     IF FRAN-EGEN-BILD                                                    
000915     OR FRAN-4324-BILD                                                    
000916                                                                          
000917       IF MFS-ENTER AND GAMLA-NYCKLAR                                     
000918         MOVE MID-KDPRCGRP-ENTER TO WS-MID-KDPRCGRP-ENTER                 
000919         MOVE MID-TIRFS-ENTER   TO WS-DARFS-NYCKEL                        
000920         IF MID-TIRFS-ENTER NOT = ZERO                                    
000921           IF MID-TIRFS-ENTER < 5000000000                                
000922             MOVE 20            TO WS-DARFS-NYCKEL (1:2)                  
000923           ELSE                                                           
000924             IF MID-TIRFS-ENTER < 9999999999                              
000925               MOVE 19          TO WS-DARFS-NYCKEL (1:2)                  
000926             ELSE                                                         
000927               MOVE 999999999999 TO WS-DARFS-NYCKEL                       
000928             END-IF                                                       
000929           END-IF                                                         
000930         END-IF                                                           
000931         MOVE MID-IDPRODNR-ENTER TO WS-MID-IDPRODNR-ENTER                 
000932         MOVE MID-IDPLKLST-ENTER TO WS-MID-IDPLKLST-ENTER                 
000933       ELSE                                                               
000934          IF  MID-KDPRCGRP-NEXT  = ALL '+'                                
000935            MOVE ZERO              TO WS-MID-KDPRCGRP-NEXT                
000936          ELSE                                                            
000937            IF MID-KDPRCGRP-NEXT NUMERIC                                  
000938              MOVE MID-KDPRCGRP-NEXT TO WS-MID-KDPRCGRP-NEXT              
000939            ELSE                                                          
000940              MOVE ZERO              TO WS-MID-KDPRCGRP-NEXT              
000941            END-IF                                                        
000942          END-IF                                                          
000943          IF MID-TIRFS-NEXT = ALL '+'                                     
000944            MOVE ZERO            TO MID-TIRFS-NEXT                        
000945          END-IF                                                          
000946          IF MID-TIRFS-NEXT NUMERIC                                       
000947            MOVE MID-TIRFS-NEXT  TO WS-DARFS-NYCKEL                       
000948          ELSE                                                            
000949            MOVE ZERO            TO WS-DARFS-NYCKEL                       
000950            MOVE ZERO            TO MID-TIRFS-NEXT                        
000951          END-IF                                                          
000952          IF MID-TIRFS-NEXT NOT = ZERO                                    
000953            IF MID-TIRFS-NEXT < 5000000000                                
000954              MOVE 20            TO WS-DARFS-NYCKEL (1:2)                 
000955            ELSE                                                          
000956              IF MID-TIRFS-NEXT < 9999999999                              
000957                MOVE 19          TO WS-DARFS-NYCKEL (1:2)                 
000958              ELSE                                                        
000959                MOVE 999999999999 TO WS-DARFS-NYCKEL                      
000960              END-IF                                                      
000961            END-IF                                                        
000962          END-IF                                                          
000963          IF MID-IDPRODNR-NEXT = ALL '+'                                  
000964            MOVE ZERO              TO WS-MID-IDPRODNR-NEXT                
000965          ELSE                                                            
000966            IF MID-IDPRODNR-NEXT NUMERIC                                  
000967              MOVE MID-IDPRODNR-NEXT TO WS-MID-IDPRODNR-NEXT              
000968            ELSE                                                          
000969              MOVE ZERO              TO WS-MID-IDPRODNR-NEXT              
000970            END-IF                                                        
000971          END-IF                                                          
000972          IF MID-IDPLKLST-NEXT = ALL '+'                                  
000973            MOVE ZERO              TO WS-MID-IDPLKLST-NEXT                
000974          ELSE                                                            
000975            IF MID-IDPLKLST-NEXT NUMERIC                                  
000976              MOVE MID-IDPLKLST-NEXT TO WS-MID-IDPLKLST-NEXT              
000977            ELSE                                                          
000978              MOVE ZERO              TO WS-MID-IDPLKLST-NEXT              
000979            END-IF                                                        
000980          END-IF                                                          
000981        END-IF                                                            
000982     ELSE                                                                 
000983        MOVE ZERO                TO WS-DARFS-NYCKEL                       
000984                                    WS-MID-IDPRODNR-NEXT                  
000985                                    WS-MID-IDPLKLST-NEXT                  
000986                                    WS-MID-IDPRODNR-ENTER                 
000987                                    WS-MID-IDPLKLST-ENTER                 
000988        MOVE SPACE               TO WS-MID-KDPRCGRP-NEXT                  
000989        MOVE SPACE               TO WS-MID-KDPRCGRP-ENTER                 
000990                                    WS-IDANSTNR                           
000991     END-IF                                                               
000992     EJECT                                                                
000993     IF NYA-NYCKLAR                                                       
000994                                                                          
000995       IF WS-IDANSTNR NOT = SPACE                                         
000996                                                                          
000997         IF SW-IDPRODNR = JA OR SW-ORDERID = JA                           
000998           MOVE +2 TO NYCKEL-KOMBINATION                                  
000999         ELSE                                                             
001000           MOVE +1 TO NYCKEL-KOMBINATION                                  
001001         END-IF                                                           
001002       ELSE                                                               
001003                                                                          
001004         IF SW-IDPRODNR = JA OR SW-ORDERID = JA                           
001005           MOVE +3 TO NYCKEL-KOMBINATION                                  
001006         END-IF                                                           
001007       END-IF                                                             
001008     ELSE                                                                 
001009                                                                          
001010       IF GAMLA-NYCKLAR                                                   
001011         INSPECT MID-IDPRODNR-UT REPLACING LEADING SPACE                  
001012         BY ZERO                                                          
001013                                                                          
001014         MOVE MID-IDANSTNR-UT TO WS-IDANSTNR                              
001015         IF WS-IDANSTNR        = '    0' AND NOT MFS-NEXT                 
001016            MOVE SPACE TO WS-IDANSTNR                                     
001017          ELSE                                                            
001018            INSPECT WS-IDANSTNR REPLACING LEADING SPACE BY ZERO           
001019         END-IF                                                           
001020                                                                          
001021         MOVE MID-IDPRODNR-UT TO WS-IDPRODNR                              
001022         IF WS-IDANSTNR NOT = SPACE                                       
001023           MOVE JA TO SW-IDANSTNR                                         
001024         END-IF                                                           
001025                                                                          
001026         IF WS-IDPRODNR > ZERO                                            
001027           MOVE JA TO SW-IDPRODNR                                         
001028         ELSE                                                             
001029                                                                          
001030           IF WS-IDDISTR > ZERO                                           
001031           AND WS-IDORDNR > ZERO                                          
001032             MOVE JA TO SW-ORDERID                                        
001033           END-IF                                                         
001034         END-IF                                                           
001035                                                                          
001036         IF SW-ORDERID = JA                                               
001037                                                                          
001038            IF WS-IDANSTNR = ZERO                                         
001039                                                                          
001040               IF MFS-NEXT                                                
001041                  MOVE SPACE TO WS-IDANSTNR                               
001042                  MOVE NEJ TO SW-IDANSTNR                                 
001043               END-IF                                                     
001044            END-IF                                                        
001045         END-IF                                                           
001046                                                                          
001047         IF WS-IDANSTNR NOT = SPACE                                       
001048                                                                          
001049           IF SW-IDPRODNR = JA OR SW-ORDERID = JA                         
001050             MOVE +2 TO NYCKEL-KOMBINATION                                
001051           ELSE                                                           
001052             MOVE +1 TO NYCKEL-KOMBINATION                                
001053           END-IF                                                         
001054         ELSE                                                             
001055                                                                          
001056           IF SW-IDPRODNR = JA OR SW-ORDERID = JA                         
001057             MOVE +3 TO NYCKEL-KOMBINATION                                
001058           END-IF                                                         
001059         END-IF                                                           
001060                                                                          
001061       END-IF                                                             
001062     END-IF                                                               
001063     IF ENDAST-ANSTNR                                                     
001064       MOVE ZERO TO        WS-IDDISTR                                     
001065                           WS-IDKUNDNR                                    
001066                           WS-IDORDNR                                     
001067                           WS-IDPRODNR                                    
001068     ELSE                                                                 
001069       IF ANSTNR-ORDERID AND SW-IDPRODNR = JA                             
001070         MOVE ZERO TO        WS-IDDISTR                                   
001071                             WS-IDKUNDNR                                  
001072                             WS-IDORDNR                                   
001073       ELSE                                                               
001074         IF ANSTNR-ORDERID AND SW-ORDERID  = JA                           
001075           MOVE ZERO TO        WS-IDPRODNR                                
001076         ELSE                                                             
001077           IF ENDAST-ORDERID AND SW-IDPRODNR = JA                         
001078             MOVE SPACE TO       WS-IDANSTNR                              
001079             MOVE ZERO TO        WS-IDDISTR                               
001080                                 WS-IDKUNDNR                              
001081                                 WS-IDORDNR                               
001082           ELSE                                                           
001083             IF ENDAST-ORDERID AND SW-ORDERID = JA                        
001084               MOVE SPACE TO       WS-IDANSTNR                            
001085               MOVE ZERO TO        WS-IDPRODNR                            
001086             END-IF                                                       
001087           END-IF                                                         
001088         END-IF                                                           
001089       END-IF                                                             
001090     END-IF                                                               
001091     EJECT                                                                
001092     MOVE WS-IDANSTNR  TO MOD-IDANSTNR-UT                                 
001093     INSPECT MOD-IDANSTNR-UT REPLACING                                    
001094                             LEADING ZEROES BY SPACE                      
001095     IF MOD-IDANSTNR-UT = SPACE                                           
001096        MOVE '    0' TO MOD-IDANSTNR-UT                                   
001097     END-IF                                                               
001098     MOVE WS-IDDISTR   TO MOD-IDDISTR-UT                                  
001099     INSPECT MOD-IDDISTR-UT REPLACING                                     
001100                            LEADING ZEROES BY SPACE                       
001101     MOVE WS-IDKUNDNR  TO MOD-IDKUNDNR-UT                                 
001102     INSPECT MOD-IDKUNDNR-UT REPLACING                                    
001103                             LEADING ZEROES BY SPACE                      
001104     MOVE WS-IDORDNR   TO MOD-IDORDNR-UT                                  
001105     INSPECT MOD-IDORDNR-UT REPLACING                                     
001106                            LEADING ZEROES BY SPACE                       
001107     MOVE WS-IDPRODNR  TO MOD-IDPRODNR-UT                                 
001108     INSPECT MOD-IDPRODNR-UT REPLACING                                    
001109                             LEADING ZEROES BY SPACE                      
001110     EJECT                                                                
001111     IF MFS-UPDATE                                                        
001112       MOVE NEJ       TO SW-AENDRA                                        
001113       MOVE +1 TO MID-RAD-IND                                             
001114       PERFORM UNTIL MID-IDPRODNR (MID-RAD-IND) = '       ' OR            
001115                     MID-RAD-IND = 14                                     
001116         IF MID-IDANSTNR-NEW (MID-RAD-IND) NOT = ALL '+'                  
001117           IF MID-IDANSTNR-NEW (MID-RAD-IND) NUMERIC                      
001118             MOVE MFS-NUM-FAELT-RAETT TO                                  
001119                               MOD-IDANSTNR-NEW-ATTR (MID-RAD-IND)        
001120             MOVE JA TO SW-AENDRA                                         
001121           ELSE                                                           
001122             MOVE +12 TO WS-KDFEL                                         
001123             MOVE MFS-NUM-FAELT-FEL TO                                    
001124                               MOD-IDANSTNR-NEW-ATTR (MID-RAD-IND)        
001125           END-IF                                                         
001126         END-IF                                                           
001127         ADD +1 TO MID-RAD-IND                                            
001128       END-PERFORM                                                        
001129      IF WS-KDFEL = ZERO AND                                              
001130        SW-AENDRA = NEJ                                                   
001131        MOVE 13 TO WS-KDFEL                                               
001132      END-IF                                                              
001133     END-IF                                                               
001134     .                                                                    
001135     EJECT                                                                
001136 C-LAS-IDPRODNR SECTION.                                                  
001137                                                                          
001138     MOVE 'STA C-LAS'  TO PGM-POS                                         
001139     MOVE NEJ          TO WS-SLINGA-KLAR                                  
001140     MOVE IDDISTR-WS   TO W-4A1-IDDISTR                                   
001141     MOVE IDKUNDNR-WS  TO W-4A1-IDKUNDNR                                  
001142     MOVE IDORDNR-WS   TO W-4A1-IDORDNR                                   
001143                                                                          
001144     PERFORM IMS-GU-WDE4ASEQ                                              
001145                                                                          
001146     PERFORM UNTIL KUNDORDER-SEK-SAKNAS OR                                
001147                   SLINGA-KLAR                                            
001148       IF  KORD-IDDC             = WS-IDDC                                
001149       AND KORD-KVORDRAD-LEVPL   = 0                                      
001150         MOVE KORD-IDPRODNR              TO IDPRODNR-WS                   
001151         MOVE JA                         TO WS-SLINGA-KLAR                
001152       ELSE                                                               
001153         PERFORM IMS-GN-WDE4ASEQ                                          
001154       END-IF                                                             
001155     END-PERFORM                                                          
001156                                                                          
001157     IF NOT SLINGA-KLAR                                                   
001158        MOVE 1 TO WS-KDFEL                                                
001159     ELSE                                                                 
001160       MOVE KORD-IDORDER                 TO W-IDORDER-WDQ3                
001161       MOVE KORD-IDDC                    TO W-IDDC-WDQ3                   
001162       MOVE KORD-IDPRODNR                TO W-IDPRODNR-WDQ3               
001163       MOVE KORD-IDPLKLST                TO W-IDPLKLST-WDQ3               
001164**   SOFTWARE KONTROLL                                                    
001165       PERFORM IMS-GHU-ORQA-WDQ301                                        
001166       IF SEGMENT-FINNS                                                   
001167         IF (ODEL-IDLEVNR = '1441 ' OR 'BP2TW')                           
001168         AND ODEL-IDPRC = '9998'                                          
001169            MOVE 3 TO WS-KDFEL                                            
001170         END-IF                                                           
001171       ELSE                                                               
001172         IF (DCS-IDLEVNR-DC  = '1441 '                                    
001173         OR  DCS-IDLEVNR-EMB = 'BP2TW')                                   
001174         AND KORD-IDDISTR    =  +00098                                    
001175           MOVE 3 TO WS-KDFEL                                             
001176         END-IF                                                           
001177       END-IF                                                             
001178     END-IF                                                               
001179     .                                                                    
001180     EJECT                                                                
001181 D-UPPDATERA SECTION.                                                     
001182                                                                          
001183     MOVE 'STA D-UPP'  TO PGM-POS                                         
001184     IF SW-AENDRA = JA                                                    
001185        IF WS-KDFEL = ZERO                                                
001186           PERFORM DA-UPPDATERA-PACKARE                                   
001187           PERFORM S20-RENSA-MOD-RADER                                    
001188           IF ENDAST-ANSTNR                                               
001189              PERFORM DB-VISA-BILD                                        
001190           ELSE                                                           
001191              IF ANSTNR-ORDERID                                           
001192                 PERFORM DC-VISA-BILD                                     
001193              ELSE                                                        
001194                 IF ENDAST-ORDERID                                        
001195                    PERFORM DD-VISA-BILD                                  
001196                 END-IF                                                   
001197              END-IF                                                      
001198           END-IF                                                         
001199        END-IF                                                            
001200     END-IF                                                               
001201     .                                                                    
001202     EJECT                                                                
001203 DA-UPPDATERA-PACKARE SECTION.                                            
001204                                                                          
001205     MOVE 'STA DA-UPP'  TO PGM-POS                                        
001206     MOVE 1                                 TO WS-RAD-IND                 
001207                                                                          
001208     PERFORM IMS-GU-WDB6                                                  
001209                                                                          
001210     PERFORM UNTIL MID-IDPRODNR (WS-RAD-IND) = '       ' OR               
001211                   WS-RAD-IND = 14                                        
001212     MOVE ZERO                          TO WS-JFR-IDANSTNR                
001213     MOVE MID-IDANSTNR-NEW (WS-RAD-IND) TO WS-JFR-IDANSTNR-5              
001214     IF WS-JFR-IDANSTNR-5 NOT = '+++++'                                   
001215        MOVE MID-IDPRODNR (WS-RAD-IND)         TO                         
001216                                               W-411-IDPRODNR-MIN         
001217                                               W-411-IDPRODNR-MAX         
001218        MOVE MID-IDRADNR-ORD-TOM (WS-RAD-IND)  TO                         
001219                                               W-411-IDPURAD-MIN          
001220                                               W-411-IDPURAD-MAX          
001221        PERFORM IMS-GU-WDE411-01-BSEQ                                     
001222        MOVE KORD-IDDISTR                    TO W-401-IDDISTR             
001223                                                W-4A1-IDDISTR             
001224                                                WS-SPARA-IDDISTR          
001225        MOVE KORD-IDKUNDNR                   TO W-401-IDKUNDNR            
001226                                                W-4A1-IDKUNDNR            
001227                                                WS-SPARA-IDKUNDNR         
001228        MOVE KORD-IDKUNDRF                   TO W-401-IDKUNDRF            
001229                                                W-4A1-IDKUNDRF            
001230                                                WS-SPARA-IDKUNDRF         
001231        MOVE WS-SPARA-IDORDNR-ALFA           TO WS-SPARA-IDORDNR          
001232        MOVE KORD-IDORDER                    TO WS-SPARA-IDORDER          
001233        MOVE KORD-IDPRODNR                   TO W-401-IDPRODNR            
001234                                                WS-SPARA-IDPRODNR         
001235        MOVE KORD-IDPLKLST                   TO W-401-IDPLKLST            
001236                                                WS-SPARA-IDPLKLST         
001237        PERFORM DAA-KONTROLL-RAPPORTERING                                 
001238                                                                          
001239        IF WS-KDFEL = ZERO                                                
001240          PERFORM IMS-GHU-WDE401                                          
001241          MOVE WS-JFR-IDANSTNR              TO KORD-IDUSER                
001242          PERFORM IMS-REPL-WDE401                                         
001243                                                                          
001244          IF DCS-CDC OR DCS-CDC-TR OR DCS-SDC                             
001245            IF WS-JFR-IDANSTNR-5 = '00000'                                
001246              PERFORM DAB-SLA-AV-LASNING                                  
001247            ELSE                                                          
001248              PERFORM DAD-SLA-PA-LASNING                                  
001249            END-IF                                                        
001250          END-IF                                                          
001251                                                                          
001252          PERFORM DAE-UPPDAT-ODEL                                         
001253                                                                          
001254          PERFORM DAC-UPPDAT-PRODTAB                                      
001255        END-IF                                                            
001256     END-IF                                                               
001257                                                                          
001258     ADD 1 TO WS-RAD-IND                                                  
001259     END-PERFORM                                                          
001260     .                                                                    
001261     EJECT                                                                
001262 DAA-KONTROLL-RAPPORTERING               SECTION.                         
001263     MOVE 'STA DAA-KON'  TO PGM-POS                                       
001264                                                                          
001265     PERFORM DAAA-KONTROLL-MOT-RAD-STATUS                                 
001266     IF DCS-CDC OR DCS-CDC-TR OR DCS-SDC                                  
001267       PERFORM DAAB-KONTROLLERA-LASNINGSREG                               
001268     END-IF                                                               
001269     .                                                                    
001270     EJECT                                                                
001271 DAAA-KONTROLL-MOT-RAD-STATUS            SECTION.                         
001272     MOVE 'STA DAAA-KON'  TO PGM-POS                                      
001273                                                                          
001274     PERFORM IMS-GU-WDE401                                                
001275                                                                          
001276     IF KORD-KVORDRAD-PACK = KORD-KVORDRAD + KORD-KVORDRAD-LEVPL          
001277        MOVE 8 TO WS-KDFEL                                                
001278     END-IF                                                               
001279                                                                          
001280     MOVE KORD-IDORDER                   TO W-IDORDER-WDQ3                
001281     MOVE KORD-IDDC                      TO W-IDDC-WDQ3                   
001282     MOVE KORD-IDPRODNR                  TO W-IDPRODNR-WDQ3               
001283     MOVE KORD-IDPLKLST                  TO W-IDPLKLST-WDQ3               
001284     PERFORM IMS-GHU-ORQA-WDQ301                                          
001285     IF (ODEL-IDLEVNR = '1441 ' OR 'BP2TW')                               
001286     AND ODEL-IDPRC = '9998'                                              
001287        MOVE 3 TO WS-KDFEL                                                
001288     END-IF                                                               
001289                                                                          
001290     IF WS-KDFEL = 0                                                      
001291        IF KORD-IDUSER > ZERO                                             
001292           MOVE 'N'                 TO WS-SLINGA-KLAR                     
001293           PERFORM IMS-GNP-ORAD                                           
001294           PERFORM UNTIL ORAD-SAKNAS OR                                   
001295                         SLINGA-KLAR                                      
001296              IF ORAD-KDRADSTA > 3                                        
001297                 MOVE JA            TO WS-SLINGA-KLAR                     
001298              ELSE                                                        
001299                 PERFORM IMS-GNP-ORAD                                     
001300              END-IF                                                      
001301           END-PERFORM                                                    
001302           IF  SLINGA-KLAR                                                
001303           AND WS-JFR-IDANSTNR = ZERO                                     
001304           AND ORAD-IDLEVNR NOT = '10987'                                 
001305           AND ORAD-IDLEVNR NOT = 'BQ8VA'                                 
001306              MOVE 8                TO WS-KDFEL                           
001307           END-IF                                                         
001308        END-IF                                                            
001309     END-IF                                                               
001310     .                                                                    
001311     EJECT                                                                
001312 DAAB-KONTROLLERA-LASNINGSREG            SECTION.                         
001313     MOVE 'STA DAAB-KON'  TO PGM-POS                                      
001314                                                                          
001315     MOVE WS-IDDC                   TO W-4305-IDDC                        
001316     MOVE MID-IDPRODNR (WS-RAD-IND) TO W-4306-IDPRODNR                    
001317     PERFORM IMS-GET-XXDJ-ROT                                             
001318     PERFORM IMS-GET-XXDJ-LASNING-GODK-GE                                 
001319                                                                          
001320     IF SEGMENT-FINNS            AND                                      
001321        (4306-KDPACLAS = 1 OR 2 OR 3)                                     
001322       MOVE JA                    TO WS-SLINGA-KLAR                       
001323       MOVE 2                     TO WS-KDFEL                             
001324     END-IF                                                               
001325     .                                                                    
001326     EJECT                                                                
001327 DAB-SLA-AV-LASNING                      SECTION.                         
001328     MOVE 'STA DAB-SLA'  TO PGM-POS                                       
001329                                                                          
001330     MOVE JA                     TO FL-SAMTL-PLKLST-HAR-USER-NOLL         
001331     PERFORM IMS-GU-WDE4ASEQ                                              
001332                                                                          
001333     PERFORM UNTIL SEGMENT-SAKNAS                                         
001334       IF KORD-IDPRODNR     = MID-IDPRODNR (WS-RAD-IND)   AND             
001335          KORD-IDUSER   NOT = '00000000'                                  
001336         MOVE NEJ                TO FL-SAMTL-PLKLST-HAR-USER-NOLL         
001337       END-IF                                                             
001338                                                                          
001339       PERFORM IMS-GN-WDE4ASEQ                                            
001340     END-PERFORM                                                          
001341                                                                          
001342     IF FL-SAMTL-PLKLST-HAR-USER-NOLL = JA                                
001343       PERFORM IMS-GET-XXDJ-ROT                                           
001344       PERFORM IMS-GET-XXDJ-LASNING-GODK-GE                               
001345                                                                          
001346       IF SEGMENT-FINNS                                                   
001347         MOVE ZERO               TO 4306-KDPACLAS                         
001348         PERFORM IMS-REPL-XXDJ-LASNING                                    
001349       END-IF                                                             
001350     END-IF                                                               
001351     .                                                                    
001352     EJECT                                                                
001353 DAC-UPPDAT-PRODTAB SECTION.                                              
001354                                                                          
001355     MOVE 'STA DAC-UPP'  TO PGM-POS                                       
001356     MOVE WS-IDDC               TO W-4447-IDDC                            
001357     MOVE ODEL-IDPRC            TO W-4448-IDPRC                           
001358     PERFORM IMS-GU-4448                                                  
001359                                                                          
001360     MOVE WS-IDDC               TO W-4487-IDDC                            
001361     MOVE 4448-KDPRCGRP         TO W-4488-KDPRCGRP                        
001362     PERFORM IMS-GU-4488                                                  
001363                                                                          
001364     IF SEGMENT-FINNS                                                     
001365        MOVE ODEL-DARFS         TO W-4490-DARFS                           
001366        MOVE ODEL-IDPRODNR      TO W-4490-IDPRODNR                        
001367        MOVE ODEL-IDPLKLST      TO W-4490-IDPLKLST                        
001368        PERFORM IMS-GHU-4490                                              
001369                                                                          
001370        IF SEGMENT-FINNS                                                  
001371           MOVE WS-JFR-IDANSTNR TO 4490-IDUSER                            
001372           PERFORM IMS-REPL-4490                                          
001373        END-IF                                                            
001374     END-IF                                                               
001375     .                                                                    
001376     EJECT                                                                
001377 DAD-SLA-PA-LASNING                      SECTION.                         
001378     MOVE 'STA DAD-SLA'  TO PGM-POS                                       
001379                                                                          
001380     PERFORM IMS-GET-XXDJ-ROT                                             
001381     PERFORM IMS-GET-XXDJ-LASNING-GODK-GE                                 
001382                                                                          
001383     IF SEGMENT-FINNS                                                     
001384       IF 4306-KDPACLAS = 0                                               
001385         MOVE 5                   TO 4306-KDPACLAS                        
001386         PERFORM IMS-REPL-XXDJ-LASNING                                    
001387       END-IF                                                             
001388     ELSE                                                                 
001389       MOVE 5                       TO 4306-KDPACLAS                      
001390       MOVE LOW-VALUE               TO 4306-LOWVALUE                      
001391       MOVE MID-IDPRODNR (WS-RAD-IND) TO 4306-IDPRODNR                    
001392       MOVE NEJ                     TO 4306-FLANNULL                      
001393       PERFORM IMS-ISRT-XXDJ-LASNING                                      
001394     END-IF                                                               
001395     .                                                                    
001396     EJECT                                                                
001397 DAE-UPPDAT-ODEL    SECTION.                                              
001398     MOVE 'STA DAE-UPP'  TO PGM-POS                                       
001399                                                                          
001400     MOVE WS-SPARA-IDORDER     TO W-IDORDER-WDQ3                          
001401     MOVE WS-IDDC              TO W-IDDC-WDQ3                             
001402     MOVE WS-SPARA-IDPRODNR    TO W-IDPRODNR-WDQ3                         
001403     MOVE WS-SPARA-IDPLKLST    TO W-IDPLKLST-WDQ3                         
001404                                                                          
001405     PERFORM IMS-GHU-ORQA-WDQ301                                          
001406                                                                          
001407     MOVE WS-JFR-IDANSTNR      TO ODEL-IDUSER                             
001408                                                                          
001409     PERFORM IMS-REPL-ORQA-WDQ301                                         
001410     .                                                                    
001411     EJECT                                                                
001412 DB-VISA-BILD SECTION.                                                    
001413     MOVE 'STA DB-VISA'  TO PGM-POS                                       
001414                                                                          
001415     MOVE 'N'                          TO WS-TRAEFF-PACKARE               
001416     MOVE 1                            TO WS-RAD-IND                      
001417                                                                          
001418     MOVE WS-IDDC                      TO W-4487-IDDC                     
001419                                                                          
001420     IF MFS-ENTER AND GAMLA-NYCKLAR                                       
001421        MOVE WS-MID-KDPRCGRP-ENTER     TO W-4488-KDPRCGRP                 
001422        PERFORM IMS-GU-4488                                               
001423     ELSE                                                                 
001424       IF MFS-NEXT                                                        
001425          MOVE WS-MID-KDPRCGRP-NEXT    TO W-4488-KDPRCGRP                 
001426          PERFORM IMS-GU-4488                                             
001427       ELSE                                                               
001428          MOVE LOW-VALUE               TO W-4488-KDPRCGRP                 
001429          PERFORM IMS-GU-4488-FIRST                                       
001430       END-IF                                                             
001431     END-IF                                                               
001432     IF SEG-4488-FINNS                                                    
001433       MOVE 4488-KDPRCGRP               TO MOD-KDPRCGRP-ENTER             
001434                                           MOD-KDPRCGRP-NEXT              
001435       IF MFS-ENTER AND GAMLA-NYCKLAR                                     
001436           MOVE WS-DARFS-NYCKEL           TO W-4490-DARFS                 
001437           MOVE MID-IDPRODNR-ENTER       TO W-4490-IDPRODNR               
001438           MOVE MID-IDPLKLST-ENTER       TO W-4490-IDPLKLST               
001439           PERFORM IMS-GNP-4490-KVAL                                      
001440       ELSE                                                               
001441         IF MFS-NEXT AND WS-RAD-IND = 1                                   
001442           MOVE WS-DARFS-NYCKEL           TO W-4490-DARFS                 
001443           MOVE MID-IDPRODNR-NEXT        TO W-4490-IDPRODNR               
001444           MOVE MID-IDPLKLST-NEXT        TO W-4490-IDPLKLST               
001445           PERFORM IMS-GNP-4490-KVAL                                      
001446         ELSE                                                             
001447           PERFORM IMS-GNP-4490                                           
001448         END-IF                                                           
001449       END-IF                                                             
001450     END-IF                                                               
001451                                                                          
001452     PERFORM UNTIL SEG-4488-SAKNAS OR                                     
001453                   WS-RAD-IND = 14                                        
001454       IF 4487-IDDC           = WS-IDDC                                   
001455                                                                          
001456         PERFORM UNTIL SEG-4490-SAKNAS OR                                 
001457                       WS-RAD-IND = 14                                    
001458           IF WS-RAD-IND = 1                                              
001459             MOVE 4490-IDPRODNR       TO MOD-IDPRODNR-ENTER               
001460             MOVE 4490-IDPLKLST       TO MOD-IDPLKLST-ENTER               
001461             MOVE 4490-DARFS          TO MOD-TIRFS-ENTER                  
001462           END-IF                                                         
001463           MOVE 4490-IDPRODNR         TO WS-SPARA-IDPRODNR                
001464           MOVE 4490-IDPLKLST         TO WS-SPARA-IDPLKLST                
001465           MOVE 4490-IDUSER           TO WS-JFR-IDANSTNR                  
001466                                                                          
001467           IF WS-JFR-IDANSTNR-5 = WS-IDANSTNR                             
001468             MOVE 'J'                     TO WS-TRAEFF-PACKARE            
001469             MOVE 4490-IDPRODNR           TO W-601-IDPRODNR               
001470             PERFORM IMS-GU-WDE401-ESEQ                                   
001471             IF SEGMENT-FINNS                                             
001472               MOVE KORD-IDDISTR           TO W-401-IDDISTR               
001473                                                 WS-SPARA-IDDISTR         
001474               MOVE KORD-IDKUNDNR          TO W-401-IDKUNDNR              
001475                                                 WS-SPARA-IDKUNDNR        
001476               MOVE KORD-IDKUNDRF          TO W-401-IDKUNDRF              
001477                                                 WS-SPARA-IDKUNDRF        
001478               MOVE WS-SPARA-IDORDNR-ALFA   TO WS-SPARA-IDORDNR           
001479               MOVE WS-SPARA-IDPRODNR       TO W-401-IDPRODNR             
001480               MOVE WS-SPARA-IDPLKLST       TO W-401-IDPLKLST             
001481               MOVE KORD-IDPRODNR          TO W-401-IDPRODNR              
001482               MOVE KORD-IDPLKLST          TO W-401-IDPLKLST              
001483               PERFORM IMS-GU-WDE401                                      
001484                                                                          
001485               MOVE KORD-IDPRODNR         TO WS-JFR-IDPRODNR              
001486               MOVE KORD-IDORDER          TO WS-SPARA-IDORDER             
001487               MOVE 'J'                   TO WS-ORDERDEL-KLAR             
001488               MOVE 'N'                   TO WS-ORDERDEL-STARTAD          
001489               PERFORM IMS-GNP-ORAD                                       
001490                                                                          
001491               IF ORAD-FINNS                                              
001492                 MOVE ORAD-IDPURAD       TO WS-START-IDPURAD              
001493                                              WS-STOPP-IDPURAD            
001494                 IF ORAD-KDRADSTA < 4                                     
001495                    MOVE 'N'             TO WS-ORDERDEL-KLAR              
001496                 END-IF                                                   
001497                                                                          
001498                 IF ORAD-KDRADSTA > 3                                     
001499                    MOVE 'J'             TO WS-ORDERDEL-STARTAD           
001500                 END-IF                                                   
001501               END-IF                                                     
001502                                                                          
001503               PERFORM UNTIL ORAD-SAKNAS                                  
001504                 PERFORM IMS-GNP-ORAD                                     
001505                                                                          
001506                 IF ORAD-FINNS                                            
001507                   MOVE ORAD-IDPURAD     TO WS-STOPP-IDPURAD              
001508                                                                          
001509                   IF ORAD-KDRADSTA < 4                                   
001510                     MOVE 'N'            TO WS-ORDERDEL-KLAR              
001511                   END-IF                                                 
001512                                                                          
001513                   IF ORAD-KDRADSTA > 3                                   
001514                      MOVE 'J'           TO WS-ORDERDEL-STARTAD           
001515                   END-IF                                                 
001516                 END-IF                                                   
001517               END-PERFORM                                                
001518                                                                          
001519               PERFORM S10-FYLL-I-RAD                                     
001520             END-IF                                                       
001521           END-IF                                                         
001522                                                                          
001523           PERFORM IMS-GNP-4490                                           
001524           MOVE 4490-IDUSER           TO WS-JFR-IDANSTNR                  
001525           MOVE 4490-DARFS (3:10)     TO MOD-TIRFS-NEXT                   
001526           MOVE 4490-IDPRODNR         TO MOD-IDPRODNR-NEXT                
001527           MOVE 4490-IDPLKLST         TO MOD-IDPLKLST-NEXT                
001528         END-PERFORM                                                      
001529       END-IF                                                             
001530                                                                          
001531       IF SEG-4490-SAKNAS OR WS-RAD-IND < 14                              
001532           PERFORM IMS-GN-4488                                            
001533           PERFORM UNTIL SEG-4488-SAKNAS                                  
001534                         OR                                               
001535                         4487-IDDC = WS-IDDC                              
001536              PERFORM IMS-GN-4488                                         
001537           END-PERFORM                                                    
001538           IF SEG-4488-FINNS                                              
001539              MOVE 4488-KDPRCGRP    TO MOD-KDPRCGRP-NEXT                  
001540             PERFORM IMS-GNP-4490                                         
001541           END-IF                                                         
001542       END-IF                                                             
001543     END-PERFORM                                                          
001544                                                                          
001545     IF NOT TRAEFF-PACKARE                                                
001546        MOVE 4  TO WS-KDFEL                                               
001547*WS-KDFEL = 4 = FEL-4 = 711. ANGIVEN PACKARE SAKNAS PÅ ORDERN             
001548     ELSE                                                                 
001549       IF WS-RAD-IND = 15 AND SEG-4490-FINNS                              
001550           PERFORM UNTIL SEG-4488-SAKNAS OR                               
001551                         WS-KDFEL        = 20                             
001552             PERFORM UNTIL  SEG-4490-SAKNAS  OR                           
001553                            WS-KDFEL     = 20                             
001554                 MOVE 4490-IDUSER    TO WS-JFR-IDANSTNR                   
001555                 IF 4487-IDDC = WS-IDDC AND                               
001556                    WS-JFR-IDANSTNR-5 = WS-IDANSTNR                       
001557                       MOVE 4490-DARFS (3:10) TO MOD-TIRFS-NEXT           
001558                       MOVE 4490-IDPRODNR  TO MOD-IDPRODNR-NEXT           
001559                       MOVE 4490-IDPLKLST  TO MOD-IDPLKLST-NEXT           
001560                   MOVE 20 TO WS-KDFEL                                    
001561*WS-KDFEL = 20 = MED-1 = 778. FLER RADER FINNS                            
001562                 ELSE                                                     
001563                   PERFORM IMS-GNP-4490                                   
001564                 END-IF                                                   
001565             END-PERFORM                                                  
001566             IF WS-KDFEL             = 20                                 
001567                 CONTINUE                                                 
001568              ELSE                                                        
001569                 PERFORM IMS-GN-4488                                      
001570                 IF SEG-4488-FINNS                                        
001571                     MOVE 4487-IDDC     TO WS-4487-IDDC                   
001572                     MOVE 4488-KDPRCGRP TO MOD-KDPRCGRP-NEXT              
001573                     PERFORM IMS-GNP-4490                                 
001574                 END-IF                                                   
001575             END-IF                                                       
001576           END-PERFORM                                                    
001577       ELSE                                                               
001578         IF SEG-4488-SAKNAS                                               
001579           MOVE ZERO                   TO MOD-IDPRODNR-NEXT               
001580           MOVE ZERO                   TO MOD-IDPLKLST-NEXT               
001581           MOVE SPACE                  TO MOD-KDPRCGRP-NEXT               
001582           MOVE ZERO                   TO MOD-TIRFS-NEXT                  
001583         END-IF                                                           
001584                                                                          
001585       END-IF                                                             
001586     END-IF                                                               
001587     .                                                                    
001588     EJECT                                                                
001589 DC-VISA-BILD SECTION.                                                    
001590     MOVE 'STA DC-VISA'  TO PGM-POS                                       
001591                                                                          
001592     MOVE 1                          TO WS-RAD-IND                        
001593     MOVE IDPRODNR-WS                TO W-411-IDPRODNR-MIN                
001594                                        W-411-IDPRODNR-MAX                
001595     MOVE 1                          TO W-411-IDPURAD-MIN                 
001596     MOVE 99999                      TO W-411-IDPURAD-MAX                 
001597     PERFORM IMS-GU-WDE411-01-BSEQ                                        
001598                                                                          
001599     IF SEGMENT-FINNS                                                     
001600       MOVE KORD-IDDISTR            TO W-4A1-IDDISTR                      
001601       MOVE KORD-IDKUNDNR           TO W-4A1-IDKUNDNR                     
001602       MOVE KORD-IDKUNDRF           TO W-4A1-IDKUNDRF                     
001603       PERFORM IMS-GU-WDE4ASEQ                                            
001604                                                                          
001605       IF KUNDORDER-SEK-FINNS                                             
001606         PERFORM UNTIL KUNDORDER-SEK-SAKNAS OR                            
001607                       WS-RAD-IND = 14                                    
001608           MOVE KORD-IDDISTR         TO W-401-IDDISTR                     
001609                                        WS-SPARA-IDDISTR                  
001610           MOVE KORD-IDKUNDNR        TO W-401-IDKUNDNR                    
001611                                        WS-SPARA-IDKUNDNR                 
001612           MOVE KORD-IDKUNDRF        TO W-401-IDKUNDRF                    
001613                                        WS-SPARA-IDKUNDRF                 
001614           MOVE WS-SPARA-IDORDNR-ALFA TO WS-SPARA-IDORDNR                 
001615           MOVE KORD-IDPRODNR        TO W-401-IDPRODNR                    
001616                                        WS-SPARA-IDPRODNR                 
001617           MOVE KORD-IDPLKLST        TO W-401-IDPLKLST                    
001618                                        WS-SPARA-IDPLKLST                 
001619           MOVE KORD-IDORDER         TO WS-SPARA-IDORDER                  
001620           PERFORM IMS-GU-WDE401                                          
001621           MOVE KORD-IDPRODNR        TO WS-JFR-IDPRODNR                   
001622           MOVE KORD-IDUSER          TO WS-JFR-IDANSTNR                   
001623                                                                          
001624           IF WS-JFR-IDPRODNR   = IDPRODNR-WS AND                         
001625              WS-JFR-IDANSTNR-5 = WS-IDANSTNR                             
001626             MOVE 'J'               TO WS-ORDERDEL-KLAR                   
001627             MOVE 'N'               TO WS-ORDERDEL-STARTAD                
001628             PERFORM IMS-GNP-ORAD                                         
001629                                                                          
001630             IF ORAD-FINNS                                                
001631               MOVE ORAD-IDPURAD   TO WS-START-IDPURAD                    
001632                                      WS-STOPP-IDPURAD                    
001633               IF ORAD-KDRADSTA < 4                                       
001634                 MOVE 'N'         TO WS-ORDERDEL-KLAR                     
001635               END-IF                                                     
001636                                                                          
001637               IF ORAD-KDRADSTA > 3                                       
001638                 MOVE 'J'         TO WS-ORDERDEL-STARTAD                  
001639               END-IF                                                     
001640             END-IF                                                       
001641                                                                          
001642             PERFORM UNTIL ORAD-SAKNAS                                    
001643               PERFORM IMS-GNP-ORAD                                       
001644                                                                          
001645               IF ORAD-FINNS                                              
001646                 MOVE ORAD-IDPURAD   TO WS-STOPP-IDPURAD                  
001647                                                                          
001648                 IF ORAD-KDRADSTA < 4                                     
001649                   MOVE 'N'         TO WS-ORDERDEL-KLAR                   
001650                 END-IF                                                   
001651                                                                          
001652                 IF ORAD-KDRADSTA > 3                                     
001653                   MOVE 'J'         TO WS-ORDERDEL-STARTAD                
001654                 END-IF                                                   
001655               END-IF                                                     
001656             END-PERFORM                                                  
001657                                                                          
001658             PERFORM S10-FYLL-I-RAD                                       
001659           END-IF                                                         
001660                                                                          
001661           PERFORM IMS-GN-WDE4ASEQ                                        
001662                                                                          
001663           IF KUNDORDER-SEK-FINNS                                         
001664             MOVE KORD-IDPRODNR     TO MOD-IDPRODNR-ENTER                 
001665             MOVE KORD-IDPLKLST     TO MOD-IDPLKLST-ENTER                 
001666           ELSE                                                           
001667              MOVE ZERO              TO MOD-IDPRODNR-ENTER                
001668                                        MOD-IDPLKLST-ENTER                
001669           END-IF                                                         
001670         END-PERFORM                                                      
001671                                                                          
001672         IF WS-RAD-IND = 15 AND SEGMENT-FINNS                             
001673            MOVE 20 TO WS-KDFEL                                           
001674         END-IF                                                           
001675       ELSE                                                               
001676        MOVE 1 TO WS-KDFEL                                                
001677       END-IF                                                             
001678     END-IF                                                               
001679     .                                                                    
001680     EJECT                                                                
001681 DD-VISA-BILD SECTION.                                                    
001682     MOVE 'STA DD-VISA'  TO PGM-POS                                       
001683                                                                          
001684     MOVE 1                          TO WS-RAD-IND                        
001685     MOVE IDPRODNR-WS                TO W-411-IDPRODNR-MIN                
001686                                        W-411-IDPRODNR-MAX                
001687     MOVE 1                          TO W-411-IDPURAD-MIN                 
001688     MOVE 99999                      TO W-411-IDPURAD-MAX                 
001689     PERFORM IMS-GU-WDE411-01-BSEQ                                        
001690                                                                          
001691     IF SEGMENT-FINNS                                                     
001692       MOVE KORD-IDDISTR            TO W-4A1-IDDISTR                      
001693       MOVE KORD-IDKUNDNR           TO W-4A1-IDKUNDNR                     
001694       MOVE KORD-IDKUNDRF           TO W-4A1-IDKUNDRF                     
001695       PERFORM IMS-GU-WDE4ASEQ                                            
001696                                                                          
001697       IF KUNDORDER-SEK-FINNS                                             
001698         PERFORM UNTIL KUNDORDER-SEK-SAKNAS OR                            
001699                       WS-RAD-IND = 14                                    
001700           MOVE KORD-IDDISTR         TO W-401-IDDISTR                     
001701                                        WS-SPARA-IDDISTR                  
001702           MOVE KORD-IDKUNDNR        TO W-401-IDKUNDNR                    
001703                                        WS-SPARA-IDKUNDNR                 
001704           MOVE KORD-IDKUNDRF        TO W-401-IDKUNDRF                    
001705                                        WS-SPARA-IDKUNDRF                 
001706           MOVE WS-SPARA-IDORDNR-ALFA TO WS-SPARA-IDORDNR                 
001707           MOVE KORD-IDPRODNR        TO W-401-IDPRODNR                    
001708                                        WS-SPARA-IDPRODNR                 
001709           MOVE KORD-IDPLKLST        TO W-401-IDPLKLST                    
001710                                        WS-SPARA-IDPLKLST                 
001711           MOVE KORD-IDORDER         TO WS-SPARA-IDORDER                  
001712           PERFORM IMS-GU-WDE401                                          
001713           MOVE KORD-IDPRODNR        TO WS-JFR-IDPRODNR                   
001714                                                                          
001715           IF WS-JFR-IDPRODNR = IDPRODNR-WS                               
001716             MOVE 'J'               TO WS-ORDERDEL-KLAR                   
001717             MOVE 'N'               TO WS-ORDERDEL-STARTAD                
001718             PERFORM IMS-GNP-ORAD                                         
001719                                                                          
001720             IF ORAD-FINNS                                                
001721               MOVE ORAD-IDPURAD   TO WS-START-IDPURAD                    
001722                                      WS-STOPP-IDPURAD                    
001723               IF ORAD-KDRADSTA < 4                                       
001724                 MOVE 'N'         TO WS-ORDERDEL-KLAR                     
001725               END-IF                                                     
001726                                                                          
001727               IF ORAD-KDRADSTA > 3                                       
001728                 MOVE 'J'         TO WS-ORDERDEL-STARTAD                  
001729               END-IF                                                     
001730             END-IF                                                       
001731                                                                          
001732             PERFORM UNTIL ORAD-SAKNAS                                    
001733               PERFORM IMS-GNP-ORAD                                       
001734                                                                          
001735               IF ORAD-FINNS                                              
001736                 MOVE ORAD-IDPURAD   TO WS-STOPP-IDPURAD                  
001737                                                                          
001738                 IF ORAD-KDRADSTA < 4                                     
001739                   MOVE 'N'         TO WS-ORDERDEL-KLAR                   
001740                 END-IF                                                   
001741                                                                          
001742                 IF ORAD-KDRADSTA > 3                                     
001743                   MOVE 'J'         TO WS-ORDERDEL-STARTAD                
001744                 END-IF                                                   
001745               END-IF                                                     
001746             END-PERFORM                                                  
001747                                                                          
001748             PERFORM S10-FYLL-I-RAD                                       
001749           END-IF                                                         
001750                                                                          
001751           PERFORM IMS-GN-WDE4ASEQ                                        
001752                                                                          
001753           IF KUNDORDER-SEK-FINNS                                         
001754              MOVE KORD-IDPRODNR     TO MOD-IDPRODNR-ENTER                
001755              MOVE KORD-IDPLKLST     TO MOD-IDPLKLST-ENTER                
001756           ELSE                                                           
001757              MOVE ZERO              TO MOD-IDPRODNR-ENTER                
001758                                        MOD-IDPLKLST-ENTER                
001759           END-IF                                                         
001760         END-PERFORM                                                      
001761                                                                          
001762         IF WS-RAD-IND = 15 AND SEGMENT-FINNS                             
001763            MOVE 20 TO WS-KDFEL                                           
001764         END-IF                                                           
001765       ELSE                                                               
001766        MOVE 1 TO WS-KDFEL                                                
001767       END-IF                                                             
001768     END-IF                                                               
001769     .                                                                    
001770     EJECT                                                                
001771 E-BEHANDLA-ENDAST-ANSTNR SECTION.                                        
001772     MOVE 'STA E-BEHA'  TO PGM-POS                                        
001773                                                                          
001774     MOVE 'N'                          TO WS-TRAEFF-PACKARE               
001775     MOVE 1                            TO WS-RAD-IND                      
001776                                                                          
001777     MOVE WS-IDDC                      TO W-4487-IDDC                     
001778                                                                          
001779     IF MFS-ENTER AND GAMLA-NYCKLAR                                       
001780        MOVE WS-MID-KDPRCGRP-ENTER     TO W-4488-KDPRCGRP                 
001781        PERFORM IMS-GU-4488                                               
001782     ELSE                                                                 
001783       IF MFS-NEXT                                                        
001784          MOVE WS-MID-KDPRCGRP-NEXT    TO W-4488-KDPRCGRP                 
001785          PERFORM IMS-GU-4488                                             
001786       ELSE                                                               
001787          MOVE LOW-VALUE               TO W-4488-KDPRCGRP                 
001788          PERFORM IMS-GU-4488-FIRST                                       
001789       END-IF                                                             
001790     END-IF                                                               
001791     IF SEG-4488-FINNS                                                    
001792       MOVE 4488-KDPRCGRP               TO MOD-KDPRCGRP-ENTER             
001793                                           MOD-KDPRCGRP-NEXT              
001794       IF MFS-ENTER AND GAMLA-NYCKLAR                                     
001795           MOVE WS-DARFS-NYCKEL           TO W-4490-DARFS                 
001796           MOVE MID-IDPRODNR-ENTER       TO W-4490-IDPRODNR               
001797           MOVE MID-IDPLKLST-ENTER       TO W-4490-IDPLKLST               
001798           PERFORM IMS-GNP-4490-KVAL                                      
001799       ELSE                                                               
001800         IF MFS-NEXT AND WS-RAD-IND = 1                                   
001801           MOVE WS-DARFS-NYCKEL           TO W-4490-DARFS                 
001802           MOVE MID-IDPRODNR-NEXT        TO W-4490-IDPRODNR               
001803           MOVE MID-IDPLKLST-NEXT        TO W-4490-IDPLKLST               
001804           PERFORM IMS-GNP-4490-KVAL                                      
001805         ELSE                                                             
001806           PERFORM IMS-GNP-4490                                           
001807         END-IF                                                           
001808       END-IF                                                             
001809     END-IF                                                               
001810                                                                          
001811     PERFORM UNTIL SEG-4488-SAKNAS OR                                     
001812                   WS-RAD-IND = 14                                        
001813       IF 4487-IDDC           = WS-IDDC                                   
001814                                                                          
001815         PERFORM UNTIL SEG-4490-SAKNAS OR                                 
001816                       WS-RAD-IND = 14                                    
001817           IF WS-RAD-IND = 1                                              
001818             MOVE 4490-IDPRODNR       TO MOD-IDPRODNR-ENTER               
001819             MOVE 4490-IDPLKLST       TO MOD-IDPLKLST-ENTER               
001820             MOVE 4490-DARFS          TO MOD-TIRFS-ENTER                  
001821           END-IF                                                         
001822           MOVE 4490-IDPRODNR         TO WS-SPARA-IDPRODNR                
001823           MOVE 4490-IDPLKLST         TO WS-SPARA-IDPLKLST                
001824           MOVE 4490-IDUSER           TO WS-JFR-IDANSTNR                  
001825                                                                          
001826           IF WS-JFR-IDANSTNR-5 = WS-IDANSTNR                             
001827             MOVE 'J'                     TO WS-TRAEFF-PACKARE            
001828             MOVE 4490-IDPRODNR           TO W-601-IDPRODNR               
001829             PERFORM IMS-GU-WDE401-ESEQ                                   
001830             IF SEGMENT-FINNS                                             
001831               MOVE KORD-IDDISTR           TO W-401-IDDISTR               
001832                                                 WS-SPARA-IDDISTR         
001833               MOVE KORD-IDKUNDNR          TO W-401-IDKUNDNR              
001834                                                 WS-SPARA-IDKUNDNR        
001835               MOVE KORD-IDKUNDRF          TO W-401-IDKUNDRF              
001836                                                 WS-SPARA-IDKUNDRF        
001837               MOVE WS-SPARA-IDORDNR-ALFA   TO WS-SPARA-IDORDNR           
001838               MOVE WS-SPARA-IDPRODNR       TO W-401-IDPRODNR             
001839               MOVE WS-SPARA-IDPLKLST       TO W-401-IDPLKLST             
001840               MOVE KORD-IDPRODNR          TO W-401-IDPRODNR              
001841               MOVE KORD-IDPLKLST          TO W-401-IDPLKLST              
001842               PERFORM IMS-GU-WDE401                                      
001843                                                                          
001844               MOVE KORD-IDPRODNR         TO WS-JFR-IDPRODNR              
001845               MOVE KORD-IDORDER          TO WS-SPARA-IDORDER             
001846               MOVE 'J'                   TO WS-ORDERDEL-KLAR             
001847               MOVE 'N'                   TO WS-ORDERDEL-STARTAD          
001848               PERFORM IMS-GNP-ORAD                                       
001849                                                                          
001850               IF ORAD-FINNS                                              
001851                 MOVE ORAD-IDPURAD       TO WS-START-IDPURAD              
001852                                              WS-STOPP-IDPURAD            
001853                 IF ORAD-KDRADSTA < 4                                     
001854                    MOVE 'N'             TO WS-ORDERDEL-KLAR              
001855                 END-IF                                                   
001856                                                                          
001857                 IF ORAD-KDRADSTA > 3                                     
001858                    MOVE 'J'             TO WS-ORDERDEL-STARTAD           
001859                 END-IF                                                   
001860               END-IF                                                     
001861                                                                          
001862               PERFORM UNTIL ORAD-SAKNAS                                  
001863                 PERFORM IMS-GNP-ORAD                                     
001864                                                                          
001865                 IF ORAD-FINNS                                            
001866                   MOVE ORAD-IDPURAD     TO WS-STOPP-IDPURAD              
001867                                                                          
001868                   IF ORAD-KDRADSTA < 4                                   
001869                     MOVE 'N'            TO WS-ORDERDEL-KLAR              
001870                   END-IF                                                 
001871                                                                          
001872                   IF ORAD-KDRADSTA > 3                                   
001873                      MOVE 'J'           TO WS-ORDERDEL-STARTAD           
001874                   END-IF                                                 
001875                 END-IF                                                   
001876               END-PERFORM                                                
001877                                                                          
001878               PERFORM S10-FYLL-I-RAD                                     
001879             END-IF                                                       
001880           END-IF                                                         
001881                                                                          
001882           PERFORM IMS-GNP-4490                                           
001883           MOVE 4490-IDUSER           TO WS-JFR-IDANSTNR                  
001884           MOVE 4490-DARFS (3:10)     TO MOD-TIRFS-NEXT                   
001885           MOVE 4490-IDPRODNR         TO MOD-IDPRODNR-NEXT                
001886           MOVE 4490-IDPLKLST         TO MOD-IDPLKLST-NEXT                
001887         END-PERFORM                                                      
001888       END-IF                                                             
001889                                                                          
001890       IF SEG-4490-SAKNAS OR WS-RAD-IND < 14                              
001891           PERFORM IMS-GN-4488                                            
001892           PERFORM UNTIL SEG-4488-SAKNAS                                  
001893                         OR                                               
001894                         4487-IDDC = WS-IDDC                              
001895              PERFORM IMS-GN-4488                                         
001896           END-PERFORM                                                    
001897           IF SEG-4488-FINNS                                              
001898              MOVE 4488-KDPRCGRP    TO MOD-KDPRCGRP-NEXT                  
001899             PERFORM IMS-GNP-4490                                         
001900           END-IF                                                         
001901       END-IF                                                             
001902     END-PERFORM                                                          
001903                                                                          
001904     IF NOT TRAEFF-PACKARE                                                
001905        MOVE 5  TO WS-KDFEL                                               
001906*WS-KDFEL = 5 = FEL-5 = 712. INGA UTEST. ORDERDELAR FÖR PACKARE           
001907     ELSE                                                                 
001908       IF WS-RAD-IND = 15 AND SEG-4490-FINNS                              
001909           PERFORM UNTIL SEG-4488-SAKNAS OR                               
001910                         WS-KDFEL        = 20                             
001911             PERFORM UNTIL  SEG-4490-SAKNAS  OR                           
001912                            WS-KDFEL     = 20                             
001913                 MOVE 4490-IDUSER    TO WS-JFR-IDANSTNR                   
001914                 IF 4487-IDDC = WS-IDDC AND                               
001915                    WS-JFR-IDANSTNR-5 = WS-IDANSTNR                       
001916                       MOVE 4490-DARFS (3:10) TO MOD-TIRFS-NEXT           
001917                       MOVE 4490-IDPRODNR  TO MOD-IDPRODNR-NEXT           
001918                       MOVE 4490-IDPLKLST  TO MOD-IDPLKLST-NEXT           
001919                   MOVE 20 TO WS-KDFEL                                    
001920*WS-KDFEL = 20 = MED-1 = 778. FLER RADER FINNS                            
001921                 ELSE                                                     
001922                   PERFORM IMS-GNP-4490                                   
001923                 END-IF                                                   
001924             END-PERFORM                                                  
001925             IF WS-KDFEL             = 20                                 
001926                 CONTINUE                                                 
001927              ELSE                                                        
001928                 PERFORM IMS-GN-4488                                      
001929                 IF SEG-4488-FINNS                                        
001930                     MOVE 4487-IDDC     TO WS-4487-IDDC                   
001931                     MOVE 4488-KDPRCGRP TO MOD-KDPRCGRP-NEXT              
001932                     PERFORM IMS-GNP-4490                                 
001933                 END-IF                                                   
001934             END-IF                                                       
001935           END-PERFORM                                                    
001936       ELSE                                                               
001937         IF SEG-4488-SAKNAS                                               
001938           MOVE ZERO                   TO MOD-IDPRODNR-NEXT               
001939           MOVE ZERO                   TO MOD-IDPLKLST-NEXT               
001940           MOVE SPACE                  TO MOD-KDPRCGRP-NEXT               
001941           MOVE ZERO                   TO MOD-TIRFS-NEXT                  
001942         END-IF                                                           
001943                                                                          
001944       END-IF                                                             
001945     END-IF                                                               
001946     .                                                                    
001947     EJECT                                                                
001948 F-BEHANDLA-ANSTNR-ORDERID SECTION.                                       
001949     MOVE 'STA F-BEHA'  TO PGM-POS                                        
001950                                                                          
001951     MOVE IDPRODNR-WS                TO W-411-IDPRODNR-MIN                
001952                                        W-411-IDPRODNR-MAX                
001953     MOVE 1                          TO W-411-IDPURAD-MIN                 
001954     MOVE 99999                      TO W-411-IDPURAD-MAX                 
001955     PERFORM IMS-GU-WDE411-01-BSEQ                                        
001956                                                                          
001957     IF SEGMENT-FINNS                                                     
001958       MOVE 1                       TO WS-RAD-IND                         
001959       MOVE KORD-IDDISTR            TO W-4A1-IDDISTR                      
001960       MOVE KORD-IDKUNDNR           TO W-4A1-IDKUNDNR                     
001961       MOVE KORD-IDKUNDRF           TO W-4A1-IDKUNDRF                     
001962       PERFORM IMS-GU-WDE4ASEQ                                            
001963                                                                          
001964       IF MFS-IDPFK = '8'                                                 
001965          PERFORM UNTIL (KUNDORDER-SEK-SAKNAS)              OR            
001966                        (KORD-IDPRODNR = WS-MID-IDPRODNR-NEXT AND         
001967                         KORD-IDPLKLST = WS-MID-IDPLKLST-NEXT)            
001968            PERFORM IMS-GN-WDE4ASEQ                                       
001969          END-PERFORM                                                     
001970       END-IF                                                             
001971                                                                          
001972       PERFORM UNTIL KUNDORDER-SEK-SAKNAS OR                              
001973                     WS-RAD-IND = 14                                      
001974         MOVE KORD-IDDISTR            TO W-401-IDDISTR                    
001975                                         WS-SPARA-IDDISTR                 
001976         MOVE KORD-IDKUNDNR           TO W-401-IDKUNDNR                   
001977                                         WS-SPARA-IDKUNDNR                
001978         MOVE KORD-IDKUNDRF           TO W-401-IDKUNDRF                   
001979                                         WS-SPARA-IDKUNDRF                
001980         MOVE WS-SPARA-IDORDNR-ALFA   TO WS-SPARA-IDORDNR                 
001981         MOVE KORD-IDPRODNR           TO W-401-IDPRODNR                   
001982                                         WS-SPARA-IDPRODNR                
001983         MOVE KORD-IDPLKLST           TO W-401-IDPLKLST                   
001984                                         WS-SPARA-IDPLKLST                
001985         MOVE KORD-IDORDER            TO WS-SPARA-IDORDER                 
001986         PERFORM IMS-GU-WDE401                                            
001987                                                                          
001988         IF SEGMENT-FINNS                                                 
001989            MOVE KORD-IDPRODNR        TO WS-JFR-IDPRODNR                  
001990            MOVE KORD-IDUSER          TO WS-JFR-IDANSTNR                  
001991         ELSE                                                             
001992            MOVE ZERO                 TO WS-JFR-IDPRODNR                  
001993            MOVE ZERO                 TO WS-JFR-IDANSTNR                  
001994         END-IF                                                           
001995                                                                          
001996         IF SEGMENT-FINNS                  AND                            
001997           WS-JFR-IDPRODNR   = IDPRODNR-WS AND                            
001998           WS-JFR-IDANSTNR-5 = WS-IDANSTNR                                
001999           MOVE 'J'                     TO WS-ORDERDEL-KLAR               
002000           MOVE 'N'                     TO WS-ORDERDEL-STARTAD            
002001           PERFORM IMS-GNP-ORAD                                           
002002                                                                          
002003           IF ORAD-FINNS                                                  
002004             MOVE ORAD-IDPURAD         TO WS-START-IDPURAD                
002005                                          WS-STOPP-IDPURAD                
002006             IF ORAD-KDRADSTA < 4                                         
002007               MOVE 'N'               TO WS-ORDERDEL-KLAR                 
002008             END-IF                                                       
002009                                                                          
002010             IF ORAD-KDRADSTA > 3                                         
002011               MOVE 'J'               TO WS-ORDERDEL-STARTAD              
002012             END-IF                                                       
002013           END-IF                                                         
002014                                                                          
002015           PERFORM UNTIL ORAD-SAKNAS                                      
002016             PERFORM IMS-GNP-ORAD                                         
002017                                                                          
002018             IF ORAD-FINNS                                                
002019               MOVE ORAD-IDPURAD        TO WS-STOPP-IDPURAD               
002020               IF ORAD-KDRADSTA < 4                                       
002021                 MOVE 'N'               TO WS-ORDERDEL-KLAR               
002022               END-IF                                                     
002023                                                                          
002024               IF ORAD-KDRADSTA > 3                                       
002025                 MOVE 'J'               TO WS-ORDERDEL-STARTAD            
002026               END-IF                                                     
002027             END-IF                                                       
002028           END-PERFORM                                                    
002029                                                                          
002030           PERFORM S10-FYLL-I-RAD                                         
002031         END-IF                                                           
002032                                                                          
002033         PERFORM IMS-GN-WDE4ASEQ                                          
002034                                                                          
002035         IF KUNDORDER-SEK-FINNS                                           
002036            MOVE KORD-IDPRODNR           TO MOD-IDPRODNR-NEXT             
002037            MOVE KORD-IDPLKLST           TO MOD-IDPLKLST-NEXT             
002038         END-IF                                                           
002039       END-PERFORM                                                        
002040                                                                          
002041       IF WS-RAD-IND = 15 AND SEGMENT-FINNS                               
002042          MOVE 20 TO WS-KDFEL                                             
002043       END-IF                                                             
002044     ELSE                                                                 
002045        MOVE 1 TO WS-KDFEL                                                
002046     END-IF                                                               
002047     .                                                                    
002048     EJECT                                                                
002049 G-BEHANDLA-ENDAST-ORDERID SECTION.                                       
002050     MOVE 'STA G-BEHA'  TO PGM-POS                                        
002051                                                                          
002052     MOVE IDPRODNR-WS                TO W-411-IDPRODNR-MIN                
002053                                        W-411-IDPRODNR-MAX                
002054     MOVE 1                          TO W-411-IDPURAD-MIN                 
002055     MOVE 99999                      TO W-411-IDPURAD-MAX                 
002056     PERFORM IMS-GU-WDE411-01-BSEQ                                        
002057                                                                          
002058     IF SEGMENT-FINNS                                                     
002059       MOVE 1                       TO WS-RAD-IND                         
002060       MOVE KORD-IDDISTR            TO W-4A1-IDDISTR                      
002061       MOVE KORD-IDKUNDNR           TO W-4A1-IDKUNDNR                     
002062       MOVE KORD-IDKUNDRF           TO W-4A1-IDKUNDRF                     
002063       PERFORM IMS-GU-WDE4ASEQ                                            
002064                                                                          
002065       IF MFS-IDPFK = '8'                                                 
002066         PERFORM UNTIL (KUNDORDER-SEK-SAKNAS)              OR             
002067                       (KORD-IDPRODNR = WS-MID-IDPRODNR-NEXT AND          
002068                        KORD-IDPLKLST = WS-MID-IDPLKLST-NEXT)             
002069           PERFORM IMS-GN-WDE4ASEQ                                        
002070         END-PERFORM                                                      
002071       END-IF                                                             
002072                                                                          
002073       PERFORM UNTIL KUNDORDER-SEK-SAKNAS OR                              
002074                     WS-RAD-IND = 14                                      
002075         MOVE KORD-IDDISTR            TO W-401-IDDISTR                    
002076                                         WS-SPARA-IDDISTR                 
002077         MOVE KORD-IDKUNDNR           TO W-401-IDKUNDNR                   
002078                                         WS-SPARA-IDKUNDNR                
002079         MOVE KORD-IDKUNDRF           TO W-401-IDKUNDRF                   
002080                                         WS-SPARA-IDKUNDRF                
002081         MOVE WS-SPARA-IDORDNR-ALFA   TO WS-SPARA-IDORDNR                 
002082         MOVE KORD-IDPRODNR           TO W-401-IDPRODNR                   
002083                                         WS-SPARA-IDPRODNR                
002084         MOVE KORD-IDPLKLST           TO W-401-IDPLKLST                   
002085                                         WS-SPARA-IDPLKLST                
002086         MOVE KORD-IDORDER            TO WS-SPARA-IDORDER                 
002087         PERFORM IMS-GU-WDE401                                            
002088                                                                          
002089         IF SEGMENT-FINNS                                                 
002090            MOVE KORD-IDPRODNR           TO WS-JFR-IDPRODNR               
002091         ELSE                                                             
002092            MOVE ZERO                    TO WS-JFR-IDPRODNR               
002093         END-IF                                                           
002094                                                                          
002095         IF SEGMENT-FINNS                 AND                             
002096           WS-JFR-IDPRODNR = IDPRODNR-WS                                  
002097           MOVE 'N'                     TO WS-ORDERDEL-STARTAD            
002098           MOVE 'J'                     TO WS-ORDERDEL-KLAR               
002099           PERFORM IMS-GNP-ORAD                                           
002100                                                                          
002101           IF ORAD-FINNS                                                  
002102             MOVE ORAD-IDPURAD         TO WS-START-IDPURAD                
002103                                           WS-STOPP-IDPURAD               
002104             IF ORAD-KDRADSTA < 4                                         
002105                MOVE 'N'               TO WS-ORDERDEL-KLAR                
002106             END-IF                                                       
002107                                                                          
002108             IF ORAD-KDRADSTA > 3                                         
002109                MOVE 'J'               TO WS-ORDERDEL-STARTAD             
002110             END-IF                                                       
002111           END-IF                                                         
002112                                                                          
002113           PERFORM UNTIL ORAD-SAKNAS                                      
002114             PERFORM IMS-GNP-ORAD                                         
002115                                                                          
002116             IF ORAD-FINNS                                                
002117               MOVE ORAD-IDPURAD         TO WS-STOPP-IDPURAD              
002118                                                                          
002119               IF ORAD-KDRADSTA < 4                                       
002120                  MOVE 'N'               TO WS-ORDERDEL-KLAR              
002121               END-IF                                                     
002122                                                                          
002123               IF ORAD-KDRADSTA > 3                                       
002124                  MOVE 'J'               TO WS-ORDERDEL-STARTAD           
002125               END-IF                                                     
002126             END-IF                                                       
002127           END-PERFORM                                                    
002128                                                                          
002129           PERFORM S10-FYLL-I-RAD                                         
002130         END-IF                                                           
002131                                                                          
002132         PERFORM IMS-GN-WDE4ASEQ                                          
002133                                                                          
002134         IF KUNDORDER-SEK-FINNS                                           
002135            MOVE KORD-IDPRODNR        TO MOD-IDPRODNR-NEXT                
002136            MOVE KORD-IDPLKLST        TO MOD-IDPLKLST-NEXT                
002137         END-IF                                                           
002138       END-PERFORM                                                        
002139                                                                          
002140       IF WS-RAD-IND = 15 AND SEGMENT-FINNS                               
002141          MOVE 20 TO WS-KDFEL                                             
002142       END-IF                                                             
002143     ELSE                                                                 
002144        MOVE 1 TO WS-KDFEL                                                
002145     END-IF                                                               
002146     .                                                                    
002147     EJECT                                                                
002148 H-KOLLA-OM-FELTRYCK SECTION.                                             
002149     MOVE 'STA H-KOLL'  TO PGM-POS                                        
002150                                                                          
002151     MOVE JA TO INDATA-SW                                                 
002152                                                                          
002153     IF FRAN-EGEN-BILD                                                    
002154     OR FRAN-4324-BILD                                                    
002155        MOVE 1 TO INDX                                                    
002156        PERFORM UNTIL INDX = 14                                           
002157          IF MID-IDANSTNR-NEW (INDX) NOT = ALL '+'                        
002158             MOVE NEJ TO INDATA-SW                                        
002159          END-IF                                                          
002160          ADD 1 TO INDX                                                   
002161        END-PERFORM                                                       
002162     END-IF                                                               
002163                                                                          
002164     IF INDATA-FEL                                                        
002165        MOVE 19 TO WS-KDFEL                                               
002166        MOVE MFS-ROER-EJ-FAELT TO MOD-IDPRODNR-NEXT                       
002167                                  MOD-IDPLKLST-NEXT                       
002168                                  MOD-KDPRCGRP-NEXT                       
002169                                  MOD-TIRFS-NEXT                          
002170        MOVE 1 TO INDX                                                    
002171        PERFORM UNTIL INDX = 14                                           
002172           MOVE MFS-ADD-LAES-IN-FAELT                                     
002173                               TO MOD-IDPRODNR-ATTR        (INDX)         
002174                                  MOD-IDRADNR-ORD-TOM-ATTR (INDX)         
002175                                  MOD-IDANSTNR-ATTR        (INDX)         
002176                                  MOD-IDANSTNR-NEW-ATTR    (INDX)         
002177           ADD 1 TO INDX                                                  
002178        END-PERFORM                                                       
002179     END-IF                                                               
002180     .                                                                    
002181     EJECT                                                                
002182 J-HAMTA-MEDDELANDE SECTION.                                              
002183                                                                          
002184     MOVE 'STA J-HAMTA'  TO PGM-POS                                       
002185     EVALUATE WS-KDFEL                                                    
002186       WHEN  1 MOVE FEL-1  (SPRAK-INDX) TO MOD-TEMFSFEL                   
002187       WHEN  2 MOVE FEL-2  (SPRAK-INDX) TO MOD-TEMFSFEL                   
002188       WHEN  3 MOVE FEL-3  (SPRAK-INDX) TO MOD-TEMFSFEL                   
002189       WHEN  4 MOVE FEL-4  (SPRAK-INDX) TO MOD-TEMFSFEL                   
002190       WHEN  5 MOVE FEL-5  (SPRAK-INDX) TO MOD-TEMFSFEL                   
002191       WHEN  8 MOVE FEL-8  (SPRAK-INDX) TO MOD-TEMFSFEL                   
002192       WHEN  9 MOVE FEL-9  (SPRAK-INDX) TO MOD-TEMFSFEL                   
002193       WHEN 10 MOVE FEL-10 (SPRAK-INDX) TO MOD-TEMFSFEL                   
002194       WHEN 11 MOVE FEL-11 (SPRAK-INDX) TO MOD-TEMFSFEL                   
002195       WHEN 12 MOVE FEL-12 (SPRAK-INDX) TO MOD-TEMFSFEL                   
002196       WHEN 13 MOVE FEL-13 (SPRAK-INDX) TO MOD-TEMFSFEL                   
002197       WHEN 14 MOVE FEL-14 (SPRAK-INDX) TO MOD-TEMFSFEL                   
002198       WHEN 16 MOVE FEL-16 (SPRAK-INDX) TO MOD-TEMFSFEL                   
002199       WHEN 17 MOVE FEL-17 (SPRAK-INDX) TO MOD-TEMFSFEL                   
002200       WHEN 18 MOVE FEL-18 (SPRAK-INDX) TO MOD-TEMFSFEL                   
002201       WHEN 19 MOVE FEL-19 (SPRAK-INDX) TO MOD-TEMFSFEL                   
002202       WHEN 20 MOVE MED-1  (SPRAK-INDX) TO MOD-TEMFSFEL                   
002203       WHEN 21 MOVE MED-2  (SPRAK-INDX) TO MOD-TEMFSFEL                   
002204     END-EVALUATE                                                         
002205     .                                                                    
002206     EJECT                                                                
002207 K-VISA-BILD-IGEN SECTION.                                                
002208                                                                          
002209     MOVE 'STA K-VISA '  TO PGM-POS                                       
002210     MOVE 1 TO MOD-RAD-IND                                                
002211     PERFORM UNTIL MOD-RAD-IND = 14                                       
002212       MOVE MFS-ROER-EJ-FAELT TO MOD-IDDISTR      (MOD-RAD-IND)           
002213                                 MOD-IDKUNDNR     (MOD-RAD-IND)           
002214                                 MOD-IDORDNR      (MOD-RAD-IND)           
002215                                 MOD-IDPRODNR     (MOD-RAD-IND)           
002216                                 MOD-IDPLKLST     (MOD-RAD-IND)           
002217                             MOD-IDRADNR-ORD-FROM (MOD-RAD-IND)           
002218                             MOD-IDRADNR-ORD-TOM  (MOD-RAD-IND)           
002219                                 MOD-IDANSTNR     (MOD-RAD-IND)           
002220                                 MOD-IDANSTNR-NEW (MOD-RAD-IND)           
002221       ADD 1 TO MOD-RAD-IND                                               
002222     END-PERFORM                                                          
002223     .                                                                    
002224     EJECT                                                                
002225 L-TOM-SKAERM SECTION.                                                    
002226                                                                          
002227     MOVE 'STA L-TOM  '  TO PGM-POS                                       
002228     MOVE MFS-RENSA-FAELT                TO MOD-TEMFSFEL                  
002229                                            MOD-IDANSTNR-IN               
002230                                            MOD-IDANSTNR-UT               
002231                                            MOD-IDDISTR-IN                
002232                                            MOD-IDDISTR-UT                
002233                                            MOD-IDKUNDNR-IN               
002234                                            MOD-IDKUNDNR-UT               
002235                                            MOD-IDORDNR-IN                
002236                                            MOD-IDORDNR-UT                
002237                                            MOD-IDKOLLI-IN                
002238                                            MOD-IDKOLLI-UT                
002239                                            MOD-IDPRODNR-IN               
002240                                            MOD-IDPRODNR-UT               
002241                                            MOD-TEMFSINF                  
002242     MOVE 1 TO MOD-RAD-IND                                                
002243     PERFORM UNTIL MOD-RAD-IND = 14                                       
002244       MOVE MFS-RENSA-FAELT   TO MOD-IDDISTR      (MOD-RAD-IND)           
002245                                 MOD-IDKUNDNR     (MOD-RAD-IND)           
002246                                 MOD-IDORDNR      (MOD-RAD-IND)           
002247                                 MOD-IDPRODNR     (MOD-RAD-IND)           
002248                                 MOD-IDPLKLST     (MOD-RAD-IND)           
002249                             MOD-IDRADNR-ORD-FROM (MOD-RAD-IND)           
002250                             MOD-KDASTERISK       (MOD-RAD-IND)           
002251                             MOD-IDRADNR-ORD-TOM  (MOD-RAD-IND)           
002252                                 MOD-IDANSTNR     (MOD-RAD-IND)           
002253                                 MOD-IDANSTNR-NEW (MOD-RAD-IND)           
002254       ADD 1 TO MOD-RAD-IND                                               
002255     END-PERFORM                                                          
002256     .                                                                    
002257     EJECT                                                                
002258 M-RETURN-TO-4324 SECTION.                                                
002259                                                                          
002260     MOVE '++++'               TO PTOP-MID-IDDISTR-IN                     
002261     MOVE '++++++'             TO PTOP-MID-IDKUNDNR-IN                    
002262     MOVE '+++++'              TO PTOP-MID-IDORDNR-IN                     
002263     MOVE '+'                  TO PTOP-MID-KDORDKL-IN                     
002264     MOVE '+++++++'            TO PTOP-MID-IDPRODNR-IN                    
002265                                                                          
002266     MOVE MID-KDPRCGRP-4324    TO PTOP-MID-KDPRCGRP-IN                    
002267     MOVE WS-IDDC              TO PTOP-MID-IDDC-IN                        
002268                                                                          
002269     MOVE MFS-KDMFSFOR         TO PTOP-KDMFSFOR                           
002270     COMPUTE PTOP-LL = LENGTH OF PTOP-MID-W4I32401 + 17                   
002271     .                                                                    
002272     EJECT                                                                
002273 S10-FYLL-I-RAD SECTION.                                                  
002274     MOVE 'STA S10-FYLL'  TO PGM-POS                                      
002275                                                                          
002276     MOVE WS-SPARA-IDPRODNR    TO W-601-IDPRODNR                          
002277     PERFORM IMS-GU-WDE601                                                
002278                                                                          
002279     PERFORM S10A-KOLLA-OM-ORDER-KLAR                                     
002280                                                                          
002281     IF VISA-EJ-RAD-PA-BILD                                               
002282        OR                                                                
002283        VORD-IDDC       NOT = WS-IDDC                                     
002284       CONTINUE                                                           
002285     ELSE                                                                 
002286       MOVE WS-SPARA-IDDISTR     TO MOD-IDDISTR  (WS-RAD-IND)             
002287       MOVE WS-SPARA-IDKUNDNR    TO MOD-IDKUNDNR (WS-RAD-IND)             
002288       MOVE WS-SPARA-IDORDNR     TO MOD-IDORDNR  (WS-RAD-IND)             
002289       MOVE WS-SPARA-IDPRODNR    TO MOD-IDPRODNR (WS-RAD-IND)             
002290       MOVE WS-SPARA-IDPLKLST    TO MOD-IDPLKLST (WS-RAD-IND)             
002291       IF ENDAST-ANSTNR                                                   
002292         MOVE WS-JFR-IDANSTNR-5  TO MOD-IDANSTNR (WS-RAD-IND)             
002293       ELSE                                                               
002294         MOVE KORD-IDUSER        TO WS-JFR-IDANSTNR                       
002295         MOVE WS-JFR-IDANSTNR-5  TO MOD-IDANSTNR (WS-RAD-IND)             
002296       END-IF                                                             
002297                                                                          
002298       IF  ORDERDEL-KLAR                                                  
002299           IF ORDERDEL-STATUS-PACKAD                                      
002300              MOVE '**'            TO MOD-KDASTERISK (WS-RAD-IND)         
002301           ELSE                                                           
002302              MOVE '*'             TO MOD-KDASTERISK (WS-RAD-IND)         
002303           END-IF                                                         
002304       ELSE                                                               
002305         IF  ORDERDEL-STARTAD                                             
002306             MOVE '* '             TO MOD-KDASTERISK (WS-RAD-IND)         
002307         ELSE                                                             
002308             MOVE MFS-RENSA-FAELT  TO MOD-KDASTERISK (WS-RAD-IND)         
002309         END-IF                                                           
002310       END-IF                                                             
002311                                                                          
002312       MOVE WS-START-IDPURAD  TO MOD-IDRADNR-ORD-FROM (WS-RAD-IND)        
002313       MOVE WS-STOPP-IDPURAD  TO MOD-IDRADNR-ORD-TOM  (WS-RAD-IND)        
002314       ADD 1 TO WS-RAD-IND                                                
002315     END-IF                                                               
002316     .                                                                    
002317     EJECT                                                                
002318 S10A-KOLLA-OM-ORDER-KLAR SECTION.                                        
002319                                                                          
002320     MOVE 'STA S10A-KOLL'  TO PGM-POS                                     
002321     MOVE NEJ TO WS-VISA-RAD                                              
002322                 WS-ORDERDEL-STATUS                                       
002323     MOVE WS-SPARA-IDORDER  TO W-IDORDER-WDQ3-MIN                         
002324                               W-IDORDER-WDQ3-MAX                         
002325     MOVE WS-IDDC           TO W-IDDC-WDQ3-MIN                            
002326                               W-IDDC-WDQ3-MAX                            
002327     MOVE WS-SPARA-IDPRODNR TO W-IDPRODNR-WDQ3-MIN                        
002328                               W-IDPRODNR-WDQ3-MAX                        
002329                                                                          
002330     PERFORM IMS-GU-ORQA-WDQ301                                           
002331                                                                          
002332     PERFORM UNTIL SEGMENT-SAKNAS                                         
002333             OR                                                           
002334            (VISA-RAD-PA-BILD                                             
002335             AND                                                          
002336             ODEL-IDPLKLST > WS-SPARA-IDPLKLST)                           
002337        IF SEGMENT-FINNS                                                  
002338           IF ODEL-KDODELSTA NOT = 'P'                                    
002339              IF (ODEL-IDLEVNR = '1441 ' OR 'BP2TW')                      
002340              AND ODEL-IDPRC = '9998'                                     
002341**SOFTWARE KONTROLL                                                       
002342                 CONTINUE                                                 
002343              ELSE                                                        
002344                 MOVE JA TO WS-VISA-RAD                                   
002345              END-IF                                                      
002346           END-IF                                                         
002347           IF ODEL-KDODELSTA = 'P'                                        
002348              AND                                                         
002349              ODEL-IDPLKLST = WS-SPARA-IDPLKLST                           
002350              MOVE JA TO WS-ORDERDEL-STATUS                               
002351           END-IF                                                         
002352           PERFORM IMS-GN-ORQA-WDQ301                                     
002353        END-IF                                                            
002354     END-PERFORM                                                          
002355     .                                                                    
002356     EJECT                                                                
002357 S20-RENSA-MOD-RADER SECTION.                                             
002358                                                                          
002359                                                                          
002360     MOVE 'STA S20-RENSA'  TO PGM-POS                                     
002361     MOVE 1 TO MOD-RAD-IND                                                
002362     PERFORM UNTIL MOD-RAD-IND = 14                                       
002363       MOVE MFS-RENSA-FAELT TO  MOD-IDDISTR       (MOD-RAD-IND)           
002364                                MOD-IDKUNDNR      (MOD-RAD-IND)           
002365                                MOD-IDORDNR       (MOD-RAD-IND)           
002366                                MOD-IDPRODNR      (MOD-RAD-IND)           
002367                                MOD-IDPLKLST      (MOD-RAD-IND)           
002368                            MOD-IDRADNR-ORD-FROM  (MOD-RAD-IND)           
002369                            MOD-KDASTERISK        (MOD-RAD-IND)           
002370                            MOD-IDRADNR-ORD-TOM   (MOD-RAD-IND)           
002371                                MOD-IDANSTNR      (MOD-RAD-IND)           
002372                                MOD-IDANSTNR-NEW  (MOD-RAD-IND)           
002373       ADD 1 TO MOD-RAD-IND                                               
002374     END-PERFORM                                                          
002375     .                                                                    
002376     EJECT                                                                
002377* IMS SEKTIONER                                                           
002378     SKIP3                                                                
002379 IMS-GET-MSG SECTION.                                                     
002380                                                                          
002381     MOVE '  QC' TO GODK-STATUSKODER                                      
002382     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
002383     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
002384     PERFORM IMS-STATUSKONTROLL                                           
002385     .                                                                    
002386     SKIP3                                                                
002387 IMS-INSERT-MSG SECTION.                                                  
002388                                                                          
002389     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
002390       MOVE '0' TO MFS-KDHUVOMR                                           
002391     END-IF                                                               
002392     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
002393     MOVE SPACE TO GODK-STATUSKODER                                       
002394     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
002395     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
002396     PERFORM IMS-STATUSKONTROLL                                           
002397     .                                                                    
002398     EJECT                                                                
002399 IMS-ISRT-ALT-MSG SECTION.                                                
002400                                                                          
002401     MOVE SPACE TO GODK-STATUSKODER                                       
002402     CALL CBLTDLI USING ISRT ALT-PCB P-TO-P-SW                            
002403     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
002404     PERFORM IMS-STATUSKONTROLL                                           
002405     .                                                                    
002406     SKIP3                                                                
002407 IMS-GU-WDE401 SECTION.                                                   
002408     SKIP2                                                                
002409     STRING 'WDE401  (WDE401KY =' W-WDE401-KUNDORDER-X ')'                
002410            DELIMITED BY SIZE INTO SSA1                                   
002411     MOVE '  GE' TO GODK-STATUSKODER                                      
002412     CALL CBLTDLI USING GU WDE4-PCB DLI-IO-E401 SSA1                      
002413     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
002414     PERFORM IMS-STATUSKONTROLL                                           
002415     .                                                                    
002416     SKIP3                                                                
002417 IMS-GHU-WDE401 SECTION.                                                  
002418     SKIP2                                                                
002419     STRING 'WDE401  (WDE401KY =' W-WDE401-KUNDORDER-X ')'                
002420            DELIMITED BY SIZE INTO SSA1                                   
002421     MOVE '    ' TO GODK-STATUSKODER                                      
002422     CALL CBLTDLI USING GHU WDE4-PCB DLI-IO-E401 SSA1                     
002423     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
002424     PERFORM IMS-STATUSKONTROLL                                           
002425     .                                                                    
002426     SKIP3                                                                
002427 IMS-REPL-WDE401 SECTION.                                                 
002428     SKIP2                                                                
002429     MOVE '  ' TO GODK-STATUSKODER                                        
002430     CALL CBLTDLI USING REPL WDE4-PCB DLI-IO-E401                         
002431     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
002432     PERFORM IMS-STATUSKONTROLL                                           
002433     .                                                                    
002434     SKIP2                                                                
002435 IMS-GNP-ORAD           SECTION.                                          
002436     SKIP2                                                                
002437     MOVE 'WDE411 ' TO SSA1                                               
002438     MOVE '  GE' TO GODK-STATUSKODER                                      
002439     CALL CBLTDLI USING GNP WDE4-PCB DLI-IO-E411 SSA1                     
002440     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
002441                              STATUS-ORAD-WS                              
002442     PERFORM IMS-STATUSKONTROLL                                           
002443     .                                                                    
002444     EJECT                                                                
002445 IMS-GU-WDE4ASEQ           SECTION.                                       
002446     SKIP2                                                                
002447     STRING 'WDE401  (WDE4ASEQ =' W-WDE4A1-KUNDORDER-X ')'                
002448            DELIMITED BY SIZE INTO SSA1                                   
002449     MOVE '  GE' TO GODK-STATUSKODER                                      
002450     CALL CBLTDLI USING GU  WDE41-PCB DLI-IO-E401 SSA1                    
002451     MOVE WDE41-STATUS-CODE TO STATUS-WS                                  
002452                               STATUS-KUNDORDER-SEK-WS                    
002453     PERFORM IMS-STATUSKONTROLL                                           
002454     .                                                                    
002455     EJECT                                                                
002456 IMS-GN-WDE4ASEQ           SECTION.                                       
002457     SKIP2                                                                
002458     STRING 'WDE401  (WDE4ASEQ =' W-WDE4A1-KUNDORDER-X ')'                
002459            DELIMITED BY SIZE INTO SSA1                                   
002460     MOVE '  GE' TO GODK-STATUSKODER                                      
002461     CALL CBLTDLI USING GN  WDE41-PCB DLI-IO-E401 SSA1                    
002462     MOVE WDE41-STATUS-CODE TO STATUS-WS                                  
002463                               STATUS-KUNDORDER-SEK-WS                    
002464     PERFORM IMS-STATUSKONTROLL                                           
002465     .                                                                    
002466     EJECT                                                                
002467 IMS-GU-WDE411-01-BSEQ      SECTION.                                      
002468     SKIP2                                                                
002469     STRING 'WDE411  (WDE4BSEQ>=' W-WDE411-KEYSEQ-MIN-X                   
002470                    '&WDE4BSEQ<=' W-WDE411-KEYSEQ-MAX-X ')'               
002471            DELIMITED BY SIZE INTO SSA1                                   
002472     MOVE 'WDE401 ' TO SSA2                                               
002473     MOVE '  GE' TO GODK-STATUSKODER                                      
002474     CALL CBLTDLI USING GU  WDE42-PCB DLI-IO-E401 SSA1 SSA2               
002475     MOVE WDE42-STATUS-CODE TO STATUS-WS                                  
002476     PERFORM IMS-STATUSKONTROLL                                           
002477     .                                                                    
002478     EJECT                                                                
002479 IMS-GU-WDE401-ESEQ SECTION.                                              
002480     SKIP2                                                                
002481     STRING 'WDE401  (WDE4ESEQ =' W-WDE601-VOLVOORDER-X ')'               
002482            DELIMITED BY SIZE INTO SSA1                                   
002483     MOVE '  GE' TO GODK-STATUSKODER                                      
002484     CALL CBLTDLI USING GU WDE43-PCB DLI-IO-E401 SSA1                     
002485     MOVE WDE43-STATUS-CODE TO STATUS-WS                                  
002486     PERFORM IMS-STATUSKONTROLL                                           
002487     .                                                                    
002488     SKIP3                                                                
002489 IMS-GU-WDE601          SECTION.                                          
002490                                                                          
002491     STRING 'WDE601  (IDPRODNR =' W-WDE601-VOLVOORDER-X ')'               
002492            DELIMITED BY SIZE INTO SSA1                                   
002493     MOVE '  ' TO GODK-STATUSKODER                                        
002494     CALL CBLTDLI USING GU WDE6-PCB DLI-IO-E601 SSA1                      
002495     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
002496     PERFORM IMS-STATUSKONTROLL                                           
002497     .                                                                    
002498     EJECT                                                                
002499 IMS-GET-XXDJ-ROT SECTION.                                                
002500     SKIP2                                                                
002501     STRING 'WLXXDJ01(WDGXKEY  =' W-4305-X ')'                            
002502            DELIMITED BY SIZE INTO SSA1                                   
002503     MOVE '  ' TO GODK-STATUSKODER                                        
002504     CALL CBLTDLI USING GU XXDJ-PCB DLI-IO-AREA SSA1                      
002505     MOVE XXDJ-STATUS-CODE TO STATUS-WS                                   
002506     PERFORM IMS-STATUSKONTROLL                                           
002507     .                                                                    
002508     SKIP3                                                                
002509 IMS-GET-XXDJ-LASNING-GODK-GE SECTION.                                    
002510                                                                          
002511     STRING 'WLXXDJ11(WDGXKEY  =' W-4306-X ')'                            
002512            DELIMITED BY SIZE INTO SSA1                                   
002513     MOVE '  GE' TO GODK-STATUSKODER                                      
002514     CALL CBLTDLI USING GHNP XXDJ-PCB DLI-IO-AREA SSA1                    
002515     MOVE XXDJ-STATUS-CODE TO STATUS-WS                                   
002516     PERFORM IMS-STATUSKONTROLL                                           
002517     .                                                                    
002518     SKIP3                                                                
002519 IMS-REPL-XXDJ-LASNING SECTION.                                           
002520     SKIP2                                                                
002521     MOVE '  ' TO GODK-STATUSKODER                                        
002522     CALL CBLTDLI USING REPL XXDJ-PCB DLI-IO-AREA                         
002523     MOVE XXDJ-STATUS-CODE TO STATUS-WS                                   
002524     PERFORM IMS-STATUSKONTROLL                                           
002525     .                                                                    
002526     SKIP3                                                                
002527 IMS-ISRT-XXDJ-LASNING SECTION.                                           
002528                                                                          
002529     STRING 'WLXXDJ01(WDGXKEY  =' W-4305-X ')'                            
002530            DELIMITED BY SIZE INTO SSA1                                   
002531     MOVE 'WLXXDJ11 ' TO SSA2                                             
002532     MOVE '  ' TO GODK-STATUSKODER                                        
002533     CALL CBLTDLI USING ISRT XXDJ-PCB DLI-IO-AREA SSA1 SSA2               
002534     MOVE XXDJ-STATUS-CODE TO STATUS-WS                                   
002535     PERFORM IMS-STATUSKONTROLL                                           
002536     .                                                                    
002537     EJECT                                                                
002538 IMS-GU-4448 SECTION.                                                     
002539     SKIP2                                                                
002540     STRING 'WLXXKH01(WDGXKEY  =' W-4447-X ')'                            
002541            DELIMITED BY SIZE INTO SSA1                                   
002542     STRING 'WLXXKH11(WDGXKEY  =' W-4448-X ')'                            
002543            DELIMITED BY SIZE INTO SSA2                                   
002544     MOVE '    ' TO GODK-STATUSKODER                                      
002545     CALL CBLTDLI USING GU XXKH-PCB DLI-IO-WDGX4448 SSA1 SSA2             
002546     MOVE XXKH-STATUS-CODE TO STATUS-WS                                   
002547     PERFORM IMS-STATUSKONTROLL                                           
002548     .                                                                    
002549     SKIP3                                                                
002550 IMS-GU-4488-FIRST SECTION.                                               
002551     SKIP2                                                                
002552     STRING 'WDR401  *D(WDGXKEY  =' W-4487-X ')'                          
002553            DELIMITED BY SIZE INTO SSA1                                   
002554     STRING 'WDGX4488(KDPRCGRP=>' W-4488-X ')'                            
002555            DELIMITED BY SIZE INTO SSA2                                   
002556     MOVE '  GEGB' TO GODK-STATUSKODER                                    
002557     CALL CBLTDLI USING GU 4487-PCB DLI-IO-AREA3 SSA1 SSA2                
002558     MOVE 4487-STATUS-CODE TO STATUS-WS                                   
002559                              STATUS-4488-WS                              
002560     PERFORM IMS-STATUSKONTROLL                                           
002561     .                                                                    
002562     SKIP3                                                                
002563 IMS-GU-4488 SECTION.                                                     
002564     SKIP2                                                                
002565     STRING 'WDR401  *D(WDGXKEY  =' W-4487-X ')'                          
002566            DELIMITED BY SIZE INTO SSA1                                   
002567     STRING 'WDGX4488(KDPRCGRP=>' W-4488-X ')'                            
002568            DELIMITED BY SIZE INTO SSA2                                   
002569     MOVE '  GE' TO GODK-STATUSKODER                                      
002570     CALL CBLTDLI USING GU 4487-PCB DLI-IO-AREA3 SSA1 SSA2                
002571     MOVE 4487-STATUS-CODE TO STATUS-WS                                   
002572                              STATUS-4488-WS                              
002573     PERFORM IMS-STATUSKONTROLL                                           
002574     .                                                                    
002575     SKIP3                                                                
002576 IMS-GHU-4490 SECTION.                                                    
002577     SKIP2                                                                
002578     STRING 'WDR401  (WDGXKEY  =' W-4487-X ')'                            
002579            DELIMITED BY SIZE INTO SSA1                                   
002580     STRING 'WDGX4488(KDPRCGRP =' W-4488-X ')'                            
002581            DELIMITED BY SIZE INTO SSA2                                   
002582     STRING 'WDGX4490(KY4490   =' W-4490-X ')'                            
002583            DELIMITED BY SIZE INTO SSA3                                   
002584     MOVE '  GE' TO GODK-STATUSKODER                                      
002585     CALL CBLTDLI USING GHU 4487-PCB DLI-IO-WDGX4490                      
002586                        SSA1 SSA2 SSA3                                    
002587     MOVE 4487-STATUS-CODE TO STATUS-WS                                   
002588                              STATUS-4490-WS                              
002589     PERFORM IMS-STATUSKONTROLL                                           
002590     .                                                                    
002591     SKIP3                                                                
002592 IMS-GN-4488 SECTION.                                                     
002593     SKIP2                                                                
002594     STRING 'WDR401  *D(WDGXKEY  =' W-4487-X ')'                          
002595            DELIMITED BY SIZE INTO SSA1                                   
002596     STRING 'WDGX4488(KDPRCGRP=>' W-4488-X ')'                            
002597            DELIMITED BY SIZE INTO SSA2                                   
002598     MOVE '  GEGB' TO GODK-STATUSKODER                                    
002599     CALL CBLTDLI USING GN 4487-PCB DLI-IO-AREA3 SSA1 SSA2                
002600     MOVE 4487-STATUS-CODE TO STATUS-WS                                   
002601                              STATUS-4488-WS                              
002602     PERFORM IMS-STATUSKONTROLL                                           
002603     .                                                                    
002604     SKIP3                                                                
002605 IMS-GNP-4490-KVAL SECTION.                                               
002606     SKIP2                                                                
002607     STRING 'WDGX4490(KY4490   =' W-4490-X ')'                            
002608            DELIMITED BY SIZE INTO SSA1                                   
002609     MOVE '  GE' TO GODK-STATUSKODER                                      
002610     CALL CBLTDLI USING GNP 4487-PCB DLI-IO-WDGX4490 SSA1                 
002611     MOVE 4487-STATUS-CODE TO STATUS-WS                                   
002612                              STATUS-4490-WS                              
002613     PERFORM IMS-STATUSKONTROLL                                           
002614     .                                                                    
002615     SKIP3                                                                
002616 IMS-GNP-4490 SECTION.                                                    
002617     SKIP2                                                                
002618     MOVE 'WDGX4490 ' TO SSA1                                             
002619     MOVE '  GE' TO GODK-STATUSKODER                                      
002620     CALL CBLTDLI USING GNP 4487-PCB DLI-IO-WDGX4490 SSA1                 
002621     MOVE 4487-STATUS-CODE TO STATUS-WS                                   
002622                              STATUS-4490-WS                              
002623     PERFORM IMS-STATUSKONTROLL                                           
002624     .                                                                    
002625     SKIP3                                                                
002626 IMS-REPL-4490           SECTION.                                         
002627     SKIP2                                                                
002628     MOVE '  ' TO GODK-STATUSKODER                                        
002629     CALL CBLTDLI USING REPL 4487-PCB DLI-IO-WDGX4490                     
002630     MOVE 4487-STATUS-CODE TO STATUS-WS                                   
002631                              STATUS-4490-WS                              
002632     PERFORM IMS-STATUSKONTROLL                                           
002633     .                                                                    
002634     SKIP2                                                                
002635 IMS-GU-ORQA-WDQ301 SECTION.                                              
002636     SKIP2                                                                
002637     STRING 'WLORQA01(WDQ301KY>=' W-WDQ301KY-MIN-X                        
002638                    '&WDQ301KY<=' W-WDQ301KY-MAX-X ')'                    
002639          DELIMITED BY SIZE INTO SSA1                                     
002640     MOVE '  GE' TO GODK-STATUSKODER                                      
002641     CALL CBLTDLI USING GU ORQA-PCB DLI-IO-WDQ301 SSA1                    
002642     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
002643     PERFORM IMS-STATUSKONTROLL                                           
002644     .                                                                    
002645     SKIP3                                                                
002646 IMS-GN-ORQA-WDQ301 SECTION.                                              
002647     SKIP2                                                                
002648     STRING 'WLORQA01(WDQ301KY>=' W-WDQ301KY-MIN-X                        
002649                    '&WDQ301KY<=' W-WDQ301KY-MAX-X ')'                    
002650          DELIMITED BY SIZE INTO SSA1                                     
002651     MOVE '  GE' TO GODK-STATUSKODER                                      
002652     CALL CBLTDLI USING GN ORQA-PCB DLI-IO-WDQ301 SSA1                    
002653     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
002654     PERFORM IMS-STATUSKONTROLL                                           
002655     .                                                                    
002656     SKIP3                                                                
002657 IMS-GHU-ORQA-WDQ301 SECTION.                                             
002658     SKIP2                                                                
002659     STRING 'WLORQA01(WDQ301KY =' W-WDQ301KY-X ')'                        
002660          DELIMITED BY SIZE INTO SSA1                                     
002661     MOVE '  GE' TO GODK-STATUSKODER                                      
002662     CALL CBLTDLI USING GHU ORQA-PCB DLI-IO-WDQ301 SSA1                   
002663     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
002664     PERFORM IMS-STATUSKONTROLL                                           
002665     .                                                                    
002666     SKIP3                                                                
002667 IMS-REPL-ORQA-WDQ301  SECTION.                                           
002668     SKIP2                                                                
002669     MOVE '  ' TO GODK-STATUSKODER                                        
002670     CALL CBLTDLI USING REPL ORQA-PCB DLI-IO-WDQ301                       
002671     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
002672     PERFORM IMS-STATUSKONTROLL                                           
002673     .                                                                    
002674                                                                          
002675 IMS-GU-WDB6      SECTION.                                                
002676     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
002677          DELIMITED BY SIZE INTO SSA1                                     
002678     MOVE '  ' TO GODK-STATUSKODER                                        
002679     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B6 SSA1                   
002680     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
002681     PERFORM IMS-STATUSKONTROLL                                           
002682     .                                                                    
002683     EJECT                                                                
002684 IMS-STATUSKONTROLL SECTION.                                              
002685     SKIP2                                                                
002686     SET STATUS-IX TO 1                                                   
002687     SEARCH GODK-STATUS AT END CALL FELLOG                                
002688       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
002689     END-SEARCH                                                           
002690     .                                                                    
