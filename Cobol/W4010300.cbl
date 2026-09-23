000001 ID DIVISION.                                                             
000002 PROGRAM-ID.     W4010300.                                                
000003 AUTHOR.         GÖRAN KJELLSON                                           
000004                                                                          
000005                                                                          
000006 DATE-COMPILED.                                                           
000007     DATE-WRITTEN.   APRIL 1979.                                          
000008     REMARKS.                                                             
000009*    FUNKTION.   TP-FRÅGE-PROGRAM SALDOLISTA  GRUNDBILD                   
000010*                                                                         
000011*                PROGRAMMET LÄSER:                                        
000012*                 *WLBENA - BENÄMNNINGSREGISTER WDD3                      
000013*                 *WLERSA - ERSÄTTNINGSREGISTER WDD7                      
000014*                 *WLARTC - ARTIKELREGISTER     WDK6                      
000015*                 *WLARTS - ARTIKELREGISTER     WDK7                      
000016*                                                                         
000017*    INDATA.                                                              
000018*        TRANSAKTION: W4T103                                              
000019*        MID:         W4I10301                                            
000020*    UTDATA.                                                              
000021*        MOD:         W4O10301                                            
000022*    SUBPROGRAM.                                                          
000023*        FELLOG                                                           
000024*    SKIP2                                                                
000025*                                                                         
000026*   ÄNDRINGAR:                                                            
000027*        03-05-15. TILLAGT FUNKTION FÖR ATT BEGRÄNSA INFORMATION          
000028*                  FÖR USER VARS SEC-IDLEVNR PÅ USER-BASEN                
000029*                  INTE ÄR LIKA MED HUVUDLEVERANTÖREN.                    
000030*                  ( SEC-IDLEVNR = SPACE, FÅR SE ALLT )    /C.E.          
000031*    E-TRACKER: 7450319  2008-HÖST  VOHF                                  
000032*    E-TRACKER: 10254592 2015 DECOMISSION VOHF                            
000033*                                                                         
000034 ENVIRONMENT DIVISION.                                                    
000035     SKIP2                                                                
000036 DATA DIVISION.                                                           
000037     EJECT                                                                
000038 WORKING-STORAGE SECTION.                                                 
000039                                                                          
000040*    -- CHECKED BY WY2000                                                 
000041 77  W-KDCLPOST               PIC S9(1)      COMP-3.                      
000042 77  SPARAD-KDERS             PIC 9(3).                                   
000043 77  WS-KVDISP                PIC S9(7)              VALUE ZERO.          
000044 77  WS-KVDISP-SDC            PIC S9(7)              VALUE ZERO.          
000045 77  WS-KVUTRS-SDC            PIC S9(7)              VALUE ZERO.          
000046 77  WS-KVDISP-TOT            PIC S9(7)              VALUE ZERO.          
000047 77  WS-KVAKS                 PIC S9(7)              VALUE ZERO.          
000048 77  WS-KVPB                  PIC S9(6)V9(1) COMP-3  VALUE ZERO.          
000049 77  WS-KVOKS-TOT-CDC         PIC S9(9)      COMP-3  VALUE ZERO.          
000050 77  WS-KVOKS-TOT-SDC         PIC S9(9)      COMP-3  VALUE ZERO.          
000051 77  MAX-MOD-LAENGD           PIC S9(4)  COMP SYNC   VALUE ZERO.          
000052 77  CL-INDEX                 PIC S9(9)  COMP SYNC.                       
000053 77  IX                       PIC S9(9)  COMP SYNC.                       
000054 77  IX-CDC                   PIC S9(9)  COMP SYNC   VALUE +1.            
000055 77  IX-SDC                   PIC S9(9)  COMP SYNC   VALUE +2.            
000056 77  WS-IDTRANS               PIC X(4).                                   
000057     88  WS-GODKAEND-BILD      VALUE '4101' '4102' '4103' '4104'          
000058                                     '4105' '4106' '4107' '4108'.         
000059     88  EGEN-MID              VALUE '4103'.                              
000060                                                                          
000061 77  WS-IDLEVNR-8        PIC X(8)               VALUE SPACE.              
000062                                                                          
000063 01  DAGENS-DATUM             PIC 9(6)    VALUE ZERO.                     
000064                                                                          
000065 01  ARBETSAREOR.                                                         
000066*     -- FÖR REDIGERING AV IDAVINR FRÅN IDFS                              
000067  02     WS-IDAVINR              PIC 9(7)    VALUE ZERO.                  
000068  02     FILLER                  REDEFINES WS-IDAVINR.                    
000069   03    WS-IDAVINR-TKN          OCCURS 7                                 
000070                                 PIC 9(1).                                
000071*     -- FÖR REDIGERING AV IDAVINR FRÅN IDFS                              
000072  02     WS-IDFS                 PIC X(8)    VALUE SPACE.                 
000073  02     FILLER                  REDEFINES WS-IDFS.                       
000074   03    WS-IDFS-TKN             OCCURS 8                                 
000075                                 PIC 9(1).                                
000076*     -- FÄLTLÄNGD IDFS                                                   
000077  02     K-IDFS-LNG              PIC S9(9)   VALUE +8   COMP SYNC.        
000078*     -- FÄLTLÄNGD IDAVINR                                                
000079  02     K-IDAVINR-LNG           PIC S9(9)   VALUE +7   COMP SYNC.        
000080                                                                          
000081 01      IX-INDEXVARIABLER.                                               
000082                                                                          
000083*     -- TECKEN I WS-IDFS                                                 
000084  02     IX-IDFS                 PIC S9(9)   VALUE ZERO COMP SYNC.        
000085*     -- TECKEN I WS-IDAVINR                                              
000086  02     IX-IDAVINR              PIC S9(9)   VALUE ZERO COMP SYNC.        
000087                                                                          
000088     EJECT                                                                
000089 01  KONSTANTER.                                                          
000090     03  JA                   PIC X       VALUE 'J'.                      
000091     03  NEJ                  PIC X       VALUE 'N'.                      
000092     03  FRANSKA              PIC 9       VALUE 2.                        
000093     03  SPANSKA              PIC 9       VALUE 3.                        
000094     03  TYSKA                PIC 9       VALUE 4.                        
000095     03  EJ-TEXT              PIC X       VALUE 'N'.                      
000096                                                                          
000097     03  FARLIGA-TEXTER.                                                  
000098         05  FILLER           PIC X(12)   VALUE ' FARL.GODS  '.           
000099         05  FILLER           PIC X(12)   VALUE 'DANG.GOODS  '.           
000100     03  FILLER REDEFINES FARLIGA-TEXTER.                                 
000101         05  FARLIG-TEXT      PIC X(12) OCCURS 2.                         
000102                                                                          
000103     03  PSN-TEXT.                                                        
000104         05  FILLER           PIC X(10)   VALUE ' PSN      '.             
000105     EJECT                                                                
000106 01  NYCKEL-SW                PIC X.                                      
000107     88  NYCKLAR-OK                       VALUE 'J'.                      
000108                                                                          
000109 01  KVERS-SW                 PIC X.                                      
000110     88  FLER-ERSETTNINGAR                VALUE 'J'.                      
000111                                                                          
000112 77  SECURITY-SW                 PIC X       VALUE 'N'.                   
000113     88  PASSED-SECURITY-CHECK               VALUE 'J'.                   
000114     88  BLOCKED-SECURITY-CHECK              VALUE 'N'.                   
000115                                                                          
000116 01      W-IDARTNR-X.                                                     
000117   03    W-IDARTNR            PIC S9(9)   VALUE ZERO  COMP-3.             
000118                                                                          
000119 01      W-IDSKYLT-X.                                                     
000120   03    W-IDSKYLT            PIC X(3).                                   
000121                                                                          
000122 01  W-KDARTHNT-N             PIC 9(6).                                   
000123 01  FILLER REDEFINES W-KDARTHNT-N.                                       
000124   03  W-KVQPAC-N             PIC 9(3).                                   
000125   03  FILLER                 PIC 9(3).                                   
000126     EJECT                                                                
000127 01      MEDDELANDEN.                                                     
000128   03  FILLER-1.                                                          
000129     05  FILLER          PIC X(36)                                        
000130         VALUE 'ARTIKELN FINNS EJ I ARTIKELREGISTRET'.                    
000131     05  FILLER          PIC 9  COMP-3 VALUE 3.                           
000132     05  FILLER          PIC X(3).                                        
000133     05  FILLER          PIC X(35)                                        
000134      VALUE 'THIS PARTNO. IS NOT IN THE DATABASE'.                        
000135     05  FILLER          PIC 9  COMP-3 VALUE 3.                           
000136     05  FILLER          PIC X(4).                                        
000137   03    FILLER REDEFINES FILLER-1.                                       
000138     05  ARTIKELN-UTGONGEN PIC X(40)  OCCURS 2.                           
000139                                                                          
000140   03  FILLER-15.                                                         
000141     05  FILLER          PIC X(20)                                        
000142         VALUE 'ARTIKELN ÄR UTGÅNGEN'.                                    
000143     05  FILLER          PIC 9  COMP-3 VALUE 3.                           
000144     05  FILLER          PIC X(19).                                       
000145     05  FILLER          PIC X(23)                                        
000146      VALUE 'THIS PARTNO. IS DELETED'.                                    
000147     05  FILLER          PIC 9  COMP-3 VALUE 3.                           
000148     05  FILLER          PIC X(16).                                       
000149   03    FILLER REDEFINES FILLER-15.                                      
000150     05  MEDDELANDE-2   PIC X(40)  OCCURS 2.                              
000151                                                                          
000152   03  FILLER-2.                                                          
000153     05  FILLER          PIC X(26)                                        
000154         VALUE 'ARTIKELNUMRET EJ NUMERISKT'.                              
000155     05  FILLER          PIC 9  COMP-3 VALUE 3.                           
000156     05  FILLER          PIC X(13).                                       
000157     05  FILLER          PIC X(22)                                        
000158      VALUE 'PARTNUMBER NOT NUMERIC'.                                     
000159     05  FILLER          PIC 9  COMP-3 VALUE 3.                           
000160     05  FILLER          PIC X(17).                                       
000161   03    FILLER REDEFINES FILLER-2.                                       
000162     05  EJ-NUMERISK    PIC X(40)  OCCURS 2.                              
000163                                                                          
000164   03  FILLER-3.                                                          
000165     05  FILLER          PIC X(16)                                        
000166         VALUE 'ERSATT MED ANTAL'.                                        
000167     05  FILLER          PIC 9  COMP-3 VALUE 3.                           
000168     05  FILLER          PIC X(3).                                        
000169     05  FILLER          PIC X(20)                                        
000170         VALUE 'REPL.WITH  QUANTITY '.                                    
000171   03  FILLER REDEFINES FILLER-3.                                         
000172     05  ERSETTNINGSTEXT PIC X(19)  OCCURS 2.                             
000173                                                                          
000174   03  FILLER-4.                                                          
000175     05  FILLER          PIC X(16)                                        
000176         VALUE 'FLER RADER FINNS'.                                        
000177     05  FILLER          PIC 9  COMP-3 VALUE 3.                           
000178     05  FILLER          PIC X(3).                                        
000179     05  FILLER          PIC X(17)                                        
000180         VALUE 'MORE ITEMS EXISTS'.                                       
000181     05  FILLER          PIC 9  COMP-3 VALUE 3.                           
000182     05  FILLER          PIC X(2).                                        
000183   03  FILLER REDEFINES FILLER-4.                                         
000184     05  MER-RADER       PIC X(19)  OCCURS 2.                             
000185                                                                          
000186   03  FILLER-5.                                                          
000187     05  FILLER          PIC X(19)                                        
000188         VALUE 'ARTIKELNR ÄR ERSATT'.                                     
000189     05  FILLER          PIC 9  COMP-3 VALUE 3.                           
000190     05  FILLER          PIC X(10).                                       
000191     05  FILLER          PIC X(24)                                        
000192         VALUE 'THIS PARTNO. IS REPLACED'.                                
000193     05  FILLER          PIC 9  COMP-3 VALUE 3.                           
000194     05  FILLER          PIC X(5).                                        
000195   03  FILLER REDEFINES FILLER-5.                                         
000196     05  ARTIKELN-ERSATT PIC X(30)  OCCURS 2.                             
000197     EJECT                                                                
000198                                                                          
000199 01  MESSAGE-CODES.                                                       
000200     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
000201     03  CONFLICT                PIC X(3)    VALUE '002'.                 
000202     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
000203     03  URVAL-SAKNAS            PIC X(3)    VALUE '005'.                 
000204     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
000205     03  ERR-NOT-REGISTERED      PIC X(3)    VALUE '010'.                 
000206     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
000207     03  ARTIKEL-SAKNAS          PIC X(3)    VALUE '017'.                 
000208     03  ARTIKEL-UTGANGEN        PIC X(3)    VALUE '018'.                 
000209     03  INF-UPDATE-NOT-DONE     PIC X(3)    VALUE '034'.                 
000210     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
000211     03  ARTIKEL-ERSATT          PIC X(3)    VALUE '220'.                 
000212     03  ARTIKEL-SAKNAS-SDC      PIC X(3)    VALUE '305'.                 
000213     03  EJ-GODK-REFILL          PIC X(3)    VALUE '307'.                 
000214     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
000215     03  ERR-NOT-AUTHORIZED      PIC X(3)    VALUE '405'.                 
000216     EJECT                                                                
000217                                                                          
000218 01  DYNAMISKA-SUBPROGRAM.                                                
000219     03  CBLTDLI         PIC X(8)          VALUE 'CBLTDLI'.               
000220     03  FELLOG          PIC X(8)          VALUE 'FELLOG'.                
000221     03  W005INIT        PIC X(8)          VALUE 'W005INIT'.              
000222     03  WMEDKONV        PIC X(8)         VALUE 'WMEDKONV'.               
000223     EJECT                                                                
000224                                                                          
000225*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
000226*01 -COPY WMSGINIT                                                        
000227     EJECT                                                                
000228*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
000229*01 -COPY WMEDAREA                                                        
000230     EJECT                                                                
000231*                        ****    TP-AREOR                                 
000232 01  FILLER  PIC X(16)   VALUE 'MID-AREA'.                                
000233                                                                          
000234*01      MID -COPY W4I10301 -PRE MID-.                                    
000235     EJECT                                                                
000236 01  FILLER  PIC X(16)   VALUE 'MOD-AREA'.                                
000237*01      -COPY WMSGAREA                                                   
000238     EJECT                                                                
000239*  03    MOD -COPY W4O10301 -PRE MOD- -RED MSG-AREA.                      
000240     EJECT                                                                
000241*01  -COPY WMFSAREA.                                                      
000242     EJECT                                                                
000243******************************************************************        
000244*****                                                                     
000245*****    ARBETS-AREOR TILL IMS-SEKTIONERNA                                
000246*****                                                                     
000247 01  IMS-WS.                                                              
000248   03    FILLER          PIC X(16)   VALUE '     IMS-WS     '.            
000249                                                                          
000250*****                    **** STATUS-KOD FRÅN IMS                         
000251   03    STATUS-WS       PIC XX.                                          
000252         88  SEGMENT-FINNS       VALUE '  '.                              
000253         88  SEGMENT-SAKNAS      VALUE 'GE'.                              
000254     SKIP3                                                                
000255   03    GODK-STATUSKODER.                                                
000256     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
000257     SKIP3                                                                
000258 01      SSA1            PIC X(64).                                       
000259 01      SSA2            PIC X(64).                                       
000260     EJECT                                                                
000261*                            IMS FUNKTIONSKODER                           
000262*01      -COPY W0003                                                      
000263     EJECT                                                                
000264*                            DLI INPUT-OUTPUT AREA                        
000265 01      DLI-IO-AREA     PIC X(900)  VALUE SPACE.                         
000266*01  WLERSA11     -COPY WDD702 -PRE ERSA-  -RED DLI-IO-AREA.              
000267     EJECT                                                                
000268*01  WLARTC01     -COPY WDK601             -RED DLI-IO-AREA.              
000269     EJECT                                                                
000270*01  WLARTC11     -COPY WDK611             -RED DLI-IO-AREA.              
000271     EJECT                                                                
000272*01  WLARTS01     -COPY WDK701             -RED DLI-IO-AREA.              
000273     EJECT                                                                
000274*01  WLARTS11     -COPY WDK711             -RED DLI-IO-AREA.              
000275     EJECT                                                                
000276*01  WLBENA01     -COPY WDD301 -PRE BENA-  -RED DLI-IO-AREA.              
000277     EJECT                                                                
000278*01  WLBENA11     -COPY WDD311 -PRE BENA-  -RED DLI-IO-AREA.              
000279     EJECT                                                                
000280 01  FILLER                      PIC X(16) VALUE 'ARTM-IO-AREA'.          
000281 01  ARTM-IO-AREA.                                                        
000282*    03           -COPY WDK901                                            
000283     EJECT                                                                
000284 LINKAGE SECTION.                                                         
000285     SKIP2                                                                
000286*01  -COPY W0009     -PRE MSG-                                            
000287     EJECT                                                                
000288*01  -COPY W0008     -PRE USEA-.                                          
000289         05  FILLER           PIC X.                                      
000290     SKIP2                                                                
000291*01  -COPY W0008     -PRE ERSA-.                                          
000292         05  FILLER           PIC X.                                      
000293     EJECT                                                                
000294*01  -COPY W0008     -PRE ARTC-.                                          
000295         05  FILLER           PIC X.                                      
000296     EJECT                                                                
000297*01  -COPY W0008     -PRE ARTS-.                                          
000298         05  FILLER           PIC X.                                      
000299     EJECT                                                                
000300*01  -COPY W0008     -PRE BENA-.                                          
000301         05  FILLER           PIC X.                                      
000302     EJECT                                                                
000303*01  -COPY W0008     -PRE ARTM-.                                          
000304         05  FILLER           PIC X.                                      
000305     EJECT                                                                
000306                                                                          
000307 PROCEDURE DIVISION USING MSG-PCB  USEA-PCB ERSA-PCB                      
000308                          ARTC-PCB ARTS-PCB BENA-PCB ARTM-PCB.            
000309                                                                          
000310     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB ERSA-PCB                      
000311                          ARTC-PCB ARTS-PCB BENA-PCB ARTM-PCB.            
000312                                                                          
000313                                                                          
000314     PERFORM IMS-GET-MSG                                                  
000315     IF SEGMENT-FINNS                                                     
000316       PERFORM A-KOLLA-NYCKLAR                                            
000317       IF NYCKLAR-OK                                                      
000318         PERFORM S1-SECURITY-CHECK-PARTNO                                 
000319         IF PASSED-SECURITY-CHECK                                         
000320                                                                          
000321           PERFORM IMS-GET-WLARTC01-ARTIKEL                               
000322           IF SEGMENT-FINNS                                               
000323             PERFORM B-REDIGERA-WLBENA-INFO                               
000324             PERFORM C-REDIGERA-WLARTC-INFO                               
000325             PERFORM D-REDIGERA-WLARTS-INFO                               
000326             PERFORM E-REDIGERA-WLERSA-INFO                               
000327           ELSE                                                           
000328             MOVE ARTIKELN-UTGONGEN (CL-INDEX) TO MOD-TEMFSFEL            
000329             IF MSGI-IDARTNR NOT NUMERIC                                  
000330                PERFORM H-RENSA-NYCKLAR                                   
000331             END-IF                                                       
000332           END-IF                                                         
000333         END-IF                                                           
000334                                                                          
000335       ELSE                                                               
000336          PERFORM H-RENSA-NYCKLAR                                         
000337       END-IF                                                             
000338       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
000339       PERFORM IMS-INSERT-MSG                                             
000340     END-IF                                                               
000341     MOVE ZERO TO RETURN-CODE                                             
000342     GOBACK                                                               
000343     .                                                                    
000344     EJECT                                                                
000345 A-KOLLA-NYCKLAR SECTION.                                                 
000346                                                                          
000347     MOVE JA TO NYCKEL-SW                                                 
000348     MOVE NEJ TO KVERS-SW                                                 
000349     IF MSG-DUBBLA-TRANSKODER                                             
000350         MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I10301               
000351         MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                
000352         MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR CL-INDEX                     
000353     ELSE                                                                 
000354         MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I10301                
000355         MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                
000356         MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR CL-INDEX                     
000357     END-IF                                                               
000358                                                                          
000359     COMPUTE MAX-MOD-LAENGD = LENGTH OF MOD-W4O10301 + 4                  
000360     MOVE MFS-IDTRANS        TO WS-IDTRANS                                
000361                                                                          
000362     IF MID-IDARTNR-IN     NOT = ALL '+'                                  
000363       MOVE ZERO TO MID-KVERS                                             
000364     END-IF                                                               
000365                                                                          
000366     IF MID-KVERS NOT NUMERIC OR MID-KVERS = SPACE                        
000367       MOVE ZERO TO MID-KVERS                                             
000368     END-IF                                                               
000369                                                                          
000370     IF MID-KVERS NOT = ZERO                                              
000371       MOVE JA TO KVERS-SW                                                
000372     END-IF                                                               
000373                                                                          
000374     MOVE LOW-VALUE TO MOD-W4O10301                                       
000375     MOVE 'W4O103N1' TO MFS-IDMOD                                         
000376     MOVE '4103' TO MOD-IDTRANS                                           
000377                                                                          
000378     MOVE ALL '+'           TO MSGI-WMSGINIT                              
000379     MOVE '001'             TO MSGI-KDCALL                                
000380     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
000381     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
000382     MOVE '4103'            TO MSGI-IDTRANS                               
000383                                                                          
000384     IF MFS-IDTRANS = '4103'                                              
000385     OR (MID-IDARTNR-IN NUMERIC                                           
000386     AND MID-IDARTNR-IN > ZERO)                                           
000387         MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                              
000388     END-IF                                                               
000389                                                                          
000390     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
000391                                                                          
000392     MOVE MSGI-IDLAND-SPR TO MED-IDSKYLT                                  
000393                                                                          
000394     IF  MSGI-IDLAND-SPR = 'GB'                                           
000395         MOVE 2              TO CL-INDEX                                  
000396     ELSE                                                                 
000397         MOVE 1              TO CL-INDEX                                  
000398     END-IF                                                               
000399                                                                          
000400     MOVE MSGI-IDARTNR    TO MOD-IDARTNR-UT                               
000401     INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE               
000402                                                                          
000403     MOVE MAX-MOD-LAENGD TO MSG-KVLL                                      
000404                                                                          
000405     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
000406     IF MSGI-IDARTNR NOT NUMERIC                                          
000407         MOVE NEJ TO NYCKEL-SW                                            
000408         MOVE EJ-NUMERISK (CL-INDEX) TO MOD-TEMFSFEL                      
000409     ELSE                                                                 
000410         MOVE MSGI-IDARTNR   TO W-IDARTNR                                 
000411     END-IF                                                               
000412                                                                          
000413     ACCEPT DAGENS-DATUM    FROM DATE                                     
000414     .                                                                    
000415     EJECT                                                                
000416 B-REDIGERA-WLBENA-INFO SECTION.                                          
000417                                                                          
000418     MOVE '-'            TO MOD-STRECK                                    
000419     MOVE ART-REKSIFFR   TO MOD-REKSIFFR                                  
000420     MOVE ART-IDLEVNR    TO MOD-IDLEVNR                                   
000421     MOVE ART-KDERS-UTG  TO MOD-KDERS                                     
000422                            SPARAD-KDERS                                  
000423     MOVE ART-TIERSDAT   TO MOD-TIERSDAT                                  
000424                                                                          
000425***  SEGMENT 07 (UTLÄNDSK BENÄMNING)                                      
000426*                                                                         
000427     PERFORM IMS-BENA-LAS-ROTSEG                                          
000428     MOVE 'D  '               TO W-IDSKYLT                                
000429     PERFORM IMS-BENA-LAS-TEXTSEG                                         
000430                                                                          
000431     IF  SEGMENT-FINNS                                                    
000432       MOVE BENA-TEXT-BEART   TO MOD-BEART-TYS                            
000433     ELSE                                                                 
000434       MOVE SPACE             TO MOD-BEART-TYS                            
000435     END-IF                                                               
000436                                                                          
000437     MOVE 'E  '               TO W-IDSKYLT                                
000438     PERFORM IMS-BENA-LAS-TEXTSEG                                         
000439                                                                          
000440     IF  SEGMENT-FINNS                                                    
000441       MOVE BENA-TEXT-BEART   TO MOD-BEART-SPA                            
000442     ELSE                                                                 
000443       MOVE SPACE             TO MOD-BEART-SPA                            
000444     END-IF                                                               
000445                                                                          
000446     MOVE 'F  '               TO W-IDSKYLT                                
000447     PERFORM IMS-BENA-LAS-TEXTSEG                                         
000448                                                                          
000449     IF  SEGMENT-FINNS                                                    
000450       MOVE BENA-TEXT-BEART   TO MOD-BEART-FRA                            
000451     ELSE                                                                 
000452       MOVE SPACE             TO MOD-BEART-FRA                            
000453     END-IF                                                               
000454                                                                          
000455     MOVE 'GB '               TO W-IDSKYLT                                
000456     PERFORM IMS-BENA-LAS-TEXTSEG                                         
000457                                                                          
000458     IF  SEGMENT-FINNS                                                    
000459       MOVE BENA-TEXT-BEART   TO MOD-BEART-ENG                            
000460     ELSE                                                                 
000461       MOVE SPACE             TO MOD-BEART-ENG                            
000462     END-IF                                                               
000463                                                                          
000464     MOVE 'S  '               TO W-IDSKYLT                                
000465     PERFORM IMS-BENA-LAS-TEXTSEG                                         
000466                                                                          
000467     IF  SEGMENT-FINNS                                                    
000468       MOVE BENA-TEXT-BEART   TO MOD-BEART-SVE                            
000469     ELSE                                                                 
000470       MOVE SPACE             TO MOD-BEART-SVE                            
000471     END-IF                                                               
000472     .                                                                    
000473     EJECT                                                                
000474 C-REDIGERA-WLARTC-INFO   SECTION.                                        
000475                                                                          
000476     PERFORM IMS-GNP-WLARTC11-CLAG                                        
000477     IF SEGMENT-FINNS                                                     
000478                                                                          
000479       PERFORM CA-BERAEKNA-SALDON                                         
000480                                                                          
000481       MOVE CLAG-KVRESS             TO MOD-KVRESS                         
000482       MOVE CLAG-KVUTRS             TO MOD-KVUTRS (IX-CDC)                
000483       MOVE CLAG-KVSLAGER           TO MOD-KVSLAGER                       
000484                                                                          
000485*    -- REDIGERA IDAVINR                                                  
000486       MOVE CLAG-IDFS-SEN          TO WS-IDFS                             
000487       MOVE ZERO                   TO WS-IDAVINR                          
000488       MOVE K-IDFS-LNG             TO IX-IDFS                             
000489       MOVE K-IDAVINR-LNG          TO IX-IDAVINR                          
000490       PERFORM UNTIL (IX-IDFS      = ZERO                                 
000491                  OR  IX-IDAVINR   = ZERO)                                
000492         IF  WS-IDFS-TKN (IX-IDFS) NUMERIC                                
000493           MOVE WS-IDFS-TKN (IX-IDFS)                                     
000494                                   TO WS-IDAVINR-TKN (IX-IDAVINR)         
000495           SUBTRACT 1              FROM IX-IDAVINR                        
000496         END-IF                                                           
000497         SUBTRACT 1                FROM IX-IDFS                           
000498       END-PERFORM                                                        
000499       MOVE WS-IDAVINR              TO MOD-IDAVINR                        
000500                                                                          
000501       MOVE CLAG-KVROS              TO MOD-KVROS                          
000502                                                                          
000503       COMPUTE MOD-KVPB (IX-CDC) =  CLAG-KVPB-SATS +                      
000504                                    CLAG-KVPB-SEP +                       
000505                                    CLAG-KVPB-TPO                         
000506                                                                          
000507       MOVE CLAG-KVSPANT            TO MOD-KVSPANT                        
000508       MOVE CLAG-IDANSK             TO MOD-IDANSK                         
000509       MOVE CLAG-KDHF               TO MOD-KDHF                           
000510                                                                          
000511       PERFORM CB-BESTAEMMA-SPARRKOD                                      
000512                                                                          
000513       MOVE CLAG-ADLAGOMR           TO MOD-ADLAGOMR                       
000514       MOVE CLAG-ADGANG             TO MOD-ADGANG                         
000515       MOVE CLAG-ADPLATS            TO MOD-ADPLATS                        
000516       IF CLAG-ADLAGOMR-SVS   > ZERO OR                                   
000517          CLAG-ADLAGOMR-CD(1) > ZERO                                      
000518         MOVE JA                    TO MOD-FLERPL                         
000519       ELSE                                                               
000520         MOVE NEJ                   TO MOD-FLERPL                         
000521       END-IF                                                             
000522       MOVE CLAG-KVQPACK-1          TO MOD-KVQPACK-1                      
000523                                                                          
000524       MOVE CLAG-KDARTHNT           TO W-KDARTHNT-N                       
000525       MOVE W-KVQPAC-N              TO MOD-FIKT-QPACK                     
000526                                                                          
000527       MOVE CLAG-REDIRLEV           TO MOD-REDIRLEV                       
000528       MOVE CLAG-KDERS              TO MOD-KDERS                          
000529                                       SPARAD-KDERS                       
000530                                                                          
000531       IF CLAG-IDPSN > 0                                                  
000532          MOVE PSN-TEXT             TO MOD-IDPSN-TEXT                     
000533          MOVE CLAG-IDPSN           TO MOD-IDPSN                          
000534       END-IF                                                             
000535                                                                          
000536       MOVE CLAG-KDFARLIG           TO MOD-KDFARLIG                       
000537       MOVE FARLIG-TEXT (CL-INDEX)  TO MOD-KDFARLIG-TEXT                  
000538       IF CLAG-KDFARLIG = 4                                               
000539       OR CLAG-KDFARLIG = 7                                               
000540         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KDFARLIG-ATTR                  
000541       END-IF                                                             
000542                                                                          
000543       MOVE CLAG-KDSRA              TO MOD-KDSRA                          
000544       MOVE CLAG-KDARTURS           TO MOD-KDARTURS                       
000545       PERFORM S20-HAMTA-FLPCOO                                           
000546     END-IF                                                               
000547     .                                                                    
000548     EJECT                                                                
000549 CA-BERAEKNA-SALDON SECTION.                                              
000550                                                                          
000551     PERFORM IMS-GET-ARTM-WDK9                                            
000552     IF SEGMENT-FINNS                                                     
000553       COMPUTE WS-KVOKS-TOT-CDC = ART-KVOKS-BULK +                        
000554                                  ART-KVOKS-DAG  +                        
000555                                  ART-KVOKS-VOR                           
000556     END-IF                                                               
000557     MOVE '  '                 TO STATUS-WS                               
000558                                                                          
000559     COMPUTE WS-KVDISP          = CLAG-KVLS   -                           
000560                                  CLAG-KVRESS -                           
000561                                  CLAG-KVUTRS -                           
000562                                  WS-KVOKS-TOT-CDC                        
000563     MOVE WS-KVDISP            TO MOD-KVDISP(IX-CDC)                      
000564                                                                          
000565     COMPUTE MOD-KVAKS(IX-CDC) =  CLAG-KVAKS-CDC +                        
000567                                  CLAG-KVAKS-T                            
000568     .                                                                    
000569     EJECT                                                                
000570 CB-BESTAEMMA-SPARRKOD SECTION.                                           
000571                                                                          
000572     MOVE ZERO TO MOD-SPARRKOD                                            
000573                                                                          
000574     IF CLAG-KDUART = 'M'                                                 
000575       MOVE 6 TO MOD-SPARRKOD                                             
000576     ELSE                                                                 
000577       IF CLAG-KDUART = 'S'                                               
000578         MOVE 4 TO MOD-SPARRKOD                                           
000579       ELSE                                                               
000580         IF CLAG-FLLSRDEL = 'N'                                           
000581           MOVE 3 TO MOD-SPARRKOD                                         
000582         ELSE                                                             
000583           IF CLAG-KDLEVSP = 20 OR 21                                     
000584             MOVE 2 TO MOD-SPARRKOD                                       
000585           END-IF                                                         
000586         END-IF                                                           
000587       END-IF                                                             
000588     END-IF                                                               
000589     .                                                                    
000590     EJECT                                                                
000591 D-REDIGERA-WLARTS-INFO SECTION.                                          
000592                                                                          
000593     PERFORM IMS-GET-WLARTS01-ARTIKEL                                     
000594     IF SEGMENT-FINNS                                                     
000595       PERFORM IMS-GNP-WLARTS11-SLAG                                      
000596                                                                          
000597       PERFORM UNTIL SEGMENT-SAKNAS                                       
000598         COMPUTE WS-KVPB       = WS-KVPB       +                          
000599                                 SLAG-KVPB-REF                            
000600                                                                          
000601         COMPUTE WS-KVDISP-SDC = SLAG-KVLS   -                            
000602                                 SLAG-KVUTRS -                            
000603                                 SLAG-KVOKS-BULK -                        
000604                                 SLAG-KVOKS-DAG                           
000605                                                                          
000606         COMPUTE WS-KVDISP-TOT = WS-KVDISP-TOT +                          
000607                                 WS-KVDISP-SDC                            
000608         MOVE ZERO TO WS-KVDISP-SDC                                       
000609                                                                          
000610         COMPUTE WS-KVUTRS-SDC = WS-KVUTRS-SDC +                          
000611                                 SLAG-KVUTRS                              
000612                                                                          
000613         COMPUTE WS-KVAKS      = WS-KVAKS     +                           
000614                                 SLAG-KVAKS-SDC +                         
000615                                 SLAG-KVAKS-PAV                           
000616                                                                          
000617         PERFORM IMS-GNP-WLARTS11-SLAG                                    
000618       END-PERFORM                                                        
000619                                                                          
000620       MOVE WS-KVDISP-TOT     TO MOD-KVDISP(IX-SDC)                       
000621       MOVE WS-KVUTRS-SDC     TO MOD-KVUTRS(IX-SDC)                       
000622       MOVE WS-KVAKS          TO MOD-KVAKS(IX-SDC)                        
000623       MOVE WS-KVPB           TO MOD-KVPB(IX-SDC)                         
000624     END-IF                                                               
000625     .                                                                    
000626     EJECT                                                                
000627 E-REDIGERA-WLERSA-INFO SECTION.                                          
000628                                                                          
000629     IF SPARAD-KDERS > 10                                                 
000630       IF SPARAD-KDERS = 29                                               
000631         MOVE ARTIKELN-UTGONGEN (CL-INDEX) TO MOD-TEMFSFEL                
000632       ELSE                                                               
000633         MOVE ARTIKELN-ERSATT (CL-INDEX)   TO MOD-TEMFSFEL                
000634       END-IF                                                             
000635       MOVE MID-KVERS TO IX                                               
000636       PERFORM IMS-GET-WLERSA-ARTIKEL                                     
000637       IF SEGMENT-FINNS                                                   
000638         PERFORM IMS-GET-WLERSA-ERSETTNING                                
000639         PERFORM UNTIL SEGMENT-SAKNAS OR IX NOT > 0                       
000640           PERFORM IMS-GET-WLERSA-ERSETTNING                              
000641           SUBTRACT 1 FROM IX                                             
000642         END-PERFORM                                                      
000643         IF SEGMENT-FINNS                                                 
000644           MOVE ERSETTNINGSTEXT (CL-INDEX) TO MOD-ERS-TEXT                
000645           MOVE +1 TO IX                                                  
000646           PERFORM UNTIL SEGMENT-SAKNAS OR IX NOT < 11                    
000647             IF ERSA-FLTEXT = EJ-TEXT                                     
000648               MOVE ERSA-IDARTNR-TILLK TO MOD-IDARTNR-TILLK (IX)          
000649               MOVE ERSA-DIERS-TILLK   TO MOD-DIERS-TILLK (IX)            
000650             ELSE                                                         
000651               MOVE ERSA-BEERS         TO MOD-BEERS (IX)                  
000652             END-IF                                                       
000653             ADD +1 TO IX                                                 
000654             ADD +1 TO MID-KVERS                                          
000655             PERFORM IMS-GET-WLERSA-ERSETTNING                            
000656           END-PERFORM                                                    
000657           IF IX = 11                                                     
000658             MOVE MER-RADER (CL-INDEX) TO MOD-FLER-RADER                  
000659             MOVE MID-KVERS            TO MOD-KVERS                       
000660           ELSE                                                           
000661             MOVE ZERO                 TO MOD-KVERS                       
000662           END-IF                                                         
000663         END-IF                                                           
000664       END-IF                                                             
000665     ELSE                                                                 
000666       MOVE ZERO TO MOD-KVERS                                             
000667     END-IF                                                               
000668     .                                                                    
000669     EJECT                                                                
000670 H-RENSA-NYCKLAR SECTION.                                                 
000671     MOVE MFS-RENSA-FAELT           TO  MOD-IDARTNR-UT                    
000672     .                                                                    
000673     EJECT                                                                
000674                                                                          
000675 S1-SECURITY-CHECK-PARTNO SECTION.                                        
000676     SKIP2                                                                
000677*    --- CHECK IF USER IS GRANTED TO SEE PART-INFO                        
000678     PERFORM IMS-GET-WLARTC01-ARTIKEL                                     
000679     IF  SEGMENT-FINNS                                                    
000680       MOVE ART-IDLEVNR          TO WS-IDLEVNR-8                          
000681       IF MSGI-KDARBTYP-SEC-IDLEV = WS-IDLEVNR-8                          
000682       OR MSGI-KDARBTYP-SEC-IDLEV = SPACE OR LOW-VALUE                    
000683*        --- USER AUTHORIZED                                              
000684         SET PASSED-SECURITY-CHECK TO TRUE                                
000685       ELSE                                                               
000686*        --- OBEHÖRIG USER / USER NOT AUTHORIZED                          
000687         MOVE ERR-NOT-AUTHORIZED TO MED-IDMFSFEL                          
000688         CALL WMEDKONV USING MED-WMEDAREA                                 
000689         MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                
000690                                                                          
000691         SET BLOCKED-SECURITY-CHECK TO TRUE                               
000692       END-IF                                                             
000693     ELSE                                                                 
000694         MOVE ARTIKELN-UTGONGEN (CL-INDEX) TO MOD-TEMFSFEL                
000695     END-IF                                                               
000696     .                                                                    
000697     EJECT                                                                
000698                                                                          
000699 S20-HAMTA-FLPCOO          SECTION.                                       
000700     IF CLAG-KDPCOO > ' '                                                 
000701       IF DAGENS-DATUM > CLAG-TIGILTIG-PCOO                               
000702         MOVE JA  TO MOD-FLPCOO                                           
000703       ELSE                                                               
000704         MOVE NEJ TO MOD-FLPCOO                                           
000705       END-IF                                                             
000706     ELSE                                                                 
000707       MOVE JA    TO MOD-FLPCOO                                           
000708     END-IF                                                               
000709     IF MOD-FLPCOO = 'J'                                                  
000710       MOVE '*' TO MOD-FLPCOO                                             
000711     ELSE                                                                 
000712       MOVE ' ' TO MOD-FLPCOO                                             
000713     END-IF                                                               
000714     .                                                                    
000715     EJECT                                                                
000716                                                                          
000717 IMS-GET-MSG SECTION.                                                     
000718                                                                          
000719     MOVE '  QC' TO GODK-STATUSKODER                                      
000720     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
000721     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
000722     PERFORM IMS-STATUSKONTROLL                                           
000723     .                                                                    
000724     SKIP3                                                                
000725 IMS-INSERT-MSG SECTION.                                                  
000726                                                                          
000727     IF  MSGI-IDLAND-SPR NOT = 'GB'                                       
000728         MOVE '0' TO MFS-KDHUVOMR                                         
000729     END-IF                                                               
000730                                                                          
000731     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
000732     MOVE SPACE TO GODK-STATUSKODER                                       
000733     CALL CBLTDLI USING ISRT MSG-PCB                                      
000734                          MSG-IO-AREA MFS-IDMOD                           
000735     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
000736     PERFORM IMS-STATUSKONTROLL                                           
000737     .                                                                    
000738     EJECT                                                                
000739 IMS-BENA-LAS-ROTSEG SECTION.                                             
000740                                                                          
000741     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
000742            DELIMITED BY SIZE INTO SSA1                                   
000743     MOVE '  ' TO GODK-STATUSKODER                                        
000744     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA SSA1                      
000745     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
000746     PERFORM IMS-STATUSKONTROLL                                           
000747     .                                                                    
000748     SKIP3                                                                
000749 IMS-BENA-LAS-TEXTSEG SECTION.                                            
000750                                                                          
000751     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
000752            DELIMITED BY SIZE INTO SSA1                                   
000753     MOVE '  GE' TO GODK-STATUSKODER                                      
000754     CALL CBLTDLI USING GNP BENA-PCB DLI-IO-AREA SSA1                     
000755     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
000756     PERFORM IMS-STATUSKONTROLL                                           
000757     .                                                                    
000758     EJECT                                                                
000759 IMS-GET-WLARTC01-ARTIKEL  SECTION.                                       
000760                                                                          
000761     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
000762     DELIMITED BY SIZE INTO SSA1                                          
000763     MOVE '  GE' TO GODK-STATUSKODER                                      
000764     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA SSA1                      
000765     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
000766     PERFORM IMS-STATUSKONTROLL                                           
000767     .                                                                    
000768     SKIP3                                                                
000769 IMS-GNP-WLARTC11-CLAG SECTION.                                           
000770                                                                          
000771     MOVE 'WLARTC11 ' TO SSA1                                             
000772     MOVE '  GE' TO GODK-STATUSKODER                                      
000773     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA SSA1                     
000774     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
000775     PERFORM IMS-STATUSKONTROLL                                           
000776     .                                                                    
000777     EJECT                                                                
000778 IMS-GET-WLARTS01-ARTIKEL  SECTION.                                       
000779                                                                          
000780     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
000781     DELIMITED BY SIZE INTO SSA1                                          
000782     MOVE '  GE' TO GODK-STATUSKODER                                      
000783     CALL CBLTDLI USING GU ARTS-PCB DLI-IO-AREA SSA1                      
000784     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
000785     PERFORM IMS-STATUSKONTROLL                                           
000786     .                                                                    
000787     SKIP3                                                                
000788 IMS-GNP-WLARTS11-SLAG SECTION.                                           
000789                                                                          
000790     MOVE 'WLARTS11 ' TO SSA1                                             
000791     MOVE '  GE' TO GODK-STATUSKODER                                      
000792     CALL CBLTDLI USING GNP ARTS-PCB DLI-IO-AREA SSA1                     
000793     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
000794     PERFORM IMS-STATUSKONTROLL                                           
000795     .                                                                    
000796     EJECT                                                                
000797 IMS-GET-WLERSA-ARTIKEL     SECTION.                                      
000798                                                                          
000799     STRING 'WLERSA01(IDARTNR  =' W-IDARTNR-X ')'                         
000800            DELIMITED BY SIZE INTO SSA1                                   
000801     MOVE '  GE' TO GODK-STATUSKODER                                      
000802     CALL CBLTDLI USING GU ERSA-PCB DLI-IO-AREA SSA1                      
000803     MOVE ERSA-STATUS-CODE TO STATUS-WS                                   
000804     PERFORM IMS-STATUSKONTROLL                                           
000805     .                                                                    
000806     SKIP3                                                                
000807 IMS-GET-WLERSA-ERSETTNING  SECTION.                                      
000808                                                                          
000809     MOVE 'WLERSA11 ' TO SSA1                                             
000810     MOVE '  GE' TO GODK-STATUSKODER                                      
000811     CALL CBLTDLI USING GNP ERSA-PCB DLI-IO-AREA SSA1                     
000812     MOVE ERSA-STATUS-CODE TO STATUS-WS                                   
000813     PERFORM IMS-STATUSKONTROLL                                           
000814     .                                                                    
000815     EJECT                                                                
000816 IMS-GET-ARTM-WDK9 SECTION.                                               
000817                                                                          
000818     STRING 'WLARTM01(IDARTNR  =' W-IDARTNR-X ')'                         
000819     DELIMITED BY SIZE INTO SSA1                                          
000820     MOVE '  GE' TO GODK-STATUSKODER                                      
000821     CALL CBLTDLI USING GU ARTM-PCB ARTM-IO-AREA SSA1                     
000822     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
000823     PERFORM IMS-STATUSKONTROLL                                           
000824     .                                                                    
000825     SKIP3                                                                
000826 IMS-STATUSKONTROLL SECTION.                                              
000827                                                                          
000828     SET STATUS-IX TO 1                                                   
000829     SEARCH GODK-STATUS AT END CALL FELLOG                                
000830       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
000831     END-SEARCH                                                           
000832     .                                                                    
