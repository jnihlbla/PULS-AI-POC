000001 ID DIVISION.                                                             
000002 PROGRAM-ID.     W4010200.                                                
000003 AUTHOR.         INGRID DANIELSSON.                                       
000004 DATE-WRITTEN.   OKT 1977.                                                
000005     REMARKS.                                                             
000006*    FUNKTION.                                                            
000007*        TP-PROGRAM FÖR LAGRET (LAGERINFORMATION).                        
000008*        PROGRAMMET LÄSER LOGISK DATABAS WLARTC (ARTREG WDD6),            
000009*        LOGISKA DATABASEN WLBENA (BENÄMNINGSREG WDD3),                   
000010*        SAMT LOGISKA DATABASEN WLINLB (LEVPLAN WDD9)                     
000011*    INDATA.                                                              
000012*        TRANSAKTION: W4T102                                              
000013*        MID:         W4I10201                                            
000014*    UTDATA.                                                              
000015*        MOD:         W4O10201                                            
000016*    SUBPROGRAM                                                           
000017*        FELLOG                                                           
000018*                                                                         
000019*   ÄNDRINGAR:                                                            
000020*        03-05-15. TILLAGT FUNKTION FÖR ATT BEGRÄNSA INFORMATION          
000021*                  FÖR USER VARS SEC-IDLEVNR PÅ USER-BASEN                
000022*                  INTE ÄR LIKA MED HUVUDLEVERANTÖREN.                    
000023*                  ( SEC-IDLEVNR = SPACE, FÅR SE ALLT )    /C.E.          
000024*     E-TRACKER: 7450319  2008-HÖST    VOHF                               
000025*     E-TRACKER: 10254592 2015 DECOMISSION VOHF                           
000026*                                                                         
000027 ENVIRONMENT DIVISION.                                                    
000028 DATA DIVISION.                                                           
000029     EJECT                                                                
000030 WORKING-STORAGE SECTION.                                                 
000031                                                                          
000032*    -- CHECKED BY WY2000                                                 
000033*                                                                         
000034 77  IDARTNR             PIC X(9).                                        
000035 77  WS-KVBR             PIC S9(7)  COMP-3.                               
000036 77  JA                  PIC X                  VALUE 'J'.                
000037 77  NEJ                 PIC X                  VALUE 'N'.                
000038 77  DEFINITIV           PIC S9                 VALUE +1 COMP-3.          
000039 77  WS-IDLEVNR-8        PIC X(8)               VALUE SPACE.              
000040 77  WS-PRARTBES         PIC X(1)               VALUE 'N'.                
000041 77  DAGENS-AAAAMMDD     PIC 9(8)               VALUE ZERO.               
000042 77  WS-KVPB-TOT         PIC S9(6)V9(1)         VALUE ZERO.               
000043 77  WS-KVAKS            PIC S9(6)V9(1)         VALUE ZERO.               
000044 77  WS-KVLS             PIC S9(6)V9(1)         VALUE ZERO.               
000045 77  IX-CDC              PIC S9(9)  COMP SYNC   VALUE +1.                 
000046 77  IX-SDC              PIC S9(9)  COMP SYNC   VALUE +2.                 
000047 77  MAX-MOD-LAENGD      PIC S9(4)  COMP SYNC   VALUE ZERO.               
000048 77  WS-IDTRANS          PIC X(4).                                        
000049     88  WS-GODKAEND-BILD      VALUE '4101' '4102' '4103' '4104'          
000050                                     '4105' '4106' '4107' '4108'.         
000051     88  EGEN-MID              VALUE '4102'.                              
000052                                                                          
000053                                                                          
000054 77  SECURITY-SW                 PIC X       VALUE 'N'.                   
000055     88  PASSED-SECURITY-CHECK               VALUE 'J'.                   
000056     88  BLOCKED-SECURITY-CHECK              VALUE 'N'.                   
000057                                                                          
000058 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
000059                                                                          
000060 01  NYCKEL-SW           PIC X.                                           
000061     88  NYCKLAR-OK                             VALUE 'J'.                
000062                                                                          
000063 01  INDEX-ORD.                                                           
000064     03  IX              PIC S9(9)  COMP SYNC   VALUE ZERO.               
000065     03  IND             PIC S9(9)  COMP SYNC   VALUE ZERO.               
000066                                                                          
000067 01  W-IDSKYLT-X.                                                         
000068     03  W-IDSKYLT       PIC X(3).                                        
000069                                                                          
000070 01  W-IDARTNR-X.                                                         
000071     03  W-IDARTNR       PIC S9(9)   COMP-3     VALUE ZERO.               
000072                                                                          
000073 01  W-DAPRLIST-X.                                                        
000074     03  W-DAPRLIST      PIC   9(8)  VALUE ZERO.                          
000075                                                                          
000076 01  W-WDD901KY-X.                                                        
000077     03  W-IDARTNR-D9    PIC S9(9)   COMP-3     VALUE ZERO.               
000078     03  W-IDDC-D9       PIC X(2)               VALUE SPACE.              
000079     EJECT                                                                
000080 01  FELMEDDELANDE.                                                       
000081     03  FEL-1   PIC X(26)   VALUE 'PART NUMBER IS NOT NUMERIC'.          
000082     03  FEL-2   PIC X(35)   VALUE 'THIS PARTNO. IS NOT IN THE DAT        
000083-                                  'ABASE'.                               
000084     03  FEL-3   PIC X(29)  VALUE 'THIS PARTNO. HAS BEEN DELETED'.        
000085     03  FEL-5   PIC X(36)  VALUE  'MORE SUPERSESSION INF. ARE AVA        
000086-                                  'ILABLE'.                              
000087     EJECT                                                                
000088 01  DYNAMISKA-SUBPROGRAM.                                                
000089     03  CBLTDLI         PIC X(8)         VALUE 'CBLTDLI'.                
000090     03  FELLOG          PIC X(8)         VALUE 'FELLOG'.                 
000091     03  W005INIT        PIC X(8)         VALUE 'W005INIT'.               
000092     03  WMEDKONV        PIC X(8)         VALUE 'WMEDKONV'.               
000093     EJECT                                                                
000094*    ----- PARAMETRAR TILL SUBPROGRAM W005INIT                            
000095*01 -COPY WMSGINIT                                                        
000096     EJECT                                                                
000097*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
000098*01 -COPY WMEDAREA                                                        
000099     EJECT                                                                
000100 01  MESSAGE-CODES.                                                       
000101     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
000102     03  CONFLICT                PIC X(3)    VALUE '002'.                 
000103     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
000104     03  URVAL-SAKNAS            PIC X(3)    VALUE '005'.                 
000105     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
000106     03  ERR-NOT-REGISTERED      PIC X(3)    VALUE '010'.                 
000107     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
000108     03  ARTIKEL-SAKNAS          PIC X(3)    VALUE '017'.                 
000109     03  ARTIKEL-UTGANGEN        PIC X(3)    VALUE '018'.                 
000110     03  INF-UPDATE-NOT-DONE     PIC X(3)    VALUE '034'.                 
000111     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
000112     03  ARTIKEL-ERSATT          PIC X(3)    VALUE '220'.                 
000113     03  ARTIKEL-SAKNAS-SDC      PIC X(3)    VALUE '305'.                 
000114     03  EJ-GODK-REFILL          PIC X(3)    VALUE '307'.                 
000115     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
000116     03  ERR-NOT-AUTHORIZED      PIC X(3)    VALUE '405'.                 
000117                                                                          
000118 01  FILLER              PIC X(16)        VALUE 'SPAR-WDK611'.            
000119*01  WLARTC11   -COPY WDK611 -PRE SPAR-.                                  
000120     EJECT                                                                
000121*                    *** TP-AREOR                                         
000122*01  MID  -COPY W4I10201  -PRE MID-                                       
000123     EJECT                                                                
000124*01      -COPY  WMSGAREA                                                  
000125     EJECT                                                                
000126*    03  MOD   -COPY W4O10201 -PRE MOD- -RED MSG-AREA.                    
000127     EJECT                                                                
000128*01  -COPY WMFSAREA.                                                      
000129     EJECT                                                                
000130*            ARBETSAREOR TILL IMS-SEKTIONERNA                             
000131*                                                                         
000132 01  IMS-WS.                                                              
000133     03  FILLER          PIC X(8)    VALUE 'IMS-WS  '.                    
000134     SKIP3                                                                
000135*            *** STATUSKOD FRÅN IMS ***                                   
000136     03  STATUS-WS       PIC XX.                                          
000137         88  SEGMENT-FINNS           VALUE '  '.                          
000138         88  SEGMENT-SAKNAS          VALUE 'GE'.                          
000139         88  BASEN-SLUT              VALUE 'GB'.                          
000140     SKIP3                                                                
000141     03  GODK-STATUSKODER.                                                
000142         05  GODK-STATUS   PIC XX  OCCURS 5  INDEXED BY STATUS-IX.        
000143     SKIP3                                                                
000144     03  SSA1              PIC X(32).                                     
000145     03  SSA2              PIC X(32).                                     
000146     EJECT                                                                
000147*            *** IMS-FUNKTIONSKODER ***                                   
000148*    03      -COPY   W0003                                                
000149     EJECT                                                                
000150*            *** DLI INPUT-OUTPUT AREA ***                                
000151 01  DLI-IO-AREA         PIC X(900)      VALUE SPACE.                     
000152     SKIP3                                                                
000153*01  WLBENA11   -COPY WDD311 -PRE BENA-    -RED DLI-IO-AREA.              
000154     EJECT                                                                
000155*01  WLARTC01   -COPY WDK601               -RED DLI-IO-AREA.              
000156     EJECT                                                                
000157*01  WLARTC11   -COPY WDK611               -RED DLI-IO-AREA.              
000158     EJECT                                                                
000159*01  WLINLB11   -COPY WDD902 -PRE INLB11-  -RED DLI-IO-AREA.              
000160     EJECT                                                                
000161 01  DLI-IO-WDK701.                                                       
000162*    03         -COPY WDK701.                                             
000163 01  DLI-IO-WDK711.                                                       
000164*    03         -COPY WDK711.                                             
000165 01  DLI-IO-WDK621.                                                       
000166*    03         -COPY WDK621                                              
000167 LINKAGE SECTION.                                                         
000168*01         -COPY W0009     -PRE MSG-                                     
000169     EJECT                                                                
000170*01         -COPY W0008     -PRE USEA-                                    
000171         05  FILLER      PIC X.                                           
000172     EJECT                                                                
000173*01         -COPY W0008     -PRE BENA-                                    
000174         05  FILLER      PIC X.                                           
000175     EJECT                                                                
000176*01         -COPY W0008     -PRE ARTC-                                    
000177         05  FILLER      PIC X.                                           
000178     EJECT                                                                
000179*01         -COPY W0008     -PRE WDK7-                                    
000180         05  FILLER      PIC X.                                           
000181     EJECT                                                                
000182*01         -COPY W0008     -PRE INLB-                                    
000183         05  FILLER      PIC X.                                           
000184     EJECT                                                                
000185 PROCEDURE DIVISION USING  MSG-PCB   USEA-PCB BENA-PCB ARTC-PCB           
000186                           WDK7-PCB  INLB-PCB.                            
000187     ENTRY 'DLITCBL' USING MSG-PCB   USEA-PCB BENA-PCB ARTC-PCB           
000188                           WDK7-PCB  INLB-PCB.                            
000189     SKIP2                                                                
000190     PERFORM IMS-GET-MSG                                                  
000191     IF SEGMENT-FINNS                                                     
000192       PERFORM A-KOLLA-NYCKLAR                                            
000193       IF NYCKLAR-OK                                                      
000194         PERFORM S1-SECURITY-CHECK-PARTNO                                 
000195         IF PASSED-SECURITY-CHECK                                         
000196           PERFORM IMS-GET-WLARTC01-ARTIKEL                               
000197           IF SEGMENT-FINNS                                               
000198             PERFORM B-LAES-BEARB-REDIGERA-ARTREG                         
000199             PERFORM C-LAES-BEARB-RED-WLBENA-TEXT                         
000200             PERFORM D-LAES-RED-WLINLB-LEVINFO                            
000201           ELSE                                                           
000202             IF MSGI-IDARTNR NOT NUMERIC                                  
000203               PERFORM E-RENSA-NYCKLAR                                    
000204             END-IF                                                       
000205             MOVE FEL-2 TO MOD-TEMFSFEL                                   
000206           END-IF                                                         
000207         END-IF                                                           
000208       ELSE                                                               
000209          IF NOT WS-GODKAEND-BILD                                         
000210             PERFORM E-RENSA-NYCKLAR                                      
000211          END-IF                                                          
000212       END-IF                                                             
000213       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
000214       PERFORM IMS-ISRT-MSG                                               
000215     END-IF                                                               
000216     MOVE ZERO TO RETURN-CODE                                             
000217     GOBACK                                                               
000218     .                                                                    
000219     EJECT                                                                
000220 A-KOLLA-NYCKLAR SECTION.                                                 
000221                                                                          
000222     MOVE JA TO NYCKEL-SW                                                 
000223     IF MSG-DUBBLA-TRANSKODER                                             
000224       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I10201                 
000225       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
000226       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
000227       MOVE ZERO TO MID-VAGNSKIP                                          
000228     ELSE                                                                 
000229       MOVE MSG-INDATA-MINUS-1-TRANSKOD TO MID-W4I10201                   
000230       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
000231       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
000232     END-IF                                                               
000233                                                                          
000234     COMPUTE MAX-MOD-LAENGD = LENGTH OF MOD-W4O10201 + 4                  
000235                                                                          
000236     MOVE MFS-IDTRANS      TO WS-IDTRANS                                  
000237                                                                          
000238     IF MFS-IDTRANS NOT = '4102'                                          
000239       MOVE ZERO TO MID-VAGNSKIP                                          
000240     END-IF                                                               
000241                                                                          
000242     MOVE ALL '+'           TO MSGI-WMSGINIT                              
000243     MOVE '001'             TO MSGI-KDCALL                                
000244     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
000245     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
000246     MOVE '4102'            TO MSGI-IDTRANS                               
000247                                                                          
000248     IF MFS-IDTRANS = '4102'                                              
000249     OR (MID-IDARTNR1   NUMERIC                                           
000250     AND MID-IDARTNR1   > ZERO)                                           
000251         MOVE MID-IDARTNR1   TO MSGI-IDARTNR                              
000252     END-IF                                                               
000253                                                                          
000254     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
000255                                                                          
000256     MOVE LOW-VALUE TO MOD-W4O10201                                       
000257     MOVE 'W4O102N1' TO MFS-IDMOD                                         
000258     MOVE '4102' TO MOD-IDTRANS                                           
000259     MOVE ZERO TO MOD-VAGNSKIP                                            
000260     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
000261     MOVE MSGI-IDARTNR TO MOD-IDARTNR-UT                                  
000262     MOVE '-' TO MOD-STRECK                                               
000263     INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE               
000264                                                                          
000265     IF MSGI-IDARTNR NOT NUMERIC                                          
000266       MOVE FEL-1 TO MOD-TEMFSFEL                                         
000267       MOVE NEJ TO NYCKEL-SW                                              
000268     ELSE                                                                 
000269       MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL                               
000270       MOVE MSGI-IDARTNR TO W-IDARTNR                                     
000271     END-IF                                                               
000272                                                                          
000273     ACCEPT DAGENS-DATUM    FROM DATE                                     
000274     .                                                                    
000275     EJECT                                                                
000276 B-LAES-BEARB-REDIGERA-ARTREG   SECTION.                                  
000277                                                                          
000278*ARTREG CDC-WDK6                                                          
000279*                                                                         
000280     PERFORM BA-BEARB-REDIGERA-WLARTC01                                   
000281                                                                          
000282     IF ART-KDERS-UTG > ZERO                                              
000283       MOVE FEL-3         TO MOD-TEMFSFEL                                 
000284     ELSE                                                                 
000285       PERFORM IMS-GNP-WLARTC11-CLAG                                      
000286       PERFORM BB-BEARB-REDIGERA-WLARTC11                                 
000287                                                                          
000288*ARTREG   SDC-WDK7                                                        
000289*                                                                         
000290       PERFORM IMS-GET-WDK701-ARTIKEL                                     
000291                                                                          
000292       IF SEGMENT-FINNS                                                   
000293         PERFORM IMS-GNP-WDK711-SLAG                                      
000294         PERFORM BC-FLYTTA-SPAR-AREA                                      
000295                                                                          
000296         PERFORM UNTIL SEGMENT-SAKNAS                                     
000297           PERFORM BD-BEARB-REDIGERA-WDK711                               
000298           PERFORM IMS-GNP-WDK711-SLAG                                    
000299         END-PERFORM                                                      
000300                                                                          
000301         PERFORM BE-FLYTTA-TILL-MOD                                       
000302       END-IF                                                             
000303     END-IF                                                               
000304     .                                                                    
000305     EJECT                                                                
000306 BA-BEARB-REDIGERA-WLARTC01 SECTION.                                      
000307                                                                          
000308     MOVE ART-REKSIFFR    TO MOD-REKSIFFR                                 
000309     MOVE ART-IDLEVNR     TO MOD-IDLEVNR                                  
000310     MOVE ART-KDSORT      TO MOD-KDSORT                                   
000311     MOVE ART-IDFKNGRP    TO MOD-IDFKNGRP                                 
000312                                                                          
000313     .                                                                    
000314     EJECT                                                                
000315 BB-BEARB-REDIGERA-WLARTC11 SECTION.                                      
000316                                                                          
000317     MOVE CLAG-IDANSK TO MOD-IDANSK                                       
000318     MOVE CLAG-KDGK               TO MOD-KDGK                             
000319     MOVE CLAG-VKART              TO MOD-VKART                            
000320     MOVE CLAG-VLARTNTO           TO MOD-VLARTNTO                         
000321     MOVE CLAG-KDVVKL             TO MOD-KDVVKL                           
000322     MOVE CLAG-PRINK              TO MOD-PRINK                            
000323     PERFORM BBA-BASPRIS                                                  
000324     MOVE CLAG-PRARTSTD           TO MOD-PRARTSTD                         
000325     MOVE CLAG-PRARTSJK           TO MOD-PRARTSJK                         
000326     MOVE CLAG-PRDIRLON           TO MOD-PRDIRLON                         
000327     MOVE CLAG-PRDMTRL            TO MOD-PRDMTRL                          
000328     MOVE CLAG-PROVRPAL           TO MOD-PROVRPAL                         
000329     MOVE CLAG-ADLAGOMR           TO MOD-ADLAGOMR                         
000330     MOVE CLAG-ADGANG             TO MOD-ADGANG                           
000331     MOVE CLAG-ADPLATS            TO MOD-ADPLATS                          
000332     IF CLAG-ADLAGOMR-SVS   > ZERO OR                                     
000333        CLAG-ADLAGOMR-CD(1) > ZERO                                        
000334       MOVE JA                    TO MOD-FLERPL                           
000335     ELSE                                                                 
000336       MOVE NEJ                   TO MOD-FLERPL                           
000337     END-IF                                                               
000338     MOVE CLAG-BEFT               TO MOD-BEFT(IX-CDC)                     
000339                                     SPAR-CLAG-BEFT                       
000340     MOVE CLAG-KDARTHNT           TO MOD-KDARTHNT                         
000341     MOVE CLAG-KDERS              TO MOD-KDERS                            
000342                                                                          
000343     COMPUTE MOD-KVPB-TOT (IX-CDC) = CLAG-KVPB-SEP  +                     
000344                                     CLAG-KVPB-SATS +                     
000345                                     CLAG-KVPB-TPO                        
000346     MOVE CLAG-KVPB-SATS          TO MOD-KVPB-SATS                        
000347                                                                          
000348     COMPUTE MOD-KVAKS(IX-CDC)     = CLAG-KVAKS-CDC +                     
000349                                     CLAG-KVAKS-PAV +                     
000350                                     CLAG-KVAKS-T                         
000351     MOVE CLAG-KVLS               TO MOD-KVLS (IX-CDC)                    
000352     MOVE CLAG-KDFARLIG           TO MOD-KDFARLIG                         
000353     MOVE CLAG-KDARTURS           TO MOD-KDARTURS(IX-CDC)                 
000354                                     SPAR-CLAG-KDARTURS                   
000355     PERFORM S20-HAMTA-FLPCOO                                             
000356                                                                          
000357     MOVE CLAG-IDARTNR-EMBQ0      TO MOD-IDARTNR-EMBQ0(IX-CDC)            
000358                                     SPAR-CLAG-IDARTNR-EMBQ0              
000359     MOVE CLAG-KDEMBKOD-0         TO MOD-KDEMBKOD-0(IX-CDC)               
000360                                     SPAR-CLAG-KDEMBKOD-0                 
000361     MOVE CLAG-KVQPACK-0          TO MOD-KVQPACK-0                        
000362     MOVE CLAG-IDARTNR-EMBQ1      TO MOD-IDARTNR-EMBQ1(IX-CDC)            
000363                                     SPAR-CLAG-IDARTNR-EMBQ1              
000364     MOVE CLAG-KDEMBKOD-1         TO MOD-KDEMBKOD-1(IX-CDC)               
000365                                     SPAR-CLAG-KDEMBKOD-1                 
000366     MOVE CLAG-KVQPACK-1          TO MOD-KVQPACK-1                        
000367     MOVE CLAG-IDARTNR-EMBQ2      TO MOD-IDARTNR-EMBQ2(IX-CDC)            
000368                                     SPAR-CLAG-IDARTNR-EMBQ2              
000369     MOVE CLAG-KDEMBKOD-2         TO MOD-KDEMBKOD-2(IX-CDC)               
000370                                     SPAR-CLAG-KDEMBKOD-2                 
000371     MOVE CLAG-KVQPACK-2          TO MOD-KVQPACK-2                        
000372     MOVE CLAG-IDARTNR-EMBQ3      TO MOD-IDARTNR-EMBQ3(IX-CDC)            
000373                                     SPAR-CLAG-IDARTNR-EMBQ3              
000374     MOVE CLAG-KVQPACK-3          TO MOD-KVQPACK-3                        
000375     MOVE CLAG-IDARTNR-EMBQ4      TO MOD-IDARTNR-EMBQ4(IX-CDC)            
000376                                     SPAR-CLAG-IDARTNR-EMBQ4              
000377     MOVE CLAG-KVQPACK-4          TO MOD-KVQPACK-4                        
000378                                                                          
000379     MOVE +1                      TO IX                                   
000380     MOVE +1                      TO IND                                  
000381     IF SEGMENT-FINNS                                                     
000382        PERFORM UNTIL IND > +4                                            
000383           MOVE CLAG-IDKAT (IND)  TO MOD-IDKAT (IX)                       
000384           ADD +1                 TO IX                                   
000385           ADD +1                 TO IND                                  
000386        END-PERFORM                                                       
000387     END-IF                                                               
000388     .                                                                    
000389     EJECT                                                                
000390 BBA-BASPRIS  SECTION.                                                    
000391     SKIP1                                                                
000392     MOVE FUNCTION CURRENT-DATE(1:8) TO DAGENS-AAAAMMDD                   
000393     COMPUTE W-DAPRLIST = 99999999 - DAGENS-AAAAMMDD                      
000394     PERFORM IMS-GNP-WLARTC21                                             
000395     IF SEGMENT-SAKNAS                                                    
000396       MOVE CLAG-PRARTSTD       TO MOD-PRARTBES                           
000397     ELSE                                                                 
000398       MOVE NEJ                 TO WS-PRARTBES                            
000399       PERFORM UNTIL  SEGMENT-SAKNAS                                      
000400         IF PRL-SUINLEV-PR > ZERO                                         
000401           MOVE PRL-PRARTBES-PR  TO MOD-PRARTBES                          
000402           SET SEGMENT-SAKNAS TO TRUE                                     
000403         ELSE                                                             
000404           IF WS-PRARTBES = NEJ                                           
000405             MOVE PRL-PRARTBES-PR TO MOD-PRARTBES                         
000406             MOVE JA              TO WS-PRARTBES                          
000407           END-IF                                                         
000408           PERFORM IMS-GNP-WLARTC21                                       
000409         END-IF                                                           
000410       END-PERFORM                                                        
000411     END-IF                                                               
000412     .                                                                    
000413     EJECT                                                                
000414 BC-FLYTTA-SPAR-AREA SECTION.                                             
000415                                                                          
000416     MOVE SPAR-CLAG-BEFT          TO MOD-BEFT (IX-SDC)                    
000417     MOVE SPAR-CLAG-KDARTURS      TO MOD-KDARTURS(IX-SDC)                 
000418     MOVE SPAR-CLAG-KDEMBKOD-0    TO MOD-KDEMBKOD-0 (IX-SDC)              
000419     MOVE SPAR-CLAG-KDEMBKOD-1    TO MOD-KDEMBKOD-1 (IX-SDC)              
000420     MOVE SPAR-CLAG-KDEMBKOD-2    TO MOD-KDEMBKOD-2 (IX-SDC)              
000421     MOVE SPAR-CLAG-IDARTNR-EMBQ0 TO MOD-IDARTNR-EMBQ0 (IX-SDC)           
000422     MOVE SPAR-CLAG-IDARTNR-EMBQ1 TO MOD-IDARTNR-EMBQ1 (IX-SDC)           
000423     MOVE SPAR-CLAG-IDARTNR-EMBQ2 TO MOD-IDARTNR-EMBQ2 (IX-SDC)           
000424     MOVE SPAR-CLAG-IDARTNR-EMBQ3 TO MOD-IDARTNR-EMBQ3 (IX-SDC)           
000425     MOVE SPAR-CLAG-IDARTNR-EMBQ4 TO MOD-IDARTNR-EMBQ4 (IX-SDC)           
000426     .                                                                    
000427     EJECT                                                                
000428 BD-BEARB-REDIGERA-WDK711 SECTION.                                        
000429                                                                          
000430     COMPUTE WS-KVPB-TOT = WS-KVPB-TOT    +                               
000431                           SLAG-KVPB-REF                                  
000432                                                                          
000433     COMPUTE WS-KVAKS    = WS-KVAKS  +                                    
000434                           SLAG-KVAKS-SDC +                               
000435                           SLAG-KVAKS-PAV                                 
000436                                                                          
000437     COMPUTE WS-KVLS     = WS-KVLS   +                                    
000438                           SLAG-KVLS                                      
000439     .                                                                    
000440     EJECT                                                                
000441 BE-FLYTTA-TILL-MOD SECTION.                                              
000442                                                                          
000443     MOVE WS-KVPB-TOT  TO MOD-KVPB-TOT(IX-SDC)                            
000444                                                                          
000445     MOVE WS-KVAKS     TO MOD-KVAKS(IX-SDC)                               
000446                                                                          
000447     MOVE WS-KVLS      TO MOD-KVLS(IX-SDC)                                
000448     .                                                                    
000449     EJECT                                                                
000450 C-LAES-BEARB-RED-WLBENA-TEXT SECTION.                                    
000451                                                                          
000452*** SEGMENT 01 (BENÄMNINGSNUMER)                                          
000453*                                                                         
000454     PERFORM IMS-BENA-LASGU-ROTSEG                                        
000455                                                                          
000456*** SEGMENT 11 (NATIONALITETSTECKEN)                                      
000457*                                                                         
000458     MOVE 'GB ' TO        W-IDSKYLT                                       
000459     PERFORM IMS-BENA-LASGNP-TEXTSEG                                      
000460     IF SEGMENT-FINNS                                                     
000461         MOVE BENA-TEXT-BEART TO MOD-BEART-ENG                            
000462     ELSE                                                                 
000463         MOVE SPACE TO MOD-BEART-ENG                                      
000464     END-IF                                                               
000465     MOVE 'S  ' TO        W-IDSKYLT                                       
000466     PERFORM IMS-BENA-LASGNP-TEXTSEG                                      
000467     IF SEGMENT-FINNS                                                     
000468         MOVE BENA-TEXT-BEART TO MOD-BEART-SVE                            
000469     ELSE                                                                 
000470         MOVE SPACE TO MOD-BEART-SVE                                      
000471     END-IF                                                               
000472     .                                                                    
000473     EJECT                                                                
000474 D-LAES-RED-WLINLB-LEVINFO SECTION.                                       
000475                                                                          
000476     MOVE MSGI-IDARTNR  TO W-IDARTNR-D9                                   
000477     MOVE MSGI-IDDC     TO W-IDDC-D9                                      
000478     PERFORM IMS-GET-WLINLB-ARTIKEL                                       
000479     IF SEGMENT-FINNS                                                     
000480***   SEGMENT 92 (LEVERANSPLAN-INFO)                                      
000481*                                                                         
000482       MOVE ZERO TO WS-KVBR                                               
000483       PERFORM IMS-GET-WLINLB-LEVERANSPLAN                                
000484                                                                          
000485       PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                         
000486           ADD INLB11-KVBR  TO WS-KVBR                                    
000487           PERFORM IMS-GET-WLINLB-LEVERANSPLAN                            
000488       END-PERFORM                                                        
000489                                                                          
000490       MOVE WS-KVBR TO MOD-KVBR                                           
000491     END-IF                                                               
000492     .                                                                    
000493     EJECT                                                                
000494 E-RENSA-NYCKLAR SECTION.                                                 
000495                                                                          
000496     MOVE MFS-RENSA-FAELT           TO  MOD-IDARTNR-UT                    
000497     .                                                                    
000498     EJECT                                                                
000499                                                                          
000500 S1-SECURITY-CHECK-PARTNO SECTION.                                        
000501     SKIP2                                                                
000502*    --- CHECK IF USER IS GRANTED TO SEE PART-INFO                        
000503     PERFORM IMS-GET-WLARTC01-ARTIKEL                                     
000504     IF  SEGMENT-FINNS                                                    
000505       MOVE ART-IDLEVNR          TO WS-IDLEVNR-8                          
000506       IF MSGI-KDARBTYP-SEC-IDLEV = WS-IDLEVNR-8                          
000507       OR MSGI-KDARBTYP-SEC-IDLEV = SPACE OR LOW-VALUE                    
000508*        --- USER GRANTED                                                 
000509         SET PASSED-SECURITY-CHECK TO TRUE                                
000510       ELSE                                                               
000511*        --- OBEHÖRIG USER / USER NOT AUTHORIZED                          
000512         MOVE ERR-NOT-AUTHORIZED TO MED-IDMFSFEL                          
000513         CALL WMEDKONV USING MED-WMEDAREA                                 
000514         MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                
000515                                                                          
000516         SET BLOCKED-SECURITY-CHECK TO TRUE                               
000517       END-IF                                                             
000518     ELSE                                                                 
000519       MOVE FEL-2 TO MOD-TEMFSFEL                                         
000520     END-IF                                                               
000521     .                                                                    
000522     EJECT                                                                
000523                                                                          
000524 S20-HAMTA-FLPCOO          SECTION.                                       
000525     IF CLAG-KDPCOO > ' '                                                 
000526       IF DAGENS-DATUM > CLAG-TIGILTIG-PCOO                               
000527         MOVE JA  TO MOD-FLPCOO                                           
000528       ELSE                                                               
000529         MOVE NEJ TO MOD-FLPCOO                                           
000530       END-IF                                                             
000531     ELSE                                                                 
000532       MOVE JA    TO MOD-FLPCOO                                           
000533     END-IF                                                               
000534     IF MOD-FLPCOO = 'J'                                                  
000535       MOVE '*' TO MOD-FLPCOO                                             
000536     ELSE                                                                 
000537       MOVE ' ' TO MOD-FLPCOO                                             
000538     END-IF                                                               
000539     .                                                                    
000540     EJECT                                                                
000541                                                                          
000542 IMS-GET-MSG SECTION.                                                     
000543                                                                          
000544     MOVE '  QC' TO GODK-STATUSKODER                                      
000545     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
000546     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
000547     PERFORM IMS-STATUS-KONTROLL                                          
000548     .                                                                    
000549     SKIP3                                                                
000550 IMS-ISRT-MSG SECTION.                                                    
000551                                                                          
000552     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
000553       MOVE '0' TO MFS-KDHUVOMR                                           
000554     END-IF                                                               
000555     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
000556     MOVE SPACE TO GODK-STATUSKODER                                       
000557     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
000558     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
000559     PERFORM IMS-STATUS-KONTROLL                                          
000560     .                                                                    
000561     EJECT                                                                
000562 IMS-GET-WLARTC01-ARTIKEL SECTION.                                        
000563                                                                          
000564     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
000565            DELIMITED BY SIZE INTO SSA1                                   
000566     MOVE '  GE' TO GODK-STATUSKODER                                      
000567     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA SSA1                      
000568     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
000569     PERFORM IMS-STATUS-KONTROLL                                          
000570     .                                                                    
000571     SKIP3                                                                
000572 IMS-GNP-WLARTC11-CLAG SECTION.                                           
000573                                                                          
000574     MOVE 'WLARTC11' TO SSA1                                              
000575     MOVE '  GE'     TO GODK-STATUSKODER                                  
000576     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA SSA1                     
000577     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
000578     PERFORM IMS-STATUS-KONTROLL                                          
000579     .                                                                    
000580     EJECT                                                                
000581 IMS-GNP-WLARTC21 SECTION.                                                
000582                                                                          
000583     STRING 'WLARTC21(DAPRLIST=>' W-DAPRLIST-X ')'                        
000584            DELIMITED BY SIZE INTO SSA1                                   
000585     MOVE '  GE' TO GODK-STATUSKODER                                      
000586     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-WDK621 SSA1                   
000587     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
000588     PERFORM IMS-STATUS-KONTROLL                                          
000589     .                                                                    
000590     EJECT                                                                
000591 IMS-GET-WDK701-ARTIKEL SECTION.                                          
000592                                                                          
000593     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
000594            DELIMITED BY SIZE INTO SSA1                                   
000595     MOVE '  GE' TO GODK-STATUSKODER                                      
000596     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK701 SSA1                    
000597     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
000598     PERFORM IMS-STATUS-KONTROLL                                          
000599     .                                                                    
000600     SKIP3                                                                
000601 IMS-GNP-WDK711-SLAG SECTION.                                             
000602                                                                          
000603     MOVE 'WDK711  ' TO SSA1                                              
000604     MOVE '  GE'     TO GODK-STATUSKODER                                  
000605     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK711 SSA1                   
000606     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
000607     PERFORM IMS-STATUS-KONTROLL                                          
000608     .                                                                    
000609     EJECT                                                                
000610 IMS-BENA-LASGU-ROTSEG SECTION.                                           
000611                                                                          
000612     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
000613             DELIMITED BY SIZE INTO SSA1                                  
000614     MOVE '  ' TO GODK-STATUSKODER                                        
000615     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA SSA1                      
000616     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
000617     PERFORM IMS-STATUS-KONTROLL                                          
000618     .                                                                    
000619     SKIP3                                                                
000620 IMS-BENA-LASGNP-TEXTSEG SECTION.                                         
000621                                                                          
000622     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
000623             DELIMITED BY SIZE INTO SSA1                                  
000624     MOVE '  GE' TO GODK-STATUSKODER                                      
000625     CALL CBLTDLI USING GNP BENA-PCB DLI-IO-AREA SSA1                     
000626     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
000627     PERFORM IMS-STATUS-KONTROLL                                          
000628     .                                                                    
000629     EJECT                                                                
000630 IMS-GET-WLINLB-ARTIKEL SECTION.                                          
000631                                                                          
000632     STRING 'WLINLB01(WDD901KY =' W-WDD901KY-X ')'                        
000633            DELIMITED BY SIZE INTO SSA1                                   
000634     MOVE '  GE' TO GODK-STATUSKODER                                      
000635     CALL CBLTDLI USING GU INLB-PCB DLI-IO-AREA SSA1                      
000636     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
000637     PERFORM IMS-STATUS-KONTROLL                                          
000638     .                                                                    
000639     SKIP3                                                                
000640 IMS-GET-WLINLB-LEVERANSPLAN SECTION.                                     
000641                                                                          
000642     MOVE 'WLINLB11'  TO SSA1                                             
000643     MOVE '  GE' TO GODK-STATUSKODER                                      
000644     CALL CBLTDLI USING GNP INLB-PCB DLI-IO-AREA SSA1                     
000645     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
000646     PERFORM IMS-STATUS-KONTROLL                                          
000647     .                                                                    
000648     EJECT                                                                
000649 IMS-STATUS-KONTROLL SECTION.                                             
000650                                                                          
000651     SET STATUS-IX TO 1                                                   
000652     SEARCH GODK-STATUS                                                   
000653         AT END  CALL FELLOG                                              
000654         WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                
000655     END-SEARCH                                                           
000656     .                                                                    
