000001 ID DIVISION.                                                             
000002     SKIP2                                                                
000003 PROGRAM-ID.     W3711600.                                                
000004*AUTHOR.         RONNY STENHOLM.                                          
000005*DATE-WRITTEN.   92/06/17.                                                
000006                                                                          
000007*    REMARKS.                                                             
000008*                                                                         
000009*                                                                         
000010*    FUNKTION:                                                            
000011*                                                                         
000012*        PROGRAMMET UPPDATERAR WLXXCQ (WDGX)                              
000013*        PROGRAMMET UPPDATERAR WLBYTF (WDM6)                              
000014*        MED RETURER INKOMNA FRÅN VIPS FÖR                                
000015*        IDENTIFIERING, KONTROLL OCH GODKÄNNANDE.                         
000016*                                                                         
000017*    ABENDKODER:                                                          
000018*        U1000 -  . . . .                                                 
000019*        U0016 - OM RAPPORT/DISTRNR REDAN ÄR UPPLAGT PÅ WDM6.             
000020*                                                                         
000021*    CHANGE LOG:                                                          
000022*                                                                         
000023*    DIGAMBAR/20021003                                                    
000024*    WDM6E INDEX IS CHANGED TO REFER THE IDBYTRAP-9KOMPL INSTEAD          
000025*    OF THE IDBYTRAP. THIS IS TO SHOW THE DETAILS IN DESCENDING           
000026*    ORDER OF THE IDBYTRAP.IDBYTRAP-9KOMPL FIELD IS ADDED IN              
000027*    WDM611                                                               
000028                                                                          
000029                                                                          
000030     SKIP3                                                                
000031 ENVIRONMENT DIVISION.                                                    
000032     SKIP2                                                                
000033 INPUT-OUTPUT SECTION.                                                    
000034                                                                          
000035 FILE-CONTROL.                                                            
000036     SKIP2                                                                
000037*          --- VIPS                                                       
000038     SELECT W46011                     ASSIGN TO W37116D1.                
000039*          --- UTFIL:                                                     
000040*                        - -  FIL MED DUBBLETTER                          
000041*                                                                         
000042     SELECT W371DUB                    ASSIGN TO W37116D2.                
000043*          --- UTFIL:                                                     
000044*          --- ARTIKLAR SOM INTE FINNS                                    
000045     SELECT W371FEL                    ASSIGN TO W37116D3.                
000046*          --- ARTIKLAR SOM SAKNAR SJÄLVKOST                              
000047     SELECT W371FE2                    ASSIGN TO W37116D4.                
000048     SKIP2                                                                
000049     EJECT                                                                
000050 DATA DIVISION.                                                           
000051     SKIP3                                                                
000052 FILE SECTION.                                                            
000053     SKIP3                                                                
000054 FD  W46011                                                               
000055     RECORDING       F                                                    
000056     BLOCK CONTAINS  0.                                                   
000057     SKIP2                                                                
000058*01  -COPY W37116      -L.                                                
000059     EJECT                                                                
000060*---                                                                      
000061 FD  W371DUB                                                              
000062     LABEL RECORD   STANDARD                                              
000063     RECORDING      F                                                     
000064     BLOCK CONTAINS 0.                                                    
000065     SKIP2                                                                
000066 01  DUB-POST       PIC X(80).                                            
000067     SKIP2                                                                
000068 FD  W371FEL                                                              
000069     LABEL RECORD   STANDARD                                              
000070     RECORDING      F                                                     
000071     BLOCK CONTAINS 0.                                                    
000072     SKIP2                                                                
000073*01  POST   -COPY W37116  -PRE FEL- -L.                                   
000074     SKIP2                                                                
000075 FD  W371FE2                                                              
000076     LABEL RECORD   STANDARD                                              
000077     RECORDING      F                                                     
000078     BLOCK CONTAINS 0.                                                    
000079     SKIP2                                                                
000080*01  POST   -COPY W37116  -PRE FE2- -L.                                   
000081     SKIP2                                                                
000082*---                                                                      
000083 WORKING-STORAGE SECTION.                                                 
000084     SKIP2                                                                
000085                                                                          
000086*    -- CHECKED BY WY2000                                                 
000087 77  IDPGM                       PIC X(8)    VALUE 'W3711600'.            
000088 01  CHKP-VAR.                                                            
000089 03  CHKP-MSG-IO-AREA-LENGTH     PIC S9(9)   VALUE +32 COMP SYNC.         
000090 03  CHKP-MSG-IO-AREA            PIC X(32)   VALUE SPACE.                 
000091 03  CHKP-AREA-LENGTH            PIC S9(9)   VALUE +32 COMP SYNC.         
000092 03  CHKP-AREA                   PIC X(32)   VALUE SPACE.                 
000093 03  CHKP-ANT                    PIC S9(3)   VALUE +0.                    
000094 03  CHKP-MAX                    PIC S9(3)   VALUE +900.                  
000095 03  CHKP-TOT                    PIC S9(7)   VALUE ZERO.                  
000096 77  JA                          PIC X       VALUE 'J'.                   
000097 77  NEJ                         PIC X       VALUE 'N'.                   
000098 77  NY-RAPPORT                  PIC X       VALUE 'N'.                   
000099 77  POSTER-BEHANDLADE           PIC X       VALUE 'N'.                   
000100 77  SKRIV-RAD                   PIC X       VALUE 'J'.                   
000101 77  W-KVPOST-3142               PIC S9(7)   VALUE ZERO COMP-3.           
000102 77  SPAR-IDDISTR                PIC S9(5)   VALUE ZERO COMP-3.           
000103 77  SPAR-IDBYTRAP               PIC S9(7)   VALUE ZERO COMP-3.           
000104 77  W-KVRETUR-TOT               PIC S9(7)   VALUE ZERO COMP-3.           
000105 77  W-WDM611-RAD-RAK            PIC S9(7)   VALUE ZERO COMP-3.           
000106 77  INDX                        PIC S9(9)   VALUE ZERO COMP-3.           
000107 77  WS-FLBYTGAR                 PIC X       VALUE ' '.                   
000108 77  FOERSTA-POST                PIC X       VALUE 'J'.                   
000109 01  FILLER                  PIC X(16)   VALUE 'ANTAL-POSTER'.            
000110 01  WS-ANTAL-POSTER         PIC S9(9)   VALUE ZERO COMP-3.               
000111     SKIP2                                                                
000112 01  FELTEXT.                                                             
000113     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
000114     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
000115                                                                          
000116 01  FILLER                  PIC X(16)   VALUE 'WS-SEKTION'.              
000117 01  WS-SEKTION                  PIC X(30)   VALUE SPACE.                 
000118 01  FILLER                  PIC X(16)   VALUE 'WS-IMS-SEKTION'.          
000119 01  WS-IMS-SEKTION              PIC X(30)   VALUE SPACE.                 
000120                                                                          
000121 77  W46011-EOF-SW               PIC X       VALUE 'N'.                   
000122     88  END-OF-W46011                       VALUE 'J'.                   
000123 77  OBJ-RAD-SLUT-SW             PIC X       VALUE 'N'.                   
000124     88  OBJ-RAD-SLUT                        VALUE 'J'.                   
000125 77  OLD-NEW-RAPP-SW             PIC X       VALUE 'N'.                   
000126     88  OLD-RAPPORT                         VALUE 'J'.                   
000127     88  NEW-RAPPORT                         VALUE 'N'.                   
000128 77  FOERSTA-DUB-SW              PIC X       VALUE 'J'.                   
000129     88  FOERSTA-DUB-POST                    VALUE 'J'.                   
000130     EJECT                                                                
000131 77  W-IDBYTRAP-9KOMPL           PIC S9(7)   VALUE ZERO COMP-3.           
000132 77  W-9KOMPL                    PIC  9(7)   VALUE 9999999.               
000133                                                                          
000134 01  WS-IDBYTRAD                 PIC 9(4)    VALUE ZERO.                  
000135 01  FILLER REDEFINES WS-IDBYTRAD.                                        
000136     03 WS-IDBYTRAD-XX           PIC 9(2).                                
000137     03 WS-IDBYTRAD-00           PIC 9(2).                                
000138                                                                          
000139                                                                          
000140 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
000141 01  FILLER REDEFINES DAGENS-DATUM.                                       
000142     03  DAGENS-DATUM-AAR        PIC 9(2).                                
000143     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
000144     03  DAGENS-DATUM-DAG        PIC 9(2).                                
000145     EJECT                                                                
000146                                                                          
000147 01  TEST-KDBYTREF               PIC X(3).                                
000148                                                                          
000149*01  FILLER -COPY WWBYT15   -RED TEST-KDBYTREF                            
000150     EJECT                                                                
000151 01  -COPY WWDCKONS                                                       
000152     EJECT                                                                
000153 01  -COPY WWDC99                                                         
000154     EJECT                                                                
000155 01  DYNAMISKA-SUBPROGRAM.                                                
000156*                                                                         
000157     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
000158     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
000159     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
000160     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
000161*- - - - - - -  - - - - - - - RETURKODER                                  
000162 01  RETURKODER.                                                          
000163     03  RKOD-ABEND-UTAN-DUMP    PIC S9(4) COMP VALUE +16.                
000164*- - - - - - - - - - - - - -  MEDDELANDE                                  
000165 01  MEDDELANDE.                                                          
000166     03  MEDDELANDE-1           PIC X(29)   VALUE                         
000167         'RAPPORT/DISTRNR FINNS REDAN  '.                                 
000168     03  MEDDELANDE-2           PIC X(42)   VALUE                         
000169         'OBS TAG BORT ALLA POSTER MED DENNA NYCKEL '.                    
000170     03  MEDDELANDE-3           PIC X(42)   VALUE                         
000171         'RADNUMMER EJ NUMERISKT TA BORT POSTEN '.                        
000172     03  MEDDELANDE-4           PIC X(42)   VALUE                         
000173         'RADNUMMER NUMERISKT MEN FELAKTIGT     '.                        
000174* RUBRIK TEXTER TILL DUBLETT FILEN (FILEN SKICKAS SOM MAIL)               
000175 01  RUBRIKER.                                                            
000176     03  RUBRIK-1               PIC X(41)   VALUE                         
000177         'VIPS HAS SENT INCORRECT RHM TRANSACTIONS '.                     
000178     03  RUBRIK-2               PIC X(45)   VALUE                         
000179         'TO ROUTINE W371D1 IN PULS (EXCHANGE RETURNS).'.                 
000180     03  RUBRIK-3               PIC X(41)   VALUE                         
000181         'REASON FOR ERROR COULD BE THAT VIPS SEND '.                     
000182     03  RUBRIK-4               PIC X(49)   VALUE                         
000183         'DUPLICATE REPORTS OR THAT DISTRICT/REPORT NUMBER '.             
000184     03  RUBRIK-5               PIC X(18)   VALUE                         
000185         'ARE EQUAL TO ZERO.'.                                            
000186     03  RUBRIK-6               PIC X(40)   VALUE                         
000187         'DISTRICT   CUSTOMER   REPORT     PARTID '.                      
000188     EJECT                                                                
000189*    --- PARAMETRAR TILL POSTSUM                                          
000190*                                                                         
000191*01  -COPY W0005   -PRE  POSTSUM-                                         
000192     EJECT                                                                
000193 01  IN-AREA-START               PIC X(24)   VALUE                        
000194                                             'IN-AREA-START'.             
000195     SKIP2                                                                
000196                                                                          
000197*01  AREA -COPY W37116     -PRE IN-                                       
000198*                                                                         
000199 01  RAD-AREA-START               PIC X(24)   VALUE                       
000200                                             'RAD-AREA-START'.            
000201 01  RAD-AREA.                                                            
000202     03 RAD-FILLER1               PIC X(4)  VALUE SPACE.                  
000203     03 RAD-IDDISTR               PIC Z(3)9.                              
000204     03 RAD-FILLER2               PIC X(5)  VALUE SPACE.                  
000205     03 RAD-IDKUND                PIC Z(5)9.                              
000206     03 RAD-FILLER3               PIC X(2)  VALUE SPACE.                  
000207     03 RAD-IDBYTRAP              PIC Z(6)9.                              
000208     03 FILLER4                   PIC X(2)  VALUE SPACE.                  
000209     03 RAD-IDARTNR               PIC Z(8)9.                              
000210     03 FILLER5                   PIC X(41) VALUE SPACE.                  
000211     EJECT                                                                
000212 01  DUB-AREA                     PIC X(80).                              
000213     EJECT                                                                
000214 01  FILLER                   PIC X(16) VALUE 'FEL-AREA'.                 
000215*01  AREA  -PRE FEL- -COPY W37116                                         
000216     EJECT                                                                
000217 01  FILLER                   PIC X(16) VALUE 'FE2-AREA'.                 
000218*01  AREA  -PRE FE2- -COPY W37116                                         
000219     EJECT                                                                
000220 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
000221     SKIP3                                                                
000222 01  NYCKLAR-TILL-DLI.                                                    
000223     03  W-WDGXKEY-X.                                                     
000224         05  FILLER              PIC X(4)   VALUE '3141'.                 
000225         05  FILLER              PIC X(26)  VALUE LOW-VALUE.              
000226     03  W-KDSEGKEY-X.                                                    
000227         05  W-KDSEGKEY          PIC X(30)    VALUE SPACE.                
000228                                                                          
000229     03  W-WDM601KY-X.                                                    
000230         05  W-IDDISTR           PIC S9(5)    VALUE ZERO COMP-3.          
000231         05  W-IDBYTRAP          PIC S9(7)    VALUE ZERO COMP-3.          
000232                                                                          
000233     03  W-WDM611KY-X.                                                    
000234         05  W-IDARTNR-OBJ       PIC S9(9)   VALUE ZERO COMP-3.           
000235         05  W-IDTABNR           PIC S9(3)   VALUE ZERO COMP-3.           
000236                                                                          
000237                                                                          
000238     03  W-IDARTNR-ART-X.                                                 
000239         05 W-IDARTNR-ART    PIC S9(9)       COMP-3.                      
000240                                                                          
000241     SKIP2                                                                
000242*    --- STATUS-KOD FRÅN IMS                                              
000243 01  STATUS-WS                   PIC XX.                                  
000244     88  SEGMENT-FINNS                       VALUE '  '.                  
000245     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
000246     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
000247     88  SEGMENT-SLUT                        VALUE 'GB'.                  
000248     88  IMS-EJ-OK                           VALUE 'XD'.                  
000249     SKIP2                                                                
000250 01  GODK-STATUSKODER.                                                    
000251     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
000252     SKIP3                                                                
000253 01  SSA1                        PIC X(64).                               
000254 01  SSA2                        PIC X(64).                               
000255     EJECT                                                                
000256*    --- IMS FUNKTIONSKODER                                               
000257*01  -COPY W0003                                                          
000258     EJECT                                                                
000259*    ---  DLI INPUT-OUTPUT AREA                                           
000260 01  FILLER                      PIC X(16) VALUE 'DLI-IO-AREA'.           
000261     SKIP3                                                                
000262 01  DLI-IO-AREA.                                                         
000263     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
000264     SKIP3                                                                
000265     03  WLXXCQ01 REDEFINES IO-AREA.                                      
000266*        05  -COPY WDGX01  -PRE XXCQ-                                     
000267     SKIP3                                                                
000268     03  WLXXCQ40 REDEFINES IO-AREA.                                      
000269*        05  -COPY WDGX3142  -PRE XXCQ-                                   
000270     SKIP3                                                                
000271     03  WLBYTF01 REDEFINES IO-AREA.                                      
000272*        05  -COPY WDM601  -PRE BYT01-                                    
000273     SKIP3                                                                
000274     03  WLBYTF11 REDEFINES IO-AREA.                                      
000275*        05  -COPY WDM611  -PRE BYT11-                                    
000276                                                                          
000277 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLARTC01'.                    
000278 01  DLI-IO-WLARTC01.                                                     
000279*    03  -COPY WDK601  -PRE ARTC-                                         
000280 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLARTC11'.                    
000281 01  DLI-IO-WLARTC11.                                                     
000282*    03  -COPY WDK611  -PRE ARTC-                                         
000283     EJECT                                                                
000284 LINKAGE SECTION.                                                         
000285                                                                          
000286*01  -COPY W0009   -PRE MSG-                                              
000287     EJECT                                                                
000288*01  -COPY W0008  -PRE XXCQ-                                              
000289     05  FILLER                  PIC X.                                   
000290     EJECT                                                                
000291*01  -COPY W0008  -PRE BYTF-                                              
000292     05  FILLER                  PIC X.                                   
000293     EJECT                                                                
000294*01  -COPY W0008  -PRE ARTC-                                              
000295     05  FILLER                  PIC X.                                   
000296     EJECT                                                                
000297 PROCEDURE DIVISION  USING MSG-PCB XXCQ-PCB BYTF-PCB ARTC-PCB.            
000298     ENTRY 'DLITCBL' USING MSG-PCB XXCQ-PCB BYTF-PCB ARTC-PCB.            
000299                                                                          
000300     SKIP2                                                                
000301     PERFORM A-INIT                                                       
000302     PERFORM S01-LAES-W46011                                              
000303     PERFORM B-KOLLA-CHECKPOINT                                           
000304     IF NOT END-OF-W46011                                                 
000305       MOVE JA TO POSTER-BEHANDLADE                                       
000306     END-IF                                                               
000307     PERFORM UNTIL END-OF-W46011                                          
000308       IF SPAR-IDDISTR = IN-IDDISTR AND                                   
000309          SPAR-IDBYTRAP = IN-IDBYTRAP                                     
000310         IF W-WDM611-RAD-RAK = ZERO                                       
000311           IF IN-IDDISTR = 7512 OR 2878 OR 778 OR 878                     
000312              OR 974 OR 978 OR 1090 OR 7625                               
000313             MOVE WC-SDC-NL-ET TO IN-IDDC                                 
000314           ELSE                                                           
000315             IF IN-IDDC = WC-SDC-NL                                       
000316               MOVE WC-SDC-NL-ET TO IN-IDDC                               
000317             END-IF                                                       
000318           END-IF                                                         
000319           MOVE IN-IDDC TO WS-IDDC                                        
000320           IF SDC-NL-ET OR NDC-NA                                         
000321             MOVE IN-IDARTNR-OBJ TO W-IDARTNR-ART                         
000322             PERFORM IMS-GU-ARTC                                          
000323             IF SEGMENT-FINNS                                             
000324               IF ((ARTC-ART-KDPRODSL = 14 OR 24 OR 34 OR 54) AND         
000325                  (ARTC-ART-IDFKNGRP = 3119 OR 3113 OR                    
000326                                       3114 OR 3116))                     
000327                 IF SDC-NL-ET                                             
000328                   MOVE WC-CDC-SE     TO IN-IDDC                          
000329                 ELSE                                                     
000330                   MOVE WC-NDC-US-BAT TO IN-IDDC                          
000331                 END-IF                                                   
000332               END-IF                                                     
000333               PERFORM IMS-GHU-BYTF-RAPP                                  
000334               IF IN-IDDC NOT = BYT01-RAPP-IDDC                           
000335                 MOVE IN-IDDC TO BYT01-RAPP-IDDC                          
000336                 PERFORM IMS-REPL-BYTF-RAPP                               
000337               END-IF                                                     
000338             END-IF                                                       
000339           END-IF                                                         
000340         END-IF                                                           
000341         PERFORM D-LAEGG-UPP-RAD                                          
000342       ELSE                                                               
000343         IF FOERSTA-POST = NEJ                                            
000344           PERFORM E-LAEGG-UPP-RAPP-ANTAL-RETUR                           
000345         ELSE                                                             
000346           MOVE NEJ TO FOERSTA-POST                                       
000347         END-IF                                                           
000348         IF CHKP-ANT > CHKP-MAX                                           
000349           PERFORM S02-TAG-CHECKPOINT                                     
000350         END-IF                                                           
000351         MOVE ZERO TO W-WDM611-RAD-RAK                                    
000352         PERFORM C-LAEGG-UPP-RAPP                                         
000353         PERFORM D-LAEGG-UPP-RAD                                          
000354       END-IF                                                             
000355       PERFORM S01-LAES-W46011                                            
000356     END-PERFORM                                                          
000357                                                                          
000358     IF POSTER-BEHANDLADE = JA                                            
000359       PERFORM E-LAEGG-UPP-RAPP-ANTAL-RETUR                               
000360       PERFORM F-NOLL-STAELL-RESTART-SEG                                  
000361     END-IF                                                               
000362     PERFORM Z-FINIT                                                      
000363     MOVE ZERO TO RETURN-CODE                                             
000364     GOBACK                                                               
000365     .                                                                    
000366     EJECT                                                                
000367 A-INIT SECTION.                                                          
000368     MOVE 'A-INIT'              TO WS-SEKTION                             
000369     SKIP2                                                                
000370                                                                          
000371     PERFORM IMS-RESTART                                                  
000372                                                                          
000373     OPEN INPUT W46011                                                    
000374                                                                          
000375     OPEN OUTPUT W371DUB                                                  
000376                 W371FEL                                                  
000377                 W371FE2                                                  
000378                                                                          
000379     ACCEPT DAGENS-DATUM       FROM DATE                                  
000380                                                                          
000381     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
000382     .                                                                    
000383     EJECT                                                                
000384 B-KOLLA-CHECKPOINT SECTION.                                              
000385     MOVE 'B-KOLLA-CHECKPOINT'  TO WS-SEKTION                             
000386     PERFORM IMS-GET-XXCQ-ROT                                             
000387     PERFORM IMS-GET-XXCQ-SEG                                             
000388*----DET FÖRSTA SEGMENTET ----------------------------------------        
000389     IF SEGMENT-FINNS                                                     
000390       MOVE XXCQ-3142-KVPOST     TO W-KVPOST-3142                         
000391*-----------------------------------------------------------------        
000392       IF W-KVPOST-3142       = ZERO                                      
000393          CONTINUE                                                        
000394       ELSE                                                               
000395*---------PGM HAR ABENDAT OCH SKA NU OMSTARTAS                            
000396          PERFORM BA-FELHANTERING-UTFIL                                   
000397          DISPLAY 'OMSTART AV PGM > ANTALRAPP ' W-KVPOST-3142             
000398       END-IF                                                             
000399     END-IF                                                               
000400                                                                          
000401     .                                                                    
000402     EJECT                                                                
000403 BA-FELHANTERING-UTFIL  SECTION.                                          
000404     MOVE 'BA-FELHANTERING-UTFIL'  TO WS-SEKTION                          
000405     SKIP3                                                                
000406     MOVE +1 TO INDX                                                      
000407                                                                          
000408     PERFORM UNTIL INDX = W-KVPOST-3142                                   
000409       IF SPAR-IDDISTR NOT = IN-IDDISTR OR                                
000410         SPAR-IDBYTRAP NOT = IN-IDBYTRAP                                  
000411         MOVE IN-IDDISTR TO SPAR-IDDISTR                                  
000412         MOVE IN-IDBYTRAP TO SPAR-IDBYTRAP                                
000413       END-IF                                                             
000414       ADD +1 TO INDX                                                     
000415       PERFORM S01-LAES-W46011                                            
000416     END-PERFORM                                                          
000417*--- LÄSER DE ARTIKLAR SOM HÖR TILL SISTA RAPPORTEN.                      
000418     PERFORM UNTIL NY-RAPPORT = JA                                        
000419       IF SPAR-IDDISTR NOT = IN-IDDISTR OR                                
000420          SPAR-IDBYTRAP NOT = IN-IDBYTRAP                                 
000421          MOVE JA TO  NY-RAPPORT                                          
000422       ELSE                                                               
000423          PERFORM S01-LAES-W46011                                         
000424       END-IF                                                             
000425     END-PERFORM                                                          
000426     MOVE ZERO       TO SPAR-IDDISTR                                      
000427                        SPAR-IDBYTRAP                                     
000428     .                                                                    
000429     EJECT                                                                
000430 C-LAEGG-UPP-RAPP SECTION.                                                
000431     MOVE 'C-LAEGG-UPP-RAPP'   TO WS-SEKTION                              
000432     SKIP2                                                                
000433     MOVE IN-IDDISTR          TO W-IDDISTR                                
000434                                 SPAR-IDDISTR                             
000435                                 BYT01-RAPP-IDDISTR                       
000436     MOVE IN-IDBYTRAP         TO W-IDBYTRAP                               
000437                                 SPAR-IDBYTRAP                            
000438                                 BYT01-RAPP-IDBYTRAP                      
000439     MOVE SPACE               TO BYT01-RAPP-ADBYTANK                      
000440     MOVE IN-IDFAKT           TO BYT01-RAPP-IDFAKT                        
000441     MOVE IN-IDKUNDNR         TO BYT01-RAPP-IDKUNDNR                      
000442**FIX UNTIL US AND OTHER COUNTRIES HAVE UPDATED THEIR VIPS SYSTEMS        
000443** WITH CORRECT IDDC***************20030218**********                     
000444     IF IN-IDDISTR = 7512 OR 2878 OR 778 OR 878 OR 974 OR 978             
000445          OR 1090 OR 7625                                                 
000446        MOVE  WC-SDC-NL-ET TO IN-IDDC                                     
000447     ELSE                                                                 
000448        IF IN-IDDC =  WC-SDC-NL                                           
000449           MOVE  WC-SDC-NL-ET TO IN-IDDC                                  
000450        END-IF                                                            
000451     END-IF                                                               
000452     MOVE IN-IDDC TO WS-IDDC                                              
000453     IF SDC-NL-ET OR NDC-NA                                               
000454       MOVE IN-IDARTNR-OBJ         TO W-IDARTNR-ART                       
000455       PERFORM IMS-GU-ARTC                                                
000456       IF SEGMENT-FINNS                                                   
000457          IF ((ARTC-ART-KDPRODSL = 14 OR 24 OR 34 OR 54) AND              
000458             (ARTC-ART-IDFKNGRP = 3119 OR 3113 OR 3114 OR 3116))          
000459             IF SDC-NL-ET                                                 
000460               MOVE WC-CDC-SE     TO IN-IDDC                              
000461             ELSE                                                         
000462               MOVE WC-NDC-US-BAT TO IN-IDDC                              
000463             END-IF                                                       
000464          END-IF                                                          
000465       END-IF                                                             
000466     END-IF                                                               
000467*********************END OF FIX**************************                 
000468     MOVE IN-IDDC             TO BYT01-RAPP-IDDC                          
000469     MOVE SPACE               TO BYT01-RAPP-IDUSER                        
000470     MOVE '2'                 TO BYT01-RAPP-KDBYTSTA-RAPP                 
000471     MOVE SPACE               TO BYT01-RAPP-KDBYTSTA-AVL                  
000472     MOVE ZERO                TO BYT01-RAPP-KVRETUR-TOT                   
000473     MOVE ZERO                TO BYT01-RAPP-DAANKDAG                      
000474     MOVE IN-TIREGDAT         TO BYT01-RAPP-DAREGDAT                      
000475     IF IN-TIREGDAT NOT = ZERO                                            
000476       IF IN-TIREGDAT < 500000                                            
000477         MOVE 20              TO BYT01-RAPP-DAREGDAT (1:2)                
000478       ELSE                                                               
000479         IF IN-TIREGDAT < 999999                                          
000480           MOVE 19            TO BYT01-RAPP-DAREGDAT (1:2)                
000481         ELSE                                                             
000482*          MOVE 99999999      TO BYT01-RAPP-DAREGDAT                      
000483           MOVE FUNCTION CURRENT-DATE (1:8) TO                            
000484                                 BYT01-RAPP-DAREGDAT                      
000485                                                                          
000486         END-IF                                                           
000487       END-IF                                                             
000488     END-IF                                                               
000489     MOVE ZERO                TO BYT01-RAPP-DAREGDAT-GODK                 
000490     MOVE SPACE               TO BYT01-RAPP-KDBYTBEK                      
000491     IF IN-FLBYTGAR = 'Y'                                                 
000492        MOVE 'J'              TO BYT01-RAPP-FLBYTGAR                      
000493                                 WS-FLBYTGAR                              
000494     ELSE                                                                 
000495        MOVE 'N'              TO BYT01-RAPP-FLBYTGAR                      
000496                                 WS-FLBYTGAR                              
000497     END-IF                                                               
000498     MOVE 'N'                 TO BYT01-RAPP-FLBYGODK                      
000499     MOVE NEJ                 TO OLD-NEW-RAPP-SW                          
000500     MOVE ZERO                TO WS-IDBYTRAD                              
000501                                                                          
000502                                                                          
000503                                                                          
000504                                                                          
000505     IF IN-IDDISTR > 0 AND IN-IDBYTRAP > 0                                
000506                                                                          
000507       PERFORM IMS-ISRT-BYTF-RAPP                                         
000508       IF SEGMENT-FINNS                                                   
000509         MOVE NEJ TO OBJ-RAD-SLUT-SW                                      
000510         ADD +1 TO  CHKP-ANT                                              
000511         MOVE JA TO SKRIV-RAD                                             
000512       ELSE                                                               
000513******************************************************************        
000514**** DUBLETTER RADEN FINNS REDAN *********************************        
000515******************************************************************        
000516         MOVE NEJ TO SKRIV-RAD                                            
000517       END-IF                                                             
000518     ELSE                                                                 
000519******************************************************************        
000520**** DISTRIKT OCH RAPPORTNR ÄR LIKA MED NOLL *********************        
000521******************************************************************        
000522       MOVE NEJ TO SKRIV-RAD                                              
000523     END-IF                                                               
000524     .                                                                    
000525     EJECT                                                                
000526 D-LAEGG-UPP-RAD SECTION.                                                 
000527     MOVE 'D-LAEGG-UPP-RAD'   TO WS-SEKTION                               
000528     SKIP2                                                                
000529                                                                          
000530     IF SKRIV-RAD = JA                                                    
000531       MOVE IN-IDARTNR-OBJ         TO BYT11-OBJ-IDARTNR-OBJ               
000532                                      W-IDARTNR-ART                       
000533       IF   IN-IDARTNR-OBJ  > ZERO                                        
000534         MOVE ZERO       TO BYT11-OBJ-IDTABNR                             
000535       ELSE                                                               
000536         MOVE IN-IDTABNR TO BYT11-OBJ-IDTABNR                             
000537       END-IF                                                             
000538       MOVE IN-BERADREF            TO BYT11-OBJ-BERADREF                  
000539       IF WS-FLBYTGAR = 'J'                                               
000540          MOVE '220'                 TO BYT11-OBJ-KDBYTREF                
000541       ELSE                                                               
000542          MOVE SPACE               TO BYT11-OBJ-KDBYTREF                  
000543       END-IF                                                             
000544       IF IN-IDORDNR7  NUMERIC                                            
000545         MOVE IN-IDORDNR7            TO BYT11-OBJ-IDORDER                 
000546       ELSE                                                               
000547         MOVE ZERO                   TO BYT11-OBJ-IDORDER                 
000548       END-IF                                                             
000549                                                                          
000550*VAD HÄNDER O M DET KOMMER EN RAPPORT MED NOLL I FÖRSTA RADEN             
000551*MEN MED RADNR SEDAN??                                                    
000552       IF OLD-RAPPORT                                                     
000553         ADD +1 TO WS-IDBYTRAD                                            
000554         MOVE WS-IDBYTRAD         TO BYT11-OBJ-IDBYTRAD                   
000555                                                                          
000556       ELSE                                                               
000557         IF IN-IDBYTRAD  NUMERIC                                          
000558           IF IN-IDBYTRAD > ZERO                                          
000559             MOVE IN-IDBYTRAD            TO WS-IDBYTRAD                   
000560             IF WS-IDBYTRAD-00   = 00                                     
000561               MOVE IN-IDBYTRAD         TO BYT11-OBJ-IDBYTRAD             
000562             ELSE                                                         
000563               DISPLAY MEDDELANDE-4                                       
000564               DISPLAY 'IN-IDBYTRAD ' IN-IDBYTRAD                         
000565               DISPLAY 'IN-IDBYTRAP ' IN-IDBYTRAP                         
000566               DISPLAY 'IN-IDDISTR  ' IN-IDDISTR                          
000567               CALL ABEND USING RKOD-ABEND-UTAN-DUMP                      
000568             END-IF                                                       
000569           ELSE                                                           
000570*            FLYTTA IN NUMMER 1-99 OCH SÄTT FLAGGA                        
000571             MOVE JA TO    OLD-NEW-RAPP-SW                                
000572             ADD +1 TO WS-IDBYTRAD                                        
000573             MOVE WS-IDBYTRAD         TO BYT11-OBJ-IDBYTRAD               
000574           END-IF                                                         
000575         ELSE                                                             
000576           DISPLAY MEDDELANDE-3                                           
000577           DISPLAY 'IN-IDBYTRAD ' IN-IDBYTRAD                             
000578           DISPLAY 'IN-IDBYTRAP ' IN-IDBYTRAP                             
000579           DISPLAY 'IN-IDDISTR  ' IN-IDDISTR                              
000580           CALL ABEND USING RKOD-ABEND-UTAN-DUMP                          
000581         END-IF                                                           
000582       END-IF                                                             
000583                                                                          
000584       MOVE SPACE                  TO BYT11-OBJ-KDBYTSTA-OBJ              
000585       MOVE SPACE                  TO BYT11-OBJ-KDBYTSTA-AVL              
000586       MOVE ZERO                   TO BYT11-OBJ-KVRETUR-GODK              
000587       MOVE SPACE                  TO BYT11-OBJ-FLSKROT                   
000588*- - - - ÄNDRAT FRÅN 600 TILL MINDRE ÄN 1000                              
000589*- - - - PGA ATT DET ÄR SVÅRT ATT ÄNDRA I AS-400                          
000590*- - - - GJORT I SAMBAND MED NDC-PROJEKTET/RONNY S.                       
000591       IF   IN-KVRETUR-URSP  > +999                                       
000592         MOVE +1 TO IN-KVRETUR-URSP                                       
000593       END-IF                                                             
000594       MOVE IN-KVRETUR-URSP        TO BYT11-OBJ-KVRETUR-URSP              
000595*- - - - KONTROLLERA ATT ARTIKELNR FINNS I WDK6                           
000596       IF IN-IDARTNR-OBJ  > ZERO                                          
000597          PERFORM IMS-GU-ARTC                                             
000598          IF SEGMENT-FINNS                                                
000599*****************************************************************         
000600**** FEL PÅ ARTIKELN SAKNAR SJÄLVKOST FEL INLEVERANSDATUM ********        
000601**** OM ERSÄTTNINGSKOD 52                                 ********        
000602******************************************************************        
000603             PERFORM IMS-GET-ARTC11                                       
000604             IF SEGMENT-FINNS                                             
000605                IF ARTC-CLAG-PRARTSJK > ZERO                              
000606                   ADD  IN-KVRETUR-URSP        TO W-KVRETUR-TOT           
000607                   COMPUTE W-IDBYTRAP-9KOMPL = W-9KOMPL -                 
000608                                                     W-IDBYTRAP           
000609                   MOVE W-IDBYTRAP-9KOMPL TO                              
000610                                     BYT11-OBJ-IDBYTRAP-9KOMPL            
000611                   PERFORM IMS-ISRT-BYTF-RAD                              
000612                   ADD 1 TO W-WDM611-RAD-RAK                              
000613                ELSE                                                      
000614                   MOVE IN-W37116 TO FE2-W37116                           
000615                   WRITE FE2-POST         FROM FE2-AREA                   
000616                   MOVE 'W371F2'          TO POSTSUM-FDNAMN               
000617                   MOVE 'W37116D4'        TO POSTSUM-DDNAMN2              
000618                   MOVE 'FE2 '            TO POSTSUM-TRANSTYP             
000619                   CALL POSTSUM USING POSTSUM-PARM                        
000620                END-IF                                                    
000621             ELSE                                                         
000622                MOVE IN-W37116 TO FE2-W37116                              
000623                WRITE FE2-POST         FROM FE2-AREA                      
000624                MOVE 'W371F2'          TO POSTSUM-FDNAMN                  
000625                MOVE 'W37116D4'        TO POSTSUM-DDNAMN2                 
000626                MOVE 'FE2 '            TO POSTSUM-TRANSTYP                
000627                CALL POSTSUM USING POSTSUM-PARM                           
000628             END-IF                                                       
000629          ELSE                                                            
000630******************************************************************        
000631**** ARTIKELN SAKNAS PÅ ARTIKELREGISTRET *************************        
000632******************************************************************        
000633             MOVE IN-W37116 TO FEL-W37116                                 
000634             WRITE FEL-POST         FROM FEL-AREA                         
000635             MOVE 'W371FE'          TO POSTSUM-FDNAMN                     
000636             MOVE 'W37116D3'        TO POSTSUM-DDNAMN2                    
000637             MOVE 'FEL '            TO POSTSUM-TRANSTYP                   
000638             CALL POSTSUM USING POSTSUM-PARM                              
000639          END-IF                                                          
000640       ELSE                                                               
000641          ADD  IN-KVRETUR-URSP        TO W-KVRETUR-TOT                    
000642                                                                          
000643          COMPUTE W-IDBYTRAP-9KOMPL    = W-9KOMPL - W-IDBYTRAP            
000644          MOVE W-IDBYTRAP-9KOMPL      TO BYT11-OBJ-IDBYTRAP-9KOMPL        
000645          PERFORM IMS-ISRT-BYTF-RAD                                       
000646          ADD 1 TO W-WDM611-RAD-RAK                                       
000647       END-IF                                                             
000648                                                                          
000649     ELSE                                                                 
000650       IF FOERSTA-DUB-POST                                                
000651          PERFORM DA-SKAPA-RUBRIKER                                       
000652          MOVE NEJ TO FOERSTA-DUB-SW                                      
000653       END-IF                                                             
000654       MOVE IN-IDDISTR     TO RAD-IDDISTR                                 
000655       MOVE IN-IDKUNDNR    TO RAD-IDKUND                                  
000656       MOVE IN-IDBYTRAP    TO RAD-IDBYTRAP                                
000657       MOVE IN-IDARTNR-OBJ TO RAD-IDARTNR                                 
000658       MOVE RAD-AREA TO DUB-AREA                                          
000659       PERFORM S03-SKRIV-W371DUB                                          
000660     END-IF                                                               
000661     .                                                                    
000662     EJECT                                                                
000663                                                                          
000664 DA-SKAPA-RUBRIKER SECTION.                                               
000665     MOVE RUBRIK-1 TO DUB-AREA                                            
000666     PERFORM S03-SKRIV-W371DUB                                            
000667     MOVE RUBRIK-2 TO DUB-AREA                                            
000668     PERFORM S03-SKRIV-W371DUB                                            
000669     MOVE RUBRIK-3 TO DUB-AREA                                            
000670     PERFORM S03-SKRIV-W371DUB                                            
000671     MOVE RUBRIK-4 TO DUB-AREA                                            
000672     PERFORM S03-SKRIV-W371DUB                                            
000673     MOVE RUBRIK-5 TO DUB-AREA                                            
000674     PERFORM S03-SKRIV-W371DUB                                            
000675     MOVE SPACE    TO DUB-AREA                                            
000676     PERFORM S03-SKRIV-W371DUB                                            
000677     MOVE RUBRIK-6 TO DUB-AREA                                            
000678     PERFORM S03-SKRIV-W371DUB                                            
000679     .                                                                    
000680     EJECT                                                                
000681                                                                          
000682 E-LAEGG-UPP-RAPP-ANTAL-RETUR SECTION.                                    
000683     MOVE 'E-LAEGG-UPP-RAPP-ANTAL-RETUR' TO WS-SEKTION                    
000684     SKIP2                                                                
000685     IF SKRIV-RAD  = JA                                                   
000686       PERFORM IMS-GHU-BYTF-RAPP                                          
000687       MOVE W-KVRETUR-TOT      TO BYT01-RAPP-KVRETUR-TOT                  
000688       PERFORM IMS-REPL-BYTF-RAPP                                         
000689       MOVE ZERO TO W-KVRETUR-TOT                                         
000690       ADD +1 TO  CHKP-ANT                                                
000691     END-IF                                                               
000692                                                                          
000693     .                                                                    
000694     EJECT                                                                
000695 F-NOLL-STAELL-RESTART-SEG SECTION.                                       
000696     MOVE 'F-NOLL-STAELL-RESTART-SEG' TO WS-SEKTION                       
000697     SKIP2                                                                
000698     PERFORM IMS-GET-XXCQ-ROT                                             
000699     PERFORM IMS-GET-XXCQ-SEG                                             
000700     MOVE ZERO TO XXCQ-3142-KVPOST                                        
000701     PERFORM IMS-REPL-XXCQ-SEG                                            
000702     .                                                                    
000703     EJECT                                                                
000704 Z-FINIT          SECTION.                                                
000705     MOVE 'Z-FINIT' TO WS-SEKTION                                         
000706                                                                          
000707                                                                          
000708     CLOSE W46011                                                         
000709           W371DUB                                                        
000710           W371FEL                                                        
000711           W371FE2                                                        
000712     SKIP2                                                                
000713     MOVE 'S' TO POSTSUM-OPKOD                                            
000714     CALL POSTSUM USING POSTSUM-PARM                                      
000715     .                                                                    
000716     EJECT                                                                
000717 S01-LAES-W46011  SECTION.                                                
000718     MOVE 'S01-LAES-W46011' TO WS-SEKTION                                 
000719     SKIP2                                                                
000720     READ W46011 INTO IN-AREA                                             
000721     AT END                                                               
000722        SET END-OF-W46011 TO TRUE                                         
000723                                                                          
000724     NOT AT END                                                           
000725        MOVE 'W46011' TO POSTSUM-FDNAMN                                   
000726        MOVE 'W37116D1' TO POSTSUM-DDNAMN2                                
000727        MOVE IN-IDPTYP TO POSTSUM-TRANSTYP                                
000728        CALL POSTSUM USING POSTSUM-PARM                                   
000729        ADD +1        TO WS-ANTAL-POSTER                                  
000730     END-READ                                                             
000731     .                                                                    
000732     EJECT                                                                
000733 S02-TAG-CHECKPOINT   SECTION.                                            
000734     MOVE 'S02-TAG-CHECKPOINT' TO WS-SEKTION                              
000735     SKIP2                                                                
000736     ADD CHKP-ANT TO CHKP-TOT                                             
000737     PERFORM IMS-GET-XXCQ-ROT                                             
000738     PERFORM IMS-GET-XXCQ-SEG                                             
000739     MOVE CHKP-TOT TO XXCQ-3142-KVPOST                                    
000740     PERFORM IMS-REPL-XXCQ-SEG                                            
000741                                                                          
000742* --- VID CHECKPOINT TAGNING SÅ TAPPAR MAN GN-POSITION I BASEN            
000743* --- SPARA DATABASNYCKLAR OM DET BEHÖVS                                  
000744     PERFORM IMS-CHECKPOINT                                               
000745* --- LÄS OM DATABAS OM DET BEHÖVS                                        
000746     MOVE ZERO TO  CHKP-ANT                                               
000747     .                                                                    
000748     EJECT                                                                
000749 S03-SKRIV-W371DUB SECTION.                                               
000750     WRITE DUB-POST         FROM DUB-AREA                                 
000751     MOVE 'W371DU'          TO POSTSUM-FDNAMN                             
000752     MOVE 'W37116D2'        TO POSTSUM-DDNAMN2                            
000753     MOVE 'DUBB'            TO POSTSUM-TRANSTYP                           
000754     CALL POSTSUM USING POSTSUM-PARM                                      
000755     .                                                                    
000756     EJECT                                                                
000757* --- IMS SEKTIONER ---                                                   
000758     SKIP3                                                                
000759 IMS-GET-XXCQ-ROT SECTION.                                                
000760     MOVE 'IMS-GET-XXCQ-ROT' TO WS-IMS-SEKTION                            
000761     STRING 'WLXXCQ01(WDGXKEY  =' W-WDGXKEY-X ')'                         
000762          DELIMITED BY SIZE INTO SSA1                                     
000763     MOVE '  GE' TO GODK-STATUSKODER                                      
000764     CALL CBLTDLI USING GU XXCQ-PCB DLI-IO-AREA SSA1                      
000765     MOVE XXCQ-STATUS-CODE TO STATUS-WS                                   
000766     PERFORM IMS-STATUSKONTROLL                                           
000767     .                                                                    
000768 IMS-GET-XXCQ-SEG SECTION.                                                
000769     MOVE 'IMS-GET-XXCQ-SEG' TO WS-IMS-SEKTION                            
000770     STRING 'WLXXCQ01(WDGXKEY  =' W-WDGXKEY-X ')'                         
000771          DELIMITED BY SIZE INTO SSA1                                     
000772     MOVE  'WLXXCQ11 ' TO SSA2                                            
000773     MOVE '  GE' TO GODK-STATUSKODER                                      
000774     CALL CBLTDLI USING GHNP XXCQ-PCB DLI-IO-AREA SSA1 SSA2               
000775     MOVE XXCQ-STATUS-CODE TO STATUS-WS                                   
000776     PERFORM IMS-STATUSKONTROLL                                           
000777     .                                                                    
000778     SKIP3                                                                
000779 IMS-REPL-XXCQ-SEG SECTION.                                               
000780     MOVE 'IMS-REPL-XXCQ-SEG' TO WS-IMS-SEKTION                           
000781                                                                          
000782     MOVE '  ' TO GODK-STATUSKODER                                        
000783     CALL CBLTDLI USING REPL XXCQ-PCB DLI-IO-AREA                         
000784     MOVE XXCQ-STATUS-CODE TO STATUS-WS                                   
000785     PERFORM IMS-STATUSKONTROLL                                           
000786     .                                                                    
000787 IMS-GHU-BYTF-RAPP SECTION.                                               
000788     MOVE 'IMS-GHU-BYTF-RAPP' TO WS-IMS-SEKTION                           
000789     STRING 'WLBYTF01(WDM601KY =' W-WDM601KY-X ')'                        
000790          DELIMITED BY SIZE INTO SSA1                                     
000791     MOVE '  ' TO GODK-STATUSKODER                                        
000792     CALL CBLTDLI USING GHU BYTF-PCB DLI-IO-AREA SSA1                     
000793     MOVE BYTF-STATUS-CODE TO STATUS-WS                                   
000794     PERFORM IMS-STATUSKONTROLL                                           
000795     .                                                                    
000796     EJECT                                                                
000797 IMS-ISRT-BYTF-RAPP SECTION.                                              
000798     MOVE 'IMS-ISRT-BYTF-RAPP' TO WS-IMS-SEKTION                          
000799                                                                          
000800     MOVE 'WLBYTF01 ' TO SSA1                                             
000801*    MOVE '  ' TO GODK-STATUSKODER                                        
000802     MOVE '  II' TO GODK-STATUSKODER                                      
000803     CALL CBLTDLI USING ISRT BYTF-PCB DLI-IO-AREA SSA1                    
000804     MOVE BYTF-STATUS-CODE TO STATUS-WS                                   
000805     PERFORM IMS-STATUSKONTROLL                                           
000806     .                                                                    
000807 IMS-REPL-BYTF-RAPP SECTION.                                              
000808     MOVE 'IMS-REPL-BYTF-RAPP' TO WS-IMS-SEKTION                          
000809                                                                          
000810     MOVE '  ' TO GODK-STATUSKODER                                        
000811     CALL CBLTDLI USING REPL BYTF-PCB DLI-IO-AREA                         
000812     MOVE BYTF-STATUS-CODE TO STATUS-WS                                   
000813     PERFORM IMS-STATUSKONTROLL                                           
000814     .                                                                    
000815     EJECT                                                                
000816 IMS-ISRT-BYTF-RAD SECTION.                                               
000817     MOVE 'IMS-ISRT-BYTF-RAD' TO WS-IMS-SEKTION                           
000818                                                                          
000819     STRING 'WLBYTF01(WDM601KY =' W-WDM601KY-X ')'                        
000820          DELIMITED BY SIZE INTO SSA1                                     
000821     MOVE 'WLBYTF11 ' TO SSA2                                             
000822     MOVE '  II' TO GODK-STATUSKODER                                      
000823*    MOVE '  ' TO GODK-STATUSKODER                                        
000824     CALL CBLTDLI USING ISRT BYTF-PCB DLI-IO-AREA SSA1 SSA2               
000825     MOVE BYTF-STATUS-CODE TO STATUS-WS                                   
000826     PERFORM IMS-STATUSKONTROLL                                           
000827     .                                                                    
000828 IMS-GU-ARTC SECTION.                                                     
000829     MOVE 'IMS-GU-ARTC' TO WS-IMS-SEKTION                                 
000830     SKIP2                                                                
000831     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-ART-X ')'                     
000832           DELIMITED BY SIZE INTO SSA1                                    
000833     MOVE '  GE' TO GODK-STATUSKODER                                      
000834     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-WLARTC01 SSA1                  
000835     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
000836     PERFORM IMS-STATUSKONTROLL                                           
000837     .                                                                    
000838     EJECT                                                                
000839 IMS-GET-ARTC11 SECTION.                                                  
000840     SKIP3                                                                
000841     STRING 'WLARTC11(KDSEGKEY =1)'                                       
000842                     DELIMITED BY SIZE INTO SSA1                          
000843     MOVE '  GE' TO GODK-STATUSKODER                                      
000844     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-WLARTC11 SSA1                 
000845     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
000846     PERFORM IMS-STATUSKONTROLL                                           
000847     .                                                                    
000848     EJECT                                                                
000849 IMS-RESTART SECTION.                                                     
000850     MOVE 'IMS-RESTART' TO WS-IMS-SEKTION                                 
000851     SKIP2                                                                
000852     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
000853     MOVE '  ' TO GODK-STATUSKODER                                        
000854     CALL CBLTDLI USING XRST MSG-PCB                                      
000855                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
000856                        CHKP-AREA-LENGTH CHKP-AREA                        
000857     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
000858     PERFORM IMS-STATUSKONTROLL                                           
000859     .                                                                    
000860 IMS-CHECKPOINT SECTION.                                                  
000861     MOVE 'IMS-CHECKPOINT' TO WS-IMS-SEKTION                              
000862     SKIP2                                                                
000863     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
000864     MOVE '  XD' TO GODK-STATUSKODER                                      
000865     CALL CBLTDLI USING CHKP MSG-PCB                                      
000866                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
000867                        CHKP-AREA-LENGTH CHKP-AREA                        
000868     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
000869     PERFORM IMS-STATUSKONTROLL                                           
000870                                                                          
000871     IF IMS-EJ-OK                                                         
000872       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
000873       DISPLAY FELTEXT                                                    
000874       CALL FELLOG                                                        
000875     END-IF                                                               
000876     .                                                                    
000877     EJECT                                                                
000878 IMS-STATUSKONTROLL SECTION.                                              
000879     SKIP2                                                                
000880     SET STATUS-IX TO 1                                                   
000881     SEARCH GODK-STATUS                                                   
000882       AT END                                                             
000883         MOVE 'XXXXXXXXXX' TO FELTEXT-STR                                 
000884         DISPLAY FELTEXT                                                  
000885         CALL FELLOG                                                      
000886       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
000887         CONTINUE                                                         
000888     END-SEARCH                                                           
000889     .                                                                    
