000001 ID DIVISION.                                                             
000002                                                                          
000003 PROGRAM-ID.     W6115100.                                                
000004 AUTHOR.         LARS THELL.                                              
000005 DATE-WRITTEN.   92/07/02.                                                
000006 DATE-COMPILED.                                                           
000007                                                                          
000008*    FUNCTION:                                                            
000009*        LÄSER FIL MED STATUSPOSTER FRÅN W6G3 OCH SUMMERAR VAD SOM        
000010*        PRODUCERATS IDAG OCH GENOMLOPPSTIDEN.                            
000011*                                                                         
000012*                                                                         
000013*    NOTE:                                                                
000014*        CONSTANTMEDLEM W61155ML SKALL VARA SORTERAD PÅ IDDC FÖR          
000015*        RÄTT PROGRAMFUNKTION.                                            
000016*                                                                         
000017*    ABEND CODES:                                                         
000018*        U0016 -  . . . .                                                 
000019*        U1000 -  TABELL FULL ELLER FEL FRÅN WORKDAY                      
000020*                                                                         
000021                                                                          
000022     SKIP3                                                                
000023 ENVIRONMENT DIVISION.                                                    
000024     SKIP2                                                                
000025 INPUT-OUTPUT SECTION.                                                    
000026                                                                          
000027 FILE-CONTROL.                                                            
000028     SKIP2                                                                
000029*          --- STATUSPOSTER FRÅN W6G3                                     
000030     SELECT W61140                     ASSIGN TO W61151D1.                
000031     SKIP2                                                                
000032*                                                                         
000033     SELECT W61155ML                   ASSIGN TO W61151D2.                
000034*          --- SUMMAFIL MED PRODUCERAT IDAG OCH GENOMLOPPSTID             
000035     SELECT W61151                     ASSIGN TO W61151D3.                
000036     EJECT                                                                
000037 DATA DIVISION.                                                           
000038     SKIP3                                                                
000039 FILE SECTION.                                                            
000040     SKIP3                                                                
000041 FD  W61140                                                               
000042     RECORDING       F                                                    
000043     BLOCK CONTAINS  0.                                                   
000044     SKIP2                                                                
000045*01  -COPY W6114001      -L.                                              
000046     SKIP3                                                                
000047 FD  W61155ML                                                             
000048     RECORDING       F                                                    
000049     BLOCK CONTAINS  0.                                                   
000050     SKIP2                                                                
000051 01  FILLER             PIC X(80).                                        
000052     SKIP3                                                                
000053 FD  W61151                                                               
000054     RECORDING       F                                                    
000055     BLOCK CONTAINS  0.                                                   
000056     SKIP2                                                                
000057*01  POST -COPY W6115101 -PRE  UT-  -L.                                   
000058     EJECT                                                                
000059 WORKING-STORAGE SECTION.                                                 
000060     SKIP2                                                                
000061                                                                          
000062*    -- CHECKED BY WY2000                                                 
000063 77  IDPGM                       PIC X(8)    VALUE 'W6115100'.            
000064 77  JA                          PIC X       VALUE 'J'.                   
000065 77  NEJ                         PIC X       VALUE 'N'.                   
000066                                                                          
000067 77  MAX-SUM-IX                  PIC S9(9)   VALUE +200 COMP SYNC.        
000068                                                                          
000069 77  MAX-MAAL-IX                 PIC S9(9)   VALUE +200 COMP SYNC.        
000070                                                                          
000071 77  DCIX                        PIC S9(9)   COMP-3 VALUE ZERO.           
000072                                                                          
000073*01  IDDC-WS.                                                             
000074*    03 FILLER                  PIC X       VALUE '1'.                    
000075*    03 WS-DCIX                 PIC 9       VALUE ZERO.                   
000076                                                                          
000077 01  DC-SPARFAELT.                                                        
000078    03 FILLER OCCURS 6.                                                   
000079     05 SPAR-KDINLUPF           PIC X(4)       VALUE SPACE.               
000080     05 W-ADINLOMR              PIC X(4)       VALUE SPACE.               
000081     05 W-KDINLUPF              PIC X(4)       VALUE SPACE.               
000082     05 W-OLD-IDLOPNRM          PIC S9(9)      VALUE ZERO COMP-3.         
000083     05 W-TIGLT-MM              PIC S9(7)      VALUE ZERO COMP-3.         
000084     05 W-TOT-IDLOPNRM          PIC S9(9)      VALUE ZERO COMP-3.         
000085     05 W-TOT-KVART             PIC S9(5)      VALUE ZERO COMP-3.         
000086     05 W-TOT-KVRADER           PIC S9(5)      VALUE ZERO COMP-3.         
000087     05 W-TOT-KVRADER-MAAL      PIC S9(5)      VALUE ZERO COMP-3.         
000088     05 W-TOT-KVRADER-MAAL-PRIO PIC S9(9)      VALUE ZERO COMP-3.         
000089     05 W-TOT-KVRADER-PRIO      PIC S9(5)      VALUE ZERO COMP-3.         
000090     05 W-TOT-SUBEL             PIC S9(9)V9(2) VALUE ZERO COMP-3.         
000091     05 W-TOT-SUBEL-PRIO        PIC S9(9)V9(2) VALUE ZERO COMP-3.         
000092     05 W-TOT-TIGLT-MM          PIC S9(9)      VALUE ZERO COMP-3.         
000093     05 W-TOT-TIGLT-PRIO-MM     PIC S9(9)      VALUE ZERO COMP-3.         
000094                                                                          
000095 77  WS-TOT-SUBEL-PRIO       PIC S9(9)V9(2)  VALUE ZERO COMP-3.           
000096 77  WS-SUBEL-PRIO           PIC S9(9)V9(2)  VALUE ZERO COMP-3.           
000097                                                                          
000098 77  W-TIKLOCK-OLD-MM        PIC 9(8)        VALUE ZERO.                  
000099 77  W-TIKLOCK-NEW-MM        PIC 9(8)        VALUE ZERO.                  
000100                                                                          
000101 01  W-NEW-TIKLOCK           PIC 9(8)        VALUE ZERO.                  
000102 01  FILLER REDEFINES W-NEW-TIKLOCK.                                      
000103   05 W-NEW-HH               PIC 9(2).                                    
000104   05 W-NEW-MM               PIC 9(2).                                    
000105   05 FILLER                 PIC 9(4).                                    
000106                                                                          
000107 01  W-OLD-TIKLOCK           PIC 9(8)        VALUE ZERO.                  
000108 01  FILLER REDEFINES W-OLD-TIKLOCK.                                      
000109   05 W-OLD-HH               PIC 9(2).                                    
000110   05 W-OLD-MM               PIC 9(2).                                    
000111   05 FILLER                 PIC 9(4).                                    
000112                                                                          
000113 01  W-GLT                   PIC 9(5)        VALUE ZERO.                  
000114 01  FILLER REDEFINES W-GLT.                                              
000115   05 W-GLT-HH               PIC 9(3).                                    
000116   05 W-GLT-MM               PIC 9(2).                                    
000117                                                                          
000118 77  W-TIKLOCK-MM            PIC S9(5)      COMP-3  VALUE ZERO.           
000119 77  W-TOT-MM                PIC S9(5)      COMP-3  VALUE ZERO.           
000120                                                                          
000121 77  W61140-EOF-SW               PIC X       VALUE 'N'.                   
000122     88  END-OF-W61140                       VALUE 'J'.                   
000123                                                                          
000124 77  W61155ML-EOF-SW             PIC X       VALUE 'N'.                   
000125     88  END-OF-W61155ML                     VALUE 'J'.                   
000126                                                                          
000127 77  OLD-SW                      PIC XX      VALUE '  '.                  
000128     88  OLD-FM                              VALUE 'FM'.                  
000129     88  OLD-EM                              VALUE 'EM'.                  
000130                                                                          
000131 77  NEW-SW                      PIC XX      VALUE '  '.                  
000132     88  NEW-FM                              VALUE 'FM'.                  
000133     88  NEW-EM                              VALUE 'EM'.                  
000134                                                                          
000135 77  ADD-RADER-SW                PIC X       VALUE 'N'.                   
000136     88  ADD-RADER                           VALUE 'J'.                   
000137                                                                          
000138 77  SISTA-FB-SW                 PIC X       VALUE 'N'.                   
000139     88  SISTA-FB                            VALUE 'J'.                   
000140     EJECT                                                                
000141 01  DAGENS-DATUM                PIC 9(6).                                
000142 01  FILLER REDEFINES DAGENS-DATUM.                                       
000143     03  DAGENS-YEAR             PIC 9(2).                                
000144     03  DAGENS-MONTH            PIC 9(2).                                
000145     03  DAGENS-DAY              PIC 9(2).                                
000146     EJECT                                                                
000147*      --- VALID IDDC CODES                                               
000148*                                                                         
000149*01    -COPY WWDC99                                                       
000150       EJECT                                                              
000151*                                                                         
000152*01    -COPY WWDCKONS                                                     
000153       EJECT                                                              
000154 01  DYNAMISKA-SUBPROGRAM.                                                
000155*                                                                         
000156     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
000157     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
000158     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
000159     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY'.             
000160     SKIP2                                                                
000161*    --- PARAMETRAR TILL ABEND                                            
000162                                                                          
000163 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
000164 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
000165     SKIP2                                                                
000166 01  FELTEXT.                                                             
000167     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
000168     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
000169     EJECT                                                                
000170*    --- PARAMETRAR TILL DATKORT                                          
000171*                                                                         
000172 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W61151'.              
000173     SKIP2                                                                
000174 01  DATUMKORT-ID                PIC X(6)    VALUE '000001'.              
000175     EJECT                                                                
000176*01  -COPY WDATKORT                                                       
000177     EJECT                                                                
000178*    --- PARAMETRAR TILL POSTSUM                                          
000179*                                                                         
000180*01  -COPY W0005   -PRE  POSTSUM-                                         
000181     EJECT                                                                
000182                                                                          
000183*01  -COPY WORKAREA                                                       
000184     EJECT                                                                
000185                                                                          
000186*    --- AREA FÖR BERÄKNING AV GLT PER PARTI                              
000187 01  DC-W-SPAR1.                                                          
000188     03  FILLER  OCCURS 6.                                                
000189*      05 AREA -COPY W6114001    -PRE W-SPAR1-                            
000190     EJECT                                                                
000191                                                                          
000192*    --- AREA FÖR BERÄKNING AV GLT PER UPPFÖLJNINGSTATUS                  
000193 01  DC-W-SPAR2.                                                          
000194     03  FILLER  OCCURS 6.                                                
000195*      05 AREA -COPY W6114001    -PRE W-SPAR2-                            
000196     EJECT                                                                
000197                                                                          
000198 01  IN-AREA-START           PIC X(24)   VALUE 'IN-AREA-START  '.         
000199*01  AREA -COPY W6114001     -PRE IN-                                     
000200     EJECT                                                                
000201                                                                          
000202 01  MAAL-AREA-START         PIC X(24)   VALUE 'MAAL-AREA-START'.         
000203 01  MAAL-AREA.                                                           
000204     03 MAAL-AREA-DATA.                                                   
000205        05 MAAL-IDDC             PIC XX.                                  
000206        05 MAAL-KDINLUPF         PIC X(4).                                
000207        05 MAAL-TIGLT            PIC 9(5).                                
000208        05 MAAL-TIGLT-PRIO       PIC 9(5).                                
000209     03 FILLER                   PIC X(66).                               
000210     EJECT                                                                
000211                                                                          
000212 01  UT-AREA-START           PIC X(24)   VALUE 'UT-AREA-START  '.         
000213*01  AREA -COPY W6115101     -PRE UT-                                     
000214     EJECT                                                                
000215                                                                          
000216*    --- TABELL FÖR SUMMERINGAR PER KDINLUPF                              
000217 01  FILLER                 PIC X(24)   VALUE 'SUM-TAB-START'.            
000218 01  SUM-TABELL.                                                          
000219   03 DC-W-SUM       OCCURS 6   INDEXED BY SU-DCIX.                       
000220     05 W-SUM-TAB    OCCURS 200 INDEXED BY SUM-IX.                        
000221       07  W-SUM-KDINLUPF          PIC X(4).                              
000222       07  W-SUM-KVRADER           PIC S9(5)      COMP-3.                 
000223       07  W-SUM-KVRADER-PRIO      PIC S9(5)      COMP-3.                 
000224       07  W-SUM-KVART             PIC S9(5)      COMP-3.                 
000225       07  W-SUM-SUBEL             PIC S9(9)V9(2) COMP-3.                 
000226       07  W-SUM-SUBEL-PRIO        PIC S9(9)V9(2) COMP-3.                 
000227       07  W-SUM-TIGLT-MM          PIC S9(7)      COMP-3.                 
000228       07  W-SUM-TIGLT-PRIO-MM     PIC S9(7)      COMP-3.                 
000229       07  W-SUM-KVRADER-MAAL      PIC S9(5)      COMP-3.                 
000230       07  W-SUM-KVRADER-MAAL-PRIO PIC S9(5)      COMP-3.                 
000231       07  W-LAST-IDLOPNRM         PIC S9(9)      COMP-3.                 
000232     EJECT                                                                
000233 01  FILLER                 PIC X(24) VALUE   'MAAL-TABELL'.              
000234 01  MAAL-TABELL.                                                         
000235    03 DC-W-MAAL     OCCURS 2   INDEXED BY MA-DCIX.                       
000236      05  W-MAAL-TAB OCCURS 200 INDEXED BY MAAL-IX.                       
000237         06 W-MAAL-TOTAL.                                                 
000238             07 W-MAAL-IDDC              PIC XX.                          
000239             07 W-MAAL-KDINLUPF          PIC X(4).                        
000240             07 W-MAAL-TIGLT             PIC 9(5).                        
000241             07 W-MAAL-TIGLT-PRIO        PIC 9(5).                        
000242     EJECT                                                                
000243 PROCEDURE DIVISION.                                                      
000244                                                                          
000245     PERFORM A-INIT                                                       
000246     PERFORM S01-LAES-W61140                                              
000247     PERFORM UNTIL END-OF-W61140                                          
000248         MOVE IN-IDDC    TO WS-IDDC                                       
000249         IF CDC-SE                                                        
000250           PERFORM B-BEHANDLA-STATUS-POST                                 
000251         END-IF                                                           
000252         PERFORM S01-LAES-W61140                                          
000253     END-PERFORM                                                          
000254                                                                          
000255     PERFORM C-TOEM-KDINLUPF-TAB                                          
000256     PERFORM D-SKRIV-TOTAL-POST                                           
000257                                                                          
000258     PERFORM Z-FINIT                                                      
000259                                                                          
000260     MOVE ZERO TO RETURN-CODE                                             
000261     GOBACK                                                               
000262     .                                                                    
000263     EJECT                                                                
000264 A-INIT SECTION.                                                          
000265                                                                          
000266     OPEN INPUT  W61140                                                   
000267                 W61155ML                                                 
000268                                                                          
000269     OPEN OUTPUT W61151                                                   
000270     SKIP2                                                                
000271     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID                         
000272                        DATUMKORT                                         
000273     MOVE D-AAR                TO  DAGENS-YEAR                            
000274     MOVE D-MAANAD             TO  DAGENS-MONTH                           
000275     MOVE D-DAG                TO  DAGENS-DAY                             
000276                                                                          
000277     MOVE IDPGM                TO  POSTSUM-PROGNAMN                       
000278                                                                          
000279                                                                          
000280     SET SU-DCIX          TO +1                                           
000281     SET SUM-IX           TO +1                                           
000282     PERFORM UNTIL SU-DCIX   >  +6                                        
000283       PERFORM UNTIL SUM-IX > MAX-SUM-IX                                  
000284           MOVE SPACE  TO W-SUM-KDINLUPF        (SU-DCIX SUM-IX)          
000285           MOVE ZERO   TO W-SUM-KVRADER         (SU-DCIX SUM-IX)          
000286                          W-SUM-KVRADER-PRIO    (SU-DCIX SUM-IX)          
000287                          W-SUM-KVART           (SU-DCIX SUM-IX)          
000288                          W-SUM-SUBEL           (SU-DCIX SUM-IX)          
000289                          W-SUM-SUBEL-PRIO      (SU-DCIX SUM-IX)          
000290                          W-SUM-TIGLT-MM        (SU-DCIX SUM-IX)          
000291                          W-SUM-TIGLT-PRIO-MM   (SU-DCIX SUM-IX)          
000292                          W-SUM-KVRADER-MAAL    (SU-DCIX SUM-IX)          
000293                          W-SUM-KVRADER-MAAL-PRIO (SU-DCIX SUM-IX)        
000294                          W-LAST-IDLOPNRM       (SU-DCIX SUM-IX)          
000295           SET SUM-IX UP BY +1                                            
000296       END-PERFORM                                                        
000297                                                                          
000298       MOVE 'R31 '     TO W-SUM-KDINLUPF  (SU-DCIX   1 )                  
000299       IF SU-DCIX = 1                                                     
000300         MOVE 'C2  '     TO W-SUM-KDINLUPF  (1         2 )                
000301       END-IF                                                             
000302       SET SUM-IX     TO +1                                               
000303       SET SU-DCIX UP BY +1                                               
000304     END-PERFORM                                                          
000305                                                                          
000306     MOVE SPACE TO W-SPAR1-AREA (1)     W-SPAR1-AREA (2)                  
000307                   W-SPAR2-AREA (1)     W-SPAR2-AREA (2)                  
000308     MOVE ZERO TO  W-SPAR1-IDLOPNRM (1) W-SPAR1-IDLOPNRM (2)              
000309                   W-SPAR2-IDLOPNRM (1) W-SPAR2-IDLOPNRM (2)              
000310                   W-SPAR2-IDRADNR (1)  W-SPAR2-IDRADNR (2)               
000311     PERFORM S02-LAES-W61155ML                                            
000312                                                                          
000313     SET MAAL-IX TO +1                                                    
000314*                             --- LADDA MÅL-TABELL FÖR CDC                
000315     MOVE MAAL-IDDC    TO WS-IDDC                                         
000316     PERFORM UNTIL END-OF-W61155ML OR CDC-TR                              
000317       MOVE MAAL-AREA-DATA TO W-MAAL-TOTAL (1 MAAL-IX)                    
000318       SET MAAL-IX UP BY +1                                               
000319       PERFORM S02-LAES-W61155ML                                          
000320     END-PERFORM                                                          
000321*                             --- RENSA RESTEN AV CDC-TABELLEN            
000322     PERFORM UNTIL MAAL-IX > MAX-MAAL-IX                                  
000323       MOVE SPACE TO W-MAAL-KDINLUPF   (1 MAAL-IX)                        
000324                     W-MAAL-IDDC       (1 MAAL-IX)                        
000325       MOVE ZERO  TO W-MAAL-TIGLT      (1 MAAL-IX)                        
000326                     W-MAAL-TIGLT-PRIO (1 MAAL-IX)                        
000327       SET MAAL-IX UP BY +1                                               
000328     END-PERFORM                                                          
000329                                                                          
000330     SET MAAL-IX TO +1                                                    
000331*                             --- LADDA MÅL-TABELL FÖR ST                 
000332     PERFORM UNTIL END-OF-W61155ML                                        
000333       MOVE MAAL-AREA-DATA TO W-MAAL-TOTAL (2 MAAL-IX)                    
000334       SET MAAL-IX UP BY +1                                               
000335       PERFORM S02-LAES-W61155ML                                          
000336     END-PERFORM                                                          
000337*                             --- RENSA RESTEN AV ST-TABELLEN             
000338     PERFORM UNTIL MAAL-IX > MAX-MAAL-IX                                  
000339       MOVE SPACE TO W-MAAL-KDINLUPF   (2 MAAL-IX)                        
000340                     W-MAAL-IDDC       (2 MAAL-IX)                        
000341       MOVE ZERO  TO W-MAAL-TIGLT      (2 MAAL-IX)                        
000342                     W-MAAL-TIGLT-PRIO (2 MAAL-IX)                        
000343       SET MAAL-IX UP BY +1                                               
000344     END-PERFORM                                                          
000345     .                                                                    
000346     EJECT                                                                
000347 B-BEHANDLA-STATUS-POST     SECTION.                                      
000348                                                                          
000349*    MOVE IN-IDDC (2:1)  TO  DCIX                                         
000350     EVALUATE TRUE                                                        
000351       WHEN CDC-SE                                                        
000352         MOVE 1          TO  DCIX                                         
000353       WHEN CDC-TR                                                        
000354         MOVE 2          TO  DCIX                                         
000355       WHEN NDC-CN-71                                                     
000356         MOVE 3          TO  DCIX                                         
000357       WHEN NDC-CN-72                                                     
000358         MOVE 4          TO  DCIX                                         
000359       WHEN NDC-CN-73                                                     
000360         MOVE 5          TO  DCIX                                         
000361       WHEN NDC-CN-74                                                     
000362         MOVE 6          TO  DCIX                                         
000363     END-EVALUATE                                                         
000364     SET  SU-DCIX        TO  DCIX                                         
000365     SET  MA-DCIX        TO  DCIX                                         
000366                                                                          
000367     PERFORM BA-KOLLA-OM-NY-KDINLUPF                                      
000368     IF IN-IDLOPNRM    = W-SPAR1-IDLOPNRM (DCIX)                          
000369         CONTINUE                                                         
000370     ELSE                                                                 
000371         MOVE IN-AREA TO W-SPAR1-AREA (DCIX)                              
000372     END-IF                                                               
000373                                                                          
000374     IF  IN-IDLOPNRM        = W-SPAR2-IDLOPNRM (DCIX)                     
000375     AND IN-IDRADNR         = W-SPAR2-IDRADNR  (DCIX)                     
000376     AND IN-TIREGDAT        = DAGENS-DATUM                                
000377         PERFORM BB-TA-FRAM-KDINLUPF-INDEX                                
000378                                                                          
000379         IF IN-KDINLUPF-NXT = SPACE                                       
000380           IF  IN-KDINLUPF   = W-SPAR2-KDINLUPF (DCIX)                    
000381           AND IN-KDINLSTA   = W-SPAR2-KDINLSTA (DCIX)                    
000382           AND IN-KVINLART   = W-SPAR2-KVINLART (DCIX)                    
000383               CONTINUE                                                   
000384           ELSE                                                           
000385               PERFORM S10-KOLLA-OM-SISTA-FBGRUPP                         
000386               IF SISTA-FB                                                
000387                 PERFORM BC-BEHANDLA-PRODUCERAT-IDAG                      
000388               END-IF                                                     
000389               PERFORM BD-BEHANDLA-GENOMLOPPSTID                          
000390           END-IF                                                         
000391         ELSE                                                             
000392           IF  IN-KDINLUPF-NXT    = W-SPAR2-KDINLUPF (DCIX)               
000393           AND IN-KDINLSTA        = W-SPAR2-KDINLSTA (DCIX)               
000394           AND IN-KVINLART        = W-SPAR2-KVINLART (DCIX)               
000395              CONTINUE                                                    
000396           ELSE                                                           
000397              PERFORM S10-KOLLA-OM-SISTA-FBGRUPP                          
000398              IF SISTA-FB                                                 
000399                 PERFORM BC-BEHANDLA-PRODUCERAT-IDAG                      
000400              END-IF                                                      
000401              PERFORM BD-BEHANDLA-GENOMLOPPSTID                           
000402           END-IF                                                         
000403         END-IF                                                           
000404                                                                          
000405     END-IF                                                               
000406     IF (IN-KDINLSTA = 'INL' OR 'VOR' OR 'FRD')                           
000407     AND IN-TIREGDAT = DAGENS-DATUM                                       
000408       IF IN-IDRADNR NOT = W-SPAR2-IDRADNR (DCIX)                         
000409                                                                          
000410         PERFORM BA-KOLLA-OM-NY-KDINLUPF                                  
000411                                                                          
000412         ADD +1   TO W-SUM-KVRADER (SU-DCIX SUM-IX)                       
000413         IF IN-KDINLPRIO      <  +31                                      
000414           ADD +1      TO W-SUM-KVRADER-PRIO (SU-DCIX SUM-IX)             
000415                                                                          
000416           COMPUTE WS-SUBEL-PRIO  =  IN-PRARTSTD * IN-KVINLART            
000417           ADD     WS-SUBEL-PRIO                                          
000418                             TO W-SUM-SUBEL-PRIO (SU-DCIX SUM-IX)         
000419         END-IF                                                           
000420                                                                          
000421         COMPUTE W-SUM-SUBEL (SU-DCIX SUM-IX) =                           
000422                 W-SUM-SUBEL (SU-DCIX SUM-IX)                             
000423               + (IN-KVINLART * IN-PRARTSTD)                              
000424                                                                          
000425         IF IN-IDLOPNRM NOT = W-LAST-IDLOPNRM  (SU-DCIX SUM-IX)           
000426           ADD +1           TO W-SUM-KVART     (SU-DCIX SUM-IX)           
000427           MOVE IN-IDLOPNRM TO W-LAST-IDLOPNRM (SU-DCIX SUM-IX)           
000428         END-IF                                                           
000429       END-IF                                                             
000430     END-IF                                                               
000431                                                                          
000432     MOVE IN-AREA     TO W-SPAR2-AREA (DCIX)                              
000433                                                                          
000434     IF  IN-TIREGDAT     = DAGENS-DATUM                                   
000435     AND (IN-KDINLSTA     = 'INL' OR 'VOR' OR 'FRD')                      
000436     AND IN-IDRADNR  NOT = +1                                             
000437            PERFORM BE-BEHANDLA-TOTALER                                   
000438     END-IF                                                               
000439     .                                                                    
000440     EJECT                                                                
000441 BA-KOLLA-OM-NY-KDINLUPF     SECTION.                                     
000442                                                                          
000443     IF IN-KDINLUPF-NXT        = SPACE                                    
000444         MOVE IN-KDINLUPF      TO W-KDINLUPF (DCIX)                       
000445     ELSE                                                                 
000446         MOVE IN-KDINLUPF-NXT  TO W-KDINLUPF (DCIX)                       
000447     END-IF                                                               
000448                                                                          
000449     IF  W-KDINLUPF (DCIX)   = SPACE                                      
000450     AND IN-KDINLSTA     = SPACE                                          
000451        MOVE 'R31 '           TO W-KDINLUPF (DCIX)                        
000452     END-IF                                                               
000453                                                                          
000454     IF  W-KDINLUPF (DCIX)   = SPACE                                      
000455     AND IN-KDINLSTA     = 'AVI'                                          
000456        MOVE 'C2  '           TO W-KDINLUPF (DCIX)                        
000457     END-IF                                                               
000458                                                                          
000459     SET SUM-IX    TO +1                                                  
000460     PERFORM UNTIL SUM-IX  >  MAX-SUM-IX                                  
000461             OR W-SUM-KDINLUPF (SU-DCIX SUM-IX) = SPACE                   
000462             OR W-SUM-KDINLUPF (SU-DCIX SUM-IX) = W-KDINLUPF(DCIX)        
000463                 SET SUM-IX UP BY +1                                      
000464     END-PERFORM                                                          
000465                                                                          
000466     IF SUM-IX   > MAX-SUM-IX                                             
000467         STRING 'SUM TAB FULL FÖR IDDC=' IN-IDDC                          
000468         DELIMITED BY SIZE   INTO  FELTEXT-STR                            
000469         DISPLAY FELTEXT                                                  
000470         CALL ABEND USING RKOD-ABEND-MED-DUMP                             
000471     ELSE                                                                 
000472        IF W-SUM-KDINLUPF (SU-DCIX SUM-IX) = SPACE                        
000473           MOVE W-KDINLUPF (DCIX)                                         
000474                        TO W-SUM-KDINLUPF     (SU-DCIX SUM-IX)            
000475           MOVE ZERO    TO W-SUM-KVRADER      (SU-DCIX SUM-IX)            
000476                           W-SUM-KVRADER-PRIO (SU-DCIX SUM-IX)            
000477                           W-SUM-KVART        (SU-DCIX SUM-IX)            
000478                           W-SUM-SUBEL        (SU-DCIX SUM-IX)            
000479                           W-SUM-SUBEL-PRIO   (SU-DCIX SUM-IX)            
000480                           W-SUM-TIGLT-MM     (SU-DCIX SUM-IX)            
000481                           W-SUM-TIGLT-PRIO-MM (SU-DCIX SUM-IX)           
000482                           W-SUM-KVRADER-MAAL (SU-DCIX SUM-IX)            
000483                        W-SUM-KVRADER-MAAL-PRIO (SU-DCIX SUM-IX)          
000484                           W-LAST-IDLOPNRM    (SU-DCIX SUM-IX)            
000485         END-IF                                                           
000486     END-IF                                                               
000487     .                                                                    
000488     EJECT                                                                
000489 BB-TA-FRAM-KDINLUPF-INDEX  SECTION.                                      
000490                                                                          
000491** GÅ IGENOM KDINLUPF TAB FÖR ATT FÅ FRAM RÄTT SUM-IX EFTERSOM            
000492** GENOMLOPPSTIDEN M M SKALL BERÄKNAS PÅ FÖREGÅENDE POST                  
000493** FÖR DETTA IDDC.                                                        
000494                                                                          
000495     IF W-SPAR2-KDINLUPF-NXT (DCIX)     = SPACE                           
000496         MOVE W-SPAR2-KDINLUPF (DCIX)     TO W-KDINLUPF (DCIX)            
000497     ELSE                                                                 
000498         MOVE W-SPAR2-KDINLUPF-NXT (DCIX) TO W-KDINLUPF (DCIX)            
000499     END-IF                                                               
000500                                                                          
000501     IF  W-KDINLUPF (DCIX)          = SPACE                               
000502     AND W-SPAR2-KDINLSTA (DCIX)    = SPACE                               
000503         MOVE 'R31 '           TO W-KDINLUPF (DCIX)                       
000504     END-IF                                                               
000505                                                                          
000506     IF  W-KDINLUPF (DCIX)          = SPACE                               
000507     AND W-SPAR2-KDINLSTA (DCIX)    = 'AVI'                               
000508         MOVE 'C2  '           TO W-KDINLUPF (DCIX)                       
000509     END-IF                                                               
000510                                                                          
000511     SET SUM-IX      TO +1                                                
000512     PERFORM UNTIL SUM-IX  >  MAX-SUM-IX                                  
000513             OR W-SUM-KDINLUPF (SU-DCIX SUM-IX) = W-KDINLUPF(DCIX)        
000514                  SET SUM-IX UP BY +1                                     
000515     END-PERFORM                                                          
000516                                                                          
000517     IF SUM-IX  > MAX-SUM-IX                                              
000518         STRING  'KDINLUPF SAKNAS I SUM TAB FÖR IDDC=' IN-IDDC            
000519         DELIMITED BY SIZE   INTO  FELTEXT-STR                            
000520         DISPLAY FELTEXT                                                  
000521         CALL ABEND USING RKOD-ABEND-MED-DUMP                             
000522     END-IF                                                               
000523     .                                                                    
000524     EJECT                                                                
000525 BC-BEHANDLA-PRODUCERAT-IDAG SECTION.                                     
000526                                                                          
000527     IF  IN-KDINLSTA = W-SPAR2-KDINLSTA (DCIX)                            
000528     AND IN-KDINLUPF = W-SPAR2-KDINLUPF (DCIX)                            
000529     AND ( IN-KVINLART NOT = W-SPAR2-KVINLART (DCIX) )                    
000530       MOVE NEJ TO ADD-RADER-SW                                           
000531     ELSE                                                                 
000532       MOVE JA  TO ADD-RADER-SW                                           
000533       ADD +1   TO W-SUM-KVRADER (SU-DCIX SUM-IX)                         
000534       IF IN-KDINLPRIO      <  +31                                        
000535         ADD +1           TO W-SUM-KVRADER-PRIO (SU-DCIX  SUM-IX)         
000536         COMPUTE WS-SUBEL-PRIO = IN-PRARTSTD * IN-KVINLART                
000537         ADD     WS-SUBEL-PRIO                                            
000538                             TO W-SUM-SUBEL-PRIO (SU-DCIX SUM-IX)         
000539       END-IF                                                             
000540                                                                          
000541       COMPUTE W-SUM-SUBEL (SU-DCIX SUM-IX) =                             
000542               W-SUM-SUBEL (SU-DCIX SUM-IX)                               
000543                                    + (IN-KVINLART * IN-PRARTSTD)         
000544                                                                          
000545       IF IN-IDLOPNRM NOT = W-LAST-IDLOPNRM (SU-DCIX SUM-IX)              
000546         ADD +1           TO W-SUM-KVART    (SU-DCIX SUM-IX)              
000547         MOVE IN-IDLOPNRM TO W-LAST-IDLOPNRM(SU-DCIX SUM-IX)              
000548       END-IF                                                             
000549     END-IF                                                               
000550                                                                          
000551     .                                                                    
000552     EJECT                                                                
000553 BD-BEHANDLA-GENOMLOPPSTID   SECTION.                                     
000554                                                                          
000555     PERFORM BDA-BERAEKNA-ARBETSDAGAR                                     
000556     MOVE IN-TIKLOCK              TO W-NEW-TIKLOCK                        
000557     MOVE W-SPAR2-TIKLOCK (DCIX)  TO W-OLD-TIKLOCK                        
000558                                                                          
000559     PERFORM S20-BERAEKNA-TIGLT                                           
000560                                                                          
000561     IF IN-KVKOLLI = +0                                                   
000562       ADD W-TOT-MM      TO W-SUM-TIGLT-MM (SU-DCIX SUM-IX)               
000563     ELSE                                                                 
000564       COMPUTE W-SUM-TIGLT-MM (SU-DCIX SUM-IX) =                          
000565               W-SUM-TIGLT-MM (SU-DCIX SUM-IX) +                          
000566                                            IN-KVKOLLI * W-TOT-MM         
000567     END-IF                                                               
000568                                                                          
000569     IF CDC-SE                                                            
000570       PERFORM S11-KOLLA-MAAL-TABELL                                      
000571     END-IF                                                               
000572                                                                          
000573     IF IN-KDINLPRIO      <  +31                                          
000574         ADD W-TOT-MM  TO W-SUM-TIGLT-PRIO-MM (SU-DCIX SUM-IX)            
000575*        IF ADD-RADER OR IN-FLINLI = JA                                   
000576         IF (ADD-RADER OR IN-FLINLI = JA) AND CDC-SE                      
000577           IF MAAL-IX NOT > MAX-MAAL-IX                                   
000578                                                                          
000579             IF W-TOT-MM < W-MAAL-TIGLT-PRIO (MA-DCIX MAAL-IX)            
000580               ADD +1 TO W-SUM-KVRADER-MAAL-PRIO (SU-DCIX SUM-IX)         
000581             END-IF                                                       
000582                                                                          
000583             IF W-TOT-MM < W-MAAL-TIGLT    (MA-DCIX MAAL-IX)              
000584               ADD +1 TO W-SUM-KVRADER-MAAL (SU-DCIX SUM-IX)              
000585             END-IF                                                       
000586           END-IF                                                         
000587         END-IF                                                           
000588     ELSE                                                                 
000589*       IF ADD-RADER OR IN-FLINLI = JA                                    
000590        IF (ADD-RADER OR IN-FLINLI = JA) AND CDC-SE                       
000591          IF MAAL-IX NOT > MAX-MAAL-IX                                    
000592                                                                          
000593            IF W-TOT-MM < W-MAAL-TIGLT     (MA-DCIX MAAL-IX)              
000594              ADD +1 TO W-SUM-KVRADER-MAAL (SU-DCIX SUM-IX)               
000595            END-IF                                                        
000596          END-IF                                                          
000597        END-IF                                                            
000598     END-IF                                                               
000599     .                                                                    
000600     EJECT                                                                
000601 BDA-BERAEKNA-ARBETSDAGAR    SECTION.                                     
000602                                                                          
000603     MOVE +001                       TO WORK-KDCALL                       
000604     IF WS-IDDC = '12'                                                    
000605       MOVE '91'                     TO WORK-IDDC                         
000606     ELSE                                                                 
000607       MOVE WS-IDDC                  TO WORK-IDDC                         
000608     END-IF                                                               
000609     MOVE W-SPAR2-TIREGDAT (DCIX)    TO WORK-TIAAMMDD-FOM                 
000610     MOVE IN-TIREGDAT                TO WORK-TIAAMMDD-TOM                 
000611                                                                          
000612     IF WORK-TIAAMMDD-FOM       =  WORK-TIAAMMDD-TOM                      
000613         MOVE ZERO             TO  WORK-KVWORKD                           
000614     ELSE                                                                 
000615         CALL WORKDAY USING WORK-KDCALL                                   
000616                   WORK-DATE-AREA WORK-KDSVAR                             
000617                                                                          
000618         IF WORK-KDSVAR-OK                                                
000619** STARTDAG OCH SLUTDAG FÖRUTSÄTTS VARA ICKE-HELA ARBETSDAGAR             
000620             IF WORK-KVWORKD > 2                                          
000621               COMPUTE  WORK-KVWORKD = WORK-KVWORKD - 2                   
000622             ELSE                                                         
000623               MOVE ZERO TO WORK-KVWORKD                                  
000624             END-IF                                                       
000625         ELSE                                                             
000626             MOVE 'FEL UR WORKDAY' TO FELTEXT-STR                         
000627             CALL ABEND USING RKOD-ABEND-MED-DUMP                         
000628         END-IF                                                           
000629     END-IF                                                               
000630     .                                                                    
000631     EJECT                                                                
000632 BE-BEHANDLA-TOTALER         SECTION.                                     
000633                                                                          
000634     ADD +1                   TO W-TOT-KVRADER (DCIX)                     
000635                                                                          
000636     COMPUTE W-TOT-SUBEL (DCIX) = W-TOT-SUBEL (DCIX)                      
000637                                + (IN-KVINLART * IN-PRARTSTD)             
000638                                                                          
000639     IF IN-IDLOPNRM NOT = W-TOT-IDLOPNRM (DCIX)                           
000640       ADD +1 TO W-TOT-KVART (DCIX)                                       
000641       MOVE IN-IDLOPNRM TO W-TOT-IDLOPNRM (DCIX)                          
000642     END-IF                                                               
000643                                                                          
000644     PERFORM BEA-BEHANDLA-GENOMLOPPSTID-TOT                               
000645     .                                                                    
000646     EJECT                                                                
000647 BEA-BEHANDLA-GENOMLOPPSTID-TOT   SECTION.                                
000648                                                                          
000649     PERFORM BEAA-BERAEKNA-ARBETSDAGAR-TOT                                
000650     MOVE IN-TIKLOCK              TO W-NEW-TIKLOCK                        
000651     MOVE W-SPAR1-TIKLOCK (DCIX)  TO W-OLD-TIKLOCK                        
000652                                                                          
000653     PERFORM S20-TOT-BERAEKNA-TIGLT                                       
000654                                                                          
000655     ADD W-TOT-MM              TO W-TOT-TIGLT-MM (DCIX)                   
000656                                                                          
000657     MOVE W-KDINLUPF (DCIX)    TO SPAR-KDINLUPF (DCIX)                    
000658     MOVE 'TOT '               TO W-KDINLUPF    (DCIX)                    
000659     PERFORM S11-KOLLA-MAAL-TABELL                                        
000660     MOVE SPAR-KDINLUPF (DCIX) TO W-KDINLUPF    (DCIX)                    
000661                                                                          
000662     IF IN-KDINLPRIO           <  +31                                     
000663         ADD W-TOT-MM          TO W-TOT-TIGLT-PRIO-MM (DCIX)              
000664         ADD +1                TO W-TOT-KVRADER-PRIO  (DCIX)              
000665         COMPUTE WS-TOT-SUBEL-PRIO  = IN-PRARTSTD * IN-KVINLART           
000666         ADD     WS-TOT-SUBEL-PRIO  TO W-TOT-SUBEL-PRIO (DCIX)            
000667*        IF MAAL-IX NOT > MAX-MAAL-IX                                     
000668         IF (MAAL-IX NOT > MAX-MAAL-IX) AND CDC-SE                        
000669           IF W-TOT-MM  < W-MAAL-TIGLT-PRIO  (MA-DCIX MAAL-IX)            
000670              ADD +1 TO W-TOT-KVRADER-MAAL-PRIO (DCIX)                    
000671           END-IF                                                         
000672           IF W-TOT-MM < W-MAAL-TIGLT      (MA-DCIX MAAL-IX)              
000673                 ADD +1 TO W-TOT-KVRADER-MAAL (DCIX)                      
000674           END-IF                                                         
000675         END-IF                                                           
000676     ELSE                                                                 
000677*      IF MAAL-IX NOT > MAX-MAAL-IX                                       
000678       IF (MAAL-IX NOT > MAX-MAAL-IX) AND CDC-SE                          
000679         IF W-TOT-MM < W-MAAL-TIGLT  (MA-DCIX MAAL-IX)                    
000680           ADD +1 TO W-TOT-KVRADER-MAAL (DCIX)                            
000681         END-IF                                                           
000682       END-IF                                                             
000683     END-IF                                                               
000684     .                                                                    
000685     EJECT                                                                
000686 BEAA-BERAEKNA-ARBETSDAGAR-TOT  SECTION.                                  
000687                                                                          
000688     MOVE +001                    TO WORK-KDCALL                          
000689                                                                          
000690     IF WS-IDDC = '12'                                                    
000691        MOVE '91'                 TO WORK-IDDC                            
000692     ELSE                                                                 
000693        MOVE WS-IDDC              TO WORK-IDDC                            
000694     END-IF                                                               
000695                                                                          
000696     MOVE W-SPAR1-TIREGDAT (DCIX) TO WORK-TIAAMMDD-FOM                    
000697     MOVE IN-TIREGDAT             TO WORK-TIAAMMDD-TOM                    
000698                                                                          
000699     IF WORK-TIAAMMDD-FOM       =  WORK-TIAAMMDD-TOM                      
000700         MOVE ZERO             TO WORK-KVWORKD                            
000701      ELSE                                                                
000702         CALL WORKDAY USING WORK-KDCALL                                   
000703                   WORK-DATE-AREA WORK-KDSVAR                             
000704                                                                          
000705         IF WORK-KDSVAR-OK                                                
000706** STARTDAG OCH SLUTDAG FÖRUTSÄTTS VARA ICKE-HELA ARBETSDAGAR             
000707             IF WORK-KVWORKD > 2                                          
000708               COMPUTE  WORK-KVWORKD = WORK-KVWORKD - 2                   
000709             ELSE                                                         
000710               MOVE ZERO TO WORK-KVWORKD                                  
000711             END-IF                                                       
000712         ELSE                                                             
000713             MOVE 'FEL UR WORKDAY' TO FELTEXT-STR                         
000714             CALL ABEND USING RKOD-ABEND-MED-DUMP                         
000715         END-IF                                                           
000716     END-IF                                                               
000717     .                                                                    
000718     EJECT                                                                
000719 C-TOEM-KDINLUPF-TAB        SECTION.                                      
000720                                                                          
000721     MOVE   'RAD'    TO   UT-IDPTYP                                       
000722     MOVE    +1      TO   DCIX                                            
000723     SET   SU-DCIX   TO   DCIX                                            
000724     PERFORM UNTIL SU-DCIX  >  +6                                         
000725                                                                          
000726*      MOVE    DCIX  TO  WS-DCIX                                          
000727*      MOVE IDDC-WS  TO  UT-IDDC                                          
000728       EVALUATE DCIX                                                      
000729         WHEN 1                                                           
000730           MOVE WC-CDC-SE              TO UT-IDDC                         
000731         WHEN 2                                                           
000732           MOVE WC-CDC-TR              TO UT-IDDC                         
000733         WHEN 3                                                           
000734           MOVE WC-NDC-CN-71           TO UT-IDDC                         
000735         WHEN 4                                                           
000736           MOVE WC-NDC-CN-72           TO UT-IDDC                         
000737         WHEN 5                                                           
000738           MOVE WC-NDC-CN-73           TO UT-IDDC                         
000739         WHEN 6                                                           
000740           MOVE WC-NDC-CN-74           TO UT-IDDC                         
000741       END-EVALUATE                                                       
000742                                                                          
000743       SET   SUM-IX    TO +1                                              
000744       PERFORM UNTIL SUM-IX  >  MAX-SUM-IX                                
000745               OR  W-SUM-KDINLUPF (SU-DCIX SUM-IX) =  SPACE               
000746         MOVE W-SUM-KDINLUPF     (SU-DCIX SUM-IX) TO UT-KDINLUPF          
000747         MOVE W-SUM-KVRADER      (SU-DCIX SUM-IX) TO UT-KVRADER           
000748         MOVE W-SUM-KVRADER-PRIO (SU-DCIX SUM-IX)                         
000749                                               TO UT-KVRADER-PRIO         
000750         MOVE W-SUM-KVART        (SU-DCIX SUM-IX) TO UT-KVART             
000751         MOVE W-SUM-SUBEL        (SU-DCIX SUM-IX) TO UT-SUBEL             
000752         MOVE W-SUM-SUBEL-PRIO  (SU-DCIX SUM-IX) TO UT-SUBEL-PRIO         
000753         MOVE W-SUM-KVRADER-MAAL (SU-DCIX SUM-IX)                         
000754                                          TO UT-KVRADER-MAAL              
000755         MOVE W-SUM-KVRADER-MAAL-PRIO (SU-DCIX SUM-IX)                    
000756                                          TO UT-KVRADER-MAAL-PRIO         
000757                                                                          
000758         IF W-SUM-KVRADER (SU-DCIX SUM-IX) > ZERO                         
000759             COMPUTE W-TIGLT-MM (DCIX)  =                                 
000760                   W-SUM-TIGLT-MM (SU-DCIX SUM-IX)                        
000761                 / W-SUM-KVRADER  (SU-DCIX SUM-IX)                        
000762             DIVIDE W-TIGLT-MM (DCIX) BY 60 GIVING                        
000763                   W-GLT-HH REMAINDER W-GLT-MM                            
000764             COMPUTE UT-TIGLT     =  W-GLT   / 100                        
000765                                                                          
000766             IF W-SUM-TIGLT-PRIO-MM (SU-DCIX SUM-IX) = ZERO OR            
000767                W-SUM-KVRADER-PRIO  (SU-DCIX SUM-IX) = ZERO               
000768                 MOVE ZERO                TO UT-TIGLT-PRIO                
000769             ELSE                                                         
000770                 COMPUTE W-TIGLT-MM (DCIX) =                              
000771                     W-SUM-TIGLT-PRIO-MM (SU-DCIX SUM-IX) /               
000772                     W-SUM-KVRADER-PRIO  (SU-DCIX SUM-IX)                 
000773                 DIVIDE W-TIGLT-MM (DCIX) BY 60 GIVING                    
000774                 W-GLT-HH REMAINDER W-GLT-MM                              
000775                 COMPUTE UT-TIGLT-PRIO = W-GLT / 100                      
000776             END-IF                                                       
000777                                                                          
000778             PERFORM S03-SKRIV-W61151                                     
000779         END-IF                                                           
000780         SET SUM-IX UP BY +1                                              
000781       END-PERFORM                                                        
000782                                                                          
000783       SET SU-DCIX UP BY +1                                               
000784       ADD   +1    TO    DCIX                                             
000785     END-PERFORM                                                          
000786     .                                                                    
000787     EJECT                                                                
000788 D-SKRIV-TOTAL-POST         SECTION.                                      
000789                                                                          
000790     MOVE      +1     TO   DCIX                                           
000791     MOVE    'TOT'    TO   UT-IDPTYP                                      
000792     MOVE    SPACE    TO   UT-KDINLUPF                                    
000793     PERFORM UNTIL DCIX > +6                                              
000794                                                                          
000795*      MOVE DCIX      TO WS-DCIX                                          
000796*      MOVE              IDDC-WS       TO UT-IDDC                         
000797       EVALUATE DCIX                                                      
000798         WHEN 1                                                           
000799           MOVE WC-CDC-SE              TO UT-IDDC                         
000800         WHEN 2                                                           
000801           MOVE WC-CDC-TR              TO UT-IDDC                         
000802         WHEN 3                                                           
000803           MOVE WC-NDC-CN-71           TO UT-IDDC                         
000804         WHEN 4                                                           
000805           MOVE WC-NDC-CN-72           TO UT-IDDC                         
000806         WHEN 5                                                           
000807           MOVE WC-NDC-CN-73           TO UT-IDDC                         
000808         WHEN 6                                                           
000809           MOVE WC-NDC-CN-74           TO UT-IDDC                         
000810       END-EVALUATE                                                       
000811                                                                          
000812       MOVE W-TOT-KVRADER       (DCIX) TO UT-KVRADER                      
000813       MOVE W-TOT-KVRADER-PRIO  (DCIX) TO UT-KVRADER-PRIO                 
000814       MOVE W-TOT-KVART         (DCIX) TO UT-KVART                        
000815       MOVE W-TOT-SUBEL         (DCIX) TO UT-SUBEL                        
000816       MOVE W-TOT-SUBEL-PRIO    (DCIX) TO UT-SUBEL-PRIO                   
000817       MOVE W-TOT-KVRADER-MAAL  (DCIX) TO UT-KVRADER-MAAL                 
000818       MOVE W-TOT-KVRADER-MAAL-PRIO (DCIX)                                
000819                                       TO UT-KVRADER-MAAL-PRIO            
000820       IF W-TOT-KVRADER (DCIX)  =  ZERO                                   
000821           MOVE ZERO                   TO UT-TIGLT                        
000822       ELSE                                                               
000823           COMPUTE   W-TIGLT-MM     (DCIX)  =                             
000824                     W-TOT-TIGLT-MM (DCIX)                                
000825                   / W-TOT-KVRADER  (DCIX)                                
000826                                                                          
000827           DIVIDE W-TIGLT-MM (DCIX)  BY 60 GIVING                         
000828                  W-GLT-HH REMAINDER W-GLT-MM                             
000829                                                                          
000830           COMPUTE UT-TIGLT      =  W-GLT / 100                           
000831       END-IF                                                             
000832                                                                          
000833       IF W-TOT-KVRADER-PRIO (DCIX) =  ZERO                               
000834           MOVE ZERO             TO UT-TIGLT-PRIO                         
000835       ELSE                                                               
000836           COMPUTE    W-TIGLT-MM          (DCIX) =                        
000837                      W-TOT-TIGLT-PRIO-MM (DCIX)                          
000838                    / W-TOT-KVRADER-PRIO  (DCIX)                          
000839                                                                          
000840           DIVIDE W-TIGLT-MM (DCIX) BY 60 GIVING                          
000841                  W-GLT-HH REMAINDER W-GLT-MM                             
000842                                                                          
000843           COMPUTE  UT-TIGLT-PRIO   =   W-GLT  / 100                      
000844       END-IF                                                             
000845       PERFORM S03-SKRIV-W61151                                           
000846       ADD  +1   TO   DCIX                                                
000847     END-PERFORM                                                          
000848     .                                                                    
000849     EJECT                                                                
000850 Z-FINIT SECTION.                                                         
000851     CLOSE W61140                                                         
000852           W61155ML                                                       
000853           W61151                                                         
000854     SKIP2                                                                
000855     MOVE 'S' TO POSTSUM-OPKOD                                            
000856     CALL POSTSUM USING POSTSUM-PARM                                      
000857     .                                                                    
000858     EJECT                                                                
000859 S01-LAES-W61140  SECTION.                                                
000860     SKIP2                                                                
000861     READ W61140 INTO IN-AREA                                             
000862     AT END                                                               
000863        SET END-OF-W61140 TO TRUE                                         
000864                                                                          
000865     NOT AT END                                                           
000866        MOVE 'W61140'          TO POSTSUM-FDNAMN                          
000867        MOVE 'W61151D1'        TO POSTSUM-DDNAMN2                         
000868        CALL POSTSUM USING POSTSUM-PARM                                   
000869     END-READ                                                             
000870     .                                                                    
000871     EJECT                                                                
000872 S02-LAES-W61155ML SECTION.                                               
000873     SKIP2                                                                
000874     READ W61155ML INTO MAAL-AREA                                         
000875     AT END                                                               
000876        SET  END-OF-W61155ML TO TRUE                                      
000877                                                                          
000878     NOT AT END                                                           
000879        MOVE 'W61155ML'            TO POSTSUM-FDNAMN                      
000880        MOVE 'W61151D2'            TO POSTSUM-DDNAMN2                     
000881        CALL POSTSUM USING POSTSUM-PARM                                   
000882     END-READ                                                             
000883     .                                                                    
000884     EJECT                                                                
000885 S03-SKRIV-W61151 SECTION.                                                
000886     SKIP2                                                                
000887     WRITE UT-POST FROM UT-AREA                                           
000888                                                                          
000889     MOVE UT-IDPTYP            TO POSTSUM-TRANSTYP                        
000890     MOVE 'W61151'             TO POSTSUM-FDNAMN                          
000891     MOVE 'W61151D3'           TO POSTSUM-DDNAMN2                         
000892     CALL POSTSUM USING POSTSUM-PARM                                      
000893     .                                                                    
000894     EJECT                                                                
000895 S10-KOLLA-OM-SISTA-FBGRUPP SECTION.                                      
000896     SKIP2                                                                
000897     MOVE JA TO SISTA-FB-SW                                               
000898     IF IN-KDINLUPF NOT = W-SPAR2-KDINLUPF (DCIX)                         
000899       IF IN-KDINLUPF (1:2) = 'FB' OR                                     
000900          IN-KDINLUPF (1:3) = '573' OR                                    
000901          IN-KDINLUPF (1:3) = '583' OR                                    
000902          IN-KDINLUPF (1:3) = '588' OR                                    
000903          IN-KDINLUPF (1:3) = '589' OR                                    
000904          IN-KDINLUPF (1:3) = 'EM1' OR                                    
000905          IN-KDINLUPF (1:3) = '512' OR                                    
000906          IN-KDINLUPF (1:3) = 'SVS'                                       
000907         IF W-SPAR2-KDINLUPF (DCIX) (1:2) = 'FB'  OR                      
000908            W-SPAR2-KDINLUPF (DCIX) (1:3) = '573' OR                      
000909            W-SPAR2-KDINLUPF (DCIX) (1:3) = '583' OR                      
000910            W-SPAR2-KDINLUPF (DCIX) (1:3) = '588' OR                      
000911            W-SPAR2-KDINLUPF (DCIX) (1:3) = '589' OR                      
000912            W-SPAR2-KDINLUPF (DCIX) (1:3) = 'EM1' OR                      
000913            W-SPAR2-KDINLUPF (DCIX) (1:3) = '512' OR                      
000914            W-SPAR2-KDINLUPF (DCIX) (1:3) = 'SVS'                         
000915            MOVE NEJ     TO SISTA-FB-SW                                   
000916         END-IF                                                           
000917       END-IF                                                             
000918     END-IF                                                               
000919     .                                                                    
000920     EJECT                                                                
000921 S11-KOLLA-MAAL-TABELL SECTION.                                           
000922     SKIP2                                                                
000923     SET MAAL-IX    TO +1                                                 
000924     PERFORM UNTIL MAAL-IX  > MAX-MAAL-IX                                 
000925             OR    W-MAAL-KDINLUPF  (MA-DCIX MAAL-IX)                     
000926                 = W-KDINLUPF       (DCIX)                                
000927        SET MAAL-IX UP BY +1                                              
000928     END-PERFORM                                                          
000929     .                                                                    
000930     EJECT                                                                
000931 S20-BERAEKNA-TIGLT   SECTION.                                            
000932                                                                          
000933     PERFORM S20A-JUSTERA-TIDERNA                                         
000934                                                                          
000935     PERFORM S20B-BESTAEM-FM-EM                                           
000936                                                                          
000937     PERFORM S20C-BERAEKNA-OLD                                            
000938                                                                          
000939     PERFORM S20D-BERAEKNA-NEW                                            
000940                                                                          
000941     IF W-SPAR2-TIREGDAT (DCIX) = IN-TIREGDAT                             
000942       COMPUTE W-TIKLOCK-MM =                                             
000943       ((W-TIKLOCK-OLD-MM + W-TIKLOCK-NEW-MM) - 492)                      
000944     ELSE                                                                 
000945       COMPUTE W-TIKLOCK-MM = W-TIKLOCK-OLD-MM + W-TIKLOCK-NEW-MM         
000946     END-IF                                                               
000947                                                                          
000948*** 8.2 TIM = 492 MIN                                                     
000949     COMPUTE W-TOT-MM          = (WORK-KVWORKD * 492) +                   
000950                                  W-TIKLOCK-MM                            
000951     .                                                                    
000952     EJECT                                                                
000953 S20-TOT-BERAEKNA-TIGLT   SECTION.                                        
000954                                                                          
000955     PERFORM S20A-JUSTERA-TIDERNA                                         
000956                                                                          
000957     PERFORM S20B-BESTAEM-FM-EM                                           
000958                                                                          
000959     PERFORM S20C-BERAEKNA-OLD                                            
000960                                                                          
000961     PERFORM S20D-BERAEKNA-NEW                                            
000962                                                                          
000963     IF W-SPAR1-TIREGDAT (DCIX) = IN-TIREGDAT                             
000964       COMPUTE W-TIKLOCK-MM =                                             
000965       ((W-TIKLOCK-OLD-MM + W-TIKLOCK-NEW-MM) - 492)                      
000966     ELSE                                                                 
000967       COMPUTE W-TIKLOCK-MM = W-TIKLOCK-OLD-MM + W-TIKLOCK-NEW-MM         
000968     END-IF                                                               
000969                                                                          
000970*** 8.2 TIM = 492 MIN                                                     
000971     COMPUTE W-TOT-MM          = (WORK-KVWORKD * 492) +                   
000972                                  W-TIKLOCK-MM                            
000973     .                                                                    
000974     EJECT                                                                
000975 S20A-JUSTERA-TIDERNA  SECTION.                                           
000976                                                                          
000977** OM TIDERNA LIGGER UTANFÖR 07.00 - 11.00 ; 11.42 - 15.54                
000978** SÅ JUSTERAS DESSA TILL NÄRMSTA INTERVALL                               
000979                                                                          
000980     IF W-OLD-HH < 7                                                      
000981       MOVE 7    TO W-OLD-HH                                              
000982       MOVE ZERO TO W-OLD-MM                                              
000983     ELSE                                                                 
000984       IF W-OLD-HH = 11                                                   
000985         IF W-OLD-MM < 42                                                 
000986           IF W-OLD-MM < 21                                               
000987             MOVE ZERO TO W-OLD-MM                                        
000988           ELSE                                                           
000989             MOVE 42   TO W-OLD-MM                                        
000990           END-IF                                                         
000991         END-IF                                                           
000992       ELSE                                                               
000993         IF (W-OLD-HH > 15) OR (W-OLD-HH = 15 AND W-OLD-MM > 54)          
000994           MOVE 15 TO W-OLD-HH                                            
000995           MOVE 54 TO W-OLD-MM                                            
000996         END-IF                                                           
000997       END-IF                                                             
000998     END-IF                                                               
000999                                                                          
001000                                                                          
001001     IF W-NEW-HH < 7                                                      
001002       MOVE 7    TO W-NEW-HH                                              
001003       MOVE ZERO TO W-NEW-MM                                              
001004     ELSE                                                                 
001005       IF W-NEW-HH = 11                                                   
001006         IF W-NEW-MM < 42                                                 
001007           IF W-NEW-MM < 21                                               
001008             MOVE ZERO TO W-NEW-MM                                        
001009           ELSE                                                           
001010             MOVE 42   TO W-NEW-MM                                        
001011           END-IF                                                         
001012         END-IF                                                           
001013       ELSE                                                               
001014         IF (W-NEW-HH > 15) OR (W-NEW-HH = 15 AND W-NEW-MM > 54)          
001015           MOVE 15 TO W-NEW-HH                                            
001016           MOVE 54 TO W-NEW-MM                                            
001017         END-IF                                                           
001018       END-IF                                                             
001019     END-IF                                                               
001020                                                                          
001021     .                                                                    
001022     EJECT                                                                
001023 S20B-BESTAEM-FM-EM  SECTION.                                             
001024                                                                          
001025     MOVE 'FM' TO OLD-SW                                                  
001026     IF W-OLD-HH <= 11                                                    
001027       IF W-OLD-HH = 11 AND W-OLD-MM > 41                                 
001028         MOVE 'EM' TO OLD-SW                                              
001029       END-IF                                                             
001030     ELSE                                                                 
001031       MOVE 'EM' TO OLD-SW                                                
001032     END-IF                                                               
001033                                                                          
001034     MOVE 'FM' TO NEW-SW                                                  
001035     IF W-NEW-HH <= 11                                                    
001036       IF W-NEW-HH = 11 AND W-NEW-MM > 41                                 
001037         MOVE 'EM' TO NEW-SW                                              
001038       END-IF                                                             
001039     ELSE                                                                 
001040       MOVE 'EM' TO NEW-SW                                                
001041     END-IF                                                               
001042                                                                          
001043     .                                                                    
001044     EJECT                                                                
001045 S20C-BERAEKNA-OLD SECTION.                                               
001046                                                                          
001047     IF OLD-FM                                                            
001048       COMPUTE W-TIKLOCK-OLD-MM =                                         
001049       (((11 * 60) - (W-OLD-HH * 60)) + (0 - W-OLD-MM)) + 252             
001050** 4.2 TIM = 252 MIN                                                      
001051     ELSE                                                                 
001052       COMPUTE W-TIKLOCK-OLD-MM =                                         
001053       (((15 * 60) - (W-OLD-HH * 60)) + (54 - W-OLD-MM))                  
001054     END-IF                                                               
001055                                                                          
001056     .                                                                    
001057     EJECT                                                                
001058 S20D-BERAEKNA-NEW SECTION.                                               
001059                                                                          
001060     IF NEW-FM                                                            
001061       COMPUTE W-TIKLOCK-NEW-MM =                                         
001062       (((7 * 60) - (W-NEW-HH * 60)) + W-NEW-MM)                          
001063     ELSE                                                                 
001064       COMPUTE W-TIKLOCK-NEW-MM =                                         
001065       (((240 + (W-NEW-HH * 60)) - ((11 * 60) + 42))) + W-NEW-MM          
001066** 4.0 TIM = 240 MIN                                                      
001067     END-IF                                                               
001068     .                                                                    
