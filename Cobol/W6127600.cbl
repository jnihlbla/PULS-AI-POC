000001 ID DIVISION.                                                             
000002                                                                          
000003 PROGRAM-ID.     W6127600.                                                
000004 AUTHOR.         KJELL (JOHAN L).                                         
000005 DATE-WRITTEN.   2011-10-14 (1997-06-23).                                 
000006 DATE-COMPILED.                                                           
000007                                                                          
000008*                                                                         
000009*FUNKTION                                                                 
000010*   SKRIVER UT REFILL/AK FOLLOW-UP-RAPPORTER PER DC                       
000011*   FÖR DC:N SOM KÖR CLASSIC PULS OCH DÄRFÖR FÅR RAPPORTEN                
000012*   VIA EXPRESS DELIVERY ELLER D&P-MAIL/PRINTER                           
000013*   DESSUTOM FÅR LDC:ER OCH ANDRA SOM KÖR WEBB-PULS                       
000014*   MOTSVARANDE RAPPORT TILL FOLLOW-UP-MENYN                              
000015*                                                                         
000016*   PROGRAMMET ÄR EN DEL AV ETT TIDIGARE´W6127600 SOM                     
000017*   DELATS UPP I FLERA DELAR.                                             
000018*   ANDRA DELAR SKAPAR INDATAT (7F) OCH HANTERAR UTSKRIFT                 
000019*   TILL MANAGEMENT-FOLLOW-UP (7G)..                                      
000020*                                                                         
000021                                                                          
000022     SKIP3                                                                
000023 ENVIRONMENT DIVISION.                                                    
000024     SKIP2                                                                
000025 INPUT-OUTPUT SECTION.                                                    
000026                                                                          
000027 FILE-CONTROL.                                                            
000028     SKIP2                                                                
000029*          --- INFIL SORTERAD PER DC                                      
000030     SELECT W6127F                     ASSIGN TO W61276D1.                
000031                                                                          
000032*          --- UTLISTA VIA EXPRESS-DELIVERY                               
000033     SELECT W61276-001                 ASSIGN TO W61276D2.                
000034                                                                          
000035*          --- LISTA TILL D&P SKICAS VIA WZ01SEND                         
000036                                                                          
000037     EJECT                                                                
000038 DATA DIVISION.                                                           
000039     SKIP3                                                                
000040 FILE SECTION.                                                            
000041     SKIP3                                                                
000042 FD  W6127F                                                               
000043     RECORDING       F                                                    
000044     BLOCK CONTAINS  0.                                                   
000045                                                                          
000046*01  -COPY W6127F      -L.                                                
000047     SKIP3                                                                
000048 FD  W61276-001                                                           
000049     RECORDING       F                                                    
000050     BLOCK CONTAINS  0.                                                   
000051     SKIP2                                                                
000052 01  W61276-001-RAD              PIC X(121).                              
000053     EJECT                                                                
000054 WORKING-STORAGE SECTION.                                                 
000055                                                                          
000056 77  IDPGM                       PIC X(8)    VALUE 'W6127600'.            
000057 77  JA                          PIC X       VALUE 'J'.                   
000058 77  NEJ                         PIC X       VALUE 'N'.                   
000059                                                                          
000060 77  SUMMA-POST                  PIC X       VALUE '9'.                   
000061                                                                          
000062 77  KDRC-DISPLAY                PIC Z(5).                                
000063 77  ERROR-TEXT                  PIC X(80)   VALUE SPACE.                 
000064                                                                          
000065 01  FELTEXT.                                                             
000066     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
000067     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
000068                                                                          
000069 01  SPAR-IDDC                   PIC XX      VALUE SPACE.                 
000070 01  SPAR-FLWEBDC                PIC X       VALUE SPACE.                 
000071                                                                          
000072 01  SW-RAPPORT-VIA-DAP          PIC X.                                   
000073 01  SW-RAPPORT-SKRIVEN          PIC X       VALUE 'N'.                   
000074                                                                          
000075 01  WZ04-SEND-IDCOM             PIC S9(9)   COMP VALUE +0.               
000076                                                                          
000077 01  W6127F-EOF-SW               PIC X       VALUE 'N'.                   
000078     88  END-OF-W6127F                       VALUE 'J'.                   
000079                                                                          
000080     EJECT                                                                
000081 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
000082 01  FILLER REDEFINES DAGENS-DATUM.                                       
000083     03  DAGENS-DATUM-AAR        PIC 9(2).                                
000084     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
000085     03  DAGENS-DATUM-DAG        PIC 9(2).                                
000086                                                                          
000087 01  WS-YYMMDDHHMM.                                                       
000088     03 WS-YYMMDD                PIC  9(6).                               
000089     03 WS-TIME                  PIC  9(4).                               
000090                                                                          
000091 01  WS-HHMMSSTH.                                                         
000092     03 WS-HHMM                  PIC  9(4).                               
000093     03 WS-SSTH                  PIC  9(4).                               
000094                                                                          
000101     EJECT                                                                
000102 01  DYNAMISKA-SUBPROGRAM.                                                
000103*                                                                         
000104     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
000105     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
000106     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
000107     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
000108                                                                          
000109*    --- PARAMETRAR TILL ABEND                                            
000110 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
000111 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
000112 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
000113 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
000114 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
000115     EJECT                                                                
000116 01  FILLER                      PIC X(16)   VALUE 'WZ01SEND'.            
000117*01  -COPY WZ01SEND                                                       
000118     EJECT                                                                
000119*    --- PARAMETRAR TILL DATKORT                                          
000120*                                                                         
000121 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W61276'.              
000122     SKIP2                                                                
000123 01  DATUMKORT-ID                PIC X(6)    VALUE '000001'.              
000124     SKIP2                                                                
000125*01  -COPY WDATKORT                                                       
000126     EJECT                                                                
000127*    --- PARAMETRAR TILL POSTSUM                                          
000128*                                                                         
000129*01  -COPY W0005   -PRE  POSTSUM-                                         
000130     EJECT                                                                
000131*    --- PARAMETRAR TILL W612TIME                                         
000132*                                                                         
000133*01  -COPY W612TID     -PRE TIME-                                         
000134     EJECT                                                                
000137*01  -COPY WWDC99                                                         
000138                                                                          
000139     EJECT                                                                
000140 01  IN-AREA-START               PIC X(24)   VALUE                        
000141                                 'IN-AREA-START   '.                      
000142                                                                          
000143*01  AREA -COPY W6127F     -PRE IN-                                       
000144                                                                          
000145     EJECT                                                                
000146                                                                          
000147 01  HDR-AREA-START              PIC X(24)   VALUE                        
000148                                 'HDR-AREA-START  '.                      
000149                                                                          
000150 01  HDR-AREA.                                                            
000151*   03  -COPY WZ01REQU -PRE HDR-                                          
000152*   03  -COPY WZ04HDR                                                     
000153*                                                                         
000154                                                                          
000155 01  WEB-POST-AREA-START         PIC X(24)   VALUE                        
000156                                 'WEB-POST-AREA-START '.                  
000157                                                                          
000158 01  WEB-POST-AREA.                                                       
000159*    03 -COPY W612761 -PRE WEB-                                           
000160                                                                          
000161     EJECT                                                                
000162 01  W001-AREA-START             PIC X(24)   VALUE                        
000163                                 'W001-AREA-START  '.                     
000164                                                                          
000165 01  W001-HJALPAREOR.                                                     
000166*                                                                         
000167     03  W001-SKIP               PIC 9(3) COMP-3  VALUE 3.                
000168     03  W001-CC                 PIC X       VALUE SPACE.                 
000169     03  W001-ANTAL-RADER                                                 
000170                                 PIC 9(3)    VALUE 999.                   
000171     03  W001-MAX-RADER-PER-SIDA                                          
000172                                 PIC 9(3)    VALUE 42.                    
000173     03  W001-MAX-POSITIONER-PER-RAD                                      
000174                                 PIC 9(3)    VALUE 120.                   
000175     03  W001-LISTNR             PIC X(11)   VALUE 'W61276-001'.          
000176     03  W001-SIDRAKNARE         PIC S9(5)   COMP-3 VALUE ZERO.           
000177                                                                          
000178                                                                          
000179 01  W001-RAD.                                                            
000180     03  FILLER                  PIC X(121)  VALUE SPACE.                 
000181                                                                          
000182 01  W001-DAP-POST.                                                       
000183     03  FILLER                  PIC X(121)  VALUE SPACE.                 
000184                                                                          
000185     EJECT                                                                
000186 01  W001-RUBRIK1.                                                        
000187*                                                                         
000188     03  FILLER                  PIC X(3) VALUE SPACE.                    
000189     03  FILLER                  PIC X(23)                                
000190                                VALUE 'VOLVO CAR CUST. SERVICE'.          
000191     03  FILLER                  PIC X(2) VALUE SPACE.                    
000192     03  FILLER                  PIC X(10)                                
000193                                VALUE 'W61276-001'.                       
000194     03  FILLER                  PIC X(4) VALUE SPACE.                    
000195     03  FILLER                  PIC X(20)                                
000196                                VALUE 'WEEKLY AK FOLLOW-UP.'.             
000197     03  FILLER                  PIC X(10) VALUE SPACE.                   
000198     03  FILLER                  PIC X(3) VALUE 'DC '.                    
000199     03  RUBRIK-DC-1             PIC XX.                                  
000200     03  FILLER                  PIC X(4) VALUE SPACE.                    
000201     03  FILLER                  PIC X(6) VALUE 'WEEK: '.                 
000202     03  RUBRIK-VECKA            PIC 9(4).                                
000203     03  FILLER                  PIC X(8) VALUE SPACE.                    
000204     03  RUBRIK-DATUM            PIC XXBXXBXX.                            
000205     03  FILLER                  PIC X(3) VALUE SPACE.                    
000206     03  FILLER                  PIC X(4)                                 
000207                                 VALUE 'PAGE'.                            
000208     03  W001-SID                PIC Z(2)9.                               
000209     EJECT                                                                
000210 01  W001-RUBRIK2.                                                        
000211*                                                                         
000212     03  FILLER                  PIC X(20) VALUE SPACE.                   
000213     03  FILLER                  PIC X(16)                                
000214                                 VALUE 'TIME   IN   DAYS'.                
000215     03  FILLER                  PIC X(7)  VALUE SPACE.                   
000216     03  FILLER                  PIC X(6)  VALUE 'BINNED'.                
000217     03  FILLER                  PIC X(9)  VALUE SPACE.                   
000218     03  FILLER                  PIC X(4)  VALUE 'PRIO'.                  
000219     03  FILLER                  PIC X(7)  VALUE SPACE.                   
000220     03  FILLER                  PIC X(7)  VALUE 'BINNED'.                
000221     EJECT                                                                
000222 01  W001-RUBRIK3.                                                        
000223*                                                                         
000224     03  FILLER                  PIC X(20) VALUE SPACE.                   
000225     03  FILLER                  PIC X(3)  VALUE 'TOT'.                   
000226     03  FILLER                  PIC X(9)  VALUE SPACE.                   
000227     03  FILLER                  PIC X(4)  VALUE 'PRIO'.                  
000228     03  FILLER                  PIC X(8)   VALUE SPACE.                  
000229     03  FILLER                  PIC X(5)  VALUE 'LINES'.                 
000230     03  FILLER                  PIC X(8)  VALUE SPACE.                   
000231     03  FILLER                  PIC X(5)  VALUE 'LINES'.                 
000232     03  FILLER                  PIC X(8)  VALUE SPACE.                   
000233     03  FILLER                  PIC X(5)  VALUE 'VALUE'.                 
000234     EJECT                                                                
000235 01  W001-DETALJ.                                                         
000236     03 FILLER                     PIC X(3)    VALUE SPACE.               
000237     03 W001-KDREFTYP-TEXT         PIC X(9).                              
000238     03 FILLER                     PIC X       VALUE SPACE.               
000239     03 W001-KVDAGDEC-DAYS-BINNED  PIC Z(7)9.9.                           
000240     03 W001-DAYS-BINNED-X REDEFINES W001-KVDAGDEC-DAYS-BINNED            
000241                                   PIC X(10).                             
000242     03 FILLER                     PIC XXX     VALUE SPACE.               
000243     03 W001-KVDAGDEC-DAYS-PRIO    PIC Z(7)9.9.                           
000244     03 W001-DAYS-PRIO-X REDEFINES W001-KVDAGDEC-DAYS-PRIO                
000245                                   PIC X(10).                             
000246     03 FILLER                     PIC XXX     VALUE SPACE.               
000247     03 W001-KVANTAL-LINES-BINNED  PIC Z(9)9.                             
000248     03 FILLER                     PIC XXX     VALUE SPACE.               
000249     03 W001-KVANTAL-LINES-PRIO    PIC Z(9)9.                             
000250     03 FILLER                     PIC XXX     VALUE SPACE.               
000251     03 W001-SUARTNTO-BINNED       PIC Z(6)9.99.                          
000252                                                                          
000253     EJECT                                                                
000254 PROCEDURE DIVISION.                                                      
000255 MAIN SECTION.                                                            
000256                                                                          
000257     PERFORM A-INIT                                                       
000258     PERFORM S01-LAES-W6127F                                              
000259     PERFORM UNTIL END-OF-W6127F                                          
000260                                                                          
000261       IF IN-IDDC NOT = SPAR-IDDC                                         
000262         IF SW-RAPPORT-SKRIVEN = JA                                       
000263           PERFORM B-SLUT-DC                                              
000264         END-IF                                                           
000265         PERFORM B-NYTT-DC                                                
000266         PERFORM C-SKRIV-RUBRIKER                                         
000267       END-IF                                                             
000268                                                                          
000269*      -- OM INTE ALLT ÄR NOLLOR                                          
000270       IF IN-KVANTAL-LINES-AK > 0                                         
000271       OR IN-KVANTAL-LINES-BINNED > 0                                     
000272       OR IN-KVANTAL-LINES-PRIO > 0                                       
000273       OR IN-SUARTNTO-BINNED > 0                                          
000274         PERFORM D-SKRIV-DATA                                             
000275       END-IF                                                             
000276                                                                          
000277       PERFORM S01-LAES-W6127F                                            
000278     END-PERFORM                                                          
000279                                                                          
000280     IF SW-RAPPORT-SKRIVEN = JA                                           
000282       PERFORM B-SLUT-DC                                                  
000284     END-IF                                                               
000285                                                                          
000286     PERFORM Z-FINIT                                                      
000287     MOVE ZERO TO RETURN-CODE                                             
000288     GOBACK                                                               
000289     .                                                                    
000290     EJECT                                                                
000291 A-INIT SECTION.                                                          
000292                                                                          
000293     OPEN INPUT  W6127F                                                   
000294     OPEN OUTPUT W61276-001                                               
000295                                                                          
000296     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
000297                                                                          
000298     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
000299     MOVE D-AAR     TO  DAGENS-DATUM-AAR                                  
000300     MOVE D-MAANAD  TO  DAGENS-DATUM-MAANAD                               
000301     MOVE D-DAG     TO  DAGENS-DATUM-DAG                                  
000302                                                                          
000303*    TIME-STAMP FOR D&P HDR-IDLIST                                        
000304     ACCEPT WS-YYMMDD      FROM DATE                                      
000305     ACCEPT WS-HHMMSSTH    FROM TIME                                      
000306     MOVE   WS-HHMM        TO WS-TIME                                     
000307     .                                                                    
000308                                                                          
000309     EJECT                                                                
000310 B-SLUT-DC  SECTION.                                                      
000311                                                                          
000318     IF SW-RAPPORT-VIA-DAP = JA                                           
000319       PERFORM S50-SEND-CLOSE                                             
000320     END-IF                                                               
000321     .                                                                    
000322                                                                          
000362     EJECT                                                                
000363 B-NYTT-DC  SECTION.                                                      
000364                                                                          
000365     MOVE IN-IDDC    TO SPAR-IDDC                                         
000366     MOVE IN-FLWEBDC TO SPAR-FLWEBDC                                      
000367                                                                          
000368*    -- FÖR NÄRVARANDE FÅR BÅDA PACIFIC-DC:NA RAPPORT                     
000369*    -- VIA D&P-MAIL, MEDAN ÖVRIGA DC:N SOM KÖR CLASSIC                   
000370*    -- PULS FÅR DEN VIA SYSOUT TILL EXPRESS-DELIVERY.                    
000371*    -- ALLA LAGER SOM KÖR WEBB-PULS FÅR                                  
000372*    -- RAPPORTEN VIA D&P TILL "FOLLOW-UP" MENYN.                         
000373     MOVE SPAR-IDDC TO WS-IDDC                                            
000374     IF NDC-PACIFIC OR CDC-SE OR SPAR-FLWEBDC = JA                        
000375       MOVE JA  TO SW-RAPPORT-VIA-DAP                                     
000376     ELSE                                                                 
000377       MOVE NEJ TO SW-RAPPORT-VIA-DAP                                     
000378     END-IF                                                               
000379                                                                          
000380     IF SW-RAPPORT-VIA-DAP = JA                                           
000381       PERFORM S50-SEND-OPEN                                              
000382       PERFORM S15-SKAPA-DAP-HEADER                                       
000383     END-IF                                                               
000384                                                                          
000385*    -- RÄKNAREN ÖKAS MED 1 VID UTSKRIFT AV SIDRUBRIK                     
000386     MOVE 0 TO W001-SIDRAKNARE                                            
000393     .                                                                    
000394                                                                          
000395     EJECT                                                                
000396 C-SKRIV-RUBRIKER SECTION.                                                
000397                                                                          
000398     IF SPAR-FLWEBDC = NEJ                                                
000399       MOVE DAGENS-DATUM  TO RUBRIK-DATUM                                 
000400       MOVE IN-TIAAVV     TO RUBRIK-VECKA                                 
000401                                                                          
000402       ADD 1 TO W001-SIDRAKNARE                                           
000403       MOVE W001-SIDRAKNARE TO W001-SID                                   
000404       MOVE SPAR-IDDC TO RUBRIK-DC-1                                      
000405                                                                          
000406       MOVE 99 TO W001-SKIP                                               
000407       MOVE W001-RUBRIK1 TO W001-RAD                                      
000408       PERFORM S20-SKRIV-SKICKA-RAD                                       
000409                                                                          
000410       MOVE 2 TO W001-SKIP                                                
000411       MOVE W001-RUBRIK2 TO W001-RAD                                      
000412       PERFORM S20-SKRIV-SKICKA-RAD                                       
000413                                                                          
000414       MOVE 1 TO W001-SKIP                                                
000415       MOVE W001-RUBRIK3 TO W001-RAD                                      
000416       PERFORM S20-SKRIV-SKICKA-RAD                                       
000417                                                                          
000418       MOVE +4 TO W001-ANTAL-RADER                                        
000419       MOVE 2 TO W001-SKIP                                                
000420     END-IF                                                               
000421                                                                          
000422     MOVE JA TO SW-RAPPORT-SKRIVEN                                        
000423     .                                                                    
000424                                                                          
000425     EJECT                                                                
000426 D-SKRIV-DATA          SECTION.                                           
000427                                                                          
000428     IF IN-KDREFTYP = SUMMA-POST                                          
000429       IF SPAR-FLWEBDC = JA                                               
000430         PERFORM DC-SKRIV-SUMMAPOST                                       
000431       ELSE                                                               
000432         PERFORM DD-SKRIV-SUMMARAD                                        
000433       END-IF                                                             
000434     ELSE                                                                 
000435       IF SPAR-FLWEBDC = JA                                               
000436         PERFORM DA-SKRIV-DETALJPOST                                      
000437       ELSE                                                               
000438         PERFORM DB-SKRIV-DETALJRAD                                       
000439       END-IF                                                             
000440     END-IF                                                               
000441                                                                          
000447     .                                                                    
000448                                                                          
000449     EJECT                                                                
000450 DA-SKRIV-DETALJPOST    SECTION.                                          
000451                                                                          
000452     MOVE '1         '              TO WEB-IDAFPRCD                       
000453     MOVE SPAR-IDDC                 TO WEB-IDDC                           
000454     MOVE IN-KVDAGDEC-DAYS-BINNED   TO WEB-TIME-TOT                       
000455     MOVE IN-KVDAGDEC-DAYS-PRIO     TO WEB-DAYS-PRIO                      
000456     MOVE IN-KVANTAL-LINES-AK       TO WEB-AK-LINES                       
000457     MOVE IN-KVANTAL-LINES-BINNED   TO WEB-BINNED-LINES                   
000458     MOVE IN-KVANTAL-LINES-PRIO     TO WEB-BINNED-PRIO                    
000459     MOVE IN-TIAAVV                 TO WEB-TIAAVV                         
000460     MOVE IN-KDREFTYP               TO WEB-KDREFTYP                       
000461*--- MOVE IN-SUARTNTO-BINNED        TO WEB-SUARTNTO-BINNED                
000462                                                                          
000463     PERFORM S50-PUT-WEB-POST                                             
000464     .                                                                    
000465                                                                          
000466     EJECT                                                                
000467 DB-SKRIV-DETALJRAD     SECTION.                                          
000468                                                                          
000469     EVALUATE IN-KDREFTYP                                                 
000470       WHEN 'A'   MOVE 'AIR'        TO W001-KDREFTYP-TEXT                 
000471       WHEN 'B'   MOVE 'BOAT'       TO W001-KDREFTYP-TEXT                 
000472       WHEN 'T'   MOVE 'TRANSFERS'  TO W001-KDREFTYP-TEXT                 
000473       WHEN 'Z'   MOVE 'OTHERS'     TO W001-KDREFTYP-TEXT                 
000474     END-EVALUATE                                                         
000475                                                                          
000476*--- MOVE IN-KVANTAL-LINES-AK       TO W001-KVANTAL-LINES-AK              
000477     MOVE IN-KVDAGDEC-DAYS-BINNED   TO W001-KVDAGDEC-DAYS-BINNED          
000478     MOVE IN-KVDAGDEC-DAYS-PRIO     TO W001-KVDAGDEC-DAYS-PRIO            
000479     MOVE IN-KVANTAL-LINES-BINNED   TO W001-KVANTAL-LINES-BINNED          
000480     MOVE IN-KVANTAL-LINES-PRIO     TO W001-KVANTAL-LINES-PRIO            
000481     MOVE IN-SUARTNTO-BINNED        TO W001-SUARTNTO-BINNED               
000482                                                                          
000483     MOVE W001-DETALJ  TO  W001-RAD                                       
000484     PERFORM S20-SKRIV-SKICKA-RAD                                         
000485                                                                          
000486     MOVE 1 TO W001-SKIP                                                  
000487     .                                                                    
000488                                                                          
000489     EJECT                                                                
000490 DC-SKRIV-SUMMAPOST  SECTION.                                             
000491                                                                          
000492     MOVE '2         '              TO WEB-IDAFPRCD                       
000493     MOVE SPAR-IDDC                 TO WEB-IDDC                           
000494     MOVE IN-KVDAGDEC-DAYS-BINNED   TO WEB-TIME-TOT                       
000495     MOVE IN-KVDAGDEC-DAYS-PRIO     TO WEB-DAYS-PRIO                      
000496     MOVE IN-KVANTAL-LINES-AK       TO WEB-AK-LINES                       
000497     MOVE IN-KVANTAL-LINES-BINNED   TO WEB-BINNED-LINES                   
000498     MOVE IN-KVANTAL-LINES-PRIO     TO WEB-BINNED-PRIO                    
000499     MOVE IN-TIAAVV                 TO WEB-TIAAVV                         
000500     MOVE SUMMA-POST                TO WEB-KDREFTYP                       
000502*--- MOVE IN-SUARTNTO-BINNED        TO WEB-SUARTNTO-BINNED                
000503                                                                          
000504     PERFORM S50-PUT-WEB-POST                                             
000505     .                                                                    
000506                                                                          
000507     EJECT                                                                
000508 DD-SKRIV-SUMMARAD  SECTION.                                              
000509                                                                          
000510     MOVE 'SUMMARY'                 TO W001-KDREFTYP-TEXT                 
000511                                                                          
000512*    MOVE SPACE                     TO W001-DAYS-BINNED-X                 
000513     MOVE IN-KVDAGDEC-DAYS-BINNED   TO W001-KVDAGDEC-DAYS-BINNED          
000514*    MOVE SPACE                     TO W001-DAYS-PRIO-X                   
000515     MOVE IN-KVDAGDEC-DAYS-PRIO     TO W001-KVDAGDEC-DAYS-PRIO            
000516                                                                          
000517     MOVE IN-KVANTAL-LINES-BINNED   TO W001-KVANTAL-LINES-BINNED          
000518     MOVE IN-KVANTAL-LINES-PRIO     TO W001-KVANTAL-LINES-PRIO            
000519     MOVE IN-SUARTNTO-BINNED        TO W001-SUARTNTO-BINNED               
000520                                                                          
000521     MOVE 2 TO W001-SKIP                                                  
000522     MOVE W001-DETALJ  TO  W001-RAD                                       
000523     PERFORM S20-SKRIV-SKICKA-RAD                                         
000524     .                                                                    
000525                                                                          
000526                                                                          
000527     EJECT                                                                
000528 Z-FINIT SECTION.                                                         
000529                                                                          
000530     CLOSE W6127F                                                         
000531     CLOSE W61276-001                                                     
000532     SKIP2                                                                
000533     MOVE 'S' TO POSTSUM-OPKOD                                            
000534     CALL POSTSUM USING POSTSUM-PARM                                      
000535     .                                                                    
000536     EJECT                                                                
000537 S01-LAES-W6127F  SECTION.                                                
000538     READ W6127F INTO IN-AREA                                             
000539     AT END                                                               
000540        MOVE HIGH-VALUE TO IN-AREA                                        
000541        SET END-OF-W6127F TO TRUE                                         
000542                                                                          
000543     NOT AT END                                                           
000544        MOVE 'W6127F'   TO POSTSUM-FDNAMN                                 
000545        MOVE 'W61276D1' TO POSTSUM-DDNAMN2                                
000546        MOVE IN-IDDC    TO POSTSUM-TRANSTYP                               
000547        CALL POSTSUM USING POSTSUM-PARM                                   
000548     END-READ                                                             
000549                                                                          
000550     .                                                                    
000551                                                                          
000552     EJECT                                                                
000553 S15-SKAPA-DAP-HEADER      SECTION.                                       
000554                                                                          
000555     MOVE SEND-IDCOM TO WZ04-SEND-IDCOM                                   
000556     MOVE 1                          TO HDR-REQU-IDMSGVER                 
000557     MOVE 'E'                        TO HDR-REQU-KDPGMACT                 
000558     MOVE IDPGM                      TO HDR-REQU-IDUSER                   
000559                                                                          
000560     MOVE SPACE                      TO HDR-IDOUTREC                      
000561                                                                          
000562*    -- OUTTYPE OCH OUTREC ÄR OLIKA FÖR WEBB-DC OCH NDC-JP/AU             
000563     IF NDC-PACIFIC OR CDC-SE                                             
000564       MOVE SPACE                    TO HDR-IDOUTTYPE                     
000565       MOVE 'W61276-0'               TO HDR-IDOUTTYPE(1:8)                
000566       MOVE SPAR-IDDC                TO HDR-IDOUTTYPE(9:2)                
000567                                                                          
000568       MOVE 'W61276'                 TO HDR-IDOUTREC                      
000569     END-IF                                                               
000570     IF SPAR-FLWEBDC = JA                                                 
000571       MOVE 'REFILL-AK-WEEK'         TO HDR-IDOUTTYPE                     
000572                                                                          
000573       MOVE SPACE                    TO HDR-IDOUTREC                      
000574       MOVE SPAR-IDDC                TO HDR-IDOUTREC(1:2)                 
000575       MOVE 'W61276'                 TO HDR-IDOUTREC(3:8)                 
000576     END-IF                                                               
000577                                                                          
000578     MOVE WS-YYMMDDHHMM              TO HDR-IDLIST                        
000579                                                                          
000580     PERFORM S50-PUT-HEADER                                               
000581     .                                                                    
000582                                                                          
000583     EJECT                                                                
000584 S20-SKRIV-SKICKA-RAD   SECTION.                                          
000585                                                                          
000586*    -- TRADITIONELL RAPPORT VIA PRINTER ELLER D&P                        
000587     IF SW-RAPPORT-VIA-DAP = JA                                           
000588       IF W001-SKIP = 99                                                  
000589*        -- PAGE SKIP - BYT TILL ASA STYRTECKEN                           
000590         MOVE '1'        TO W001-RAD(1:1)                                 
000591         MOVE 1 TO W001-SKIP                                              
000592       END-IF                                                             
000593*      -- EMULERA ÖVRIGA SKIPS MED BLANKA RADER                           
000594       IF W001-SKIP = 3                                                   
000595         MOVE SPACE      TO W001-DAP-POST                                 
000596         PERFORM S50-PUT-REPORT-LINE                                      
000597       END-IF                                                             
000598       IF W001-SKIP = 2 OR 3                                              
000599         MOVE SPACE      TO W001-DAP-POST                                 
000600         PERFORM S50-PUT-REPORT-LINE                                      
000601       END-IF                                                             
000602       MOVE W001-RAD TO W001-DAP-POST                                     
000603       PERFORM S50-PUT-REPORT-LINE                                        
000604                                                                          
000605     ELSE                                                                 
000606       IF W001-SKIP = 99                                                  
000607         WRITE W61276-001-RAD FROM W001-RAD AFTER PAGE                    
000608         MOVE 1 TO W001-SKIP                                              
000609       ELSE                                                               
000610         WRITE W61276-001-RAD FROM W001-RAD AFTER W001-SKIP               
000611       END-IF                                                             
000612     END-IF                                                               
000613     .                                                                    
000614                                                                          
000615     EJECT                                                                
000616 S50-SEND-OPEN SECTION.                                                   
000617                                                                          
000618     MOVE 'CARPARTS.DAP.DISTRDOC'         TO SEND-ADDISPABS               
000619     MOVE 'OPEN'                          TO SEND-KDFUNC                  
000620     CALL WZ01SEND USING SEND-CONTROL-AREA                                
000621                         SEND-OPEN-AREA                                   
000622     IF SEND-KDRC > ZERO                                                  
000623       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
000624       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
000625       DELIMITED BY SIZE INTO ERROR-TEXT                                  
000626       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
000627     END-IF                                                               
000628     .                                                                    
000629     SKIP3                                                                
000630 S50-PUT-HEADER SECTION.                                                  
000631     MOVE 'PUT'                           TO SEND-KDFUNC                  
000632     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
000633     MOVE LENGTH OF HDR-AREA              TO SEND-KVDLEN                  
000634     CALL WZ01SEND USING SEND-CONTROL-AREA                                
000635                         SEND-KVDLEN                                      
000636                         HDR-AREA                                         
000637     IF SEND-KDRC > ZERO                                                  
000638       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
000639       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
000640       DELIMITED BY SIZE INTO ERROR-TEXT                                  
000641       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
000642     END-IF                                                               
000643     .                                                                    
000644                                                                          
000645     EJECT                                                                
000646 S50-PUT-REPORT-LINE       SECTION.                                       
000647                                                                          
000648     MOVE 'PUT'                           TO SEND-KDFUNC                  
000649     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
000650     MOVE LENGTH OF W001-DAP-POST         TO SEND-KVDLEN                  
000651     CALL WZ01SEND USING SEND-CONTROL-AREA                                
000652                         SEND-KVDLEN                                      
000653                         W001-DAP-POST                                    
000654     IF SEND-KDRC > ZERO                                                  
000655       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
000656       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
000657       DELIMITED BY SIZE INTO ERROR-TEXT                                  
000658       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
000659     END-IF                                                               
000660                                                                          
000661     MOVE 'DAP'      TO POSTSUM-FDNAMN                                    
000662     MOVE 'DAP'      TO POSTSUM-DDNAMN2                                   
000663     MOVE IN-IDDC    TO POSTSUM-TRANSTYP                                  
000664     CALL POSTSUM USING POSTSUM-PARM                                      
000665     .                                                                    
000666                                                                          
000667 S50-PUT-WEB-POST          SECTION.                                       
000668                                                                          
000669     MOVE 'PUT'                           TO SEND-KDFUNC                  
000670     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
000671     MOVE LENGTH OF WEB-POST-AREA         TO SEND-KVDLEN                  
000672     CALL WZ01SEND USING SEND-CONTROL-AREA                                
000673                         SEND-KVDLEN                                      
000674                         WEB-POST-AREA                                    
000675     IF SEND-KDRC > ZERO                                                  
000676       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
000677       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
000678       DELIMITED BY SIZE INTO ERROR-TEXT                                  
000679       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
000680     END-IF                                                               
000681                                                                          
000682     MOVE 'WEB'      TO POSTSUM-FDNAMN                                    
000683     MOVE 'WEB'      TO POSTSUM-DDNAMN2                                   
000684     MOVE IN-IDDC    TO POSTSUM-TRANSTYP                                  
000685     CALL POSTSUM USING POSTSUM-PARM                                      
000686     .                                                                    
000687                                                                          
000688 S50-SEND-CLOSE SECTION.                                                  
000689     MOVE 'CLOSE'                         TO SEND-KDFUNC                  
000690     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
000691     CALL WZ01SEND USING SEND-CONTROL-AREA                                
000692     .                                                                    
000693                                                                          
000694 S99-ABEND SECTION.                                                       
000695                                                                          
000696     SKIP2                                                                
000697     MOVE 'S' TO POSTSUM-OPKOD                                            
000698     CALL POSTSUM USING POSTSUM-PARM                                      
000699     CALL ABEND USING RKOD-ABEND                                          
000700     .                                                                    
