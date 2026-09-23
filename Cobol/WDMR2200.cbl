000001 ID DIVISION.                                                             
000002     SKIP2                                                                
000003 PROGRAM-ID.     WDMR2200.                                                
000004*AUTHOR.         KJELL ANDRÉ.                                             
000005*DATE-WRITTEN.   08/11/21.                                                
000006                                                                          
000007*    FUNKTION:                                                            
000008*        PLOCKA UT DATA FRÅN ETT COBOL-PROGRAM. DATAT ANVÄNDS FÖR         
000009*        ATT SKAPA FÖDDATA TILL DMR.                                      
000010*        KOPIA AV WDMR2000 OCH OMGJORT FÖR MULTIPLA PROGRAM               
000011*        OCH INPUT-FORMAT ENLIGT FMEMBLST                                 
000012*                                                                         
000013*    ABENDKODER:                                                          
000014*        U0016 -  . . . .                                                 
000015*        U1000 -  . . . .                                                 
000016*                                                                         
000017                                                                          
000018     SKIP3                                                                
000019 ENVIRONMENT DIVISION.                                                    
000020     SKIP2                                                                
000021 INPUT-OUTPUT SECTION.                                                    
000022                                                                          
000023 FILE-CONTROL.                                                            
000024     SKIP2                                                                
000025*          --- ETT ANTAL COBOLPROGRAM                                     
000026     SELECT COBOLIN                    ASSIGN TO WDMR22D1.                
000027*          --- DATA FÖR ATT SKAPA FÖDDATA TILL DMR                        
000028     SELECT COBOLOUT                   ASSIGN TO WDMR22D2.                
000029     EJECT                                                                
000030 DATA DIVISION.                                                           
000031     SKIP3                                                                
000032 FILE SECTION.                                                            
000033     SKIP3                                                                
000034 FD  COBOLIN                                                              
000035     RECORDING       F                                                    
000036     BLOCK CONTAINS  0.                                                   
000037     EJECT                                                                
000038 01  FILLER          PIC X(132).                                          
000039     SKIP3                                                                
000040 FD  COBOLOUT                                                             
000041     RECORDING       F                                                    
000042     BLOCK CONTAINS  0.                                                   
000043     EJECT                                                                
000044 01  UT-POST         PIC X(136).                                          
000045     EJECT                                                                
000046 WORKING-STORAGE SECTION.                                                 
000047     SKIP2                                                                
000048                                                                          
000049 77  IDPGM                       PIC X(8)    VALUE 'WDMR2200'.            
000050 77  JA                          PIC X       VALUE 'J'.                   
000051 77  NEJ                         PIC X       VALUE 'N'.                   
000052     SKIP2                                                                
000053 77  GEMENER                     PIC X(29)                                
000054                value 'abcdefghijklmnopqrstuvwxyzåäö'.                    
000055 77  VERSALER                    PIC X(29)                                
000056                VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZÅÄÖ'.                    
000057 01  WS-SECTION                  PIC X       VALUE 'N'.                   
000058 01  END-OF-OPEN-SATS            PIC X       VALUE 'N'.                   
000059                                                                          
000060 77  COBOLIN-EOF-SW              PIC X       VALUE 'N'.                   
000061     88  END-OF-COBOLIN                      VALUE 'J'.                   
000062 01  ORD-TYP                     PIC X       VALUE SPACE.                 
000063 01  RAD.                                                                 
000064     03  FILLER                  PIC X(3).                                
000065     03  END-COPY                PIC X(10).                               
000066     03  FILLER                  PIC X(11).                               
000067     03  LENGTH-X                PIC X(8).                                
000068     03  FILLER                  PIC X(5).                                
000069     03  OLD-LENGTH-X            PIC X(12).                               
000070     03  FILLER                  PIC X(17).                               
000071 01  RADX2                       PIC X(2).                                
000072 01  RADX20                      PIC X(20).                               
000073                                                                          
000074 01  ORDEN.                                                               
000075     03 ORD1                     PIC X(20)   VALUE SPACE.                 
000076     03 ORD2                     PIC X(20)   VALUE SPACE.                 
000077     03 ORD3                     PIC X(20)   VALUE SPACE.                 
000078     03 ORD4                     PIC X(20)   VALUE SPACE.                 
000079     03 ORD5                     PIC X(20)   VALUE SPACE.                 
000080     03 ORD6                     PIC X(20)   VALUE SPACE.                 
000081     03 ORD7                     PIC X(20)   VALUE SPACE.                 
000082     03 ORD8                     PIC X(20)   VALUE SPACE.                 
000083 01  ORD-TAB REDEFINES ORDEN.                                             
000084     03 ORD OCCURS 8             PIC X(20).                               
000085                                                                          
000086 01  OIX                         PIC S9(9)   BINARY.                      
000087                                                                          
000088 01  ORDX4                       PIC X(4)    VALUE SPACE.                 
000089                                                                          
000090     EJECT                                                                
000091 01  FILLER                      PIC X(16)   VALUE                        
000092                                            'IN-AREA-START'.              
000093 01  IN-AREA.                                                             
000094     03  FILLER                  PIC X.                                   
000095     03  IN-RADMARKERING         PIC X(3).                                
000096     03  FILLER                  PIC X(32).                               
000097     03  IN-RAD                  PIC X(66).                               
000098     03  FILLER                  PIC X(30).                               
000099                                                                          
000100 01  IN-AREA-2 REDEFINES IN-AREA.                                         
000101     03  FILLER                  PIC X(63).                               
000102     03  IN-NAMNMARKERING        PIC X(13).                               
000103     03  FILLER                  PIC X(15).                               
000104     03  IN-PGMNAMN              PIC X(8).                                
000105     03  FILLER                  PIC X(33).                               
000106     EJECT                                                                
000107 01  FILLER                      PIC X(16)   VALUE                        
000108                                            'UT-AREA-START'.              
000109 01  UT-AREA.                                                             
000110     03  UT-PGMNAMN              PIC X(8).                                
000111     03  UT-TYP                  PIC X(8)    VALUE SPACE.                 
000112     03  UT-DATA                 PIC X(120)  VALUE SPACE.                 
000113     03  UT-AREA2  REDEFINES UT-DATA.                                     
000114       05  UT-DATA-ORD1            PIC X(20).                             
000115       05  UT-DATA-ORD2            PIC X(20).                             
000116       05  UT-DATA-ORD3            PIC X(20).                             
000117       05  UT-DATA-ORD4            PIC X(20).                             
000118       05  UT-DATA-ORD5            PIC X(20).                             
000119       05  UT-DATA-ORD6            PIC X(20).                             
000120     SKIP2                                                                
000121 77  REM-TYP                     PIC X(8)    VALUE 'REM'.                 
000122 77  SEL-TYP                     PIC X(8)    VALUE 'SEL'.                 
000123 77  FD-TYP                      PIC X(8)    VALUE 'FD'.                  
000124 77  SD-TYP                      PIC X(8)    VALUE 'SD'.                  
000125 77  FILCTX-TYP                  PIC X(8)    VALUE 'FILCTX'.              
000126 77  OPNIN-TYP                   PIC X(8)    VALUE 'OPNIN'.               
000127 77  OPNOUT-TYP                  PIC X(8)    VALUE 'OPNOUT'.              
000128 77  CALL-TYP                    PIC X(8)    VALUE 'CALL'.                
000129 77  CTX-TYP                     PIC X(8)    VALUE 'CTX'.                 
000130 77  DB2-TYP                     PIC X(8)    VALUE 'DB2'.                 
000131 77  CTXMEM-TYP                  PIC X(8)    VALUE 'CTXMEM'.              
000132 77  USNG-TYP                    PIC X(8)    VALUE 'USING'.               
000133 77  GIV-TYP                     PIC X(8)    VALUE 'GIVING'.              
000134 77  W-END-COPY                  PIC X(10)   VALUE ' END COPY '.          
000135 77  W-LENGTH-X                  PIC X(8)    VALUE ' LENGTH='.            
000136 77  W-OLD-LENGTH-X              PIC X(12)   VALUE ' OLD LENGTH='.        
000139 77  SOURCE-RAD                  PIC X(3)    VALUE 'Rec'.                 
000140 77  SOURCE-RAD2                 PIC X(3)    VALUE 'REC'.                 
000141 77  SID-RUBRIK                  PIC X(3)    VALUE 'IBM'.                 
000142     EJECT                                                                
000143 01  DYNAMISKA-SUBPROGRAM.                                                
000144*                                                                         
000145     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
000146     03  WCOBORD                 PIC X(8)    VALUE 'WCOBORD'.             
000147     SKIP2                                                                
000148*    --- PARAMETRAR TILL ABEND                                            
000149                                                                          
000150 77  RKOD                        PIC S9(4)   COMP VALUE +0.               
000151 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
000152 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
000153     SKIP2                                                                
000154 01  FELTEXT.                                                             
000155     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
000156     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
000157     EJECT                                                                
000158 PROCEDURE DIVISION.                                                      
000159     SKIP2                                                                
000160     PERFORM A-INIT                                                       
000161     PERFORM S01-LAES-COBOLIN                                             
000162     PERFORM S02-LAES-TILL-PGM-START                                      
000163     PERFORM UNTIL END-OF-COBOLIN                                         
000164       MOVE NEJ TO WS-SECTION                                             
000165       PERFORM UNTIL END-OF-COBOLIN                                       
000166         OR (IN-RADMARKERING NOT = SOURCE-RAD AND SOURCE-RAD2)            
000167                                                                          
000168         IF RADX20 NOT = '*' AND RADX2 NOT = '*-'                         
000169           INSPECT RAD CONVERTING GEMENER TO VERSALER                     
000170           CALL WCOBORD USING ORD-TYP RAD                                 
000171                     ORD1 ORD2 ORD3 ORD4 ORD5 ORD6 ORD7 ORD8              
000172                                                                          
000173           IF ORD-TYP NOT = SPACE                                         
000174             EVALUATE TRUE                                                
000175                                                                          
000176               WHEN ORD-TYP = 'S'                                         
000177                 IF RADX2 NOT = '*' AND '**' AND WS-SECTION = NEJ         
000178                   PERFORM B-ANALYSERA-SELECT                             
000179                 END-IF                                                   
000180                 PERFORM S01-LAES-COBOLIN                                 
000181                                                                          
000182               WHEN ORD-TYP = 'F'                                         
000183                 IF RADX2 NOT = '*' AND '**'                              
000184                   PERFORM C-ANALYSERA-FD                                 
000185                 END-IF                                                   
000186                 PERFORM S01-LAES-COBOLIN                                 
000187                                                                          
000188               WHEN ORD-TYP = 'A'                                         
000189                 IF RADX2 NOT = '*' AND '**'                              
000190                   PERFORM D-ANALYSERA-SD                                 
000191                 END-IF                                                   
000192                 PERFORM S01-LAES-COBOLIN                                 
000193                                                                          
000194               WHEN ORD-TYP = 'C'                                         
000195                 IF ORD1 NOT = '*++INCLUDE' AND '++INCLUDE'               
000196                   PERFORM E-ANALYSERA-COPY                               
000197                 END-IF                                                   
000198                 PERFORM S01-LAES-COBOLIN                                 
000199                                                                          
000200               WHEN ORD-TYP = 'W'                                         
000201                 IF RADX2 NOT = '*' AND '**'                              
000202                   MOVE JA TO WS-SECTION                                  
000203                 END-IF                                                   
000204                 PERFORM S01-LAES-COBOLIN                                 
000205                                                                          
000206               WHEN ORD-TYP = 'I' OR 'O'                                  
000207                 MOVE ZERO TO TALLY                                       
000208                 INSPECT RAD TALLYING TALLY FOR ALL QUOTES                
000209                 IF RADX2 NOT = '*' AND '**' AND TALLY = ZERO             
000210                                                                          
000211                   IF ORD2 = 'PROCEDURE' OR ORD3 = 'PROCEDURE'            
000212                   OR ORD4 = 'PROCEDURE' OR ORD5 = 'PROCEDURE'            
000213                     PERFORM S01-LAES-COBOLIN                             
000214                   ELSE                                                   
000215                     PERFORM F-ANALYSERA-OPEN                             
000216                   END-IF                                                 
000217                                                                          
000218                 ELSE                                                     
000219                   PERFORM S01-LAES-COBOLIN                               
000220                 END-IF                                                   
000221                                                                          
000222               WHEN ORD-TYP = 'P'                                         
000223                 MOVE ZERO TO TALLY                                       
000224                 INSPECT RAD TALLYING TALLY FOR ALL QUOTES                
000225                 IF RADX2 NOT = '*' AND '**' AND TALLY = 0                
000226                   PERFORM G-ANALYSERA-CALL                               
000227                 END-IF                                                   
000228                 PERFORM S01-LAES-COBOLIN                                 
000229                                                                          
000230               WHEN ORD-TYP = 'D'                                         
000231                 IF RADX2 NOT = '*' AND '**'                              
000232                   PERFORM H-ANALYSERA-DB2                                
000233                 END-IF                                                   
000234                 PERFORM S01-LAES-COBOLIN                                 
000235                                                                          
000236               WHEN ORD-TYP = 'U'                                         
000237                 IF RADX2 NOT = '*' AND '**'                              
000238                   PERFORM I-ANALYSERA-USING                              
000239                 END-IF                                                   
000240                 PERFORM S01-LAES-COBOLIN                                 
000241                                                                          
000242               WHEN ORD-TYP = 'G'                                         
000243                 IF RADX2 NOT = '*' AND '**'                              
000244                   PERFORM J-ANALYSERA-GIVING                             
000245                 END-IF                                                   
000246                 PERFORM S01-LAES-COBOLIN                                 
000247                                                                          
000248               WHEN OTHER                                                 
000249                 DISPLAY 'Unknown type of line: ' ORD-TYP                 
000250                 PERFORM S99-ABEND                                        
000251             END-EVALUATE                                                 
000252                                                                          
000253           ELSE                                                           
000254             IF WS-SECTION = NEJ                                          
000255               IF END-COPY = W-END-COPY AND LENGTH-X = W-LENGTH-X         
000256                   AND OLD-LENGTH-X = W-OLD-LENGTH-X                      
000257                 MOVE CTXMEM-TYP TO UT-TYP                                
000258                 MOVE SPACE TO UT-DATA                                    
000259                 PERFORM S11-SKRIV-COBOLOUT                               
000260               END-IF                                                     
000261             END-IF                                                       
000262             PERFORM S01-LAES-COBOLIN                                     
000263           END-IF                                                         
000264         ELSE                                                             
000265           PERFORM S01-LAES-COBOLIN                                       
000266         END-IF                                                           
000267       END-PERFORM                                                        
000268        PERFORM S02-LAES-TILL-PGM-START                                   
000269     END-PERFORM                                                          
000270                                                                          
000271     PERFORM Z-FINIT                                                      
000272                                                                          
000273     MOVE RKOD TO RETURN-CODE                                             
000274     GOBACK                                                               
000275     .                                                                    
000276     EJECT                                                                
000277 A-INIT SECTION.                                                          
000278     SKIP2                                                                
000279     OPEN INPUT  COBOLIN                                                  
000280     OPEN OUTPUT COBOLOUT                                                 
000281     MOVE ZERO TO RKOD                                                    
000282     .                                                                    
000283     EJECT                                                                
000284 B-ANALYSERA-SELECT  SECTION.                                             
000285     SKIP2                                                                
000286     MOVE SEL-TYP TO UT-TYP                                               
000287     MOVE SPACE TO UT-DATA                                                
000288     MOVE ORD2 TO UT-DATA-ORD1                                            
000289     IF ORD4 = 'TO'                                                       
000290       MOVE ORD5 TO UT-DATA-ORD2                                          
000291     ELSE                                                                 
000292       MOVE ORD4 TO UT-DATA-ORD2                                          
000293     END-IF                                                               
000294     PERFORM S11-SKRIV-COBOLOUT                                           
000295     .                                                                    
000296     EJECT                                                                
000297 C-ANALYSERA-FD  SECTION.                                                 
000298     SKIP2                                                                
000299     MOVE FD-TYP TO UT-TYP                                                
000300     MOVE SPACE TO UT-DATA                                                
000301     MOVE ORD2 TO UT-DATA-ORD1                                            
000302     PERFORM S11-SKRIV-COBOLOUT                                           
000303     .                                                                    
000304     EJECT                                                                
000305 D-ANALYSERA-SD  SECTION.                                                 
000306     SKIP2                                                                
000307     MOVE SD-TYP TO UT-TYP                                                
000308     MOVE SPACE TO UT-DATA                                                
000309     MOVE ORD2 TO UT-DATA-ORD1                                            
000310     PERFORM S11-SKRIV-COBOLOUT                                           
000311     .                                                                    
000312     EJECT                                                                
000313 E-ANALYSERA-COPY  SECTION.                                               
000314     SKIP2                                                                
000315     IF WS-SECTION = JA                                                   
000316       MOVE CTX-TYP TO UT-TYP                                             
000317     ELSE                                                                 
000318       MOVE FILCTX-TYP TO UT-TYP                                          
000319     END-IF                                                               
000320     MOVE SPACE TO UT-DATA                                                
000321     EVALUATE TRUE                                                        
000322       WHEN ORD1 = '-COPY'                                                
000323         MOVE ORD2 TO UT-DATA-ORD1                                        
000324         MOVE ORD4 TO UT-DATA-ORD2                                        
000325                                                                          
000326       WHEN ORD2 = '-COPY'                                                
000327         MOVE ORD3 TO UT-DATA-ORD1                                        
000328         MOVE ORD5 TO UT-DATA-ORD2                                        
000329                                                                          
000330       WHEN ORD3 = '-COPY'                                                
000331         MOVE ORD4 TO UT-DATA-ORD1                                        
000332         MOVE ORD6 TO UT-DATA-ORD2                                        
000333                                                                          
000334       WHEN ORD4 = '-COPY'                                                
000335         MOVE ORD5 TO UT-DATA-ORD1                                        
000336         MOVE ORD7 TO UT-DATA-ORD2                                        
000337                                                                          
000338       WHEN ORD5 = '-COPY'                                                
000339         MOVE ORD6 TO UT-DATA-ORD1                                        
000340         MOVE ORD8 TO UT-DATA-ORD2                                        
000341                                                                          
000342       WHEN ORD6 = '-COPY'                                                
000343         MOVE ORD7  TO UT-DATA-ORD1                                       
000344         MOVE SPACE TO UT-DATA-ORD2                                       
000345                                                                          
000346       WHEN ORD7 = '-COPY'                                                
000347         MOVE ORD8  TO UT-DATA-ORD1                                       
000348         MOVE SPACE TO UT-DATA-ORD2                                       
000349                                                                          
000350       WHEN OTHER                                                         
000351         MOVE 'UNKNOWN' TO UT-DATA-ORD1                                   
000352         MOVE SPACE     TO UT-DATA-ORD2                                   
000353     END-EVALUATE                                                         
000354     PERFORM S11-SKRIV-COBOLOUT                                           
000355     .                                                                    
000356     EJECT                                                                
000357 F-ANALYSERA-OPEN  SECTION.                                               
000358     SKIP2                                                                
000359     MOVE SPACE TO UT-DATA                                                
000360     PERFORM FA-ANALYSERA-OPEN-RAD                                        
000361                                                                          
000362     PERFORM S01-LAES-COBOLIN                                             
000363     INSPECT RAD CONVERTING GEMENER TO VERSALER                           
000364     MOVE NEJ TO END-OF-OPEN-SATS                                         
000365     PERFORM UNTIL END-OF-COBOLIN OR RAD = SPACE                          
000366             OR END-OF-OPEN-SATS = JA                                     
000367       IF RADX2 NOT = '* ' AND '**' AND '*-'                              
000368         MOVE 'U' TO ORD-TYP                                              
000369         CALL WCOBORD USING ORD-TYP RAD                                   
000370            ORD1 ORD2 ORD3 ORD4 ORD5 ORD6 ORD7 ORD8                       
000371         MOVE ORD1 TO ORDX4                                               
000372         EVALUATE TRUE                                                    
000373           WHEN ORD-TYP NOT = SPACE                                       
000374             MOVE JA TO END-OF-OPEN-SATS                                  
000375                                                                          
000376           WHEN ORDX4 = 'SKIP'                                            
000377             MOVE JA TO END-OF-OPEN-SATS                                  
000378                                                                          
000379           WHEN ORD1 = '.' OR 'EJECT' OR 'PERFORM'                        
000380               OR 'IF' OR 'ELSE' OR 'END-IF'                              
000381               OR 'READ' OR 'WRITE' OR 'MOVE'                             
000382               OR 'PERFORM' OR 'END-PERFORM'                              
000383               OR 'EVALUATE' OR 'END-EVALUATE'                            
000384               OR 'STRING' OR 'UNSTRING' OR 'GOBACK'                      
000385               OR 'INSPECT' OR 'CALL' OR 'WHEN'                           
000386               OR 'SORT' OR 'SEARCH' OR 'SET' OR 'ACCEPT'                 
000387               OR 'ADD'  OR 'COMPUTE' OR 'SUBTRACT' OR 'SEARCH'           
000388               OR 'CONTINUE'                                              
000389             MOVE JA TO END-OF-OPEN-SATS                                  
000390                                                                          
000391           WHEN ORD2 = 'SECTION.'                                         
000392             MOVE JA TO END-OF-OPEN-SATS                                  
000393                                                                          
000394           WHEN OTHER                                                     
000395             PERFORM FA-ANALYSERA-OPEN-RAD                                
000396             PERFORM S01-LAES-COBOLIN                                     
000397             INSPECT RAD CONVERTING GEMENER                               
000398                      TO VERSALER                                         
000399                                                                          
000400         END-EVALUATE                                                     
000401       ELSE                                                               
000402         PERFORM S01-LAES-COBOLIN                                         
000403         INSPECT RAD CONVERTING GEMENER TO VERSALER                       
000404       END-IF                                                             
000405     END-PERFORM                                                          
000406     .                                                                    
000407     EJECT                                                                
000408 FA-ANALYSERA-OPEN-RAD  SECTION.                                          
000409     SKIP2                                                                
000410     MOVE 1 TO OIX                                                        
000411     PERFORM UNTIL OIX > 8 OR ORD(OIX) = SPACE                            
000412                                                                          
000413       EVALUATE TRUE                                                      
000414        WHEN ORD(OIX) = 'OPEN'                                            
000415          CONTINUE                                                        
000416        WHEN ORD(OIX) = 'INPUT'                                           
000417          MOVE OPNIN-TYP  TO UT-TYP                                       
000418        WHEN ORD(OIX) = 'OUTPUT'                                          
000419          MOVE OPNOUT-TYP  TO UT-TYP                                      
000420        WHEN OTHER                                                        
000421          MOVE ORD(OIX)   TO UT-DATA-ORD1                                 
000422          PERFORM S11-SKRIV-COBOLOUT                                      
000423       END-EVALUATE                                                       
000424                                                                          
000425       ADD 1 TO OIX                                                       
000426     END-PERFORM                                                          
000427                                                                          
000428     .                                                                    
000429     EJECT                                                                
000430 G-ANALYSERA-CALL  SECTION.                                               
000431     SKIP2                                                                
000432     MOVE CALL-TYP TO UT-TYP                                              
000433     MOVE SPACE TO UT-DATA                                                
000434     EVALUATE TRUE                                                        
000435       WHEN ORD1 = 'CALL'                                                 
000436         MOVE ORD2 TO UT-DATA-ORD1                                        
000437                                                                          
000438       WHEN ORD2 = 'CALL'                                                 
000439         MOVE ORD3 TO UT-DATA-ORD1                                        
000440                                                                          
000441       WHEN ORD3 = 'CALL'                                                 
000442         MOVE ORD4 TO UT-DATA-ORD1                                        
000443                                                                          
000444       WHEN ORD4 = 'CALL'                                                 
000445         MOVE ORD5 TO UT-DATA-ORD1                                        
000446                                                                          
000447       WHEN ORD5 = 'CALL'                                                 
000448         MOVE ORD6 TO UT-DATA-ORD1                                        
000449                                                                          
000450       WHEN OTHER                                                         
000451         MOVE ORD7 TO UT-DATA-ORD1                                        
000452     END-EVALUATE                                                         
000453     PERFORM S11-SKRIV-COBOLOUT                                           
000454     .                                                                    
000455     EJECT                                                                
000456 H-ANALYSERA-DB2  SECTION.                                                
000457     SKIP2                                                                
000458     MOVE DB2-TYP TO UT-TYP                                               
000459     MOVE SPACE TO UT-DATA                                                
000460     PERFORM S11-SKRIV-COBOLOUT                                           
000461     .                                                                    
000462     EJECT                                                                
000463 I-ANALYSERA-USING  SECTION.                                              
000464     SKIP2                                                                
000465     MOVE USNG-TYP TO UT-TYP                                              
000466     MOVE SPACE TO UT-DATA                                                
000467     MOVE ORD1 TO UT-DATA-ORD1                                            
000468     EVALUATE TRUE                                                        
000469       WHEN ORD1 = 'USING'                                                
000470         MOVE ORD2 TO UT-DATA-ORD2                                        
000471                                                                          
000472       WHEN ORD2 = 'USING'                                                
000473         MOVE ORD3 TO UT-DATA-ORD2                                        
000474                                                                          
000475       WHEN ORD3 = 'USING'                                                
000476         MOVE ORD4 TO UT-DATA-ORD2                                        
000477                                                                          
000478       WHEN ORD4 = 'USING'                                                
000479         MOVE ORD5 TO UT-DATA-ORD2                                        
000480                                                                          
000481       WHEN OTHER                                                         
000482         MOVE ORD6 TO UT-DATA-ORD2                                        
000483     END-EVALUATE                                                         
000484                                                                          
000485     PERFORM S11-SKRIV-COBOLOUT                                           
000486     .                                                                    
000487     EJECT                                                                
000488 J-ANALYSERA-GIVING  SECTION.                                             
000489     SKIP2                                                                
000490     MOVE GIV-TYP TO UT-TYP                                               
000491     MOVE SPACE TO UT-DATA                                                
000492     EVALUATE TRUE                                                        
000493       WHEN ORD1 = 'GIVING'                                               
000494         MOVE ORD2 TO UT-DATA-ORD1                                        
000495                                                                          
000496       WHEN ORD2 = 'GIVING'                                               
000497         MOVE ORD3 TO UT-DATA-ORD1                                        
000498                                                                          
000499       WHEN ORD3 = 'GIVING'                                               
000500         MOVE ORD4 TO UT-DATA-ORD1                                        
000501                                                                          
000502       WHEN OTHER                                                         
000503         MOVE ORD5 TO UT-DATA-ORD1                                        
000504     END-EVALUATE                                                         
000505                                                                          
000506     PERFORM S11-SKRIV-COBOLOUT                                           
000507     .                                                                    
000508     EJECT                                                                
000509 Z-FINIT SECTION.                                                         
000510     SKIP2                                                                
000511     CLOSE COBOLIN                                                        
000512           COBOLOUT                                                       
000513     .                                                                    
000514     EJECT                                                                
000515 S01-LAES-COBOLIN  SECTION.                                               
000516     SKIP2                                                                
000517     READ COBOLIN INTO IN-AREA                                            
000518       AT END SET END-OF-COBOLIN TO TRUE                                  
000519       NOT AT END MOVE IN-RAD TO RAD RADX2 RADX20                         
000520     END-READ                                                             
000521     IF IN-RADMARKERING = SID-RUBRIK                                      
000522*      -- SKIPPA SID-RUBRIKRAD                                            
000523       READ COBOLIN INTO IN-AREA                                          
000524         AT END SET END-OF-COBOLIN TO TRUE                                
000525         NOT AT END MOVE IN-RAD TO RAD RADX2 RADX20                       
000526       END-READ                                                           
000527     END-IF                                                               
000528     .                                                                    
000529     EJECT                                                                
000530 S02-LAES-TILL-PGM-START  SECTION.                                        
000531     SKIP2                                                                
000532*    PERFORM S01-LAES-COBOLIN                                             
000533     PERFORM UNTIL END-OF-COBOLIN                                         
000534     OR IN-NAMNMARKERING = 'Data Set Name'                                
000535       PERFORM S01-LAES-COBOLIN                                           
000536     END-PERFORM                                                          
000537     IF IN-NAMNMARKERING = 'Data Set Name'                                
000538        MOVE IN-PGMNAMN TO UT-PGMNAMN                                     
000539        INSPECT UT-PGMNAMN REPLACING ALL ')' BY SPACE                     
000540*       -- SKIPPA KOMMENTARRAD FRAM TILL FÖRSTA SOURCE-RAD                
000541        PERFORM S01-LAES-COBOLIN                                          
000542        PERFORM S01-LAES-COBOLIN                                          
000543     END-IF                                                               
000544     .                                                                    
000545     EJECT                                                                
000546 S11-SKRIV-COBOLOUT SECTION.                                              
000547     SKIP2                                                                
000548     WRITE UT-POST FROM UT-AREA                                           
000549     .                                                                    
000550     EJECT                                                                
000551 S99-ABEND SECTION.                                                       
000552                                                                          
000553     SKIP2                                                                
000554     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
000555     .                                                                    
