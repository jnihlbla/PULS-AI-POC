000001 ID DIVISION.                                                             
000002 PROGRAM-ID.     W2718700.                                                
000003 AUTHOR.         STEFAN ÅSGÅRDEN.                                         
000004 DATE-WRITTEN.   OKT 2002.                                                
000005 DATE-COMPILED.                                                           
000006                                                                          
000007*    FUNKTION:                                                            
000008*                                                                         
000009*                PROGRAMMET LÄSER FAKTURERADE KOLLIN OCH SUMMERAR         
000010*                BRUTTOVOLYMEN FÖR FLYG                                   
000011*                                                                         
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
000025     SELECT W27187IN                   ASSIGN TO W27187D1.                
000026*                                                                         
000027     SELECT W27187UT                   ASSIGN TO W27187D2.                
000028     SKIP2                                                                
000029     EJECT                                                                
000030 DATA DIVISION.                                                           
000031     SKIP3                                                                
000032 FILE SECTION.                                                            
000033     SKIP3                                                                
000034                                                                          
000035 FD  W27187IN                                                             
000036     RECORDING F                                                          
000037     BLOCK CONTAINS  0.                                                   
000038                                                                          
000039                                                                          
000040*01  POST -COPY W479060  -PRE  IN-  -L.                                   
000041                                                                          
000042     SKIP3                                                                
000043                                                                          
000044 FD  W27187UT                                                             
000045     RECORDING F                                                          
000046     BLOCK CONTAINS  0.                                                   
000047                                                                          
000048*01  POST -COPY W27187   -PRE  UT-  -L.                                   
000049                                                                          
000050*                                                                         
000051     EJECT                                                                
000052 WORKING-STORAGE SECTION.                                                 
000053*    -- CHECKED BY WY2000                                                 
000054 77  IDPGM                       PIC X(8)    VALUE 'W2718700'.            
000055 77  DC-IX                       PIC S9(4)   VALUE +0  COMP SYNC.         
000056 77  IX                          PIC S9(4)   VALUE +0  COMP SYNC.         
000057 77  MAX-TAB-IX                  PIC S9(4)   VALUE +10 COMP SYNC.         
000058 77  JA                          PIC X       VALUE 'J'.                   
000059 77  NEJ                         PIC X       VALUE 'N'.                   
000060     SKIP2                                                                
000061 01  WS.                                                                  
000062     03  WS-AKTUELL-AAVV         PIC 9(4)    VALUE ZERO.                  
000063     03  WS-KTRL-AAVV            PIC 9(4)    VALUE ZERO.                  
000064     03  WS-IDDC                 PIC X(2)    VALUE SPACE.                 
000065     03  WS-VLORDBTO-KOLLI       PIC 9(7)V9(3)                            
000066                                             VALUE ZERO.                  
000067     03  WS-SPARA-DISTR          PIC 9(5)    VALUE ZERO.                  
000068     EJECT                                                                
000069 01  POST-ANT                    PIC S9(7)   VALUE ZERO COMP-3.           
000070                                                                          
000071 01  FILLER          PIC X(16)   VALUE 'SPAR-FAELT START'.                
000072                                                                          
000073 77  RKOD-ABEND-UTAN-DUMP       PIC S9(4)   VALUE +16 COMP SYNC.          
000074 77  RKOD-ABEND-MED-DUMP        PIC S9(4)   VALUE +1000 COMP SYNC.        
000075                                                                          
000076 01  WS-ARBETSAREA.                                                       
000077*    DATE + TIME  FÖR SKAPANDE AV INLEVERANSNUMMER                        
000078     03  WS-TIAAMMDDTTMMSSTH     PIC 9(14)  VALUE ZERO.                   
000079     03  FILLER REDEFINES WS-TIAAMMDDTTMMSSTH.                            
000080         05  WS-TIAAMMDD-DATE    PIC 9(6).                                
000081         05  WS-TTMMSSTH-TIME    PIC 9(8).                                
000082     03  WS-IDINLEV              PIC S9(15) VALUE ZERO COMP-3.            
000083     03  WS-IDFAKT               PIC  9(7) VALUE ZERO.                    
000084                                                                          
000085     EJECT                                                                
000086 01  FELTEXT.                                                             
000087     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
000088     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
000089                                                                          
000090 77  W27187-EOF-SW               PIC X       VALUE 'N'.                   
000091     88  END-OF-W27187                       VALUE 'J'.                   
000092     EJECT                                                                
000093 77  AIR-VOLYM-SW                PIC X       VALUE 'N'.                   
000094     88  AIR-VOLYM-JA                        VALUE 'J'.                   
000095     EJECT                                                                
000096 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
000097 01  FILLER REDEFINES DAGENS-DATUM.                                       
000098     03  DAGENS-DATUM-AAR        PIC 9(2).                                
000099     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
000100     03  DAGENS-DATUM-DAG        PIC 9(2).                                
000101     SKIP3                                                                
000102 01  WS-AAMMDD                   PIC 9(6).                                
000103 01  FILLER REDEFINES WS-AAMMDD.                                          
000104     03  WS-AA               PIC 9(2).                                    
000105     03  FILLER              PIC 9(4).                                    
000106                                                                          
000107     EJECT                                                                
000108                                                                          
000109*      --- VALID IDDC CODES                                               
000110*                                                                         
000111*01    -COPY WWDC99                                                       
000112*01    -COPY WWDIST35                                                     
000113       EJECT                                                              
000114                                                                          
000115 01  DYNAMISKA-SUBPROGRAM.                                                
000116     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.            
000117     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
000118     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY '.            
000119     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
000120     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
000121     03  W009VADD                PIC X(8)    VALUE 'W009VADD'.            
000122     EJECT                                                                
000123*                                                                         
000124 01  FILLER                      PIC X(16)   VALUE 'POSTSUM '.            
000125*    --- PARAMETRAR TILL POSTSUM                                          
000126*01  -COPY W0005   -PRE  POSTSUM-                                         
000127     EJECT                                                                
000128                                                                          
000129 01  FILLER                      PIC X(16)   VALUE 'WDATAREA'.            
000130*    ---PARAMETRAR TILL DATKONV                                           
000131*01  -COPY WDATAREA                                                       
000132     EJECT                                                                
000133****************************************** PARAM. W009VADD                
000134 01  W009VADDW.                                                           
000135     03  W009VADDW-AAVV      PIC S9(5)               COMP-3.              
000136     03  W009VADDW-ANTAL     PIC S9(3)               COMP-3.              
000137     EJECT                                                                
000138*    --- PARAMETRAR TILL WORKDAY                                          
000139*                                                                         
000140*01  -COPY WORKAREA                                                       
000141     EJECT                                                                
000142 01  IN-AREA-START               PIC X(24)   VALUE                        
000143                                             'IN-AREA-START'.             
000144     SKIP2                                                                
000145*01  AREA -COPY W479060    -PRE IN-                                       
000146                                                                          
000147     EJECT                                                                
000148 01  UT-AREA-START              PIC X(24)   VALUE                         
000149                                 'UT-AREA-START  '.                       
000150*01  AREA -COPY W27187     -PRE UT-                                       
000151     SKIP2                                                                
000152                                                                          
000153                                                                          
000154     EJECT                                                                
000155 PROCEDURE DIVISION.                                                      
000156                                                                          
000157     PERFORM A-INIT                                                       
000158                                                                          
000159     PERFORM S01-LAES-W27187IN                                            
000160                                                                          
000161     PERFORM UNTIL END-OF-W27187                                          
000162                                                                          
000163       PERFORM B-NOLLSTALL                                                
000164                                                                          
000165       IF (DIST35-REFILL-NA                                               
000166       OR  DIST35-REFILL-NP                                               
000167       OR  DIST35-REFILL-CN                                               
000168       OR  DIST35-REFILL-NX                                               
000169       OR  DIST35-REFILL-NS                                               
000170       OR  DIST35-NONVCC-REFILL)                                          
000171                                                                          
000172           MOVE IN-IDDISTR           TO WS-SPARA-DISTR                    
000173                                                                          
000174           PERFORM UNTIL END-OF-W27187                                    
000175           OR IN-IDDISTR    NOT = WS-SPARA-DISTR                          
000176                                                                          
000177              IF IN-KDORDKL     = 1                                       
000178                 SET AIR-VOLYM-JA    TO TRUE                              
000179                 PERFORM C-BEHANDLA                                       
000180              END-IF                                                      
000181              PERFORM S01-LAES-W27187IN                                   
000182           END-PERFORM                                                    
000183*                                                                         
000184           IF AIR-VOLYM-JA                                                
000185              MOVE WS-KTRL-AAVV      TO UT-VECKA                          
000186              MOVE WS-SPARA-DISTR    TO UT-IDDISTR                        
000187              COMPUTE UT-VLORDBTO-KOLLI ROUNDED =                         
000188                                        WS-VLORDBTO-KOLLI * 1             
000189              PERFORM S02-SKRIV-UTFIL                                     
000190                                                                          
000191              DISPLAY '           '                                       
000192              DISPLAY '           '                                       
000193              DISPLAY '           '                                       
000194              DISPLAY 'DISTRIKT : ' WS-SPARA-DISTR                        
000195              DISPLAY 'WS-BVOL  : ' WS-VLORDBTO-KOLLI                     
000196           END-IF                                                         
000197       ELSE                                                               
000198           PERFORM S01-LAES-W27187IN                                      
000199       END-IF                                                             
000200                                                                          
000201     END-PERFORM                                                          
000202                                                                          
000203     PERFORM Z-FINIT                                                      
000204                                                                          
000205     MOVE ZERO TO RETURN-CODE                                             
000206     GOBACK                                                               
000207     .                                                                    
000208     EJECT                                                                
000209 A-INIT SECTION.                                                          
000210     SKIP2                                                                
000211                                                                          
000212     OPEN INPUT  W27187IN                                                 
000213     OPEN OUTPUT W27187UT                                                 
000214     ACCEPT DAGENS-DATUM   FROM DATE                                      
000215                                                                          
000216     MOVE IDPGM              TO POSTSUM-PROGNAMN                          
000217                                                                          
000218     MOVE 'AAMMDD'           TO DAT-KDDATFORM                             
000219     MOVE DAGENS-DATUM       TO DAT-I-TIDATUM                             
000220                                                                          
000221     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
000222                     DAT-O-TIDATUM DAT-KDSVAR                             
000223                                                                          
000224     IF DAT-KDSVAR-OK                                                     
000225                                                                          
000226       MOVE DAT-TIAAVV-GRP   TO WS-AKTUELL-AAVV                           
000227       MOVE WS-AKTUELL-AAVV  TO W009VADDW-AAVV                            
000228       MOVE -1               TO W009VADDW-ANTAL                           
000229       CALL W009VADD USING W009VADDW-AAVV W009VADDW-ANTAL                 
000230       MOVE W009VADDW-AAVV   TO WS-KTRL-AAVV                              
000231                                                                          
000232     ELSE                                                                 
000233         STRING ' FEL FRÅN DATUMRUTIN WDATKONV '                          
000234         DELIMITED BY SIZE INTO FELTEXT                                   
000235         CALL FELLOG                                                      
000236     END-IF                                                               
000237     .                                                                    
000238     EJECT                                                                
000239 B-NOLLSTALL SECTION.                                                     
000240     SKIP2                                                                
000241                                                                          
000242     MOVE NEJ                TO AIR-VOLYM-SW                              
000243     MOVE ZERO               TO WS-VLORDBTO-KOLLI                         
000244     .                                                                    
000245     EJECT                                                                
000246 C-BEHANDLA SECTION.                                                      
000247                                                                          
000248     ADD IN-VLORDBTO-KOLLI   TO WS-VLORDBTO-KOLLI                         
000249     .                                                                    
000250     EJECT                                                                
000251                                                                          
000252 Z-FINIT SECTION.                                                         
000253                                                                          
000254     CLOSE W27187IN                                                       
000255           W27187UT                                                       
000256                                                                          
000257     MOVE 'S'                TO POSTSUM-OPKOD                             
000258     CALL POSTSUM         USING POSTSUM-PARM                              
000259     .                                                                    
000260     EJECT                                                                
000261 S01-LAES-W27187IN SECTION.                                               
000262     SKIP2                                                                
000263     READ W27187IN         INTO IN-AREA                                   
000264     AT END                                                               
000265        MOVE HIGH-VALUE      TO IN-AREA                                   
000266        MOVE JA              TO W27187-EOF-SW                             
000267                                                                          
000268     NOT AT END                                                           
000269        MOVE 'W27187IN'      TO POSTSUM-FDNAMN                            
000270        MOVE 'W27187D1'      TO POSTSUM-DDNAMN2                           
000271        MOVE SPACE           TO POSTSUM-TRANSTYP                          
000272        CALL POSTSUM      USING POSTSUM-PARM                              
000273                                                                          
000274        MOVE IN-IDDISTR      TO DIST35-IDDISTR                            
000275     END-READ                                                             
000276     .                                                                    
000277     EJECT                                                                
000278 S02-SKRIV-UTFIL SECTION.                                                 
000279     SKIP2                                                                
000280     WRITE UT-POST         FROM UT-AREA                                   
000281     MOVE 'W27187UT'         TO POSTSUM-FDNAMN                            
000282     MOVE 'W27187D2'         TO POSTSUM-DDNAMN2                           
000283     MOVE  SPACE             TO POSTSUM-TRANSTYP                          
000284     CALL POSTSUM         USING POSTSUM-PARM                              
000285     .                                                                    
