000001 ID DIVISION.                                                             
000002 PROGRAM-ID.     W2210800.                                                
000003 AUTHOR.         HELGEGREN PER-ANDERS.                                    
000004 DATE-WRITTEN.   00/10/27.                                                
000005 DATE-COMPILED.                                                           
000006                                                                          
000007                                                                          
000008*    FUNKTION:                                                            
000009*        LÄGGER UPP LARM PÅ LARMBAS WDD4                                  
000010*                                                                         
000011*        PROGRAMMET UPPDATERAR WDD4                                       
000012*                                                                         
000013*                                                                         
000014*--- ÄNDRINGAR:                                                           
000015*    2013-04-16     E'TRACKER 10143273 CHINA  LOCAL SOURCING              
000016*                   LAGT TILL IDDC PÅ WDD401                              
000017*                                                                         
000018*                                                                         
000019                                                                          
000020     SKIP3                                                                
000021 ENVIRONMENT DIVISION.                                                    
000022     SKIP2                                                                
000023 INPUT-OUTPUT SECTION.                                                    
000024                                                                          
000025 FILE-CONTROL.                                                            
000026     SKIP2                                                                
000027*          --- LARMPOSTER FRÅN W22107                                     
000028     SELECT W22108                     ASSIGN TO W22108D1.                
000029     EJECT                                                                
000030 DATA DIVISION.                                                           
000031     SKIP3                                                                
000032 FILE SECTION.                                                            
000033     SKIP3                                                                
000034 FD  W22108                                                               
000035     RECORDING       F                                                    
000036     BLOCK CONTAINS  0.                                                   
000037                                                                          
000038*01  -COPY WDD401        -L.                                              
000039     EJECT                                                                
000040 WORKING-STORAGE SECTION.                                                 
000041                                                                          
000042                                                                          
000043*    -- CHECKED BY WY2000                                                 
000044 77  IDPGM                       PIC X(8)    VALUE 'W2210800'.            
000045 01  CHKP-VAR.                                                            
000046     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
000047     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
000048     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
000049     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
000050     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
000051     03 CHKP-MAX                 PIC S9(3)   VALUE +800 COMP-3.           
000052 01  W-W22108-KVPOST-IN          PIC S9(5)   VALUE ZERO COMP-3.           
000054 77  JA                          PIC X       VALUE 'J'.                   
000055 77  NEJ                         PIC X       VALUE 'N'.                   
000056     SKIP2                                                                
000057 01  FELTEXT.                                                             
000058     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
000059     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
000060                                                                          
000061 77  W22108-EOF-SW               PIC X       VALUE 'N'.                   
000062     88  END-OF-W22108                       VALUE 'J'.                   
000063     EJECT                                                                
000073 01  DYNAMISKA-SUBPROGRAM.                                                
000074*                                                                         
000075     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
000076     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
000077     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
000078     EJECT                                                                
000079*    --- PARAMETRAR TILL POSTSUM                                          
000080*                                                                         
000081*01  -COPY W0005   -PRE  POSTSUM-                                         
000082     EJECT                                                                
000083 01  IN-AREA-START               PIC X(24)   VALUE                        
000084                                             'IN-AREA-START'.             
000085     SKIP2                                                                
000086                                                                          
000087*01  AREA -COPY WDD401       -PRE IN-                                     
000088*                                                                         
000089     EJECT                                                                
000090 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
000091     SKIP3                                                                
000092 01  NYCKLAR-TILL-DLI.                                                    
000093     03  W-WDD401KEY-MIN-X.                                               
000094       05  W-DAREGDAT-9KOMPL-MIN PIC 9(8)    VALUE ZERO.                  
000095       05  FILLER                PIC X(5)    VALUE LOW-VALUE.             
000096     03  W-WDD401KEY-MAX-X.                                               
000097       05  W-DAREGDAT-9KOMPL-MAX PIC 9(8)    VALUE ZERO.                  
000098       05  FILLER                PIC X(5)    VALUE HIGH-VALUE.            
000099                                                                          
000100     03  W-IDARTNR-X.                                                     
000101         05  W-IDARTNR           PIC S9(9) COMP-3   VALUE ZERO.           
000102     03  W-IDDC-X.                                                        
000103         05  W-IDDC              PIC X(02)          VALUE SPACE.          
000104     03  W-IDLEVNR-X.                                                     
000105         05  W-IDLEVNR           PIC X(5)           VALUE SPACE.          
000106     03  W-KDLARM-X.                                                      
000107         05  W-KDLARM            PIC S9(3) COMP-3   VALUE ZERO.           
000108     SKIP2                                                                
000109*    --- STATUS-KOD FRÅN IMS                                              
000110 01  STATUS-WS                   PIC XX.                                  
000111     88  SEGMENT-FINNS                       VALUE '  '.                  
000112     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
000113     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
000114     88  SEGMENT-SLUT                        VALUE 'GB'.                  
000115     88  IMS-EJ-OK                           VALUE 'XD'.                  
000116     SKIP2                                                                
000117 01  GODK-STATUSKODER.                                                    
000118     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
000119     SKIP3                                                                
000120 01  SSA1                        PIC X(160).                              
000122     EJECT                                                                
000123*    --- IMS FUNKTIONSKODER                                               
000124*01  -COPY W0003                                                          
000125     EJECT                                                                
000126*    ---  DLI INPUT-OUTPUT AREA                                           
000127                                                                          
000128 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD401'.                      
000129 01  DLI-IO-WDD401.                                                       
000130*    03  -COPY WDD401                                                     
000131                                                                          
000132     EJECT                                                                
000133 LINKAGE SECTION.                                                         
000134                                                                          
000135*01  -COPY W0009   -PRE MSG-                                              
000136                                                                          
000137*01  -COPY W0008  -PRE WDD4-                                              
000138     05  FILLER                  PIC X.                                   
000139     EJECT                                                                
000140 PROCEDURE DIVISION  USING MSG-PCB WDD4-PCB.                              
000141 MAIN SECTION.                                                            
000142     ENTRY 'DLITCBL' USING MSG-PCB WDD4-PCB.                              
000143                                                                          
000144     SKIP2                                                                
000145     PERFORM A-INIT                                                       
000146     PERFORM S01-LAES-W22108                                              
000147     PERFORM UNTIL END-OF-W22108                                          
000148       IF CHKP-ANT > CHKP-MAX                                             
000149         PERFORM X-TAG-CHECKPOINT                                         
000150       END-IF                                                             
000151       IF IN-LAK-KDLARM = 225                                             
000153         MOVE IN-LAK-DAREGDAT-9KOMPL                                      
000154                                  TO W-DAREGDAT-9KOMPL-MIN                
000155                                     W-DAREGDAT-9KOMPL-MAX                
000156         MOVE IN-LAK-IDARTNR      TO W-IDARTNR                            
000157         MOVE IN-LAK-IDDC         TO W-IDDC                               
000158         MOVE IN-LAK-IDLEVNR      TO W-IDLEVNR                            
000159         MOVE 220                 TO W-KDLARM                             
000160         PERFORM IMS-GHU-WDD401                                           
000161**  IF 220 ALARM EXIST DON'T INSERT 225 ALARM IF SAME ALARM DAY           
000162         IF SEGMENT-SAKNAS                                                
000167           MOVE IN-AREA TO LAK-WDD401                                     
000168           PERFORM IMS-ISRT-WDD401                                        
000169         END-IF                                                           
000170       ELSE                                                               
000171         MOVE IN-AREA  TO LAK-WDD401                                      
000172         PERFORM IMS-ISRT-WDD401                                          
000173                                                                          
000174         IF LAK-KDLARM = 220                                              
000175            PERFORM B-DLET-225                                            
000176         END-IF                                                           
000177       END-IF                                                             
000178                                                                          
000179       PERFORM S01-LAES-W22108                                            
000180     END-PERFORM                                                          
000181                                                                          
000182                                                                          
000183     PERFORM Z-FINIT                                                      
000184                                                                          
000185     MOVE ZERO TO RETURN-CODE                                             
000186     GOBACK                                                               
000187     .                                                                    
000188     EJECT                                                                
000189 A-INIT SECTION.                                                          
000190     SKIP2                                                                
000191                                                                          
000192     PERFORM IMS-RESTART                                                  
000193                                                                          
000194     OPEN INPUT W22108                                                    
000195                                                                          
000196                                                                          
000197     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
000198     .                                                                    
000199     EJECT                                                                
000200 B-DLET-225 SECTION.                                                      
000201                                                                          
000202*    LARM 'NOT ARRIVED' (225) SKALL SLÄCKAS                               
000203*    DÅ LARM 'PREADVICE DEVIATION' (220) KOMMER                           
000204                                                                          
000205     MOVE LAK-DAREGDAT-9KOMPL                                             
000206                           TO W-DAREGDAT-9KOMPL-MIN                       
000207                              W-DAREGDAT-9KOMPL-MAX                       
000208     MOVE LAK-IDARTNR      TO W-IDARTNR                                   
000209     MOVE LAK-IDDC         TO W-IDDC                                      
000210     MOVE LAK-IDLEVNR      TO W-IDLEVNR                                   
000211     MOVE 225              TO W-KDLARM                                    
000212     PERFORM IMS-GHU-WDD401                                               
000213     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
000214        PERFORM IMS-DLET-WDD401                                           
000216        PERFORM IMS-GHN-WDD401                                            
000217     END-PERFORM                                                          
000218     .                                                                    
000219     EJECT                                                                
000232 Z-FINIT SECTION.                                                         
000234                                                                          
000235     CLOSE W22108                                                         
000236     SKIP2                                                                
000237     MOVE 'S' TO POSTSUM-OPKOD                                            
000238     CALL POSTSUM USING POSTSUM-PARM                                      
000239     .                                                                    
000240     EJECT                                                                
000241 S01-LAES-W22108  SECTION.                                                
000242     SKIP2                                                                
000243     READ W22108 INTO IN-AREA                                             
000244     AT END                                                               
000245        SET END-OF-W22108 TO TRUE                                         
000246                                                                          
000247     NOT AT END                                                           
000248        MOVE 'W22108'   TO POSTSUM-FDNAMN                                 
000249        MOVE 'W22108D1' TO POSTSUM-DDNAMN2                                
000250        MOVE 'IN'       TO POSTSUM-TRANSTYP                               
000251        CALL POSTSUM USING POSTSUM-PARM                                   
000252                                                                          
000253        ADD 1 TO W-W22108-KVPOST-IN                                       
000254     END-READ                                                             
000255     .                                                                    
000256     EJECT                                                                
000257 X-TAG-CHECKPOINT   SECTION.                                              
000258                                                                          
000259     PERFORM IMS-CHECKPOINT                                               
000260     MOVE ZERO TO CHKP-ANT                                                
000261                                                                          
000262     DISPLAY ' CHKP: ANTAL INPOSTER= ' W-W22108-KVPOST-IN                 
000263     .                                                                    
000264     EJECT                                                                
000265* --- IMS SEKTIONER ---                                                   
000266                                                                          
000267 IMS-GHU-WDD401 SECTION.                                                  
000268                                                                          
000270     STRING 'WDD401  (WDD401KY>=' W-WDD401KEY-MIN-X                       
000271                    '&WDD401KY<=' W-WDD401KEY-MAX-X                       
000272                    '&IDARTNR  =' W-IDARTNR-X                             
000273                    '&IDDC     =' W-IDDC-X                                
000274                    '&IDLEVNR  =' W-IDLEVNR-X                             
000275                    '&KDLARM   =' W-KDLARM-X ')'                          
000276          DELIMITED BY SIZE INTO SSA1                                     
000277     MOVE '  GE' TO GODK-STATUSKODER                                      
000278     CALL CBLTDLI USING GHU WDD4-PCB DLI-IO-WDD401 SSA1                   
000279     MOVE WDD4-STATUS-CODE TO STATUS-WS                                   
000280     PERFORM IMS-STATUSKONTROLL                                           
000281     .                                                                    
000282     SKIP3                                                                
000283 IMS-GHN-WDD401 SECTION.                                                  
000284                                                                          
000285     STRING 'WDD401  (WDD401KY>=' W-WDD401KEY-MIN-X                       
000286                    '&WDD401KY<=' W-WDD401KEY-MAX-X                       
000287                    '&IDARTNR  =' W-IDARTNR-X                             
000288                    '&IDDC     =' W-IDDC-X                                
000289                    '&IDLEVNR  =' W-IDLEVNR-X                             
000290                    '&KDLARM   =' W-KDLARM-X ')'                          
000295          DELIMITED BY SIZE INTO SSA1                                     
000296     MOVE '  GEGB' TO GODK-STATUSKODER                                    
000297     CALL CBLTDLI USING GHN WDD4-PCB DLI-IO-WDD401 SSA1                   
000298     MOVE WDD4-STATUS-CODE TO STATUS-WS                                   
000299     PERFORM IMS-STATUSKONTROLL                                           
000300     .                                                                    
000301     EJECT                                                                
000302 IMS-ISRT-WDD401   SECTION.                                               
000303                                                                          
000304     STRING 'WDD401      '                                                
000305          DELIMITED BY SIZE INTO SSA1                                     
000306     MOVE '    '            TO GODK-STATUSKODER                           
000307     CALL CBLTDLI USING ISRT WDD4-PCB DLI-IO-WDD401 SSA1                  
000308     MOVE WDD4-STATUS-CODE  TO STATUS-WS                                  
000309     PERFORM IMS-STATUSKONTROLL                                           
000310                                                                          
000311     ADD +1                 TO CHKP-ANT                                   
000312     .                                                                    
000313     SKIP3                                                                
000314 IMS-DLET-WDD401   SECTION.                                               
000315                                                                          
000316     MOVE '  GE'            TO GODK-STATUSKODER                           
000317     CALL CBLTDLI USING DLET WDD4-PCB DLI-IO-WDD401                       
000318     MOVE WDD4-STATUS-CODE  TO STATUS-WS                                  
000319     PERFORM IMS-STATUSKONTROLL                                           
000320                                                                          
000321     MOVE 'W22108'   TO POSTSUM-FDNAMN                                    
000322     MOVE 'WDD401  ' TO POSTSUM-DDNAMN2                                   
000323     MOVE 'DLET'     TO POSTSUM-TRANSTYP                                  
000324     CALL POSTSUM USING POSTSUM-PARM                                      
000325                                                                          
000326     ADD +1                 TO CHKP-ANT                                   
000327     .                                                                    
000328     EJECT                                                                
000329 IMS-RESTART SECTION.                                                     
000330     SKIP2                                                                
000331     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
000332     MOVE '  ' TO GODK-STATUSKODER                                        
000333     CALL CBLTDLI USING XRST MSG-PCB                                      
000334                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
000335                        CHKP-AREA-LENGTH CHKP-AREA                        
000336     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
000337     PERFORM IMS-STATUSKONTROLL                                           
000338     .                                                                    
000339     SKIP3                                                                
000340 IMS-CHECKPOINT SECTION.                                                  
000341     SKIP2                                                                
000342     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
000343     MOVE '  XD' TO GODK-STATUSKODER                                      
000344     CALL CBLTDLI USING CHKP MSG-PCB                                      
000345                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
000346                        CHKP-AREA-LENGTH CHKP-AREA                        
000347     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
000348     PERFORM IMS-STATUSKONTROLL                                           
000349                                                                          
000350     IF IMS-EJ-OK                                                         
000351       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
000352       DISPLAY FELTEXT                                                    
000353       CALL FELLOG                                                        
000354     END-IF                                                               
000355     .                                                                    
000356     EJECT                                                                
000357 IMS-STATUSKONTROLL SECTION.                                              
000358     SKIP2                                                                
000359     SET STATUS-IX TO 1                                                   
000360     SEARCH GODK-STATUS                                                   
000361       AT END                                                             
000362         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
000363           DELIMITED BY SIZE INTO FELTEXT                                 
000364         DISPLAY FELTEXT                                                  
000365         CALL FELLOG                                                      
000366       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
000367         CONTINUE                                                         
000368     END-SEARCH                                                           
000370     .                                                                    
