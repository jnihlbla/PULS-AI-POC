000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W3358H00.                                                
000400 AUTHOR.         ANDERS HENRIKSSON                                        
000500 DATE-WRITTEN.   130222                                                   
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        LÄSER NER WDF1 OCH SORTERAR UTFILEN PÅ IDLEVNR.                  
001000*                                                                         
001100*        PROGRAMMET LÄSER      WDF1                                       
001200*                                                                         
001300*    ABENDKODER:                                                          
001400*        U0016 -  . . . .                                                 
001500*        U1000 -  . . . .                                                 
001600*                                                                         
001700                                                                          
001800     SKIP3                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000     SKIP2                                                                
002100 INPUT-OUTPUT SECTION.                                                    
002200                                                                          
002300 FILE-CONTROL.                                                            
002400     SKIP2                                                                
002500*          --- FIL MED TULLFAKTOR                                         
002600     SELECT W3358H                     ASSIGN TO W3358HD1.                
002700     SKIP2                                                                
002800*          --- SORTERINGSFIL                                              
002900     SELECT SORTFIL                    ASSIGN TO W3358HDS.                
003000     EJECT                                                                
003100 DATA DIVISION.                                                           
003200     SKIP3                                                                
003300 FILE SECTION.                                                            
003400     SKIP3                                                                
003500***EÖ GÖR OM UTFILEN + SORTFILEN PGA ATT BARA EN RETULF ÄR MED            
003600 FD  W3358H                                                               
003700     RECORDING       F                                                    
003800     BLOCK CONTAINS  0.                                                   
003900     SKIP2                                                                
004000*01  POST -COPY W3358H -PRE  UT-  -L.                                     
004100     SKIP3                                                                
004200 SD  SORTFIL                                                              
004300     RECORDING       F                                                    
004400     SKIP2                                                                
004500*01  POST -COPY W3358H -PRE  SORT-                                        
004600     EJECT                                                                
004700 WORKING-STORAGE SECTION.                                                 
004800                                                                          
004900                                                                          
005000*    -- CHECKED BY WY2000                                                 
005100 77  IDPGM                       PIC X(8)    VALUE 'W335CN00'.            
005200 77  JA                          PIC X       VALUE 'J'.                   
005300 77  NEJ                         PIC X       VALUE 'N'.                   
005500 77  FLAGGA-02-SEG               PIC X       VALUE 'N'.                   
005600                                                                          
005700 77  SKRIV-SW                   PIC X       VALUE 'N'.                    
005800     88  SKRIV-POST                          VALUE 'J'.                   
005900 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006000 01  FILLER REDEFINES DAGENS-DATUM.                                       
006100     03  DAGENS-DATUM-AAR        PIC 9(2).                                
006200     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
006300     03  DAGENS-DATUM-DAG        PIC 9(2).                                
006400     EJECT                                                                
006500 01  DYNAMISKA-SUBPROGRAM.                                                
006600*                                                                         
006700     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
006800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007000     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007100     SKIP2                                                                
007200*    --- PARAMETRAR TILL ABEND                                            
007300                                                                          
007400 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
007500 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
007600     SKIP2                                                                
007700 01  FELTEXT.                                                             
007800     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007900     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
008000     EJECT                                                                
008100*    --- PARAMETRAR TILL POSTSUM                                          
008200*                                                                         
008300*01  -COPY W0005   -PRE  POSTSUM-                                         
008400     EJECT                                                                
008500 01  SORTWS-AREA-START           PIC X(24)   VALUE                        
008600                                 'SORTWS-AREA-START'.                     
008700     SKIP2                                                                
008800                                                                          
008900*01  AREA -COPY W3358H     -PRE SORTWS-                                   
009000     SKIP2                                                                
009100 01  SORT-RETURN-X               PIC X(2)  VALUE SPACE.                   
009200     EJECT                                                                
009300*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
009400*                                                                         
009500     SKIP2                                                                
009600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009700     SKIP3                                                                
009800 01  NYCKLAR-TILL-DLI.                                                    
009900     03  W-IDLEVNR-X.                                                     
010000         05  W-IDLEVNR           PIC X(5)   VALUE SPACE.                  
010100     SKIP2                                                                
010200*    --- STATUS-KOD FRÅN IMS                                              
010300 01  STATUS-WS                   PIC XX.                                  
010400     88  SEGMENT-FINNS                       VALUE '  '.                  
010500     88  SEGMENT-SLUT                        VALUE 'GB'.                  
010600     SKIP2                                                                
010700 01  GODK-STATUSKODER.                                                    
010800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
010900     SKIP3                                                                
011000 01  SSA1                        PIC X(64).                               
011100 01  SSA2                        PIC X(64).                               
011200     EJECT                                                                
011300*    --- IMS FUNKTIONSKODER                                               
011400*01  -COPY W0003                                                          
011500     EJECT                                                                
011600*    ---  DLI INPUT-OUTPUT AREA                                           
011700 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
011800     SKIP3                                                                
011900 01  DLI-IO-AREA.                                                         
012000     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
012100     SKIP3                                                                
012200     03  WDF101 REDEFINES IO-AREA.                                        
012300*        05  -COPY WDF101                                                 
012400     EJECT                                                                
012500     03  WDF102 REDEFINES IO-AREA.                                        
012600*        05  -COPY WDF102                                                 
012700     EJECT                                                                
012800 LINKAGE SECTION.                                                         
012900                                                                          
013000     SKIP2                                                                
013100*01  -COPY W0008  -PRE WDF1-                                              
013200     05  FILLER                  PIC X.                                   
013300     EJECT                                                                
013400 PROCEDURE DIVISION  USING WDF1-PCB.                                      
013500     ENTRY 'DLITCBL' USING WDF1-PCB.                                      
013600                                                                          
013700     SKIP2                                                                
013800     PERFORM A-INIT                                                       
013900                                                                          
014000     SORT SORTFIL ASCENDING KEY SORT-IDLEVNR                              
014100                  INPUT PROCEDURE B-SORT-INPUT                            
014200                  GIVING W3358H                                           
014300                                                                          
014400     IF SORT-RETURN NOT = 0                                               
014500       MOVE SORT-RETURN TO SORT-RETURN-X                                  
014600       STRING 'RETURKOD ' SORT-RETURN-X ' FRÅN SORT'                      
014700           DELIMITED BY SIZE                                              
014800           INTO FELTEXT-STR                                               
014900       DISPLAY FELTEXT                                                    
015000                                                                          
015100     ELSE                                                                 
015200       PERFORM Z-FINIT                                                    
015300                                                                          
015400       MOVE ZERO TO RETURN-CODE                                           
015500       GOBACK                                                             
015600     END-IF                                                               
015700                                                                          
015800     .                                                                    
015900     EJECT                                                                
016000 A-INIT SECTION.                                                          
016100     SKIP2                                                                
016200     ACCEPT DAGENS-DATUM  FROM DATE                                       
016300     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
016400     PERFORM AA-NOLLSTAELL-UTPOST                                         
016500     .                                                                    
016600     EJECT                                                                
016700 AA-NOLLSTAELL-UTPOST SECTION.                                            
016800     SKIP2                                                                
016900     MOVE ZERO TO  SORTWS-RETULF-CN                                       
016900     MOVE ZERO TO  SORTWS-RETULF-US                                       
017000     MOVE SPACE TO SORTWS-IDLEVNR                                         
017100     MOVE SPACE TO SORTWS-IDLEVNR-MOTSV                                   
017200     .                                                                    
017300     EJECT                                                                
017400 B-SORT-INPUT SECTION.                                                    
017500     SKIP2                                                                
017600     PERFORM IMS-GET-WDF1                                                 
017700     PERFORM UNTIL SEGMENT-SLUT                                           
017800       EVALUATE WDF1-SEG-NAME-FB                                          
017900         WHEN 'WDF101  '                                                  
018000           IF SKRIV-POST AND FLAGGA-02-SEG = JA                           
018100             PERFORM S31-RELEASE-W3358H                                   
018200             PERFORM AA-NOLLSTAELL-UTPOST                                 
018300           END-IF                                                         
018400           IF LEV-IDLEVNR NOT = SPACE                                     
018500             MOVE LEV-IDLEVNR       TO SORTWS-IDLEVNR                     
018600             MOVE LEV-IDLEVNR-MOTSV TO SORTWS-IDLEVNR-MOTSV               
018700             MOVE JA              TO SKRIV-SW                             
018800           ELSE                                                           
018900             MOVE NEJ TO SKRIV-SW                                         
019000           END-IF                                                         
019100           MOVE NEJ TO FLAGGA-02-SEG                                      
019200         WHEN 'WDF102  '                                                  
019300           IF TULL-IDLANDX2 = 'CN' OR 'US'                                
019400             IF SKRIV-POST                                                
019500               IF DAGENS-DATUM > TULL-TITULF OR                           
019600                  DAGENS-DATUM = TULL-TITULF                              
                        IF TULL-IDLANDX2 = 'CN'                                 
019700                     MOVE TULL-RETULF-1 TO SORTWS-RETULF-CN               
                        ELSE                                                    
019700                     MOVE TULL-RETULF-1 TO SORTWS-RETULF-US               
                        END-IF                                                  
019800               ELSE                                                       
                        IF TULL-IDLANDX2 = 'CN'                                 
019700                     MOVE TULL-RETULF-2 TO SORTWS-RETULF-CN               
                        ELSE                                                    
019700                     MOVE TULL-RETULF-2 TO SORTWS-RETULF-US               
                        END-IF                                                  
020000               END-IF                                                     
020100             END-IF                                                       
020200             MOVE JA TO FLAGGA-02-SEG                                     
020300           END-IF                                                         
020400       END-EVALUATE                                                       
020500       PERFORM IMS-GET-WDF1                                               
020600     END-PERFORM                                                          
020700     IF SKRIV-POST AND FLAGGA-02-SEG = JA                                 
020800       PERFORM S31-RELEASE-W3358H                                         
020900     END-IF                                                               
021000     .                                                                    
021100     EJECT                                                                
021200 Z-FINIT SECTION.                                                         
021300     SKIP2                                                                
021400     MOVE 'S' TO POSTSUM-OPKOD                                            
021500     CALL POSTSUM USING POSTSUM-PARM                                      
021600     .                                                                    
021700     EJECT                                                                
021800 S31-RELEASE-W3358H SECTION.                                              
021900     SKIP2                                                                
022000     RELEASE SORT-POST FROM SORTWS-AREA                                   
022100                                                                          
022200     MOVE 'WDF1'  TO POSTSUM-TRANSTYP                                     
022300     MOVE 'W3358H' TO POSTSUM-FDNAMN                                      
022400     MOVE 'W3358HD1' TO POSTSUM-DDNAMN2                                   
022500     CALL POSTSUM USING POSTSUM-PARM                                      
022600     .                                                                    
022700     EJECT                                                                
022800 S99-ABEND SECTION.                                                       
022900     SKIP2                                                                
023000     SKIP2                                                                
023100     MOVE 'S' TO POSTSUM-OPKOD                                            
023200     CALL POSTSUM USING POSTSUM-PARM                                      
023300     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
023400     .                                                                    
023500     EJECT                                                                
023600* --- IMS SEKTIONER ---                                                   
023700     SKIP3                                                                
023800     EJECT                                                                
023900 IMS-GET-WDF1   SECTION.                                                  
024000     SKIP2                                                                
024100     CALL CBLTDLI USING GN WDF1-PCB DLI-IO-AREA                           
024200     MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
024300     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
024400     PERFORM IMS-STATUSKONTROLL                                           
024500     .                                                                    
024600     EJECT                                                                
024700 IMS-STATUSKONTROLL SECTION.                                              
024800     SKIP2                                                                
024900     SET STATUS-IX TO 1                                                   
025000     SEARCH GODK-STATUS                                                   
025100       AT END                                                             
025200         MOVE 'XXXXXXXXXXXXXX' TO FELTEXT-STR                             
025300         DISPLAY FELTEXT                                                  
025400         CALL FELLOG                                                      
025500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
025600         CONTINUE                                                         
025700     END-SEARCH                                                           
025800     .                                                                    
