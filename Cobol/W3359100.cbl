000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W3359100.                                                
000400 AUTHOR.         ELEONOR ÖSTRÖM.                                          
000500 DATE-WRITTEN.   02/12/05.                                                
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
002600     SELECT W33591                     ASSIGN TO W33591D1.                
002700     SKIP2                                                                
003100*          --- SORTERINGSFIL                                              
003200     SELECT SORTFIL                    ASSIGN TO W33591DS.                
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500     SKIP3                                                                
003600 FILE SECTION.                                                            
003700     SKIP3                                                                
003800***EÖ GÖR OM UTFILEN + SORTFILEN PGA ATT BARA EN RETULF ÄR MED            
003900 FD  W33591                                                               
004000     RECORDING       F                                                    
004100     BLOCK CONTAINS  0.                                                   
004200     SKIP2                                                                
004300*01  POST -COPY W33591 -PRE  UT-  -L.                                     
004400     SKIP3                                                                
005100 SD  SORTFIL                                                              
005200     RECORDING       F                                                    
005300     SKIP2                                                                
005400*01  POST -COPY W33591 -PRE  SORT-                                        
005500     EJECT                                                                
005600 WORKING-STORAGE SECTION.                                                 
005700                                                                          
005800                                                                          
005900*    -- CHECKED BY WY2000                                                 
006000 77  IDPGM                       PIC X(8)    VALUE 'W3359100'.            
006100 77  JA                          PIC X       VALUE 'J'.                   
006200 77  NEJ                         PIC X       VALUE 'N'.                   
006300 77  FLAGGA-02-SEG-SE            PIC X       VALUE 'N'.                   
006310 77  FLAGGA-02-SEG-CN            PIC X       VALUE 'N'.                   
006400                                                                          
006500 77  SKRIV-SW-SE                PIC X       VALUE 'N'.                    
006600     88  SKRIV-POST-SE                       VALUE 'J'.                   
006800 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006900 01  FILLER REDEFINES DAGENS-DATUM.                                       
007000     03  DAGENS-DATUM-AAR        PIC 9(2).                                
007100     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
007200     03  DAGENS-DATUM-DAG        PIC 9(2).                                
007300     EJECT                                                                
007400 01  DYNAMISKA-SUBPROGRAM.                                                
007500*                                                                         
007600     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
007700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007900     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
008000     SKIP2                                                                
008100*    --- PARAMETRAR TILL ABEND                                            
008200                                                                          
008300 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
008400 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
008500     SKIP2                                                                
008600 01  FELTEXT.                                                             
008700     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
008800     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
008900     EJECT                                                                
009000*    --- PARAMETRAR TILL POSTSUM                                          
009100*                                                                         
009200*01  -COPY W0005   -PRE  POSTSUM-                                         
009300     EJECT                                                                
009400 01  SORTWS-AREA-START           PIC X(24)   VALUE                        
009500                                 'SORTWS-AREA-START'.                     
009600     SKIP2                                                                
009700                                                                          
009800*01  AREA -COPY W33591     -PRE SORTWS-                                   
009900     SKIP2                                                                
010000 01  SORT-RETURN-X               PIC X(2)  VALUE SPACE.                   
010100     EJECT                                                                
010200*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
010300*                                                                         
010400     SKIP2                                                                
010500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010600     SKIP3                                                                
010700 01  NYCKLAR-TILL-DLI.                                                    
010800     03  W-IDLEVNR-X.                                                     
010900         05  W-IDLEVNR           PIC X(5)   VALUE SPACE.                  
011000     SKIP2                                                                
011100*    --- STATUS-KOD FRÅN IMS                                              
011200 01  STATUS-WS                   PIC XX.                                  
011300     88  SEGMENT-FINNS                       VALUE '  '.                  
011400     88  SEGMENT-SLUT                        VALUE 'GB'.                  
011500     SKIP2                                                                
011600 01  GODK-STATUSKODER.                                                    
011700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
011800     SKIP3                                                                
011900 01  SSA1                        PIC X(64).                               
012000 01  SSA2                        PIC X(64).                               
012100     EJECT                                                                
012200*    --- IMS FUNKTIONSKODER                                               
012300*01  -COPY W0003                                                          
012400     EJECT                                                                
012500*    ---  DLI INPUT-OUTPUT AREA                                           
012600 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
012700     SKIP3                                                                
012800 01  DLI-IO-AREA.                                                         
012900     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
013000     SKIP3                                                                
013100     03  WDF101 REDEFINES IO-AREA.                                        
013200*        05  -COPY WDF101                                                 
013300     EJECT                                                                
013400     03  WDF102 REDEFINES IO-AREA.                                        
013500*        05  -COPY WDF102                                                 
013600     EJECT                                                                
013700 LINKAGE SECTION.                                                         
013800                                                                          
013900     SKIP2                                                                
014000*01  -COPY W0008  -PRE WDF1-                                              
014100     05  FILLER                  PIC X.                                   
014200     EJECT                                                                
014300 PROCEDURE DIVISION  USING WDF1-PCB.                                      
014400     ENTRY 'DLITCBL' USING WDF1-PCB.                                      
014500                                                                          
014600     SKIP2                                                                
014700     PERFORM A-INIT                                                       
014800                                                                          
014900     SORT SORTFIL ASCENDING KEY SORT-IDLEVNR                              
015000                  INPUT PROCEDURE B-SORT-INPUT                            
015100                  GIVING W33591                                           
015200                                                                          
015300     IF SORT-RETURN NOT = 0                                               
015400       MOVE SORT-RETURN TO SORT-RETURN-X                                  
015500       STRING 'RETURKOD ' SORT-RETURN-X ' FRÅN SORT'                      
015600           DELIMITED BY SIZE                                              
015700           INTO FELTEXT-STR                                               
015800       DISPLAY FELTEXT                                                    
015900       PERFORM S99-ABEND                                                  
016000     ELSE                                                                 
016100       PERFORM Z-FINIT                                                    
016200                                                                          
016300       MOVE ZERO TO RETURN-CODE                                           
016400       GOBACK                                                             
016500     END-IF                                                               
016600                                                                          
016700     .                                                                    
016800     EJECT                                                                
016900 A-INIT SECTION.                                                          
017000     SKIP2                                                                
017100     ACCEPT DAGENS-DATUM  FROM DATE                                       
017200     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
017300     PERFORM AA-NOLLSTAELL-UTPOST-SE                                      
017400     .                                                                    
017500     EJECT                                                                
017600 AA-NOLLSTAELL-UTPOST-SE SECTION.                                         
017700     SKIP2                                                                
017800     MOVE ZERO TO  SORTWS-RETULF                                          
017900     MOVE SPACE TO SORTWS-IDLEVNR                                         
018000     MOVE SPACE TO SORTWS-IDLEVNR-MOTSV                                   
018001     .                                                                    
018002     EJECT                                                                
018300 B-SORT-INPUT SECTION.                                                    
018400     SKIP2                                                                
018500     PERFORM IMS-GET-WDF1                                                 
018600     PERFORM UNTIL SEGMENT-SLUT                                           
018700       EVALUATE WDF1-SEG-NAME-FB                                          
018800         WHEN 'WDF101  '                                                  
018900           IF SKRIV-POST-SE AND FLAGGA-02-SEG-SE = JA                     
019000             PERFORM S31-RELEASE-W33591                                   
019100             PERFORM AA-NOLLSTAELL-UTPOST-SE                              
019200           END-IF                                                         
019300           IF LEV-IDLEVNR NOT = SPACE                                     
019400             MOVE LEV-IDLEVNR       TO SORTWS-IDLEVNR                     
019500             MOVE LEV-IDLEVNR-MOTSV TO SORTWS-IDLEVNR-MOTSV               
019600             MOVE JA              TO SKRIV-SW-SE                          
019700           ELSE                                                           
019800             MOVE NEJ TO SKRIV-SW-SE                                      
019900           END-IF                                                         
020000           MOVE NEJ TO FLAGGA-02-SEG-SE                                   
020100         WHEN 'WDF102  '                                                  
020110           IF TULL-IDLANDX2 = 'SE'                                        
020200             IF SKRIV-POST-SE                                             
020300               IF DAGENS-DATUM > TULL-TITULF OR                           
020400                  DAGENS-DATUM = TULL-TITULF                              
020500                  MOVE TULL-RETULF-1 TO SORTWS-RETULF                     
020600               ELSE                                                       
020700                  MOVE TULL-RETULF-2 TO SORTWS-RETULF                     
020800               END-IF                                                     
020900             END-IF                                                       
021000             MOVE JA TO FLAGGA-02-SEG-SE                                  
021010           END-IF                                                         
021100       END-EVALUATE                                                       
021200       PERFORM IMS-GET-WDF1                                               
021300     END-PERFORM                                                          
021400     IF SKRIV-POST-SE AND FLAGGA-02-SEG-SE = JA                           
021500       PERFORM S31-RELEASE-W33591                                         
021600     END-IF                                                               
021700     .                                                                    
021800     EJECT                                                                
021900 Z-FINIT SECTION.                                                         
022000     SKIP2                                                                
022100     MOVE 'S' TO POSTSUM-OPKOD                                            
022200     CALL POSTSUM USING POSTSUM-PARM                                      
022300     .                                                                    
022400     EJECT                                                                
022500 S31-RELEASE-W33591 SECTION.                                              
022600     SKIP2                                                                
022700     RELEASE SORT-POST FROM SORTWS-AREA                                   
022800                                                                          
022900     MOVE 'WDF1'  TO POSTSUM-TRANSTYP                                     
023000     MOVE 'W33591' TO POSTSUM-FDNAMN                                      
023100     MOVE 'W33591D1' TO POSTSUM-DDNAMN2                                   
023200     CALL POSTSUM USING POSTSUM-PARM                                      
023300     .                                                                    
023400     EJECT                                                                
023600 S99-ABEND SECTION.                                                       
023700     SKIP2                                                                
023800     SKIP2                                                                
023900     MOVE 'S' TO POSTSUM-OPKOD                                            
024000     CALL POSTSUM USING POSTSUM-PARM                                      
024100     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
024200     .                                                                    
024300     EJECT                                                                
024400* --- IMS SEKTIONER ---                                                   
024500     SKIP3                                                                
024600     EJECT                                                                
024700 IMS-GET-WDF1   SECTION.                                                  
024800     SKIP2                                                                
024900     CALL CBLTDLI USING GN WDF1-PCB DLI-IO-AREA                           
025000     MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
025100     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
025200     PERFORM IMS-STATUSKONTROLL                                           
025300     .                                                                    
025400     EJECT                                                                
025500 IMS-STATUSKONTROLL SECTION.                                              
025600     SKIP2                                                                
025700     SET STATUS-IX TO 1                                                   
025800     SEARCH GODK-STATUS                                                   
025900       AT END                                                             
026000         MOVE 'XXXXXXXXXXXXXX' TO FELTEXT-STR                             
026100         DISPLAY FELTEXT                                                  
026200         CALL FELLOG                                                      
026300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
026400         CONTINUE                                                         
026500     END-SEARCH                                                           
026600     .                                                                    
