000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     WDMR6200.                                                
000400*AUTHOR.         KARIN OLSSON.                                            
000500*DATE-WRITTEN.   92/03/19.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        SUGA UT INFORMATION TILL DATA MANAGER FRÅN PSB                   
001100*                                                                         
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
002500*          --- PSB MEDLEMMAR                                              
002600     SELECT WDMR61                     ASSIGN TO WDMR62D1.                
002700     SKIP2                                                                
002800*          --- INFORMATION FÖR VIDARE BEARBETNING                         
002900     SELECT WDMR62                     ASSIGN TO WDMR62D2.                
002901     SKIP2                                                                
002910*          --- INFORMATION FÖR VIDARE BEARBETNING                         
002920     SELECT WDMR63                     ASSIGN TO WDMR62D3.                
003000     SKIP2                                                                
003100     EJECT                                                                
003200 DATA DIVISION.                                                           
003300     SKIP3                                                                
003400 FILE SECTION.                                                            
003500     SKIP3                                                                
003600 FD  WDMR61                                                               
003700     RECORDING       F                                                    
003800     BLOCK CONTAINS  0.                                                   
003900     SKIP2                                                                
004000 01  FILLER                      PIC X(132).                              
004100     SKIP3                                                                
004200 FD  WDMR62                                                               
004300     RECORDING       F                                                    
004400     BLOCK CONTAINS  0.                                                   
004500     SKIP2                                                                
004600 01  UT-POST62                   PIC X(25).                               
004610     SKIP3                                                                
004620 FD  WDMR63                                                               
004630     RECORDING       F                                                    
004640     BLOCK CONTAINS  0.                                                   
004650     SKIP2                                                                
004660 01  UT-POST63                   PIC X(25).                               
004700     EJECT                                                                
004800 WORKING-STORAGE SECTION.                                                 
004900     SKIP2                                                                
004901                                                                          
004910*    -- CHECKED BY WY2000                                                 
005000 77  IDPGM                       PIC X(8)    VALUE 'WDMR6200'.            
005100 77  JA                          PIC X       VALUE 'J'.                   
005200 77  NEJ                         PIC X       VALUE 'N'.                   
005300 77  EXPECTING-PCB               PIC X       VALUE 'N'.                   
005400 77  EXPECTING-SENSEG            PIC X       VALUE 'N'.                   
005500 77  EXPECTING-CONT              PIC X       VALUE 'N'.                   
005600 77  PCB                         PIC X       VALUE 'P'.                   
005700 77  SENSEG                      PIC X       VALUE 'S'.                   
005800 77  INREF                       PIC X       VALUE 'I'.                   
005900 77  UPDREF                      PIC X       VALUE 'U'.                   
006000 77  OUTREF                      PIC X       VALUE 'O'.                   
006100 77  TRANSREF                    PIC X       VALUE 'T'.                   
006200 77  DBTYP                       PIC X(4)    VALUE 'DB'.                  
006300 77  TPTYP                       PIC X(4)    VALUE 'TP'.                  
006400 77  GSAMTYP                     PIC X(4)    VALUE 'GSAM'.                
006500                                                                          
006600 77  POST-KONTROLL-SW            PIC X       VALUE 'N'.                   
006700     88  POST-FEL                            VALUE 'N'.                   
006800     88  POST-OK                             VALUE 'J'.                   
006900                                                                          
007000 77  INFIL-EOF-SW                PIC X       VALUE 'N'.                   
007100     88  END-OF-INFIL                        VALUE 'J'.                   
007200                                                                          
007300     EJECT                                                                
007400 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
007500 01  FILLER REDEFINES DAGENS-DATUM.                                       
007600     03  DAGENS-DATUM-AAR        PIC 9(2).                                
007700     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
007800     03  DAGENS-DATUM-DAG        PIC 9(2).                                
007900     EJECT                                                                
008000 01  DYNAMISKA-SUBPROGRAM.                                                
008100*                                                                         
008200     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
008300     03  WDMR6210                PIC X(8)    VALUE 'WDMR6210'.            
008400     SKIP2                                                                
008500*    --- PARAMETRAR TILL ABEND                                            
008600                                                                          
008700 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
008800 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
008900     SKIP2                                                                
009000 01  FELTEXT.                                                             
009100     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
009200     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
009300     SKIP2                                                                
009400*    --- PARAMETRAR TILL WDMR6210                                         
009500 01  WRAD                        PIC X(70).                               
009600 01  WRAD-TYP                    PIC X.                                   
009700 01  WPCB-TYP                    PIC X(4).                                
009800 01  WNAMN                       PIC X(8).                                
009900 01  WREF                        PIC X.                                   
010000     EJECT                                                                
010100 01  IN-AREA-START               PIC X(24)   VALUE                        
010200                                 'IN-AREA-START  '.                       
010300     SKIP2                                                                
010410 01  IN-AREA                PIC X(132).                                   
010420 01  FILLER REDEFINES IN-AREA.                                            
010430     03                     PIC X(63).                                    
010440     03 IN-DSNAME-ORD       PIC X(14).                                    
010450     03 IN-DSN-MBR          PIC X(55).                                    
010460                                                                          
010470 01  FILLER REDEFINES IN-AREA.                                            
010480     03                     PIC X(1).                                     
010490     03 IN-REC-ORD          PIC X(3).                                     
010500     03                     PIC X(26).                                    
010600     03 PSB-RAD-INFO.                                                     
010800       05 IN-KOMMENTAR      PIC X.                                        
010900       05 IN-PSB-RAD        PIC X(70).                                    
011000       05 IN-CONT           PIC X.                                        
011100       05 FILLER            PIC X(8).                                     
011110                                                                          
011200 01  IN-PSB-MBR             PIC X(8).                                     
011400     EJECT                                                                
011500 01  UT-AREA-START               PIC X(24)   VALUE                        
011600                                 'UT-AREA-START  '.                       
011700     SKIP2                                                                
011800 01  UT-AREA.                                                             
011900     03 UT-PSB-MBR               PIC X(12).                               
012000     03 UT-REF                   PIC X.                                   
012100     03 UT-NAMN                  PIC X(12).                               
012200     EJECT                                                                
012300 01  SPAR-PSB-MBR                PIC X(8).                                
012400 01  SPAR-RAD-TYP                PIC X.                                   
012500 01  SPAR-PCB-TYP                PIC X(4).                                
012600 01  SPAR-NAMN                   PIC X(8).                                
012700 01  SPAR-REF                    PIC X.                                   
012800     SKIP2                                                                
012900 01  WPAR-REF                    PIC X.                                   
013000     EJECT                                                                
013100 PROCEDURE DIVISION.                                                      
013200                                                                          
013300 STYR SECTION.                                                            
013400                                                                          
013500     PERFORM A-INIT                                                       
013600                                                                          
013700     PERFORM S01-LAES-INFIL                                               
013800     PERFORM UNTIL END-OF-INFIL                                           
013900       SET POST-OK TO TRUE                                                
014000       EVALUATE TRUE                                                      
014010         WHEN IN-DSNAME-ORD = 'Data Set Name:'                            
014020           MOVE ZERO TO TALLY                                             
014030           INSPECT IN-DSN-MBR TALLYING TALLY FOR                          
014040             CHARACTERS BEFORE INITIAL '('                                
014050           ADD 2 TO TALLY                                                 
014060           UNSTRING IN-DSN-MBR DELIMITED BY ')'                           
014070             INTO IN-PSB-MBR                                              
014080             WITH POINTER TALLY                                           
014200           IF IN-PSB-MBR NOT = SPAR-PSB-MBR                               
014300             MOVE IN-PSB-MBR TO SPAR-PSB-MBR                              
014400             MOVE JA TO EXPECTING-PCB                                     
014500             MOVE NEJ TO EXPECTING-SENSEG                                 
014600             MOVE NEJ TO EXPECTING-CONT                                   
014700           END-IF                                                         
014710         WHEN IN-REC-ORD = 'Rec' OR 'REC'                                 
014800           IF IN-KOMMENTAR = '*'                                          
014900             CONTINUE                                                     
015000           ELSE                                                           
015100             PERFORM B-UNDERSOEK-RAD                                      
015110           END-IF                                                         
015200       END-EVALUATE                                                       
015300       PERFORM S01-LAES-INFIL                                             
015400     END-PERFORM                                                          
015500                                                                          
015600     PERFORM Z-FINIT                                                      
015700                                                                          
015800     MOVE ZERO TO RETURN-CODE                                             
015900     GOBACK                                                               
016000     .                                                                    
016100     EJECT                                                                
016200 A-INIT  SECTION.                                                         
016300                                                                          
016400     OPEN INPUT  WDMR61                                                   
016500     OPEN OUTPUT WDMR62                                                   
016510                 WDMR63                                                   
016600                                                                          
016700     ACCEPT DAGENS-DATUM  FROM DATE                                       
016800     .                                                                    
016900     EJECT                                                                
017000 B-UNDERSOEK-RAD  SECTION.                                                
017100     SKIP2                                                                
017200     MOVE IN-PSB-RAD TO WRAD                                              
017300     MOVE SPACE TO WRAD-TYP WPCB-TYP WNAMN WREF                           
017400     IF EXPECTING-CONT = JA                                               
017500       MOVE SPAR-RAD-TYP TO WRAD-TYP                                      
017600     ELSE                                                                 
017700       MOVE SPACE TO SPAR-RAD-TYP SPAR-PCB-TYP SPAR-NAMN SPAR-REF         
017800     END-IF                                                               
017900                                                                          
018000     CALL WDMR6210 USING WRAD WRAD-TYP WPCB-TYP WNAMN WREF                
018100                                                                          
018200     IF EXPECTING-CONT = JA                                               
018300       IF SPAR-PCB-TYP NOT = SPACE                                        
018400         MOVE SPAR-PCB-TYP TO WPCB-TYP                                    
018500       END-IF                                                             
018600       IF SPAR-NAMN NOT = SPACE                                           
018700         MOVE SPAR-NAMN TO WNAMN                                          
018800       END-IF                                                             
018900       IF SPAR-REF NOT = SPACE                                            
019000         MOVE SPAR-REF TO WREF                                            
019100       END-IF                                                             
019200     END-IF                                                               
019300                                                                          
019400     EVALUATE TRUE                                                        
019500       WHEN WRAD-TYP = SPACE                                              
019600         CONTINUE                                                         
019700       WHEN WRAD-TYP = PCB                                                
019800         IF EXPECTING-PCB = JA                                            
019900           PERFORM BA-BEHANDLA-PCB                                        
020000         ELSE                                                             
020100           STRING 'FICK PSB NÄR SENSEG VAR VÄNTAT. AKTUELLT PSB '         
020200                  DELIMITED BY SIZE                                       
020300                  SPAR-PSB-MBR DELIMITED BY SPACE                         
020400             INTO FELTEXT-STR                                             
020500           DISPLAY FELTEXT                                                
020600           PERFORM S99-ABEND                                              
020700         END-IF                                                           
020800       WHEN WRAD-TYP = SENSEG                                             
020900         IF EXPECTING-SENSEG = JA                                         
021000           PERFORM BB-BEHANDLA-SENSEG                                     
021100         ELSE                                                             
021200           STRING 'FICK SENSEG NÄR PSB VAR VÄNTAT. AKTUELLT PSB '         
021300                  DELIMITED BY SIZE                                       
021400                  SPAR-PSB-MBR DELIMITED BY SPACE                         
021500             INTO FELTEXT-STR                                             
021600           DISPLAY FELTEXT                                                
021700           PERFORM S99-ABEND                                              
021800         END-IF                                                           
021900       WHEN OTHER                                                         
022000         STRING 'OKÄND RADTYP ' WRAD-TYP ' AKTUELLT PSB '                 
022100                DELIMITED BY SIZE                                         
022200                SPAR-PSB-MBR DELIMITED BY SPACE                           
022300           INTO FELTEXT-STR                                               
022400         DISPLAY FELTEXT                                                  
022500         PERFORM S99-ABEND                                                
022600     END-EVALUATE                                                         
022700     .                                                                    
022800     EJECT                                                                
022900 BA-BEHANDLA-PCB SECTION.                                                 
023000     SKIP2                                                                
023100     IF IN-CONT NOT = SPACE                                               
023200       MOVE JA TO EXPECTING-CONT                                          
023300       MOVE WRAD-TYP TO SPAR-RAD-TYP                                      
023400       MOVE WPCB-TYP TO SPAR-PCB-TYP                                      
023500       MOVE WNAMN TO SPAR-NAMN                                            
023600       MOVE WREF  TO SPAR-REF                                             
023700     ELSE                                                                 
023800       MOVE NEJ TO EXPECTING-CONT                                         
023900       MOVE SPACE TO UT-AREA                                              
024000       EVALUATE TRUE                                                      
024100         WHEN WPCB-TYP = DBTYP                                            
024200           MOVE WREF TO WPAR-REF                                          
024300           MOVE JA TO  EXPECTING-SENSEG                                   
024400           MOVE NEJ TO EXPECTING-PCB                                      
024500         WHEN WPCB-TYP = TPTYP                                            
024600           IF WNAMN NOT = SPACE                                           
024700             STRING SPAR-PSB-MBR DELIMITED BY SPACE                       
024800               '-PSB' DELIMITED BY SIZE                                   
024900               INTO UT-PSB-MBR                                            
025000             MOVE TRANSREF TO UT-REF                                      
025100             MOVE WNAMN TO UT-NAMN                                        
025200             PERFORM S11-SKRIV-WDMR62                                     
025210             PERFORM S12-SKRIV-WDMR63                                     
025300           END-IF                                                         
025400           MOVE NEJ TO  EXPECTING-SENSEG                                  
025500         WHEN WPCB-TYP = GSAMTYP                                          
025600           STRING SPAR-PSB-MBR DELIMITED BY SPACE                         
025700             '-PSB' DELIMITED BY SIZE                                     
025800             INTO UT-PSB-MBR                                              
025900           MOVE OUTREF TO UT-REF                                          
026000           MOVE WNAMN TO UT-NAMN                                          
026100           PERFORM S11-SKRIV-WDMR62                                       
026200           MOVE NEJ TO  EXPECTING-SENSEG                                  
026300         WHEN OTHER                                                       
026400           STRING 'OKÄND PCBTYP ' WPCB-TYP ' AKTUELLT PSB '               
026500                  DELIMITED BY SIZE                                       
026600                  SPAR-PSB-MBR DELIMITED BY SPACE                         
026700             INTO FELTEXT-STR                                             
026800           DISPLAY FELTEXT                                                
026900           PERFORM S99-ABEND                                              
027000       END-EVALUATE                                                       
027100     END-IF                                                               
027200     .                                                                    
027300     EJECT                                                                
027400 BB-BEHANDLA-SENSEG SECTION.                                              
027500     SKIP2                                                                
027600     IF IN-CONT NOT = SPACE                                               
027700       MOVE JA TO EXPECTING-CONT                                          
027800       MOVE NEJ TO  EXPECTING-PCB                                         
027900       MOVE WRAD-TYP TO SPAR-RAD-TYP                                      
028000       MOVE WNAMN TO SPAR-NAMN                                            
028100       MOVE WREF  TO SPAR-REF                                             
028200     ELSE                                                                 
028300       MOVE NEJ TO EXPECTING-CONT                                         
028400       MOVE JA TO  EXPECTING-PCB                                          
028500       MOVE SPACE TO UT-AREA                                              
028600       STRING SPAR-PSB-MBR DELIMITED BY SPACE                             
028700         '-PSB' DELIMITED BY SIZE                                         
028800         INTO UT-PSB-MBR                                                  
028900       IF WREF = SPACE                                                    
029000         MOVE WPAR-REF TO UT-REF                                          
029100       ELSE                                                               
029200         MOVE WREF TO UT-REF                                              
029300       END-IF                                                             
029400       STRING WNAMN DELIMITED BY SPACE                                    
029500         '-CTX' DELIMITED BY SIZE                                         
029600         INTO UT-NAMN                                                     
029700       PERFORM S11-SKRIV-WDMR62                                           
029800     END-IF                                                               
029900     .                                                                    
030000     EJECT                                                                
030100 Z-FINIT SECTION.                                                         
030200     SKIP2                                                                
030300     CLOSE WDMR61 WDMR62 WDMR63                                           
030400     .                                                                    
030500     EJECT                                                                
030600 S01-LAES-INFIL  SECTION.                                                 
030700     SKIP2                                                                
030800     READ WDMR61 INTO IN-AREA                                             
030900       AT END                                                             
031000          SET END-OF-INFIL TO TRUE                                        
031100       END-READ                                                           
031200     .                                                                    
031300     SKIP3                                                                
031400 S11-SKRIV-WDMR62 SECTION.                                                
031500     SKIP2                                                                
031600     IF POST-OK                                                           
031700       WRITE UT-POST62 FROM UT-AREA                                       
031800     END-IF                                                               
031900     .                                                                    
032000     SKIP3                                                                
032010 S12-SKRIV-WDMR63 SECTION.                                                
032020     SKIP2                                                                
032030     IF POST-OK                                                           
032040       WRITE UT-POST63 FROM UT-AREA                                       
032050     END-IF                                                               
032060     .                                                                    
032070     EJECT                                                                
032100 S99-ABEND SECTION.                                                       
032200     SKIP2                                                                
032300     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
032400     .                                                                    
