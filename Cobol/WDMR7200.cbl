000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     WDMR7200.                                                
000400*AUTHOR.         KARIN OLSSON.                                            
000500*DATE-WRITTEN.   92/03/19.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        SUGA UT INFORMATION TILL DATA MANAGER FRÅN DBD                   
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
002500*          --- DBD MEDLEMMAR                                              
002600     SELECT WDMR71                     ASSIGN TO WDMR72D1.                
002700     SKIP2                                                                
002800*          --- INFORMATION FÖR VIDARE BEARBETNING                         
002900     SELECT WDMR72                     ASSIGN TO WDMR72D2.                
003000     SKIP2                                                                
003100     EJECT                                                                
003200 DATA DIVISION.                                                           
003300     SKIP3                                                                
003400 FILE SECTION.                                                            
003500     SKIP3                                                                
003600 FD  WDMR71                                                               
003700     RECORDING       F                                                    
003800     BLOCK CONTAINS  0.                                                   
003900     SKIP2                                                                
004000 01  FILLER                      PIC X(132).                              
004100     SKIP3                                                                
004200 FD  WDMR72                                                               
004300     RECORDING       F                                                    
004400     BLOCK CONTAINS  0.                                                   
004500     SKIP2                                                                
004600 01  UT-POST                     PIC X(36).                               
004700     EJECT                                                                
004800 WORKING-STORAGE SECTION.                                                 
004900     SKIP2                                                                
004901                                                                          
004910*    -- CHECKED BY WY2000                                                 
005000 77  IDPGM                       PIC X(8)    VALUE 'WDMR7200'.            
005100 77  JA                          PIC X       VALUE 'J'.                   
005200 77  NEJ                         PIC X       VALUE 'N'.                   
005300 77  LOG-DBD                     PIC X       VALUE 'J'.                   
005310 77  EXPECTING-DBD               PIC X       VALUE 'N'.                   
005400 77  EXPECTING-SEGM              PIC X       VALUE 'N'.                   
005500 77  EXPECTING-CONT              PIC X       VALUE 'N'.                   
005600 77  DBD                         PIC X       VALUE 'D'.                   
005700 77  SEGM                        PIC X       VALUE 'S'.                   
005810 77  LOGISKT                     PIC X       VALUE 'L'.                   
005820 77  FYSISKT                     PIC X       VALUE 'F'.                   
005900                                                                          
006000 77  POST-KONTROLL-SW            PIC X       VALUE 'N'.                   
006100     88  POST-FEL                            VALUE 'N'.                   
006200     88  POST-OK                             VALUE 'J'.                   
006300                                                                          
006400 77  INFIL-EOF-SW                PIC X       VALUE 'N'.                   
006500     88  END-OF-INFIL                        VALUE 'J'.                   
006600                                                                          
006700     EJECT                                                                
006800 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006900 01  FILLER REDEFINES DAGENS-DATUM.                                       
007000     03  DAGENS-DATUM-AAR        PIC 9(2).                                
007100     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
007200     03  DAGENS-DATUM-DAG        PIC 9(2).                                
007300     EJECT                                                                
007400 01  DYNAMISKA-SUBPROGRAM.                                                
007500*                                                                         
007600     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
007700     03  WDMR7210                PIC X(8)    VALUE 'WDMR7210'.            
007800     SKIP2                                                                
007900*    --- PARAMETRAR TILL ABEND                                            
008000                                                                          
008100 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
008200 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
008300     SKIP2                                                                
008400 01  FELTEXT.                                                             
008500     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
008600     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
008700     SKIP2                                                                
008800*    --- PARAMETRAR TILL WDMR7210                                         
008900 01  WRAD                        PIC X(70).                               
009000 01  WRAD-TYP                    PIC X.                                   
009100 01  WDBD-TYP                    PIC X(4).                                
009200 01  WLNAMN                      PIC X(8).                                
009300 01  WFNAMN1                     PIC X(8).                                
009400 01  WFNAMN2                     PIC X(8).                                
009500     EJECT                                                                
009600 01  IN-AREA-START               PIC X(24)   VALUE                        
009700                                 'IN-AREA-START  '.                       
009800     SKIP2                                                                
009810 01  IN-AREA                PIC X(132).                                   
009820 01  FILLER REDEFINES IN-AREA.                                            
009830     03                     PIC X(63).                                    
009840     03 IN-DSNAME-ORD       PIC X(14).                                    
009850     03 IN-DSN-MBR          PIC X(55).                                    
009860                                                                          
009870 01  FILLER REDEFINES IN-AREA.                                            
009880     03                     PIC X(1).                                     
009890     03 IN-REC-ORD          PIC X(3).                                     
009891     03                     PIC X(26).                                    
009892     03 PSB-RAD-INFO.                                                     
009893       05 IN-KOMMENTAR      PIC X.                                        
009894       05 IN-DBD-RAD        PIC X(70).                                    
009895       05 IN-CONT           PIC X.                                        
009896       05 FILLER            PIC X(8).                                     
009900                                                                          
010880 01  IN-DBD-MBR             PIC X(8).                                     
010900     EJECT                                                                
011000 01  UT-AREA-START               PIC X(24)   VALUE                        
011100                                 'UT-AREA-START  '.                       
011200     SKIP2                                                                
011300 01  UT-AREA.                                                             
011600     03 UT-LNAMN                 PIC X(12).                               
011700     03 UT-FNAMN1                PIC X(12).                               
011800     03 UT-FNAMN2                PIC X(12).                               
011900     EJECT                                                                
012000 01  SPAR-DBD-MBR                PIC X(8).                                
012100 01  SPAR-RAD-TYP                PIC X.                                   
012200 01  SPAR-DBD-TYP                PIC X(4).                                
012300 01  SPAR-LNAMN                  PIC X(8).                                
012400 01  SPAR-FNAMN1                 PIC X(8).                                
012500 01  SPAR-FNAMN2                 PIC X(8).                                
012600     SKIP2                                                                
012700     EJECT                                                                
012800 PROCEDURE DIVISION.                                                      
012900                                                                          
013000 STYR SECTION.                                                            
013100                                                                          
013200     PERFORM A-INIT                                                       
013300                                                                          
013400     PERFORM S01-LAES-INFIL                                               
013500     PERFORM UNTIL END-OF-INFIL                                           
013600       SET POST-OK TO TRUE                                                
013700       EVALUATE TRUE                                                      
013710         WHEN IN-DSNAME-ORD = 'Data Set Name:'                            
013720           MOVE ZERO TO TALLY                                             
013730           INSPECT IN-DSN-MBR TALLYING TALLY FOR                          
013740             CHARACTERS BEFORE INITIAL '('                                
013750           ADD 2 TO TALLY                                                 
013760           UNSTRING IN-DSN-MBR DELIMITED BY ')'                           
013770             INTO IN-DBD-MBR                                              
013780             WITH POINTER TALLY                                           
013781           IF IN-DBD-MBR NOT = SPAR-DBD-MBR                               
013782             MOVE IN-DBD-MBR TO SPAR-DBD-MBR                              
013783             MOVE JA TO EXPECTING-DBD                                     
013784             MOVE JA TO LOG-DBD                                           
013785             MOVE NEJ TO EXPECTING-SEGM                                   
013786             MOVE NEJ TO EXPECTING-CONT                                   
013787           END-IF                                                         
013796         WHEN IN-REC-ORD = 'Rec'OR 'REC'                                  
014500           IF IN-KOMMENTAR = '*'                                          
014600             CONTINUE                                                     
014700           ELSE                                                           
014710             IF LOG-DBD = JA                                              
014800               PERFORM B-UNDERSOEK-RAD                                    
014810             END-IF                                                       
014820           END-IF                                                         
014900       END-EVALUATE                                                       
015000       PERFORM S01-LAES-INFIL                                             
015100     END-PERFORM                                                          
015200                                                                          
015300     PERFORM Z-FINIT                                                      
015400                                                                          
015500     MOVE ZERO TO RETURN-CODE                                             
015600     GOBACK                                                               
015700     .                                                                    
015800     EJECT                                                                
015900 A-INIT  SECTION.                                                         
016000                                                                          
016100     OPEN INPUT  WDMR71                                                   
016200     OPEN OUTPUT WDMR72                                                   
016300                                                                          
016400     ACCEPT DAGENS-DATUM  FROM DATE                                       
016500     .                                                                    
016600     EJECT                                                                
016700 B-UNDERSOEK-RAD  SECTION.                                                
016800     SKIP2                                                                
016900     MOVE IN-DBD-RAD TO WRAD                                              
017000     MOVE SPACE TO WRAD-TYP WDBD-TYP WLNAMN WFNAMN1 WFNAMN2               
017100     IF EXPECTING-CONT = JA                                               
017200       MOVE SPAR-RAD-TYP TO WRAD-TYP                                      
017300     ELSE                                                                 
017400       MOVE SPACE TO SPAR-RAD-TYP SPAR-DBD-TYP SPAR-LNAMN                 
017410                     SPAR-FNAMN1 SPAR-FNAMN2                              
017500     END-IF                                                               
017600                                                                          
017700     CALL WDMR7210 USING WRAD WRAD-TYP WDBD-TYP WLNAMN                    
017710                         WFNAMN1 WFNAMN2                                  
017800                                                                          
017900     IF EXPECTING-CONT = JA                                               
018000       IF SPAR-DBD-TYP NOT = SPACE                                        
018100         MOVE SPAR-DBD-TYP TO WDBD-TYP                                    
018200       END-IF                                                             
018300       IF SPAR-LNAMN NOT = SPACE                                          
018400         MOVE SPAR-LNAMN TO WLNAMN                                        
018500       END-IF                                                             
018600       IF SPAR-FNAMN1 NOT = SPACE                                         
018700         MOVE SPAR-FNAMN1 TO WFNAMN1                                      
018800       END-IF                                                             
018810       IF SPAR-FNAMN2 NOT = SPACE                                         
018820         MOVE SPAR-FNAMN2 TO WFNAMN2                                      
018830       END-IF                                                             
018900     END-IF                                                               
019000                                                                          
019100     EVALUATE TRUE                                                        
019200       WHEN WRAD-TYP = SPACE                                              
019300         CONTINUE                                                         
019400       WHEN WRAD-TYP = DBD                                                
019500         IF EXPECTING-DBD = JA                                            
019600           PERFORM BA-BEHANDLA-DBD                                        
019700         ELSE                                                             
019710           STRING 'FICK DBD NÄR ETT SEGM VÄNTADES. AKTUELLT DBD '         
019720                  DELIMITED BY SIZE                                       
019730                  SPAR-DBD-MBR DELIMITED BY SPACE                         
019740             INTO FELTEXT-STR                                             
019750           DISPLAY FELTEXT                                                
019760           PERFORM S99-ABEND                                              
019900         END-IF                                                           
020000       WHEN WRAD-TYP = SEGM                                               
020100         IF EXPECTING-SEGM = JA                                           
020200           PERFORM BB-BEHANDLA-SEGM                                       
020300         ELSE                                                             
020410           STRING 'FICK SEGM NÄR ETT DBD VÄNTADES. AKTUELLT DBD '         
020420                  DELIMITED BY SIZE                                       
020430                  SPAR-DBD-MBR DELIMITED BY SPACE                         
020440             INTO FELTEXT-STR                                             
020450           DISPLAY FELTEXT                                                
020460           PERFORM S99-ABEND                                              
020500         END-IF                                                           
020600       WHEN OTHER                                                         
020610         STRING 'OKÄND RADTYP ' WRAD-TYP                                  
020620                ' AKTUELLT DBD ' DELIMITED BY SIZE                        
020630                SPAR-DBD-MBR DELIMITED BY SPACE                           
020640           INTO FELTEXT-STR                                               
020650         DISPLAY FELTEXT                                                  
020800         PERFORM S99-ABEND                                                
020900     END-EVALUATE                                                         
021000     .                                                                    
021100     EJECT                                                                
021200 BA-BEHANDLA-DBD SECTION.                                                 
021300     SKIP2                                                                
021400     IF IN-CONT NOT = SPACE                                               
021500       MOVE JA TO EXPECTING-CONT                                          
021600       MOVE WRAD-TYP TO SPAR-RAD-TYP                                      
021700       MOVE WDBD-TYP TO SPAR-DBD-TYP                                      
021800       MOVE WLNAMN TO SPAR-LNAMN                                          
022000     ELSE                                                                 
022100       MOVE NEJ TO EXPECTING-CONT                                         
022200       MOVE SPACE TO UT-AREA                                              
022300       EVALUATE TRUE                                                      
022400         WHEN WDBD-TYP = LOGISKT                                          
022600           MOVE JA TO  EXPECTING-SEGM                                     
022700           MOVE NEJ TO EXPECTING-DBD                                      
022800         WHEN WDBD-TYP = FYSISKT                                          
023000           MOVE NEJ TO LOG-DBD                                            
023400         WHEN OTHER                                                       
023410           STRING SPAR-DBD-MBR DELIMITED BY SPACE                         
023500                  ' HAR OKÄND DBD-TYP ' DELIMITED BY SIZE                 
023510                  WDBD-TYP DELIMITED BY SIZE                              
023520             INTO FELTEXT-STR                                             
023530           DISPLAY FELTEXT                                                
023540           PERFORM S99-ABEND                                              
023600       END-EVALUATE                                                       
023700     END-IF                                                               
023800     .                                                                    
023900     EJECT                                                                
024000 BB-BEHANDLA-SEGM SECTION.                                                
024100     SKIP2                                                                
024200     IF IN-CONT NOT = SPACE                                               
024300       MOVE JA TO EXPECTING-CONT                                          
024400       MOVE NEJ TO  EXPECTING-DBD                                         
024500       MOVE WRAD-TYP TO SPAR-RAD-TYP                                      
024600       MOVE WLNAMN TO SPAR-LNAMN                                          
024700       MOVE WFNAMN1 TO SPAR-FNAMN1                                        
024710       MOVE WFNAMN2 TO SPAR-FNAMN2                                        
024800     ELSE                                                                 
024900       MOVE NEJ TO EXPECTING-CONT                                         
025000       MOVE JA TO  EXPECTING-DBD                                          
025100       MOVE SPACE TO UT-AREA                                              
025400       STRING WLNAMN DELIMITED BY SPACE                                   
025410         '-CTX' DELIMITED BY SIZE                                         
025420         INTO UT-LNAMN                                                    
025430       STRING WFNAMN1 DELIMITED BY SPACE                                  
025440         '-CTX' DELIMITED BY SIZE                                         
025450         INTO UT-FNAMN1                                                   
025500       IF WFNAMN2 NOT = SPACE                                             
025501         STRING WFNAMN2 DELIMITED BY SPACE                                
025502           '-CTX' DELIMITED BY SIZE                                       
025503           INTO UT-FNAMN2                                                 
025510       END-IF                                                             
026000       PERFORM S11-SKRIV-UTFIL                                            
026100     END-IF                                                               
026200     .                                                                    
026300     EJECT                                                                
026400 Z-FINIT SECTION.                                                         
026500     SKIP2                                                                
026600     CLOSE WDMR71 WDMR72                                                  
026700     .                                                                    
026800     EJECT                                                                
026900 S01-LAES-INFIL  SECTION.                                                 
027000     SKIP2                                                                
027100     READ WDMR71 INTO IN-AREA                                             
027200       AT END                                                             
027300          SET END-OF-INFIL TO TRUE                                        
027400       END-READ                                                           
027500     .                                                                    
027600     SKIP3                                                                
027700 S11-SKRIV-UTFIL SECTION.                                                 
027800     SKIP2                                                                
027900     IF POST-OK                                                           
028000       WRITE UT-POST FROM UT-AREA                                         
028100     END-IF                                                               
028200     .                                                                    
028300     EJECT                                                                
028400 S99-ABEND SECTION.                                                       
028500     SKIP2                                                                
028600     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
028700     .                                                                    
