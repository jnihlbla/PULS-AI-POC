000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WDMR2500.                                                
000300 AUTHOR.         ANDRE KJELL.                                             
000400 DATE-WRITTEN.   09/02/13.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNCTION:                                                            
000900*        MATCHNING AV COBOL-PGM INFO FÖR FÖDNING AV DMR                   
001000*                                                                         
001100                                                                          
001200     SKIP3                                                                
001300 ENVIRONMENT DIVISION.                                                    
001400     SKIP2                                                                
001500 INPUT-OUTPUT SECTION.                                                    
001600                                                                          
001700 FILE-CONTROL.                                                            
001800     SKIP2                                                                
001900*          --- DENNA VECKAN PGM INFO                                      
002000     SELECT WDMR24                     ASSIGN TO WDMR25D1.                
002100     SKIP2                                                                
002200*          --- FÖRRA VECKAN PGM INFO                                      
002300     SELECT WDMROM                     ASSIGN TO WDMR25D2.                
002400     SKIP2                                                                
002500*          --- INFO OM ÄNDRADE MODULER                                    
002600     SELECT WDMR25                     ASSIGN TO WDMR25D3.                
002700     EJECT                                                                
002800 DATA DIVISION.                                                           
002900     SKIP3                                                                
003000 FILE SECTION.                                                            
003100     SKIP3                                                                
003200 FD  WDMR24                                                               
003300     RECORDING       F                                                    
003400     BLOCK CONTAINS  0.                                                   
003500                                                                          
003600 01  NEW-RECORD      PIC X(136).                                          
003700     SKIP3                                                                
003800 FD  WDMROM                                                               
003900     RECORDING       F                                                    
004000     BLOCK CONTAINS  0.                                                   
004100                                                                          
004200 01  OLD-RECORD      PIC X(136).                                          
004300     SKIP3                                                                
004400 FD  WDMR25                                                               
004500     RECORDING       F                                                    
004600     BLOCK CONTAINS  0.                                                   
004700                                                                          
004800 01  OUT-RECORD      PIC X(136).                                          
004900     EJECT                                                                
005000 WORKING-STORAGE SECTION.                                                 
005100                                                                          
005200 77  IDPGM                       PIC X(8)    VALUE 'WDMR2500'.            
005300 77  YES                         PIC X       VALUE 'J'.                   
005400 77  NOO                         PIC X       VALUE 'N'.                   
005500                                                                          
005600 77  WDMR24-EOF-SW               PIC X       VALUE 'N'.                   
005700     88  END-OF-NEW                          VALUE 'Y'.                   
005800                                                                          
005900 77  WDMROM-EOF-SW               PIC X       VALUE 'N'.                   
006000     88  END-OF-OLD                          VALUE 'Y'.                   
006100                                                                          
006200 01  CURRENT-MODULE              PIC X(8)    VALUE SPACE.                 
006300     EJECT                                                                
006400 01  GENERAL-SUBPROGRAMS.                                                 
006500     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
006600     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006700     SKIP2                                                                
006800*    --- PARAMETERS TO ABEND                                              
006900                                                                          
007000 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
007100 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
007200 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
007300     SKIP2                                                                
007400 01  ERROR-TEXT.                                                          
007500     03  FILLER                  PIC X(8)    VALUE 'ERRORTXT'.            
007600     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
007700     EJECT                                                                
007800*    --- PARAMETRAR TILL POSTSUM                                          
007900*                                                                         
008000*01  -COPY W0005   -PRE  POSTSUM-                                         
008100     EJECT                                                                
008200 01  NEW-AREA-START              PIC X(24)   VALUE                        
008300                                 'NEW-AREA-START'.                        
008400 01  NEW-AREA.                                                            
008500     03  NEW-MODULE              PIC X(8).                                
008600     03  NEW-TYPE                PIC X(8).                                
008700     03  NEW-DATA.                                                        
008800       05  NEW-DATA-WORD1           PIC X(20).                            
008900       05  NEW-DATA-WORD2           PIC X(20).                            
009000       05  NEW-DATA-WORD3           PIC X(20).                            
009100       05  NEW-DATA-WORD4           PIC X(20).                            
009200       05  NEW-DATA-WORD5           PIC X(20).                            
009300       05  NEW-DATA-WORD6           PIC X(20).                            
009400                                                                          
009500 01  OLD-AREA-START              PIC X(24)   VALUE                        
009600                                 'OLD-AREA-START'.                        
009700 01  OLD-AREA.                                                            
009800     03  OLD-MODULE              PIC X(8).                                
009900     03  OLD-TYPE                PIC X(8).                                
010000     03  OLD-DATA.                                                        
010100       05  OLD-DATA-WORD1           PIC X(20).                            
010200       05  OLD-DATA-WORD2           PIC X(20).                            
010300       05  OLD-DATA-WORD3           PIC X(20).                            
010400       05  OLD-DATA-WORD4           PIC X(20).                            
010500       05  OLD-DATA-WORD5           PIC X(20).                            
010600       05  OLD-DATA-WORD6           PIC X(20).                            
010700                                                                          
010800 01  OUT-AREA-START               PIC X(24)   VALUE                       
010900                                 'OUT-AREA-START'.                        
011000 01  OUT-AREA.                                                            
011100     03  OUT-MODULE              PIC X(8).                                
011200     03  OUT-TYPE                PIC X(8).                                
011300     03  OUT-DATA.                                                        
011400       05  OUT-DATA-WORD1           PIC X(20).                            
011500       05  OUT-DATA-WORD2           PIC X(20).                            
011600       05  OUT-DATA-WORD3           PIC X(20).                            
011700       05  OUT-DATA-WORD4           PIC X(20).                            
011800       05  OUT-DATA-WORD5           PIC X(20).                            
011900       05  OUT-DATA-WORD6           PIC X(20).                            
012000     EJECT                                                                
012100 01  MAX-TAB-IX                   PIC S9(4) BINARY VALUE 300.             
012200 01  WORK-IX                      PIC S9(4) BINARY VALUE ZERO.            
012300                                                                          
012400 01  OLD-TABLE-START              PIC X(24)   VALUE                       
012500                                 'OLD-TABLE-START'.                       
012600 01  OLD-IX                       PIC S9(4) BINARY VALUE ZERO.            
012700 01  OLD-TABLE.                                                           
012800     03 OLD-TAB-LINE OCCURS 300  PIC X(136).                              
012900                                                                          
013000 01  NEW-TABLE-START              PIC X(24)   VALUE                       
013100                                 'NEW-TABLE-START'.                       
013200 01  NEW-IX                       PIC S9(4) BINARY VALUE ZERO.            
013300 01  NEW-TABLE.                                                           
013400     03 NEW-TAB-LINE OCCURS 300  PIC X(136).                              
013500                                                                          
013600     EJECT                                                                
013700 PROCEDURE DIVISION.                                                      
013800 MAIN SECTION.                                                            
013900     SKIP2                                                                
014000                                                                          
014100     PERFORM A-INIT                                                       
014200     PERFORM S01-READ-WDMR24                                              
014300     PERFORM S02-READ-WDMROM                                              
014400     PERFORM E-CLEAR-TABLES                                               
014500                                                                          
014600     PERFORM UNTIL END-OF-NEW AND END-OF-OLD                              
014700       IF OLD-MODULE <= NEW-MODULE                                        
014800         MOVE OLD-MODULE TO CURRENT-MODULE                                
014900       ELSE                                                               
015000         MOVE NEW-MODULE TO CURRENT-MODULE                                
015100       END-IF                                                             
015200                                                                          
015300       IF OLD-MODULE = CURRENT-MODULE                                     
015400         PERFORM B-SAVE-OLD-MODULE-LINES                                  
015500       END-IF                                                             
015600       IF NEW-MODULE = CURRENT-MODULE                                     
015700         PERFORM C-SAVE-NEW-MODULE-LINES                                  
015800       END-IF                                                             
015900                                                                          
016000       IF OLD-IX NOT = NEW-IX                                             
016100       OR OLD-TABLE NOT = NEW-TABLE                                       
016200         PERFORM D-WRITE-CHANGES                                          
016300       END-IF                                                             
016400       PERFORM E-CLEAR-TABLES                                             
016500                                                                          
016600     END-PERFORM                                                          
016700                                                                          
016800     PERFORM Z-FINIT                                                      
016900                                                                          
017000     MOVE ZERO TO RETURN-CODE                                             
017100     GOBACK                                                               
017200     .                                                                    
017300     EJECT                                                                
017400 A-INIT SECTION.                                                          
017500                                                                          
017600     OPEN INPUT  WDMR24                                                   
017700                 WDMROM                                                   
017800     OPEN OUTPUT WDMR25                                                   
017900                                                                          
018000     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
018100                                                                          
018200     .                                                                    
018300     EJECT                                                                
018400 B-SAVE-OLD-MODULE-LINES SECTION.                                         
018500                                                                          
018600     PERFORM UNTIL END-OF-OLD                                             
018700                OR OLD-MODULE NOT = CURRENT-MODULE                        
018800                OR OLD-IX >=  MAX-TAB-IX                                  
018900        ADD 1 TO OLD-IX                                                   
019000        MOVE OLD-AREA TO OLD-TAB-LINE (OLD-IX)                            
019100                                                                          
019200        PERFORM S02-READ-WDMROM                                           
019300     END-PERFORM                                                          
019400     PERFORM UNTIL END-OF-OLD                                             
019500                OR OLD-MODULE NOT = CURRENT-MODULE                        
019600        PERFORM S02-READ-WDMROM                                           
019700     END-PERFORM                                                          
019800     .                                                                    
019900                                                                          
020000 C-SAVE-NEW-MODULE-LINES SECTION.                                         
020100                                                                          
020200     PERFORM UNTIL END-OF-NEW                                             
020300                OR NEW-MODULE NOT = CURRENT-MODULE                        
020400                OR NEW-IX >=  MAX-TAB-IX                                  
020500        ADD 1 TO NEW-IX                                                   
020600        MOVE NEW-AREA TO NEW-TAB-LINE (NEW-IX)                            
020700                                                                          
020800        PERFORM S01-READ-WDMR24                                           
020900     END-PERFORM                                                          
021000     PERFORM UNTIL END-OF-NEW                                             
021100                OR NEW-MODULE NOT = CURRENT-MODULE                        
021200        PERFORM S01-READ-WDMR24                                           
021300     END-PERFORM                                                          
021400     .                                                                    
021500     EJECT                                                                
021600 D-WRITE-CHANGES SECTION.                                                 
021700                                                                          
021800     IF NEW-IX = 0                                                        
021900       MOVE CURRENT-MODULE  TO OUT-MODULE                                 
022000       MOVE 'DELETE'        TO OUT-TYPE                                   
022100       MOVE SPACE           TO OUT-DATA                                   
022200       PERFORM S11-WRITE-WDMR25                                           
022300     ELSE                                                                 
022400       MOVE 1  TO WORK-IX                                                 
022500       PERFORM UNTIL WORK-IX > NEW-IX                                     
022600         MOVE NEW-TAB-LINE (WORK-IX) TO OUT-AREA                          
022700         PERFORM S11-WRITE-WDMR25                                         
022800         ADD 1 TO WORK-IX                                                 
022900       END-PERFORM                                                        
023000     END-IF                                                               
023100     .                                                                    
023200     EJECT                                                                
023300 E-CLEAR-TABLES SECTION.                                                  
023400                                                                          
023500     MOVE ZERO  TO OLD-IX    NEW-IX                                       
023600     MOVE SPACE TO OLD-TABLE NEW-TABLE                                    
023700     .                                                                    
023800     EJECT                                                                
023900 Z-FINIT SECTION.                                                         
024000     CLOSE WDMR24                                                         
024100           WDMROM                                                         
024200           WDMR25                                                         
024300     SKIP2                                                                
024400     MOVE 'S' TO POSTSUM-OPKOD                                            
024500     CALL POSTSUM USING POSTSUM-PARM                                      
024600     .                                                                    
024700     EJECT                                                                
024800 S01-READ-WDMR24  SECTION.                                                
024900     READ WDMR24 INTO NEW-AREA                                            
025000     AT END                                                               
025100        MOVE HIGH-VALUE TO NEW-AREA                                       
025200        SET END-OF-NEW    TO TRUE                                         
025300                                                                          
025400     NOT AT END                                                           
025500        MOVE 'WDMR24' TO POSTSUM-FDNAMN                                   
025600        MOVE 'WDMR25D1' TO POSTSUM-DDNAMN2                                
025700        MOVE SPACE      TO POSTSUM-TRANSTYP                               
025800        CALL POSTSUM USING POSTSUM-PARM                                   
025900     END-READ                                                             
026000     .                                                                    
026100     EJECT                                                                
026200 S02-READ-WDMROM  SECTION.                                                
026300     READ WDMROM INTO OLD-AREA                                            
026400     AT END                                                               
026500        MOVE HIGH-VALUE TO OLD-AREA                                       
026600        SET END-OF-OLD    TO TRUE                                         
026700                                                                          
026800     NOT AT END                                                           
026900        MOVE 'WDMROM' TO POSTSUM-FDNAMN                                   
027000        MOVE 'WDMR25D2' TO POSTSUM-DDNAMN2                                
027100        MOVE SPACE      TO POSTSUM-TRANSTYP                               
027200        CALL POSTSUM USING POSTSUM-PARM                                   
027300     END-READ                                                             
027400     .                                                                    
027500     EJECT                                                                
027600 S11-WRITE-WDMR25 SECTION.                                                
027700                                                                          
027800     WRITE OUT-RECORD FROM OUT-AREA                                       
027900                                                                          
028000     MOVE 'WDMR25' TO POSTSUM-FDNAMN                                      
028100     MOVE 'WDMR25D3' TO POSTSUM-DDNAMN2                                   
028200     CALL POSTSUM USING POSTSUM-PARM                                      
028300     .                                                                    
028400     EJECT                                                                
028500 S99-ABEND SECTION.                                                       
028600                                                                          
028700     SKIP2                                                                
028800     MOVE 'S' TO POSTSUM-OPKOD                                            
028900     CALL POSTSUM USING POSTSUM-PARM                                      
029000     CALL ABEND USING RKOD-ABEND                                          
029100     .                                                                    
