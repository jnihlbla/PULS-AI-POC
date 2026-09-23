000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WXTRA700.                                                
000300 AUTHOR.         STENING INGER.                                           
000400 DATE-WRITTEN.   21/04/07.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNCTION: CREATE FILE TO AZURE DATALAKE IN DISPLAY FORMAT            
000900*                                                                         
001000*    ABENDCODES:                                                          
001100*                                                                         
001200                                                                          
001300     SKIP3                                                                
001400 ENVIRONMENT DIVISION.                                                    
001500     SKIP2                                                                
001600 INPUT-OUTPUT SECTION.                                                    
001700                                                                          
001800 FILE-CONTROL.                                                            
001900     SKIP2                                                                
002000*          --- INPUT FILE FROM WXTRA0                                     
002100     SELECT WXTRA7                     ASSIGN TO WXTRA7D1.                
002200*          --- OUTPUT FILE TO AZURE DATALAKE                              
002300     SELECT WXTRA7X                    ASSIGN TO WXTRA7D2.                
002400     EJECT                                                                
002500 DATA DIVISION.                                                           
002600     SKIP3                                                                
002700 FILE SECTION.                                                            
002800     SKIP3                                                                
002900 FD  WXTRA7                                                               
003000     RECORDING       F                                                    
003100     BLOCK CONTAINS  0.                                                   
003200                                                                          
003300*01  -COPY WXTRA7          -PRE  IN-   -L.                                
003400     SKIP3                                                                
003500 FD  WXTRA7X                                                              
003600     RECORDING       F                                                    
003700     BLOCK CONTAINS  0.                                                   
003800                                                                          
003900*01  RECORD -COPY WXTRA7X  -PRE  OUTX- -L.                                
004000     EJECT                                                                
004100 WORKING-STORAGE SECTION.                                                 
004200                                                                          
004300 77  IDPGM                       PIC X(8)    VALUE 'WXTRA700'.            
004400 77  YES                         PIC X       VALUE 'J'.                   
004500 77  NOO                         PIC X       VALUE 'N'.                   
004600 77  IX1                         PIC S9(9)   VALUE +0 COMP SYNC.          
004700                                                                          
004800 77  WXTRA7-EOF-SW               PIC X       VALUE 'N'.                   
004900     88  END-OF-WXTRA7                       VALUE 'J'.                   
005000     EJECT                                                                
005100 01  GENERAL-SUBPROGRAMS.                                                 
005200*                                                                         
005300     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
005400     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005500     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
005600     SKIP2                                                                
005700*    --- PARAMETERS TO ABEND                                              
005800                                                                          
005900 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006000 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
006100 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
006200     SKIP2                                                                
006300 01  ERROR-TEXT.                                                          
006400     03  FILLER                  PIC X(8)    VALUE 'ERR-TEXT'.            
006500     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
006600     EJECT                                                                
006700*    --- PARAMETRAR TILL POSTSUM                                          
006800*                                                                         
006900*01  -COPY W0005   -PRE  POSTSUM-                                         
007000     EJECT                                                                
007100 01  IN-AREA-START               PIC X(24)   VALUE                        
007200                                 'IN-AREA-START  '.                       
007300                                                                          
007400*01  AREA -COPY WXTRA7      -PRE IN-                                      
007500     EJECT                                                                
007600 01  OUT1-AREA-START             PIC X(24)   VALUE                        
007700                                 'OUT1-AREA-START  '.                     
007800                                                                          
007900*01  AREA -COPY WXTRA7X     -PRE OUTX-                                    
008000     EJECT                                                                
008100 PROCEDURE DIVISION.                                                      
008200 MAIN SECTION.                                                            
008300     SKIP2                                                                
008400                                                                          
008500     PERFORM A-INIT                                                       
008600     PERFORM S01-READ-WXTRA7                                              
008700     PERFORM UNTIL END-OF-WXTRA7                                          
008800       PERFORM S11-WRITE-WXTRA7X                                          
008900                                                                          
009000       PERFORM S01-READ-WXTRA7                                            
009100     END-PERFORM                                                          
009200                                                                          
009300     PERFORM Z-FINIT                                                      
009400                                                                          
009500     MOVE ZERO TO RETURN-CODE                                             
009600     GOBACK                                                               
009700     .                                                                    
009800     EJECT                                                                
009900 A-INIT SECTION.                                                          
010000                                                                          
010100     OPEN INPUT  WXTRA7                                                   
010200                                                                          
010300          OUTPUT WXTRA7X                                                  
010400                                                                          
010500     .                                                                    
010600     EJECT                                                                
010700 Z-FINIT SECTION.                                                         
010800     CLOSE WXTRA7                                                         
010900           WXTRA7X                                                        
011000     SKIP2                                                                
011100     MOVE 'S' TO POSTSUM-OPKOD                                            
011200     CALL POSTSUM USING POSTSUM-PARM                                      
011300     .                                                                    
011400     EJECT                                                                
011500 S01-READ-WXTRA7  SECTION.                                                
011600                                                                          
011700     READ WXTRA7 INTO IN-AREA                                             
011800     AT END                                                               
011900        MOVE HIGH-VALUE TO IN-AREA                                        
012000        SET END-OF-WXTRA7 TO TRUE                                         
012100                                                                          
012200     NOT AT END                                                           
012300        MOVE 'WXTRA7'   TO POSTSUM-FDNAMN                                 
012400        MOVE 'WXTRA7D1' TO POSTSUM-DDNAMN2                                
012500        MOVE SPACE      TO POSTSUM-TRANSTYP                               
012600        CALL POSTSUM USING POSTSUM-PARM                                   
012700     END-READ                                                             
012800     .                                                                    
012900     EJECT                                                                
013000 S11-WRITE-WXTRA7X SECTION.                                               
013100                                                                          
013200     MOVE IN-IDDISTR             TO OUTX-IDDISTR                          
013300     MOVE IN-IDKUNDNR            TO OUTX-IDKUNDNR                         
013400     MOVE IN-IDPRODNR            TO OUTX-IDPRODNR                         
013500     MOVE IN-IDORDER             TO OUTX-IDORDER                          
013600     MOVE IN-IDDC                TO OUTX-IDDC                             
013700     MOVE IN-TIBEGPAC            TO OUTX-TIBEGPAC                         
013800     MOVE IN-TIBEGTID            TO OUTX-TIBEGTID                         
013900     MOVE IN-TIORDREG            TO OUTX-TIORDREG                         
014000     MOVE IN-IDPLKLST            TO OUTX-IDPLKLST                         
014100     MOVE IN-IDARTNR             TO OUTX-IDARTNR                          
014200     MOVE IN-ADLAGOMR-VERKLIG    TO OUTX-ADLAGOMR-VERKLIG                 
014300     MOVE IN-ADLAGOMR-DLB        TO OUTX-ADLAGOMR-DLB                     
014400     MOVE IN-ADGANG-DLB          TO OUTX-ADGANG-DLB                       
014500     MOVE IN-ADPLATS-DLB         TO OUTX-ADPLATS-DLB                      
014600     MOVE IN-KDFRAKT             TO OUTX-KDFRAKT                          
014700     MOVE IN-KDORDKL             TO OUTX-KDORDKL                          
014800     MOVE IN-KVBEART             TO OUTX-KVBEART                          
014900     MOVE IN-KVLEVART            TO OUTX-KVLEVART                         
015000     MOVE IN-VKARTNTO            TO OUTX-VKARTNTO                         
015100     MOVE IN-VLARTNTO            TO OUTX-VLARTNTO                         
015200     MOVE IN-FLDIRLEV            TO OUTX-FLDIRLEV                         
015300     MOVE IN-IDKUNDRF-RO         TO OUTX-IDKUNDRF-RO                      
015400     MOVE IN-IDKOLLI             TO OUTX-IDKOLLI                          
015500     MOVE IN-IDUSER              TO OUTX-IDUSER                           
015600     MOVE IN-KVLEVART2           TO OUTX-KVLEVART2                        
015700     MOVE IN-IDPRC               TO OUTX-IDPRC                            
015800     MOVE IN-IDSKIFT             TO OUTX-IDSKIFT                          
015900     MOVE IN-KDPRODSL            TO OUTX-KDPRODSL                         
016000     MOVE IN-PRARTSTD            TO OUTX-PRARTSTD                         
016100     MOVE IN-TIPACKN             TO OUTX-TIPACKN                          
016200     MOVE IN-IDPLOCK             TO OUTX-IDPLOCK                          
016300     MOVE IN-TIREGTID            TO OUTX-TIREGTID                         
016400     MOVE IN-TIUTSKR             TO OUTX-TIUTSKR                          
016500     MOVE IN-TIUTSTID            TO OUTX-TIUTSTID                         
016600     MOVE IN-TIPACTID            TO OUTX-TIPACTID                         
016700     MOVE IN-KDPRCGRP            TO OUTX-KDPRCGRP                         
016800                                                                          
016900     PERFORM S11A-WRITE-WXTRA7X                                           
017000     .                                                                    
017100     EJECT                                                                
017200 S11A-WRITE-WXTRA7X SECTION.                                              
017300                                                                          
017400     WRITE OUTX-RECORD FROM OUTX-AREA                                     
017500                                                                          
017600     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
017700     MOVE 'WXTRA7X'  TO POSTSUM-FDNAMN                                    
017800     MOVE 'WXTRA7D2' TO POSTSUM-DDNAMN2                                   
017900     CALL POSTSUM USING POSTSUM-PARM                                      
018000     .                                                                    
018100     EJECT                                                                
018200 S99-ABEND SECTION.                                                       
018300                                                                          
018400     SKIP2                                                                
018500     MOVE 'S' TO POSTSUM-OPKOD                                            
018600     CALL POSTSUM USING POSTSUM-PARM                                      
018700     CALL ABEND USING RKOD-ABEND                                          
018800     .                                                                    
