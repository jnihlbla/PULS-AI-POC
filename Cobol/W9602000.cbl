000100***MPOPT AMODE=24                                                         
000200 ID DIVISION.                                                             
000300     SKIP2                                                                
000400 PROGRAM-ID.     W9602000.                                                
000500 AUTHOR.         KARIN OLSSON.                                            
000600 DATE-WRITTEN.   92/07/21.                                                
000700                                                                          
000800     REMARKS.                                                             
000900*                                                                         
001000*    FUNKTION:                                                            
001100*        TESTDRIVER.                                                      
001200*                                                                         
001300*    ABENDKODER:                                                          
001400*        U0016 -  . . . .                                                 
001500*        U1000 -  . . . .                                                 
001600*                                                                         
001700                                                                          
001800                                                                          
001900     SKIP3                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100     SKIP2                                                                
002200 INPUT-OUTPUT SECTION.                                                    
002300     EJECT                                                                
002400 FILE-CONTROL.                                                            
002500     SKIP2                                                                
002600*              LADDMODULSBIBLIOTEK                                        
002700     SELECT LOADLIB                ASSIGN TO LOADDD.                      
002800     EJECT                                                                
002900 DATA DIVISION.                                                           
003000     SKIP3                                                                
003100 FILE SECTION.                                                            
003200     SKIP3                                                                
003300 FD  LOADLIB                                                              
003400     RECORDING U.                                                         
003500 01  FILLER             PIC X(500).                                       
003600     EJECT                                                                
003700 WORKING-STORAGE SECTION.                                                 
003800     SKIP2                                                                
003900                                                                          
004000*    -- CHECKED BY WY2000                                                 
004100 77  IDPGM                       PIC X(8)    VALUE 'W9602000'.            
004200 77  JA                          PIC X       VALUE 'J'.                   
004300 77  NEJ                         PIC X       VALUE 'N'.                   
004400     SKIP2                                                                
004500 77  ISP-VGET                    PIC X(8)    VALUE 'VGET    '.            
004600 77  ISP-VPUT                    PIC X(8)    VALUE 'VPUT    '.            
004700 77  ISP-VDEFINE                 PIC X(8)    VALUE 'VDEFINE '.            
004800 77  ISP-SHARED                  PIC X(8)    VALUE 'SHARED  '.            
004900 77  CHAR                        PIC X(8)    VALUE 'CHAR    '.            
005000 77  VDEFINE-OPT                 PIC X(16)                                
005100                              VALUE '(COPY NOBSCAN)'.                     
005200 77  ISPLLIB                     PIC X(8)    VALUE 'ISPLLIB '.            
005300 77  DATASET                     PIC X(8)    VALUE 'DATASET '.            
005400     EJECT                                                                
005500 01  DYNAMISKA-SUBPROGRAM.                                                
005600*                                                                         
005700     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005800     03  ISPLINK                 PIC X(8)    VALUE 'ISPLINK'.             
005900     03  WLOAD                   PIC X(8)    VALUE 'WLOAD'.               
006000     03  WDELETE                 PIC X(8)    VALUE 'WDELETE'.             
006100     SKIP2                                                                
006200 77  PGM-RKOD                    PIC S9(4)   COMP VALUE ZERO.             
006300                                                                          
006400 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
006500 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
006600     EJECT                                                                
006700 01  WSUBPGM                     PIC X(8)    VALUE SPACE.                 
006800 01  WANTAL                      PIC X(2)    VALUE SPACE.                 
006900 01  WGRUPP1                     PIC X(500)  VALUE SPACE.                 
007000 01  WPARM1                      PIC X(100)  VALUE SPACE.                 
007100 01  WPARM2                      PIC X(100)  VALUE SPACE.                 
007200 01  WPARM3                      PIC X(100)  VALUE SPACE.                 
007300 01  WPARM4                      PIC X(100)  VALUE SPACE.                 
007400 01  WPARM5                      PIC X(100)  VALUE SPACE.                 
007500 01  WPARM6                      PIC X(100)  VALUE SPACE.                 
007600 01  WPARM7                      PIC X(100)  VALUE SPACE.                 
007700 01  WPARM8                      PIC X(100)  VALUE SPACE.                 
007800 01  WPARM9                      PIC X(100)  VALUE SPACE.                 
007900 01  WPARM10                     PIC X(100)  VALUE SPACE.                 
008000     SKIP2                                                                
008100 01  N-SUBPGM                    PIC X(8)    VALUE 'SUBPGM'.              
008200 01  N-ANTAL                     PIC X(8)    VALUE 'ANTAL'.               
008300 01  N-GRUPP1                    PIC X(8)    VALUE 'GRUPP1'.              
008400 01  N-PARM1                     PIC X(8)    VALUE 'PARM1'.               
008500 01  N-PARM2                     PIC X(8)    VALUE 'PARM2'.               
008600 01  N-PARM3                     PIC X(8)    VALUE 'PARM3'.               
008700 01  N-PARM4                     PIC X(8)    VALUE 'PARM4'.               
008800 01  N-PARM5                     PIC X(8)    VALUE 'PARM5'.               
008900 01  N-PARM6                     PIC X(8)    VALUE 'PARM6'.               
009000 01  N-PARM7                     PIC X(8)    VALUE 'PARM7'.               
009100 01  N-PARM8                     PIC X(8)    VALUE 'PARM8'.               
009200 01  N-PARM9                     PIC X(8)    VALUE 'PARM9'.               
009300 01  N-PARM10                    PIC X(8)    VALUE 'PARM10'.              
009400     SKIP2                                                                
009500 01  L-SUBPGM                    PIC S9(9) COMP VALUE +8.                 
009600 01  L-ANTAL                     PIC S9(9) COMP VALUE +2.                 
009700 01  L-GRUPP1                    PIC S9(9) COMP VALUE +500.               
009800 01  L-PARM1                     PIC S9(9) COMP VALUE +100.               
009900 01  L-PARM2                     PIC S9(9) COMP VALUE +100.               
010000 01  L-PARM3                     PIC S9(9) COMP VALUE +100.               
010100 01  L-PARM4                     PIC S9(9) COMP VALUE +100.               
010200 01  L-PARM5                     PIC S9(9) COMP VALUE +100.               
010300 01  L-PARM6                     PIC S9(9) COMP VALUE +100.               
010400 01  L-PARM7                     PIC S9(9) COMP VALUE +100.               
010500 01  L-PARM8                     PIC S9(9) COMP VALUE +100.               
010600 01  L-PARM9                     PIC S9(9) COMP VALUE +100.               
010700 01  L-PARM10                    PIC S9(9) COMP VALUE +100.               
010800     EJECT                                                                
010900 PROCEDURE DIVISION.                                                      
011000     SKIP2                                                                
011100     PERFORM A-INIT                                                       
011200                                                                          
011300     PERFORM B-ANROPA-SUBPROGRAM                                          
011400                                                                          
011500     MOVE PGM-RKOD TO RETURN-CODE                                         
011600     GOBACK                                                               
011700     .                                                                    
011800     EJECT                                                                
011900 A-INIT SECTION.                                                          
012000     SKIP2                                                                
012100                                                                          
012200     CALL ISPLINK USING ISP-VDEFINE N-SUBPGM WSUBPGM CHAR                 
012300                         L-SUBPGM VDEFINE-OPT                             
012400     IF RETURN-CODE > 0                                                   
012500       DISPLAY 'SAKNAR NAMN PÅ SUBPROGRAM'                                
012600       PERFORM S99-ABEND                                                  
012700     END-IF                                                               
012800                                                                          
012900     CALL ISPLINK USING ISP-VDEFINE N-ANTAL WANTAL CHAR                   
013000                         L-ANTAL VDEFINE-OPT                              
013100     IF RETURN-CODE > 0                                                   
013200       DISPLAY 'SAKNAR ANTAL PARAMETRAR'                                  
013300       PERFORM S99-ABEND                                                  
013400     END-IF                                                               
013500                                                                          
013600     IF WANTAL = 'G'                                                      
013700       CALL ISPLINK USING ISP-VDEFINE N-GRUPP1 WGRUPP1 CHAR               
013800                           L-GRUPP1 VDEFINE-OPT                           
013900       IF RETURN-CODE > 0                                                 
014000         DISPLAY 'SAKNAR GRUPP1'                                          
014100         PERFORM S99-ABEND                                                
014200       END-IF                                                             
014300     ELSE                                                                 
014400                                                                          
014500       CALL ISPLINK USING ISP-VDEFINE N-PARM1 WPARM1 CHAR                 
014600                           L-PARM1 VDEFINE-OPT                            
014700       IF RETURN-CODE > 0                                                 
014800         DISPLAY 'SAKNAR PARM1'                                           
014900         PERFORM S99-ABEND                                                
015000       END-IF                                                             
015100                                                                          
015200       CALL ISPLINK USING ISP-VDEFINE N-PARM2 WPARM2 CHAR                 
015300                           L-PARM2 VDEFINE-OPT                            
015400       IF RETURN-CODE > 0                                                 
015500         DISPLAY 'SAKNAR PARM2'                                           
015600         PERFORM S99-ABEND                                                
015700       END-IF                                                             
015800                                                                          
015900       CALL ISPLINK USING ISP-VDEFINE N-PARM3 WPARM3 CHAR                 
016000                           L-PARM3 VDEFINE-OPT                            
016100       IF RETURN-CODE > 0                                                 
016200         DISPLAY 'SAKNAR PARM2'                                           
016300         PERFORM S99-ABEND                                                
016400       END-IF                                                             
016500                                                                          
016600       CALL ISPLINK USING ISP-VDEFINE N-PARM4 WPARM4 CHAR                 
016700                           L-PARM4 VDEFINE-OPT                            
016800       IF RETURN-CODE > 0                                                 
016900         DISPLAY 'SAKNAR PARM4'                                           
017000         PERFORM S99-ABEND                                                
017100       END-IF                                                             
017200                                                                          
017300       CALL ISPLINK USING ISP-VDEFINE N-PARM5 WPARM5 CHAR                 
017400                           L-PARM5 VDEFINE-OPT                            
017500       IF RETURN-CODE > 0                                                 
017600         DISPLAY 'SAKNAR PARM5'                                           
017700         PERFORM S99-ABEND                                                
017800       END-IF                                                             
017900                                                                          
018000       CALL ISPLINK USING ISP-VDEFINE N-PARM6 WPARM6 CHAR                 
018100                           L-PARM6 VDEFINE-OPT                            
018200       IF RETURN-CODE > 0                                                 
018300         DISPLAY 'SAKNAR PARM6'                                           
018400         PERFORM S99-ABEND                                                
018500       END-IF                                                             
018600                                                                          
018700       CALL ISPLINK USING ISP-VDEFINE N-PARM7 WPARM7 CHAR                 
018800                           L-PARM7 VDEFINE-OPT                            
018900       IF RETURN-CODE > 0                                                 
019000         DISPLAY 'SAKNAR PARM7'                                           
019100         PERFORM S99-ABEND                                                
019200       END-IF                                                             
019300                                                                          
019400       CALL ISPLINK USING ISP-VDEFINE N-PARM8 WPARM8 CHAR                 
019500                           L-PARM8 VDEFINE-OPT                            
019600       IF RETURN-CODE > 0                                                 
019700         DISPLAY 'SAKNAR PARM8'                                           
019800         PERFORM S99-ABEND                                                
019900       END-IF                                                             
020000                                                                          
020100       CALL ISPLINK USING ISP-VDEFINE N-PARM9 WPARM9 CHAR                 
020200                           L-PARM9 VDEFINE-OPT                            
020300       IF RETURN-CODE > 0                                                 
020400         DISPLAY 'SAKNAR PARM9'                                           
020500         PERFORM S99-ABEND                                                
020600       END-IF                                                             
020700                                                                          
020800       CALL ISPLINK USING ISP-VDEFINE N-PARM10 WPARM10 CHAR               
020900                           L-PARM10 VDEFINE-OPT                           
021000       IF RETURN-CODE > 0                                                 
021100         DISPLAY 'SAKNAR PARM10'                                          
021200         PERFORM S99-ABEND                                                
021300       END-IF                                                             
021400     END-IF                                                               
021500                                                                          
021600     .                                                                    
021700     EJECT                                                                
021800 B-ANROPA-SUBPROGRAM SECTION.                                             
021900     SKIP2                                                                
022000     OPEN INPUT LOADLIB                                                   
022100     CALL WLOAD USING LOADLIB WSUBPGM                                     
022200     CLOSE LOADLIB                                                        
022300                                                                          
022400     EVALUATE TRUE                                                        
022500                                                                          
022600       WHEN WANTAL = 'G'                                                  
022700         CALL WSUBPGM USING WGRUPP1                                       
022800         MOVE RETURN-CODE TO PGM-RKOD                                     
022900                                                                          
023000         CALL ISPLINK USING ISP-VPUT N-GRUPP1 ISP-SHARED                  
023100                                                                          
023200       WHEN WANTAL = '1'                                                  
023300         CALL WSUBPGM USING WPARM1                                        
023400         MOVE RETURN-CODE TO PGM-RKOD                                     
023500                                                                          
023600         CALL ISPLINK USING ISP-VPUT N-PARM1 ISP-SHARED                   
023700                                                                          
023800       WHEN WANTAL = '2'                                                  
023900         CALL WSUBPGM USING WPARM1 WPARM2                                 
024000         MOVE RETURN-CODE TO PGM-RKOD                                     
024100                                                                          
024200         CALL ISPLINK USING ISP-VPUT N-PARM1 ISP-SHARED                   
024300         CALL ISPLINK USING ISP-VPUT N-PARM2 ISP-SHARED                   
024400                                                                          
024500       WHEN WANTAL = '3'                                                  
024600         CALL WSUBPGM USING WPARM1 WPARM2 WPARM3                          
024700         MOVE RETURN-CODE TO PGM-RKOD                                     
024800                                                                          
024900         CALL ISPLINK USING ISP-VPUT N-PARM1 ISP-SHARED                   
025000         CALL ISPLINK USING ISP-VPUT N-PARM2 ISP-SHARED                   
025100         CALL ISPLINK USING ISP-VPUT N-PARM3 ISP-SHARED                   
025200                                                                          
025300       WHEN WANTAL = '4'                                                  
025400         CALL WSUBPGM USING WPARM1 WPARM2 WPARM3 WPARM4                   
025500         MOVE RETURN-CODE TO PGM-RKOD                                     
025600                                                                          
025700         CALL ISPLINK USING ISP-VPUT N-PARM1 ISP-SHARED                   
025800         CALL ISPLINK USING ISP-VPUT N-PARM2 ISP-SHARED                   
025900         CALL ISPLINK USING ISP-VPUT N-PARM3 ISP-SHARED                   
026000         CALL ISPLINK USING ISP-VPUT N-PARM4 ISP-SHARED                   
026100                                                                          
026200       WHEN WANTAL = '5'                                                  
026300         CALL WSUBPGM USING WPARM1 WPARM2 WPARM3 WPARM4 WPARM5            
026400         MOVE RETURN-CODE TO PGM-RKOD                                     
026500                                                                          
026600         CALL ISPLINK USING ISP-VPUT N-PARM1 ISP-SHARED                   
026700         CALL ISPLINK USING ISP-VPUT N-PARM2 ISP-SHARED                   
026800         CALL ISPLINK USING ISP-VPUT N-PARM3 ISP-SHARED                   
026900         CALL ISPLINK USING ISP-VPUT N-PARM4 ISP-SHARED                   
027000         CALL ISPLINK USING ISP-VPUT N-PARM5 ISP-SHARED                   
027100                                                                          
027200       WHEN WANTAL = '6'                                                  
027300         CALL WSUBPGM USING WPARM1 WPARM2 WPARM3 WPARM4 WPARM5            
027400                            WPARM6                                        
027500         MOVE RETURN-CODE TO PGM-RKOD                                     
027600                                                                          
027700         CALL ISPLINK USING ISP-VPUT N-PARM1 ISP-SHARED                   
027800         CALL ISPLINK USING ISP-VPUT N-PARM2 ISP-SHARED                   
027900         CALL ISPLINK USING ISP-VPUT N-PARM3 ISP-SHARED                   
028000         CALL ISPLINK USING ISP-VPUT N-PARM4 ISP-SHARED                   
028100         CALL ISPLINK USING ISP-VPUT N-PARM5 ISP-SHARED                   
028200         CALL ISPLINK USING ISP-VPUT N-PARM6 ISP-SHARED                   
028300                                                                          
028400       WHEN WANTAL = '7'                                                  
028500         CALL WSUBPGM USING WPARM1 WPARM2 WPARM3 WPARM4 WPARM5            
028600                            WPARM6 WPARM7                                 
028700         MOVE RETURN-CODE TO PGM-RKOD                                     
028800                                                                          
028900         CALL ISPLINK USING ISP-VPUT N-PARM1 ISP-SHARED                   
029000         CALL ISPLINK USING ISP-VPUT N-PARM2 ISP-SHARED                   
029100         CALL ISPLINK USING ISP-VPUT N-PARM3 ISP-SHARED                   
029200         CALL ISPLINK USING ISP-VPUT N-PARM4 ISP-SHARED                   
029300         CALL ISPLINK USING ISP-VPUT N-PARM5 ISP-SHARED                   
029400         CALL ISPLINK USING ISP-VPUT N-PARM6 ISP-SHARED                   
029500         CALL ISPLINK USING ISP-VPUT N-PARM7 ISP-SHARED                   
029600                                                                          
029700       WHEN WANTAL = '8'                                                  
029800         CALL WSUBPGM USING WPARM1 WPARM2 WPARM3 WPARM4 WPARM5            
029900                            WPARM6 WPARM7 WPARM8                          
030000         MOVE RETURN-CODE TO PGM-RKOD                                     
030100                                                                          
030200         CALL ISPLINK USING ISP-VPUT N-PARM1 ISP-SHARED                   
030300         CALL ISPLINK USING ISP-VPUT N-PARM2 ISP-SHARED                   
030400         CALL ISPLINK USING ISP-VPUT N-PARM3 ISP-SHARED                   
030500         CALL ISPLINK USING ISP-VPUT N-PARM4 ISP-SHARED                   
030600         CALL ISPLINK USING ISP-VPUT N-PARM5 ISP-SHARED                   
030700         CALL ISPLINK USING ISP-VPUT N-PARM6 ISP-SHARED                   
030800         CALL ISPLINK USING ISP-VPUT N-PARM7 ISP-SHARED                   
030900         CALL ISPLINK USING ISP-VPUT N-PARM8 ISP-SHARED                   
031000                                                                          
031100       WHEN WANTAL = '9'                                                  
031200         CALL WSUBPGM USING WPARM1 WPARM2 WPARM3 WPARM4 WPARM5            
031300                            WPARM6 WPARM7 WPARM8 WPARM9                   
031400         MOVE RETURN-CODE TO PGM-RKOD                                     
031500                                                                          
031600         CALL ISPLINK USING ISP-VPUT N-PARM1 ISP-SHARED                   
031700         CALL ISPLINK USING ISP-VPUT N-PARM2 ISP-SHARED                   
031800         CALL ISPLINK USING ISP-VPUT N-PARM3 ISP-SHARED                   
031900         CALL ISPLINK USING ISP-VPUT N-PARM4 ISP-SHARED                   
032000         CALL ISPLINK USING ISP-VPUT N-PARM5 ISP-SHARED                   
032100         CALL ISPLINK USING ISP-VPUT N-PARM6 ISP-SHARED                   
032200         CALL ISPLINK USING ISP-VPUT N-PARM7 ISP-SHARED                   
032300         CALL ISPLINK USING ISP-VPUT N-PARM8 ISP-SHARED                   
032400         CALL ISPLINK USING ISP-VPUT N-PARM9 ISP-SHARED                   
032500                                                                          
032600       WHEN WANTAL = '10'                                                 
032700         CALL WSUBPGM USING WPARM1 WPARM2 WPARM3 WPARM4 WPARM5            
032800                            WPARM6 WPARM7 WPARM8 WPARM9 WPARM10           
032900         MOVE RETURN-CODE TO PGM-RKOD                                     
033000                                                                          
033100         CALL ISPLINK USING ISP-VPUT N-PARM1 ISP-SHARED                   
033200         CALL ISPLINK USING ISP-VPUT N-PARM2 ISP-SHARED                   
033300         CALL ISPLINK USING ISP-VPUT N-PARM3 ISP-SHARED                   
033400         CALL ISPLINK USING ISP-VPUT N-PARM4 ISP-SHARED                   
033500         CALL ISPLINK USING ISP-VPUT N-PARM5 ISP-SHARED                   
033600         CALL ISPLINK USING ISP-VPUT N-PARM6 ISP-SHARED                   
033700         CALL ISPLINK USING ISP-VPUT N-PARM7 ISP-SHARED                   
033800         CALL ISPLINK USING ISP-VPUT N-PARM8 ISP-SHARED                   
033900         CALL ISPLINK USING ISP-VPUT N-PARM9 ISP-SHARED                   
034000         CALL ISPLINK USING ISP-VPUT N-PARM10 ISP-SHARED                  
034100     END-EVALUATE                                                         
034200                                                                          
034300     CANCEL WSUBPGM                                                       
034400                                                                          
034500     CALL WDELETE USING WSUBPGM                                           
034600     .                                                                    
034700     EJECT                                                                
034800 S99-ABEND SECTION.                                                       
034900                                                                          
035000     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
035100     .                                                                    
