000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WZ140500.                                                
000300 AUTHOR.         ANDRE KJELL.                                             
000400 DATE-WRITTEN.   02/10/21.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNCTION:                                                            
000900*        READ A FILE CONTAING RECORDS THAT HAVE BEEN SPLIT                
001000*        INTO 80-BYTE PIECES, JOIN THE PIECES, AND WRITE THE              
001100*        COMPLETE RECORDS ONTO AN OUTPUT FILE.                            
001200*                                                                         
001300*        2010-01-15:  RENAMED FROM WZ112000                               
001400                                                                          
001500     SKIP3                                                                
001600 ENVIRONMENT DIVISION.                                                    
001700     SKIP2                                                                
001800 INPUT-OUTPUT SECTION.                                                    
001900                                                                          
002000 FILE-CONTROL.                                                            
002100     SKIP2                                                                
002200*          --- INPUT DATA PIECES                                          
002300     SELECT INDATA                     ASSIGN TO SYSUT1.                  
002400                                                                          
002500*          --- OUTPUT JOINED PIECES                                       
002600     SELECT OUTDATA                    ASSIGN TO SYSUT2.                  
002700     SKIP2                                                                
002800     EJECT                                                                
002900 DATA DIVISION.                                                           
003000     SKIP3                                                                
003100 FILE SECTION.                                                            
003200     SKIP3                                                                
003300 FD  INDATA                                                               
003400     RECORDING F                                                          
003500     BLOCK CONTAINS  0.                                                   
003600                                                                          
003700 01  FILLER                     PIC X(80).                                
003800     SKIP3                                                                
003900 FD  OUTDATA                                                              
004000     RECORD IS VARYING FROM 1 TO 3000 CHARACTERS                          
004100            DEPENDING ON OUT-LENGTH                                       
004200     BLOCK CONTAINS  0.                                                   
004300                                                                          
004400 01  OUT-RCD                    PIC X(3000).                              
004500     SKIP3                                                                
004600 WORKING-STORAGE SECTION.                                                 
004700                                                                          
004800 77  YES                         PIC X       VALUE 'J'.                   
004900 77  NOO                         PIC X       VALUE 'N'.                   
005000                                                                          
005100 77  INDATA-EOF                  PIC X       VALUE 'N'.                   
005200     88 END-OF-INDATA                        VALUE 'J'.                   
005300 77  WRITE-SWITCH                PIC X.                                   
005400 77  GOBACK-RC                   PIC 9(4)   BINARY.                       
005500                                                                          
005600 01  IN-AREA-START               PIC X(16)   VALUE  'IN-AREA'.            
005700 01  IN-AREA.                                                             
005800*                                -- X MEANS CONTINUATION                  
005900*                                -- SPACE MEANS FIRST PIECE               
006000   03  IN-CONT                   PIC X.                                   
006100*                                -- LENGTH OF THIS PIECE                  
006200   03  IN-LENGTH                 PIC 9(4)   BINARY.                       
006300   03  IN-PIECE                  PIC X(77).                               
006400                                                                          
006500 77  OUT-LENGTH                  PIC 9(4)   BINARY.                       
006600 77  OUT-POS                     PIC 9(4)   BINARY.                       
006700                                                                          
006800 01  OUT-AREA-START              PIC X(16)   VALUE  'OUT-AREA'.           
006900 01  OUT-AREA                    PIC X(3000).                             
007000                                                                          
007100     EJECT                                                                
007200 PROCEDURE DIVISION.                                                      
007300 MAIN SECTION.                                                            
007400                                                                          
007500     SKIP2                                                                
007600     OPEN INPUT INDATA                                                    
007700          OUTPUT OUTDATA                                                  
007800                                                                          
007900     MOVE 1 TO OUT-POS                                                    
008000     MOVE NOO TO WRITE-SWITCH                                             
008100                                                                          
008200     PERFORM S01-READ-INDATA                                              
008300     PERFORM UNTIL END-OF-INDATA                                          
008400       IF WRITE-SWITCH = YES AND IN-CONT = SPACE                          
008500*        -- WRITE PREVIOUSLY JOINED PIECES                                
008600         PERFORM S02-WRITE-OUTDATA                                        
008700         MOVE 1 TO OUT-POS                                                
008800         MOVE NOO TO WRITE-SWITCH                                         
008900       END-IF                                                             
009000                                                                          
009100       MOVE IN-PIECE(1:IN-LENGTH) TO OUT-AREA(OUT-POS:IN-LENGTH)          
009200       ADD IN-LENGTH TO OUT-POS                                           
009300       MOVE YES TO WRITE-SWITCH                                           
009400                                                                          
009500       PERFORM S01-READ-INDATA                                            
009600     END-PERFORM                                                          
009700                                                                          
009800     MOVE ZERO TO GOBACK-RC                                               
009900     IF WRITE-SWITCH = YES                                                
010000*      -- WRITE THE LAST BATCH OF PIECES                                  
010100       PERFORM S02-WRITE-OUTDATA                                          
010200     ELSE                                                                 
010300*      -- NO DATA WAS WRITTEN                                             
010400       MOVE 4 TO GOBACK-RC                                                
010500     END-IF                                                               
010600                                                                          
010700     CLOSE INDATA, OUTDATA                                                
010800                                                                          
010900     MOVE GOBACK-RC TO RETURN-CODE                                        
011000     GOBACK                                                               
011100     .                                                                    
011200     EJECT                                                                
011300 S01-READ-INDATA  SECTION.                                                
011400     SKIP2                                                                
011500     READ INDATA INTO IN-AREA                                             
011600     AT END                                                               
011700        SET END-OF-INDATA TO TRUE                                         
011800     END-READ                                                             
011900     .                                                                    
012000     EJECT                                                                
012100 S02-WRITE-OUTDATA  SECTION.                                              
012200     SKIP2                                                                
012300     SUBTRACT 1 FROM OUT-POS GIVING OUT-LENGTH                            
012400     WRITE OUT-RCD FROM OUT-AREA                                          
012500*    DISPLAY 'OUT: ' OUT-LENGTH ' ' OUT-AREA(1:OUT-LENGTH)                
012600     .                                                                    
