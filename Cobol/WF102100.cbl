000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WF102100.                                                
000300 AUTHOR.         REDDY RAHUL.                                             
000400 DATE-WRITTEN.   14/02/07.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNCTION:                                                            
000900*        CHANGE LAYOUT OF MONTHLY CURRENCY FILE RECEIVED                  
001000*        FOR SEK CURRENCY                                                 
001100*                                                                         
001200*    ABENDCODES:                                                          
001300*        U0016 -  . . . .                                                 
001400*        U1000 -  . . . .                                                 
001500*                                                                         
001600*    CHANGE LOG:                                                          
001700*      YY/MM/DD - INITIALS        - DESCRIPTION.                          
001800*                                                                         
001900*      14/02/07 - REDDY RAHUL     - ETRACKER 10222254                     
002000*                                   INITIAL VERSION.                      
002100                                                                          
002200     SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400     SKIP2                                                                
002500 INPUT-OUTPUT SECTION.                                                    
002600                                                                          
002700 FILE-CONTROL.                                                            
002800     SKIP2                                                                
002900*          --- MONTHLY CURRENCY DATA                                      
003000     SELECT WF1011                     ASSIGN TO WF1021D1.                
003100     SKIP2                                                                
003200*          --- MONTHLY CURRENCY DATA                                      
003300     SELECT WF1021                     ASSIGN TO WF1021D2.                
003400     EJECT                                                                
003500 DATA DIVISION.                                                           
003600     SKIP3                                                                
003700 FILE SECTION.                                                            
003800     SKIP3                                                                
003900 FD  WF1011                                                               
004000     RECORDING       F                                                    
004100     BLOCK CONTAINS  0.                                                   
004200 01  IN-RECORD                   PIC X(41).                               
004300     SKIP3                                                                
004400 FD  WF1021                                                               
004500     RECORDING       F                                                    
004600     BLOCK CONTAINS  0.                                                   
004700 01  UT-RECORD                   PIC X(46).                               
004800     EJECT                                                                
004900 WORKING-STORAGE SECTION.                                                 
005000                                                                          
005100 77  IDPGM                       PIC X(8)    VALUE 'WF102100'.            
005200 77  YES                         PIC X       VALUE 'J'.                   
005300 77  NOO                         PIC X       VALUE 'N'.                   
005400 77  WS-REC-COUNT                PIC 9(8)    VALUE ZERO.                  
005500                                                                          
005600 77  WF1011-EOF-SW               PIC X       VALUE 'N'.                   
005700     88  END-OF-WF1011                       VALUE 'J'.                   
005800     EJECT                                                                
005900 01  GENERAL-SUBPROGRAMS.                                                 
006000*                                                                         
006100     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006200     SKIP2                                                                
006300 01  ERROR-TEXT.                                                          
006400     03  FILLER                  PIC X(10)   VALUE 'ERROR-TEXT'.          
006500     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
006600     EJECT                                                                
006700*    --- PARAMETRAR TILL POSTSUM                                          
006800*                                                                         
006900*01  -COPY W0005   -PRE  POSTSUM-                                         
007000     EJECT                                                                
007100 01  IN-AREA-START               PIC X(24)   VALUE                        
007200                                 'IN-AREA-START  '.                       
007300     SKIP2                                                                
007400 01  IN-AREA.                                                             
007500     03  IN-IDPTYP               PIC X(4).                                
007600     03  PZZZ-AREA.                                                       
007700       05  FILLER                PIC X(37).                               
007800     03  P002-AREA REDEFINES PZZZ-AREA.                                   
007900       05  IN-KDVALISO           PIC X(5).                                
008000       05  IN-REVALUTA           PIC 9(5).                                
008100       05  IN-PRKURS             PIC 9(6)V9(5).                           
008200       05  IN-DASTADAT           PIC 9(8).                                
008300       05  IN-DAREGDAT           PIC 9(8).                                
008400     EJECT                                                                
008500 01  UT-AREA-START               PIC X(24)   VALUE                        
008600                                 'UT-AREA-START  '.                       
008700     SKIP2                                                                
008800 01  UT-AREA.                                                             
008900     03  UT-KDVALISO             PIC X(05).                               
009000     03  UT-REVALUTA-FROM        PIC 9(5).                                
009100     03  UT-REVALUTA-TO          PIC 9(5).                                
009200     03  UT-PRKURS               PIC 9(6)V9(5).                           
009300     03  UT-DASTADAT             PIC 9(8).                                
009400     03  UT-DAREGDAT             PIC 9(8).                                
009500     03  UT-IDLEGSEL             PIC X(04).                               
009600     EJECT                                                                
009700 PROCEDURE DIVISION.                                                      
009800 MAIN SECTION.                                                            
009900     SKIP2                                                                
010000                                                                          
010100     PERFORM A-INIT                                                       
010200                                                                          
010300     PERFORM S01-READ-WF1011                                              
010400     PERFORM UNTIL END-OF-WF1011                                          
010500       MOVE IN-KDVALISO          TO UT-KDVALISO                           
010600       MOVE IN-REVALUTA          TO UT-REVALUTA-FROM                      
010700       MOVE 1                    TO UT-REVALUTA-TO                        
010800       MOVE IN-PRKURS            TO UT-PRKURS                             
010900       MOVE 'VCCS'               TO UT-IDLEGSEL                           
011000       MOVE IN-DASTADAT          TO UT-DASTADAT                           
011100       MOVE IN-DAREGDAT          TO UT-DAREGDAT                           
011200       PERFORM S11-WRITE-WF1021                                           
011300       PERFORM S01-READ-WF1011                                            
011400     END-PERFORM                                                          
011500                                                                          
011600     PERFORM Z-FINIT                                                      
011700                                                                          
011800     MOVE ZERO                   TO RETURN-CODE                           
011900     GOBACK                                                               
012000     .                                                                    
012100     EJECT                                                                
012200 A-INIT SECTION.                                                          
012300                                                                          
012400     OPEN INPUT  WF1011                                                   
012500                                                                          
012600     OPEN OUTPUT WF1021                                                   
012700     SKIP2                                                                
012800     MOVE IDPGM                  TO POSTSUM-PROGNAMN                      
012900     .                                                                    
013000     EJECT                                                                
013100 Z-FINIT SECTION.                                                         
013200     CLOSE WF1011                                                         
013300           WF1021                                                         
013400     SKIP2                                                                
013500     MOVE 'S'                    TO POSTSUM-OPKOD                         
013600     CALL POSTSUM             USING POSTSUM-PARM                          
013700     .                                                                    
013800     EJECT                                                                
013900 S01-READ-WF1011  SECTION.                                                
014000     READ WF1011               INTO IN-AREA                               
014100     AT END                                                               
014200        MOVE HIGH-VALUE          TO IN-AREA                               
014300        SET END-OF-WF1011        TO TRUE                                  
014400                                                                          
014500     NOT AT END                                                           
014600        MOVE 'WF1011'            TO POSTSUM-FDNAMN                        
014700        MOVE 'WF1021D1'          TO POSTSUM-DDNAMN2                       
014800        MOVE SPACE               TO POSTSUM-TRANSTYP                      
014900        CALL POSTSUM          USING POSTSUM-PARM                          
015000     END-READ                                                             
015100     .                                                                    
015200     EJECT                                                                
015300                                                                          
015400 S11-WRITE-WF1021 SECTION.                                                
015500     WRITE UT-RECORD           FROM UT-AREA                               
015600                                                                          
015700     MOVE SPACES                 TO POSTSUM-TRANSTYP                      
015800     MOVE 'WF1021'               TO POSTSUM-FDNAMN                        
015900     MOVE 'WF1021D2'             TO POSTSUM-DDNAMN2                       
016000     CALL POSTSUM             USING POSTSUM-PARM                          
016100     .                                                                    
016200     EJECT                                                                
