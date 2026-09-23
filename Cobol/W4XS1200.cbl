000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4XS1200.                                                
000400*AUTHOR.         STEFANO GIOBBI.                                          
000500*DATE-WRITTEN.   94/06/20.                                                
000600                                                                          
000700*                                                                         
000800*    FUNKTION:                                                            
000900*                                                                         
001000*        PROGRAMMET INGÅR I UPPFÖLJNINGSMILJÖN OCH KÖRS NORMALT           
001100*        FRÅN VIOS.                                                       
001200*                                                                         
001300*        PROGRAMMET SÅLLAR UT DE POSTER FRÅN DAGLIGA LAGERBANDET          
001400*        VARS SVENSKA BENÄMNING MOTSVARAR DET SÖKBEGREPP SOM              
001500*        FYLLTS I PÅ PANEL W4XS12.                                        
001600*        SÖKBEGREPPET KOMMER IN SOM SYSIN DD *-KORT I PROCEDUREN.         
001700*                                                                         
001800*                                                                         
001900                                                                          
002000     SKIP3                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200     SKIP2                                                                
002300 INPUT-OUTPUT SECTION.                                                    
002400                                                                          
002500 FILE-CONTROL.                                                            
002600     SKIP2                                                                
002700*          --- INDATA FRÅN PANEL W4XS12                                   
002800     SELECT SYSIN                      ASSIGN TO W4XS12D1.                
002900     SKIP2                                                                
003000*          --- DLB - ENDAST CDC                                           
003100     SELECT WCDCDAY                    ASSIGN TO W4XS12D2.                
003200     SKIP2                                                                
003300*          --- ALLA ART FRÅN DLB SOM MOTSV SÖKBEGREPPET                   
003400     SELECT W4XS12                     ASSIGN TO W4XS12D3.                
003500     EJECT                                                                
003600 DATA DIVISION.                                                           
003700     SKIP3                                                                
003800 FILE SECTION.                                                            
003900     SKIP3                                                                
004000 FD  SYSIN                                                                
004100     RECORDING       F                                                    
004200     BLOCK CONTAINS  0.                                                   
004300                                                                          
004400 01  SYS-INPOST      PIC X(80).                                           
004500     SKIP3                                                                
004600 FD  WCDCDAY                                                              
004700     RECORDING       F                                                    
004800     BLOCK CONTAINS  0.                                                   
004900                                                                          
005000*01  -COPY WCDCPART     -L.                                               
005100     SKIP3                                                                
005200 FD  W4XS12                                                               
005300     RECORDING       F                                                    
005400     BLOCK CONTAINS  0.                                                   
005500                                                                          
005600*01  POST -COPY WCDCPART -PRE  UT-  -L.                                   
005700     EJECT                                                                
005800 WORKING-STORAGE SECTION.                                                 
005900                                                                          
005901                                                                          
005910*    -- CHECKED BY WY2000                                                 
006000 77  IDPGM                       PIC X(8)    VALUE 'W4XS1200'.            
006100 77  YES                         PIC X       VALUE 'Y'.                   
006200 77  NOO                         PIC X       VALUE 'N'.                   
006300                                                                          
006400 77  SYSIN-EOF-SW                PIC X       VALUE 'N'.                   
006500     88  END-OF-SYSIN                        VALUE 'Y'.                   
006600                                                                          
006700 77  WCDCDAY-EOF-SW               PIC X       VALUE 'N'.                  
006800     88  END-OF-WCDCDAY                       VALUE 'Y'.                  
006900     EJECT                                                                
007000 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
007100 01  FILLER REDEFINES TODAYS-DATE.                                        
007200     03  TODAYS-DATE-YEAR        PIC 9(2).                                
007300     03  TODAYS-DATE-MONTH       PIC 9(2).                                
007400     03  TODAYS-DATE-DAY         PIC 9(2).                                
007500     EJECT                                                                
007600 01  GENERAL-SUBPROGRAM.                                                  
007700*                                                                         
007800     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
007900     SKIP2                                                                
008000*    --- PARAMETERS TO ABEND                                              
008100                                                                          
008200 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
008300 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
008400     SKIP2                                                                
008500 01  ERRTEXT.                                                             
008600     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
008700     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
008800     EJECT                                                                
008900 01  SWITCH-AREA.                                                         
009000     03  FILLER                  PIC X(8)    VALUE 'SWITCH-'.             
009100     03  SW-TRAFF                PIC X(1)    VALUE 'N'.                   
009200     SKIP2                                                                
009300 01  TALLY-AREA.                                                          
009400     03  FILLER                  PIC X(8)    VALUE 'TALLY-A'.             
009500     03  TALLY-HOO               PIC S9(2) COMP-3 VALUE +0.               
009600     03  SOK-LENGTH              PIC S9(2) COMP-3 VALUE +0.               
009700     EJECT                                                                
009800 01  SYS-AREA-START              PIC X(24)   VALUE                        
009900                                 'SYS-AREA-START  '.                      
010000                                                                          
010100 01  SYS-INAREA.                                                          
010200    03  SYS-SOKBEGREPP           PIC X(25) VALUE SPACE.                   
010300    03  FILLER                   PIC X(55) VALUE SPACE.                   
010400                                                                          
010500     EJECT                                                                
010600 01  DLB-AREA-START              PIC X(24)   VALUE                        
010700                                 'DLB-AREA-START  '.                      
010800     SKIP2                                                                
010900                                                                          
011000 01  DLB-INAREA.                                                          
011100*03  AREA -COPY WCDCPART    -PRE DLB-                                     
011200     EJECT                                                                
011300 01  UT-AREA-START               PIC X(24)   VALUE                        
011400                                 'UT-AREA-START  '.                       
011500     SKIP2                                                                
011600                                                                          
011700*01  AREA -COPY WCDCPART    -PRE UT-                                      
011800     EJECT                                                                
011900 PROCEDURE DIVISION.                                                      
012000                                                                          
012100     PERFORM A-INIT                                                       
012200                                                                          
012300     PERFORM S01-READ-SYSIN                                               
012400     PERFORM B-BESTAM-SOKBEGREPP                                          
012500                                                                          
012600     PERFORM S02-READ-WCDCDAY                                             
012700     PERFORM UNTIL END-OF-WCDCDAY                                         
012800                                                                          
012900       PERFORM C-KOLLA-BEART-SVE                                          
013000       IF TALLY-HOO > 0                                                   
013100         PERFORM S11-WRITE-W4XS12                                         
013200       END-IF                                                             
013300       PERFORM S02-READ-WCDCDAY                                           
013400                                                                          
013500     END-PERFORM                                                          
013600                                                                          
013700     PERFORM Z-FINIT                                                      
013800                                                                          
013900     MOVE ZERO TO RETURN-CODE                                             
014000     GOBACK                                                               
014100     .                                                                    
014200     EJECT                                                                
014300 A-INIT SECTION.                                                          
014400                                                                          
014500     OPEN INPUT  SYSIN                                                    
014600                 WCDCDAY                                                  
014700                                                                          
014800     OPEN OUTPUT W4XS12                                                   
014900                                                                          
015000     ACCEPT TODAYS-DATE FROM DATE                                         
015100     .                                                                    
015200     EJECT                                                                
015300 B-BESTAM-SOKBEGREPP SECTION.                                             
015400                                                                          
015500     INSPECT SYS-SOKBEGREPP TALLYING TALLY-HOO FOR ALL SPACES             
015600                                                                          
015700     COMPUTE SOK-LENGTH = LENGTH OF SYS-SOKBEGREPP - TALLY-HOO            
015800     .                                                                    
015900     EJECT                                                                
016000 C-KOLLA-BEART-SVE SECTION.                                               
016100                                                                          
016200     MOVE ZERO TO TALLY-HOO                                               
016300     INSPECT DLB-BEART-SVE TALLYING TALLY-HOO FOR ALL                     
016400       SYS-SOKBEGREPP(1:SOK-LENGTH)                                       
016500     .                                                                    
016600     EJECT                                                                
016700 Z-FINIT SECTION.                                                         
016800                                                                          
016900     CLOSE SYSIN                                                          
017000           WCDCDAY                                                        
017100           W4XS12                                                         
017200     .                                                                    
017300     EJECT                                                                
017400 S01-READ-SYSIN  SECTION.                                                 
017500                                                                          
017600     READ SYSIN INTO SYS-INAREA                                           
017700     AT END                                                               
017800        MOVE HIGH-VALUE   TO SYS-INAREA                                   
017900        SET  END-OF-SYSIN TO TRUE                                         
018000     END-READ                                                             
018100     .                                                                    
018200     EJECT                                                                
018300 S02-READ-WCDCDAY  SECTION.                                               
018400                                                                          
018500     READ WCDCDAY INTO DLB-AREA                                           
018600     AT END                                                               
018700        MOVE HIGH-VALUE    TO DLB-INAREA                                  
018800        SET  END-OF-WCDCDAY TO TRUE                                       
018900     END-READ                                                             
019000     .                                                                    
019100     EJECT                                                                
019200 S11-WRITE-W4XS12 SECTION.                                                
019300                                                                          
019400     MOVE  DLB-WCDCPART TO    UT-WCDCPART                                 
019500     WRITE UT-POST     FROM  UT-AREA                                      
019600     .                                                                    
