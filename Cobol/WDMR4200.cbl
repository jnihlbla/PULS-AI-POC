000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     WDMR4200.                                                
000400*AUTHOR.         KJELL ANDRE.                                             
000500*DATE-WRITTEN.   92/02/13.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*      LÄSER EN FIL MED JOBB-JCL FRÅN R.PROD.JCL, OCH SÖKER REDA          
001100*      PÅ ALLA EXEC-SATSER.                                               
001200*      FÖR VARJE FUNNEN EXEC-SATS SKRIVS EN POST MED INFO OM JOBB         
001300*      OCH ANROPAD PROCEDUR/PROGRAM.                                      
001400*                                                                         
001500                                                                          
001600     SKIP3                                                                
001700 ENVIRONMENT DIVISION.                                                    
001800     SKIP2                                                                
001900 INPUT-OUTPUT SECTION.                                                    
002000                                                                          
002100 FILE-CONTROL.                                                            
002200     SKIP2                                                                
002300*          --- JCL FÖR JOBB FRÅN JCL-BIBLIOTEKET                          
002400     SELECT WDMR41                     ASSIGN TO WDMR42D1.                
002500     SKIP2                                                                
002600*          --- EXEC-INFO                                                  
002700     SELECT WDMR42                     ASSIGN TO WDMR42D2.                
002800     EJECT                                                                
002900 DATA DIVISION.                                                           
003000     SKIP3                                                                
003100 FILE SECTION.                                                            
003200     SKIP3                                                                
003300 FD  WDMR41                                                               
003400     RECORDING       F                                                    
003500     BLOCK CONTAINS  0.                                                   
003600     SKIP2                                                                
003700 01  FILLER          PIC X(132).                                          
003800     SKIP3                                                                
003900 FD  WDMR42                                                               
004000     RECORDING       F                                                    
004100     BLOCK CONTAINS  0.                                                   
004200     SKIP2                                                                
004300 01  UT-POST         PIC X(17).                                           
004400     EJECT                                                                
004500 WORKING-STORAGE SECTION.                                                 
004600     SKIP2                                                                
004601                                                                          
004610*    -- CHECKED BY WY2000                                                 
004700 77  IDPGM                       PIC X(8)    VALUE 'WDMR4200'.            
004800 77  JA                          PIC X       VALUE 'J'.                   
004900 77  NEJ                         PIC X       VALUE 'N'.                   
005000                                                                          
005100 77  WDMR41-EOF-SW               PIC X       VALUE 'N'.                   
005200     88  END-OF-WDMR41                       VALUE 'J'.                   
005300                                                                          
005400 77  EXEC-PTR                    PIC S9(9)   COMP.                        
005500 77  EXEC-PTR2                   PIC S9(9)   COMP.                        
005600 77  WORK-DATA                   PIC X(20).                               
005700     EJECT                                                                
005800 01  IN-AREA-START               PIC X(24)   VALUE                        
005900                                 'IN-AREA-START  '.                       
006000 01  IN-AREA                PIC X(132).                                   
006010 01  FILLER REDEFINES IN-AREA.                                            
006100     03                     PIC X(63).                                    
006200     03 IN-DSNAME-ORD       PIC X(14).                                    
006300     03 IN-DSN-MBR          PIC X(55).                                    
006310 01  FILLER REDEFINES IN-AREA.                                            
006320     03                     PIC X(1).                                     
006330     03 IN-REC-ORD          PIC X(3).                                     
006340     03                     PIC X(26).                                    
006350     03 JCL-RAD-INFO.                                                     
006351       05  FILLER                    PIC XX.                              
006352         88 JCL-RAD                     VALUE '//'.                       
006353       05  JCL-DATA                  PIC X(69).                           
006355       05  FILLER REDEFINES JCL-DATA PIC X.                               
006356         88 KOMMENTAR-RAD               VALUE '*'.                        
006400                                                                          
007900     EJECT                                                                
008000 01  UT-AREA-START               PIC X(24)   VALUE                        
008100                                 'UT-AREA-START  '.                       
008200     SKIP2                                                                
008300 01  UT-AREA.                                                             
008400     03  UT-JOBBNAMN             PIC X(8).                                
008500     03  UT-EXECNAMN             PIC X(8).                                
008600     03  UT-EXECTYP              PIC X.                                   
008700     EJECT                                                                
008800 PROCEDURE DIVISION.                                                      
008900     SKIP2                                                                
009000                                                                          
009100     PERFORM A-INIT                                                       
009200     PERFORM S01-LAES-WDMR41                                              
009300     PERFORM UNTIL END-OF-WDMR41                                          
009400       IF IN-DSNAME-ORD = 'Data Set Name:'                                
009510         MOVE ZERO TO TALLY                                               
009520         INSPECT IN-DSN-MBR TALLYING TALLY FOR                            
009530           CHARACTERS BEFORE INITIAL '('                                  
009540         ADD 2 TO TALLY                                                   
009550         UNSTRING IN-DSN-MBR DELIMITED BY ')'                             
009560           INTO UT-JOBBNAMN                                               
009570           WITH POINTER TALLY                                             
009600       END-IF                                                             
009700       IF (IN-REC-ORD = 'Rec' OR 'REC')                                   
009710       AND JCL-RAD AND NOT KOMMENTAR-RAD                                  
009800         MOVE ZERO TO EXEC-PTR                                            
009900         INSPECT JCL-DATA TALLYING EXEC-PTR FOR CHARACTERS                
010000                 BEFORE INITIAL ' EXEC '                                  
010100         IF EXEC-PTR < 50                                                 
010200           ADD 6 TO EXEC-PTR                                              
010300           INSPECT JCL-DATA (EXEC-PTR : 70 - EXEC-PTR )                   
010400                   TALLYING EXEC-PTR FOR LEADING SPACE                    
010500           MOVE JCL-DATA (EXEC-PTR : 13)                                  
010600                TO WORK-DATA                                              
010700           INSPECT WORK-DATA REPLACING CHARACTERS BY SPACE                
010800                   AFTER INITIAL  ','                                     
010900           INSPECT WORK-DATA REPLACING ALL ',' BY SPACE                   
011000                                                                          
011100           IF WORK-DATA (1:4) = 'PGM='                                    
011200             MOVE WORK-DATA (5:12) TO UT-EXECNAMN                         
011300             MOVE 'P' TO UT-EXECTYP                                       
011400           ELSE IF WORK-DATA (1:5) = 'PROC='                              
011401             MOVE WORK-DATA (6:13) TO UT-EXECNAMN                         
011402             MOVE SPACE TO UT-EXECTYP                                     
011410           ELSE                                                           
011500             MOVE WORK-DATA TO UT-EXECNAMN                                
011600             MOVE SPACE TO UT-EXECTYP                                     
011700           END-IF                                                         
011710           END-IF                                                         
011800           WRITE UT-POST FROM UT-AREA                                     
011900         END-IF                                                           
012000       END-IF                                                             
012100                                                                          
012200       PERFORM S01-LAES-WDMR41                                            
012300     END-PERFORM                                                          
012400                                                                          
012500                                                                          
012600     PERFORM Z-FINIT                                                      
012700                                                                          
012800     MOVE ZERO TO RETURN-CODE                                             
012900     GOBACK                                                               
013000     .                                                                    
013100     EJECT                                                                
013200 A-INIT SECTION.                                                          
013300                                                                          
013400     OPEN INPUT  WDMR41                                                   
013500     OPEN OUTPUT WDMR42                                                   
013600     .                                                                    
013700     EJECT                                                                
013800 Z-FINIT SECTION.                                                         
013900     CLOSE WDMR41                                                         
014000           WDMR42                                                         
014100     .                                                                    
014200     EJECT                                                                
014300 S01-LAES-WDMR41  SECTION.                                                
014400     SKIP2                                                                
014500     READ WDMR41 INTO IN-AREA                                             
014600     AT END                                                               
014700        SET END-OF-WDMR41 TO TRUE                                         
014800     END-READ                                                             
014900     .                                                                    
