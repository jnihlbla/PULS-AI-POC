000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W9300300.                                                
000300 AUTHOR.         BHAT ARCHANA.                                            
000400 DATE-WRITTEN.   20/09/14.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNCTION:                                                            
000900*        THIS PROGRAM RECEIVES YEARLY RATE FROM BILLIT INTO FILE          
001000*        W93005.                                                          
001100*                                                                         
001200*                                                                         
001300                                                                          
001400     SKIP3                                                                
001500 ENVIRONMENT DIVISION.                                                    
001600     SKIP2                                                                
001700 INPUT-OUTPUT SECTION.                                                    
001800                                                                          
001900 FILE-CONTROL.                                                            
002000     SKIP2                                                                
002100*          --- YEARLY RATES                                               
002200     SELECT W93005                     ASSIGN TO W93003D1.                
002300     EJECT                                                                
002400 DATA DIVISION.                                                           
002500     SKIP3                                                                
002600 FILE SECTION.                                                            
002700     SKIP3                                                                
002800 FD  W93005                                                               
002900     RECORDING       F                                                    
003000     BLOCK CONTAINS  0.                                                   
003100                                                                          
003200*01  POST -COPY W93005 -PRE  UT-  -L.                                     
003300     EJECT                                                                
003400 WORKING-STORAGE SECTION.                                                 
003500                                                                          
003600 77  IDPGM                       PIC X(8)    VALUE 'W9300300'.            
003700 77  YES                         PIC X       VALUE 'J'.                   
003800 77  NOO                         PIC X       VALUE 'N'.                   
003900 77  KDRC-DISPLAY                PIC Z(5).                                
004000     SKIP2                                                                
004100 01  ERR-TEXT.                                                            
004200     03  FILLER                  PIC X(10)   VALUE 'ERROR-TEXT'.          
004300     03  ERR-TEXT-STR            PIC X(72)   VALUE SPACE.                 
004400     EJECT                                                                
004500 01  GENERAL-SUBPROGRAMS.                                                 
004600*                                                                         
004700     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
004800     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
004900     03  WZ01RECV                PIC X(8)    VALUE 'WZ01RECV'.            
005000     EJECT                                                                
005100*    --- PARAMETRAR TILL POSTSUM                                          
005200*                                                                         
005300*01  -COPY W0005   -PRE  POSTSUM-                                         
005400     EJECT                                                                
005500 01  UT-AREA-START               PIC X(24)   VALUE  'UT-AREA'.            
005600                                                                          
005700*01  AREA -COPY W93005     -PRE UT-                                       
005800     EJECT                                                                
005900 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
006000     EJECT                                                                
006100 01  FILLER                      PIC X(16)   VALUE 'RECV-CONTROL'.        
006200     SKIP3                                                                
006300 01  -COPY WZ01RECV                                                       
006400     EJECT                                                                
006500 01  FILLER                      PIC X(16)   VALUE 'RECV-AREA'.           
006600     SKIP3                                                                
006700 01  RECV-AREA.                                                           
006800*    03  -COPY WF1032       -PRE BIL-                                     
006900     EJECT                                                                
007000 LINKAGE SECTION.                                                         
007100                                                                          
007200*01  -COPY W0009   -PRE MSG-                                              
007300     EJECT                                                                
007400 PROCEDURE DIVISION  USING MSG-PCB.                                       
007500 MAIN SECTION.                                                            
007600     ENTRY 'DLITCBL' USING MSG-PCB.                                       
007700                                                                          
007900     PERFORM A-INIT                                                       
008000     PERFORM S03-OPEN-CURR                                                
008200     IF RECV-KDRC = 0                                                     
008210       PERFORM S03-GET-CURR                                               
008400       PERFORM B-RECIEVE                                                  
008500     END-IF                                                               
008600                                                                          
008700     PERFORM S03-CLOSE-CURR                                               
008800                                                                          
008900                                                                          
009000     PERFORM Z-FINIT                                                      
009100                                                                          
009200     MOVE ZERO TO RETURN-CODE                                             
009300     GOBACK                                                               
009400     .                                                                    
009500     EJECT                                                                
009600 A-INIT SECTION.                                                          
009700     SKIP2                                                                
009800                                                                          
009900     OPEN OUTPUT W93005                                                   
010000                                                                          
010100                                                                          
010200     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
010300     .                                                                    
010400     EJECT                                                                
010500 B-RECIEVE SECTION.                                                       
010600                                                                          
010700     PERFORM UNTIL RECV-KDRC > 0                                          
010800       PERFORM BA-MOVE-FIELDS                                             
010900       PERFORM S11-WRITE-W93005                                           
011000       PERFORM S03-GET-CURR                                               
011100     END-PERFORM                                                          
011200     .                                                                    
011300     EJECT                                                                
011400 BA-MOVE-FIELDS SECTION.                                                  
011500                                                                          
011600     MOVE BIL-IDLEGSEL       TO UT-IDLEGSEL                               
011700     MOVE BIL-KDVALISO       TO UT-KDVALISO                               
011800     MOVE BIL-DASTADAT       TO UT-DASTADAT                               
011900     MOVE BIL-REVALUTA-FROM  TO UT-REVALUTA-FROM                          
012000     MOVE BIL-REVALUTA-TO    TO UT-REVALUTA-TO                            
012100     MOVE BIL-PRKURS-NEW     TO UT-PRKURS-NEW                             
012200     MOVE BIL-DAREGDAT       TO UT-DAREGDAT                               
012300     MOVE BIL-DAUPPDAT       TO UT-DAUPPDAT                               
012400     MOVE BIL-DADELDAT       TO UT-DADELDAT                               
012500     MOVE BIL-IDUSER         TO UT-IDUSER                                 
012600     .                                                                    
012700     EJECT                                                                
012800 Z-FINIT SECTION.                                                         
012900                                                                          
013000                                                                          
013100     CLOSE W93005                                                         
013200     SKIP2                                                                
013300     MOVE 'S' TO POSTSUM-OPKOD                                            
013400     CALL POSTSUM USING POSTSUM-PARM                                      
013500     .                                                                    
013600     EJECT                                                                
013700 S11-WRITE-W93005 SECTION.                                                
013800     SKIP2                                                                
013900     WRITE UT-POST FROM UT-AREA                                           
014000                                                                          
014100     MOVE SPACES    TO POSTSUM-TRANSTYP                                   
014200     MOVE 'W93005 ' TO POSTSUM-FDNAMN                                     
014300     MOVE 'W93003D1' TO POSTSUM-DDNAMN2                                   
014400     CALL POSTSUM USING POSTSUM-PARM                                      
014500     .                                                                    
014600     EJECT                                                                
014700*    --- DISPATCHER-SECTIONS                                              
014800 S03-OPEN-CURR SECTION.                                                   
014900                                                                          
015000     MOVE 'OPEN'                       TO RECV-KDFUNC                     
015100     MOVE 'CARPARTS.PULS.YEARCURRENCY' TO RECV-ADDISPABS                  
015200     CALL WZ01RECV USING RECV-CONTROL-AREA RECV-OPEN-AREA                 
015300                                                                          
015400     IF RECV-KDRC = 0                                                     
015401     OR RECV-KDRC = 20                                                    
015402        CONTINUE                                                          
015410     ELSE                                                                 
015500       MOVE RECV-KDRC TO KDRC-DISPLAY                                     
015600       STRING 'WZ01RECV OPEN ERROR RC=' KDRC-DISPLAY                      
015700       DELIMITED BY SIZE INTO ERR-TEXT-STR                                
015800       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
015900     END-IF                                                               
016000     .                                                                    
016100     SKIP3                                                                
016200 S03-GET-CURR SECTION.                                                    
016300                                                                          
016400     MOVE 'GET'                      TO RECV-KDFUNC                       
016500     MOVE LENGTH OF RECV-AREA        TO RECV-KVDLEN                       
016600     CALL WZ01RECV USING RECV-CONTROL-AREA RECV-KVDLEN RECV-AREA          
016700                                                                          
016800     IF RECV-KDRC > 1                                                     
016900       MOVE RECV-KDRC TO KDRC-DISPLAY                                     
017000       STRING 'WZ01RECV GET ERROR RC=' KDRC-DISPLAY                       
017100       DELIMITED BY SIZE INTO ERR-TEXT-STR                                
017200       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
017300     END-IF                                                               
017400     .                                                                    
017500     SKIP3                                                                
017600 S03-CLOSE-CURR SECTION.                                                  
017700                                                                          
017800     MOVE 'CLOSE'                    TO RECV-KDFUNC                       
017900     CALL WZ01RECV USING RECV-CONTROL-AREA                                
018000                                                                          
018100     IF RECV-KDRC > 0                                                     
018200       MOVE RECV-KDRC TO KDRC-DISPLAY                                     
018300       STRING 'WZ01RECV CLOSE ERROR RC=' KDRC-DISPLAY                     
018400       DELIMITED BY SIZE INTO ERR-TEXT-STR                                
018500       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
018600     END-IF                                                               
018700     .                                                                    
018800     EJECT                                                                
