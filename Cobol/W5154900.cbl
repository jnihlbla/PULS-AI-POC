000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5154900.                                                
000300 AUTHOR.         SATHEESH RAGUR                                           
000400 DATE-WRITTEN.   NOV 2017.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*                                                                         
000900*       -PROGRAMMET LÄSER      WDH5                                       
001000*                                                                         
001100*    ABENDKODER:                                                          
001200*        U0016 -  . . . .                                                 
001300*        U1000 -  . . . .                                                 
001400*                                                                         
001500                                                                          
001600 ENVIRONMENT DIVISION.                                                    
001700                                                                          
001800 INPUT-OUTPUT SECTION.                                                    
001900                                                                          
002000 FILE-CONTROL.                                                            
002100*          --- INPUT FROM W5156B                                          
002200     SELECT W5156B                     ASSIGN TO W51549D1.                
002300                                                                          
002400*          --- OUTPUT WITH INDIAN CURRENCY                                
002500     SELECT W51549                     ASSIGN TO W51549D2.                
002600                                                                          
002700     EJECT                                                                
002800                                                                          
002900 DATA DIVISION.                                                           
003000                                                                          
003100 FILE SECTION.                                                            
003200 FD  W5156B                                                               
003300     RECORDING       F                                                    
003400     BLOCK CONTAINS  0.                                                   
003500*01  -COPY WDR801    -PRE  IN-  -L.                                       
003600                                                                          
003700 FD  W51549                                                               
003800     RECORDING       F                                                    
003900     BLOCK CONTAINS  0.                                                   
004000*01  POST   -COPY W51549 -PRE  UT-  -L.                                   
004100                                                                          
004200 WORKING-STORAGE SECTION.                                                 
004300 77  IDPGM                        PIC X(8)    VALUE 'W5154900'.           
004400 77  JA                           PIC X       VALUE 'J'.                  
004500 77  NEJ                          PIC X       VALUE 'N'.                  
004600 77  FELTEXT                      PIC X(80).                              
004700 77  WS-ACTUAL-DATE               PIC S9(16) COMP-3 VALUE ZERO.           
004800 77  WS-ACTUAL-DATE-X             PIC X(16)   VALUE ZERO.                 
004900 77  W5156B-EOF-SW                PIC X       VALUE 'N'.                  
005000     88  END-OF-W5156B                        VALUE 'J'.                  
005001 77  WS-KDVALISO-HUV              PIC X(3)    VALUE 'SEK'.                
005010 77  W-DATE-AAMM                  PIC 9(4)    VALUE ZERO.                 
005020 77  WS-SAVE-MONTH                PIC 9(4)    VALUE ZERO.                 
005030 77  W-PRKURS                     PIC S9(6)V9(5) VALUE +0                 
005040                                                   COMP-3.                
005050 77  W-REVALUTA                   PIC S9(5) COMP-3 VALUE ZERO.            
005100                                                                          
005200 01  DYNAMISKA-SUBPROGRAM.                                                
005300     03  ABEND                    PIC X(8)    VALUE 'ABEND'.              
005400     03  CBLTDLI                  PIC X(8)    VALUE 'CBLTDLI '.           
005500     03  FELLOG                   PIC X(8)    VALUE 'FELLOG  '.           
005600     03  POSTSUM                  PIC X(8)    VALUE 'POSTSUM'.            
005610     03  W510CURR                 PIC X(8)    VALUE 'W510CURR'.           
005700                                                                          
006800*    --- PARAMETRAR TILL ABEND                                            
006900 77  RKOD-ABEND                   PIC S9(4)   COMP VALUE +0.              
007000 77  RKOD-ABEND-UTAN-DUMP         PIC S9(4)   COMP VALUE +16.             
007100 77  RKOD-ABEND-MED-DUMP          PIC S9(4)   COMP VALUE +1000.           
007200     EJECT                                                                
007300                                                                          
007400*    --- PARAMETRAR TILL POSTSUM                                          
007500*01  -COPY W0005   -PRE  POSTSUM-                                         
007600     EJECT                                                                
007700                                                                          
007710*01  -COPY W510CURR                                                       
007720                                                                          
007800 01  IN-AREA-START               PIC X(24)   VALUE                        
007900                                 'IN-AREA-START  '.                       
008000                                                                          
008100*01  AREA -COPY WDR801     -PRE IN-                                       
008200*    05   -COPY W510EKHA   -PRE IN- -RED IN-FIL-WDR801-DATA               
008300     EJECT                                                                
008400                                                                          
008500 01  UT-AREA-START               PIC X(24)   VALUE                        
008600                                 'UT-AREA-START  '.                       
008700                                                                          
008800*01  AREA -COPY W51549     -PRE UT-                                       
008900     EJECT                                                                
009000                                                                          
011800 LINKAGE SECTION.                                                         
011900*01  -COPY W0008  -PRE WDG2-                                              
012000     05  FILLER                  PIC X.                                   
012100                                                                          
012200     EJECT                                                                
012300                                                                          
012400 PROCEDURE DIVISION  USING WDG2-PCB.                                      
012500                                                                          
012600 MAIN SECTION.                                                            
012700     ENTRY 'DLITCBL' USING WDG2-PCB.                                      
012800                                                                          
012900     PERFORM A-INIT                                                       
013000                                                                          
013100     PERFORM S01-READ-W5156B                                              
013200     PERFORM UNTIL END-OF-W5156B                                          
013300       PERFORM B-CONVERT-SEK-INR                                          
013400       PERFORM S01-READ-W5156B                                            
013500     END-PERFORM                                                          
013600                                                                          
013700     PERFORM Z-FINIT                                                      
013800     MOVE ZERO TO RETURN-CODE                                             
013900     GOBACK                                                               
014000     .                                                                    
014100     EJECT                                                                
014200                                                                          
014300 A-INIT SECTION.                                                          
014400     OPEN INPUT  W5156B                                                   
014500     OPEN OUTPUT W51549                                                   
014600     MOVE IDPGM    TO POSTSUM-PROGNAMN                                    
014700     .                                                                    
014800     EJECT                                                                
014900                                                                          
015000 B-CONVERT-SEK-INR SECTION.                                               
015710     MOVE IN-FIL-TIREGDAT         TO WS-ACTUAL-DATE-X                     
015720     MOVE WS-ACTUAL-DATE-X(2:4)   TO W-DATE-AAMM                          
015730                                                                          
015740     IF W-DATE-AAMM = WS-SAVE-MONTH                                       
015750       CONTINUE                                                           
015760     ELSE                                                                 
015770       MOVE W-DATE-AAMM           TO WS-SAVE-MONTH                        
015780                                     CURR-TIAAMM                          
015790       MOVE WS-KDVALISO-HUV       TO CURR-KDVALISO-HUV                    
015791       MOVE IN-EKH-KDVALISO       TO CURR-KDVALISO-ROW                    
015792       MOVE 'M'                   TO CURR-KDVALTYP                        
015793       CALL W510CURR USING CURR-W510CURR WDG2-PCB                         
015794       IF CURR-KDSVAR = ' '                                               
015795         MOVE CURR-PRKURS-NEW     TO W-PRKURS                             
015796         MOVE CURR-REVALUTA-TO    TO W-REVALUTA                           
015797       ELSE                                                               
015798         MOVE 1                   TO W-PRKURS                             
015799         MOVE 1                   TO W-REVALUTA                           
015800       END-IF                                                             
015801     END-IF                                                               
015802     COMPUTE IN-EKH-SUBEL  ROUNDED =                                      
015803              IN-EKH-SUBEL / (W-PRKURS / W-REVALUTA)                      
015810     MOVE IN-EKH-IDDC-REC         TO UT-IDDC                              
015900     MOVE IN-EKH-DAVERDAT         TO UT-DAINLEV                           
016000     MOVE IN-EKH-SUBEL            TO UT-PRARTNTO                          
016100     PERFORM S02-WRITE-W51549                                             
016200     .                                                                    
016300     EJECT                                                                
016400                                                                          
016500 Z-FINIT SECTION.                                                         
016600     CLOSE W5156B                                                         
016700     CLOSE W51549                                                         
016800     MOVE 'S' TO POSTSUM-OPKOD                                            
016900     CALL POSTSUM USING POSTSUM-PARM                                      
017000     .                                                                    
017100     EJECT                                                                
017200                                                                          
017300 S01-READ-W5156B  SECTION.                                                
017400     READ W5156B INTO IN-AREA                                             
017500     AT END                                                               
017600        SET END-OF-W5156B TO TRUE                                         
017700                                                                          
017800     NOT AT END                                                           
017900        MOVE 'IN'         TO POSTSUM-TRANSTYP                             
018000        MOVE 'W5156B'     TO POSTSUM-FDNAMN                               
018100        MOVE 'W51549D1'   TO POSTSUM-DDNAMN2                              
018200        CALL POSTSUM USING POSTSUM-PARM                                   
018300     END-READ                                                             
018400     .                                                                    
018500                                                                          
018600 S02-WRITE-W51549 SECTION.                                                
018700     WRITE UT-POST              FROM UT-AREA                              
018800                                                                          
018900     MOVE 'W51549'              TO POSTSUM-FDNAMN                         
019000     MOVE 'W51549D2'            TO POSTSUM-DDNAMN2                        
019100     CALL POSTSUM USING POSTSUM-PARM                                      
019200     .                                                                    
019300                                                                          
