000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5704900.                                                
000300 AUTHOR.         ANDERS HENRIKSSON                                        
000400 DATE-WRITTEN.   MAY 2012.                                                
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
002100*          --- INPUT FROM W5706B                                          
002200     SELECT W5706B                     ASSIGN TO W57049D1.                
002300                                                                          
002400*          --- OUTPUT WITH CHINESE CURRENCY                               
002500     SELECT W57049                     ASSIGN TO W57049D2.                
002600                                                                          
002700     EJECT                                                                
002800                                                                          
002900 DATA DIVISION.                                                           
003000                                                                          
003100 FILE SECTION.                                                            
003200 FD  W5706B                                                               
003300     RECORDING       F                                                    
003400     BLOCK CONTAINS  0.                                                   
003500*01  -COPY WDR801    -PRE  IN-  -L.                                       
003600                                                                          
003700 FD  W57049                                                               
003800     RECORDING       F                                                    
003900     BLOCK CONTAINS  0.                                                   
004000*01  POST   -COPY W57049 -PRE  UT-  -L.                                   
004100                                                                          
004200 WORKING-STORAGE SECTION.                                                 
004300 77  IDPGM                        PIC X(8)    VALUE 'W5704900'.           
004400 77  JA                           PIC X       VALUE 'J'.                  
004500 77  NEJ                          PIC X       VALUE 'N'.                  
004600 77  FELTEXT                      PIC X(80).                              
004610 77  WS-KDVALISO-HUV              PIC X(3)    VALUE 'SEK'.                
004700 77  WS-ACTUAL-DATE               PIC S9(16) COMP-3 VALUE ZERO.           
004800 77  WS-ACTUAL-DATE-X             PIC X(16)   VALUE ZERO.                 
004801 77  W-DATE-AAMM                  PIC 9(4)    VALUE ZERO.                 
004810 77  WS-SAVE-MONTH                PIC 9(4)    VALUE ZERO.                 
004820 77  W-PRKURS                     PIC S9(6)V9(5) VALUE +0                 
004830                                                   COMP-3.                
004840 77  W-REVALUTA                   PIC S9(5) COMP-3 VALUE ZERO.            
004900 77  W5706B-EOF-SW                PIC X       VALUE 'N'.                  
005000     88  END-OF-W5706B                        VALUE 'J'.                  
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
007610*01  -COPY WWDC99                                                         
007700                                                                          
007710 01  W510CURR-AREA               PIC X(24)   VALUE                        
007720                                 'W510CURR-AREA  '.                       
007730*01  -COPY W510CURR                                                       
007740     EJECT                                                                
007750                                                                          
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
008800*01  AREA -COPY W57049     -PRE UT-                                       
008900     EJECT                                                                
009000                                                                          
011800 LINKAGE SECTION.                                                         
011900*01  -COPY W0008  -PRE 9305-                                              
012000     05  FILLER                  PIC X.                                   
012100                                                                          
012200     EJECT                                                                
012300                                                                          
012400 PROCEDURE DIVISION  USING 9305-PCB.                                      
012500                                                                          
012600 MAIN SECTION.                                                            
012700     ENTRY 'DLITCBL' USING 9305-PCB.                                      
012800                                                                          
012900     PERFORM A-INIT                                                       
013000                                                                          
013100     PERFORM S01-READ-W5706B                                              
013200     PERFORM UNTIL END-OF-W5706B                                          
013210       MOVE IN-EKH-IDDC-REC TO WS-IDDC                                    
013220       IF XDC-NON-VCC-OWNED OR LDC-CN                                     
013300         PERFORM B-CONVERT-SEK-LOC-CURRENCY                               
013301       ELSE                                                               
013302         CONTINUE                                                         
013310       END-IF                                                             
013400       PERFORM S01-READ-W5706B                                            
013500     END-PERFORM                                                          
013600                                                                          
013700     PERFORM Z-FINIT                                                      
013800     MOVE ZERO TO RETURN-CODE                                             
013900     GOBACK                                                               
014000     .                                                                    
014100     EJECT                                                                
014200                                                                          
014300 A-INIT SECTION.                                                          
014400     OPEN INPUT  W5706B                                                   
014500     OPEN OUTPUT W57049                                                   
014600     .                                                                    
014700     EJECT                                                                
014800                                                                          
014900 B-CONVERT-SEK-LOC-CURRENCY SECTION.                                      
014910                                                                          
015000     MOVE IN-FIL-TIREGDAT         TO WS-ACTUAL-DATE-X                     
015100     MOVE WS-ACTUAL-DATE-X(2:4)   TO W-DATE-AAMM                          
015200                                                                          
015620     IF W-DATE-AAMM = WS-SAVE-MONTH                                       
015630       CONTINUE                                                           
015640     ELSE                                                                 
015680       MOVE W-DATE-AAMM           TO WS-SAVE-MONTH                        
015690                                     CURR-TIAAMM                          
015691       MOVE WS-KDVALISO-HUV       TO CURR-KDVALISO-HUV                    
015692       MOVE IN-EKH-KDVALISO       TO CURR-KDVALISO-ROW                    
015693       MOVE 'M'                   TO CURR-KDVALTYP                        
015694       CALL W510CURR USING CURR-W510CURR 9305-PCB                         
015695       IF CURR-KDSVAR = ' '                                               
015696         MOVE CURR-PRKURS-NEW     TO W-PRKURS                             
015697         MOVE CURR-REVALUTA-TO    TO W-REVALUTA                           
015698       ELSE                                                               
015700         MOVE 1                   TO W-PRKURS                             
015701         MOVE 1                   TO W-REVALUTA                           
015703       END-IF                                                             
015706     END-IF                                                               
015708     COMPUTE IN-EKH-SUBEL  ROUNDED =                                      
015709              IN-EKH-SUBEL / (W-PRKURS / W-REVALUTA)                      
015710     MOVE IN-EKH-IDDC-REC       TO UT-IDDC                                
015800     MOVE IN-EKH-DAVERDAT       TO UT-DAINLEV                             
015900     MOVE IN-EKH-SUBEL          TO UT-PRARTNTO                            
015910     IF IN-FIL-CT-IDSYSTEM = 'W570'                                       
015920       MOVE 'CN05'              TO UT-KDTRADP                             
015930     ELSE                                                                 
015940       MOVE IN-FIL-CT-IDSYSTEM  TO UT-KDTRADP                             
015950     END-IF                                                               
016000     PERFORM S02-WRITE-W57049                                             
016100     .                                                                    
016200     EJECT                                                                
016300                                                                          
016400 Z-FINIT SECTION.                                                         
016500     CLOSE W5706B                                                         
016600     CLOSE W57049                                                         
016700     MOVE 'S' TO POSTSUM-OPKOD                                            
016800     CALL POSTSUM USING POSTSUM-PARM                                      
016900     .                                                                    
017000     EJECT                                                                
017100                                                                          
017400 S01-READ-W5706B  SECTION.                                                
017500     READ W5706B INTO IN-AREA                                             
017600     AT END                                                               
017700        SET END-OF-W5706B TO TRUE                                         
017800                                                                          
017900     NOT AT END                                                           
018000        MOVE 'IN'         TO POSTSUM-TRANSTYP                             
018100        MOVE 'W5706B'     TO POSTSUM-FDNAMN                               
018200        MOVE 'W57049D1'   TO POSTSUM-DDNAMN2                              
018300        CALL POSTSUM USING POSTSUM-PARM                                   
018400     END-READ                                                             
018500     .                                                                    
018600                                                                          
018700 S02-WRITE-W57049 SECTION.                                                
018800     WRITE UT-POST              FROM UT-AREA                              
018900                                                                          
019000     MOVE 'UT'                  TO POSTSUM-TRANSTYP                       
019100     MOVE 'W57049'              TO POSTSUM-FDNAMN                         
019200     MOVE 'W57049D2'            TO POSTSUM-DDNAMN2                        
019300     CALL POSTSUM USING POSTSUM-PARM                                      
019400     .                                                                    
019500                                                                          
