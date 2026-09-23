000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5125400.                                                
000300 AUTHOR.         SARASWATHY.                                              
000400 DATE-WRITTEN.   APR 2017.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*                                                                         
000900*       -PROGRAM CONVERTS CNY,USD TO SEK                                  
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
002100*          --- INPUT FROM W5106A                                          
002200     SELECT W5106A                     ASSIGN TO W51254D1.                
002300                                                                          
002400*          --- OUTPUT WITH CNY,USD CURRENCY                               
002500     SELECT W51254                     ASSIGN TO W51254D2.                
002600                                                                          
002700     EJECT                                                                
002800                                                                          
002900 DATA DIVISION.                                                           
003000                                                                          
003100 FILE SECTION.                                                            
003200 FD  W5106A                                                               
003300     RECORDING       F                                                    
003400     BLOCK CONTAINS  0.                                                   
003500*01  -COPY WDR901    -PRE  IN-  -L.                                       
003600                                                                          
003700 FD  W51254                                                               
003800     RECORDING       F                                                    
003900     BLOCK CONTAINS  0.                                                   
004000*01  POST   -COPY W51254 -PRE  UT-  -L.                                   
004100                                                                          
004200 WORKING-STORAGE SECTION.                                                 
004300 77  IDPGM                        PIC X(8)    VALUE 'W5125400'.           
004400 77  JA                           PIC X       VALUE 'J'.                  
004500 77  NEJ                          PIC X       VALUE 'N'.                  
004600 77  FELTEXT                      PIC X(80).                              
004700 77  W-DATE-AAMM                  PIC 9(4)    VALUE ZERO.                 
004710 77  WS-KDVALISO-HUV              PIC X(3)    VALUE 'SEK'.                
004720 77  WS-ACTUAL-DATE               PIC S9(16) COMP-3 VALUE ZERO.           
004800 77  WS-ACTUAL-DATE-X             PIC X(16)   VALUE ZERO.                 
004900 77  W5106A-EOF-SW                PIC X       VALUE 'N'.                  
005000     88  END-OF-W5106A                        VALUE 'J'.                  
005100                                                                          
005200 01  DYNAMISKA-SUBPROGRAM.                                                
005300     03  ABEND                    PIC X(8)    VALUE 'ABEND'.              
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
007720     EJECT                                                                
007730                                                                          
007800 01  IN-AREA-START               PIC X(24)   VALUE                        
007900                                 'IN-AREA-START  '.                       
008000                                                                          
008100*01  AREA -COPY WDR901     -PRE IN-                                       
008200*    05   -COPY W510EKHA   -PRE IN- -RED IN-FIL-WDR901-DATA               
008300     EJECT                                                                
008400                                                                          
008500 01  UT-AREA-START               PIC X(24)   VALUE                        
008600                                 'UT-AREA-START  '.                       
008700                                                                          
008800*01  AREA -COPY W51254     -PRE UT-                                       
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
013100     PERFORM S01-READ-W5106A                                              
013200     PERFORM UNTIL END-OF-W5106A                                          
013300       PERFORM B-CONVERT-CNY-SEK                                          
013400       PERFORM S01-READ-W5106A                                            
013500     END-PERFORM                                                          
013600                                                                          
013700     MOVE ZERO TO RETURN-CODE                                             
013800     GOBACK                                                               
013900     .                                                                    
014000     EJECT                                                                
014100                                                                          
014200 A-INIT SECTION.                                                          
014300     OPEN INPUT  W5106A                                                   
014400     OPEN OUTPUT W51254                                                   
014500     .                                                                    
014600     EJECT                                                                
014700                                                                          
014800 B-CONVERT-CNY-SEK SECTION.                                               
014900      IF IN-EKH-KDVALISO = 'CNY' OR 'USD'                                 
015000        MOVE IN-EKH-KDVALISO         TO UT-KDVALISO                       
015010                                        CURR-KDVALISO-ROW                 
015100        MOVE IN-EKH-SUBEL            TO UT-PRARTNTO                       
015200        MOVE IN-FIL-DAREGDAT         TO WS-ACTUAL-DATE-X                  
015400        MOVE WS-ACTUAL-DATE-X(3:2)   TO W-DATE-AAMM(1:2)                  
015410        MOVE WS-ACTUAL-DATE-X(5:2)   TO W-DATE-AAMM(3:2)                  
015420        MOVE W-DATE-AAMM             TO CURR-TIAAMM                       
015430        MOVE WS-KDVALISO-HUV         TO CURR-KDVALISO-HUV                 
015440        MOVE 'M'                     TO CURR-KDVALTYP                     
015500        CALL W510CURR USING CURR-W510CURR WDG2-PCB                        
015600        IF CURR-KDSVAR = ' '                                              
015610           CONTINUE                                                       
015620        ELSE                                                              
015621           MOVE 1                    TO CURR-PRKURS-NEW                   
015630        END-IF                                                            
015700        COMPUTE IN-EKH-SUBEL  ROUNDED =                                   
015800                IN-EKH-SUBEL * CURR-PRKURS-NEW                            
015900        MOVE IN-EKH-SUBEL            TO UT-PRARTNTO-SEK                   
016100        MOVE IN-EKH-IDDC-REC         TO UT-IDDC-REC                       
016200        MOVE IN-EKH-IDDC-SEND        TO UT-IDDC-SEND                      
016300        MOVE IN-EKH-DAVERDAT         TO UT-DAINLEV                        
016400        MOVE IN-EKH-IDVERGL          TO UT-IDVERGL                        
016600        PERFORM S02-WRITE-W51254                                          
016700      END-IF                                                              
018800     .                                                                    
018900     EJECT                                                                
019000                                                                          
019200                                                                          
019300 S01-READ-W5106A  SECTION.                                                
019400     READ W5106A INTO IN-AREA                                             
019500     AT END                                                               
019600        SET END-OF-W5106A TO TRUE                                         
019700                                                                          
019800     NOT AT END                                                           
019900        MOVE 'IN'         TO POSTSUM-TRANSTYP                             
020000        MOVE 'W5106A'     TO POSTSUM-FDNAMN                               
020100        MOVE 'W51254D1'   TO POSTSUM-DDNAMN2                              
020200        CALL POSTSUM USING POSTSUM-PARM                                   
020300     END-READ                                                             
020400     .                                                                    
020500                                                                          
020600 S02-WRITE-W51254 SECTION.                                                
020700     WRITE UT-POST              FROM UT-AREA                              
020800                                                                          
020900     MOVE 'W51254'              TO POSTSUM-FDNAMN                         
021000     MOVE 'W51254D2'            TO POSTSUM-DDNAMN2                        
021100     CALL POSTSUM USING POSTSUM-PARM                                      
021200     .                                                                    
021300                                                                          
