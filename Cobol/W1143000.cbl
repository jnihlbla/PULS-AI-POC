000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.             W1143000.                                        
000400 AUTHOR.                 LOTTA.                                           
000500     DATE-WRITTEN.       DEC-88.                                          
000600*                                                                         
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*            MÄRKER ARTIKLAR I NYPON DÄR BEST,AVTAL LÄGGS UPP             
001100*            PÅ ART.REG (ARTIKLARNA ÄR KÖPTA AV INKÖP PV).                
001200     EJECT                                                                
001300 ENVIRONMENT DIVISION.                                                    
001400     SKIP2                                                                
001500 INPUT-OUTPUT SECTION.                                                    
001600                                                                          
001700 FILE-CONTROL.                                                            
001800     SKIP2                                                                
001900*                                                                         
002000     SELECT  W11430-INFIL             ASSIGN TO    W11430D1.              
002100*                                                                         
002200     SELECT  W11430-NY-GEN            ASSIGN TO    W11430D2.              
002300     EJECT                                                                
002400 DATA DIVISION.                                                           
002500     SKIP2                                                                
002600 FILE SECTION.                                                            
002700     SKIP2                                                                
002800 FD  W11430-INFIL                                                         
002900     LABEL RECORD STANDARD                                                
003000     RECORDING      F                                                     
003100     BLOCK CONTAINS 0.                                                    
003200     SKIP2                                                                
003300 01  W11430-INPOST.                                                       
003400*03  -COPY  W212R22  -L.                                                  
003500 ++INCLUDE   W212R22CC0                                                   
003600     EJECT                                                                
003700 FD  W11430-NY-GEN                                                        
003800     LABEL RECORD STANDARD                                                
003900     RECORDING      F                                                     
004000     BLOCK CONTAINS 0.                                                    
004100     SKIP2                                                                
004200 01  W11430-UTPOST.                                                       
004300*03  -COPY  W212R22   -L.                                                 
004400 ++INCLUDE   W212R22CC0                                                   
004500     EJECT                                                                
004600 WORKING-STORAGE SECTION.                                                 
004700     SKIP2                                                                
004701                                                                          
004710*    -- CHECKED BY WY2000                                                 
004800 77  PROGRAM-NAMN                PIC X(8) VALUE 'W1143000'.               
004900     SKIP2                                                                
005000*    ---- FLAGGOR / SWITCHAR -----------------------------------          
005100 77  W11430-EOF                  PIC X       VALUE 'N'.                   
005200 77  JA                          PIC X       VALUE 'J'.                   
005300 77  NEJ                         PIC X       VALUE 'N'.                   
005400 77  WS-ANT-REPL                 PIC S9(3)   VALUE +0    COMP-3.          
005500*                                                                         
005600     EJECT                                                                
005700*--------------------------------------------------------------*          
005800*    SUBPROGRAM OCH PARAMETERAREOR                                        
005900*--------------------------------------------------------------*          
006000     SKIP2                                                                
006100 01  DYNAMISKA-SUBPROGRAM.                                                
006200   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM '.            
006300   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
006400   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
006500   03  W980SOP                   PIC X(8)    VALUE 'W980SOP '.            
006600     EJECT                                                                
006700*    ---- PARAMETRAR TILL W980SOP                                         
006800 01  FILLER               PIC X(16)   VALUE  'W980SOP-AREA'.              
006900*01  -COPY WSOPAREA.                                                      
007000 ++INCLUDE WSOPAREAC0                                                     
007100     EJECT                                                                
007200*    ---- PARAMETRAR TILL POSTSUM                                         
007300 01  FILLER               PIC X(16)   VALUE  'POSTSUM-AREA'.              
007400*01  -COPY W0005 -PRE POSTSUM-.                                           
007500*++INCLUDE W0005CCCC0                                                     
007600     EJECT                                                                
007700*--------------------------------------------------------------*          
007800*    IN-AREA                                                   *          
007900*--------------------------------------------------------------*          
008000 01  FILLER                      PIC X(16)   VALUE 'IN-AREA'.             
008100 01  IN-AREA.                                                             
008200     03  IN-POST                 PIC X(54).                               
008300*    03  IN-R22 -COPY W212R22 -PRE IN- -RED IN-POST.                      
008400 ++INCLUDE W212R22CC0                                                     
008500                                                                          
008600*    03  IN-R23 -COPY W212R23 -RED IN-POST -L.                            
008700 ++INCLUDE W212R23CC0                                                     
008800     EJECT                                                                
008900*--------------------------------------------------------------*          
009000*    UT-AREA                                                   *          
009100*--------------------------------------------------------------*          
009200 01  FILLER                      PIC X(16)   VALUE 'UT-AREA'.             
009300 01  UT-AREA.                                                             
009400     03  UT-POST                 PIC X(54).                               
009500*    03  UT-R22 -COPY W212R22 -PRE UT- -RED UT-POST.                      
009600 ++INCLUDE W212R22CC0                                                     
009700                                                                          
009800*    03  UT-R23 -COPY W212R23 -RED UT-POST -L.                            
009900 ++INCLUDE W212R23CC0                                                     
010000*--------------------------------------------------------------*          
010100*    ARBETS-AREOR FÖR IMS-SEKTIONERNA                                     
010200*--------------------------------------------------------------*          
010300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010400     SKIP2                                                                
010500*    ---- STATUSKOD FRÅN IMS                                              
010600 01  STATUS-WS                   PIC XX.                                  
010700     88  SEGMENT-FINNS                       VALUE '  '.                  
010800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
010900     SKIP2                                                                
011000 01  GODK-STATUSKODER.                                                    
011100   03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                  
011200     SKIP2                                                                
011300 01  SSA1                        PIC X(64).                               
011400     SKIP2                                                                
011500*--------------------------------------------------------------*          
011600*    NYCKLAR OCH SÖKFÄLT TILL DLI                              *          
011700*--------------------------------------------------------------*          
011800 01  FILLER                  PIC X(16) VALUE 'NYCKLAR-DLI'.               
011900 01  NYCKLAR-TILL-DLI.                                                    
012000   03  W-IDARTNR-X.                                                       
012100     05  W-IDARTNR           PIC S9(9)  COMP-3 VALUE ZERO.                
012200*                                                                         
012300*01  -COPY W0003                                                          
012400 ++INCLUDE W0003CCCC0                                                     
012500     EJECT                                                                
012600*--------------------------------------------------------------*          
012700*    DLI-IO-AREA / IO-AREOR                                               
012800*--------------------------------------------------------------*          
012900 01  DLI-IO-AREA.                                                         
013000   03 IO-AREA                    PIC X(600)  VALUE SPACE.                 
013100     SKIP2                                                                
013200*********** NY-ARTIKLAR-REGISTRET (NYPON)                                 
013300*  03  WLARTG01 -COPY WDD201 -PRE ARTG01- -RED IO-AREA.                   
013400 ++INCLUDE WDD201CCC0                                                     
013500     EJECT                                                                
013600*--------------------------------------------------------------*          
013700     EJECT                                                                
013800 LINKAGE SECTION.                                                         
013900* - - - - - - - - -LOGISK TERMINAL PCB-COPYTEXT FÖR BMP                   
014000*01      -COPY W0009     -PRE MSG-                                        
014100 ++INCLUDE W0009CCCC0                                                     
014200     EJECT                                                                
014300*01      -COPY W0008     -PRE ARTG-                                       
014400 ++INCLUDE W0008CCCC0                                                     
014500      05 FILLER          PIC X(2).                                        
014600     EJECT                                                                
014700 PROCEDURE DIVISION USING MSG-PCB  ARTG-PCB.                              
014800     ENTRY 'DLITCBL' USING MSG-PCB  ARTG-PCB.                             
014900     SKIP2                                                                
015000     PERFORM A-INIT                                                       
015100     PERFORM S01-LAES-INFIL                                               
015200     PERFORM UNTIL W11430-EOF  = JA OR WS-ANT-REPL > +199                 
015300        MOVE IN-IDARTNR  TO W-IDARTNR                                     
015400        PERFORM IMS-GHU-ARTG01                                            
015500        IF SEGMENT-FINNS                                                  
015600           MOVE '9'          TO ARTG01-ART-KDANSKQ                        
015700           PERFORM IMS-REPL-ARTG01                                        
015800           ADD +1               TO WS-ANT-REPL                            
015900           MOVE 'W11430D3'  TO  POSTSUM-DDNAMN2                           
016000           MOVE 'W11430'    TO  POSTSUM-FDNAMN                            
016100           MOVE 'REPL'      TO  POSTSUM-TRANSTYP                          
016200           CALL POSTSUM USING POSTSUM-PARM                                
016300        END-IF                                                            
016400        PERFORM S01-LAES-INFIL                                            
016500     END-PERFORM                                                          
016600*                                                                         
016700                                                                          
016800     IF W11430-EOF = JA                                                   
016900        CONTINUE                                                          
017000     ELSE                                                                 
017100        PERFORM S99-SKRIV-NY-INFIL                                        
017200        PERFORM ZZ-STARTA-NYTT-JOBB-VIA-SOP                               
017300     END-IF                                                               
017400                                                                          
017500     PERFORM Z-FINIT                                                      
017600     MOVE ZERO TO RETURN-CODE                                             
017700     GOBACK                                                               
017800     .                                                                    
017900     EJECT                                                                
018000 A-INIT SECTION.                                                          
018100     SKIP2                                                                
018200     OPEN INPUT  W11430-INFIL                                             
018300     OPEN OUTPUT W11430-NY-GEN                                            
018400*                                                                         
018500     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN                                
018600     MOVE 'W11430D1'   TO POSTSUM-DDNAMN2                                 
018700     MOVE 'W11430'     TO POSTSUM-FDNAMN                                  
018800     MOVE +0           TO WS-ANT-REPL                                     
018900     .                                                                    
019000     EJECT                                                                
019100 S01-LAES-INFIL SECTION.                                                  
019200     SKIP2                                                                
019300     READ W11430-INFIL INTO IN-AREA                                       
019400        AT END MOVE JA TO W11430-EOF                                      
019500     END-READ                                                             
019600*                                                                         
019700     IF W11430-EOF = NEJ                                                  
019800        MOVE 'W11430D1'  TO  POSTSUM-DDNAMN2                              
019900        MOVE 'W11430'    TO  POSTSUM-FDNAMN                               
020000        MOVE 'IN-'       TO  POSTSUM-TRANSTYP                             
020100        CALL POSTSUM USING POSTSUM-PARM                                   
020200     END-IF                                                               
020300     .                                                                    
020400     EJECT                                                                
020500 S99-SKRIV-NY-INFIL SECTION.                                              
020600     SKIP2                                                                
020700     PERFORM UNTIL W11430-EOF = JA                                        
020800        WRITE W11430-UTPOST FROM IN-AREA                                  
020900        END-WRITE                                                         
021000*                                                                         
021100        MOVE 'W11430D2'  TO  POSTSUM-DDNAMN2                              
021200        MOVE 'W11430'    TO  POSTSUM-FDNAMN                               
021300        MOVE 'UT-'       TO  POSTSUM-TRANSTYP                             
021400        CALL POSTSUM USING POSTSUM-PARM                                   
021500        PERFORM S01-LAES-INFIL                                            
021600     END-PERFORM                                                          
021700     .                                                                    
021800     EJECT                                                                
021900 Z-FINIT   SECTION.                                                       
022000     SKIP2                                                                
022100     CLOSE   W11430-INFIL                                                 
022200     CLOSE   W11430-NY-GEN                                                
022300                                                                          
022400     MOVE 'S' TO POSTSUM-OPKOD                                            
022500     CALL POSTSUM USING POSTSUM-PARM                                      
022600                                                                          
022700     .                                                                    
022800     EJECT                                                                
022900 ZZ-STARTA-NYTT-JOBB-VIA-SOP   SECTION.                                   
023000     SKIP2                                                                
023100************ KOLLA LADDNING OMSTART PÅ JOBBET ******************          
023200     MOVE SPACE          TO SOP-DDPREFIX                                  
023300     MOVE 'O'            TO SOP-SOPFUNC                                   
023400     MOVE 'W114J030'     TO SOP-PROC-NAME                                 
023500     MOVE ZERO           TO SOP-ACTPASS-DATE                              
023600                                                                          
023700     CALL W980SOP USING SOP-PARM-AREA                                     
023800                                                                          
023900     IF SOP-RETCODE > +8                                                  
024000        CALL FELLOG                                                       
024100     END-IF                                                               
024200                                                                          
024300     .                                                                    
024400     EJECT                                                                
024500* IMS SECTIONER                                                           
024600     SKIP2                                                                
024700 IMS-GHU-ARTG01 SECTION.                                                  
024800     STRING 'WLARTG01(IDARTNR  =' W-IDARTNR-X ')'                         
024900            DELIMITED BY SIZE INTO SSA1                                   
025000     MOVE '  GE' TO GODK-STATUSKODER                                      
025100     CALL CBLTDLI USING GHU ARTG-PCB IO-AREA SSA1                         
025200     MOVE ARTG-STATUS-CODE TO STATUS-WS                                   
025300     PERFORM IMS-STATUSKONTROLL                                           
025400     .                                                                    
025500     SKIP2                                                                
025600 IMS-REPL-ARTG01 SECTION.                                                 
025700     MOVE '  ' TO GODK-STATUSKODER                                        
025800     CALL CBLTDLI USING REPL ARTG-PCB IO-AREA                             
025900     MOVE ARTG-STATUS-CODE TO STATUS-WS                                   
026000     PERFORM IMS-STATUSKONTROLL                                           
026100     .                                                                    
026200     SKIP2                                                                
026300 IMS-STATUSKONTROLL SECTION.                                              
026400     SET STATUS-IX TO 1                                                   
026500     SEARCH GODK-STATUS                                                   
026600       AT END CALL FELLOG                                                 
026700       WHEN GODK-STATUS(STATUS-IX) = STATUS-WS CONTINUE                   
026800     END-SEARCH                                                           
026900     .                                                                    
