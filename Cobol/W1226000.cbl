000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.             W1226000.                                        
000400 AUTHOR.                 THOMAS LARSSON.                                  
000500     DATE-WRITTEN.       OKT 1990.                                        
000600*                                                                         
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*            BMP-PROGRAM                                                  
001100*                                                                         
001200*            PROGRAMMET LÄSER INFIL W12225                                
001300*            MOTSVARANDE ARTIKELNUMMER LÄSES PÅ WDK9/WLARTM.              
001400*            OM ARTIKEL FINNS PÅ WDK9 TAS DEN BORT                        
001500*            ANNARS LÄSES NÄSTA POST PÅ INFILEN.                          
001600*                                                                         
001700*                                                                         
001800     EJECT                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000     SKIP2                                                                
002100 INPUT-OUTPUT SECTION.                                                    
002200                                                                          
002300 FILE-CONTROL.                                                            
002400     SKIP2                                                                
002500*                            INFILER:                                     
002600                                                                          
002700     SELECT  W12225                   ASSIGN TO    W12260D1.              
002800                                                                          
002900 DATA DIVISION.                                                           
003000     SKIP2                                                                
003100 FILE SECTION.                                                            
003200     SKIP2                                                                
003300 FD  W12225                                                               
003400     LABEL RECORD STANDARD                                                
003500     RECORDING      F                                                     
003600     BLOCK CONTAINS 0.                                                    
003700     SKIP2                                                                
003800*01  POST -COPY W12225   -PRE W12225-                                     
004000 WORKING-STORAGE SECTION.                                                 
004100     SKIP2                                                                
004101                                                                          
004110*    -- CHECKED BY WY2000                                                 
004200 77  PROGRAM-NAMN                PIC X(8) VALUE 'W1226000'.               
004300     SKIP2                                                                
004400*    ---- GENERELLA KONSTANTER                                            
004500                                                                          
004600 77  JA                          PIC X       VALUE 'J'.                   
004700 77  NEJ                         PIC X       VALUE 'N'.                   
004800 77  W12225-EOF                  PIC X       VALUE 'N'.                   
004800 77  W-DLET-WDK901               PIC 9(7)    VALUE ZERO.                  
004900     SKIP2                                                                
005000*    ---- ARBETSFÄLT                                                      
005100 77  CHKP-ID                     PIC X(8)    VALUE 'W1226000'.            
005200 77  MSG-IO-AREA-LENGTH          PIC S9(9)   VALUE +32  COMP SYNC.        
005300 77  MSG-IO-AREA                 PIC X(32)   VALUE SPACE.                 
005400 77  CHKP-AREA-1-LENGTH          PIC S9(9)   VALUE +32  COMP SYNC.        
005500 77  CHKP-AREA-1                 PIC X(32)   VALUE SPACE.                 
005600 77  INDX-WDK9                   PIC 9(4)    VALUE ZERO.                  
005700     SKIP2                                                                
005800     EJECT                                                                
005900*    ---- SUBPROGRAM OCH PARAMETERAREOR                                   
006000     SKIP2                                                                
006100 01  DYNAMISKA-SUBPROGRAM.                                                
006200   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM '.            
006300   03  DATKORT                   PIC X(8)    VALUE 'DATKORT'.             
006400   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
006500   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
006600     SKIP2                                                                
006700                                                                          
006800*    ---- PARAMETRAR TILL POSTSUM                                         
006900                                                                          
007000*01  -COPY W0005       -PRE POSTSUM-.                                     
007200     EJECT                                                                
007300* ------------------- ARBETSAREA FÖR INFIL                                
007400 01  FILLER                      PIC X(24) VALUE 'INFIL'.                 
007500                                                                          
007600*01  POST -COPY W12225 -PRE W-IN-                                         
007800                                                                          
007900*    ---- ARBETS-AREOR FÖR IMS-SEKTIONERNA                                
008000*                                                                         
008100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008200     SKIP2                                                                
008300*    ---- STATUSKOD FRÅN IMS                                              
008400                                                                          
008500 01  STATUS-WS                   PIC XX.                                  
008600     88  SEGMENT-FINNS                       VALUE '  '.                  
008700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
008800     88  IMS-EJ-OK                           VALUE 'XD'.                  
008900     SKIP2                                                                
009000 01  GODK-STATUSKODER.                                                    
009100   03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                  
009200     SKIP2                                                                
009300 01  SSA1                        PIC X(64).                               
009400 01  SSA2                        PIC X(64).                               
009500 01  SSA3                        PIC X(64).                               
009600     SKIP2                                                                
009700*    ---- NYCKLAR OCH SÖKFÄLT TILL DLI                                    
009800                                                                          
009900 01  NYCKLAR-TILL-DLI.                                                    
010000   03  W-IDARTNR-X.                                                       
010100     05  W-IDARTNR               PIC S9(9)                COMP-3.         
010200                                                                          
010300*01  -COPY W0003                                                          
010500     EJECT                                                                
010600 01  DLI-IO-AREA.                                                         
010700   03 IO-AREA                    PIC X(100)  VALUE SPACE.                 
010800     SKIP2                                                                
010900*  03  AREA     -COPY WDK901 -PRE ARTM01- -RED IO-AREA.                   
011100     EJECT                                                                
011200 LINKAGE SECTION.                                                         
011300*01      -COPY W0009     -PRE MSG-                                        
011500     EJECT                                                                
011600*01      -COPY W0008     -PRE ARTM-                                       
011800      05 FILLER          PIC X.                                           
011900     EJECT                                                                
012000                                                                          
012100 PROCEDURE DIVISION USING MSG-PCB ARTM-PCB.                               
012200     ENTRY 'DLITCBL' USING MSG-PCB ARTM-PCB.                              
012300     SKIP2                                                                
012400     PERFORM A-INIT                                                       
012500     MOVE  1             TO INDX-WDK9                                     
012600     MOVE NEJ            TO W12225-EOF                                    
012700     PERFORM B-LAES-INFIL                                                 
012800     IF W12225-EOF = JA                                                   
012900       DISPLAY 'ARTIKLAR SAKNAS PÅ INFILEN'                               
013000     END-IF                                                               
013100     PERFORM UNTIL W12225-EOF = JA                                        
013200       MOVE 'W-INPOST' TO POSTSUM-DDNAMN2                                 
013300       MOVE 'FIL   '   TO POSTSUM-FDNAMN                                  
013400       CALL POSTSUM USING POSTSUM-PARM                                    
013500                                                                          
013600       MOVE W-IN-IDARTNR TO W-IDARTNR                                     
013700       PERFORM IMS-GET-ARTM01                                             
013800       IF SEGMENT-FINNS                                                   
013900         PERFORM IMS-DLET-ARTM01                                          
               ADD 1 TO W-DLET-WDK901                                           
014000         ADD  1          TO INDX-WDK9                                     
014100         MOVE 'WDK9    ' TO POSTSUM-DDNAMN2                               
014200         MOVE 'DLET  '   TO POSTSUM-FDNAMN                                
014300         CALL POSTSUM USING POSTSUM-PARM                                  
014400       END-IF                                                             
014500       IF INDX-WDK9 = 100                                                 
014600         PERFORM IMS-CHECKPOINT                                           
014700         MOVE ZERO       TO INDX-WDK9                                     
014800         MOVE  1         TO INDX-WDK9                                     
014900       END-IF                                                             
015000       PERFORM B-LAES-INFIL                                               
015100     END-PERFORM                                                          
015200     PERFORM Z-FINIT                                                      
015300     MOVE ZERO TO RETURN-CODE                                             
015400     GOBACK                                                               
015500     .                                                                    
015600     EJECT                                                                
015700 A-INIT SECTION.                                                          
015800     SKIP2                                                                
015900     PERFORM IMS-RESTART                                                  
016000     OPEN INPUT W12225                                                    
016100     MOVE 'W12260' TO POSTSUM-PROGNAMN                                    
016200     .                                                                    
016300     EJECT                                                                
016400 B-LAES-INFIL SECTION.                                                    
016500     SKIP2                                                                
016600     READ W12225 INTO W-IN-POST                                           
016700         AT END MOVE JA TO W12225-EOF                                     
016800     .                                                                    
016900     EJECT                                                                
017000 Z-FINIT   SECTION.                                                       
017100     SKIP2                                                                
           DISPLAY 'ANTAL BORTTAGNA WDK901: ' W-DLET-WDK901                     
017200     CLOSE W12225                                                         
017300     MOVE 'S' TO POSTSUM-OPKOD                                            
017400     CALL POSTSUM USING POSTSUM-PARM                                      
017500     .                                                                    
017600     EJECT                                                                
017700                                                                          
017800* IMS SECTIONER                                                           
017900     SKIP2                                                                
018000 IMS-RESTART SECTION.                                                     
018100     SKIP2                                                                
018200     MOVE SPACE TO MSG-IO-AREA                                            
018300     MOVE '  ' TO GODK-STATUSKODER                                        
018400     CALL CBLTDLI USING XRST MSG-PCB                                      
018500                        MSG-IO-AREA-LENGTH MSG-IO-AREA                    
018600                        CHKP-AREA-1-LENGTH CHKP-AREA-1                    
018700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
018800     PERFORM IMS-STATUSKONTROLL                                           
018900     .                                                                    
019000     SKIP2                                                                
019100 IMS-CHECKPOINT SECTION.                                                  
019200     SKIP2                                                                
019300     MOVE CHKP-ID TO MSG-IO-AREA                                          
019400     MOVE '  XD' TO GODK-STATUSKODER                                      
019500     CALL CBLTDLI USING CHKP MSG-PCB                                      
019600                        MSG-IO-AREA-LENGTH MSG-IO-AREA                    
019700                        CHKP-AREA-1-LENGTH CHKP-AREA-1                    
019800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
019900     PERFORM IMS-STATUSKONTROLL                                           
020000     IF IMS-EJ-OK                                                         
020100       DISPLAY 'IMS-KONTROLLREGION EJ TILLGÄNGLIG'                        
020200       CALL FELLOG                                                        
020300     END-IF                                                               
020400     .                                                                    
020500     EJECT                                                                
020600 IMS-GET-ARTM01 SECTION.                                                  
020700     SKIP2                                                                
020800     STRING 'WLARTM01(IDARTNR  =' W-IDARTNR-X ')'                         
020900            DELIMITED BY SIZE INTO SSA1                                   
021000     MOVE '  GE' TO GODK-STATUSKODER                                      
021100     CALL CBLTDLI USING GHU ARTM-PCB DLI-IO-AREA SSA1                     
021200     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
021300     PERFORM IMS-STATUSKONTROLL                                           
021400     .                                                                    
021500     SKIP2                                                                
021600 IMS-DLET-ARTM01 SECTION.                                                 
021700     SKIP2                                                                
021800     MOVE '  ' TO GODK-STATUSKODER                                        
021900     CALL CBLTDLI USING DLET ARTM-PCB DLI-IO-AREA                         
022000     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
022100     PERFORM IMS-STATUSKONTROLL                                           
022200     .                                                                    
022300     EJECT                                                                
022400 IMS-STATUSKONTROLL SECTION.                                              
022500     SET STATUS-IX TO 1                                                   
022600     SEARCH GODK-STATUS                                                   
022700       AT END CALL FELLOG                                                 
022800       WHEN GODK-STATUS(STATUS-IX) = STATUS-WS CONTINUE                   
022900     END-SEARCH                                                           
023000     .                                                                    
