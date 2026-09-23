000300 ID DIVISION.                                                             
000400 PROGRAM-ID.         W2225010.                                            
000500*              PROGRAM CONVERTED BY                                       
000600*              COBOL CONVERSION AID PO 5785-ABJ                           
000700*              CONVERSION DATE 05/25/91 10:48:58.                         
000800*AUTHOR.             JANNE MELANDER.                                      
000900*DATE-WRITTEN.       OKTOBER 1986.                                        
001000*REMARKS.                                                                 
001100*                                                                         
001300*                    PROGRAMMET ÄR ETT SUBPROGRAM TILL W22250             
001400*                    SOM SKÖTER OM SAMTLIGA IMS-CALL ÅT DETSAMMA          
001500*                    MOT  ARTC(WDD6).                                     
001600     SKIP3                                                                
001700 ENVIRONMENT DIVISION.                                                    
001800     SKIP3                                                                
001900 DATA DIVISION.                                                           
002000     EJECT                                                                
002100 WORKING-STORAGE SECTION.                                                 
002110                                                                          
002200*    -- CHECKED BY WY2000                                                 
002600*                                                                         
002700 77  JA                          PIC X       VALUE 'J'.                   
002800 77  NEJ                         PIC X       VALUE 'N'.                   
002900 77  TEST-SVAR                   PIC XXXX.                                
003000     SKIP3                                                                
003100*                                *** GENERELLA SUBRUTINER                 
003200 01  SUBPROGRAM.                                                          
003300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
003400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
003500     SKIP3                                                                
003600**********************************************************                
003700*    ARBETSAREOR TILL IMS-SEKTIONERNA.                                    
003800*                                                                         
003900 01  IMS-WS.                                                              
004000     03  FILLER                  PIC X(8)    VALUE 'IMS-WS  '.            
004100     SKIP3                                                                
004200     03  STATUS-WS               PIC XX.                                  
004300         88  SEGMENT-FINNS                   VALUE '  '.                  
004400         88  SEGMENT-SAKNAS                  VALUE 'GE'.                  
004500     03  W-IDARTNR-X.                                                     
004600         05  W-IDARTNR           PIC S9(9)   COMP-3.                      
004900     SKIP3                                                                
005000     03  GODK-STATUSKODER.                                                
005100         05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX  PIC XX.           
005200     SKIP3                                                                
005300     03  SSA1                    PIC X(50).                               
005400     03  SSA2                    PIC X(50).                               
005500     03  SSA3                    PIC X(50).                               
005600     03  SSA4                    PIC X(50).                               
005700     SKIP3                                                                
005800*01  -COPY W0003                                                          
006000     EJECT                                                                
006100 01  FILLER.                                                              
006110 03  DLI-IO-AREA                 PIC X(900).                              
006200     SKIP1                                                                
006300*03  FILLER  -COPY WDK601                -RED DLI-IO-AREA.                
006500     EJECT                                                                
006600*03  FILLER  -COPY WDK611                -RED DLI-IO-AREA.                
006800     EJECT                                                                
008100 LINKAGE SECTION.                                                         
008200*01  0-AREA  -COPY W222L500     -PRE LINK                                 
008400     EJECT                                                                
008500 01  LINK-DATA-AREA.                                                      
008600     03  LINK-AREA            PIC X(200).                                 
008700*  03 1-AREA    -COPY W222L501    -PRE LINK -RED LINK-AREA                
008900     EJECT                                                                
009000*  03 2-AREA    -COPY W222L502    -PRE LINK -RED LINK-AREA                
009200     EJECT                                                                
009700*01  -COPY W0008 -PRE ARTC-                                               
009900     05  FILLER                  PIC X(32).                               
010000     EJECT                                                                
010100 PROCEDURE DIVISION USING LINK0-AREA LINK-DATA-AREA                       
010200     ARTC-PCB.                                                            
010300     SKIP3                                                                
010400                                                                          
010500     EVALUATE LINK0-KDCALL                                                
010600     WHEN LINK0-LAES-ROT-WDD601                                           
010700       PERFORM A-LAES-ROT-WDD601                                          
010800     WHEN LINK0-LAES-WDD631                                               
010900       PERFORM B-LAES-WDD631                                              
011000     WHEN LINK0-LAES-WDD660                                               
011100       PERFORM C-LAES-WDD660                                              
011400     WHEN OTHER                                                           
011500       MOVE LINK0-KDSVAR-FEL TO LINK0-KDSVAR                              
011600     END-EVALUATE                                                         
011700     MOVE ZERO TO RETURN-CODE                                             
011800     GOBACK                                                               
011900     .                                                                    
012000     EJECT                                                                
012100 A-LAES-ROT-WDD601  SECTION.                                              
012200     SKIP1                                                                
012300     MOVE LINK0-IDARTNR        TO W-IDARTNR                               
012400     PERFORM IMS-LAES-ROT-WLARTC01                                        
012500     IF SEGMENT-FINNS                                                     
012600       MOVE LINK0-KDSVAR-OK   TO LINK0-KDSVAR                             
012700     ELSE                                                                 
012800       MOVE LINK0-KDSVAR-FEL  TO LINK0-KDSVAR                             
012900     END-IF                                                               
013000     .                                                                    
013100     EJECT                                                                
013200 B-LAES-WDD631 SECTION.                                                   
013300     SKIP1                                                                
013400***  LÄSER PB-SEP FÖR IDARTNR-EMBQ                                        
013500     PERFORM IMS-LAES-WLARTC11                                            
013600     IF SEGMENT-FINNS                                                     
013700       MOVE LINK0-KDSVAR-OK   TO LINK0-KDSVAR                             
013800       MOVE CLAG-KVPB-SEP     TO LINK2-KVPB-SEP                           
013900     ELSE                                                                 
014000       MOVE LINK0-KDSVAR-FEL  TO LINK0-KDSVAR                             
014100     END-IF                                                               
014200     .                                                                    
014300     EJECT                                                                
014400 C-LAES-WDD660 SECTION.                                                   
015600     SKIP1                                                                
015800     PERFORM IMS-LAES-WLARTC11                                            
015900     IF SEGMENT-FINNS                                                     
016000       MOVE LINK0-KDSVAR-OK    TO LINK0-KDSVAR                            
016100       MOVE CLAG-IDARTNR-EMBQ0 TO LINK1-IDARTNR-EMBQ0                     
016200       MOVE CLAG-IDARTNR-EMBQ1 TO LINK1-IDARTNR-EMBQ1                     
016300       MOVE CLAG-IDARTNR-EMBQ2 TO LINK1-IDARTNR-EMBQ2                     
016400       MOVE CLAG-BEFT          TO LINK1-BEFT                              
016500     ELSE                                                                 
016600       MOVE ZERO              TO LINK1-IDARTNR-EMBQ0                      
016700       MOVE ZERO              TO LINK1-IDARTNR-EMBQ1                      
016800       MOVE ZERO              TO LINK1-IDARTNR-EMBQ2                      
016900       MOVE ZERO              TO LINK1-BEFT                               
017000       MOVE LINK0-KDSVAR-FEL  TO LINK0-KDSVAR                             
017100     END-IF                                                               
017200     .                                                                    
017300     EJECT                                                                
017400 IMS-LAES-ROT-WLARTC01 SECTION.                                           
017500     SKIP1                                                                
017600     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
017700     DELIMITED BY SIZE INTO SSA1                                          
017800     MOVE '  GE' TO GODK-STATUSKODER                                      
017900     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA SSA1                      
018000     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
018100     PERFORM IMS-STATUSKONTROLL                                           
018200     .                                                                    
018300     EJECT                                                                
018400 IMS-LAES-WLARTC11 SECTION.                                               
018500     SKIP1                                                                
018600     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
018700     DELIMITED BY SIZE INTO SSA1                                          
018800     MOVE 'WLARTC11 ' TO SSA2                                             
019100     MOVE '  GE' TO GODK-STATUSKODER                                      
019200     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA SSA1 SSA2                
019300     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
019400     PERFORM IMS-STATUSKONTROLL                                           
019500     .                                                                    
019600     EJECT                                                                
022000 IMS-STATUSKONTROLL SECTION.                                              
022100     SET STATUS-IX TO 1                                                   
022200     SEARCH GODK-STATUS AT END CALL FELLOG                                
022300     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                             
022400     CONTINUE                                                             
022500     END-SEARCH                                                           
022700     .                                                                    
