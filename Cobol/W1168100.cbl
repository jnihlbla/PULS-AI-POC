000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W1168100.                                                
000300 AUTHOR.         STINA MOGREN.                                            
000400 DATE-WRITTEN.   03/10/15.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*      - KOPIERAR PRISFIL INNEHÅLLANDE NYA SJÄLVKOSTPRISER TILL           
000900*        MÅNADSKURS MED 'USA-PRISFIL', OCH KOMPLETTERAD                   
001000*        MED DE NYA SJÄLVKOSTNADSPRISERNA,                                
001100*        PRISERNA RÄKNAS OM TILL MARKNADSBOLAGSVALUTA                     
001200*                                                                         
001300*    ABENDKODER:                                                          
001400*        U0016 -  . . . .                                                 
001500                                                                          
001600     SKIP3                                                                
001700 ENVIRONMENT DIVISION.                                                    
001800     SKIP2                                                                
001900 INPUT-OUTPUT SECTION.                                                    
002000                                                                          
002100 FILE-CONTROL.                                                            
002200     SKIP2                                                                
002500*       --- INFIL FRÅN W11624                                             
002600     SELECT W11624                     ASSIGN TO W11681D1.                
002700     SKIP2                                                                
003100*       --- SAMMA SOM IN, MED ÄNDRAD SJÄLVKOST                            
003200     SELECT W11681                     ASSIGN TO W11681D2.                
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500     SKIP2                                                                
003600 FILE SECTION.                                                            
003700     SKIP3                                                                
004500 FD  W11624                                                               
004600     RECORDING       F                                                    
004700     BLOCK CONTAINS  0.                                                   
004800                                                                          
004900*01  -COPY W11620A       -L.                                              
005000     SKIP3                                                                
005700 FD  W11681                                                               
005800     RECORDING       V                                                    
005900     BLOCK CONTAINS  0.                                                   
006000                                                                          
006100*01  POST -COPY W11620   -PRE  UT-  -L.                                   
006200     EJECT                                                                
006300 WORKING-STORAGE SECTION.                                                 
006400                                                                          
006500 77  IDPGM                       PIC X(8)      VALUE 'W1168100'.          
006600 77  JA                          PIC X         VALUE 'J'.                 
006700 77  NEJ                         PIC X         VALUE 'N'.                 
006800                                                                          
007000 77  WS-IDLEVNR                  PIC 9(5)      VALUE ZERO.                
007100 77  WS-IDLEVNR-LOC              PIC 9(5)      VALUE ZERO.                
007200                                                                          
007300 01  WS-PRARTSJK                 PIC 9(7)V9(2) VALUE ZERO.                
007400                                                                          
007700 77  WS-IDMARKBO                 PIC X      VALUE SPACE.                  
008100                                                                          
008110*01    -COPY WWDCLAND                                                     
008120                                                                          
008200 77  W11624-EOF-SW               PIC X         VALUE 'N'.                 
008300     88  END-OF-W11624                         VALUE 'J'.                 
008400                                                                          
008800 01  DAGENS-DATUM                PIC 9(8)      VALUE ZERO.                
008900     EJECT                                                                
009000 01  WS-AAAAMMDD.                                                         
009100     03  WS-SEKEL                       PIC 9(2).                         
009200     03  WS-AAMMDD                      PIC 9(6).                         
009300 01  WS-TIFINLV REDEFINES WS-AAAAMMDD   PIC 9(8).                         
009400*                                                                         
009440     EJECT                                                                
009491 01  WS-MQ-LINE1.                                                         
009492     03  FILLER                  PIC X(337)  VALUE                        
009493                                 '¤MQMPROP Vidb_Source=Full'.             
009494 01  WS-MQ-LINE2.                                                         
009495     03  FILLER                  PIC X(337)  VALUE                        
009496                                 '¤MQMPROP LoadType=Full'.                
009497 01  WS-MQ-LINE3.                                                         
009498     03  FILLER                  PIC X(16)   VALUE                        
009499                                 '¤MQMPROP Market='.                      
009500     03  WS-MQ-IDLANDX2          PIC X(2)    VALUE SPACE.                 
009501     03  FILLER                  PIC X(319)  VALUE SPACE.                 
009502     EJECT                                                                
009503                                                                          
009510 01  DYNAMISKA-SUBPROGRAM.                                                
009600*                                                                         
009700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009900     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
010000     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
010200     SKIP2                                                                
010300                                                                          
011900 01  FELTEXT.                                                             
012000     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
012100     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
012200     EJECT                                                                
012300*    --- PARAMETRAR TILL SUBPROGRAM W335CURR                              
012400*01 -COPY W335CURR                                                        
012500     EJECT                                                                
012600*    --- PARAMETRAR TILL POSTSUM                                          
012700*                                                                         
012800*01  -COPY W0005   -PRE  POSTSUM-                                         
012900     EJECT                                                                
013000 01  FILLER                      PIC X(16) VALUE 'WDATAREA'.              
013100*01 -COPY WDATAREA                                                        
013200     EJECT                                                                
013300 01  FILLER                      PIC X(24)   VALUE 'IN-AREA'.             
013400                                                                          
013500*01  AREA -COPY W11620A      -PRE IN-                                     
013600     EJECT                                                                
014100 01  FILLER                      PIC X(24)   VALUE 'UT-AREA'.             
014200                                                                          
014300*01  AREA -COPY W11620       -PRE UT-                                     
014400     EJECT                                                                
014500*    --- ARBETS-AREOR TILL  IMS-SEKTIONERNA                               
014600*                                                                         
014700 01  NYCKLAR-TILL-DLI.                                                    
016300                                                                          
016400     03  W-WDB101KY-X.                                                    
016500         05  W-IDPARTNR            PIC X(9)  VALUE SPACE.                 
016600         05  W-IDFTG               PIC 9(2)  VALUE ZERO.                  
016700                                                                          
016800     03   W-WDB1B-LOW-X.                                                  
016900         05  W-B-IDLANDX2-LOW      PIC X(2)  VALUE SPACE.                 
017000                                                                          
017100     03  W-WDB1B-HIGH-X.                                                  
017200         05  W-B-IDLANDX2-HIGH     PIC X(2)  VALUE HIGH-VALUE.            
017300                                                                          
017400     03  W-WDB1B1KY-LOW.                                                  
017500         05  W-IDLANDX2-LOW        PIC X(2)    VALUE SPACE.               
017600         05  W-IDMARKBO-LOW        PIC X(1)    VALUE 'A'.                 
017700         05  W-IDPARTNR-LOW        PIC X(9)    VALUE LOW-VALUE.           
017800         05  W-IDFTG-LOW           PIC 9(2)    VALUE ZERO.                
017900                                                                          
018000     03  W-WDB1B1KY-HIGH.                                                 
018100         05  W-IDLANDX2-HIGH       PIC X(2)    VALUE SPACE.               
018200         05  W-IDMARKBO-HIGH       PIC X(1)    VALUE 'G'.                 
018300         05  W-IDPARTNR-HIGH       PIC X(9)    VALUE HIGH-VALUE.          
018400         05  W-IDFTG-HIGH          PIC 9(2)    VALUE 99.                  
018500     03  W-IDLAND-X.                                                      
018600         05  W-IDLAND              PIC X(2)    VALUE SPACE.               
018700                                                                          
018800*    --- STATUS-KOD FRÅN IMS                                              
018900 01  STATUS-WS                   PIC XX.                                  
019000     88  SEGMENT-FINNS                       VALUE '  '.                  
019100     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
019200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
019300     88  BASEN-SLUT                          VALUE 'GB'.                  
019400     SKIP2                                                                
019500 01  GODK-STATUSKODER.                                                    
019600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
019700     SKIP3                                                                
019800 01  SSA1                        PIC X(64).                               
019900 01  SSA2                        PIC X(64).                               
020000     EJECT                                                                
020100*    --- IMS FUNKTIONSKODER                                               
020200*01  -COPY W0003                                                          
020300     EJECT                                                                
021100 01  FILLER                  PIC X(16)   VALUE 'WDB1B1-POST'.             
021200 01  DLI-IO-WDB1B1.                                                       
021300*    03  -COPY WDB1B1                                                     
021400      EJECT                                                               
021500 01  DLI-IO-WDB101.                                                       
021600*    03  -COPY WDB101                                                     
021700      EJECT                                                               
021800 LINKAGE SECTION.                                                         
022500*01  -COPY W0008      -PRE WDB1-                                          
022600     05  FILLER                  PIC X.                                   
022700     EJECT                                                                
022800*01  -COPY W0008      -PRE  WDB1B-                                        
022900     05  FILLER                  PIC X(16).                               
023000     EJECT                                                                
023100 PROCEDURE DIVISION   USING  WDB1-PCB WDB1B-PCB.                          
023300 MAIN SECTION.                                                            
023400     ENTRY 'DLITCBL'  USING  WDB1-PCB WDB1B-PCB.                          
023600                                                                          
023700     PERFORM A-INIT                                                       
023800                                                                          
023900     PERFORM S01-LAES-W11624                                              
023910     PERFORM S07-HAMTA-IDMARKBO                                           
024000     PERFORM UNTIL END-OF-W11624                                          
024100         MOVE IN-PRARTSJK    TO WS-PRARTSJK                               
024200         PERFORM B-FLYTTA-SKRIV-UTPOST                                    
024300         PERFORM S01-LAES-W11624                                          
024400     END-PERFORM                                                          
024500                                                                          
024600     PERFORM Z-FINIT                                                      
024700                                                                          
024800     MOVE ZERO TO RETURN-CODE                                             
024900     GOBACK                                                               
025000     .                                                                    
025100     EJECT                                                                
025200 A-INIT SECTION.                                                          
025300                                                                          
025400     OPEN INPUT  W11624                                                   
025700          OUTPUT W11681                                                   
025800                                                                          
025900     MOVE FUNCTION CURRENT-DATE(1:8) TO DAGENS-DATUM                      
026000                                                                          
026700     .                                                                    
026800     EJECT                                                                
026900 B-FLYTTA-SKRIV-UTPOST SECTION.                                           
027000                                                                          
027010     IF IN-IDLANDX2 NOT = WS-MQ-IDLANDX2                                  
027018       WRITE UT-POST           FROM WS-MQ-LINE1                           
027019       MOVE SPACE                TO UT-POST                               
027020       WRITE UT-POST           FROM WS-MQ-LINE2                           
027030       MOVE SPACE                TO UT-POST                               
027040       MOVE IN-IDLANDX2          TO WS-MQ-IDLANDX2                        
027050       WRITE UT-POST           FROM WS-MQ-LINE3                           
027060       MOVE SPACE                TO UT-POST                               
034030     END-IF                                                               
034031                                                                          
034033     MOVE IN-IDPTYP              TO UT-IDPTYP                             
034034     MOVE IN-IDARTNR20           TO UT-IDARTNR20                          
034035     MOVE IN-RAD                 TO UT-RAD                                
034036     MOVE IN-DADATUM             TO UT-DADATUM                            
034037     MOVE IN-IDLEVNR-DUBLETT     TO UT-IDLEVNR-DUBLETT                    
034038     MOVE IN-IDLEVNR-LOC-DUBLETT TO UT-IDLEVNR-LOC-DUBLETT                
034039     MOVE IN-KDTIPPR             TO UT-KDTIPPR                            
034040     MOVE IN-IDKAT(01)           TO UT-IDKAT(01)                          
034041     MOVE IN-IDKAT(02)           TO UT-IDKAT(02)                          
034042     MOVE IN-IDKAT(03)           TO UT-IDKAT(03)                          
034043     MOVE IN-BELEVART            TO UT-BELEVART                           
034044     MOVE IN-FLGEMFMC            TO UT-FLGEMFMC                           
034045     MOVE IN-IDPROJUP            TO UT-IDPROJUP                           
034046     MOVE IN-BEARTEXT            TO UT-BEARTEXT                           
034047     MOVE IN-TIURPROD           TO UT-TIURPROD                            
034059                                                                          
034060     WRITE UT-POST FROM UT-AREA                                           
034061                                                                          
034062     MOVE 'W11681'   TO POSTSUM-FDNAMN                                    
034063     MOVE 'W11681D2' TO POSTSUM-DDNAMN2                                   
034064     CALL POSTSUM USING POSTSUM-PARM                                      
034065     .                                                                    
034066 Z-FINIT SECTION.                                                         
034067                                                                          
034068     DISPLAY 'MARKN.BOLAG ' WS-IDMARKBO                                   
034069                                                                          
034070     CLOSE W11624                                                         
034071           W11681                                                         
034073                                                                          
034074     MOVE 'S'        TO POSTSUM-OPKOD                                     
034075     CALL POSTSUM USING POSTSUM-PARM                                      
034076     .                                                                    
034077     EJECT                                                                
034078 S01-LAES-W11624  SECTION.                                                
034079                                                                          
034080     READ W11624 INTO IN-AREA                                             
034081     AT END                                                               
034082        SET END-OF-W11624 TO TRUE                                         
034083     NOT AT END                                                           
034084        MOVE 'W11624'   TO POSTSUM-FDNAMN                                 
034085        MOVE 'W11681D2' TO POSTSUM-DDNAMN2                                
034086        CALL POSTSUM USING POSTSUM-PARM                                   
034087     END-READ                                                             
034088     .                                                                    
034089     EJECT                                                                
036500 S07-HAMTA-IDMARKBO   SECTION.                                            
036501                                                                          
036600     IF WS-IDMARKBO = SPACE                                               
036700       MOVE IN-IDLANDX2           TO W-B-IDLANDX2-LOW                     
036800                                     W-B-IDLANDX2-HIGH                    
036900                                     W-IDLANDX2-LOW                       
037000                                     W-IDLANDX2-HIGH                      
037100       PERFORM IMS-GU-WDB1B1                                              
037200       IF SEGMENT-FINNS                                                   
037300         MOVE SEQB-IDPARTNR           TO W-IDPARTNR                       
037400         MOVE SEQB-IDFTG              TO W-IDFTG                          
037500         PERFORM IMS-GU-WDB101                                            
037600         IF SEGMENT-FINNS                                                 
037700           MOVE BET-IDMARKBO          TO WS-IDMARKBO                      
037800         END-IF                                                           
037900       END-IF                                                             
038000       IF W-IDLAND = 'US'                                                 
038100*           USA KAN FÅ FEL MARKNADSBOLAG                                  
038200         MOVE 'E'                     TO WS-IDMARKBO                      
038300       END-IF                                                             
038400     END-IF                                                               
038401                                                                          
038500     .                                                                    
038600     EJECT                                                                
039892                                                                          
042500 IMS-GU-WDB101 SECTION.                                                   
042600     STRING 'WDB101  (WDB101KY =' W-WDB101KY-X ')'                        
042700          DELIMITED BY SIZE INTO SSA1                                     
042800     MOVE '  GE' TO GODK-STATUSKODER                                      
042900     CALL CBLTDLI USING GU WDB1-PCB DLI-IO-WDB101 SSA1                    
043000     MOVE WDB1-STATUS-CODE TO STATUS-WS                                   
043100     PERFORM IMS-STATUSKONTROLL                                           
043200     .                                                                    
043300     SKIP3                                                                
043400 IMS-GU-WDB1B1 SECTION.                                                   
043500     STRING 'WDB1B1  (WDB1B1KY=>' W-WDB1B1KY-LOW                          
043600                    '&WDB1B1KY=<' W-WDB1B1KY-HIGH ')'                     
043700          DELIMITED BY SIZE INTO SSA1                                     
043800     MOVE '    ' TO GODK-STATUSKODER                                      
043900     CALL CBLTDLI USING GU WDB1B-PCB DLI-IO-WDB1B1 SSA1                   
044000     MOVE WDB1B-STATUS-CODE TO STATUS-WS                                  
044100     PERFORM IMS-STATUSKONTROLL                                           
044200     .                                                                    
044300     SKIP3                                                                
044400 IMS-STATUSKONTROLL SECTION.                                              
044500                                                                          
044600     SET STATUS-IX TO 1                                                   
044700     SEARCH GODK-STATUS                                                   
044800       AT END CALL FELLOG                                                 
044900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
045000     END-SEARCH                                                           
045100     .                                                                    
045200     EJECT                                                                
