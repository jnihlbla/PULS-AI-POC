000101 ID DIVISION.                                                             
000201 PROGRAM-ID.     W1168200.                                                
000301 AUTHOR.         STINA MOGREN.                                            
000401 DATE-WRITTEN.   03/10/15.                                                
000501 DATE-COMPILED.                                                           
000601                                                                          
000701*    FUNKTION:                                                            
000801*      - KOPIERAR PRISFIL INNEHÅLLANDE NYA SJÄLVKOSTPRISER TILL           
000901*        MÅNADSKURS MED 'USA-PRISFIL', OCH KOMPLETTERAD                   
001001*        MED DE NYA SJÄLVKOSTNADSPRISERNA,                                
001101*        PRISERNA RÄKNAS OM TILL MARKNADSBOLAGSVALUTA                     
001201*                                                                         
001301*    ABENDKODER:                                                          
001401*        U0016 -  . . . .                                                 
001501                                                                          
001601     SKIP3                                                                
001701 ENVIRONMENT DIVISION.                                                    
001801     SKIP2                                                                
001901 INPUT-OUTPUT SECTION.                                                    
002001                                                                          
002101 FILE-CONTROL.                                                            
002201     SKIP2                                                                
002301*       --- INFIL FRÅN W11624                                             
002401     SELECT W11624                     ASSIGN TO W11682D1.                
002501     SKIP2                                                                
002901*       --- SAMMA SOM IN, MED ÄNDRAD SJÄLVKOST                            
003001     SELECT W11682                     ASSIGN TO W11682D2.                
003101     EJECT                                                                
003201 DATA DIVISION.                                                           
003301     SKIP2                                                                
003401 FILE SECTION.                                                            
003501     SKIP3                                                                
003601 FD  W11624                                                               
003701     RECORDING       F                                                    
003801     BLOCK CONTAINS  0.                                                   
003901                                                                          
004001*01  -COPY W11620B       -L.                                              
005001     SKIP3                                                                
005701 FD  W11682                                                               
005801     RECORDING       V                                                    
005901     BLOCK CONTAINS  0.                                                   
006001                                                                          
006101*01  POST -COPY W11620   -PRE  UT-  -L.                                   
006201     EJECT                                                                
006301 WORKING-STORAGE SECTION.                                                 
006401                                                                          
006501 77  IDPGM                       PIC X(8)      VALUE 'W1168200'.          
006601 77  JA                          PIC X         VALUE 'J'.                 
006701 77  NEJ                         PIC X         VALUE 'N'.                 
006801                                                                          
006901 77  WS-IDLEVNR                  PIC 9(5)      VALUE ZERO.                
007001 77  WS-IDLEVNR-LOC              PIC 9(5)      VALUE ZERO.                
007101                                                                          
007201 01  WS-PRARTSJK                 PIC 9(7)V9(2) VALUE ZERO.                
007301                                                                          
007401 77  WS-IDMARKBO                 PIC X      VALUE SPACE.                  
007701                                                                          
007801 77  W11624-EOF-SW               PIC X         VALUE 'N'.                 
007901     88  END-OF-W11624                         VALUE 'J'.                 
008001                                                                          
008401 01  DAGENS-DATUM                PIC 9(8)      VALUE ZERO.                
008501     EJECT                                                                
008601 01  WS-AAAAMMDD.                                                         
008701     03  WS-SEKEL                       PIC 9(2).                         
008801     03  WS-AAMMDD                      PIC 9(6).                         
008901 01  WS-TIFINLV REDEFINES WS-AAAAMMDD   PIC 9(8).                         
009001*                                                                         
009101     EJECT                                                                
009201 01  WS-MQ-LINE1.                                                         
009301     03  FILLER                  PIC X(337)  VALUE                        
009401                                 '¤MQMPROP Vidb_Source=Delta'.            
009501 01  WS-MQ-LINE2.                                                         
009601     03  FILLER                  PIC X(337)  VALUE                        
009701                                 '¤MQMPROP LoadType=Delta'.               
010000 01  WS-MQ-LINE3.                                                         
010001     03  FILLER                  PIC X(16)   VALUE                        
010002                                 '¤MQMPROP Market='.                      
010003     03  WS-MQ-IDLANDX2          PIC X(2)    VALUE SPACE.                 
010005     03  FILLER                  PIC X(319)  VALUE SPACE.                 
010006                                                                          
010101     EJECT                                                                
010201 01  DYNAMISKA-SUBPROGRAM.                                                
010300*                                                                         
010400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010600     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
010700     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
010900     SKIP2                                                                
011000                                                                          
011100* INFO OM KÖRTYP(M5) FRÅN CONSTANTMEDLEM VALD AV JCL'EN                   
011200* INFON KOMMER SOM FIL D1                                                 
011300                                                                          
011401 01  FELTEXT.                                                             
011501     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
011601     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
011701     EJECT                                                                
012101*    --- PARAMETRAR TILL POSTSUM                                          
012201*                                                                         
012301*01  -COPY W0005   -PRE  POSTSUM-                                         
012401     EJECT                                                                
012501 01  FILLER                      PIC X(16) VALUE 'WDATAREA'.              
012601*01 -COPY WDATAREA                                                        
012701     EJECT                                                                
012801 01  FILLER                      PIC X(24)   VALUE 'IN-AREA'.             
012901                                                                          
013001*01  AREA -COPY W11620B      -PRE IN-                                     
013101     EJECT                                                                
013601 01  FILLER                      PIC X(24)   VALUE 'UT-AREA'.             
013701                                                                          
013801*01  AREA -COPY W11620       -PRE UT-                                     
013901     EJECT                                                                
014001*    --- ARBETS-AREOR TILL  IMS-SEKTIONERNA                               
014101*                                                                         
014201 01  NYCKLAR-TILL-DLI.                                                    
016001     03  W-WDB101KY-X.                                                    
016101         05  W-IDPARTNR            PIC X(9)  VALUE SPACE.                 
016201         05  W-IDFTG               PIC 9(2)  VALUE ZERO.                  
016301                                                                          
016401     03   W-WDB1B-LOW-X.                                                  
016501         05  W-B-IDLANDX2-LOW      PIC X(2)  VALUE SPACE.                 
016601                                                                          
016701     03  W-WDB1B-HIGH-X.                                                  
016801         05  W-B-IDLANDX2-HIGH     PIC X(2)  VALUE HIGH-VALUE.            
016901                                                                          
017001     03  W-WDB1B1KY-LOW.                                                  
017101         05  W-IDLANDX2-LOW        PIC X(2)    VALUE SPACE.               
017201         05  W-IDMARKBO-LOW        PIC X(1)    VALUE 'A'.                 
017301         05  W-IDPARTNR-LOW        PIC X(9)    VALUE LOW-VALUE.           
017401         05  W-IDFTG-LOW           PIC 9(2)    VALUE ZERO.                
017501                                                                          
017601     03  W-WDB1B1KY-HIGH.                                                 
017701         05  W-IDLANDX2-HIGH       PIC X(2)    VALUE SPACE.               
017801         05  W-IDMARKBO-HIGH       PIC X(1)    VALUE 'G'.                 
017901         05  W-IDPARTNR-HIGH       PIC X(9)    VALUE HIGH-VALUE.          
018001         05  W-IDFTG-HIGH          PIC 9(2)    VALUE 99.                  
018101                                                                          
018201                                                                          
018301                                                                          
018401*    --- STATUS-KOD FRÅN IMS                                              
018501 01  STATUS-WS                   PIC XX.                                  
018601     88  SEGMENT-FINNS                       VALUE '  '.                  
018701     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
018801     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
018901     88  BASEN-SLUT                          VALUE 'GB'.                  
019001     SKIP2                                                                
019101 01  GODK-STATUSKODER.                                                    
019201     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
019301     SKIP3                                                                
019401 01  SSA1                        PIC X(64).                               
019501 01  SSA2                        PIC X(64).                               
019601     EJECT                                                                
019701*    --- IMS FUNKTIONSKODER                                               
019801*01  -COPY W0003                                                          
019901     EJECT                                                                
020401 01  FILLER                  PIC X(16)   VALUE 'WDB1B1-POST'.             
020501 01  DLI-IO-WDB1B1.                                                       
020601*    03  -COPY WDB1B1                                                     
020701      EJECT                                                               
020801 01  DLI-IO-WDB101.                                                       
020901*    03  -COPY WDB101                                                     
021001      EJECT                                                               
022001 LINKAGE SECTION.                                                         
022401*01  -COPY W0008      -PRE WDB1-                                          
022501     05  FILLER                  PIC X.                                   
022601     EJECT                                                                
022701*01  -COPY W0008      -PRE  WDB1B-                                        
022801     05  FILLER                  PIC X(16).                               
022901     EJECT                                                                
023001 PROCEDURE DIVISION   USING  WDB1-PCB WDB1B-PCB.                          
023201 MAIN SECTION.                                                            
023301     ENTRY 'DLITCBL'  USING  WDB1-PCB WDB1B-PCB.                          
023501                                                                          
023601     PERFORM A-INIT                                                       
023701                                                                          
023801     PERFORM S01-LAES-W11624                                              
023901     PERFORM UNTIL END-OF-W11624                                          
024001         MOVE IN-PRARTSJK    TO WS-PRARTSJK                               
024101         PERFORM S07-HAMTA-IDMARKBO                                       
024201         PERFORM B-FLYTTA-SKRIV-UTPOST                                    
024301         PERFORM S01-LAES-W11624                                          
024401     END-PERFORM                                                          
024501                                                                          
024601     PERFORM Z-FINIT                                                      
024701                                                                          
024801     MOVE ZERO TO RETURN-CODE                                             
024901     GOBACK                                                               
025001     .                                                                    
025101     EJECT                                                                
025201 A-INIT SECTION.                                                          
025301                                                                          
025401     OPEN INPUT  W11624                                                   
025601          OUTPUT W11682                                                   
025701                                                                          
025801     MOVE FUNCTION CURRENT-DATE(1:8) TO DAGENS-DATUM                      
025901                                                                          
026401     .                                                                    
026501     EJECT                                                                
026601 B-FLYTTA-SKRIV-UTPOST SECTION.                                           
026701                                                                          
026801     IF  IN-IDLANDX2 = WS-MQ-IDLANDX2                                     
027001       CONTINUE                                                           
027101     ELSE                                                                 
027201       WRITE UT-POST           FROM WS-MQ-LINE1                           
027301       MOVE SPACE                TO UT-POST                               
027302       WRITE UT-POST           FROM WS-MQ-LINE2                           
027303       MOVE SPACE                TO UT-POST                               
027501       MOVE IN-IDLANDX2          TO WS-MQ-IDLANDX2                        
027701       WRITE UT-POST           FROM WS-MQ-LINE3                           
027801       MOVE SPACE                TO UT-POST                               
027901     END-IF                                                               
028001                                                                          
028100     MOVE IN-IDPTYP              TO UT-IDPTYP                             
028201     MOVE IN-IDARTNR20           TO UT-IDARTNR20                          
028301     MOVE IN-RAD                 TO UT-RAD                                
028401     MOVE IN-DADATUM             TO UT-DADATUM                            
028501     MOVE IN-IDLEVNR-DUBLETT     TO UT-IDLEVNR-DUBLETT                    
028601     MOVE IN-IDLEVNR-LOC-DUBLETT TO UT-IDLEVNR-LOC-DUBLETT                
028701     MOVE IN-KDTIPPR             TO UT-KDTIPPR                            
028801     MOVE IN-IDKAT(01)           TO UT-IDKAT(01)                          
028901     MOVE IN-IDKAT(02)           TO UT-IDKAT(02)                          
029001     MOVE IN-IDKAT(03)           TO UT-IDKAT(03)                          
029101     MOVE IN-BELEVART            TO UT-BELEVART                           
029201     MOVE IN-FLGEMFMC            TO UT-FLGEMFMC                           
029301     MOVE IN-IDPROJUP            TO UT-IDPROJUP                           
029401     MOVE IN-BEARTEXT            TO UT-BEARTEXT                           
029501     MOVE IN-TIURPROD           TO UT-TIURPROD                            
029700                                                                          
029800     WRITE UT-POST FROM UT-AREA                                           
029900                                                                          
030000     MOVE 'W11682'   TO POSTSUM-FDNAMN                                    
030100     MOVE 'W11682D2' TO POSTSUM-DDNAMN2                                   
030200     CALL POSTSUM USING POSTSUM-PARM                                      
030300     .                                                                    
030400 Z-FINIT SECTION.                                                         
030500                                                                          
030600     DISPLAY 'MARKN.BOLAG ' WS-IDMARKBO                                   
030700                                                                          
030800     CLOSE W11624                                                         
030900           W11682                                                         
031100                                                                          
031200     MOVE 'S'        TO POSTSUM-OPKOD                                     
031300     CALL POSTSUM USING POSTSUM-PARM                                      
031400     .                                                                    
031500     EJECT                                                                
031600 S01-LAES-W11624  SECTION.                                                
031700                                                                          
031800     READ W11624 INTO IN-AREA                                             
031900     AT END                                                               
032000        SET END-OF-W11624 TO TRUE                                         
032100     NOT AT END                                                           
032200        MOVE 'W11624'   TO POSTSUM-FDNAMN                                 
032300        MOVE 'W11682D1' TO POSTSUM-DDNAMN2                                
032400        CALL POSTSUM USING POSTSUM-PARM                                   
032500     END-READ                                                             
032600     .                                                                    
032700     EJECT                                                                
036401 S07-HAMTA-IDMARKBO   SECTION.                                            
036501                                                                          
036601     IF WS-IDMARKBO = SPACE                                               
036701*INGER KOLLA ATT JAG HAR RÄTT NYCKLAR?                                    
036800       MOVE IN-IDLANDX2           TO W-B-IDLANDX2-LOW                     
036900                                     W-B-IDLANDX2-HIGH                    
037000                                     W-IDLANDX2-LOW                       
037100                                     W-IDLANDX2-HIGH                      
037200       PERFORM IMS-GU-WDB1B1                                              
037300       IF SEGMENT-FINNS                                                   
037400         MOVE SEQB-IDPARTNR           TO W-IDPARTNR                       
037500         MOVE SEQB-IDFTG              TO W-IDFTG                          
037600         PERFORM IMS-GU-WDB101                                            
037700         IF SEGMENT-FINNS                                                 
037800           MOVE BET-IDMARKBO          TO WS-IDMARKBO                      
037900         END-IF                                                           
038000       END-IF                                                             
038100       IF IN-IDLANDX2 = 'US'                                              
038200*           USA KAN FÅ FEL MARKNADSBOLAG                                  
038300         MOVE 'E'                     TO WS-IDMARKBO                      
038400       END-IF                                                             
038500     END-IF                                                               
038601                                                                          
039000     .                                                                    
039100     EJECT                                                                
040400**********************IMS-LÄSNINGAR*********                              
041801 IMS-GU-WDB101 SECTION.                                                   
041901     STRING 'WDB101  (WDB101KY =' W-WDB101KY-X ')'                        
042001          DELIMITED BY SIZE INTO SSA1                                     
042101     MOVE '  GE' TO GODK-STATUSKODER                                      
042201     CALL CBLTDLI USING GU WDB1-PCB DLI-IO-WDB101 SSA1                    
042301     MOVE WDB1-STATUS-CODE TO STATUS-WS                                   
042401     PERFORM IMS-STATUSKONTROLL                                           
042501     .                                                                    
042601     SKIP3                                                                
042701 IMS-GU-WDB1B1 SECTION.                                                   
042801     STRING 'WDB1B1  (WDB1B1KY=>' W-WDB1B1KY-LOW                          
042901                    '&WDB1B1KY=<' W-WDB1B1KY-HIGH ')'                     
043001          DELIMITED BY SIZE INTO SSA1                                     
043101     MOVE '    ' TO GODK-STATUSKODER                                      
043201     CALL CBLTDLI USING GU WDB1B-PCB DLI-IO-WDB1B1 SSA1                   
043301     MOVE WDB1B-STATUS-CODE TO STATUS-WS                                  
043401     PERFORM IMS-STATUSKONTROLL                                           
043501     .                                                                    
043601     SKIP3                                                                
043701 IMS-STATUSKONTROLL SECTION.                                              
043801                                                                          
043901     SET STATUS-IX TO 1                                                   
044001     SEARCH GODK-STATUS                                                   
044101       AT END CALL FELLOG                                                 
044201       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
044301     END-SEARCH                                                           
044401     .                                                                    
044501     EJECT                                                                
