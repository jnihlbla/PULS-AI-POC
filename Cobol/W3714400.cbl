001100 ID DIVISION.                                                             
001200 PROGRAM-ID.     W3714400.                                                
001300 AUTHOR.         MARKUS ASPFJÄLL.                                         
001400 DATE-WRITTEN.   99/12/27.                                                
001500 DATE-COMPILED.                                                           
001600                                                                          
001700                                                                          
001800*    PROGRAMTYP:BMP                                                       
001810*    FUNKTION:                                                            
001900*        LÄSER INFIL W37131 OCH UPPDATERAR WDGX58-WDR1                    
002000*                                                                         
002110*        PROGRAMMET UPPDATERAR WDR1                                       
002200*                                                                         
002300                                                                          
002400     SKIP3                                                                
002500 ENVIRONMENT DIVISION.                                                    
002600     SKIP2                                                                
002700 INPUT-OUTPUT SECTION.                                                    
002800                                                                          
002900 FILE-CONTROL.                                                            
003001     SKIP2                                                                
003002*          --- INFIL MED UPPDATERINGS UPPGIFTER WDGX3158                  
003010     SELECT W37131                     ASSIGN TO W37144D1.                
003200     EJECT                                                                
003300 DATA DIVISION.                                                           
003400     SKIP3                                                                
003500 FILE SECTION.                                                            
003601     SKIP3                                                                
003602 FD  W37131                                                               
003603     RECORDING       F                                                    
003604     BLOCK CONTAINS  0.                                                   
003605                                                                          
003610*01  -COPY W37107      -L.                                                
003700     EJECT                                                                
003800 WORKING-STORAGE SECTION.                                                 
003900                                                                          
003901                                                                          
003910*    -- CHECKED BY WY2000                                                 
004000 77  IDPGM                       PIC X(8)    VALUE 'W3714400'.            
004100 01  CHKP-VAR.                                                            
004200     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
004300     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
004400     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
004500     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
004600     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
004700     03 CHKP-MAX                 PIC S9(3)   VALUE +500 COMP-3.           
004800 77  JA                          PIC X       VALUE 'J'.                   
004900 77  NEJ                         PIC X       VALUE 'N'.                   
005000     SKIP2                                                                
005100 01  FELTEXT.                                                             
005200     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005300     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005501                                                                          
005502 77  W37131-EOF-SW               PIC X       VALUE 'N'.                   
005510     88  END-OF-W37131                       VALUE 'J'.                   
005800     EJECT                                                                
005900 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006000 01  FILLER REDEFINES DAGENS-DATUM.                                       
006100     03  DAGENS-DATUM-AAR        PIC 9(2).                                
006200     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
006300     03  DAGENS-DATUM-DAG        PIC 9(2).                                
006400     EJECT                                                                
006500 01  DYNAMISKA-SUBPROGRAM.                                                
006600*                                                                         
006700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006910     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007001     EJECT                                                                
007002*    --- PARAMETRAR TILL POSTSUM                                          
007003*                                                                         
007010*01  -COPY W0005   -PRE  POSTSUM-                                         
007301     EJECT                                                                
007302 01  IN-AREA-START               PIC X(24)   VALUE                        
007303                                             'IN-AREA-START'.             
007304     SKIP2                                                                
007305                                                                          
007310*01  AREA -COPY W37107     -PRE IN-                                       
007400*                                                                         
007500     EJECT                                                                
007600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007700     SKIP3                                                                
007800 01  NYCKLAR-TILL-DLI.                                                    
007901     03  W-3157-IDHTYP-X.                                                 
007902         05  W-3157-IDHTYP       PIC  X(4)  VALUE '3157'.                 
007903         05  W-3157-LOW-VALUE    PIC  X(26) VALUE LOW-VALUE.              
007904                                                                          
007905     03  W-3158-WDGXKEY-X.                                                
007910         05  W-3158-IDDISTR      PIC  S9(5) VALUE ZERO COMP-3.            
007911         05  W-3158-KDEXCHA      PIC  S9(3) VALUE ZERO COMP-3.            
008000     SKIP2                                                                
008100*    --- STATUS-KOD FRÅN IMS                                              
008200 01  STATUS-WS                   PIC XX.                                  
008300     88  SEGMENT-FINNS                       VALUE '  '.                  
008400     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
008500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
008600     88  SEGMENT-SLUT                        VALUE 'GB'.                  
008700     88  IMS-EJ-OK                           VALUE 'XD'.                  
008800     SKIP2                                                                
008900 01  GODK-STATUSKODER.                                                    
009000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009100     SKIP3                                                                
009200 01  SSA1                        PIC X(64).                               
009300 01  SSA2                        PIC X(64).                               
009400     EJECT                                                                
009500*    --- IMS FUNKTIONSKODER                                               
009600*01  -COPY W0003                                                          
009700     EJECT                                                                
009900*    ---  DLI INPUT-OUTPUT AREA                                           
010000                                                                          
010101 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX3158'.                    
010102 01  DLI-IO-WDGX3158.                                                     
010110*    03  -COPY WDGX3158                                                   
010200                                                                          
010600     EJECT                                                                
010700 LINKAGE SECTION.                                                         
010800                                                                          
010900*01  -COPY W0009   -PRE MSG-                                              
011001                                                                          
011002*01  -COPY W0008  -PRE 3158-                                              
011010     05  FILLER                  PIC X.                                   
011300     EJECT                                                                
011401 PROCEDURE DIVISION  USING MSG-PCB 3158-PCB.                              
011402 MAIN SECTION.                                                            
011410     ENTRY 'DLITCBL' USING MSG-PCB 3158-PCB.                              
011500                                                                          
011700     SKIP2                                                                
011800     PERFORM A-INIT                                                       
011910     PERFORM S01-LAES-W37131                                              
012000     PERFORM UNTIL END-OF-W37131                                          
012100       IF CHKP-ANT > CHKP-MAX                                             
012200         PERFORM X-TAG-CHECKPOINT                                         
012300       END-IF                                                             
012400       MOVE IN-IDDISTR      TO W-3158-IDDISTR                             
012500       MOVE IN-KDEXCHA      TO W-3158-KDEXCHA                             
012600       PERFORM IMS-GU-WDGX3158                                            
012700       IF SEGMENT-FINNS                                                   
012800         PERFORM B-BEARBETA                                               
012801         PERFORM IMS-REPL-WDGX3158                                        
012802         ADD +1             TO CHKP-ANT                                   
012810       END-IF                                                             
013010       PERFORM S01-LAES-W37131                                            
013100     END-PERFORM                                                          
013200                                                                          
013300                                                                          
013400     PERFORM Z-FINIT                                                      
013500                                                                          
013600     MOVE ZERO TO RETURN-CODE                                             
013700     GOBACK                                                               
013800     .                                                                    
013900     EJECT                                                                
014000 A-INIT SECTION.                                                          
014100     SKIP2                                                                
014200                                                                          
014300     PERFORM IMS-RESTART                                                  
014501                                                                          
014510     OPEN INPUT W37131                                                    
014800                                                                          
015100                                                                          
015210     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
015500     .                                                                    
015700     EJECT                                                                
015710 B-BEARBETA SECTION.                                                      
015720     MOVE IN-SUPOINT-UTG           TO 3158-SUPOINT-BAL                    
015721     MOVE IN-SUPOINT-RIT           TO 3158-SUPOINT-RIT                    
015722     MOVE IN-SUPOINT-PP            TO 3158-SUPOINT-PP                     
015730     .                                                                    
015740     EJECT                                                                
015800 Z-FINIT SECTION.                                                         
015900                                                                          
016301                                                                          
016310     CLOSE W37131                                                         
016501     SKIP2                                                                
016502     MOVE 'S' TO POSTSUM-OPKOD                                            
016510     CALL POSTSUM USING POSTSUM-PARM                                      
016700     .                                                                    
016801     EJECT                                                                
016802 S01-LAES-W37131  SECTION.                                                
016803     SKIP2                                                                
016804     READ W37131 INTO IN-AREA                                             
016805     AT END                                                               
016806        MOVE HIGH-VALUE TO IN-AREA                                        
016807        SET END-OF-W37131 TO TRUE                                         
016808                                                                          
016809     NOT AT END                                                           
016810        MOVE 'W37131' TO POSTSUM-FDNAMN                                   
016811        MOVE 'W37144D1' TO POSTSUM-DDNAMN2                                
016812        MOVE 'UT-'     TO POSTSUM-TRANSTYP                                
016813        CALL POSTSUM USING POSTSUM-PARM                                   
016814                                                                          
016815*       ADD 1 TO W-W37108-KVPOST-IN                                       
016816     END-READ                                                             
016820     .                                                                    
017100     EJECT                                                                
017200 X-TAG-CHECKPOINT   SECTION.                                              
017300                                                                          
017400* --- VID CHECKPOINTTAGGNING SÅ TAPPAR MAN GN-POSITION I BASEN            
017500* --- SPARA DATABASNYCKLAR OM DET BEHÖVS                                  
017900     PERFORM IMS-CHECKPOINT                                               
018000     MOVE ZERO TO CHKP-ANT                                                
018100* --- LÄS OM DATABAS OM DET BEHÖVS                                        
018200     .                                                                    
018300     EJECT                                                                
018400* --- IMS SEKTIONER ---                                                   
018500                                                                          
018601     EJECT                                                                
018602 IMS-GU-WDGX3158 SECTION.                                                 
018603     STRING 'WDR101  (WDGXKEY  =' W-3157-IDHTYP-X ')'                     
018604          DELIMITED BY SIZE INTO SSA1                                     
018605     STRING 'WDGX3158(KEY3158  =' W-3158-WDGXKEY-X ')'                    
018606          DELIMITED BY SIZE INTO SSA2                                     
018607     MOVE '  GE'           TO GODK-STATUSKODER                            
018608     CALL CBLTDLI USING GHU  3158-PCB DLI-IO-WDGX3158 SSA1 SSA2           
018609     MOVE 3158-STATUS-CODE TO STATUS-WS                                   
018610     PERFORM IMS-STATUSKONTROLL                                           
018611     .                                                                    
018612     EJECT                                                                
018613 IMS-REPL-WDGX3158 SECTION.                                               
018614                                                                          
018615     MOVE '  ' TO GODK-STATUSKODER                                        
018616     CALL CBLTDLI USING REPL 3158-PCB DLI-IO-WDGX3158                     
018617     MOVE 3158-STATUS-CODE TO STATUS-WS                                   
018618     PERFORM IMS-STATUSKONTROLL                                           
018619     .                                                                    
018620     EJECT                                                                
018621                                                                          
018622 IMS-GET-WDGX3158 SECTION.                                                
018623                                                                          
018624     STRING 'WDGX3158(KEY3158  =' W-3158-WDGXKEY-X ')'                    
018625          DELIMITED BY SIZE INTO SSA1                                     
018626     MOVE '  GE' TO GODK-STATUSKODER                                      
018627     CALL CBLTDLI USING GHNP 3158-PCB DLI-IO-WDGX3158 SSA1                
018628     MOVE 3158-STATUS-CODE TO STATUS-WS                                   
018629     PERFORM IMS-STATUSKONTROLL                                           
018630     .                                                                    
018631     SKIP3                                                                
018632 IMS-ISRT-WDGX3158 SECTION.                                               
018633                                                                          
018634     STRING 'WDR101  (WDGXKEY  =' W-3157-IDHTYP-X ')'                     
018635          DELIMITED BY SIZE INTO SSA1                                     
018636     MOVE 'WDGX3158 ' TO SSA2                                             
018637     MOVE '  II' TO GODK-STATUSKODER                                      
018638     CALL CBLTDLI USING ISRT 3158-PCB DLI-IO-WDGX3158 SSA1 SSA2           
018639     MOVE 3158-STATUS-CODE TO STATUS-WS                                   
018640     PERFORM IMS-STATUSKONTROLL                                           
018641     .                                                                    
018642     SKIP3                                                                
018800 IMS-RESTART SECTION.                                                     
018900     SKIP2                                                                
019000     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
019100     MOVE '  ' TO GODK-STATUSKODER                                        
019200     CALL CBLTDLI USING XRST MSG-PCB                                      
019300                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
019400                        CHKP-AREA-LENGTH CHKP-AREA                        
019500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
019600     PERFORM IMS-STATUSKONTROLL                                           
019700     .                                                                    
019800     SKIP3                                                                
019900 IMS-CHECKPOINT SECTION.                                                  
020000     SKIP2                                                                
020100     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
020200     MOVE '  XD' TO GODK-STATUSKODER                                      
020300     CALL CBLTDLI USING CHKP MSG-PCB                                      
020400                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
020500                        CHKP-AREA-LENGTH CHKP-AREA                        
020600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
020700     PERFORM IMS-STATUSKONTROLL                                           
020800                                                                          
020900     IF IMS-EJ-OK                                                         
021000       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
021100       DISPLAY FELTEXT                                                    
021200       CALL FELLOG                                                        
021300     END-IF                                                               
021400     .                                                                    
021500     EJECT                                                                
021600 IMS-STATUSKONTROLL SECTION.                                              
021700     SKIP2                                                                
021800     SET STATUS-IX TO 1                                                   
021900     SEARCH GODK-STATUS                                                   
022000       AT END                                                             
022100         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
022200           DELIMITED BY SIZE INTO FELTEXT                                 
022300         DISPLAY FELTEXT                                                  
022400         CALL FELLOG                                                      
022500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
022600         CONTINUE                                                         
022700     END-SEARCH                                                           
022800     .                                                                    
