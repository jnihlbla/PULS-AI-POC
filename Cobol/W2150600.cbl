000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2150600.                                                
000400 AUTHOR.         ANN JORDEBO.                                             
000500 DATE-WRITTEN.   91/01/31.                                                
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        BMP SOM LÄSER FIL FRÅN W2150200. FILEN INNEHÅLLER                
001100*        SATSARTIKLAR SOM SKA BEORDRAS I SATSORDERSYSTEMET.               
001200*        PROGRAMMET LÄGGER UPP ETT SEGMENT PÅ WLXXCN11 PER                
001300*        ARTIKEL/POST.                                                    
001400*                                                                         
001500*        PROGRAMMET UPPDATERAR WLXXCN (WDR5)                              
001600*                                                                         
001700*    ABENDKODER:                                                          
001800*        U0016 -  . . . .                                                 
001900*        U1000 -  . . . .                                                 
002000*                                                                         
002100                                                                          
002200     SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400     SKIP2                                                                
002500 INPUT-OUTPUT SECTION.                                                    
002600                                                                          
002700 FILE-CONTROL.                                                            
002800     SKIP2                                                                
002900*          --- FIL MED SATSARTIKLAR FÖR BEORDRING                         
003000     SELECT W21502                     ASSIGN TO W21506D1.                
003100     EJECT                                                                
003200 DATA DIVISION.                                                           
003300     SKIP3                                                                
003400 FILE SECTION.                                                            
003500     SKIP3                                                                
003600 FD  W21502                                                               
003700     LABEL RECORD    STANDARD                                             
003800     RECORDING       F                                                    
003900     BLOCK CONTAINS  0.                                                   
004000     SKIP2                                                                
004100*01  -COPY W21502          -L.                                            
004300                                                                          
004400     EJECT                                                                
004500 WORKING-STORAGE SECTION.                                                 
004600     SKIP2                                                                
004601                                                                          
004610*    -- CHECKED BY WY2000                                                 
004700 77  IDPGM                       PIC X(8)    VALUE 'W2150600'.            
004800 77  MSG-IO-AREA-LENGTH          PIC S9(9)   VALUE +32 COMP SYNC.         
004900 77  MSG-IO-AREA                 PIC X(32)   VALUE SPACE.                 
005000 77  CHKP-AREA-LENGTH            PIC S9(9)   VALUE +32 COMP SYNC.         
005100 77  CHKP-AREA                   PIC X(32)   VALUE SPACE.                 
005200 77  JA                          PIC X       VALUE 'J'.                   
005300 77  NEJ                         PIC X       VALUE 'N'.                   
005400 77  CHKP-MAX                    PIC S9(3)   VALUE +100.                  
005500 77  CHKP-ANT                    PIC S9(3)   VALUE ZERO.                  
005600                                                                          
005700 77  W21502-EOF-SW               PIC X       VALUE 'N'.                   
005800     88  END-OF-W21502                       VALUE 'J'.                   
005900     EJECT                                                                
006000 01  DYNAMISKA-SUBPROGRAM.                                                
006100*                                                                         
006200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006400     03  VIMSREGT                PIC X(8)    VALUE 'VIMSREGT'.            
006500     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006600     EJECT                                                                
006700*    --- PARAMETRAR TILL VIMSREGT                                         
006800     SKIP2                                                                
006900 01  FILLER                      PIC X(16)   VALUE                        
007000                                             'VIMSREGT-AREA'.             
007100     SKIP2                                                                
007200 01  IMS-VIMSREGT                PIC S9(9)   COMP SYNC.                   
007300     88  BMP                                 VALUE +8.                    
007400     88  BATCH                               VALUE +16 THRU +64.          
007500     EJECT                                                                
007600*    --- PARAMETRAR TILL POSTSUM                                          
007700*                                                                         
007800*01  -COPY W0005      -PRE  POSTSUM-                                      
008000     EJECT                                                                
008100 01  IN-AREA-START               PIC X(24)   VALUE                        
008200                                             'IN-AREA-START'.             
008300     SKIP2                                                                
008400                                                                          
008500*01  AREA -COPY W21502         -PRE IN-                                   
008700*                                                                         
008800     EJECT                                                                
008900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009000     SKIP3                                                                
009100 01  NYCKLAR-TILL-DLI.                                                    
009200     03  W-WDGXKEY-2235-X.                                                
009300         05  W-IDHTYP            PIC X(4)    VALUE '2235'.                
009400         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
009500     SKIP2                                                                
009600*    --- STATUS-KOD FRÅN IMS                                              
009700 01  STATUS-WS                   PIC XX.                                  
009800     88  SEGMENT-FINNS                       VALUE '  '.                  
009900     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
010000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
010100     88  SEGMENT-SLUT                        VALUE 'GB'.                  
010200     88  IMS-EJ-OK                           VALUE 'XD'.                  
010300     SKIP2                                                                
010400 01  GODK-STATUSKODER.                                                    
010500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
010600     SKIP3                                                                
010700 01  SSA1                        PIC X(64).                               
010800 01  SSA2                        PIC X(64).                               
010900     EJECT                                                                
011000*    --- IMS FUNKTIONSKODER                                               
011100*01  -COPY W0003                                                          
011300     EJECT                                                                
011400*    ---  DLI INPUT-OUTPUT AREA                                           
011500 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
011600     SKIP3                                                                
011700 01  DLI-IO-AREA.                                                         
011800     03  IO-AREA                 PIC X(42)   VALUE SPACE.                 
011900     SKIP3                                                                
012000     03  WLXXCN11 REDEFINES IO-AREA.                                      
012100*        05  -COPY WDGX2236   -PRE XXCN-                                  
012300     EJECT                                                                
012400 LINKAGE SECTION.                                                         
012500                                                                          
012600*01  -COPY W0009      -PRE MSG-                                           
012800     EJECT                                                                
012900*01  -COPY W0008      -PRE XXCN-                                          
013100     05  FILLER                  PIC X.                                   
013200     EJECT                                                                
013300 PROCEDURE DIVISION  USING MSG-PCB XXCN-PCB.                              
013400     ENTRY 'DLITCBL' USING MSG-PCB XXCN-PCB.                              
013500                                                                          
013600     SKIP2                                                                
013700     PERFORM A-INIT                                                       
013800                                                                          
013900     PERFORM S01-LAES-W21502                                              
014000     MOVE +1 TO CHKP-ANT                                                  
014100     PERFORM UNTIL END-OF-W21502                                          
014200       IF CHKP-ANT > CHKP-MAX                                             
014300         PERFORM X-TAG-CHECKPOINT                                         
014400         MOVE +0 TO CHKP-ANT                                              
014500       END-IF                                                             
014600       PERFORM B-FLYTTA-DATA                                              
014700       PERFORM IMS-ISRT-XXCN-2236                                         
014800       PERFORM S01-LAES-W21502                                            
014900                                                                          
015000       ADD +1 TO CHKP-ANT                                                 
015100     END-PERFORM                                                          
015200                                                                          
015300     PERFORM Z-FINIT                                                      
015400                                                                          
015500     MOVE ZERO TO RETURN-CODE                                             
015600     GOBACK                                                               
015700     .                                                                    
015800     EJECT                                                                
015900 A-INIT SECTION.                                                          
016000                                                                          
016100     PERFORM IMS-RESTART                                                  
016200                                                                          
016300     OPEN INPUT W21502                                                    
016400                                                                          
016500     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
016600     .                                                                    
016700     EJECT                                                                
016800 B-FLYTTA-DATA SECTION.                                                   
016900                                                                          
017000     MOVE IN-IDARTNR     TO XXCN-2236-IDARTNR                             
017100     MOVE IN-TIBEHOV     TO XXCN-2236-TIBEHOV                             
017200     MOVE LOW-VALUE      TO XXCN-2236-LOW-VALUE                           
017300     MOVE IN-IDANSK      TO XXCN-2236-IDANSK                              
017400     MOVE IN-IDDISTR     TO XXCN-2236-IDDISTR                             
017500     MOVE IN-IDLEVNR     TO XXCN-2236-IDLEVNR                             
017600     MOVE IN-KDCLAGER    TO XXCN-2236-KDCLAGER                            
017700     MOVE IN-KVBEART     TO XXCN-2236-KVBEART                             
017800     MOVE LOW-VALUE      TO XXCN-2236-FILLER                              
017900     .                                                                    
018000     EJECT                                                                
018100 X-TAG-CHECKPOINT   SECTION.                                              
018200                                                                          
018300     PERFORM IMS-CHECKPOINT                                               
018400     .                                                                    
018500     EJECT                                                                
018600 Z-FINIT SECTION.                                                         
018700                                                                          
018800     CLOSE W21502                                                         
018900                                                                          
019000     MOVE 'S' TO POSTSUM-OPKOD                                            
019100     CALL POSTSUM USING POSTSUM-PARM                                      
019200     .                                                                    
019300     EJECT                                                                
019400 S01-LAES-W21502  SECTION.                                                
019500     SKIP2                                                                
019600     READ W21502 INTO IN-AREA                                             
019700     AT END                                                               
019800        SET END-OF-W21502 TO TRUE                                         
019900                                                                          
020000     NOT AT END                                                           
020100        MOVE 'W21502' TO POSTSUM-FDNAMN                                   
020200        MOVE 'W21506D1' TO POSTSUM-DDNAMN2                                
020300        CALL POSTSUM USING POSTSUM-PARM                                   
020400     END-READ                                                             
020500     .                                                                    
020600     EJECT                                                                
020700* --- IMS SEKTIONER ---                                                   
020800     SKIP3                                                                
020900     EJECT                                                                
021000 IMS-ISRT-XXCN-2236 SECTION.                                              
021100                                                                          
021200     STRING 'WLXXCN01(WDGXKEY  =' W-WDGXKEY-2235-X ')'                    
021300          DELIMITED BY SIZE INTO SSA1                                     
021400     MOVE 'WLXXCN11 ' TO SSA2                                             
021500     MOVE '  II' TO GODK-STATUSKODER                                      
021600     CALL CBLTDLI USING ISRT XXCN-PCB DLI-IO-AREA SSA1 SSA2               
021700     MOVE XXCN-STATUS-CODE TO STATUS-WS                                   
021800     PERFORM IMS-STATUSKONTROLL                                           
021900     .                                                                    
022000     EJECT                                                                
022100 IMS-RESTART SECTION.                                                     
022200     SKIP2                                                                
022300     MOVE SPACE TO MSG-IO-AREA                                            
022400     MOVE '  ' TO GODK-STATUSKODER                                        
022500     CALL CBLTDLI USING XRST MSG-PCB                                      
022600                        MSG-IO-AREA-LENGTH MSG-IO-AREA                    
022700                        CHKP-AREA-LENGTH CHKP-AREA                        
022800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
022900     PERFORM IMS-STATUSKONTROLL                                           
023000     .                                                                    
023100     EJECT                                                                
023200 IMS-CHECKPOINT SECTION.                                                  
023300     SKIP2                                                                
023400     MOVE SPACE TO MSG-IO-AREA                                            
023500     MOVE '  XD' TO GODK-STATUSKODER                                      
023600     CALL CBLTDLI USING CHKP MSG-PCB                                      
023700                        MSG-IO-AREA-LENGTH MSG-IO-AREA                    
023800                        CHKP-AREA-LENGTH CHKP-AREA                        
023900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
024000     PERFORM IMS-STATUSKONTROLL                                           
024100                                                                          
024200     IF IMS-EJ-OK                                                         
024300       DISPLAY 'IMS-KONTROLLREGION EJ TILLGÄNGLIG'                        
024400       CALL FELLOG                                                        
024500     END-IF                                                               
024600     .                                                                    
024700     EJECT                                                                
024800 IMS-STATUSKONTROLL SECTION.                                              
024900     SKIP2                                                                
025000     SET STATUS-IX TO 1                                                   
025100     SEARCH GODK-STATUS                                                   
025200       AT END CALL FELLOG                                                 
025300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
025400     END-SEARCH                                                           
025500     .                                                                    
