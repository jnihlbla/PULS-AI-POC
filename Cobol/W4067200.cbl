000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4067200.                                                
000400 AUTHOR.         CAP GEMINI / BOH                                         
000500     DATE-WRITTEN.   JAN   86.                                            
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION.                                                            
001000*    PROGRAMMET ÄR ETT FRÅGEPROGRAM SOM ÄVEN KAN ANROPAS FRÅN             
001100*    W4067100.                                                            
001200*                                                                         
001300*                                                                         
001400*    INDATA.                                                              
001500*        TRANSAKTION: W4T672                                              
001600*        MID:         W4I67201                                            
001700*                                                                         
001800*    UTDATA.                                                              
001900*        MOD:         W4O67201                                            
002000     EJECT                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200     SKIP3                                                                
002300 DATA DIVISION.                                                           
002400     EJECT                                                                
002500 WORKING-STORAGE SECTION.                                                 
002501                                                                          
002510*    -- CHECKED BY WY2000                                                 
002600 77   PROGRAM-NAMN           VALUE 'W4067200'                             
002700                                 PIC X(8).                                
002800 77  JA                          PIC X(1)    VALUE 'J'.                   
002900 77  NEJ                         PIC X(1)    VALUE 'N'.                   
003000 77  INDX                        PIC S9(4)   COMP SYNC.                   
003100 77  MOD-LAENGD                  PIC S9(4)   VALUE +182 COMP SYNC.        
003200 77  WS-BEROUTE                  PIC X(25).                               
003300 77  WS-IDTRANSP-NAMN            PIC X(15).                               
003400 77  WS-IDTRANS                  PIC X(4).                                
003500     88  WS-GODKAEND-BILD                    VALUE '4671' '4672'.         
003600     SKIP2                                                                
003700 01  DYNAMISKA-SUBPROGRAM.                                                
003800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
003900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
004000     EJECT                                                                
004100 01  NYCKLAR-TILL-DLI.                                                    
004200                                                                          
004300     03  W-4423-WDGXKEY-X.                                                
004400         05  FILLER              PIC X(4)    VALUE '4423'.                
004500         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
004600                                                                          
004700     03  W-4424-WDGXKEY-X.                                                
004800         05  W-4424-BEROUTE       PIC X(25).                              
004900         05  W-4424-IDTRANSP-NAMN PIC X(15).                              
005000     EJECT                                                                
005100 01  FELMEDDELANDE.                                                       
005200                                                                          
005300     03  FEL-1.                                                           
005400         05  FILLER              PIC X(40)   VALUE                        
005500             '749 FEL NYCKEL                          '.                  
005600         05  FILLER              PIC X(40)   VALUE                        
005700             '749 VERKEERDE SLEUTEL - HERBEGIN        '.                  
005800     03  FEL-749 REDEFINES FEL-1 OCCURS 2 PIC X(40).                      
005900     EJECT                                                                
006000 01    FILLER              PIC X(16)   VALUE 'MFS-WS'.                    
006100     SKIP3                                                                
006200 01    FILLER              PIC X(16)   VALUE 'MID W4I672 MID'.            
006300*01  MID -COPY W4I67201.                                                  
006500     EJECT                                                                
006600*01  -COPY WMSGAREA                                                       
006800     EJECT                                                                
006900*    03  MOD -COPY W4O67201  -RED MSG-AREA.                               
007100     EJECT                                                                
007200*01  -COPY WMFSAREA                                                       
007400     EJECT                                                                
007500 01  IMS-WS.                                                              
007600     03  FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
007700     SKIP3                                                                
007800*                        **** STATUS-KOD FRÅN IMS                         
007900     03  STATUS-WS               PIC X(2).                                
008000         88  SEGMENT-FINNS                   VALUE '  '.                  
008100         88  SEGMENT-SAKNAS                  VALUE 'GE'.                  
008200     SKIP3                                                                
008300     03  GODK-STATUSKODER.                                                
008400         05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC X(2).          
008500     SKIP3                                                                
008600 01  SSA1                        PIC X(64).                               
008700 01  SSA2                        PIC X(64).                               
008800     EJECT                                                                
008900*                            IMS FUNKTIONSKODER                           
009000*01  -COPY W0003                                                          
009200     EJECT                                                                
009300*                            DLI INPUT-OUTPUT AREA                        
009400 01  DLI-IO-AREA.                                                         
009500     03  IO-AREA                 PIC X(140)  VALUE SPACE.                 
009600     SKIP2                                                                
009700*    03  WLXXDQ11 -COPY WDGX4424   -RED IO-AREA.                          
009900     EJECT                                                                
010000 LINKAGE SECTION.                                                         
010100*01  -COPY W0009     -PRE MSG-                                            
010300     EJECT                                                                
010400*01  -COPY W0008     -PRE XXDQ-                                           
010600     05  FILLER                  PIC X.                                   
010700     EJECT                                                                
010800 PROCEDURE DIVISION USING MSG-PCB XXDQ-PCB.                               
010900     SKIP2                                                                
011000     ENTRY 'DLITCBL' USING MSG-PCB XXDQ-PCB.                              
011100     SKIP2                                                                
011200     PERFORM IMS-GET-MSG                                                  
011300     SKIP1                                                                
011400     IF SEGMENT-FINNS                                                     
011500         PERFORM A-INIT-SPARA-INPUT                                       
011600     SKIP1                                                                
011700         PERFORM IMS-GU-4423                                              
011800     SKIP1                                                                
011900         PERFORM B-BEHANDLA-ID                                            
012000     SKIP1                                                                
012100         IF NOT WS-GODKAEND-BILD                                          
012200             PERFORM C-RENSA-NYCKLAR                                      
012300         END-IF                                                           
012400         PERFORM IMS-INSERT-MSG                                           
012500     END-IF                                                               
012600     SKIP1                                                                
012700     MOVE ZERO TO RETURN-CODE                                             
012800     GOBACK                                                               
012900     .                                                                    
013000     EJECT                                                                
013100 A-INIT-SPARA-INPUT SECTION.                                              
013200     SKIP2                                                                
013300     IF MSG-DUBBLA-TRANSKODER                                             
013400         MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I67201               
013500         MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                
013600         MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                              
013700         MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                         
013800         MOVE MSG-IDPFK TO MFS-IDPFK                                      
013900     ELSE                                                                 
014000         MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I67201                
014100         MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                
014200         MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                              
014300         MOVE ' ' TO MFS-KDTRTYP        MFS-IDPFK                         
014400     END-IF                                                               
014500     MOVE MFS-IDTRANS TO WS-IDTRANS                                       
014600     SKIP1                                                                
014700     IF MID-BEROUTE-IN = ALL '+'                                          
014800         MOVE MID-BEROUTE-UT TO WS-BEROUTE                                
014900     ELSE                                                                 
015000         MOVE MID-BEROUTE-IN TO WS-BEROUTE                                
015100     END-IF                                                               
015200     SKIP1                                                                
015300     IF MID-IDTRANSP-NAMN-IN = ALL '+'                                    
015400         MOVE MID-IDTRANSP-NAMN-UT TO WS-IDTRANSP-NAMN                    
015500     ELSE                                                                 
015600         MOVE MID-IDTRANSP-NAMN-IN TO WS-IDTRANSP-NAMN                    
015700     END-IF                                                               
015800     SKIP1                                                                
015900     MOVE LOW-VALUE      TO MSG-AREA                                      
016000     MOVE 'W4O67201'     TO MFS-IDMOD                                     
016100     MOVE '4672'         TO MOD-IDTRANS                                   
016200     MOVE MOD-LAENGD     TO MSG-KVLL                                      
016300     SKIP1                                                                
016400     MOVE WS-BEROUTE       TO MOD-BEROUTE-UT                              
016500     MOVE WS-IDTRANSP-NAMN TO MOD-IDTRANSP-NAMN-UT                        
016600     SKIP1                                                                
016700     IF SWEDISH-TEXT                                                      
016800         MOVE +1 TO INDX                                                  
016900     ELSE                                                                 
017000         MOVE +2 TO INDX                                                  
017100     END-IF                                                               
017200     SKIP1                                                                
017300     MOVE MFS-RENSA-FAELT TO MOD-BEROUTE-IN                               
017400                             MOD-IDTRANSP-NAMN-IN                         
017500                             MOD-KVKOLLI-TRPT                             
017600                             MOD-VKORDBTO-TRPT                            
017700                             MOD-VLORDBTO-TRPT                            
017800                             MOD-ADFLGEO                                  
017900                             MOD-ADFLOMR                                  
018000                             MOD-ADRUTNIV                                 
018100                             MOD-TEMFSFEL                                 
018200                             MOD-TEMFSINF                                 
018300     .                                                                    
018400     EJECT                                                                
018500 B-BEHANDLA-ID SECTION.                                                   
018600     SKIP2                                                                
018700     MOVE WS-BEROUTE        TO W-4424-BEROUTE                             
018800     MOVE WS-IDTRANSP-NAMN  TO W-4424-IDTRANSP-NAMN                       
018900     SKIP1                                                                
019000     PERFORM IMS-GNP-4424-KVAL                                            
019100     SKIP1                                                                
019200     IF SEGMENT-FINNS                                                     
019300     SKIP1                                                                
019400         PERFORM BA-REDIGERA-MOD-RAD                                      
019500     SKIP1                                                                
019600     ELSE                                                                 
019700     SKIP1                                                                
019800         MOVE FEL-749(INDX)         TO MOD-TEMFSFEL                       
019900     SKIP1                                                                
020000     END-IF                                                               
020100     .                                                                    
020200     EJECT                                                                
020300 BA-REDIGERA-MOD-RAD SECTION.                                             
020400     SKIP2                                                                
020500     MOVE TCENT-KVKOLLI-TRPT        TO MOD-KVKOLLI-TRPT                   
020600     MOVE TCENT-VKORDBTO-TRPT       TO MOD-VKORDBTO-TRPT                  
020700     MOVE TCENT-VLORDBTO-TRPT       TO MOD-VLORDBTO-TRPT                  
020800     MOVE TCENT-ADFLGEO             TO MOD-ADFLGEO                        
020900     MOVE TCENT-ADFLOMR             TO MOD-ADFLOMR                        
021000     MOVE TCENT-ADRUTNIV            TO MOD-ADRUTNIV                       
021100     .                                                                    
021200     EJECT                                                                
021300 C-RENSA-NYCKLAR SECTION.                                                 
021400     MOVE MFS-RENSA-FAELT            TO MOD-BEROUTE-UT                    
021500                                        MOD-IDTRANSP-NAMN-UT              
021600     EJECT                                                                
021700* IMS SEKTIONER                                                           
021800     SKIP3                                                                
021900     .                                                                    
022000 IMS-GET-MSG SECTION.                                                     
022100     MOVE '  QC' TO GODK-STATUSKODER                                      
022200     CALL CBLTDLI USING GU                                                
022300                          MSG-PCB                                         
022400                          MSG-IO-AREA                                     
022500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
022600     PERFORM IMS-STATUSKONTROLL                                           
022700     SKIP3                                                                
022800     .                                                                    
022900 IMS-INSERT-MSG SECTION.                                                  
023000     IF ENGLISH-TEXT                                                      
023100       MOVE 'N' TO MFS-KDHUVOMR                                           
023200     END-IF                                                               
023300     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
023400     MOVE SPACE TO GODK-STATUSKODER                                       
023500     CALL CBLTDLI USING ISRT                                              
023600                          MSG-PCB                                         
023700                          MSG-IO-AREA                                     
023800                          MFS-IDMOD                                       
023900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
024000     PERFORM IMS-STATUSKONTROLL                                           
024100     .                                                                    
024200     EJECT                                                                
024300 IMS-GU-4423 SECTION.                                                     
024400     STRING 'WLXXDQ01(WDGXKEY  =' W-4423-WDGXKEY-X ')'                    
024500            DELIMITED BY SIZE INTO SSA1                                   
024600     MOVE '  ' TO GODK-STATUSKODER                                        
024700     CALL CBLTDLI USING GU                                                
024800                          XXDQ-PCB                                        
024900                          DLI-IO-AREA                                     
025000                          SSA1                                            
025100     MOVE XXDQ-STATUS-CODE TO STATUS-WS                                   
025200     PERFORM IMS-STATUSKONTROLL                                           
025300     SKIP3                                                                
025400     .                                                                    
025500 IMS-GNP-4424-KVAL SECTION.                                               
025600     STRING 'WLXXDQ11(WDGXKEY  =' W-4424-WDGXKEY-X ')'                    
025700            DELIMITED BY SIZE INTO SSA1                                   
025800     MOVE '  GE' TO GODK-STATUSKODER                                      
025900     CALL CBLTDLI USING GNP                                               
026000                          XXDQ-PCB                                        
026100                          DLI-IO-AREA                                     
026200                          SSA1                                            
026300     MOVE XXDQ-STATUS-CODE TO STATUS-WS                                   
026400     PERFORM IMS-STATUSKONTROLL                                           
026500     .                                                                    
026600     EJECT                                                                
026700 IMS-STATUSKONTROLL SECTION.                                              
026800     SET STATUS-IX TO 1                                                   
026900     SEARCH GODK-STATUS AT END CALL FELLOG                                
027000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
027100     END-SEARCH                                                           
027200     .                                                                    
