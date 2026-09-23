000100 ID DIVISION.                                                             
000200*                                                                         
000300 PROGRAM-ID.             W4882200.                                        
000400*AUTHOR.                 MATS VINNEFORS.                                  
000500*DATE-WRITTEN.           MARS 1985.                                       
000600                                                                          
000700                                                                          
000800*      FUNKTION:                                                          
000900*            UPPDATERING AV CENTRAL SALDOBAS.                             
001000*            UPPDATERING AV HÄNDELSEBASEN.                                
001100*            SKAPAR FELTRANSAR PÅ HÄNDELSEBASEN.                          
001200                                                                          
001300*      SUBPROGRAM:                                                        
001400*            W4882210   - SKÖTER ALLA ANROP MOT IMS                       
001500*            FELLOG                                                       
001600                                                                          
001700*      CHANGE LOG:                                                        
001800*      YY/MM/DD - NAME            - CHANGE DESCRIPTION                    
001900*      ----------------------------------------------------------         
002000*      15/10/23 - REDDY RAHUL     - RAHL AND PULS.                        
002100*                                   CHECK IF PART EXISTS IN WDK6          
002200*                                   BEFORE SENDING BALANCES.              
002300*                                   E'TRACKER 10263392                    
002400*                                                                         
002500     EJECT                                                                
002600 ENVIRONMENT DIVISION.                                                    
002700     SKIP2                                                                
002800 INPUT-OUTPUT SECTION.                                                    
002900                                                                          
003000 FILE-CONTROL.                                                            
003100     SKIP2                                                                
003200*                            INFILER:                                     
003300                                                                          
003400*                                                                         
003500     SELECT  W48821                   ASSIGN  UT-S-W48822D1.              
003600                                                                          
003700*                                                                         
003800     EJECT                                                                
003900 DATA DIVISION.                                                           
004000     SKIP2                                                                
004100 FILE SECTION.                                                            
004200     SKIP3                                                                
004300 FD  W48821                                                               
004400     RECORDING      F                                                     
004500     BLOCK CONTAINS 0.                                                    
004600     SKIP3                                                                
004700*01  -COPY W488020     -L.                                                
004800     EJECT                                                                
004900 WORKING-STORAGE SECTION.                                                 
005000                                                                          
005100*    -- CHECKED BY WY2000                                                 
005200 77  IDPGM                       PIC X(8)    VALUE 'W4882200'.            
005300     SKIP2                                                                
005400*- - - - - - - - - - - - - -  GENERELLA KONSTANTER                        
005500*                                                                         
005600 77  JA                          PIC X(1)    VALUE 'J'.                   
005700 77  NEJ                         PIC X(1)    VALUE 'N'.                   
005800 77  W48821-EOF                  PIC X(1)    VALUE 'N'.                   
005900     SKIP2                                                                
006000*- - - - - - - - - - - - - -  ARBETSFÄLT                                  
006100 77  W-IDHTYP-4801               PIC X(4)    VALUE '4801'.                
006200 77  W-CHKP-RAEKNARE             PIC S9(3)   VALUE +0    COMP-3.          
006300 77  W-CHKP-MAX                  PIC S9(3)   VALUE +100  COMP-3.          
006400 77  W-ANTAL-POSTER              PIC S9(3)   VALUE +0    COMP-3.          
006500 77  W-ANTAL-FEL-POSTER          PIC S9(5)   VALUE +0    COMP-3.          
006600 77  W-ANTAL-IBM-POSTER          PIC S9(5)   VALUE +0    COMP-3.          
006700     SKIP3                                                                
006800 01  DYNAMISKA-SUBPROGRAM.                                                
006900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007100     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.            
007200     03  W4882210                PIC X(8)    VALUE 'W4882210'.            
007300     EJECT                                                                
007400*- - - - - - - - - - - - - -  PARAMETRAR TILL POSTSUM                     
007500                                                                          
007600*01  -COPY W0005       -PRE POSTSUM-.                                     
007700     EJECT                                                                
007800 01  FILLER                      PIC  X(24)  VALUE                        
007900                                             'IN-AREA-START'.             
008000 01  IN-AREA.                                                             
008100*    03  W488020 -COPY W488020    -PRE IN-.                               
008200     SKIP3                                                                
008300*    03  W488021 -COPY W488021    -RED IN-W488020  -PRE IN-.              
008400     EJECT                                                                
008500*- - - - - - - - - - - - - - PARAMETER-AREOR TILL IMS-SUBPGM              
008600*                                                                         
008700 01  FILLER                      PIC X(24)   VALUE                        
008800                                             '0-AREA-START'.              
008900                                                                          
009000*01  AREA -COPY W488L220   -PRE CALL0-.                                   
009100     EJECT                                                                
009200*                                                                         
009300 01  FILLER                      PIC X(24)   VALUE                        
009400                                             '1-AREA-START'.              
009500*01  AREA -COPY W488L221   -PRE CALL1-.                                   
009600     EJECT                                                                
009700 01  FILLER                      PIC X(24)   VALUE                        
009800                                             '2-AREA-START'.              
009900*                                                                         
010000*01  AREA -COPY W488L222   -PRE CALL2-.                                   
010100     EJECT                                                                
010200 01  FILLER                      PIC X(24)   VALUE                        
010300                                             '3-AREA-START'.              
010400*                                                                         
010500*01  AREA -COPY W488L223   -PRE CALL3-.                                   
010600     EJECT                                                                
010700                                                                          
010800*        ARBETSAREOR TILL IMS-SEKTIONERNA                                 
010900*                                                                         
011000 01  IMS-WS.                                                              
011100     03  FILLER                  PIC X(8)    VALUE 'IMS-WS  '.            
011200     SKIP3                                                                
011300*                            *** STATUSKOD FRÅN IMS                       
011400     03  STATUS-WS               PIC X(2).                                
011500         88  SEGMENT-FINNS                   VALUE '  '.                  
011600         88  SEGMENT-SAKNAS                  VALUE 'GE'.                  
011700     SKIP3                                                                
011800     03  GODK-STATUSKODER.                                                
011900         05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC X(2).          
012000     SKIP3                                                                
012100     03  SSA1                    PIC X(96).                               
012200     EJECT                                                                
012300*01  -COPY W0003                                                          
012400     EJECT                                                                
012500                                                                          
012600 01  FILLER                      PIC X(20)   VALUE 'KEYS TO DLI'.         
012700 01  NYCKLAR-TILL-DLI.                                                    
012800     03  W-IDARTNR-X.                                                     
012900         05  W-IDARTNR           PIC S9(9)   COMP-3.                      
013000                                                                          
013100 01  FILLER                      PIC X(20)   VALUE 'IO AREA'.             
013200 01  DLI-IO-AREA.                                                         
013300     03  IO-AREA                 PIC X(128)  VALUE SPACE.                 
013400     SKIP3                                                                
013500*    03  WDK601 -COPY WDK601                -RED IO-AREA.                 
013600                                                                          
013700 LINKAGE SECTION.                                                         
013800     SKIP2                                                                
013900 01  MSG-PCB                     PIC X(1).                                
014000     SKIP1                                                                
014100 01  ARTD-PCB                    PIC X(1).                                
014200     SKIP1                                                                
014300 01  HTR-PCB                     PIC X(1).                                
014400     SKIP1                                                                
014500*01  -COPY W0008                -PRE WDK6-.                               
014600       05 FILLER                 PIC X.                                   
014700     EJECT                                                                
014800 PROCEDURE DIVISION USING MSG-PCB ARTD-PCB HTR-PCB WDK6-PCB.              
014900     ENTRY 'DLITCBL' USING MSG-PCB ARTD-PCB HTR-PCB WDK6-PCB.             
015000     SKIP2                                                                
015100     MOVE CALL0-RESTART TO CALL0-KDCALL                                   
015200     CALL W4882210 USING CALL0-AREA CALL2-AREA MSG-PCB                    
015300                         ARTD-PCB HTR-PCB                                 
015400                                                                          
015500     PERFORM A-INIT                                                       
015600                                                                          
015700     PERFORM S01-LAS-W48821                                               
015800     PERFORM UNTIL                                                        
015900      NOT ( W48821-EOF = NEJ )                                            
016000       PERFORM B-KONTROLLERA-START-TYP                                    
016100       PERFORM S02-TAG-CHECKPOINT                                         
016200                                                                          
016300       PERFORM UNTIL                                                      
016400        NOT ( W48821-EOF = NEJ AND IN-ARTSUM-IDPTYP-021 =                 
016500          '021' )                                                         
016600         IF W-CHKP-RAEKNARE = W-CHKP-MAX                                  
016700           PERFORM C-UPPDATERA-HTR-ANTAL-POSTER                           
016800           PERFORM S02-TAG-CHECKPOINT                                     
016900         END-IF                                                           
017000         IF IN-ARTSUM-IDFELKOD NOT = SPACE                                
017100           PERFORM S03-UPPDATERA-FELPOST                                  
017200         ELSE                                                             
017300           MOVE IN-ARTSUM-IDARTNR TO W-IDARTNR                            
017400           PERFORM IMS-GU-WDK601                                          
017500           IF SEGMENT-SAKNAS                                              
017600             MOVE '203' TO IN-ARTSUM-IDFELKOD                             
017700             PERFORM S03-UPPDATERA-FELPOST                                
017800           ELSE                                                           
017900             PERFORM D-LAS-SALDO                                          
018000                                                                          
018100             IF CALL0-KDSVAR = CALL0-KDSVAR-OK                            
018200               PERFORM E-RAEKNA-UT-SALDO                                  
018300               IF CALL1-KVBUFF-F < +0 OR CALL1-KVBUFF-OF < +0             
018400                  OR CALL1-KVKOLLI-F < +0                                 
018500                  OR CALL1-KVKOLLI-OF < +0                                
018600                 MOVE '221' TO IN-ARTSUM-IDFELKOD                         
018700                 PERFORM S03-UPPDATERA-FELPOST                            
018800               ELSE                                                       
018900                 PERFORM F-UPPDATERA-SALDO                                
019000               END-IF                                                     
019100             ELSE                                                         
019200               MOVE '222' TO IN-ARTSUM-IDFELKOD                           
019300               PERFORM S03-UPPDATERA-FELPOST                              
019400             END-IF                                                       
019500           END-IF                                                         
019600         END-IF                                                           
019700         PERFORM S01-LAS-W48821                                           
019800                                                                          
019900       END-PERFORM                                                        
020000       PERFORM G-AVSLUTA                                                  
020100     END-PERFORM                                                          
020200     PERFORM Z-FINIT                                                      
020300                                                                          
020400     MOVE ZERO TO RETURN-CODE                                             
020500     GOBACK                                                               
020600     CONTINUE.                                                            
020700     EJECT                                                                
020800 A-INIT SECTION.                                                          
020900     SKIP2                                                                
021000     OPEN INPUT W48821                                                    
021100                                                                          
021200     MOVE SPACE TO IN-AREA                                                
021300                                                                          
021400     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
021500     MOVE 'W41821' TO POSTSUM-FDNAMN                                      
021600     MOVE 'W41822D1' TO POSTSUM-DDNAMN2                                   
021700     CONTINUE.                                                            
021800     EJECT                                                                
021900 B-KONTROLLERA-START-TYP SECTION.                                         
022000     SKIP2                                                                
022100     MOVE W-IDHTYP-4801 TO CALL2-IDHTYP                                   
022200                                                                          
022300     MOVE CALL0-LAS-HTR-TID TO CALL0-KDCALL                               
022400     CALL W4882210 USING CALL0-AREA CALL2-AREA MSG-PCB                    
022500                         ARTD-PCB HTR-PCB                                 
022600                                                                          
022700     IF CALL2-KDTRSTAT = 1 OR 2 OR 3                                      
022800       IF IN-SALDO-IDPTYP-020 = '020'                                     
022900                                                                          
023000         IF CALL2-KDTRSTAT = 1 OR 2                                       
023100           ACCEPT CALL2-TIUPPTID-KLAR FROM TIME                           
023200           MOVE IN-SALDO-KVSALDOPOST-PDP TO CALL2-KVSALDOPOST-PDP         
023300           MOVE ZERO TO CALL2-KVSALDOPOST-FEL                             
023400                                                                          
023500           IF IN-SALDO-KVSALDOPOST-PDP = IN-SALDO-KVSALDOPOST-IBM         
023600             MOVE ZERO TO CALL2-KVSALDOPOST-IBM                           
023700             MOVE 3 TO CALL2-KDTRSTAT                                     
023800           ELSE                                                           
023900             MOVE IN-SALDO-KVSALDOPOST-IBM TO                             
024000             CALL2-KVSALDOPOST-IBM                                        
024100             MOVE 2 TO CALL2-KDTRSTAT                                     
024200             MOVE JA TO W48821-EOF                                        
024300           END-IF                                                         
024400           MOVE CALL0-UPPDATERA-HTR-TID TO CALL0-KDCALL                   
024500           CALL W4882210 USING CALL0-AREA CALL2-AREA MSG-PCB              
024600                               ARTD-PCB HTR-PCB                           
024700         ELSE                                                             
024800           MOVE +0 TO W-ANTAL-POSTER                                      
024900           PERFORM UNTIL                                                  
025000            NOT ( W-ANTAL-POSTER < CALL2-KVSALDOPOST-IBM )                
025100             PERFORM S01-LAS-W48821                                       
025200             ADD +1 TO W-ANTAL-POSTER                                     
025300           END-PERFORM                                                    
025400         END-IF                                                           
025500         PERFORM S01-LAS-W48821                                           
025600       ELSE                                                               
025700         DISPLAY ' FÖRSTA POST EJ 020-POST'                               
025800         CALL FELLOG                                                      
025900       END-IF                                                             
026000     ELSE                                                                 
026100       MOVE JA TO W48821-EOF                                              
026200     END-IF                                                               
026300     CONTINUE.                                                            
026400     EJECT                                                                
026500 C-UPPDATERA-HTR-ANTAL-POSTER SECTION.                                    
026600     SKIP2                                                                
026700     MOVE CALL0-LAS-HTR-TID TO CALL0-KDCALL                               
026800     CALL W4882210 USING CALL0-AREA CALL2-AREA MSG-PCB                    
026900                         ARTD-PCB HTR-PCB                                 
027000                                                                          
027100     ADD W-ANTAL-IBM-POSTER TO CALL2-KVSALDOPOST-IBM                      
027200     ADD W-ANTAL-FEL-POSTER TO CALL2-KVSALDOPOST-FEL                      
027300                               CALL2-KVSALDOPOST-FELTOTAL                 
027400                                                                          
027500     MOVE CALL0-UPPDATERA-HTR-TID TO CALL0-KDCALL                         
027600     CALL W4882210 USING CALL0-AREA CALL2-AREA MSG-PCB                    
027700                         ARTD-PCB HTR-PCB                                 
027800     CONTINUE.                                                            
027900     EJECT                                                                
028000 D-LAS-SALDO SECTION.                                                     
028100     SKIP2                                                                
028200     MOVE IN-ARTSUM-IDARTNR TO CALL1-IDARTNR                              
028300                                                                          
028400     MOVE CALL0-LAS-SALDO TO CALL0-KDCALL                                 
028500     CALL W4882210 USING CALL0-AREA CALL1-AREA MSG-PCB                    
028600                         ARTD-PCB HTR-PCB                                 
028700     CONTINUE.                                                            
028800     EJECT                                                                
028900 E-RAEKNA-UT-SALDO SECTION.                                               
029000     SKIP2                                                                
029100     ADD IN-ARTSUM-KVBUFF-F    TO CALL1-KVBUFF-F                          
029200     ADD IN-ARTSUM-KVBUFF-OF   TO CALL1-KVBUFF-OF                         
029300     ADD IN-ARTSUM-KVKOLLI-F   TO CALL1-KVKOLLI-F                         
029400     ADD IN-ARTSUM-KVKOLLI-OF  TO CALL1-KVKOLLI-OF                        
029500     CONTINUE.                                                            
029600     EJECT                                                                
029700 F-UPPDATERA-SALDO SECTION.                                               
029800     SKIP2                                                                
029900     ADD 1 TO W-CHKP-RAEKNARE                                             
030000              W-ANTAL-IBM-POSTER                                          
030100                                                                          
030200     MOVE CALL0-UPPDATERA-SALDO TO CALL0-KDCALL                           
030300     CALL W4882210 USING CALL0-AREA CALL1-AREA MSG-PCB                    
030400                         ARTD-PCB HTR-PCB                                 
030500     CONTINUE.                                                            
030600     EJECT                                                                
030700 G-AVSLUTA SECTION.                                                       
030800     SKIP2                                                                
030900     MOVE CALL0-LAS-HTR-TID TO CALL0-KDCALL                               
031000     CALL W4882210 USING CALL0-AREA CALL2-AREA MSG-PCB                    
031100                         ARTD-PCB HTR-PCB                                 
031200                                                                          
031300     IF CALL2-KDTRSTAT = 3                                                
031400       ADD W-ANTAL-IBM-POSTER TO CALL2-KVSALDOPOST-IBM                    
031500       ADD W-ANTAL-FEL-POSTER TO CALL2-KVSALDOPOST-FEL                    
031600                                 CALL2-KVSALDOPOST-FELTOTAL               
031700                                                                          
031800       ACCEPT CALL2-TIUPPTID-KLAR FROM TIME                               
031900       MOVE 4 TO CALL2-KDTRSTAT                                           
032000                                                                          
032100       MOVE CALL0-UPPDATERA-HTR-TID TO CALL0-KDCALL                       
032200       CALL W4882210 USING CALL0-AREA CALL2-AREA MSG-PCB                  
032300                           ARTD-PCB HTR-PCB                               
032400     END-IF                                                               
032500     CONTINUE.                                                            
032600     EJECT                                                                
032700 Z-FINIT   SECTION.                                                       
032800     SKIP2                                                                
032900     CLOSE  W48821                                                        
033000     MOVE 'S' TO POSTSUM-OPKOD                                            
033100                                                                          
033200     CALL POSTSUM USING POSTSUM-PARM                                      
033300     CONTINUE.                                                            
033400     EJECT                                                                
033500 S01-LAS-W48821 SECTION.                                                  
033600     SKIP2                                                                
033700     READ W48821 INTO IN-AREA                                             
033800          AT END                                                          
033900          MOVE JA TO W48821-EOF                                           
034000     END-READ                                                             
034100                                                                          
034200     IF W48821-EOF = NEJ                                                  
034300       MOVE IN-SALDO-IDPTYP-020 TO POSTSUM-TRANSTYP                       
034400       MOVE 'W48821' TO POSTSUM-FDNAMN                                    
034500       MOVE 'W48822D1' TO POSTSUM-DDNAMN2                                 
034600                                                                          
034700       CALL POSTSUM USING POSTSUM-PARM                                    
034800     END-IF                                                               
034900     CONTINUE.                                                            
035000     EJECT                                                                
035100 S02-TAG-CHECKPOINT SECTION.                                              
035200     SKIP2                                                                
035300     MOVE CALL0-CHECKPOINT TO CALL0-KDCALL                                
035400     CALL W4882210 USING CALL0-AREA CALL1-AREA MSG-PCB                    
035500                         ARTD-PCB HTR-PCB                                 
035600                                                                          
035700     MOVE +0 TO W-CHKP-RAEKNARE                                           
035800                W-ANTAL-IBM-POSTER                                        
035900                W-ANTAL-FEL-POSTER                                        
036000     CONTINUE.                                                            
036100     EJECT                                                                
036200 S03-UPPDATERA-FELPOST SECTION.                                           
036300     SKIP2                                                                
036400     MOVE IN-ARTSUM-IDLOPNRF TO CALL3-IDLOPNRF                            
036500     MOVE IN-ARTSUM-IDARTNR TO CALL3-IDARTNR                              
036600     MOVE IN-ARTSUM-IDFELKOD TO CALL3-IDFELKOD                            
036700     ACCEPT CALL3-TIREGDAT FROM DATE                                      
036800                                                                          
036900     MOVE CALL0-UPPDATERA-HTR-FEL TO CALL0-KDCALL                         
037000     CALL W4882210 USING CALL0-AREA CALL3-AREA MSG-PCB                    
037100                         ARTD-PCB HTR-PCB                                 
037200                                                                          
037300     ADD 1 TO W-CHKP-RAEKNARE                                             
037400              W-ANTAL-IBM-POSTER                                          
037500              W-ANTAL-FEL-POSTER                                          
037600     CONTINUE.                                                            
037700* IMS SECTIONER                                                           
037800     SKIP3                                                                
037900 IMS-GU-WDK601 SECTION.                                                   
038000     SKIP2                                                                
038100     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
038200            DELIMITED BY SIZE INTO SSA1                                   
038300     MOVE '  GE' TO GODK-STATUSKODER                                      
038400     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-AREA SSA1                      
038500     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
038600     PERFORM IMS-STATUSKONTROLL                                           
038700     CONTINUE.                                                            
038800     SKIP3                                                                
038900 IMS-STATUSKONTROLL SECTION.                                              
039000     SET STATUS-IX TO 1                                                   
039100     SEARCH GODK-STATUS AT END CALL FELLOG                                
039200     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                             
039300     CONTINUE                                                             
039400     END-SEARCH                                                           
039500     CONTINUE.                                                            
