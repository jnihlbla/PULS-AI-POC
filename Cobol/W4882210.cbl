000300 ID DIVISION.                                                             
000400 PROGRAM-ID.              W4882210.                                       
000800*AUTHOR.                  MATS VINNEFORS                                  
000900*DATE-WRITTEN.            MARS 1985.                                      
001100*        PROGRAMMET ÄR ETT SUBPROGRAM TILL W4882200                       
001200*        OCH SKÖTER OM SAMTLIGA IMS-CALL ÅT DETSAMMA                      
001300     SKIP3                                                                
001400 ENVIRONMENT DIVISION.                                                    
001500     SKIP3                                                                
001600 DATA DIVISION.                                                           
001700     EJECT                                                                
001800 WORKING-STORAGE SECTION.                                                 
001801*    -- CHECKED BY WY2000                                                 
001810     SKIP3                                                                
001900 77  IDPGM                       PIC X(8)    VALUE 'W4882210'.            
002600 77  CHKP-ID                     PIC X(8)    VALUE 'W4882210'.            
002700 77  MSG-IO-AREA-LENGTH          PIC S9(9)   VALUE +32  COMP SYNC.        
002800 77  MSG-IO-AREA                 PIC X(32)   VALUE SPACE.                 
002900 77  CHKP-AREA-1-LENGTH          PIC S9(9)   VALUE +32  COMP SYNC.        
003000 77  CHKP-AREA-1                 PIC X(32)   VALUE SPACE.                 
003100     SKIP3                                                                
003200 01  NYCKLAR-TILL-DLI.                                                    
003300     03  W-IDARTNR-X.                                                     
003400         05  W-IDARTNR           PIC S9(9)   COMP-3.                      
003500     03  W-WDD811KY-X.                                                    
003600         05  W-IDDC              PIC  X(2)   VALUE '11'.                  
003700         05  W-ADBUFFOMR         PIC S9(3)   VALUE +1   COMP-3.           
003710         05  W-DABUFPAF          PIC  9(8)   VALUE ZERO.                  
003800         05  W-ADBUFFGANG        PIC S9(3)   VALUE +0   COMP-3.           
003900         05  W-ADBUFFPL          PIC S9(5)   VALUE +0   COMP-3.           
004100*    03  -COPY WDGX01.                                                    
004300     EJECT                                                                
004400*                            *** GENERELLA SUBRUTINER                     
004500 01  SUBPROGRAM.                                                          
004600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
004700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
004800     EJECT                                                                
004900*        ARBETSAREOR TILL IMS-SEKTIONERNA                                 
005000*                                                                         
005100 01  IMS-WS.                                                              
005200     03  FILLER                  PIC X(8)    VALUE 'IMS-WS  '.            
005300     SKIP3                                                                
005400*                            *** STATUSKOD FRÅN IMS                       
005500     03  STATUS-WS               PIC X(2).                                
005600         88  SEGMENT-FINNS                   VALUE '  '.                  
005700         88  SEGMENT-SAKNAS                  VALUE 'GE'.                  
005800         88  SEGMENT-FINNS-REDAN             VALUE 'II'.                  
005900         88  IMS-ANROP-OK                    VALUE '  '.                  
006000         88  IMS-EJ-OK                       VALUE 'XD'.                  
006100     SKIP3                                                                
006200     03  GODK-STATUSKODER.                                                
006300         05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC X(2).          
006400     SKIP3                                                                
006500     03  SSA1                    PIC X(96).                               
006600     03  SSA2                    PIC X(96).                               
006700     EJECT                                                                
006800*01  -COPY W0003                                                          
007000     EJECT                                                                
007100 01  DLI-IO-AREA.                                                         
007200     03  IO-AREA                 PIC X(128)  VALUE SPACE.                 
007300     SKIP3                                                                
007400*    03  WDD801 -COPY WDD801    -PRE ARTD-  -RED IO-AREA.                 
007600     EJECT                                                                
007700*    03  WDD811 -COPY WDD811    -PRE ARTD-  -RED IO-AREA.                 
007900     EJECT                                                                
008000*    03  WLXXDD11 -COPY WDGX4802            -RED IO-AREA.                 
008200     EJECT                                                                
008300*    03  WLXXDD12 -COPY WDGX4804            -RED IO-AREA.                 
008500     EJECT                                                                
008600 LINKAGE SECTION.                                                         
008700     SKIP2                                                                
008800*01  AREA -COPY W488L220        -PRE LINK0-.                              
009000     EJECT                                                                
009100 01  LINK-DATA-AREA.                                                      
009200     03  LINK-AREA               PIC X(64).                               
009300     SKIP3                                                                
009400*    03  AREA -COPY W488L221    -PRE LINK1- -RED LINK-AREA.               
009600     EJECT                                                                
009700*    03  AREA -COPY W488L222    -PRE LINK2- -RED LINK-AREA.               
009900     EJECT                                                                
010000*    03  AREA -COPY W488L223    -PRE LINK3- -RED LINK-AREA.               
010200     EJECT                                                                
010300*01  -COPY W0009                -PRE MSG-.                                
010500     SKIP2                                                                
010600*01  -COPY W0008                -PRE ARTD-.                               
010800      05 FILLER          PIC X.                                           
010900     EJECT                                                                
011000*01  -COPY W0008                -PRE HTR-.                                
011200      05 FILLER          PIC X.                                           
011300     EJECT                                                                
011400 PROCEDURE DIVISION USING LINK0-AREA                                      
011500                          LINK-DATA-AREA                                  
011600                          MSG-PCB                                         
011700                          ARTD-PCB                                        
011800                          HTR-PCB.                                        
011900     SKIP2                                                                
012000     EVALUATE LINK0-KDCALL                                                
012100     WHEN LINK0-LAS-HTR-TID                                               
012200       PERFORM A-LAS-HTR-TID                                              
012300     WHEN LINK0-UPPDATERA-HTR-TID                                         
012400       PERFORM B-UPPDATERA-HTR-TID                                        
012500     WHEN LINK0-UPPDATERA-HTR-FEL                                         
012600       PERFORM C-UPPDATERA-HTR-FEL                                        
012700     WHEN LINK0-LAS-SALDO                                                 
012800       PERFORM D-LAS-SALDO                                                
012900     WHEN LINK0-UPPDATERA-SALDO                                           
013000       PERFORM E-UPPDATERA-SALDO                                          
013100     WHEN LINK0-RESTART                                                   
013200       PERFORM IMS-RESTART                                                
013300     WHEN LINK0-CHECKPOINT                                                
013400       PERFORM IMS-CHECKPOINT                                             
013500     WHEN OTHER                                                           
013600       MOVE LINK0-KDSVAR-FEL TO LINK0-KDSVAR                              
013700     END-EVALUATE                                                         
013800     MOVE ZERO TO RETURN-CODE                                             
013900     GOBACK                                                               
014000     CONTINUE.                                                            
014100     EJECT                                                                
014200 A-LAS-HTR-TID SECTION.                                                   
014300     SKIP2                                                                
014400     MOVE LINK2-IDHTYP TO IDHTYP                                          
014500     MOVE LOW-VALUE TO NYCKEL-VALFRI                                      
014600                                                                          
014700     PERFORM IMS-GET-HTR-TID                                              
014800                                                                          
014900     MOVE 4804-TIUPPTID-KLAR (1) TO LINK2-TIUPPTID-KLAR                   
015000     MOVE 4804-KVSALDOPOST-PDP (1) TO LINK2-KVSALDOPOST-PDP               
015100     MOVE 4804-KVSALDOPOST-IBM (1) TO LINK2-KVSALDOPOST-IBM               
015200     MOVE 4804-KVSALDOPOST-FEL (1) TO LINK2-KVSALDOPOST-FEL               
015300     MOVE 4804-KDTRSTAT (1) TO LINK2-KDTRSTAT                             
015400     MOVE 4804-KVSALDOPOST-FELTOTAL TO LINK2-KVSALDOPOST-FELTOTAL         
015500                                                                          
015600     MOVE LINK0-KDSVAR-OK TO LINK0-KDSVAR                                 
015700     CONTINUE.                                                            
015800     EJECT                                                                
015900 B-UPPDATERA-HTR-TID SECTION.                                             
016000     SKIP2                                                                
016100     MOVE LINK2-TIUPPTID-KLAR TO 4804-TIUPPTID-KLAR (1)                   
016200     MOVE LINK2-KVSALDOPOST-PDP TO 4804-KVSALDOPOST-PDP (1)               
016300     MOVE LINK2-KVSALDOPOST-IBM TO 4804-KVSALDOPOST-IBM (1)               
016400     MOVE LINK2-KVSALDOPOST-FEL TO 4804-KVSALDOPOST-FEL (1)               
016500     MOVE LINK2-KDTRSTAT TO 4804-KDTRSTAT (1)                             
016600     MOVE LINK2-KVSALDOPOST-FELTOTAL TO 4804-KVSALDOPOST-FELTOTAL         
016700                                                                          
016800     PERFORM IMS-REPLACE-HTR                                              
016900                                                                          
017000     MOVE LINK0-KDSVAR-OK TO LINK0-KDSVAR                                 
017100     CONTINUE.                                                            
017200     EJECT                                                                
017300 C-UPPDATERA-HTR-FEL SECTION.                                             
017400     SKIP2                                                                
017500     MOVE SPACE TO IO-AREA                                                
017600                                                                          
017700     MOVE '1' TO 4802-KDSEGKEY                                            
017800     MOVE LINK3-IDLOPNRF TO 4802-IDLOPNRF                                 
017900     MOVE LINK3-IDARTNR TO 4802-IDARTNR                                   
018000     MOVE LINK3-IDFELKOD TO 4802-IDFELKOD                                 
018100     MOVE LINK3-TIREGDAT TO 4802-TIREGDAT                                 
018200                                                                          
018300     PERFORM IMS-INSERT-HTR-FEL                                           
018400     CONTINUE.                                                            
018500     EJECT                                                                
018600 D-LAS-SALDO SECTION.                                                     
018700     SKIP2                                                                
018800     MOVE LINK1-IDARTNR  TO W-IDARTNR                                     
018900                                                                          
019000     PERFORM IMS-LAS-SALDO                                                
019100                                                                          
019200     IF SEGMENT-FINNS                                                     
019300       MOVE ARTD-SALDO-KVBUFF-F    TO LINK1-KVBUFF-F                      
019400       MOVE ARTD-SALDO-KVBUFF-OF   TO LINK1-KVBUFF-OF                     
019500       MOVE ARTD-SALDO-KVKOLLI-F   TO LINK1-KVKOLLI-F                     
019600       MOVE ARTD-SALDO-KVKOLLI-OF  TO LINK1-KVKOLLI-OF                    
019700       MOVE LINK0-KDSVAR-OK TO LINK0-KDSVAR                               
019800     ELSE                                                                 
019900       MOVE LINK0-KDSVAR-FEL TO LINK0-KDSVAR                              
020000     END-IF                                                               
020100     CONTINUE.                                                            
020200     EJECT                                                                
020300 E-UPPDATERA-SALDO SECTION.                                               
020400     SKIP2                                                                
020500     MOVE LINK1-KVBUFF-F      TO ARTD-SALDO-KVBUFF-F                      
020600     MOVE LINK1-KVBUFF-OF     TO ARTD-SALDO-KVBUFF-OF                     
020700     MOVE LINK1-KVKOLLI-F     TO ARTD-SALDO-KVKOLLI-F                     
020800     MOVE LINK1-KVKOLLI-OF    TO ARTD-SALDO-KVKOLLI-OF                    
020900                                                                          
021000     PERFORM IMS-REPLACE-SALDO                                            
021100     CONTINUE.                                                            
021200     EJECT                                                                
021300* IMS SECTIONER                                                           
021400     SKIP3                                                                
021500 IMS-RESTART SECTION.                                                     
021600     SKIP2                                                                
021700     MOVE SPACE TO MSG-IO-AREA                                            
021800     MOVE '  ' TO GODK-STATUSKODER                                        
021900                                                                          
022000     CALL CBLTDLI USING XRST MSG-PCB                                      
022100                        MSG-IO-AREA-LENGTH MSG-IO-AREA                    
022200                        CHKP-AREA-1-LENGTH CHKP-AREA-1                    
022300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
022400     PERFORM IMS-STATUSKONTROLL                                           
022500     CONTINUE.                                                            
022600     SKIP3                                                                
022700 IMS-CHECKPOINT SECTION.                                                  
022800     SKIP2                                                                
022900     MOVE CHKP-ID TO MSG-IO-AREA                                          
023000     MOVE '  XD' TO GODK-STATUSKODER                                      
023100     CALL CBLTDLI USING CHKP MSG-PCB                                      
023200                        MSG-IO-AREA-LENGTH MSG-IO-AREA                    
023300                        CHKP-AREA-1-LENGTH CHKP-AREA-1                    
023400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
023500     PERFORM IMS-STATUSKONTROLL                                           
023600     IF IMS-EJ-OK                                                         
023700       DISPLAY 'IMS-KONTROLLREGION EJ TILLGÄNGLIG'                        
023800       CALL FELLOG                                                        
023900     END-IF                                                               
024000     CONTINUE.                                                            
024100     EJECT                                                                
024200 IMS-GET-HTR-TID SECTION.                                                 
024300     SKIP2                                                                
024400     STRING 'WLXXDD01(WDGXKEY  =' WDGX01 ')'                              
024500            DELIMITED BY SIZE INTO SSA1                                   
024600     STRING 'WLXXDD12(KDSEGKEY =1)'                                       
024700            DELIMITED BY SIZE INTO SSA2                                   
024800     MOVE '  ' TO GODK-STATUSKODER                                        
024900     CALL CBLTDLI USING GHU HTR-PCB DLI-IO-AREA SSA1 SSA2                 
025000     MOVE HTR-STATUS-CODE TO STATUS-WS                                    
025100     PERFORM IMS-STATUSKONTROLL                                           
025200     CONTINUE.                                                            
025300     SKIP3                                                                
025400 IMS-REPLACE-HTR SECTION.                                                 
025500     SKIP2                                                                
025600     MOVE '  ' TO GODK-STATUSKODER                                        
025700     CALL CBLTDLI USING REPL HTR-PCB DLI-IO-AREA                          
025800     MOVE HTR-STATUS-CODE TO STATUS-WS                                    
025900     PERFORM IMS-STATUSKONTROLL                                           
026000     CONTINUE.                                                            
026100     SKIP3                                                                
026200 IMS-INSERT-HTR-FEL SECTION.                                              
026300     SKIP2                                                                
026400     STRING 'WLXXDD01(WDGXKEY  =' WDGX01 ')'                              
026500            DELIMITED BY SIZE INTO SSA1                                   
026600     MOVE 'WLXXDD11*L' TO SSA2                                            
026700     MOVE '  ' TO GODK-STATUSKODER                                        
026800     CALL CBLTDLI USING ISRT HTR-PCB DLI-IO-AREA SSA1 SSA2                
026900     MOVE HTR-STATUS-CODE TO STATUS-WS                                    
027000     PERFORM IMS-STATUSKONTROLL                                           
027100     CONTINUE.                                                            
027200     EJECT                                                                
027300 IMS-LAS-SALDO SECTION.                                                   
027400     SKIP2                                                                
027500     STRING 'WLARTD01(IDARTNR  =' W-IDARTNR-X ')'                         
027600            DELIMITED BY SIZE INTO SSA1                                   
027700     STRING 'WLARTD11(WDD811KY =' W-WDD811KY-X ')'                        
027800            DELIMITED BY SIZE INTO SSA2                                   
027900     MOVE '  GE' TO GODK-STATUSKODER                                      
028000     CALL CBLTDLI USING GHU ARTD-PCB DLI-IO-AREA SSA1 SSA2                
028100     MOVE ARTD-STATUS-CODE TO STATUS-WS                                   
028200     PERFORM IMS-STATUSKONTROLL                                           
028300     CONTINUE.                                                            
028400     SKIP3                                                                
028500 IMS-REPLACE-SALDO SECTION.                                               
028600     SKIP2                                                                
028700     MOVE '  ' TO GODK-STATUSKODER                                        
028800     CALL CBLTDLI USING REPL ARTD-PCB DLI-IO-AREA                         
028900     MOVE ARTD-STATUS-CODE TO STATUS-WS                                   
029000     PERFORM IMS-STATUSKONTROLL                                           
029100     CONTINUE.                                                            
029200     EJECT                                                                
029300 IMS-STATUSKONTROLL SECTION.                                              
029400     SET STATUS-IX TO 1                                                   
029500     SEARCH GODK-STATUS AT END CALL FELLOG                                
029600     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                             
029700     CONTINUE                                                             
029800     END-SEARCH                                                           
029900     CONTINUE                                                             
030000            CONTINUE.                                                     
030100 IMS-EXIT. EXIT.                                                          
030200     CONTINUE.                                                            
