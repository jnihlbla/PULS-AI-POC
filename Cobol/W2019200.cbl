001300 ID DIVISION.                                                             
001400 PROGRAM-ID.     W2019200.                                                
001500 AUTHOR.         MÅNS SAMUELSSON.                                         
001600 DATE-WRITTEN.   98/04/22.                                                
001700 DATE-COMPILED.                                                           
001800                                                                          
001900*    FUNKTION:                                                            
002000*        START AV REFILL BMP'R                                            
002100*        STARTAS AV W00507 PÅ KLOCKSLAG                                   
002200*                                                                         
002400*                                                                         
002500*    INDATA.                                                              
002600*        TRANSAKTION: W2T192                                              
002700*        MID:         W2I19201                                            
002800*                                                                         
002900*    UTDATA.                                                              
003000*        MOD:         W0O60601                                            
003100                                                                          
003200     SKIP3                                                                
003300 ENVIRONMENT DIVISION.                                                    
003400     EJECT                                                                
003500 DATA DIVISION.                                                           
003600 WORKING-STORAGE SECTION.                                                 
003601                                                                          
003610*    -- CHECKED BY WY2000                                                 
003700 77  IDPGM                       PIC X(08)   VALUE 'W2019200'.            
003800                                                                          
003900*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004000 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004100                                                                          
004200 77  JA                          PIC X       VALUE 'J'.                   
004300 77  NEJ                         PIC X       VALUE 'N'.                   
004310                                                                          
004320 01  SUBPROGRAM.                                                          
004330     03 CBLTDLI                  PIC X(8)    VALUE 'CBLTDLI '.            
004340     03 FELLOG                   PIC X(8)    VALUE 'FELLOG  '.            
004400                                                                          
009000 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
009100     SKIP3                                                                
009200*01  MID -COPY W2I19201                                                   
009300     EJECT                                                                
009301*01    -COPY WMSGAREA                                                     
009302     EJECT                                                                
009310*01    -COPY WMFSAREA                                                     
009320     EJECT                                                                
009400*- - - - - - - - - - - -   - - - PARAMETRAR TILL SOP                      
009500 01  W-PROG-TO-PROG-SW.                                                   
009600*03  -COPY WMSGSOP                                                        
009700     EJECT                                                                
010500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
010600*                                                                         
010700     EJECT                                                                
010800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010900     SKIP3                                                                
011300*    --- STATUS-KOD FRÅN IMS                                              
011400 01  STATUS-WS                   PIC XX.                                  
011500     88  SEGMENT-FINNS                       VALUE '  '.                  
011600     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
011700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
011800     SKIP2                                                                
011900 01  GODK-STATUSKODER.                                                    
012000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012100     SKIP3                                                                
012200 01  SSA1                        PIC X(64).                               
012300 01  SSA2                        PIC X(64).                               
012400     EJECT                                                                
012500*    --- IMS FUNKTIONSKODER                                               
012600*01  -COPY W0003                                                          
012800     EJECT                                                                
012900*    ---  DLI INPUT-OUTPUT AREA                                           
013000                                                                          
013400     EJECT                                                                
013500 LINKAGE SECTION.                                                         
013600*01  -COPY W0009   -PRE MSG-                                              
013700*01  -COPY W0009   -PRE 0606-                                             
014000     EJECT                                                                
014101 PROCEDURE DIVISION  USING MSG-PCB 0606-PCB.                              
014102 MAIN SECTION.                                                            
014110     ENTRY 'DLITCBL' USING MSG-PCB 0606-PCB.                              
014200                                                                          
014400     PERFORM IMS-GET-MSG                                                  
014500     IF SEGMENT-FINNS                                                     
014600       PERFORM A-INIT                                                     
014700       PERFORM B-BESTAELL-RUTIN                                           
014800     END-IF                                                               
016200     MOVE ZERO TO RETURN-CODE                                             
016300     GOBACK                                                               
016400     .                                                                    
016500     EJECT                                                                
016600 A-INIT SECTION.                                                          
016700                                                                          
016800     IF MSG-DUBBLA-TRANSKODER                                             
016900       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W2I19201                 
017000       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
017100       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
017200     ELSE                                                                 
017300       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W2I19201                  
017400       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
017500       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
017600     END-IF                                                               
017700                                                                          
017800     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
017900     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
018100                                                                          
019500     .                                                                    
019600     EJECT                                                                
019700 B-BESTAELL-RUTIN SECTION.                                                
019701                                                                          
019740                                                                          
019750     MOVE '2192' TO MSGSOP-IDTRANS                                        
019760     MOVE MFS-KDMFSFOR TO MSGSOP-KDMFSFOR                                 
019770     MOVE 'W271B4' TO MSGSOP-IDPROCESS                                    
019780     MOVE 'O' TO MSGSOP-KDSOPFUNK                                         
019790     PERFORM IMS-INSERT-ALT                                               
019900     .                                                                    
020000     EJECT                                                                
020010 IMS-GET-MSG SECTION.                                                     
020020     SKIP2                                                                
020030     MOVE SPACE TO MSG-IO-AREA                                            
020040     MOVE '  QC' TO GODK-STATUSKODER                                      
020050     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
020060     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
020070     PERFORM IMS-STATUSKONTROLL                                           
020080     .                                                                    
020090     EJECT                                                                
020100 IMS-INSERT-ALT SECTION.                                                  
020200                                                                          
020300     MOVE SPACE TO GODK-STATUSKODER                                       
020400     CALL CBLTDLI USING ISRT 0606-PCB W-PROG-TO-PROG-SW                   
020500     MOVE 0606-STATUS-CODE TO STATUS-WS                                   
020600     PERFORM IMS-STATUSKONTROLL                                           
020700     .                                                                    
020800     SKIP3                                                                
031800 IMS-STATUSKONTROLL SECTION.                                              
031900                                                                          
032000     SET STATUS-IX TO 1                                                   
032100     SEARCH GODK-STATUS                                                   
032200       AT END                                                             
032300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
032400         DELIMITED BY SIZE INTO FELTEXT                                   
032500         CALL FELLOG                                                      
032600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
032700         CONTINUE                                                         
032800     END-SEARCH                                                           
032900     .                                                                    
