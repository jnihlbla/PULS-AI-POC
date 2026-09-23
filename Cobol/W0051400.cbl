000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W0051400.                                                
000300 AUTHOR.         RICHARD THÖRNGREN.                                       
000400 DATE-WRITTEN.   98/09/22.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        MPP-INIT-SECURITY                                                
000900*        VISAR, LÄGGER UPP, TAR BORT, FÖRÄNDRAR                           
001000*        SECURITY-INFO PÅ MPP-INIT-DATABASEN                              
001100*                                                                         
001200*        PROGRAMMET UPPATERAR WLUSEA (WDP7)                               
001300*                                                                         
001400*    INDATA.                                                              
001500*        TRANSAKTION: W0T514                                              
001600*        MID:         W0I51401                                            
001700*                                                                         
001800*    UTDATA.                                                              
001900*        MOD:         W0O51401                                            
002000                                                                          
002100                                                                          
002200 ENVIRONMENT DIVISION.                                                    
002300                                                                          
002400 DATA DIVISION.                                                           
002500     EJECT                                                                
002600 WORKING-STORAGE SECTION.                                                 
002700*    -- CHECKED BY WY2000                                                 
002800                                                                          
002900 77  IDPGM                       PIC X(08)   VALUE 'W0051400'.            
003000 77  W-COMPILED                  PIC X(16)   VALUE SPACE.                 
003100                                                                          
003200 77  FELTEXT                     PIC X(80)   VALUE SPACE.                 
003300                                                                          
003400 77  JA                          PIC X       VALUE 'J'.                   
003500 77  NEJ                         PIC X       VALUE 'N'.                   
003600                                                                          
003700 77  WS-IDUSER                   PIC X(8)    VALUE SPACE.                 
003800 77  WS-DATE                     PIC 9(6)    VALUE ZERO.                  
003900 77  WS-TIME                     PIC 9(8)    VALUE ZERO.                  
004000                                                                          
004100 77  INDATA-SW                   PIC X       VALUE 'J'.                   
004200     88  INDATA-OK                           VALUE 'J'.                   
004300     88  INDATA-FEL                          VALUE 'N'.                   
004400                                                                          
004500 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
004600     88  EGEN-MID                            VALUE '0511'.                
004700     88  GODK-MID                            VALUE '0511' '0512'          
004800                                                   '0513' '0514'          
004900                                                   '0515' '0516'          
005000                                                   '0517' '0518'          
005100                                                   '0519' '0551'.         
005200                                                                          
005300                                                                          
005400     SKIP3                                                                
005500*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
005600 01  GENERELLA-SUBPROGRAM.                                                
005700     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
005800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006000     EJECT                                                                
006100*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
006200*   -COPY WMEDAREA                                                        
006300     SKIP3                                                                
006400 01  MESSAGE-CODES.                                                       
006500     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
006600     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
006700     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
006800     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
006900     EJECT                                                                
007000*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
007100*                                                                         
007200 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
007300                                                                          
007400*01  MID -COPY W0I51401                                                   
007500     EJECT                                                                
007600 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
007700                                                                          
007800*01  -COPY WMSGAREA                                                       
007900     EJECT                                                                
008000     03  MOD REDEFINES MSG-AREA.                                          
008100*      05  -COPY W0O51401                                                 
008200     EJECT                                                                
008300 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
008400*01  -COPY WMFSAREA                                                       
008500     EJECT                                                                
008600*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
008700                                                                          
008800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008900     SKIP3                                                                
009000 01  NYCKLAR-TILL-DLI.                                                    
009100     03  W-WDP701KY-X.                                                    
009200         05  W-IDUSER            PIC X(8)     VALUE SPACE.                
009300     SKIP2                                                                
009400*    --- STATUS-KOD FRÅN IMS                                              
009500 01  STATUS-WS                   PIC XX.                                  
009600     88  SEGMENT-FINNS                       VALUE '  '.                  
009700     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
009800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
009900     SKIP2                                                                
010000 01  GODK-STATUSKODER.                                                    
010100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
010200     SKIP3                                                                
010300 01  SSA1                        PIC X(64).                               
010400     EJECT                                                                
010500*    --- IMS FUNKTIONSKODER                                               
010600*01  -COPY W0003                                                          
010700     EJECT                                                                
010800*    ---  DLI INPUT-OUTPUT AREA                                           
010900 01  FILLER                      PIC X(16) VALUE 'DLI-IO-USER01'.         
011000                                                                          
011100 01  DLI-IO-USERA01.                                                      
011200*    03  -COPY WDP701                                                     
011300     EJECT                                                                
011400 LINKAGE SECTION.                                                         
011500*01  -COPY W0009      -PRE MSG-                                           
011600                                                                          
011700*01  -COPY W0008      -PRE USEA-                                          
011800     05  FILLER                  PIC X.                                   
011900     EJECT                                                                
012000 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB.                              
012100 MAIN SECTION.                                                            
012200     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB.                              
012300                                                                          
012400     PERFORM IMS-GET-MSG                                                  
012500     IF SEGMENT-FINNS                                                     
012600       PERFORM A-INIT                                                     
012700       PERFORM B-KOLLA-NYCKLAR                                            
012800       IF MFS-UPDATE                                                      
012900         PERFORM G-KOLLA-INPUT                                            
013000         IF INDATA-OK                                                     
013100           PERFORM H-UPPDATERA                                            
013200         END-IF                                                           
013300       ELSE                                                               
013400         PERFORM F-LAES-VISA-INFO                                         
013500       END-IF                                                             
013600       COMPUTE MSG-KVLL = LENGTH OF MOD-W0O51401 + 4                      
013700       PERFORM IMS-INSERT-MSG                                             
013800     END-IF                                                               
013900                                                                          
014000     MOVE ZERO TO RETURN-CODE                                             
014100     GOBACK                                                               
014200     .                                                                    
014300     EJECT                                                                
014400 A-INIT SECTION.                                                          
014500                                                                          
014600     MOVE WHEN-COMPILED TO W-COMPILED                                     
014700     ACCEPT WS-DATE FROM DATE                                             
014800     ACCEPT WS-TIME FROM TIME                                             
014900                                                                          
015000     IF MSG-DUBBLA-TRANSKODER                                             
015100       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W0I51401                 
015200       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
015300       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
015400     ELSE                                                                 
015500       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W0I51401                  
015600       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
015700       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
015800     END-IF                                                               
015900                                                                          
016000     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
016100     MOVE MSG-IDPFK TO MFS-IDPFK                                          
016200     MOVE MFS-IDTRANS TO W-IDTRANS                                        
016300                                                                          
016400     MOVE LOW-VALUE TO MSG-AREA                                           
016500     MOVE 'W0O51401' TO MFS-IDMOD                                         
016600     MOVE '0514' TO MOD-IDTRANS                                           
016700     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
016800                                                                          
016900     IF NOT EGEN-MID                                                      
017000       MOVE SPACE TO MFS-KDTRTYP                                          
017100     END-IF                                                               
017200                                                                          
017300     IF ENGLISH-TEXT                                                      
017400       MOVE 'GB ' TO MED-IDSKYLT                                          
017500     ELSE                                                                 
017600       MOVE 'S  ' TO MED-IDSKYLT                                          
017700     END-IF                                                               
017800     .                                                                    
017900                                                                          
018000     EJECT                                                                
018100 B-KOLLA-NYCKLAR SECTION.                                                 
018200                                                                          
018300     MOVE MFS-RENSA-FAELT TO MOD-IDUSER-IN                                
018400                                                                          
018500     IF MID-IDUSER-IN = ALL '+'                                           
018600       MOVE MID-IDUSER-UT TO WS-IDUSER                                    
018700     ELSE                                                                 
018800       MOVE MID-IDUSER-IN TO WS-IDUSER                                    
018900     END-IF                                                               
019000     IF WS-IDUSER = SPACE                                                 
019100       MOVE MSG-SIGNON-USERID TO WS-IDUSER                                
019200     END-IF                                                               
019300     MOVE WS-IDUSER TO W-IDUSER                                           
019400                                                                          
019500     IF GODK-MID                                                          
019600       MOVE WS-IDUSER TO MOD-IDUSER-UT                                    
019700     ELSE                                                                 
019800       MOVE MFS-RENSA-FAELT TO MOD-IDUSER-UT                              
019900     END-IF                                                               
020000     .                                                                    
020100     EJECT                                                                
020200 F-LAES-VISA-INFO SECTION.                                                
020300                                                                          
020400     PERFORM IMS-GET-USEA-ROT                                             
020500     IF SEGMENT-FINNS                                                     
020600       MOVE INIT-KDARBTYP-SEC       TO MOD-KDARBTYP-SEC                   
020620       MOVE INIT-KDARBTYP-SEC-IDLEV TO MOD-KDARBTYP-SEC-IDLEV             
020630       MOVE INIT-KDARBTYP-SEC-4352  TO MOD-KDARBTYP-SEC-4352              
020700     ELSE                                                                 
020800       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
020900       CALL WMEDKONV USING MED-WMEDAREA                                   
021000       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
021100       PERFORM MFS-RENSA-FAELT-UT                                         
021200     END-IF                                                               
021300     .                                                                    
021400     EJECT                                                                
021500 G-KOLLA-INPUT SECTION.                                                   
021600                                                                          
021610     MOVE JA TO INDATA-SW                                                 
021620     IF MID-KDARBTYP-SEC       = ALL '+' AND                              
021622        MID-KDARBTYP-SEC-4352  = ALL '+' AND                              
021623        MID-KDARBTYP-SEC-IDLEV = ALL '+'                                  
021630       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
021640       CALL WMEDKONV USING MED-WMEDAREA                                   
021650       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
021670       PERFORM MFS-ROER-EJ-FAELT-UT                                       
021680       MOVE NEJ TO INDATA-SW                                              
021690     ELSE                                                                 
021692       MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDARBTYP-SEC-ATTR                 
021693       MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDARBTYP-SEC-IDLEV-ATTR           
021706                                                                          
021707       IF MID-KDARBTYP-SEC-4352 NOT = ALL '+'                             
021708         IF MID-KDARBTYP-SEC-4352 = 'WO' OR 'WS' OR SPACE                 
021709           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDARBTYP-SEC-4352-ATTR        
021710         ELSE                                                             
021711           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDARBTYP-SEC-4352-ATTR          
021712           MOVE NEJ TO INDATA-SW                                          
021713         END-IF                                                           
021714       END-IF                                                             
021715                                                                          
021716       IF INDATA-FEL                                                      
021717         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                        
021718         CALL WMEDKONV USING MED-WMEDAREA                                 
021719         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
021720         PERFORM MFS-ROER-EJ-FAELT-UT                                     
021721       END-IF                                                             
021730     END-IF                                                               
021800     .                                                                    
021900                                                                          
022000     EJECT                                                                
022100 H-UPPDATERA SECTION.                                                     
022200                                                                          
022210     PERFORM MFS-ROER-EJ-FAELT-UT                                         
022220                                                                          
022300     PERFORM IMS-GET-USEA-ROT                                             
022400     IF SEGMENT-FINNS                                                     
022500       PERFORM HA-MOVE-DATA                                               
022600       PERFORM IMS-REPL-USEA                                              
022700     ELSE                                                                 
022800       MOVE NEJ TO INDATA-SW                                              
022900     END-IF                                                               
023000                                                                          
023100     IF INDATA-FEL                                                        
023200       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
023300       CALL WMEDKONV USING MED-WMEDAREA                                   
023400       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
023500     ELSE                                                                 
023600       MOVE INF-UPDATE-DONE TO MED-IDMFSINF                               
023700       CALL WMEDKONV USING MED-WMEDAREA                                   
023800       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
023900     END-IF                                                               
024000     .                                                                    
024100     EJECT                                                                
024200 HA-MOVE-DATA SECTION.                                                    
024300                                                                          
024310     IF MID-KDARBTYP-SEC NOT = ALL '+'                                    
024320       MOVE MID-KDARBTYP-SEC      TO INIT-KDARBTYP-SEC                    
024321                                     MOD-KDARBTYP-SEC                     
024330     END-IF                                                               
024380     IF MID-KDARBTYP-SEC-IDLEV NOT = ALL '+'                              
024390       MOVE MID-KDARBTYP-SEC-IDLEV TO INIT-KDARBTYP-SEC-IDLEV             
024400                                      MOD-KDARBTYP-SEC-IDLEV              
024410     END-IF                                                               
024420     IF MID-KDARBTYP-SEC-4352 NOT = ALL '+'                               
024430       MOVE MID-KDARBTYP-SEC-4352 TO INIT-KDARBTYP-SEC-4352               
024440                                     MOD-KDARBTYP-SEC-4352                
024450     END-IF                                                               
024500     MOVE WS-DATE          TO INIT-TIUPPDAT                               
024600     MOVE WS-TIME          TO INIT-TIUPPTID                               
024700     .                                                                    
024800                                                                          
024900     EJECT                                                                
025000 MFS-RENSA-FAELT-UT SECTION.                                              
025100                                                                          
025200     MOVE MFS-RENSA-FAELT           TO MOD-KDARBTYP-SEC                   
025220                                       MOD-KDARBTYP-SEC-IDLEV             
025230                                       MOD-KDARBTYP-SEC-4352              
025300     .                                                                    
025400                                                                          
025500     EJECT                                                                
025600 MFS-ROER-EJ-FAELT-UT SECTION.                                            
025700                                                                          
025800     MOVE MFS-ROER-EJ-FAELT         TO MOD-KDARBTYP-SEC                   
025820                                       MOD-KDARBTYP-SEC-IDLEV             
025900                                       MOD-KDARBTYP-SEC-4352              
026000     .                                                                    
026100     EJECT                                                                
026200* --- IMS SEKTIONER ---                                                   
026300                                                                          
026400 IMS-GET-MSG SECTION.                                                     
026500                                                                          
026600     MOVE '  QC' TO GODK-STATUSKODER                                      
026700     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
026800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
026900     PERFORM IMS-STATUSKONTROLL                                           
027000     .                                                                    
027100                                                                          
027200                                                                          
027300 IMS-INSERT-MSG SECTION.                                                  
027400                                                                          
027500     IF INIT-IDLAND-SPR = 'GB'                                            
027600       MOVE '0' TO MFS-KDHUVOMR                                           
027700     END-IF                                                               
027800     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
027900     MOVE SPACE TO GODK-STATUSKODER                                       
028000     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
028100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
028200     PERFORM IMS-STATUSKONTROLL                                           
028300     .                                                                    
028400                                                                          
028500     EJECT                                                                
028600 IMS-GET-USEA-ROT SECTION.                                                
028700     STRING 'WLUSEA01(IDUSER   =' W-WDP701KY-X ')'                        
028800          DELIMITED BY SIZE INTO SSA1                                     
028900     MOVE '  GE' TO GODK-STATUSKODER                                      
029000     CALL CBLTDLI USING GHU USEA-PCB DLI-IO-USERA01 SSA1                  
029100     MOVE USEA-STATUS-CODE TO STATUS-WS                                   
029200     PERFORM IMS-STATUSKONTROLL                                           
029300     .                                                                    
029400                                                                          
029500                                                                          
029600 IMS-REPL-USEA SECTION.                                                   
029700                                                                          
029800     MOVE '  ' TO GODK-STATUSKODER                                        
029900     CALL CBLTDLI USING REPL USEA-PCB DLI-IO-USERA01                      
030000     MOVE USEA-STATUS-CODE TO STATUS-WS                                   
030100     PERFORM IMS-STATUSKONTROLL                                           
030200     .                                                                    
030300                                                                          
030400     EJECT                                                                
030500 IMS-STATUSKONTROLL SECTION.                                              
030600                                                                          
030700     SET STATUS-IX TO 1                                                   
030800     SEARCH GODK-STATUS                                                   
030900       AT END                                                             
031000         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
031100         DELIMITED BY SIZE INTO FELTEXT                                   
031200         CALL FELLOG                                                      
031300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
031400         CONTINUE                                                         
031500     END-SEARCH                                                           
031600     .                                                                    
