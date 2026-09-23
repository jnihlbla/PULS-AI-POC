000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4041500.                                                
000300 AUTHOR.         THOMAS LARSSON.                                          
000400 DATE-WRITTEN.   96/03/29.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        UPPDATERING AV LAGER ( DC ) INFORMATION.                         
000900*                                                                         
001000*        PROGRAMMET UPPDATERAR WLGMTB (WDB3)                              
001100*        PROGRAMMET LÄSER      WLGMTC (WDB5)                              
001200*        PROGRAMMET LÄSER      WLGMTA (WDB2)                              
001300*        PROGRAMMET LÄSER      WDB6                                       
001400*                                                                         
001500*    INDATA.                                                              
001600*        TRANSAKTION: W4T415                                              
001700*        MID:         W4I41501                                            
001800*                                                                         
001900*    UTDATA.                                                              
002000*        MOD:         W4O41501                                            
002100                                                                          
002200     SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400     EJECT                                                                
002500 DATA DIVISION.                                                           
002600 WORKING-STORAGE SECTION.                                                 
002700*    -COPY WY2000W1                                                       
002800     SKIP3                                                                
002900 77  IDPGM                       PIC X(08)   VALUE 'W4041500'.            
003000                                                                          
003100*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003200 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003300                                                                          
003400 77  INDX                        PIC S9(4)   VALUE +0  COMP SYNC.         
003500 77  MAX-INDX                    PIC S9(4)   VALUE +11 COMP SYNC.         
003600 77  JA                          PIC X       VALUE 'J'.                   
003700 77  NEJ                         PIC X       VALUE 'N'.                   
003800 77  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
003900                                                                          
004000 77  WS-KVLEDTIM-0               PIC S9(3)V9(2)  VALUE ZERO.              
004100 77  WS-KVLEDTIM-1               PIC S9(3)V9(2)  VALUE ZERO.              
004200 77  WS-KVLEDTIM-2               PIC S9(3)V9(2)  VALUE ZERO.              
004300 77  WS-KVLEDTIM-3               PIC S9(3)V9(2)  VALUE ZERO.              
004400 77  WS-KVLEDTIM-4               PIC S9(3)V9(2)  VALUE ZERO.              
004500                                                                          
004600 77  WS-KVLEDTIM-0-FIX           PIC 9(2)9.9(2) VALUE ZERO.               
004700 77  WS-KVLEDTIM-1-FIX           PIC 9(2)9.9(2) VALUE ZERO.               
004800 77  WS-KVLEDTIM-2-FIX           PIC 9(2)9.9(2) VALUE ZERO.               
004900 77  WS-KVLEDTIM-3-FIX           PIC 9(2)9.9(2) VALUE ZERO.               
005000 77  WS-KVLEDTIM-4-FIX           PIC 9(2)9.9(2) VALUE ZERO.               
005100                                                                          
005200 77  WS-KDGENFRA-VOR             PIC 9(2)    VALUE ZERO.                  
005300 77  WS-KDGENFRA-DO              PIC 9(2)    VALUE ZERO.                  
005400 77  WS-KDGENFRA-MO              PIC 9(2)    VALUE ZERO.                  
005500                                                                          
005600 77  WS-IDDISTR                  PIC S9(5)   VALUE ZERO COMP-3.           
005700 77  WS-IDKUNDNR                 PIC S9(7)   VALUE ZERO COMP-3.           
005800 77  WS-FLSAMFAK                 PIC X(1)    VALUE 'N'.                   
005900                                                                          
006000 77  WS-KDFORSKN                 PIC S9(3)   VALUE ZERO COMP-3.           
006100 77  WS-KDMOMSIN                 PIC S9      VALUE ZERO COMP-3.           
006200 77  WS-KDSPFKTK                 PIC S9(3)   VALUE ZERO COMP-3.           
006300 77  WS-KDTULLVE                 PIC S9      VALUE ZERO COMP-3.           
006400                                                                          
006500 77  SPAR-KDFORSKN               PIC X(3)    VALUE SPACE.                 
006600 77  SPAR-KDMOMSIN               PIC X(1)    VALUE SPACE.                 
006700 77  SPAR-KDSPFKTK               PIC X(1)    VALUE SPACE.                 
006800 77  SPAR-KDTULLVE               PIC X(1)    VALUE SPACE.                 
006900                                                                          
007000 77  W-DEF                       PIC 9(3).                                
007100                                                                          
007200 01  SPAR-KDSPFKTK-X.                                                     
007300     03  SPAR-KDSPFKTK-N         PIC 9(3).                                
007400                                                                          
007500 01  KDROPACK-TYP                PIC X(1).                                
007600     88  GODK-KDROPACK           VALUE '0' '1' '2' '3' '4'                
007700                                       '5' '6' '7' '8'                    
007800                                       'A' 'B' 'C' 'D' 'E'                
007900                                       'F' 'G' 'H' 'I' 'J'                
008000                                       'K' 'N'.                           
008100                                                                          
008200 01  WS-KVLEDTIM                 PIC 9(3)V9(2).                           
008300 01  W-KVLEDTIM REDEFINES WS-KVLEDTIM.                                    
008400     03  W-KVLEDTIM-HHH          PIC 9(3).                                
008500     03  W-KVLEDTIM-MM           PIC 9(2).                                
008600                                                                          
008700*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
008800                                                                          
008900 77  GAELLANDE-SW                PIC X       VALUE 'J'.                   
009000     88  GAELLANDE-KUND                      VALUE 'J'.                   
009100     88  STOPPAD-KUND                        VALUE 'N'.                   
009200                                                                          
009300 77  INDATA-SW                   PIC X       VALUE 'J'.                   
009400     88  INDATA-OK                           VALUE 'J'.                   
009500     88  INDATA-FEL                          VALUE 'N'.                   
009600                                                                          
009700 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
009800     88  NYCKLAR-OK                          VALUE 'J'.                   
009900     88  NYCKLAR-FEL                         VALUE 'N'.                   
010000                                                                          
010100       EJECT                                                              
010200 01  TEST-IDDISTR                PIC S9(5)   COMP-3.                      
010300                                                                          
010400 01  FILLER REDEFINES TEST-IDDISTR.                                       
010500*    03 -COPY WWDIST40                                                    
010600     EJECT                                                                
010700 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
010800     88  EGEN-MID                            VALUE '4415'.                
010900     88  GODK-MID                            VALUE '4411' '4412'          
011000                                                   '4413' '4414'          
011100                                                   '4415' '4416'          
011200                                                   '4417' '4418'          
011300                                                   '4419'.                
011400                                                                          
011500     88  NYCKEL-MID                          VALUE '4417' '4418'.         
011600     88  HELP-MID                            VALUE '0551'.                
011700     EJECT                                                                
011800*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
011900 01  GENERELLA-SUBPROGRAM.                                                
012000     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
012100     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
012200     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
012300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
012400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
012500     EJECT                                                                
012600*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
012700 01  FILLER                      PIC X(16)   VALUE 'WMEDAREA'.            
012800*01 -COPY WMEDAREA                                                        
012900     SKIP3                                                                
013000 01  MESSAGE-CODES.                                                       
013100     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
013200     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
013300     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
013400     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
013500     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
013600     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
013700     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
013800     EJECT                                                                
013900*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
014000*                                                                         
014100 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
014200     SKIP3                                                                
014300*01 -COPY WMSGINIT                                                        
014400     EJECT                                                                
014500 01  FILLER                      PIC X(16)   VALUE 'WDECEDIT'.            
014600     SKIP3                                                                
014700*01 -COPY WDECAREA                                                        
014800     SKIP3                                                                
014900*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
015000*                                                                         
015100 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
015200     SKIP3                                                                
015300*01  MID -COPY W4I41501                                                   
015400     EJECT                                                                
015500 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
015600     SKIP3                                                                
015700*01  -COPY WMSGAREA                                                       
015800     EJECT                                                                
015900     03  MOD REDEFINES MSG-AREA.                                          
016000*      05  -COPY W4O41501                                                 
016100     EJECT                                                                
016200 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
016300     SKIP3                                                                
016400*01  -COPY WMFSAREA                                                       
016500     EJECT                                                                
016600*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
016700*                                                                         
016800     SKIP2                                                                
016900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
017000     SKIP3                                                                
017100 01  SPAR-AREA.                                                           
017200     03  SPAR-IDTRANS            PIC X(4)    VALUE SPACE.                 
017300     03  SPAR-WDB301KY-ENTER     PIC X(9)    VALUE SPACE.                 
017400     03  SPAR-WDB301KY-NEXT      PIC X(9)    VALUE SPACE.                 
017500     SKIP3                                                                
017600 01  NYCKLAR-TILL-BLAEDDRING.                                             
017700     03  W-MINKEY-IDDC           PIC X(2)    VALUE SPACE.                 
017800     03  W-MINKEY-IDDISTR        PIC S9(5)   VALUE ZERO COMP-3.           
017900     03  W-MINKEY-IDKUNDNR       PIC S9(7)   VALUE ZERO COMP-3.           
018000                                                                          
018100     SKIP3                                                                
018200 01  NYCKLAR-TILL-DLI.                                                    
018300     03  W-WDB301KY-X.                                                    
018400         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
018500         05  W-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.           
018600         05  W-IDKUNDNR          PIC S9(7)   VALUE ZERO COMP-3.           
018700     SKIP2                                                                
018800     03  W-WDB301KY-DEF-X.                                                
018900         05  W-IDDC-DEF          PIC X(2)    VALUE SPACE.                 
019000         05  W-IDDISTR-DEF       PIC S9(5)   VALUE ZERO COMP-3.           
019100         05  W-IDKUNDNR-DEF      PIC S9(7) VALUE +9999999 COMP-3.         
019200     SKIP2                                                                
019300     03  W-WDB301KY-MIN-X.                                                
019400         05  W-IDDC-MIN          PIC X(2)    VALUE SPACE.                 
019500         05  W-IDDISTR-MIN       PIC S9(5)   VALUE ZERO COMP-3.           
019600         05  W-IDKUNDNR-MIN      PIC S9(7)   VALUE ZERO COMP-3.           
019700     SKIP2                                                                
019800     03  W-WDB301KY-MAX-X.                                                
019900         05  W-IDDC-MAX          PIC X(2)    VALUE SPACE.                 
020000         05  W-IDDISTR-MAX       PIC S9(5)   VALUE ZERO COMP-3.           
020100         05  W-IDKUNDNR-MAX      PIC S9(7) COMP-3 VALUE +9999999.         
020200     SKIP2                                                                
020300     03  W-WDB301KY-COPY-X.                                               
020400         05  W-IDDC-COPY         PIC X(2)    VALUE SPACE.                 
020500         05  W-IDDISTR-COPY      PIC S9(5)   VALUE ZERO COMP-3.           
020600         05  W-IDKUNDNR-COPY     PIC S9(7)   VALUE ZERO COMP-3.           
020700     SKIP2                                                                
020800     03  W-IDGMT-X.                                                       
020900         05  W-IDDISTR-WDB2      PIC S9(5)   VALUE ZERO COMP-3.           
021000         05  W-IDKUNDNR-WDB2     PIC S9(7)   VALUE ZERO COMP-3.           
021100     SKIP2                                                                
021200     03  W-IDGMT-MIN-X.                                                   
021300         05  W-IDDISTR-WDB2-MIN  PIC S9(5)   VALUE ZERO COMP-3.           
021400         05  W-IDKUNDNR-WDB2-MIN PIC S9(7)   VALUE ZERO COMP-3.           
021500     SKIP2                                                                
021600     03  W-IDGMT-MAX-X.                                                   
021700         05  W-IDDISTR-WDB2-MAX  PIC S9(5)   VALUE ZERO COMP-3.           
021800         05  W-IDKUNDNR-WDB2-MAX PIC S9(7)  VALUE +9999999 COMP-3.        
021900     SKIP2                                                                
022000                                                                          
022100     03  W-WDB501KY-X.                                                    
022200         05  W-IDDC-WDB5         PIC X(2)    VALUE SPACE.                 
022300         05  W-KDFRAKT-WDB5      PIC S9(3)   VALUE ZERO COMP-3.           
022400         05  W-IDDISTR-WDB5      PIC S9(5)   VALUE ZERO COMP-3.           
022500         05  W-IDKUNDNR-WDB5     PIC S9(7) VALUE +9999999 COMP-3.         
022600     SKIP2                                                                
022700     03  W-WDGXKEY-4443-X.                                                
022800         05  W-IDHTYP-4443       PIC X(4)    VALUE '4443'.                
022900         05  W-IDDC-4443         PIC X(2)    VALUE SPACE.                 
023000         05  W-IDPKLTAB          PIC X(2)    VALUE SPACE.                 
023100         05  FILLER              PIC X(22)   VALUE LOW-VALUE.             
023200                                                                          
023300     SKIP2                                                                
023400     03  W-WDGXKEY-4445-X.                                                
023500         05  W-IDHTYP-4445       PIC X(4)    VALUE '4445'.                
023600         05  W-IDDC-4445         PIC X(2)    VALUE SPACE.                 
023700         05  W-IDPRCTAB          PIC 9(2)    VALUE ZERO.                  
023800         05  FILLER              PIC X(22)   VALUE LOW-VALUE.             
023900                                                                          
024000     03  W-IDDC-B6-X.                                                     
024100         05 W-IDDC-B6                  PIC X(2).                          
024200                                                                          
024300     SKIP2                                                                
024400*    --- STATUS-KOD FRÅN IMS                                              
024500 01  STATUS-WS                   PIC XX.                                  
024600     88  SEGMENT-FINNS                       VALUE '  '.                  
024700     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
024800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
024900     88  BAS-SLUT                            VALUE 'GB'.                  
025000     SKIP2                                                                
025100 01  GODK-STATUSKODER.                                                    
025200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
025300     SKIP3                                                                
025400 01  SSA1                        PIC X(64).                               
025500 01  SSA2                        PIC X(64).                               
025600     EJECT                                                                
025700*    --- IMS FUNKTIONSKODER                                               
025800*01  -COPY W0003                                                          
025900     EJECT                                                                
026000*    ---  DLI INPUT-OUTPUT AREA                                           
026100 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
026200     SKIP3                                                                
026300 01  FILLER                      PIC X(16)   VALUE 'WDB301-AREA'.         
026400 01  DLI-IO-AREA.                                                         
026500     03  IO-AREA                 PIC X(300)  VALUE SPACE.                 
026600     SKIP3                                                                
026700     03  WLGMTB01 REDEFINES IO-AREA.                                      
026800*        05  -COPY WDB301  -PRE GMTB-                                     
026900     EJECT                                                                
027000                                                                          
027100 01  FILLER                      PIC X(16)   VALUE                        
027200                                             'COPY-WDB301-AREA'.          
027300 01  DLI-IO-AREA-1.                                                       
027400     03  IO-AREA-1               PIC X(300)  VALUE SPACE.                 
027500     SKIP3                                                                
027600     03  WLGMTB01 REDEFINES IO-AREA-1.                                    
027700*        05  -COPY WDB301  -PRE COPY-                                     
027800     EJECT                                                                
027900                                                                          
028000 01  FILLER                      PIC X(16)   VALUE                        
028100                                             'COPY-WDB501-AREA'.          
028200 01  DLI-IO-AREA-2.                                                       
028300     03  IO-AREA-2               PIC X(300)  VALUE SPACE.                 
028400     SKIP3                                                                
028500     03  WLGMTC01 REDEFINES IO-AREA-2.                                    
028600*        05  -COPY WDB501  -PRE GMTC-                                     
028700     EJECT                                                                
028800 01  FILLER                      PIC X(16)   VALUE                        
028900                                             'COPY-WDB201-AREA'.          
029000 01  DLI-IO-AREA-3.                                                       
029100     03  WLGMTA01.                                                        
029200*        05  -COPY WDB201  -PRE GMTA-                                     
029300     EJECT                                                                
029400                                                                          
029500 01  DLI-IO-AREA-4.                                                       
029600     03  IO-AREA-4               PIC X(300)  VALUE SPACE.                 
029700     SKIP3                                                                
029800     03  WLGMTB01 REDEFINES IO-AREA-4.                                    
029900*        05  -COPY WDB301  -PRE NEXT-                                     
030000     EJECT                                                                
030100                                                                          
030200 01  DLI-IO-AREA-9.                                                       
030300     03  IO-AREA-9               PIC X(300)  VALUE SPACE.                 
030400     SKIP3                                                                
030500     03  WLGMTB01 REDEFINES IO-AREA-9.                                    
030600*        05  -COPY WDB301  -PRE DEF-                                      
030700                                                                          
030800 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
030900 01   DLI-IO-AREA-B601.                                                   
031000*     03  -COPY WDB601                                                    
031100                                                                          
031200     EJECT                                                                
031300                                                                          
031400 LINKAGE SECTION.                                                         
031500*01  -COPY W0009   -PRE MSG-                                              
031600*01  -COPY W0008   -PRE USEA-                                             
031700     05  FILLER                  PIC X.                                   
031800     EJECT                                                                
031900*01  -COPY W0008  -PRE GMTB-                                              
032000     05  FILLER                  PIC X.                                   
032100     EJECT                                                                
032200*01  -COPY W0008  -PRE GMTBU-                                             
032300     05  FILLER                  PIC X.                                   
032400     EJECT                                                                
032500*01  -COPY W0008  -PRE GMTC-                                              
032600     05  FILLER                  PIC X.                                   
032700     EJECT                                                                
032800*01  -COPY W0008  -PRE GMTA-                                              
032900     05  FILLER                  PIC X.                                   
033000     EJECT                                                                
033100*01  -COPY W0008  -PRE XXKG-                                              
033200     05  FILLER                  PIC X.                                   
033300     EJECT                                                                
033400*01  -COPY W0008  -PRE XXKF-                                              
033500     05  FILLER                  PIC X.                                   
033600     EJECT                                                                
033700*01  -COPY W0008  -PRE WDB6-                                              
033800     05  FILLER                  PIC X.                                   
033900     EJECT                                                                
034000 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB GMTB-PCB GMTBU-PCB            
034100                           GMTC-PCB GMTA-PCB XXKG-PCB XXKF-PCB            
034200                           WDB6-PCB.                                      
034300 MAIN SECTION.                                                            
034400     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB GMTB-PCB GMTBU-PCB            
034500                           GMTC-PCB GMTA-PCB XXKG-PCB XXKF-PCB            
034600                           WDB6-PCB.                                      
034700                                                                          
034800     PERFORM IMS-GET-MSG                                                  
034900     IF SEGMENT-FINNS                                                     
035000       PERFORM A-INIT                                                     
035100       PERFORM B-KOLLA-NYCKLAR                                            
035200       IF NYCKLAR-OK                                                      
035300         IF MFS-UPDATE                                                    
035400           PERFORM G-KOLLA-INPUT                                          
035500           IF INDATA-OK                                                   
035600             PERFORM H-UPPDATERA                                          
035700           END-IF                                                         
035800           PERFORM S10-SAMMA-SIDA-EFTER-UPPDAT                            
035900         ELSE                                                             
036000           IF MFS-FIRST                                                   
036100             PERFORM C-FOERSTA-SIDA                                       
036200           ELSE                                                           
036300             IF MFS-NEXT                                                  
036400               PERFORM D-NAESTA-SIDA                                      
036500             ELSE                                                         
036600               PERFORM E-SAMMA-SIDA                                       
036700             END-IF                                                       
036800           END-IF                                                         
036900         END-IF                                                           
037000         PERFORM F-LAES-VISA-INFO                                         
037100       END-IF                                                             
037200*    --- IF ANSWER TO SCREEN:       MSG-KVLL = MOD-LENGTH + 4             
037300*    --- IF PROGRAM-TO-PROGRAM-SWITCH:       = MOD-LENGTH + 17            
037400       COMPUTE MSG-KVLL = LENGTH OF MOD-W4O41501 + 4                      
037500       PERFORM IMS-INSERT-MSG                                             
037600     END-IF                                                               
037700                                                                          
037800     MOVE ZERO TO RETURN-CODE                                             
037900     GOBACK                                                               
038000     .                                                                    
038100     EJECT                                                                
038200 A-INIT SECTION.                                                          
038300                                                                          
038400     IF MSG-DUBBLA-TRANSKODER                                             
038500       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I41501                 
038600       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
038700       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
038800     ELSE                                                                 
038900       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I41501                  
039000       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
039100       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
039200     END-IF                                                               
039300                                                                          
039400     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
039500     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
039600     MOVE MFS-IDTRANS TO W-IDTRANS                                        
039700                                                                          
039800     MOVE LOW-VALUE TO MSG-AREA                                           
039900     MOVE 'W4O415N1' TO MFS-IDMOD                                         
040000     MOVE '4415' TO MOD-IDTRANS                                           
040100     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
040200                                                                          
040300     IF EGEN-MID OR HELP-MID                                              
040400       CONTINUE                                                           
040500     ELSE                                                                 
040600       MOVE SPACE TO MFS-KDTRTYP                                          
040700       MOVE '7' TO MFS-IDPFK                                              
040800     END-IF                                                               
040900                                                                          
041000     ACCEPT DAGENS-DATUM FROM DATE                                        
041100     .                                                                    
041200     EJECT                                                                
041300 B-KOLLA-NYCKLAR SECTION.                                                 
041400                                                                          
041500     MOVE ALL '+'           TO MSGI-WMSGINIT                              
041600     MOVE '001'             TO MSGI-KDCALL                                
041700     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
041800     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
041900     MOVE '4415'            TO MSGI-IDTRANS                               
042000     IF EGEN-MID                                                          
042100       MOVE MID-IDDISTR-IN       TO MSGI-IDDISTR                          
042200       MOVE MID-IDKUNDNR-IN      TO MSGI-IDKUNDNR                         
042300     END-IF                                                               
042400                                                                          
042500     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
042600     MOVE MSGI-IDLAND-SPR   TO MED-IDSKYLT                                
042700     MOVE MSGI-SPAR-AREA    TO SPAR-AREA                                  
042800     MOVE JA TO NYCKLAR-SW                                                
042900                                                                          
043000*    -- KONTROLL AV IDDC                                                  
043100     MOVE MFS-RENSA-FAELT TO MOD-IDDC-IN                                  
043200                                                                          
043300     IF MID-IDDC-IN  = ALL '+'                                            
043400       IF EGEN-MID OR NYCKEL-MID                                          
043500         MOVE MID-IDDC-UT TO W-IDDC-B6                                    
043600       ELSE                                                               
043700         MOVE MSGI-IDDC TO W-IDDC-B6                                      
043800       END-IF                                                             
043900     ELSE                                                                 
044000       MOVE MID-IDDC-IN TO W-IDDC-B6                                      
044100       MOVE '7'         TO MFS-IDPFK                                      
044200       MOVE SPACE       TO MFS-KDTRTYP                                    
044300     END-IF                                                               
044400     PERFORM IMS-GU-WDB601                                                
044500                                                                          
044600     IF DCS-KDDC = SPACE OR DCS-DDC OR DCS-CDC-TR                         
044700                                                                          
044800       MOVE NEJ TO NYCKLAR-SW                                             
044900     ELSE                                                                 
045000                                                                          
045100       MOVE DCS-IDDC  TO W-IDDC                                           
045200                         W-IDDC-MIN                                       
045300                         W-IDDC-MAX                                       
045400                         W-IDDC-COPY                                      
045500                         W-IDDC-WDB5                                      
045600                         W-IDDC-DEF                                       
045700                         W-IDDC-4445                                      
045800                         W-IDDC-4443                                      
045900                                                                          
046000     END-IF                                                               
046100                                                                          
046200*    -- KONTROLL AV IDDISTR                                               
046300     MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-IN                               
046400                                                                          
046500     IF MID-IDDISTR-IN NOT = ALL '+'                                      
046600       MOVE '7'         TO MFS-IDPFK                                      
046700       MOVE SPACE       TO MFS-KDTRTYP                                    
046800     END-IF                                                               
046900     IF MSGI-IDDISTR NUMERIC AND MSGI-IDDISTR > 0                         
047000       MOVE MSGI-IDDISTR TO W-IDDISTR                                     
047100                            W-IDDISTR-MIN                                 
047200                            W-IDDISTR-MAX                                 
047300                            W-IDDISTR-COPY                                
047400                            W-IDDISTR-WDB5                                
047500                            W-IDDISTR-WDB2                                
047600                            W-IDDISTR-WDB2-MIN                            
047700                            W-IDDISTR-WDB2-MAX                            
047800                            WS-IDDISTR                                    
047900                            W-IDDISTR-DEF                                 
048000                            TEST-IDDISTR                                  
048100     ELSE                                                                 
048200       MOVE NEJ TO NYCKLAR-SW                                             
048300     END-IF                                                               
048400                                                                          
048500                                                                          
048600*    -- KONTROLL AV KUNDNR                                                
048700     MOVE MFS-RENSA-FAELT TO MOD-IDKUNDNR-IN                              
048800                                                                          
048900     IF MID-IDKUNDNR-IN NOT = ALL '+'                                     
049000       MOVE '7'         TO MFS-IDPFK                                      
049100       MOVE SPACE       TO MFS-KDTRTYP                                    
049200     END-IF                                                               
049300                                                                          
049400     IF MSGI-IDKUNDNR NUMERIC                                             
049500       MOVE MSGI-IDKUNDNR TO W-IDKUNDNR                                   
049600                             W-IDKUNDNR-MIN                               
049700                             W-IDKUNDNR-COPY                              
049800                             WS-IDKUNDNR                                  
049900     ELSE                                                                 
050000       MOVE NEJ TO NYCKLAR-SW                                             
050100     END-IF                                                               
050200                                                                          
050300     IF GODK-MID OR NYCKLAR-OK                                            
050400       MOVE DCS-IDDC            TO MOD-IDDC-UT                            
050500       MOVE MSGI-IDDISTR        TO MOD-IDDISTR-UT                         
050600       INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE             
050700       MOVE MSGI-IDKUNDNR       TO MOD-IDKUNDNR-UT                        
050800       INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE            
050900     ELSE                                                                 
051000       MOVE MFS-RENSA-FAELT TO MOD-IDDC-UT                                
051100                               MOD-IDDISTR-UT                             
051200                               MOD-IDKUNDNR-UT                            
051300     END-IF                                                               
051400                                                                          
051500     IF NYCKLAR-FEL                                                       
051600       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
051700       CALL WMEDKONV USING MED-WMEDAREA                                   
051800       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
051900       PERFORM MFS-RENSA-FAELT-IN                                         
052000       PERFORM MFS-RENSA-FAELT-UT                                         
052100     END-IF                                                               
052200     .                                                                    
052300     EJECT                                                                
052400 C-FOERSTA-SIDA SECTION.                                                  
052500                                                                          
052600     MOVE INF-FIRST-PAGE TO MED-IDMFSFEL                                  
052700     CALL WMEDKONV USING MED-WMEDAREA                                     
052800     MOVE MED-MFSFEL TO MOD-TEMFSFEL                                      
052900                                                                          
053000     PERFORM MFS-RENSA-FAELT-IN                                           
053100     .                                                                    
053200     EJECT                                                                
053300                                                                          
053400 D-NAESTA-SIDA SECTION.                                                   
053500                                                                          
053600     IF SPAR-IDTRANS = '4415'                                             
053700       MOVE SPAR-WDB301KY-NEXT   TO NYCKLAR-TILL-BLAEDDRING               
053800       MOVE W-MINKEY-IDDC        TO W-IDDC                                
053900                                    W-IDDC-MIN                            
054000                                    W-IDDC-MAX                            
054100                                    W-IDDC-WDB5                           
054200                                    W-IDDC-DEF                            
054300                                    W-IDDC-4445                           
054400                                    W-IDDC-4443                           
054500       MOVE W-MINKEY-IDDISTR     TO W-IDDISTR                             
054600                                    W-IDDISTR-MIN                         
054700                                    W-IDDISTR-MAX                         
054800                                    W-IDDISTR-WDB5                        
054900                                    W-IDDISTR-WDB2                        
055000                                    W-IDDISTR-WDB2-MIN                    
055100                                    W-IDDISTR-WDB2-MAX                    
055200                                    W-IDDISTR-DEF                         
055300       MOVE W-MINKEY-IDKUNDNR    TO W-IDKUNDNR                            
055400                                    W-IDKUNDNR-MIN                        
055500     END-IF                                                               
055600     PERFORM MFS-RENSA-FAELT-IN                                           
055700     .                                                                    
055800     EJECT                                                                
055900                                                                          
056000 E-SAMMA-SIDA SECTION.                                                    
056100                                                                          
056200     PERFORM MFS-RENSA-FAELT-UT                                           
056300     IF EGEN-MID OR HELP-MID                                              
056400       IF MID-INPUT = ALL '+'                                             
056500         IF SPAR-IDTRANS = '4415'                                         
056600           MOVE SPAR-WDB301KY-ENTER                                       
056700                                 TO NYCKLAR-TILL-BLAEDDRING               
056800           MOVE W-MINKEY-IDDC    TO W-IDDC                                
056900                                    W-IDDC-MIN                            
057000                                    W-IDDC-MAX                            
057100                                    W-IDDC-WDB5                           
057200                                    W-IDDC-DEF                            
057300                                    W-IDDC-4445                           
057400                                    W-IDDC-4443                           
057500                                                                          
057600           MOVE W-MINKEY-IDDISTR TO W-IDDISTR                             
057700                                    W-IDDISTR-MIN                         
057800                                    W-IDDISTR-MAX                         
057900                                    W-IDDISTR-WDB5                        
058000                                    W-IDDISTR-WDB2                        
058100                                    W-IDDISTR-WDB2-MIN                    
058200                                    W-IDDISTR-WDB2-MAX                    
058300                                    W-IDDISTR-DEF                         
058400           MOVE W-MINKEY-IDKUNDNR TO W-IDKUNDNR                           
058500                                     W-IDKUNDNR-MIN                       
058600         END-IF                                                           
058700         PERFORM MFS-RENSA-FAELT-IN                                       
058800       ELSE                                                               
058900         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
059000         CALL WMEDKONV USING MED-WMEDAREA                                 
059100         MOVE MED-MFSINF TO MOD-TEMFSFEL                                  
059200         PERFORM EA-MID-INDATA-TILL-MOD                                   
059300       END-IF                                                             
059400     ELSE                                                                 
059500       PERFORM MFS-RENSA-FAELT-IN                                         
059600     END-IF                                                               
059700     .                                                                    
059800     EJECT                                                                
059900 EA-MID-INDATA-TILL-MOD SECTION.                                          
060000                                                                          
060100     IF MID-KDCMD-IN NOT = ALL '+'                                        
060200       MOVE MID-KDCMD-IN          TO MOD-KDCMD-IN                         
060300       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDCMD-IN-ATTR                    
060400     ELSE                                                                 
060500       MOVE MFS-RENSA-FAELT       TO MOD-KDCMD-IN                         
060600     END-IF                                                               
060700                                                                          
060800     IF MID-KUNDNR-IN NOT = ALL '+'                                       
060900       IF  MID-KUNDNR-IN (1:3) = 'DEF'                                    
061000       OR  MID-KUNDNR-IN (2:3) = 'DEF'                                    
061100       OR  MID-KUNDNR-IN (3:3) = 'DEF'                                    
061200       OR  MID-KUNDNR-IN (4:3) = 'DEF'                                    
061400         MOVE '999999'            TO MID-KUNDNR-IN                        
061500         MOVE 'DEF   '            TO MOD-KUNDNR-IN                        
061600       ELSE                                                               
061700         MOVE MID-KUNDNR-IN       TO MOD-KUNDNR-IN                        
061800       END-IF                                                             
061900       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KUNDNR-IN-ATTR                   
062000     ELSE                                                                 
062100       MOVE MFS-RENSA-FAELT       TO MOD-KUNDNR-IN                        
062200     END-IF                                                               
062300                                                                          
062400     IF MID-IDGMTOMR-IN NOT = ALL '+'                                     
062500       MOVE MID-IDGMTOMR-IN       TO MOD-IDGMTOMR-IN                      
062600       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDGMTOMR-IN-ATTR                 
062700     ELSE                                                                 
062800       MOVE MFS-RENSA-FAELT       TO MOD-IDGMTOMR-IN                      
062900     END-IF                                                               
063000                                                                          
063100     IF MID-IDPKLTAB-IN NOT = ALL '+'                                     
063200       MOVE MID-IDPKLTAB-IN       TO MOD-IDPKLTAB-IN                      
063300       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDPKLTAB-IN-ATTR                 
063400     ELSE                                                                 
063500       MOVE MFS-RENSA-FAELT       TO MOD-IDPKLTAB-IN                      
063600     END-IF                                                               
063610                                                                          
063620     IF MID-KVDAGAR-TRP-DAY-IN NOT = ALL '+'                              
063630       MOVE MID-KVDAGAR-TRP-DAY-IN TO MOD-KVDAGAR-TRP-DAY-IN              
063640       MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-KVDAGAR-TRP-DAY-IN-ATTR         
063650     ELSE                                                                 
063660       MOVE MFS-RENSA-FAELT        TO MOD-KVDAGAR-TRP-DAY-IN              
063670     END-IF                                                               
063700                                                                          
063800     IF MID-IDPRCTAB-IN NOT = ALL '+'                                     
063900       MOVE MID-IDPRCTAB-IN       TO MOD-IDPRCTAB-IN                      
064000       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDPRCTAB-IN-ATTR                 
064100     ELSE                                                                 
064200       MOVE MFS-RENSA-FAELT       TO MOD-IDPRCTAB-IN                      
064300     END-IF                                                               
064400                                                                          
064500     IF MID-KDGENFRA-VOR-IN NOT = ALL '+'                                 
064600       MOVE MID-KDGENFRA-VOR-IN       TO MOD-KDGENFRA-VOR-IN              
064700       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDGENFRA-VOR-IN-ATTR             
064800     ELSE                                                                 
064900       MOVE MFS-RENSA-FAELT       TO MOD-KDGENFRA-VOR-IN                  
065000     END-IF                                                               
065100                                                                          
065200     IF MID-KDGENFRA-DO-IN NOT = ALL '+'                                  
065300       MOVE MID-KDGENFRA-DO-IN       TO MOD-KDGENFRA-DO-IN                
065400       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDGENFRA-DO-IN-ATTR              
065500     ELSE                                                                 
065600       MOVE MFS-RENSA-FAELT       TO MOD-KDGENFRA-DO-IN                   
065700     END-IF                                                               
065800                                                                          
065900     IF MID-KDGENFRA-MO-IN NOT = ALL '+'                                  
066000       MOVE MID-KDGENFRA-MO-IN       TO MOD-KDGENFRA-MO-IN                
066100       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDGENFRA-MO-IN-ATTR              
066200     ELSE                                                                 
066300       MOVE MFS-RENSA-FAELT       TO MOD-KDGENFRA-MO-IN                   
066400     END-IF                                                               
066500                                                                          
066600     IF MID-KVLEDTIM-0-IN NOT = ALL '+'                                   
066700       MOVE MID-KVLEDTIM-0-IN       TO DEC-IDFRIDATA                      
066800       MOVE 3                       TO DEC-KVHELTAL                       
066900       MOVE 2                       TO DEC-KVDECIMAL                      
067000       CALL WDECEDIT USING DEC-WDECAREA                                   
067100       IF DEC-KDSVAR-OK                                                   
067200         MOVE DEC-IDEDITDATA        TO MOD-KVLEDTIM-0-IN                  
067300         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KVLEDTIM-0-IN-ATTR             
067400       ELSE                                                               
067500         MOVE MFS-NUM-FAELT-FEL     TO MOD-KVLEDTIM-0-IN-ATTR             
067600         MOVE NEJ TO INDATA-SW                                            
067700       END-IF                                                             
067800     ELSE                                                                 
067900       MOVE MFS-RENSA-FAELT         TO MOD-KVLEDTIM-0-IN                  
068000     END-IF                                                               
068100                                                                          
068200     IF MID-KVLEDTIM-1-IN NOT = ALL '+'                                   
068300       MOVE MID-KVLEDTIM-1-IN       TO DEC-IDFRIDATA                      
068400       MOVE 3                       TO DEC-KVHELTAL                       
068500       MOVE 2                       TO DEC-KVDECIMAL                      
068600       CALL WDECEDIT USING DEC-WDECAREA                                   
068700       IF DEC-KDSVAR-OK                                                   
068800         MOVE DEC-IDEDITDATA        TO MOD-KVLEDTIM-1-IN                  
068900         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KVLEDTIM-1-IN-ATTR             
069000       ELSE                                                               
069100         MOVE MFS-NUM-FAELT-FEL     TO MOD-KVLEDTIM-1-IN-ATTR             
069200         MOVE NEJ TO INDATA-SW                                            
069300       END-IF                                                             
069400     ELSE                                                                 
069500       MOVE MFS-RENSA-FAELT         TO MOD-KVLEDTIM-1-IN                  
069600     END-IF                                                               
069700                                                                          
069800     IF MID-KVLEDTIM-2-IN NOT = ALL '+'                                   
069900       MOVE MID-KVLEDTIM-2-IN       TO DEC-IDFRIDATA                      
070000       MOVE 3                       TO DEC-KVHELTAL                       
070100       MOVE 2                       TO DEC-KVDECIMAL                      
070200       CALL WDECEDIT USING DEC-WDECAREA                                   
070300       IF DEC-KDSVAR-OK                                                   
070400         MOVE DEC-IDEDITDATA        TO MOD-KVLEDTIM-2-IN                  
070500         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KVLEDTIM-2-IN-ATTR             
070600       ELSE                                                               
070700         MOVE MFS-NUM-FAELT-FEL     TO MOD-KVLEDTIM-2-IN-ATTR             
070800         MOVE NEJ TO INDATA-SW                                            
070900       END-IF                                                             
071000     ELSE                                                                 
071100       MOVE MFS-RENSA-FAELT         TO MOD-KVLEDTIM-2-IN                  
071200     END-IF                                                               
071300                                                                          
071400     IF MID-KVLEDTIM-3-IN NOT = ALL '+'                                   
071500       MOVE MID-KVLEDTIM-3-IN       TO DEC-IDFRIDATA                      
071600       MOVE 3                       TO DEC-KVHELTAL                       
071700       MOVE 2                       TO DEC-KVDECIMAL                      
071800       CALL WDECEDIT USING DEC-WDECAREA                                   
071900       IF DEC-KDSVAR-OK                                                   
072000         MOVE DEC-IDEDITDATA        TO MOD-KVLEDTIM-3-IN                  
072100         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KVLEDTIM-3-IN-ATTR             
072200       ELSE                                                               
072300         MOVE MFS-NUM-FAELT-FEL     TO MOD-KVLEDTIM-3-IN-ATTR             
072400         MOVE NEJ TO INDATA-SW                                            
072500       END-IF                                                             
072600     ELSE                                                                 
072700       MOVE MFS-RENSA-FAELT         TO MOD-KVLEDTIM-3-IN                  
072800     END-IF                                                               
072900                                                                          
073000     IF MID-KVLEDTIM-4-IN NOT = ALL '+'                                   
073100       MOVE MID-KVLEDTIM-4-IN       TO DEC-IDFRIDATA                      
073200       MOVE 3                       TO DEC-KVHELTAL                       
073300       MOVE 2                       TO DEC-KVDECIMAL                      
073400       CALL WDECEDIT USING DEC-WDECAREA                                   
073500       IF DEC-KDSVAR-OK                                                   
073600         MOVE DEC-IDEDITDATA        TO MOD-KVLEDTIM-4-IN                  
073700         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KVLEDTIM-4-IN-ATTR             
073800       ELSE                                                               
073900         MOVE MFS-NUM-FAELT-FEL     TO MOD-KVLEDTIM-4-IN-ATTR             
074000         MOVE NEJ TO INDATA-SW                                            
074100       END-IF                                                             
074200     ELSE                                                                 
074300       MOVE MFS-RENSA-FAELT         TO MOD-KVLEDTIM-4-IN                  
074400     END-IF                                                               
074500                                                                          
074600     IF MID-KDFORSKN-IN NOT = ALL '+'                                     
074700       MOVE MID-KDFORSKN-IN       TO MOD-KDFORSKN-IN                      
074800       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDFORSKN-IN-ATTR                 
074900     ELSE                                                                 
075000       MOVE MFS-RENSA-FAELT       TO MOD-KDFORSKN-IN                      
075100     END-IF                                                               
075200                                                                          
075300     IF MID-KDSPFKTK-IN NOT = ALL '+'                                     
075400       MOVE MID-KDSPFKTK-IN       TO MOD-KDSPFKTK-IN                      
075500       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDSPFKTK-IN-ATTR                 
075600     ELSE                                                                 
075700       MOVE MFS-RENSA-FAELT       TO MOD-KDSPFKTK-IN                      
075800     END-IF                                                               
075900                                                                          
076000     IF MID-KDTULLVE-IN NOT = ALL '+'                                     
076100       MOVE MID-KDTULLVE-IN       TO MOD-KDTULLVE-IN                      
076200       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDTULLVE-IN-ATTR                 
076300     ELSE                                                                 
076400       MOVE MFS-RENSA-FAELT       TO MOD-KDTULLVE-IN                      
076500     END-IF                                                               
076600                                                                          
076700     IF MID-KDMOMSIN-IN NOT = ALL '+'                                     
076800       MOVE MID-KDMOMSIN-IN       TO MOD-KDMOMSIN-IN                      
076900       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDMOMSIN-IN-ATTR                 
077000     ELSE                                                                 
077100       MOVE MFS-RENSA-FAELT       TO MOD-KDMOMSIN-IN                      
077200     END-IF                                                               
077300     IF MID-KDROPACK-DAG-IN NOT = ALL '+'                                 
077400       MOVE MID-KDROPACK-DAG-IN   TO MOD-KDROPACK-DAG-IN                  
077500       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDROPACK-DAG-IN-ATTR             
077600     ELSE                                                                 
077700       MOVE MFS-RENSA-FAELT       TO MOD-KDROPACK-DAG-IN                  
077800     END-IF                                                               
077900                                                                          
078000     IF MID-KDROPACK-BULK-IN NOT = ALL '+'                                
078100       MOVE MID-KDROPACK-BULK-IN  TO MOD-KDROPACK-BULK-IN                 
078200       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDROPACK-BULK-IN-ATTR            
078300     ELSE                                                                 
078400       MOVE MFS-RENSA-FAELT       TO MOD-KDROPACK-BULK-IN                 
078500     END-IF                                                               
078600     .                                                                    
078700     EJECT                                                                
078800                                                                          
078900 F-LAES-VISA-INFO SECTION.                                                
079000                                                                          
079100     PERFORM IMS-GU-WDB301                                                
079200                                                                          
079300     IF SEGMENT-SAKNAS                                                    
079400*        LÄMPLIGT FELMEDDELANDE                                           
079500*       CALL WMEDKONV USING MED-WMEDAREA                                  
079600*       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
079700*       MOVE 'DC UPPG. SAKNAS ' TO MOD-TEMFSFEL                           
079800        PERFORM MFS-RENSA-FAELT-UT                                        
079900        MOVE W-WDB301KY-MIN-X        TO NYCKLAR-TILL-BLAEDDRING           
080000*       MOVE SPACE                     TO W-MINKEY-IDDC                   
080100*       MOVE ZERO                      TO W-MINKEY-IDDISTR                
080200*                                         W-MINKEY-IDKUNDNR               
080300                                                                          
080400       MOVE NYCKLAR-TILL-BLAEDDRING TO SPAR-WDB301KY-ENTER                
080500                                       SPAR-WDB301KY-NEXT                 
080600       MOVE '002'     TO MSGI-KDCALL                                      
080700       MOVE '4415'    TO MSGI-IDTRANS                                     
080800                         SPAR-IDTRANS                                     
080900       MOVE SPAR-AREA TO MSGI-SPAR-AREA                                   
081000       CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                         
081100     ELSE                                                                 
081200                                                                          
081300***** SPARA ENTER NYCKLAR PÅ USERBASEN                                    
081400                                                                          
081500       MOVE GMTB-DC-IDDC               TO W-MINKEY-IDDC                   
081600       MOVE GMTB-DC-IDDISTR            TO W-MINKEY-IDDISTR                
081700       MOVE GMTB-DC-IDKUNDNR           TO W-MINKEY-IDKUNDNR               
081800                                                                          
081900       MOVE NYCKLAR-TILL-BLAEDDRING TO SPAR-WDB301KY-ENTER                
082000                                                                          
082100       MOVE +1 TO INDX                                                    
082200       PERFORM UNTIL INDX > MAX-INDX                                      
082300                                                                          
082400         IF SEGMENT-FINNS AND GMTB-DC-IDKUNDNR < 9999999                  
082500                                                                          
082600           MOVE GMTB-DC-IDKUNDNR        TO MOD-IDKUNDNR     (INDX)        
082700           MOVE GMTB-DC-IDGMTOMR        TO MOD-IDGMTOMR     (INDX)        
082800           MOVE GMTB-DC-KVDAGAR-TRP-DAY TO                                
082801                                        MOD-KVDAGAR-TRP-DAY (INDX)        
082810           MOVE GMTB-DC-IDPKLTAB        TO MOD-IDPKLTAB     (INDX)        
082900           MOVE GMTB-DC-IDPRCTAB        TO MOD-IDPRCTAB     (INDX)        
083000           MOVE GMTB-DC-KDGENFRA-VOR    TO MOD-KDGENFRA-VOR (INDX)        
083100           MOVE GMTB-DC-KDGENFRA-DO     TO MOD-KDGENFRA-DO  (INDX)        
083200           MOVE GMTB-DC-KDGENFRA-MO     TO MOD-KDGENFRA-MO  (INDX)        
083300           MOVE GMTB-DC-KVLEDTIM-0      TO MOD-KVLEDTIM-0   (INDX)        
083400           MOVE GMTB-DC-KVLEDTIM-1      TO MOD-KVLEDTIM-1   (INDX)        
083500           MOVE GMTB-DC-KVLEDTIM-2      TO MOD-KVLEDTIM-2   (INDX)        
083600           MOVE GMTB-DC-KVLEDTIM-3      TO MOD-KVLEDTIM-3   (INDX)        
083700           MOVE GMTB-DC-KVLEDTIM-4      TO MOD-KVLEDTIM-4   (INDX)        
083800           MOVE GMTB-DC-KDFORSKN        TO MOD-KDFORSKN     (INDX)        
083900           MOVE GMTB-DC-KDSPFKTK        TO MOD-KDSPFKTK     (INDX)        
084000           MOVE GMTB-DC-KDTULLVE        TO MOD-KDTULLVE     (INDX)        
084100           MOVE GMTB-DC-KDMOMSIN        TO MOD-KDMOMSIN     (INDX)        
084200           MOVE GMTB-DC-KDROPACK-DAG    TO MOD-KDROPACK-DAG (INDX)        
084300           MOVE GMTB-DC-KDROPACK-BULK   TO MOD-KDROPACK-BULK(INDX)        
084400                                                                          
084500           ADD +1 TO INDX                                                 
084600           PERFORM IMS-GN-WDB301                                          
084700         ELSE                                                             
084800           MOVE MFS-RENSA-FAELT         TO MOD-IDKUNDNR     (INDX)        
084900                                         MOD-IDGMTOMR       (INDX)        
085000                                        MOD-KVDAGAR-TRP-DAY (INDX)        
085010                                         MOD-IDPKLTAB       (INDX)        
085100                                         MOD-IDPRCTAB       (INDX)        
085200                                         MOD-KDGENFRA-VOR   (INDX)        
085300                                         MOD-KDGENFRA-DO    (INDX)        
085400                                         MOD-KDGENFRA-MO    (INDX)        
085500                                         MOD-KVLEDTIM-0     (INDX)        
085600                                         MOD-KVLEDTIM-1     (INDX)        
085700                                         MOD-KVLEDTIM-2     (INDX)        
085800                                         MOD-KVLEDTIM-3     (INDX)        
085900                                         MOD-KVLEDTIM-4     (INDX)        
086000                                         MOD-KDFORSKN       (INDX)        
086100                                         MOD-KDSPFKTK       (INDX)        
086200                                         MOD-KDTULLVE       (INDX)        
086300                                         MOD-KDMOMSIN       (INDX)        
086400                                         MOD-KDROPACK-DAG (INDX)          
086500                                         MOD-KDROPACK-BULK(INDX)          
086600                                                                          
086700           ADD +1 TO INDX                                                 
086800         END-IF                                                           
086900       END-PERFORM                                                        
087000                                                                          
087100       IF SEGMENT-FINNS AND                                               
087200         GMTB-DC-IDKUNDNR < +9999999                                      
087300                                                                          
087400***** SPARA NEXT NYCKLAR PÅ USERBASEN                                     
087500                                                                          
087600         MOVE GMTB-DC-IDDC       TO W-MINKEY-IDDC                         
087700         MOVE GMTB-DC-IDDISTR    TO W-MINKEY-IDDISTR                      
087800         MOVE GMTB-DC-IDKUNDNR   TO W-MINKEY-IDKUNDNR                     
087900                                                                          
088000         IF MFS-UPDATE                                                    
088100           CONTINUE                                                       
088200         ELSE                                                             
088300           MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                      
088400           CALL WMEDKONV USING MED-WMEDAREA                               
088500           MOVE MED-MFSINF TO MOD-TEMFSINF                                
088600         END-IF                                                           
088700       ELSE                                                               
088800         CONTINUE                                                         
088900       END-IF                                                             
089000                                                                          
089100       MOVE NYCKLAR-TILL-BLAEDDRING TO SPAR-WDB301KY-NEXT                 
089200       MOVE '002'     TO MSGI-KDCALL                                      
089300       MOVE '4415'    TO MSGI-IDTRANS                                     
089400                         SPAR-IDTRANS                                     
089500       MOVE SPAR-AREA TO MSGI-SPAR-AREA                                   
089600       CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                         
089700                                                                          
089800     END-IF                                                               
089900                                                                          
090000****** DEFAULT LASNING                                                    
090100     PERFORM IMS-GU-WDB301-DEF                                            
090200     IF SEGMENT-FINNS                                                     
090300       MOVE DEF-DC-IDGMTOMR      TO MOD-IDGMTOMR-UT                       
090400       MOVE DEF-DC-KVDAGAR-TRP-DAY  TO                                    
090401                                     MOD-KVDAGAR-TRP-DAY-UT               
090410       MOVE DEF-DC-IDPKLTAB       TO MOD-IDPKLTAB-UT                      
090500       MOVE DEF-DC-IDPRCTAB       TO MOD-IDPRCTAB-UT                      
090600       MOVE DEF-DC-KDGENFRA-VOR   TO MOD-KDGENFRA-VOR-UT                  
090700       MOVE DEF-DC-KDGENFRA-DO    TO MOD-KDGENFRA-DO-UT                   
090800       MOVE DEF-DC-KDGENFRA-MO    TO MOD-KDGENFRA-MO-UT                   
090900       MOVE DEF-DC-KVLEDTIM-0     TO MOD-KVLEDTIM-0-UT                    
091000       MOVE DEF-DC-KVLEDTIM-1     TO MOD-KVLEDTIM-1-UT                    
091100       MOVE DEF-DC-KVLEDTIM-2     TO MOD-KVLEDTIM-2-UT                    
091200       MOVE DEF-DC-KVLEDTIM-3     TO MOD-KVLEDTIM-3-UT                    
091300       MOVE DEF-DC-KVLEDTIM-4     TO MOD-KVLEDTIM-4-UT                    
091400       MOVE DEF-DC-KDFORSKN       TO MOD-KDFORSKN-UT                      
091500       MOVE DEF-DC-KDSPFKTK       TO MOD-KDSPFKTK-UT                      
091600       MOVE DEF-DC-KDTULLVE       TO MOD-KDTULLVE-UT                      
091700       MOVE DEF-DC-KDMOMSIN       TO MOD-KDMOMSIN-UT                      
091800       MOVE DEF-DC-KDROPACK-DAG   TO MOD-KDROPACK-DAG-UT                  
091900       MOVE DEF-DC-KDROPACK-BULK TO MOD-KDROPACK-BULK-UT                  
092000     ELSE                                                                 
092100       MOVE 'DC-INFORMATION MISSING '   TO MOD-TEMFSFEL                   
092200       PERFORM MFS-RENSA-FAELT-UT                                         
092300                                                                          
092400     END-IF                                                               
092500     .                                                                    
092600     EJECT                                                                
092700                                                                          
092800 G-KOLLA-INPUT SECTION.                                                   
092900                                                                          
093000     MOVE JA  TO INDATA-SW                                                
093100     IF MID-INPUT = ALL '+'                                               
093200       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
093300       CALL WMEDKONV USING MED-WMEDAREA                                   
093400       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
093500       PERFORM MFS-ROER-EJ-FAELT-IN                                       
093600       PERFORM MFS-ROER-EJ-FAELT-UT                                       
093700       MOVE NEJ TO INDATA-SW                                              
093800     ELSE                                                                 
093900                                                                          
094000       IF MID-KDCMD-IN NOT = ALL '+'                                      
094100         IF MID-KDCMD-IN = 'N' OR 'E' OR 'D' OR 'C'                       
094200           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDCMD-IN-ATTR                 
094300                                                                          
094400           IF MID-KDCMD-IN = 'N'                                          
094500             PERFORM GA-KONTROLLERA-NEW                                   
094600           ELSE                                                           
094700             IF MID-KDCMD-IN = 'E'                                        
094800               PERFORM GB-KONTROLLERA-EDIT                                
094900             ELSE                                                         
095000               IF MID-KDCMD-IN = 'D'                                      
095100                 PERFORM GC-KONTROLLERA-DELETE                            
095200               ELSE                                                       
095300                 PERFORM GD-KONTROLLERA-COPY                              
095400               END-IF                                                     
095500             END-IF                                                       
095600           END-IF                                                         
095700         ELSE                                                             
095800            MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCMD-IN-ATTR                  
095900            MOVE NEJ TO INDATA-SW                                         
096000         END-IF                                                           
096100       ELSE                                                               
096200          MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDCMD-IN-ATTR                  
096300          MOVE NEJ TO INDATA-SW                                           
096400       END-IF                                                             
096500                                                                          
096600       IF INDATA-FEL                                                      
096700         IF GAELLANDE-KUND                                                
096800           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
096900           CALL WMEDKONV USING MED-WMEDAREA                               
097000           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
097100           PERFORM MFS-ROER-EJ-FAELT-UT                                   
097200           PERFORM MFS-ROER-EJ-FAELT-IN                                   
097300         ELSE                                                             
097400*          MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
097500*          CALL WMEDKONV USING MED-WMEDAREA                               
097600*          MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
097700           MOVE 'GOODS RECEIVER NOT VALID ' TO MOD-TEMFSFEL               
097800           PERFORM MFS-ROER-EJ-FAELT-UT                                   
097900           PERFORM MFS-ROER-EJ-FAELT-IN                                   
098000         END-IF                                                           
098100       END-IF                                                             
098200     END-IF                                                               
098300     .                                                                    
098400     EJECT                                                                
098500                                                                          
098600 GA-KONTROLLERA-NEW SECTION.                                              
098700                                                                          
098800                                                                          
098900**** KONTROLL AV KUNDNR                                                   
099000                                                                          
099100     IF MID-KUNDNR-IN NOT = ALL '+'                                       
099200       IF  MID-KUNDNR-IN (1:3) = 'DEF'                                    
099300       OR  MID-KUNDNR-IN (2:3) = 'DEF'                                    
099400       OR  MID-KUNDNR-IN (3:3) = 'DEF'                                    
099500       OR  MID-KUNDNR-IN (4:3) = 'DEF'                                    
099700         MOVE '999999'            TO MID-KUNDNR-IN                        
099800       END-IF                                                             
099900       IF MID-KUNDNR-IN NOT NUMERIC                                       
100000         MOVE MFS-ALFA-FAELT-FEL    TO MOD-KUNDNR-IN-ATTR                 
100100         MOVE NEJ TO INDATA-SW                                            
100200       ELSE                                                               
100210         IF MID-KUNDNR-IN = 999999                                        
100211            MOVE 9999999            TO W-IDKUNDNR-COPY                    
100212                                       W-IDKUNDNR-WDB2                    
100220         ELSE                                                             
100300            MOVE MID-KUNDNR-IN      TO W-IDKUNDNR-COPY                    
100400                                       W-IDKUNDNR-WDB2                    
100410         END-IF                                                           
100500         PERFORM IMS-GU-WDB301-COPY                                       
100600         IF SEGMENT-SAKNAS                                                
100700           IF MID-KUNDNR-IN = 999999                                      
100800             MOVE MFS-ALFA-FAELT-RAETT TO MOD-KUNDNR-IN-ATTR              
100900           ELSE                                                           
101000             PERFORM IMS-GU-WDB301-DEF                                    
101100             IF SEGMENT-FINNS                                             
101200               PERFORM S01-HAEMTA-UPPG-WDB201                             
101300               IF GAELLANDE-KUND                                          
101400                 MOVE MFS-ALFA-FAELT-RAETT TO MOD-KUNDNR-IN-ATTR          
101500               ELSE                                                       
101600                 MOVE MFS-ALFA-FAELT-FEL TO MOD-KUNDNR-IN-ATTR            
101700                 MOVE NEJ TO INDATA-SW                                    
101800               END-IF                                                     
101900             ELSE                                                         
102000               MOVE ' DEFAULT ROW MISSING ' TO MOD-TEMFSINF               
102100               MOVE MFS-ALFA-FAELT-FEL TO MOD-KUNDNR-IN-ATTR              
102200               MOVE NEJ TO INDATA-SW                                      
102300             END-IF                                                       
102400           END-IF                                                         
102500         ELSE                                                             
102600           MOVE 'GOODS RECEIVER ALREADY PRESENT ' TO MOD-TEMFSINF         
102700           MOVE NEJ TO INDATA-SW                                          
102800           MOVE MFS-ALFA-FAELT-FEL      TO MOD-KUNDNR-IN-ATTR             
102900         END-IF                                                           
103000       END-IF                                                             
103100     ELSE                                                                 
103200       MOVE MFS-ALFA-FAELT-FEL      TO MOD-KUNDNR-IN-ATTR                 
103300       MOVE NEJ TO INDATA-SW                                              
103400     END-IF                                                               
103500                                                                          
103600**** KONTROLL AV IDGMTOMR                                                 
103700                                                                          
103800     IF MID-IDGMTOMR-IN NOT = ALL '+'                                     
103900       IF MID-IDGMTOMR-IN NOT NUMERIC                                     
104000         MOVE MFS-NUM-FAELT-FEL     TO MOD-IDGMTOMR-IN-ATTR               
104100         MOVE NEJ TO INDATA-SW                                            
104200       ELSE                                                               
104300         MOVE MFS-NUM-FAELT-RAETT   TO MOD-IDGMTOMR-IN-ATTR               
104400       END-IF                                                             
104500     END-IF                                                               
104600                                                                          
104700**** KONTROLL AV KVDAGAR-TRP-DAY                                          
104800                                                                          
104900     IF MID-KVDAGAR-TRP-DAY-IN NOT = ALL '+'                              
105000       IF MID-KVDAGAR-TRP-DAY-IN NUMERIC                                  
105400           MOVE MFS-ALFA-FAELT-RAETT TO                                   
105500                                  MOD-KVDAGAR-TRP-DAY-IN-ATTR             
105900       ELSE                                                               
106000         MOVE MFS-ALFA-FAELT-FEL     TO                                   
106010                                  MOD-KVDAGAR-TRP-DAY-IN-ATTR             
106100         MOVE NEJ TO INDATA-SW                                            
106200       END-IF                                                             
106300     END-IF                                                               
106400                                                                          
106410**** KONTROLL AV IDPKLTAB                                                 
106420                                                                          
106430     IF MID-IDPKLTAB-IN NOT = ALL '+'                                     
106440       IF MID-IDPKLTAB-IN NUMERIC                                         
106450         MOVE MID-IDPKLTAB-IN TO W-IDPKLTAB                               
106460         PERFORM IMS-GU-XXKF-IDPKLTAB-UNIK                                
106470         IF SEGMENT-FINNS                                                 
106480           MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDPKLTAB-IN-ATTR              
106490         ELSE                                                             
106491           MOVE MFS-ALFA-FAELT-FEL  TO MOD-IDPKLTAB-IN-ATTR               
106492           MOVE NEJ TO INDATA-SW                                          
106493         END-IF                                                           
106494       ELSE                                                               
106495         MOVE MFS-ALFA-FAELT-FEL    TO MOD-IDPKLTAB-IN-ATTR               
106496         MOVE NEJ TO INDATA-SW                                            
106497       END-IF                                                             
106498     END-IF                                                               
106499                                                                          
106500**** KONTROLL AV IDPRCTAB                                                 
106600                                                                          
106700     IF MID-IDPRCTAB-IN NOT = ALL '+'                                     
106800       IF MID-IDPRCTAB-IN NOT NUMERIC                                     
106900         MOVE MFS-NUM-FAELT-FEL     TO MOD-IDPRCTAB-IN-ATTR               
107000         MOVE NEJ TO INDATA-SW                                            
107100       ELSE                                                               
107200         IF MID-IDPRCTAB-IN < +1                                          
107300           MOVE MFS-NUM-FAELT-FEL   TO MOD-IDPRCTAB-IN-ATTR               
107400           MOVE NEJ TO INDATA-SW                                          
107500         ELSE                                                             
107600           MOVE MID-IDPRCTAB-IN TO W-IDPRCTAB                             
107700           PERFORM IMS-GU-XXKG-IDPRCTAB-UNIK                              
107800           IF SEGMENT-FINNS                                               
107900             MOVE MFS-NUM-FAELT-RAETT TO MOD-IDPRCTAB-IN-ATTR             
108000           ELSE                                                           
108100             MOVE MFS-NUM-FAELT-FEL   TO MOD-IDPRCTAB-IN-ATTR             
108200             MOVE NEJ TO INDATA-SW                                        
108300           END-IF                                                         
108400         END-IF                                                           
108500       END-IF                                                             
108600     END-IF                                                               
108700                                                                          
108800**** KONTROLL AV GENERELL FRAKTKOD VOR ORDER KLASS 0                      
108900                                                                          
109000     IF MID-KDGENFRA-VOR-IN NOT = ALL '+'                                 
109100       IF MID-KDGENFRA-VOR-IN NOT NUMERIC                                 
109200         MOVE MFS-NUM-FAELT-FEL     TO MOD-KDGENFRA-VOR-IN-ATTR           
109300         MOVE NEJ TO INDATA-SW                                            
109400       ELSE                                                               
109500         IF MID-KDGENFRA-VOR-IN > ZERO                                    
109600           IF MID-KDGENFRA-VOR-IN = '88'                                  
109700             MOVE MFS-NUM-FAELT-FEL TO MOD-KDGENFRA-VOR-IN-ATTR           
109800             MOVE NEJ TO INDATA-SW                                        
109900           ELSE                                                           
110000             MOVE MFS-NUM-FAELT-RAETT TO MOD-KDGENFRA-VOR-IN-ATTR         
110100             MOVE MID-KDGENFRA-VOR-IN TO WS-KDGENFRA-VOR                  
110200           END-IF                                                         
110300         ELSE                                                             
110400           MOVE MFS-NUM-FAELT-FEL   TO MOD-KDGENFRA-VOR-IN-ATTR           
110500           MOVE NEJ TO INDATA-SW                                          
110600         END-IF                                                           
110700       END-IF                                                             
110800     ELSE                                                                 
110900       MOVE MFS-NUM-FAELT-FEL       TO MOD-KDGENFRA-VOR-IN-ATTR           
111000       MOVE NEJ TO INDATA-SW                                              
111100     END-IF                                                               
111200                                                                          
111300**** KONTROLL AV GENERELL FRAKTKOD DAGORDER KLASS 1                       
111400                                                                          
111500     IF MID-KDGENFRA-DO-IN NOT = ALL '+'                                  
111600       IF MID-KDGENFRA-DO-IN NOT NUMERIC                                  
111700         MOVE MFS-NUM-FAELT-FEL     TO MOD-KDGENFRA-DO-IN-ATTR            
111800         MOVE NEJ TO INDATA-SW                                            
111900       ELSE                                                               
112000         IF MID-KDGENFRA-DO-IN > ZERO                                     
112100           IF MID-KDGENFRA-DO-IN = '88'                                   
112200             IF DIST40-NDC-NA OR DIST40-NDC-PACIFIC                       
112300                              OR DIST40-NDC-CN                            
112400               MOVE MFS-NUM-FAELT-RAETT TO MOD-KDGENFRA-DO-IN-ATTR        
112500               MOVE MID-KDGENFRA-DO-IN TO WS-KDGENFRA-DO                  
112600             ELSE                                                         
112700               MOVE MFS-NUM-FAELT-FEL TO MOD-KDGENFRA-DO-IN-ATTR          
112800               MOVE NEJ TO INDATA-SW                                      
112900             END-IF                                                       
113000           ELSE                                                           
113100             MOVE MFS-NUM-FAELT-RAETT TO MOD-KDGENFRA-DO-IN-ATTR          
113200             MOVE MID-KDGENFRA-DO-IN TO WS-KDGENFRA-DO                    
113300           END-IF                                                         
113400         ELSE                                                             
113500           MOVE MFS-NUM-FAELT-FEL   TO MOD-KDGENFRA-DO-IN-ATTR            
113600           MOVE NEJ TO INDATA-SW                                          
113700         END-IF                                                           
113800       END-IF                                                             
113900     ELSE                                                                 
114000       MOVE MFS-NUM-FAELT-FEL       TO MOD-KDGENFRA-DO-IN-ATTR            
114100       MOVE NEJ TO INDATA-SW                                              
114200     END-IF                                                               
114300                                                                          
114400**** KONTROLL AV GENERELL FRAKTKOD MÅNADSORDER KLASS 2-4                  
114500                                                                          
114600     IF MID-KDGENFRA-MO-IN NOT = ALL '+'                                  
114700       IF MID-KDGENFRA-MO-IN NOT NUMERIC                                  
114800         MOVE MFS-NUM-FAELT-FEL     TO MOD-KDGENFRA-MO-IN-ATTR            
114900         MOVE NEJ TO INDATA-SW                                            
115000       ELSE                                                               
115100         IF MID-KDGENFRA-MO-IN > ZERO                                     
115200           IF MID-KDGENFRA-MO-IN = '88'                                   
115300             MOVE MFS-NUM-FAELT-FEL TO MOD-KDGENFRA-MO-IN-ATTR            
115400             MOVE NEJ TO INDATA-SW                                        
115500           ELSE                                                           
115600             MOVE MFS-NUM-FAELT-RAETT TO MOD-KDGENFRA-MO-IN-ATTR          
115700             MOVE MID-KDGENFRA-MO-IN TO WS-KDGENFRA-MO                    
115800           END-IF                                                         
115900         ELSE                                                             
116000           MOVE MFS-NUM-FAELT-FEL   TO MOD-KDGENFRA-MO-IN-ATTR            
116100           MOVE NEJ TO INDATA-SW                                          
116200         END-IF                                                           
116300       END-IF                                                             
116400     ELSE                                                                 
116500       MOVE MFS-NUM-FAELT-FEL       TO MOD-KDGENFRA-MO-IN-ATTR            
116600       MOVE NEJ TO INDATA-SW                                              
116700     END-IF                                                               
116800                                                                          
116900     IF INDATA-OK                                                         
117000       PERFORM GE-KOLLA-WDB5                                              
117100     END-IF                                                               
117200                                                                          
117300**** KONTROLL AV LEDTID KLASS 0                                           
117400                                                                          
117500     IF MID-KVLEDTIM-0-IN NOT = ALL '+'                                   
117600       MOVE MID-KVLEDTIM-0-IN TO DEC-IDFRIDATA                            
117700       MOVE 3                   TO DEC-KVHELTAL                           
117800       MOVE 2                   TO DEC-KVDECIMAL                          
117900                                                                          
118000       CALL WDECEDIT USING DEC-WDECAREA                                   
118100                                                                          
118200       IF DEC-KDSVAR-OK                                                   
118300         MOVE DEC-IDEDITDATA    TO WS-KVLEDTIM-0                          
118400         MOVE WS-KVLEDTIM-0     TO WS-KVLEDTIM                            
118500         IF W-KVLEDTIM-MM > 59                                            
118600         OR WS-KVLEDTIM-0 = 0.00                                          
118700           MOVE MFS-NUM-FAELT-FEL   TO MOD-KVLEDTIM-0-IN-ATTR             
118800           MOVE NEJ TO INDATA-SW                                          
118900         ELSE                                                             
119000           MOVE MFS-NUM-FAELT-RAETT TO MOD-KVLEDTIM-0-IN-ATTR             
119100         END-IF                                                           
119200       ELSE                                                               
119300         MOVE MFS-NUM-FAELT-FEL     TO MOD-KVLEDTIM-0-IN-ATTR             
119400         MOVE NEJ TO INDATA-SW                                            
119500       END-IF                                                             
119600     ELSE                                                                 
119700       MOVE MFS-NUM-FAELT-FEL       TO MOD-KVLEDTIM-0-IN-ATTR             
119800       MOVE NEJ TO INDATA-SW                                              
119900     END-IF                                                               
120000                                                                          
120100**** KONTROLL AV LEDTID KLASS 1                                           
120200                                                                          
120300     IF MID-KVLEDTIM-1-IN NOT = ALL '+'                                   
120400       MOVE MID-KVLEDTIM-1-IN TO DEC-IDFRIDATA                            
120500       MOVE 3                   TO DEC-KVHELTAL                           
120600       MOVE 2                   TO DEC-KVDECIMAL                          
120700                                                                          
120800       CALL WDECEDIT USING DEC-WDECAREA                                   
120900                                                                          
121000       IF DEC-KDSVAR-OK                                                   
121100         MOVE DEC-IDEDITDATA    TO WS-KVLEDTIM-1                          
121200         MOVE WS-KVLEDTIM-1     TO WS-KVLEDTIM                            
121300         IF W-KVLEDTIM-MM > 59                                            
121400         OR WS-KVLEDTIM-1 = 0.00                                          
121500           MOVE MFS-NUM-FAELT-FEL   TO MOD-KVLEDTIM-1-IN-ATTR             
121600           MOVE NEJ TO INDATA-SW                                          
121700         ELSE                                                             
121800           MOVE MFS-NUM-FAELT-RAETT TO MOD-KVLEDTIM-1-IN-ATTR             
121900         END-IF                                                           
122000       ELSE                                                               
122100         MOVE MFS-NUM-FAELT-FEL     TO MOD-KVLEDTIM-1-IN-ATTR             
122200         MOVE NEJ TO INDATA-SW                                            
122300       END-IF                                                             
122400     ELSE                                                                 
122500       MOVE MFS-NUM-FAELT-FEL       TO MOD-KVLEDTIM-1-IN-ATTR             
122600       MOVE NEJ TO INDATA-SW                                              
122700     END-IF                                                               
122800                                                                          
122900**** KONTROLL AV LEDTID KLASS 2                                           
123000                                                                          
123100     IF MID-KVLEDTIM-2-IN NOT = ALL '+'                                   
123200       MOVE MID-KVLEDTIM-2-IN TO DEC-IDFRIDATA                            
123300       MOVE 3                   TO DEC-KVHELTAL                           
123400       MOVE 2                   TO DEC-KVDECIMAL                          
123500                                                                          
123600       CALL WDECEDIT USING DEC-WDECAREA                                   
123700                                                                          
123800       IF DEC-KDSVAR-OK                                                   
123900         MOVE DEC-IDEDITDATA    TO WS-KVLEDTIM-2                          
124000         MOVE WS-KVLEDTIM-2     TO WS-KVLEDTIM                            
124100         IF W-KVLEDTIM    > 59                                            
124200         OR WS-KVLEDTIM-2 = 0.00                                          
124300           MOVE MFS-NUM-FAELT-FEL   TO MOD-KVLEDTIM-2-IN-ATTR             
124400           MOVE NEJ TO INDATA-SW                                          
124500         ELSE                                                             
124600           MOVE MFS-NUM-FAELT-RAETT TO MOD-KVLEDTIM-2-IN-ATTR             
124700         END-IF                                                           
124800       ELSE                                                               
124900         MOVE MFS-NUM-FAELT-FEL     TO MOD-KVLEDTIM-2-IN-ATTR             
125000         MOVE NEJ TO INDATA-SW                                            
125100       END-IF                                                             
125200     ELSE                                                                 
125300       MOVE MFS-NUM-FAELT-FEL       TO MOD-KVLEDTIM-2-IN-ATTR             
125400       MOVE NEJ TO INDATA-SW                                              
125500     END-IF                                                               
125600                                                                          
125700**** KONTROLL AV LEDTID KLASS 3                                           
125800                                                                          
125900     IF MID-KVLEDTIM-3-IN NOT = ALL '+'                                   
126000       MOVE MID-KVLEDTIM-3-IN TO DEC-IDFRIDATA                            
126100       MOVE 3                   TO DEC-KVHELTAL                           
126200       MOVE 2                   TO DEC-KVDECIMAL                          
126300                                                                          
126400       CALL WDECEDIT USING DEC-WDECAREA                                   
126500                                                                          
126600       IF DEC-KDSVAR-OK                                                   
126700         MOVE DEC-IDEDITDATA    TO WS-KVLEDTIM-3                          
126800         MOVE WS-KVLEDTIM-3     TO WS-KVLEDTIM                            
126900         IF W-KVLEDTIM-MM > 59                                            
127000         OR WS-KVLEDTIM-3 = 0.00                                          
127100           MOVE MFS-NUM-FAELT-FEL   TO MOD-KVLEDTIM-3-IN-ATTR             
127200           MOVE NEJ TO INDATA-SW                                          
127300         ELSE                                                             
127400           MOVE MFS-NUM-FAELT-RAETT TO MOD-KVLEDTIM-3-IN-ATTR             
127500         END-IF                                                           
127600       ELSE                                                               
127700         MOVE MFS-NUM-FAELT-FEL     TO MOD-KVLEDTIM-3-IN-ATTR             
127800         MOVE NEJ TO INDATA-SW                                            
127900       END-IF                                                             
128000     ELSE                                                                 
128100       MOVE MFS-NUM-FAELT-FEL       TO MOD-KVLEDTIM-3-IN-ATTR             
128200       MOVE NEJ TO INDATA-SW                                              
128300     END-IF                                                               
128400                                                                          
128500**** KONTROLL AV LEDTID KLASS 4                                           
128600                                                                          
128700     IF MID-KVLEDTIM-4-IN NOT = ALL '+'                                   
128800       MOVE MID-KVLEDTIM-4-IN TO DEC-IDFRIDATA                            
128900       MOVE 3                   TO DEC-KVHELTAL                           
129000       MOVE 2                   TO DEC-KVDECIMAL                          
129100                                                                          
129200       CALL WDECEDIT USING DEC-WDECAREA                                   
129300                                                                          
129400       IF DEC-KDSVAR-OK                                                   
129500         MOVE DEC-IDEDITDATA    TO WS-KVLEDTIM-4                          
129600         MOVE WS-KVLEDTIM-4     TO WS-KVLEDTIM                            
129700         IF W-KVLEDTIM-MM > 59                                            
129800         OR WS-KVLEDTIM-4 = 0.00                                          
129900           MOVE MFS-NUM-FAELT-FEL   TO MOD-KVLEDTIM-4-IN-ATTR             
130000           MOVE NEJ TO INDATA-SW                                          
130100         ELSE                                                             
130200           MOVE MFS-NUM-FAELT-RAETT TO MOD-KVLEDTIM-4-IN-ATTR             
130300         END-IF                                                           
130400       ELSE                                                               
130500         MOVE MFS-NUM-FAELT-FEL     TO MOD-KVLEDTIM-4-IN-ATTR             
130600         MOVE NEJ TO INDATA-SW                                            
130700       END-IF                                                             
130800     ELSE                                                                 
130900       MOVE MFS-NUM-FAELT-FEL       TO MOD-KVLEDTIM-4-IN-ATTR             
131000       MOVE NEJ TO INDATA-SW                                              
131100     END-IF                                                               
131200                                                                          
131300************ GODK KDROPACK ÄR 0-9 OCH A-L **********************          
131400                                                                          
131500     IF MID-KDROPACK-DAG-IN NOT = ALL '+'                                 
131600       MOVE MID-KDROPACK-DAG-IN  TO KDROPACK-TYP                          
131700       IF GODK-KDROPACK                                                   
131800         MOVE MFS-ALFA-FAELT-RAETT  TO MOD-KDROPACK-DAG-IN-ATTR           
131900       ELSE                                                               
132000         MOVE MFS-ALFA-FAELT-FEL    TO MOD-KDROPACK-DAG-IN-ATTR           
132100         MOVE NEJ TO INDATA-SW                                            
132200       END-IF                                                             
132300     END-IF                                                               
132400                                                                          
132500     IF MID-KDROPACK-BULK-IN NOT = ALL '+'                                
132600       MOVE MID-KDROPACK-BULK-IN TO KDROPACK-TYP                          
132700       IF GODK-KDROPACK                                                   
132800         MOVE MFS-ALFA-FAELT-RAETT  TO MOD-KDROPACK-BULK-IN-ATTR          
132900       ELSE                                                               
133000         MOVE MFS-ALFA-FAELT-FEL    TO MOD-KDROPACK-BULK-IN-ATTR          
133100         MOVE NEJ TO INDATA-SW                                            
133200       END-IF                                                             
133300     END-IF                                                               
133400                                                                          
133500****************** NEDANSTÅENDE FÄLT FÅR INTE ÄNDRAS OM **********        
133600****************** DET ÄR SAMFAKTURERING               ***********        
133700                                                                          
133800**** KONTROLL AV FÖRSÄKRANSKOD                                            
133900                                                                          
134000     PERFORM S03-HAEMTA-DEF-UPPG-WDB3                                     
134100                                                                          
134200     IF MID-KDFORSKN-IN NOT = ALL '+'                                     
134300       IF MID-KDFORSKN-IN NOT NUMERIC                                     
134400         MOVE MFS-NUM-FAELT-FEL     TO MOD-KDFORSKN-IN-ATTR               
134500         MOVE NEJ TO INDATA-SW                                            
134600       ELSE                                                               
134700         IF WS-FLSAMFAK = NEJ                                             
134800           MOVE MFS-NUM-FAELT-RAETT TO MOD-KDFORSKN-IN-ATTR               
134900         ELSE                                                             
135000           IF MID-KDFORSKN-IN = SPAR-KDFORSKN                             
135100             MOVE MFS-NUM-FAELT-RAETT TO MOD-KDFORSKN-IN-ATTR             
135200           ELSE                                                           
135300             MOVE MFS-NUM-FAELT-FEL TO MOD-KDFORSKN-IN-ATTR               
135400             MOVE NEJ TO INDATA-SW                                        
135500           END-IF                                                         
135600         END-IF                                                           
135700       END-IF                                                             
135800     END-IF                                                               
135900                                                                          
136000**** KONTROLL AV SPECIAL FAKTURA                                          
136100                                                                          
136200     IF MID-KDSPFKTK-IN NOT = ALL '+'                                     
136300       IF MID-KDSPFKTK-IN NOT NUMERIC                                     
136400         MOVE MFS-NUM-FAELT-FEL     TO MOD-KDSPFKTK-IN-ATTR               
136500         MOVE NEJ TO INDATA-SW                                            
136600       ELSE                                                               
136700         IF MID-KDSPFKTK-IN < '8'                                         
136800           IF WS-FLSAMFAK = NEJ                                           
136900             MOVE MFS-NUM-FAELT-RAETT TO MOD-KDSPFKTK-IN-ATTR             
137000           ELSE                                                           
137100             IF MID-KDSPFKTK-IN = SPAR-KDSPFKTK                           
137200               MOVE MFS-NUM-FAELT-RAETT TO MOD-KDSPFKTK-IN-ATTR           
137300             ELSE                                                         
137400               MOVE MFS-NUM-FAELT-FEL TO MOD-KDSPFKTK-IN-ATTR             
137500               MOVE NEJ TO INDATA-SW                                      
137600             END-IF                                                       
137700           END-IF                                                         
137800         ELSE                                                             
137900           MOVE MFS-NUM-FAELT-FEL   TO MOD-KDSPFKTK-IN-ATTR               
138000           MOVE NEJ TO INDATA-SW                                          
138100         END-IF                                                           
138200       END-IF                                                             
138300     END-IF                                                               
138400                                                                          
138500**** KONTROLL AV TYP AV PRIS PÅ TULLFAKTURA                               
138600                                                                          
138700     IF MID-KDTULLVE-IN NOT = ALL '+'                                     
138800       IF MID-KDTULLVE-IN NOT NUMERIC                                     
138900         MOVE MFS-NUM-FAELT-FEL     TO MOD-KDTULLVE-IN-ATTR               
139000         MOVE NEJ TO INDATA-SW                                            
139100       ELSE                                                               
139200********* BÖR KONTROLLERAS TL                                             
139300         IF MID-KDTULLVE-IN = '0' OR '1' OR '4' OR '5' OR                 
139400            '6' OR '7' OR '9'                                             
139500           IF WS-FLSAMFAK = NEJ                                           
139600             MOVE MFS-NUM-FAELT-RAETT TO MOD-KDTULLVE-IN-ATTR             
139700           ELSE                                                           
139800             IF MID-KDTULLVE-IN = SPAR-KDTULLVE                           
139900               MOVE MFS-NUM-FAELT-RAETT TO MOD-KDTULLVE-IN-ATTR           
140000             ELSE                                                         
140100               MOVE MFS-NUM-FAELT-FEL TO MOD-KDTULLVE-IN-ATTR             
140200               MOVE NEJ TO INDATA-SW                                      
140300             END-IF                                                       
140400           END-IF                                                         
140500         ELSE                                                             
140600           MOVE MFS-NUM-FAELT-FEL   TO MOD-KDTULLVE-IN-ATTR               
140700           MOVE NEJ TO INDATA-SW                                          
140800         END-IF                                                           
140900       END-IF                                                             
141000     END-IF                                                               
141100                                                                          
141200**** KONTROLL AV MOMSINSTRUKTION                                          
141300                                                                          
141400     IF MID-KDMOMSIN-IN NOT = ALL '+'                                     
141500       IF MID-KDMOMSIN-IN NOT NUMERIC                                     
141600         MOVE MFS-NUM-FAELT-FEL     TO MOD-KDMOMSIN-IN-ATTR               
141700         MOVE NEJ TO INDATA-SW                                            
141800       ELSE                                                               
141900         IF MID-KDMOMSIN-IN = '1' OR '2'                                  
142000           IF WS-FLSAMFAK = NEJ                                           
142100             MOVE MFS-NUM-FAELT-RAETT TO MOD-KDMOMSIN-IN-ATTR             
142200           ELSE                                                           
142300             IF MID-KDMOMSIN-IN = SPAR-KDMOMSIN                           
142400               MOVE MFS-NUM-FAELT-RAETT TO MOD-KDMOMSIN-IN-ATTR           
142500             ELSE                                                         
142600               MOVE MFS-NUM-FAELT-FEL TO MOD-KDMOMSIN-IN-ATTR             
142700               MOVE NEJ TO INDATA-SW                                      
142800             END-IF                                                       
142900           END-IF                                                         
143000         ELSE                                                             
143100           MOVE MFS-NUM-FAELT-FEL   TO MOD-KDMOMSIN-IN-ATTR               
143200           MOVE NEJ TO INDATA-SW                                          
143300         END-IF                                                           
143400       END-IF                                                             
143500     ELSE                                                                 
143600       MOVE MFS-NUM-FAELT-FEL       TO MOD-KDMOMSIN-IN-ATTR               
143700       MOVE NEJ TO INDATA-SW                                              
143800     END-IF                                                               
143900     .                                                                    
144000     EJECT                                                                
144100                                                                          
144200 GB-KONTROLLERA-EDIT SECTION.                                             
144300                                                                          
144400                                                                          
144500**** KONTROLL AV KUNDNR                                                   
144600                                                                          
144700     IF MID-KUNDNR-IN NOT = ALL '+'                                       
144800       IF  MID-KUNDNR-IN (1:3) = 'DEF'                                    
144900       OR  MID-KUNDNR-IN (2:3) = 'DEF'                                    
145000       OR  MID-KUNDNR-IN (3:3) = 'DEF'                                    
145100       OR  MID-KUNDNR-IN (4:3) = 'DEF'                                    
145300         MOVE '999999'            TO MID-KUNDNR-IN                        
145400       END-IF                                                             
145500       IF MID-KUNDNR-IN NUMERIC                                           
145510          IF MID-KUNDNR-IN = 999999                                       
145511            MOVE 9999999       TO W-IDKUNDNR-COPY                         
145512                               W-IDKUNDNR-WDB2                            
145520          ELSE                                                            
145600            MOVE MID-KUNDNR-IN TO W-IDKUNDNR-COPY                         
145700                               W-IDKUNDNR-WDB2                            
145710          END-IF                                                          
145800         PERFORM IMS-GU-WDB301-COPY                                       
145900         IF SEGMENT-FINNS                                                 
146000           IF MID-KUNDNR-IN = 999999                                      
146100             MOVE MFS-ALFA-FAELT-RAETT TO MOD-KUNDNR-IN-ATTR              
146200           ELSE                                                           
146300             PERFORM IMS-GU-WDB301-DEF                                    
146400             IF SEGMENT-FINNS                                             
146500               PERFORM S01-HAEMTA-UPPG-WDB201                             
146600               IF GAELLANDE-KUND                                          
146700                 MOVE MFS-ALFA-FAELT-RAETT TO MOD-KUNDNR-IN-ATTR          
146800               ELSE                                                       
146900                 MOVE MFS-ALFA-FAELT-FEL TO MOD-KUNDNR-IN-ATTR            
147000                 MOVE NEJ TO INDATA-SW                                    
147100               END-IF                                                     
147200             ELSE                                                         
147300               MOVE ' DEFAULT ROW MISSING ' TO MOD-TEMFSINF               
147400               MOVE MFS-ALFA-FAELT-FEL TO MOD-KUNDNR-IN-ATTR              
147500               MOVE NEJ TO INDATA-SW                                      
147600             END-IF                                                       
147700           END-IF                                                         
147800         ELSE                                                             
147900           MOVE MFS-ALFA-FAELT-FEL    TO MOD-KUNDNR-IN-ATTR               
148000           MOVE NEJ TO INDATA-SW                                          
148100           MOVE 'GOODS RECEIVER MISSING '     TO MOD-TEMFSINF             
148200         END-IF                                                           
148300       ELSE                                                               
148400         MOVE MFS-ALFA-FAELT-FEL    TO MOD-KUNDNR-IN-ATTR                 
148500         MOVE NEJ TO INDATA-SW                                            
148600       END-IF                                                             
148700     ELSE                                                                 
148800       MOVE MFS-ALFA-FAELT-FEL      TO MOD-KUNDNR-IN-ATTR                 
148900       MOVE NEJ TO INDATA-SW                                              
149000     END-IF                                                               
149100                                                                          
149200**** KONTROLL AV IDGMTOMR                                                 
149300                                                                          
149400     IF MID-IDGMTOMR-IN NOT = ALL '+'                                     
149500       IF MID-IDGMTOMR-IN NOT NUMERIC                                     
149600         MOVE MFS-NUM-FAELT-FEL     TO MOD-IDGMTOMR-IN-ATTR               
149700         MOVE NEJ TO INDATA-SW                                            
149800       ELSE                                                               
149900         MOVE MFS-NUM-FAELT-RAETT   TO MOD-IDGMTOMR-IN-ATTR               
150000       END-IF                                                             
150100     END-IF                                                               
150200                                                                          
150300**** KONTROLL AV KVDAGAR-TRP-DAY                                          
150400                                                                          
150500     IF MID-KVDAGAR-TRP-DAY-IN NOT = ALL '+'                              
150600       IF MID-KVDAGAR-TRP-DAY-IN NUMERIC                                  
151000         MOVE MFS-ALFA-FAELT-RAETT TO MOD-KVDAGAR-TRP-DAY-IN-ATTR         
151500       ELSE                                                               
151600         MOVE MFS-ALFA-FAELT-FEL   TO MOD-KVDAGAR-TRP-DAY-IN-ATTR         
151700         MOVE NEJ TO INDATA-SW                                            
151800       END-IF                                                             
151900     END-IF                                                               
152000                                                                          
152010                                                                          
152020**** KONTROLL AV IDPKLTAB                                                 
152030                                                                          
152040     IF MID-IDPKLTAB-IN NOT = ALL '+'                                     
152050       IF MID-IDPKLTAB-IN NUMERIC                                         
152060         MOVE MID-IDPKLTAB-IN TO W-IDPKLTAB                               
152070         PERFORM IMS-GU-XXKF-IDPKLTAB-UNIK                                
152080         IF SEGMENT-FINNS                                                 
152090           MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDPKLTAB-IN-ATTR              
152091         ELSE                                                             
152092           MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDPKLTAB-IN-ATTR              
152093           MOVE NEJ TO INDATA-SW                                          
152094         END-IF                                                           
152095       ELSE                                                               
152096         MOVE MFS-ALFA-FAELT-FEL TO MOD-IDPKLTAB-IN-ATTR                  
152097         MOVE NEJ TO INDATA-SW                                            
152098       END-IF                                                             
152099     END-IF                                                               
152100                                                                          
152110**** KONTROLL AV IDPRCTAB                                                 
152200                                                                          
152300     IF MID-IDPRCTAB-IN NOT = ALL '+'                                     
152400       IF MID-IDPRCTAB-IN NOT NUMERIC                                     
152500         MOVE MFS-NUM-FAELT-FEL     TO MOD-IDPRCTAB-IN-ATTR               
152600         MOVE NEJ TO INDATA-SW                                            
152700       ELSE                                                               
152800         IF MID-IDPRCTAB-IN < +1                                          
152900           MOVE MFS-NUM-FAELT-FEL   TO MOD-IDPRCTAB-IN-ATTR               
153000           MOVE NEJ TO INDATA-SW                                          
153100         ELSE                                                             
153200           MOVE MID-IDPRCTAB-IN TO W-IDPRCTAB                             
153300           PERFORM IMS-GU-XXKG-IDPRCTAB-UNIK                              
153400           IF SEGMENT-FINNS                                               
153500             MOVE MFS-NUM-FAELT-RAETT TO MOD-IDPRCTAB-IN-ATTR             
153600           ELSE                                                           
153700             MOVE MFS-NUM-FAELT-FEL   TO MOD-IDPRCTAB-IN-ATTR             
153800             MOVE NEJ TO INDATA-SW                                        
153900           END-IF                                                         
154000         END-IF                                                           
154100       END-IF                                                             
154200     END-IF                                                               
154300                                                                          
154400**** KONTROLL AV GENERELL FRAKTKOD VOR ORDER KLASS 0                      
154500                                                                          
154600     IF MID-KDGENFRA-VOR-IN NOT = ALL '+'                                 
154700       IF MID-KDGENFRA-VOR-IN NOT NUMERIC                                 
154800         MOVE MFS-NUM-FAELT-FEL     TO MOD-KDGENFRA-VOR-IN-ATTR           
154900         MOVE NEJ TO INDATA-SW                                            
155000       ELSE                                                               
155100         IF MID-KDGENFRA-VOR-IN > ZERO                                    
155200           IF MID-KDGENFRA-VOR-IN = '88'                                  
155300             MOVE MFS-NUM-FAELT-FEL TO MOD-KDGENFRA-VOR-IN-ATTR           
155400             MOVE NEJ TO INDATA-SW                                        
155500           ELSE                                                           
155600             MOVE MFS-NUM-FAELT-RAETT TO MOD-KDGENFRA-VOR-IN-ATTR         
155700             MOVE MID-KDGENFRA-VOR-IN TO WS-KDGENFRA-VOR                  
155800           END-IF                                                         
155900         ELSE                                                             
156000           MOVE MFS-NUM-FAELT-FEL   TO MOD-KDGENFRA-VOR-IN-ATTR           
156100           MOVE NEJ TO INDATA-SW                                          
156200         END-IF                                                           
156300       END-IF                                                             
156400     ELSE                                                                 
156500       MOVE ZERO TO WS-KDGENFRA-VOR                                       
156600     END-IF                                                               
156700                                                                          
156800**** KONTROLL AV GENERELL FRAKTKOD DAGORDER KLASS 1                       
156900                                                                          
157000     IF MID-KDGENFRA-DO-IN NOT = ALL '+'                                  
157100       IF MID-KDGENFRA-DO-IN NOT NUMERIC                                  
157200         MOVE MFS-NUM-FAELT-FEL     TO MOD-KDGENFRA-DO-IN-ATTR            
157300         MOVE NEJ TO INDATA-SW                                            
157400       ELSE                                                               
157500         IF MID-KDGENFRA-DO-IN > ZERO                                     
157600           IF MID-KDGENFRA-DO-IN = '88'                                   
157700             IF DIST40-NDC-NA OR DIST40-NDC-PACIFIC                       
157800                              OR DIST40-NDC-CN                            
157900               MOVE MFS-NUM-FAELT-RAETT TO MOD-KDGENFRA-DO-IN-ATTR        
158000               MOVE MID-KDGENFRA-DO-IN TO WS-KDGENFRA-DO                  
158100             ELSE                                                         
158200               MOVE MFS-NUM-FAELT-FEL TO MOD-KDGENFRA-DO-IN-ATTR          
158300               MOVE NEJ TO INDATA-SW                                      
158400             END-IF                                                       
158500           ELSE                                                           
158600             MOVE MFS-NUM-FAELT-RAETT TO MOD-KDGENFRA-DO-IN-ATTR          
158700             MOVE MID-KDGENFRA-DO-IN TO WS-KDGENFRA-DO                    
158800           END-IF                                                         
158900         ELSE                                                             
159000           MOVE MFS-NUM-FAELT-FEL   TO MOD-KDGENFRA-DO-IN-ATTR            
159100           MOVE NEJ TO INDATA-SW                                          
159200         END-IF                                                           
159300       END-IF                                                             
159400     ELSE                                                                 
159500       MOVE ZERO TO WS-KDGENFRA-DO                                        
159600     END-IF                                                               
159700                                                                          
159800**** KONTROLL AV GENERELL FRAKTKOD MÅNADSORDER KLASS 2-4                  
159900                                                                          
160000     IF MID-KDGENFRA-MO-IN NOT = ALL '+'                                  
160100       IF MID-KDGENFRA-MO-IN NOT NUMERIC                                  
160200         MOVE MFS-NUM-FAELT-FEL     TO MOD-KDGENFRA-MO-IN-ATTR            
160300         MOVE NEJ TO INDATA-SW                                            
160400       ELSE                                                               
160500         IF MID-KDGENFRA-MO-IN > ZERO                                     
160600           IF MID-KDGENFRA-MO-IN = '88'                                   
160700             MOVE MFS-NUM-FAELT-FEL TO MOD-KDGENFRA-MO-IN-ATTR            
160800             MOVE NEJ TO INDATA-SW                                        
160900           ELSE                                                           
161000             MOVE MFS-NUM-FAELT-RAETT TO MOD-KDGENFRA-MO-IN-ATTR          
161100             MOVE MID-KDGENFRA-MO-IN TO WS-KDGENFRA-MO                    
161200           END-IF                                                         
161300         ELSE                                                             
161400           MOVE MFS-NUM-FAELT-FEL   TO MOD-KDGENFRA-MO-IN-ATTR            
161500           MOVE NEJ TO INDATA-SW                                          
161600         END-IF                                                           
161700       END-IF                                                             
161800     ELSE                                                                 
161900       MOVE ZERO TO WS-KDGENFRA-MO                                        
162000     END-IF                                                               
162100                                                                          
162200     IF INDATA-OK                                                         
162300       PERFORM GE-KOLLA-WDB5                                              
162400     END-IF                                                               
162500                                                                          
162600**** KONTROLL AV LEDTID KLASS 0                                           
162700                                                                          
162800     IF MID-KVLEDTIM-0-IN NOT = ALL '+'                                   
162900       MOVE MID-KVLEDTIM-0-IN TO DEC-IDFRIDATA                            
163000       MOVE 3                   TO DEC-KVHELTAL                           
163100       MOVE 2                   TO DEC-KVDECIMAL                          
163200                                                                          
163300       CALL WDECEDIT USING DEC-WDECAREA                                   
163400                                                                          
163500       IF DEC-KDSVAR-OK                                                   
163600         MOVE DEC-IDEDITDATA    TO WS-KVLEDTIM-0                          
163700         MOVE WS-KVLEDTIM-0     TO WS-KVLEDTIM                            
163800         IF W-KVLEDTIM-MM > 59                                            
163900         OR WS-KVLEDTIM-0 = 0.00                                          
164000           MOVE MFS-NUM-FAELT-FEL   TO MOD-KVLEDTIM-0-IN-ATTR             
164100           MOVE NEJ TO INDATA-SW                                          
164200         ELSE                                                             
164300           MOVE MFS-NUM-FAELT-RAETT TO MOD-KVLEDTIM-0-IN-ATTR             
164400         END-IF                                                           
164500       ELSE                                                               
164600         MOVE MFS-NUM-FAELT-FEL     TO MOD-KVLEDTIM-0-IN-ATTR             
164700         MOVE NEJ TO INDATA-SW                                            
164800       END-IF                                                             
164900     END-IF                                                               
165000                                                                          
165100**** KONTROLL AV LEDTID KLASS 1                                           
165200                                                                          
165300     IF MID-KVLEDTIM-1-IN NOT = ALL '+'                                   
165400       MOVE MID-KVLEDTIM-1-IN TO DEC-IDFRIDATA                            
165500       MOVE 3                   TO DEC-KVHELTAL                           
165600       MOVE 2                   TO DEC-KVDECIMAL                          
165700                                                                          
165800       CALL WDECEDIT USING DEC-WDECAREA                                   
165900                                                                          
166000       IF DEC-KDSVAR-OK                                                   
166100         MOVE DEC-IDEDITDATA    TO WS-KVLEDTIM-1                          
166200         MOVE WS-KVLEDTIM-1     TO WS-KVLEDTIM                            
166300         IF W-KVLEDTIM-MM > 59                                            
166400         OR WS-KVLEDTIM-1 = 0.00                                          
166500           MOVE MFS-NUM-FAELT-FEL   TO MOD-KVLEDTIM-1-IN-ATTR             
166600           MOVE NEJ TO INDATA-SW                                          
166700         ELSE                                                             
166800           MOVE MFS-NUM-FAELT-RAETT TO MOD-KVLEDTIM-1-IN-ATTR             
166900         END-IF                                                           
167000       ELSE                                                               
167100         MOVE MFS-NUM-FAELT-FEL     TO MOD-KVLEDTIM-1-IN-ATTR             
167200         MOVE NEJ TO INDATA-SW                                            
167300       END-IF                                                             
167400     END-IF                                                               
167500                                                                          
167600**** KONTROLL AV LEDTID KLASS 2                                           
167700                                                                          
167800     IF MID-KVLEDTIM-2-IN NOT = ALL '+'                                   
167900       MOVE MID-KVLEDTIM-2-IN TO DEC-IDFRIDATA                            
168000       MOVE 3                   TO DEC-KVHELTAL                           
168100       MOVE 2                   TO DEC-KVDECIMAL                          
168200                                                                          
168300       CALL WDECEDIT USING DEC-WDECAREA                                   
168400                                                                          
168500       IF DEC-KDSVAR-OK                                                   
168600         MOVE DEC-IDEDITDATA    TO WS-KVLEDTIM-2                          
168700         MOVE WS-KVLEDTIM-2     TO WS-KVLEDTIM                            
168800         IF W-KVLEDTIM    > 59                                            
168900         OR WS-KVLEDTIM-2 = 0.00                                          
169000           MOVE MFS-NUM-FAELT-FEL   TO MOD-KVLEDTIM-2-IN-ATTR             
169100           MOVE NEJ TO INDATA-SW                                          
169200         ELSE                                                             
169300           MOVE MFS-NUM-FAELT-RAETT TO MOD-KVLEDTIM-2-IN-ATTR             
169400         END-IF                                                           
169500       ELSE                                                               
169600         MOVE MFS-NUM-FAELT-FEL     TO MOD-KVLEDTIM-2-IN-ATTR             
169700         MOVE NEJ TO INDATA-SW                                            
169800       END-IF                                                             
169900     END-IF                                                               
170000                                                                          
170100**** KONTROLL AV LEDTID KLASS 3                                           
170200                                                                          
170300     IF MID-KVLEDTIM-3-IN NOT = ALL '+'                                   
170400       MOVE MID-KVLEDTIM-3-IN TO DEC-IDFRIDATA                            
170500       MOVE 3                   TO DEC-KVHELTAL                           
170600       MOVE 2                   TO DEC-KVDECIMAL                          
170700                                                                          
170800       CALL WDECEDIT USING DEC-WDECAREA                                   
170900                                                                          
171000       IF DEC-KDSVAR-OK                                                   
171100         MOVE DEC-IDEDITDATA    TO WS-KVLEDTIM-3                          
171200         MOVE WS-KVLEDTIM-3     TO WS-KVLEDTIM                            
171300         IF W-KVLEDTIM-MM > 59                                            
171400         OR WS-KVLEDTIM-3 = 0.00                                          
171500           MOVE MFS-NUM-FAELT-FEL   TO MOD-KVLEDTIM-3-IN-ATTR             
171600           MOVE NEJ TO INDATA-SW                                          
171700         ELSE                                                             
171800           MOVE MFS-NUM-FAELT-RAETT TO MOD-KVLEDTIM-3-IN-ATTR             
171900         END-IF                                                           
172000       ELSE                                                               
172100         MOVE MFS-NUM-FAELT-FEL     TO MOD-KVLEDTIM-3-IN-ATTR             
172200         MOVE NEJ TO INDATA-SW                                            
172300       END-IF                                                             
172400     END-IF                                                               
172500                                                                          
172600**** KONTROLL AV LEDTID KLASS 4                                           
172700                                                                          
172800     IF MID-KVLEDTIM-4-IN NOT = ALL '+'                                   
172900       MOVE MID-KVLEDTIM-4-IN TO DEC-IDFRIDATA                            
173000       MOVE 3                   TO DEC-KVHELTAL                           
173100       MOVE 2                   TO DEC-KVDECIMAL                          
173200                                                                          
173300       CALL WDECEDIT USING DEC-WDECAREA                                   
173400                                                                          
173500       IF DEC-KDSVAR-OK                                                   
173600         MOVE DEC-IDEDITDATA    TO WS-KVLEDTIM-4                          
173700         MOVE WS-KVLEDTIM-4     TO WS-KVLEDTIM                            
173800         IF W-KVLEDTIM-MM > 59                                            
173900         OR WS-KVLEDTIM-4 = 0.00                                          
174000           MOVE MFS-NUM-FAELT-FEL   TO MOD-KVLEDTIM-4-IN-ATTR             
174100           MOVE NEJ TO INDATA-SW                                          
174200         ELSE                                                             
174300           MOVE MFS-NUM-FAELT-RAETT TO MOD-KVLEDTIM-4-IN-ATTR             
174400         END-IF                                                           
174500       ELSE                                                               
174600         MOVE MFS-NUM-FAELT-FEL     TO MOD-KVLEDTIM-4-IN-ATTR             
174700         MOVE NEJ TO INDATA-SW                                            
174800       END-IF                                                             
174900     END-IF                                                               
175000                                                                          
175100************ GODK KDROPACK ÄR 0-9 OCH A-L **********************          
175200                                                                          
175300     IF MID-KDROPACK-DAG-IN NOT = ALL '+'                                 
175400       MOVE MID-KDROPACK-DAG-IN TO KDROPACK-TYP                           
175500       IF GODK-KDROPACK                                                   
175600         MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDROPACK-DAG-IN-ATTR            
175700       ELSE                                                               
175800         MOVE MFS-ALFA-FAELT-FEL    TO MOD-KDROPACK-DAG-IN-ATTR           
175900         MOVE NEJ TO INDATA-SW                                            
176000       END-IF                                                             
176100     END-IF                                                               
176200                                                                          
176300     IF MID-KDROPACK-BULK-IN NOT = ALL '+'                                
176400       MOVE MID-KDROPACK-BULK-IN TO KDROPACK-TYP                          
176500       IF GODK-KDROPACK                                                   
176600         MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDROPACK-BULK-IN-ATTR           
176700       ELSE                                                               
176800         MOVE MFS-ALFA-FAELT-FEL    TO MOD-KDROPACK-BULK-IN-ATTR          
176900         MOVE NEJ TO INDATA-SW                                            
177000       END-IF                                                             
177100     END-IF                                                               
177200                                                                          
177300**************** NEDANSTÅENDE FÄLT FÅR INTE ÄNDRAS OM ************        
177400**************** SAMFAKTURERING                       ************        
177500                                                                          
177600**** KONTROLL AV FÖRSÄKRANSKOD                                            
177700                                                                          
177800     PERFORM S03-HAEMTA-DEF-UPPG-WDB3                                     
177900                                                                          
178000     IF MID-KDFORSKN-IN NOT = ALL '+'                                     
178100       IF MID-KDFORSKN-IN NOT NUMERIC                                     
178200         MOVE MFS-NUM-FAELT-FEL     TO MOD-KDFORSKN-IN-ATTR               
178300         MOVE NEJ TO INDATA-SW                                            
178400       ELSE                                                               
178500         IF WS-FLSAMFAK = NEJ                                             
178600           MOVE MFS-NUM-FAELT-RAETT TO MOD-KDFORSKN-IN-ATTR               
178700         ELSE                                                             
178800           IF MID-KDFORSKN-IN = SPAR-KDFORSKN                             
178900             MOVE MFS-NUM-FAELT-RAETT TO MOD-KDFORSKN-IN-ATTR             
179000           ELSE                                                           
179100             MOVE MFS-NUM-FAELT-FEL TO MOD-KDFORSKN-IN-ATTR               
179200             MOVE NEJ TO INDATA-SW                                        
179300           END-IF                                                         
179400         END-IF                                                           
179500       END-IF                                                             
179600     END-IF                                                               
179700                                                                          
179800**** KONTROLL AV SPECIAL FAKTURA                                          
179900                                                                          
180000     IF MID-KDSPFKTK-IN NOT = ALL '+'                                     
180100       IF MID-KDSPFKTK-IN NOT NUMERIC                                     
180200         MOVE MFS-NUM-FAELT-FEL     TO MOD-KDSPFKTK-IN-ATTR               
180300         MOVE NEJ TO INDATA-SW                                            
180400       ELSE                                                               
180500         IF MID-KDSPFKTK-IN < '8'                                         
180600           IF WS-FLSAMFAK = NEJ                                           
180700             MOVE MFS-NUM-FAELT-RAETT TO MOD-KDSPFKTK-IN-ATTR             
180800           ELSE                                                           
180900             IF MID-KDSPFKTK-IN = SPAR-KDSPFKTK                           
181000               MOVE MFS-NUM-FAELT-RAETT TO MOD-KDSPFKTK-IN-ATTR           
181100             ELSE                                                         
181200               MOVE MFS-NUM-FAELT-FEL TO MOD-KDSPFKTK-IN-ATTR             
181300               MOVE NEJ TO INDATA-SW                                      
181400             END-IF                                                       
181500           END-IF                                                         
181600         ELSE                                                             
181700           MOVE MFS-NUM-FAELT-FEL   TO MOD-KDSPFKTK-IN-ATTR               
181800           MOVE NEJ TO INDATA-SW                                          
181900         END-IF                                                           
182000       END-IF                                                             
182100     END-IF                                                               
182200                                                                          
182300**** KONTROLL AV TYP AV PRIS PÅ TULLFAKTURA                               
182400                                                                          
182500     IF MID-KDTULLVE-IN NOT = ALL '+'                                     
182600       IF MID-KDTULLVE-IN NOT NUMERIC                                     
182700         MOVE MFS-NUM-FAELT-FEL     TO MOD-KDTULLVE-IN-ATTR               
182800         MOVE NEJ TO INDATA-SW                                            
182900       ELSE                                                               
183000********* BÖR KONTROLLERAS TL                                             
183100         IF MID-KDTULLVE-IN = '0' OR '1' OR '4' OR '5' OR                 
183200            '6' OR '7' OR '9'                                             
183300           IF WS-FLSAMFAK = NEJ                                           
183400             MOVE MFS-NUM-FAELT-RAETT TO MOD-KDTULLVE-IN-ATTR             
183500           ELSE                                                           
183600             IF MID-KDTULLVE-IN = SPAR-KDTULLVE                           
183700               MOVE MFS-NUM-FAELT-RAETT TO MOD-KDTULLVE-IN-ATTR           
183800             ELSE                                                         
183900               MOVE MFS-NUM-FAELT-FEL TO MOD-KDTULLVE-IN-ATTR             
184000               MOVE NEJ TO INDATA-SW                                      
184100             END-IF                                                       
184200           END-IF                                                         
184300         ELSE                                                             
184400           MOVE MFS-NUM-FAELT-FEL   TO MOD-KDTULLVE-IN-ATTR               
184500           MOVE NEJ TO INDATA-SW                                          
184600         END-IF                                                           
184700       END-IF                                                             
184800     END-IF                                                               
184900                                                                          
185000**** KONTROLL AV MOMSINSTRUKTION                                          
185100                                                                          
185200     IF MID-KDMOMSIN-IN NOT = ALL '+'                                     
185300       IF MID-KDMOMSIN-IN NOT NUMERIC                                     
185400         MOVE MFS-NUM-FAELT-FEL     TO MOD-KDMOMSIN-IN-ATTR               
185500         MOVE NEJ TO INDATA-SW                                            
185600       ELSE                                                               
185700         IF MID-KDMOMSIN-IN = '1' OR '2'                                  
185800           IF WS-FLSAMFAK = NEJ                                           
185900             MOVE MFS-NUM-FAELT-RAETT TO MOD-KDMOMSIN-IN-ATTR             
186000           ELSE                                                           
186100             IF MID-KDMOMSIN-IN = SPAR-KDMOMSIN                           
186200               MOVE MFS-NUM-FAELT-RAETT TO MOD-KDMOMSIN-IN-ATTR           
186300             ELSE                                                         
186400               MOVE MFS-NUM-FAELT-FEL TO MOD-KDMOMSIN-IN-ATTR             
186500               MOVE NEJ TO INDATA-SW                                      
186600             END-IF                                                       
186700           END-IF                                                         
186800         ELSE                                                             
186900           MOVE MFS-NUM-FAELT-FEL   TO MOD-KDMOMSIN-IN-ATTR               
187000           MOVE NEJ TO INDATA-SW                                          
187100         END-IF                                                           
187200       END-IF                                                             
187300     END-IF                                                               
187400                                                                          
187500     .                                                                    
187600     EJECT                                                                
187700                                                                          
187800 GC-KONTROLLERA-DELETE SECTION.                                           
187900                                                                          
188000                                                                          
188100**** KONTROLL AV KUNDNR                                                   
188200                                                                          
188300     IF MID-KUNDNR-IN NOT = ALL '+'                                       
188400       IF  MID-KUNDNR-IN (1:3) = 'DEF'                                    
188500       OR  MID-KUNDNR-IN (2:3) = 'DEF'                                    
188600       OR  MID-KUNDNR-IN (3:3) = 'DEF'                                    
188700       OR  MID-KUNDNR-IN (4:3) = 'DEF'                                    
188900         MOVE '999999'            TO MID-KUNDNR-IN                        
189000       END-IF                                                             
189100       IF MID-KUNDNR-IN NOT NUMERIC                                       
189200          MOVE MFS-ALFA-FAELT-FEL TO MOD-KUNDNR-IN-ATTR                   
189300          MOVE NEJ TO INDATA-SW                                           
189400       ELSE                                                               
189500         IF MID-KUNDNR-IN NOT = 999999                                    
189511           MOVE MID-KUNDNR-IN TO W-IDKUNDNR-COPY                          
189700           PERFORM IMS-GU-WDB301-COPY                                     
189800           IF SEGMENT-FINNS                                               
189900             MOVE MFS-ALFA-FAELT-RAETT TO MOD-KUNDNR-IN-ATTR              
190000           ELSE                                                           
190100             MOVE MFS-ALFA-FAELT-FEL TO MOD-KUNDNR-IN-ATTR                
190200             MOVE NEJ TO INDATA-SW                                        
190300             MOVE 'GOODS RECEIVER MISSING ' TO MOD-TEMFSINF               
190400           END-IF                                                         
190500         ELSE                                                             
190600           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KUNDNR-IN-ATTR                
190700           MOVE MFS-ALFA-FAELT-FEL  TO MOD-KDCMD-IN-ATTR                  
190800           MOVE NEJ TO INDATA-SW                                          
190900           MOVE 'DEFAULT ROW CAN NOT BE DELETED ' TO  MOD-TEMFSINF        
191000         END-IF                                                           
191100       END-IF                                                             
191200     ELSE                                                                 
191300       MOVE MFS-ALFA-FAELT-FEL      TO MOD-KUNDNR-IN-ATTR                 
191400       MOVE NEJ TO INDATA-SW                                              
191500     END-IF                                                               
191600     .                                                                    
191700     EJECT                                                                
191800                                                                          
191900 GD-KONTROLLERA-COPY SECTION.                                             
192000                                                                          
192100                                                                          
192200**** KONTROLL AV KUNDNR                                                   
192300                                                                          
192400     IF MID-KUNDNR-IN NOT = ALL '+'                                       
192500       IF  MID-KUNDNR-IN (1:3) = 'DEF'                                    
192600       OR  MID-KUNDNR-IN (2:3) = 'DEF'                                    
192700       OR  MID-KUNDNR-IN (3:3) = 'DEF'                                    
192800       OR  MID-KUNDNR-IN (4:3) = 'DEF'                                    
193000         MOVE '999999'            TO MID-KUNDNR-IN                        
193100       END-IF                                                             
193200       IF MID-KUNDNR-IN NUMERIC                                           
193210         IF MID-KUNDNR-IN = 999999                                        
193211           MOVE 9999999       TO W-IDKUNDNR-COPY                          
193220         ELSE                                                             
193300           MOVE MID-KUNDNR-IN TO W-IDKUNDNR-COPY                          
193310         END-IF                                                           
193400                                                                          
193500         PERFORM IMS-GU-WDB301-COPY                                       
193600         IF SEGMENT-FINNS                                                 
193700           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KUNDNR-IN-ATTR                
193800         ELSE                                                             
193900           MOVE MFS-ALFA-FAELT-FEL  TO MOD-KUNDNR-IN-ATTR                 
194000           MOVE NEJ TO INDATA-SW                                          
194100           MOVE 'GOODS RECEIVER MISSING ' TO MOD-TEMFSINF                 
194200         END-IF                                                           
194300       ELSE                                                               
194400         MOVE MFS-ALFA-FAELT-FEL    TO MOD-KUNDNR-IN-ATTR                 
194500         MOVE NEJ TO INDATA-SW                                            
194600       END-IF                                                             
194700     ELSE                                                                 
194800       MOVE MFS-ALFA-FAELT-FEL      TO MOD-KUNDNR-IN-ATTR                 
194900       MOVE NEJ TO INDATA-SW                                              
195000     END-IF                                                               
195100     .                                                                    
195200     EJECT                                                                
195300                                                                          
195400 GE-KOLLA-WDB5 SECTION.                                                   
195500                                                                          
195600     IF WS-KDGENFRA-VOR > ZERO                                            
195700       MOVE WS-KDGENFRA-VOR TO  W-KDFRAKT-WDB5                            
195800       PERFORM IMS-GU-WDB501                                              
195900       IF SEGMENT-SAKNAS                                                  
196000         MOVE MFS-NUM-FAELT-FEL TO MOD-KDGENFRA-VOR-IN-ATTR               
196100         MOVE NEJ TO INDATA-SW                                            
196200       END-IF                                                             
196300     END-IF                                                               
196400                                                                          
196500     IF WS-KDGENFRA-DO > ZERO                                             
196600       MOVE WS-KDGENFRA-DO  TO  W-KDFRAKT-WDB5                            
196700                                                                          
196800       PERFORM IMS-GU-WDB501                                              
196900       IF SEGMENT-SAKNAS                                                  
197000         MOVE MFS-NUM-FAELT-FEL TO MOD-KDGENFRA-DO-IN-ATTR                
197100         MOVE NEJ TO INDATA-SW                                            
197200       END-IF                                                             
197300     END-IF                                                               
197400                                                                          
197500     IF WS-KDGENFRA-MO > ZERO                                             
197600                                                                          
197700       MOVE WS-KDGENFRA-MO  TO  W-KDFRAKT-WDB5                            
197800       PERFORM IMS-GU-WDB501                                              
197900       IF SEGMENT-SAKNAS                                                  
198000         MOVE MFS-NUM-FAELT-FEL TO MOD-KDGENFRA-MO-IN-ATTR                
198100         MOVE NEJ TO INDATA-SW                                            
198200       END-IF                                                             
198300     END-IF                                                               
198400                                                                          
198500     .                                                                    
198600     EJECT                                                                
198700                                                                          
198800 H-UPPDATERA SECTION.                                                     
198900                                                                          
198910     IF MID-KUNDNR-IN = '999999'                                          
198920        MOVE 9999999       TO W-IDKUNDNR                                  
198930     ELSE                                                                 
198931        MOVE MID-KUNDNR-IN TO W-IDKUNDNR                                  
198940     END-IF                                                               
199100                                                                          
199200     PERFORM IMS-GHU-WDB301                                               
199300     IF SEGMENT-SAKNAS                                                    
199400       IF MID-KDCMD-IN = 'N'                                              
199500         PERFORM HA-ISRT-NEW-CUSTOMER                                     
199600       END-IF                                                             
199700     ELSE                                                                 
199800       IF MID-KDCMD-IN = 'E'                                              
199900         PERFORM HB-REPLACE-CUSTOMER                                      
200000       ELSE                                                               
200100         IF MID-KDCMD-IN = 'D'                                            
200200           PERFORM HC-DELETE-CUSTOMER                                     
200300         ELSE                                                             
200400           PERFORM HD-COPY-CUSTOMER                                       
200500         END-IF                                                           
200600       END-IF                                                             
200700     END-IF                                                               
200800                                                                          
200900     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
201000     CALL WMEDKONV USING MED-WMEDAREA                                     
201100     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
201200     PERFORM MFS-FORM-ATTR                                                
201300*    PERFORM MFS-RENSA-FAELT-IN                                           
201400* * * MFS-ROR-EJ-FAELT TILL FASTA VÄRDEN                                  
201500     .                                                                    
201600     EJECT                                                                
201700                                                                          
201800 HA-ISRT-NEW-CUSTOMER SECTION.                                            
201900                                                                          
202000     MOVE DCS-IDDC        TO GMTB-DC-IDDC                                 
202100     MOVE WS-IDDISTR      TO GMTB-DC-IDDISTR                              
202210     IF MID-KUNDNR-IN = '999999'                                          
202220        MOVE 9999999       TO GMTB-DC-IDKUNDNR                            
202230     ELSE                                                                 
202240        MOVE MID-KUNDNR-IN TO GMTB-DC-IDKUNDNR                            
202250     END-IF                                                               
202300                                                                          
202400     IF MID-IDGMTOMR-IN NOT = ALL '+'                                     
202500       MOVE MID-IDGMTOMR-IN      TO GMTB-DC-IDGMTOMR                      
202600     ELSE                                                                 
202700       MOVE 0000                 TO GMTB-DC-IDGMTOMR                      
202800     END-IF                                                               
203410                                                                          
203420     IF MID-KVDAGAR-TRP-DAY-IN NOT = ALL '+'                              
203430       MOVE MID-KVDAGAR-TRP-DAY-IN TO GMTB-DC-KVDAGAR-TRP-DAY             
203440     ELSE                                                                 
203450       MOVE ZERO                   TO GMTB-DC-KVDAGAR-TRP-DAY             
203460     END-IF                                                               
203470                                                                          
203480     IF MID-IDPKLTAB-IN NOT = ALL '+'                                     
203490       MOVE MID-IDPKLTAB-IN      TO GMTB-DC-IDPKLTAB                      
203491     ELSE                                                                 
203492       MOVE '01'                 TO GMTB-DC-IDPKLTAB                      
203493     END-IF                                                               
203500                                                                          
203600     IF MID-IDPRCTAB-IN NOT = ALL '+'                                     
203700       MOVE MID-IDPRCTAB-IN      TO GMTB-DC-IDPRCTAB                      
203800     ELSE                                                                 
203900       MOVE +1                   TO GMTB-DC-IDPRCTAB                      
204000     END-IF                                                               
204100                                                                          
204200     MOVE MID-KDGENFRA-VOR-IN    TO GMTB-DC-KDGENFRA-VOR                  
204300     MOVE MID-KDGENFRA-DO-IN     TO GMTB-DC-KDGENFRA-DO                   
204400     MOVE MID-KDGENFRA-MO-IN     TO GMTB-DC-KDGENFRA-MO                   
204500                                                                          
204600     MOVE WS-KVLEDTIM-0          TO GMTB-DC-KVLEDTIM-0                    
204700     MOVE WS-KVLEDTIM-1          TO GMTB-DC-KVLEDTIM-1                    
204800     MOVE WS-KVLEDTIM-2          TO GMTB-DC-KVLEDTIM-2                    
204900     MOVE WS-KVLEDTIM-3          TO GMTB-DC-KVLEDTIM-3                    
205000     MOVE WS-KVLEDTIM-4          TO GMTB-DC-KVLEDTIM-4                    
205100                                                                          
205200     IF MID-KDFORSKN-IN NOT = ALL '+'                                     
205300       MOVE MID-KDFORSKN-IN      TO GMTB-DC-KDFORSKN                      
205400     ELSE                                                                 
205500       MOVE +0                   TO GMTB-DC-KDFORSKN                      
205600     END-IF                                                               
205700                                                                          
205800     IF MID-KDSPFKTK-IN NOT = ALL '+'                                     
205900       MOVE MID-KDSPFKTK-IN      TO GMTB-DC-KDSPFKTK                      
206000     ELSE                                                                 
206100       MOVE +0                   TO GMTB-DC-KDSPFKTK                      
206200     END-IF                                                               
206300                                                                          
206400     IF MID-KDTULLVE-IN NOT = ALL '+'                                     
206500       MOVE MID-KDTULLVE-IN      TO GMTB-DC-KDTULLVE                      
206600     ELSE                                                                 
206700       MOVE +0                   TO GMTB-DC-KDTULLVE                      
206800     END-IF                                                               
206900                                                                          
207000     MOVE MID-KDMOMSIN-IN        TO GMTB-DC-KDMOMSIN                      
207100                                                                          
207200     IF MID-KDROPACK-DAG-IN NOT = ALL '+'                                 
207300       MOVE MID-KDROPACK-DAG-IN TO GMTB-DC-KDROPACK-DAG                   
207400     ELSE                                                                 
207500       MOVE ZERO                TO GMTB-DC-KDROPACK-DAG                   
207600     END-IF                                                               
207700                                                                          
207800     IF MID-KDROPACK-BULK-IN NOT = ALL '+'                                
207900       MOVE MID-KDROPACK-BULK-IN TO GMTB-DC-KDROPACK-BULK                 
208000     ELSE                                                                 
208100       MOVE ZERO                 TO GMTB-DC-KDROPACK-BULK                 
208200     END-IF                                                               
208300                                                                          
208400     PERFORM IMS-ISRT-WDB301                                              
208500     PERFORM MFS-RENSA-FAELT-IN                                           
208600     .                                                                    
208700     EJECT                                                                
208800                                                                          
208900 HB-REPLACE-CUSTOMER SECTION.                                             
209000                                                                          
209100*    IF MID-KUNDNR-IN NOT = ALL '+'                                       
209200*      MOVE MID-KUNDNR-IN     TO GMTB-DC-IDKUNDNR                         
209300*    END-IF                                                               
209400                                                                          
209500     IF MID-IDGMTOMR-IN NOT =    ALL '+'                                  
209600       MOVE MID-IDGMTOMR-IN     TO GMTB-DC-IDGMTOMR                       
209700     END-IF                                                               
209800                                                                          
209900     IF MID-KVDAGAR-TRP-DAY-IN NOT = ALL '+'                              
210000       MOVE MID-KVDAGAR-TRP-DAY-IN     TO GMTB-DC-KVDAGAR-TRP-DAY         
210100     END-IF                                                               
210110                                                                          
210120     IF MID-IDPKLTAB-IN NOT = ALL '+'                                     
210130       MOVE MID-IDPKLTAB-IN     TO GMTB-DC-IDPKLTAB                       
210140     END-IF                                                               
210200                                                                          
210300     IF MID-IDPRCTAB-IN NOT = ALL '+'                                     
210400       MOVE MID-IDPRCTAB-IN     TO GMTB-DC-IDPRCTAB                       
210500     END-IF                                                               
210600     IF MID-KDGENFRA-VOR-IN NOT = ALL '+'                                 
210700       MOVE MID-KDGENFRA-VOR-IN TO GMTB-DC-KDGENFRA-VOR                   
210800     END-IF                                                               
210900                                                                          
211000     IF MID-KDGENFRA-DO-IN NOT = ALL '+'                                  
211100       MOVE MID-KDGENFRA-DO-IN  TO GMTB-DC-KDGENFRA-DO                    
211200     END-IF                                                               
211300                                                                          
211400     IF MID-KDGENFRA-MO-IN NOT = ALL '+'                                  
211500       MOVE MID-KDGENFRA-MO-IN  TO GMTB-DC-KDGENFRA-MO                    
211600     END-IF                                                               
211700                                                                          
211800     IF MID-KVLEDTIM-0-IN NOT = ALL '+'                                   
211900       MOVE WS-KVLEDTIM-0       TO GMTB-DC-KVLEDTIM-0                     
212000     END-IF                                                               
212100                                                                          
212200     IF MID-KVLEDTIM-1-IN NOT = ALL '+'                                   
212300       MOVE WS-KVLEDTIM-1       TO GMTB-DC-KVLEDTIM-1                     
212400     END-IF                                                               
212500                                                                          
212600     IF MID-KVLEDTIM-2-IN NOT = ALL '+'                                   
212700       MOVE WS-KVLEDTIM-2       TO GMTB-DC-KVLEDTIM-2                     
212800     END-IF                                                               
212900                                                                          
213000     IF MID-KVLEDTIM-3-IN NOT = ALL '+'                                   
213100       MOVE WS-KVLEDTIM-3       TO GMTB-DC-KVLEDTIM-3                     
213200     END-IF                                                               
213300                                                                          
213400     IF MID-KVLEDTIM-4-IN NOT = ALL '+'                                   
213500       MOVE WS-KVLEDTIM-4       TO GMTB-DC-KVLEDTIM-4                     
213600     END-IF                                                               
213700                                                                          
213800     IF MID-KDROPACK-DAG-IN NOT = ALL '+'                                 
213900       MOVE MID-KDROPACK-DAG-IN  TO GMTB-DC-KDROPACK-DAG                  
214000     END-IF                                                               
214100                                                                          
214200     IF MID-KDROPACK-BULK-IN NOT = ALL '+'                                
214300       MOVE MID-KDROPACK-BULK-IN TO GMTB-DC-KDROPACK-BULK                 
214400     END-IF                                                               
214500                                                                          
214600     IF MID-KDFORSKN-IN    NOT = ALL '+'                                  
214700       MOVE MID-KDFORSKN-IN     TO GMTB-DC-KDFORSKN                       
214800     END-IF                                                               
214900                                                                          
215000     IF MID-KDSPFKTK-IN    NOT = ALL '+'                                  
215100       MOVE MID-KDSPFKTK-IN     TO GMTB-DC-KDSPFKTK                       
215200     END-IF                                                               
215300                                                                          
215400     IF MID-KDTULLVE-IN    NOT = ALL '+'                                  
215500       MOVE MID-KDTULLVE-IN     TO GMTB-DC-KDTULLVE                       
215600     END-IF                                                               
215700                                                                          
215800     IF MID-KDMOMSIN-IN    NOT = ALL '+'                                  
215900       MOVE MID-KDMOMSIN-IN     TO GMTB-DC-KDMOMSIN                       
216000     END-IF                                                               
216100                                                                          
216200     PERFORM IMS-REPL-WDB301                                              
216300     IF W-IDKUNDNR  = 9999999                                             
216400       IF MID-KDFORSKN-IN NOT = ALL '+' OR                                
216500          MID-KDSPFKTK-IN NOT = ALL '+' OR                                
216600          MID-KDTULLVE-IN NOT = ALL '+' OR                                
216700          MID-KDMOMSIN-IN NOT = ALL '+'                                   
216800                                                                          
216900          PERFORM IMS-GU-WDB201-FIRST                                     
217000                                                                          
217100          IF SEGMENT-FINNS                                                
217200          AND GMTA-GMT-FLSAMFAK = JA                                      
217300            PERFORM HBA-REPLACE-ALLA-KUNDER                               
217400          END-IF                                                          
217500                                                                          
217600       END-IF                                                             
217700     END-IF                                                               
217800     PERFORM MFS-RENSA-FAELT-IN                                           
217900                                                                          
218000     .                                                                    
218100     EJECT                                                                
218200                                                                          
218300 HBA-REPLACE-ALLA-KUNDER SECTION.                                         
218400                                                                          
218500     MOVE ZERO TO W-IDKUNDNR-MIN                                          
218600     PERFORM S02-HAEMTA-DEF-UPPG-WDB3                                     
218700     PERFORM IMS-GHU-WDB301-FIRST                                         
218800     PERFORM UNTIL SEGMENT-SAKNAS OR                                      
218900             BAS-SLUT                                                     
219000                                                                          
219100       MOVE WS-KDFORSKN           TO NEXT-DC-KDFORSKN                     
219200       MOVE WS-KDSPFKTK           TO NEXT-DC-KDSPFKTK                     
219300       MOVE WS-KDTULLVE           TO NEXT-DC-KDTULLVE                     
219400       MOVE WS-KDMOMSIN           TO NEXT-DC-KDMOMSIN                     
219500                                                                          
219600       PERFORM IMS-REPL-WDB301-ALL                                        
219700                                                                          
219800       PERFORM IMS-GHN-WDB301                                             
219900     END-PERFORM                                                          
220000     .                                                                    
220100     EJECT                                                                
220200                                                                          
220300 HC-DELETE-CUSTOMER SECTION.                                              
220400                                                                          
220500     PERFORM IMS-DLET-WDB301                                              
220600     PERFORM MFS-RENSA-FAELT-IN                                           
220700     .                                                                    
220800     EJECT                                                                
220900                                                                          
221000 HD-COPY-CUSTOMER SECTION.                                                
221100                                                                          
221200     MOVE MFS-RENSA-FAELT        TO MOD-KUNDNR-IN                         
221300                                    MOD-KDCMD-IN                          
221400     MOVE COPY-DC-IDGMTOMR       TO MOD-IDGMTOMR-IN                       
221500     MOVE COPY-DC-KVDAGAR-TRP-DAY                                         
221501                                 TO MOD-KVDAGAR-TRP-DAY-IN                
221510     MOVE COPY-DC-IDPKLTAB       TO MOD-IDPKLTAB-IN                       
221600     MOVE COPY-DC-IDPRCTAB       TO MOD-IDPRCTAB-IN                       
221700     MOVE COPY-DC-KDGENFRA-VOR   TO WS-KDGENFRA-VOR                       
221800     MOVE WS-KDGENFRA-VOR        TO MOD-KDGENFRA-VOR-IN                   
221900     MOVE COPY-DC-KDGENFRA-DO    TO WS-KDGENFRA-DO                        
222000     MOVE WS-KDGENFRA-DO         TO MOD-KDGENFRA-DO-IN                    
222100     MOVE COPY-DC-KDGENFRA-MO    TO WS-KDGENFRA-MO                        
222200     MOVE WS-KDGENFRA-MO         TO MOD-KDGENFRA-MO-IN                    
222300     MOVE COPY-DC-KVLEDTIM-0     TO WS-KVLEDTIM-0-FIX                     
222400     MOVE WS-KVLEDTIM-0-FIX      TO MOD-KVLEDTIM-0-IN                     
222500     MOVE COPY-DC-KVLEDTIM-1     TO WS-KVLEDTIM-1-FIX                     
222600     MOVE WS-KVLEDTIM-1-FIX      TO MOD-KVLEDTIM-1-IN                     
222700     MOVE COPY-DC-KVLEDTIM-2     TO WS-KVLEDTIM-2-FIX                     
222800     MOVE WS-KVLEDTIM-2-FIX      TO MOD-KVLEDTIM-2-IN                     
222900     MOVE COPY-DC-KVLEDTIM-3     TO WS-KVLEDTIM-3-FIX                     
223000     MOVE WS-KVLEDTIM-3-FIX      TO MOD-KVLEDTIM-3-IN                     
223100     MOVE COPY-DC-KVLEDTIM-4     TO WS-KVLEDTIM-4-FIX                     
223200     MOVE WS-KVLEDTIM-4-FIX      TO MOD-KVLEDTIM-4-IN                     
223300     MOVE COPY-DC-KDFORSKN       TO MOD-KDFORSKN-IN                       
223400     MOVE COPY-DC-KDSPFKTK       TO MOD-KDSPFKTK-IN                       
223500     MOVE COPY-DC-KDTULLVE       TO MOD-KDTULLVE-IN                       
223600     MOVE COPY-DC-KDMOMSIN       TO MOD-KDMOMSIN-IN                       
223700     MOVE COPY-DC-KDROPACK-DAG   TO MOD-KDROPACK-DAG-IN                   
223800     MOVE COPY-DC-KDROPACK-BULK  TO MOD-KDROPACK-BULK-IN                  
223900                                                                          
224000     PERFORM MFS-LAES-IN-IGEN                                             
224100     .                                                                    
224200     EJECT                                                                
224300                                                                          
224400 S01-HAEMTA-UPPG-WDB201 SECTION.                                          
224500                                                                          
224600     PERFORM IMS-GU-WDB201                                                
224700     IF SEGMENT-FINNS                                                     
224800*      IF GMTA-GMT-TISTADAT > ZERO                                        
224900*        IF GMTA-GMT-TISTODAT > ZERO                                      
225000*          MOVE GMTA-GMT-TISTODAT   TO TMP1-YYMMDD                        
225100*          MOVE DAGENS-DATUM        TO TMP2-YYMMDD                        
225200*          PERFORM WY2000P1                                               
225300*          IF TMP1-YYMMDD > TMP2-YYMMDD                                   
225400*            MOVE JA TO GAELLANDE-SW                                      
225500*            MOVE GMTA-GMT-FLSAMFAK TO WS-FLSAMFAK                        
225600*          ELSE                                                           
225700*            MOVE NEJ TO GAELLANDE-SW                                     
225800*          END-IF                                                         
225900*        ELSE                                                             
226000*          MOVE JA TO GAELLANDE-SW                                        
226100*          MOVE GMTA-GMT-FLSAMFAK TO WS-FLSAMFAK                          
226200*        END-IF                                                           
226300*      ELSE                                                               
226400*        MOVE NEJ TO GAELLANDE-SW                                         
226500*      END-IF                                                             
226600       MOVE GMTA-GMT-FLSAMFAK TO WS-FLSAMFAK                              
226700       MOVE JA TO GAELLANDE-SW                                            
226800     ELSE                                                                 
226900       MOVE NEJ               TO GAELLANDE-SW                             
227000     END-IF                                                               
227100     .                                                                    
227200     EJECT                                                                
227300                                                                          
227400 S02-HAEMTA-DEF-UPPG-WDB3 SECTION.                                        
227500                                                                          
227600     PERFORM IMS-GU-WDB301-DEF                                            
227700     IF SEGMENT-FINNS                                                     
227800                                                                          
227900       IF MID-KDFORSKN-IN  NOT = ALL '+'                                  
228000         MOVE MID-KDFORSKN-IN   TO WS-KDFORSKN                            
228100       ELSE                                                               
228200         MOVE DEF-DC-KDFORSKN   TO WS-KDFORSKN                            
228300       END-IF                                                             
228400                                                                          
228500       IF MID-KDSPFKTK-IN  NOT = ALL '+'                                  
228600         MOVE MID-KDSPFKTK-IN   TO WS-KDSPFKTK                            
228700       ELSE                                                               
228800         MOVE DEF-DC-KDSPFKTK   TO WS-KDSPFKTK                            
228900       END-IF                                                             
229000                                                                          
229100       IF MID-KDTULLVE-IN  NOT = ALL '+'                                  
229200         MOVE MID-KDTULLVE-IN   TO WS-KDTULLVE                            
229300       ELSE                                                               
229400         MOVE DEF-DC-KDTULLVE   TO WS-KDTULLVE                            
229500       END-IF                                                             
229600                                                                          
229700       IF MID-KDMOMSIN-IN  NOT = ALL '+'                                  
229800         MOVE MID-KDMOMSIN-IN   TO WS-KDMOMSIN                            
229900       ELSE                                                               
230000         MOVE DEF-DC-KDMOMSIN   TO WS-KDMOMSIN                            
230100       END-IF                                                             
230200     END-IF                                                               
230300     .                                                                    
230400     EJECT                                                                
230500                                                                          
230600 S03-HAEMTA-DEF-UPPG-WDB3 SECTION.                                        
230700                                                                          
230800     PERFORM IMS-GU-WDB301-DEF                                            
230900     IF SEGMENT-FINNS                                                     
231000                                                                          
231100       MOVE DEF-DC-KDFORSKN     TO SPAR-KDFORSKN                          
231200       MOVE DEF-DC-KDSPFKTK     TO SPAR-KDSPFKTK-N                        
231300       MOVE SPAR-KDSPFKTK-X (3:1) TO SPAR-KDSPFKTK                        
231400       MOVE DEF-DC-KDTULLVE     TO SPAR-KDTULLVE                          
231500       MOVE DEF-DC-KDMOMSIN     TO SPAR-KDMOMSIN                          
231600     END-IF                                                               
231700     .                                                                    
231800     EJECT                                                                
231900                                                                          
232000 S10-SAMMA-SIDA-EFTER-UPPDAT SECTION.                                     
232100                                                                          
232200***********FÖR ATT VISA SAMMA SIDA VID UPPDATERING *************          
232300                                                                          
232400     PERFORM MFS-RENSA-FAELT-UT                                           
232500     IF EGEN-MID OR HELP-MID                                              
232600       IF SPAR-IDTRANS = '4415'                                           
232700         MOVE SPAR-WDB301KY-ENTER                                         
232800                             TO NYCKLAR-TILL-BLAEDDRING                   
232900         MOVE W-MINKEY-IDDC        TO W-IDDC                              
233000                                      W-IDDC-MIN                          
233100                                      W-IDDC-MAX                          
233200                                      W-IDDC-WDB5                         
233300                                      W-IDDC-DEF                          
233400                                      W-IDDC-4445                         
233500                                      W-IDDC-4443                         
233600                                                                          
233700         MOVE W-MINKEY-IDDISTR     TO W-IDDISTR                           
233800                                      W-IDDISTR-MIN                       
233900                                      W-IDDISTR-MAX                       
234000                                      W-IDDISTR-WDB5                      
234100                                      W-IDDISTR-WDB2                      
234200                                      W-IDDISTR-WDB2-MIN                  
234300                                      W-IDDISTR-WDB2-MAX                  
234400                                      W-IDDISTR-DEF                       
234500         MOVE W-MINKEY-IDKUNDNR    TO W-IDKUNDNR                          
234600                                      W-IDKUNDNR-MIN                      
234700       END-IF                                                             
234800     END-IF                                                               
234900     .                                                                    
235000     EJECT                                                                
235100                                                                          
235200 MFS-RENSA-FAELT-UT SECTION.                                              
235300                                                                          
235400*    --- ALLA UTDATA-FÄLT                                                 
235500     MOVE MFS-RENSA-FAELT TO MOD-IDGMTOMR-UT                              
235600                             MOD-KVDAGAR-TRP-DAY-UT                       
235610                             MOD-IDPKLTAB-UT                              
235700                             MOD-IDPRCTAB-UT                              
235800                             MOD-KDGENFRA-VOR-UT                          
235900                             MOD-KDGENFRA-DO-UT                           
236000                             MOD-KDGENFRA-MO-UT                           
236100                             MOD-KVLEDTIM-0-UT                            
236200                             MOD-KVLEDTIM-1-UT                            
236300                             MOD-KVLEDTIM-2-UT                            
236400                             MOD-KVLEDTIM-3-UT                            
236500                             MOD-KVLEDTIM-4-UT                            
236600                             MOD-KDFORSKN-UT                              
236700                             MOD-KDSPFKTK-UT                              
236800                             MOD-KDTULLVE-UT                              
236900                             MOD-KDMOMSIN-UT                              
237000                             MOD-KDROPACK-DAG-UT                          
237100                             MOD-KDROPACK-BULK-UT                         
237200     MOVE +1 TO INDX                                                      
237300     PERFORM UNTIL INDX > MAX-INDX                                        
237400       PERFORM MFS-RENSA-RAD-FAELT-UT                                     
237500       ADD +1 TO INDX                                                     
237600     END-PERFORM                                                          
237700     .                                                                    
237800     SKIP3                                                                
237900 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
238000                                                                          
238100     MOVE MFS-RENSA-FAELT  TO MOD-IDKUNDNR      (INDX)                    
238200                              MOD-IDGMTOMR      (INDX)                    
238300                              MOD-KVDAGAR-TRP-DAY(INDX)                   
238310                              MOD-IDPKLTAB      (INDX)                    
238400                              MOD-IDPRCTAB      (INDX)                    
238500                              MOD-KDGENFRA-VOR  (INDX)                    
238600                              MOD-KDGENFRA-DO   (INDX)                    
238700                              MOD-KDGENFRA-MO   (INDX)                    
238800                              MOD-KVLEDTIM-0    (INDX)                    
238900                              MOD-KVLEDTIM-1    (INDX)                    
239000                              MOD-KVLEDTIM-2    (INDX)                    
239100                              MOD-KVLEDTIM-3    (INDX)                    
239200                              MOD-KVLEDTIM-4    (INDX)                    
239300                              MOD-KDFORSKN      (INDX)                    
239400                              MOD-KDSPFKTK      (INDX)                    
239500                              MOD-KDTULLVE      (INDX)                    
239600                              MOD-KDTULLVE      (INDX)                    
239700                              MOD-KDMOMSIN      (INDX)                    
239800                              MOD-KDROPACK-DAG  (INDX)                    
239900                              MOD-KDROPACK-BULK (INDX)                    
240000     .                                                                    
240100     EJECT                                                                
240200                                                                          
240300 MFS-RENSA-FAELT-IN SECTION.                                              
240400                                                                          
240500*    --- ALLA INDATA-FÄLT                                                 
240600     MOVE MFS-RENSA-FAELT TO MOD-KDCMD-IN                                 
240700                             MOD-KUNDNR-IN                                
240800                             MOD-IDGMTOMR-IN                              
240900                             MOD-KVDAGAR-TRP-DAY-IN                       
240910                             MOD-IDPKLTAB-IN                              
241000                             MOD-IDPRCTAB-IN                              
241100                             MOD-KDGENFRA-VOR-IN                          
241200                             MOD-KDGENFRA-DO-IN                           
241300                             MOD-KDGENFRA-MO-IN                           
241400                             MOD-KVLEDTIM-0-IN                            
241500                             MOD-KVLEDTIM-1-IN                            
241600                             MOD-KVLEDTIM-2-IN                            
241700                             MOD-KVLEDTIM-3-IN                            
241800                             MOD-KVLEDTIM-4-IN                            
241900                             MOD-KDFORSKN-IN                              
242000                             MOD-KDSPFKTK-IN                              
242100                             MOD-KDTULLVE-IN                              
242200                             MOD-KDMOMSIN-IN                              
242300                             MOD-KDROPACK-DAG-IN                          
242400                             MOD-KDROPACK-BULK-IN                         
242500     .                                                                    
242600     EJECT                                                                
242700 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
242800                                                                          
242900*    --- ALLA UTDATA-FÄLT                                                 
243000                                                                          
243100*    --- ALLA UTDATA-FÄLT                                                 
243200     MOVE MFS-ROER-EJ-FAELT TO MOD-IDGMTOMR-UT                            
243300                               MOD-KVDAGAR-TRP-DAY-UT                     
243310                               MOD-IDPKLTAB-UT                            
243400                               MOD-IDPRCTAB-UT                            
243500                               MOD-KDGENFRA-VOR-UT                        
243600                               MOD-KDGENFRA-DO-UT                         
243700                               MOD-KDGENFRA-MO-UT                         
243800                               MOD-KVLEDTIM-0-UT                          
243900                               MOD-KVLEDTIM-1-UT                          
244000                               MOD-KVLEDTIM-2-UT                          
244100                               MOD-KVLEDTIM-3-UT                          
244200                               MOD-KVLEDTIM-4-UT                          
244300                               MOD-KDFORSKN-UT                            
244400                               MOD-KDSPFKTK-UT                            
244500                               MOD-KDTULLVE-UT                            
244600                               MOD-KDMOMSIN-UT                            
244700                               MOD-KDROPACK-DAG-UT                        
244800                               MOD-KDROPACK-BULK-UT                       
244900     MOVE +1 TO INDX                                                      
245000     PERFORM UNTIL INDX > MAX-INDX                                        
245100       PERFORM MFS-ROER-EJ-RAD-FAELT-UT                                   
245200       ADD +1 TO INDX                                                     
245300     END-PERFORM                                                          
245400     .                                                                    
245500     SKIP3                                                                
245600 MFS-ROER-EJ-RAD-FAELT-UT SECTION.                                        
245700                                                                          
245800     MOVE MFS-ROER-EJ-FAELT TO MOD-IDKUNDNR      (INDX)                   
245900                               MOD-IDGMTOMR      (INDX)                   
246000                               MOD-KVDAGAR-TRP-DAY (INDX)                 
246040                               MOD-IDPKLTAB      (INDX)                   
246100                               MOD-IDPRCTAB      (INDX)                   
246200                               MOD-KDGENFRA-VOR  (INDX)                   
246300                               MOD-KDGENFRA-DO   (INDX)                   
246400                               MOD-KDGENFRA-MO   (INDX)                   
246500                               MOD-KVLEDTIM-0    (INDX)                   
246600                               MOD-KVLEDTIM-1    (INDX)                   
246700                               MOD-KVLEDTIM-2    (INDX)                   
246800                               MOD-KVLEDTIM-3    (INDX)                   
246900                               MOD-KVLEDTIM-4    (INDX)                   
247000                               MOD-KDFORSKN      (INDX)                   
247100                               MOD-KDSPFKTK      (INDX)                   
247200                               MOD-KDTULLVE      (INDX)                   
247300                               MOD-KDTULLVE      (INDX)                   
247400                               MOD-KDMOMSIN      (INDX)                   
247500                               MOD-KDROPACK-DAG  (INDX)                   
247600                               MOD-KDROPACK-BULK (INDX)                   
247700     .                                                                    
247800     EJECT                                                                
247900                                                                          
248000 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
248100                                                                          
248200*    --- ALLA INDATA-FÄLT                                                 
248300     MOVE MFS-ROER-EJ-FAELT TO MOD-KDCMD-IN                               
248400                               MOD-KUNDNR-IN                              
248500                               MOD-IDGMTOMR-IN                            
248600                               MOD-KVDAGAR-TRP-DAY-IN                     
248610                               MOD-IDPKLTAB-IN                            
248700                               MOD-IDPRCTAB-IN                            
248800                               MOD-KDGENFRA-VOR-IN                        
248900                               MOD-KDGENFRA-DO-IN                         
249000                               MOD-KDGENFRA-MO-IN                         
249100                               MOD-KVLEDTIM-0-IN                          
249200                               MOD-KVLEDTIM-1-IN                          
249300                               MOD-KVLEDTIM-2-IN                          
249400                               MOD-KVLEDTIM-3-IN                          
249500                               MOD-KVLEDTIM-4-IN                          
249600                               MOD-KDFORSKN-IN                            
249700                               MOD-KDSPFKTK-IN                            
249800                               MOD-KDTULLVE-IN                            
249900                               MOD-KDMOMSIN-IN                            
250000                               MOD-KDROPACK-DAG-IN                        
250100                               MOD-KDROPACK-BULK-IN                       
250200     .                                                                    
250300     EJECT                                                                
250400 MFS-FORM-ATTR SECTION.                                                   
250500                                                                          
250600*    --- ALLA INDATA-FÄLT                                                 
250700     MOVE MFS-FORMATETS-ATTR TO MOD-KDCMD-IN-ATTR                         
250800                                MOD-KUNDNR-IN-ATTR                        
250900                                MOD-IDGMTOMR-IN-ATTR                      
251000                                MOD-KVDAGAR-TRP-DAY-IN-ATTR               
251010                                MOD-IDPKLTAB-IN-ATTR                      
251100                                MOD-IDPRCTAB-IN-ATTR                      
251200                                MOD-KDGENFRA-VOR-IN-ATTR                  
251300                                MOD-KDGENFRA-DO-IN-ATTR                   
251400                                MOD-KDGENFRA-MO-IN-ATTR                   
251500                                MOD-KVLEDTIM-0-IN-ATTR                    
251600                                MOD-KVLEDTIM-1-IN-ATTR                    
251700                                MOD-KVLEDTIM-2-IN-ATTR                    
251800                                MOD-KVLEDTIM-3-IN-ATTR                    
251900                                MOD-KVLEDTIM-4-IN-ATTR                    
252000                                MOD-KDFORSKN-IN-ATTR                      
252100                                MOD-KDSPFKTK-IN-ATTR                      
252200                                MOD-KDTULLVE-IN-ATTR                      
252300                                MOD-KDMOMSIN-IN-ATTR                      
252400                                MOD-KDROPACK-DAG-IN-ATTR                  
252500                                MOD-KDROPACK-BULK-IN-ATTR                 
252600     .                                                                    
252700     SKIP2                                                                
252800 MFS-LAES-IN-IGEN SECTION.                                                
252900                                                                          
253000*    --- ALLA INDATA-FÄLT                                                 
253100     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDCMD-IN-ATTR                      
253200                                   MOD-KUNDNR-IN-ATTR                     
253300                                   MOD-IDGMTOMR-IN-ATTR                   
253400                                   MOD-KVDAGAR-TRP-DAY-IN-ATTR            
253410                                   MOD-IDPKLTAB-IN-ATTR                   
253500                                   MOD-IDPRCTAB-IN-ATTR                   
253600                                   MOD-KDGENFRA-VOR-IN-ATTR               
253700                                   MOD-KDGENFRA-DO-IN-ATTR                
253800                                   MOD-KDGENFRA-MO-IN-ATTR                
253900                                   MOD-KVLEDTIM-0-IN-ATTR                 
254000                                   MOD-KVLEDTIM-1-IN-ATTR                 
254100                                   MOD-KVLEDTIM-2-IN-ATTR                 
254200                                   MOD-KVLEDTIM-3-IN-ATTR                 
254300                                   MOD-KVLEDTIM-4-IN-ATTR                 
254400                                   MOD-KDFORSKN-IN-ATTR                   
254500                                   MOD-KDSPFKTK-IN-ATTR                   
254600                                   MOD-KDTULLVE-IN-ATTR                   
254700                                   MOD-KDMOMSIN-IN-ATTR                   
254800                                   MOD-KDROPACK-DAG-IN-ATTR               
254900                                   MOD-KDROPACK-BULK-IN-ATTR              
255000     .                                                                    
255100     EJECT                                                                
255200* --- IMS SEKTIONER ---                                                   
255300     SKIP3                                                                
255400 IMS-GET-MSG SECTION.                                                     
255500                                                                          
255600     MOVE '  QC' TO GODK-STATUSKODER                                      
255700     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
255800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
255900     PERFORM IMS-STATUSKONTROLL                                           
256000     .                                                                    
256100     SKIP3                                                                
256200 IMS-INSERT-MSG SECTION.                                                  
256300                                                                          
256400     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
256500       MOVE 'N' TO MFS-KDHUVOMR                                           
256600     END-IF                                                               
256700     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
256800     MOVE SPACE TO GODK-STATUSKODER                                       
256900     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
257000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
257100     PERFORM IMS-STATUSKONTROLL                                           
257200     .                                                                    
257300     EJECT                                                                
257400 IMS-GU-WDB201 SECTION.                                                   
257500                                                                          
257600     STRING 'WLGMTA01(IDGMT    =' W-IDGMT-X ')'                           
257700          DELIMITED BY SIZE INTO SSA1                                     
257800     MOVE '  GE' TO GODK-STATUSKODER                                      
257900     CALL CBLTDLI USING GU GMTA-PCB DLI-IO-AREA-3 SSA1                    
258000     MOVE GMTA-STATUS-CODE TO STATUS-WS                                   
258100     PERFORM IMS-STATUSKONTROLL                                           
258200     .                                                                    
258300     SKIP3                                                                
258400 IMS-GU-WDB201-FIRST SECTION.                                             
258500                                                                          
258600     STRING 'WLGMTA01(IDGMT   >=' W-IDGMT-MIN-X                           
258700                    '&IDGMT   <=' W-IDGMT-MAX-X ')'                       
258800          DELIMITED BY SIZE INTO SSA1                                     
258900     MOVE '  GE' TO GODK-STATUSKODER                                      
259000     CALL CBLTDLI USING GU GMTA-PCB DLI-IO-AREA-3 SSA1                    
259100     MOVE GMTA-STATUS-CODE TO STATUS-WS                                   
259200     PERFORM IMS-STATUSKONTROLL                                           
259300     .                                                                    
259400     SKIP3                                                                
259500 IMS-GU-WDB501 SECTION.                                                   
259600                                                                          
259700     STRING 'WLGMTC01(WDB501KY =' W-WDB501KY-X ')'                        
259800          DELIMITED BY SIZE INTO SSA1                                     
259900     MOVE '  GE' TO GODK-STATUSKODER                                      
260000     CALL CBLTDLI USING GU GMTC-PCB DLI-IO-AREA-2 SSA1                    
260100     MOVE GMTC-STATUS-CODE TO STATUS-WS                                   
260200     PERFORM IMS-STATUSKONTROLL                                           
260300     .                                                                    
260400     SKIP3                                                                
260500 IMS-GU-WDB301-COPY SECTION.                                              
260600                                                                          
260700     STRING 'WLGMTB01(WDB301KY =' W-WDB301KY-COPY-X ')'                   
260800          DELIMITED BY SIZE INTO SSA1                                     
260900     MOVE '  GE' TO GODK-STATUSKODER                                      
261000     CALL CBLTDLI USING GU GMTB-PCB DLI-IO-AREA-1 SSA1                    
261100     MOVE GMTB-STATUS-CODE TO STATUS-WS                                   
261200     PERFORM IMS-STATUSKONTROLL                                           
261300     .                                                                    
261400     SKIP3                                                                
261500 IMS-GHU-WDB301 SECTION.                                                  
261600                                                                          
261700     STRING 'WLGMTB01(WDB301KY =' W-WDB301KY-X ')'                        
261800          DELIMITED BY SIZE INTO SSA1                                     
261900     MOVE '  GE' TO GODK-STATUSKODER                                      
262000     CALL CBLTDLI USING GHU GMTBU-PCB DLI-IO-AREA SSA1                    
262100     MOVE GMTBU-STATUS-CODE TO STATUS-WS                                  
262200     PERFORM IMS-STATUSKONTROLL                                           
262300     .                                                                    
262400     SKIP3                                                                
262500 IMS-GN-WDB301 SECTION.                                                   
262600                                                                          
262700     STRING 'WLGMTB01(WDB301KY>=' W-WDB301KY-MIN-X                        
262800                    '&WDB301KY<=' W-WDB301KY-MAX-X ')'                    
262900          DELIMITED BY SIZE INTO SSA1                                     
263000     MOVE '  GE' TO GODK-STATUSKODER                                      
263100     CALL CBLTDLI USING GN GMTB-PCB DLI-IO-AREA SSA1                      
263200     MOVE GMTB-STATUS-CODE TO STATUS-WS                                   
263300     PERFORM IMS-STATUSKONTROLL                                           
263400     .                                                                    
263500     SKIP3                                                                
263600 IMS-GHU-WDB301-FIRST SECTION.                                            
263700                                                                          
263800     STRING 'WLGMTB01(WDB301KY>=' W-WDB301KY-MIN-X                        
263900                    '&WDB301KY<=' W-WDB301KY-MAX-X ')'                    
264000          DELIMITED BY SIZE INTO SSA1                                     
264100     MOVE '  GE' TO GODK-STATUSKODER                                      
264200     CALL CBLTDLI USING GHU GMTBU-PCB DLI-IO-AREA-4 SSA1                  
264300     MOVE GMTBU-STATUS-CODE TO STATUS-WS                                  
264400     PERFORM IMS-STATUSKONTROLL                                           
264500     .                                                                    
264600     SKIP3                                                                
264700 IMS-GHN-WDB301 SECTION.                                                  
264800                                                                          
264900     STRING 'WLGMTB01(WDB301KY>=' W-WDB301KY-MIN-X                        
265000                    '&WDB301KY<=' W-WDB301KY-MAX-X ')'                    
265100          DELIMITED BY SIZE INTO SSA1                                     
265200     MOVE '  GE' TO GODK-STATUSKODER                                      
265300     CALL CBLTDLI USING GHN GMTBU-PCB DLI-IO-AREA-4 SSA1                  
265400     MOVE GMTBU-STATUS-CODE TO STATUS-WS                                  
265500     PERFORM IMS-STATUSKONTROLL                                           
265600     .                                                                    
265700     SKIP3                                                                
265800 IMS-GU-WDB301 SECTION.                                                   
265900                                                                          
266000     STRING 'WLGMTB01(WDB301KY>=' W-WDB301KY-MIN-X                        
266100                    '&WDB301KY<=' W-WDB301KY-MAX-X ')'                    
266200          DELIMITED BY SIZE INTO SSA1                                     
266300     MOVE '  GE' TO GODK-STATUSKODER                                      
266400     CALL CBLTDLI USING GU GMTB-PCB DLI-IO-AREA SSA1                      
266500     MOVE GMTB-STATUS-CODE TO STATUS-WS                                   
266600     PERFORM IMS-STATUSKONTROLL                                           
266700     .                                                                    
266800     SKIP3                                                                
266900 IMS-GU-WDB301-DEF SECTION.                                               
267000                                                                          
267100     STRING 'WLGMTB01(WDB301KY =' W-WDB301KY-DEF-X ')'                    
267200          DELIMITED BY SIZE INTO SSA1                                     
267300     MOVE '  GE' TO GODK-STATUSKODER                                      
267400     CALL CBLTDLI USING GU GMTB-PCB DLI-IO-AREA-9 SSA1                    
267500     MOVE GMTB-STATUS-CODE TO STATUS-WS                                   
267600     PERFORM IMS-STATUSKONTROLL                                           
267700     .                                                                    
267800     SKIP3                                                                
267900 IMS-ISRT-WDB301 SECTION.                                                 
268000                                                                          
268100     MOVE 'WLGMTB01 ' TO SSA1                                             
268200     MOVE '  II' TO GODK-STATUSKODER                                      
268300     CALL CBLTDLI USING ISRT GMTBU-PCB DLI-IO-AREA SSA1                   
268400     MOVE GMTBU-STATUS-CODE TO STATUS-WS                                  
268500     PERFORM IMS-STATUSKONTROLL                                           
268600     .                                                                    
268700     SKIP3                                                                
268800 IMS-REPL-WDB301 SECTION.                                                 
268900                                                                          
269000     MOVE '  ' TO GODK-STATUSKODER                                        
269100     CALL CBLTDLI USING REPL GMTBU-PCB DLI-IO-AREA                        
269200     MOVE GMTBU-STATUS-CODE TO STATUS-WS                                  
269300     PERFORM IMS-STATUSKONTROLL                                           
269400     .                                                                    
269500     SKIP3                                                                
269600 IMS-REPL-WDB301-ALL SECTION.                                             
269700                                                                          
269800     MOVE '  ' TO GODK-STATUSKODER                                        
269900     CALL CBLTDLI USING REPL GMTBU-PCB DLI-IO-AREA-4                      
270000     MOVE GMTBU-STATUS-CODE TO STATUS-WS                                  
270100     PERFORM IMS-STATUSKONTROLL                                           
270200     .                                                                    
270300     SKIP3                                                                
270400 IMS-DLET-WDB301 SECTION.                                                 
270500                                                                          
270600     MOVE '  ' TO GODK-STATUSKODER                                        
270700     CALL CBLTDLI USING DLET GMTBU-PCB DLI-IO-AREA                        
270800     MOVE GMTBU-STATUS-CODE TO STATUS-WS                                  
270900     PERFORM IMS-STATUSKONTROLL                                           
271000     .                                                                    
271100     EJECT                                                                
271200 IMS-GU-XXKG-IDPRCTAB-UNIK SECTION.                                       
271300                                                                          
271400     STRING 'WLXXKG01(WDGXKEY  =' W-WDGXKEY-4445-X ')'                    
271500          DELIMITED BY SIZE INTO SSA1                                     
271600     MOVE '  GE' TO GODK-STATUSKODER                                      
271700     CALL CBLTDLI USING GU XXKG-PCB DLI-IO-AREA-4 SSA1                    
271800     MOVE XXKG-STATUS-CODE TO STATUS-WS                                   
271900     PERFORM IMS-STATUSKONTROLL                                           
272000     .                                                                    
272100     SKIP3                                                                
272200 IMS-GU-XXKF-IDPKLTAB-UNIK SECTION.                                       
272300                                                                          
272400     STRING 'WLXXKF01(WDGXKEY  =' W-WDGXKEY-4443-X ')'                    
272500          DELIMITED BY SIZE INTO SSA1                                     
272600     MOVE '  GE' TO GODK-STATUSKODER                                      
272700     CALL CBLTDLI USING GU XXKF-PCB DLI-IO-AREA-4 SSA1                    
272800     MOVE XXKF-STATUS-CODE TO STATUS-WS                                   
272900     PERFORM IMS-STATUSKONTROLL                                           
273000     .                                                                    
273100     SKIP3                                                                
273200 IMS-GU-WDB601    SECTION.                                                
273300     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
273400          DELIMITED BY SIZE INTO SSA1                                     
273500     MOVE '  GE' TO GODK-STATUSKODER                                      
273600     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
273700     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
273800     PERFORM IMS-STATUSKONTROLL                                           
273900     IF SEGMENT-SAKNAS                                                    
274000         MOVE SPACE TO DCS-KDDC                                           
274100     END-IF                                                               
274200     .                                                                    
274300 IMS-STATUSKONTROLL SECTION.                                              
274400                                                                          
274500     SET STATUS-IX TO 1                                                   
274600     SEARCH GODK-STATUS                                                   
274700       AT END                                                             
274800         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
274900         DELIMITED BY SIZE INTO FELTEXT                                   
275000         CALL FELLOG                                                      
275100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
275200         CONTINUE                                                         
275300     END-SEARCH                                                           
275400     .                                                                    
275500     EJECT                                                                
