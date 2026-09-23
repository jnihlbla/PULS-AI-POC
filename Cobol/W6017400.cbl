000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6017400.                                                
000300 AUTHOR.         MARKUS ASPFJÄLL.                                         
000400 DATE-WRITTEN.   99/03/04.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        LÄSER WDE6 BASEN                                                 
000900*        GODKÄNNA INLEVERANS MED DIREKTLEVERANSER                         
001000*                                                                         
001100*        PROGRAMMET UPPDATERAR WDE6                                       
001200*        PROGRAMMET UPPDATERAR WLFILA (WDR6)                              
001300*                                                                         
001400*    INDATA.                                                              
001500*        TRANSAKTION: W6T174                                              
001600*        MID:         W6I17401                                            
001700*                                                                         
001800*    UTDATA.                                                              
001900*        MOD:         W6O17401                                            
002000                                                                          
002100     SKIP3                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     EJECT                                                                
002400 DATA DIVISION.                                                           
002500 WORKING-STORAGE SECTION.                                                 
002600                                                                          
002700*    -- CHECKED BY WY2000                                                 
002800 77  IDPGM                       PIC X(08)   VALUE 'W6017400'.            
002900                                                                          
003000*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003100 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003200                                                                          
003300 77  JA                          PIC X       VALUE 'J'.                   
003400 77  NEJ                         PIC X       VALUE 'N'.                   
003500 77  WS-CDC-11                   PIC X(2)    VALUE '11'.                  
003600                                                                          
003700*    --- INDEX FÖR BLÄDDRINGSRADER                                        
003800 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
003900 77  MAX-INDX                    PIC S9(4)  VALUE +15   COMP SYNC.        
004000 77  MAX-PRODX                   PIC S9(4)  VALUE +10   COMP SYNC.        
004100 77  PRODX                       PIC S9(4)  VALUE +0    COMP SYNC.        
004200 77  RADX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004300*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
004400                                                                          
004500 77  INDATA-SW                   PIC X       VALUE 'J'.                   
004600     88  INDATA-OK                           VALUE 'J'.                   
004700     88  INDATA-FEL                          VALUE 'N'.                   
004800                                                                          
004900 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005000     88  NYCKLAR-OK                          VALUE 'J'.                   
005100     88  NYCKLAR-FEL                         VALUE 'N'.                   
005200                                                                          
005300 77  ALLT-OK-SW                  PIC X       VALUE 'J'.                   
005400     88  ALLT-OK                             VALUE 'J'.                   
005500     88  ALLT-FEL                            VALUE 'N'.                   
005600                                                                          
005700 77  UPDATE-SW                   PIC X       VALUE 'N'.                   
005800     88  UPDATE-OK                           VALUE 'J'.                   
005900     88  UPDATE-NO                           VALUE 'N'.                   
006000                                                                          
006100 77  P-SW                        PIC X       VALUE 'N'.                   
006200     88  PACKAD                              VALUE 'J'.                   
006300                                                                          
006400 77  U-SW                        PIC X       VALUE 'N'.                   
006500     88  UTSKRIVEN                           VALUE 'J'.                   
006600                                                                          
006700 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
006800     88  EGEN-MID                            VALUE '6174'.                
006900     88  GODK-MID                            VALUE '6171' '6172'          
007000                                                   '6173' '6174'          
007100                                                   '6175' '6176'          
007200                                                   '6177' '6178'          
007300                                                   '6179'.                
007400     88  HELP-MID                            VALUE '0551'.                
007500     EJECT                                                                
007600 01  TEST-IDDISTR                 PIC 9(5)   COMP-3.                      
007700 01  FILLER REDEFINES TEST-IDDISTR.                                       
007800*    03 -COPY WWDIST03.                                                   
007900 01     FILLER REDEFINES TEST-IDDISTR.                                    
008000*  03   -COPY WWDIST21.                                                   
008100 01  FILLER    REDEFINES TEST-IDDISTR.                                    
008200*  03   -COPY WWDIST85.                                                   
008300     EJECT                                                                
008400                                                                          
008500 01  WS-SPAR-DASUPREF             PIC 9(8).                               
008600 01  WS-SPAR-TISUPTID             PIC S9(5) COMP-3.                       
008700 01  WS-SPAR-IDSUPREF             PIC X(10).                              
008800 01  WS-SPAR-IDLEVNR              PIC  X(5) VALUE SPACE.                  
008900 01  WS-SPAR-IDPRODNR             PIC S9(7) COMP-3.                       
009000 01  WS-SPAR-KVKOLLI              PIC S9(5) COMP-3.                       
009100 01  CMD-ANTAL                    PIC S9(2)  VALUE +0.                    
009200 01  WS-TISUPTID-X                PIC S9(5) COMP-3.                       
009300*    --- DIV DATUM-/TIDFÄLT                                               
009400 01  WS-DAGENS-KLOCKA.                                                    
009500     03  WS-DAGENS-TTMMSS        PIC 9(6).                                
009600     03  WS-DAGENS-TS            PIC 9(3).                                
009700 01  WS-DAGENS-DATUM.                                                     
009800     03 WS-DAGENS-DATUM-SS       PIC 9(2).                                
009900     03 WS-DAGENS-DATUM-AAMMDD   PIC 9(6).                                
010000 01  WS-DASUPREF.                                                         
010100     03  WS-SEKEL                PIC 9(2)    VALUE ZERO.                  
010200     03  WS-AAMMDD               PIC 9(6)    VALUE ZERO.                  
010300 01  WS-TISUPTID-1               PIC 9(5)    VALUE ZERO.                  
010400 01  WS-TISUPTID.                                                         
010500     03  WS-NOLL                 PIC X.                                   
010600     03  WS-HHMM                 PIC 9(4).                                
010700 01  WS-TID-DEC                  PIC 9(2)V9(2).                           
010800 01  FILLER                      PIC X(16) VALUE 'WS-SECTION '.           
010900 01  WS-SECTION                  PIC X(16).                               
011000 01  FILLER                      PIC X(16) VALUE 'STATUS-WS'.             
011100 01  WS-STATUS                   PIC XX.                                  
011200 01  WS-KVKOLLI                  PIC S9(5) COMP-3.                        
011300     EJECT                                                                
011400*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
011500 01  GENERELLA-SUBPROGRAM.                                                
011600     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
011700     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
011800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
011900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
012000     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
012100     03  W403PLAT                PIC X(8)    VALUE 'W403PLAT'.            
012200     EJECT                                                                
012300*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
012400*01 -COPY WMEDAREA                                                        
012500     SKIP3                                                                
012600 01  MESSAGE-CODES.                                                       
012700     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
012800     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
012900     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
013000     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
013100     03  INF-NO-UPDATE           PIC X(3)    VALUE '034'.                 
013200     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
013300     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
013400     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
013500     03  TRANSPORT-MISSING       PIC X(3)    VALUE '087'.                 
013600     03  ERR-ADDRESS-HANDLING    PIC X(3)    VALUE '730'.                 
013700     03  ITEM-MISSING            PIC X(3)    VALUE '029'.                 
013800     EJECT                                                                
013900                                                                          
014000 01  FILLER                      PIC X(16)   VALUE 'WDATAREA'.            
014100*01  -COPY WDATAREA                                                       
014200     EJECT                                                                
014300*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
014400*                                                                         
014500 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
014600     SKIP3                                                                
014700*01 -COPY WMSGINIT                                                        
014800     EJECT                                                                
014900 01  FILLER                      PIC X(16)   VALUE 'W403PLAT'.            
015000     SKIP3                                                                
015100*01 -COPY W403PLAT                                                        
015200     EJECT                                                                
015300*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
015400*                                                                         
015500 01  FILLER                        PIC X(16)  VALUE 'SPAR-AREA '.         
015600 01  SPAR-AREA.                                                           
015700     03  SPAR-IDTRANS              PIC X(4)    VALUE '6174'.              
015800     03  SPAR-MID-IDLEVNR          PIC X(5)    VALUE SPACE.               
015900     03  SPAR-MID-DASUPREF         PIC X(8).                              
016000     03  SPAR-MID-IDSUPREF         PIC X(10).                             
016100     03  SPAR-IDLEVNR-ENTER        PIC  X(5)   VALUE SPACE.               
016200     03  SPAR-IDLEVNR-NEXT         PIC  X(5)   VALUE SPACE.               
016300     03  SPAR-DASUPREF-ENTER       PIC  9(8).                             
016400     03  SPAR-DASUPREF-NEXT        PIC  9(8).                             
016500     03  SPAR-IDSUPREF-ENTER       PIC X(10).                             
016600     03  SPAR-IDSUPREF-NEXT        PIC X(10).                             
016700                                                                          
016800                                                                          
016900     03  SPAR-TABELL.                                                     
017000       05 SPAR-TABELL OCCURS 15.                                          
017100           07  SPAR-IDLEVNR-TAB          PIC  X(5) VALUE SPACE.           
017200           07  SPAR-DASUPREF-TAB         PIC 9(8).                        
017300           07  SPAR-TISUPTID-TAB         PIC S9(5) COMP-3.                
017400           07  SPAR-IDSUPREF-TAB         PIC X(10).                       
017500           07  SPAR-KVKOLLI-TAB          PIC S9(5) COMP-3.                
017600           07  SPAR-IDPRODNR-TAB         PIC S9(7) COMP-3.                
017700                                                                          
017800     EJECT                                                                
017900*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
018000*                                                                         
018100 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
018200     SKIP3                                                                
018300*01  MID -COPY W6I17401                                                   
018400     EJECT                                                                
018500 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
018600     SKIP3                                                                
018700*01  -COPY WMSGAREA                                                       
018800     EJECT                                                                
018900     03  MOD REDEFINES MSG-AREA.                                          
019000*      05  -COPY W6O17401                                                 
019100     EJECT                                                                
019200 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
019300     SKIP3                                                                
019400*01  -COPY WMFSAREA                                                       
019500     EJECT                                                                
019600 01  FILLER                      PIC X(16)  VALUE '4322-ARB-AREA'.        
019700* ARBETSAREA XXJK-WDGX4322                                                
019800 01  -COPY WDGX4322    -PRE XXJK-                                         
019900*                                                                         
020000*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
020100*                                                                         
020200     EJECT                                                                
020300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
020400     SKIP3                                                                
020500 01  NYCKLAR-TILL-DLI.                                                    
020600*    --- VÄRDE PÅ BLÄDRINGSNYCKEL FÖR FÖRSTA RADEN PÅ SKÄRMEN             
020700                                                                          
020800     03  W-WDE6FSEQ-X.                                                    
020900         05 W-IDLEVNR-X          PIC  X(5)   VALUE SPACE.                 
021000         05 W-DASUPREF-X         PIC 9(8)    VALUE ZERO.                  
021100         05 W-IDSUPREF-X         PIC X(10)   VALUE LOW-VALUE.             
021200                                                                          
021300     03  W-WDE6FSEQ-MAX.                                                  
021400         05 W-IDLEVNR-MAX        PIC  X(5)   VALUE HIGH-VALUE.            
021500         05 W-DASUPREF-MAX       PIC 9(8)    VALUE 99999999.              
021600         05 W-IDSUPREF-MAX       PIC X(10)   VALUE HIGH-VALUE.            
021700                                                                          
021800     03  W-WDE6FSEQ-MIN.                                                  
021900         05 W-IDLEVNR-MIN        PIC  X(5)   VALUE LOW-VALUE.             
022000         05 W-DASUPREF-MIN       PIC 9(8)    VALUE ZERO.                  
022100         05 W-IDSUPREF-MIN       PIC X(10)   VALUE LOW-VALUE.             
022200                                                                          
022300     03  W-IDPRODNR-X.                                                    
022400         05 W-IDPRODNR           PIC S9(7)   VALUE ZERO COMP-3.           
022500                                                                          
022600     03  W-IDKOLLI-X.                                                     
022700         05 W-IDKOLLI            PIC S9(5)   VALUE ZERO COMP-3.           
022800     03  W-KDKOLSTA-X.                                                    
022900         05 W-KDKOLSTA           PIC S9      VALUE ZERO COMP-3.           
023000                                                                          
023100     03  W-4726-WDGXKEY-ROT-X.                                            
023200         05    W-4726-IDHTYP     PIC X(4)    VALUE '4726'.                
023300         05    W-4726-FLBATCH    PIC X(1)    VALUE SPACE.                 
023400         05    W-4726-LOWVALUE   PIC X(25)   VALUE LOW-VALUE.             
023500*                                                                         
023600     03  W-4726-WDGXKEY-UNDSEG-X.                                         
023700         05    W-4726-IDDISTR    PIC S9(5)   COMP-3.                      
023800         05    W-4726-IDKUNDNR   PIC S9(7)   COMP-3.                      
023900         05    W-4726-IDDC       PIC X(2).                                
024000         05    W-4726-KDFAKTYP   PIC X.                                   
024100*                                                                         
024200     03  W-4321-IDHTYP-X.                                                 
024300         05  W-4321-IDHTYP         PIC X(4)  VALUE '4321'.                
024400         05  W-4321-NYCKEL-VALFRI  PIC X(26) VALUE LOW-VALUE.             
024500*                                                                         
024600     EJECT                                                                
024700     SKIP2                                                                
024800*    --- STATUS-KOD FRÅN IMS                                              
024900 01  STATUS-WS                   PIC XX.                                  
025000     88  SEGMENT-FINNS                       VALUE '  '.                  
025100     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
025200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
025300     SKIP2                                                                
025400 01  GODK-STATUSKODER.                                                    
025500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
025600     SKIP3                                                                
025700 01  SSA1                        PIC X(128).                              
025800 01  SSA2                        PIC X(64).                               
025900 01  SSA3                        PIC X(64).                               
026000     EJECT                                                                
026100*    --- IMS FUNKTIONSKODER                                               
026200*01  -COPY W0003                                                          
026300     EJECT                                                                
026400*    ---  DLI INPUT-OUTPUT AREA                                           
026500                                                                          
026600 01  FILLER         PIC X(16) VALUE 'DLI-IO-E601'.                        
026700 01  DLI-IO-E601.                                                         
026800*    03  -COPY WDE601                                                     
026900     EJECT                                                                
027000 01  FILLER         PIC X(16) VALUE 'DLI-IO-E611'.                        
027100 01  DLI-IO-E611.                                                         
027200*    03  -COPY WDE611                                                     
027300     EJECT                                                                
027400 01  FILLER         PIC X(16) VALUE 'DLI-IO-E401'.                        
027500 01  DLI-IO-E401.                                                         
027600*    03  -COPY WDE401                                                     
027700     EJECT                                                                
027800 01  FILLER          PIC X(16) VALUE 'DLI-IO-WLXXDV01'.                   
027900 01  DLI-IO-WLXXDV01 PIC X(500).                                          
028000     EJECT                                                                
028100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLXXDV11'.                    
028200 01  DLI-IO-WLXXDV11.                                                     
028300*    03  -COPY WDGX4726                                                   
028400     EJECT                                                                
028500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLXXDV21'.                    
028600 01  DLI-IO-WLXXDV21.                                                     
028700*    03  -COPY WDGX4727                                                   
028800     EJECT                                                                
028900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLFILA01'.                    
029000*01  WLFILA01    -COPY WDR601                                             
029100*    05 -COPY W46341 -PRE LOGG- -RED FIL-WDR601-DATA                      
029200     EJECT                                                                
029300 01  FILLER         PIC X(16)   VALUE '4322-AREA'.                        
029400 01  -COPY WDGX01      -PRE 4321-                                         
029500 01  -COPY WDGX4322                                                       
029600     EJECT                                                                
029700 LINKAGE SECTION.                                                         
029800*01  -COPY W0009   -PRE MSG-                                              
029900*01  -COPY W0008   -PRE USEA-                                             
030000     05  FILLER                  PIC X.                                   
030100*01  -COPY W0008  -PRE WDE4E-                                             
030200     05  FILLER                  PIC X.                                   
030300*01  -COPY W0008  -PRE WDE6F-                                             
030400     05  FILLER                  PIC X.                                   
030500*01  -COPY W0008  -PRE WDE6-                                              
030600     05  FILLER                  PIC X.                                   
030700*01  -COPY W0008  -PRE XXDV-                                              
030800     05  FILLER                  PIC X.                                   
030900     EJECT                                                                
031000*01  -COPY W0008  -PRE PLATS-DM-                                          
031100     05  FILLER                  PIC X.                                   
031200     EJECT                                                                
031300*01  -COPY W0008  -PRE PLATS-DN-                                          
031400     05  FILLER                  PIC X.                                   
031500     EJECT                                                                
031600*01  -COPY W0008  -PRE PLATS-DP-                                          
031700     05  FILLER                  PIC X.                                   
031800     EJECT                                                                
031900*01  -COPY W0008  -PRE PLATS-DO-                                          
032000     05  FILLER                  PIC X.                                   
032100     EJECT                                                                
032200*01  -COPY W0008  -PRE PLATS-WDE6C-                                       
032300     05  FILLER                  PIC X.                                   
032400     EJECT                                                                
032500*01  -COPY W0008  -PRE PLATS-GMTC-                                        
032600     05  FILLER                  PIC X.                                   
032700     EJECT                                                                
032800*01  -COPY W0008  -PRE PLATS-WDB6-                                        
032900     05  FILLER                  PIC X.                                   
033000     EJECT                                                                
033100*01  -COPY W0008  -PRE FILA-                                              
033200     05  FILLER                  PIC X.                                   
033300     EJECT                                                                
033400*01  -COPY W0008  -PRE 4322-                                              
033500     05  FILLER                  PIC X.                                   
033600     EJECT                                                                
033700 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB WDE4E-PCB WDE6F-PCB           
033800                                   WDE6-PCB                               
033900                           XXDV-PCB PLATS-DM-PCB PLATS-DN-PCB             
034000                           PLATS-DP-PCB PLATS-DO-PCB                      
034100                           PLATS-WDE6C-PCB PLATS-GMTC-PCB                 
034200                           PLATS-WDB6-PCB  FILA-PCB 4322-PCB.             
034300 MAIN SECTION.                                                            
034400     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB WDE4E-PCB WDE6F-PCB           
034500                                   WDE6-PCB                               
034600                           XXDV-PCB PLATS-DM-PCB PLATS-DN-PCB             
034700                           PLATS-DP-PCB PLATS-DO-PCB                      
034800                           PLATS-WDE6C-PCB PLATS-GMTC-PCB                 
034900                           PLATS-WDB6-PCB  FILA-PCB 4322-PCB.             
035000                                                                          
035100     PERFORM IMS-GET-MSG                                                  
035200     IF SEGMENT-FINNS                                                     
035300       PERFORM A-INIT                                                     
035400       PERFORM B-KOLLA-NYCKLAR                                            
035500       IF NYCKLAR-OK                                                      
035600         IF MFS-UPDATE                                                    
035700           PERFORM G-KOLLA-INPUT                                          
035800           IF INDATA-OK                                                   
035900             PERFORM H-UPPDATERA                                          
036000           END-IF                                                         
036100         ELSE                                                             
036200           IF MFS-FIRST                                                   
036300             PERFORM C-FOERSTA-SIDA                                       
036400           ELSE                                                           
036500             IF MFS-NEXT                                                  
036600               PERFORM D-NAESTA-SIDA                                      
036700             ELSE                                                         
036800               PERFORM E-SAMMA-SIDA                                       
036900             END-IF                                                       
037000           END-IF                                                         
037100         END-IF                                                           
037200         IF ALLT-OK                                                       
037300           PERFORM F-LAES-VISA-INFO                                       
037400         END-IF                                                           
037500       END-IF                                                             
037600       COMPUTE MSG-KVLL = LENGTH OF MOD-W6O17401 + 4                      
037700       PERFORM IMS-INSERT-MSG                                             
037800     END-IF                                                               
037900                                                                          
038000     MOVE ZERO TO RETURN-CODE                                             
038100     GOBACK                                                               
038200     .                                                                    
038300     EJECT                                                                
038400 A-INIT SECTION.                                                          
038500                                                                          
038600     IF MSG-DUBBLA-TRANSKODER                                             
038700       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W6I17401                 
038800       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
038900       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
039000     ELSE                                                                 
039100       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W6I17401                  
039200       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
039300       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
039400     END-IF                                                               
039500                                                                          
039600     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
039700     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
039800     MOVE MFS-IDTRANS TO W-IDTRANS                                        
039900                                                                          
040000     MOVE LOW-VALUE TO MSG-AREA                                           
040100     MOVE 'W6O174N1' TO MFS-IDMOD                                         
040200     MOVE '6174' TO MOD-IDTRANS                                           
040300     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
040400                                                                          
040500     IF EGEN-MID OR HELP-MID                                              
040600       CONTINUE                                                           
040700     ELSE                                                                 
040800       MOVE SPACE TO MFS-KDTRTYP                                          
040900       MOVE '7' TO MFS-IDPFK                                              
041000     END-IF                                                               
041100     MOVE 'GB'                            TO MED-IDSKYLT                  
041200     MOVE FUNCTION CURRENT-DATE(1:8)      TO WS-DAGENS-DATUM              
041300     ACCEPT WS-DAGENS-KLOCKA              FROM TIME                       
041400     .                                                                    
041500     EJECT                                                                
041600 B-KOLLA-NYCKLAR SECTION.                                                 
041700                                                                          
041800     MOVE ALL '+'           TO MSGI-WMSGINIT                              
041900     MOVE '001'             TO MSGI-KDCALL                                
042000     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
042100     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
042200     MOVE '6174'            TO MSGI-IDTRANS                               
042300     IF EGEN-MID                                                          
042400         MOVE MID-IDLEVNR-IN     TO MSGI-IDLEVNR                          
042500         MOVE MID-IDLEVNR-IN     TO SPAR-MID-IDLEVNR                      
042600     END-IF                                                               
042700     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
042800                                                                          
042900     MOVE JA TO NYCKLAR-SW                                                
043000     MOVE MSGI-SPAR-AREA TO SPAR-AREA                                     
043100     IF EGEN-MID                                                          
043200       MOVE SPACE              TO SPAR-MID-IDLEVNR                        
043300       MOVE ALL '+'            TO SPAR-MID-DASUPREF                       
043400                                  SPAR-MID-IDSUPREF                       
043500     END-IF                                                               
043600     IF GODK-MID                                                          
043700***    -- KONTROLL AV IDLEVNR                                             
043800       MOVE SPACE           TO MOD-TEMFSFEL                               
043900       MOVE SPACE           TO MOD-TEMFSINF                               
044000       MOVE MFS-RENSA-FAELT TO MOD-IDLEVNR-IN                             
044100                                                                          
044200       IF MID-IDLEVNR-IN NOT = ALL '+'                                    
044300         MOVE '7'         TO MFS-IDPFK                                    
044400         MOVE SPACE       TO MFS-KDTRTYP                                  
044500       END-IF                                                             
044600       MOVE MSGI-IDLEVNR   TO W-IDLEVNR-X                                 
044700                              W-IDLEVNR-MIN                               
044800                              W-IDLEVNR-MAX                               
044900                              SPAR-MID-IDLEVNR                            
045000                              LOGG-IDLEVNR                                
045100                                                                          
045200***   -- KONTROLL AV DASUPREF                                             
045300       IF MID-DASUPREF-IN = ALL '+' AND                                   
045400          MID-DASUPREF-UT NOT = SPACE                                     
045500         MOVE MID-DASUPREF-UT     TO MID-DASUPREF-IN                      
045600       END-IF                                                             
045700                                                                          
045800       INSPECT MID-DASUPREF-IN REPLACING ALL SPACE BY ZERO                
045900                                                                          
046000       MOVE MFS-RENSA-FAELT TO MOD-DASUPREF-IN                            
046100       IF W-IDTRANS = '6175'                                              
046200*        IF SPAR-MID-DASUPREF NOT = ALL '+'                               
046300           MOVE SPAR-MID-DASUPREF(1:6) TO MID-DASUPREF-IN                 
046400           MOVE ALL '+'                TO SPAR-MID-DASUPREF               
046500*        END-IF                                                           
046600       END-IF                                                             
046700       IF MID-DASUPREF-IN NOT = ALL '+'                                   
046800         MOVE '7'         TO MFS-IDPFK                                    
046900         MOVE SPACE       TO MFS-KDTRTYP                                  
047000       END-IF                                                             
047100       IF MSGI-IDLEVNR    NOT = ALL '+' AND                               
047200          MID-DASUPREF-IN NOT = ALL '+'                                   
047300         IF MID-DASUPREF-IN NUMERIC                                       
047400           MOVE 'AAMMDD' TO DAT-KDDATFORM                                 
047500           MOVE MID-DASUPREF-IN TO DAT-I-TIDATUM                          
047600                                                                          
047700           CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                
047800                               DAT-O-TIDATUM DAT-KDSVAR                   
047900                                                                          
048000           IF DAT-KDSVAR-OK                                               
048100            IF MID-DASUPREF-IN(1:2) > 50                                  
048200              MOVE 19            TO W-DASUPREF-X(1:2)                     
048300                                    W-DASUPREF-MIN(1:2)                   
048400                                    W-DASUPREF-MAX(1:2)                   
048500            ELSE                                                          
048600              MOVE 20            TO W-DASUPREF-X(1:2)                     
048700                                    W-DASUPREF-MIN(1:2)                   
048800                                    W-DASUPREF-MAX(1:2)                   
048900            END-IF                                                        
049000            MOVE MID-DASUPREF-IN TO W-DASUPREF-X(3:6)                     
049100                                    W-DASUPREF-MIN(3:6)                   
049200                                    W-DASUPREF-MAX(3:6)                   
049300                                    MOD-DASUPREF-UT                       
049400                                    SPAR-MID-DASUPREF                     
049500           ELSE                                                           
049600            IF DAT-I-TIDATUM NOT = ZERO                                   
049700              MOVE NEJ TO NYCKLAR-SW                                      
049800              MOVE 'FELAKTIGT DATUM' TO MOD-TEMFSFEL                      
049900            END-IF                                                        
050000           END-IF                                                         
050100         ELSE                                                             
050200           MOVE MFS-RENSA-FAELT TO MOD-DASUPREF-UT                        
050300*          MOVE NEJ TO NYCKLAR-SW                                         
050400         END-IF                                                           
050500       ELSE                                                               
050600         MOVE MFS-RENSA-FAELT TO MOD-DASUPREF-UT                          
050700       END-IF                                                             
050800                                                                          
050900***   -- KONTROLL AV IDSUPREF                                             
051000       IF MID-IDSUPREF-IN = ALL '+' AND                                   
051100          MID-IDSUPREF-UT NOT = SPACE                                     
051200         MOVE MID-IDSUPREF-UT     TO MID-IDSUPREF-IN                      
051300       END-IF                                                             
051400                                                                          
051500       IF W-IDTRANS = '6175'                                              
051600*        IF SPAR-MID-IDSUPREF NOT = ALL '+'                               
051700           MOVE SPAR-MID-IDSUPREF TO MID-IDSUPREF-IN                      
051800           MOVE ALL '+'           TO  SPAR-MID-IDSUPREF                   
051900*        END-IF                                                           
052000       END-IF                                                             
052100       IF MID-IDSUPREF-IN NOT = ALL '+'                                   
052200         MOVE '7'         TO MFS-IDPFK                                    
052300         MOVE SPACE       TO MFS-KDTRTYP                                  
052400       END-IF                                                             
052500       MOVE MFS-RENSA-FAELT TO MOD-IDSUPREF-IN                            
052600       IF MSGI-IDLEVNR            = ALL '+' AND                           
052700          MID-IDSUPREF-IN NOT     = ALL '+'                               
052800         MOVE NEJ                 TO NYCKLAR-SW                           
052900       ELSE                                                               
053000         IF MID-IDSUPREF-IN NOT    = ALL '+'                              
053100           MOVE MID-IDSUPREF-IN     TO W-IDSUPREF-X                       
053200                                     W-IDSUPREF-MIN                       
053300                                     W-IDSUPREF-MAX                       
053400                                     MOD-IDSUPREF-UT                      
053500                                     SPAR-MID-IDSUPREF                    
053600         ELSE                                                             
053700           MOVE MFS-RENSA-FAELT   TO MOD-IDSUPREF-UT                      
053800         END-IF                                                           
053900       END-IF                                                             
054000                                                                          
054100       IF GODK-MID OR  NYCKLAR-OK                                         
054200         MOVE MSGI-IDLEVNR        TO MOD-IDLEVNR-UT                       
054300         MOVE MID-DASUPREF-IN     TO MOD-DASUPREF-UT                      
054400         INSPECT MOD-DASUPREF-UT REPLACING LEADING '+'  BY SPACE          
054500         MOVE MID-IDSUPREF-IN     TO MOD-IDSUPREF-UT                      
054600         INSPECT MOD-IDSUPREF-UT REPLACING LEADING '+'  BY SPACE          
054700       ELSE                                                               
054800         MOVE MFS-RENSA-FAELT     TO MOD-IDLEVNR-UT                       
054900                                     MOD-DASUPREF-UT                      
055000                                     MOD-IDSUPREF-UT                      
055100       END-IF                                                             
055200                                                                          
055300       IF NYCKLAR-FEL                                                     
055400        IF MOD-TEMFSFEL  = SPACE                                          
055500         MOVE ERR-WRONG-KEY       TO MED-IDMFSFEL                         
055600         CALL WMEDKONV USING MED-WMEDAREA                                 
055700         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
055800        END-IF                                                            
055900        PERFORM MFS-RENSA-FAELT-IN                                        
056000        PERFORM MFS-RENSA-FAELT-UT                                        
056100       END-IF                                                             
056200     ELSE                                                                 
056300       MOVE NEJ TO NYCKLAR-SW                                             
056400       MOVE 'EJ GODK-MID ' TO MOD-TEMFSINF                                
056500       PERFORM MFS-RENSA-FAELT-IN                                         
056600       PERFORM MFS-RENSA-FAELT-UT                                         
056700     END-IF                                                               
056800     .                                                                    
056900     EJECT                                                                
057000 C-FOERSTA-SIDA SECTION.                                                  
057100                                                                          
057200     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
057300     CALL WMEDKONV USING MED-WMEDAREA                                     
057400     MOVE MED-MFSINF TO MOD-TEMFSFEL                                      
057500                                                                          
057600     PERFORM MFS-RENSA-FAELT-IN                                           
057700     .                                                                    
057800     EJECT                                                                
057900 D-NAESTA-SIDA SECTION.                                                   
058000                                                                          
058100     IF SPAR-IDTRANS = '6174'                                             
058200       MOVE SPAR-IDLEVNR-NEXT TO W-IDLEVNR-MIN                            
058300       MOVE SPAR-DASUPREF-NEXT TO W-DASUPREF-MIN                          
058400       MOVE SPAR-IDSUPREF-NEXT TO W-IDSUPREF-MIN                          
058500     ELSE                                                                 
058600       PERFORM MFS-RENSA-FAELT-IN                                         
058700     END-IF                                                               
058800     .                                                                    
058900     EJECT                                                                
059000 E-SAMMA-SIDA SECTION.                                                    
059100                                                                          
059200     IF SPAR-IDTRANS = '6174' OR '0551'                                   
059300       MOVE SPAR-IDLEVNR-ENTER      TO W-IDLEVNR-MIN                      
059400       MOVE SPAR-DASUPREF-ENTER     TO W-DASUPREF-MIN                     
059500       MOVE SPAR-IDSUPREF-ENTER     TO W-IDSUPREF-MIN                     
059600       MOVE +1 TO INDX                                                    
059700       PERFORM UNTIL INDX > MAX-INDX                                      
059800       IF MID-CMD(INDX) = ALL '+'                                         
059900         MOVE MFS-RENSA-FAELT       TO MOD-CMD(INDX)                      
060000         ADD +1 TO INDX                                                   
060100       ELSE                                                               
060200         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
060300         CALL WMEDKONV USING MED-WMEDAREA                                 
060400         MOVE MED-MFSINF TO MOD-TEMFSFEL                                  
060500         MOVE MFS-ROER-EJ-FAELT     TO MOD-CMD-ATTR(INDX)                 
060600         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-CMD-ATTR(INDX)                 
060700         MOVE 16 TO INDX                                                  
060800       END-IF                                                             
060900       END-PERFORM                                                        
061000     ELSE                                                                 
061100       PERFORM MFS-RENSA-FAELT-IN                                         
061200     END-IF                                                               
061300     .                                                                    
061400     EJECT                                                                
061500 F-LAES-VISA-INFO SECTION.                                                
061600                                                                          
061700     PERFORM FA-LAES-GRUNDDATA                                            
061800                                                                          
061900     IF SEGMENT-SAKNAS                                                    
062000        MOVE ITEM-MISSING TO MED-IDMFSINF                                 
062100        CALL WMEDKONV USING MED-WMEDAREA                                  
062200        MOVE MED-MFSINF   TO MOD-TEMFSFEL                                 
062300*       PERFORM MFS-RENSA-FAELT-UT                                        
062400     ELSE                                                                 
062500      IF SEGMENT-FINNS                                                    
062600       MOVE W-IDLEVNR-MIN              TO W-IDLEVNR-X                     
062700       MOVE W-DASUPREF-MIN             TO W-DASUPREF-X                    
062800       MOVE W-IDSUPREF-MIN             TO W-IDSUPREF-X                    
062900                                                                          
063000       MOVE +1 TO INDX                                                    
063100       MOVE +1 TO WS-SPAR-KVKOLLI                                         
063200       IF SEGMENT-FINNS                                                   
063300        IF MFS-NEXT                                                       
063400         MOVE KOLLI-IDLEVNR            TO SPAR-IDLEVNR-ENTER              
063500         MOVE KOLLI-DASUPREF           TO SPAR-DASUPREF-ENTER             
063600         MOVE KOLLI-IDSUPREF           TO SPAR-IDSUPREF-ENTER             
063700        ELSE                                                              
063800         MOVE W-IDLEVNR-MIN            TO SPAR-IDLEVNR-ENTER              
063900         MOVE W-DASUPREF-MIN           TO SPAR-DASUPREF-ENTER             
064000         MOVE W-IDSUPREF-MIN           TO SPAR-IDSUPREF-ENTER             
064100        END-IF                                                            
064200       ELSE                                                               
064300         MOVE W-IDLEVNR-MIN            TO SPAR-IDLEVNR-ENTER              
064400         MOVE W-DASUPREF-MIN           TO SPAR-DASUPREF-ENTER             
064500         MOVE W-IDSUPREF-MIN           TO SPAR-IDSUPREF-ENTER             
064600       END-IF                                                             
064700       PERFORM FC-LAES-WDE601                                             
064800       MOVE VORD-IDPRODNR            TO SPAR-IDPRODNR-TAB(INDX)           
064900                                          WS-SPAR-IDPRODNR                
065000       PERFORM UNTIL INDX > MAX-INDX                                      
065100         IF SEGMENT-FINNS AND KOLLI-KDKOLSTA = 0                          
065200                                                                          
065300            MOVE KOLLI-DASUPREF        TO WS-SPAR-DASUPREF                
065400            MOVE KOLLI-TISUPTID        TO WS-SPAR-TISUPTID                
065500            MOVE KOLLI-IDSUPREF        TO WS-SPAR-IDSUPREF                
065600            MOVE KOLLI-IDLEVNR         TO SPAR-IDLEVNR-TAB (INDX)         
065700            MOVE WS-SPAR-DASUPREF      TO WS-DASUPREF                     
065800                                          SPAR-DASUPREF-TAB(INDX)         
065900            MOVE WS-AAMMDD             TO MOD-DASUPREF     (INDX)         
066000            MOVE WS-SPAR-TISUPTID      TO WS-TISUPTID-1                   
066100                                          SPAR-TISUPTID-TAB(INDX)         
066200            MOVE WS-TISUPTID-1         TO WS-TISUPTID                     
066300            COMPUTE WS-TID-DEC = WS-HHMM / 100                            
066400            MOVE WS-TID-DEC            TO MOD-TISUPREF     (INDX)         
066500            MOVE WS-SPAR-IDSUPREF      TO MOD-IDSUPREF     (INDX)         
066600                                          SPAR-IDSUPREF-TAB(INDX)         
066700            MOVE WS-SPAR-KVKOLLI       TO MOD-KVKOLLI      (INDX)         
066800                                          SPAR-KVKOLLI-TAB (INDX)         
066900            PERFORM FB-LAES-RADDATA                                       
067000            PERFORM UNTIL WS-SPAR-DASUPREF NOT =                          
067100                          KOLLI-DASUPREF                                  
067200                    OR WS-SPAR-IDSUPREF NOT = KOLLI-IDSUPREF              
067300                    OR SEGMENT-SAKNAS                                     
067400              IF SEGMENT-FINNS AND KOLLI-KDKOLSTA = 0                     
067500                COMPUTE WS-SPAR-KVKOLLI  = WS-SPAR-KVKOLLI + 1            
067600                MOVE WS-SPAR-KVKOLLI  TO MOD-KVKOLLI       (INDX)         
067700              ELSE                                                        
067800                MOVE WS-SPAR-KVKOLLI  TO MOD-KVKOLLI       (INDX)         
067900                                         SPAR-KVKOLLI-TAB  (INDX)         
068000              END-IF                                                      
068100              PERFORM FB-LAES-RADDATA                                     
068200            END-PERFORM                                                   
068300            ADD +1                     TO INDX                            
068400            IF SEGMENT-FINNS AND INDX NOT > MAX-INDX                      
068500             PERFORM FC-LAES-WDE601                                       
068600              MOVE VORD-IDPRODNR       TO SPAR-IDPRODNR-TAB(INDX)         
068700              MOVE VORD-IDPRODNR       TO WS-SPAR-IDPRODNR                
068800              MOVE +1                  TO WS-SPAR-KVKOLLI                 
068900            END-IF                                                        
069000                                                                          
069100         ELSE                                                             
069200           IF SEGMENT-SAKNAS AND INDX = 1                                 
069300            MOVE TRANSPORT-MISSING TO MED-IDMFSINF                        
069400            CALL WMEDKONV USING MED-WMEDAREA                              
069500            MOVE MED-MFSINF TO MOD-TEMFSFEL                               
069600           END-IF                                                         
069700           IF SEGMENT-SAKNAS                                              
069800             PERFORM UNTIL INDX > MAX-INDX                                
069900               MOVE MFS-RENSA-FAELT TO MOD-DASUPREF (INDX)                
070000                                       MOD-TISUPREF (INDX)                
070100                                       MOD-IDSUPREF (INDX)                
070200                                       MOD-KVKOLLI  (INDX)                
070300               ADD +1 TO INDX                                             
070400             END-PERFORM                                                  
070500           ELSE                                                           
070600             PERFORM FB-LAES-RADDATA                                      
070700           END-IF                                                         
070800         END-IF                                                           
070900       END-PERFORM                                                        
071000                                                                          
071100       IF SEGMENT-FINNS                                                   
071200         MOVE KOLLI-IDLEVNR            TO SPAR-IDLEVNR-NEXT               
071300         MOVE KOLLI-DASUPREF           TO SPAR-DASUPREF-NEXT              
071400         MOVE KOLLI-IDSUPREF           TO SPAR-IDSUPREF-NEXT              
071500         MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                        
071600         CALL WMEDKONV USING MED-WMEDAREA                                 
071700         MOVE MED-TEMFSINF TO MOD-TEMFSINF                                
071800       ELSE                                                               
071900         MOVE W-IDLEVNR-MIN            TO SPAR-IDLEVNR-NEXT               
072000         MOVE W-DASUPREF-MIN           TO SPAR-DASUPREF-NEXT              
072100         MOVE W-IDSUPREF-MIN           TO SPAR-IDSUPREF-NEXT              
072200       END-IF                                                             
072300                                                                          
072400       MOVE '002'      TO MSGI-KDCALL                                     
072500       MOVE '6174'     TO SPAR-IDTRANS                                    
072600       MOVE SPAR-AREA  TO MSGI-SPAR-AREA                                  
072700       CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                         
072800      END-IF                                                              
072900     END-IF                                                               
073000     .                                                                    
073100     EJECT                                                                
073200 FA-LAES-GRUNDDATA SECTION.                                               
073300     IF W-IDSUPREF-MIN = W-IDSUPREF-MAX                                   
073400       PERFORM IMS-GET-SUPREF                                             
073500     ELSE                                                                 
073600       PERFORM IMS-GET-KOLLISEQ                                           
073700     END-IF                                                               
073800     .                                                                    
073900     EJECT                                                                
074000                                                                          
074100 FB-LAES-RADDATA SECTION.                                                 
074200     PERFORM IMS-GN-WDE611                                                
074300     .                                                                    
074400     EJECT                                                                
074500                                                                          
074600 FC-LAES-WDE601  SECTION.                                                 
074700     PERFORM IMS-GNP-VORDSEQ                                              
074800     .                                                                    
074900     EJECT                                                                
075000                                                                          
075100 G-KOLLA-INPUT SECTION.                                                   
075200                                                                          
075300     MOVE JA    TO INDATA-SW                                              
075400     MOVE SPACE TO MOD-TEMFSFEL                                           
075500***    -- KONTROLL AV SELECT-RAD                                          
075600     MOVE +1    TO INDX                                                   
075700     PERFORM UNTIL INDX > MAX-INDX                                        
075800       IF MID-CMD (INDX) NOT  = '+' AND ' '                               
075900         IF MID-CMD (INDX)  = 'Y' OR 'J'                                  
076000          MOVE INDX TO RADX                                               
076100          MOVE MFS-ALFA-FAELT-RAETT TO MOD-CMD-ATTR (INDX)                
076200         ELSE                                                             
076300          MOVE NEJ TO INDATA-SW                                           
076400          MOVE MFS-ALFA-FAELT-FEL TO MOD-CMD-ATTR (INDX)                  
076500          MOVE 15 TO INDX                                                 
076600         END-IF                                                           
076700       END-IF                                                             
076800       ADD 1 TO INDX                                                      
076900     END-PERFORM                                                          
077000     IF INDATA-FEL OR RADX = 0                                            
077100      IF MOD-TEMFSFEL = SPACE                                             
077200       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSINF                          
077300       CALL WMEDKONV USING MED-WMEDAREA                                   
077400       MOVE MED-MFSINF TO MOD-TEMFSFEL                                    
077500      END-IF                                                              
077600      PERFORM MFS-ROER-EJ-FAELT-IN                                        
077700      PERFORM MFS-ROER-EJ-FAELT-UT                                        
077800      MOVE NEJ TO INDATA-SW                                               
077900      MOVE NEJ TO ALLT-OK-SW                                              
078000     ELSE                                                                 
078100                                                                          
078200***KONTROLLERA ATT CMD ÄR IFYLLT MEN BARA 1 GÅNG************              
078300       MOVE +1 TO INDX                                                    
078400       MOVE +0 TO CMD-ANTAL                                               
078500       PERFORM UNTIL INDX > MAX-INDX                                      
078600        IF MID-CMD(INDX) = '+' OR ' '                                     
078700          CONTINUE                                                        
078800        ELSE                                                              
078900         IF MID-CMD(INDX) = 'Y' OR 'J'                                    
079000          ADD +1 TO  CMD-ANTAL                                            
079100         END-IF                                                           
079200        END-IF                                                            
079300                                                                          
079400        IF CMD-ANTAL > +1                                                 
079500         IF MID-CMD(INDX) = 'J' OR 'Y'                                    
079600          MOVE MFS-ALFA-FAELT-FEL     TO MOD-CMD-ATTR(INDX)               
079700         END-IF                                                           
079800        ELSE                                                              
079900         MOVE MFS-ALFA-FAELT-RAETT   TO MOD-CMD-ATTR(INDX)                
080000        END-IF                                                            
080100                                                                          
080200       ADD +1 TO INDX                                                     
080300       END-PERFORM                                                        
080400                                                                          
080500       IF CMD-ANTAL > 1                                                   
080600         MOVE NEJ                    TO INDATA-SW                         
080700         MOVE NEJ                    TO ALLT-OK-SW                        
080800         MOVE 'MARKERA BARA EN RAD'  TO MOD-TEMFSINF                      
080900       END-IF                                                             
081000       IF INDATA-FEL                                                      
081100         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                        
081200         CALL WMEDKONV USING MED-WMEDAREA                                 
081300         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
081400         PERFORM MFS-ROER-EJ-FAELT-UT                                     
081500         PERFORM MFS-ROER-EJ-FAELT-IN                                     
081600       END-IF                                                             
081700     END-IF                                                               
081800     .                                                                    
081900     EJECT                                                                
082000 H-UPPDATERA SECTION.                                                     
082100     MOVE RADX TO INDX                                                    
082200     MOVE SPAR-DASUPREF-TAB(INDX)  TO W-DASUPREF-X                        
082300                                      W-DASUPREF-MIN                      
082400                                      W-DASUPREF-MAX                      
082500                                      LOGG-DASUPREF                       
082600     MOVE SPAR-TISUPTID-TAB(INDX)  TO WS-TISUPTID-X                       
082700     MOVE SPAR-IDSUPREF-TAB(INDX)  TO W-IDSUPREF-X                        
082800                                      W-IDSUPREF-MIN                      
082900                                      W-IDSUPREF-MAX                      
083000                                      LOGG-IDSUPREF                       
083100     MOVE SPAR-IDLEVNR-TAB(INDX)   TO W-IDLEVNR-MIN                       
083200                                      W-IDLEVNR-X                         
083300                                      W-IDLEVNR-MAX                       
083400     PERFORM IMS-GU-KOLLISEQ                                              
083500     IF SEGMENT-FINNS                                                     
083600       MOVE KOLLI-IDKOLLI          TO W-IDKOLLI                           
083700                                      LOGG-IDKOLLI                        
083800       PERFORM IMS-GNP-VORDSEQ                                            
083900       MOVE VORD-IDPRODNR          TO W-IDPRODNR                          
084000                                      LOGG-IDPRODNR                       
084100                                                                          
084200       PERFORM UNTIL SEGMENT-SAKNAS                                       
084300         IF KOLLI-DASUPREF = W-DASUPREF-X       AND                       
084400            KOLLI-TISUPTID = WS-TISUPTID-X AND                            
084500            KOLLI-IDSUPREF = W-IDSUPREF-X       AND                       
084600            KOLLI-IDLEVNR       = W-IDLEVNR-X   AND                       
084700            KOLLI-KDVIA         = '01'          AND                       
084800            KOLLI-KDKOLSTA = 0                                            
084900           PERFORM IMS-GHU-KOLLI                                          
085000           PERFORM IMS-GU-WDE401-ESEQ                                     
085100           PERFORM HA-DEFINIERA-PLATS                                     
085200           IF PLATS-KDSVAR NOT = SPACE                                    
085300             MOVE ERR-ADDRESS-HANDLING TO MED-IDMFSFEL                    
085400             CALL WMEDKONV USING MED-WMEDAREA                             
085500             MOVE MED-MFSFEL           TO MOD-TEMFSFEL                    
085600             PERFORM MFS-ROER-EJ-FAELT-IN                                 
085700             PERFORM MFS-ROER-EJ-FAELT-UT                                 
085800           ELSE                                                           
085900             PERFORM HC-UPPDATERA-KOLLI                                   
086000             PERFORM HD-SKAPA-LOGG                                        
086100           END-IF                                                         
086200                                                                          
086300           PERFORM IMS-GHU-VORD                                           
086400           PERFORM UNTIL SEGMENT-SAKNAS                                   
086500             PERFORM IMS-GNP-KOLLI                                        
086600             IF KOLLI-KDKOLSTA = 0                                        
086700               MOVE JA                  TO U-SW                           
086800             ELSE                                                         
086900               IF KOLLI-KDKOLSTA = 1                                      
087000                 MOVE JA                TO P-SW                           
087100               END-IF                                                     
087200             END-IF                                                       
087300           END-PERFORM                                                    
087400           PERFORM IMS-GHU-VORD                                           
087500           MOVE VORD-IDDISTR            TO TEST-IDDISTR                   
087600* ALLA RADER PACKADE                                                      
087700           IF (PACKAD AND NOT UTSKRIVEN) AND                              
087800              (VORD-KVORDRAD = VORD-KVORDRAD-PACK)                        
087900             IF VORD-KDORDSTA NOT = 3                                     
088000               IF VORD-FLAUTFAK = JA AND DIST03-SVERIGE-2                 
088100                 PERFORM HB-NYUPPL-WDGX4726-27                            
088200               END-IF                                                     
088300             END-IF                                                       
088400             MOVE 3                     TO VORD-KDORDSTA                  
088500           END-IF                                                         
088600           ADD +1 TO VORD-KVKOLLI                                         
088700           PERFORM IMS-REPL-VORD                                          
088800         END-IF                                                           
088900         PERFORM IMS-GN-WDE611-STA                                        
089000         IF SEGMENT-FINNS                                                 
089100           MOVE KOLLI-IDKOLLI          TO W-IDKOLLI                       
089200           PERFORM IMS-GNP-VORDSEQ                                        
089300           MOVE VORD-IDPRODNR          TO W-IDPRODNR                      
089400         END-IF                                                           
089500       END-PERFORM                                                        
089600                                                                          
089700     ELSE                                                                 
089800       MOVE NEJ TO UPDATE-SW                                              
089900     END-IF                                                               
090000                                                                          
090100     IF UPDATE-OK                                                         
090200      MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                
090300      CALL WMEDKONV USING MED-WMEDAREA                                    
090400      MOVE MED-MFSINF TO MOD-TEMFSINF                                     
090500      PERFORM MFS-FORM-ATTR                                               
090600      PERFORM MFS-RENSA-FAELT-IN                                          
090700* *   MFS-ROR-EJ-FAELT TILL FASTA VÄRDEN                                  
090800     END-IF                                                               
090900     IF UPDATE-NO                                                         
091000      MOVE INF-NO-UPDATE   TO MED-IDMFSINF                                
091100      CALL WMEDKONV USING MED-WMEDAREA                                    
091200      MOVE MED-MFSINF TO MOD-TEMFSINF                                     
091300      PERFORM MFS-FORM-ATTR                                               
091400      PERFORM MFS-RENSA-FAELT-IN                                          
091500* *   MFS-ROR-EJ-FAELT TILL FASTA VÄRDEN                                  
091600     END-IF                                                               
091700     .                                                                    
091800     EJECT                                                                
091900 HA-DEFINIERA-PLATS SECTION.                                              
092000                                                                          
092100     IF KOLLI-KDORDKL = 4                                                 
092200        MOVE +1                 TO PLATS-KDCALL                           
092300     ELSE                                                                 
092400        MOVE +2                 TO PLATS-KDCALL                           
092500     END-IF                                                               
092600     MOVE WS-CDC-11           TO PLATS-IDDC                               
092700     MOVE KOLLI-IDDISTR       TO PLATS-IDDISTR                            
092800     MOVE KOLLI-IDKUNDNR TO PLATS-IDKUNDNR                                
092900     MOVE VORD-KDFRAKT        TO PLATS-KDFRAKT                            
093000     MOVE KORD-IDORDNR5       TO PLATS-IDORDNR                            
093100     MOVE KOLLI-KDORDKL       TO PLATS-KDORDKLX                           
093200     MOVE ZERO                TO PLATS-DIKOLLIH                           
093300                                 PLATS-DIKOLLIL                           
093400                                 PLATS-DIKOLLIB                           
093500                                 PLATS-VKORDNTO-KOLLI                     
093600     MOVE SPACE               TO PLATS-KDKOLLID                           
093700     MOVE KOLLI-ADFLGEO       TO PLATS-ADFLGEO                            
093800     MOVE KOLLI-ADFLOMR       TO PLATS-ADFLOMR                            
093900     MOVE KOLLI-ADRUTNIV TO PLATS-ADRUTNIV                                
094000     MOVE ZERO                TO PLATS-IDTRPTNR                           
094100                                 PLATS-DIHMODUL                           
094200                                 PLATS-DIDMODUL                           
094300                                 PLATS-ADVMODUL                           
094400                                 PLATS-ADHMODUL                           
094500     MOVE SPACE               TO PLATS-FLUTLAST                           
094600                                 PLATS-IDDC-CROSS                         
094700                                                                          
094800     CALL W403PLAT USING PLATS-W403PLAT                                   
094900                         PLATS-DM-PCB                                     
095000                         PLATS-DN-PCB                                     
095100                         PLATS-DP-PCB                                     
095200                         PLATS-DO-PCB                                     
095300                         PLATS-WDE6C-PCB                                  
095400                         PLATS-GMTC-PCB                                   
095500                         PLATS-WDB6-PCB                                   
095600                                                                          
095700     .                                                                    
095800     EJECT                                                                
095900 HB-NYUPPL-WDGX4726-27 SECTION.                                           
096000                                                                          
096100     MOVE '4726'                  TO W-4726-IDHTYP                        
096200*    IF  DIST03-SVERIGE                                                   
096300*        MOVE JA                  TO W-4726-FLBATCH                       
096400*    ELSE                                                                 
096500         MOVE NEJ                 TO W-4726-FLBATCH                       
096600*    END-IF                                                               
096700     MOVE LOW-VALUE               TO W-4726-LOWVALUE                      
096800     PERFORM IMS-GU-4726-ROT                                              
096900                                                                          
097000     MOVE VORD-IDDISTR            TO W-4726-IDDISTR                       
097100     MOVE VORD-IDKUNDNR           TO W-4726-IDKUNDNR                      
097200     MOVE VORD-IDDC               TO W-4726-IDDC                          
097300     MOVE VORD-KDFAKTYP           TO W-4726-KDFAKTYP                      
097400     PERFORM IMS-GHNP-4726-UNDERSEG                                       
097500                                                                          
097600     IF SEGMENT-SAKNAS                                                    
097700       MOVE VORD-IDDISTR          TO AUTFAKT-IDDISTR                      
097800       MOVE VORD-IDKUNDNR         TO AUTFAKT-IDKUNDNR                     
097900       MOVE VORD-IDDC             TO AUTFAKT-IDDC                         
098000       MOVE VORD-KDFAKTYP         TO AUTFAKT-KDFAKTYP                     
098100       PERFORM IMS-INSERT-4726-UNDERSEG                                   
098200     END-IF                                                               
098300                                                                          
098400     MOVE VORD-IDPRODNR           TO AUTFAKT-IDPRODNR                     
098500     MOVE ZERO                    TO AUTFAKT-IDSKEPPN                     
098600                                     AUTFAKT-PRFRAKT                      
098700     IF  DIST03-SVERIGE                                                   
098800         MOVE NEJ                 TO AUTFAKT-FLLASTA                      
098900     ELSE                                                                 
099000         MOVE JA                  TO AUTFAKT-FLLASTA                      
099100     END-IF                                                               
099200                                                                          
099300     PERFORM IMS-INSERT-4727                                              
099400     .                                                                    
099500     EJECT                                                                
099600 HC-UPPDATERA-KOLLI SECTION.                                              
099700                                                                          
099800     PERFORM IMS-GHU-KOLLI                                                
099900     MOVE PLATS-IDTRPTNR          TO KOLLI-IDTRPTNR                       
100000     MOVE PLATS-ADFLGEO           TO KOLLI-ADFLGEO                        
100100     MOVE PLATS-ADFLOMR           TO KOLLI-ADFLOMR                        
100200     MOVE PLATS-ADRUTNIV          TO KOLLI-ADRUTNIV                       
100300     MOVE PLATS-ADVMODUL          TO KOLLI-ADVMODUL                       
100400     MOVE PLATS-DIDMODUL          TO KOLLI-DIDMODUL                       
100500     MOVE PLATS-DIHMODUL          TO KOLLI-DIHMODUL                       
100600     MOVE PLATS-ADHMODUL          TO KOLLI-ADHMODUL                       
100700     MOVE PLATS-FLUTLAST          TO KOLLI-FLUTLAST                       
100900     MOVE VORD-IDDISTR            TO TEST-IDDISTR                         
101000     IF VORD-FLAUTFAK = JA AND DIST03-SVERIGE-2                           
101100       MOVE NEJ                   TO KOLLI-FLUTLAST                       
101200     END-IF                                                               
101300     MOVE 1                       TO KOLLI-KDKOLSTA                       
101400     MOVE WS-DAGENS-DATUM-AAMMDD  TO KOLLI-TIPACKN                        
101500     MOVE WS-DAGENS-TTMMSS        TO KOLLI-TIPACTID                       
101600     MOVE 'REPL PÅ KOLLI' TO WS-SECTION                                   
101700     PERFORM IMS-REPL-KOLLI                                               
101800     PERFORM S01-SKAPA-TRANS-TILL-SV-AF                                   
101900     MOVE JA TO UPDATE-SW                                                 
102000     .                                                                    
102100     EJECT                                                                
102200                                                                          
102300 HD-SKAPA-LOGG SECTION.                                                   
102400                                                                          
102500     MOVE 'W463'                      TO FIL-CT-IDSYSTEM                  
102600     MOVE 'VIA'                       TO FIL-CT-IDPTYP                    
102700     MOVE ' '                         TO FIL-CT-IDVTYP                    
102800                                                                          
102900     MOVE 'W6017400'                  TO FIL-IDPGM                        
103000     MOVE FUNCTION CURRENT-DATE (3:6) TO FIL-TIREGDAT                     
103100     MOVE FUNCTION CURRENT-DATE (1:8) TO LOGG-DAREGDAT                    
103200     ACCEPT FIL-TIKLOCK               FROM TIME                           
103300     MOVE FIL-TIKLOCK                 TO LOGG-TIREGTID                    
103400     MOVE 1                           TO FIL-IDSEKVNR                     
103500     MOVE 'VIA'                       TO LOGG-IDPTYP                      
103600     MOVE VORD-IDDC                   TO LOGG-IDDC                        
103700     MOVE VORD-IDDISTR                TO LOGG-IDDISTR                     
103800     MOVE VORD-IDKUNDNR               TO LOGG-IDKUNDNR                    
103900     MOVE ZERO                        TO LOGG-IDRADNR                     
104000                                         LOGG-IDARTNR                     
104100                                         LOGG-IDORDNR7                    
104200                                         LOGG-KVANTAL                     
104300     IF KOLLI-TIPACKN > 501231                                            
104400       COMPUTE LOGG-DAPACKN = 19000000 + KOLLI-TIPACKN                    
104500     ELSE                                                                 
104600       COMPUTE LOGG-DAPACKN = 20000000 + KOLLI-TIPACKN                    
104700     END-IF                                                               
104800     MOVE KOLLI-TIPACTID              TO LOGG-TIPACTID                    
104900     COMPUTE LOGG-TISUPTID = KOLLI-TISUPTID / 10                          
105000     MOVE KOLLI-KDORDKL               TO LOGG-KDORDKL                     
105100     MOVE KOLLI-KDVIA                 TO LOGG-KDVIA                       
105200     MOVE ZERO                        TO LOGG-DABEKDAT                    
105300                                         LOGG-DAFAKT                      
105400                                         LOGG-DALEVDAT                    
105500                                         LOGG-DASKEPPN                    
105600                                         LOGG-DASNDDAT                    
105700                                         LOGG-TIBEKR                      
105800                                         LOGG-TISNDTID                    
105900                                         LOGG-KDORDBEK                    
106000     MOVE SPACE                       TO LOGG-BERADREF                    
106100                                                                          
106200     PERFORM IMS-ISRT-LOGG                                                
106300     PERFORM UNTIL SEGMENT-FINNS                                          
106400       ADD +1  TO FIL-IDSEKVNR                                            
106500       PERFORM IMS-ISRT-LOGG                                              
106600     END-PERFORM                                                          
106700     .                                                                    
106800     EJECT                                                                
106900 S01-SKAPA-TRANS-TILL-SV-AF SECTION.                                      
107000                                                                          
107100**   SKAPA INFO TILL SVENSKA ÅF, SÄNDS VIA VR.                            
107200     IF DIST03-SVERIGE-100-799                                            
107300        OR DIST03-NORGE                                                   
107400        OR DIST03-DANMARK-900                                             
107500        OR DIST85-PU-VIA-VR                                               
107600        OR DIST21-TYRE                                                    
107700       MOVE W-IDPRODNR         TO XXJK-4322-IDPRODNR                      
107800       MOVE KOLLI-IDKOLLI      TO XXJK-4322-IDKOLLI                       
107900       MOVE XXJK-4322-WDGX4322 TO 4322-WDGX4322                           
108000       PERFORM IMS-ISRT-4322-SEGM                                         
108100     END-IF                                                               
108200     .                                                                    
108300     EJECT                                                                
108400 MFS-RENSA-FAELT-UT SECTION.                                              
108500                                                                          
108600     MOVE +1 TO INDX                                                      
108700     PERFORM UNTIL INDX > MAX-INDX                                        
108800       MOVE MFS-RENSA-FAELT TO MOD-CMD     (INDX)                         
108900                               MOD-DASUPREF(INDX)                         
109000                               MOD-TISUPREF(INDX)                         
109100                               MOD-IDSUPREF(INDX)                         
109200                               MOD-KVKOLLI (INDX)                         
109300     ADD +1 TO INDX                                                       
109400     END-PERFORM                                                          
109500     .                                                                    
109600     SKIP3                                                                
109700 MFS-RENSA-FAELT-IN SECTION.                                              
109800                                                                          
109900*    --- ALLA INDATA-FÄLT                                                 
110000     MOVE MFS-RENSA-FAELT TO MOD-IDLEVNR-IN                               
110100                             MOD-DASUPREF-IN                              
110200                             MOD-IDSUPREF-IN                              
110300     .                                                                    
110400     EJECT                                                                
110500 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
110600                                                                          
110700*    --- ALLA UTDATA-FÄLT                                                 
110800*    --- INKL BLÄDDRINGSNYCKLAR OCH RAD-DATA                              
110900     MOVE MFS-ROER-EJ-FAELT TO  MOD-IDLEVNR-IN                            
111000                                MOD-DASUPREF-IN                           
111100                                MOD-IDSUPREF-IN                           
111200                                                                          
111300     MOVE +1 TO INDX                                                      
111400     PERFORM UNTIL INDX > MAX-INDX                                        
111500       PERFORM MFS-ROER-EJ-RAD-FAELT-UT                                   
111600       ADD +1 TO INDX                                                     
111700     END-PERFORM                                                          
111800     SKIP2                                                                
111900     .                                                                    
112000     EJECT                                                                
112100 MFS-ROER-EJ-RAD-FAELT-UT  SECTION.                                       
112200                                                                          
112300*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
112400     MOVE MFS-ROER-EJ-FAELT TO MOD-CMD      (INDX)                        
112500                               MOD-DASUPREF (INDX)                        
112600                               MOD-IDSUPREF (INDX)                        
112700                               MOD-KVKOLLI  (INDX)                        
112800     .                                                                    
112900     SKIP3                                                                
113000 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
113100                                                                          
113200*    --- ALLA INDATA-FÄLT                                                 
113300     MOVE MFS-ROER-EJ-FAELT TO MOD-IDLEVNR-IN                             
113400                               MOD-DASUPREF-IN                            
113500                               MOD-IDSUPREF-IN                            
113600                                                                          
113700     .                                                                    
113800     EJECT                                                                
113900 MFS-FORM-ATTR SECTION.                                                   
114000                                                                          
114100*    --- ALLA INDATA-FÄLT                                                 
114200     MOVE +1 TO INDX                                                      
114300     PERFORM UNTIL INDX > MAX-INDX                                        
114400     MOVE MFS-FORMATETS-ATTR TO MOD-CMD-ATTR (INDX)                       
114500     ADD +1 TO INDX                                                       
114600     END-PERFORM                                                          
114700     .                                                                    
114800     SKIP2                                                                
114900* --- IMS SEKTIONER ---                                                   
115000     SKIP3                                                                
115100 IMS-GET-MSG SECTION.                                                     
115200                                                                          
115300     MOVE '  QC' TO GODK-STATUSKODER                                      
115400     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
115500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
115600     PERFORM IMS-STATUSKONTROLL                                           
115700     .                                                                    
115800     SKIP3                                                                
115900 IMS-INSERT-MSG SECTION.                                                  
116000                                                                          
116100*    IF MSGI-IDLAND-SPR = 'SE'                                            
116200*      MOVE 'N' TO MFS-KDHUVOMR                                           
116300*    END-IF                                                               
116400     IF ENGLISH-TEXT                                                      
116500       MOVE 'N' TO MFS-KDHUVOMR                                           
116600     END-IF                                                               
116700                                                                          
116800     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
116900     MOVE SPACE TO GODK-STATUSKODER                                       
117000     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
117100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
117200     PERFORM IMS-STATUSKONTROLL                                           
117300     .                                                                    
117400     EJECT                                                                
117500 IMS-GHU-VORD     SECTION.                                                
117600                                                                          
117700     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
117800          DELIMITED BY SIZE INTO SSA1                                     
117900     MOVE '  GE' TO GODK-STATUSKODER                                      
118000     CALL CBLTDLI USING GHU WDE6-PCB DLI-IO-E601 SSA1                     
118100     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
118200     PERFORM IMS-STATUSKONTROLL                                           
118300     .                                                                    
118400     SKIP3                                                                
118500 IMS-REPL-VORD     SECTION.                                               
118600                                                                          
118700     MOVE '  ' TO GODK-STATUSKODER                                        
118800     CALL CBLTDLI USING REPL WDE6-PCB DLI-IO-E601                         
118900     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
119000     PERFORM IMS-STATUSKONTROLL                                           
119100     .                                                                    
119200     EJECT                                                                
119300 IMS-GHU-KOLLI    SECTION.                                                
119400                                                                          
119500     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
119600          DELIMITED BY SIZE INTO SSA1                                     
119700     STRING 'WDE611  (IDKOLLI  =' W-IDKOLLI-X ')'                         
119800          DELIMITED BY SIZE INTO SSA2                                     
119900     MOVE '  GE' TO GODK-STATUSKODER                                      
120000     CALL CBLTDLI USING GHU WDE6-PCB DLI-IO-E611 SSA1 SSA2                
120100     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
120200     PERFORM IMS-STATUSKONTROLL                                           
120300     .                                                                    
120400     SKIP3                                                                
120500 IMS-GNP-KOLLI    SECTION.                                                
120600                                                                          
120700     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
120800          DELIMITED BY SIZE INTO SSA1                                     
120900     MOVE 'WDE611'            TO SSA2                                     
121000     MOVE '  GE ' TO GODK-STATUSKODER                                     
121100     CALL CBLTDLI USING GNP WDE6-PCB DLI-IO-E611 SSA1 SSA2                
121200     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
121300     PERFORM IMS-STATUSKONTROLL                                           
121400     .                                                                    
121500     SKIP3                                                                
121600 IMS-REPL-KOLLI    SECTION.                                               
121700                                                                          
121800     MOVE '  ' TO GODK-STATUSKODER                                        
121900     CALL CBLTDLI USING REPL WDE6-PCB DLI-IO-E611                         
122000     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
122100     PERFORM IMS-STATUSKONTROLL                                           
122200     .                                                                    
122300     EJECT                                                                
122400 IMS-GU-WDE401-ESEQ SECTION.                                              
122500                                                                          
122600     STRING 'WDE401  (WDE4ESEQ =' W-IDPRODNR-X ')'                        
122700          DELIMITED BY SIZE INTO SSA1                                     
122800     MOVE '  '                 TO GODK-STATUSKODER                        
122900     CALL CBLTDLI USING GU WDE4E-PCB DLI-IO-E401 SSA1                     
123000     MOVE WDE4E-STATUS-CODE  TO STATUS-WS                                 
123100     PERFORM IMS-STATUSKONTROLL                                           
123200     .                                                                    
123300 IMS-GET-KOLLISEQ SECTION.                                                
123400                                                                          
123500     STRING 'WDE611  (WDE6FSEQ>=' W-WDE6FSEQ-MIN                          
123600                    '&WDE6FSEQ<=' W-WDE6FSEQ-MAX                          
123700                    '&KDKOLSTA =' W-KDKOLSTA-X ')'                        
123800          DELIMITED BY SIZE INTO SSA1                                     
123900     MOVE '  GE' TO GODK-STATUSKODER                                      
124000     CALL CBLTDLI USING GU  WDE6F-PCB DLI-IO-E611 SSA1                    
124100     MOVE WDE6F-STATUS-CODE TO STATUS-WS                                  
124200     PERFORM IMS-STATUSKONTROLL                                           
124300     .                                                                    
124400     SKIP3                                                                
124500 IMS-GET-SUPREF   SECTION.                                                
124600                                                                          
124700     STRING 'WDE611  (WDE6FSEQ>=' W-WDE6FSEQ-MIN                          
124800                    '&WDE6FSEQ<=' W-WDE6FSEQ-MAX                          
124900                    '&KDKOLSTA =' W-KDKOLSTA-X                            
125000                    '&IDSUPREF =' W-IDSUPREF-MIN ')'                      
125100          DELIMITED BY SIZE INTO SSA1                                     
125200     MOVE '  GE' TO GODK-STATUSKODER                                      
125300     CALL CBLTDLI USING GU  WDE6F-PCB DLI-IO-E611 SSA1                    
125400     MOVE WDE6F-STATUS-CODE TO STATUS-WS                                  
125500     PERFORM IMS-STATUSKONTROLL                                           
125600     .                                                                    
125700     SKIP3                                                                
125800 IMS-GU-KOLLISEQ SECTION.                                                 
125900                                                                          
126000     STRING 'WDE611  (WDE6FSEQ =' W-WDE6FSEQ-MIN                          
126100                    '&KDKOLSTA =' W-KDKOLSTA-X ')'                        
126200          DELIMITED BY SIZE INTO SSA1                                     
126300     MOVE '  GE' TO GODK-STATUSKODER                                      
126400     CALL CBLTDLI USING GU  WDE6F-PCB DLI-IO-E611 SSA1                    
126500     MOVE WDE6F-STATUS-CODE TO STATUS-WS                                  
126600     PERFORM IMS-STATUSKONTROLL                                           
126700     .                                                                    
126800     SKIP3                                                                
126900 IMS-GN-WDE611 SECTION.                                                   
127000                                                                          
127100     STRING 'WDE611  (WDE6FSEQ>=' W-WDE6FSEQ-MIN                          
127200                    '&WDE6FSEQ<=' W-WDE6FSEQ-MAX ')'                      
127300          DELIMITED BY SIZE INTO SSA1                                     
127400     MOVE '  GEGB' TO GODK-STATUSKODER                                    
127500     CALL CBLTDLI USING GN   WDE6F-PCB DLI-IO-E611 SSA1                   
127600     MOVE WDE6F-STATUS-CODE TO STATUS-WS                                  
127700     PERFORM IMS-STATUSKONTROLL                                           
127800     .                                                                    
127900     SKIP3                                                                
128000 IMS-GN-WDE611-STA SECTION.                                               
128100                                                                          
128200     STRING 'WDE611  (WDE6FSEQ>=' W-WDE6FSEQ-MIN                          
128300                    '&WDE6FSEQ<=' W-WDE6FSEQ-MAX                          
128400                    '&KDKOLSTA =' W-KDKOLSTA-X ')'                        
128500          DELIMITED BY SIZE INTO SSA1                                     
128600     MOVE '  GEGB' TO GODK-STATUSKODER                                    
128700     CALL CBLTDLI USING GN   WDE6F-PCB DLI-IO-E611 SSA1                   
128800     MOVE WDE6F-STATUS-CODE TO STATUS-WS                                  
128900     PERFORM IMS-STATUSKONTROLL                                           
129000     .                                                                    
129100     SKIP3                                                                
129200 IMS-GNP-VORDSEQ SECTION.                                                 
129300                                                                          
129400     MOVE 'WDE601'  TO SSA1                                               
129500     MOVE '  ' TO GODK-STATUSKODER                                        
129600     CALL CBLTDLI USING GNP  WDE6F-PCB DLI-IO-E601 SSA1                   
129700     MOVE WDE6F-STATUS-CODE TO STATUS-WS                                  
129800     PERFORM IMS-STATUSKONTROLL                                           
129900     .                                                                    
130000     SKIP3                                                                
130100 IMS-GU-4726-ROT SECTION.                                                 
130200     STRING 'WLXXDV01(WDGXKEY  =' W-4726-WDGXKEY-ROT-X ')'                
130300            DELIMITED BY SIZE INTO SSA1                                   
130400     MOVE '  ' TO GODK-STATUSKODER                                        
130500     CALL CBLTDLI USING GU     XXDV-PCB DLI-IO-WLXXDV01 SSA1              
130600     MOVE XXDV-STATUS-CODE TO STATUS-WS                                   
130700     PERFORM IMS-STATUSKONTROLL                                           
130800     .                                                                    
130900                                                                          
131000 IMS-GHNP-4726-UNDERSEG SECTION.                                          
131100     STRING 'WLXXDV11(WDGXKEY  =' W-4726-WDGXKEY-UNDSEG-X ')'             
131200            DELIMITED BY SIZE INTO SSA1                                   
131300     MOVE '  GE'           TO GODK-STATUSKODER                            
131400     CALL CBLTDLI USING GHNP   XXDV-PCB DLI-IO-WLXXDV11 SSA1              
131500     MOVE XXDV-STATUS-CODE TO STATUS-WS                                   
131600     PERFORM IMS-STATUSKONTROLL                                           
131700     .                                                                    
131800                                                                          
131900 IMS-INSERT-4726-UNDERSEG SECTION.                                        
132000     STRING 'WLXXDV01(WDGXKEY  =' W-4726-WDGXKEY-ROT-X ')'                
132100            DELIMITED BY SIZE INTO SSA1                                   
132200     MOVE 'WLXXDV11 '      TO SSA2                                        
132300     MOVE '  ' TO GODK-STATUSKODER                                        
132400     CALL CBLTDLI USING ISRT XXDV-PCB DLI-IO-WLXXDV11 SSA1 SSA2           
132500     MOVE XXDV-STATUS-CODE TO STATUS-WS                                   
132600     PERFORM IMS-STATUSKONTROLL                                           
132700     .                                                                    
132800                                                                          
132900 IMS-INSERT-4727 SECTION.                                                 
133000     STRING 'WLXXDV01(WDGXKEY  =' W-4726-WDGXKEY-ROT-X ')'                
133100            DELIMITED BY SIZE INTO SSA1                                   
133200     STRING 'WLXXDV11(WDGXKEY  =' W-4726-WDGXKEY-UNDSEG-X ')'             
133300            DELIMITED BY SIZE INTO SSA2                                   
133400     MOVE 'WLXXDV21 '      TO SSA3                                        
133500     MOVE '  II'           TO GODK-STATUSKODER                            
133600     CALL CBLTDLI USING ISRT XXDV-PCB DLI-IO-WLXXDV21                     
133700                               SSA1 SSA2 SSA3                             
133800     MOVE XXDV-STATUS-CODE TO STATUS-WS                                   
133900     PERFORM IMS-STATUSKONTROLL                                           
134000     .                                                                    
134100                                                                          
134200 IMS-ISRT-LOGG SECTION.                                                   
134300     SKIP2                                                                
134400     MOVE 'WLFILA01 ' TO SSA1                                             
134500     MOVE '  II' TO GODK-STATUSKODER                                      
134600     CALL CBLTDLI USING ISRT FILA-PCB WLFILA01 SSA1                       
134700     MOVE FILA-STATUS-CODE TO STATUS-WS                                   
134800     PERFORM IMS-STATUSKONTROLL                                           
134900     .                                                                    
135000                                                                          
135100 IMS-ISRT-4322-SEGM SECTION.                                              
135200     STRING 'WDG201  (WDGXKEY  =' W-4321-IDHTYP-X ')'                     
135300            DELIMITED BY SIZE INTO SSA1                                   
135400     MOVE 'WDG202  *L' TO SSA2                                            
135500     MOVE '  ' TO GODK-STATUSKODER                                        
135600     CALL CBLTDLI USING ISRT 4322-PCB 4322-WDGX4322 SSA1 SSA2             
135700     MOVE 4322-STATUS-CODE TO STATUS-WS                                   
135800     PERFORM IMS-STATUSKONTROLL                                           
135900     .                                                                    
136000     SKIP2                                                                
136100 IMS-STATUSKONTROLL SECTION.                                              
136200                                                                          
136300     SET STATUS-IX TO 1                                                   
136400     SEARCH GODK-STATUS                                                   
136500       AT END                                                             
136600         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
136700         DELIMITED BY SIZE INTO FELTEXT                                   
136800         CALL FELLOG                                                      
136900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
137000         CONTINUE                                                         
137100     END-SEARCH                                                           
137200     .                                                                    
