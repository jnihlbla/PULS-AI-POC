000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6017600.                                                
000300 AUTHOR.         ANDERSSON BERT.                                          
000400 DATE-WRITTEN.   11/02/09.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        SCREEN FOR SCANNING DDGS BARCODE                                 
000900*        W6017600 REPORTS CASE AS PACKED                                  
001000*                                                                         
001100*        THE PROGRAM UPDATES   WDP7                                       
001200*        THE PROGRAM READS     WDE6F                                      
001300*        THE PROGRAM READS     WDE4A                                      
001400*        THE PROGRAM UPDATES   WDE6                                       
001500*        THE PROGRAM UPDATES   WDG7                                       
001600*        THE PROGRAM UPDATES   WDG2                                       
001700*                                                                         
001800*    INDATA.                                                              
001900*        TRANSACTION: W6T176                                              
002000*        MID:         W6I17601                                            
002100*                                                                         
002200*    OUTDATA.                                                             
002300*        MOD:         W6O17601                                            
002400                                                                          
002500     SKIP3                                                                
002600 ENVIRONMENT DIVISION.                                                    
002700                                                                          
002800 DATA DIVISION.                                                           
002900     EJECT                                                                
003000 WORKING-STORAGE SECTION.                                                 
003100 77  IDPGM                       PIC X(08)   VALUE 'W6017600'.            
003200 77  KDRC-DISPLAY                PIC Z(5).                                
003300 77  CURRENT-TEXT                PIC X(08)   VALUE 'CURRENT'.             
003400 77  WS-CURRENT-SECTION          PIC X(24)   VALUE 'MAIN'.                
003500 77  CURRENT-IMS-SECTION-TEXT    PIC X(08)   VALUE 'IMS-SEC'.             
003600 77  WS-CURRENT-IMS-SECTION      PIC X(24)   VALUE SPACE.                 
003700 77  WS-CURRENT-POS              PIC X(02)   VALUE SPACE.                 
003800 77  W-MED-IDMFSFEL              PIC  X(3)   VALUE SPACE.                 
003900 77  WS-KOLLI-IDDISTR            PIC  9(4)   VALUE ZERO.                  
004000 77  WS-KVKOLLI-NOT-RECIVED      PIC 9(5)    VALUE ZERO.                  
004100 77  WS-KVKOLLI-RECIVED          PIC 9(5)    VALUE ZERO.                  
004200                                                                          
004300*                                                                         
004400*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
004500 77  FILLER                      PIC X(08)   VALUE 'ERRORTEX'.            
004600 77  ERROR-TEXT                  PIC X(48)   VALUE SPACE.                 
004700                                                                          
004800 77  JA                          PIC X       VALUE 'J'.                   
004900 77  YES                         PIC X       VALUE 'J'.                   
005000 77  NEJ                         PIC X       VALUE 'N'.                   
005100 77  NOO                         PIC X       VALUE 'N'.                   
005200                                                                          
005300*    --- INDEX FÖR UPPDATERINGSRADER                                      
005400 77  FILLER                      PIC X(08)  VALUE 'INDXINDX'.             
005500 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
005600 77  MAX-RADINDX                 PIC S9(4)  VALUE +26   COMP SYNC.        
005700 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
005800 77  WS-MID-IDLEVNR              PIC X(5)   VALUE SPACE.                  
005900 77  WS-MID-IDDISTR              PIC X(4)   VALUE ZERO.                   
006000                                                                          
006100 77  WS-INDX                     PIC  9(4)  VALUE ZERO.                   
006200*    --- WORK FIELDS FOR ACTUAL KEYVALUES OF SCREEN                       
006300                                                                          
006400 77  INDATA-SW                   PIC X       VALUE 'J'.                   
006500     88  INDATA-OK                           VALUE 'J'.                   
006600     88  INDATA-WRONG                        VALUE 'N'.                   
006700                                                                          
006800 77  KEYS-SW                     PIC X       VALUE 'J'.                   
006900     88  KEYS-OK                             VALUE 'J'.                   
007000     88  KEYS-WRONG                          VALUE 'N'.                   
007100                                                                          
007200 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
007300     88  OWN-MID                             VALUE '6176'.                
007400     88  GOOD-MID                            VALUE '6174' '6175'          
007500                                                   '6176'.                
007600     88  HELP-MID                            VALUE '0551'.                
007700                                                                          
007800 77  SW-TRAFF                PIC  X(1)        VALUE 'N'.                  
007900     88 TRAFF                                 VALUE 'J'.                  
008000     88 EJ-TRAFF                              VALUE 'N'.                  
008100                                                                          
008200 77  SW-INDATA-FOUND         PIC  X(1)        VALUE 'N'.                  
008300     88 INDATA-FOUND                          VALUE 'J'.                  
008400     88 INDATA-NOT-FOUND                      VALUE 'N'.                  
008500                                                                          
008600 77  LINE-UPDATED-SW             PIC X       VALUE 'N'.                   
008700     88  LINE-UPDATED                        VALUE 'J'.                   
008800                                                                          
008900 77  UPDATE-DONE-SW              PIC X       VALUE 'N'.                   
009000     88  UPDATE-DONE                         VALUE 'J'.                   
009100                                                                          
009200 77  PRINTED-SW                  PIC X       VALUE 'N'.                   
009300     88  PRINTED                             VALUE 'J'.                   
009400                                                                          
009500 77  PACK-REPOTED-SW             PIC X       VALUE 'N'.                   
009600     88  PACK-REPOTED                        VALUE 'J'.                   
009700                                                                          
009800 77  FILLER                      PIC X(08)  VALUE 'SW-E6F'.               
009900 01  SW-E6F                      PIC X       VALUE 'U'.                   
010000     88  WITHOUT-SEARCH-FIELD                VALUE 'U'.                   
010100     88  SEARCH-WITH-IDSUPREF                VALUE 'I'.                   
010200     88  SEARCH-WITH-IDDISTR                 VALUE 'D'.                   
010300     88  SEARCH-WITH-IDSUPREF-IDDISTR        VALUE 'X'.                   
010400                                                                          
010500 01  FILLER                      PIC X(8)   VALUE 'TESTDIST'.             
010600 01     TEST-IDDISTR             PIC 9(5)    COMP-3.                      
010700 01     FILLER REDEFINES TEST-IDDISTR.                                    
010800*  03   -COPY WWDIST13.                                                   
010900 01     FILLER REDEFINES TEST-IDDISTR.                                    
011000*  03   -COPY WWDIST03.                                                   
011100 01     FILLER REDEFINES TEST-IDDISTR.                                    
011200*  03   -COPY WWDIST21.                                                   
011300 01  FILLER    REDEFINES TEST-IDDISTR.                                    
011400*  03   -COPY WWDIST85.                                                   
011500     EJECT                                                                
011600 01  FILLER                  PIC X(16) VALUE 'KONSTANTER'.                
011700*01  KONSTANTER.                                                          
011800*    03  W-FEL-795           PIC  X(3)        VALUE '795'.                
011900*    03  W-FEL-758           PIC  X(3)        VALUE '758'.                
012000                                                                          
012100 77  WS-CDC-11                   PIC X(2)    VALUE '11'.                  
012200                                                                          
012300*    --- DIV DATUM-/TIDFÄLT                                               
012400 01  WS-DAGENS-KLOCKA            PIC 9(9).                                
012500 01  WS-DAGENS-DATUM.                                                     
012600     03 WS-DAGENS-DATUM-SS       PIC 9(2).                                
012700     03 WS-DAGENS-DATUM-AAMMDD   PIC 9(6).                                
012800 01  WS-DAGDAT-AAVVD             PIC 9(5).                                
012900 01  WS-DASUPREF.                                                         
013000     03  WS-SEKEL                PIC 9(2)    VALUE ZERO.                  
013100     03  WS-AAMMDD               PIC 9(6)    VALUE ZERO.                  
013200 01  WS-TITID                    PIC 9(7).                                
013300 01  WS-TITID-ALFA.                                                       
013400     03  WS-NOLL                 PIC X(1).                                
013500     03  WS-HHMM                 PIC 9(4).                                
013600     03  WS-SS                   PIC X(2).                                
013700 01  WS-TITID2                   PIC 9(5).                                
013800 01  WS-TITID2-ALFA.                                                      
013900     03  WS-NOLL                 PIC X(1).                                
014000     03  WS-HHMM2                PIC 9(4).                                
014100 01  WS-HHMM-DEC                 PIC 9(2)V9(2).                           
014200     EJECT                                                                
014300*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
014400 01  GENERAL-SUBPROGRAMS.                                                 
014500     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
014600     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
014700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
014800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
014900     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
015000     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
015100     03  W403PLAT                PIC X(8)    VALUE 'W403PLAT'.            
015200     03 ABEND                   PIC X(8)    VALUE 'ABEND   '.             
015300     EJECT                                                                
015400     SKIP2                                                                
015500 01    ABENDKODER.                                                        
015600     03 FILLER                  PIC X(16) VALUE 'ABENDKODER'.             
015700     03 RKOD-ABEND-UTAN-DUMP    PIC S9(4) COMP SYNC VALUE +16.            
015800     03 RKOD-ABEND-MED-DUMP     PIC S9(4) COMP SYNC VALUE +33.            
015900     03 RKOD-FELTEXT            PIC X(32) VALUE SPACE.                    
016000     EJECT                                                                
016100*    --- PARAMETERS FOR SUBPROGRAM WMEDKONV                               
016200*01 -COPY WMEDAREA                                                        
016300     SKIP3                                                                
016400 01  MESSAGE-CODES.                                                       
016500     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
016600     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
016700     03  ERR-RECORD-MISSING      PIC X(3)    VALUE '029'.                 
016800     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
016900     03  ERR-ADDRESS-HANDLING    PIC X(3)    VALUE '730'.                 
017000     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
017100     03  INF-UPDATE-NOT-DONE     PIC X(3)    VALUE '034'.                 
017200     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
017300     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
017400     EJECT                                                                
017500 01  FILLER                      PIC X(16)   VALUE 'WDARAREA'.            
017600*01  -COPY WDATAREA                                                       
017700     EJECT                                                                
017800 01  FILLER                      PIC X(16)   VALUE 'WDECAREA'.            
017900*01  -COPY WDECAREA                                                       
018000     EJECT                                                                
018100 01  FILLER                      PIC X(16)   VALUE 'W403PLAT '.           
018200*01 -COPY W403PLAT                                                        
018300     EJECT                                                                
018400*    --- PARAMETERS FOR SUB PROGRAM W005INIT                              
018500*                                                                         
018600 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
018700     SKIP3                                                                
018800*01 -COPY WMSGINIT                                                        
018900     EJECT                                                                
019000*    --- AREA CONTAINING DATA TO BE SAVED BETWEEN DIALOG STEPS            
019100*                                                                         
019200 01  FILLER                      PIC X(8)   VALUE 'SPARAREA'.             
019300 01  SPAR-AREA.                                                           
019400     03  SPAR-IDTRANS            PIC X(4)   VALUE '6176'.                 
019500     03  SPAR-MID-DASUPREF.                                               
019600         05 SPAR-MID-SEKEL       PIC 9(2)   VALUE ZERO.                   
019700         05 SPAR-MID-TISUPREF    PIC X(6)   VALUE ZERO.                   
019800     03  SPAR-MID-IDSUPREF       PIC X(10)  VALUE SPACE.                  
019900     SKIP3                                                                
020000*                                                                         
020100*    --- AREAS FOR MFS AND SCREEN MANAGEMENT                              
020200*                                                                         
020300 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
020400     SKIP3                                                                
020500*01  MID -COPY W6I17601                                                   
020600     EJECT                                                                
020700 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
020800     SKIP3                                                                
020900*01  -COPY WMSGAREA                                                       
021000     EJECT                                                                
021100     03  MOD REDEFINES MSG-AREA.                                          
021200*      05  -COPY W6O17601                                                 
021300     EJECT                                                                
021400 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
021500     SKIP3                                                                
021600*01  -COPY WMFSAREA                                                       
021700     EJECT                                                                
021800*    --- WORK-AREAS FOR IMS-SECTIONS                                      
021900*                                                                         
022000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
022100     SKIP3                                                                
022200 01  KEYS-FOR-DLI.                                                        
022300     03  W-IDUSER-X.                                                      
022400         05  W-IDUSER            PIC X(8)    VALUE SPACE.                 
022500                                                                          
022600     03 W-WDE4A1KY-MIN-X.                                                 
022700       05 W-E4A1-IDDISTR-MIN     PIC S9(5)   VALUE ZERO  COMP-3.          
022800       05 W-E4A1-IDKUNDNR-MIN    PIC S9(7)   VALUE ZERO  COMP-3.          
022900       05 W-E4A1-IDKUNDRF-MIN.                                            
023000         07  W-E4A1-IDORDNR-MIN  PIC 9(5)    VALUE ZERO.                  
023100         07  FILLER              PIC X(5)    VALUE SPACE.                 
023200       05 FILLER                 PIC X(6)    VALUE LOW-VALUE.             
023300                                                                          
023400     03 W-WDE4A1KY-MAX-X.                                                 
023500       05 W-E4A1-IDDISTR-MAX     PIC S9(5)   VALUE ZERO  COMP-3.          
023600       05 W-E4A1-IDKUNDNR-MAX    PIC S9(7)   VALUE ZERO  COMP-3.          
023700       05 W-E4A1-IDKUNDRF-MAX.                                            
023800         07  W-E4A1-IDORDNR-MAX  PIC 9(5)    VALUE ZERO.                  
023900         07  FILLER              PIC X(5)    VALUE SPACE.                 
024000       05 FILLER                 PIC X(6)    VALUE HIGH-VALUE.            
024100                                                                          
024200     03 W-WDE401-X.                                                       
024300       05 W-IDDISTR              PIC S9(5)   VALUE ZERO  COMP-3.          
024400       05 W-IDKUNDNR             PIC S9(7)   VALUE ZERO  COMP-3.          
024500       05 W-IDKUNDRF.                                                     
024600         07 W-IDORDNR5           PIC X(5).                                
024700         07 FILLER               PIC X(5).                                
024800*                                                                         
024900     03  W-WDE6F1KY-MIN-X.                                                
025000         05  W-IDLEVNR-MIN       PIC X(5)    VALUE LOW-VALUE.             
025100         05  W-DASUPREF-MIN      PIC 9(8)    VALUE ZERO.                  
025200         05  W-IDSUPREF-MIN      PIC X(10)   VALUE LOW-VALUE.             
025300         05  FILLER              PIC X(7)    VALUE LOW-VALUE.             
025400                                                                          
025500     03  W-WDE6F1KY-MAX-X.                                                
025600         05  W-IDLEVNR-MAX       PIC X(5)    VALUE HIGH-VALUE.            
025700         05  W-DASUPREF-MAX      PIC 9(8)    VALUE 99999999.              
025800         05  W-IDSUPREF-MAX      PIC X(10)   VALUE HIGH-VALUE.            
025900         05  FILLER              PIC X(7)    VALUE HIGH-VALUE.            
026000                                                                          
026100     03  W-IDPRODNR-ASEQ-X.                                               
026200         05  W-IDPRODNR-ASEQ     PIC S9(7)   VALUE ZERO COMP-3.           
026300                                                                          
026400     03  W-IDPRODNR-X.                                                    
026500         05  W-IDPRODNR          PIC S9(7)   VALUE ZERO COMP-3.           
026600                                                                          
026700     03  W-IDKOLLI-X.                                                     
026800         05  W-IDKOLLI           PIC S9(5)   VALUE ZERO COMP-3.           
026900                                                                          
027000     03  W-IDDISTR-F1-X.                                                  
027100         05  W-IDDISTR-F1        PIC S9(5)   VALUE ZERO COMP-3.           
027200                                                                          
027300     03  W-IDSUPREF-F1-X.                                                 
027400         05  W-IDSUPREF-F1       PIC X(10)   VALUE SPACE.                 
027500*                                                                         
027600     03  W-KDSEGKEY-X.                                                    
027610         05 W-KDSEGKEY           PIC X(1)    VALUE '1'.                   
027620*                                                                         
027700     03  W-4726-WDGXKEY-ROT-X.                                            
027800         05    W-4726-IDHTYP     PIC X(4)    VALUE '4726'.                
027900         05    W-4726-FLBATCH    PIC X(1)    VALUE SPACE.                 
028000         05    W-4726-LOWVALUE   PIC X(25)   VALUE LOW-VALUE.             
028100*                                                                         
028200     03  W-4726-WDGXKEY-UNDSEG-X.                                         
028300         05    W-4726-IDDISTR    PIC S9(5)   COMP-3.                      
028400         05    W-4726-IDKUNDNR   PIC S9(7)   COMP-3.                      
028500         05    W-4726-IDDC       PIC X(2).                                
028600         05    W-4726-KDFAKTYP   PIC X.                                   
028700*                                                                         
028800   03    W-4321-IDHTYP-X.                                                 
028900         05  W-4321-IDHTYP         PIC X(4)  VALUE '4321'.                
029000         05  W-4321-NYCKEL-VALFRI  PIC X(26) VALUE LOW-VALUE.             
029100*                                                                         
029200 01  FILLER                      PIC X(08) VALUE 'STATUS-A'.              
029300     SKIP2                                                                
029400*    --- STATUS CODES FROM IMS                                            
029500 01  STATUS-WS                   PIC XX.                                  
029600     88  SEGMENT-FOUND                       VALUE '  '.                  
029700     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
029800     88  SEGMENT-MISSING                     VALUE 'GE'.                  
029900     88  SEGMENT-FINISH                      VALUE 'GB'.                  
030000     SKIP2                                                                
030100 01  GOOD-STATUSCODES.                                                    
030200     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
030300     SKIP3                                                                
030400 01  FILLER                      PIC X(08) VALUE 'SSA-AREA'.              
030500     SKIP3                                                                
030600 01  SSA1                        PIC X(160).                              
030700 01  SSA2                        PIC X(64).                               
030800 01  SSA3                        PIC X(64).                               
030900     EJECT                                                                
031000*    --- IMS FUNCTION CODES                                               
031100*01  -COPY W0003                                                          
031200     EJECT                                                                
031300*    ---  DLI INPUT-OUTPUT AREA                                           
031400                                                                          
031500 01  FILLER         PIC X(16)  VALUE 'DLI-IO-E4A '.                       
031600 01  DLI-IO-WDE4A1.                                                       
031700*    03  -COPY WDE4A1                                                     
031800     EJECT                                                                
031900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE601'.                      
032000 01  DLI-IO-WDE601.                                                       
032100*    03  -COPY WDE601                                                     
032200     EJECT                                                                
032300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE611'.                      
032400 01  DLI-IO-WDE611.                                                       
032500*    03  -COPY WDE611                                                     
032600     EJECT                                                                
032610 01  FILLER               PIC X(16)   VALUE 'WDE621  '.                   
032620 01  DLI-IO-WDE621.                                                       
032630*    03  -COPY WDE621                                                     
032640     EJECT                                                                
032700 01  FILLER          PIC X(16) VALUE 'DLI-IO-WDE6F1'.                     
032800 01  DLI-IO-WDE6F1.                                                       
032900*    03  -COPY WDE6F1                                                     
033000     EJECT                                                                
033100 01  FILLER          PIC X(16) VALUE 'DLI-IO-WDGX4726'.                   
033200 01  DLI-IO-WDGX4726.                                                     
033300*    03  -COPY WDGX4726                                                   
033400     EJECT                                                                
033500 01  FILLER          PIC X(16) VALUE 'DLI-IO-WDGX4727'.                   
033600 01  DLI-IO-WDGX4727.                                                     
033700*    03  -COPY WDGX4727                                                   
033800     EJECT                                                                
033900 01  FILLER          PIC X(16) VALUE 'DLI-IO-WDR601'.                     
034000*01  DLI-IO-WDR601   -COPY WDR601                                         
034100*    05 -COPY W46341 -PRE LOGG- -RED FIL-WDR601-DATA                      
034200     EJECT                                                                
034300 01  FILLER                      PIC X(16)   VALUE '4322-AREA'.           
034400 01  -COPY WDGX01      -PRE 4321-                                         
034500 01  -COPY WDGX4322                                                       
034600     EJECT                                                                
034700 LINKAGE SECTION.                                                         
034800*01  -COPY W0009   -PRE MSG-                                              
034900                                                                          
035000*01  -COPY W0008  -PRE WDP7-                                              
035100     05  FILLER                  PIC X.                                   
035200                                                                          
035300*01  -COPY W0008  -PRE WDE4A-                                             
035400     05  FILLER                  PIC X.                                   
035500                                                                          
035600*01  -COPY W0008  -PRE WDE6-                                              
035700     05  FILLER                  PIC X.                                   
035800                                                                          
035900*01  -COPY W0008  -PRE WDE6F-                                             
036000     05  FILLER                  PIC X.                                   
036100                                                                          
036200*01  -COPY W0008  -PRE WDG7-                                              
036300     05  FILLER                  PIC X.                                   
036400                                                                          
036500*01  -COPY W0008  -PRE WDR6-                                              
036600     05  FILLER                  PIC X.                                   
036700                                                                          
036800*01  -COPY W0008  -PRE WDG2-                                              
036900     05  FILLER                  PIC X.                                   
037000     EJECT                                                                
037100*01  -COPY W0008  -PRE PLATS-DM-                                          
037200     05  FILLER                  PIC X.                                   
037300*01  -COPY W0008  -PRE PLATS-DN-                                          
037400     05  FILLER                  PIC X.                                   
037500*01  -COPY W0008  -PRE PLATS-DP-                                          
037600     05  FILLER                  PIC X.                                   
037700*01  -COPY W0008  -PRE PLATS-DO-                                          
037800     05  FILLER                  PIC X.                                   
037900*01  -COPY W0008  -PRE PLATS-WDE6C-                                       
038000     05  FILLER                  PIC X.                                   
038100*01  -COPY W0008  -PRE PLATS-GMTC-                                        
038200     05  FILLER                  PIC X.                                   
038300     EJECT                                                                
038400*01  -COPY W0008  -PRE PLATS-WDB6-                                        
038500     05  FILLER                  PIC X.                                   
038600     EJECT                                                                
038700 PROCEDURE DIVISION  USING MSG-PCB   WDP7-PCB  WDE4A-PCB WDE6-PCB         
038800                           WDE6F-PCB WDG7-PCB  WDR6-PCB  WDG2-PCB         
038900                     PLATS-DM-PCB PLATS-DN-PCB PLATS-DP-PCB               
039000                     PLATS-DO-PCB PLATS-WDE6C-PCB PLATS-GMTC-PCB          
039100                     PLATS-WDB6-PCB.                                      
039200                                                                          
039300 MAIN SECTION.                                                            
039400     ENTRY 'DLITCBL' USING MSG-PCB   WDP7-PCB  WDE4A-PCB WDE6-PCB         
039500                           WDE6F-PCB WDG7-PCB  WDR6-PCB  WDG2-PCB         
039600                     PLATS-DM-PCB PLATS-DN-PCB PLATS-DP-PCB               
039700                     PLATS-DO-PCB PLATS-WDE6C-PCB PLATS-GMTC-PCB          
039800                     PLATS-WDB6-PCB.                                      
039900                                                                          
040000     PERFORM IMS-GET-MSG                                                  
040100     IF SEGMENT-FOUND                                                     
040200       PERFORM A-INIT                                                     
040300       PERFORM B-CHECK-KEYS                                               
040400       IF KEYS-OK                                                         
040500         IF MFS-UPDATE                                                    
040600         OR MFS-ENTER                                                     
040700           PERFORM C-CHECK-INPUT-AND-UPDATE                               
040800         ELSE                                                             
040900           PERFORM E-READ-AGAIN                                           
041000         END-IF                                                           
041100                                                                          
041200         IF INDATA-OK                                                     
041300           PERFORM F-READ-SHOW-INFO                                       
041400         END-IF                                                           
041500* FÖR ATT SCANNING SKA FUNGERA                                            
041600         IF KEYS-OK                                                       
041700           IF INDX > MAX-RADINDX                                          
041800             MOVE MAX-RADINDX TO INDX                                     
041900           END-IF                                                         
042000           MOVE MFS-FORMAT-DEFAULT-ATTR                                   
042100             TO MOD-IDKOLLI-UPD-ATTR(INDX)                                
042200         END-IF                                                           
042300       END-IF                                                             
042400       COMPUTE MSG-KVLL = LENGTH OF MOD-W6O17601 + 4                      
042500       PERFORM IMS-INSERT-MSG                                             
042600     END-IF                                                               
042700                                                                          
042800     MOVE ZERO TO RETURN-CODE                                             
042900     GOBACK                                                               
043000     .                                                                    
043100     EJECT                                                                
043200 A-INIT SECTION.                                                          
043300     MOVE 'A-INIT         '   TO WS-CURRENT-SECTION                       
043400                                                                          
043500     IF MSG-DOUBLE-TRANSACTIONS                                           
043600       MOVE MSG-INDATA-MINUS-2-TRANSACT   TO MID-W6I17601                 
043700       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
043800       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
043900     ELSE                                                                 
044000       MOVE MSG-INDATA-MINUS-1-TRANSACT  TO MID-W6I17601                  
044100       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
044200       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
044300     END-IF                                                               
044400                                                                          
044500     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
044600     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
044700     MOVE MFS-IDTRANS TO W-IDTRANS                                        
044800                                                                          
044900     MOVE NEJ                    TO UPDATE-DONE-SW                        
045000                                                                          
045100     MOVE SPACE       TO W-MED-IDMFSFEL                                   
045200     MOVE LOW-VALUE TO MSG-AREA                                           
045300     MOVE 'W6O176N1' TO MFS-IDMOD                                         
045400     MOVE '6176' TO MOD-IDTRANS                                           
045500     MOVE MFS-ERASE-FIELD TO MOD-TEMFSFEL MOD-TEMFSINF                    
045600                                                                          
045700     IF OWN-MID OR HELP-MID                                               
045800       CONTINUE                                                           
045900     ELSE                                                                 
046000       MOVE SPACE TO MFS-KDTRTYP                                          
046100       MOVE '7' TO MFS-IDPFK                                              
046200     END-IF                                                               
046300                                                                          
046400     MOVE FUNCTION CURRENT-DATE(1:8)      TO WS-DAGENS-DATUM              
046500     ACCEPT WS-DAGENS-KLOCKA              FROM TIME                       
046600                                                                          
046700     MOVE 'IDAG'      TO DAT-KDDATFORM                                    
046800     CALL WDATKONV USING DAT-KDDATFORM                                    
046900                         DAT-I-TIDATUM                                    
047000                         DAT-O-TIDATUM                                    
047100                         DAT-KDSVAR                                       
047200     MOVE DAT-TIAAVVD TO WS-DAGDAT-AAVVD                                  
047300     .                                                                    
047400     EJECT                                                                
047500 B-CHECK-KEYS SECTION.                                                    
047600     MOVE 'B-CHECK-KEYS   ' TO WS-CURRENT-SECTION                         
047700                                                                          
047800     MOVE ALL '+'               TO MSGI-WMSGINIT                          
047900     MOVE '001'                 TO MSGI-KDCALL                            
048000     MOVE MSG-LTERM-NAME        TO MSGI-IDLTERM-USER                      
048100     MOVE MSG-SIGNON-USERID     TO MSGI-IDUSER                            
048200     MOVE '6176'                TO MSGI-IDTRANS                           
048300                                                                          
048400     IF OWN-MID                                                           
048500       MOVE MID-IDLEVNR-IN      TO MSGI-IDLEVNR                           
048600                                                                          
048700       MOVE MID-TISUPREF-IN     TO MSGI-DASUPREF(3:6)                     
048800       IF MID-TISUPREF-IN = ALL '+'                                       
048900         MOVE '++'              TO MSGI-DASUPREF(1:2)                     
049000       ELSE                                                               
049100         MOVE '20'              TO MSGI-DASUPREF(1:2)                     
049200       END-IF                                                             
049300                                                                          
049400       MOVE MID-IDSUPREF-IN     TO MSGI-IDSUPREF                          
049500                                                                          
049600       MOVE MID-IDDISTR-IN      TO MSGI-IDDISTR                           
049700     END-IF                                                               
049800                                                                          
049900     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
050000                                                                          
050100*    - LANGUAGE TO BE USED BY MEDKONV                                     
050200     MOVE MSGI-IDLAND-SPR       TO MED-IDSKYLT                            
050300                                                                          
050400     MOVE JA                    TO KEYS-SW                                
050500                                                                          
050600*    -- KONTROLL AV IDLEVNR                                               
050700     MOVE MFS-RENSA-FAELT       TO MOD-IDLEVNR-IN                         
050800                                                                          
050900     IF MSGI-IDLEVNR = ALL '+'                                            
051000       MOVE NEJ                 TO KEYS-SW                                
051100       MOVE SPACE               TO MOD-IDLEVNR-UT                         
051200                                                                          
051300     ELSE                                                                 
051400                                                                          
051500       IF MSGI-IDLEVNR > SPACE                                            
051600         MOVE MSGI-IDLEVNR      TO MOD-IDLEVNR-UT                         
051700                                                                          
051800         MOVE MSGI-IDLEVNR      TO W-IDLEVNR-MIN                          
051900                                   W-IDLEVNR-MAX                          
052000         MOVE 'U'               TO SW-E6F                                 
052100       ELSE                                                               
052200         MOVE NEJ               TO KEYS-SW                                
052300         MOVE SPACE             TO MOD-IDLEVNR-UT                         
052400                                                                          
052500       END-IF                                                             
052600     END-IF                                                               
052700                                                                          
052800*    -- KONTROLL AV TISUPREF                                              
052900     MOVE MFS-RENSA-FAELT       TO MOD-TISUPREF-IN                        
053000                                                                          
053100     IF MSGI-DASUPREF(3:6) = ALL '+'                                      
053200       MOVE ZERO                TO WS-SEKEL                               
053300                                   WS-AAMMDD                              
053400       MOVE SPACE               TO MOD-TISUPREF-UT                        
053500     ELSE                                                                 
053600       INSPECT MSGI-DASUPREF      REPLACING ALL SPACE BY ZERO             
053700                                                                          
053800       IF  MSGI-DASUPREF(3:6) NUMERIC                                     
053900       AND MSGI-DASUPREF(3:6) > ZERO                                      
054000         MOVE 'AAMMDD'            TO DAT-KDDATFORM                        
054100         MOVE MSGI-DASUPREF(3:6) TO DAT-I-TIDATUM                         
054200         CALL WDATKONV USING DAT-KDDATFORM,                               
054300                             DAT-I-TIDATUM,                               
054400                             DAT-O-TIDATUM,                               
054500                             DAT-KDSVAR                                   
054600         IF DAT-KDSVAR = 'F'                                              
054700           MOVE NEJ TO KEYS-SW                                            
054800           MOVE MSGI-DASUPREF(3:6) TO MOD-TISUPREF-UT                     
054900           INSPECT MOD-TISUPREF-UT REPLACING LEADING ZERO BY SPACE        
055000                                                                          
055100         ELSE                                                             
055200           IF MSGI-DASUPREF(3:6) < '500000'                               
055300             MOVE 20              TO WS-SEKEL                             
055400           ELSE                                                           
055500             MOVE 19              TO WS-SEKEL                             
055600           END-IF                                                         
055700                                                                          
055800           MOVE MSGI-DASUPREF(3:6) TO WS-AAMMDD                           
055900                                     MOD-TISUPREF-UT                      
056000                                                                          
056100           MOVE WS-DASUPREF       TO W-DASUPREF-MIN                       
056200                                     W-DASUPREF-MAX                       
056300           MOVE 'U'               TO SW-E6F                               
056400         END-IF                                                           
056500       ELSE                                                               
056600         MOVE ZERO                TO WS-SEKEL                             
056700                                     WS-AAMMDD                            
056800         MOVE MSGI-DASUPREF(3:6)   TO MOD-TISUPREF-UT                     
056900         INSPECT MOD-TISUPREF-UT REPLACING LEADING ZERO BY SPACE          
057000       END-IF                                                             
057100     END-IF                                                               
057200                                                                          
057300*    -- KONTROLL AV IDSUPREF                                              
057400     MOVE MFS-RENSA-FAELT       TO MOD-IDSUPREF-IN                        
057500                                                                          
057600     IF MSGI-IDSUPREF = ALL '+' OR SPACE                                  
057700         MOVE SPACE             TO MOD-IDSUPREF-UT                        
057800         MOVE SPACE             TO W-IDSUPREF-F1                          
057900     ELSE                                                                 
058000       IF WS-DASUPREF > ZERO                                              
058100                                                                          
058200         MOVE MSGI-IDSUPREF     TO W-IDSUPREF-MIN                         
058300         MOVE MSGI-IDSUPREF     TO W-IDSUPREF-MAX                         
058400       ELSE                                                               
058500                                                                          
058600         MOVE MSGI-IDSUPREF     TO W-IDSUPREF-F1                          
058700         MOVE 'I'               TO SW-E6F                                 
058800       END-IF                                                             
058900       MOVE MSGI-IDSUPREF       TO MOD-IDSUPREF-UT                        
059000     END-IF                                                               
059100                                                                          
059200*    -- KONTROLL AV IDDISTR                                               
059300     MOVE MFS-RENSA-FAELT       TO MOD-IDDISTR-IN                         
059400                                                                          
059500     IF MSGI-IDDISTR = ALL '+'                                            
059600     OR MSGI-IDDISTR = SPACE                                              
059700     OR MSGI-IDDISTR = ZERO                                               
059800       MOVE SPACE               TO MOD-IDDISTR-UT                         
059900       MOVE ZERO                TO W-IDDISTR-F1                           
060000     ELSE                                                                 
060100       INSPECT MSGI-IDDISTR REPLACING ALL SPACE BY ZERO                   
060200       IF MSGI-IDDISTR NUMERIC                                            
060300         IF MSGI-IDDISTR > ZERO                                           
060400                                                                          
060500           MOVE MSGI-IDDISTR    TO W-IDDISTR-F1                           
060600           MOVE MSGI-IDDISTR    TO MOD-IDDISTR-UT                         
060700           INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE         
060800                                                                          
060900           IF WS-DASUPREF > ZERO                                          
061000             MOVE 'D'           TO SW-E6F                                 
061100           ELSE                                                           
061200             IF MSGI-IDSUPREF = ALL '+'                                   
061300             OR MSGI-IDSUPREF = SPACE                                     
061400               MOVE 'D'         TO SW-E6F                                 
061500             ELSE                                                         
061600               MOVE 'X'         TO SW-E6F                                 
061700             END-IF                                                       
061800           END-IF                                                         
061900         ELSE                                                             
062000           MOVE MSGI-IDDISTR    TO MOD-IDDISTR-UT                         
062100           INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE         
062200         END-IF                                                           
062300       ELSE                                                               
062400         MOVE NEJ TO KEYS-SW                                              
062500         MOVE MSGI-IDDISTR      TO MOD-IDDISTR-UT                         
062600         INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE           
062700       END-IF                                                             
062800     END-IF                                                               
062900                                                                          
063000     IF OWN-MID OR KEYS-OK                                                
063100       CONTINUE                                                           
063200     ELSE                                                                 
063300       MOVE MFS-ERASE-FIELD     TO MOD-IDLEVNR-UT                         
063400       MOVE MFS-ERASE-FIELD     TO MOD-TISUPREF-UT                        
063500       MOVE MFS-ERASE-FIELD     TO MOD-IDSUPREF-UT                        
063600       MOVE MFS-ERASE-FIELD     TO MOD-IDDISTR-UT                         
063700     END-IF                                                               
063800                                                                          
063900     IF KEYS-WRONG                                                        
064000       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
064100       CALL WMEDKONV USING MED-WMEDAREA                                   
064200       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
064300       PERFORM MFS-ERASE-FIELD-IN                                         
064400       PERFORM MFS-ERASE-FIELD-OUT                                        
064500     END-IF                                                               
064600     .                                                                    
064700     EJECT                                                                
064800 C-CHECK-INPUT-AND-UPDATE     SECTION.                                    
064900     MOVE 'C-CHECK-INPUT-AND-UPDATE' TO WS-CURRENT-SECTION                
065000                                                                          
065100     MOVE NEJ                        TO SW-INDATA-FOUND                   
065200     MOVE +1                         TO INDX                              
065300     MOVE SPACE                      TO W-MED-IDMFSFEL                    
065400                                                                          
065500     PERFORM UNTIL INDX > MAX-RADINDX                                     
065600                                                                          
065700       IF MID-INPUT(INDX) = ALL '+' OR SPACE                              
065800         CONTINUE                                                         
065900       ELSE                                                               
066000         MOVE JA                     TO SW-INDATA-FOUND                   
066100         PERFORM CA-CHECK-INDATA-LINE                                     
066200         IF INDATA-OK                                                     
066300                                                                          
066400           IF MFS-UPDATE                                                  
066500             PERFORM CB-UPDATE-INDATA-LINE                                
066600           ELSE                                                           
066700             PERFORM CC-MFS-DO-NOT-TOUCH-FIELD                            
066800           END-IF                                                         
066900         ELSE                                                             
067000           PERFORM CC-MFS-DO-NOT-TOUCH-FIELD                              
067100         END-IF                                                           
067200       END-IF                                                             
067300                                                                          
067400       ADD +1                        TO INDX                              
067500     END-PERFORM                                                          
067600                                                                          
067700       IF INDATA-WRONG                                                    
067800                                                                          
067900         IF W-MED-IDMFSFEL            = SPACE                             
068000            MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                     
068100         ELSE                                                             
068200            MOVE W-MED-IDMFSFEL       TO MED-IDMFSFEL                     
068300         END-IF                                                           
068400                                                                          
068500         CALL WMEDKONV USING MED-WMEDAREA                                 
068600         MOVE MED-MFSFEL          TO MOD-TEMFSFEL                         
068700       END-IF                                                             
068800                                                                          
068900       IF UPDATE-DONE                                                     
069000         MOVE INF-UPDATE-DONE           TO MED-IDMFSINF                   
069100         CALL WMEDKONV USING MED-WMEDAREA                                 
069200         MOVE MED-MFSINF                TO MOD-TEMFSINF                   
069300       ELSE                                                               
069400         MOVE INF-UPDATE-NOT-DONE       TO MED-IDMFSINF                   
069500         CALL WMEDKONV USING MED-WMEDAREA                                 
069600         MOVE MED-MFSINF                TO MOD-TEMFSINF                   
069700       END-IF                                                             
069800     .                                                                    
069900     EJECT                                                                
070000                                                                          
070100 CA-CHECK-INDATA-LINE         SECTION.                                    
070200     MOVE 'CA-CHECK-INDATA-LINE'     TO WS-CURRENT-SECTION                
070300     MOVE JA                         TO INDATA-SW                         
070400                                                                          
070500                                                                          
070600     IF MID-IDDISTR-UPD(INDX) = ALL '+' OR SPACE                          
070700       MOVE MFS-NUM-FAELT-FEL        TO MOD-IDDISTR-UPD-ATTR(INDX)        
070800       MOVE NEJ                      TO INDATA-SW                         
070900     ELSE                                                                 
071000                                                                          
071100       INSPECT MID-IDDISTR-UPD(INDX)                                      
071200               REPLACING LEADING SPACE BY ZERO                            
071300                                                                          
071400       IF MID-IDDISTR-UPD(INDX) NUMERIC                                   
071500         MOVE MID-IDDISTR-UPD(INDX) TO W-E4A1-IDDISTR-MIN                 
071600         MOVE MID-IDDISTR-UPD(INDX) TO W-E4A1-IDDISTR-MAX                 
071700         MOVE MFS-NUM-FAELT-RAETT    TO MOD-IDDISTR-UPD-ATTR(INDX)        
071800       ELSE                                                               
071900         MOVE MFS-NUM-FAELT-FEL      TO MOD-IDDISTR-UPD-ATTR(INDX)        
072000         MOVE NEJ                    TO INDATA-SW                         
072100       END-IF                                                             
072200     END-IF                                                               
072300                                                                          
072400     IF MID-IDKUNDNR-UPD(INDX)      = ALL '+'                             
072500         MOVE MFS-NUM-FAELT-FEL                                           
072600                                 TO MOD-IDKUNDNR-UPD-ATTR(INDX)           
072700         MOVE NEJ                   TO INDATA-SW                          
072800     ELSE                                                                 
072900                                                                          
073000       INSPECT MID-IDKUNDNR-UPD(INDX)                                     
073100               REPLACING LEADING SPACE BY ZERO                            
073200                                                                          
073300       IF MID-IDKUNDNR-UPD(INDX) NUMERIC                                  
073400         MOVE MID-IDKUNDNR-UPD(INDX)                                      
073500           TO W-E4A1-IDKUNDNR-MIN                                         
073600              W-E4A1-IDKUNDNR-MAX                                         
073700         MOVE MFS-NUM-FAELT-RAETT                                         
073800                                 TO MOD-IDKUNDNR-UPD-ATTR(INDX)           
073900       ELSE                                                               
074000         MOVE MFS-NUM-FAELT-FEL     TO MOD-IDKUNDNR-UPD-ATTR(INDX)        
074100         MOVE NEJ                   TO INDATA-SW                          
074200       END-IF                                                             
074300     END-IF                                                               
074400                                                                          
074500     IF MID-IDORDNR7-UPD(INDX)     = ALL '+'                              
074600       MOVE MFS-NUM-FAELT-FEL                                             
074700                               TO MOD-IDORDNR7-UPD-ATTR(INDX)             
074800       MOVE NEJ                     TO INDATA-SW                          
074900     ELSE                                                                 
075000                                                                          
075100       INSPECT MID-IDORDNR7-UPD(INDX)                                     
075200               REPLACING LEADING SPACE BY ZERO                            
075300                                                                          
075400       IF MID-IDORDNR7-UPD(INDX) NUMERIC                                  
075500         IF MID-IDORDNR7-UPD(INDX)(1:2) = ZERO                            
075600                                                                          
075700           MOVE MID-IDORDNR7-UPD(INDX)(3:5)                               
075800                        TO W-E4A1-IDORDNR-MIN (1:5)                       
075900                           W-E4A1-IDORDNR-MAX (1:5)                       
076000           MOVE MFS-NUM-FAELT-RAETT                                       
076100                        TO MOD-IDORDNR7-UPD-ATTR(INDX)                    
076200         ELSE                                                             
076300           MOVE MFS-NUM-FAELT-FEL                                         
076400                        TO MOD-IDORDNR7-UPD-ATTR(INDX)                    
076500           MOVE NEJ        TO INDATA-SW                                   
076600         END-IF                                                           
076700       ELSE                                                               
076800         MOVE MFS-NUM-FAELT-FEL                                           
076900                                 TO MOD-IDORDNR7-UPD-ATTR(INDX)           
077000         MOVE NEJ                   TO INDATA-SW                          
077100       END-IF                                                             
077200     END-IF                                                               
077300                                                                          
077400     IF MID-IDKOLLI-UPD(INDX)     = ALL '+'                               
077500       MOVE MFS-NUM-FAELT-FEL       TO MOD-IDKOLLI-UPD-ATTR(INDX)         
077600       MOVE NEJ                     TO INDATA-SW                          
077700     ELSE                                                                 
077800                                                                          
077900       INSPECT MID-IDKOLLI-UPD(INDX)                                      
078000               REPLACING LEADING SPACE BY ZERO                            
078100                                                                          
078200       IF MID-IDKOLLI-UPD(INDX) NUMERIC                                   
078300         MOVE MID-IDKOLLI-UPD(INDX)                                       
078400                                 TO W-IDKOLLI                             
078500         MOVE MFS-NUM-FAELT-RAETT                                         
078600                                 TO MOD-IDKOLLI-UPD-ATTR(INDX)            
078700       ELSE                                                               
078800         MOVE MFS-NUM-FAELT-FEL     TO MOD-IDKOLLI-UPD-ATTR(INDX)         
078900         MOVE NEJ                   TO INDATA-SW                          
079000       END-IF                                                             
079100     END-IF                                                               
079200*                                                                         
079300     IF INDATA-OK                                                         
079400       PERFORM IMS-GU-WDE4A1                                              
079500                                                                          
079600       MOVE NEJ     TO SW-TRAFF                                           
079700                                                                          
079800       PERFORM UNTIL SEGMENT-MISSING OR SEGMENT-FINISH OR                 
079900                     TRAFF                                                
080000         MOVE SEQA-IDPRODNR          TO W-IDPRODNR                        
080100         MOVE MID-IDKOLLI-UPD(INDX) TO W-IDKOLLI                          
080200*                                                                         
080300         PERFORM IMS-GHU-WDE601                                           
080400         PERFORM IMS-GHNP-WDE611                                          
080500         IF SEGMENT-FOUND                                                 
080600           MOVE JA            TO SW-TRAFF                                 
080700                                                                          
080800           IF KOLLI-KDKOLSTA = +0                                         
080900             CONTINUE                                                     
081000           ELSE                                                           
081100             MOVE 'PACK'           TO MOD-NOTIFICATION(INDX)              
081200             MOVE MFS-NUM-FAELT-FEL                                       
081300               TO MOD-IDKOLLI-UPD-ATTR(INDX)                              
081400             MOVE NEJ               TO INDATA-SW                          
081500           END-IF                                                         
081600                                                                          
081700           IF INDATA-OK                                                   
081800             IF MSGI-IDLEVNR > SPACE                                      
081900               IF KOLLI-IDLEVNR = MSGI-IDLEVNR                            
082000                 CONTINUE                                                 
082100               ELSE                                                       
082200                 MOVE 'SUPL'        TO MOD-NOTIFICATION(INDX)             
082300                 MOVE MFS-NUM-FAELT-FEL                                   
082400                   TO MOD-IDKOLLI-UPD-ATTR(INDX)                          
082500                 MOVE NEJ           TO INDATA-SW                          
082600               END-IF                                                     
082700             END-IF                                                       
082800           END-IF                                                         
082900                                                                          
083000           IF INDATA-OK                                                   
083100                                                                          
083200             IF MSGI-DASUPREF(3:6) > ZERO                                 
083300               IF KOLLI-DASUPREF = MSGI-DASUPREF                          
083400                 CONTINUE                                                 
083500               ELSE                                                       
083600                 MOVE 'DATE'       TO MOD-NOTIFICATION(INDX)              
083700                 MOVE MFS-NUM-FAELT-FEL                                   
083800                   TO MOD-IDKOLLI-UPD-ATTR(INDX)                          
083900                 MOVE NEJ           TO INDATA-SW                          
084000               END-IF                                                     
084100             END-IF                                                       
084200           END-IF                                                         
084300                                                                          
084400           IF INDATA-OK                                                   
084500             IF MSGI-IDSUPREF > SPACE                                     
084600               IF KOLLI-IDSUPREF = MSGI-IDSUPREF                          
084700                 CONTINUE                                                 
084800               ELSE                                                       
084900                 MOVE 'REF '       TO MOD-NOTIFICATION(INDX)              
085000                 MOVE MFS-NUM-FAELT-FEL                                   
085100                   TO MOD-IDKOLLI-UPD-ATTR(INDX)                          
085200                 MOVE NEJ           TO INDATA-SW                          
085300               END-IF                                                     
085400             END-IF                                                       
085500           END-IF                                                         
085600                                                                          
085700           IF INDATA-OK                                                   
085800             IF MSGI-IDDISTR > ZERO                                       
085900               MOVE KOLLI-IDDISTR         TO WS-KOLLI-IDDISTR             
086000               IF WS-KOLLI-IDDISTR = MSGI-IDDISTR                         
086100                 MOVE MFS-NUM-FAELT-RAETT                                 
086200                   TO MOD-IDKOLLI-UPD-ATTR(INDX)                          
086300               ELSE                                                       
086400                 MOVE 'DIST'       TO MOD-NOTIFICATION(INDX)              
086500                 MOVE MFS-NUM-FAELT-FEL                                   
086600                   TO MOD-IDKOLLI-UPD-ATTR(INDX)                          
086700                 MOVE NEJ           TO INDATA-SW                          
086800               END-IF                                                     
086900             END-IF                                                       
087000           END-IF                                                         
087100         END-IF                                                           
087200                                                                          
087300*NÄSTA PRODNR                                                             
087400         PERFORM IMS-GN-WDE4A1                                            
087500       END-PERFORM                                                        
087600                                                                          
087700       IF NOT TRAFF                                                       
087800*CASE MISSING                                                             
087900         MOVE 'MISS'               TO MOD-NOTIFICATION(INDX)              
088000         MOVE MFS-ADD-SAETT-CURSOR                                        
088100           TO MOD-IDKOLLI-UPD-ATTR(INDX)                                  
088200         MOVE NEJ                  TO INDATA-SW                           
088300       END-IF                                                             
088400     END-IF                                                               
088500     .                                                                    
088600     EJECT                                                                
088700 CB-UPDATE-INDATA-LINE SECTION.                                           
088800     MOVE 'CB-UPDATE-INDATA-LINE' TO WS-CURRENT-SECTION                   
088900                                                                          
089000     MOVE NEJ                      TO LINE-UPDATED-SW                     
089100                                                                          
089200     IF KOLLI-KDKOLSTA = 0                                                
089300       MOVE '60176'                TO IDPGM                               
089400                                                                          
089500       PERFORM CBA-DEFINIERA-PLATS                                        
089600                                                                          
089700       IF PLATS-KDSVAR NOT = SPACE                                        
089800         MOVE 'ADDR'                     TO MOD-NOTIFICATION(INDX)        
089900                                                                          
090000         MOVE MFS-NUM-FAELT-FEL    TO MOD-IDKOLLI-UPD-ATTR(INDX)          
090100                                                                          
090200         MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-IDDISTR-UPD(INDX)             
090300                                        MOD-IDKUNDNR-UPD(INDX)            
090400                                        MOD-IDORDNR7-UPD(INDX)            
090500                                        MOD-IDKOLLI-UPD(INDX)             
090600                                                                          
090700          MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-IDDISTR-UPD(INDX)            
090800                                        MOD-IDKUNDNR-UPD(INDX)            
090900                                        MOD-IDORDNR7-UPD(INDX)            
091000                                        MOD-IDKOLLI-UPD(INDX)             
091100       ELSE                                                               
091200                                                                          
091300         MOVE PLATS-IDTRPTNR             TO KOLLI-IDTRPTNR                
091400         MOVE PLATS-ADFLGEO              TO KOLLI-ADFLGEO                 
091500         MOVE PLATS-ADFLOMR              TO KOLLI-ADFLOMR                 
091600         MOVE PLATS-ADRUTNIV             TO KOLLI-ADRUTNIV                
091700         MOVE PLATS-ADVMODUL             TO KOLLI-ADVMODUL                
091800         MOVE PLATS-DIDMODUL             TO KOLLI-DIDMODUL                
091900         MOVE PLATS-DIHMODUL             TO KOLLI-DIHMODUL                
092000         MOVE PLATS-ADHMODUL             TO KOLLI-ADHMODUL                
092100         MOVE PLATS-FLUTLAST             TO KOLLI-FLUTLAST                
092200         MOVE VORD-IDDISTR               TO TEST-IDDISTR                  
092300                                                                          
092400         IF VORD-FLAUTFAK = JA AND DIST03-SVERIGE-2                       
092500           MOVE NEJ                      TO KOLLI-FLUTLAST                
092600         END-IF                                                           
092700         MOVE 1                          TO KOLLI-KDKOLSTA                
092800         MOVE WS-DAGENS-DATUM-AAMMDD TO KOLLI-TIPACKN                     
092900         COMPUTE KOLLI-TIPACTID = WS-DAGENS-KLOCKA / 100                  
093000         END-COMPUTE                                                      
093100         IF KOLLI-TIAAVVD-PATR = ZERO                                     
093200            PERFORM S01-SKAPA-TRANS-TILL-SV-AF                            
093300         END-IF                                                           
093400                                                                          
093500         PERFORM IMS-REPL-WDE611                                          
093600         PERFORM CBD-SKAPA-LOGG                                           
093610*CREATE WDE621                                                            
093620         IF PLATS-IDDC-CROSS > SPACES                                     
093630          MOVE W-KDSEGKEY-X     TO CROSS-KDSEGKEY                         
093640          MOVE PLATS-IDDC       TO CROSS-IDDC-SEND                        
093650          MOVE PLATS-IDDC-CROSS TO CROSS-IDDC-CROSS                       
093660          MOVE KOLLI-IDDISTR    TO CROSS-IDDISTR                          
093670          MOVE KOLLI-IDKUNDNR   TO CROSS-IDKUNDNR                         
093680          MOVE VORD-IDPRODNR    TO CROSS-IDPRODNR                         
093690          MOVE KOLLI-IDKOLLI    TO CROSS-IDKOLLI                          
093691          MOVE KOLLI-IDLEVNR    TO CROSS-IDLEVNR                          
093692          MOVE KOLLI-IDSUPREF   TO CROSS-IDSUPREF                         
093693          MOVE KOLLI-DARFS(3:6) TO CROSS-TIRFSDAT                         
093694          MOVE ZERO             TO CROSS-IDTRPTNR-CROSS                   
093695          MOVE ZERO             TO CROSS-TIRECXDAT                        
093696          MOVE ZERO             TO CROSS-TIRECXTID                        
093697          MOVE ZERO             TO CROSS-TISKEPPN                         
093698          MOVE ZERO             TO CROSS-IDSHIPM-CROSS                    
093699          MOVE SPACE            TO CROSS-IDLBBET-CROSS                    
093700          MOVE 1                TO CROSS-KDKOLSTA-CROSS                   
093701          PERFORM IMS-ISRT-WDE621                                         
093710         END-IF                                                           
093720                                                                          
093730         MOVE JA                         TO LINE-UPDATED-SW               
093800         MOVE JA                         TO UPDATE-DONE-SW                
093900       END-IF                                                             
094000     END-IF                                                               
094100                                                                          
094200     IF LINE-UPDATED                                                      
094300       MOVE NEJ                           TO PACK-REPOTED-SW              
094400                                             PRINTED-SW                   
094500       PERFORM IMS-GU-WDE611                                              
094600*                                                                         
094700       IF KOLLI-KDKOLSTA = 0                                              
094800         MOVE JA                          TO PRINTED-SW                   
094900       ELSE                                                               
095000         IF KOLLI-KDKOLSTA = 1                                            
095100           MOVE JA                        TO PACK-REPOTED-SW              
095200         END-IF                                                           
095300       END-IF                                                             
095400                                                                          
095500* HÄR SÄTTS NYA VÄRDEN FÖR DIV. FÄLT I WDE601 OCH                         
095600* HÄR AVGÖRS OM BEFINTLIGA WDGX4726/27-SEGMENT SKALL TAS BORT             
095700* ELLER OM NYA SÅDANA SKALL LÄGGAS UPP                                    
095800       PERFORM IMS-GHU-WDE601                                             
095900       MOVE VORD-IDDISTR                  TO TEST-IDDISTR                 
096000                                                                          
096100       IF  PRINTED                                                        
096200       AND PACK-REPOTED                                                   
096300         IF VORD-KDORDSTA NOT = 2                                         
096400           IF VORD-KDORDSTA = 3                                           
096500             IF  VORD-FLAUTFAK = JA                                       
096600             AND DIST03-SVERIGE-2                                         
096700               PERFORM CBB-BORTTAG-WDGX4726-27                            
096800             END-IF                                                       
096900           END-IF                                                         
097000* PRINTED, PACKNING PÅBÖRJAD                                              
097100           MOVE 2                         TO VORD-KDORDSTA                
097200         END-IF                                                           
097300       ELSE                                                               
097400         IF PRINTED                                                       
097500                                                                          
097600           IF VORD-KDORDSTA NOT = 1                                       
097700             IF VORD-KDORDSTA = 3                                         
097800                                                                          
097900               IF  VORD-FLAUTFAK = JA                                     
098000               AND DIST03-SVERIGE-2                                       
098100                 PERFORM CBB-BORTTAG-WDGX4726-27                          
098200               END-IF                                                     
098300             END-IF                                                       
098400                                                                          
098500* PRINTED, PACKNING EJ PÅBÖRJAD                                           
098600             MOVE 1                       TO VORD-KDORDSTA                
098700           END-IF                                                         
098800         ELSE                                                             
098900                                                                          
099000           IF  PACK-REPOTED                                               
099100           AND VORD-KVORDRAD = VORD-KVORDRAD-PACK                         
099200                                                                          
099300             IF VORD-KDORDSTA NOT = 3                                     
099400                                                                          
099500               IF  VORD-FLAUTFAK = JA                                     
099600               AND DIST03-SVERIGE-2                                       
099700                 PERFORM CBC-NYUPPL-WDGX4726-27                           
099800               END-IF                                                     
099900* PACKNING KLAR                                                           
100000               MOVE 3                     TO VORD-KDORDSTA                
100100             END-IF                                                       
100200                                                                          
100300           END-IF                                                         
100400         END-IF                                                           
100500       END-IF                                                             
100600                                                                          
100700       ADD +1                             TO VORD-KVKOLLI                 
100800       PERFORM IMS-REPL-WDE601                                            
100900                                                                          
101000       MOVE MFS-FORMATETS-ATTR TO MOD-IDDISTR-UPD-ATTR(INDX)              
101100                                  MOD-IDKUNDNR-UPD-ATTR(INDX)             
101200                                  MOD-IDORDNR7-UPD-ATTR(INDX)             
101300                                  MOD-IDKOLLI-UPD-ATTR(INDX)              
101400                                                                          
101500       MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-UPD (INDX)                     
101600                               MOD-IDKUNDNR-UPD (INDX)                    
101700                               MOD-IDORDNR7-UPD (INDX)                    
101800                               MOD-IDKOLLI-UPD (INDX)                     
101900                               MOD-NOTIFICATION(INDX)                     
102000     END-IF                                                               
102100     .                                                                    
102200     EJECT                                                                
102300                                                                          
102400 CBA-DEFINIERA-PLATS SECTION.                                             
102500     MOVE 'CBA-DEFINIERA-PLATS ' TO WS-CURRENT-SECTION                    
102600                                                                          
102700     IF KOLLI-KDORDKL = 4                                                 
102800        MOVE +1               TO PLATS-KDCALL                             
102900     ELSE                                                                 
103000        MOVE +2               TO PLATS-KDCALL                             
103100     END-IF                                                               
103200     MOVE WS-CDC-11           TO PLATS-IDDC                               
103300     MOVE KOLLI-IDDISTR       TO PLATS-IDDISTR                            
103400     MOVE KOLLI-IDKUNDNR      TO PLATS-IDKUNDNR                           
103500     MOVE VORD-KDFRAKT        TO PLATS-KDFRAKT                            
103600                                                                          
103700     MOVE SEQA-IDORDNR5       TO PLATS-IDORDNR                            
103800     MOVE KOLLI-KDORDKL       TO PLATS-KDORDKLX                           
103900     MOVE ZERO                TO PLATS-DIKOLLIH                           
104000                                 PLATS-DIKOLLIL                           
104100                                 PLATS-DIKOLLIB                           
104200                                 PLATS-VKORDNTO-KOLLI                     
104300     MOVE SPACE               TO PLATS-KDKOLLID                           
104400     MOVE KOLLI-ADFLGEO       TO PLATS-ADFLGEO                            
104500     MOVE KOLLI-ADFLOMR       TO PLATS-ADFLOMR                            
104600     MOVE KOLLI-ADRUTNIV      TO PLATS-ADRUTNIV                           
104700     MOVE ZERO                TO PLATS-IDTRPTNR                           
104800                                 PLATS-DIHMODUL                           
104900                                 PLATS-DIDMODUL                           
105000                                 PLATS-ADVMODUL                           
105100                                 PLATS-ADHMODUL                           
105200     MOVE SPACE               TO PLATS-FLUTLAST                           
105300                                 PLATS-IDDC-CROSS                         
105400                                                                          
105500     CALL W403PLAT USING PLATS-W403PLAT                                   
105600                         PLATS-DM-PCB                                     
105700                         PLATS-DN-PCB                                     
105800                         PLATS-DP-PCB                                     
105900                         PLATS-DO-PCB                                     
106000                         PLATS-WDE6C-PCB                                  
106100                         PLATS-GMTC-PCB                                   
106200                         PLATS-WDB6-PCB                                   
106300     .                                                                    
106400     EJECT                                                                
106500 CBB-BORTTAG-WDGX4726-27 SECTION.                                         
106600     MOVE 'CBB-BORTTAG-WDGX4726-27 ' TO WS-CURRENT-SECTION                
106700                                                                          
106800     MOVE '4726'                  TO W-4726-IDHTYP                        
106900*    IF  DIST03-SVERIGE                                                   
107000*        MOVE JA                  TO W-4726-FLBATCH                       
107100*    ELSE                                                                 
107200         MOVE NEJ                 TO W-4726-FLBATCH                       
107300*    END-IF                                                               
107400     MOVE LOW-VALUE               TO W-4726-LOWVALUE                      
107500     PERFORM IMS-GHU-4726-ROT                                             
107600                                                                          
107700     MOVE VORD-IDDISTR            TO W-4726-IDDISTR                       
107800     MOVE VORD-IDKUNDNR           TO W-4726-IDKUNDNR                      
107900     MOVE VORD-IDDC               TO W-4726-IDDC                          
108000     MOVE VORD-KDFAKTYP           TO W-4726-KDFAKTYP                      
108100     PERFORM IMS-GHNP-4726-UNDERSEG                                       
108200                                                                          
108300     IF SEGMENT-FOUND                                                     
108400       MOVE VORD-IDDISTR          TO AUTFAKT-IDDISTR                      
108500       MOVE VORD-IDKUNDNR         TO AUTFAKT-IDKUNDNR                     
108600       MOVE VORD-IDDC             TO AUTFAKT-IDDC                         
108700       MOVE VORD-KDFAKTYP         TO AUTFAKT-KDFAKTYP                     
108800       PERFORM IMS-DLET-4726-UNDERSEG                                     
108900     END-IF                                                               
109000     .                                                                    
109100     EJECT                                                                
109200 CBC-NYUPPL-WDGX4726-27 SECTION.                                          
109300     MOVE 'CBC-NYUPPL-WDGX4726-27 ' TO WS-CURRENT-SECTION                 
109400                                                                          
109500     MOVE '4726'                  TO W-4726-IDHTYP                        
109600*    IF  DIST03-SVERIGE                                                   
109700*        MOVE JA                  TO W-4726-FLBATCH                       
109800*    ELSE                                                                 
109900         MOVE NEJ                 TO W-4726-FLBATCH                       
110000*    END-IF                                                               
110100     MOVE LOW-VALUE               TO W-4726-LOWVALUE                      
110200     PERFORM IMS-GHU-4726-ROT                                             
110300                                                                          
110400     MOVE VORD-IDDISTR            TO W-4726-IDDISTR                       
110500     MOVE VORD-IDKUNDNR           TO W-4726-IDKUNDNR                      
110600     MOVE VORD-IDDC               TO W-4726-IDDC                          
110700     MOVE VORD-KDFAKTYP           TO W-4726-KDFAKTYP                      
110800     PERFORM IMS-GHNP-4726-UNDERSEG                                       
110900                                                                          
111000     IF SEGMENT-MISSING                                                   
111100       MOVE VORD-IDDISTR          TO AUTFAKT-IDDISTR                      
111200       MOVE VORD-IDKUNDNR         TO AUTFAKT-IDKUNDNR                     
111300       MOVE VORD-IDDC             TO AUTFAKT-IDDC                         
111400       MOVE VORD-KDFAKTYP         TO AUTFAKT-KDFAKTYP                     
111500       PERFORM IMS-INSERT-4726-UNDERSEG                                   
111600     END-IF                                                               
111700                                                                          
111800     MOVE VORD-IDPRODNR           TO AUTFAKT-IDPRODNR                     
111900     MOVE ZERO                    TO AUTFAKT-IDSKEPPN                     
112000                                     AUTFAKT-PRFRAKT                      
112100     IF  DIST03-SVERIGE                                                   
112200         MOVE NEJ                 TO AUTFAKT-FLLASTA                      
112300     ELSE                                                                 
112400         MOVE JA                  TO AUTFAKT-FLLASTA                      
112500     END-IF                                                               
112600                                                                          
112700     PERFORM IMS-INSERT-4727                                              
112800     .                                                                    
112900     EJECT                                                                
113000 CBD-SKAPA-LOGG SECTION.                                                  
113100     MOVE 'CBD-SKAPA-LOGG      ' TO WS-CURRENT-SECTION                    
113200                                                                          
113300     MOVE 'W463'                      TO FIL-CT-IDSYSTEM                  
113400     MOVE 'VIA'                       TO FIL-CT-IDPTYP                    
113500     MOVE ' '                         TO FIL-CT-IDVTYP                    
113600                                                                          
113700     MOVE 'W6017600'                  TO FIL-IDPGM                        
113800     MOVE FUNCTION CURRENT-DATE (3:6) TO FIL-TIREGDAT                     
113900     MOVE FUNCTION CURRENT-DATE (1:8) TO LOGG-DAREGDAT                    
114000     ACCEPT FIL-TIKLOCK               FROM TIME                           
114100     MOVE FIL-TIKLOCK                 TO LOGG-TIREGTID                    
114200     MOVE 1                           TO FIL-IDSEKVNR                     
114300     MOVE 'VIA'                       TO LOGG-IDPTYP                      
114400     MOVE VORD-IDDC                   TO LOGG-IDDC                        
114500     MOVE VORD-IDDISTR                TO LOGG-IDDISTR                     
114600     MOVE VORD-IDKUNDNR               TO LOGG-IDKUNDNR                    
114700     MOVE ZERO                        TO LOGG-IDRADNR                     
114800                                         LOGG-IDARTNR                     
114900                                         LOGG-IDORDNR7                    
115000                                         LOGG-KVANTAL                     
115100     IF KOLLI-TIPACKN > 501231                                            
115200       COMPUTE LOGG-DAPACKN = 19000000 + KOLLI-TIPACKN                    
115300     ELSE                                                                 
115400       COMPUTE LOGG-DAPACKN = 20000000 + KOLLI-TIPACKN                    
115500     END-IF                                                               
115600     MOVE KOLLI-IDSUPREF              TO LOGG-IDSUPREF                    
115700     MOVE KOLLI-DASUPREF              TO LOGG-DASUPREF                    
115800     MOVE KOLLI-TIPACTID              TO LOGG-TIPACTID                    
115900     COMPUTE LOGG-TISUPTID = KOLLI-TISUPTID / 10                          
116000     MOVE KOLLI-KDORDKL               TO LOGG-KDORDKL                     
116100     MOVE KOLLI-KDVIA                 TO LOGG-KDVIA                       
116200     MOVE ZERO                        TO LOGG-DABEKDAT                    
116300                                         LOGG-DAFAKT                      
116400                                         LOGG-DALEVDAT                    
116500                                         LOGG-DASKEPPN                    
116600                                         LOGG-DASNDDAT                    
116700                                         LOGG-TIBEKR                      
116800                                         LOGG-TISNDTID                    
116900                                         LOGG-KDORDBEK                    
117000     MOVE SPACE                       TO LOGG-BERADREF                    
117100                                                                          
117200     PERFORM IMS-ISRT-WDR601                                              
117300     PERFORM UNTIL SEGMENT-FOUND                                          
117400       ADD +1  TO FIL-IDSEKVNR                                            
117500       PERFORM IMS-ISRT-WDR601                                            
117600     END-PERFORM                                                          
117700     .                                                                    
117800     EJECT                                                                
117900                                                                          
118000 CC-MFS-DO-NOT-TOUCH-FIELD    SECTION.                                    
118100                                                                          
118200     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-IDDISTR-UPD(INDX)                 
118300                                    MOD-IDKUNDNR-UPD(INDX)                
118400                                    MOD-IDORDNR7-UPD(INDX)                
118500                                    MOD-IDKOLLI-UPD(INDX)                 
118600                                                                          
118700     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-IDDISTR-UPD(INDX)                 
118800                                    MOD-IDKUNDNR-UPD(INDX)                
118900                                    MOD-IDORDNR7-UPD(INDX)                
119000                                    MOD-IDKOLLI-UPD(INDX)                 
119100     .                                                                    
119200     EJECT                                                                
119300                                                                          
119400 E-READ-AGAIN           SECTION.                                          
119500     MOVE 'E-READ-AGAIN ' TO WS-CURRENT-SECTION                           
119600                                                                          
119700     IF OWN-MID OR HELP-MID                                               
119800       MOVE +1 TO INDX                                                    
119900       MOVE NEJ TO SW-TRAFF                                               
120000                                                                          
120100       PERFORM UNTIL INDX > MAX-RADINDX                                   
120200         IF MID-INPUT(INDX) = ALL '+'                                     
120300         OR MID-INPUT(INDX) = SPACE                                       
120400           CONTINUE                                                       
120500         ELSE                                                             
120600           MOVE JA TO SW-TRAFF                                            
120700                                                                          
120800           MOVE INDX TO WS-INDX                                           
120900         END-IF                                                           
121000         ADD +1 TO INDX                                                   
121100       END-PERFORM                                                        
121200                                                                          
121300       IF TRAFF                                                           
121400         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
121500         CALL WMEDKONV USING MED-WMEDAREA                                 
121600         MOVE MED-MFSINF TO MOD-TEMFSFEL                                  
121700                                                                          
121800         PERFORM EA-MID-INDATA-TILL-MOD                                   
121900       END-IF                                                             
122000                                                                          
122100     ELSE                                                                 
122200       PERFORM MFS-ERASE-FIELD-IN                                         
122300       PERFORM MFS-ERASE-FIELD-OUT                                        
122400     END-IF                                                               
122500                                                                          
122600     .                                                                    
122700     EJECT                                                                
122800 EA-MID-INDATA-TILL-MOD SECTION.                                          
122900     MOVE 'EA-MID-INDATA-TILL-MOD ' TO WS-CURRENT-SECTION                 
123000                                                                          
123100     MOVE +1                        TO INDX                               
123200                                                                          
123300     PERFORM UNTIL INDX > MAX-RADINDX                                     
123400                                                                          
123500       IF MID-IDDISTR-UPD(INDX)    =  ALL '+'                             
123600       OR MID-IDDISTR-UPD(INDX)    >  SPACE                               
123700         MOVE MFS-RENSA-FAELT       TO MOD-IDDISTR-UPD(INDX)              
123800       ELSE                                                               
123900         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDDISTR-UPD-ATTR(INDX)         
124000         MOVE MID-IDDISTR-UPD(INDX) TO MOD-IDDISTR-UPD(INDX)              
124100       END-IF                                                             
124200                                                                          
124300       IF MID-IDKUNDNR-UPD(INDX)   =  ALL '+' OR SPACE                    
124400       OR MID-IDKUNDNR-UPD(INDX)    >  SPACE                              
124500         MOVE MFS-RENSA-FAELT       TO MOD-IDKUNDNR-UPD(INDX)             
124600       ELSE                                                               
124700         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDKUNDNR-UPD-ATTR(INDX)        
124800         MOVE MID-IDKUNDNR-UPD(INDX) TO MOD-IDKUNDNR-UPD(INDX)            
124900       END-IF                                                             
125000                                                                          
125100       IF MID-IDORDNR7-UPD(INDX)   =  ALL '+' OR SPACE                    
125200       OR MID-IDORDNR7-UPD(INDX)    >  SPACE                              
125300         MOVE MFS-RENSA-FAELT       TO MOD-IDORDNR7-UPD(INDX)             
125400       ELSE                                                               
125500         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDORDNR7-UPD-ATTR(INDX)        
125600         MOVE MID-IDORDNR7-UPD(INDX) TO MOD-IDORDNR7-UPD(INDX)            
125700       END-IF                                                             
125800                                                                          
125900       IF  MID-IDKOLLI-UPD(INDX)    =  ALL '+' OR SPACE                   
126000       OR  MID-IDKOLLI-UPD(INDX)    >  SPACE                              
126100         MOVE MFS-RENSA-FAELT       TO MOD-IDKOLLI-UPD(INDX)              
126200       ELSE                                                               
126300         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDKOLLI-UPD-ATTR(INDX)         
126400         MOVE MID-IDKOLLI-UPD(INDX) TO MOD-IDKOLLI-UPD(INDX)              
126500       END-IF                                                             
126600                                                                          
126700       ADD +1                       TO INDX                               
126800     END-PERFORM                                                          
126900                                                                          
127000     .                                                                    
127100     EJECT                                                                
127200 F-READ-SHOW-INFO SECTION.                                                
127300     MOVE 'F-READ-SHOW-INFO  ' TO WS-CURRENT-SECTION                      
127400                                                                          
127500     MOVE 0                        TO WS-KVKOLLI-NOT-RECIVED              
127600     MOVE 0                        TO WS-KVKOLLI-RECIVED                  
127700                                                                          
127800     PERFORM FA-LAES-RADDATA-GU                                           
127900                                                                          
128000     PERFORM UNTIL SEGMENT-MISSING OR SEGMENT-FINISH                      
128100       IF SEQF-KDKOLSTA = 0                                               
128200         ADD +1                    TO WS-KVKOLLI-NOT-RECIVED              
128300       ELSE                                                               
128400         IF SEQF-KDKOLSTA = +1                                            
128500           ADD +1                  TO WS-KVKOLLI-RECIVED                  
128600         END-IF                                                           
128700       END-IF                                                             
128800                                                                          
128900       PERFORM FB-LAES-RADDATA-GN                                         
129000     END-PERFORM                                                          
129100                                                                          
129200     MOVE WS-KVKOLLI-NOT-RECIVED   TO MOD-KVKOLLI-NOT-RECIVED             
129300     INSPECT MOD-KVKOLLI-NOT-RECIVED REPLACING                            
129400     LEADING ZERO BY SPACE                                                
129500                                                                          
129600     MOVE WS-KVKOLLI-RECIVED       TO MOD-KVKOLLI-RECIVED                 
129700     INSPECT MOD-KVKOLLI-RECIVED REPLACING                                
129800     LEADING ZERO BY SPACE                                                
129900                                                                          
130000     MOVE MFS-ADD-SAETT-CURSOR     TO MOD-IDDISTR-UPD-ATTR (1)            
130100                                                                          
130200     MOVE '002'                    TO MSGI-KDCALL                         
130300     MOVE '6176'                   TO SPAR-IDTRANS                        
130400     MOVE SPAR-AREA                TO MSGI-SPAR-AREA                      
130500     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
130600     .                                                                    
130700     EJECT                                                                
130800                                                                          
130900 FA-LAES-RADDATA-GU     SECTION.                                          
131000     MOVE 'FA-LAES-RADDATA-GU'     TO WS-CURRENT-SECTION                  
131100                                                                          
131200*URVAL AV KOLLIN BEROENDE PÅ IFYLLDA NYCKLAR.                             
131300*                                                                         
131400     IF WITHOUT-SEARCH-FIELD                                              
131500*       VALUE 'U'.                                                        
131600       PERFORM IMS-GU-KOLLI-WDE6F1                                        
131700     ELSE                                                                 
131800       IF SEARCH-WITH-IDSUPREF                                            
131900*         VALUE 'I'.                                                      
132000         PERFORM IMS-GU-SUPREF-WDE6F1                                     
132100       ELSE                                                               
132200         IF SEARCH-WITH-IDDISTR                                           
132300*           VALUE 'D'.                                                    
132400           PERFORM IMS-GU-DISTR-WDE6F1                                    
132500*                                                                         
132600         ELSE                                                             
132700           IF SEARCH-WITH-IDSUPREF-IDDISTR                                
132800*             VALUE 'X'.                                                  
132900             PERFORM IMS-GU-DISTR-IDSUPREF-WDE6F1                         
133000           END-IF                                                         
133100         END-IF                                                           
133200       END-IF                                                             
133300     END-IF                                                               
133400     .                                                                    
133500     EJECT                                                                
133600 FB-LAES-RADDATA-GN SECTION.                                              
133700     MOVE 'FB-LAES-RADDATA-GN '    TO WS-CURRENT-SECTION                  
133800                                                                          
133900*URVAL AV KOLLIN BEROENDE PÅ IFYLLDA NYCKLAR.                             
134000                                                                          
134100     IF WITHOUT-SEARCH-FIELD                                              
134200*       VALUE 'U'.                                                        
134300       PERFORM IMS-GN-KOLLI-WDE6F1                                        
134400     ELSE                                                                 
134500       IF SEARCH-WITH-IDSUPREF                                            
134600*         VALUE 'I'.                                                      
134700         PERFORM IMS-GN-SUPREF-WDE6F1                                     
134800       ELSE                                                               
134900         IF SEARCH-WITH-IDDISTR                                           
135000*           VALUE 'D'.                                                    
135100           PERFORM IMS-GN-DISTR-WDE6F1                                    
135200         ELSE                                                             
135300           IF SEARCH-WITH-IDSUPREF-IDDISTR                                
135400*             VALUE 'X'.                                                  
135500             PERFORM IMS-GN-DISTR-IDSUPREF-WDE6F1                         
135600           END-IF                                                         
135700         END-IF                                                           
135800       END-IF                                                             
135900     END-IF                                                               
136000     .                                                                    
136100     EJECT                                                                
136200                                                                          
136300 S01-SKAPA-TRANS-TILL-SV-AF SECTION.                                      
136400     MOVE 'S01-SKAPA-TRANS-TILL-SV-AF'  TO WS-CURRENT-SECTION             
136500                                                                          
136600**   SKAPA INFO TILL SVENSKA ÅF, SÄNDS VIA VR.                            
136700     IF DIST03-SVERIGE-100-799                                            
136800        OR DIST03-NORGE                                                   
136900        OR DIST03-DANMARK-900                                             
137000        OR DIST85-PU-VIA-VR                                               
137100        OR DIST21-TYRE                                                    
137200       MOVE W-IDPRODNR         TO 4322-IDPRODNR                           
137300       MOVE KOLLI-IDKOLLI      TO 4322-IDKOLLI                            
137400*      MOVE 4322-WDGX4322      TO 4322-WDGX4322                           
137500       PERFORM IMS-ISRT-4322-SEGM                                         
137600                                                                          
137700* EN LITEN UPPDATERING AV KOLLIT FÖR ATT UNDVIKA DUBBLA TRANSAR           
137800       MOVE WS-DAGDAT-AAVVD    TO KOLLI-TIAAVVD-PATR                      
137900     END-IF                                                               
138000     .                                                                    
138100     EJECT                                                                
138200                                                                          
138300 MFS-ERASE-FIELD-OUT SECTION.                                             
138400     MOVE 'MFS-ERASE-FIELD-OUT  ' TO WS-CURRENT-SECTION                   
138500                                                                          
138600     MOVE +1 TO INDX                                                      
138700     PERFORM UNTIL INDX > MAX-RADINDX                                     
138800       MOVE MFS-ERASE-FIELD TO MOD-IDDISTR-UPD    (INDX)                  
138900       MOVE MFS-ERASE-FIELD TO MOD-IDKUNDNR-UPD   (INDX)                  
139000       MOVE MFS-ERASE-FIELD TO MOD-IDORDNR7-UPD   (INDX)                  
139100       MOVE MFS-ERASE-FIELD TO MOD-IDKOLLI-UPD    (INDX)                  
139200       MOVE MFS-ERASE-FIELD TO MOD-NOTIFICATION(INDX)                     
139300       ADD +1 TO INDX                                                     
139400     END-PERFORM                                                          
139500     .                                                                    
139600     EJECT                                                                
139700                                                                          
139800 MFS-ERASE-FIELD-IN SECTION.                                              
139900                                                                          
140000*    --- ALLA INDATA-FÄLT                                                 
140100                                                                          
140200     MOVE +1 TO INDX                                                      
140300     PERFORM UNTIL INDX > MAX-RADINDX                                     
140400       MOVE MFS-RENSA-FAELT TO MID-IDDISTR-UPD    (INDX)                  
140500                               MID-IDKUNDNR-UPD   (INDX)                  
140600                               MID-IDORDNR7-UPD   (INDX)                  
140700                               MID-IDKOLLI-UPD    (INDX)                  
140800       ADD +1 TO INDX                                                     
140900     END-PERFORM                                                          
141000     .                                                                    
141100     EJECT                                                                
141200 MFS-DONT-TOUCH-FIELD-OUT  SECTION.                                       
141300                                                                          
141400*    --- ALLA UTDATA-FÄLT                                                 
141500     MOVE +1 TO INDX                                                      
141600     PERFORM UNTIL INDX > MAX-RADINDX                                     
141700                                                                          
141800       MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-IDDISTR-UPD(INDX)               
141900                                     MOD-IDKUNDNR-UPD(INDX)               
142000                                     MOD-IDORDNR7-UPD(INDX)               
142100                                     MOD-IDKOLLI-UPD(INDX)                
142200*                                    MOD-NOTIFICATION(INDX)               
142300       ADD +1 TO INDX                                                     
142400     END-PERFORM                                                          
142500     .                                                                    
142600     SKIP3                                                                
142700 MFS-DONT-TOUCH-FIELD-IN  SECTION.                                        
142800                                                                          
142900*    --- ALLA INDATA-FÄLT                                                 
143000*    MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-XXXXXXXX-IN                       
143100*                                MOD-XXXXXXXX-IN                          
143200     MOVE +1 TO INDX                                                      
143300     PERFORM UNTIL INDX > MAX-RADINDX                                     
143400                                                                          
143500       MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-IDDISTR-UPD(INDX)               
143600                                     MOD-IDKUNDNR-UPD(INDX)               
143700                                     MOD-IDORDNR7-UPD(INDX)               
143800                                     MOD-IDKOLLI-UPD(INDX)                
143900*                                    MOD-NOTIFICATION(INDX)               
144000       ADD +1 TO INDX                                                     
144100     END-PERFORM                                                          
144200     .                                                                    
144300     EJECT                                                                
144400* --- IMS SECTIONS ---                                                    
144500     SKIP3                                                                
144600 IMS-GET-MSG SECTION.                                                     
144700                                                                          
144800     MOVE '  QC' TO GOOD-STATUSCODES                                      
144900     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
145000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
145100     PERFORM IMS-STATUSCHECK                                              
145200     .                                                                    
145300     SKIP3                                                                
145400 IMS-INSERT-MSG SECTION.                                                  
145500                                                                          
145600     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
145700     MOVE SPACE TO GOOD-STATUSCODES                                       
145800     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
145900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
146000     PERFORM IMS-STATUSCHECK                                              
146100     .                                                                    
146200     EJECT                                                                
146300 IMS-GU-WDE4A1       SECTION.                                             
146400     MOVE 'IMS-GU-WDE4A1'  TO WS-CURRENT-IMS-SECTION                      
146500                                                                          
146600     STRING 'WDE4A1  (WDE4A1KY>=' W-WDE4A1KY-MIN-X                        
146700                    '&WDE4A1KY<=' W-WDE4A1KY-MAX-X ')'                    
146800     DELIMITED BY SIZE INTO SSA1                                          
146900     MOVE '  GE' TO GOOD-STATUSCODES                                      
147000     CALL CBLTDLI USING GU WDE4A-PCB DLI-IO-WDE4A1 SSA1                   
147100     MOVE WDE4A-STATUS-CODE TO STATUS-WS                                  
147200     PERFORM IMS-STATUSCHECK                                              
147300     .                                                                    
147400     SKIP3                                                                
147500 IMS-GN-WDE4A1       SECTION.                                             
147600     MOVE 'IMS-GU-WDE4A1'  TO WS-CURRENT-IMS-SECTION                      
147700                                                                          
147800     STRING 'WDE4A1  (WDE4A1KY>=' W-WDE4A1KY-MIN-X                        
147900                    '&WDE4A1KY<=' W-WDE4A1KY-MAX-X ')'                    
148000     DELIMITED BY SIZE INTO SSA1                                          
148100     MOVE '  GE' TO GOOD-STATUSCODES                                      
148200     CALL CBLTDLI USING GN WDE4A-PCB DLI-IO-WDE4A1 SSA1                   
148300     MOVE WDE4A-STATUS-CODE TO STATUS-WS                                  
148400     PERFORM IMS-STATUSCHECK                                              
148500     .                                                                    
148600     SKIP3                                                                
148700 IMS-GU-WDE611 SECTION.                                                   
148800     MOVE 'IMS-GU-WDE611    '   TO WS-CURRENT-IMS-SECTION                 
148900                                                                          
149000     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
149100            DELIMITED BY SIZE INTO SSA1                                   
149200     STRING 'WDE611  (IDKOLLI  =' W-IDKOLLI-X ')'                         
149300            DELIMITED BY SIZE INTO SSA2                                   
149400     MOVE '  '              TO GOOD-STATUSCODES                           
149500     CALL CBLTDLI USING GU WDE6-PCB DLI-IO-WDE611 SSA1 SSA2               
149600     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
149700     PERFORM IMS-STATUSCHECK                                              
149800     .                                                                    
149900                                                                          
150000 IMS-GN-DISTR-WDE6F1 SECTION.                                             
150100     MOVE 'IMS-GN-DISTR-WDE6F1 '   TO WS-CURRENT-IMS-SECTION              
150200                                                                          
150300     STRING 'WDE6F1  (WDE6F1KY>=' W-WDE6F1KY-MIN-X                        
150400                    '&WDE6F1KY<=' W-WDE6F1KY-MAX-X                        
150500                    '&IDDISTR  =' W-IDDISTR-F1-X ')'                      
150600            DELIMITED BY SIZE INTO SSA1                                   
150700     MOVE '  GEGB'            TO GOOD-STATUSCODES                         
150800     CALL CBLTDLI USING GN  WDE6F-PCB DLI-IO-WDE6F1 SSA1                  
150900     MOVE WDE6F-STATUS-CODE TO STATUS-WS                                  
151000     PERFORM IMS-STATUSCHECK                                              
151100     .                                                                    
151200     EJECT                                                                
151300 IMS-GU-DISTR-WDE6F1 SECTION.                                             
151400     MOVE 'IMS-GU-DISTR-WDE6F1 '   TO WS-CURRENT-IMS-SECTION              
151500                                                                          
151600     STRING 'WDE6F1  (WDE6F1KY>=' W-WDE6F1KY-MIN-X                        
151700                    '&WDE6F1KY<=' W-WDE6F1KY-MAX-X                        
151800                    '&IDDISTR  =' W-IDDISTR-F1-X ')'                      
151900            DELIMITED BY SIZE INTO SSA1                                   
152000     MOVE '  GE'            TO GOOD-STATUSCODES                           
152100     CALL CBLTDLI USING GU  WDE6F-PCB DLI-IO-WDE6F1 SSA1                  
152200     MOVE WDE6F-STATUS-CODE TO STATUS-WS                                  
152300     PERFORM IMS-STATUSCHECK                                              
152400     .                                                                    
152500     EJECT                                                                
152600 IMS-GN-DISTR-IDSUPREF-WDE6F1 SECTION.                                    
152700     MOVE 'IMS-GN-DISTR-IDSUPREF-WDE6F1'                                  
152800       TO WS-CURRENT-IMS-SECTION                                          
152900                                                                          
153000     STRING 'WDE6F1  (WDE6F1KY>=' W-WDE6F1KY-MIN-X                        
153100                    '&WDE6F1KY<=' W-WDE6F1KY-MAX-X                        
153200                    '&IDSUPREF =' W-IDSUPREF-F1-X                         
153300                    '&IDDISTR  =' W-IDDISTR-F1-X ')'                      
153400            DELIMITED BY SIZE INTO SSA1                                   
153500     MOVE '  GEGB'            TO GOOD-STATUSCODES                         
153600     CALL CBLTDLI USING GN  WDE6F-PCB DLI-IO-WDE6F1 SSA1                  
153700     MOVE WDE6F-STATUS-CODE TO STATUS-WS                                  
153800     PERFORM IMS-STATUSCHECK                                              
153900     .                                                                    
154000     EJECT                                                                
154100 IMS-GU-DISTR-IDSUPREF-WDE6F1 SECTION.                                    
154200     MOVE 'IMS-GU-DISTR-IDSUPREF-WDE6F1' TO WS-CURRENT-IMS-SECTION        
154300                                                                          
154400     STRING 'WDE6F1  (WDE6F1KY>=' W-WDE6F1KY-MIN-X                        
154500                    '&WDE6F1KY<=' W-WDE6F1KY-MAX-X                        
154600                    '&IDSUPREF =' W-IDSUPREF-F1-X                         
154700                    '&IDDISTR  =' W-IDDISTR-F1-X ')'                      
154800            DELIMITED BY SIZE INTO SSA1                                   
154900     MOVE '  GE'            TO GOOD-STATUSCODES                           
155000     CALL CBLTDLI USING GU  WDE6F-PCB DLI-IO-WDE6F1 SSA1                  
155100     MOVE WDE6F-STATUS-CODE TO STATUS-WS                                  
155200     PERFORM IMS-STATUSCHECK                                              
155300     .                                                                    
155400     EJECT                                                                
155500 IMS-GN-KOLLI-WDE6F1 SECTION.                                             
155600     MOVE 'IMS-GN-KOLLI-WDE6F1 '  TO WS-CURRENT-IMS-SECTION               
155700                                                                          
155800     STRING 'WDE6F1  (WDE6F1KY>=' W-WDE6F1KY-MIN-X                        
155900                    '&WDE6F1KY<=' W-WDE6F1KY-MAX-X ')'                    
156000            DELIMITED BY SIZE INTO SSA1                                   
156100     MOVE '  GEGB'          TO GOOD-STATUSCODES                           
156200     CALL CBLTDLI USING GN  WDE6F-PCB DLI-IO-WDE6F1 SSA1                  
156300     MOVE WDE6F-STATUS-CODE TO STATUS-WS                                  
156400     PERFORM IMS-STATUSCHECK                                              
156500     .                                                                    
156600     EJECT                                                                
156700 IMS-GU-KOLLI-WDE6F1 SECTION.                                             
156800     MOVE 'IMS-GU-KOLLI-WDE6F1 '  TO WS-CURRENT-IMS-SECTION               
156900                                                                          
157000     STRING 'WDE6F1  (WDE6F1KY>=' W-WDE6F1KY-MIN-X                        
157100                    '&WDE6F1KY<=' W-WDE6F1KY-MAX-X ')'                    
157200            DELIMITED BY SIZE INTO SSA1                                   
157300     MOVE '  GE'          TO GOOD-STATUSCODES                             
157400     CALL CBLTDLI USING GU  WDE6F-PCB DLI-IO-WDE6F1 SSA1                  
157500     MOVE WDE6F-STATUS-CODE TO STATUS-WS                                  
157600     PERFORM IMS-STATUSCHECK                                              
157700     .                                                                    
157800     EJECT                                                                
157900 IMS-GU-SUPREF-WDE6F1 SECTION.                                            
158000     MOVE 'IMS-GU-SUPREF-WDE6F1 '  TO WS-CURRENT-IMS-SECTION              
158100                                                                          
158200     STRING 'WDE6F1  (WDE6F1KY>=' W-WDE6F1KY-MIN-X                        
158300                    '&WDE6F1KY<=' W-WDE6F1KY-MAX-X                        
158400                    '&IDSUPREF =' W-IDSUPREF-F1-X ')'                     
158500            DELIMITED BY SIZE INTO SSA1                                   
158600     MOVE '  GE'          TO GOOD-STATUSCODES                             
158700     CALL CBLTDLI USING GU  WDE6F-PCB DLI-IO-WDE6F1 SSA1                  
158800     MOVE WDE6F-STATUS-CODE TO STATUS-WS                                  
158900     PERFORM IMS-STATUSCHECK                                              
159000     .                                                                    
159100     EJECT                                                                
159200 IMS-GN-SUPREF-WDE6F1 SECTION.                                            
159300     MOVE 'IMS-GN-SUPREF-WDE6F1 '  TO WS-CURRENT-IMS-SECTION              
159400                                                                          
159500     STRING 'WDE6F1  (WDE6F1KY>=' W-WDE6F1KY-MIN-X                        
159600                    '&WDE6F1KY<=' W-WDE6F1KY-MAX-X                        
159700                    '&IDSUPREF =' W-IDSUPREF-F1-X ')'                     
159800            DELIMITED BY SIZE INTO SSA1                                   
159900     MOVE '  GEGB'          TO GOOD-STATUSCODES                           
160000     CALL CBLTDLI USING GN  WDE6F-PCB DLI-IO-WDE6F1 SSA1                  
160100     MOVE WDE6F-STATUS-CODE TO STATUS-WS                                  
160200     PERFORM IMS-STATUSCHECK                                              
160300     .                                                                    
160400     EJECT                                                                
160500 IMS-GHU-WDE601 SECTION.                                                  
160600     MOVE 'IMS-GHU-WDE601   '   TO WS-CURRENT-IMS-SECTION                 
160700                                                                          
160800     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
160900          DELIMITED BY SIZE INTO SSA1                                     
161000     MOVE '  ' TO GOOD-STATUSCODES                                        
161100     CALL CBLTDLI USING GHU WDE6-PCB DLI-IO-WDE601 SSA1                   
161200     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
161300     PERFORM IMS-STATUSCHECK                                              
161400     .                                                                    
161500     SKIP3                                                                
161600 IMS-REPL-WDE601 SECTION.                                                 
161700     MOVE 'IMS-REPL-WDE601  '   TO WS-CURRENT-IMS-SECTION                 
161800                                                                          
161900     MOVE '  ' TO GOOD-STATUSCODES                                        
162000     CALL CBLTDLI USING REPL WDE6-PCB DLI-IO-WDE601                       
162100     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
162200     PERFORM IMS-STATUSCHECK                                              
162300     .                                                                    
162400     EJECT                                                                
162500 IMS-REPL-WDE611 SECTION.                                                 
162600     MOVE 'IMS-REPL-WDE611  '   TO WS-CURRENT-IMS-SECTION                 
162700                                                                          
162800     MOVE '  ' TO GOOD-STATUSCODES                                        
162900     CALL CBLTDLI USING REPL WDE6-PCB DLI-IO-WDE611                       
163000     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
163100     PERFORM IMS-STATUSCHECK                                              
163200     .                                                                    
163300     EJECT                                                                
163400                                                                          
163500 IMS-GHNP-WDE611       SECTION.                                           
163600     MOVE ' IMS-GHNP-WDE611  '   TO WS-CURRENT-IMS-SECTION                
163700                                                                          
163800     STRING 'WDE611  (IDKOLLI  =' W-IDKOLLI-X ')'                         
163900            DELIMITED BY SIZE INTO SSA1                                   
164000     MOVE '  GE'            TO GOOD-STATUSCODES                           
164100     CALL CBLTDLI USING GHNP WDE6-PCB DLI-IO-WDE611 SSA1                  
164200     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
164300     PERFORM IMS-STATUSCHECK                                              
164400     .                                                                    
164500                                                                          
164600 IMS-ISRT-WDR601           SECTION.                                       
164700     MOVE 'IMS-ISRT-WDR601  '   TO WS-CURRENT-IMS-SECTION                 
164800                                                                          
164900     MOVE 'WLFILA01 ' TO SSA1                                             
165000     MOVE '  II' TO GOOD-STATUSCODES                                      
165100     CALL CBLTDLI USING ISRT WDR6-PCB DLI-IO-WDR601 SSA1                  
165200     MOVE WDR6-STATUS-CODE TO STATUS-WS                                   
165300     PERFORM IMS-STATUSCHECK                                              
165400     .                                                                    
165500     EJECT                                                                
165600                                                                          
165700*WLXXDV01                                                                 
165800 IMS-GHU-4726-ROT SECTION.                                                
165900     MOVE 'IMS-GHU-4726-ROT '   TO WS-CURRENT-IMS-SECTION                 
166000                                                                          
166100     STRING 'WDG701  (WDGXKEY  =' W-4726-WDGXKEY-ROT-X ')'                
166200            DELIMITED BY SIZE INTO SSA1                                   
166300     MOVE '  ' TO GOOD-STATUSCODES                                        
166400     CALL CBLTDLI USING GHU    WDG7-PCB DLI-IO-WDGX4726 SSA1              
166500     MOVE WDG7-STATUS-CODE TO STATUS-WS                                   
166600     PERFORM IMS-STATUSCHECK                                              
166700     .                                                                    
166800                                                                          
166900 IMS-GHNP-4726-UNDERSEG SECTION.                                          
167000     MOVE 'IMS-GHNP-4726-UNDERSEG '   TO WS-CURRENT-IMS-SECTION           
167100                                                                          
167200     STRING 'WDG717  (WDGXKEY  =' W-4726-WDGXKEY-UNDSEG-X ')'             
167300            DELIMITED BY SIZE INTO SSA1                                   
167400     MOVE '  GE'           TO GOOD-STATUSCODES                            
167500     CALL CBLTDLI USING GHNP   WDG7-PCB DLI-IO-WDGX4727 SSA1              
167600     MOVE WDG7-STATUS-CODE TO STATUS-WS                                   
167700     PERFORM IMS-STATUSCHECK                                              
167800     .                                                                    
167900                                                                          
168000 IMS-INSERT-4726-UNDERSEG SECTION.                                        
168100     MOVE 'IMS-INSERT-4726-UNDERSEG'  TO WS-CURRENT-IMS-SECTION           
168200                                                                          
168300     STRING 'WDG701  (WDGXKEY  =' W-4726-WDGXKEY-ROT-X ')'                
168400            DELIMITED BY SIZE INTO SSA1                                   
168500     MOVE 'WDG717   '      TO SSA2                                        
168600     MOVE '  ' TO GOOD-STATUSCODES                                        
168700     CALL CBLTDLI USING ISRT WDG7-PCB DLI-IO-WDGX4726 SSA1 SSA2           
168800     MOVE WDG7-STATUS-CODE TO STATUS-WS                                   
168900     PERFORM IMS-STATUSCHECK                                              
169000     .                                                                    
169100                                                                          
169200 IMS-INSERT-4727 SECTION.                                                 
169300     MOVE 'IMS-INSERT-4727     '  TO WS-CURRENT-IMS-SECTION               
169400                                                                          
169500     STRING 'WDG701  (WDGXKEY  =' W-4726-WDGXKEY-ROT-X ')'                
169600            DELIMITED BY SIZE INTO SSA1                                   
169700     STRING 'WDG717  (WDGXKEY  =' W-4726-WDGXKEY-UNDSEG-X ')'             
169800            DELIMITED BY SIZE INTO SSA2                                   
169900     MOVE 'WDG718   '      TO SSA3                                        
170000     MOVE '  II'           TO GOOD-STATUSCODES                            
170100     CALL CBLTDLI USING ISRT WDG7-PCB DLI-IO-WDGX4727                     
170200                               SSA1 SSA2 SSA3                             
170300     MOVE WDG7-STATUS-CODE TO STATUS-WS                                   
170400     PERFORM IMS-STATUSCHECK                                              
170500     .                                                                    
170600                                                                          
170700 IMS-DLET-4726-UNDERSEG SECTION.                                          
170800     MOVE 'IMS-DLET-4726-UNDERSEG  '  TO WS-CURRENT-IMS-SECTION           
170900                                                                          
171000     MOVE '  '             TO GOOD-STATUSCODES                            
171100     CALL CBLTDLI USING DLET WDG7-PCB DLI-IO-WDGX4726                     
171200     MOVE WDG7-STATUS-CODE TO STATUS-WS                                   
171300     PERFORM IMS-STATUSCHECK                                              
171400     .                                                                    
171500     EJECT                                                                
171600*WLXXDV                                                                   
171700                                                                          
171800 IMS-ISRT-4322-SEGM SECTION.                                              
171900     MOVE 'IMS-ISRT-4322-SEGM    '  TO WS-CURRENT-IMS-SECTION             
172000                                                                          
172100     STRING 'WDG201  (WDGXKEY  =' W-4321-IDHTYP-X ')'                     
172200            DELIMITED BY SIZE INTO SSA1                                   
172300     MOVE 'WDG202  *L' TO SSA2                                            
172400     MOVE '  ' TO GOOD-STATUSCODES                                        
172500     CALL CBLTDLI USING ISRT WDG2-PCB 4322-WDGX4322 SSA1 SSA2             
172600     MOVE WDG2-STATUS-CODE TO STATUS-WS                                   
172700     PERFORM IMS-STATUSCHECK                                              
172800     .                                                                    
172900     SKIP2                                                                
172910 IMS-ISRT-WDE621 SECTION.                                                 
172920                                                                          
172930     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
172940          DELIMITED BY SIZE INTO SSA1                                     
172950     STRING 'WDE611  (IDKOLLI  =' W-IDKOLLI-X ')'                         
172960          DELIMITED BY SIZE INTO SSA2                                     
172970     MOVE 'WDE621 ' TO SSA3                                               
172980     MOVE '  II' TO GOOD-STATUSCODES                                      
172990     CALL CBLTDLI USING ISRT WDE6-PCB DLI-IO-WDE621 SSA1 SSA2 SSA3        
172991     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
172992     PERFORM IMS-STATUSCHECK                                              
172993     .                                                                    
172994     EJECT                                                                
173000 IMS-STATUSCHECK SECTION.                                                 
173100                                                                          
173200     SET STATUS-IX TO 1                                                   
173300     SEARCH GOOD-STATUS                                                   
173400       AT END                                                             
173500         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
173600         DELIMITED BY SIZE INTO ERROR-TEXT                                
173700         CALL FELLOG                                                      
173800       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
173900         CONTINUE                                                         
174000     END-SEARCH                                                           
174100     .                                                                    
