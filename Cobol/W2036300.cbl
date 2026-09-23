000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W2036300.                                                
000400 AUTHOR.         STEFAN KIHLBERG.                                         
000500 DATE-WRITTEN.   96/10/29.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        UPPDATERAR DATAELEMENT SOM STYR REFILL                           
001000*                                                                         
001100*        PROGRAMMET UPPDATERAR WLARTC (WDK6)                              
001200*                              WDK7                                       
001300*                              WLOIGA (WDL7)                              
001400*                                                                         
001500*    INDATA.                                                              
001600*        TRANSAKTION: W2T363                                              
001700*        MID:         W2I36301                                            
001800*                                                                         
001900*    UTDATA.                                                              
002000*        MOD:         W2O36301                                            
002100                                                                          
002200     SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400     EJECT                                                                
002500 DATA DIVISION.                                                           
002600 WORKING-STORAGE SECTION.                                                 
002700                                                                          
002800*    -COPY WY2000W1                                                       
002900*    -- CHECKED BY WY2000                                                 
003000 77  IDPGM                       PIC X(08)   VALUE 'W2036300'.            
003100                                                                          
003200*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003300 01  FELTEXT.                                                             
003400     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
003500     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
003600     EJECT                                                                
003700                                                                          
003800                                                                          
003900 77  JA                          PIC X       VALUE 'J'.                   
004000 77  YES                         PIC X       VALUE 'Y'.                   
004100 77  NEJ                         PIC X       VALUE 'N'.                   
004200 77  FL-PRARTBES                 PIC X       VALUE 'N'.                   
004300 77  DAGENS-AAAAMMDD             PIC 9(8)    VALUE ZERO.                  
004400 77  AKTIV                       PIC X       VALUE 'A'.                   
004500 77  DEFINITIV                   PIC S9      VALUE +1 COMP-3.             
004600 77  IX                          PIC 9(3)    VALUE ZERO.                  
004700 77  IX-DC                       PIC 9(3)    VALUE ZERO.                  
004800 77  IX-DC-N                     PIC 9(3)    VALUE ZERO.                  
004900 77  IX-DC-MAX                   PIC 9(3)    VALUE ZERO.                  
005000 77  WS-IDARTNR                  PIC X(9)    VALUE SPACES.                
005100 77  WS-IDLEVNR-NUM              PIC X(05)   VALUE ZEROES.                
005200 77  WS-KDARTURS                 PIC X(2)    VALUE SPACES.                
005300 77  WS-FLREFERAL                PIC X(1)    VALUE SPACES.                
005400 77  WS-VKART                    PIC S9(7)   VALUE ZERO   COMP-3.         
005500 77  WS-VLARTNTO                 PIC S9(8)V9(1)                           
005600                                             VALUE ZERO   COMP-3.         
005700                                                                          
005800*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005900                                                                          
006000 77  INDATA-SW                   PIC X       VALUE 'J'.                   
006100     88  INDATA-OK                           VALUE 'J'.                   
006200     88  INDATA-FEL                          VALUE 'N'.                   
006300                                                                          
006400 77  SW-BUYER-UP                 PIC X       VALUE 'N'.                   
006500     88  BUYER-YES                           VALUE 'J'.                   
006600     88  BUYER-NO                            VALUE 'N'.                   
006700                                                                          
006800 77  SUPPLR-SW                   PIC X       VALUE 'J'.                   
006900     88  SUPPLR-OK                           VALUE 'J'.                   
007000     88  SUPPLR-FEL                          VALUE 'N'.                   
007100                                                                          
007200 77  UPD-WDK712-SW               PIC X       VALUE 'N'.                   
007300     88  UPD-WDK712-OK                       VALUE 'J'.                   
007400     88  UPD-WDK712-FEL                      VALUE 'N'.                   
007500                                                                          
007600 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
007700     88  NYCKLAR-OK                          VALUE 'J'.                   
007800     88  NYCKLAR-FEL                         VALUE 'N'.                   
007900                                                                          
008000 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
008100     88  EGEN-MID                            VALUE '2363'.                
008200     88  GODK-MID                            VALUE '2361' '2362'.         
008300     88  HELP-MID                            VALUE '0551'.                
008400     EJECT                                                                
008500*    --- ÖVRIGA FLAGGOR                                                   
008600 77  SW-AENDRA-WDK721            PIC X       VALUE 'N'.                   
008700                                                                          
008800*    --- FLAG FOR BUYER                                                   
008900 77  SW-BUYER                    PIC X       VALUE 'N'.                   
009000     88  BUYER-OK                            VALUE 'J'.                   
009100                                                                          
009200*    --- ÖVRIGA ARBETSFÄLT                                                
009300                                                                          
009400 01  ARBETSFALT.                                                          
009500     03 IDDC-WS                  PIC X(2)    VALUE SPACE.                 
009600     03 W-IDDC-IN                PIC X(2)    VALUE SPACE.                 
009700     03 IDDC-WS-NUM              PIC 9(2)    VALUE ZERO.                  
009800     03 WS-IDPERSON-BUY          PIC S9(3)   VALUE ZERO COMP-3.           
009900     03 WS-TEMFSINF              PIC X(55)   VALUE SPACE.                 
010000                                                                          
010100     03  WS-IDDC-SPAR            PIC X(2)    VALUE SPACE.                 
010200     03  WS-DC-NR                PIC X(2)    OCCURS 4.                    
010300                                                                          
010400     03 WS-IDDC-REF-GRP                      OCCURS 4.                    
010500        05 WS-IDDC-REF-SLAG      PIC X(2).                                
010600                                                                          
010700     03 WS-TEARTNOT-FL           PIC X(1)    OCCURS 4.                    
010800                                                                          
010900 01  DB2-WS.                                                              
011000     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
011100         88  CURSOR-OK                       VALUE 000.                   
011200         88  LINES-FOUND                     VALUE 000.                   
011300         88  LINES-MISSING                   VALUE 100.                   
011400         88  RESOURCE-WRONG                  VALUE 904.                   
011500     03  GOOD-SQLCODECODES.                                               
011600         05  GOOD-SQLCODE OCCURS 5                                        
011700             INDEXED BY SQLCODE-IX PIC 9(3).                              
011800                                                                          
011900 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
012000                                                                          
012100  1  DAGENS-TIAAVVD              PIC 9(5)    VALUE ZERO.                  
012200 01  FILLER REDEFINES DAGENS-TIAAVVD.                                     
012300     03 DAGENS-TIAAVV            PIC 9(4).                                
012400     03 DAGENS-TID               PIC 9(1).                                
012500                                                                          
012600 01  DATUMFALT.                                                           
012700     03 DAGENS-DATUM             PIC S9(7)   VALUE ZERO COMP-3.           
012800                                                                          
012900*    ---KONTROLLFÄLT                                                      
013000                                                                          
013100*                                                                         
013200*01    -COPY WWPRODSL                                                     
013300                                                                          
013400*      --- VALID IDDC CODES                                               
013500*                                                                         
013600*01    -COPY WWDCKONS                                                     
013700*01    -COPY WWDC99                                                       
013800       EJECT                                                              
013900                                                                          
014000*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
014100 01  GENERELLA-SUBPROGRAM.                                                
014200     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
014300     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
014400     03  W005WDK7                PIC X(8)    VALUE 'W005WDK7'.            
014500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
014600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
014700     03  ABEND                   PIC X(8)    VALUE 'ABEND  '.             
014800     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
014900     EJECT                                                                
015000*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
015100*01 -COPY WMEDAREA                                                        
015200     SKIP3                                                                
015300     EJECT                                                                
015400 01  MESSAGE-CODES.                                                       
015500     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
015600     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
015700     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
015800     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
015900     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
016000     03  PART-MISSING            PIC X(3)    VALUE '017'.                 
016100     03  PART-SUPERSEDED         PIC X(3)    VALUE '018'.                 
016200     03  MISSING-DC              PIC X(3)    VALUE '026'.                 
016300 01  MEDDELANDEN.                                                         
016400     03  MED-1                   PIC X(55)                                
016500         VALUE 'PART NOT AVAILABLE AT ANY NDC          '.                 
016600     03  MED-2                   PIC X(40)                                
016700         VALUE 'NO REFILLPART                          '.                 
016800     03  MED-3                   PIC X(40)                                
016900         VALUE 'CDC PART                               '.                 
017000     03  MED-4                   PIC X(40)                                
017100         VALUE 'WRONG ORIGIN CODE                      '.                 
017200     03  MED-5                   PIC X(40)                                
017300         VALUE 'NO ORIGIN CODE AVAILABLE               '.                 
017400     03  MED-6                   PIC X(40)                                
017500         VALUE 'LOCAL PART                             '.                 
017600     03  MED-7                   PIC X(40)                                
017700         VALUE 'UNKNOWN SUPPLIER                       '.                 
017800     03  MED-8                   PIC X(40)                                
017900         VALUE 'REFILL ORDER/PROPOSAL EXISTS           '.                 
018000     03  MED-9                   PIC X(40)                                
018100         VALUE 'SUPPLIER NUMBER SAME AS OWN DC         '.                 
018200     03  MED-10                  PIC X(40)                                
018300         VALUE 'LOCAL SUPPLIER NOT ALLOWED             '.                 
018400     03  MED-11                  PIC X(40)                                
018500         VALUE 'UPDATE OF LOCAL SUPPLIER NOT ALLOWED   '.                 
018600     03  MED-12                  PIC X(40)                                
018700         VALUE 'NOT ALLOWED SUPPLIER                   '.                 
018800     03  MED-13                  PIC X(40)                                
018900         VALUE 'UPDATING OF BUYER LOCKED               '.                 
019000     03  MED-14                  PIC X(40)                                
019100         VALUE 'FFC EXIST                              '.                 
019200     03  FEL-125                 PIC X(47)                                
019300         VALUE 'USE EQUAL SUPPLIER, SEE SCREEN 2111    '.                 
019400     EJECT                                                                
019500*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
019600 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
019700*01 -COPY WMSGINIT                                                        
019800     SKIP3                                                                
019900*    --- PARAMETRAR TILL SUBPROGRAM W005WDK7                              
020000 01  FILLER                      PIC X(16)   VALUE 'W005WDK7'.            
020100*01 -COPY W005WDK7                                                        
020200     SKIP3                                                                
020300*    --- PARAMETRAR TILL ABEND                                            
020400 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
020500 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
020600     EJECT                                                                
020700*01 -COPY WDATAREA                                                        
020800     SKIP3                                                                
020900*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
021000*                                                                         
021100 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
021200     SKIP3                                                                
021300*01  MID -COPY W2I36301                                                   
021400     EJECT                                                                
021500 01  FILLER                      PIC X(16)   VALUE 'MSG/MOD-AREA'.        
021600     SKIP3                                                                
021700*01  -COPY WMSGAREA                                                       
021800     EJECT                                                                
021900     03  MOD REDEFINES MSG-AREA.                                          
022000*      05  -COPY W2O36301                                                 
022100     EJECT                                                                
022200 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
022300     SKIP3                                                                
022400*01  -COPY WMFSAREA                                                       
022500     EJECT                                                                
022600*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
022700*                                                                         
022800     EJECT                                                                
022900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
023000     SKIP3                                                                
023100 01  NYCKLAR-TILL-DLI.                                                    
023200     03  W-IDARTNR-X.                                                     
023300         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
023400     03  W-DAPRLIST-X.                                                    
023500         05  W-DAPRLIST          PIC 9(8)    VALUE ZERO.                  
023600     03  W-KDSEGKEY-X.                                                    
023700         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
023800     03  W-IDDC-X.                                                        
023900         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
024000     03  W-IDDC-MIN-X.                                                    
024100         05  W-IDDC-MIN          PIC X(2)    VALUE SPACE.                 
024200     03  W-IDDC-MAX-X.                                                    
024300         05  W-IDDC-MAX          PIC X(2)    VALUE SPACE.                 
024400     03  W-IDLAND-X.                                                      
024500         05  W-IDLAND            PIC X(2)    VALUE SPACE.                 
024600     03  W-IDLANDX2-X.                                                    
024700         05  W-IDLANDX2          PIC X(2)    VALUE SPACE.                 
024800     03  W-WDGX2507-X.                                                    
024900         05  W-IDHTYP-2507       PIC X(4)     VALUE '2507'.               
025000         05  FILLER              PIC X(26)    VALUE LOW-VALUE.            
025100                                                                          
025200     03 W-WDE301KY-MIN-X.                                                 
025300         05  W-IDDC-E3MIN        PIC X(2)    VALUE SPACES.                
025400         05  W-IDPERSON-BUY-MIN  PIC S9(3)   VALUE ZERO COMP-3.           
025500         05  W-KDREFTYP-MIN      PIC X       VALUE SPACE.                 
025600         05  W-IDARTNR-MIN       PIC S9(9)   VALUE ZERO COMP-3.           
025700         05  W-IDDISTR-MIN       PIC S9(5)   VALUE ZERO COMP-3.           
025800                                                                          
025900     03 W-WDE301KY-MAX-X.                                                 
026000         05  W-IDDC-E3MAX        PIC X(2)    VALUE SPACES.                
026100         05  W-IDPERSON-BUY-MAX  PIC S9(3)   VALUE +999 COMP-3.           
026200         05  W-KDREFTYP-MAX      PIC X       VALUE HIGH-VALUE.            
026300         05  W-IDARTNR-MAX       PIC S9(9)   VALUE                        
026400                                               +999999999 COMP-3.         
026500         05  W-IDDISTR-MAX       PIC S9(5)   VALUE +99999 COMP-3.         
026600                                                                          
026700     03  W-IDLEVNR-X.                                                     
026800         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
026900     03  W-IDSKYLT-X.                                                     
027000         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
027100     03  W-IDDC-TP5-X.                                                    
027200         05  W-IDDC-TP5          PIC X(2)    VALUE SPACES.                
027300                                                                          
027400     03  W-IDDC-B6-X.                                                     
027500         05  W-IDDC-B6           PIC X(2)    VALUE SPACE.                 
027601                                                                          
027701     03  W-IDDC-REF-X.                                                    
027801         05  W-IDDC-REF          PIC X(2)    VALUE SPACE.                 
027901                                                                          
028001     03  W-WDB615KY-X.                                                    
028101         05  W-IDTRANS-B6        PIC X(4)    VALUE SPACE.                 
028201         05  W-IDDC-REF-B6       PIC X(2)    VALUE SPACE.                 
028300     SKIP2                                                                
028400*    --- STATUS-KOD FRÅN IMS                                              
028500 01  STATUS-WS                   PIC XX.                                  
028600     88  SEGMENT-FINNS                       VALUE '  '.                  
028700     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
028800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
028900     88  SEGMENT-SLUT                        VALUE 'GB'.                  
029000     SKIP2                                                                
029100 01  GODK-STATUSKODER.                                                    
029200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
029300     SKIP3                                                                
029400 01  SSA1                        PIC X(64).                               
029500 01  SSA2                        PIC X(64).                               
029600 01  SSA3                        PIC X(64).                               
029700     EJECT                                                                
029800*    --- IMS FUNKTIONSKODER                                               
029900*01  -COPY W0003                                                          
030000     EJECT                                                                
030100 01   DLI-IO-AREA-B601.                                                   
030200*     03  -COPY WDB601                                                    
030300*    ---  DLI INPUT-OUTPUT AREA                                           
030400                                                                          
030410 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDB615'.             
030420 01  DLI-IO-WDB615.                                                       
030430*    03  -COPY WDB615                                                     
030431                                                                          
030440 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDB616'.             
030450 01  DLI-IO-WDB616.                                                       
030460*    03  -COPY WDB616                                                     
030470                                                                          
030500 01  FILLER         PIC X(24) VALUE 'DLI-IO-ARTC01'.                      
030600 01  DLI-IO-ARTC01.                                                       
030700*    03  -COPY WDK601                                                     
030800     EJECT                                                                
030900                                                                          
031000 01  FILLER         PIC X(24) VALUE 'DLI-IO-ARTC11'.                      
031100 01  DLI-IO-ARTC11.                                                       
031200*    03  -COPY WDK611                                                     
031300                                                                          
031400 01  FILLER         PIC X(24) VALUE 'DLI-IO-ARTC21'.                      
031500 01  DLI-IO-ARTC21.                                                       
031600*    03  -COPY WDK621                                                     
031700                                                                          
031800 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK701'.                      
031900 01  DLI-IO-WDK701.                                                       
032000*    03  -COPY WDK701                                                     
032100                                                                          
032200 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK711'.                      
032300 01  DLI-IO-WDK711.                                                       
032400*    03  -COPY WDK711                                                     
032500     EJECT                                                                
032600                                                                          
032700 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK721'.                      
032800 01  DLI-IO-WDK721.                                                       
032900*    03  -COPY WDK721                                                     
033000     EJECT                                                                
033100                                                                          
033200 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK712'.                      
033300 01  DLI-IO-WDK712.                                                       
033400*    03  -COPY WDK712                                                     
033500     EJECT                                                                
033600                                                                          
033700 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK727'.                      
033800 01  DLI-IO-WDK727.                                                       
033900*    03  -COPY WDK727                                                     
034000     EJECT                                                                
034100                                                                          
034200 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDE301'.                      
034300 01  DLI-IO-WDE301.                                                       
034400*    03  -COPY WDE301                                                     
034500     EJECT                                                                
034600                                                                          
034700 01  FILLER         PIC X(24) VALUE 'DLI-IO-LEVA01'.                      
034800 01  DLI-IO-LEVA01.                                                       
034900*    03  -COPY WDF101                                                     
035000     EJECT                                                                
035100                                                                          
035200 01  FILLER         PIC X(24) VALUE 'DLI-IO-BENA01'.                      
035300 01  DLI-IO-BENA01.                                                       
035400*    03  -COPY WDD301  -PRE BENA-                                         
035500     EJECT                                                                
035600                                                                          
035700 01  FILLER         PIC X(24) VALUE 'DLI-IO-BENA11'.                      
035800 01  DLI-IO-BENA11.                                                       
035900*    03  -COPY WDD311  -PRE BENA-                                         
036000     EJECT                                                                
036100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX2508'.                    
036200 01  DLI-IO-WDGX2508.                                                     
036300*    03  -COPY WDGX2508                                                   
036400     EJECT                                                                
036500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX2510'.                    
036600 01  DLI-IO-WDGX2510.                                                     
036700*    03  -COPY WDGX2510                                                   
036800     EJECT                                                                
036900                                                                          
037000 01  FILLER         PIC X(16) VALUE 'SQLCA-AREA'.                         
037100       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
037200       EXEC SQL INCLUDE TP5IDDC END-EXEC.                                 
037300       EXEC SQL INCLUDE TP4TRAN END-EXEC.                                 
037400                                                                          
037500 01  FILLER         PIC X(16) VALUE 'TP5IDDC-AREA'.                       
037600                                                                          
037700*01  -COPY TP5IDDC -PRE TP5IDDC-                                          
037800 LINKAGE SECTION.                                                         
037900*01  -COPY W0009   -PRE MSG-                                              
038000                                                                          
038100*01  -COPY W0008   -PRE USEA-                                             
038200     05  FILLER                  PIC X.                                   
038300     EJECT                                                                
038400                                                                          
038500*01  -COPY W0008  -PRE ARTC-                                              
038600     05  FILLER                  PIC X.                                   
038700     EJECT                                                                
038800                                                                          
038900*01  -COPY W0008  -PRE WDK7-                                              
039000     05  FILLER                  PIC X.                                   
039100     EJECT                                                                
039200                                                                          
039300*01  -COPY W0008  -PRE WDK72-                                             
039400     05  FILLER                  PIC X.                                   
039500     EJECT                                                                
039600                                                                          
039700*01  -COPY W0008  -PRE LEVA-                                              
039800     05  FILLER                  PIC X.                                   
039900     EJECT                                                                
040000                                                                          
040100*01  -COPY W0008  -PRE BENA-                                              
040200     05  FILLER                  PIC X.                                   
040300     EJECT                                                                
040400                                                                          
040500*01  -COPY W0008  -PRE REFL-2501-                                         
040600     05  FILLER                  PIC X.                                   
040700*01  -COPY W0008  -PRE OIGA-                                              
040800     05  FILLER                  PIC X.                                   
040900     EJECT                                                                
041000*01  -COPY W0008  -PRE WDB6-                                              
041100     05  FILLER                  PIC X.                                   
041200     EJECT                                                                
041300                                                                          
041400*01  -COPY W0008  -PRE WDE3-                                              
041500     05  FILLER                  PIC X.                                   
041600     EJECT                                                                
041700                                                                          
041800*01  -COPY W0008  -PRE WDR5-                                              
041900     05  FILLER                  PIC X.                                   
042000     EJECT                                                                
042100                                                                          
042200 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB                               
042300                           ARTC-PCB WDK7-PCB WDK72-PCB LEVA-PCB           
042400                           BENA-PCB REFL-2501-PCB OIGA-PCB                
042500                           WDB6-PCB WDE3-PCB WDR5-PCB.                    
042600 MAIN SECTION.                                                            
042700     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB                               
042800                           ARTC-PCB WDK7-PCB WDK72-PCB LEVA-PCB           
042900                           BENA-PCB REFL-2501-PCB OIGA-PCB                
043000                           WDB6-PCB WDE3-PCB WDR5-PCB.                    
043100     PERFORM IMS-GET-MSG                                                  
043200     IF SEGMENT-FINNS                                                     
043300        PERFORM A-INIT                                                    
043400        PERFORM B-KOLLA-NYCKLAR                                           
043500        IF NYCKLAR-OK                                                     
043600           IF MFS-UPDATE                                                  
043700              PERFORM G-KOLLA-INPUT                                       
043800              IF INDATA-OK                                                
043900                 PERFORM H-UPPDATERA                                      
044000              END-IF                                                      
044100           ELSE                                                           
044200              IF MFS-FIRST                                                
044300                 PERFORM C-FOERSTA-SIDA                                   
044400              ELSE                                                        
044500                 PERFORM E-SAMMA-SIDA                                     
044600              END-IF                                                      
044700           END-IF                                                         
044800           PERFORM F-LAES-VISA-INFO                                       
044900        END-IF                                                            
045000        COMPUTE MSG-KVLL = LENGTH OF MOD-W2O36301 + 4                     
045100        PERFORM IMS-INSERT-MSG                                            
045200     END-IF                                                               
045300                                                                          
045400     MOVE ZERO TO RETURN-CODE                                             
045500     GOBACK                                                               
045600     .                                                                    
045700     EJECT                                                                
045800                                                                          
045900                                                                          
046000 A-INIT SECTION.                                                          
046100     ACCEPT DAGENS-DATUM FROM DATE                                        
046200                                                                          
046300     IF MSG-DUBBLA-TRANSKODER                                             
046400       MOVE MSG-INDATA-MINUS-2-TRANSKODER                                 
046500                            TO MID-W2I36301                               
046600       MOVE MSG-IDTRANS-2   TO MFS-IDTRANS                                
046700       MOVE MSG-KDMFSFOR-2  TO MFS-KDMFSFOR                               
046800     ELSE                                                                 
046900       MOVE MSG-INDATA-MINUS-1-TRANSKOD                                   
047000                            TO MID-W2I36301                               
047100       MOVE MSG-IDTRANS-1   TO MFS-IDTRANS                                
047200       MOVE MSG-KDMFSFOR-1  TO MFS-KDMFSFOR                               
047300     END-IF                                                               
047400                                                                          
047500     MOVE MSG-KDTRTYP       TO MFS-KDTRTYP                                
047600     MOVE MSG-IDPFK         TO MFS-IDPFK                                  
047700     MOVE MFS-IDTRANS       TO W-IDTRANS                                  
047800                                                                          
047900     MOVE LOW-VALUE         TO MSG-AREA                                   
048000     MOVE 'W2O363N1'        TO MFS-IDMOD                                  
048100     MOVE '2363'            TO MOD-IDTRANS                                
048200     MOVE MFS-RENSA-FAELT   TO MOD-TEMFSFEL MOD-TEMFSINF                  
048300                                                                          
048400     IF MSGI-IDLAND-SPR = 'SE'                                            
048500       MOVE '0'             TO MFS-KDHUVOMR                               
048600     END-IF                                                               
048700                                                                          
048800     IF EGEN-MID OR HELP-MID                                              
048900       CONTINUE                                                           
049000     ELSE                                                                 
049100       MOVE SPACE           TO MFS-KDTRTYP                                
049200       MOVE '7'             TO MFS-IDPFK                                  
049300     END-IF                                                               
049400     MOVE +4                TO IX-DC-MAX                                  
049500     .                                                                    
049600     EJECT                                                                
049700                                                                          
049800 B-KOLLA-NYCKLAR SECTION.                                                 
049900                                                                          
050000     MOVE ALL '+'           TO MSGI-WMSGINIT                              
050100     MOVE '001'             TO MSGI-KDCALL                                
050200     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
050300     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
050400     MOVE '2363'            TO MSGI-IDTRANS                               
050500     IF EGEN-MID                                                          
050600     OR  MID-IDARTNR-IN     NUMERIC                                       
050700     AND MID-IDARTNR-IN      > ZERO                                       
050800         MOVE MID-IDARTNR-IN                                              
050900                            TO MSGI-IDARTNR                               
051000         MOVE MID-IDDC-IN   TO MSGI-IDDC-KEY                              
051100     END-IF                                                               
051200     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
051300                                                                          
051400     IF MSGI-IDLAND-SPR = 'SE'                                            
051500       MOVE 'S  '           TO MED-IDSKYLT                                
051600     ELSE                                                                 
051700       MOVE 'GB '           TO MED-IDSKYLT                                
051800     END-IF                                                               
051900                                                                          
052000     MOVE JA                TO NYCKLAR-SW                                 
052100                                                                          
052200*    -- KONTROLL AV IDARTNR                                               
052300     MOVE MFS-RENSA-FAELT   TO MOD-IDARTNR-IN                             
052400                                                                          
052500     IF MID-IDARTNR-IN NOT = ALL '+'                                      
052600       MOVE '7'             TO MFS-IDPFK                                  
052700       MOVE SPACE           TO MFS-KDTRTYP                                
052800     END-IF                                                               
052900                                                                          
053000     MOVE MSGI-IDARTNR      TO WS-IDARTNR                                 
053100     INSPECT WS-IDARTNR   REPLACING LEADING SPACE BY ZERO                 
053200     IF WS-IDARTNR NUMERIC AND WS-IDARTNR > ZERO                          
053300        MOVE WS-IDARTNR     TO W-IDARTNR                                  
053400     ELSE                                                                 
053500        MOVE NEJ            TO NYCKLAR-SW                                 
053600     END-IF                                                               
053700                                                                          
053800*    -- KONTROLL AV IDDC                                                  
053900     MOVE MFS-RENSA-FAELT   TO MOD-IDDC-IN                                
054000                                                                          
054100     IF MID-IDDC-IN NOT = ALL '+'                                         
054200       MOVE '7'             TO MFS-IDPFK                                  
054300       MOVE SPACE           TO MFS-KDTRTYP                                
054400     END-IF                                                               
054500                                                                          
054600     MOVE MSGI-IDDC-KEY     TO W-IDDC                                     
054700                               W-IDDC-TP5                                 
054800                               WS-IDDC                                    
054900                               W-IDDC-B6                                  
055000                               W-IDDC-IN                                  
055100     PERFORM IMS-GU-WDB601                                                
055200     IF SEGMENT-SAKNAS                                                    
055300        MOVE NEJ            TO NYCKLAR-SW                                 
055400     ELSE                                                                 
055500       IF NDC-NA                                                          
055600          MOVE MSGI-IDDC-KEY                                              
055700                            TO MOD-IDDC-UT                                
055800       ELSE                                                               
055900          MOVE NEJ          TO NYCKLAR-SW                                 
056000       END-IF                                                             
056100     END-IF                                                               
056200                                                                          
056300     IF NYCKLAR-OK                                                        
056400       PERFORM S01-GET-DCGROUP-ALL-DC                                     
056500     ELSE                                                                 
056600       MOVE NEJ             TO NYCKLAR-SW                                 
056700       MOVE MFS-RENSA-FAELT TO MOD-IDDC-UT                                
056800     END-IF                                                               
056900                                                                          
057000     MOVE WS-IDARTNR        TO MOD-IDARTNR-UT                             
057100     INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE               
057200                                                                          
057300     IF NYCKLAR-FEL                                                       
057400        MOVE ERR-WRONG-KEY   TO MED-IDMFSFEL                              
057500        MOVE 'GB '           TO MED-IDSKYLT                               
057600        CALL WMEDKONV     USING MED-WMEDAREA                              
057700        MOVE MED-TEMFSFEL    TO MOD-TEMFSFEL                              
057800        PERFORM MFS-RENSA-FAELT-IN                                        
057900        PERFORM MFS-RENSA-FAELT-UT                                        
058000     END-IF                                                               
058100     .                                                                    
058200     EJECT                                                                
058300                                                                          
058400                                                                          
058500 C-FOERSTA-SIDA SECTION.                                                  
058600                                                                          
058700     PERFORM MFS-RENSA-FAELT-IN                                           
058800     .                                                                    
058900     EJECT                                                                
059000                                                                          
059100 E-SAMMA-SIDA SECTION.                                                    
059200     IF EGEN-MID OR HELP-MID                                              
059300       IF MID-INPUT = ALL '+'                                             
059400         PERFORM MFS-RENSA-FAELT-IN                                       
059500       ELSE                                                               
059600         MOVE INF-PRESS-PF11                                              
059700                            TO MED-IDMFSINF                               
059800         MOVE 'GB '         TO MED-IDSKYLT                                
059900         CALL WMEDKONV   USING MED-WMEDAREA                               
060000         MOVE MED-MFSINF    TO MOD-TEMFSINF                               
060100         PERFORM EA-MID-INDATA-TILL-MOD                                   
060200       END-IF                                                             
060300     ELSE                                                                 
060400       PERFORM MFS-RENSA-FAELT-IN                                         
060500     END-IF                                                               
060600     .                                                                    
060700     EJECT                                                                
060800                                                                          
060900                                                                          
061000 EA-MID-INDATA-TILL-MOD SECTION.                                          
061100                                                                          
061200     IF MID-FLREFERAL NOT = ALL '+'                                       
061300        MOVE MFS-ROER-EJ-FAELT                                            
061400                          TO MOD-FLREFERAL-IN                             
061500     ELSE                                                                 
061600        MOVE MFS-RENSA-FAELT                                              
061700                          TO MOD-FLREFERAL-IN                             
061800     END-IF                                                               
061900     MOVE MFS-ADD-LAES-IN-FAELT                                           
062000                          TO MOD-FLREFERAL-IN-ATTR                        
062100     MOVE 1                 TO IX-DC                                      
062200     PERFORM UNTIL IX-DC     > IX-DC-MAX                                  
062300       IF MID-IDLEVNR-SLAG(IX-DC) NOT = ALL '+'                           
062400          MOVE MFS-ROER-EJ-FAELT                                          
062500                            TO MOD-IDLEVNR-SLAG-IN(IX-DC)                 
062600       ELSE                                                               
062700          MOVE MFS-RENSA-FAELT                                            
062800                            TO MOD-IDLEVNR-SLAG-IN(IX-DC)                 
062900       END-IF                                                             
063000       MOVE MFS-ADD-LAES-IN-FAELT                                         
063100                            TO MOD-IDLEVNR-SLAG-IN-ATTR(IX-DC)            
063200*                                                                         
063300       IF MID-FLORDSP(IX-DC) NOT = ALL '+'                                
063400          MOVE MFS-ROER-EJ-FAELT                                          
063500                            TO MOD-FLORDSP-IN(IX-DC)                      
063600       ELSE                                                               
063700          MOVE MFS-RENSA-FAELT                                            
063800                            TO MOD-FLORDSP-IN(IX-DC)                      
063900       END-IF                                                             
064000       MOVE MFS-ADD-LAES-IN-FAELT                                         
064100                            TO MOD-FLORDSP-IN-ATTR(IX-DC)                 
064200*                                                                         
064300       IF MID-FLSPBULK(IX-DC) NOT = ALL '+'                               
064400          MOVE MFS-ROER-EJ-FAELT                                          
064500                            TO MOD-FLSPBULK-IN(IX-DC)                     
064600       ELSE                                                               
064700          MOVE MFS-RENSA-FAELT                                            
064800                            TO MOD-FLSPBULK-IN(IX-DC)                     
064900       END-IF                                                             
065000       MOVE MFS-ADD-LAES-IN-FAELT                                         
065100                            TO MOD-FLSPBULK-IN-ATTR(IX-DC)                
065200*                                                                         
065300       IF MID-IDPERSON-BUY(IX-DC) NOT = ALL '+'                           
065400         MOVE MFS-ROER-EJ-FAELT    TO MOD-IDPERSON-BUY-IN(IX-DC)          
065500       ELSE                                                               
065600         MOVE MFS-RENSA-FAELT      TO MOD-IDPERSON-BUY-IN(IX-DC)          
065700       END-IF                                                             
065800       MOVE MFS-ADD-LAES-IN-FAELT  TO                                     
065900                                 MOD-IDPERSON-BUY-IN-ATTR(IX-DC)          
066000*                                                                         
066100       IF MID-TEARTNOT-ORDER(IX-DC)   NOT = ALL '+'                       
066200         IF MID-TEARTNOT-ORDER(IX-DC) NOT =                               
066300                                      MOD-TEARTNOT-ORDER-IN(IX-DC)        
066400            MOVE MID-TEARTNOT-ORDER(IX-DC)                                
066500                                   TO MOD-TEARTNOT-ORDER-IN(IX-DC)        
066600            MOVE MFS-ADD-LAES-IN-FAELT TO                                 
066700                                MOD-TEARTNOT-ORDER-IN-ATTR(IX-DC)         
066800            MOVE JA                    TO WS-TEARTNOT-FL (IX-DC)          
066900         END-IF                                                           
067000       ELSE                                                               
067100         MOVE MFS-RENSA-FAELT      TO MOD-TEARTNOT-ORDER-IN(IX-DC)        
067200       END-IF                                                             
067300       ADD 1                       TO IX-DC                               
067400     END-PERFORM                                                          
067500                                                                          
067600     .                                                                    
067700     EJECT                                                                
067800                                                                          
067900                                                                          
068000 F-LAES-VISA-INFO SECTION.                                                
068100                                                                          
068200     PERFORM FA-LAES-GRPDC                                                
068300     PERFORM FB-LAES-GRUNDDATA                                            
068400                                                                          
068500     IF SEGMENT-SAKNAS                                                    
068600       MOVE PART-MISSING      TO MED-IDMFSFEL                             
068700       MOVE 'GB '             TO MED-IDSKYLT                              
068800       CALL WMEDKONV       USING MED-WMEDAREA                             
068900       MOVE MED-TEMFSFEL      TO MOD-TEMFSFEL                             
069000       PERFORM MFS-RENSA-FAELT-UT                                         
069100       PERFORM MFS-STAENG-FAELT-IN                                        
069200     ELSE                                                                 
069300       IF ART-KDERS-UTG > +0                                              
069400         MOVE PART-SUPERSEDED TO MED-IDMFSFEL                             
069500         MOVE 'GB '           TO MED-IDSKYLT                              
069600         CALL WMEDKONV     USING MED-WMEDAREA                             
069700         MOVE MED-TEMFSFEL    TO MOD-TEMFSFEL                             
069800         PERFORM MFS-RENSA-FAELT-UT                                       
069900         PERFORM MFS-STAENG-FAELT-IN                                      
070000       ELSE                                                               
070100         PERFORM FC-VISA                                                  
070200       END-IF                                                             
070300     END-IF                                                               
070400     .                                                                    
070500     EJECT                                                                
070600                                                                          
070700 FA-LAES-GRPDC SECTION.                                                   
070800     MOVE +1                  TO IX-DC                                    
070900     PERFORM UNTIL     IX-DC   > IX-DC-MAX                                
071000       MOVE WS-DC-NR (IX-DC)  TO MOD-IDDC-GRP (IX-DC)                     
071100       ADD +1                 TO IX-DC                                    
071200     END-PERFORM                                                          
071300     .                                                                    
071400     EJECT                                                                
071500                                                                          
071600 FB-LAES-GRUNDDATA SECTION.                                               
071700     PERFORM IMS-GU-ARTC01                                                
071800     .                                                                    
071900     EJECT                                                                
072000                                                                          
072100 FC-VISA SECTION.                                                         
072200                                                                          
072300**READS COUNTRY FOR DC ENTERED ON SCREEN IN HEAD ROW                      
072400     PERFORM S02-GET-IDLAND                                               
072500     PERFORM IMS-GU-WDK712                                                
072600     IF SEGMENT-FINNS                                                     
072700       IF LART-FLREFERAL = JA                                             
072800          MOVE YES            TO MOD-FLREFERAL                            
072900       ELSE                                                               
073000          MOVE LART-FLREFERAL TO MOD-FLREFERAL                            
073100       END-IF                                                             
073200                                                                          
073300       MOVE LART-IDLANDX2 TO W-IDLANDX2                                   
073400       PERFORM IMS-GU-WDGX2510                                            
073500       IF SEGMENT-FINNS                                                   
073600         MOVE 2510-IDUSER     TO MOD-IDUSER-REFERAL                       
073700         MOVE 2510-TIREGDAT   TO MOD-TIREGDAT-REFERAL                     
073800       ELSE                                                               
073900         MOVE MFS-RENSA-FAELT TO MOD-IDUSER-REFERAL                       
074000                                 MOD-TIREGDAT-REFERAL                     
074100       END-IF                                                             
074200                                                                          
074300     ELSE                                                                 
074400       MOVE MFS-RENSA-FAELT   TO MOD-FLREFERAL                            
074500                                 MOD-IDUSER-REFERAL                       
074600                                 MOD-TIREGDAT-REFERAL                     
074700       MOVE MFS-STAENG-FAELT-NOMOD                                        
074800                              TO MOD-FLREFERAL-IN-ATTR                    
074900     END-IF                                                               
075000     MOVE +1                  TO IX-DC                                    
075100     PERFORM UNTIL     IX-DC   > IX-DC-MAX                                
075200       MOVE WS-DC-NR (IX-DC)  TO W-IDDC                                   
075300       PERFORM IMS-GU-WDK711                                              
075400       IF SEGMENT-FINNS                                                   
075500          MOVE SLAG-IDPERSON-BUY                                          
075600                              TO MOD-IDPERSON-BUY (IX-DC)                 
075700          MOVE SLAG-IDLEVNR   TO MOD-IDLEVNR-SLAG (IX-DC)                 
075800          IF SLAG-FLORDSP = JA                                            
075900             MOVE YES         TO MOD-FLORDSP      (IX-DC)                 
076000          ELSE                                                            
076100             MOVE SLAG-FLORDSP                                            
076200                              TO MOD-FLORDSP      (IX-DC)                 
076300          END-IF                                                          
076400          IF SLAG-FLSPBULK = JA                                           
076500             MOVE YES         TO MOD-FLSPBULK     (IX-DC)                 
076600          ELSE                                                            
076700             MOVE SLAG-FLSPBULK                                           
076800                              TO MOD-FLSPBULK     (IX-DC)                 
076900          END-IF                                                          
077000          MOVE SLAG-KVDAGAR-MANLT                                         
077100                              TO MOD-KVDAGAR-MANLT(IX-DC)                 
077200*                                                                         
077300          PERFORM IMS-GU-WDK721                                           
077400           IF SEGMENT-FINNS                                               
077500                                                                          
077600              IF WS-TEARTNOT-FL (IX-DC)  NOT = JA                         
077700                 MOVE SBLK-TEARTNOT-ORDER                                 
077800                              TO MOD-TEARTNOT-ORDER-IN(IX-DC)             
077900              END-IF                                                      
078000              MOVE SBLK-IDUSER-ORDSP                                      
078100                              TO MOD-IDUSER-ORDSP(IX-DC)                  
078200              MOVE SBLK-IDUSER-SPBULK                                     
078300                              TO MOD-IDUSER-SPBULK(IX-DC)                 
078400              IF SBLK-DAORDSP > ZERO                                      
078500                 MOVE SBLK-DAORDSP                                        
078600                              TO MOD-DAORDSP(IX-DC)                       
078700              ELSE                                                        
078800                 MOVE MFS-RENSA-FAELT                                     
078900                              TO MOD-DAORDSP(IX-DC)                       
079000              END-IF                                                      
079100              IF SBLK-DASPBULK > ZERO                                     
079200                 MOVE SBLK-DASPBULK                                       
079300                              TO MOD-DASPBULK(IX-DC)                      
079400              ELSE                                                        
079500                 MOVE MFS-RENSA-FAELT                                     
079600                              TO MOD-DASPBULK(IX-DC)                      
079700              END-IF                                                      
079800           ELSE                                                           
079900              MOVE MFS-RENSA-FAELT                                        
080000                              TO MOD-TEARTNOT-ORDER-IN(IX-DC)             
080100                                 MOD-DAORDSP(IX-DC)                       
080200                                 MOD-DASPBULK(IX-DC)                      
080300                                 MOD-IDUSER-ORDSP(IX-DC)                  
080400                                 MOD-IDUSER-SPBULK(IX-DC)                 
080500           END-IF                                                         
080600       ELSE                                                               
080700         MOVE MFS-RENSA-FAELT                                             
080800                               TO MOD-IDLEVNR-SLAG     (IX-DC)            
080900                                  MOD-FLORDSP          (IX-DC)            
081000                                  MOD-DAORDSP          (IX-DC)            
081100                                  MOD-IDUSER-ORDSP     (IX-DC)            
081200                                  MOD-FLSPBULK         (IX-DC)            
081300                                  MOD-DASPBULK         (IX-DC)            
081400                                  MOD-IDUSER-SPBULK    (IX-DC)            
081500                                  MOD-KVDAGAR-MANLT    (IX-DC)            
081600                                  MOD-IDPERSON-BUY     (IX-DC)            
081700                                  MOD-TEARTNOT-ORDER-IN(IX-DC)            
081800         PERFORM MFS-CLOSE-BLANK-DC-FIELD-IN                              
081900       END-IF                                                             
082000                                                                          
082100       ADD +1 TO IX-DC                                                    
082200     END-PERFORM                                                          
082300                                                                          
082400     PERFORM IMS-GU-BENA01-BSEQ                                           
082500     IF SEGMENT-FINNS                                                     
082600        PERFORM IMS-GNP-BENA11                                            
082700        IF SEGMENT-FINNS                                                  
082800           MOVE BENA-TEXT-BEART TO MOD-BEART-ENG                          
082900        ELSE                                                              
083000           MOVE MFS-RENSA-FAELT TO MOD-BEART-ENG                          
083100        END-IF                                                            
083200     ELSE                                                                 
083300        MOVE MFS-RENSA-FAELT    TO MOD-BEART-ENG                          
083400     END-IF                                                               
083500                                                                          
083600     MOVE +1                    TO IX-DC                                  
083700     PERFORM UNTIL IX-DC         > 4                                      
083800       IF WS-DC-NR (IX-DC)       > SPACES                                 
083900          CONTINUE                                                        
084000       ELSE                                                               
084100          PERFORM MFS-CLOSE-BLANK-DC-FIELD-IN                             
084200       END-IF                                                             
084300       ADD +1                   TO IX-DC                                  
084400     END-PERFORM                                                          
084500     .                                                                    
084600     EJECT                                                                
084700                                                                          
084800 G-KOLLA-INPUT SECTION.                                                   
084900     MOVE JA                     TO INDATA-SW                             
085000     IF MID-INPUT                 = ALL '+'                               
085100       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
085200       MOVE 'GB '                TO MED-IDSKYLT                           
085300       CALL WMEDKONV          USING MED-WMEDAREA                          
085400       MOVE MED-TEMFSFEL         TO MOD-TEMFSFEL                          
085500       PERFORM MFS-ROER-EJ-FAELT-IN                                       
085600       PERFORM MFS-ROER-EJ-FAELT-UT                                       
085700       MOVE NEJ                  TO INDATA-SW                             
085800     ELSE                                                                 
085900       PERFORM IMS-GU-WDK701                                              
086000       IF SEGMENT-SAKNAS                                                  
086100         MOVE NEJ                TO INDATA-SW                             
086200         MOVE PART-MISSING       TO MED-IDMFSFEL                          
086300         MOVE 'GB '              TO MED-IDSKYLT                           
086400         CALL WMEDKONV        USING MED-WMEDAREA                          
086500         MOVE MED-MFSFEL         TO MOD-TEMFSFEL                          
086600       ELSE                                                               
086700         PERFORM GA-KOLLA-INPUT-1                                         
086800         IF INDATA-FEL                                                    
086900           MOVE ERR-CORR-HILITE-FLDS                                      
087000                                 TO MED-IDMFSFEL                          
087100           MOVE 'GB '            TO MED-IDSKYLT                           
087200           CALL WMEDKONV      USING MED-WMEDAREA                          
087300           MOVE MED-MFSFEL       TO MOD-TEMFSFEL                          
087400           PERFORM MFS-ROER-EJ-FAELT-UT                                   
087500           PERFORM MFS-ROER-EJ-FAELT-IN                                   
087600         ELSE                                                             
087700           PERFORM GB-KOLLA-INPUT-2                                       
087800           IF INDATA-FEL                                                  
087900             PERFORM MFS-ROER-EJ-FAELT-UT                                 
088000             PERFORM MFS-ROER-EJ-FAELT-IN                                 
088100           END-IF                                                         
088200         END-IF                                                           
088300       END-IF                                                             
088400     END-IF                                                               
088500     .                                                                    
088600     EJECT                                                                
088700                                                                          
088800 GA-KOLLA-INPUT-1 SECTION.                                                
088900                                                                          
089000*IDLEVNR-SLAG                                                             
089100     MOVE 1                            TO IX-DC                           
089200     PERFORM UNTIL   IX-DC   > IX-DC-MAX                                  
089300       IF MID-IDLEVNR-SLAG(IX-DC) NOT   = ALL '+'                         
089400          MOVE MFS-ALFA-FAELT-RAETT    TO                                 
089500                                MOD-IDLEVNR-SLAG-IN-ATTR (IX-DC)          
089600       ELSE                                                               
089700          MOVE MFS-ALFA-FAELT-RAETT    TO                                 
089800                                MOD-IDLEVNR-SLAG-IN-ATTR (IX-DC)          
089900       END-IF                                                             
090000                                                                          
090100*FLORDSP                                                                  
090200       IF MID-FLORDSP(IX-DC)  NOT = ALL '+'                               
090300         IF MID-FLORDSP(IX-DC)    = JA OR YES OR NEJ                      
090400            MOVE MFS-ALFA-FAELT-RAETT                                     
090500                                 TO MOD-FLORDSP-IN-ATTR(IX-DC)            
090600         ELSE                                                             
090700            MOVE MFS-ALFA-FAELT-FEL                                       
090800                                 TO MOD-FLORDSP-IN-ATTR(IX-DC)            
090900            MOVE NEJ             TO INDATA-SW                             
091000         END-IF                                                           
091100       ELSE                                                               
091200         MOVE MFS-ALFA-FAELT-RAETT                                        
091300                                 TO MOD-FLORDSP-IN-ATTR(IX-DC)            
091400       END-IF                                                             
091500                                                                          
091600*FLSPBULK                                                                 
091700       IF MID-FLSPBULK(IX-DC) NOT = ALL '+'                               
091800         IF MID-FLSPBULK(IX-DC)   = JA OR YES OR NEJ                      
091900            MOVE MFS-ALFA-FAELT-RAETT                                     
092000                                 TO MOD-FLSPBULK-IN-ATTR(IX-DC)           
092100         ELSE                                                             
092200            MOVE MFS-ALFA-FAELT-FEL                                       
092300                                 TO MOD-FLSPBULK-IN-ATTR(IX-DC)           
092400            MOVE NEJ             TO INDATA-SW                             
092500         END-IF                                                           
092600       ELSE                                                               
092700         MOVE MFS-ALFA-FAELT-RAETT                                        
092800                                 TO MOD-FLSPBULK-IN-ATTR(IX-DC)           
092900       END-IF                                                             
093000                                                                          
093100*TEARTNOT-ORDER                                                           
093200       IF MID-TEARTNOT-ORDER(IX-DC) = ALL '+'                             
093300          MOVE MFS-RENSA-FAELT                                            
093400                                 TO MOD-TEARTNOT-ORDER-IN(IX-DC)          
093500       ELSE                                                               
093600          MOVE MID-TEARTNOT-ORDER(IX-DC)                                  
093700                                 TO MOD-TEARTNOT-ORDER-IN(IX-DC)          
093800          MOVE MFS-ALFA-FAELT-RAETT                                       
093900                                 TO MOD-TEARTNOT-ORDER-IN(IX-DC)          
094000       END-IF                                                             
094100                                                                          
094200*IDPERSON-BUY                                                             
094300       IF MID-IDPERSON-BUY(IX-DC)    NOT = ALL '+'                        
094400          IF MID-IDPERSON-BUY(IX-DC) NOT NUMERIC                          
094500             MOVE MFS-NUM-FAELT-FEL                                       
094600                              TO MOD-IDPERSON-BUY-IN-ATTR(IX-DC)          
094700             MOVE NEJ         TO INDATA-SW                                
094800          ELSE                                                            
094900             MOVE MFS-NUM-FAELT-RAETT                                     
095000                              TO MOD-IDPERSON-BUY-IN-ATTR(IX-DC)          
095100          END-IF                                                          
095200       ELSE                                                               
095300         MOVE MFS-NUM-FAELT-RAETT                                         
095400                              TO MOD-IDPERSON-BUY-IN-ATTR(IX-DC)          
095500       END-IF                                                             
095600                                                                          
095700       ADD 1 TO IX-DC                                                     
095800     END-PERFORM                                                          
095900*FLREFERAL                                                                
096000     IF MID-FLREFERAL NOT = ALL '+'                                       
096100        IF MID-FLREFERAL = JA OR YES OR NEJ                               
096200           MOVE MFS-ALFA-FAELT-RAETT TO                                   
096300                              MOD-FLREFERAL-IN-ATTR                       
096400        ELSE                                                              
096500           MOVE MFS-ALFA-FAELT-FEL      TO                                
096600                              MOD-FLREFERAL-IN-ATTR                       
096700           MOVE NEJ TO INDATA-SW                                          
096800        END-IF                                                            
096900     ELSE                                                                 
097000         MOVE MFS-ALFA-FAELT-RAETT      TO                                
097100                              MOD-FLREFERAL-IN-ATTR                       
097200     END-IF                                                               
097300                                                                          
097400     .                                                                    
097500     EJECT                                                                
097600                                                                          
097700 GB-KOLLA-INPUT-2 SECTION.                                                
097800                                                                          
097900     PERFORM IMS-GU-ARTC01                                                
098000     IF SEGMENT-SAKNAS                                                    
098100       MOVE NEJ               TO INDATA-SW                                
098200       MOVE PART-MISSING      TO MED-IDMFSFEL                             
098300       MOVE 'GB '             TO MED-IDSKYLT                              
098400       CALL WMEDKONV       USING MED-WMEDAREA                             
098500       MOVE MED-MFSFEL        TO MOD-TEMFSFEL                             
098600     ELSE                                                                 
098700       IF ART-KDERS-UTG > ZERO                                            
098800         MOVE NEJ TO INDATA-SW                                            
098900         MOVE PART-SUPERSEDED TO MED-IDMFSFEL                             
099000         MOVE 'GB '           TO MED-IDSKYLT                              
099100         CALL WMEDKONV     USING MED-WMEDAREA                             
099200         MOVE MED-MFSFEL      TO MOD-TEMFSFEL                             
099300       ELSE                                                               
099400         PERFORM IMS-GNP-ARTC11                                           
099500*IDLEVNR-SLAG                                                             
099600         MOVE 1 TO IX-DC                                                  
099700         PERFORM UNTIL IX-DC > IX-DC-MAX                                  
099800           MOVE WS-DC-NR (IX-DC)                                          
099900                               TO W-IDDC                                  
100000                                  W-IDDC-B6                               
100100           PERFORM IMS-GU-WDB601                                          
100200           PERFORM IMS-GU-WDK711                                          
100300           IF MID-IDLEVNR-SLAG(IX-DC) NOT = ALL '+'                       
100400              MOVE ART-KDPRODSL  TO TEST-KDPRODSL                         
100500              IF KDPRODSL-LOCAL                                           
100600                 IF MID-IDLEVNR-SLAG(IX-DC) = '1441' OR 'BP2TW'           
100700                    MOVE MFS-ALFA-FAELT-FEL                               
100800                               TO MOD-IDLEVNR-SLAG-IN-ATTR(IX-DC)         
100900                    MOVE NEJ   TO INDATA-SW                               
101000                    MOVE MED-6 TO MOD-TEMFSINF                            
101100                 END-IF                                                   
101200              ELSE                                                        
101300                 MOVE MFS-ALFA-FAELT-RAETT                                
101400                               TO MOD-IDLEVNR-SLAG-IN-ATTR(IX-DC)         
101500              END-IF                                                      
101600                                                                          
101700*          IF FUTURE FORECAST EXISTS SUPPLIER CHANGE NOT ALLOWED          
101800              PERFORM IMS-GNP-WDK727                                      
101900              IF SEGMENT-FINNS                                            
102000                 MOVE MFS-ALFA-FAELT-FEL                                  
102100                               TO MOD-IDLEVNR-SLAG-IN-ATTR(IX-DC)         
102200                 MOVE NEJ      TO INDATA-SW                               
102300                 MOVE MED-14   TO MOD-TEMFSINF                            
102400              ELSE                                                        
102500                 MOVE MFS-ALFA-FAELT-RAETT                                
102600                               TO MOD-IDLEVNR-SLAG-IN-ATTR(IX-DC)         
102700              END-IF                                                      
102800*                                                                         
102900              MOVE WS-DC-NR (IX-DC)                                       
103000                               TO W-IDDC-E3MIN                            
103100                                  W-IDDC-E3MAX                            
103200              IF INDATA-OK                                                
103300                 PERFORM IMS-GN-WDE3-MIN-MAX                              
103400                 PERFORM UNTIL SEGMENT-SLUT                               
103500                         OR SEGMENT-SAKNAS                                
103600                         OR SUPPLR-FEL                                    
103700                  IF REF-IDARTNR     = W-IDARTNR                          
103800                     IF REF-IDLEVNR  = SLAG-IDLEVNR                       
103900                       MOVE MFS-ALFA-FAELT-FEL                            
104000                               TO MOD-IDLEVNR-SLAG-IN-ATTR(IX-DC)         
104100                       MOVE NEJ    TO INDATA-SW                           
104200                                      SUPPLR-SW                           
104300                       MOVE MED-8  TO MOD-TEMFSINF                        
104400                     ELSE                                                 
104500                       MOVE MFS-ALFA-FAELT-RAETT                          
104600                               TO MOD-IDLEVNR-SLAG-IN-ATTR(IX-DC)         
104700                     END-IF                                               
104800                  ELSE                                                    
104900                     MOVE MFS-ALFA-FAELT-RAETT                            
105000                               TO MOD-IDLEVNR-SLAG-IN-ATTR(IX-DC)         
105100                  END-IF                                                  
105200                  PERFORM IMS-GN-WDE3-MIN-MAX                             
105300                 END-PERFORM                                              
105400              END-IF                                                      
105500*                                                                         
105600*             SUPPLIER SAME AS OWN DC                                     
105700              IF INDATA-OK                                                
105800                 IF MID-IDLEVNR-SLAG(IX-DC)  = DCS-IDLEVNR-DC             
105900                                                                          
106000                    MOVE MFS-ALFA-FAELT-FEL                               
106100                               TO MOD-IDLEVNR-SLAG-IN-ATTR(IX-DC)         
106200                    MOVE NEJ     TO INDATA-SW                             
106300                    MOVE MED-9   TO MOD-TEMFSINF                          
106400                 ELSE                                                     
106500                    MOVE MFS-ALFA-FAELT-RAETT                             
106600                               TO MOD-IDLEVNR-SLAG-IN-ATTR(IX-DC)         
106700                 END-IF                                                   
106800              END-IF                                                      
106900*                                                                         
107000*                                                                         
107100              IF INDATA-OK                                                
107200               MOVE MID-IDLEVNR-SLAG(IX-DC)   TO W-IDLEVNR                
107300               PERFORM IMS-GU-LEVA01                                      
107400               IF SEGMENT-FINNS                                           
107500                 MOVE MFS-ALFA-FAELT-RAETT   TO                           
107600                                 MOD-IDLEVNR-SLAG-IN-ATTR(IX-DC)          
107700                 MOVE LEV-IDLEVNR            TO WS-IDLEVNR-NUM            
107800                                                                          
107900                 INSPECT WS-IDLEVNR-NUM REPLACING                         
108000                                                ALL SPACE BY ZERO         
108100                 IF WS-IDLEVNR-NUM NUMERIC                                
108200                    IF LEV-IDLEVNR-MOTSV  NOT = SPACE                     
108300                       MOVE MFS-ALFA-FAELT-FEL                            
108400                              TO MOD-IDLEVNR-SLAG-IN-ATTR(IX-DC)          
108500                       MOVE NEJ              TO INDATA-SW                 
108600                       MOVE FEL-125          TO MOD-TEMFSINF              
108700                    ELSE                                                  
108800                       MOVE MFS-ALFA-FAELT-RAETT                          
108900                              TO MOD-IDLEVNR-SLAG-IN-ATTR(IX-DC)          
109000                    END-IF                                                
109100                 ELSE                                                     
109200***                  IDLEVNR ÄR ALFA                                      
109300                    MOVE MFS-ALFA-FAELT-RAETT                             
109400                              TO MOD-IDLEVNR-SLAG-IN-ATTR(IX-DC)          
109500                 END-IF                                                   
109600               ELSE                                                       
109700                 MOVE MFS-ALFA-FAELT-FEL     TO                           
109800                                 MOD-IDLEVNR-SLAG-IN-ATTR(IX-DC)          
109900                 MOVE NEJ                    TO INDATA-SW                 
110000                 MOVE MED-7                  TO MOD-TEMFSINF              
110100               END-IF                                                     
110200              END-IF                                                      
110300                                                                          
110400*             NO CHANGE IN SUPPLIER FOR LOCALLY SOURCED PART              
110500              IF INDATA-OK                                                
110600               IF  DCS-USA                                                
110700               AND SLAG-IDDC-REF = SPACES                                 
110800                   MOVE MFS-ALFA-FAELT-FEL                                
110900                              TO MOD-IDLEVNR-SLAG-IN-ATTR (IX-DC)         
111000                   MOVE NEJ     TO INDATA-SW                              
111100                   MOVE MED-11  TO MOD-TEMFSINF                           
111200               ELSE                                                       
111300                   MOVE MFS-ALFA-FAELT-RAETT                              
111400                              TO MOD-IDLEVNR-SLAG-IN-ATTR (IX-DC)         
111500               END-IF                                                     
111600              END-IF                                                      
111700                                                                          
111800*             CHECK WDB601 FOR VALID SUPPLIER AND REFILLING DC            
111900              MOVE LOW-VALUES         TO W-IDDC-MIN                       
112000              MOVE HIGH-VALUES        TO W-IDDC-MAX                       
112100              MOVE SPACES             TO WS-IDDC-REF-SLAG (IX-DC)         
112200              IF  DCS-NDC-NA                                              
112300              AND INDATA-OK                                               
112400                 PERFORM IMS-GU-WDB601-FIRST                              
113702                 PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT             
113703                         OR (MID-IDLEVNR-SLAG(IX-DC)                      
113704                                              = DCS-IDLEVNR-DC)           
114300                   PERFORM IMS-GN-WDB601-MIN-MAX                          
114400                 END-PERFORM                                              
114500                                                                          
114600                 IF MID-IDLEVNR-SLAG(IX-DC)                               
114700                                     = DCS-IDLEVNR-DC                     
114800                    IF  DCS-IDDC NOT = WS-DC-NR (IX-DC)                   
114900                    AND (DCS-NDC    OR DCS-CDC )                          
115000                        IF DCS-NDC                                        
115100                           PERFORM GBB-VALIDATE-WDB615-WDB616             
115200                        END-IF                                            
115300                        IF (DCS-NDC AND SUPPLR-OK) OR DCS-CDC             
115400                           MOVE DCS-IDDC                                  
115500                                    TO WS-IDDC-REF-SLAG (IX-DC)           
115600                           MOVE MFS-ALFA-FAELT-RAETT                      
115700                             TO MOD-IDLEVNR-SLAG-IN-ATTR(IX-DC)           
115800                        END-IF                                            
115900                    END-IF                                                
115901                 END-IF                                                   
115902*                                                                         
115903                 MOVE WS-DC-NR (IX-DC)      TO WS-IDDC                    
115904                 IF  WS-IDDC-REF-SLAG(IX-DC) = SPACES                     
115905                 AND NDC-US                                               
115906                 AND MOD-TEMFSINF NOT =                                   
115907                                       'REFILL FLOW NOT AVAILABLE'        
115908                       MOVE MFS-ALFA-FAELT-FEL                            
115909                               TO MOD-IDLEVNR-SLAG-IN-ATTR(IX-DC)         
115910                       MOVE NEJ             TO INDATA-SW                  
115911                       MOVE MED-10          TO MOD-TEMFSINF               
115912                 END-IF                                                   
115913*                VALIDATION FOR REFILL RULES GLOBAL EXPORTS               
115914                 IF WS-IDDC-REF-SLAG(IX-DC) > SPACES                      
117000                       MOVE WS-IDDC-REF-SLAG(IX-DC)                       
117100                                            TO W-IDDC                     
117200                                               WS-IDDC                    
117300                       PERFORM GBA-SUPPL-DC-CHECK                         
117400                       MOVE WS-DC-NR (IX-DC)   TO W-IDDC                  
117600                 END-IF                                                   
117700              END-IF                                                      
117800           END-IF                                                         
117900                                                                          
118000           IF INDATA-OK                                                   
118100              IF MID-IDPERSON-BUY(IX-DC)     NOT = ALL '+'                
118200                 IF SLAG-FLBUYUPD = 'J'                                   
118300                   MOVE MFS-ALFA-FAELT-FEL                                
118400                             TO MOD-IDPERSON-BUY-IN-ATTR(IX-DC)           
118500                   MOVE NEJ       TO INDATA-SW                            
118600                   MOVE MED-13 TO MOD-TEMFSINF                            
118700                 ELSE                                                     
118800                   MOVE MFS-ALFA-FAELT-RAETT                              
118900                            TO MOD-IDPERSON-BUY-IN-ATTR(IX-DC)            
119000                 END-IF                                                   
119100              END-IF                                                      
119200           END-IF                                                         
119300*TEARTNOT                                                                 
119400           IF MID-TEARTNOT-ORDER(IX-DC)  NOT  = ALL '+'                   
119500              PERFORM IMS-GU-WDK721                                       
119600              IF SEGMENT-FINNS                                            
119700                 IF MID-TEARTNOT-ORDER(IX-DC)                             
119800                                     NOT  = SBLK-TEARTNOT-ORDER           
119900                    MOVE JA              TO WS-TEARTNOT-FL (IX-DC)        
120000                 ELSE                                                     
120100                    MOVE NEJ             TO WS-TEARTNOT-FL (IX-DC)        
120200                 END-IF                                                   
120300              ELSE                                                        
120400                 MOVE JA                 TO WS-TEARTNOT-FL (IX-DC)        
120500              END-IF                                                      
120600           END-IF                                                         
120700                                                                          
120800           ADD 1                         TO IX-DC                         
120900         END-PERFORM                                                      
121000       END-IF                                                             
121100     END-IF                                                               
121200     .                                                                    
121300     EJECT                                                                
121400                                                                          
126101 GBA-SUPPL-DC-CHECK SECTION.                                              
126104                                                                          
126122*CHECKING REFILLING DC AND DC WHICH REFILLS REFILLING DC,                 
126123*TO AVOID CIRCULAR REFILL FLOW                                            
126124     IF NDC AND INDATA-OK                                                 
126125        PERFORM IMS-GU-WDK711                                             
126126        IF SEGMENT-SAKNAS                                                 
126127           MOVE MFS-ALFA-FAELT-FEL                                        
126128                             TO MOD-IDLEVNR-SLAG-IN-ATTR(IX-DC)           
126129           MOVE NEJ            TO INDATA-SW                               
126130           MOVE MED-12         TO MOD-TEMFSINF                            
126131        ELSE                                                              
126132          MOVE SLAG-IDDC-REF   TO WS-IDDC                                 
126135          IF SLAG-IDDC-REF  = WS-DC-NR (IX-DC)                            
126136             MOVE MFS-ALFA-FAELT-FEL                                      
126137                             TO MOD-IDLEVNR-SLAG-IN-ATTR(IX-DC)           
126138             MOVE NEJ          TO INDATA-SW                               
126139             MOVE MED-12       TO MOD-TEMFSINF                            
126141          END-IF                                                          
126142                                                                          
126143        END-IF                                                            
126144     END-IF                                                               
126145     .                                                                    
126146     EJECT                                                                
126147                                                                          
126200 GBB-VALIDATE-WDB615-WDB616 SECTION.                                      
126301*WDB616 SHOULD HAVE VALID REFILL DISTRICT (4404 SCREEN)                   
126401*WDB615 SHOULD HAVE UPDATED LEAD TIME (4408 SCREEN)                       
126500                                                                          
126601     MOVE WS-DC-NR (IX-DC) TO W-IDDC                                      
126701     MOVE '4408'           TO W-IDTRANS-B6                                
126801     MOVE DCS-IDDC         TO W-IDDC-REF-B6                               
126901     PERFORM IMS-GU-WDB615                                                
127001     IF SEGMENT-FINNS                                                     
130601        MOVE DCS-IDDC      TO W-IDDC-REF                                  
130701        PERFORM IMS-GU-WDB616                                             
130801        IF  SEGMENT-FINNS                                                 
130901        AND REF-IDDISTR-REFILL > ZERO                                     
131001          MOVE JA TO SUPPLR-SW                                            
131201        ELSE                                                              
131301          MOVE NEJ TO SUPPLR-SW                                           
131401          MOVE MFS-ALFA-FAELT-FEL                                         
131501                   TO MOD-IDLEVNR-SLAG-IN-ATTR(IX-DC)                     
131601          MOVE NEJ     TO INDATA-SW                                       
132001          MOVE 'REFILL FLOW NOT AVAILABLE'                                
132101                       TO MOD-TEMFSINF                                    
132201        END-IF                                                            
132401     ELSE                                                                 
132501       MOVE NEJ TO SUPPLR-SW                                              
132601       MOVE MFS-ALFA-FAELT-FEL                                            
132701                TO MOD-IDLEVNR-SLAG-IN-ATTR(IX-DC)                        
132801       MOVE NEJ     TO INDATA-SW                                          
132901       MOVE 'REFILL FLOW NOT AVAILABLE'                                   
133001                    TO MOD-TEMFSINF                                       
133101     END-IF                                                               
133102     .                                                                    
133103     EJECT                                                                
133104                                                                          
133500 H-UPPDATERA SECTION.                                                     
133600                                                                          
133700     IF MID-IDLEVNR-SLAG-GRP   NOT = ALL '+'                              
133800     OR MID-FLORDSP-GRP        NOT = ALL '+'                              
133900     OR MID-FLSPBULK-GRP       NOT = ALL '+'                              
134000     OR MID-TEARTNOT-GRP       NOT = ALL '+'                              
134100     OR MID-IDPERSON-BUY-GRP   NOT = ALL '+'                              
134200     OR MID-FLREFERAL          NOT = ALL '+'                              
134300        PERFORM HA-UPPDATERA-WDK7                                         
134400     END-IF                                                               
134500                                                                          
134600     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
134700     MOVE 'GB '         TO MED-IDSKYLT                                    
134800     CALL WMEDKONV USING MED-WMEDAREA                                     
134900     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
135000     PERFORM MFS-FORM-ATTR                                                
135100     PERFORM MFS-RENSA-FAELT-IN                                           
135200     .                                                                    
135300     EJECT                                                                
135400                                                                          
135500 HA-UPPDATERA-WDK7 SECTION.                                               
135600                                                                          
135700     PERFORM IMS-GU-WDK701                                                
135800     MOVE 1                 TO IX-DC                                      
135900     PERFORM UNTIL   IX-DC   > IX-DC-MAX                                  
136000       IF MID-IDLEVNR-SLAG(IX-DC)     NOT = ALL '+'                       
136100       OR MID-FLORDSP(IX-DC)          NOT = ALL '+'                       
136200       OR MID-FLSPBULK(IX-DC)         NOT = ALL '+'                       
136300       OR MID-IDPERSON-BUY(IX-DC)     NOT = ALL '+'                       
136400       OR (MID-TEARTNOT-ORDER(IX-DC)  NOT = ALL '+'                       
136500       AND WS-TEARTNOT-FL (IX-DC)         = JA)                           
136600          MOVE WS-DC-NR (IX-DC)        TO W-IDDC                          
136700          PERFORM IMS-GHNP-WDK711                                         
136800          PERFORM IMS-GHU-WDK721                                          
136900          IF SEGMENT-SAKNAS                                               
137000             MOVE ALL '+'              TO WDK7-W005WDK7                   
137100             MOVE 'WDK721'             TO WDK7-IDSEGM                     
137200             MOVE W-IDARTNR            TO WDK7-IDARTNR-KFB                
137300             MOVE W-IDDC               TO WDK7-IDDC-KFB                   
137400                                                                          
137500             CALL W005WDK7          USING WDK7-W005WDK7 WDB6-PCB          
137600                                          ARTC-PCB WDK72-PCB              
137700             PERFORM IMS-GHU-WDK721                                       
137800          END-IF                                                          
137900*                                                                         
138000          MOVE NEJ TO SW-AENDRA-WDK721                                    
138100          IF MID-IDLEVNR-SLAG(IX-DC) NOT = ALL '+'                        
138200             MOVE MID-IDLEVNR-SLAG(IX-DC)                                 
138300                                        TO SLAG-IDLEVNR                   
138400             MOVE WS-IDDC-REF-SLAG(IX-DC)                                 
138500                                        TO SLAG-IDDC-REF                  
138600                                           WS-IDDC                        
138700          END-IF                                                          
138800*                                                                         
138900          IF (MID-TEARTNOT-ORDER(IX-DC) NOT = ALL '+'                     
139000          AND WS-TEARTNOT-FL (IX-DC)        = JA)                         
139100             MOVE JA                    TO SW-AENDRA-WDK721               
139200             MOVE MID-TEARTNOT-ORDER(IX-DC)                               
139300                                        TO SBLK-TEARTNOT-ORDER            
139400          END-IF                                                          
139500                                                                          
139600* --       ÅTERSTÄLL RÖRELSEINDIKATOR (FLREFNYO) PÅ ORDERINGÅNGS-         
139700* --       REGISTRET WDL7 VID UPPDATERING AV BLOCKKOD.                    
139800          IF MID-FLORDSP(IX-DC)     NOT = ALL '+'                         
139900          OR MID-FLSPBULK(IX-DC)    NOT = ALL '+'                         
140000             MOVE NEJ                  TO SLAG-FLREFNYO                   
140100          END-IF                                                          
140200                                                                          
140300          IF MID-FLORDSP(IX-DC)     NOT = ALL '+'                         
140400             IF MID-FLORDSP(IX-DC)  NOT = SLAG-FLORDSP                    
140500                MOVE DAGENS-DATUM      TO SBLK-DAORDSP                    
140600                MOVE MSGI-IDUSER       TO SBLK-IDUSER-ORDSP               
140700                MOVE JA                TO SW-AENDRA-WDK721                
140800             END-IF                                                       
140900             IF MID-FLORDSP(IX-DC)      = YES OR JA                       
141000                MOVE JA                TO SLAG-FLORDSP                    
141100             ELSE                                                         
141200                MOVE MID-FLORDSP(IX-DC)                                   
141300                                       TO SLAG-FLORDSP                    
141400             END-IF                                                       
141500          END-IF                                                          
141600*                                                                         
141700          IF MID-FLSPBULK(IX-DC)    NOT = ALL '+'                         
141800             IF MID-FLSPBULK(IX-DC) NOT = SLAG-FLSPBULK                   
141900                MOVE DAGENS-DATUM      TO SBLK-DASPBULK                   
142000                MOVE MSGI-IDUSER       TO SBLK-IDUSER-SPBULK              
142100                MOVE JA                TO SW-AENDRA-WDK721                
142200             END-IF                                                       
142300             IF MID-FLSPBULK(IX-DC)     = YES OR JA                       
142400                MOVE JA                TO SLAG-FLSPBULK                   
142500             ELSE                                                         
142600                MOVE MID-FLSPBULK(IX-DC)                                  
142700                                       TO SLAG-FLSPBULK                   
142800             END-IF                                                       
142900          END-IF                                                          
143000          IF MID-IDPERSON-BUY(IX-DC) NOT = ALL '+'                        
143100             MOVE MID-IDPERSON-BUY(IX-DC)                                 
143200                                        TO SLAG-IDPERSON-BUY              
143300             IF MID-IDPERSON-BUY(IX-DC) NOT = ZERO                        
143400               MOVE JA                  TO SLAG-FLBUYUPD                  
143500             END-IF                                                       
143600          END-IF                                                          
143700*                                                                         
143800          PERFORM IMS-REPL-WDK7                                           
143900                                                                          
144000          IF SW-AENDRA-WDK721 = JA                                        
144100             PERFORM IMS-REPL-WDK721                                      
144200             MOVE NEJ                  TO WS-TEARTNOT-FL (IX-DC)          
144300          END-IF                                                          
144400       END-IF                                                             
144500       ADD 1                            TO IX-DC                          
144600     END-PERFORM                                                          
144700*                                                                         
144800*         UPDATE VOLUME, WEIGHT AND COE WHEN US REFILLED FROM             
144900*         CHINA OR CDC                                                    
145000     MOVE    1                 TO IX-DC                                   
145100     PERFORM UNTIL   IX-DC      > IX-DC-MAX                               
145200       MOVE NEJ                           TO UPD-WDK712-SW                
145300       MOVE SPACES                        TO WS-KDARTURS                  
145400       MOVE ZERO                          TO WS-VKART                     
145500                                             WS-VLARTNTO                  
145600                                                                          
145700       IF MID-IDLEVNR-SLAG(IX-DC) NOT      = ALL '+'                      
145800          MOVE WS-IDDC-REF-SLAG(IX-DC)    TO WS-IDDC                      
145900                                                                          
146000          IF CDC                                                          
146100             IF CLAG-KDARTURS              > SPACE                        
146200                MOVE CLAG-KDARTURS        TO WS-KDARTURS                  
146300                MOVE JA                   TO UPD-WDK712-SW                
146400             END-IF                                                       
146500             IF CLAG-VKART                 > ZERO                         
146600                MOVE CLAG-VKART           TO WS-VKART                     
146700                MOVE JA                   TO UPD-WDK712-SW                
146800             END-IF                                                       
146900             IF CLAG-VLARTNTO              > ZERO                         
147000                MOVE CLAG-VLARTNTO        TO WS-VLARTNTO                  
147100                MOVE JA                   TO UPD-WDK712-SW                
147200             END-IF                                                       
147300          ELSE                                                            
147400             PERFORM S02-GET-IDLAND                                       
147500             PERFORM IMS-GU-WDK712                                        
147600             IF SEGMENT-FINNS                                             
147700                IF LART-KDARTURS     > SPACE                              
147800                   MOVE LART-KDARTURS     TO WS-KDARTURS                  
147900                   MOVE JA                TO UPD-WDK712-SW                
148000                END-IF                                                    
148100                IF LART-VKART        > ZERO                               
148200                   MOVE LART-VKART        TO WS-VKART                     
148300                   MOVE JA                TO UPD-WDK712-SW                
148400                END-IF                                                    
148500                IF LART-VLARTNTO     > ZERO                               
148600                   MOVE LART-VLARTNTO     TO WS-VLARTNTO                  
148700                   MOVE JA                TO UPD-WDK712-SW                
148800                END-IF                                                    
148900             END-IF                                                       
149000          END-IF                                                          
149100                                                                          
149200                                                                          
149300          IF UPD-WDK712-OK                                                
149400             MOVE WS-DC-NR (IX-DC)        TO WS-IDDC                      
149500             PERFORM S02-GET-IDLAND                                       
149600             PERFORM IMS-GHU-WDK712                                       
149700             IF SEGMENT-FINNS                                             
149800                IF WS-KDARTURS             > SPACE                        
149900                   MOVE WS-KDARTURS       TO LART-KDARTURS                
150000                END-IF                                                    
150100                IF WS-VKART                > ZERO                         
150200                   MOVE WS-VKART          TO LART-VKART                   
150300                END-IF                                                    
150400                IF WS-VLARTNTO             > ZERO                         
150500                   MOVE WS-VLARTNTO       TO LART-VLARTNTO                
150600                END-IF                                                    
150700                PERFORM IMS-REPL-WDK712                                   
150800             END-IF                                                       
150900          END-IF                                                          
151000       END-IF                                                             
151100       ADD 1                              TO IX-DC                        
151200     END-PERFORM                                                          
151300                                                                          
151400     MOVE W-IDDC-IN          TO WS-IDDC                                   
151500     PERFORM S02-GET-IDLAND                                               
151600     IF MID-FLREFERAL NOT = ALL '+'                                       
151700        PERFORM IMS-GHU-WDK712                                            
151800        IF SEGMENT-FINNS                                                  
151900          IF MID-FLREFERAL NOT = LART-FLREFERAL                           
152000             MOVE JA                   TO UPD-WDK712-SW                   
152100             PERFORM HAA-UPD-WDGX2510                                     
152200          END-IF                                                          
152300          IF MID-FLREFERAL              = YES OR JA                       
152400             MOVE JA                   TO LART-FLREFERAL                  
152500          ELSE                                                            
152600             MOVE MID-FLREFERAL                                           
152700                                    TO LART-FLREFERAL                     
152800          END-IF                                                          
152900          PERFORM IMS-REPL-WDK712                                         
153000        END-IF                                                            
153100     END-IF                                                               
153200     .                                                                    
153300     EJECT                                                                
153400                                                                          
153500 HAA-UPD-WDGX2510 SECTION.                                                
153600                                                                          
153700     MOVE LART-IDLANDX2             TO W-IDLANDX2                         
153800     PERFORM IMS-GHU-WDGX2508                                             
153900     IF SEGMENT-SAKNAS                                                    
154000        MOVE LART-IDLANDX2          TO 2508-IDLANDX2                      
154100        PERFORM IMS-ISRT-WDGX2508                                         
154200     END-IF                                                               
154300                                                                          
154400     PERFORM IMS-GHU-WDGX2510                                             
154500     IF SEGMENT-SAKNAS                                                    
154600        MOVE W-IDARTNR              TO 2510-IDARTNR                       
154700        MOVE MID-FLREFERAL          TO 2510-FLREFERAL                     
154800        MOVE MSGI-IDUSER            TO 2510-IDUSER                        
154900        MOVE DAGENS-DATUM           TO 2510-TIREGDAT                      
155000        PERFORM IMS-ISRT-WDGX2510                                         
155100     ELSE                                                                 
155200        MOVE MID-FLREFERAL          TO 2510-FLREFERAL                     
155300        MOVE MSGI-IDUSER            TO 2510-IDUSER                        
155400        MOVE DAGENS-DATUM           TO 2510-TIREGDAT                      
155500        PERFORM IMS-REPL-WDGX2510                                         
155600     END-IF                                                               
155700     .                                                                    
155800     EJECT                                                                
155900                                                                          
156000 S01-GET-DCGROUP-ALL-DC SECTION.                                          
156100                                                                          
156200     PERFORM DB2-DCL-OPN-TP5IDDC-CRS                                      
156300     PERFORM DB2-FETCH-TP5IDDC-CRS                                        
156400     IF LINES-FOUND                                                       
156500       MOVE +1              TO IX-DC                                      
156600       PERFORM UNTIL IX-DC > IX-DC-MAX OR LINES-MISSING                   
156700                                                                          
156800         MOVE TP5IDDC-IDDC  TO WS-DC-NR (IX-DC)                           
156900         PERFORM DB2-FETCH-TP5IDDC-CRS                                    
157000         MOVE IX-DC         TO IX-DC-N                                    
157100         ADD +1             TO IX-DC                                      
157200       END-PERFORM                                                        
157300       MOVE IX-DC-N         TO IX-DC-MAX                                  
157400     ELSE                                                                 
157500        MOVE NEJ            TO NYCKLAR-SW                                 
157600        MOVE MFS-RENSA-FAELT                                              
157700                            TO MOD-IDDC-UT                                
157800     END-IF                                                               
157900     PERFORM DB2-CLOSE-TP5IDDC-CRS                                        
158000     .                                                                    
158100     EJECT                                                                
158200                                                                          
158300 S02-GET-IDLAND SECTION.                                                  
158400                                                                          
158500     IF NDC-CN                                                            
158600          MOVE  'CN'                TO W-IDLAND                           
158700     ELSE                                                                 
158800       IF NDC-US                                                          
158900          MOVE  'US'                TO W-IDLAND                           
159000       ELSE                                                               
159100         IF NDC-CA                                                        
159200            MOVE 'CA'               TO W-IDLAND                           
159300         END-IF                                                           
159400       END-IF                                                             
159500     END-IF                                                               
159600     .                                                                    
159700     EJECT                                                                
159800                                                                          
159900 S99-ABEND SECTION.                                                       
160000                                                                          
160100     SKIP2                                                                
160200     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
160300     .                                                                    
160400     EJECT                                                                
160500                                                                          
160600 MFS-RENSA-FAELT-UT SECTION.                                              
160700                                                                          
160800*    --- ALLA UTDATA-FÄLT                                                 
160900     MOVE 1                    TO IX-DC                                   
161000     PERFORM UNTIL    IX-DC     > 4                                       
161100       MOVE MFS-RENSA-FAELT    TO MOD-IDLEVNR-SLAG     (IX-DC)            
161200                                  MOD-FLORDSP          (IX-DC)            
161300                                  MOD-DAORDSP          (IX-DC)            
161400                                  MOD-IDUSER-ORDSP     (IX-DC)            
161500                                  MOD-FLSPBULK         (IX-DC)            
161600                                  MOD-DASPBULK         (IX-DC)            
161700                                  MOD-IDUSER-SPBULK    (IX-DC)            
161800                                  MOD-KVDAGAR-MANLT    (IX-DC)            
161900                                  MOD-IDPERSON-BUY     (IX-DC)            
162000       ADD  1                  TO IX-DC                                   
162100     END-PERFORM                                                          
162200     MOVE MFS-RENSA-FAELT      TO MOD-FLREFERAL                           
162300     .                                                                    
162400     SKIP3                                                                
162500                                                                          
162600 MFS-RENSA-FAELT-IN SECTION.                                              
162700                                                                          
162800*    --- ALLA INDATA-FÄLT                                                 
162900     MOVE 1                    TO IX-DC                                   
163000     PERFORM UNTIL    IX-DC     > 4                                       
163100       MOVE MFS-RENSA-FAELT    TO MOD-IDLEVNR-SLAG-IN   (IX-DC)           
163200                                  MOD-FLORDSP-IN        (IX-DC)           
163300                                  MOD-FLSPBULK-IN       (IX-DC)           
163400                                  MOD-IDPERSON-BUY-IN   (IX-DC)           
163500                                  MOD-TEARTNOT-ORDER-IN (IX-DC)           
163600       ADD  1                  TO IX-DC                                   
163700     END-PERFORM                                                          
163800     MOVE MFS-RENSA-FAELT      TO MOD-FLREFERAL-IN                        
163900     .                                                                    
164000     EJECT                                                                
164100                                                                          
164200 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
164300                                                                          
164400*    --- ALLA UTDATA-FÄLT                                                 
164500     MOVE 1 TO IX-DC                                                      
164600     PERFORM UNTIL IX-DC > IX-DC-MAX                                      
164700       MOVE MFS-ROER-EJ-FAELT  TO MOD-IDLEVNR-SLAG(IX-DC)                 
164800                                  MOD-FLORDSP(IX-DC)                      
164900                                  MOD-FLSPBULK(IX-DC)                     
165000                                  MOD-KVDAGAR-MANLT(IX-DC)                
165100                                  MOD-TEARTNOT-ORDER-IN(IX-DC)            
165200                                  MOD-IDPERSON-BUY(IX-DC)                 
165300       ADD  1                  TO IX-DC                                   
165400     END-PERFORM                                                          
165500     MOVE MFS-ROER-EJ-FAELT    TO MOD-FLREFERAL                           
165600     .                                                                    
165700     SKIP3                                                                
165800                                                                          
165900 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
166000                                                                          
166100*    --- ALLA INDATA-FÄLT                                                 
166200     MOVE 1 TO IX-DC                                                      
166300     PERFORM UNTIL IX-DC > IX-DC-MAX                                      
166400       MOVE MFS-ROER-EJ-FAELT  TO MOD-IDLEVNR-SLAG-IN   (IX-DC)           
166500                                  MOD-FLORDSP-IN        (IX-DC)           
166600                                  MOD-FLSPBULK-IN       (IX-DC)           
166700                                  MOD-TEARTNOT-ORDER-IN (IX-DC)           
166800                                  MOD-IDPERSON-BUY-IN   (IX-DC)           
166900       ADD  1                  TO IX-DC                                   
167000     END-PERFORM                                                          
167100     MOVE MFS-ROER-EJ-FAELT    TO MOD-FLREFERAL-IN                        
167200     .                                                                    
167300     EJECT                                                                
167400                                                                          
167500 MFS-FORM-ATTR SECTION.                                                   
167600                                                                          
167700*    --- ALLA INDATA-FÄLT                                                 
167800     MOVE 1 TO IX-DC                                                      
167900     PERFORM UNTIL IX-DC > IX-DC-MAX                                      
168000       MOVE MFS-FORMATETS-ATTR TO                                         
168100                               MOD-IDLEVNR-SLAG-IN-ATTR  (IX-DC)          
168200                               MOD-FLORDSP-IN-ATTR       (IX-DC)          
168300                               MOD-TEARTNOT-ORDER-IN-ATTR(IX-DC)          
168400                               MOD-IDPERSON-BUY-IN-ATTR  (IX-DC)          
168500       ADD  1                  TO IX-DC                                   
168600     END-PERFORM                                                          
168700     MOVE MFS-FORMATETS-ATTR TO MOD-FLREFERAL-IN-ATTR                     
168800     .                                                                    
168900     SKIP2                                                                
169000 MFS-STAENG-FAELT-IN SECTION.                                             
169100                                                                          
169200*    --- ALLA INDATA-FÄLT                                                 
169300     MOVE 1 TO IX-DC                                                      
169400     PERFORM UNTIL IX-DC > 4                                              
169500        MOVE MFS-STAENG-FAELT-NOMOD                                       
169600                            TO MOD-IDLEVNR-SLAG-IN-ATTR  (IX-DC)          
169700                               MOD-FLORDSP-IN-ATTR       (IX-DC)          
169800                               MOD-FLSPBULK-IN-ATTR      (IX-DC)          
169900                               MOD-IDPERSON-BUY-IN-ATTR  (IX-DC)          
170000                               MOD-TEARTNOT-ORDER-IN-ATTR(IX-DC)          
170100        ADD  1 TO IX-DC                                                   
170200     END-PERFORM                                                          
170300     MOVE MFS-STAENG-FAELT-NOMOD    TO MOD-FLREFERAL-IN-ATTR              
170400     .                                                                    
170500     EJECT                                                                
170600                                                                          
170700 MFS-CLOSE-BLANK-DC-FIELD-IN  SECTION.                                    
170800     SKIP2                                                                
170900     MOVE MFS-STAENG-FAELT-NOMOD                                          
171000                            TO MOD-IDLEVNR-SLAG-IN-ATTR  (IX-DC)          
171100                               MOD-FLORDSP-IN-ATTR       (IX-DC)          
171200                               MOD-FLSPBULK-IN-ATTR      (IX-DC)          
171300                               MOD-IDPERSON-BUY-IN-ATTR  (IX-DC)          
171400                               MOD-TEARTNOT-ORDER-IN-ATTR(IX-DC)          
171500     .                                                                    
171600     EJECT                                                                
171700                                                                          
171800* --- IMS SEKTIONER ---                                                   
171900                                                                          
172000 IMS-GET-MSG SECTION.                                                     
172100                                                                          
172200     MOVE '  QC'            TO GODK-STATUSKODER                           
172300     CALL CBLTDLI        USING GU MSG-PCB MSG-IO-AREA                     
172400     MOVE MSG-STATUS-CODE   TO STATUS-WS                                  
172500     PERFORM IMS-STATUSKONTROLL                                           
172600     .                                                                    
172700     EJECT                                                                
172800                                                                          
172900 IMS-INSERT-MSG SECTION.                                                  
173000                                                                          
173100     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
173200       MOVE 'N'             TO MFS-KDHUVOMR                               
173300     END-IF                                                               
173400     MOVE LOW-VALUE         TO MSG-KDZ1 MSG-KDZ2                          
173500     MOVE SPACE             TO GODK-STATUSKODER                           
173600     CALL CBLTDLI        USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD         
173700     MOVE MSG-STATUS-CODE   TO STATUS-WS                                  
173800     PERFORM IMS-STATUSKONTROLL                                           
173900     .                                                                    
174000     EJECT                                                                
174100                                                                          
174200 IMS-GU-ARTC01 SECTION.                                                   
174300                                                                          
174400     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
174500          DELIMITED BY SIZE INTO SSA1                                     
174600     MOVE '  GE'            TO GODK-STATUSKODER                           
174700     CALL CBLTDLI        USING GHU ARTC-PCB DLI-IO-ARTC01 SSA1            
174800     MOVE ARTC-STATUS-CODE  TO STATUS-WS                                  
174900     PERFORM IMS-STATUSKONTROLL                                           
175000     .                                                                    
175100     EJECT                                                                
175200                                                                          
175300 IMS-GNP-ARTC11 SECTION.                                                  
175400     STRING 'WLARTC11(KDSEGKEY =' W-KDSEGKEY-X ')'                        
175500          DELIMITED BY SIZE INTO SSA1                                     
175600     MOVE '  GE'            TO GODK-STATUSKODER                           
175700     CALL CBLTDLI        USING GNP ARTC-PCB DLI-IO-ARTC11 SSA1            
175800     MOVE ARTC-STATUS-CODE  TO STATUS-WS                                  
175900     PERFORM IMS-STATUSKONTROLL                                           
176000     .                                                                    
176100     EJECT                                                                
176200                                                                          
176300 IMS-GNP-ARTC21 SECTION.                                                  
176400                                                                          
176500     STRING 'WLARTC21(DAPRLIST=>' W-DAPRLIST-X ')'                        
176600          DELIMITED BY SIZE INTO SSA1                                     
176700     MOVE '  GE'            TO GODK-STATUSKODER                           
176800     CALL CBLTDLI        USING GNP ARTC-PCB DLI-IO-ARTC21 SSA1            
176900     MOVE ARTC-STATUS-CODE  TO STATUS-WS                                  
177000     PERFORM IMS-STATUSKONTROLL                                           
177100     .                                                                    
177200     EJECT                                                                
177300                                                                          
177400 IMS-GU-WDK701 SECTION.                                                   
177500                                                                          
177600     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
177700          DELIMITED BY SIZE INTO SSA1                                     
177800     MOVE '  GE'            TO GODK-STATUSKODER                           
177900     CALL CBLTDLI        USING GU WDK7-PCB DLI-IO-WDK701 SSA1             
178000     MOVE WDK7-STATUS-CODE  TO STATUS-WS                                  
178100     PERFORM IMS-STATUSKONTROLL                                           
178200     .                                                                    
178300     EJECT                                                                
178400 IMS-GU-WDK711 SECTION.                                                   
178500                                                                          
178600     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
178700          DELIMITED BY SIZE INTO SSA1                                     
178800     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
178900          DELIMITED BY SIZE INTO SSA2                                     
179000     MOVE '  GE'            TO GODK-STATUSKODER                           
179100     CALL CBLTDLI        USING GU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2        
179200     MOVE WDK7-STATUS-CODE  TO STATUS-WS                                  
179300     PERFORM IMS-STATUSKONTROLL                                           
179400     .                                                                    
179500     SKIP3                                                                
179600 IMS-GNP-WDK711 SECTION.                                                  
179700                                                                          
179800     MOVE 'WDK711   '       TO SSA1                                       
179900     MOVE '  GE'            TO GODK-STATUSKODER                           
180000     CALL CBLTDLI        USING GNP WDK7-PCB DLI-IO-WDK711 SSA1            
180100     MOVE WDK7-STATUS-CODE  TO STATUS-WS                                  
180200     PERFORM IMS-STATUSKONTROLL                                           
180300     .                                                                    
180400     EJECT                                                                
180500 IMS-GNP-WDK711-DC SECTION.                                               
180600                                                                          
180700     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
180800          DELIMITED BY SIZE INTO SSA1                                     
180900     MOVE '  GE'            TO GODK-STATUSKODER                           
181000     CALL CBLTDLI        USING GNP WDK7-PCB DLI-IO-WDK711 SSA1            
181100     MOVE WDK7-STATUS-CODE  TO STATUS-WS                                  
181200     PERFORM IMS-STATUSKONTROLL                                           
181300     .                                                                    
181400     EJECT                                                                
181500 IMS-GHNP-WDK711 SECTION.                                                 
181600                                                                          
181700     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
181800          DELIMITED BY SIZE INTO SSA1                                     
181900     MOVE '  '              TO GODK-STATUSKODER                           
182000     CALL CBLTDLI        USING GHNP WDK7-PCB DLI-IO-WDK711 SSA1           
182100     MOVE WDK7-STATUS-CODE  TO STATUS-WS                                  
182200     PERFORM IMS-STATUSKONTROLL                                           
182300     .                                                                    
182400     EJECT                                                                
182500                                                                          
182600                                                                          
182700 IMS-GHNP-WDK711-MIN-MAX SECTION.                                         
182800                                                                          
182900     STRING 'WDK711  (IDDC    >=' W-IDDC-MIN-X                            
183000                    '&IDDC    <=' W-IDDC-MAX-X ')'                        
183100          DELIMITED BY SIZE INTO SSA1                                     
183200     MOVE '  GE'            TO GODK-STATUSKODER                           
183300     CALL CBLTDLI        USING GHNP WDK7-PCB DLI-IO-WDK711 SSA1           
183400     MOVE WDK7-STATUS-CODE  TO STATUS-WS                                  
183500     PERFORM IMS-STATUSKONTROLL                                           
183600     .                                                                    
183700     EJECT                                                                
183800 IMS-GU-WDK721 SECTION.                                                   
183900                                                                          
184000     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
184100          DELIMITED BY SIZE INTO SSA1                                     
184200     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
184300          DELIMITED BY SIZE INTO SSA2                                     
184400     STRING 'WDK721  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
184500          DELIMITED BY SIZE INTO SSA3                                     
184600     MOVE '  GE'            TO GODK-STATUSKODER                           
184700     CALL CBLTDLI        USING GU WDK72-PCB DLI-IO-WDK721                 
184800                               SSA1 SSA2 SSA3                             
184900     MOVE WDK72-STATUS-CODE TO STATUS-WS                                  
185000     PERFORM IMS-STATUSKONTROLL                                           
185100     .                                                                    
185200     EJECT                                                                
185300 IMS-GHU-WDK721 SECTION.                                                  
185400                                                                          
185500     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
185600          DELIMITED BY SIZE INTO SSA1                                     
185700     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
185800          DELIMITED BY SIZE INTO SSA2                                     
185900     STRING 'WDK721  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
186000          DELIMITED BY SIZE INTO SSA3                                     
186100     MOVE '  GE'            TO GODK-STATUSKODER                           
186200     CALL CBLTDLI        USING GHU WDK72-PCB DLI-IO-WDK721                
186300                               SSA1 SSA2 SSA3                             
186400     MOVE WDK72-STATUS-CODE TO STATUS-WS                                  
186500     PERFORM IMS-STATUSKONTROLL                                           
186600     .                                                                    
186700     EJECT                                                                
186800 IMS-GNP-WDK727 SECTION.                                                  
186900                                                                          
187000     MOVE 'WDK727  '        TO SSA1                                       
187100     MOVE '  GE'            TO GODK-STATUSKODER                           
187200     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK727 SSA1                   
187300     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
187400     PERFORM IMS-STATUSKONTROLL                                           
187500     .                                                                    
187600     EJECT                                                                
187700                                                                          
187800 IMS-REPL-WDK721 SECTION.                                                 
187900                                                                          
188000     MOVE '  '              TO GODK-STATUSKODER                           
188100     CALL CBLTDLI        USING REPL WDK72-PCB DLI-IO-WDK721               
188200     MOVE WDK72-STATUS-CODE TO STATUS-WS                                  
188300     PERFORM IMS-STATUSKONTROLL                                           
188400     .                                                                    
188500     EJECT                                                                
188600                                                                          
188700 IMS-REPL-WDK7 SECTION.                                                   
188800                                                                          
188900     MOVE '  '              TO GODK-STATUSKODER                           
189000     CALL CBLTDLI        USING REPL WDK7-PCB DLI-IO-WDK711                
189100     MOVE WDK7-STATUS-CODE  TO STATUS-WS                                  
189200     PERFORM IMS-STATUSKONTROLL                                           
189300     .                                                                    
189400     EJECT                                                                
189500                                                                          
189600 IMS-GU-WDK712   SECTION.                                                 
189700                                                                          
189800     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
189900          DELIMITED BY SIZE INTO SSA1                                     
190000     STRING 'WDK712  (IDLAND   =' W-IDLAND-X ')'                          
190100          DELIMITED BY SIZE INTO SSA2                                     
190200     MOVE '  GE'              TO GODK-STATUSKODER                         
190300     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK712 SSA1 SSA2               
190400     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
190500     PERFORM IMS-STATUSKONTROLL                                           
190600     .                                                                    
190700     EJECT                                                                
190800                                                                          
190900 IMS-GHU-WDK712   SECTION.                                                
191000                                                                          
191100     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
191200          DELIMITED BY SIZE INTO SSA1                                     
191300     STRING 'WDK712  (IDLAND   =' W-IDLAND-X ')'                          
191400          DELIMITED BY SIZE INTO SSA2                                     
191500     MOVE '  '                TO GODK-STATUSKODER                         
191600     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-WDK712 SSA1 SSA2              
191700     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
191800     PERFORM IMS-STATUSKONTROLL                                           
191900     .                                                                    
192000     EJECT                                                                
192100                                                                          
192200 IMS-REPL-WDK712 SECTION.                                                 
192300                                                                          
192400     MOVE '  '              TO GODK-STATUSKODER                           
192500     CALL CBLTDLI        USING REPL WDK7-PCB DLI-IO-WDK712                
192600     MOVE WDK7-STATUS-CODE  TO STATUS-WS                                  
192700     PERFORM IMS-STATUSKONTROLL                                           
192800     .                                                                    
192900     EJECT                                                                
193000                                                                          
193100 IMS-GU-LEVA01 SECTION.                                                   
193200                                                                          
193300     STRING 'WLLEVA01(IDLEVNR  =' W-IDLEVNR-X ')'                         
193400          DELIMITED BY SIZE INTO SSA1                                     
193500     MOVE '  GE'            TO GODK-STATUSKODER                           
193600     CALL CBLTDLI        USING GU LEVA-PCB DLI-IO-LEVA01 SSA1             
193700     MOVE LEVA-STATUS-CODE  TO STATUS-WS                                  
193800     PERFORM IMS-STATUSKONTROLL                                           
193900     .                                                                    
194000     EJECT                                                                
194100                                                                          
194200 IMS-GU-BENA01-BSEQ SECTION.                                              
194300     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
194400          DELIMITED BY SIZE INTO SSA1                                     
194500     MOVE '  GE'            TO GODK-STATUSKODER                           
194600     CALL CBLTDLI        USING GU BENA-PCB DLI-IO-BENA01 SSA1             
194700     MOVE BENA-STATUS-CODE  TO STATUS-WS                                  
194800     PERFORM IMS-STATUSKONTROLL                                           
194900     .                                                                    
195000     EJECT                                                                
195100                                                                          
195200 IMS-GNP-BENA11 SECTION.                                                  
195300     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
195400          DELIMITED BY SIZE INTO SSA1                                     
195500     MOVE '  GE'            TO GODK-STATUSKODER                           
195600     CALL CBLTDLI        USING GU BENA-PCB DLI-IO-BENA11 SSA1             
195700     MOVE BENA-STATUS-CODE  TO STATUS-WS                                  
195800     PERFORM IMS-STATUSKONTROLL                                           
195900     .                                                                    
196000     EJECT                                                                
196100                                                                          
196200 IMS-GN-WDE3-MIN-MAX SECTION.                                             
196300     STRING 'WDE301  (WDE301KY>=' W-WDE301KY-MIN-X                        
196400                    '&WDE301KY<=' W-WDE301KY-MAX-X ')'                    
196500          DELIMITED BY SIZE INTO SSA1                                     
196600     MOVE '  GEGB'          TO GODK-STATUSKODER                           
196700     CALL CBLTDLI        USING GN WDE3-PCB DLI-IO-WDE301 SSA1             
196800     MOVE WDE3-STATUS-CODE  TO STATUS-WS                                  
196900     PERFORM IMS-STATUSKONTROLL                                           
197000     .                                                                    
197100     EJECT                                                                
197200                                                                          
197300 IMS-GU-WDB601       SECTION.                                             
197400                                                                          
197500     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
197600          DELIMITED BY SIZE INTO SSA1                                     
197700     MOVE '  GE'            TO GODK-STATUSKODER                           
197800     CALL CBLTDLI        USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1          
197900     MOVE WDB6-STATUS-CODE  TO STATUS-WS                                  
198000     PERFORM IMS-STATUSKONTROLL                                           
198100     .                                                                    
198200     EJECT                                                                
198300                                                                          
198400 IMS-GU-WDB601-FIRST   SECTION.                                           
198500                                                                          
198600     MOVE   'WDB601  *F '   TO SSA1                                       
198700     MOVE '  GE'            TO GODK-STATUSKODER                           
198800     CALL CBLTDLI        USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1          
198900     MOVE WDB6-STATUS-CODE  TO STATUS-WS                                  
199000     PERFORM IMS-STATUSKONTROLL                                           
199100     .                                                                    
199200     EJECT                                                                
199300 IMS-GN-WDB601-MIN-MAX SECTION.                                           
199400                                                                          
199500     STRING 'WDB601  (IDDC    >=' W-IDDC-MIN-X                            
199600                    '&IDDC    <=' W-IDDC-MAX-X ')'                        
199700          DELIMITED BY SIZE INTO SSA1                                     
199800     MOVE '  GE'            TO GODK-STATUSKODER                           
199900     CALL CBLTDLI        USING GN WDB6-PCB DLI-IO-AREA-B601 SSA1          
200000     MOVE WDB6-STATUS-CODE  TO STATUS-WS                                  
200100     PERFORM IMS-STATUSKONTROLL                                           
200200     .                                                                    
200300     EJECT                                                                
200401 IMS-GU-WDB615 SECTION.                                                   
200501                                                                          
200801     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
200901          DELIMITED BY SIZE INTO SSA1                                     
201001     STRING 'WDB615  (WDB615KY =' W-WDB615KY-X ')'                        
201101          DELIMITED BY SIZE INTO SSA2                                     
201201     MOVE '  GE' TO GODK-STATUSKODER                                      
201301     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB615 SSA1 SSA2               
201401     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
201501     PERFORM IMS-STATUSKONTROLL                                           
201601     .                                                                    
201701     SKIP3                                                                
202001 IMS-GU-WDB616 SECTION.                                                   
202101                                                                          
202201     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
202301          DELIMITED BY SIZE INTO SSA1                                     
202401     STRING 'WDB616  (IDDCREF  =' W-IDDC-REF-X ')'                        
202501          DELIMITED BY SIZE INTO SSA2                                     
202601     MOVE '  GE' TO GODK-STATUSKODER                                      
202602     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB616 SSA1 SSA2               
202603     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
202604     PERFORM IMS-STATUSKONTROLL                                           
202605     .                                                                    
202700 IMS-GHU-WDGX2508 SECTION.                                                
202800                                                                          
202900     STRING 'WDR501  (WDGXKEY  =' W-WDGX2507-X ')'                        
203000          DELIMITED BY SIZE INTO SSA1                                     
203100     STRING 'WDGX2508(IDLANDX2 =' W-IDLANDX2-X ')'                        
203200          DELIMITED BY SIZE INTO SSA2                                     
203300     MOVE '  GE'              TO GODK-STATUSKODER                         
203400     CALL CBLTDLI USING GHU WDR5-PCB DLI-IO-WDGX2508 SSA1 SSA2            
203500     MOVE WDR5-STATUS-CODE    TO STATUS-WS                                
203600     PERFORM IMS-STATUSKONTROLL                                           
203700     .                                                                    
203800                                                                          
203900 IMS-ISRT-WDGX2508 SECTION.                                               
204000                                                                          
204100     STRING 'WDR501  (WDGXKEY  =' W-WDGX2507-X ')'                        
204200            DELIMITED BY SIZE INTO SSA1                                   
204300     MOVE 'WDGX2508'            TO SSA2                                   
204400     MOVE '  '                  TO GODK-STATUSKODER                       
204500     CALL CBLTDLI USING ISRT WDR5-PCB DLI-IO-WDGX2508 SSA1 SSA2           
204600     MOVE WDR5-STATUS-CODE      TO STATUS-WS                              
204700     PERFORM IMS-STATUSKONTROLL                                           
204800     .                                                                    
204900                                                                          
205000 IMS-GHU-WDGX2510 SECTION.                                                
205100                                                                          
205200     STRING 'WDR501  (WDGXKEY  =' W-WDGX2507-X ')'                        
205300          DELIMITED BY SIZE INTO SSA1                                     
205400     STRING 'WDGX2508(IDLANDX2 =' W-IDLANDX2-X ')'                        
205500          DELIMITED BY SIZE INTO SSA2                                     
205600     STRING 'WDGX2510(IDARTNR  =' W-IDARTNR-X ')'                         
205700          DELIMITED BY SIZE INTO SSA3                                     
205800     MOVE '  GE'             TO GODK-STATUSKODER                          
205900     CALL CBLTDLI USING GHU WDR5-PCB DLI-IO-WDGX2510                      
206000                                     SSA1 SSA2 SSA3                       
206100     MOVE WDR5-STATUS-CODE    TO STATUS-WS                                
206200     PERFORM IMS-STATUSKONTROLL                                           
206300     .                                                                    
206400                                                                          
206500 IMS-ISRT-WDGX2510 SECTION.                                               
206600                                                                          
206700     STRING 'WDR501  (WDGXKEY  =' W-WDGX2507-X ')'                        
206800            DELIMITED BY SIZE INTO SSA1                                   
206900     STRING 'WDGX2508(IDLANDX2 =' W-IDLANDX2-X ')'                        
207000          DELIMITED BY SIZE   INTO SSA2                                   
207100     MOVE 'WDGX2510'            TO SSA3                                   
207200     MOVE '  '                  TO GODK-STATUSKODER                       
207300     CALL CBLTDLI USING ISRT WDR5-PCB DLI-IO-WDGX2510                     
207400                                      SSA1 SSA2 SSA3                      
207500     MOVE WDR5-STATUS-CODE      TO STATUS-WS                              
207600     PERFORM IMS-STATUSKONTROLL                                           
207700     .                                                                    
207800                                                                          
207900 IMS-REPL-WDGX2510 SECTION.                                               
208000                                                                          
208100     MOVE '  ' TO GODK-STATUSKODER                                        
208200     CALL CBLTDLI USING REPL WDR5-PCB DLI-IO-WDGX2510                     
208300     MOVE WDR5-STATUS-CODE      TO STATUS-WS                              
208400     PERFORM IMS-STATUSKONTROLL                                           
208500     .                                                                    
208600                                                                          
208700 IMS-GU-WDGX2510 SECTION.                                                 
208800                                                                          
208900     STRING 'WDR501  (WDGXKEY  =' W-WDGX2507-X ')'                        
209000          DELIMITED BY SIZE INTO SSA1                                     
209100     STRING 'WDGX2508(IDLANDX2 =' W-IDLANDX2-X ')'                        
209200          DELIMITED BY SIZE INTO SSA2                                     
209300     STRING 'WDGX2510(IDARTNR  =' W-IDARTNR-X ')'                         
209400          DELIMITED BY SIZE INTO SSA3                                     
209500     MOVE '  GE'             TO GODK-STATUSKODER                          
209600     CALL CBLTDLI USING GU WDR5-PCB DLI-IO-WDGX2510                       
209700                                     SSA1 SSA2 SSA3                       
209800     MOVE WDR5-STATUS-CODE    TO STATUS-WS                                
209900     PERFORM IMS-STATUSKONTROLL                                           
210000     .                                                                    
210100                                                                          
210200     EJECT                                                                
210300 IMS-STATUSKONTROLL SECTION.                                              
210400                                                                          
210500     SET STATUS-IX TO 1                                                   
210600     SEARCH GODK-STATUS                                                   
210700       AT END                                                             
210800         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
210900         DELIMITED BY SIZE INTO FELTEXT                                   
211000         CALL FELLOG                                                      
211100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
211200         CONTINUE                                                         
211300     END-SEARCH                                                           
211400     .                                                                    
211500*    -COPY WY2000P1                                                       
211600 DB2-DCL-OPN-TP5IDDC-CRS  SECTION.                                        
211700                                                                          
211800     MOVE 000100            TO GOOD-SQLCODECODES                          
211900                                                                          
212000     EXEC SQL                                                             
212100        DECLARE TP5IDDC-CRS CURSOR FOR                                    
212200                                                                          
212300          SELECT  IDDC, IDLOPNR_DC                                        
212400                                                                          
212500          FROM    TP5IDDC                                                 
212600          WHERE   IDLOPNR_DC = (SELECT IDLOPNR_DC                         
212700                                FROM TP5IDDC                              
212800                                WHERE IDDC = :W-IDDC-TP5)                 
212900          ORDER BY IDDC                                                   
213000     END-EXEC                                                             
213100                                                                          
213200     MOVE 000100            TO GOOD-SQLCODECODES                          
213300     EXEC SQL OPEN TP5IDDC-CRS END-EXEC                                   
213400                                                                          
213500     .                                                                    
213600 DB2-FETCH-TP5IDDC-CRS  SECTION.                                          
213700     SKIP2                                                                
213800     MOVE 000100            TO GOOD-SQLCODECODES                          
213900     EXEC SQL                                                             
214000        FETCH TP5IDDC-CRS INTO :TP5IDDC-IDDC                              
214100                              ,:TP5IDDC-IDLOPNR-DC                        
214200     END-EXEC                                                             
214300                                                                          
214400     MOVE SQLCODE           TO SQLCODE-WS                                 
214500     PERFORM DB2-STATUS-CHECK                                             
214600     .                                                                    
214700     SKIP3                                                                
214800 DB2-CLOSE-TP5IDDC-CRS  SECTION.                                          
214900                                                                          
215000     EXEC SQL CLOSE TP5IDDC-CRS END-EXEC                                  
215100     .                                                                    
215200     EJECT                                                                
215300 DB2-STATUS-CHECK  SECTION.                                               
215400                                                                          
215500     SET SQLCODE-IX TO 1                                                  
215600     SEARCH GOOD-SQLCODE                                                  
215700       AT END                                                             
215800*         STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
215900*         DELIMITED BY SIZE INTO ERROR-TEXT                               
216000          CALL ABEND USING RKOD-ABEND-DB2                                 
216100       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
216200     END-SEARCH                                                           
217000     .                                                                    
220000     EJECT                                                                
