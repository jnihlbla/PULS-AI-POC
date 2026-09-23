000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2036400.                                                
000300 AUTHOR.         STEFAN ANDREASSON.                                       
000400 DATE-WRITTEN.   98/11/27.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*                                                                         
000900*                                                                         
001000*        DETTA PROGRAM ÄR EN KOPIA AV W2036300                            
001100*                                                                         
001200*        UPPDATERAR DATAELEMENT SOM STYR REFILL                           
001300*                                                                         
001400*        PROGRAMMET UPPDATERAR WLARTC (WDK6)                              
001500*                              WLARTS (WDK7)                              
001600*                              WLOIGA (WDL7)                              
001700*                                                                         
001800*    INDATA.                                                              
001900*        TRANSAKTION: W2T364                                              
002000*        MID:         W2I36401                                            
002100*                                                                         
002200*    UTDATA.                                                              
002300*        MOD:         W2O36401                                            
002400                                                                          
002500     SKIP3                                                                
002600 ENVIRONMENT DIVISION.                                                    
002700     EJECT                                                                
002800 DATA DIVISION.                                                           
002900 WORKING-STORAGE SECTION.                                                 
003000                                                                          
003100*    -COPY WY2000W1                                                       
003200*    -- CHECKED BY WY2000                                                 
003300 77  IDPGM                       PIC X(08)   VALUE 'W2036400'.            
003400                                                                          
003500*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003600 01  FELTEXT.                                                             
003700     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
003800     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
003900     EJECT                                                                
004000                                                                          
004100                                                                          
004200 77  JA                          PIC X       VALUE 'J'.                   
004300 77  YES                         PIC X       VALUE 'Y'.                   
004400 77  NEJ                         PIC X       VALUE 'N'.                   
004500 77  AKTIV                       PIC X       VALUE 'A'.                   
004600 77  DEFINITIV                   PIC S9      VALUE +1 COMP-3.             
004700 77  IX                          PIC 9(3)    VALUE ZERO.                  
004800 77  WS-IDLEVNR-NUM              PIC X(05).                               
004900 77  WS-KDARTURS                 PIC X(2)    VALUE SPACES.                
005000 77  WS-VKART                    PIC S9(7)   VALUE ZERO   COMP-3.         
005100 77  WS-VLARTNTO                 PIC S9(8)V9(1)                           
005200                                             VALUE ZERO   COMP-3.         
005300 77  W-IDLAND-REF                PIC X(2)    VALUE SPACES.                
005400     88  IDLAND-CN                           VALUE 'CN'.                  
005500     88  IDLAND-SE                           VALUE 'SE'.                  
005600     88  IDLAND-US                           VALUE 'US'.                  
005700                                                                          
005800                                                                          
005900*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
006000                                                                          
006100 77  INDATA-SW                   PIC X       VALUE 'J'.                   
006200     88  INDATA-OK                           VALUE 'J'.                   
006300     88  INDATA-FEL                          VALUE 'N'.                   
006400                                                                          
006500 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
006600     88  NYCKLAR-OK                          VALUE 'J'.                   
006700     88  NYCKLAR-FEL                         VALUE 'N'.                   
006800                                                                          
006900 77  TRAFF-SW                    PIC X       VALUE 'N'.                   
007000     88  TRAFF-OK                            VALUE 'J'.                   
007100     88  NO-TRAFF                            VALUE 'N'.                   
007200                                                                          
007300 77  SUPPLIER-SW                 PIC X       VALUE 'N'.                   
007400     88  SUPPLIER-OK                         VALUE 'J'.                   
007500     88  SUPPLIER-NEJ                        VALUE 'N'.                   
007600                                                                          
007700 77  UPD-WDK712-SW               PIC X       VALUE 'N'.                   
007800     88  UPD-WDK712-OK                       VALUE 'J'.                   
007900     88  UPD-WDK712-FEL                      VALUE 'N'.                   
008000                                                                          
008100 77  SW-REF-LEV                  PIC X       VALUE 'J'.                   
008200     88  REFILLED                            VALUE 'J'.                   
008300     88  LOCALPO                             VALUE 'N'.                   
008400                                                                          
008500 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
008600     88  EGEN-MID                            VALUE '2364'.                
008700     88  GODK-MID                            VALUE '2361' '2362'          
008800                                                   '2363'.                
008900     88  HELP-MID                            VALUE '0551'.                
009000     EJECT                                                                
009100*    --- ÖVRIGA FLAGGOR                                                   
009200 77  SW-AENDRA-WDK721            PIC X       VALUE 'N'.                   
009300                                                                          
009400*    --- ÖVRIGA ARBETSFÄLT                                                
009500                                                                          
009600 01  ARBETSFALT.                                                          
009700     03 WS-PRARTBES              PIC S9(7)V9(2) VALUE ZERO COMP-3.        
009800     03 FL-PRARTBES              PIC X       VALUE 'N'.                   
009900     03 IX1                      PIC 9(2)    VALUE ZERO.                  
010000     03 IX2                      PIC 9(2)    VALUE ZERO.                  
010100     03 URLAND-IX                PIC S9(3)   VALUE ZERO COMP-3.           
010200     03 WS-VECKA                 PIC 9(2)    VALUE ZERO.                  
010300     03 WS-TEMFSINF              PIC X(55)   VALUE SPACE.                 
010400     03 WS-IDDC-SPAR             PIC X(2)    VALUE SPACE.                 
010500     03 WS-IDDC-REF              PIC X(2)    VALUE SPACE.                 
010600     03 WS-IDPERSON-BUY          PIC 9(3)    VALUE ZERO.                  
010700     03 WS-IDLAND-SPAR           PIC X(2)    VALUE SPACE.                 
010800     03  WS-IDDC-LAND            PIC  X(2)   VALUE SPACE.                 
010900                                                                          
011000     03 WS-IDLEVNR-SLAG-GRP OCCURS 4.                                     
011100        05 WS-IDLEVNR-SLAG       PIC X(5).                                
011200                                                                          
011300 01  DAGENS-AAAAMMDD             PIC 9(8)    VALUE ZERO.                  
011400 01  DAGENS-TIAAVVD              PIC 9(5)    VALUE ZERO.                  
011500 01  FILLER REDEFINES DAGENS-TIAAVVD.                                     
011600     03 DAGENS-TIAAVV            PIC 9(4).                                
011700     03 DAGENS-TID               PIC 9(1).                                
011800                                                                          
011900 01  DATUMFALT.                                                           
012000     03 DAGENS-DATUM             PIC S9(7) VALUE ZERO COMP-3.             
012100                                                                          
012200*    ---KONTROLLFÄLT                                                      
012300                                                                          
012400*      --- VALID IDDC CODES                                               
012500*                                                                         
012600*01    -COPY WWDC99                                                       
012700*01    -COPY WWDC99        -PRE REF-                                      
012800*01    -COPY WWDCKONS                                                     
012900       EJECT                                                              
013000                                                                          
013100*01    -COPY WWPRODSL                                                     
013200       EJECT                                                              
013300*01    -COPY WWDCLAND                                                     
013400*                                                                         
013500                                                                          
013600*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
013700 01  GENERELLA-SUBPROGRAM.                                                
013800     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
013900     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
014000     03  W005WDK7                PIC X(8)    VALUE 'W005WDK7'.            
014100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
014200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
014300     03  ABEND                   PIC X(8)    VALUE 'ABEND  '.             
014400     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
014500     03  W271REFL                PIC X(8)    VALUE 'W271REFL'.            
014600     03  W271UTUP                PIC X(8)    VALUE 'W271UTUP'.            
014700     EJECT                                                                
014800*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
014900*01 -COPY WMEDAREA                                                        
015000     EJECT                                                                
015100*    --- PARAMETRAR TILL W271REFL                                         
015200*01 -COPY W271REFL                                                        
015300     EJECT                                                                
015400*    --- PARAMETRAR TILL W271UTUP                                         
015500*01 -COPY W271UTUP                                                        
015600     EJECT                                                                
015700*    --- PARAMETRAR TILL W005WDK7                                         
015800*01 -COPY W005WDK7                                                        
015900     EJECT                                                                
016000     SKIP3                                                                
016100 01  MESSAGE-CODES.                                                       
016200     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
016300     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
016400     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
016500     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
016600     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
016700     03  PART-MISSING            PIC X(3)    VALUE '017'.                 
016800     03  PART-SUPERSEDED         PIC X(3)    VALUE '018'.                 
016900     03  ERR-NOT-REFILL-PART     PIC X(3)    VALUE '957'.                 
017000     03  UPDATE-NOT-ALLOWED      PIC X(3)    VALUE '007'.                 
017100 01  MEDDELANDEN.                                                         
017200     03  MED-1                   PIC X(55)                                
017300         VALUE 'PART NOT AVAILABLE AT ANY NDC          '.                 
017400     03  MED-2                   PIC X(40)                                
017500         VALUE 'NO REFILLPART                          '.                 
017600     03  MED-3                   PIC X(40)                                
017700         VALUE 'CDC PART                               '.                 
017800     03  MED-4                   PIC X(40)                                
017900         VALUE 'WRONG ORIGIN CODE                      '.                 
018000     03  MED-5                   PIC X(40)                                
018100         VALUE 'NO ORIGIN CODE AVAILABLE               '.                 
018200     03  MED-6                   PIC X(40)                                
018300         VALUE 'LOCAL PART                             '.                 
018400     03  MED-7                   PIC X(40)                                
018500         VALUE 'UNKNOWN SUPPLIER                       '.                 
018600     03  MED-8                   PIC X(40)                                
018700         VALUE 'INVALID SUPPLIER                       '.                 
018800     03  MED-9                   PIC X(40)                                
018900         VALUE 'SUPPLIER SAME AS THE DC                '.                 
019000     03  MED-13                  PIC X(40)                                
019100         VALUE 'SAME SUPPLIER AS TODAY                 '.                 
019200     03  MED-14                  PIC X(40)                                
019300         VALUE 'REFILL ORDER OR PROPOSAL EXISTS        '.                 
019400     03  MED-15                  PIC X(40)                                
019500         VALUE 'WRONG SUPPLIER                         '.                 
019600     03  MED-16                  PIC X(40)                                
019700         VALUE 'NOT ALLOWED SUPPLIER                   '.                 
019800     03  MED-17                  PIC X(40)                                
019900         VALUE 'UPDATING OF BUYER LOCKED               '.                 
020000     03  MED-18                  PIC X(40)                                
020100         VALUE 'FFC EXISTS                             '.                 
020200     03  MED-19                  PIC X(40)                                
020300         VALUE 'REFILL FLOW NOT AVAILABLE              '.                 
020400     03  FEL-125                 PIC X(47) VALUE                          
020500            'USE EQUAL SUPPLIER, SEE SCREEN 2111'.                        
020600     EJECT                                                                
020700*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
020800*                                                                         
020900 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
021000     SKIP3                                                                
021100*01 -COPY WMSGINIT                                                        
021200     SKIP3                                                                
021300*    --- PARAMETRAR TILL ABEND                                            
021400                                                                          
021500 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
021600 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
021700     EJECT                                                                
021800*01 -COPY WDATAREA                                                        
021900     SKIP3                                                                
022000*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
022100*                                                                         
022200 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
022300     SKIP3                                                                
022400*01  MID -COPY W2I36401                                                   
022500     EJECT                                                                
022600 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
022700     SKIP3                                                                
022800*01  -COPY WMSGAREA                                                       
022900     EJECT                                                                
023000     03  MOD REDEFINES MSG-AREA.                                          
023100*      05  -COPY W2O36401                                                 
023200     EJECT                                                                
023300 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
023400     SKIP3                                                                
023500*01  -COPY WMFSAREA                                                       
023600     EJECT                                                                
023700*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
023800*                                                                         
023900     EJECT                                                                
024000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
024100     SKIP3                                                                
024200 01  NYCKLAR-TILL-DLI.                                                    
024300     03  W-IDARTNR-X.                                                     
024400         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
024500     03  W-KDSEGKEY-X.                                                    
024600         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
024700     03  W-IDLAND-X.                                                      
024800         05  W-IDLAND            PIC X(2)    VALUE SPACE.                 
024900     03  W-IDLANDX2-X.                                                    
025000         05  W-IDLANDX2          PIC X(2)    VALUE SPACE.                 
025100     03  W-IDDC-X.                                                        
025200         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
025300     03  W-IDDC-B6-X.                                                     
025400         05  W-IDDC-B6           PIC X(2)    VALUE SPACE.                 
025500     03  W-IDDC-K7-X.                                                     
025600         05  W-IDDC-K7           PIC X(2)    VALUE SPACE.                 
025700     03  W-IDLEVNR-X.                                                     
025800         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
025900     03  W-IDLEVNR-K7-X.                                                  
026000         05  W-IDLEVNR-K7        PIC X(5)    VALUE SPACE.                 
026100     03  W-IDLEVNRDC-X.                                                   
026200         05  W-IDLEVNRDC         PIC X(5)    VALUE SPACE.                 
026300     03  W-DAPRLIST-X.                                                    
026400         05  W-DAPRLIST          PIC 9(8)    VALUE ZERO.                  
026500     03  W-IDSKYLT-X.                                                     
026600         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
026700     03 W-WDE301KY-MIN-X.                                                 
026800         05  W-IDDC-MIN-E3       PIC X(2)  VALUE SPACE.                   
026900         05  FILER               PIC X(11) VALUE LOW-VALUE.               
027000     03  W-KDREFTYP-MIN-X.                                                
027100         05  W-KDREFTYP-MIN      PIC X       VALUE 'A'.                   
027200     03  W-KDREFTYP-MAX-X.                                                
027300         05  W-KDREFTYP-MAX      PIC X       VALUE 'R'.                   
027400     03  W-WDGX2507-X.                                                    
027500         05  W-IDHTYP-2507       PIC X(4)     VALUE '2507'.               
027600         05  FILLER              PIC X(26)    VALUE LOW-VALUE.            
027700                                                                          
027800     03 W-WDE301KY-MAX-X.                                                 
027900         05  W-IDDC-MAX-E3       PIC X(2)  VALUE SPACE.                   
028000         05  FILER               PIC X(11) VALUE HIGH-VALUE.              
028100     03  W-IDDC-REF-X.                                                    
028200         05  W-IDDC-REF          PIC X(2)    VALUE SPACE.                 
028300     03  W-WDB615KY-X.                                                    
028400         05  W-IDTRANS-B6        PIC X(4)    VALUE SPACE.                 
028500         05  W-IDDC-REF-B6       PIC X(2)    VALUE SPACE.                 
028600     SKIP2                                                                
028700*    --- STATUS-KOD FRÅN IMS                                              
028800 01  STATUS-WS                   PIC XX.                                  
028900     88  SEGMENT-FINNS                       VALUE '  '.                  
029000     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
029100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
029200     88  SEGMENT-SLUT                        VALUE 'GB'.                  
029300     SKIP2                                                                
029400 01  GODK-STATUSKODER.                                                    
029500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
029600     SKIP3                                                                
029700 01  SSA1                        PIC X(128).                              
029800 01  SSA2                        PIC X(64).                               
029900 01  SSA3                        PIC X(64).                               
030000     EJECT                                                                
030100*    --- IMS FUNKTIONSKODER                                               
030200*01  -COPY W0003                                                          
030300     EJECT                                                                
030400*    ---  DLI INPUT-OUTPUT AREA                                           
030500                                                                          
030600 01  FILLER         PIC X(24) VALUE 'DLI-IO-ARTC01'.                      
030700 01  DLI-IO-ARTC01.                                                       
030800*    03  -COPY WDK601                                                     
030900     EJECT                                                                
031000                                                                          
031100 01  FILLER         PIC X(24) VALUE 'DLI-IO-ARTC11'.                      
031200 01  DLI-IO-ARTC11.                                                       
031300*    03  -COPY WDK611                                                     
031400                                                                          
031500 01  FILLER         PIC X(24) VALUE 'DLI-IO-ARTC21'.                      
031600 01  DLI-IO-ARTC21.                                                       
031700*    03  -COPY WDK621                                                     
031800                                                                          
031900 01  FILLER         PIC X(24) VALUE 'DLI-IO-ARTS01'.                      
032000 01  DLI-IO-ARTS01.                                                       
032100*    03  -COPY WDK701                                                     
032200                                                                          
032300 01  FILLER         PIC X(24) VALUE 'DLI-IO-ARTS11'.                      
032400 01  DLI-IO-ARTS11.                                                       
032500*    03  -COPY WDK711                                                     
032600     EJECT                                                                
032700                                                                          
032800 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK721'.                      
032900 01  DLI-IO-WDK721.                                                       
033000*    03  -COPY WDK721                                                     
033100     EJECT                                                                
033200                                                                          
033300 01  FILLER         PIC X(24) VALUE 'DLI-IO-LEVA01'.                      
033400 01  DLI-IO-LEVA01.                                                       
033500*    03  -COPY WDF101                                                     
033600     EJECT                                                                
033700                                                                          
033800 01  FILLER         PIC X(24) VALUE 'DLI-IO-BENA01'.                      
033900 01  DLI-IO-BENA01.                                                       
034000*    03  -COPY WDD301  -PRE BENA-                                         
034100     EJECT                                                                
034200                                                                          
034300 01  FILLER         PIC X(24) VALUE 'DLI-IO-BENA11'.                      
034400 01  DLI-IO-BENA11.                                                       
034500*    03  -COPY WDD311  -PRE BENA-                                         
034600     EJECT                                                                
034700 01  FILLER                  PIC X(16) VALUE 'DLI-IO-OIGA11'.             
034800     SKIP3                                                                
034900 01  DLI-IO-AREA-OIGA11.                                                  
035000*        05  -COPY WDL711                                                 
035100     EJECT                                                                
035200                                                                          
035300 01  FILLER                  PIC X(24) VALUE 'DLI-IO-WDK701'.             
035400 01  DLI-IO-WDK701.                                                       
035500*    03  -COPY WDK701  -PRE K7-                                           
035600                                                                          
035700 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK711'.             
035800 01  DLI-IO-WDK711.                                                       
035900*    03  -COPY WDK711  -PRE K7-                                           
036000                                                                          
036100 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK722'.             
036200 01  DLI-IO-WDK722.                                                       
036300*    03  -COPY WDK722                                                     
036400                                                                          
036500 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK727'.             
036600 01  DLI-IO-WDK727.                                                       
036700*    03  -COPY WDK727                                                     
036800                                                                          
036900 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK712'.             
037000 01  DLI-IO-WDK712.                                                       
037100*    03  -COPY WDK712                                                     
037200                                                                          
037300 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDB601'.             
037400 01  DLI-IO-WDB601.                                                       
037500*    03  -COPY WDB601                                                     
037600                                                                          
037700                                                                          
037800 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDB615'.             
037900 01  DLI-IO-WDB615.                                                       
038000*    03  -COPY WDB615                                                     
038100 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDB616'.             
038200 01  DLI-IO-WDB616.                                                       
038300*    03  -COPY WDB616                                                     
038400                                                                          
038500 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDE301'.             
038600 01  DLI-IO-WDE301.                                                       
038700*    03  -COPY WDE301                                                     
038800     EJECT                                                                
038900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX2508'.                    
039000 01  DLI-IO-WDGX2508.                                                     
039100*    03  -COPY WDGX2508                                                   
039200     EJECT                                                                
039300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX2510'.                    
039400 01  DLI-IO-WDGX2510.                                                     
039500*    03  -COPY WDGX2510                                                   
039600     EJECT                                                                
039700                                                                          
039800 LINKAGE SECTION.                                                         
039900*01  -COPY W0009   -PRE MSG-                                              
040000                                                                          
040100*01  -COPY W0008   -PRE USEA-                                             
040200     05  FILLER                  PIC X.                                   
040300     EJECT                                                                
040400                                                                          
040500*01  -COPY W0008  -PRE ARTC-                                              
040600     05  FILLER                  PIC X.                                   
040700     EJECT                                                                
040800                                                                          
040900*01  -COPY W0008  -PRE ARTS-                                              
041000     05  FILLER                  PIC X.                                   
041100     EJECT                                                                
041200                                                                          
041300*01  -COPY W0008  -PRE WDK7-                                              
041400     05  FILLER                  PIC X.                                   
041500     EJECT                                                                
041600                                                                          
041700*01  -COPY W0008  -PRE LEVA-                                              
041800     05  FILLER                  PIC X.                                   
041900     EJECT                                                                
042000                                                                          
042100*01  -COPY W0008  -PRE BENA-                                              
042200     05  FILLER                  PIC X.                                   
042300     EJECT                                                                
042400*01  -COPY W0008  -PRE REFL-2501-                                         
042500     05  FILLER                  PIC X.                                   
042600*01  -COPY W0008  -PRE OIGA-                                              
042700     05  FILLER                  PIC X.                                   
042800     EJECT                                                                
042900*01  -COPY W0008      -PRE WDB6-                                          
043000     05  FILLER                  PIC X.                                   
043100     EJECT                                                                
043200*01  -COPY W0008      -PRE WDK7-22-                                       
043300     05  FILLER                  PIC X.                                   
043400     EJECT                                                                
043500 01  UTIL-WDK6-PCB               PIC X.                                   
043600 01  UTIL-WDK7-PCB               PIC X.                                   
043700 01  UTIL-WDB6-PCB               PIC X.                                   
043800     EJECT                                                                
043900*01  -COPY W0008      -PRE WDK72-                                         
044000     05  FILLER                  PIC X.                                   
044100     EJECT                                                                
044200*01  -COPY W0008      -PRE WDE3-                                          
044300     05  FILLER                  PIC X.                                   
044400     EJECT                                                                
044500*01  -COPY W0008      -PRE WDR5-                                          
044600     05  FILLER                  PIC X.                                   
044700     EJECT                                                                
044800*****W271UTUP**********                                                   
044900 01  UTUP1-WDK7-PCB              PIC X.                                   
045000 01  UTUP1-WDB6-PCB              PIC X.                                   
045100 01  UTUP1-UTIL-WDK6-PCB         PIC X.                                   
045200 01  UTUP1-UTIL-WDK7-PCB         PIC X.                                   
045300 01  UTUP1-UTIL-WDB6-PCB         PIC X.                                   
045400     EJECT                                                                
045500                                                                          
045600 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB                               
045700                           ARTC-PCB ARTS-PCB WDK7-PCB LEVA-PCB            
045800                           BENA-PCB REFL-2501-PCB OIGA-PCB                
045900                           WDB6-PCB WDK7-22-PCB                           
046000                           UTIL-WDK6-PCB                                  
046100                           UTIL-WDK7-PCB                                  
046200                           UTIL-WDB6-PCB                                  
046300                           WDK72-PCB WDE3-PCB WDR5-PCB                    
046400                           UTUP1-WDK7-PCB                                 
046500                           UTUP1-WDB6-PCB                                 
046600                           UTUP1-UTIL-WDK6-PCB                            
046700                           UTUP1-UTIL-WDK7-PCB                            
046800                           UTUP1-UTIL-WDB6-PCB.                           
046900 MAIN SECTION.                                                            
047000     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB                               
047100                           ARTC-PCB ARTS-PCB WDK7-PCB LEVA-PCB            
047200                           BENA-PCB REFL-2501-PCB OIGA-PCB                
047300                           WDB6-PCB WDK7-22-PCB                           
047400                           UTIL-WDK6-PCB                                  
047500                           UTIL-WDK7-PCB                                  
047600                           UTIL-WDB6-PCB                                  
047700                           WDK72-PCB WDE3-PCB WDR5-PCB                    
047800                           UTUP1-WDK7-PCB                                 
047900                           UTUP1-WDB6-PCB                                 
048000                           UTUP1-UTIL-WDK6-PCB                            
048100                           UTUP1-UTIL-WDK7-PCB                            
048200                           UTUP1-UTIL-WDB6-PCB.                           
048300                                                                          
048400     PERFORM IMS-GET-MSG                                                  
048500     IF SEGMENT-FINNS                                                     
048600       PERFORM A-INIT                                                     
048700       PERFORM B-KOLLA-NYCKLAR                                            
048800       IF NYCKLAR-OK                                                      
048900         IF MFS-UPDATE                                                    
049000           PERFORM G-KOLLA-INPUT                                          
049100           IF INDATA-OK                                                   
049200             PERFORM H-UPPDATERA                                          
049300           END-IF                                                         
049400         ELSE                                                             
049500           IF MFS-FIRST                                                   
049600             PERFORM C-FOERSTA-SIDA                                       
049700           ELSE                                                           
049800             PERFORM E-SAMMA-SIDA                                         
049900           END-IF                                                         
050000         END-IF                                                           
050100         PERFORM F-LAES-VISA-INFO                                         
050200       END-IF                                                             
050300       COMPUTE MSG-KVLL = LENGTH OF MOD-W2O36401 + 4                      
050400       PERFORM IMS-INSERT-MSG                                             
050500     END-IF                                                               
050600                                                                          
050700     MOVE ZERO TO RETURN-CODE                                             
050800     GOBACK                                                               
050900     .                                                                    
051000     EJECT                                                                
051100                                                                          
051200                                                                          
051300 A-INIT SECTION.                                                          
051400                                                                          
051500     ACCEPT DAGENS-DATUM FROM DATE                                        
051600                                                                          
051700     IF MSG-DUBBLA-TRANSKODER                                             
051800       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W2I36401                 
051900       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
052000       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
052100     ELSE                                                                 
052200       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W2I36401                  
052300       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
052400       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
052500     END-IF                                                               
052600                                                                          
052700     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
052800     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
052900     MOVE MFS-IDTRANS TO W-IDTRANS                                        
053000                                                                          
053100     MOVE LOW-VALUE TO MSG-AREA                                           
053200     MOVE 'W2O364N1' TO MFS-IDMOD                                         
053300     MOVE '2364' TO MOD-IDTRANS                                           
053400     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
053500                                                                          
053600     IF EGEN-MID OR HELP-MID                                              
053700       CONTINUE                                                           
053800     ELSE                                                                 
053900       MOVE SPACE TO MFS-KDTRTYP                                          
054000       MOVE '7' TO MFS-IDPFK                                              
054100     END-IF                                                               
054200                                                                          
054300     MOVE 'GB ' TO MED-IDSKYLT                                            
054400     .                                                                    
054500     EJECT                                                                
054600                                                                          
054700                                                                          
054800 B-KOLLA-NYCKLAR SECTION.                                                 
054900                                                                          
055000     MOVE ALL '+'           TO MSGI-WMSGINIT                              
055100     MOVE '001'             TO MSGI-KDCALL                                
055200     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
055300     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
055400     MOVE '2364'            TO MSGI-IDTRANS                               
055500     IF EGEN-MID                                                          
055600        MOVE MID-IDARTNR-IN  TO MSGI-IDARTNR                              
055700        MOVE MID-IDDC-IN     TO MSGI-IDDC-KEY                             
055800     ELSE                                                                 
055900        IF MID-IDARTNR-IN NUMERIC                                         
056000        AND MID-IDARTNR-IN > ZERO                                         
056100           MOVE MID-IDARTNR-IN  TO MSGI-IDARTNR                           
056200        END-IF                                                            
056300     END-IF                                                               
056400     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
056500                                                                          
056600     MOVE JA TO NYCKLAR-SW                                                
056700                                                                          
056800                                                                          
056900*    -- KONTROLL AV IDARTNR                                               
057000     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
057100                                                                          
057200     IF MID-IDARTNR-IN NOT = ALL '+'                                      
057300       MOVE '7'         TO MFS-IDPFK                                      
057400       MOVE SPACE       TO MFS-KDTRTYP                                    
057500     END-IF                                                               
057600     INSPECT MSGI-IDARTNR REPLACING LEADING SPACE BY ZERO                 
057700     IF MSGI-IDARTNR NUMERIC                                              
057800       MOVE MSGI-IDARTNR TO W-IDARTNR                                     
057900     ELSE                                                                 
058000       MOVE NEJ TO NYCKLAR-SW                                             
058100     END-IF                                                               
058200                                                                          
058300                                                                          
058400*    -- KONTROLL AV IDDC                                                  
058500     MOVE MFS-RENSA-FAELT TO MOD-IDDC-IN                                  
058600                                                                          
058700     IF MID-IDDC-IN NOT = ALL '+'                                         
058800       MOVE '7'           TO MFS-IDPFK                                    
058900       MOVE SPACE         TO MFS-KDTRTYP                                  
059000     END-IF                                                               
059100                                                                          
059200     MOVE MSGI-IDDC-KEY TO WS-IDDC                                        
059300                           WS-IDDC-SPAR                                   
059400                           W-IDDC-B6                                      
059500                           WS-IDDC-LAND                                   
059600                                                                          
059700     IF NDC  AND NOT NDC-NA                                               
059800       MOVE MSGI-IDDC-KEY TO W-IDDC                                       
059900     ELSE                                                                 
060000       MOVE NEJ           TO NYCKLAR-SW                                   
060100     END-IF                                                               
060200                                                                          
060300     PERFORM S9-SEARCH-IDLAND                                             
060400     MOVE W-IDLAND              TO WS-IDLAND-SPAR                         
060500                                                                          
060600     IF NYCKLAR-OK                                                        
060700       MOVE MSGI-IDDC-KEY       TO MOD-IDDC-UT                            
060800       MOVE MSGI-IDARTNR        TO MOD-IDARTNR-UT                         
060900       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
061000     ELSE                                                                 
061100       MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-UT                             
061200     END-IF                                                               
061300                                                                          
061400     IF NYCKLAR-FEL                                                       
061500       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
061600       MOVE 'GB '         TO MED-IDSKYLT                                  
061700       CALL WMEDKONV USING MED-WMEDAREA                                   
061800       MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                  
061900       PERFORM MFS-RENSA-FAELT-IN                                         
062000       PERFORM MFS-RENSA-FAELT-UT                                         
062100     END-IF                                                               
062200     .                                                                    
062300     EJECT                                                                
062400                                                                          
062500                                                                          
062600 C-FOERSTA-SIDA SECTION.                                                  
062700                                                                          
062800     PERFORM MFS-RENSA-FAELT-IN                                           
062900     .                                                                    
063000     EJECT                                                                
063100                                                                          
063200                                                                          
063300 E-SAMMA-SIDA SECTION.                                                    
063400                                                                          
063500     IF EGEN-MID OR HELP-MID                                              
063600       IF MID-INPUT = ALL '+'                                             
063700         PERFORM MFS-RENSA-FAELT-IN                                       
063800       ELSE                                                               
063900         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
064000         MOVE 'GB '         TO MED-IDSKYLT                                
064100         CALL WMEDKONV USING MED-WMEDAREA                                 
064200         MOVE MED-TEMFSINF TO MOD-TEMFSINF                                
064300         PERFORM EA-MID-INDATA-TILL-MOD                                   
064400       END-IF                                                             
064500     ELSE                                                                 
064600       PERFORM MFS-RENSA-FAELT-IN                                         
064700     END-IF                                                               
064800     .                                                                    
064900     EJECT                                                                
065000                                                                          
065100                                                                          
065200 EA-MID-INDATA-TILL-MOD SECTION.                                          
065300                                                                          
065400     IF MID-IDLEVNR-SLAG NOT = ALL '+'                                    
065500        MOVE MFS-ROER-EJ-FAELT TO                                         
065600                            MOD-IDLEVNR-SLAG-IN                           
065700     ELSE                                                                 
065800        MOVE MFS-RENSA-FAELT TO MOD-IDLEVNR-SLAG-IN                       
065900     END-IF.                                                              
066000     MOVE MFS-ALFA-FAELT-RAETT TO                                         
066100                         MOD-IDLEVNR-SLAG-IN-ATTR.                        
066200                                                                          
066300                                                                          
066400     IF MID-FLORDSP NOT = ALL '+'                                         
066500        MOVE MFS-ROER-EJ-FAELT        TO MOD-FLORDSP-IN                   
066600     ELSE                                                                 
066700        MOVE MFS-RENSA-FAELT          TO MOD-FLORDSP-IN                   
066800     END-IF.                                                              
066900     MOVE MFS-ALFA-FAELT-RAETT TO                                         
067000                         MOD-FLORDSP-IN-ATTR.                             
067100                                                                          
067200                                                                          
067300     IF MID-FLSPBULK NOT = ALL '+'                                        
067400        MOVE MFS-ROER-EJ-FAELT        TO MOD-FLSPBULK-IN                  
067500     ELSE                                                                 
067600        MOVE MFS-RENSA-FAELT          TO MOD-FLSPBULK-IN                  
067700     END-IF.                                                              
067800     MOVE MFS-ALFA-FAELT-RAETT TO                                         
067900                         MOD-FLSPBULK-IN-ATTR.                            
068000                                                                          
068100                                                                          
068200     IF MID-KVDAGAR-MANLT NOT = ALL '+'                                   
068300        MOVE MFS-ROER-EJ-FAELT     TO MOD-KVDAGAR-MANLT-IN                
068400     ELSE                                                                 
068500        MOVE MFS-RENSA-FAELT       TO MOD-KVDAGAR-MANLT-IN                
068600     END-IF.                                                              
068700     MOVE MFS-NUM-FAELT-RAETT     TO                                      
068800                              MOD-KVDAGAR-MANLT-IN-ATTR.                  
068900                                                                          
069000                                                                          
069100     IF MID-IDPERSON-BUY NOT = ALL '+'                                    
069200        MOVE MFS-ROER-EJ-FAELT     TO MOD-IDPERSON-BUY-IN                 
069300     ELSE                                                                 
069400        MOVE MFS-RENSA-FAELT       TO MOD-IDPERSON-BUY-IN                 
069500     END-IF.                                                              
069600     MOVE MFS-NUM-FAELT-RAETT                                             
069700                           TO MOD-IDPERSON-BUY-IN-ATTR.                   
069800                                                                          
069900                                                                          
070000     IF MID-FLREFERAL NOT = ALL '+'                                       
070100        MOVE MFS-ROER-EJ-FAELT        TO MOD-FLREFERAL-IN                 
070200     ELSE                                                                 
070300        MOVE MFS-RENSA-FAELT          TO MOD-FLREFERAL-IN                 
070400     END-IF.                                                              
070500     MOVE MFS-ALFA-FAELT-RAETT TO                                         
070600                         MOD-FLREFERAL-IN-ATTR.                           
070700                                                                          
070800                                                                          
070900     IF MID-TEARTNOT-ORDER NOT = ALL '+'                                  
071000        MOVE MFS-ROER-EJ-FAELT     TO MOD-TEARTNOT-ORDER-IN               
071100     ELSE                                                                 
071200        MOVE MFS-RENSA-FAELT       TO MOD-TEARTNOT-ORDER-IN               
071300     END-IF.                                                              
071400     MOVE MFS-ALFA-FAELT-RAETT     TO                                     
071500                            MOD-TEARTNOT-ORDER-IN-ATTR                    
071600                                                                          
071700     .                                                                    
071800     EJECT                                                                
071900                                                                          
072000                                                                          
072100 F-LAES-VISA-INFO SECTION.                                                
072200                                                                          
072300     PERFORM FA-LAES-GRUNDDATA                                            
072400                                                                          
072500     IF SEGMENT-SAKNAS                                                    
072600        MOVE PART-MISSING TO MED-IDMFSFEL                                 
072700        MOVE 'GB '        TO MED-IDSKYLT                                  
072800        CALL WMEDKONV USING MED-WMEDAREA                                  
072900        MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                 
073000        PERFORM MFS-RENSA-FAELT-UT                                        
073100     ELSE                                                                 
073200        IF ART-KDERS-UTG > +0                                             
073300           MOVE PART-SUPERSEDED TO MED-IDMFSFEL                           
073400           MOVE 'GB '         TO MED-IDSKYLT                              
073500           CALL WMEDKONV USING MED-WMEDAREA                               
073600           MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                              
073700           PERFORM MFS-RENSA-FAELT-UT                                     
073800        ELSE                                                              
073900           PERFORM FB-VISA                                                
074000        END-IF                                                            
074100     END-IF                                                               
074200     .                                                                    
074300     EJECT                                                                
074400                                                                          
074500                                                                          
074600 FA-LAES-GRUNDDATA SECTION.                                               
074700                                                                          
074800     PERFORM IMS-GU-ARTC01                                                
074900     .                                                                    
075000     EJECT                                                                
075100                                                                          
075200                                                                          
075300 FB-VISA SECTION.                                                         
075400                                                                          
075500     MOVE MFS-RENSA-FAELT TO MOD-IDLEVNR-SLAG.                            
075600                                                                          
075700     PERFORM IMS-GU-ARTS01                                                
075800     IF SEGMENT-FINNS                                                     
075900        PERFORM IMS-GNP-ARTS11-DC                                         
076000        IF SEGMENT-FINNS                                                  
076100           IF SLAG-IDDC-REF = SPACE AND NDC-CN                            
076200                MOVE ERR-NOT-REFILL-PART TO MED-IDMFSFEL                  
076300                MOVE 'GB ' TO MED-IDSKYLT                                 
076400                CALL WMEDKONV USING MED-WMEDAREA                          
076500                MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                         
076600                PERFORM MFS-RENSA-FAELT-IN                                
076700                PERFORM MFS-RENSA-FAELT-UT                                
076800           ELSE                                                           
076900             PERFORM UNTIL SEGMENT-SAKNAS                                 
077000                MOVE SLAG-IDDC TO WS-IDDC                                 
077100*                                 W-IDDC                                  
077200                IF NDC                                                    
077300                AND (WS-IDDC = W-IDDC)                                    
077400                   MOVE SLAG-IDLEVNR TO MOD-IDLEVNR-SLAG                  
077500                   MOVE SLAG-IDPERSON-BUY                                 
077600                                      TO MOD-IDPERSON-BUY                 
077700                   IF SLAG-FLORDSP = JA                                   
077800                      MOVE YES         TO MOD-FLORDSP                     
077900                   ELSE                                                   
078000                      MOVE SLAG-FLORDSP TO MOD-FLORDSP                    
078100                   END-IF                                                 
078200                   IF SLAG-FLSPBULK = JA                                  
078300                      MOVE YES         TO MOD-FLSPBULK                    
078400                   ELSE                                                   
078500                      MOVE SLAG-FLSPBULK TO MOD-FLSPBULK                  
078600                   END-IF                                                 
078700                   MOVE SLAG-KVDAGAR-MANLT                                
078800                                      TO MOD-KVDAGAR-MANLT                
078900                   PERFORM IMS-GU-WDK721                                  
079000                   IF SEGMENT-FINNS                                       
079100                       MOVE SBLK-TEARTNOT-ORDER                           
079200                                     TO MOD-TEARTNOT-ORDER-IN             
079300                       MOVE SBLK-IDUSER-ORDSP                             
079400                                     TO MOD-IDUSER-ORDSP                  
079500                       MOVE SBLK-IDUSER-SPBULK                            
079600                                     TO MOD-IDUSER-SPBULK                 
079700                       IF SBLK-DAORDSP > ZERO                             
079800                         MOVE SBLK-DAORDSP                                
079900                                      TO MOD-DAORDSP                      
080000                       ELSE                                               
080100                         MOVE MFS-RENSA-FAELT                             
080200                                      TO MOD-DAORDSP                      
080300                       END-IF                                             
080400                       IF SBLK-DASPBULK > ZERO                            
080500                         MOVE SBLK-DASPBULK                               
080600                                      TO MOD-DASPBULK                     
080700                       ELSE                                               
080800                         MOVE MFS-RENSA-FAELT                             
080900                                      TO MOD-DASPBULK                     
081000                       END-IF                                             
081100                   ELSE                                                   
081200                     MOVE MFS-RENSA-FAELT                                 
081300                                     TO MOD-TEARTNOT-ORDER-IN             
081400                                         MOD-DAORDSP                      
081500                                         MOD-DASPBULK                     
081600                                         MOD-IDUSER-ORDSP                 
081700                                         MOD-IDUSER-SPBULK                
081800                   END-IF                                                 
081900                   PERFORM IMS-GU-WDK722                                  
082000                   IF SEGMENT-FINNS                                       
082100                     MOVE XLAG-IDLEVNR-FRAM                               
082200                                 TO MOD-IDLEVNR-XLAG                      
082300                     MOVE XLAG-TILEVDAT                                   
082400                                 TO MOD-TILEVDAT-XLAG                     
082500                   ELSE                                                   
082600                      MOVE MFS-RENSA-FAELT                                
082700                                     TO MOD-IDLEVNR-XLAG                  
082800                                        MOD-TILEVDAT-XLAG                 
082900                   END-IF                                                 
083000                                                                          
083100                   MOVE WS-IDLAND-SPAR  TO W-IDLAND                       
083200                   PERFORM IMS-GU-WDK712                                  
083300                   IF SEGMENT-FINNS                                       
083400                    IF LART-FLREFERAL = JA                                
083500                      MOVE YES             TO MOD-FLREFERAL               
083600                    ELSE                                                  
083700                      MOVE LART-FLREFERAL  TO MOD-FLREFERAL               
083800                    END-IF                                                
083900                    MOVE LART-IDLANDX2 TO W-IDLANDX2                      
084000                    PERFORM IMS-GU-WDGX2510                               
084100                    IF SEGMENT-FINNS                                      
084200                      MOVE 2510-IDUSER     TO MOD-IDUSER-REFERAL          
084300                      MOVE 2510-TIREGDAT   TO MOD-TIREGDAT-REFERAL        
084400                    ELSE                                                  
084500                      MOVE MFS-RENSA-FAELT TO MOD-IDUSER-REFERAL          
084600                                              MOD-TIREGDAT-REFERAL        
084700                    END-IF                                                
084800                   ELSE                                                   
084900                      MOVE MFS-RENSA-FAELT TO MOD-FLREFERAL               
085000                                              MOD-IDUSER-REFERAL          
085100                                              MOD-TIREGDAT-REFERAL        
085200                    MOVE MFS-STAENG-FAELT-NOMOD                           
085300                                          TO MOD-FLREFERAL-IN-ATTR        
085400                   END-IF                                                 
085500                END-IF                                                    
085600                PERFORM IMS-GNP-ARTS11-DC                                 
085700             END-PERFORM                                                  
085800          END-IF                                                          
085900        ELSE                                                              
086000           IF INDATA-FEL                                                  
086100              CONTINUE                                                    
086200           ELSE                                                           
086300              MOVE MFS-RENSA-FAELT TO MOD-TEMFSINF                        
086400           END-IF                                                         
086500        END-IF                                                            
086600     ELSE                                                                 
086700        IF INDATA-FEL                                                     
086800           CONTINUE                                                       
086900        ELSE                                                              
087000           MOVE MFS-RENSA-FAELT  TO MOD-TEMFSINF                          
087100        END-IF                                                            
087200     END-IF                                                               
087300                                                                          
087400     IF MOD-IDLEVNR-SLAG = MFS-RENSA-FAELT                                
087500        MOVE MFS-STAENG-FAELT-NOMOD TO                                    
087600                        MOD-IDLEVNR-SLAG-IN-ATTR                          
087700                        MOD-FLORDSP-IN-ATTR                               
087800                        MOD-FLSPBULK-IN-ATTR                              
087900                        MOD-KVDAGAR-MANLT-IN-ATTR                         
088000                        MOD-IDPERSON-BUY-IN-ATTR                          
088100                        MOD-TEARTNOT-ORDER-IN-ATTR                        
088200                        MOD-FLREFERAL-IN-ATTR                             
088300     END-IF                                                               
088400                                                                          
088500     PERFORM IMS-GU-BENA01-BSEQ                                           
088600     IF SEGMENT-FINNS                                                     
088700       PERFORM IMS-GNP-BENA11                                             
088800       IF SEGMENT-FINNS                                                   
088900         MOVE BENA-TEXT-BEART TO MOD-BEART-ENG                            
089000       ELSE                                                               
089100         MOVE MFS-RENSA-FAELT TO MOD-BEART-ENG                            
089200       END-IF                                                             
089300     ELSE                                                                 
089400       MOVE MFS-RENSA-FAELT   TO MOD-BEART-ENG                            
089500     END-IF                                                               
089600     .                                                                    
089700     EJECT                                                                
089800                                                                          
089900                                                                          
090000 G-KOLLA-INPUT SECTION.                                                   
090100                                                                          
090200     MOVE WS-IDDC-SPAR          TO WS-IDDC                                
090300     IF NDC-NA                                                            
090400        MOVE  NEJ               TO INDATA-SW                              
090500        MOVE UPDATE-NOT-ALLOWED TO MED-IDMFSFEL                           
090600        MOVE 'GB '              TO MED-IDSKYLT                            
090700        CALL WMEDKONV        USING MED-WMEDAREA                           
090800        MOVE MED-TEMFSFEL       TO MOD-TEMFSFEL                           
090900                                                                          
091000     ELSE                                                                 
091100        MOVE JA                 TO INDATA-SW                              
091200        IF MID-INPUT = ALL '+'                                            
091300          MOVE ERR-PF11-AND-NO-DATA                                       
091400                                TO MED-IDMFSFEL                           
091500          MOVE 'GB '            TO MED-IDSKYLT                            
091600          CALL WMEDKONV      USING MED-WMEDAREA                           
091700          MOVE MED-TEMFSFEL     TO MOD-TEMFSFEL                           
091800          PERFORM MFS-ROER-EJ-FAELT-IN                                    
091900          PERFORM MFS-ROER-EJ-FAELT-UT                                    
092000          MOVE NEJ              TO INDATA-SW                              
092100        ELSE                                                              
092200          PERFORM GA-KOLLA-INPUT-1                                        
092300          IF INDATA-FEL                                                   
092400            MOVE ERR-CORR-HILITE-FLDS                                     
092500                                TO MED-IDMFSFEL                           
092600            MOVE 'GB '          TO MED-IDSKYLT                            
092700            CALL WMEDKONV    USING MED-WMEDAREA                           
092800            MOVE MED-MFSFEL     TO MOD-TEMFSFEL                           
092900            PERFORM MFS-ROER-EJ-FAELT-UT                                  
093000            PERFORM MFS-ROER-EJ-FAELT-IN                                  
093100          ELSE                                                            
093200            PERFORM GB-KOLLA-INPUT-2                                      
093300            IF INDATA-FEL                                                 
093400               PERFORM MFS-ROER-EJ-FAELT-UT                               
093500               PERFORM MFS-ROER-EJ-FAELT-IN                               
093600            END-IF                                                        
093700          END-IF                                                          
093800        END-IF                                                            
093900     END-IF                                                               
094000     .                                                                    
094100     EJECT                                                                
094200                                                                          
094300 GA-KOLLA-INPUT-1 SECTION.                                                
094400                                                                          
094500                                                                          
094600*IDLEVNR-SLAG                                                             
094700     IF MID-IDLEVNR-SLAG NOT = ALL '+'                                    
094800        MOVE MFS-ALFA-FAELT-RAETT TO                                      
094900                   MOD-IDLEVNR-SLAG-IN-ATTR                               
095000     ELSE                                                                 
095100         MOVE MFS-ALFA-FAELT-RAETT TO                                     
095200                         MOD-IDLEVNR-SLAG-IN-ATTR                         
095300     END-IF.                                                              
095400                                                                          
095500                                                                          
095600*FLORDSP                                                                  
095700     IF MID-FLORDSP NOT = ALL '+'                                         
095800        IF MID-FLORDSP = JA OR YES OR NEJ                                 
095900           MOVE MFS-ALFA-FAELT-RAETT TO                                   
096000                              MOD-FLORDSP-IN-ATTR                         
096100        ELSE                                                              
096200           MOVE MFS-ALFA-FAELT-FEL      TO                                
096300                              MOD-FLORDSP-IN-ATTR                         
096400           MOVE NEJ TO INDATA-SW                                          
096500        END-IF                                                            
096600     ELSE                                                                 
096700         MOVE MFS-ALFA-FAELT-RAETT      TO                                
096800                              MOD-FLORDSP-IN-ATTR                         
096900     END-IF                                                               
097000                                                                          
097100                                                                          
097200*FLSPBULK                                                                 
097300     IF MID-FLSPBULK NOT = ALL '+'                                        
097400        IF MID-FLSPBULK = JA OR YES OR NEJ                                
097500           MOVE MFS-ALFA-FAELT-RAETT TO                                   
097600                              MOD-FLSPBULK-IN-ATTR                        
097700        ELSE                                                              
097800           MOVE MFS-ALFA-FAELT-FEL TO                                     
097900                              MOD-FLSPBULK-IN-ATTR                        
098000           MOVE NEJ TO INDATA-SW                                          
098100        END-IF                                                            
098200     ELSE                                                                 
098300         MOVE MFS-ALFA-FAELT-RAETT TO                                     
098400                              MOD-FLSPBULK-IN-ATTR                        
098500     END-IF                                                               
098600                                                                          
098700                                                                          
098800*KVDAGAR-MANLT                                                            
098900     IF MID-KVDAGAR-MANLT NOT = ALL '+'                                   
099000        PERFORM IMS-GU-ARTS01                                             
099100        PERFORM IMS-GNP-ARTS11-DC                                         
099200        IF SEGMENT-FINNS                                                  
099300          IF MID-KVDAGAR-MANLT NOT NUMERIC                                
099400          OR (SLAG-IDDC-REF = '11' OR SLAG-IDDC-REF(1:1) = '7')           
099500          OR SLAG-IDDC(1:1) = '7'                                         
099600             MOVE MFS-NUM-FAELT-FEL TO                                    
099700                               MOD-KVDAGAR-MANLT-IN-ATTR                  
099800             MOVE NEJ TO INDATA-SW                                        
099900          ELSE                                                            
100000             MOVE MFS-NUM-FAELT-RAETT TO                                  
100100                               MOD-KVDAGAR-MANLT-IN-ATTR                  
100200          END-IF                                                          
100300        ELSE                                                              
100400           MOVE MFS-NUM-FAELT-FEL TO                                      
100500                             MOD-KVDAGAR-MANLT-IN-ATTR                    
100600        END-IF                                                            
100700     ELSE                                                                 
100800         MOVE MFS-NUM-FAELT-RAETT TO                                      
100900                                 MOD-KVDAGAR-MANLT-IN-ATTR                
101000     END-IF                                                               
101100                                                                          
101200                                                                          
101300*IDPERSON-BUY                                                             
101400     IF MID-IDPERSON-BUY NOT = ALL '+'                                    
101500        IF MID-IDPERSON-BUY NOT NUMERIC                                   
101600           MOVE MFS-NUM-FAELT-FEL TO                                      
101700                              MOD-IDPERSON-BUY-IN-ATTR                    
101800           MOVE NEJ TO INDATA-SW                                          
101900        ELSE                                                              
102000           MOVE MFS-NUM-FAELT-RAETT TO                                    
102100                              MOD-IDPERSON-BUY-IN-ATTR                    
102200        END-IF                                                            
102300     ELSE                                                                 
102400         MOVE MFS-NUM-FAELT-RAETT TO                                      
102500                              MOD-IDPERSON-BUY-IN-ATTR                    
102600     END-IF                                                               
102700                                                                          
102800                                                                          
102900*FLREFERAL                                                                
103000     IF MID-FLREFERAL NOT = ALL '+'                                       
103100        IF MID-FLREFERAL = JA OR YES OR NEJ                               
103200           MOVE MFS-ALFA-FAELT-RAETT TO                                   
103300                              MOD-FLREFERAL-IN-ATTR                       
103400        ELSE                                                              
103500           MOVE MFS-ALFA-FAELT-FEL TO                                     
103600                              MOD-FLREFERAL-IN-ATTR                       
103700           MOVE NEJ TO INDATA-SW                                          
103800        END-IF                                                            
103900     ELSE                                                                 
104000         MOVE MFS-ALFA-FAELT-RAETT TO                                     
104100                              MOD-FLREFERAL-IN-ATTR                       
104200     END-IF                                                               
104300                                                                          
104400                                                                          
104500*TEARTNOT-ORDER                                                           
104600     IF MID-TEARTNOT-ORDER = ALL '+'                                      
104700        MOVE MFS-RENSA-FAELT                                              
104800                          TO MOD-TEARTNOT-ORDER-IN                        
104900     ELSE                                                                 
105000        MOVE MID-TEARTNOT-ORDER                                           
105100                          TO MOD-TEARTNOT-ORDER-IN                        
105200     END-IF                                                               
105300     MOVE MFS-ALFA-FAELT-RAETT                                            
105400                          TO MOD-TEARTNOT-ORDER-IN-ATTR                   
105500                                                                          
105600                                                                          
105700     .                                                                    
105800     EJECT                                                                
105900                                                                          
106000                                                                          
106100 GB-KOLLA-INPUT-2 SECTION.                                                
106200                                                                          
106300     PERFORM IMS-GU-ARTC01                                                
106400     IF SEGMENT-SAKNAS                                                    
106500        MOVE NEJ TO INDATA-SW                                             
106600        MOVE PART-MISSING  TO MED-IDMFSFEL                                
106700        MOVE 'GB '         TO MED-IDSKYLT                                 
106800        CALL WMEDKONV USING MED-WMEDAREA                                  
106900        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
107000     ELSE                                                                 
107100       IF ART-KDERS-UTG > ZERO                                            
107200          MOVE NEJ TO INDATA-SW                                           
107300          MOVE PART-SUPERSEDED TO MED-IDMFSFEL                            
107400          MOVE 'GB '       TO MED-IDSKYLT                                 
107500          CALL WMEDKONV USING MED-WMEDAREA                                
107600          MOVE MED-MFSFEL TO MOD-TEMFSFEL                                 
107700       ELSE                                                               
107800          PERFORM IMS-GNP-ARTC11                                          
107900          PERFORM IMS-GU-WDK711                                           
108000*IDLEVNR-SLAG                                                             
108100          IF MID-IDLEVNR-SLAG NOT = ALL '+'                               
108200            IF NDC                                                        
108300              PERFORM GB3-KOLLA-REFILLBAS                                 
108400            END-IF                                                        
108500            IF NDC-US                                                     
108600            AND (NDC-US-SE OR NDC-US-CH)                                  
108700                PERFORM GB1-NDC-PURCH-RULES                               
108800            END-IF                                                        
108900            IF INDATA-OK                                                  
109000              IF MID-IDLEVNR-SLAG = K7-SLAG-IDLEVNR                       
109100                 MOVE MFS-ALFA-FAELT-FEL TO                               
109200                                      MOD-IDLEVNR-SLAG-IN-ATTR            
109300                 MOVE NEJ                TO INDATA-SW                     
109400                 MOVE MED-13             TO MOD-TEMFSINF                  
109500              ELSE                                                        
109600                MOVE MID-IDLEVNR-SLAG TO W-IDLEVNRDC                      
109700                PERFORM IMS-GU-WDB601-LEVNR-DC                            
109800                IF SEGMENT-FINNS                                          
109900                  MOVE JA TO SW-REF-LEV                                   
110000                ELSE                                                      
110100                  PERFORM IMS-GU-LEVA01                                   
110200                  IF SEGMENT-FINNS                                        
110300                    MOVE NEJ TO SW-REF-LEV                                
110400                  ELSE                                                    
110500                    MOVE MFS-ALFA-FAELT-FEL TO                            
110600                                   MOD-IDLEVNR-SLAG-IN-ATTR               
110700                    MOVE NEJ              TO INDATA-SW                    
110800                    MOVE MED-8            TO MOD-TEMFSINF                 
110900                  END-IF                                                  
111000                END-IF                                                    
111100************                                                              
111200*               CHECKS CHANGE TO REFILL DC                                
111300                 IF INDATA-OK AND REFILLED                                
111400                   MOVE DCS-IDDC          TO WS-IDDC-REF                  
111500                   MOVE DCS-IDLANDX2      TO W-IDLAND-REF                 
111600*                    CHECK REFILLING DC REFILLED FROM DC,                 
111700*                    TO AVOID CIRCULAR REFILL FLOW                        
111800                     IF DCS-NDC                                           
111900                        MOVE DCS-IDDC             TO W-IDDC-K7            
112000                        PERFORM IMS-GU-WDK711-REF                         
112100                        IF SEGMENT-FINNS                                  
112200                           MOVE K7-SLAG-IDDC-REF                          
112300                                              TO REF-WS-IDDC              
112400                           IF ( REF-WS-IDDC = MSGI-IDDC-KEY)              
112500                              MOVE MFS-ALFA-FAELT-FEL TO                  
112600                                   MOD-IDLEVNR-SLAG-IN-ATTR               
112700                              MOVE NEJ            TO INDATA-SW            
112800                              MOVE MED-16         TO MOD-TEMFSINF         
112900                           END-IF                                         
113000                        ELSE                                              
113100                           MOVE MFS-ALFA-FAELT-FEL TO                     
113200                                   MOD-IDLEVNR-SLAG-IN-ATTR               
113300                           MOVE NEJ               TO INDATA-SW            
113400                           MOVE MED-16            TO MOD-TEMFSINF         
113500                        END-IF                                            
113600                     END-IF                                               
113700                   IF DCS-IDDC = MSGI-IDDC-KEY                            
113800                      MOVE MFS-ALFA-FAELT-FEL TO                          
113900                               MOD-IDLEVNR-SLAG-IN-ATTR                   
114000                      MOVE NEJ         TO INDATA-SW                       
114100                      MOVE MED-9       TO MOD-TEMFSINF                    
114200                   ELSE                                                   
114300                      MOVE MSGI-IDDC-KEY      TO W-IDDC                   
114400                      MOVE '4408'             TO W-IDTRANS-B6             
114500                      MOVE DCS-IDDC           TO W-IDDC-REF-B6            
114600                      PERFORM IMS-GU-WDB615                               
114700                      IF SEGMENT-FINNS                                    
114800                        IF DCS-IDDC                 = WC-CDC-SE           
114900*------------ DISTRICT DETAILS FOR CDC ARE STORED ON WDB601               
115000                         MOVE MSGI-IDDC-KEY            TO W-IDDC          
115100                         PERFORM IMS-GU-WDB601                            
115200                         IF      SEGMENT-FINNS                            
115300                         AND DCS-IDDISTR-REFILL > ZERO                    
115400                           MOVE MFS-ALFA-FAELT-RAETT                      
115500                                   TO MOD-IDLEVNR-SLAG-IN-ATTR            
115600                         ELSE                                             
115700                           MOVE MFS-ALFA-FAELT-FEL                        
115800                                   TO MOD-IDLEVNR-SLAG-IN-ATTR            
115900                           MOVE NEJ         TO INDATA-SW                  
116000                           MOVE MED-19      TO MOD-TEMFSINF               
116100                         END-IF                                           
116200                        ELSE                                              
116300*------------ DISTRICT DETAILS FOR OTHER DCS ARE STORED ON WDB616         
116400                         MOVE DCS-IDDC          TO W-IDDC-REF             
116500                         PERFORM IMS-GU-WDB616                            
116600                         IF      SEGMENT-FINNS                            
116700                         AND REF-IDDISTR-REFILL > ZERO                    
116800                           MOVE MFS-ALFA-FAELT-RAETT                      
116900                                   TO MOD-IDLEVNR-SLAG-IN-ATTR            
117000                         ELSE                                             
117100                           MOVE MFS-ALFA-FAELT-FEL                        
117200                                   TO MOD-IDLEVNR-SLAG-IN-ATTR            
117300                           MOVE NEJ         TO INDATA-SW                  
117400                           MOVE MED-19      TO MOD-TEMFSINF               
117500                         END-IF                                           
117600                        END-IF                                            
117700                      ELSE                                                
117800                        MOVE MFS-ALFA-FAELT-FEL                           
117900                                   TO MOD-IDLEVNR-SLAG-IN-ATTR            
118000                        MOVE NEJ            TO INDATA-SW                  
118100                        MOVE MED-19         TO MOD-TEMFSINF               
118200                      END-IF                                              
118300                                                                          
118400                   END-IF                                                 
118500                 ELSE                                                     
118600********LOCAL PURCHASING                                                  
118610                   IF NDC-CN                                              
118611                     MOVE MFS-ALFA-FAELT-FEL TO                           
118612                          MOD-IDLEVNR-SLAG-IN-ATTR                        
118613                     MOVE NEJ          TO INDATA-SW                       
118614                     MOVE MED-16       TO MOD-TEMFSINF                    
118620                   ELSE                                                   
118700                     MOVE LEV-IDLEVNR  TO WS-IDLEVNR-NUM                  
118800                     INSPECT WS-IDLEVNR-NUM REPLACING                     
118900                                            ALL SPACE BY ZERO             
119000                     IF WS-IDLEVNR-NUM NUMERIC                            
119100                       IF LEV-IDLEVNR-MOTSV NOT = SPACE                   
119200                         MOVE MFS-ALFA-FAELT-FEL TO                       
119300                              MOD-IDLEVNR-SLAG-IN-ATTR                    
119400                         MOVE NEJ      TO INDATA-SW                       
119500                         MOVE FEL-125  TO MOD-TEMFSINF                    
119600                       ELSE                                               
119700                         MOVE MFS-ALFA-FAELT-RAETT TO                     
119800                              MOD-IDLEVNR-SLAG-IN-ATTR                    
119900                       END-IF                                             
120000                     ELSE                                                 
120100***                     IDLEVNR ÄR ALFA                                   
120200                         MOVE MFS-ALFA-FAELT-RAETT TO                     
120300                              MOD-IDLEVNR-SLAG-IN-ATTR                    
120400                     END-IF                                               
120410                   END-IF                                                 
120500                 END-IF                                                   
120600*               END-IF                                                    
120700              END-IF                                                      
120800*          IF FUTURE FORECAST EXISTS SUPPLIER CHANGE NOT ALLOWED          
120900              PERFORM IMS-GNP-WDK727                                      
121000              IF SEGMENT-FINNS                                            
121100                 MOVE MFS-ALFA-FAELT-FEL                                  
121200                                  TO MOD-IDLEVNR-SLAG-IN-ATTR             
121300                 MOVE NEJ         TO INDATA-SW                            
121400                 MOVE MED-18      TO MOD-TEMFSINF                         
121500              ELSE                                                        
121600                 MOVE MFS-ALFA-FAELT-RAETT                                
121700                                  TO MOD-IDLEVNR-SLAG-IN-ATTR             
121800                 MOVE MID-IDLEVNR-SLAG                                    
121900                                  TO W-IDLEVNR                            
122000              END-IF                                                      
122100*                                                                         
122200              IF INDATA-SW = JA                                           
122300                MOVE ART-KDPRODSL TO TEST-KDPRODSL                        
122400                IF KDPRODSL-LOCAL                                         
122500                   IF MID-IDLEVNR-SLAG = '1441' OR 'BP2TW'                
122600                      MOVE MFS-ALFA-FAELT-FEL TO                          
122700                                 MOD-IDLEVNR-SLAG-IN-ATTR                 
122800                      MOVE NEJ TO INDATA-SW                               
122900                      MOVE MED-6 TO MOD-TEMFSINF                          
123000                   ELSE                                                   
123100                      MOVE MFS-ALFA-FAELT-RAETT TO                        
123200                                 MOD-IDLEVNR-SLAG-IN-ATTR                 
123300                      MOVE MID-IDLEVNR-SLAG TO W-IDLEVNR                  
123400                   END-IF                                                 
123500                END-IF                                                    
123600*                                                                         
123700                                                                          
123800                MOVE MID-IDLEVNR-SLAG TO W-IDLEVNR                        
123900                PERFORM IMS-GU-LEVA01                                     
124000                IF SEGMENT-FINNS                                          
124100                   MOVE MFS-ALFA-FAELT-RAETT TO                           
124200                              MOD-IDLEVNR-SLAG-IN-ATTR                    
124300                ELSE                                                      
124400                   MOVE MFS-ALFA-FAELT-FEL TO                             
124500                              MOD-IDLEVNR-SLAG-IN-ATTR                    
124600                   MOVE NEJ TO INDATA-SW                                  
124700                   MOVE MED-7 TO MOD-TEMFSINF                             
124800                END-IF                                                    
124900              END-IF                                                      
125000            END-IF                                                        
125100          END-IF                                                          
125200          IF INDATA-OK                                                    
125300             IF MID-IDPERSON-BUY             NOT = ALL '+'                
125400                IF K7-SLAG-FLBUYUPD = 'J'                                 
125500                  MOVE MFS-ALFA-FAELT-FEL                                 
125600                            TO MOD-IDPERSON-BUY-IN-ATTR                   
125700                  MOVE NEJ        TO INDATA-SW                            
125800                  MOVE MED-17 TO MOD-TEMFSINF                             
125900                ELSE                                                      
126000                  MOVE MFS-ALFA-FAELT-RAETT                               
126100                           TO MOD-IDPERSON-BUY-IN-ATTR                    
126200                END-IF                                                    
126300             END-IF                                                       
126400          END-IF                                                          
126500       END-IF                                                             
126600     END-IF                                                               
126700                                                                          
126800     .                                                                    
126900     EJECT                                                                
127000 GB1-NDC-PURCH-RULES  SECTION.                                            
127100                                                                          
127200     MOVE NEJ                    TO SUPPLIER-SW                           
127300                                                                          
127400     PERFORM IMS-GN-WDB601                                                
127500     PERFORM UNTIL SEGMENT-SLUT                                           
127600        IF DCS-CDC                                                        
127700*       OR (DCS-NDC-NA AND DCS-USA)                                       
127800           IF  DCS-IDLEVNR-DC     = MID-IDLEVNR-SLAG                      
127900               MOVE JA           TO SUPPLIER-SW                           
128000           END-IF                                                         
128100        END-IF                                                            
128200        PERFORM IMS-GN-WDB601                                             
128300     END-PERFORM                                                          
128400                                                                          
128500     IF SUPPLIER-NEJ                                                      
128600*    OR MID-IDLEVNR-SLAG     NOT  > SPACES                                
128700        MOVE MFS-ALFA-FAELT-FEL  TO MOD-IDLEVNR-SLAG-IN-ATTR              
128800        MOVE NEJ                 TO INDATA-SW                             
128900        MOVE MED-15              TO MOD-TEMFSINF                          
129000     END-IF                                                               
129100     .                                                                    
129200     EJECT                                                                
129300 GB3-KOLLA-REFILLBAS SECTION.                                             
129400                                                                          
129500     MOVE W-IDDC                 TO W-IDDC-MIN-E3                         
129600                                    W-IDDC-MAX-E3                         
129700                                                                          
129800     PERFORM IMS-GU-WDE301                                                
129900     IF SEGMENT-FINNS                                                     
130000       MOVE MFS-ALFA-FAELT-FEL TO MOD-IDLEVNR-SLAG-IN-ATTR                
130100       MOVE NEJ TO INDATA-SW                                              
130200       MOVE MED-14 TO MOD-TEMFSINF                                        
130300     END-IF                                                               
130400     .                                                                    
130500     EJECT                                                                
130600 H-UPPDATERA SECTION.                                                     
130700                                                                          
130800     IF MID-IDLEVNR-SLAG     NOT = ALL '+'                                
130900     OR MID-FLORDSP          NOT = ALL '+'                                
131000     OR MID-FLSPBULK         NOT = ALL '+'                                
131100     OR MID-KVDAGAR-MANLT    NOT = ALL '+'                                
131200     OR MID-IDPERSON-BUY     NOT = ALL '+'                                
131300     OR MID-TEARTNOT-ORDER   NOT = ALL '+'                                
131400     OR MID-FLREFERAL        NOT = ALL '+'                                
131500        PERFORM HA-UPPDATERA                                              
131600     END-IF                                                               
131700                                                                          
131800     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
131900     MOVE 'GB '         TO MED-IDSKYLT                                    
132000     CALL WMEDKONV USING MED-WMEDAREA                                     
132100     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
132200     PERFORM MFS-FORM-ATTR                                                
132300     PERFORM MFS-RENSA-FAELT-IN                                           
132400     .                                                                    
132500     EJECT                                                                
132600                                                                          
132700 HA-UPPDATERA SECTION.                                                    
132800                                                                          
132900     INITIALIZE  REFL-W271REFL                                            
133000     PERFORM IMS-GU-ARTS01                                                
133100     IF MID-IDLEVNR-SLAG NOT = ALL '+'                                    
133200     OR MID-FLORDSP NOT = ALL '+'                                         
133300     OR MID-FLSPBULK NOT = ALL '+'                                        
133400     OR MID-KVDAGAR-MANLT NOT = ALL '+'                                   
133500     OR MID-IDPERSON-BUY NOT = ALL '+'                                    
133600     OR MID-TEARTNOT-ORDER NOT = ALL '+'                                  
133700        PERFORM IMS-GHNP-ARTS11                                           
133800        PERFORM IMS-GHU-WDK721                                            
133900        IF SEGMENT-SAKNAS                                                 
134000          MOVE ALL '+'      TO WDK7-W005WDK7                              
134100          MOVE 'WDK721'     TO WDK7-IDSEGM                                
134200          MOVE W-IDARTNR    TO WDK7-IDARTNR-KFB                           
134300          MOVE W-IDDC       TO WDK7-IDDC-KFB                              
134400                                                                          
134500          CALL W005WDK7 USING WDK7-W005WDK7 WDB6-PCB                      
134600                                            ARTC-PCB WDK7-PCB             
134700          PERFORM IMS-GHU-WDK721                                          
134800        END-IF                                                            
134900        MOVE NEJ TO SW-AENDRA-WDK721                                      
135000        IF MID-IDLEVNR-SLAG NOT = ALL '+'                                 
135100           MOVE MID-IDLEVNR-SLAG  TO SLAG-IDLEVNR                         
135200           IF MID-IDLEVNR-SLAG = '1441'                                   
135300             MOVE '11'            TO SLAG-IDDC-REF                        
135400           ELSE                                                           
135500             IF REFILLED                                                  
135600               MOVE WS-IDDC-REF   TO SLAG-IDDC-REF                        
135700                                      W-IDDC-K7                           
135800               PERFORM IMS-GU-WDK711-REF                                  
135900               IF SEGMENT-SAKNAS                                          
136000                 MOVE ALL '+'     TO WDK7-W005WDK7                        
136100                 MOVE 'WDK711'    TO WDK7-IDSEGM                          
136200                 MOVE W-IDARTNR   TO WDK7-IDARTNR-KFB                     
136300                 MOVE W-IDDC-K7   TO WDK7-IDDC-KFB                        
136400                                    WDK7-IDDC                             
136500                 CALL W005WDK7 USING WDK7-W005WDK7 WDB6-PCB               
136600                                     ARTC-PCB WDK72-PCB                   
136700               END-IF                                                     
136800             ELSE                                                         
136900               MOVE SPACE         TO SLAG-IDDC-REF                        
137000             END-IF                                                       
137100           END-IF                                                         
137200        END-IF                                                            
137300        IF MID-IDPERSON-BUY NOT = ALL '+'                                 
137400           MOVE MID-IDPERSON-BUY    TO WS-IDPERSON-BUY                    
137500* --        NO UPDATE ALLOWED FOR NDC-NA                                  
137600           IF NDC-NA                                                      
137700              CONTINUE                                                    
137800           ELSE                                                           
137900              MOVE WS-IDPERSON-BUY  TO SLAG-IDPERSON-BUY                  
138000              IF WS-IDPERSON-BUY NOT = ZERO                               
138100                MOVE 'J'            TO SLAG-FLBUYUPD                      
138200              END-IF                                                      
138300           END-IF                                                         
138400        END-IF                                                            
138500        IF MID-TEARTNOT-ORDER NOT = ALL '+'                               
138600              MOVE JA           TO SW-AENDRA-WDK721                       
138700              MOVE MID-TEARTNOT-ORDER TO SBLK-TEARTNOT-ORDER              
138800        END-IF                                                            
138900                                                                          
139000* --       ÅTERSTÄLL RÖRELSEINDIKATOR (FLREFNYO) PÅ ORDERINGÅNGS-         
139100* --       REGISTRET WDL7 VID UPPDATERING AV BLOCKKOD.                    
139200        IF MID-FLORDSP NOT = ALL '+'                                      
139300        OR MID-FLSPBULK NOT = ALL '+'                                     
139400            MOVE NEJ                TO SLAG-FLREFNYO                      
139500        END-IF                                                            
139600                                                                          
139700        IF MID-FLORDSP NOT = ALL '+'                                      
139800           IF MID-FLORDSP NOT = SLAG-FLORDSP                              
139900             MOVE DAGENS-DATUM      TO SBLK-DAORDSP                       
140000             MOVE MSGI-IDUSER       TO SBLK-IDUSER-ORDSP                  
140100             MOVE JA TO SW-AENDRA-WDK721                                  
140200           END-IF                                                         
140300           IF MID-FLORDSP = YES                                           
140400              MOVE JA                 TO SLAG-FLORDSP                     
140500           ELSE                                                           
140600              MOVE MID-FLORDSP TO SLAG-FLORDSP                            
140700           END-IF                                                         
140800        END-IF                                                            
140900        IF MID-FLSPBULK NOT = ALL '+'                                     
141000           IF MID-FLSPBULK NOT = SLAG-FLSPBULK                            
141100             MOVE DAGENS-DATUM      TO SBLK-DASPBULK                      
141200             MOVE MSGI-IDUSER       TO SBLK-IDUSER-SPBULK                 
141300             MOVE JA TO SW-AENDRA-WDK721                                  
141400           END-IF                                                         
141500           IF MID-FLSPBULK = YES                                          
141600              MOVE JA                  TO SLAG-FLSPBULK                   
141700           ELSE                                                           
141800              MOVE MID-FLSPBULK TO SLAG-FLSPBULK                          
141900           END-IF                                                         
142000        END-IF                                                            
142100        IF MID-KVDAGAR-MANLT NOT = ALL '+'                                
142200           MOVE MID-KVDAGAR-MANLT                                         
142300                              TO SLAG-KVDAGAR-MANLT                       
142400                                 REFL-NDC-KVDAGAR-TBT-DC                  
142500           MOVE W-IDDC           TO REFL-IDDC                             
142600           MOVE W-IDARTNR        TO REFL-IDARTNR                          
142700           MOVE SLAG-IDDC-REF    TO REFL-IDDC-REF                         
142800           MOVE SLAG-IDREFTAB TO REFL-IDREFTAB                            
142900           MOVE SLAG-FLWILSON TO REFL-FLWILSON                            
143000                                                                          
143100           PERFORM HAA-GET-BESPRIS                                        
143200           MOVE WS-PRARTBES                                               
143300                                    TO REFL-PRARTBES                      
143400           MOVE 1                   TO IX                                 
143500           PERFORM UNTIL IX > 12                                          
143600             MOVE SLAG-RESEASON(IX) TO REFL-RESEASON(IX)                  
143700             ADD 1 TO IX                                                  
143800           END-PERFORM                                                    
143900                                                                          
144000           MOVE SLAG-IDLEVNR        TO REFL-IN-IDLEVNR-DC                 
144100                                                                          
144200           MOVE SLAG-TIREFPKT       TO TMP1-YYMMDD                        
144300           MOVE DAGENS-DATUM        TO TMP2-YYMMDD                        
144400           PERFORM WY2000P1                                               
144500                                                                          
144600           IF TMP1-YYMMDD >= TMP2-YYMMDD                                  
144700             MOVE SLAG-KVREFPKT     TO REFL-IN-KVREFPKT                   
144800           ELSE                                                           
144900             MOVE ZERO              TO REFL-IN-KVREFPKT                   
145000           END-IF                                                         
145100                                                                          
145200           MOVE SLAG-TIREFPAF       TO TMP1-YYMMDD                        
145300           MOVE DAGENS-DATUM        TO TMP2-YYMMDD                        
145400           PERFORM WY2000P1                                               
145500                                                                          
145600           IF TMP1-YYMMDD >= TMP2-YYMMDD                                  
145700             MOVE SLAG-KVREFBER     TO REFL-IN-KVREFBER                   
145800           ELSE                                                           
145900             MOVE ZERO              TO REFL-IN-KVREFBER                   
146000           END-IF                                                         
146100                                                                          
146200           INITIALIZE UTUP-W271UTUP                                       
146300           MOVE 004             TO UTUP-KDCALL                            
146400           MOVE W-IDARTNR       TO UTUP-IDARTNR                           
146500           MOVE W-IDDC          TO UTUP-IDDC                              
146600           MOVE SLAG-IDDC-REF   TO UTUP-IDDC-REF                          
146700           MOVE REFL-NDC-KVDAGAR-TBT-DC                                   
146800                                TO UTUP-LEADTIME                          
146900                                                                          
147000           CALL W271UTUP USING UTUP-W271UTUP                              
147100                               UTUP1-WDK7-PCB                             
147200                               UTUP1-WDB6-PCB                             
147300                               UTUP1-UTIL-WDK6-PCB                        
147400                               UTUP1-UTIL-WDK7-PCB                        
147500                               UTUP1-UTIL-WDB6-PCB                        
147600           IF UTUP-KDSVAR-OK                                              
147700              MOVE UTUP-LEADTID-BEHOV TO REFL-IN-LEADTID-BEHOV            
147800           ELSE                                                           
147900              DISPLAY 'W271UTUP-ERROR :' UTUP-TEXT                        
148000              CALL FELLOG                                                 
148100           END-IF                                                         
148200                                                                          
148300           CALL W271REFL USING REFL-W271REFL REFL-2501-PCB                
148400                               WDB6-PCB                                   
148500                               WDK7-22-PCB                                
148600                               UTIL-WDK6-PCB                              
148700                               UTIL-WDK7-PCB                              
148800                               UTIL-WDB6-PCB                              
148900                                                                          
149000           MOVE SLAG-TIREFPKT       TO TMP1-YYMMDD                        
149100           MOVE DAGENS-DATUM        TO TMP2-YYMMDD                        
149200           PERFORM WY2000P1                                               
149300           IF TMP1-YYMMDD >= TMP2-YYMMDD                                  
149400*                                                                         
149500*--- INGEN UPPDATERING AV KVREFPKT PGA MANUELLT DATUM ÄR SATT             
149600             CONTINUE                                                     
149700           ELSE                                                           
149800             MOVE REFL-KVREFPKT     TO SLAG-KVREFPKT                      
149900           END-IF                                                         
150000                                                                          
150100           MOVE SLAG-TIREFPAF       TO TMP1-YYMMDD                        
150200           MOVE DAGENS-DATUM        TO TMP2-YYMMDD                        
150300           PERFORM WY2000P1                                               
150400           IF TMP1-YYMMDD >= TMP2-YYMMDD                                  
150500*                                                                         
150600*--- INGEN UPPDATERING AV KVREFBER PGA MANUELLT DATUM ÄR SATT             
150700             CONTINUE                                                     
150800           ELSE                                                           
150900             MOVE REFL-KVREFBER     TO SLAG-KVREFBER                      
151000           END-IF                                                         
151100        END-IF                                                            
151200        PERFORM IMS-REPL-ARTS11                                           
151300        IF SW-AENDRA-WDK721 = JA                                          
151400          PERFORM IMS-REPL-WDK721                                         
151500        END-IF                                                            
151600     END-IF                                                               
151700                                                                          
151800*    UPDATE VOLUME, WEIGHT AND COE WHEN CN REFILLED FROM US OR SE         
151900*    GLOBAL EXPORTS                                                       
152000     IF MID-IDLEVNR-SLAG         NOT = ALL '+'                            
152100        IF DCS-NDC-CN                                                     
152200           IF IDLAND-SE                                                   
152300              IF CLAG-KDARTURS    > SPACE                                 
152400                 MOVE CLAG-KDARTURS    TO WS-KDARTURS                     
152500                 MOVE JA               TO UPD-WDK712-SW                   
152600              END-IF                                                      
152700              IF CLAG-VKART       > ZERO                                  
152800                 MOVE CLAG-VKART       TO WS-VKART                        
152900                 MOVE JA               TO UPD-WDK712-SW                   
153000              END-IF                                                      
153100              IF CLAG-VLARTNTO    > ZERO                                  
153200                 MOVE CLAG-VLARTNTO    TO WS-VLARTNTO                     
153300                 MOVE JA               TO UPD-WDK712-SW                   
153400              END-IF                                                      
153500           ELSE                                                           
153600             IF IDLAND-US                                                 
153700                MOVE W-IDLAND-REF        TO W-IDLAND                      
153800                PERFORM IMS-GU-WDK712                                     
153900                IF SEGMENT-FINNS                                          
154000                   IF LART-KDARTURS > SPACE                               
154100                      MOVE LART-KDARTURS TO WS-KDARTURS                   
154200                      MOVE JA            TO UPD-WDK712-SW                 
154300                   END-IF                                                 
154400                   IF LART-VKART  >   ZERO                                
154500                      MOVE LART-VKART    TO WS-VKART                      
154600                      MOVE JA            TO UPD-WDK712-SW                 
154700                   END-IF                                                 
154800                   IF LART-VLARTNTO   > ZERO                              
154900                      MOVE LART-VLARTNTO TO WS-VLARTNTO                   
155000                      MOVE JA            TO UPD-WDK712-SW                 
155100                   END-IF                                                 
155200                END-IF                                                    
155300             END-IF                                                       
155400           END-IF                                                         
155500                                                                          
155600           IF UPD-WDK712-OK                                               
155700              MOVE DCS-IDLANDX2        TO W-IDLAND                        
155800              PERFORM IMS-GHU-WDK712                                      
155900              IF SEGMENT-FINNS                                            
156000                 IF WS-KDARTURS > SPACES                                  
156100                    MOVE WS-KDARTURS   TO LART-KDARTURS                   
156200                 END-IF                                                   
156300                 IF WS-VKART      > ZERO                                  
156400                    MOVE WS-VKART      TO LART-VKART                      
156500                 END-IF                                                   
156600                 IF WS-VLARTNTO   > ZERO                                  
156700                    MOVE WS-VLARTNTO   TO LART-VLARTNTO                   
156800                 END-IF                                                   
156900                 PERFORM IMS-REPL-WDK712                                  
157000              END-IF                                                      
157100           END-IF                                                         
157200        END-IF                                                            
157300     END-IF                                                               
157400                                                                          
157500     IF MID-FLREFERAL NOT = ALL '+'                                       
157600**WS-IDLAND-SPAR POPULATED IN B-SECTION                                   
157700       MOVE WS-IDLAND-SPAR TO W-IDLAND                                    
157800       IF MID-FLREFERAL NOT = ALL '+'                                     
157900          PERFORM IMS-GHU-WDK712                                          
158000          MOVE 'N'    TO UPD-WDK712-SW                                    
158100          IF SEGMENT-FINNS                                                
158200            IF MID-FLREFERAL NOT = LART-FLREFERAL                         
158300               MOVE JA                 TO UPD-WDK712-SW                   
158400               PERFORM HAB-UPD-WDGX2510                                   
158500            END-IF                                                        
158600            IF MID-FLREFERAL            = YES OR JA                       
158700               MOVE JA                 TO LART-FLREFERAL                  
158800            ELSE                                                          
158900               MOVE MID-FLREFERAL                                         
159000                                      TO LART-FLREFERAL                   
159100            END-IF                                                        
159200            IF UPD-WDK712-OK                                              
159300              PERFORM IMS-REPL-WDK712                                     
159400            END-IF                                                        
159500          END-IF                                                          
159600       END-IF                                                             
159700     END-IF                                                               
159800     .                                                                    
159900     EJECT                                                                
160000                                                                          
160100 HAA-GET-BESPRIS SECTION.                                                 
160200     IF NDC-CN OR NDC-NA                                                  
160300       PERFORM HAAA-GET-BESPRIS                                           
160400     ELSE                                                                 
160500       PERFORM HAAB-GET-BESPRIS                                           
160600     END-IF                                                               
160700     .                                                                    
160800     EJECT                                                                
160900                                                                          
161000 HAAA-GET-BESPRIS SECTION.                                                
161100*    -- WDK712                                                            
161200     MOVE DCS-IDLANDX2    TO W-IDLAND                                     
161300     PERFORM IMS-GU-WDK712                                                
161400     IF SEGMENT-FINNS                                                     
161500       MOVE LART-PRMATRL     TO WS-PRARTBES                               
161600     ELSE                                                                 
161700       MOVE ZERO             TO WS-PRARTBES                               
161800     END-IF                                                               
161900     .                                                                    
162000     EJECT                                                                
162100                                                                          
162200 HAAB-GET-BESPRIS SECTION.                                                
162300                                                                          
162400     MOVE CLAG-PRARTSTD         TO WS-PRARTBES                            
162500*    MOVE FUNCTION CURRENT-DATE(1:8) TO DAGENS-AAAAMMDD                   
162600*    COMPUTE W-DAPRLIST = 99999999 - DAGENS-AAAAMMDD                      
162700*    PERFORM IMS-GNP-ARTC21                                               
162800*    IF SEGMENT-SAKNAS                                                    
162900*      MOVE CLAG-PRARTSTD       TO WS-PRARTBES                            
163000*    ELSE                                                                 
163100*      MOVE NEJ                 TO FL-PRARTBES                            
163200*      PERFORM UNTIL  SEGMENT-SAKNAS                                      
163300*        IF PRL-SUINLEV-PR > ZERO                                         
163400*          MOVE PRL-PRARTBES-PR  TO WS-PRARTBES                           
163500*          SET SEGMENT-SAKNAS TO TRUE                                     
163600*        ELSE                                                             
163700*          IF FL-PRARTBES = NEJ                                           
163800*            MOVE PRL-PRARTBES-PR TO WS-PRARTBES                          
163900*            MOVE JA              TO FL-PRARTBES                          
164000*          END-IF                                                         
164100*          PERFORM IMS-GNP-ARTC21                                         
164200*        END-IF                                                           
164300*      END-PERFORM                                                        
164400*    END-IF                                                               
164500     .                                                                    
164600     EJECT                                                                
164700                                                                          
164800 HAB-UPD-WDGX2510 SECTION.                                                
164900                                                                          
165000     MOVE LART-IDLANDX2             TO W-IDLANDX2                         
165100     PERFORM IMS-GHU-WDGX2508                                             
165200     IF SEGMENT-SAKNAS                                                    
165300        MOVE LART-IDLANDX2          TO 2508-IDLANDX2                      
165400        PERFORM IMS-ISRT-WDGX2508                                         
165500     END-IF                                                               
165600                                                                          
165700     PERFORM IMS-GHU-WDGX2510                                             
165800     IF SEGMENT-SAKNAS                                                    
165900        MOVE W-IDARTNR              TO 2510-IDARTNR                       
166000        MOVE MID-FLREFERAL          TO 2510-FLREFERAL                     
166100        MOVE MSGI-IDUSER            TO 2510-IDUSER                        
166200        MOVE DAGENS-DATUM           TO 2510-TIREGDAT                      
166300        PERFORM IMS-ISRT-WDGX2510                                         
166400     ELSE                                                                 
166500        MOVE MID-FLREFERAL          TO 2510-FLREFERAL                     
166600        MOVE MSGI-IDUSER            TO 2510-IDUSER                        
166700        MOVE DAGENS-DATUM           TO 2510-TIREGDAT                      
166800        PERFORM IMS-REPL-WDGX2510                                         
166900     END-IF                                                               
167000     .                                                                    
167100     EJECT                                                                
167200 S9-SEARCH-IDLAND SECTION.                                                
167300                                                                          
167400     SEARCH ALL DC-LAND                                                   
167500       AT END                                                             
167600         MOVE SPACE          TO W-IDLAND                                  
167700       WHEN DCLAND-IDDC (DCLAND-IX) = WS-IDDC-LAND                        
167800         MOVE DCLAND-IDLANDX2(DCLAND-IX) TO W-IDLAND                      
167900     END-SEARCH                                                           
168000     .                                                                    
168100     EJECT                                                                
168200                                                                          
168300                                                                          
168400 S99-ABEND SECTION.                                                       
168500                                                                          
168600     SKIP2                                                                
168700     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
168800     .                                                                    
168900     EJECT                                                                
169000                                                                          
169100                                                                          
169200                                                                          
169300 MFS-RENSA-FAELT-UT SECTION.                                              
169400                                                                          
169500*    --- ALLA UTDATA-FÄLT                                                 
169600     MOVE MFS-RENSA-FAELT TO MOD-IDLEVNR-SLAG                             
169700                             MOD-FLORDSP                                  
169800                             MOD-FLSPBULK                                 
169900                             MOD-KVDAGAR-MANLT                            
170000                             MOD-IDPERSON-BUY                             
170100                             MOD-FLREFERAL                                
170200                             MOD-TEARTNOT-ORDER-IN                        
170300     .                                                                    
170400     SKIP3                                                                
170500                                                                          
170600                                                                          
170700 MFS-RENSA-FAELT-IN SECTION.                                              
170800                                                                          
170900*    --- ALLA INDATA-FÄLT                                                 
171000     MOVE MFS-RENSA-FAELT TO MOD-IDLEVNR-SLAG-IN                          
171100                             MOD-FLORDSP-IN                               
171200                             MOD-FLSPBULK-IN                              
171300                             MOD-KVDAGAR-MANLT-IN                         
171400                             MOD-IDPERSON-BUY-IN                          
171500                             MOD-FLREFERAL-IN                             
171600                             MOD-TEARTNOT-ORDER-IN                        
171700     .                                                                    
171800     EJECT                                                                
171900                                                                          
172000                                                                          
172100 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
172200                                                                          
172300*    --- ALLA UTDATA-FÄLT                                                 
172400     MOVE MFS-ROER-EJ-FAELT TO MOD-IDLEVNR-SLAG                           
172500                               MOD-FLORDSP                                
172600                               MOD-FLSPBULK                               
172700                               MOD-KVDAGAR-MANLT                          
172800                               MOD-IDPERSON-BUY                           
172900                               MOD-FLREFERAL                              
173000                               MOD-TEARTNOT-ORDER-IN                      
173100     .                                                                    
173200     SKIP3                                                                
173300                                                                          
173400                                                                          
173500 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
173600                                                                          
173700*    --- ALLA INDATA-FÄLT                                                 
173800     MOVE MFS-ROER-EJ-FAELT TO MOD-IDLEVNR-SLAG-IN                        
173900                               MOD-FLORDSP-IN                             
174000                               MOD-FLSPBULK-IN                            
174100                               MOD-KVDAGAR-MANLT-IN                       
174200                               MOD-IDPERSON-BUY-IN                        
174300                               MOD-FLREFERAL-IN                           
174400                               MOD-TEARTNOT-ORDER-IN                      
174500     .                                                                    
174600     EJECT                                                                
174700                                                                          
174800                                                                          
174900 MFS-FORM-ATTR SECTION.                                                   
175000                                                                          
175100*    --- ALLA INDATA-FÄLT                                                 
175200     MOVE MFS-FORMATETS-ATTR TO MOD-IDLEVNR-SLAG-IN-ATTR                  
175300                                MOD-FLORDSP-IN-ATTR                       
175400                                MOD-FLSPBULK-IN-ATTR                      
175500                                MOD-KVDAGAR-MANLT-IN-ATTR                 
175600                                MOD-IDPERSON-BUY-IN-ATTR                  
175700                                MOD-FLREFERAL-IN-ATTR                     
175800                                MOD-TEARTNOT-ORDER-IN-ATTR                
175900     .                                                                    
176000     SKIP2                                                                
176100                                                                          
176200* --- IMS SEKTIONER ---                                                   
176300                                                                          
176400 IMS-GET-MSG SECTION.                                                     
176500                                                                          
176600     MOVE '  QC' TO GODK-STATUSKODER                                      
176700     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
176800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
176900     PERFORM IMS-STATUSKONTROLL                                           
177000     .                                                                    
177100     EJECT                                                                
177200                                                                          
177300                                                                          
177400 IMS-INSERT-MSG SECTION.                                                  
177500                                                                          
177600     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
177700       MOVE 'N' TO MFS-KDHUVOMR                                           
177800     END-IF                                                               
177900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
178000     MOVE SPACE TO GODK-STATUSKODER                                       
178100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
178200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
178300     PERFORM IMS-STATUSKONTROLL                                           
178400     .                                                                    
178500     EJECT                                                                
178600                                                                          
178700                                                                          
178800 IMS-GU-ARTC01 SECTION.                                                   
178900                                                                          
179000     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
179100          DELIMITED BY SIZE INTO SSA1                                     
179200     MOVE '  GE' TO GODK-STATUSKODER                                      
179300     CALL CBLTDLI USING GHU ARTC-PCB DLI-IO-ARTC01 SSA1                   
179400     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
179500     PERFORM IMS-STATUSKONTROLL                                           
179600     .                                                                    
179700     EJECT                                                                
179800                                                                          
179900                                                                          
180000 IMS-GNP-ARTC11 SECTION.                                                  
180100                                                                          
180200     STRING 'WLARTC11(KDSEGKEY =' W-KDSEGKEY-X ')'                        
180300          DELIMITED BY SIZE INTO SSA1                                     
180400     MOVE '  GE' TO GODK-STATUSKODER                                      
180500     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-ARTC11 SSA1                   
180600     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
180700     PERFORM IMS-STATUSKONTROLL                                           
180800     .                                                                    
180900     EJECT                                                                
181000                                                                          
181100 IMS-GNP-ARTC21 SECTION.                                                  
181200                                                                          
181300     STRING 'WLARTC21(DAPRLIST=>' W-DAPRLIST-X ')'                        
181400          DELIMITED BY SIZE INTO SSA1                                     
181500     MOVE '  GE' TO GODK-STATUSKODER                                      
181600     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-ARTC21 SSA1                   
181700     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
181800     PERFORM IMS-STATUSKONTROLL                                           
181900     .                                                                    
182000     EJECT                                                                
182100                                                                          
182200 IMS-GU-ARTS01 SECTION.                                                   
182300                                                                          
182400     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
182500          DELIMITED BY SIZE INTO SSA1                                     
182600     MOVE '  GE' TO GODK-STATUSKODER                                      
182700     CALL CBLTDLI USING GU ARTS-PCB DLI-IO-ARTS01 SSA1                    
182800     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
182900     PERFORM IMS-STATUSKONTROLL                                           
183000     .                                                                    
183100     EJECT                                                                
183200                                                                          
183300                                                                          
183400 IMS-GNP-ARTS11 SECTION.                                                  
183500                                                                          
183600     MOVE 'WLARTS11 ' TO SSA1                                             
183700     MOVE '  GE' TO GODK-STATUSKODER                                      
183800     CALL CBLTDLI USING GNP ARTS-PCB DLI-IO-ARTS11 SSA1                   
183900     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
184000     PERFORM IMS-STATUSKONTROLL                                           
184100     .                                                                    
184200     EJECT                                                                
184300                                                                          
184400                                                                          
184500 IMS-GNP-ARTS11-DC SECTION.                                               
184600                                                                          
184700     STRING 'WLARTS11(IDDC     =' W-IDDC-X ')'                            
184800          DELIMITED BY SIZE INTO SSA1                                     
184900     MOVE '  GE' TO GODK-STATUSKODER                                      
185000     CALL CBLTDLI USING GNP ARTS-PCB DLI-IO-ARTS11 SSA1                   
185100     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
185200     PERFORM IMS-STATUSKONTROLL                                           
185300     .                                                                    
185400     EJECT                                                                
185500                                                                          
185600                                                                          
185700 IMS-GHNP-ARTS11 SECTION.                                                 
185800                                                                          
185900     STRING 'WLARTS11(IDDC     =' W-IDDC-X ')'                            
186000          DELIMITED BY SIZE INTO SSA1                                     
186100     MOVE '  ' TO GODK-STATUSKODER                                        
186200     CALL CBLTDLI USING GHNP ARTS-PCB DLI-IO-ARTS11 SSA1                  
186300     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
186400     PERFORM IMS-STATUSKONTROLL                                           
186500     .                                                                    
186600     EJECT                                                                
186700                                                                          
186800 IMS-GU-WDK711 SECTION.                                                   
186900                                                                          
187000     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
187100          DELIMITED BY SIZE INTO SSA1                                     
187200     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
187300          DELIMITED BY SIZE INTO SSA2                                     
187400     MOVE '  ' TO GODK-STATUSKODER                                        
187500     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2               
187600     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
187700     PERFORM IMS-STATUSKONTROLL                                           
187800     .                                                                    
187900     EJECT                                                                
188000 IMS-GNP-WDK727 SECTION.                                                  
188100                                                                          
188200     MOVE 'WDK727  '        TO SSA1                                       
188300     MOVE '  GE'            TO GODK-STATUSKODER                           
188400     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK727 SSA1                   
188500     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
188600     PERFORM IMS-STATUSKONTROLL                                           
188700     .                                                                    
188800     EJECT                                                                
188900 IMS-GU-WDK721 SECTION.                                                   
189000                                                                          
189100     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
189200          DELIMITED BY SIZE INTO SSA1                                     
189300     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
189400          DELIMITED BY SIZE INTO SSA2                                     
189500     STRING 'WDK721  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
189600          DELIMITED BY SIZE INTO SSA3                                     
189700     MOVE '  GE' TO GODK-STATUSKODER                                      
189800     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK721 SSA1 SSA2 SSA3          
189900     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
190000     PERFORM IMS-STATUSKONTROLL                                           
190100     .                                                                    
190200     EJECT                                                                
190300 IMS-GHU-WDK721 SECTION.                                                  
190400                                                                          
190500     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
190600          DELIMITED BY SIZE INTO SSA1                                     
190700     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
190800          DELIMITED BY SIZE INTO SSA2                                     
190900     STRING 'WDK721  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
191000          DELIMITED BY SIZE INTO SSA3                                     
191100     MOVE '  GE' TO GODK-STATUSKODER                                      
191200     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-WDK721 SSA1 SSA2 SSA3         
191300     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
191400     PERFORM IMS-STATUSKONTROLL                                           
191500     .                                                                    
191600     EJECT                                                                
191700 IMS-REPL-WDK721 SECTION.                                                 
191800                                                                          
191900     MOVE '  ' TO GODK-STATUSKODER                                        
192000     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK721                       
192100     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
192200     PERFORM IMS-STATUSKONTROLL                                           
192300     .                                                                    
192400     EJECT                                                                
192500                                                                          
192600 IMS-REPL-ARTS11 SECTION.                                                 
192700                                                                          
192800     MOVE '  ' TO GODK-STATUSKODER                                        
192900     CALL CBLTDLI USING REPL ARTS-PCB DLI-IO-ARTS11                       
193000     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
193100     PERFORM IMS-STATUSKONTROLL                                           
193200     .                                                                    
193300     EJECT                                                                
193400                                                                          
193500                                                                          
193600 IMS-GU-LEVA01 SECTION.                                                   
193700                                                                          
193800     STRING 'WLLEVA01(IDLEVNR  =' W-IDLEVNR-X ')'                         
193900          DELIMITED BY SIZE INTO SSA1                                     
194000     MOVE '  GE' TO GODK-STATUSKODER                                      
194100     CALL CBLTDLI USING GU LEVA-PCB DLI-IO-LEVA01 SSA1                    
194200     MOVE LEVA-STATUS-CODE TO STATUS-WS                                   
194300     PERFORM IMS-STATUSKONTROLL                                           
194400     .                                                                    
194500     EJECT                                                                
194600                                                                          
194700 IMS-GU-BENA01-BSEQ SECTION.                                              
194800     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
194900          DELIMITED BY SIZE INTO SSA1                                     
195000     MOVE '  GE' TO GODK-STATUSKODER                                      
195100     CALL CBLTDLI USING GU BENA-PCB DLI-IO-BENA01 SSA1                    
195200     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
195300     PERFORM IMS-STATUSKONTROLL                                           
195400     .                                                                    
195500     EJECT                                                                
195600                                                                          
195700                                                                          
195800 IMS-GNP-BENA11 SECTION.                                                  
195900     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
196000          DELIMITED BY SIZE INTO SSA1                                     
196100     MOVE '  GE' TO GODK-STATUSKODER                                      
196200     CALL CBLTDLI USING GU BENA-PCB DLI-IO-BENA11 SSA1                    
196300     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
196400     PERFORM IMS-STATUSKONTROLL                                           
196500     .                                                                    
196600     EJECT                                                                
196700                                                                          
196800 IMS-GU-WDK722 SECTION.                                                   
196900                                                                          
197000     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
197100          DELIMITED BY SIZE INTO SSA1                                     
197200     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
197300          DELIMITED BY SIZE INTO SSA2                                     
197400     STRING 'WDK722  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
197500          DELIMITED BY SIZE INTO SSA3                                     
197600     MOVE '  GE' TO GODK-STATUSKODER                                      
197700     CALL CBLTDLI USING GU WDK7-22-PCB DLI-IO-WDK722                      
197800                        SSA1 SSA2 SSA3                                    
197900     MOVE WDK7-22-STATUS-CODE TO STATUS-WS                                
198000     PERFORM IMS-STATUSKONTROLL                                           
198100     .                                                                    
198200     EJECT                                                                
198300 IMS-GU-WDB601 SECTION.                                                   
198400                                                                          
198500     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
198600          DELIMITED BY SIZE INTO SSA1                                     
198700     MOVE '  GE' TO GODK-STATUSKODER                                      
198800     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
198900     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
199000     PERFORM IMS-STATUSKONTROLL                                           
199100     .                                                                    
199200     EJECT                                                                
199300 IMS-GN-WDB601 SECTION.                                                   
199400                                                                          
199500     MOVE 'WDB601   ' TO SSA1                                             
199600     MOVE '  GB' TO GODK-STATUSKODER                                      
199700     CALL CBLTDLI USING GN WDB6-PCB DLI-IO-WDB601 SSA1                    
199800     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
199900     PERFORM IMS-STATUSKONTROLL                                           
200000     .                                                                    
200100     EJECT                                                                
200200 IMS-GU-WDB601-LEVNR-DC SECTION.                                          
200300                                                                          
200400     STRING 'WDB601  (IDLEVNDC =' W-IDLEVNRDC-X ')'                       
200500          DELIMITED BY SIZE INTO SSA1                                     
200600     MOVE '  GE' TO GODK-STATUSKODER                                      
200700     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
200800     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
200900     PERFORM IMS-STATUSKONTROLL                                           
201000     .                                                                    
201100     EJECT                                                                
201200 IMS-GU-WDB615 SECTION.                                                   
201300                                                                          
201400     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
201500          DELIMITED BY SIZE INTO SSA1                                     
201600     STRING 'WDB615  (WDB615KY =' W-WDB615KY-X ')'                        
201700          DELIMITED BY SIZE INTO SSA2                                     
201800     MOVE '  GE' TO GODK-STATUSKODER                                      
201900     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB615 SSA1 SSA2               
202000     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
202100     PERFORM IMS-STATUSKONTROLL                                           
202200     .                                                                    
202300     SKIP3                                                                
202400 IMS-GU-WDB616 SECTION.                                                   
202500                                                                          
202600     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
202700          DELIMITED BY SIZE INTO SSA1                                     
202800     STRING 'WDB616  (IDDCREF  =' W-IDDC-REF-X ')'                        
202900          DELIMITED BY SIZE INTO SSA2                                     
203000     MOVE '  GE' TO GODK-STATUSKODER                                      
203100     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB616 SSA1 SSA2               
203200     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
203300     PERFORM IMS-STATUSKONTROLL                                           
203400     .                                                                    
203500 IMS-GU-WDK711-REF SECTION.                                               
203600     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
203700          DELIMITED BY SIZE INTO SSA1                                     
203800     STRING 'WDK711  (IDDC     =' W-IDDC-K7-X ')'                         
203900          DELIMITED BY SIZE INTO SSA2                                     
204000     MOVE '  GE' TO GODK-STATUSKODER                                      
204100     CALL CBLTDLI USING GU WDK72-PCB DLI-IO-WDK711                        
204200          SSA1 SSA2                                                       
204300     MOVE WDK72-STATUS-CODE TO STATUS-WS                                  
204400     PERFORM IMS-STATUSKONTROLL                                           
204500     .                                                                    
204600                                                                          
204700 IMS-GU-WDK712   SECTION.                                                 
204800                                                                          
204900     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
205000          DELIMITED BY SIZE INTO SSA1                                     
205100     STRING 'WDK712  (IDLAND   =' W-IDLAND-X ')'                          
205200          DELIMITED BY SIZE INTO SSA2                                     
205300     MOVE '  GE' TO GODK-STATUSKODER                                      
205400     CALL CBLTDLI USING GU WDK72-PCB DLI-IO-WDK712                        
205500          SSA1 SSA2                                                       
205600     MOVE WDK72-STATUS-CODE TO STATUS-WS                                  
205700     PERFORM IMS-STATUSKONTROLL                                           
205800     .                                                                    
205900     EJECT                                                                
206000 IMS-GHU-WDK712   SECTION.                                                
206100                                                                          
206200     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
206300          DELIMITED BY SIZE INTO SSA1                                     
206400     STRING 'WDK712  (IDLAND   =' W-IDLAND-X ')'                          
206500          DELIMITED BY SIZE INTO SSA2                                     
206600     MOVE '  '                TO GODK-STATUSKODER                         
206700     CALL CBLTDLI USING GHU WDK72-PCB DLI-IO-WDK712 SSA1 SSA2             
206800     MOVE WDK72-STATUS-CODE    TO STATUS-WS                               
206900     PERFORM IMS-STATUSKONTROLL                                           
207000     .                                                                    
207100     EJECT                                                                
207200                                                                          
207300 IMS-REPL-WDK712 SECTION.                                                 
207400                                                                          
207500     MOVE '  '              TO GODK-STATUSKODER                           
207600     CALL CBLTDLI        USING REPL WDK72-PCB DLI-IO-WDK712               
207700     MOVE WDK72-STATUS-CODE TO STATUS-WS                                  
207800     PERFORM IMS-STATUSKONTROLL                                           
207900     .                                                                    
208000     EJECT                                                                
208100                                                                          
208200 IMS-GU-WDE301 SECTION.                                                   
208300                                                                          
208400     STRING 'WDE301  (WDE301KY=>' W-WDE301KY-MIN-X                        
208500                    '&WDE301KY<=' W-WDE301KY-MAX-X                        
208600                    '&IDARTNR  =' W-IDARTNR-X                             
208700                    '&KDREFTYP=>' W-KDREFTYP-MIN-X                        
208800                    '&KDREFTYP<=' W-KDREFTYP-MAX-X ')'                    
208900          DELIMITED BY SIZE INTO SSA1                                     
209000     MOVE '  GE' TO GODK-STATUSKODER                                      
209100     CALL CBLTDLI USING GU WDE3-PCB DLI-IO-WDE301 SSA1                    
209200     MOVE WDE3-STATUS-CODE TO STATUS-WS                                   
209300     PERFORM IMS-STATUSKONTROLL                                           
209400     .                                                                    
209500     EJECT                                                                
209600                                                                          
209700 IMS-GHU-WDGX2508 SECTION.                                                
209800                                                                          
209900     STRING 'WDR501  (WDGXKEY  =' W-WDGX2507-X ')'                        
210000          DELIMITED BY SIZE INTO SSA1                                     
210100     STRING 'WDGX2508(IDLANDX2 =' W-IDLANDX2-X ')'                        
210200          DELIMITED BY SIZE INTO SSA2                                     
210300     MOVE '  GE'              TO GODK-STATUSKODER                         
210400     CALL CBLTDLI USING GHU WDR5-PCB DLI-IO-WDGX2508 SSA1 SSA2            
210500     MOVE WDR5-STATUS-CODE    TO STATUS-WS                                
210600     PERFORM IMS-STATUSKONTROLL                                           
210700     .                                                                    
210800                                                                          
210900 IMS-ISRT-WDGX2508 SECTION.                                               
211000                                                                          
211100     STRING 'WDR501  (WDGXKEY  =' W-WDGX2507-X ')'                        
211200            DELIMITED BY SIZE INTO SSA1                                   
211300     MOVE 'WDGX2508'            TO SSA2                                   
211400     MOVE '  '                  TO GODK-STATUSKODER                       
211500     CALL CBLTDLI USING ISRT WDR5-PCB DLI-IO-WDGX2508 SSA1 SSA2           
211600     MOVE WDR5-STATUS-CODE      TO STATUS-WS                              
211700     PERFORM IMS-STATUSKONTROLL                                           
211800     .                                                                    
211900                                                                          
212000 IMS-GHU-WDGX2510 SECTION.                                                
212100                                                                          
212200     STRING 'WDR501  (WDGXKEY  =' W-WDGX2507-X ')'                        
212300          DELIMITED BY SIZE INTO SSA1                                     
212400     STRING 'WDGX2508(IDLANDX2 =' W-IDLANDX2-X ')'                        
212500          DELIMITED BY SIZE INTO SSA2                                     
212600     STRING 'WDGX2510(IDARTNR  =' W-IDARTNR-X ')'                         
212700          DELIMITED BY SIZE INTO SSA3                                     
212800     MOVE '  GE'             TO GODK-STATUSKODER                          
212900     CALL CBLTDLI USING GHU WDR5-PCB DLI-IO-WDGX2510                      
213000                                     SSA1 SSA2 SSA3                       
213100     MOVE WDR5-STATUS-CODE    TO STATUS-WS                                
213200     PERFORM IMS-STATUSKONTROLL                                           
213300     .                                                                    
213400                                                                          
213500 IMS-ISRT-WDGX2510 SECTION.                                               
213600                                                                          
213700     STRING 'WDR501  (WDGXKEY  =' W-WDGX2507-X ')'                        
213800            DELIMITED BY SIZE INTO SSA1                                   
213900     STRING 'WDGX2508(IDLANDX2 =' W-IDLANDX2-X ')'                        
214000          DELIMITED BY SIZE   INTO SSA2                                   
214100     MOVE 'WDGX2510'            TO SSA3                                   
214200     MOVE '  '                  TO GODK-STATUSKODER                       
214300     CALL CBLTDLI USING ISRT WDR5-PCB DLI-IO-WDGX2510                     
214400                                      SSA1 SSA2 SSA3                      
214500     MOVE WDR5-STATUS-CODE      TO STATUS-WS                              
214600     PERFORM IMS-STATUSKONTROLL                                           
214700     .                                                                    
214800                                                                          
214900 IMS-REPL-WDGX2510 SECTION.                                               
215000                                                                          
215100     MOVE '  ' TO GODK-STATUSKODER                                        
215200     CALL CBLTDLI USING REPL WDR5-PCB DLI-IO-WDGX2510                     
215300     MOVE WDR5-STATUS-CODE      TO STATUS-WS                              
215400     PERFORM IMS-STATUSKONTROLL                                           
215500     .                                                                    
215600                                                                          
215700 IMS-GU-WDGX2510 SECTION.                                                 
215800                                                                          
215900     STRING 'WDR501  (WDGXKEY  =' W-WDGX2507-X ')'                        
216000          DELIMITED BY SIZE INTO SSA1                                     
216100     STRING 'WDGX2508(IDLANDX2 =' W-IDLANDX2-X ')'                        
216200          DELIMITED BY SIZE INTO SSA2                                     
216300     STRING 'WDGX2510(IDARTNR  =' W-IDARTNR-X ')'                         
216400          DELIMITED BY SIZE INTO SSA3                                     
216500     MOVE '  GE'             TO GODK-STATUSKODER                          
216600     CALL CBLTDLI USING GU WDR5-PCB DLI-IO-WDGX2510                       
216700                                     SSA1 SSA2 SSA3                       
216800     MOVE WDR5-STATUS-CODE    TO STATUS-WS                                
216900     PERFORM IMS-STATUSKONTROLL                                           
217000     .                                                                    
217100                                                                          
217200     EJECT                                                                
217300 IMS-STATUSKONTROLL SECTION.                                              
217400                                                                          
217500     SET STATUS-IX TO 1                                                   
217600     SEARCH GODK-STATUS                                                   
217700       AT END                                                             
217800         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
217900         DELIMITED BY SIZE INTO FELTEXT                                   
218000         CALL FELLOG                                                      
218100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
218200         CONTINUE                                                         
218300     END-SEARCH                                                           
218400     .                                                                    
218500     EJECT                                                                
218600*    -COPY WY2000P1                                                       
