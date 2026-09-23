000100**********************************************************                
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W2035500.                                                
000400 AUTHOR.         STEFAN ANDREASSON, FRONTEC.                              
000500 DATE-WRITTEN.   96/05/02.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        SÄSONGSANALYS                                                    
001000*                                                                         
001100*        PROGRAMMET UPPDATERAR WLARTS (WDK7)                              
001200*                              WLUSEA (WDP7)                              
001300*                              WDL7 + WDL4                                
001400*        PROGRAMMET LÄSER      WLBENA (WDD3)                              
001500*                                                                         
001600*                                                                         
001700*    INDATA.                                                              
001800*        TRANSAKTION: W2T355                                              
001900*        MID:         W2I35501                                            
002000*                                                                         
002100*    UTDATA.                                                              
002200*        MOD:         W2O35501                                            
002300*                                                                         
002400*                                                                         
002500**********************************************************                
002600*--- PROGRAMÄNDRINGAR                                                     
002700*                                                                         
002800*    2013-12-20  E'TRACKER: 10205381 CHINA PROCUREMENT SCREEN 5           
002900*                                    AND REFILL 2355.                     
003000*                                                                         
003100                                                                          
003200     SKIP3                                                                
003300 ENVIRONMENT DIVISION.                                                    
003400     EJECT                                                                
003500 DATA DIVISION.                                                           
003600 WORKING-STORAGE SECTION.                                                 
003700*    -COPY WY2000W1                                                       
003800     SKIP3                                                                
003900                                                                          
004000*    -- CHECKED BY WY2000                                                 
004100 77  IDPGM                       PIC X(08)   VALUE 'W2035500'.            
004200                                                                          
004300*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004400 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004500                                                                          
004600 77  JA                          PIC X       VALUE 'J'.                   
004700 77  NEJ                         PIC X       VALUE 'N'.                   
004800 77  DEFINITIV                   PIC S9      VALUE +1 COMP-3.             
004900 77  IX                          PIC 9(3)    VALUE ZERO.                  
005000 77  SPRAK-IX                    PIC 9(3)    VALUE ZERO.                  
005100 77  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005200                                                                          
005300*01  -COPY WWDCKONS                                                       
005400                                                                          
005500*01  -COPY WWDC99                                                         
005600                                                                          
005700 01  WS.                                                                  
005800  03 WS-TEST.                                                             
005900    05 WS-TEST-A                 PIC X(6)    VALUE SPACE.                 
006000    05 FILLER                    PIC X       VALUE '/'.                   
006100    05 WS-TEST-B                 PIC X(6)    VALUE SPACE.                 
006200  03 WS-ANTAL                    PIC 9(9)    VALUE ZERO.                  
006300  03 WS-TOT-VALANT               PIC 9(9)    VALUE ZERO.                  
006400  03 WS-TOT-SIMANT               PIC 9(9)    VALUE ZERO.                  
006500  03 WS-TOT-HISANT               PIC 9(9)    VALUE ZERO.                  
006600  03 WS-INDEX                    PIC 9(3)    VALUE ZERO.                  
006700  03 WS-INDEX-DEC                PIC 9(3)V9(3)                            
006800                                             VALUE ZERO.                  
006900  03 WS-DASPSEA                  PIC 9(7)    VALUE ZERO.                  
007000  03 WS-KVOI                     PIC 9(7)    VALUE ZERO.                  
007100  03 WS-OSAKERHET                PIC Z(2)9.9 VALUE ZERO.                  
007200  03 WS-SIMIX-SUM                PIC 9(4)    VALUE ZERO.                  
007300  03 WS-SIMIX                    PIC 9(3)    VALUE ZERO.                  
007400  03 WS-JUSTERA                  PIC S9V9(2) VALUE ZERO  COMP-3.          
007500  03 WS-IDARTNR                  PIC X(9)    VALUE SPACE.                 
007600  03 DAGENS-AAAAMMDD             PIC 9(8)    VALUE ZERO.                  
007700  03 FL-PRARTBES                 PIC X(1)    VALUE 'N'.                   
007800  03 WS-PRARTBES                 PIC S9(7)V9(2) VALUE ZERO COMP-3.        
007900*********************************************************                 
008000*    WS-MSGI-AREA ANVÄNDS FÖR ATT SPARA DET SOM LIGGER                    
008100*                 I MOD FÖR DE FÄLT DÄR SAMMA FÄLT ANVÄNDS                
008200*                 FÖR MID OCH MOD                                         
008300*    WS-MSGI-AREA SPARAS UNDAN PÅ MSGI-SPAR-AREA I                        
008400*                 NYCKELDATABASEN                                         
008500*********************************************************                 
008600  03 WS-MSGI-AREA.                                                        
008700    10 WS-MSGI-IDTRANS-2355      PIC X(4)    VALUE '2355'.                
008800    10 WS-MSGI-SIMIX             OCCURS 12                                
008900                                 PIC 9(3)    VALUE ZERO  COMP-3.          
009000    10 WS-MSGI-SIMANT            OCCURS 12                                
009100                                 PIC 9(7)    VALUE ZERO  COMP-3.          
009200    10 FILLER                    PIC X(124)  VALUE SPACE.                 
009300  03   WS-TEST-SIMIX-GRP.                                                 
009400   05  WS-TEST-SIMIX             OCCURS 12                                
009500                                 PIC 9(3)    VALUE ZERO.                  
009600                                                                          
009700*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
009800                                                                          
009900                                                                          
010000 77  INDATA-SW                   PIC X       VALUE 'J'.                   
010100     88  INDATA-OK                           VALUE 'J'.                   
010200     88  INDATA-FEL                          VALUE 'N'.                   
010300                                                                          
010400 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
010500     88  NYCKLAR-OK                          VALUE 'J'.                   
010600     88  NYCKLAR-FEL                         VALUE 'N'.                   
010700                                                                          
010800 77  SIM-INDEX-SW                PIC X       VALUE 'N'.                   
010900     88  SIM-INDEX-JA                        VALUE 'J'.                   
011000     88  SIM-INDEX-NEJ                       VALUE 'N'.                   
011100                                                                          
011200 77  SIM-ANTAL-SW                PIC X       VALUE 'N'.                   
011300     88  SIM-ANTAL-JA                        VALUE 'J'.                   
011400     88  SIM-ANTAL-NEJ                       VALUE 'N'.                   
011500                                                                          
011600 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
011700     88  EGEN-MID                            VALUE '2355'.                
011800     88  GODK-MID                            VALUE '2351' '2352'          
011900                                                   '2353' '2354'          
012000                                                   '2355' '2356'          
012100                                                   '2357' '2358'          
012200                                                   '2359'.                
012300     88  HELP-MID                            VALUE '0551'.                
012400     EJECT                                                                
012500*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
012600 01  GENERELLA-SUBPROGRAM.                                                
012700     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
012800     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
012900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
013000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
013100     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
013200     03  W271SEAS                PIC X(8)    VALUE 'W271SEAS'.            
013300     03  W271REFL                PIC X(8)    VALUE 'W271REFL'.            
013400     03  W271UTUP                PIC X(8)    VALUE 'W271UTUP'.            
013500     EJECT                                                                
013600*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
013700*01 -COPY WMEDAREA                                                        
013800     EJECT                                                                
013900*    --- PARAMETRAR TILL W271REFL                                         
014000*01 -COPY W271REFL                                                        
014100     EJECT                                                                
014200*    --- PARAMETRAR TILL W271UTUP                                         
014300*01 -COPY W271UTUP                                                        
014400     EJECT                                                                
014500 01  MESSAGE-CODES.                                                       
014600     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
014700     03  CONFLICT                PIC X(3)    VALUE '002'.                 
014800     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
014900     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
015000     03  ARTIKEL-UTGANGEN        PIC X(3)    VALUE '018'.                 
015100     03  ARTIKEL-SAKNAS          PIC X(3)    VALUE '017'.                 
015200     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
015300     03  ARTIKEL-ERSATT          PIC X(3)    VALUE '220'.                 
015400     03  PRIS-SAKNAS             PIC X(3)    VALUE '301'.                 
015500     03  ARTIKEL-SAKNAS-SDC      PIC X(3)    VALUE '305'.                 
015600     03  DIREKTLEV               PIC X(3)    VALUE '306'.                 
015700     03  EJ-GODK-REFILL          PIC X(3)    VALUE '307'.                 
015800     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
015900     03  INF-NOT-REFILL          PIC X(3)    VALUE '957'.                 
016000                                                                          
016100 01  MEDDELANDE.                                                          
016200     03  MED-1                  PIC X(30)                                 
016300         VALUE 'TOTAL OF INDEX IS NOT 1200'.                              
016400     03  MED-2.                                                           
016500      05 FILLER                 PIC X(18)                                 
016600         VALUE 'TOTAL OF INDEX IS '.                                      
016700      05 MED-2-INDEX            PIC Z(3)9.                                
016800      05 FILLER                 PIC X(16)                                 
016900         VALUE ' IT MUST BE 1200'.                                        
017000     03  MED-3                  PIC X(30)                                 
017100         VALUE 'INPUT SHOULD BE YYMMDD    '.                              
017200     03  MED-4                  PIC X(30)                                 
017300         VALUE 'USE SCREEN 2105'.                                         
017400                                                                          
017500     EJECT                                                                
017600*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
017700*01  -COPY WDATAREA                                                       
017800     EJECT                                                                
017900*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
018000*                                                                         
018100 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
018200     SKIP3                                                                
018300*01 -COPY WMSGINIT                                                        
018400     EJECT                                                                
018500*    --- PARAMETRAR TILL SUBPROGRAM W271SEAS                              
018600*                                                                         
018700 01  FILLER                      PIC X(16)   VALUE 'W271SEAS'.            
018800     SKIP3                                                                
018900*01 -COPY W271SEAS                                                        
019000     SKIP3                                                                
019100*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
019200*                                                                         
019300 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
019400     SKIP3                                                                
019500*01  MID -COPY W2I35501                                                   
019600     EJECT                                                                
019700 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
019800     SKIP3                                                                
019900*01  -COPY WMSGAREA                                                       
020000     EJECT                                                                
020100     03  MOD REDEFINES MSG-AREA.                                          
020200*      05  -COPY W2O35501                                                 
020300     EJECT                                                                
020400 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
020500     SKIP3                                                                
020600*01  -COPY WMFSAREA                                                       
020700     EJECT                                                                
020800*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
020900*                                                                         
021000     EJECT                                                                
021100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
021200     SKIP3                                                                
021300 01  NYCKLAR-TILL-DLI.                                                    
021400     03  W-IDARTNR-X.                                                     
021500         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
021600     03  W-IDLAND-X.                                                      
021700         05  W-IDLAND            PIC X(02)   VALUE SPACE.                 
021800     03  W-IDDC-X.                                                        
021900         05  W-IDDC              PIC X(02)   VALUE SPACE.                 
022000     03  W-IDUSER-X.                                                      
022100         05  W-IDUSER            PIC X(8)    VALUE SPACE.                 
022200     03  W-IDSKYLT-X.                                                     
022300         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
022400     03 W-IDLEVNR-X.                                                      
022500         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
022600     03 W-IDLEVNR-K7-X.                                                   
022700         05  W-IDLEVNR-K7        PIC X(5)    VALUE SPACE.                 
022800     03 W-DAPRLIST-X.                                                     
022900         05  W-DAPRLIST          PIC 9(8)    VALUE ZERO.                  
023000     03 W-KDSEGKEY-X.                                                     
023100         05  W-KDSEGKEY          PIC X       VALUE '1'.                   
023200     03  W-IDDC-B6-X.                                                     
023300         05 W-IDDC-B6                  PIC X(2).                          
023400     SKIP2                                                                
023500*    --- STATUS-KOD FRÅN IMS                                              
023600 01  STATUS-WS                   PIC XX.                                  
023700     88  SEGMENT-FINNS                       VALUE '  '.                  
023800     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
023900     88  SEGMENT-SAKNAS                      VALUE 'GE'                   
024000                                                   'GB'.                  
024100     SKIP2                                                                
024200 01  GODK-STATUSKODER.                                                    
024300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
024400     SKIP3                                                                
024500 01  SSA1                        PIC X(64).                               
024600 01  SSA2                        PIC X(64).                               
024700 01  SSA3                        PIC X(64).                               
024800     EJECT                                                                
024900*    --- IMS FUNKTIONSKODER                                               
025000*01  -COPY W0003                                                          
025100     EJECT                                                                
025200*    ---  DLI INPUT-OUTPUT AREA                                           
025300     SKIP3                                                                
025400 01  FILLER                     PIC X(16) VALUE 'DLI-IO-AREA-D3'.         
025500     SKIP3                                                                
025600 01  DLI-IO-AREA-D3.                                                      
025700     03  IO-AREA-D3              PIC X(150)  VALUE SPACE.                 
025800     SKIP3                                                                
025900     03  WLBENA11 REDEFINES IO-AREA-D3.                                   
026000*        05  -COPY WDD311  -PRE BENA11-                                   
026100     EJECT                                                                
026200 01  FILLER         PIC X(24) VALUE 'DLI-IO-LEVA01'.                      
026300 01  DLI-IO-LEVA01.                                                       
026400*    03  -COPY WDF101                                                     
026500     EJECT                                                                
026600                                                                          
026700 01  FILLER         PIC X(24) VALUE 'DLI-IO-LEVA16'.                      
026800 01  DLI-IO-LEVA16.                                                       
026900*    03  -COPY WDF116                                                     
027000     EJECT                                                                
027100 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK601'.             
027200     SKIP3                                                                
027300 01  DLI-IO-AREA-WDK601.                                                  
027400*    03  -COPY WDK601                                                     
027500     EJECT                                                                
027600 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK611'.             
027700     SKIP3                                                                
027800 01  DLI-IO-AREA-WDK611.                                                  
027900*    03  -COPY WDK611                                                     
028000                                                                          
028100 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK621'.             
028200     SKIP3                                                                
028300 01  DLI-IO-AREA-WDK621.                                                  
028400*    03  -COPY WDK621                                                     
028500                                                                          
028600 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK629'.             
028700     SKIP3                                                                
028800 01  DLI-IO-AREA-WDK629.                                                  
028900*    03  -COPY WDK629                                                     
029000                                                                          
029100 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
029200 01   DLI-IO-AREA-B601.                                                   
029300*     03  -COPY WDB601                                                    
029400     EJECT                                                                
029500 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK711'.             
029600 01  DLI-IO-WDK711.                                                       
029700*    03  -COPY WDK711                                                     
029800 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK712'.             
029900 01  DLI-IO-WDK712.                                                       
030000*    03  -COPY WDK712                                                     
030100     EJECT                                                                
030200 LINKAGE SECTION.                                                         
030300                                                                          
030400*01  -COPY W0009   -PRE MSG-                                              
030500     EJECT                                                                
030600*01  -COPY W0008  -PRE  USEA-                                             
030700     05  FILLER                  PIC X.                                   
030800     EJECT                                                                
030900*01  -COPY W0008  -PRE  WDK7-                                             
031000     05  FILLER                  PIC X.                                   
031100     EJECT                                                                
031200*01  -COPY W0008  -PRE  BENA-                                             
031300     05  FILLER                  PIC X.                                   
031400     EJECT                                                                
031500*01  -COPY W0008  -PRE WDK6-                                              
031600     05  FILLER                  PIC X.                                   
031700     EJECT                                                                
031800*01  -COPY W0008  -PRE  LEVA-                                             
031900     05  FILLER                  PIC X.                                   
032000     EJECT                                                                
032100*01  -COPY W0008  -PRE  WDL7-                                             
032200     05  FILLER                  PIC X.                                   
032300     EJECT                                                                
032400*01  -COPY W0008  -PRE  WDL4-                                             
032500     05  FILLER                  PIC X.                                   
032600     EJECT                                                                
032700*****W271REFL**********                                                   
032800*01  -COPY W0008  -PRE REFL-2501-                                         
032900     05  FILLER                  PIC X.                                   
033000     EJECT                                                                
033100*01  -COPY W0008  -PRE WDB6-                                              
033200     05  FILLER                  PIC X.                                   
033300     EJECT                                                                
033400*01  -COPY W0008  -PRE WDK72-                                             
033500     05  FILLER                  PIC X.                                   
033600     EJECT                                                                
033700 01  UTIL-WDK6-PCB               PIC X.                                   
033800 01  UTIL-WDK7-PCB               PIC X.                                   
033900 01  UTIL-WDB6-PCB               PIC X.                                   
034000     EJECT                                                                
034100******************************************                                
034200*01  -COPY W0008  -PRE WDK62-                                             
034300     05  FILLER                  PIC X.                                   
034400     EJECT                                                                
034500*****W271UTUP**********                                                   
034600 01  UTUP1-WDK7-PCB              PIC X.                                   
034700 01  UTUP1-WDB6-PCB              PIC X.                                   
034800 01  UTUP1-UTIL-WDK6-PCB         PIC X.                                   
034900 01  UTUP1-UTIL-WDK7-PCB         PIC X.                                   
035000 01  UTUP1-UTIL-WDB6-PCB         PIC X.                                   
035100     EJECT                                                                
035200 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB WDK7-PCB                      
035300     BENA-PCB WDK6-PCB LEVA-PCB WDL7-PCB WDL4-PCB REFL-2501-PCB           
035400     WDB6-PCB WDK72-PCB                                                   
035500     UTIL-WDK6-PCB                                                        
035600     UTIL-WDK7-PCB                                                        
035700     UTIL-WDB6-PCB                                                        
035800     WDK62-PCB                                                            
035900     UTUP1-WDK7-PCB                                                       
036000     UTUP1-WDB6-PCB                                                       
036100     UTUP1-UTIL-WDK6-PCB                                                  
036200     UTUP1-UTIL-WDK7-PCB                                                  
036300     UTUP1-UTIL-WDB6-PCB.                                                 
036400                                                                          
036500 MAIN SECTION.                                                            
036600     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB WDK7-PCB                      
036700     BENA-PCB WDK6-PCB LEVA-PCB WDL7-PCB WDL4-PCB REFL-2501-PCB           
036800     WDB6-PCB WDK72-PCB                                                   
036900     UTIL-WDK6-PCB                                                        
037000     UTIL-WDK7-PCB                                                        
037100     UTIL-WDB6-PCB                                                        
037200     WDK62-PCB                                                            
037300     UTUP1-WDK7-PCB                                                       
037400     UTUP1-WDB6-PCB                                                       
037500     UTUP1-UTIL-WDK6-PCB                                                  
037600     UTUP1-UTIL-WDK7-PCB                                                  
037700     UTUP1-UTIL-WDB6-PCB.                                                 
037800                                                                          
037900     PERFORM IMS-GET-MSG                                                  
038000     IF SEGMENT-FINNS                                                     
038100       PERFORM A-INIT                                                     
038200       PERFORM B-KOLLA-NYCKLAR                                            
038300       IF NYCKLAR-OK                                                      
038400         IF MFS-UPDATE                                                    
038500           PERFORM G-KOLLA-INPUT                                          
038600           IF INDATA-OK                                                   
038700             PERFORM H-UPPDATERA                                          
038800             PERFORM F-HAEMTA-INFO                                        
038900           END-IF                                                         
039000*          PERFORM MFS-ROER-EJ-FAELT-UT                                   
039100         ELSE                                                             
039200           PERFORM E-ENTER-TRYCKNING                                      
039300         END-IF                                                           
039400                                                                          
039500*        IF INDATA-OK                                                     
039600*   UPPDATERA BILDEN                                                      
039700           MOVE ZERO         TO WS-TOT-SIMANT                             
039800           MOVE 1              TO IX                                      
039900           PERFORM UNTIL IX > 12                                          
040000             MOVE WS-MSGI-SIMIX (IX)                                      
040100                             TO MOD-SIMIX (IX)                            
040200             MOVE WS-MSGI-SIMANT (IX)                                     
040300                             TO MOD-SIMANT (IX)                           
040400             ADD WS-MSGI-SIMANT (IX)                                      
040500                             TO WS-TOT-SIMANT                             
040600             ADD 1           TO IX                                        
040700           END-PERFORM                                                    
040800           MOVE WS-TOT-SIMANT                                             
040900                             TO MOD-TOT-SIMANT                            
041000                                                                          
041100*        END-IF                                                           
041200                                                                          
041300         MOVE '002'             TO MSGI-KDCALL                            
041400         MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                      
041500         MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                            
041600         MOVE '2351'            TO MSGI-IDTRANS                           
041700         MOVE WS-MSGI-AREA      TO MSGI-SPAR-AREA                         
041800         CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                       
041900       END-IF                                                             
042000*      MOVE WS-TEST          TO MOD-TEMFSINF                              
042100       COMPUTE MSG-KVLL = LENGTH OF MOD-W2O35501 + 4                      
042200       PERFORM IMS-INSERT-MSG                                             
042300     END-IF                                                               
042400                                                                          
042500     MOVE ZERO TO RETURN-CODE                                             
042600     GOBACK                                                               
042700     .                                                                    
042800     EJECT                                                                
042900 A-INIT SECTION.                                                          
043000                                                                          
043100     IF MSG-DUBBLA-TRANSKODER                                             
043200       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W2I35501                 
043300       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
043400       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
043500     ELSE                                                                 
043600       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W2I35501                  
043700       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
043800       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
043900     END-IF                                                               
044000                                                                          
044100     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
044200     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
044300     MOVE MFS-IDTRANS TO W-IDTRANS                                        
044400                                                                          
044500     MOVE LOW-VALUE TO MSG-AREA                                           
044600     MOVE 'W2O355N1' TO MFS-IDMOD                                         
044700     MOVE '2355' TO MOD-IDTRANS                                           
044800     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
044900                                                                          
045000     MOVE SPACE           TO MED-IDMFSINF                                 
045100     MOVE SPACE           TO MED-IDMFSFEL                                 
045200                                                                          
045300                                                                          
045400     IF EGEN-MID OR HELP-MID                                              
045500       CONTINUE                                                           
045600     ELSE                                                                 
045700       MOVE SPACE TO MFS-KDTRTYP                                          
045800       MOVE '7' TO MFS-IDPFK                                              
045900     END-IF                                                               
046000                                                                          
046100     MOVE +2      TO SPRAK-IX                                             
046200     MOVE 'GB ' TO MED-IDSKYLT                                            
046300                                                                          
046400     ACCEPT DAGENS-DATUM FROM DATE                                        
046500     .                                                                    
046600     EJECT                                                                
046700 B-KOLLA-NYCKLAR SECTION.                                                 
046800                                                                          
046900     MOVE ALL '+'           TO MSGI-WMSGINIT                              
047000     MOVE '001'             TO MSGI-KDCALL                                
047100     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
047200     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
047300     MOVE '2355'            TO MSGI-IDTRANS                               
047400     IF EGEN-MID                                                          
047500       MOVE MID-IDARTNR-IN  TO MSGI-IDARTNR                               
047600       MOVE MID-IDDC-IN     TO MSGI-IDDC-KEY                              
047700     ELSE                                                                 
047800       IF  MID-IDARTNR-IN NUMERIC                                         
047900       AND MID-IDARTNR-IN > ZERO                                          
048000         MOVE MID-IDARTNR-IN                                              
048100                            TO MSGI-IDARTNR                               
048200       END-IF                                                             
048300     END-IF                                                               
048400     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
048500                                                                          
048600     IF MSGI-SPAR-AREA (1:4) = '2355'                                     
048700       MOVE MSGI-SPAR-AREA  TO WS-MSGI-AREA                               
048800     ELSE                                                                 
048900*    --   DETTA GÖRS ENBART IFALL MAN KOMMER FRÅN                         
049000*    --   WHELP OCH I DEN TOMMA BILDEN INTE FYLLER I                      
049100*    --   VARKEN ARTNR ELLER DC SÅ SKALL MAN LÄGGA                        
049200*    --   UT INFO I BILDEN (GÖRS I EA-BEHANDLA SECTION)                   
049300*    --                                                                   
049400       INSPECT MSGI-IDARTNR REPLACING LEADING SPACE BY ZERO               
049500       MOVE MSGI-IDARTNR     TO MID-IDARTNR-IN                            
049600     END-IF                                                               
049700                                                                          
049800     MOVE JA TO NYCKLAR-SW                                                
049900                                                                          
050000*    -- KONTROLL AV IDARTNR                                               
050100     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
050200                                                                          
050300     INSPECT MSGI-IDARTNR REPLACING LEADING SPACE BY ZERO                 
050400     IF MSGI-IDARTNR NUMERIC                                              
050500       MOVE MSGI-IDARTNR     TO WS-IDARTNR                                
050600     ELSE                                                                 
050700       MOVE NEJ              TO NYCKLAR-SW                                
050800     END-IF                                                               
050900                                                                          
051000*    -- KONTROLL AV IDDC                                                  
051100     MOVE MFS-RENSA-FAELT    TO MOD-IDDC-IN                               
051200                                                                          
051300     MOVE MSGI-IDDC-KEY    TO W-IDDC-B6                                   
051400                              WS-IDDC                                     
051500     PERFORM IMS-GU-WDB601                                                
051600                                                                          
051700     IF DCS-KDDC = SPACE OR DCS-DDC                                       
051800       MOVE NEJ TO NYCKLAR-SW                                             
051900     END-IF                                                               
052000                                                                          
052100     MOVE WS-IDARTNR       TO MOD-IDARTNR-UT                              
052200                              W-IDARTNR                                   
052300     INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE               
052400     MOVE MSGI-IDDC-KEY    TO MOD-IDDC-UT                                 
052500                              W-IDDC                                      
052600                                                                          
052700     IF NDC-CN OR NDC-US                                                  
052800       PERFORM IMS-GU-WDK711                                              
052900       IF SEGMENT-FINNS                                                   
053000         IF SLAG-IDDC-REF = SPACES                                        
053100           MOVE INF-NOT-REFILL TO MED-IDMFSINF                            
053200           CALL WMEDKONV USING MED-WMEDAREA                               
053300           MOVE MED-MFSINF TO MOD-TEMFSINF                                
053400           MOVE NEJ        TO NYCKLAR-SW                                  
053500         END-IF                                                           
053600       END-IF                                                             
053700     END-IF                                                               
053800                                                                          
053900     IF CDC-SE                                                            
054000       PERFORM IMS-GU-WDK629                                              
054100       IF SEGMENT-FINNS                                                   
054200         MOVE MED-4        TO MOD-TEMFSINF                                
054300         MOVE NEJ          TO NYCKLAR-SW                                  
054400       ELSE                                                               
054500         MOVE INF-NOT-REFILL TO MED-IDMFSINF                              
054600         CALL WMEDKONV USING MED-WMEDAREA                                 
054700         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
054800         MOVE NEJ          TO NYCKLAR-SW                                  
054900       END-IF                                                             
055000     END-IF                                                               
055100                                                                          
055200     IF NYCKLAR-FEL                                                       
055300       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
055400       CALL WMEDKONV USING MED-WMEDAREA                                   
055500       MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                  
055600       PERFORM MFS-RENSA-FAELT-IN                                         
055700       PERFORM MFS-RENSA-FAELT-UT                                         
055800     END-IF                                                               
055900     .                                                                    
056000     EJECT                                                                
056100 E-ENTER-TRYCKNING SECTION.                                               
056200                                                                          
056300     PERFORM IMS-GU-WDK711                                                
056400     IF SEGMENT-FINNS                                                     
056500       PERFORM EA-BEHANDLA                                                
056600                                                                          
056700     ELSE                                                                 
056800       MOVE ARTIKEL-SAKNAS-SDC TO MED-IDMFSFEL                            
056900       CALL WMEDKONV USING MED-WMEDAREA                                   
057000       MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                  
057100       PERFORM MFS-RENSA-FAELT-IN                                         
057200       PERFORM MFS-RENSA-FAELT-UT                                         
057300     END-IF                                                               
057400     .                                                                    
057500     EJECT                                                                
057600                                                                          
057700 EA-BEHANDLA SECTION.                                                     
057800                                                                          
057900     IF NOT EGEN-MID                                                      
058000     OR MID-IDARTNR-IN NOT = ALL '+'                                      
058100     OR MID-IDDC-IN NOT = ALL '+'                                         
058200                                                                          
058300******************************************************************        
058400*      -      HÄMTA SÄSONGSINDEX FRÅN HISTORIK MHA SUBPGM W271SEAS        
058500*      -      LÄGG UT HISTORIKINFO ÄVEN I SIMULERINGSDELEN                
058600*      -      HÄMTA INFO TILL VALID-DELEN FRÅN WDK711                     
058700*                       SLAG-RESEASON                                     
058800*                       SLAG-RESEASON * SLAG-KVPB-REF                     
058900******************************************************************        
059000                                                                          
059100       IF SLAG-DASPSEA > ZERO                                             
059200         MOVE SLAG-DASPSEA (3:6)                                          
059300                             TO MOD-DASPSEA                               
059400       ELSE                                                               
059500         MOVE MFS-RENSA-FAELT                                             
059600                             TO MOD-DASPSEA                               
059700       END-IF                                                             
059800       IF SLAG-TIMANSEA > ZERO                                            
059900         MOVE SLAG-TIMANSEA  TO MOD-TIMANSEA                              
060000       ELSE                                                               
060100         MOVE MFS-RENSA-FAELT                                             
060200                             TO MOD-TIMANSEA                              
060300       END-IF                                                             
060400                                                                          
060500       PERFORM IMS-GU-BENA01-BSEQ                                         
060600       IF SEGMENT-FINNS                                                   
060700         MOVE 'USA'          TO W-IDSKYLT                                 
060800         PERFORM IMS-GNP-BENA11                                           
060900         IF SEGMENT-FINNS                                                 
061000           MOVE BENA11-TEXT-BEART TO MOD-BEART-ENG                        
061100         ELSE                                                             
061200           MOVE MFS-RENSA-FAELT TO MOD-BEART-ENG                          
061300         END-IF                                                           
061400       END-IF                                                             
061500                                                                          
061600       MOVE MSGI-IDDC-KEY    TO SEAS-IDDC                                 
061700       MOVE W-IDARTNR        TO SEAS-IDARTNR                              
061800       MOVE NEJ              TO SEAS-FLKVARTAL                            
061900                                                                          
062000       CALL W271SEAS USING SEAS-W271SEAS WDK7-PCB WDL7-PCB                
062100                                WDL4-PCB WDK6-PCB WDB6-PCB                
062200                                                                          
062300       IF SEAS-KDSVAR = SPACE                                             
062400         MOVE SEAS-ANT-HIST-AR   TO MOD-ANT-HIST-AR                       
062500         MOVE SEAS-ANT-HIST-MAN  TO MOD-ANT-HIST-MAN                      
062600         MOVE SEAS-OSAKERHET     TO WS-OSAKERHET                          
062700         MOVE WS-OSAKERHET       TO MOD-OSAKERHET                         
062800         IF SEAS-SEASON-ARTIKEL = JA                                      
062900           MOVE 'Y'              TO MOD-SEASON-ARTIKEL                    
063000         ELSE                                                             
063100           MOVE 'N'              TO MOD-SEASON-ARTIKEL                    
063200         END-IF                                                           
063300                                                                          
063400         MOVE ZERO               TO WS-TOT-VALANT                         
063500         MOVE 1                  TO IX                                    
063600         PERFORM UNTIL IX > 12                                            
063700                                                                          
063800           COMPUTE WS-INDEX = 100 * SLAG-RESEASON (IX)                    
063900           MOVE WS-INDEX         TO MOD-VALIX (IX)                        
064000           COMPUTE WS-ANTAL ROUNDED = SLAG-RESEASON (IX) *                
064100                                    SLAG-KVPB-REF                         
064200                                                                          
064300           MOVE WS-ANTAL         TO MOD-VALANT (IX)                       
064400           ADD WS-ANTAL          TO WS-TOT-VALANT                         
064500           ADD 1                 TO IX                                    
064600         END-PERFORM                                                      
064700                                                                          
064800         MOVE WS-TOT-VALANT      TO MOD-TOT-VALANT                        
064900                                                                          
065000         MOVE ZERO               TO WS-TOT-SIMANT                         
065100                                      WS-TOT-HISANT                       
065200         MOVE 1                  TO IX                                    
065300         PERFORM UNTIL IX > 12                                            
065400                                                                          
065500           MOVE SEAS-RESEASON (IX) TO MOD-HISIX (IX)                      
065600                                      WS-MSGI-SIMIX (IX)                  
065700                                                                          
065800           MOVE SEAS-KVOI (IX)   TO WS-KVOI                               
065900           MOVE WS-KVOI          TO MOD-HISANT (IX)                       
066000           ADD WS-KVOI           TO WS-TOT-HISANT                         
066100           COMPUTE WS-MSGI-SIMANT (IX) ROUNDED =                          
066200*     MULTIPLICERA FÖRST FÖR ATT INTE TAPPA DECIMALER                     
066300*     WS-INDEX ÄR I %, DÄRFÖR / 100                                       
066400                   (WS-MSGI-SIMIX (IX) * SLAG-KVPB-REF) / 100             
066500           ADD WS-MSGI-SIMANT (IX)                                        
066600                               TO WS-TOT-SIMANT                           
066700           ADD 1                 TO IX                                    
066800         END-PERFORM                                                      
066900                                                                          
067000         MOVE WS-TOT-SIMANT      TO MOD-TOT-SIMANT                        
067100         MOVE WS-TOT-HISANT      TO MOD-TOT-HISANT                        
067200                                                                          
067300       ELSE                                                               
067400         MOVE ARTIKEL-SAKNAS-SDC TO MED-IDMFSFEL                          
067500         CALL WMEDKONV USING MED-WMEDAREA                                 
067600         MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                
067700         PERFORM MFS-RENSA-FAELT-IN                                       
067800         PERFORM MFS-RENSA-FAELT-UT                                       
067900       END-IF                                                             
068000                                                                          
068100     ELSE                                                                 
068200       IF MID-INPUT = ALL '+'                                             
068300                                                                          
068400         IF MFS-IDPFK = '1'                                               
068500*                                                                         
068600*           MFS-IDPFK = '1' ÄR LIKA MED PF17                              
068700*           ANVÄNDS FÖR ATT ÅTERSTÄLLA ALLA IX TILL 100                   
068800*           DVS EJ SÄSONG                                                 
068900*                                                                         
069000           MOVE 1                TO IX                                    
069100           PERFORM UNTIL IX > 12                                          
069200                                                                          
069300             MOVE 100            TO WS-MSGI-SIMIX (IX)                    
069400             COMPUTE WS-MSGI-SIMANT (IX) ROUNDED =                        
069500*       MULTIPLICERA FÖRST FÖR ATT INTE TAPPA DECIMALER                   
069600*       WS-INDEX ÄR I %, DÄRFÖR / 100                                     
069700                     (WS-MSGI-SIMIX (IX) * SLAG-KVPB-REF) / 100           
069800             ADD 1               TO IX                                    
069900           END-PERFORM                                                    
070000         END-IF                                                           
070100         PERFORM MFS-RENSA-FAELT-IN                                       
070200         PERFORM MFS-ROER-EJ-FAELT-IN                                     
070300         PERFORM MFS-ROER-EJ-FAELT-UT                                     
070400       ELSE                                                               
070500         PERFORM EAA-KTRL-INPUT                                           
070600         IF INDATA-OK                                                     
070700           PERFORM EAB-SIMULERA-INDEX                                     
070800           MOVE INF-PRESS-PF11 TO MED-IDMFSINF                            
070900           CALL WMEDKONV USING MED-WMEDAREA                               
071000           MOVE MED-TEMFSINF TO MOD-TEMFSINF                              
071100         END-IF                                                           
071200       END-IF                                                             
071300       PERFORM MFS-ROER-EJ-FAELT-UT                                       
071400     END-IF                                                               
071500     .                                                                    
071600     EJECT                                                                
071700 EAA-KTRL-INPUT SECTION.                                                  
071800                                                                          
071900     MOVE MFS-ADD-LAES-IN-FAELT                                           
072000                           TO MOD-DASPSEA-ATTR                            
072100     IF MID-DASPSEA = ALL '+'                                             
072200     OR MID-DASPSEA = ALL SPACE                                           
072300       MOVE MFS-RENSA-FAELT  TO MID-DASPSEA                               
072400                                MOD-DASPSEA                               
072500     ELSE                                                                 
072600       IF MID-DASPSEA(1:1) = SPACE                                        
072700       OR MID-DASPSEA(2:1) = SPACE                                        
072800       OR MID-DASPSEA(3:1) = SPACE                                        
072900       OR MID-DASPSEA(4:1) = SPACE                                        
073000       OR MID-DASPSEA(5:1) = SPACE                                        
073100       OR MID-DASPSEA(6:1) = SPACE                                        
073200         MOVE MID-DASPSEA    TO MOD-DASPSEA                               
073300         MOVE NEJ            TO INDATA-SW                                 
073400         MOVE MFS-ADD-LYS-UPP-FAELT                                       
073500                             TO MOD-DASPSEA-ATTR                          
073600         MOVE ERR-CORR-HILITE-FLDS                                        
073700                             TO MED-IDMFSFEL                              
073800         CALL WMEDKONV USING MED-WMEDAREA                                 
073900         MOVE MED-TEMFSFEL   TO MOD-TEMFSFEL                              
074000         MOVE MED-3          TO MOD-TEMFSINF                              
074100       ELSE                                                               
074200         MOVE 'AAMMDD'     TO DAT-KDDATFORM                               
074300         MOVE MID-DASPSEA  TO DAT-I-TIDATUM                               
074400                              MOD-DASPSEA                                 
074500                                                                          
074600                                                                          
074700         CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                  
074800                         DAT-O-TIDATUM DAT-KDSVAR                         
074900                                                                          
075000         IF NOT DAT-KDSVAR-OK                                             
075100           MOVE NEJ TO INDATA-SW                                          
075200           MOVE MFS-ADD-LAES-IN-FAELT-HI                                  
075300                           TO MOD-DASPSEA-ATTR                            
075400           MOVE ERR-CORR-HILITE-FLDS                                      
075500                             TO MED-IDMFSFEL                              
075600           CALL WMEDKONV USING MED-WMEDAREA                               
075700           MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                              
075800         END-IF                                                           
075900       END-IF                                                             
076000     END-IF                                                               
076100                                                                          
076200     MOVE 1                  TO IX                                        
076300     PERFORM UNTIL IX > 12                                                
076400       IF MID-SIMIX (IX) NOT = ALL '+'                                    
076500         MOVE JA             TO SIM-INDEX-SW                              
076600         IF MID-SIMIX (IX) NUMERIC                                        
076700           MOVE MID-SIMIX (IX)                                            
076800                         TO WS-MSGI-SIMIX (IX)                            
076900         ELSE                                                             
077000           MOVE ERR-CORR-HILITE-FLDS                                      
077100                             TO MED-IDMFSFEL                              
077200           CALL WMEDKONV USING MED-WMEDAREA                               
077300           MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                              
077400           MOVE MFS-ALFA-FAELT-FEL                                        
077500                         TO MOD-SIMIX-ATTR (IX)                           
077600           MOVE NEJ          TO INDATA-SW                                 
077700           PERFORM MFS-ROER-EJ-FAELT-IN                                   
077800           PERFORM MFS-ROER-EJ-FAELT-UT                                   
077900         END-IF                                                           
078000       END-IF                                                             
078100       IF INDATA-OK                                                       
078200         IF MID-SIMANT (IX) NOT = ALL '+'                                 
078300           MOVE JA             TO SIM-ANTAL-SW                            
078400           IF MID-SIMANT (IX) NUMERIC                                     
078500             MOVE MID-SIMANT (IX)                                         
078600                           TO WS-MSGI-SIMANT (IX)                         
078700           ELSE                                                           
078800             MOVE ERR-CORR-HILITE-FLDS                                    
078900                               TO MED-IDMFSFEL                            
079000             CALL WMEDKONV USING MED-WMEDAREA                             
079100             MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                            
079200             MOVE MFS-ALFA-FAELT-FEL                                      
079300                           TO MOD-SIMANT-ATTR (IX)                        
079400             MOVE NEJ          TO INDATA-SW                               
079500             PERFORM MFS-ROER-EJ-FAELT-IN                                 
079600             PERFORM MFS-ROER-EJ-FAELT-UT                                 
079700           END-IF                                                         
079800         END-IF                                                           
079900       END-IF                                                             
080000       ADD 1                 TO IX                                        
080100     END-PERFORM                                                          
080200                                                                          
080300     IF INDATA-OK                                                         
080400       IF SIM-ANTAL-JA                                                    
080500       AND SIM-INDEX-JA                                                   
080600****   INTE MÖJLIGT ATT SIMULERA MED BÅDE ANTAL OCH INDEX                 
080700*****  SAMTIDIGT                                                          
080800           MOVE NEJ TO INDATA-SW                                          
080900           MOVE CONFLICT  TO MED-IDMFSFEL                                 
081000           CALL WMEDKONV USING MED-WMEDAREA                               
081100           MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                              
081200           MOVE 1                  TO IX                                  
081300           PERFORM UNTIL IX > 12                                          
081400             IF MID-SIMIX (IX) NOT = ALL '+'                              
081500               MOVE MFS-ALFA-FAELT-FEL                                    
081600                          TO MOD-SIMIX-ATTR (IX)                          
081700             END-IF                                                       
081800             IF MID-SIMANT (IX) NOT = ALL '+'                             
081900               MOVE MFS-ALFA-FAELT-FEL                                    
082000                          TO MOD-SIMANT-ATTR (IX)                         
082100             END-IF                                                       
082200             ADD 1                 TO IX                                  
082300           END-PERFORM                                                    
082400           PERFORM MFS-ROER-EJ-FAELT-IN                                   
082500           PERFORM MFS-ROER-EJ-FAELT-UT                                   
082600                                                                          
082700       ELSE                                                               
082800         IF SIM-INDEX-JA                                                  
082900******** KOLLA ATT SUMMAN BLIR 1200                                       
083000           MOVE ZERO           TO WS-SIMIX-SUM                            
083100           MOVE 1              TO IX                                      
083200           PERFORM UNTIL IX > 12                                          
083300                                                                          
083400             ADD WS-MSGI-SIMIX (IX)                                       
083500                             TO WS-SIMIX-SUM                              
083600             ADD 1           TO IX                                        
083700           END-PERFORM                                                    
083800                                                                          
083900           IF WS-SIMIX-SUM NOT = 1200                                     
084000               MOVE NEJ TO INDATA-SW                                      
084100               MOVE WS-SIMIX-SUM                                          
084200                             TO MED-2-INDEX                               
084300               MOVE MED-2      TO MOD-TEMFSINF                            
084400               MOVE 1                TO IX                                
084500               PERFORM UNTIL IX > 12                                      
084600                 IF MID-SIMIX (IX) NOT = ALL '+'                          
084700                   MOVE MFS-ALFA-FAELT-FEL                                
084800                              TO MOD-SIMIX-ATTR (IX)                      
084900                 END-IF                                                   
085000                 ADD 1               TO IX                                
085100               END-PERFORM                                                
085200               PERFORM MFS-ROER-EJ-FAELT-IN                               
085300               PERFORM MFS-ROER-EJ-FAELT-UT                               
085400           END-IF                                                         
085500         END-IF                                                           
085600                                                                          
085700       END-IF                                                             
085800     END-IF                                                               
085900                                                                          
086000     .                                                                    
086100     EJECT                                                                
086200 EAB-SIMULERA-INDEX SECTION.                                              
086300                                                                          
086400     MOVE ZERO               TO WS-TOT-SIMANT                             
086500     MOVE 1                  TO IX                                        
086600     PERFORM UNTIL IX > 12                                                
086700       MOVE WS-MSGI-SIMANT (IX)                                           
086800                             TO WS-ANTAL                                  
086900       ADD WS-ANTAL          TO WS-TOT-SIMANT                             
087000       ADD 1                 TO IX                                        
087100     END-PERFORM                                                          
087200                                                                          
087300     IF SIM-ANTAL-JA                                                      
087400                                                                          
087500       MOVE 1                TO IX                                        
087600                                                                          
087700       PERFORM UNTIL IX > 12                                              
087800         MOVE WS-MSGI-SIMANT (IX)                                         
087900                             TO WS-ANTAL                                  
088000         COMPUTE WS-INDEX-DEC ROUNDED =                                   
088100                 WS-ANTAL / (WS-TOT-SIMANT / 12)                          
088200                  ON SIZE ERROR                                           
088300                      MOVE ZERO   TO WS-INDEX-DEC                         
088400         END-COMPUTE                                                      
088500         COMPUTE WS-MSGI-SIMIX (IX) ROUNDED =                             
088600                 WS-INDEX-DEC * 100                                       
088700         ADD 1               TO IX                                        
088800       END-PERFORM                                                        
088900                                                                          
089000       MOVE ZERO             TO WS-ANTAL                                  
089100       MOVE 1                TO IX                                        
089200       PERFORM UNTIL IX > 12                                              
089300         ADD WS-MSGI-SIMIX (IX)                                           
089400                               TO WS-ANTAL                                
089500         ADD 1               TO IX                                        
089600       END-PERFORM                                                        
089700                                                                          
089800****                                                                      
089900****   NORMERA SÄSONGSINDEXEN SÅ ATT TOTALEN BLIR 12 * 100                
090000****                                                                      
090100                                                                          
090200       IF WS-ANTAL > +1200                                                
090300         MOVE -1             TO WS-JUSTERA                                
090400       ELSE                                                               
090500         MOVE +1             TO WS-JUSTERA                                
090600       END-IF                                                             
090700                                                                          
090800       PERFORM UNTIL WS-ANTAL = +1200                                     
090900                                                                          
091000         MOVE 1              TO IX                                        
091100         PERFORM UNTIL WS-ANTAL = +1200                                   
091200         OR IX > 12                                                       
091300           ADD WS-JUSTERA    TO WS-MSGI-SIMIX (IX)                        
091400                                WS-ANTAL                                  
091500           ADD 1             TO IX                                        
091600         END-PERFORM                                                      
091700       END-PERFORM                                                        
091800                                                                          
091900                                                                          
092000     ELSE                                                                 
092100*      (SIM-INDEX-JA)                                                     
092200                                                                          
092300       MOVE 1                TO IX                                        
092400       MOVE ZERO             TO WS-TOT-SIMANT                             
092500                                                                          
092600       PERFORM UNTIL IX > 12                                              
092700                                                                          
092800         COMPUTE WS-MSGI-SIMANT (IX) ROUNDED =                            
092900*   MULTIPLICERA FÖRST FÖR ATT INTE TAPPA DECIMALER                       
093000*   WS-INDEX ÄR I %, DÄRFÖR / 100                                         
093100                 (WS-MSGI-SIMIX (IX) * SLAG-KVPB-REF) / 100               
093200         ADD WS-MSGI-SIMANT (IX)                                          
093300                             TO WS-TOT-SIMANT                             
093400                                                                          
093500         ADD 1               TO IX                                        
093600                                                                          
093700       END-PERFORM                                                        
093800                                                                          
093900     END-IF                                                               
094000     .                                                                    
094100     EJECT                                                                
094200 F-HAEMTA-INFO SECTION.                                                   
094300                                                                          
094400     IF SLAG-DASPSEA > ZERO                                               
094500       MOVE SLAG-DASPSEA (3:6)                                            
094600                             TO MOD-DASPSEA                               
094700     ELSE                                                                 
094800       MOVE MFS-RENSA-FAELT                                               
094900                             TO MOD-DASPSEA                               
095000     END-IF                                                               
095100     IF SLAG-TIMANSEA > ZERO                                              
095200       MOVE SLAG-TIMANSEA    TO MOD-TIMANSEA                              
095300     ELSE                                                                 
095400       MOVE MFS-RENSA-FAELT                                               
095500                             TO MOD-TIMANSEA                              
095600     END-IF                                                               
095700                                                                          
095800     PERFORM IMS-GU-BENA01-BSEQ                                           
095900     IF SEGMENT-FINNS                                                     
096000       MOVE 'USA'            TO W-IDSKYLT                                 
096100       PERFORM IMS-GNP-BENA11                                             
096200       IF SEGMENT-FINNS                                                   
096300         MOVE BENA11-TEXT-BEART                                           
096400                             TO MOD-BEART-ENG                             
096500       ELSE                                                               
096600         MOVE MFS-RENSA-FAELT                                             
096700                             TO MOD-BEART-ENG                             
096800       END-IF                                                             
096900     END-IF                                                               
097000                                                                          
097100     MOVE MSGI-IDDC-KEY      TO SEAS-IDDC                                 
097200     MOVE W-IDARTNR          TO SEAS-IDARTNR                              
097300     MOVE NEJ                TO SEAS-FLKVARTAL                            
097400                                                                          
097500     CALL W271SEAS USING SEAS-W271SEAS WDK7-PCB WDL7-PCB                  
097600                              WDL4-PCB WDK6-PCB WDB6-PCB                  
097700                                                                          
097800     IF SEAS-KDSVAR = SPACE                                               
097900       MOVE SEAS-ANT-HIST-AR     TO MOD-ANT-HIST-AR                       
098000       MOVE SEAS-ANT-HIST-MAN    TO MOD-ANT-HIST-MAN                      
098100       MOVE SEAS-OSAKERHET       TO WS-OSAKERHET                          
098200       MOVE WS-OSAKERHET         TO MOD-OSAKERHET                         
098300       IF SEAS-SEASON-ARTIKEL = JA                                        
098400         MOVE 'Y'                TO MOD-SEASON-ARTIKEL                    
098500       ELSE                                                               
098600         MOVE 'N'                TO MOD-SEASON-ARTIKEL                    
098700       END-IF                                                             
098800                                                                          
098900       MOVE ZERO                 TO WS-TOT-VALANT                         
099000       MOVE 1                    TO IX                                    
099100       PERFORM UNTIL IX > 12                                              
099200                                                                          
099300         COMPUTE WS-INDEX = 100 * SLAG-RESEASON (IX)                      
099400         MOVE WS-INDEX           TO MOD-VALIX (IX)                        
099500         COMPUTE WS-ANTAL ROUNDED = SLAG-RESEASON (IX) *                  
099600                                  SLAG-KVPB-REF                           
099700                                                                          
099800         MOVE WS-ANTAL           TO MOD-VALANT (IX)                       
099900         ADD WS-ANTAL            TO WS-TOT-VALANT                         
100000         ADD 1                   TO IX                                    
100100       END-PERFORM                                                        
100200                                                                          
100300       MOVE WS-TOT-VALANT        TO MOD-TOT-VALANT                        
100400                                                                          
100500       MOVE ZERO                 TO WS-TOT-SIMANT                         
100600                                    WS-TOT-HISANT                         
100700       MOVE 1                    TO IX                                    
100800       PERFORM UNTIL IX > 12                                              
100900                                                                          
101000         MOVE SEAS-RESEASON (IX) TO MOD-HISIX (IX)                        
101100                                                                          
101200         MOVE SEAS-KVOI (IX)     TO WS-KVOI                               
101300         MOVE WS-KVOI            TO MOD-HISANT (IX)                       
101400         ADD WS-KVOI             TO WS-TOT-HISANT                         
101500         ADD 1                   TO IX                                    
101600       END-PERFORM                                                        
101700                                                                          
101800       MOVE WS-TOT-HISANT        TO MOD-TOT-HISANT                        
101900                                                                          
102000     ELSE                                                                 
102100       MOVE ARTIKEL-SAKNAS-SDC TO MED-IDMFSFEL                            
102200       CALL WMEDKONV USING MED-WMEDAREA                                   
102300       MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                  
102400       PERFORM MFS-RENSA-FAELT-IN                                         
102500       PERFORM MFS-RENSA-FAELT-UT                                         
102600     END-IF                                                               
102700     .                                                                    
102800     EJECT                                                                
102900 G-KOLLA-INPUT SECTION.                                                   
103000                                                                          
103100     MOVE JA  TO INDATA-SW                                                
103200     MOVE MFS-ADD-LAES-IN-FAELT                                           
103300                             TO MOD-DASPSEA-ATTR                          
103400                                                                          
103500     PERFORM IMS-GU-WDK711                                                
103600     IF SEGMENT-SAKNAS                                                    
103700       MOVE NEJ              TO INDATA-SW                                 
103800       MOVE ARTIKEL-SAKNAS-SDC                                            
103900                             TO MED-IDMFSFEL                              
104000       CALL WMEDKONV USING MED-WMEDAREA                                   
104100       MOVE MED-TEMFSFEL     TO MOD-TEMFSFEL                              
104200       PERFORM MFS-RENSA-FAELT-IN                                         
104300       PERFORM MFS-RENSA-FAELT-UT                                         
104400                                                                          
104500     ELSE                                                                 
104600       IF MID-DASPSEA = ALL '+'                                           
104700       OR MID-DASPSEA = ALL SPACE                                         
104800         MOVE MFS-RENSA-FAELT                                             
104900                               TO MID-DASPSEA                             
105000                                  MOD-DASPSEA                             
105100       ELSE                                                               
105200         IF MID-DASPSEA(1:1) = SPACE                                      
105300         OR MID-DASPSEA(2:1) = SPACE                                      
105400         OR MID-DASPSEA(3:1) = SPACE                                      
105500         OR MID-DASPSEA(4:1) = SPACE                                      
105600         OR MID-DASPSEA(5:1) = SPACE                                      
105700         OR MID-DASPSEA(6:1) = SPACE                                      
105800           MOVE MID-DASPSEA    TO MOD-DASPSEA                             
105900           MOVE NEJ            TO INDATA-SW                               
106000           MOVE MFS-ADD-LYS-UPP-FAELT                                     
106100                               TO MOD-DASPSEA-ATTR                        
106200           MOVE ERR-CORR-HILITE-FLDS                                      
106300                               TO MED-IDMFSFEL                            
106400           CALL WMEDKONV USING MED-WMEDAREA                               
106500           MOVE MED-TEMFSFEL   TO MOD-TEMFSFEL                            
106600           MOVE MED-3          TO MOD-TEMFSINF                            
106700           PERFORM MFS-ROER-EJ-FAELT-UT                                   
106800                                                                          
106900         ELSE                                                             
107000           MOVE 'AAMMDD'       TO DAT-KDDATFORM                           
107100           MOVE MID-DASPSEA    TO DAT-I-TIDATUM                           
107200                                  MOD-DASPSEA                             
107300                                                                          
107400                                                                          
107500           CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                
107600                               DAT-O-TIDATUM DAT-KDSVAR                   
107700                                                                          
107800           IF NOT DAT-KDSVAR-OK                                           
107900               MOVE NEJ        TO INDATA-SW                               
108000               MOVE ERR-CORR-HILITE-FLDS                                  
108100                               TO MED-IDMFSFEL                            
108200               CALL WMEDKONV USING MED-WMEDAREA                           
108300               MOVE MED-TEMFSFEL                                          
108400                               TO MOD-TEMFSFEL                            
108500           END-IF                                                         
108600         END-IF                                                           
108700       END-IF                                                             
108800*      MOVE MFS-ADD-LAES-IN-FAELT                                         
108900*                            TO MOD-DASPSEA-ATTR                          
109000                                                                          
109100********   KOLLA ATT SUMMAN BLIR 1200                                     
109200       MOVE ZERO             TO WS-SIMIX-SUM                              
109300       MOVE 1                TO IX                                        
109400       PERFORM UNTIL IX > 12                                              
109500         ADD WS-MSGI-SIMIX (IX)                                           
109600                             TO WS-SIMIX-SUM                              
109700         ADD 1               TO IX                                        
109800       END-PERFORM                                                        
109900                                                                          
110000       IF WS-SIMIX-SUM NOT = 1200                                         
110100           MOVE NEJ TO INDATA-SW                                          
110200           MOVE WS-SIMIX-SUM TO MED-2-INDEX                               
110300           MOVE MED-2        TO MOD-TEMFSINF                              
110400           MOVE 1            TO IX                                        
110500           PERFORM UNTIL IX > 12                                          
110600             MOVE MFS-ALFA-FAELT-FEL                                      
110700                             TO MOD-SIMIX-ATTR (IX)                       
110800             ADD 1           TO IX                                        
110900           END-PERFORM                                                    
111000           PERFORM MFS-ROER-EJ-FAELT-IN                                   
111100           PERFORM MFS-ROER-EJ-FAELT-UT                                   
111200       END-IF                                                             
111300     END-IF                                                               
111400     .                                                                    
111500     EJECT                                                                
111600 H-UPPDATERA SECTION.                                                     
111700                                                                          
111800     PERFORM IMS-GHU-WDK711                                               
111900                                                                          
112000     MOVE 'AAMMDD'           TO DAT-KDDATFORM                             
112100     MOVE MOD-DASPSEA        TO DAT-I-TIDATUM                             
112200                                                                          
112300     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
112400                     DAT-O-TIDATUM DAT-KDSVAR                             
112500                                                                          
112600     IF DAT-KDSVAR-OK                                                     
112700       MOVE DAT-TISEKEL      TO SLAG-DASPSEA (1:2)                        
112800       MOVE MOD-DASPSEA      TO SLAG-DASPSEA (3:6)                        
112900     ELSE                                                                 
113000       MOVE ZERO             TO SLAG-DASPSEA                              
113100     END-IF                                                               
113200     MOVE 1                  TO IX                                        
113300     MOVE ZERO               TO WS-TOT-VALANT                             
113400                                                                          
113500     PERFORM UNTIL IX > 12                                                
113600                                                                          
113700       MOVE WS-MSGI-SIMIX (IX)                                            
113800                             TO WS-INDEX                                  
113900                                MOD-VALIX (IX)                            
114000       COMPUTE SLAG-RESEASON (IX) = WS-INDEX / 100                        
114100       COMPUTE WS-ANTAL ROUNDED = SLAG-RESEASON (IX) *                    
114200                                  SLAG-KVPB-REF                           
114300       MOVE WS-ANTAL         TO MOD-VALANT (IX)                           
114400       ADD WS-ANTAL          TO WS-TOT-VALANT                             
114500       ADD 1                 TO IX                                        
114600     END-PERFORM                                                          
114700                                                                          
114800     IF NOT   (SLAG-RESEASON (1) = 1.00                                   
114900     AND       SLAG-RESEASON (2) = 1.00                                   
115000     AND       SLAG-RESEASON (3) = 1.00                                   
115100     AND       SLAG-RESEASON (4) = 1.00                                   
115200     AND       SLAG-RESEASON (5) = 1.00                                   
115300     AND       SLAG-RESEASON (6) = 1.00                                   
115400     AND       SLAG-RESEASON (7) = 1.00                                   
115500     AND       SLAG-RESEASON (8) = 1.00                                   
115600     AND       SLAG-RESEASON (9) = 1.00                                   
115700     AND       SLAG-RESEASON (10) = 1.00                                  
115800     AND       SLAG-RESEASON (11) = 1.00                                  
115900     AND       SLAG-RESEASON (12) = 1.00)                                 
116000     AND       SLAG-FLREFBEO = JA                                         
116100       MOVE NEJ              TO SLAG-FLREFBEO                             
116200     END-IF                                                               
116300                                                                          
116400     MOVE WS-TOT-VALANT      TO MOD-TOT-VALANT                            
116500                                                                          
116600     MOVE DAGENS-DATUM       TO SLAG-TIMANSEA                             
116700                                                                          
116800     IF  SEGMENT-FINNS                                                    
116900     AND SLAG-FLREFNYO = JA                                               
117000         MOVE NEJ            TO SLAG-FLREFNYO                             
117100     END-IF                                                               
117200     PERFORM IMS-REPL-WDK711                                              
117300                                                                          
117400*    CALCULATE REFILLING POINTS BASED ON SEASON AND UPDATE WDK7           
117500     PERFORM IMS-GHU-WDK711                                               
117600     PERFORM HA-BERAKNA-REFPKT                                            
117700                                                                          
117800     PERFORM IMS-REPL-WDK711                                              
117900                                                                          
118000     MOVE INF-UPDATE-DONE    TO MED-IDMFSINF                              
118100     CALL WMEDKONV USING MED-WMEDAREA                                     
118200     MOVE MED-TEMFSINF       TO MOD-TEMFSINF                              
118300*    PERFORM MFS-FORM-ATTR                                                
118400*    PERFORM MFS-RENSA-FAELT-IN                                           
118500*    PERFORM MFS-ROER-EJ-FAELT-UT-2                                       
118600     .                                                                    
118700     EJECT                                                                
118800 HA-BERAKNA-REFPKT SECTION.                                               
118900                                                                          
119000     INITIALIZE  REFL-W271REFL                                            
119100     MOVE SLAG-TIREFPKT      TO TMP1-YYMMDD                               
119200     MOVE DAGENS-DATUM       TO TMP2-YYMMDD                               
119300     PERFORM WY2000P1                                                     
119400     IF TMP1-YYMMDD         >= TMP2-YYMMDD                                
119500                                                                          
119600*                                                                         
119700*--- KVREFPKT FRÅN BASEN GÄLLER PGA MANUELLT DATUM ÄR SATT                
119800*                                                                         
119900       CONTINUE                                                           
120000                                                                          
120100     ELSE                                                                 
120200       MOVE ZERO             TO REFL-NDC-KVDAGAR-TBT-DC                   
120300                                                                          
120400       MOVE W-IDDC           TO REFL-IDDC                                 
120500       MOVE W-IDARTNR        TO REFL-IDARTNR                              
120600       MOVE SLAG-IDDC-REF    TO REFL-IDDC-REF                             
120700       MOVE SLAG-IDREFTAB                                                 
120800                             TO REFL-IDREFTAB                             
120900       MOVE SLAG-FLWILSON                                                 
121000                             TO REFL-FLWILSON                             
121100                                                                          
121200       MOVE SLAG-FLFLYG      TO REFL-FLFLYG                               
121300                                                                          
121400       PERFORM HAA-GET-BESPRIS                                            
121500***FÖR KINA HAR MAN FLYTTAT MATERIALPRIS TILL WS-PRARTBES                 
121600       MOVE WS-PRARTBES      TO REFL-PRARTBES                             
121700       MOVE 1                TO IX                                        
121800       PERFORM UNTIL IX > 12                                              
121900          MOVE SLAG-RESEASON(IX)                                          
122000                             TO REFL-RESEASON(IX)                         
122100          ADD 1              TO IX                                        
122200       END-PERFORM                                                        
122300                                                                          
122400       MOVE SLAG-IDLEVNR                                                  
122500                             TO REFL-IN-IDLEVNR-DC                        
122600                                                                          
122700       MOVE SLAG-TIREFPKT    TO TMP1-YYMMDD                               
122800       MOVE DAGENS-DATUM     TO TMP2-YYMMDD                               
122900       PERFORM WY2000P1                                                   
123000       IF        TMP1-YYMMDD >= TMP2-YYMMDD                               
123100         MOVE SLAG-KVREFPKT  TO REFL-IN-KVREFPKT                          
123200       ELSE                                                               
123300         MOVE ZERO           TO REFL-IN-KVREFPKT                          
123400       END-IF                                                             
123500                                                                          
123600       MOVE SLAG-TIREFPAF    TO TMP1-YYMMDD                               
123700       MOVE DAGENS-DATUM     TO TMP2-YYMMDD                               
123800       PERFORM WY2000P1                                                   
123900       IF TMP1-YYMMDD >= TMP2-YYMMDD                                      
124000         MOVE SLAG-KVREFBER                                               
124100                             TO REFL-IN-KVREFBER                          
124200       ELSE                                                               
124300         MOVE ZERO           TO REFL-IN-KVREFBER                          
124400       END-IF                                                             
124500                                                                          
124600       IF SLAG-IDDC-REF NOT = SPACES                                      
124700          MOVE ZERO                 TO REFL-NDC-KVDAGAR-TBT-DC            
124800       ELSE                                                               
124900          IF SLAG-KVDAGAR-MANLT > ZERO                                    
125000            MOVE SLAG-KVDAGAR-MANLT TO REFL-NDC-KVDAGAR-TBT-DC            
125100          ELSE                                                            
125200            MOVE SLAG-IDLEVNR       TO W-IDLEVNR                          
125300            PERFORM IMS-GU-LEVA16                                         
125400            IF SEGMENT-FINNS                                              
125500               MOVE NDC-KVDAGAR-TBT TO REFL-NDC-KVDAGAR-TBT-DC            
125600            ELSE                                                          
125700               MOVE 1               TO REFL-NDC-KVDAGAR-TBT-DC            
125800            END-IF                                                        
125900          END-IF                                                          
126000       END-IF                                                             
126100                                                                          
126200       INITIALIZE UTUP-W271UTUP                                           
126300       MOVE 004                     TO UTUP-KDCALL                        
126400       MOVE W-IDARTNR               TO UTUP-IDARTNR                       
126500       MOVE W-IDDC                  TO UTUP-IDDC                          
126600       MOVE SLAG-IDDC-REF           TO UTUP-IDDC-REF                      
126700       MOVE REFL-NDC-KVDAGAR-TBT-DC                                       
126800                                    TO UTUP-LEADTIME                      
126900                                                                          
127000       CALL W271UTUP USING UTUP-W271UTUP                                  
127100                           UTUP1-WDK7-PCB                                 
127200                           UTUP1-WDB6-PCB                                 
127300                           UTUP1-UTIL-WDK6-PCB                            
127400                           UTUP1-UTIL-WDK7-PCB                            
127500                           UTUP1-UTIL-WDB6-PCB                            
127600       IF UTUP-KDSVAR-OK                                                  
127700          MOVE UTUP-LEADTID-BEHOV   TO REFL-IN-LEADTID-BEHOV              
127800       ELSE                                                               
127900          DISPLAY 'W271UTUP-ERROR :' UTUP-TEXT                            
128000          CALL FELLOG                                                     
128100       END-IF                                                             
128200                                                                          
128300                                                                          
128400       CALL W271REFL USING REFL-W271REFL                                  
128500                           REFL-2501-PCB                                  
128600                           WDB6-PCB                                       
128700                           WDK72-PCB                                      
128800                           UTIL-WDK6-PCB                                  
128900                           UTIL-WDK7-PCB                                  
129000                           UTIL-WDB6-PCB                                  
129100                                                                          
129200       MOVE REFL-KVREFPKT           TO SLAG-KVREFPKT                      
129300     END-IF                                                               
129400     .                                                                    
129500     EJECT                                                                
129600                                                                          
129700 HAA-GET-BESPRIS SECTION.                                                 
129800                                                                          
129900     PERFORM IMS-GU-WDK611                                                
130000                                                                          
130100     IF NDC-CN OR NDC-NA                                                  
130200       MOVE DCS-IDLANDX2    TO W-IDLAND                                   
130300       PERFORM IMS-GU-WDK712                                              
130400       MOVE LART-PRMATRL    TO WS-PRARTBES                                
130500     ELSE                                                                 
130600       MOVE CLAG-PRARTSTD         TO WS-PRARTBES                          
130700*      MOVE FUNCTION CURRENT-DATE(1:8) TO DAGENS-AAAAMMDD                 
130800*      COMPUTE W-DAPRLIST = 99999999 - DAGENS-AAAAMMDD                    
130900*      PERFORM IMS-GNP-WDK621                                             
131000*      IF SEGMENT-SAKNAS                                                  
131100*        MOVE CLAG-PRARTSTD       TO WS-PRARTBES                          
131200*      ELSE                                                               
131300*        MOVE NEJ                 TO FL-PRARTBES                          
131400*        PERFORM UNTIL  SEGMENT-SAKNAS                                    
131500*          IF PRL-SUINLEV-PR > ZERO                                       
131600*            MOVE PRL-PRARTBES-PR  TO WS-PRARTBES                         
131700*            SET SEGMENT-SAKNAS TO TRUE                                   
131800*          ELSE                                                           
131900*            IF FL-PRARTBES = NEJ                                         
132000*              MOVE PRL-PRARTBES-PR TO WS-PRARTBES                        
132100*              MOVE JA              TO FL-PRARTBES                        
132200*            END-IF                                                       
132300*            PERFORM IMS-GNP-WDK621                                       
132400*          END-IF                                                         
132500*        END-PERFORM                                                      
132600*      END-IF                                                             
132700     END-IF                                                               
132800     .                                                                    
132900     EJECT                                                                
133000 MFS-RENSA-FAELT-UT SECTION.                                              
133100                                                                          
133200*    --- ALLA UTDATA-FÄLT                                                 
133300     MOVE MFS-RENSA-FAELT TO MOD-TIMANSEA                                 
133400                             MOD-BEART-ENG                                
133500                             MOD-OSAKERHET                                
133600                             MOD-ANT-HIST-AR                              
133700                             MOD-ANT-HIST-MAN                             
133800*                            MOD-TEMFSINF                                 
133900                                                                          
134000     MOVE 1                  TO IX                                        
134100     PERFORM UNTIL IX > 12                                                
134200       MOVE MFS-RENSA-FAELT TO MOD-VALIX (IX)                             
134300                               MOD-VALANT (IX)                            
134400       ADD 1                 TO IX                                        
134500     END-PERFORM                                                          
134600                                                                          
134700     MOVE 1                  TO IX                                        
134800     PERFORM UNTIL IX > 12                                                
134900       MOVE MFS-RENSA-FAELT TO MOD-HISIX (IX)                             
135000                               MOD-HISANT (IX)                            
135100       ADD 1                 TO IX                                        
135200     END-PERFORM                                                          
135300     .                                                                    
135400     SKIP3                                                                
135500 MFS-RENSA-FAELT-IN SECTION.                                              
135600                                                                          
135700*    --- ALLA INDATA-FÄLT                                                 
135800     MOVE MFS-RENSA-FAELT    TO MOD-DASPSEA                               
135900                                                                          
136000     MOVE 1                  TO IX                                        
136100     PERFORM UNTIL IX > 12                                                
136200       MOVE MFS-RENSA-FAELT TO MOD-SIMIX (IX)                             
136300                               MOD-SIMANT (IX)                            
136400       ADD 1                 TO IX                                        
136500     END-PERFORM                                                          
136600     .                                                                    
136700     EJECT                                                                
136800 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
136900                                                                          
137000*    --- ALLA UTDATA-FÄLT                                                 
137100                                                                          
137200     MOVE MFS-ROER-EJ-FAELT  TO MOD-BEART-ENG                             
137300                                MOD-TIMANSEA                              
137400                                MOD-SEASON-ARTIKEL                        
137500                                MOD-OSAKERHET                             
137600                                MOD-ANT-HIST-AR                           
137700                                MOD-ANT-HIST-MAN                          
137800                                MOD-TOT-VALANT                            
137900                                MOD-TOT-SIMANT                            
138000                                MOD-TOT-HISANT                            
138100                                                                          
138200     MOVE 1                  TO IX                                        
138300     PERFORM UNTIL IX > 12                                                
138400       MOVE MFS-ROER-EJ-FAELT  TO MOD-VALIX (IX)                          
138500                                  MOD-VALANT (IX)                         
138600       ADD 1                 TO IX                                        
138700     END-PERFORM                                                          
138800                                                                          
138900     MOVE 1                  TO IX                                        
139000     PERFORM UNTIL IX > 12                                                
139100       MOVE MFS-ROER-EJ-FAELT  TO MOD-HISIX (IX)                          
139200                                  MOD-HISANT (IX)                         
139300       ADD 1                 TO IX                                        
139400     END-PERFORM                                                          
139500     .                                                                    
139600     SKIP3                                                                
139700*MFS-ROER-EJ-FAELT-UT-2  SECTION.                                         
139800                                                                          
139900*    --- ALLA UTDATA-FÄLT (EXKL. VALID)                                   
140000*                                                                         
140100*    MOVE MFS-ROER-EJ-FAELT  TO MOD-BEART-ENG                             
140200*                               MOD-TIMANSEA                              
140300*                               MOD-OSAKERHET                             
140400*                               MOD-ANT-HIST-AR                           
140500*                               MOD-ANT-HIST-MAN                          
140600*                               MOD-TOT-SIMANT                            
140700*                               MOD-TOT-HISANT                            
140800*                                                                         
140900*    MOVE 1                  TO IX                                        
141000*    PERFORM UNTIL IX > 12                                                
141100*      MOVE MFS-ROER-EJ-FAELT  TO MOD-HISIX (IX)                          
141200*                                 MOD-HISANT (IX)                         
141300*      ADD 1                 TO IX                                        
141400*    END-PERFORM                                                          
141500*    .                                                                    
141600     SKIP3                                                                
141700 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
141800                                                                          
141900*    --- ALLA INDATA-FÄLT                                                 
142000     MOVE MFS-ROER-EJ-FAELT  TO MOD-DASPSEA                               
142100                                                                          
142200     MOVE 1                  TO IX                                        
142300     PERFORM UNTIL IX > 12                                                
142400       MOVE MFS-ROER-EJ-FAELT TO MOD-SIMIX (IX)                           
142500                                  MOD-SIMANT (IX)                         
142600       ADD 1                 TO IX                                        
142700     END-PERFORM                                                          
142800     .                                                                    
142900     EJECT                                                                
143000*MFS-FORM-ATTR SECTION.                                                   
143100*                                                                         
143200*    --- ALLA INDATA-FÄLT                                                 
143300*    MOVE MFS-FORMATETS-ATTR TO MOD-DASPSEA-ATTR                          
143400*                                                                         
143500*    MOVE 1                  TO IX                                        
143600*    PERFORM UNTIL IX > 12                                                
143700*      MOVE MFS-FORMATETS-ATTR  TO MOD-SIMIX-ATTR (IX)                    
143800*                                  MOD-SIMANT-ATTR (IX)                   
143900*      ADD 1                 TO IX                                        
144000*    END-PERFORM                                                          
144100*    .                                                                    
144200     EJECT                                                                
144300* --- IMS SEKTIONER ---                                                   
144400     SKIP3                                                                
144500 IMS-GET-MSG SECTION.                                                     
144600                                                                          
144700     MOVE '  QC' TO GODK-STATUSKODER                                      
144800     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
144900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
145000     PERFORM IMS-STATUSKONTROLL                                           
145100     .                                                                    
145200     SKIP3                                                                
145300 IMS-INSERT-MSG SECTION.                                                  
145400                                                                          
145500     IF ENGLISH-TEXT                                                      
145600       MOVE 'N' TO MFS-KDHUVOMR                                           
145700     END-IF                                                               
145800     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
145900     MOVE SPACE TO GODK-STATUSKODER                                       
146000     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
146100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
146200     PERFORM IMS-STATUSKONTROLL                                           
146300     .                                                                    
146400     EJECT                                                                
146500 IMS-GU-WDK711 SECTION.                                                   
146600                                                                          
146700     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
146800          DELIMITED BY SIZE INTO SSA1                                     
146900     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
147000          DELIMITED BY SIZE INTO SSA2                                     
147100     MOVE '  GE' TO GODK-STATUSKODER                                      
147200     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711  SSA1 SSA2              
147300     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
147400     PERFORM IMS-STATUSKONTROLL                                           
147500     .                                                                    
147600     SKIP3                                                                
147700 IMS-GHU-WDK711 SECTION.                                                  
147800                                                                          
147900     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
148000          DELIMITED BY SIZE INTO SSA1                                     
148100     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
148200          DELIMITED BY SIZE INTO SSA2                                     
148300     MOVE '  ' TO GODK-STATUSKODER                                        
148400     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-WDK711  SSA1 SSA2             
148500     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
148600     PERFORM IMS-STATUSKONTROLL                                           
148700     .                                                                    
148800     SKIP3                                                                
148900 IMS-REPL-WDK711 SECTION.                                                 
149000                                                                          
149100     MOVE '  ' TO GODK-STATUSKODER                                        
149200     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK711                       
149300     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
149400     PERFORM IMS-STATUSKONTROLL                                           
149500     .                                                                    
149600     EJECT                                                                
149700 IMS-GU-WDK712   SECTION.                                                 
149800                                                                          
149900     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
150000          DELIMITED BY SIZE INTO SSA1                                     
150100     STRING 'WDK712  (IDLAND   =' W-IDLAND-X ')'                          
150200          DELIMITED BY SIZE INTO SSA2                                     
150300     MOVE '    ' TO GODK-STATUSKODER                                      
150400     CALL CBLTDLI USING GU WDK72-PCB DLI-IO-WDK712                        
150500          SSA1 SSA2                                                       
150600     MOVE WDK72-STATUS-CODE TO STATUS-WS                                  
150700     PERFORM IMS-STATUSKONTROLL                                           
150800     .                                                                    
150900     EJECT                                                                
151000                                                                          
151100 IMS-GU-BENA01-BSEQ SECTION.                                              
151200                                                                          
151300     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
151400          DELIMITED BY SIZE INTO SSA1                                     
151500     MOVE '  GE' TO GODK-STATUSKODER                                      
151600     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA-D3 SSA1                   
151700     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
151800     PERFORM IMS-STATUSKONTROLL                                           
151900     .                                                                    
152000     SKIP3                                                                
152100 IMS-GNP-BENA11 SECTION.                                                  
152200                                                                          
152300     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
152400          DELIMITED BY SIZE INTO SSA1                                     
152500     MOVE '  GE' TO GODK-STATUSKODER                                      
152600     CALL CBLTDLI USING GNP BENA-PCB DLI-IO-AREA-D3 SSA1                  
152700     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
152800     PERFORM IMS-STATUSKONTROLL                                           
152900     .                                                                    
153000     EJECT                                                                
153100 IMS-GU-LEVA16 SECTION.                                                   
153200                                                                          
153300     STRING 'WLLEVA01(IDLEVNR  =' W-IDLEVNR-X ')'                         
153400          DELIMITED BY SIZE INTO SSA1                                     
153500     STRING 'WLLEVA16(IDDC     =' W-IDDC-X ')'                            
153600          DELIMITED BY SIZE INTO SSA2                                     
153700     MOVE '  GE' TO GODK-STATUSKODER                                      
153800     CALL CBLTDLI USING GU LEVA-PCB DLI-IO-LEVA16 SSA1 SSA2               
153900     MOVE LEVA-STATUS-CODE TO STATUS-WS                                   
154000     PERFORM IMS-STATUSKONTROLL                                           
154100     .                                                                    
154200     EJECT                                                                
154300 IMS-GU-WDK611 SECTION.                                                   
154400     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
154500          DELIMITED BY SIZE INTO SSA1                                     
154600     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
154700          DELIMITED BY SIZE INTO SSA2                                     
154800     MOVE '  ' TO GODK-STATUSKODER                                        
154900     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-AREA-WDK611 SSA1 SSA2          
155000     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
155100     PERFORM IMS-STATUSKONTROLL                                           
155200     .                                                                    
155300     EJECT                                                                
155400 IMS-GNP-WDK621 SECTION.                                                  
155500     STRING 'WDK621  (DAPRLIST=>' W-DAPRLIST-X ')'                        
155600          DELIMITED BY SIZE INTO SSA1                                     
155700     MOVE '  GE' TO GODK-STATUSKODER                                      
155800     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-AREA-WDK621 SSA1              
155900     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
156000     PERFORM IMS-STATUSKONTROLL                                           
156100     .                                                                    
156200     EJECT                                                                
156300                                                                          
156400 IMS-GU-WDK629      SECTION.                                              
156500     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
156600          DELIMITED BY SIZE INTO SSA1                                     
156700     MOVE 'WDK611  '       TO SSA2                                        
156800     MOVE 'WDK629  '       TO SSA3                                        
156900     MOVE '  GE' TO GODK-STATUSKODER                                      
157000     CALL CBLTDLI USING GU WDK62-PCB DLI-IO-AREA-WDK629                   
157100                                    SSA1 SSA2 SSA3                        
157200     MOVE WDK62-STATUS-CODE TO STATUS-WS                                  
157300     PERFORM IMS-STATUSKONTROLL                                           
157400     .                                                                    
157500     SKIP3                                                                
157600                                                                          
157700 IMS-GU-WDB601    SECTION.                                                
157800     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
157900          DELIMITED BY SIZE INTO SSA1                                     
158000     MOVE '  GE' TO GODK-STATUSKODER                                      
158100     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
158200     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
158300     PERFORM IMS-STATUSKONTROLL                                           
158400     IF SEGMENT-SAKNAS                                                    
158500         MOVE SPACE TO DCS-KDDC                                           
158600     END-IF                                                               
158700     .                                                                    
158800 IMS-STATUSKONTROLL SECTION.                                              
158900                                                                          
159000     SET STATUS-IX TO 1                                                   
159100     SEARCH GODK-STATUS                                                   
159200       AT END                                                             
159300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
159400         DELIMITED BY SIZE INTO FELTEXT                                   
159500         CALL FELLOG                                                      
159600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
159700         CONTINUE                                                         
159800     END-SEARCH                                                           
159900     .                                                                    
160000     EJECT                                                                
160100*    -COPY WY2000P1                                                       
