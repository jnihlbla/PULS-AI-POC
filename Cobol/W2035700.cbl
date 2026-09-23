000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W2035700.                                                
000400 AUTHOR.         STEFAN KIHLBERG.                                         
000500 DATE-WRITTEN.   96/10/29.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        UPPDATERAR DATAELEMENT SOM STYR REFILL                           
001000*                                                                         
001100*        PROGRAMMET UPPDATERAR WDK6                                       
001200*                              WDK7                                       
001300*                                                                         
001400*    INDATA.                                                              
001500*        TRANSAKTION: W2T357                                              
001600*        MID:         W2I35701                                            
001700*                                                                         
001800*    UTDATA.                                                              
001900*        MOD:         W2O35701                                            
002000                                                                          
002100     SKIP3                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     EJECT                                                                
002400 DATA DIVISION.                                                           
002500 WORKING-STORAGE SECTION.                                                 
002600                                                                          
002700*    -- CHECKED BY WY2000                                                 
002800 77  IDPGM                       PIC X(08)   VALUE 'W2035700'.            
002900                                                                          
003000*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003100 01  FELTEXT.                                                             
003200     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
003300     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
003400     EJECT                                                                
003500                                                                          
003600                                                                          
003700 77  JA                          PIC X       VALUE 'J'.                   
003800 77  YES                         PIC X       VALUE 'Y'.                   
003900 77  NEJ                         PIC X       VALUE 'N'.                   
004000 77  AKTIV                       PIC X       VALUE 'A'.                   
004100                                                                          
004200*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
004300                                                                          
004400 77  INDATA-SW                   PIC X       VALUE 'J'.                   
004500     88  INDATA-OK                           VALUE 'J'.                   
004600     88  INDATA-FEL                          VALUE 'N'.                   
004700                                                                          
004800 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
004900     88  NYCKLAR-OK                          VALUE 'J'.                   
005000     88  NYCKLAR-FEL                         VALUE 'N'.                   
005100                                                                          
005200 77  SW-IDPSN-CDC                PIC X       VALUE 'N'.                   
005300     88  IDPSN-CDC-JA                        VALUE 'J'.                   
005400     88  IDPSN-CDC-NEJ                       VALUE 'N'.                   
005500                                                                          
005600 77  SW-NDC-US-UPD-IDPSN         PIC X       VALUE 'N'.                   
005700     88  NDC-US-UPD-IDPSN-JA                 VALUE 'J'.                   
005800     88  NDC-US-UPD-IDPSN-NEJ                VALUE 'N'.                   
005900                                                                          
006000 77  SW-NDC-CA-UPD-IDPSN         PIC X       VALUE 'N'.                   
006100     88  NDC-CA-UPD-IDPSN-JA                 VALUE 'J'.                   
006200     88  NDC-CA-UPD-IDPSN-NEJ                VALUE 'N'.                   
006300                                                                          
006400 77  SW-NDC-US-UPD-DAPUBL        PIC X       VALUE 'N'.                   
006500     88  NDC-US-UPD-DAPUBL-JA                VALUE 'J'.                   
006600     88  NDC-US-UPD-DAPUBL-NEJ               VALUE 'N'.                   
006700                                                                          
006800 77  SW-NDC-CA-UPD-DAPUBL        PIC X       VALUE 'N'.                   
006900     88  NDC-CA-UPD-DAPUBL-JA                VALUE 'J'.                   
007000     88  NDC-CA-UPD-DAPUBL-NEJ               VALUE 'N'.                   
007100                                                                          
007110 77  SW-LYNK-PART                PIC X       VALUE 'N'.                   
007120     88  LYNK-PART                           VALUE 'J'.                   
007130                                                                          
007200 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
007300     88  EGEN-MID                            VALUE '2357'.                
007400     88  GODK-MID                            VALUE '2351' '2352'          
007500                                                   '2353' '2354'          
007600                                                   '2355' '2356'          
007700                                                   '2357' '2358'          
007800                                                   '2359'.                
007900     88  HELP-MID                            VALUE '0551'.                
008000     EJECT                                                                
008100                                                                          
008200 01  ARBETSFALT.                                                          
008300     03 IX                       PIC 9(2)    VALUE ZERO.                  
008400     03 IX1                      PIC 9(2)    VALUE ZERO.                  
008500     03 IX2                      PIC 9(2)    VALUE ZERO.                  
008600     03 IDDC-WS                  PIC X(2)    VALUE SPACE.                 
008700     03 WS-VECKA                 PIC 9(2)    VALUE ZERO.                  
008800     03 IX-DC                    PIC 9(3)    VALUE ZERO.                  
008900     03 IX-DC-N                  PIC 9(3)    VALUE ZERO.                  
009000     03 IX-DC-MAX                PIC 9(3)    VALUE ZERO.                  
009100                                                                          
009200 01  WS-FLAG-K711.                                                        
009300     03  WS-FINNS-NA             PIC X       OCCURS 4.                    
009400     03  WS-ISRT-NA              PIC X       OCCURS 4.                    
009500 01  WS-DC-TABELL.                                                        
009600     03  WS-DC-1                 PIC X(2)    VALUE SPACES.                
009700     03  WS-DC-2                 PIC X(2)    VALUE SPACES.                
009800     03  WS-DC-3                 PIC X(2)    VALUE SPACES.                
009900     03  WS-DC-4                 PIC X(2)    VALUE SPACES.                
010000 01  FILLER REDEFINES  WS-DC-TABELL.                                      
010100     03  WS-DC-NR                PIC X(2)    OCCURS 4.                    
010200*                                                                         
010300 01  WS-TIFINLV                  PIC 9(8)    VALUE ZERO.                  
010400 01  FILLER REDEFINES WS-TIFINLV.                                         
010500     03 WS-TIFINLV-SS            PIC 9(2).                                
010600     03 WS-TIFINLV-AAMMDD        PIC 9(6).                                
010700                                                                          
010800 01  WS-DAPUBL                   PIC 9(8)    VALUE ZERO.                  
010900 01  FILLER REDEFINES WS-DAPUBL.                                          
011000     03 WS-DAPUBL-SS             PIC 9(2).                                
011100     03 WS-DAPUBL-AAMMDD         PIC 9(6).                                
011200                                                                          
011300 01  DATUMFALT.                                                           
011400     03 DAGENS-DATUM             PIC S9(7)   VALUE ZERO COMP-3.           
011401                                                                          
011410 01  WS-DAGENS-PLUS2-AAR         PIC 9(8)    VALUE ZERO.                  
011420 01  FILLER REDEFINES WS-DAGENS-PLUS2-AAR.                                
011430     03 WS-DAGENS-PLUS-SS        PIC 9(2).                                
011440     03 WS-DAGENS-PLUS2-AAMMDD   PIC 9(6).                                
011500                                                                          
011600 01  WS-IDPERSON-BUY             PIC S9(3)   VALUE ZERO COMP-3.           
011700                                                                          
011800 01  WS-IDARTNR                  PIC X(9)    VALUE SPACES.                
011900                                                                          
012000 01  WS-IDPSN                    PIC 9(3)    VALUE ZERO.                  
012100                                                                          
012200*    ---KONTROLLFÄLT                                                      
012300*      --- VALID IDDC CODES                                               
012400*                                                                         
012500*01    -COPY WWDCKONS                                                     
012600*                                                                         
012700*01    -COPY WWLNDKON                                                     
012710*                                                                         
012720*01    -COPY WWPRODSL                                                     
012800       EJECT                                                              
012900 01  GENERELLA-SUBPROGRAM.                                                
013000     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
013100     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
013200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
013300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
013400     03  ABEND                   PIC X(8)    VALUE 'ABEND  '.             
013500     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
013600     03  W400ARTU                PIC X(8)    VALUE 'W400ARTU'.            
013700     03  W005WDK7                PIC X(8)    VALUE 'W005WDK7'.            
013800     EJECT                                                                
013900*    --- PARAMETRAR TILL GEMENSAMMA SUBPROGRAM                            
014000 01 FILLER                       PIC X(8)    VALUE 'W005WDK7'.            
014100*   -COPY W005WDK7                                                        
014200     EJECT                                                                
014300*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
014400*01 -COPY WMEDAREA                                                        
014500     SKIP3                                                                
014600 01  MESSAGE-CODES.                                                       
014700     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
014800     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
014900     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
015000     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
015100     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
015200     03  PART-MISSING            PIC X(3)    VALUE '017'.                 
015300     03  PART-SUPERSEDED         PIC X(3)    VALUE '018'.                 
015400     EJECT                                                                
015500 01  MEDDELANDEN.                                                         
015600     03  MED-1                   PIC X(55)                                
015700         VALUE 'PART NOT AVAILABLE AT ANY NDC   '.                        
015800     03  MED-2                   PIC X(40)                                
015900         VALUE 'NO REFILLPART                          '.                 
016000     03  MED-3                   PIC X(40)                                
016100         VALUE 'CDC PART                               '.                 
016200     03  MED-4                   PIC X(40)                                
016300         VALUE 'WRONG ORIGIN CODE                      '.                 
016400     03  MED-5                   PIC X(40)                                
016500         VALUE 'NO ORIGIN CODE AVAILABLE AT CDC        '.                 
016600     03  MED-6                   PIC X(40)                                
016700         VALUE 'LOCAL PART                             '.                 
016800     03  MED-7                   PIC X(40)                                
016900         VALUE 'UNKNOWN SUPPLIER                       '.                 
017000     03  MED-8                   PIC X(40)                                
017100         VALUE 'NO SPAREPART                           '.                 
017200     03  MED-9                   PIC X(40)                                
017300         VALUE 'ORIGIN UPDATE NOT ALLOWED              '.                 
017310     03  MED-10                  PIC X(40)                                
017320         VALUE 'STANDARD PRICE IS ZERO                 '.                 
017330     03  MED-11                  PIC X(40)                                
017340         VALUE 'LYNK & CO PART                         '.                 
017400     EJECT                                                                
017500*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
017600*                                                                         
017700 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
017800     SKIP3                                                                
017900*01 -COPY WMSGINIT                                                        
018000     SKIP3                                                                
018100*    --- PARAMETRAR TILL ABEND                                            
018200                                                                          
018300 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
018400 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
018500     EJECT                                                                
018600 01 -COPY WDATAREA                                                        
018700     EJECT                                                                
018800 01  FILLER                      PIC X(16)   VALUE 'URSPRUNG'.            
018900*01     -COPY W400ARTU                                                    
019000     EJECT                                                                
019100*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
019200*                                                                         
019300 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
019400     SKIP3                                                                
019500*01  MID -COPY W2I35701                                                   
019600     EJECT                                                                
019700 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
019800     SKIP3                                                                
019900*01  -COPY WMSGAREA                                                       
020000     EJECT                                                                
020100     03  MOD REDEFINES MSG-AREA.                                          
020200*      05  -COPY W2O35701                                                 
020300     EJECT                                                                
020400 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
020500     SKIP3                                                                
020600*01  -COPY WMFSAREA                                                       
020700     EJECT                                                                
020800 01  FILLER                      PIC X(16)   VALUE 'BYTESOBJEKT'.         
020900     SKIP3                                                                
021000*01 -COPY WWBYT03                                                         
021100     EJECT                                                                
021200 01  FILLER                      PIC X(16)   VALUE 'VALIDATE-DC'.         
021300     SKIP3                                                                
021400*01 -COPY WWDC99                                                          
021500     EJECT                                                                
021600 01  FILLER                      PIC X(16)  VALUE 'SQLCA-AREA'.           
021700       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
021800                                                                          
021900 01  FILLER                      PIC X(16)  VALUE 'SQLCODE-WS'.           
022000 01  DB2-WS.                                                              
022100     03  SQLCODE-WS              PIC 9(3)   VALUE ZERO.                   
022200         88  CURSOR-OK                      VALUE 000.                    
022300         88  LINES-FOUND                    VALUE 000.                    
022400         88  LINES-MISSING                  VALUE 100.                    
022500         88  RESOURCE-WRONG                 VALUE 904.                    
022600     03  GOOD-SQLCODECODES.                                               
022700         05  GOOD-SQLCODE OCCURS 5                                        
022800             INDEXED BY SQLCODE-IX PIC 9(3).                              
022900 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
023000     EJECT                                                                
023100*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
023200*                                                                         
023300     SKIP3                                                                
023400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
023500     SKIP3                                                                
023600 01  NYCKLAR-TILL-DLI.                                                    
023700     03  W-IDARTNR-X.                                                     
023800         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
023900     03  W-KDSEGKEY-X.                                                    
024000         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
024100     03  W-IDDC-X.                                                        
024200         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
024300     03  W-IDDC-K7-MIN-X.                                                 
024400         05  W-IDDC-K7-MIN       PIC X(2)    VALUE SPACE.                 
024500     03  W-IDDC-K7-MAX-X.                                                 
024600         05  W-IDDC-K7-MAX       PIC X(2)    VALUE SPACE.                 
024700     03  W-IDLAND-X.                                                      
024800         05  W-IDLAND            PIC X(2)    VALUE SPACE.                 
024900     03  W-IDLEVNR-X.                                                     
025000         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
025100     03  W-IDSKYLT-X.                                                     
025200         05  W-IDSKYLT           PIC X(3)    VALUE 'GB '.                 
025300     03  W-IDDC-B6-X.                                                     
025400         05 W-IDDC-B6            PIC X(2)    VALUE SPACE.                 
025500     03  W-IDDC-TP5-X.                                                    
025600         05  W-IDDC-TP5          PIC X(2)    VALUE SPACE.                 
025700                                                                          
025800     SKIP2                                                                
025900*    --- STATUS-KOD FRÅN IMS                                              
026000 01  STATUS-WS                   PIC XX.                                  
026100     88  SEGMENT-FINNS                       VALUE '  '.                  
026200     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
026300     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
026400     SKIP2                                                                
026500 01  GODK-STATUSKODER.                                                    
026600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
026700     SKIP3                                                                
026800 01  SSA1                        PIC X(64).                               
026900 01  SSA2                        PIC X(64).                               
027000 01  SSA3                        PIC X(64).                               
027100     EJECT                                                                
027200*    --- IMS FUNKTIONSKODER                                               
027300*01  -COPY W0003                                                          
027400     EJECT                                                                
027500*    ---  DLI INPUT-OUTPUT AREA                                           
027600                                                                          
027700 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK601'.                      
027800 01  DLI-IO-WDK601.                                                       
027900*    03  -COPY WDK601                                                     
028000     EJECT                                                                
028100 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK611'.                      
028200 01  DLI-IO-WDK611.                                                       
028300*    03  -COPY WDK611                                                     
028400     EJECT                                                                
028500 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK701'.                      
028600 01  DLI-IO-WDK701.                                                       
028700*    03  -COPY WDK701                                                     
028800     EJECT                                                                
028900 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK711'.                      
029000 01  DLI-IO-WDK711.                                                       
029100*    03  -COPY WDK711                                                     
029200     EJECT                                                                
029300 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK712'.                      
029400 01  DLI-IO-WDK712.                                                       
029500*    03  -COPY WDK712                                                     
029600     EJECT                                                                
029700                                                                          
029800 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK721'.                      
029900 01  DLI-IO-WDK721.                                                       
030000*    03  -COPY WDK721                                                     
030100     EJECT                                                                
030200                                                                          
030300 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDD301'.                      
030400 01  DLI-IO-WDD301.                                                       
030500*    03  -COPY WDD301  -PRE WDD3-                                         
030600     EJECT                                                                
030700 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDD311'.                      
030800 01  DLI-IO-WDD311.                                                       
030900*    03  -COPY WDD311  -PRE WDD3-                                         
031000                                                                          
031100 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
031200 01   DLI-IO-AREA-B601.                                                   
031300*     03  -COPY WDB601                                                    
031400                                                                          
031500     EJECT                                                                
031600                                                                          
031700 01  FILLER                      PIC X(16)  VALUE 'TP5IDDC-AREA'.         
031800                                                                          
031900*01  -COPY TP5IDDC -PRE TP5IDDC-                                          
032000     EJECT                                                                
032100     EXEC SQL INCLUDE TP5IDDC END-EXEC.                                   
032200     EJECT                                                                
032300 LINKAGE SECTION.                                                         
032400*01  -COPY W0009   -PRE MSG-                                              
032500     EJECT                                                                
032600*01  -COPY W0008   -PRE USEA-                                             
032700     05  FILLER                  PIC X.                                   
032800     EJECT                                                                
032900*01  -COPY W0008  -PRE WDK6-                                              
033000     05  FILLER                  PIC X.                                   
033100     EJECT                                                                
033200*01  -COPY W0008  -PRE WDK7-1-                                            
033300     05  FILLER                  PIC X.                                   
033400     EJECT                                                                
033500*01  -COPY W0008  -PRE WDK7-2-                                            
033600     05  FILLER                  PIC X.                                   
033700     EJECT                                                                
033800*01  -COPY W0008  -PRE WDD3-                                              
033900     05  FILLER                  PIC X.                                   
034000     EJECT                                                                
034100*01  -COPY W0008  -PRE WDB6-                                              
034200     05  FILLER                  PIC X.                                   
034300     EJECT                                                                
034400 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB                               
034500                           WDK6-PCB WDK7-1-PCB                            
034600                           WDK7-2-PCB                                     
034700                           WDD3-PCB WDB6-PCB.                             
034800 MAIN SECTION.                                                            
034900     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB                               
035000                           WDK6-PCB WDK7-1-PCB                            
035100                           WDK7-2-PCB                                     
035200                           WDD3-PCB WDB6-PCB.                             
035300                                                                          
035400     PERFORM IMS-GET-MSG                                                  
035500     IF SEGMENT-FINNS                                                     
035600       PERFORM A-INIT                                                     
035700       PERFORM B-KOLLA-NYCKLAR                                            
035800       IF NYCKLAR-OK                                                      
035900         IF MFS-UPDATE                                                    
036000           PERFORM G-KOLLA-INPUT                                          
036100           IF INDATA-OK                                                   
036200             PERFORM H-UPPDATERA                                          
036300           END-IF                                                         
036400         ELSE                                                             
036500           IF MFS-FIRST                                                   
036600             PERFORM C-FOERSTA-SIDA                                       
036700           ELSE                                                           
036800             PERFORM E-SAMMA-SIDA                                         
036900           END-IF                                                         
037000         END-IF                                                           
037100         PERFORM F-LAES-VISA-INFO                                         
037200       END-IF                                                             
037300       COMPUTE MSG-KVLL = LENGTH OF MOD-W2O35701 + 4                      
037400       PERFORM IMS-INSERT-MSG                                             
037500     END-IF                                                               
037600                                                                          
037700*    CALL FELLOG                                                          
037800     MOVE ZERO TO RETURN-CODE                                             
037900     GOBACK                                                               
038000     .                                                                    
038100     EJECT                                                                
038200 A-INIT SECTION.                                                          
038300                                                                          
038400     IF MSG-DUBBLA-TRANSKODER                                             
038500       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W2I35701                 
038600       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
038700       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
038800     ELSE                                                                 
038900       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W2I35701                  
039000       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
039100       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
039200     END-IF                                                               
039300                                                                          
039400     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
039500     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
039600     MOVE MFS-IDTRANS TO W-IDTRANS                                        
039700                                                                          
039800     MOVE LOW-VALUE TO MSG-AREA                                           
039900     MOVE 'W2O357N1' TO MFS-IDMOD                                         
040000     MOVE '2357' TO MOD-IDTRANS                                           
040100     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
040200                                                                          
040300     IF MSGI-IDLAND-SPR = 'SE'                                            
040400        MOVE '0' TO MFS-KDHUVOMR                                          
040500     END-IF                                                               
040600                                                                          
040700     IF EGEN-MID OR HELP-MID                                              
040800       CONTINUE                                                           
040900     ELSE                                                                 
041000       MOVE SPACE TO MFS-KDTRTYP                                          
041100       MOVE '7' TO MFS-IDPFK                                              
041200     END-IF                                                               
041300                                                                          
041400     ACCEPT DAGENS-DATUM FROM DATE                                        
041401                                                                          
041410     MOVE DAGENS-DATUM TO WS-DAGENS-PLUS2-AAMMDD                          
041420     ADD 20000  TO WS-DAGENS-PLUS2-AAMMDD                                 
041430     MOVE 20    TO WS-DAGENS-PLUS-SS                                      
041500                                                                          
041600     MOVE +4                 TO IX-DC-MAX                                 
041700     .                                                                    
041800     EJECT                                                                
041900 B-KOLLA-NYCKLAR SECTION.                                                 
042000                                                                          
042100     MOVE ALL '+'            TO MSGI-WMSGINIT                             
042200     MOVE '001'              TO MSGI-KDCALL                               
042300     MOVE MSG-LTERM-NAME     TO MSGI-IDLTERM-USER                         
042400     MOVE MSG-SIGNON-USERID  TO MSGI-IDUSER                               
042500     MOVE '2357'             TO MSGI-IDTRANS                              
042600     IF  EGEN-MID                                                         
042700     OR  (MID-IDARTNR-IN     NUMERIC                                      
042800     AND MID-IDARTNR-IN       > ZERO)                                     
042900         MOVE MID-IDARTNR-IN                                              
043000                             TO MSGI-IDARTNR                              
043100         MOVE MID-IDDC-IN    TO MSGI-IDDC-KEY                             
043200     END-IF                                                               
043300     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
043400                                                                          
043500     IF MSGI-IDLAND-SPR = 'SE'                                            
043600       MOVE 'S  '            TO MED-IDSKYLT                               
043700     ELSE                                                                 
043800       MOVE 'GB '            TO MED-IDSKYLT                               
043900     END-IF                                                               
044000                                                                          
044100     MOVE JA                 TO NYCKLAR-SW                                
044200                                                                          
044300*    -- KONTROLL AV IDARTNR                                               
044400     MOVE MFS-RENSA-FAELT    TO MOD-IDARTNR-IN                            
044500                                                                          
044600     IF  MID-IDARTNR-IN   NOT = ALL '+'                                   
044700       MOVE '7'              TO MFS-IDPFK                                 
044800       MOVE SPACE            TO MFS-KDTRTYP                               
044900     END-IF                                                               
045000                                                                          
045100     MOVE MSGI-IDARTNR       TO WS-IDARTNR                                
045200     INSPECT WS-IDARTNR REPLACING LEADING SPACE BY ZERO                   
045300                                                                          
045400     IF WS-IDARTNR NUMERIC AND WS-IDARTNR > ZERO                          
045500       MOVE WS-IDARTNR       TO W-IDARTNR                                 
045600     ELSE                                                                 
045700       MOVE NEJ              TO NYCKLAR-SW                                
045800     END-IF                                                               
045900                                                                          
046000*    -- KONTROLL AV IDDC                                                  
046100     MOVE MFS-RENSA-FAELT    TO MOD-IDDC-IN                               
046200                                                                          
046300     IF MID-IDDC-IN       NOT = ALL '+'                                   
046400        MOVE '7'             TO MFS-IDPFK                                 
046500        MOVE SPACE           TO MFS-KDTRTYP                               
046600     END-IF                                                               
046700                                                                          
046800     MOVE MSGI-IDDC-KEY      TO W-IDDC-B6                                 
046900     PERFORM IMS-GU-WDB601                                                
047000     IF DCS-NDC-NA                                                        
047100        MOVE MSGI-IDDC-KEY   TO MOD-IDDC-UT                               
047200                                W-IDDC                                    
047300                                W-IDDC-TP5                                
047400        PERFORM S01-GET-DCGROUP-ALL-DC                                    
047500     ELSE                                                                 
047600       MOVE NEJ              TO NYCKLAR-SW                                
047700       MOVE SPACES           TO MOD-IDDC-UT                               
047800     END-IF                                                               
047900                                                                          
048000     MOVE WS-IDARTNR         TO MOD-IDARTNR-UT                            
048100     INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE               
048110***LYNK PARTS ONLY IN EUROPE                                              
048120     MOVE NEJ     TO SW-LYNK-PART                                         
048130     PERFORM IMS-GU-WDK601                                                
048140     IF SEGMENT-FINNS                                                     
048160       IF  ART-KDPRODSL > 30                                              
048161       AND ART-KDPRODSL < 40                                              
048170         MOVE NEJ          TO NYCKLAR-SW                                  
048180         MOVE JA           TO SW-LYNK-PART                                
048190       END-IF                                                             
048191     END-IF                                                               
048200                                                                          
048300     IF NYCKLAR-FEL                                                       
048310       IF LYNK-PART                                                       
048320         MOVE MED-11          TO MOD-TEMFSFEL                             
048330       ELSE                                                               
048340         MOVE ERR-WRONG-KEY   TO MED-IDMFSFEL                             
048341         MOVE 'GB '          TO MED-IDSKYLT                               
048350         CALL WMEDKONV USING MED-WMEDAREA                                 
048360         MOVE MED-MFSFEL      TO MOD-TEMFSFEL                             
048370       END-IF                                                             
048800       PERFORM MFS-RENSA-FAELT-IN                                         
048900       PERFORM MFS-RENSA-FAELT-UT                                         
049000     END-IF                                                               
049100     .                                                                    
049200     EJECT                                                                
049300 C-FOERSTA-SIDA SECTION.                                                  
049400                                                                          
049500     PERFORM MFS-RENSA-FAELT-IN                                           
049600     .                                                                    
049700     EJECT                                                                
049800 E-SAMMA-SIDA SECTION.                                                    
049900                                                                          
050000     IF EGEN-MID OR HELP-MID                                              
050100       IF MID-INPUT-GRP  = ALL '+'                                        
050200         PERFORM MFS-RENSA-FAELT-IN                                       
050300       ELSE                                                               
050400         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
050500         MOVE 'GB '         TO MED-IDSKYLT                                
050600         CALL WMEDKONV USING MED-WMEDAREA                                 
050700         MOVE MED-TEMFSINF TO MOD-TEMFSINF                                
050800         PERFORM EA-MID-INDATA-TILL-MOD                                   
050900       END-IF                                                             
051000     ELSE                                                                 
051100       PERFORM MFS-RENSA-FAELT-IN                                         
051200     END-IF                                                               
051300     .                                                                    
051400     EJECT                                                                
051500 EA-MID-INDATA-TILL-MOD SECTION.                                          
051600                                                                          
051700     MOVE 1 TO IX-DC                                                      
051800     PERFORM UNTIL IX-DC > IX-DC-MAX                                      
051900*                                                                         
052000        IF MID-KDCMD(IX-DC) NOT = ALL '+'                                 
052100           MOVE MFS-ROER-EJ-FAELT  TO MOD-KDCMD-IN     (IX-DC)            
052200        ELSE                                                              
052300           MOVE MFS-RENSA-FAELT    TO MOD-KDCMD-IN     (IX-DC)            
052400        END-IF                                                            
052500        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDCMD-IN-ATTR(IX-DC)            
052600*                                                                         
052700        IF MID-FLORDSP-EJRO(IX-DC) NOT = ALL '+'                          
052800           MOVE MFS-ROER-EJ-FAELT  TO MOD-FLORDSP-EJRO-IN(IX-DC)          
052900                                                                          
053000        ELSE                                                              
053100           MOVE MFS-RENSA-FAELT    TO MOD-FLORDSP-EJRO-IN(IX-DC)          
053200        END-IF                                                            
053300        MOVE MFS-ADD-LAES-IN-FAELT TO                                     
053400                                 MOD-FLORDSP-EJRO-IN-ATTR(IX-DC)          
053500*                                                                         
053600        IF MID-IDPSN-DC(IX-DC) NOT = ALL '+'                              
053700           MOVE MFS-ROER-EJ-FAELT  TO MOD-IDPSN-IN       (IX-DC)          
053800        ELSE                                                              
053900           MOVE MFS-RENSA-FAELT    TO MOD-IDPSN-IN       (IX-DC)          
054000        END-IF                                                            
054100        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDPSN-IN-ATTR  (IX-DC)          
054200*                                                                         
054300        IF MID-DAPUBL   (IX-DC) NOT = ALL '+'                             
054400           MOVE MFS-ROER-EJ-FAELT  TO MOD-DAPUBL-IN      (IX-DC)          
054500        ELSE                                                              
054600           MOVE MFS-RENSA-FAELT    TO MOD-DAPUBL-IN      (IX-DC)          
054700        END-IF                                                            
054800        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-DAPUBL-IN-ATTR (IX-DC)          
054900                                                                          
055000        ADD 1                      TO IX-DC                               
055100     END-PERFORM                                                          
055200                                                                          
055300                                                                          
055400     .                                                                    
055500     EJECT                                                                
055600 F-LAES-VISA-INFO SECTION.                                                
055700                                                                          
055800     PERFORM FA-LAES-GRUNDDATA                                            
055900                                                                          
056000     IF SEGMENT-SAKNAS                                                    
056100        MOVE PART-MISSING        TO MED-IDMFSFEL                          
056200        MOVE 'GB '               TO MED-IDSKYLT                           
056300        CALL WMEDKONV         USING MED-WMEDAREA                          
056400        MOVE MED-TEMFSFEL        TO MOD-TEMFSFEL                          
056500        PERFORM MFS-RENSA-FAELT-UT                                        
056600        PERFORM MFS-STAENG-FAELT-IN                                       
056700     ELSE                                                                 
056800        IF ART-KDERS-UTG > +0                                             
056900           MOVE PART-SUPERSEDED  TO MED-IDMFSFEL                          
057000           MOVE 'GB '            TO MED-IDSKYLT                           
057100           CALL WMEDKONV      USING MED-WMEDAREA                          
057200           MOVE MED-TEMFSFEL     TO MOD-TEMFSFEL                          
057300           PERFORM MFS-RENSA-FAELT-UT                                     
057400           PERFORM MFS-STAENG-FAELT-IN                                    
057500        ELSE                                                              
057600           PERFORM MFS-RENSA-FAELT-UT                                     
057700           MOVE ART-TIFINLV      TO MOD-TIFINLV                           
057800           PERFORM FB-VISA                                                
057900        END-IF                                                            
058000     END-IF                                                               
058100     .                                                                    
058200     EJECT                                                                
058300 FA-LAES-GRUNDDATA SECTION.                                               
058400                                                                          
058500     PERFORM IMS-GU-WDK601                                                
058600     PERFORM FAA-DISPLAY-IDDC-GROUP                                       
058700     .                                                                    
058800     EJECT                                                                
058900 FAA-DISPLAY-IDDC-GROUP SECTION.                                          
059000                                                                          
059100     MOVE +1                     TO IX-DC                                 
059200     PERFORM UNTIL    IX-DC       > IX-DC-MAX                             
059300       MOVE WS-DC-NR (IX-DC)     TO MOD-IDDC-GRP (IX-DC)                  
059400       ADD +1                    TO IX-DC                                 
059500     END-PERFORM                                                          
059600     .                                                                    
059700     EJECT                                                                
059800                                                                          
059900 FB-VISA SECTION.                                                         
060000                                                                          
060100     PERFORM IMS-GNP-WDK611                                               
060200     IF SEGMENT-FINNS                                                     
060300        MOVE ART-TIFINLV         TO MOD-TIFINLV                           
060400        MOVE CLAG-IDPSN          TO MOD-IDPSN                             
060500        IF CLAG-KDARTURS = SPACE                                          
060600           MOVE MFS-RENSA-FAELT  TO MOD-KDARTURS-CDC                      
060700        ELSE                                                              
060800           MOVE CLAG-KDARTURS    TO MOD-KDARTURS-CDC                      
060900        END-IF                                                            
061000     ELSE                                                                 
061100        MOVE MFS-RENSA-FAELT     TO MOD-TIFINLV                           
061200                                    MOD-KDARTURS-CDC                      
061300                                    MOD-IDPSN                             
061400     END-IF                                                               
061500                                                                          
061600     MOVE 1                      TO IX-DC                                 
061700     PERFORM UNTIL       IX-DC    > IX-DC-MAX                             
061800        PERFORM FBA-VISA-K711-K721                                        
061900        PERFORM FBB-VISA-K712                                             
062000        ADD 1                    TO IX-DC                                 
062100     END-PERFORM                                                          
062200                                                                          
062300     PERFORM IMS-GU-WDD301-BSEQ                                           
062400     IF SEGMENT-FINNS                                                     
062500       PERFORM IMS-GNP-WDD311                                             
062600       IF SEGMENT-FINNS                                                   
062700         MOVE WDD3-TEXT-BEART    TO MOD-BEART-ENG                         
062800       ELSE                                                               
062900         MOVE MFS-RENSA-FAELT    TO MOD-BEART-ENG                         
063000       END-IF                                                             
063100     ELSE                                                                 
063200       MOVE MFS-RENSA-FAELT      TO MOD-BEART-ENG                         
063300     END-IF                                                               
063400                                                                          
063500     MOVE +1                     TO IX-DC                                 
063600     PERFORM UNTIL IX-DC          > 4                                     
063700       IF WS-DC-NR (IX-DC)        > SPACES                                
063800          CONTINUE                                                        
063900       ELSE                                                               
064000          PERFORM MFS-CLOSE-BLANK-DC-FIELD-IN                             
064100       END-IF                                                             
064200       ADD +1                    TO IX-DC                                 
064300     END-PERFORM                                                          
064400     .                                                                    
064500     EJECT                                                                
064600 FBA-VISA-K711-K721 SECTION.                                              
064700                                                                          
064800     PERFORM IMS-GU-WDK701                                                
064900     IF SEGMENT-FINNS                                                     
065000        MOVE WS-DC-NR(IX-DC)          TO W-IDDC                           
065100        PERFORM IMS-GNP-WDK711-KEY                                        
065200        IF SEGMENT-FINNS                                                  
065300           IF DCS-IDDC  NOT = SLAG-IDDC                                   
065400              MOVE SLAG-IDDC          TO W-IDDC-B6                        
065500              PERFORM IMS-GU-WDB601                                       
065600           END-IF                                                         
065700           IF DCS-NDC-NA                                                  
065800              MOVE SLAG-IDPERSON-BUY  TO MOD-IDPERSON     (IX-DC)         
065900              MOVE SLAG-IDLEVNR       TO MOD-IDLEVNR-NDC  (IX-DC)         
066000*                                                                         
066100              MOVE MFS-STAENG-FAELT-NOMOD                                 
066200                                      TO MOD-KDCMD-IN-ATTR(IX-DC)         
066300*                                                                         
066400              IF SLAG-FLORDSP-EJRO     = 'J'                              
066500                 MOVE 'Y'             TO MOD-FLORDSP-EJRO (IX-DC)         
066600              ELSE                                                        
066700                 MOVE SLAG-FLORDSP-EJRO                                   
066800                                      TO MOD-FLORDSP-EJRO (IX-DC)         
066900              END-IF                                                      
067000*                                                                         
067100              PERFORM IMS-GU-WDK721                                       
067200              IF SEGMENT-FINNS                                            
067300                 MOVE SBLK-IDUSER-ORDSP-EJRO                              
067400                                 TO MOD-IDUSER-ORDSP-EJRO(IX-DC)          
067500                 IF SBLK-DAORDSP-EJRO > ZERO                              
067600                    MOVE SBLK-DAORDSP-EJRO                                
067700                                 TO MOD-DAORDSP-EJRO(IX-DC)               
067800                 ELSE                                                     
067900                    MOVE MFS-RENSA-FAELT                                  
068000                                 TO MOD-DAORDSP-EJRO(IX-DC)               
068100                 END-IF                                                   
068200              ELSE                                                        
068300                 MOVE MFS-RENSA-FAELT                                     
068400                                 TO MOD-DAORDSP-EJRO(IX-DC)               
068500                                    MOD-IDUSER-ORDSP-EJRO(IX-DC)          
068600              END-IF                                                      
068700           END-IF                                                         
068800        ELSE                                                              
068900          MOVE MFS-STAENG-FAELT-NOMOD                                     
069000                               TO MOD-FLORDSP-EJRO-IN-ATTR(IX-DC)         
069100          MOVE MFS-RENSA-FAELT TO MOD-DAORDSP-EJRO(IX-DC)                 
069200                                  MOD-IDUSER-ORDSP-EJRO(IX-DC)            
069300        END-IF                                                            
069400     ELSE                                                                 
069500        IF MOD-TEMFSINF    NOT > SPACES                                   
069600           MOVE MED-1         TO MOD-TEMFSINF                             
069700        END-IF                                                            
069800        MOVE MFS-STAENG-FAELT-NOMOD                                       
069900                              TO MOD-FLORDSP-EJRO-IN-ATTR(IX-DC)          
070000                                 MOD-IDPSN-IN-ATTR       (IX-DC)          
070100                                 MOD-DAPUBL-IN-ATTR      (IX-DC)          
070200     END-IF                                                               
070300     .                                                                    
070400     EJECT                                                                
070410 FBB-VISA-K712      SECTION.                                              
070420                                                                          
070430     PERFORM IMS-GU-WDK701                                                
070440     IF SEGMENT-FINNS                                                     
070450        PERFORM IMS-GNP-WDK712                                            
070460        IF SEGMENT-FINNS                                                  
070470           MOVE WS-DC-NR (IX-DC)            TO WS-IDDC                    
070480           PERFORM UNTIL SEGMENT-SAKNAS                                   
070490              IF  LART-IDLANDX2 = 'US'                                    
070491              AND DCS-USA                                                 
070492                  MOVE LART-KDARTURS  TO MOD-KDARTURS-NDC(IX-DC)          
070493                  MOVE LART-IDPSN-DC  TO MOD-IDPSN-DC    (IX-DC)          
070494                   IF LART-DAPUBL       = ZERO                            
070495                     MOVE MFS-RENSA-FAELT                                 
070496                                      TO MOD-DAPUBL      (IX-DC)          
070497                  ELSE                                                    
070498                     IF NDC-US                                            
070499                       MOVE 'AAMMDD'  TO DAT-KDDATFORM                    
070500                       MOVE LART-DAPUBL TO DAT-I-TIDATUM                  
070501                       CALL WDATKONV USING DAT-KDDATFORM                  
070502                                           DAT-I-TIDATUM                  
070503                                           DAT-O-TIDATUM                  
070504                                           DAT-KDSVAR                     
070505                       IF DAT-KDSVAR-OK                                   
070506                          MOVE DAT-TIAAVVD                                
070507                                        TO MOD-DAPUBL    (IX-DC)          
070508                       ELSE                                               
070509                         MOVE 'FEL FRÅN WDATKONV I FB-VISA US' TO         
070510                                   FELTEXT-STR                            
070511                         DISPLAY FELTEXT                                  
070512                         PERFORM S99-ABEND                                
070513                       END-IF                                             
070514                    END-IF                                                
070515                  END-IF                                                  
070516              ELSE                                                        
070517               IF  LART-IDLANDX2 = 'CA'                                   
070518               AND DCS-CANADA                                             
070519                  MOVE LART-KDARTURS  TO MOD-KDARTURS-NDC(IX-DC)          
070520                  MOVE LART-IDPSN-DC  TO MOD-IDPSN-DC    (IX-DC)          
070521                  IF LART-DAPUBL       = ZERO                             
070522                     MOVE MFS-RENSA-FAELT                                 
070523                                      TO MOD-DAPUBL      (IX-DC)          
070524                  ELSE                                                    
070525                     IF NDC-CA                                            
070526                       MOVE 'AAMMDD'  TO DAT-KDDATFORM                    
070527                       MOVE LART-DAPUBL TO DAT-I-TIDATUM                  
070528                       CALL WDATKONV USING DAT-KDDATFORM                  
070529                                           DAT-I-TIDATUM                  
070530                                           DAT-O-TIDATUM                  
070531                                           DAT-KDSVAR                     
070532                       IF DAT-KDSVAR-OK                                   
070533                          MOVE DAT-TIAAVVD                                
070534                                        TO MOD-DAPUBL    (IX-DC)          
070535                       ELSE                                               
070536                          MOVE 'FEL FRÅN WDATKONV I FB-VISA CA'           
070537                                        TO FELTEXT-STR                    
070538                          DISPLAY FELTEXT                                 
070539                          PERFORM S99-ABEND                               
070540                       END-IF                                             
070541                     END-IF                                               
070542                  END-IF                                                  
070543               END-IF                                                     
070544              END-IF                                                      
070545              PERFORM IMS-GNP-WDK712                                      
070546           END-PERFORM                                                    
070547        ELSE                                                              
070548           MOVE MFS-RENSA-FAELT       TO MOD-DAPUBL     (IX-DC)           
070549                                         MOD-KDARTURS-NDC(IX-DC)          
070550                                         MOD-IDPSN-DC    (IX-DC)          
070551        END-IF                                                            
070552     END-IF                                                               
070553     .                                                                    
070554     EJECT                                                                
077300 G-KOLLA-INPUT SECTION.                                                   
077400                                                                          
077500     MOVE JA  TO INDATA-SW                                                
077600     IF MID-INPUT-GRP = ALL '+'                                           
077700        MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                         
077800        MOVE 'GB '                TO MED-IDSKYLT                          
077900        CALL WMEDKONV          USING MED-WMEDAREA                         
078000        MOVE MED-TEMFSFEL         TO MOD-TEMFSFEL                         
078100        PERFORM MFS-ROER-EJ-FAELT-UT                                      
078200        PERFORM MFS-ROER-EJ-FAELT-IN                                      
078300        MOVE NEJ                  TO INDATA-SW                            
078400     ELSE                                                                 
078500        PERFORM GA-KOLLA-INPUT-1                                          
078600        IF INDATA-FEL                                                     
078700           MOVE ERR-CORR-HILITE-FLDS                                      
078800                                  TO MED-IDMFSFEL                         
078900                                                                          
079000           MOVE 'GB '             TO MED-IDSKYLT                          
079100           CALL WMEDKONV       USING MED-WMEDAREA                         
079200           MOVE MED-MFSFEL        TO MOD-TEMFSFEL                         
079300           PERFORM MFS-ROER-EJ-FAELT-UT                                   
079400           PERFORM MFS-ROER-EJ-FAELT-IN                                   
079500        ELSE                                                              
079600           PERFORM GB-KOLLA-INPUT-2                                       
079700           IF INDATA-FEL                                                  
079800              MOVE ERR-CORR-HILITE-FLDS                                   
079900                                  TO MED-IDMFSFEL                         
080000              MOVE 'GB '          TO MED-IDSKYLT                          
080100              CALL WMEDKONV    USING MED-WMEDAREA                         
080200              MOVE MED-MFSFEL     TO MOD-TEMFSFEL                         
080300              PERFORM MFS-ROER-EJ-FAELT-UT                                
080400              PERFORM MFS-ROER-EJ-FAELT-IN                                
080500           END-IF                                                         
080600        END-IF                                                            
080700     END-IF                                                               
080800     .                                                                    
080900     EJECT                                                                
081000                                                                          
081100 GA-KOLLA-INPUT-1 SECTION.                                                
081200                                                                          
081300     MOVE 1 TO IX-DC                                                      
081400     PERFORM UNTIL IX-DC > IX-DC-MAX                                      
081500*KDCMD                                                                    
081600        IF MID-KDCMD(IX-DC)        NOT = ALL '+'                          
081700           IF MID-KDCMD(IX-DC)         = 'I'                              
081800              MOVE MFS-ALFA-FAELT-RAETT                                   
081900                                      TO MOD-KDCMD-IN-ATTR(IX-DC)         
082000           ELSE                                                           
082100              MOVE NEJ                TO INDATA-SW                        
082200              MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCMD-IN-ATTR(IX-DC)         
082300           END-IF                                                         
082400        ELSE                                                              
082500            MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDCMD-IN-ATTR(IX-DC)         
082600        END-IF                                                            
082700                                                                          
082800*FLORDSP-EJRO                                                             
082900        IF MID-FLORDSP-EJRO(IX-DC) NOT = ALL '+'                          
083000           IF MID-FLORDSP-EJRO(IX-DC)  = 'J' OR 'Y' OR 'N'                
083100              MOVE MFS-ALFA-FAELT-RAETT TO                                
083200                                  MOD-FLORDSP-EJRO-IN-ATTR(IX-DC)         
083300           ELSE                                                           
083400              MOVE NEJ                TO INDATA-SW                        
083500              MOVE MFS-ALFA-FAELT-FEL TO                                  
083600                                  MOD-FLORDSP-EJRO-IN-ATTR(IX-DC)         
083700           END-IF                                                         
083800        ELSE                                                              
083900            MOVE MFS-ALFA-FAELT-RAETT TO                                  
084000                                  MOD-FLORDSP-EJRO-IN-ATTR(IX-DC)         
084100        END-IF                                                            
084200                                                                          
084300*IDPSN                                                                    
084400        IF MID-IDPSN-DC (IX-DC)   NOT  = ALL '+'                          
084500           IF MID-IDPSN-DC(IX-DC) NOT NUMERIC                             
084600              MOVE MFS-NUM-FAELT-FEL  TO                                  
084700                                        MOD-IDPSN-IN-ATTR (IX-DC)         
084800              MOVE NEJ                TO INDATA-SW                        
084900           ELSE                                                           
085000              MOVE MFS-NUM-FAELT-RAETT TO                                 
085100                                        MOD-IDPSN-IN-ATTR (IX-DC)         
085200           END-IF                                                         
085300        ELSE                                                              
085400            MOVE MFS-NUM-FAELT-RAETT  TO                                  
085500                                        MOD-IDPSN-IN-ATTR (IX-DC)         
085600        END-IF                                                            
085700                                                                          
085800*DAPUBL                                                                   
085900        IF MID-DAPUBL (IX-DC)     NOT  = ALL '+'                          
086000           IF MID-DAPUBL (IX-DC)  NOT NUMERIC                             
086100              MOVE MFS-NUM-FAELT-FEL  TO                                  
086200                                       MOD-DAPUBL-IN-ATTR (IX-DC)         
086300              MOVE NEJ                TO INDATA-SW                        
086400           ELSE                                                           
086410              IF MID-DAPUBL (IX-DC) > 50000                               
086411                MOVE MFS-NUM-FAELT-FEL TO                                 
086412                                       MOD-DAPUBL-IN-ATTR (IX-DC)         
086413                MOVE NEJ              TO INDATA-SW                        
086420              ELSE                                                        
086500                MOVE MFS-NUM-FAELT-RAETT TO                               
086600                                       MOD-DAPUBL-IN-ATTR (IX-DC)         
086700             END-IF                                                       
086710           END-IF                                                         
086800        ELSE                                                              
086900           MOVE MFS-NUM-FAELT-RAETT TO MOD-DAPUBL-IN-ATTR (IX-DC)         
087000        END-IF                                                            
087100*                                                                         
087200        ADD 1                       TO IX-DC                              
087300     END-PERFORM                                                          
087400     .                                                                    
087500     EJECT                                                                
087510 GB-KOLLA-INPUT-2 SECTION.                                                
087520                                                                          
087530     PERFORM IMS-GU-WDK601                                                
087540     IF ART-KDERS-UTG             > ZERO                                  
087550        MOVE NEJ                 TO INDATA-SW                             
087560        MOVE PART-SUPERSEDED     TO MED-IDMFSFEL                          
087570        MOVE 'GB '               TO MED-IDSKYLT                           
087580        CALL WMEDKONV         USING MED-WMEDAREA                          
087590        MOVE MED-MFSFEL          TO MOD-TEMFSFEL                          
087591     ELSE                                                                 
087592                                                                          
087593***** KONTROLL OM ARTIKEL FINNS                                           
087594                                                                          
087595        PERFORM IMS-GU-WDK701                                             
087596        IF SEGMENT-FINNS                                                  
087597           MOVE +1               TO IX-DC                                 
087598           PERFORM UNTIL IX-DC    > IX-DC-MAX                             
087599             MOVE NEJ            TO WS-FINNS-NA (IX-DC)                   
087600                                    WS-ISRT-NA  (IX-DC)                   
087601             MOVE WS-DC-NR(IX-DC)                                         
087602                                 TO IDDC-WS                               
087603                                    WS-IDDC                               
087604                                    W-IDDC                                
087605             PERFORM IMS-GNP-WDK711-KEY                                   
087606             IF NDC-NA                                                    
087607                IF SEGMENT-FINNS                                          
087608                   MOVE JA       TO WS-FINNS-NA (IX-DC)                   
087609                ELSE                                                      
087610                   IF MID-KDCMD(IX-DC) = ALL '+'                          
087611                      CONTINUE                                            
087612                   ELSE                                                   
087613                      MOVE JA    TO WS-ISRT-NA  (IX-DC)                   
087614                   END-IF                                                 
087615                END-IF                                                    
087616             END-IF                                                       
087617                                                                          
087618             IF MID-DAPUBL (IX-DC)    = ALL '+'                           
087619                CONTINUE                                                  
087620             ELSE                                                         
087621                MOVE ART-TIFINLV     TO DAT-I-TIDATUM                     
087622                MOVE 'AAVVD'         TO DAT-KDDATFORM                     
087623                CALL WDATKONV     USING DAT-KDDATFORM                     
087624                                        DAT-I-TIDATUM                     
087625                                        DAT-O-TIDATUM                     
087626                                        DAT-KDSVAR                        
087627                                                                          
087628                IF DAT-KDSVAR-OK                                          
087629                   MOVE DAT-TIAAMMDD TO WS-TIFINLV-AAMMDD                 
087630                   MOVE DAT-TISEKEL  TO WS-TIFINLV-SS                     
087631                ELSE                                                      
087632                   MOVE 'FEL FRÅN WDATKONV I G-KOLLA'                     
087633                                     TO FELTEXT-STR                       
087634                   DISPLAY FELTEXT                                        
087635                   PERFORM S99-ABEND                                      
087636                END-IF                                                    
087637             END-IF                                                       
087638                                                                          
087639             IF  MID-DAPUBL(IX-DC) NOT = ALL '+'                          
087640               IF MID-DAPUBL(IX-DC) = ZERO                                
087641                 MOVE MFS-NUM-FAELT-RAETT                                 
087642                        TO MOD-DAPUBL-IN-ATTR(IX-DC)                      
087643               ELSE                                                       
087644                MOVE 'AAVVD'        TO DAT-KDDATFORM                      
087645                MOVE MID-DAPUBL(IX-DC)                                    
087646                                    TO DAT-I-TIDATUM                      
087647                CALL WDATKONV    USING DAT-KDDATFORM                      
087648                                       DAT-I-TIDATUM                      
087649                                       DAT-O-TIDATUM                      
087650                                       DAT-KDSVAR                         
087651                                                                          
087652                IF DAT-KDSVAR    NOT = SPACE                              
087653                   MOVE MFS-NUM-FAELT-FEL                                 
087654                                    TO MOD-DAPUBL-IN-ATTR(IX-DC)          
087655                   MOVE NEJ         TO INDATA-SW                          
087656                ELSE                                                      
087657                   MOVE DAT-TIAAMMDD                                      
087658                                    TO WS-DAPUBL-AAMMDD                   
087659                   MOVE DAT-TISEKEL TO WS-DAPUBL-SS                       
087660                   IF WS-DAPUBL      < WS-TIFINLV                         
087661                      MOVE MFS-NUM-FAELT-FEL                              
087662                                    TO MOD-DAPUBL-IN-ATTR(IX-DC)          
087663                      MOVE NEJ      TO INDATA-SW                          
087664                   ELSE                                                   
087665                     IF WS-DAPUBL < WS-DAGENS-PLUS2-AAR                   
087666                       MOVE MFS-NUM-FAELT-RAETT                           
087667                              TO MOD-DAPUBL-IN-ATTR(IX-DC)                
087668                     ELSE                                                 
087669                       MOVE MFS-NUM-FAELT-FEL                             
087670                              TO MOD-DAPUBL-IN-ATTR(IX-DC)                
087671                       MOVE NEJ TO INDATA-SW                              
087672                     END-IF                                               
087673                   END-IF                                                 
087674                END-IF                                                    
087675               END-IF                                                     
087676                                                                          
087677                IF  WS-FINNS-NA (IX-DC) = NEJ                             
087678                AND WS-ISRT-NA  (IX-DC) = NEJ                             
087679                    MOVE MFS-NUM-FAELT-FEL                                
087680                                    TO MOD-DAPUBL-IN-ATTR(IX-DC)          
087681                    MOVE NEJ        TO INDATA-SW                          
087682                END-IF                                                    
087683             END-IF                                                       
087684             ADD +1                 TO IX-DC                              
087685           END-PERFORM                                                    
087686        END-IF                                                            
087687                                                                          
087688*KDCMD                                                                    
087689*EJ FLLSRDEL                                                              
087690*EJ BYTES                                                                 
087691                                                                          
087692        PERFORM IMS-GNP-WDK611                                            
087693                                                                          
087694        IF MID-KDCMD(1)         = 'I'                                     
087695        OR MID-KDCMD(2)         = 'I'                                     
087696        OR MID-KDCMD(3)         = 'I'                                     
087697        OR MID-KDCMD(4)         = 'I'                                     
087698          IF CLAG-PRARTSTD = ZERO                                         
087699            MOVE NEJ                TO INDATA-SW                          
087700            MOVE MED-10             TO MOD-TEMFSINF                       
087701          END-IF                                                          
087702        END-IF                                                            
087703                                                                          
087704                                                                          
087705        IF CLAG-FLLSRDEL = NEJ                                            
087706           MOVE NEJ                 TO INDATA-SW                          
087707           MOVE MED-8               TO MOD-TEMFSINF                       
087708           MOVE 1                   TO IX-DC                              
087709           PERFORM UNTIL   IX-DC     > IX-DC-MAX                          
087710             IF MID-KDCMD(IX-DC) NOT = ALL '+'                            
087711                MOVE MFS-ALFA-FAELT-FEL                                   
087712                                    TO MOD-KDCMD-IN-ATTR(IX-DC)           
087713             END-IF                                                       
087714             ADD 1                  TO IX-DC                              
087715           END-PERFORM                                                    
087716        ELSE                                                              
087717          MOVE 1                    TO IX-DC                              
087718          PERFORM UNTIL   IX-DC      > IX-DC-MAX                          
087719            IF MID-KDCMD (IX-DC) NOT = ALL '+'                            
087720             IF CLAG-FLREFILL        = NEJ                                
087721                MOVE W-IDARTNR      TO BYT03-IDARTNR                      
087722                MOVE ART-KDPRODSL   TO TEST-KDPRODSL                      
087723                IF BYT03-OBJEKT                                           
087724                OR KDPRODSL-BYTES                                         
087725                OR KDPRODSL-LOCAL-BYTES                                   
087726                OR ART-KDSORT        = 'SW'                               
087727***          SKIP LOOP AND CONTINUE PROCESSING                            
087728                   MOVE IX-DC-MAX   TO IX-DC                              
087729                ELSE                                                      
087730                  MOVE NEJ          TO INDATA-SW                          
087731                  MOVE MED-2        TO MOD-TEMFSINF                       
087732                  IF MID-KDCMD(IX-DC)                                     
087733                                 NOT = ALL '+'                            
087734                     MOVE MFS-ALFA-FAELT-FEL                              
087735                                    TO MOD-KDCMD-IN-ATTR(IX-DC)           
087736                  END-IF                                                  
087737                END-IF                                                    
087738             END-IF                                                       
087739            END-IF                                                        
087740            ADD 1                   TO IX-DC                              
087741          END-PERFORM                                                     
087742        END-IF                                                            
087743                                                                          
087744*FLORDSPE-EJRO                                                            
087745        PERFORM IMS-GU-WDK701                                             
087746        IF SEGMENT-FINNS                                                  
087747           MOVE +1                   TO IX-DC                             
087748           PERFORM UNTIL    IX-DC     > IX-DC-MAX                         
087749             IF MID-FLORDSP-EJRO(IX-DC) NOT = ALL '+'                     
087750                MOVE WS-DC-NR(IX-DC) TO IDDC-WS                           
087751                                        W-IDDC                            
087752                PERFORM IMS-GNP-WDK711-KEY                                
087753                IF SEGMENT-FINNS                                          
087754                  CONTINUE                                                
087755                ELSE                                                      
087756                  MOVE NEJ           TO INDATA-SW                         
087757                  MOVE MFS-ALFA-FAELT-FEL TO                              
087758                                MOD-FLORDSP-EJRO-IN-ATTR(IX-DC)           
087759                END-IF                                                    
087760             END-IF                                                       
087761             ADD 1                  TO IX-DC                              
087762           END-PERFORM                                                    
087763        END-IF                                                            
087764                                                                          
087765*IDPSN DAPUBL                                                             
087766        PERFORM IMS-GU-WDK701                                             
087767        IF SEGMENT-FINNS                                                  
087768           MOVE +1                     TO IX-DC                           
087769           PERFORM UNTIL       IX-DC    > IX-DC-MAX                       
087770             MOVE WS-DC-NR(IX-DC)      TO IDDC-WS                         
087771                                          W-IDDC                          
087772                                          WS-IDDC                         
087773             IF NDC-US                                                    
087774                MOVE WC-LAND-US        TO W-IDLAND                        
087775             ELSE                                                         
087776               IF NDC-CA                                                  
087777                  MOVE WC-LAND-CA      TO W-IDLAND                        
087778               END-IF                                                     
087779             END-IF                                                       
087780                                                                          
087781             PERFORM IMS-GU-WDK712                                        
087782             IF SEGMENT-FINNS                                             
087783               IF MID-IDPSN-DC(IX-DC) NOT = ALL '+'                       
087784                  IF WS-FINNS-NA(IX-DC) = NEJ                             
087785                  AND WS-ISRT-NA(IX-DC) = NEJ                             
087786                     MOVE NEJ          TO INDATA-SW                       
087787                     MOVE MFS-ALFA-FAELT-FEL                              
087788                                       TO MOD-IDPSN-IN-ATTR(IX-DC)        
087789                  ELSE                                                    
087790                     MOVE MFS-ALFA-FAELT-RAETT                            
087791                                       TO MOD-IDPSN-IN-ATTR(IX-DC)        
087792                  END-IF                                                  
087793               END-IF                                                     
087794               IF MID-DAPUBL(IX-DC) NOT = ALL '+'                         
087795                  IF WS-FINNS-NA(IX-DC) = NEJ                             
087796                  AND WS-ISRT-NA(IX-DC) = NEJ                             
087797                     MOVE NEJ          TO INDATA-SW                       
087798                     MOVE MFS-NUM-FAELT-FEL                               
087799                                     TO MOD-DAPUBL-IN-ATTR(IX-DC)         
087800                  END-IF                                                  
087801               END-IF                                                     
087802             ELSE                                                         
087803               IF MID-IDPSN-DC(IX-DC)  NOT = ALL '+'                      
087804                  IF WS-FINNS-NA(IX-DC) = NEJ                             
087805                  AND MID-KDCMD(IX-DC)  = ALL '+'                         
087806                     MOVE NEJ          TO INDATA-SW                       
087807                     MOVE MFS-ALFA-FAELT-FEL                              
087808                                       TO MOD-IDPSN-IN-ATTR(IX-DC)        
087809                  ELSE                                                    
087810                     MOVE MFS-ALFA-FAELT-RAETT                            
087811                                       TO MOD-IDPSN-IN-ATTR(IX-DC)        
087812                  END-IF                                                  
087813               END-IF                                                     
087814               IF MID-DAPUBL(IX-DC) NOT = ALL '+'                         
087815                  IF WS-FINNS-NA(IX-DC) = NEJ                             
087816                  AND MID-KDCMD(IX-DC)  = ALL '+'                         
087817                     MOVE NEJ          TO INDATA-SW                       
087818                     MOVE MFS-NUM-FAELT-FEL                               
087819                                     TO MOD-DAPUBL-IN-ATTR(IX-DC)         
087820                  END-IF                                                  
087821               END-IF                                                     
087822             END-IF                                                       
087823             ADD  +1                   TO IX-DC                           
087824           END-PERFORM                                                    
087825        END-IF                                                            
087826     END-IF                                                               
087827     .                                                                    
087828     EJECT                                                                
109800 H-UPPDATERA SECTION.                                                     
109900                                                                          
110000     MOVE NEJ                     TO SW-IDPSN-CDC                         
110100                                     SW-NDC-US-UPD-IDPSN                  
110200                                     SW-NDC-CA-UPD-IDPSN                  
110300                                     SW-NDC-US-UPD-DAPUBL                 
110400                                     SW-NDC-CA-UPD-DAPUBL                 
110500     MOVE +1                      TO IX-DC                                
110600     PERFORM UNTIL      IX-DC      > IX-DC-MAX                            
110700       IF MID-KDCMD(IX-DC)     NOT = ALL '+'                              
110800          MOVE WS-DC-NR(IX-DC)    TO W-IDDC                               
110900                                     IDDC-WS                              
111000          IF DCS-IDDC          NOT = IDDC-WS                              
111100             MOVE IDDC-WS         TO W-IDDC-B6                            
111200             PERFORM IMS-GU-WDB601                                        
111300          END-IF                                                          
111400          PERFORM IMS-GU-WDK711                                           
111500          IF SEGMENT-SAKNAS                                               
111600             PERFORM HA-NYA-WDK711-21-SEGMENT                             
111700          END-IF                                                          
111800       END-IF                                                             
111900                                                                          
112000       IF  MID-FLORDSP-EJRO(IX-DC) = ALL '+'                              
112100       AND MID-IDPSN-DC    (IX-DC) = ALL '+'                              
112200       AND MID-DAPUBL      (IX-DC) = ALL '+'                              
112300           CONTINUE                                                       
112400       ELSE                                                               
112500           PERFORM HB-UPPDATERA-WDK7                                      
112600       END-IF                                                             
112700                                                                          
112800       ADD 1 TO IX-DC                                                     
112900     END-PERFORM                                                          
113000                                                                          
113100     MOVE INF-UPDATE-DONE         TO MED-IDMFSINF                         
113200     MOVE 'GB '                   TO MED-IDSKYLT                          
113300     CALL WMEDKONV             USING MED-WMEDAREA                         
113400     MOVE MED-MFSINF              TO MOD-TEMFSINF                         
113500     PERFORM MFS-FORM-ATTR                                                
113600     PERFORM MFS-RENSA-FAELT-IN                                           
113700     .                                                                    
113800     EJECT                                                                
113900 HA-NYA-WDK711-21-SEGMENT SECTION.                                        
114000                                                                          
114100     MOVE ALL '+'        TO WDK7-W005WDK7                                 
114200     MOVE 'WDK711'       TO WDK7-IDSEGM                                   
114300     MOVE IDDC-WS        TO WDK7-IDDC-KFB                                 
114400                            WDK7-IDDC                                     
114500     MOVE W-IDARTNR      TO WDK7-IDARTNR-KFB                              
114600                            BYT03-IDARTNR                                 
114700     IF   BYT03-OBJEKT                                                    
114800          MOVE NEJ       TO WDK7-FLREFILL                                 
114900     END-IF                                                               
115000     CALL W005WDK7 USING WDK7-W005WDK7 WDB6-PCB                           
115100                                       WDK6-PCB WDK7-1-PCB                
115200                                                                          
115300     MOVE ALL '+'        TO WDK7-W005WDK7                                 
115400     MOVE 'WDK721'       TO WDK7-IDSEGM                                   
115500     MOVE W-IDARTNR      TO WDK7-IDARTNR-KFB                              
115600     MOVE IDDC-WS        TO WDK7-IDDC-KFB                                 
115700     MOVE '1'            TO WDK7-KDSEGKEY                                 
115800                         IN WDK7-WDK721                                   
115900     MOVE DAGENS-DATUM   TO WDK7-DAORDSP-EJRO                             
116000     MOVE MSGI-IDUSER    TO WDK7-IDUSER-ORDSP-EJRO                        
116100     CALL W005WDK7 USING WDK7-W005WDK7 WDB6-PCB                           
116200                                       WDK6-PCB WDK7-2-PCB                
116300     .                                                                    
116400     EJECT                                                                
116500 HB-UPPDATERA-WDK7 SECTION.                                               
116600                                                                          
116700     PERFORM HBA-UPPDATERA-WDK711-21                                      
116800                                                                          
116900     PERFORM HBC-UPPDATERA-WDK712                                         
117000     .                                                                    
117100     EJECT                                                                
117200 HBA-UPPDATERA-WDK711-21 SECTION.                                         
117300                                                                          
117400     PERFORM IMS-GU-WDK701                                                
117500     MOVE WS-DC-NR (IX-DC)            TO W-IDDC                           
117600                                         IDDC-WS                          
117700                                         WS-IDDC                          
117800     PERFORM IMS-GHNP-WDK711                                              
117900     IF SEGMENT-FINNS                                                     
118000        IF MID-FLORDSP-EJRO(IX-DC) NOT = ALL '+'                          
118100           IF MID-FLORDSP-EJRO(IX-DC)                                     
118200                                   NOT = SLAG-FLORDSP-EJRO                
118300             PERFORM IMS-GHU-WDK721                                       
118400             IF SEGMENT-FINNS                                             
118500                MOVE DAGENS-DATUM     TO SBLK-DAORDSP-EJRO                
118600                MOVE MSGI-IDUSER      TO SBLK-IDUSER-ORDSP-EJRO           
118700                PERFORM IMS-REPL-WDK721                                   
118800             ELSE                                                         
118900               MOVE ALL '+'           TO WDK7-W005WDK7                    
119000               MOVE 'WDK721'          TO WDK7-IDSEGM                      
119100               MOVE W-IDARTNR         TO WDK7-IDARTNR-KFB                 
119200               MOVE IDDC-WS           TO WDK7-IDDC-KFB                    
119300               MOVE '1'               TO WDK7-KDSEGKEY                    
119400                                      IN WDK7-WDK721                      
119500               MOVE DAGENS-DATUM      TO WDK7-DAORDSP-EJRO                
119600               MOVE MSGI-IDUSER       TO WDK7-IDUSER-ORDSP-EJRO           
119700               CALL W005WDK7       USING WDK7-W005WDK7                    
119800                                   WDB6-PCB WDK6-PCB WDK7-2-PCB           
119900             END-IF                                                       
120000           END-IF                                                         
120100           IF MID-FLORDSP-EJRO(IX-DC)  = YES OR JA                        
120200              MOVE 'J'                TO SLAG-FLORDSP-EJRO                
120300              MOVE ZERO               TO SLAG-KVPB-REF                    
120400                                         SLAG-KVPB-HIST                   
120500              MOVE 'P'                TO SLAG-KDREFSTA                    
120600              MOVE DAGENS-DATUM       TO SLAG-TIREFSTA                    
120700           ELSE                                                           
120800              MOVE MID-FLORDSP-EJRO(IX-DC)                                
120900                                      TO SLAG-FLORDSP-EJRO                
121000           END-IF                                                         
121100        END-IF                                                            
121200        PERFORM IMS-REPL-WDK711                                           
121300     END-IF                                                               
121400     .                                                                    
121500     EJECT                                                                
121600 HBC-UPPDATERA-WDK712 SECTION.                                            
121700                                                                          
121800     MOVE WS-DC-NR (IX-DC)         TO WS-IDDC                             
121900     IF NDC-US                                                            
122000        MOVE WC-LAND-US            TO W-IDLAND                            
122100     ELSE                                                                 
122200        IF NDC-CA                                                         
122300           MOVE WC-LAND-CA         TO W-IDLAND                            
122400        END-IF                                                            
122500     END-IF                                                               
122600                                                                          
122700     IF  (NDC-US                                                          
122800     AND  NDC-US-UPD-IDPSN-NEJ )                                          
122900     OR  (NDC-CA                                                          
123000     AND  NDC-CA-UPD-IDPSN-NEJ )                                          
123100      IF MID-IDPSN-DC (IX-DC)   NOT = ALL '+'                             
123200        PERFORM IMS-GHU-WDK712                                            
123300        IF SEGMENT-FINNS                                                  
123400           MOVE MID-IDPSN-DC (IX-DC)                                      
123500                                   TO LART-IDPSN-DC                       
123600           PERFORM IMS-REPL-WDK712                                        
123700        ELSE                                                              
123800           MOVE ALL '+'            TO WDK7-W005WDK7                       
123900           MOVE 'WDK712'           TO WDK7-IDSEGM                         
124000           MOVE W-IDARTNR          TO WDK7-IDARTNR-KFB                    
124100           MOVE W-IDLAND           TO WDK7-IDLANDX2                       
124200           MOVE MID-IDPSN-DC(IX-DC)                                       
124300                                   TO WDK7-IDPSN-DC                       
124400           CALL W005WDK7        USING WDK7-W005WDK7 WDB6-PCB              
124500                                      WDK6-PCB WDK7-1-PCB                 
124600        END-IF                                                            
124700        IF NDC-US                                                         
124800           MOVE JA                 TO SW-NDC-US-UPD-IDPSN                 
124900        END-IF                                                            
125000        IF NDC-CA                                                         
125100           MOVE JA                 TO SW-NDC-CA-UPD-IDPSN                 
125200        END-IF                                                            
125300      END-IF                                                              
125400     END-IF                                                               
125500                                                                          
125600     IF  (NDC-US                                                          
125700     AND  NDC-US-UPD-DAPUBL-NEJ )                                         
125800     OR  (NDC-CA                                                          
125900     AND  NDC-CA-UPD-DAPUBL-NEJ )                                         
126000      IF MID-DAPUBL(IX-DC)       NOT = ALL '+'                            
126100        PERFORM IMS-GHU-WDK712                                            
126200        IF SEGMENT-FINNS                                                  
126300           MOVE WS-DAPUBL          TO LART-DAPUBL                         
126400           PERFORM IMS-REPL-WDK712                                        
126500        ELSE                                                              
126600           MOVE ALL '+'            TO WDK7-W005WDK7                       
126700           MOVE 'WDK712'           TO WDK7-IDSEGM                         
126800           MOVE W-IDARTNR          TO WDK7-IDARTNR-KFB                    
126900           MOVE W-IDLAND           TO WDK7-IDLANDX2                       
127000           MOVE WS-DAPUBL          TO WDK7-DAPUBL                         
127100           CALL W005WDK7        USING WDK7-W005WDK7 WDB6-PCB              
127200                                      WDK6-PCB WDK7-1-PCB                 
127300        END-IF                                                            
127400      END-IF                                                              
127500     END-IF                                                               
127600     .                                                                    
127700     EJECT                                                                
127800 S01-GET-DCGROUP-ALL-DC SECTION.                                          
127900                                                                          
128000     PERFORM DB2-DCL-OPN-TP5IDDC-CRS                                      
128100     PERFORM DB2-FETCH-TP5IDDC-CRS                                        
128200     IF LINES-FOUND                                                       
128300        MOVE +1              TO IX-DC                                     
128400        PERFORM UNTIL IX-DC > IX-DC-MAX OR LINES-MISSING                  
128500          MOVE TP5IDDC-IDDC  TO WS-DC-NR (IX-DC)                          
128600          PERFORM DB2-FETCH-TP5IDDC-CRS                                   
128700          MOVE IX-DC         TO IX-DC-N                                   
128800          ADD +1             TO IX-DC                                     
128900        END-PERFORM                                                       
129000        MOVE IX-DC-N         TO IX-DC-MAX                                 
129100     ELSE                                                                 
129200        MOVE NEJ             TO NYCKLAR-SW                                
129300        MOVE SPACES          TO MOD-IDDC-UT                               
129400     END-IF                                                               
129500     PERFORM DB2-CLOSE-TP5IDDC-CRS                                        
129600     .                                                                    
129700     EJECT                                                                
129800 S99-ABEND SECTION.                                                       
129900                                                                          
130000     SKIP2                                                                
130100     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
130200     .                                                                    
130300     EJECT                                                                
130400 MFS-RENSA-FAELT-UT SECTION.                                              
130500                                                                          
130600*    --- ALLA UTDATA-FÄLT                                                 
130700     MOVE 1                  TO IX-DC                                     
130800     PERFORM UNTIL    IX-DC   > 4                                         
130900        MOVE MFS-RENSA-FAELT TO MOD-IDLEVNR-NDC       (IX-DC)             
131000                                MOD-KDARTURS-NDC      (IX-DC)             
131100                                MOD-FLORDSP-EJRO      (IX-DC)             
131200                                MOD-DAORDSP-EJRO      (IX-DC)             
131300                                MOD-IDUSER-ORDSP-EJRO (IX-DC)             
131400                                MOD-IDPSN-DC          (IX-DC)             
131500                                MOD-DAPUBL            (IX-DC)             
131600                                MOD-IDPERSON          (IX-DC)             
131700        ADD  1               TO IX-DC                                     
131800     END-PERFORM                                                          
131900                                                                          
132000     MOVE MFS-RENSA-FAELT    TO MOD-BEART-ENG                             
132100                                MOD-TIFINLV                               
132200                                MOD-KDARTURS-CDC                          
132300                                MOD-IDPSN                                 
132400     .                                                                    
132500     SKIP3                                                                
132600 MFS-RENSA-FAELT-IN SECTION.                                              
132700                                                                          
132800*    --- ALLA INDATA-FÄLT                                                 
132900     MOVE 1                  TO IX-DC                                     
133000     PERFORM UNTIL    IX-DC   > 4                                         
133100        MOVE MFS-RENSA-FAELT                                              
133200                             TO MOD-KDCMD-IN        (IX-DC)               
133300                                MOD-FLORDSP-EJRO-IN (IX-DC)               
133400                                MOD-IDPSN-IN        (IX-DC)               
133500                                MOD-DAPUBL-IN       (IX-DC)               
133600        ADD  1               TO IX-DC                                     
133700     END-PERFORM                                                          
133800     .                                                                    
133900     EJECT                                                                
134000 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
134100                                                                          
134200*    --- ALLA UTDATA-FÄLT                                                 
134300     MOVE 1                    TO IX-DC                                   
134400     PERFORM UNTIL    IX-DC     > 4                                       
134500        MOVE MFS-ROER-EJ-FAELT TO MOD-KDARTURS-NDC (IX-DC)                
134600                                  MOD-IDLEVNR-NDC  (IX-DC)                
134700                                  MOD-FLORDSP-EJRO (IX-DC)                
134800                                  MOD-IDPSN-DC     (IX-DC)                
134900                                  MOD-IDPERSON     (IX-DC)                
135000                                  MOD-DAPUBL       (IX-DC)                
135100        ADD  1                 TO IX-DC                                   
135200     END-PERFORM                                                          
135300                                                                          
135400     MOVE MFS-ROER-EJ-FAELT    TO MOD-BEART-ENG                           
135500                                  MOD-KDARTURS-CDC                        
135600                                  MOD-TIFINLV                             
135700                                  MOD-IDPSN                               
135800     .                                                                    
135900     SKIP3                                                                
136000 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
136100                                                                          
136200*    --- ALLA INDATA-FÄLT                                                 
136300     MOVE 1                    TO IX-DC                                   
136400     PERFORM UNTIL     IX-DC    > IX-DC-MAX                               
136500        MOVE MFS-ROER-EJ-FAELT                                            
136600                               TO MOD-KDCMD-IN(IX-DC)                     
136700                                  MOD-FLORDSP-EJRO-IN(IX-DC)              
136800                                  MOD-IDPSN-IN       (IX-DC)              
136900                                  MOD-DAPUBL-IN      (IX-DC)              
137000        ADD  1                 TO IX-DC                                   
137100     END-PERFORM                                                          
137200     .                                                                    
137300     EJECT                                                                
137400 MFS-FORM-ATTR SECTION.                                                   
137500                                                                          
137600*    --- ALLA INDATA-FÄLT                                                 
137700     MOVE 1                     TO IX-DC                                  
137800     PERFORM UNTIL     IX-DC     > IX-DC-MAX                              
137900        MOVE MFS-FORMATETS-ATTR                                           
138000                                TO MOD-KDCMD-IN-ATTR       (IX-DC)        
138100                                   MOD-FLORDSP-EJRO-IN-ATTR(IX-DC)        
138200                                   MOD-IDPSN-IN-ATTR       (IX-DC)        
138300                                   MOD-DAPUBL-IN-ATTR      (IX-DC)        
138400        ADD  1                  TO IX-DC                                  
138500     END-PERFORM                                                          
138600     .                                                                    
138700     SKIP2                                                                
138800 MFS-STAENG-FAELT-IN SECTION.                                             
138900                                                                          
139000*    --- ALLA INDATA-FÄLT                                                 
139100     MOVE 1                         TO IX-DC                              
139200     PERFORM UNTIL     IX-DC         > 4                                  
139300        MOVE MFS-STAENG-FAELT-NOMOD TO                                    
139400                                   MOD-KDCMD-IN-ATTR       (IX-DC)        
139500                                   MOD-FLORDSP-EJRO-IN-ATTR(IX-DC)        
139600                                   MOD-IDPSN-IN-ATTR       (IX-DC)        
139700                                   MOD-DAPUBL-IN-ATTR      (IX-DC)        
139800        ADD  1                      TO IX-DC                              
139900     END-PERFORM                                                          
140000     .                                                                    
140100     EJECT                                                                
140200 MFS-CLOSE-BLANK-DC-FIELD-IN  SECTION.                                    
140300     SKIP2                                                                
140400     MOVE MFS-STAENG-FAELT-NOMOD                                          
140500                           TO MOD-KDCMD-IN-ATTR       (IX-DC)             
140600                              MOD-FLORDSP-EJRO-IN-ATTR(IX-DC)             
140700                              MOD-IDPSN-IN-ATTR       (IX-DC)             
140800                              MOD-DAPUBL-IN-ATTR      (IX-DC)             
140900     .                                                                    
141000     EJECT                                                                
141100* --- IMS SEKTIONER ---                                                   
141200                                                                          
141300 IMS-GET-MSG SECTION.                                                     
141400                                                                          
141500     MOVE '  QC' TO GODK-STATUSKODER                                      
141600     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
141700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
141800     PERFORM IMS-STATUSKONTROLL                                           
141900     .                                                                    
142000     SKIP2                                                                
142100 IMS-INSERT-MSG SECTION.                                                  
142200                                                                          
142300     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
142400       MOVE 'N' TO MFS-KDHUVOMR                                           
142500     END-IF                                                               
142600     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
142700     MOVE SPACE TO GODK-STATUSKODER                                       
142800     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
142900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
143000     PERFORM IMS-STATUSKONTROLL                                           
143100     .                                                                    
143200     EJECT                                                                
143300 IMS-GU-WDK601 SECTION.                                                   
143400                                                                          
143500     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
143600          DELIMITED BY SIZE INTO SSA1                                     
143700     MOVE '  GE' TO GODK-STATUSKODER                                      
143800     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
143900     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
144000     PERFORM IMS-STATUSKONTROLL                                           
144100     .                                                                    
144200     SKIP3                                                                
144300 IMS-GNP-WDK611 SECTION.                                                  
144400                                                                          
144500     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
144600          DELIMITED BY SIZE INTO SSA1                                     
144700     MOVE '  GE' TO GODK-STATUSKODER                                      
144800     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
144900     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
145000     PERFORM IMS-STATUSKONTROLL                                           
145100     .                                                                    
145200     EJECT                                                                
145300 IMS-GHU-WDK611 SECTION.                                                  
145400                                                                          
145500     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
145600          DELIMITED BY SIZE INTO SSA1                                     
145700     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
145800          DELIMITED BY SIZE INTO SSA2                                     
145900     MOVE '  ' TO GODK-STATUSKODER                                        
146000     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2              
146100     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
146200     PERFORM IMS-STATUSKONTROLL                                           
146300     .                                                                    
146400     SKIP3                                                                
146500 IMS-REPL-WDK6 SECTION.                                                   
146600                                                                          
146700     MOVE '  ' TO GODK-STATUSKODER                                        
146800     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK611                       
146900     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
147000     PERFORM IMS-STATUSKONTROLL                                           
147100     .                                                                    
147200     EJECT                                                                
147300 IMS-GU-WDK701 SECTION.                                                   
147400                                                                          
147500     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
147600          DELIMITED BY SIZE INTO SSA1                                     
147700     MOVE '  GE' TO GODK-STATUSKODER                                      
147800     CALL CBLTDLI USING GU WDK7-1-PCB DLI-IO-WDK701 SSA1                  
147900     MOVE WDK7-1-STATUS-CODE TO STATUS-WS                                 
148000     PERFORM IMS-STATUSKONTROLL                                           
148100     .                                                                    
148200     SKIP3                                                                
148300 IMS-GNP-WDK711 SECTION.                                                  
148400                                                                          
148500     MOVE 'WDK711   ' TO SSA1                                             
148600     MOVE '  GE' TO GODK-STATUSKODER                                      
148700     CALL CBLTDLI USING GNP WDK7-1-PCB DLI-IO-WDK711 SSA1                 
148800     MOVE WDK7-1-STATUS-CODE TO STATUS-WS                                 
148900     PERFORM IMS-STATUSKONTROLL                                           
149000     .                                                                    
149100     SKIP3                                                                
149200 IMS-GNP-WDK711-KEY SECTION.                                              
149300                                                                          
149400     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
149500          DELIMITED BY SIZE INTO SSA1                                     
149600     MOVE '  GE' TO GODK-STATUSKODER                                      
149700     CALL CBLTDLI USING GNP WDK7-1-PCB DLI-IO-WDK711 SSA1                 
149800     MOVE WDK7-1-STATUS-CODE TO STATUS-WS                                 
149900     PERFORM IMS-STATUSKONTROLL                                           
150000     .                                                                    
150100     SKIP3                                                                
150200 IMS-GU-WDK711 SECTION.                                                   
150300                                                                          
150400     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
150500          DELIMITED BY SIZE INTO SSA1                                     
150600     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
150700          DELIMITED BY SIZE INTO SSA2                                     
150800     MOVE '  GE' TO GODK-STATUSKODER                                      
150900     CALL CBLTDLI USING GU WDK7-1-PCB DLI-IO-WDK711 SSA1 SSA2             
151000     MOVE WDK7-1-STATUS-CODE TO STATUS-WS                                 
151100     PERFORM IMS-STATUSKONTROLL                                           
151200     .                                                                    
151300 IMS-GHNP-WDK711 SECTION.                                                 
151400                                                                          
151500     STRING 'WDK711  *F(IDDC     =' W-IDDC-X ')'                          
151600          DELIMITED BY SIZE INTO SSA1                                     
151700     MOVE '  GE' TO GODK-STATUSKODER                                      
151800     CALL CBLTDLI USING GHNP WDK7-1-PCB DLI-IO-WDK711 SSA1                
151900     MOVE WDK7-1-STATUS-CODE TO STATUS-WS                                 
152000     PERFORM IMS-STATUSKONTROLL                                           
152100     .                                                                    
152200     EJECT                                                                
152300 IMS-GHNP-WDK711-MIN-MAX SECTION.                                         
152400                                                                          
152500     STRING 'WDK711  (IDDC    >=' W-IDDC-K7-MIN-X                         
152600                    '&IDDC    <=' W-IDDC-K7-MAX-X ')'                     
152700          DELIMITED BY SIZE INTO SSA1                                     
152800     MOVE '  GE' TO GODK-STATUSKODER                                      
152900     CALL CBLTDLI USING GHNP WDK7-1-PCB DLI-IO-WDK711 SSA1                
153000     MOVE WDK7-1-STATUS-CODE TO STATUS-WS                                 
153100     PERFORM IMS-STATUSKONTROLL                                           
153200     .                                                                    
153300     EJECT                                                                
153400 IMS-GNP-WDK712 SECTION.                                                  
153500                                                                          
153600     MOVE 'WDK712   ' TO SSA1                                             
153700     MOVE '  GE' TO GODK-STATUSKODER                                      
153800     CALL CBLTDLI USING GNP WDK7-1-PCB DLI-IO-WDK712 SSA1                 
153900     MOVE WDK7-1-STATUS-CODE TO STATUS-WS                                 
154000     PERFORM IMS-STATUSKONTROLL                                           
154100     .                                                                    
154200     SKIP3                                                                
154300 IMS-GU-WDK712 SECTION.                                                   
154400                                                                          
154500     STRING 'WDK712  (IDLAND   =' W-IDLAND-X ')'                          
154600          DELIMITED BY SIZE INTO SSA1                                     
154700     MOVE '  GE' TO GODK-STATUSKODER                                      
154800     CALL CBLTDLI USING GU WDK7-1-PCB DLI-IO-WDK712 SSA1                  
154900     MOVE WDK7-1-STATUS-CODE TO STATUS-WS                                 
155000     PERFORM IMS-STATUSKONTROLL                                           
155100     .                                                                    
155200     SKIP3                                                                
155300 IMS-GHU-WDK712 SECTION.                                                  
155400                                                                          
155500     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
155600          DELIMITED BY SIZE INTO SSA1                                     
155700     STRING 'WDK712  (IDLAND   =' W-IDLAND-X ')'                          
155800          DELIMITED BY SIZE INTO SSA2                                     
155900     MOVE '  GE' TO GODK-STATUSKODER                                      
156000     CALL CBLTDLI USING GHU WDK7-1-PCB DLI-IO-WDK712 SSA1 SSA2            
156100     MOVE WDK7-1-STATUS-CODE TO STATUS-WS                                 
156200     PERFORM IMS-STATUSKONTROLL                                           
156300     .                                                                    
156400     SKIP3                                                                
156500 IMS-GU-WDK721 SECTION.                                                   
156600                                                                          
156700     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
156800          DELIMITED BY SIZE INTO SSA1                                     
156900     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
157000          DELIMITED BY SIZE INTO SSA2                                     
157100     STRING 'WDK721  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
157200          DELIMITED BY SIZE INTO SSA3                                     
157300     MOVE '  GE' TO GODK-STATUSKODER                                      
157400     CALL CBLTDLI USING GU WDK7-2-PCB DLI-IO-WDK721                       
157500                            SSA1 SSA2 SSA3                                
157600     MOVE WDK7-2-STATUS-CODE TO STATUS-WS                                 
157700     PERFORM IMS-STATUSKONTROLL                                           
157800     .                                                                    
157900     EJECT                                                                
158000 IMS-GHU-WDK721 SECTION.                                                  
158100                                                                          
158200     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
158300          DELIMITED BY SIZE INTO SSA1                                     
158400     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
158500          DELIMITED BY SIZE INTO SSA2                                     
158600     STRING 'WDK721  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
158700          DELIMITED BY SIZE INTO SSA3                                     
158800     MOVE '  GE' TO GODK-STATUSKODER                                      
158900     CALL CBLTDLI USING GHU WDK7-2-PCB DLI-IO-WDK721                      
159000                            SSA1 SSA2 SSA3                                
159100     MOVE WDK7-2-STATUS-CODE TO STATUS-WS                                 
159200     PERFORM IMS-STATUSKONTROLL                                           
159300     .                                                                    
159400     EJECT                                                                
159500 IMS-REPL-WDK721 SECTION.                                                 
159600                                                                          
159700     MOVE '  ' TO GODK-STATUSKODER                                        
159800     CALL CBLTDLI USING REPL WDK7-2-PCB DLI-IO-WDK721                     
159900     MOVE WDK7-2-STATUS-CODE TO STATUS-WS                                 
160000     PERFORM IMS-STATUSKONTROLL                                           
160100     .                                                                    
160200     EJECT                                                                
160300 IMS-REPL-WDK711 SECTION.                                                 
160400                                                                          
160500     MOVE '  ' TO GODK-STATUSKODER                                        
160600     CALL CBLTDLI USING REPL WDK7-1-PCB DLI-IO-WDK711                     
160700     MOVE WDK7-1-STATUS-CODE TO STATUS-WS                                 
160800     PERFORM IMS-STATUSKONTROLL                                           
160900     .                                                                    
161000     EJECT                                                                
161100 IMS-REPL-WDK712 SECTION.                                                 
161200                                                                          
161300     MOVE '  ' TO GODK-STATUSKODER                                        
161400     CALL CBLTDLI USING REPL WDK7-1-PCB DLI-IO-WDK712                     
161500     MOVE WDK7-1-STATUS-CODE TO STATUS-WS                                 
161600     PERFORM IMS-STATUSKONTROLL                                           
161700     .                                                                    
161800     EJECT                                                                
161900 IMS-GU-WDD301-BSEQ SECTION.                                              
162000     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
162100          DELIMITED BY SIZE INTO SSA1                                     
162200     MOVE '  GE' TO GODK-STATUSKODER                                      
162300     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-WDD301 SSA1                    
162400     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
162500     PERFORM IMS-STATUSKONTROLL                                           
162600     .                                                                    
162700     SKIP3                                                                
162800 IMS-GNP-WDD311 SECTION.                                                  
162900     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
163000          DELIMITED BY SIZE INTO SSA1                                     
163100     MOVE '  GE' TO GODK-STATUSKODER                                      
163200     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-WDD311 SSA1                    
163300     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
163400     PERFORM IMS-STATUSKONTROLL                                           
163500     .                                                                    
163600     EJECT                                                                
163700 IMS-GU-WDB601    SECTION.                                                
163800     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
163900          DELIMITED BY SIZE INTO SSA1                                     
164000     MOVE '  GE' TO GODK-STATUSKODER                                      
164100     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
164200     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
164300     PERFORM IMS-STATUSKONTROLL                                           
164400     IF SEGMENT-SAKNAS                                                    
164500         MOVE SPACE TO DCS-KDDC                                           
164600     END-IF                                                               
164700     .                                                                    
164800 IMS-STATUSKONTROLL SECTION.                                              
164900                                                                          
165000     SET STATUS-IX TO 1                                                   
165100     SEARCH GODK-STATUS                                                   
165200       AT END                                                             
165300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
165400         DELIMITED BY SIZE INTO FELTEXT                                   
165500         CALL FELLOG                                                      
165600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
165700         CONTINUE                                                         
165800     END-SEARCH                                                           
165900     .                                                                    
166000 DB2-DCL-OPN-TP5IDDC-CRS  SECTION.                                        
166100                                                                          
166200     MOVE 000100  TO GOOD-SQLCODECODES                                    
166300                                                                          
166400     EXEC SQL                                                             
166500         DECLARE TP5IDDC-CRS CURSOR FOR                                   
166600                                                                          
166700           SELECT  IDDC, IDLOPNR_DC                                       
166800                                                                          
166900           FROM    TP5IDDC                                                
167000           WHERE   IDLOPNR_DC = (SELECT IDLOPNR_DC                        
167100                                 FROM TP5IDDC                             
167200                                 WHERE IDDC = :W-IDDC-TP5)                
167300           ORDER BY IDDC                                                  
167400     END-EXEC                                                             
167500                                                                          
167600     MOVE 000100  TO GOOD-SQLCODECODES                                    
167700     EXEC SQL OPEN TP5IDDC-CRS END-EXEC                                   
167800                                                                          
167900     .                                                                    
168000     SKIP3                                                                
168100 DB2-FETCH-TP5IDDC-CRS  SECTION.                                          
168200     SKIP2                                                                
168300     MOVE 000100  TO GOOD-SQLCODECODES                                    
168400     EXEC SQL                                                             
168500         FETCH TP5IDDC-CRS INTO :TP5IDDC-IDDC                             
168600                               ,:TP5IDDC-IDLOPNR-DC                       
168700     END-EXEC                                                             
168800                                                                          
168900     MOVE SQLCODE TO SQLCODE-WS                                           
169000     PERFORM DB2-STATUS-CHECK                                             
169100     .                                                                    
169200     SKIP3                                                                
169300 DB2-CLOSE-TP5IDDC-CRS  SECTION.                                          
169400                                                                          
169500     EXEC SQL CLOSE TP5IDDC-CRS END-EXEC                                  
169600     .                                                                    
169700     EJECT                                                                
169800 DB2-STATUS-CHECK  SECTION.                                               
169900                                                                          
170000     SET SQLCODE-IX TO 1                                                  
170100     SEARCH GOOD-SQLCODE                                                  
170200       AT END                                                             
170300*         STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
170400*         DELIMITED BY SIZE INTO ERROR-TEXT                               
170500          CALL ABEND USING RKOD-ABEND-DB2                                 
170600       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
170700     END-SEARCH                                                           
170800     .                                                                    
170900     EJECT                                                                
