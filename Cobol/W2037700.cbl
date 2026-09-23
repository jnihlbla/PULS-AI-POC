000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2037700.                                                
000300 AUTHOR.         BODIL LINDAHL.                                           
000400 DATE-WRITTEN.   98/12/22.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        UPPDATERAR DATAELEMENT SOM STYR REFILL                           
000900*                                                                         
001000*        PROGRAMMET UPPDATERAR WDK6                                       
001100*                              WDK7                                       
001200*                                                                         
001300*    INDATA.                                                              
001400*        TRANSAKTION: W2T377                                              
001500*        MID:         W2I37701                                            
001600*                                                                         
001700*    UTDATA.                                                              
001800*        MOD:         W2O37701                                            
001900*                                                                         
002000                                                                          
002100     SKIP3                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     EJECT                                                                
002400 DATA DIVISION.                                                           
002500 WORKING-STORAGE SECTION.                                                 
002600                                                                          
002700*    -COPY WY2000W2                                                       
002800     SKIP3                                                                
002900 77  IDPGM                       PIC X(08)   VALUE 'W2037700'.            
003000 01  FELTEXT.                                                             
003100     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
003200     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
003300 77  JA                          PIC X       VALUE 'J'.                   
003400 77  NEJ                         PIC X       VALUE 'N'.                   
003500                                                                          
003600 77  INDATA-SW                   PIC X       VALUE 'J'.                   
003700     88  INDATA-OK                           VALUE 'J'.                   
003800     88  INDATA-FEL                          VALUE 'N'.                   
003900                                                                          
004000 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
004100     88  NYCKLAR-OK                          VALUE 'J'.                   
004200     88  NYCKLAR-FEL                         VALUE 'N'.                   
004300                                                                          
004400 77  CN-UPD-SW                   PIC X       VALUE 'J'.                   
004500     88  CN-UPD-OK                           VALUE 'J'.                   
004600     88  CN-UPD-FEL                          VALUE 'N'.                   
004700                                                                          
004800 77  SW-LYNK-PART                PIC X       VALUE 'N'.                   
004900     88  LYNK-PART                           VALUE 'J'.                   
005000                                                                          
005100 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005200     88  EGEN-MID                            VALUE '2377'.                
005300     88  GODK-MID                            VALUE '2377'.                
005400     88  HELP-MID                            VALUE '0551'.                
005500 01  ARBETSFALT.                                                          
005600     03 IX                       PIC 9(2)    VALUE ZERO.                  
005700     03 DC-IX                    PIC 9(2)    VALUE ZERO.                  
005800     03 IX1                      PIC 9(2)    VALUE ZERO.                  
005900     03 IX2                      PIC 9(2)    VALUE ZERO.                  
006000     03 INDX                     PIC 9(3)    VALUE ZERO.                  
006100     03 WS-VECKA                 PIC 9(2)    VALUE ZERO.                  
006200     03 WS-FINNS-DC              PIC X       VALUE SPACE.                 
006300     03 WS-ISRT-DC               PIC X       VALUE SPACE.                 
006400     03 WS-IDLAND                PIC X(2)    VALUE SPACE.                 
006500     03 WS-IDDC-SPAR             PIC X(2)    VALUE SPACES.                
006600                                                                          
006700 01  WS-TIFINLV                  PIC 9(8)    VALUE ZERO.                  
006800 01  FILLER REDEFINES WS-TIFINLV.                                         
006900     03 WS-TIFINLV-SS            PIC 9(2).                                
007000     03 WS-TIFINLV-AAMMDD        PIC 9(6).                                
007100                                                                          
007200 01  WS-DAPUBL                   PIC 9(8)    VALUE ZERO.                  
007300 01  FILLER REDEFINES WS-DAPUBL.                                          
007400     03 WS-DAPUBL-SS             PIC 9(2).                                
007500     03 WS-DAPUBL-AAMMDD         PIC 9(6).                                
007600                                                                          
007700 01  DATUMFALT.                                                           
007800     03 DAGENS-DATUM             PIC S9(7) VALUE ZERO COMP-3.             
007900                                                                          
008000 01  WS-DAGENS-PLUS2-AAR         PIC 9(8)    VALUE ZERO.                  
008100 01  FILLER REDEFINES WS-DAGENS-PLUS2-AAR.                                
008200     03 WS-DAGENS-PLUS-SS        PIC 9(2).                                
008300     03 WS-DAGENS-PLUS2-AAMMDD   PIC 9(6).                                
008400                                                                          
008500 01  WS-IDPSN.                                                            
008600     03  WS-FIRST             PIC 9.                                      
008700     03  FILLER               PIC 9(2).                                   
008800     EJECT                                                                
008900*   -COPY WWPRODSL                                                        
009000     EJECT                                                                
009100 01  GENERELLA-SUBPROGRAM.                                                
009200     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
009300     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
009400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009600     03  ABEND                   PIC X(8)    VALUE 'ABEND  '.             
009700     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
009800     03  W400ARTU                PIC X(8)    VALUE 'W400ARTU'.            
009900     03  W005WDK7                PIC X(8)    VALUE 'W005WDK7'.            
010000     EJECT                                                                
010100*    --- PARAMETRAR TILL GEMENSAMMA SUBPROGRAM                            
010200 01 FILLER                       PIC X(8)    VALUE 'W005WDK7'.            
010300*   -COPY W005WDK7                                                        
010400     EJECT                                                                
010500*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
010600*01 -COPY WMEDAREA                                                        
010700     SKIP3                                                                
010800 01  MESSAGE-CODES.                                                       
010900     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
011000     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
011100     03  ERR-UPD-NOT-ALLOWED     PIC X(3)    VALUE '007'.                 
011200     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
011300     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
011400     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
011500     03  PART-MISSING            PIC X(3)    VALUE '017'.                 
011600     03  PART-SUPERSEDED         PIC X(3)    VALUE '018'.                 
011700     03  INF-NOT-REFILL          PIC X(3)    VALUE '957'.                 
011800     EJECT                                                                
011900 01  MEDDELANDEN.                                                         
012000     03  MED-1                   PIC X(40)                                
012100         VALUE 'NO REFILLPART                          '.                 
012200     03  MED-2                   PIC X(40)                                
012300         VALUE 'CDC PART                               '.                 
012400     03  MED-3                   PIC X(40)                                
012500         VALUE 'WRONG ORIGIN CODE                      '.                 
012600     03  MED-4                   PIC X(40)                                
012700         VALUE 'NO SPAREPART                           '.                 
012800     03  MED-5                   PIC X(40)                                
012900         VALUE 'UPDATING NOT ALLOWED - WRONG DC        '.                 
013000     03  MED-6                   PIC X(40)                                
013100         VALUE 'PART NOT AVAILABLE AT ANY DC           '.                 
013200     03  MED-10                  PIC X(40)                                
013300         VALUE 'STD PRICE IS ZERO                      '.                 
013400     03  MED-11                  PIC X(40)                                
013500         VALUE 'LYNK & CO PART                         '.                 
013600     EJECT                                                                
013700*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
013800 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
013900     SKIP3                                                                
014000*01 -COPY WMSGINIT                                                        
014100     SKIP3                                                                
014200*    --- DC CODES                                                         
014300 01  FILLER                      PIC X(16)   VALUE 'DC CODES'.            
014400     SKIP3                                                                
014500*01 -COPY WWDC99                                                          
014600     SKIP3                                                                
014700*    --- PARAMETRAR TILL ABEND                                            
014800 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
014900 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
015000     EJECT                                                                
015100 01 -COPY WDATAREA                                                        
015200     EJECT                                                                
015300 01  FILLER                      PIC X(16)   VALUE 'URSPRUNG'.            
015400*01     -COPY W400ARTU                                                    
015500     EJECT                                                                
015600*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
015700*                                                                         
015800 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
015900     SKIP3                                                                
016000*01  MID -COPY W2I37701                                                   
016100     EJECT                                                                
016200 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
016300     SKIP3                                                                
016400*01  -COPY WMSGAREA                                                       
016500     EJECT                                                                
016600     03  MOD REDEFINES MSG-AREA.                                          
016700*      05  -COPY W2O37701                                                 
016800     EJECT                                                                
016900 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
017000     SKIP3                                                                
017100*01  -COPY WMFSAREA                                                       
017200     EJECT                                                                
017300 01  FILLER                      PIC X(16)   VALUE 'BYTESOBJEKT'.         
017400     SKIP3                                                                
017500*01 -COPY WWBYT03                                                         
017600     EJECT                                                                
017700*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
017800*                                                                         
017900     SKIP3                                                                
018000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
018100     SKIP3                                                                
018200 01  NYCKLAR-TILL-DLI.                                                    
018300     03  W-IDARTNR-X.                                                     
018400         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
018500     03  W-KDSEGKEY-X.                                                    
018600         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
018700     03  W-IDDC-X.                                                        
018800         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
018900     03  W-IDLAND-X.                                                      
019000         05  W-IDLAND            PIC X(2)    VALUE SPACE.                 
019100     03  W-IDSKYLT-X.                                                     
019200         05  W-IDSKYLT           PIC X(3)    VALUE 'GB '.                 
019300                                                                          
019400     03  W-IDDC-B6-X.                                                     
019500         05 W-IDDC-B6                  PIC X(2).                          
019600                                                                          
019700     SKIP2                                                                
019800*    --- STATUS-KOD FRÅN IMS                                              
019900 01  STATUS-WS                   PIC XX.                                  
020000     88  SEGMENT-FINNS                       VALUE '  '.                  
020100     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
020200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
020300     SKIP2                                                                
020400 01  GODK-STATUSKODER.                                                    
020500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
020600     SKIP3                                                                
020700 01  SSA1                        PIC X(64).                               
020800 01  SSA2                        PIC X(64).                               
020900 01  SSA3                        PIC X(64).                               
021000     EJECT                                                                
021100*01  -COPY WWDCKONS                                                       
021200     EJECT                                                                
021300*    --- IMS FUNKTIONSKODER                                               
021400*01  -COPY W0003                                                          
021500     EJECT                                                                
021600*    ---  DLI INPUT-OUTPUT AREA                                           
021700 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK601'.                      
021800 01  DLI-IO-WDK601.                                                       
021900*    03  -COPY WDK601                                                     
022000     EJECT                                                                
022100 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK611'.                      
022200 01  DLI-IO-WDK611.                                                       
022300*    03  -COPY WDK611                                                     
022400     EJECT                                                                
022500 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK701'.                      
022600 01  DLI-IO-WDK701.                                                       
022700*    03  -COPY WDK701                                                     
022800     EJECT                                                                
022900 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK711'.                      
023000 01  DLI-IO-WDK711.                                                       
023100*    03  -COPY WDK711                                                     
023200     EJECT                                                                
023300 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK712'.                      
023400 01  DLI-IO-WDK712.                                                       
023500*    03  -COPY WDK712                                                     
023600     EJECT                                                                
023700 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK721'.                      
023800 01  DLI-IO-WDK721.                                                       
023900*    03  -COPY WDK721                                                     
024000     EJECT                                                                
024100 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDD301'.                      
024200 01  DLI-IO-WDD301.                                                       
024300*    03  -COPY WDD301  -PRE WDD3-                                         
024400     EJECT                                                                
024500 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDD311'.                      
024600 01  DLI-IO-WDD311.                                                       
024700*    03  -COPY WDD311  -PRE WDD3-                                         
024800     EJECT                                                                
024900                                                                          
025000 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
025100 01   DLI-IO-AREA-B601.                                                   
025200*     03  -COPY WDB601                                                    
025300                                                                          
025400 LINKAGE SECTION.                                                         
025500*01  -COPY W0009   -PRE MSG-                                              
025600     EJECT                                                                
025700*01  -COPY W0008   -PRE USEA-                                             
025800     05  FILLER                  PIC X.                                   
025900     EJECT                                                                
026000*01  -COPY W0008   -PRE WDK6-                                             
026100     05  FILLER                  PIC X.                                   
026200     EJECT                                                                
026300*01  -COPY W0008   -PRE WDK7-1-                                           
026400     05  FILLER                  PIC X.                                   
026500     EJECT                                                                
026600*01  -COPY W0008   -PRE WDK7-2-                                           
026700     05  FILLER                  PIC X.                                   
026800     EJECT                                                                
026900*01  -COPY W0008   -PRE WDD3-                                             
027000     05  FILLER                  PIC X.                                   
027100     EJECT                                                                
027200*01  -COPY W0008   -PRE WDB6-                                             
027300     05  FILLER                  PIC X.                                   
027400     EJECT                                                                
027500 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB                               
027600                           WDK6-PCB WDK7-1-PCB                            
027700                           WDK7-2-PCB                                     
027800                           WDD3-PCB WDB6-PCB.                             
027900 MAIN SECTION.                                                            
028000     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB                               
028100                           WDK6-PCB WDK7-1-PCB                            
028200                           WDK7-2-PCB                                     
028300                           WDD3-PCB WDB6-PCB.                             
028400                                                                          
028500     PERFORM IMS-GET-MSG                                                  
028600     IF SEGMENT-FINNS                                                     
028700        PERFORM A-INIT                                                    
028800        PERFORM B-KOLLA-NYCKLAR                                           
028900        IF NYCKLAR-OK                                                     
029000           IF MFS-UPDATE                                                  
029100              PERFORM G-KOLLA-INPUT                                       
029200              IF INDATA-OK                                                
029300                 PERFORM H-UPPDATERA                                      
029400              END-IF                                                      
029500           ELSE                                                           
029600              IF MFS-FIRST                                                
029700                 PERFORM C-FOERSTA-SIDA                                   
029800              ELSE                                                        
029900                 PERFORM E-SAMMA-SIDA                                     
030000              END-IF                                                      
030100           END-IF                                                         
030200           PERFORM F-LAES-VISA-INFO                                       
030300        END-IF                                                            
030400        COMPUTE MSG-KVLL = LENGTH OF MOD-W2O37701 + 4                     
030500        PERFORM IMS-INSERT-MSG                                            
030600     END-IF                                                               
030700                                                                          
030800     MOVE ZERO TO RETURN-CODE                                             
030900     GOBACK                                                               
031000     .                                                                    
031100     EJECT                                                                
031200 A-INIT SECTION.                                                          
031300                                                                          
031400     IF MSG-DUBBLA-TRANSKODER                                             
031500       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W2I37701                 
031600       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
031700       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
031800     ELSE                                                                 
031900       MOVE MSG-INDATA-MINUS-1-TRANSKOD TO MID-W2I37701                   
032000       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
032100       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
032200     END-IF                                                               
032300                                                                          
032400     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
032500     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
032600     MOVE MFS-IDTRANS TO W-IDTRANS                                        
032700                                                                          
032800     MOVE LOW-VALUE TO MSG-AREA                                           
032900     MOVE 'W2O377N1' TO MFS-IDMOD                                         
033000     MOVE '2377' TO MOD-IDTRANS                                           
033100     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
033200                                                                          
033300     IF EGEN-MID                                                          
033400        CONTINUE                                                          
033500     ELSE                                                                 
033600        MOVE SPACE TO MFS-KDTRTYP                                         
033700        MOVE '7' TO MFS-IDPFK                                             
033800     END-IF                                                               
033900                                                                          
034000     MOVE 'GB ' TO MED-IDSKYLT                                            
034100                                                                          
034200     ACCEPT DAGENS-DATUM FROM DATE                                        
034300     MOVE DAGENS-DATUM TO WS-DAGENS-PLUS2-AAMMDD                          
034400     ADD 20000  TO WS-DAGENS-PLUS2-AAMMDD                                 
034500     MOVE 20    TO WS-DAGENS-PLUS-SS                                      
034600     .                                                                    
034700     EJECT                                                                
034800 B-KOLLA-NYCKLAR SECTION.                                                 
034900                                                                          
035000     MOVE ALL '+'           TO MSGI-WMSGINIT                              
035100     MOVE '001'             TO MSGI-KDCALL                                
035200     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
035300     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
035400     MOVE '2377'            TO MSGI-IDTRANS                               
035500                                                                          
035600     IF GODK-MID                                                          
035700     OR (MID-IDARTNR-IN NUMERIC                                           
035800     AND MID-IDARTNR-IN > ZERO)                                           
035900        IF MID-IDARTNR-IN NOT = ALL '+'                                   
036000           MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                            
036100        END-IF                                                            
036200     END-IF                                                               
036300                                                                          
036400     IF GODK-MID                                                          
036500        MOVE MID-IDDC-IN TO MSGI-IDDC-KEY                                 
036600     END-IF                                                               
036700                                                                          
036800     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
036900     MOVE JA TO NYCKLAR-SW                                                
037000                                                                          
037100     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
037200     IF MID-IDARTNR-IN NOT = ALL '+'                                      
037300        MOVE '7'         TO MFS-IDPFK                                     
037400        MOVE SPACE       TO MFS-KDTRTYP                                   
037500     END-IF                                                               
037600     INSPECT MSGI-IDARTNR REPLACING LEADING SPACE BY ZERO                 
037700     IF MSGI-IDARTNR NUMERIC                                              
037800        MOVE MSGI-IDARTNR TO W-IDARTNR                                    
037900     ELSE                                                                 
038000        MOVE NEJ TO NYCKLAR-SW                                            
038100     END-IF                                                               
038200                                                                          
038300     MOVE MFS-RENSA-FAELT TO MOD-IDDC-IN                                  
038400     IF MID-IDDC-IN NOT = ALL '+'                                         
038500        MOVE '7'         TO MFS-IDPFK                                     
038600        MOVE SPACE       TO MFS-KDTRTYP                                   
038700     END-IF                                                               
038800                                                                          
038900     IF MSGI-IDDC-KEY NOT = DCS-IDDC                                      
039000        MOVE MSGI-IDDC-KEY TO W-IDDC-B6                                   
039100                              WS-IDDC-SPAR                                
039200        PERFORM IMS-GU-WDB601                                             
039300     END-IF                                                               
039600     IF DCS-NDC                                                           
039700        MOVE MSGI-IDDC-KEY TO W-IDDC                                      
039800        MOVE DCS-IDLANDX2  TO WS-IDLAND                                   
039900     ELSE                                                                 
040000        MOVE NEJ TO NYCKLAR-SW                                            
040100        IF DCS-KDDC = SPACE OR DCS-DDC                                    
040200           MOVE SPACE TO DCS-IDDC                                         
040300        ELSE                                                              
040400           CONTINUE                                                       
040500        END-IF                                                            
040600     END-IF                                                               
040700                                                                          
040800     MOVE MSGI-IDDC-KEY TO WS-IDDC                                        
040900                                                                          
041000     MOVE MSGI-IDARTNR TO MOD-IDARTNR-UT                                  
041100     INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE               
041200     MOVE DCS-IDDC     TO MOD-IDDC-UT                                     
041300                                                                          
041400***LYNK PARTS ONLY IN EUROPE                                              
041500     MOVE NEJ     TO SW-LYNK-PART                                         
041600     PERFORM IMS-GU-WDK601                                                
041700     IF SEGMENT-FINNS                                                     
041810       IF  ART-KDPRODSL > 30                                              
041820       AND ART-KDPRODSL < 40                                              
041900       AND NDC                                                            
042000         MOVE NEJ          TO NYCKLAR-SW                                  
042100         MOVE JA           TO SW-LYNK-PART                                
042200       END-IF                                                             
042300     END-IF                                                               
042400                                                                          
042500     IF NYCKLAR-FEL                                                       
042600       IF LYNK-PART                                                       
042700         MOVE MED-11          TO MOD-TEMFSFEL                             
042800       ELSE                                                               
042900         MOVE ERR-WRONG-KEY   TO MED-IDMFSFEL                             
043000         MOVE 'GB '       TO MED-IDSKYLT                                  
043100         CALL WMEDKONV USING MED-WMEDAREA                                 
043200         MOVE MED-MFSFEL      TO MOD-TEMFSFEL                             
043300       END-IF                                                             
043400       PERFORM MFS-RENSA-FAELT-IN                                         
043500       PERFORM MFS-RENSA-FAELT-UT                                         
043600     END-IF                                                               
043700     .                                                                    
043800     EJECT                                                                
043900 C-FOERSTA-SIDA SECTION.                                                  
044000                                                                          
044100     PERFORM MFS-RENSA-FAELT-IN                                           
044200     .                                                                    
044300     EJECT                                                                
044400 E-SAMMA-SIDA SECTION.                                                    
044500                                                                          
044600     IF EGEN-MID                                                          
044700        IF MID-KDCMD         = ALL '+'                                    
044800        AND MID-TIAAVVD-DC   = ALL '+'                                    
044900        AND MID-FLORDSP-EJRO = ALL '+'                                    
045000        AND MID-KDARTURS-DC  = ALL '+'                                    
045100        AND MID-IDPSN-DC     = ALL '+'                                    
045200           PERFORM MFS-RENSA-FAELT-IN                                     
045300        ELSE                                                              
045400           MOVE INF-PRESS-PF11 TO MED-IDMFSINF                            
045500           MOVE 'GB '         TO MED-IDSKYLT                              
045600           CALL WMEDKONV USING MED-WMEDAREA                               
045700           MOVE MED-TEMFSINF TO MOD-TEMFSINF                              
045800           PERFORM EA-MID-INDATA-TILL-MOD                                 
045900        END-IF                                                            
046000     ELSE                                                                 
046100        PERFORM MFS-RENSA-FAELT-IN                                        
046200     END-IF                                                               
046300     .                                                                    
046400     EJECT                                                                
046500 EA-MID-INDATA-TILL-MOD SECTION.                                          
046600                                                                          
046700     IF MID-KDCMD NOT = ALL '+'                                           
046800        MOVE MFS-ROER-EJ-FAELT  TO MOD-KDCMD-IN                           
046900     ELSE                                                                 
047000        MOVE MFS-RENSA-FAELT    TO MOD-KDCMD-IN                           
047100     END-IF                                                               
047200     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDCMD-IN-ATTR                      
047300                                                                          
047400     IF MID-TIAAVVD-DC NOT = ALL '+'                                      
047500        MOVE MFS-ROER-EJ-FAELT  TO MOD-TIAAVVD-DC-IN                      
047600     ELSE                                                                 
047700        MOVE MFS-RENSA-FAELT    TO MOD-TIAAVVD-DC-IN                      
047800     END-IF                                                               
047900     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TIAAVVD-DC-IN-ATTR                 
048000                                                                          
048100     IF MID-FLORDSP-EJRO NOT = ALL '+'                                    
048200        MOVE MFS-ROER-EJ-FAELT  TO MOD-FLORDSP-EJRO-IN                    
048300     ELSE                                                                 
048400        MOVE MFS-RENSA-FAELT    TO MOD-FLORDSP-EJRO-IN                    
048500     END-IF                                                               
048600     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLORDSP-EJRO-IN-ATTR               
048700                                                                          
048800     IF MID-KDARTURS-DC NOT = ALL '+'                                     
048900        MOVE MFS-ROER-EJ-FAELT  TO MOD-KDARTURS-DC-IN                     
049000     ELSE                                                                 
049100        MOVE MFS-RENSA-FAELT    TO MOD-KDARTURS-DC-IN                     
049200     END-IF                                                               
049300     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDARTURS-DC-IN-ATTR                
049400                                                                          
049500     IF MID-IDPSN-DC NOT = ALL '+'                                        
049600        MOVE MFS-ROER-EJ-FAELT  TO MOD-IDPSN-DC-IN                        
049700     ELSE                                                                 
049800        MOVE MFS-RENSA-FAELT    TO MOD-IDPSN-DC-IN                        
049900     END-IF                                                               
050000     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDPSN-DC-IN-ATTR                   
050100     .                                                                    
050200     EJECT                                                                
050300 F-LAES-VISA-INFO SECTION.                                                
050400                                                                          
050500     IF NDC-CN OR NDC-US                                                  
050600        PERFORM FC-CHECK-CN-SUPPLIER                                      
050700     ELSE                                                                 
050800        MOVE JA  TO INDATA-SW                                             
050900     END-IF                                                               
051000                                                                          
051100     IF INDATA-OK                                                         
051200        PERFORM FA-LAES-GRUNDDATA                                         
051300                                                                          
051400        IF SEGMENT-SAKNAS                                                 
051500           MOVE PART-MISSING TO MED-IDMFSFEL                              
051600           MOVE 'GB '        TO MED-IDSKYLT                               
051700           CALL WMEDKONV USING MED-WMEDAREA                               
051800           MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                              
051900           PERFORM MFS-RENSA-FAELT-UT                                     
052000           PERFORM MFS-STAENG-FAELT-IN                                    
052100        ELSE                                                              
052200           IF ART-KDERS-UTG > +0                                          
052300              MOVE PART-SUPERSEDED TO MED-IDMFSFEL                        
052400              MOVE 'GB '           TO MED-IDSKYLT                         
052500              CALL WMEDKONV USING MED-WMEDAREA                            
052600              MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                           
052700              PERFORM MFS-RENSA-FAELT-UT                                  
052800              PERFORM MFS-STAENG-FAELT-IN                                 
052900           ELSE                                                           
053000              MOVE ART-TIFINLV TO MOD-TIFINLV                             
053100              PERFORM FB-VISA                                             
053200           END-IF                                                         
053300        END-IF                                                            
053400     END-IF                                                               
053500     .                                                                    
053600     EJECT                                                                
053700 FA-LAES-GRUNDDATA SECTION.                                               
053800                                                                          
053900     PERFORM IMS-GU-WDK601                                                
054000     .                                                                    
054100     EJECT                                                                
054200 FB-VISA SECTION.                                                         
054300                                                                          
054400     PERFORM IMS-GET-WDK611                                               
054500     IF SEGMENT-FINNS                                                     
054600        MOVE CLAG-IDPSN    TO MOD-IDPSN                                   
054700        MOVE CLAG-KDARTURS TO MOD-KDARTURS                                
054800     ELSE                                                                 
054900        MOVE MFS-RENSA-FAELT TO MOD-IDPSN                                 
055000                                MOD-KDARTURS                              
055100     END-IF                                                               
055200                                                                          
055300     PERFORM IMS-GU-WDK701                                                
055400     IF SEGMENT-FINNS                                                     
055500        PERFORM IMS-GET-WDK711                                            
055600        IF SEGMENT-FINNS                                                  
055700           MOVE WS-IDLAND           TO W-IDLAND                           
055800           PERFORM IMS-GNP-WDK712                                         
055900           IF SEGMENT-FINNS                                               
056000              MOVE LART-KDARTURS    TO MOD-KDARTURS-DC                    
056100              MOVE LART-IDPSN-DC    TO MOD-IDPSN-DC                       
056200              IF LART-DAPUBL > ZERO                                       
056300                 MOVE 'AAMMDD'      TO DAT-KDDATFORM                      
056400                 MOVE LART-DAPUBL   TO DAT-I-TIDATUM                      
056500                 CALL WDATKONV USING   DAT-KDDATFORM                      
056600                                       DAT-I-TIDATUM                      
056700                                       DAT-O-TIDATUM                      
056800                                       DAT-KDSVAR                         
056900                                                                          
057000                 IF DAT-KDSVAR-OK                                         
057100                    MOVE DAT-TIAAVVD TO MOD-TIAAVVD-DC                    
057200                 ELSE                                                     
057300                    MOVE 'FEL FRÅN WDATKONV I FB-VISA' TO                 
057400                                          FELTEXT-STR                     
057500                    DISPLAY FELTEXT                                       
057600                    PERFORM S99-ABEND                                     
057700                 END-IF                                                   
057800              END-IF                                                      
057900           ELSE                                                           
058000              MOVE SPACE          TO MOD-KDARTURS-DC                      
058100              MOVE ART-TIFINLV    TO MOD-TIAAVVD-DC                       
058200              MOVE MFS-RENSA-FAELT                                        
058300                                  TO MOD-IDPSN-DC                         
058400              MOVE MFS-STAENG-FAELT-NOMOD                                 
058500                                  TO MOD-IDPSN-DC-IN-ATTR                 
058600           END-IF                                                         
058700           IF SLAG-FLORDSP-EJRO = 'J'                                     
058800              MOVE 'Y'               TO MOD-FLORDSP-EJRO                  
058900           ELSE                                                           
059000              MOVE SLAG-FLORDSP-EJRO TO MOD-FLORDSP-EJRO                  
059100           END-IF                                                         
059200           MOVE SLAG-IDPERSON-BUY TO MOD-IDPERSON-BUY                     
059300           MOVE SLAG-IDLEVNR      TO MOD-IDLEVNR                          
059400           MOVE MFS-STAENG-FAELT-NOMOD                                    
059500                                  TO MOD-KDCMD-IN-ATTR                    
059600           PERFORM IMS-GHU-WDK721                                         
059700           IF SEGMENT-FINNS                                               
059800              MOVE SBLK-IDUSER-ORDSP-EJRO                                 
059900                                  TO MOD-IDUSER-ORDSP-EJRO                
060000              IF SBLK-DAORDSP-EJRO > ZERO                                 
060100                 MOVE SBLK-DAORDSP-EJRO                                   
060200                                  TO MOD-DAORDSP-EJRO                     
060300              ELSE                                                        
060400                 MOVE MFS-RENSA-FAELT                                     
060500                                  TO MOD-DAORDSP-EJRO                     
060600              END-IF                                                      
060700           ELSE                                                           
060800                 MOVE MFS-RENSA-FAELT                                     
060900                                  TO MOD-DAORDSP-EJRO                     
061000                                     MOD-IDUSER-ORDSP-EJRO                
061100           END-IF                                                         
061200        ELSE                                                              
061300           MOVE MFS-RENSA-FAELT TO MOD-TIAAVVD-DC                         
061400                                   MOD-FLORDSP-EJRO                       
061500                                   MOD-KDARTURS-DC                        
061600                                   MOD-IDPSN-DC                           
061700                                   MOD-IDPERSON-BUY                       
061800                                   MOD-IDLEVNR                            
061900           MOVE MFS-STAENG-FAELT-NOMOD TO                                 
062000                                   MOD-FLORDSP-EJRO-IN-ATTR               
062100                                   MOD-KDARTURS-DC-IN-ATTR                
062200                                   MOD-IDPSN-DC-IN-ATTR                   
062300        END-IF                                                            
062400     ELSE                                                                 
062500        MOVE MFS-RENSA-FAELT TO MOD-TIAAVVD-DC                            
062600                                MOD-FLORDSP-EJRO                          
062700                                MOD-KDARTURS-DC                           
062800                                MOD-IDPSN-DC                              
062900                                MOD-IDPERSON-BUY                          
063000                                MOD-IDLEVNR                               
063100        MOVE MFS-STAENG-FAELT-NOMOD TO                                    
063200                                MOD-FLORDSP-EJRO-IN-ATTR                  
063300                                MOD-KDARTURS-DC-IN-ATTR                   
063400                                MOD-IDPSN-DC-IN-ATTR                      
063500     END-IF                                                               
063600                                                                          
063700     PERFORM IMS-GU-WDD301-BSEQ                                           
063800     IF SEGMENT-FINNS                                                     
063900       PERFORM IMS-GNP-WDD311                                             
064000       IF SEGMENT-FINNS                                                   
064100         MOVE WDD3-TEXT-BEART TO MOD-BEART-ENG                            
064200       ELSE                                                               
064300         MOVE MFS-RENSA-FAELT TO MOD-BEART-ENG                            
064400       END-IF                                                             
064500     ELSE                                                                 
064600       MOVE MFS-RENSA-FAELT   TO MOD-BEART-ENG                            
064700     END-IF                                                               
064800     .                                                                    
064900     EJECT                                                                
065000 FC-CHECK-CN-SUPPLIER SECTION.                                            
065100                                                                          
065200     PERFORM IMS-GU-WDK711                                                
065300     IF SEGMENT-FINNS                                                     
065400        IF SLAG-IDDC-REF = SPACES                                         
065500           MOVE INF-NOT-REFILL  TO MED-IDMFSINF                           
065600           MOVE 'GB '           TO MED-IDSKYLT                            
065700           CALL WMEDKONV USING MED-WMEDAREA                               
065800           MOVE MED-MFSINF      TO MOD-TEMFSINF                           
065900           PERFORM MFS-RENSA-FAELT-UT                                     
066000           PERFORM MFS-STAENG-FAELT-IN                                    
066100           MOVE NEJ TO INDATA-SW                                          
066200        ELSE                                                              
066300           MOVE JA  TO INDATA-SW                                          
066400        END-IF                                                            
066500     END-IF                                                               
066600     .                                                                    
066700     EJECT                                                                
066800 G-KOLLA-INPUT SECTION.                                                   
066900                                                                          
067000     MOVE WS-IDDC-SPAR           TO WS-IDDC                               
067100     IF NDC-NA                                                            
067200        MOVE NEJ                 TO INDATA-SW                             
067300        MOVE ERR-UPD-NOT-ALLOWED TO MED-IDMFSFEL                          
067400        MOVE 'GB '               TO MED-IDSKYLT                           
067500        CALL WMEDKONV         USING MED-WMEDAREA                          
067600        MOVE MED-TEMFSFEL        TO MOD-TEMFSFEL                          
067700     ELSE                                                                 
067800        MOVE JA TO INDATA-SW                                              
067900        IF  MID-KDCMD        = ALL '+'                                    
068000        AND MID-TIAAVVD-DC   = ALL '+'                                    
068100        AND MID-FLORDSP-EJRO = ALL '+'                                    
068200        AND MID-IDPSN-DC     = ALL '+'                                    
068300        AND MID-KDARTURS-DC  = ALL '+'                                    
068400          MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                       
068500          MOVE 'GB '                TO MED-IDSKYLT                        
068600          CALL WMEDKONV USING MED-WMEDAREA                                
068700          MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                               
068800          PERFORM MFS-ROER-EJ-FAELT-UT                                    
068900          PERFORM MFS-ROER-EJ-FAELT-IN                                    
069000          MOVE NEJ TO INDATA-SW                                           
069100        ELSE                                                              
069200           PERFORM GA-KOLLA-INPUT-1                                       
069300           IF CN-UPD-FEL                                                  
069400            MOVE ERR-UPD-NOT-ALLOWED  TO MED-IDMFSFEL                     
069500            MOVE 'GB '                TO MED-IDSKYLT                      
069600            CALL WMEDKONV USING MED-WMEDAREA                              
069700            MOVE MED-MFSFEL TO MOD-TEMFSFEL                               
069800            PERFORM MFS-ROER-EJ-FAELT-UT                                  
069900            PERFORM MFS-ROER-EJ-FAELT-IN                                  
070000            MOVE NEJ TO INDATA-SW                                         
070100           ELSE                                                           
070200             IF INDATA-FEL                                                
070300                MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                 
070400                MOVE 'GB '                TO MED-IDSKYLT                  
070500                CALL WMEDKONV USING MED-WMEDAREA                          
070600                MOVE MED-MFSFEL TO MOD-TEMFSFEL                           
070700                PERFORM MFS-ROER-EJ-FAELT-UT                              
070800                PERFORM MFS-ROER-EJ-FAELT-IN                              
070900             ELSE                                                         
071000               PERFORM GB-KOLLA-INPUT-2                                   
071100               IF INDATA-FEL                                              
071200                 MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                
071300                 MOVE 'GB '                TO MED-IDSKYLT                 
071400                 CALL WMEDKONV USING MED-WMEDAREA                         
071500                 MOVE MED-MFSFEL TO MOD-TEMFSFEL                          
071600                 PERFORM MFS-ROER-EJ-FAELT-UT                             
071700                 PERFORM MFS-ROER-EJ-FAELT-IN                             
071800               ELSE                                                       
071900                 PERFORM GC-KOLLA-IDDC                                    
072000                 IF INDATA-FEL                                            
072100                    MOVE MED-5 TO MOD-TEMFSFEL                            
072200                    PERFORM MFS-ROER-EJ-FAELT-UT                          
072300                    PERFORM MFS-ROER-EJ-FAELT-IN                          
072400                 END-IF                                                   
072500               END-IF                                                     
072600             END-IF                                                       
072700           END-IF                                                         
072800        END-IF                                                            
072900     END-IF                                                               
073000     .                                                                    
073100     EJECT                                                                
073200 GA-KOLLA-INPUT-1 SECTION.                                                
073300                                                                          
073400*DAPUBL                                                                   
073500     IF MID-TIAAVVD-DC NOT = ALL '+'                                      
073600        IF NDC-CN                                                         
073700           MOVE MFS-NUM-FAELT-FEL     TO MOD-TIAAVVD-DC-IN-ATTR           
073800           MOVE NEJ TO CN-UPD-SW                                          
073900        ELSE                                                              
074000          IF MID-TIAAVVD-DC NOT NUMERIC                                   
074100             MOVE MFS-NUM-FAELT-FEL   TO MOD-TIAAVVD-DC-IN-ATTR           
074200             MOVE NEJ TO INDATA-SW                                        
074300          ELSE                                                            
074400            IF MID-TIAAVVD-DC > 50000                                     
074500              MOVE MFS-NUM-FAELT-FEL  TO MOD-TIAAVVD-DC-IN-ATTR           
074600              MOVE NEJ TO INDATA-SW                                       
074700            ELSE                                                          
074800              MOVE MFS-NUM-FAELT-RAETT TO MOD-TIAAVVD-DC-IN-ATTR          
074900            END-IF                                                        
075000          END-IF                                                          
075100        END-IF                                                            
075200     END-IF                                                               
075300                                                                          
075400*KDARTURS-DC                                                              
075500     IF MID-KDARTURS-DC NOT = ALL '+'                                     
075600        IF NDC-CN                                                         
075700           MOVE MFS-NUM-FAELT-FEL      TO MOD-KDARTURS-DC-IN-ATTR         
075800           MOVE NEJ TO CN-UPD-SW                                          
075900        ELSE                                                              
076000           MOVE MFS-ALFA-FAELT-RAETT   TO MOD-KDARTURS-DC-IN-ATTR         
076100        END-IF                                                            
076200     END-IF                                                               
076300                                                                          
076400     IF CN-UPD-OK                                                         
076500*KDCMD                                                                    
076600       IF MID-KDCMD NOT = ALL '+'                                         
076700          IF MID-KDCMD = 'I'                                              
076701             MOVE ART-KDPRODSL TO TEST-KDPRODSL                           
076710             IF KDPRODSL-LOCAL AND NDC-CN                                 
076711               MOVE NEJ TO INDATA-SW                                      
076712               MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCMD-IN-ATTR               
076720             ELSE                                                         
076800               MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDCMD-IN-ATTR             
076810             END-IF                                                       
076900          ELSE                                                            
077000             MOVE NEJ TO INDATA-SW                                        
077100             MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDCMD-IN-ATTR               
077200          END-IF                                                          
077300       END-IF                                                             
077400                                                                          
077500*FLORDSP-EJRO                                                             
077600       IF MID-FLORDSP-EJRO NOT = ALL '+'                                  
077700          IF MID-FLORDSP-EJRO = 'J' OR 'Y' OR 'N'                         
077800             MOVE MFS-ALFA-FAELT-RAETT                                    
077900                              TO MOD-FLORDSP-EJRO-IN-ATTR                 
078000          ELSE                                                            
078100             MOVE MFS-ALFA-FAELT-FEL                                      
078200                              TO MOD-FLORDSP-EJRO-IN-ATTR                 
078300             MOVE NEJ TO INDATA-SW                                        
078400          END-IF                                                          
078500       END-IF                                                             
078600                                                                          
078700*IDPSN-DC                                                                 
078800       IF MID-IDPSN-DC NOT = ALL '+'                                      
078900          IF MID-IDPSN-DC NOT NUMERIC                                     
079000             MOVE MFS-NUM-FAELT-FEL   TO MOD-IDPSN-DC-IN-ATTR             
079100             MOVE NEJ TO INDATA-SW                                        
079200          ELSE                                                            
079300             MOVE MID-IDPSN-DC TO WS-IDPSN                                
079400             IF W-IDDC NOT = DCS-IDDC                                     
079500                MOVE W-IDDC TO W-IDDC-B6                                  
079600                PERFORM IMS-GU-WDB601                                     
079700             END-IF                                                       
079800             IF DCS-NDC-PF                                                
079810             OR DCS-NDC-SA                                                
079900             OR DCS-NDC-OTHERS                                            
080800               IF DCS-JAPAN                                               
080900                 IF WS-FIRST = 5                                          
081000                   MOVE MFS-NUM-FAELT-RAETT                               
081100                                          TO MOD-IDPSN-DC-IN-ATTR         
081200                 ELSE                                                     
081300                   MOVE MFS-NUM-FAELT-FEL                                 
081400                                          TO MOD-IDPSN-DC-IN-ATTR         
081500                   MOVE NEJ               TO INDATA-SW                    
081600                 END-IF                                                   
081700               ELSE                                                       
081800                 IF DCS-AUSTRALIA                                         
081900                   IF WS-FIRST = 6                                        
082000                     MOVE MFS-NUM-FAELT-RAETT                             
082100                                          TO MOD-IDPSN-DC-IN-ATTR         
082200                   ELSE                                                   
082300                     MOVE MFS-NUM-FAELT-FEL                               
082400                                          TO MOD-IDPSN-DC-IN-ATTR         
082500                     MOVE NEJ             TO INDATA-SW                    
082600                   END-IF                                                 
082700                 ELSE                                                     
082800**                 OTHER DCS BLOCKED FROM UPDATING PSN                    
082900                   MOVE MFS-NUM-FAELT-FEL                                 
083000                                          TO MOD-IDPSN-DC-IN-ATTR         
083100                   MOVE NEJ               TO INDATA-SW                    
083200                 END-IF                                                   
083300               END-IF                                                     
083500             END-IF                                                       
083600             IF DCS-NDC-CN                                                
083700               IF WS-FIRST = 7                                            
083800                 MOVE MFS-NUM-FAELT-RAETT TO MOD-IDPSN-DC-IN-ATTR         
083900               ELSE                                                       
084000                 MOVE MFS-NUM-FAELT-FEL   TO MOD-IDPSN-DC-IN-ATTR         
084100                 MOVE NEJ                 TO INDATA-SW                    
084200               END-IF                                                     
084300             END-IF                                                       
084400             IF DCS-NDC-NA                                                
084500               IF DCS-USA                                                 
084600                  IF WS-FIRST = 1                                         
084700                     MOVE MFS-NUM-FAELT-RAETT                             
084800                                          TO MOD-IDPSN-DC-IN-ATTR         
084900                  ELSE                                                    
085000                     MOVE MFS-NUM-FAELT-FEL                               
085100                                          TO MOD-IDPSN-DC-IN-ATTR         
085200                     MOVE NEJ             TO INDATA-SW                    
085300                  END-IF                                                  
085400               ELSE                                                       
085500                 IF DCS-CANADA                                            
085600                    IF WS-FIRST = 2                                       
085700                       MOVE MFS-NUM-FAELT-RAETT                           
085800                                          TO MOD-IDPSN-DC-IN-ATTR         
085900                    ELSE                                                  
086000                       MOVE MFS-NUM-FAELT-FEL                             
086100                                          TO MOD-IDPSN-DC-IN-ATTR         
086200                       MOVE NEJ           TO INDATA-SW                    
086300                    END-IF                                                
086400                 END-IF                                                   
086500               END-IF                                                     
086600             END-IF                                                       
086700          END-IF                                                          
086800       END-IF                                                             
086900     END-IF                                                               
087000     .                                                                    
087100     EJECT                                                                
087200 GB-KOLLA-INPUT-2 SECTION.                                                
087300                                                                          
087400     PERFORM IMS-GU-WDK601                                                
087500                                                                          
087600     IF ART-KDERS-UTG > ZERO                                              
087700        MOVE NEJ TO INDATA-SW                                             
087800        MOVE PART-SUPERSEDED TO MED-IDMFSFEL                              
087900        MOVE 'GB '           TO MED-IDSKYLT                               
088000        CALL WMEDKONV USING MED-WMEDAREA                                  
088100        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
088200     ELSE                                                                 
088300                                                                          
088400*KDCMD-GRP                                                                
088500*EJ FLLSRDEL                                                              
088600*EJ BYTES                                                                 
088700                                                                          
088800        PERFORM IMS-GET-WDK611                                            
088900        IF MID-KDCMD            = 'I'                                     
089000          IF DCS-NDC-PF OR DCS-INDIA OR DCS-EMIRATES                      
089100            CONTINUE                                                      
089200          ELSE                                                            
089300            IF CLAG-PRARTSTD = ZERO                                       
089400              MOVE NEJ              TO INDATA-SW                          
089500              MOVE MED-10           TO MOD-TEMFSINF                       
089600            END-IF                                                        
089700          END-IF                                                          
089800        END-IF                                                            
089900                                                                          
090000        IF CLAG-FLLSRDEL = NEJ                                            
090100           IF MID-KDCMD NOT = ALL '+'                                     
090200              MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCMD-IN-ATTR                
090300              MOVE NEJ   TO INDATA-SW                                     
090400              MOVE MED-4 TO MOD-TEMFSINF                                  
090500           END-IF                                                         
090600        ELSE                                                              
090700           IF MID-KDCMD NOT = ALL '+'                                     
090800              IF CLAG-FLREFILL = NEJ                                      
090900                 MOVE W-IDARTNR TO BYT03-IDARTNR                          
091000                 MOVE ART-KDPRODSL TO TEST-KDPRODSL                       
091100                 IF BYT03-OBJEKT                                          
091200                 OR KDPRODSL-BYTES                                        
091300                 OR KDPRODSL-LOCAL-BYTES                                  
091400                 OR ART-KDSORT = 'SW'                                     
091500                    CONTINUE                                              
091600                 ELSE                                                     
091700                    MOVE NEJ   TO INDATA-SW                               
091800                    MOVE MED-1 TO MOD-TEMFSINF                            
091900                    MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCMD-IN-ATTR          
092000                 END-IF                                                   
092100              END-IF                                                      
092200           END-IF                                                         
092300        END-IF                                                            
092400                                                                          
092500*DAPUBL                                                                   
092600        IF MID-TIAAVVD-DC NOT = ALL '+'                                   
092700           IF MID-TIAAVVD-DC = ZERO                                       
092800             MOVE MFS-NUM-FAELT-RAETT                                     
092900                             TO MOD-TIAAVVD-DC-IN-ATTR                    
093000           ELSE                                                           
093100             MOVE ART-TIFINLV  TO DAT-I-TIDATUM                           
093200             MOVE 'AAVVD'      TO DAT-KDDATFORM                           
093300             CALL WDATKONV USING DAT-KDDATFORM                            
093400                                 DAT-I-TIDATUM                            
093500                                 DAT-O-TIDATUM                            
093600                                 DAT-KDSVAR                               
093700                                                                          
093800             IF DAT-KDSVAR-OK                                             
093900                MOVE DAT-TIAAMMDD TO WS-TIFINLV-AAMMDD                    
094000                MOVE DAT-TISEKEL TO WS-TIFINLV-SS                         
094100             ELSE                                                         
094200                MOVE 'FEL WDATKONV I G-KOLLA' TO FELTEXT-STR              
094300                DISPLAY FELTEXT                                           
094400                PERFORM S99-ABEND                                         
094500             END-IF                                                       
094600                                                                          
094700             MOVE MID-TIAAVVD-DC TO DAT-I-TIDATUM                         
094800             MOVE 'AAVVD'      TO DAT-KDDATFORM                           
094900             CALL WDATKONV  USING DAT-KDDATFORM                           
095000                                    DAT-I-TIDATUM                         
095100                                    DAT-O-TIDATUM                         
095200                                    DAT-KDSVAR                            
095300                                                                          
095400             IF DAT-KDSVAR NOT = SPACE                                    
095500                MOVE MFS-NUM-FAELT-FEL                                    
095600                            TO MOD-TIAAVVD-DC-IN-ATTR                     
095700                MOVE NEJ TO INDATA-SW                                     
095800             ELSE                                                         
095900                MOVE DAT-TIAAMMDD TO WS-DAPUBL-AAMMDD                     
096000                MOVE DAT-TISEKEL TO WS-DAPUBL-SS                          
096100             END-IF                                                       
096200                                                                          
096300             IF WS-DAPUBL < WS-TIFINLV                                    
096400                MOVE MFS-NUM-FAELT-FEL                                    
096500                             TO MOD-TIAAVVD-DC-IN-ATTR                    
096600                MOVE NEJ TO INDATA-SW                                     
096700             ELSE                                                         
096800                IF WS-DAPUBL < WS-DAGENS-PLUS2-AAR                        
096900                  MOVE MFS-NUM-FAELT-RAETT                                
097000                               TO MOD-TIAAVVD-DC-IN-ATTR                  
097100                ELSE                                                      
097200                  MOVE MFS-NUM-FAELT-FEL                                  
097300                               TO MOD-TIAAVVD-DC-IN-ATTR                  
097400                  MOVE NEJ TO INDATA-SW                                   
097500                END-IF                                                    
097600             END-IF                                                       
097700           END-IF                                                         
097800        END-IF                                                            
097900                                                                          
098000        MOVE NEJ TO WS-ISRT-DC                                            
098100                    WS-FINNS-DC                                           
098200        PERFORM IMS-GU-WDK701                                             
098300        IF SEGMENT-FINNS                                                  
098400           PERFORM IMS-GET-WDK711                                         
098500           IF SEGMENT-SAKNAS                                              
098600              PERFORM GBA-KOLLA                                           
098700              IF MID-KDCMD = ALL '+'                                      
098800                 CONTINUE                                                 
098900              ELSE                                                        
099000                 MOVE JA TO WS-ISRT-DC                                    
099100              END-IF                                                      
099200           ELSE                                                           
099300              MOVE JA TO WS-FINNS-DC                                      
099400           END-IF                                                         
099500        ELSE                                                              
099600           PERFORM GBA-KOLLA                                              
099700           MOVE NEJ TO WS-FINNS-DC                                        
099800           IF MID-KDCMD = ALL '+'                                         
099900              CONTINUE                                                    
100000           ELSE                                                           
100100              MOVE JA TO WS-ISRT-DC                                       
100200           END-IF                                                         
100300        END-IF                                                            
100400                                                                          
100500        IF WS-FINNS-DC = NEJ                                              
100600        AND WS-ISRT-DC = NEJ                                              
100700           IF MID-TIAAVVD-DC NOT = ALL '+'                                
100800              MOVE MFS-NUM-FAELT-FEL                                      
100900                              TO MOD-TIAAVVD-DC-IN-ATTR                   
101000              MOVE NEJ        TO INDATA-SW                                
101100           END-IF                                                         
101200           IF MID-IDPSN-DC   NOT = ALL '+'                                
101300              MOVE MFS-ALFA-FAELT-FEL                                     
101400                              TO MOD-IDPSN-DC-IN-ATTR                     
101500              MOVE NEJ        TO INDATA-SW                                
101600           END-IF                                                         
101700        END-IF                                                            
101800*KDARTURS                                                                 
101900        IF MID-KDARTURS-DC NOT = ALL '+'                                  
102100           IF SLAG-IDDC-REF > SPACE                                       
102200              MOVE NEJ TO INDATA-SW                                       
102300              MOVE MFS-ALFA-FAELT-FEL                                     
102400                        TO MOD-KDARTURS-DC-IN-ATTR                        
102500              MOVE MED-2 TO MOD-TEMFSINF                                  
102600           ELSE                                                           
102700              MOVE MID-KDARTURS-DC TO ARTU-KDARTURS                       
102800              MOVE SPACE    TO ARTU-IDDC                                  
102900              MOVE ZERO     TO ARTU-IDDISTR                               
103000              CALL W400ARTU USING ARTU-W400ARTU                           
103100              IF ARTU-KDARTURS-NUM = ZERO                                 
103200                 MOVE NEJ TO INDATA-SW                                    
103300                 MOVE MFS-ALFA-FAELT-FEL                                  
103400                         TO MOD-KDARTURS-DC-IN-ATTR                       
103500                 MOVE MED-3 TO MOD-TEMFSINF                               
103600              END-IF                                                      
103700           END-IF                                                         
103800        END-IF                                                            
103900     END-IF                                                               
104000     .                                                                    
104100     EJECT                                                                
104200 GBA-KOLLA SECTION.                                                       
104300                                                                          
104400     IF MID-FLORDSP-EJRO NOT = ALL '+'                                    
104500        MOVE MFS-ALFA-FAELT-FEL TO MOD-FLORDSP-EJRO-IN-ATTR               
104600        MOVE NEJ TO INDATA-SW                                             
104700     END-IF                                                               
104800     IF MID-KDARTURS-DC NOT = ALL '+'                                     
104900        MOVE MFS-ALFA-FAELT-FEL TO MOD-KDARTURS-DC-IN-ATTR                
105000        MOVE NEJ TO INDATA-SW                                             
105100     END-IF                                                               
105200     .                                                                    
105300     EJECT                                                                
105400 GC-KOLLA-IDDC SECTION.                                                   
105500                                                                          
105600     IF W-IDDC NOT = DCS-IDDC                                             
105700        MOVE W-IDDC TO W-IDDC-B6                                          
105800        PERFORM IMS-GU-WDB601                                             
105900     END-IF                                                               
106010     IF DCS-NDC-PF OR DCS-NDC-OTHERS OR DCS-NDC-SA                        
106100        IF MSGI-IDDC NOT = DCS-IDDC                                       
106200           MOVE MSGI-IDDC TO W-IDDC-B6                                    
106300           PERFORM IMS-GU-WDB601                                          
106400        END-IF                                                            
106510        IF DCS-NDC-PF OR DCS-NDC-OTHERS OR DCS-CDC OR DCS-NDC-SA          
106600           CONTINUE                                                       
106700        ELSE                                                              
106800           MOVE NEJ TO INDATA-SW                                          
106900        END-IF                                                            
107000     ELSE                                                                 
107100        IF (DCS-NDC-CN AND DCS-CHINA)                                     
107300           IF MSGI-IDDC NOT = DCS-IDDC                                    
107400              MOVE MSGI-IDDC TO W-IDDC-B6                                 
107500              PERFORM IMS-GU-WDB601                                       
107600           END-IF                                                         
107700           IF (DCS-NDC-CN AND DCS-CHINA)                                  
107900           OR DCS-CDC                                                     
108000              CONTINUE                                                    
108100           ELSE                                                           
108200              MOVE NEJ TO INDATA-SW                                       
108300           END-IF                                                         
108400        ELSE                                                              
108500           IF DCS-NDC-NA AND DCS-USA                                      
108600              IF MSGI-IDDC NOT = DCS-IDDC                                 
108700                 MOVE MSGI-IDDC TO W-IDDC-B6                              
108800                 PERFORM IMS-GU-WDB601                                    
108900              END-IF                                                      
109000              IF (DCS-NDC-NA AND DCS-USA) OR DCS-CDC                      
109100                 CONTINUE                                                 
109200              ELSE                                                        
109300                 MOVE NEJ TO INDATA-SW                                    
109400              END-IF                                                      
109500           ELSE                                                           
109600              IF DCS-NDC-NA AND DCS-CANADA                                
109700                 IF MSGI-IDDC NOT = DCS-IDDC                              
109800                    MOVE MSGI-IDDC TO W-IDDC-B6                           
109900                    PERFORM IMS-GU-WDB601                                 
110000                 END-IF                                                   
110100                 IF (DCS-NDC-NA AND DCS-CANADA) OR DCS-CDC                
110200                    CONTINUE                                              
110300                 ELSE                                                     
110400                    MOVE NEJ TO INDATA-SW                                 
110500                 END-IF                                                   
110600              END-IF                                                      
110700           END-IF                                                         
110800        END-IF                                                            
110900     END-IF                                                               
111000     .                                                                    
111100     EJECT                                                                
111200 H-UPPDATERA SECTION.                                                     
111300                                                                          
111400     IF MID-KDCMD NOT = ALL '+'                                           
111500        PERFORM IMS-GU-WDK711                                             
111600        IF SEGMENT-SAKNAS                                                 
111700           MOVE W-IDDC TO W-IDDC-B6                                       
111800           PERFORM IMS-GU-WDB601                                          
111900           PERFORM HA-NYA-WDK711-21-SEGMENT                               
112000        END-IF                                                            
112100     END-IF                                                               
112200                                                                          
112300     IF MID-TIAAVVD-DC    = ALL '+'                                       
112400     AND MID-FLORDSP-EJRO = ALL '+'                                       
112500     AND MID-KDARTURS-DC  = ALL '+'                                       
112600     AND MID-IDPSN-DC     = ALL '+'                                       
112700        CONTINUE                                                          
112800     ELSE                                                                 
112900        MOVE W-IDDC TO W-IDDC-B6                                          
113000        PERFORM IMS-GU-WDB601                                             
113100        PERFORM HC-UPPDATERA-WDK7                                         
113200     END-IF                                                               
113300                                                                          
113400     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
113500     MOVE 'GB '         TO MED-IDSKYLT                                    
113600     CALL WMEDKONV USING MED-WMEDAREA                                     
113700     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
113800     PERFORM MFS-FORM-ATTR                                                
113900     PERFORM MFS-RENSA-FAELT-IN                                           
114000     .                                                                    
114100     EJECT                                                                
114200 HA-NYA-WDK711-21-SEGMENT SECTION.                                        
114300                                                                          
114400     MOVE ALL '+'      TO WDK7-W005WDK7                                   
114500     MOVE 'WDK711'     TO WDK7-IDSEGM                                     
114600     MOVE W-IDARTNR    TO WDK7-IDARTNR-KFB                                
114700     MOVE W-IDDC       TO WDK7-IDDC-KFB                                   
114800                          WDK7-IDDC                                       
114900     IF BYT03-OBJEKT                                                      
115000       MOVE NEJ         TO WDK7-FLREFILL                                  
115100     END-IF                                                               
115200     CALL W005WDK7 USING WDK7-W005WDK7 WDB6-PCB                           
115300                                       WDK6-PCB WDK7-1-PCB                
115400                                                                          
115500     MOVE ALL '+'        TO WDK7-W005WDK7                                 
115600     MOVE 'WDK721'       TO WDK7-IDSEGM                                   
115700     MOVE W-IDARTNR      TO WDK7-IDARTNR-KFB                              
115800     MOVE W-IDDC         TO WDK7-IDDC-KFB                                 
115900     MOVE '1'            TO WDK7-KDSEGKEY                                 
116000                         IN WDK7-WDK721                                   
116100     MOVE DAGENS-DATUM   TO WDK7-DAORDSP-EJRO                             
116200     MOVE MSGI-IDUSER    TO WDK7-IDUSER-ORDSP-EJRO                        
116300     CALL W005WDK7 USING WDK7-W005WDK7 WDB6-PCB                           
116400                                       WDK6-PCB WDK7-2-PCB                
116500     .                                                                    
116600     EJECT                                                                
116700 HC-UPPDATERA-WDK7 SECTION.                                               
116800                                                                          
116900     PERFORM IMS-GU-WDK701                                                
117000     PERFORM IMS-GET-WDK711                                               
117100     IF SEGMENT-FINNS                                                     
117200        IF MID-FLORDSP-EJRO NOT = ALL '+'                                 
117300           IF MID-FLORDSP-EJRO NOT = SLAG-FLORDSP-EJRO                    
117400             MOVE SLAG-IDDC   TO W-IDDC                                   
117500             PERFORM IMS-GHU-WDK721                                       
117600             IF SEGMENT-FINNS                                             
117700               MOVE DAGENS-DATUM     TO SBLK-DAORDSP-EJRO                 
117800               MOVE MSGI-IDUSER      TO SBLK-IDUSER-ORDSP-EJRO            
117900               PERFORM IMS-REPL-WDK721                                    
118000             ELSE                                                         
118100               MOVE ALL '+'        TO WDK7-W005WDK7                       
118200               MOVE 'WDK721'       TO WDK7-IDSEGM                         
118300               MOVE W-IDARTNR      TO WDK7-IDARTNR-KFB                    
118400               MOVE W-IDDC         TO WDK7-IDDC-KFB                       
118500               MOVE DAGENS-DATUM   TO WDK7-DAORDSP-EJRO                   
118600               MOVE MSGI-IDUSER    TO WDK7-IDUSER-ORDSP-EJRO              
118700               MOVE '1'            TO WDK7-KDSEGKEY                       
118800                                   IN WDK7-WDK721                         
118900               CALL W005WDK7 USING WDK7-W005WDK7                          
119000                          WDB6-PCB WDK6-PCB WDK7-2-PCB                    
119100             END-IF                                                       
119200           END-IF                                                         
119300           IF MID-FLORDSP-EJRO = 'Y'                                      
119400              MOVE 'J'              TO SLAG-FLORDSP-EJRO                  
119500           ELSE                                                           
119600              MOVE MID-FLORDSP-EJRO TO SLAG-FLORDSP-EJRO                  
119700           END-IF                                                         
119800           IF MID-FLORDSP-EJRO = 'Y' OR 'J'                               
119900              MOVE ZERO             TO SLAG-KVPB-REF                      
120000                                       SLAG-KVPB-HIST                     
120100                                       SLAG-KVPBREOI                      
120200                                       SLAG-KVPBREOI-HIST                 
120300              MOVE 'P'              TO SLAG-KDREFSTA                      
120400              MOVE DAGENS-DATUM     TO SLAG-TIREFSTA                      
120500           END-IF                                                         
120600        END-IF                                                            
120700        PERFORM IMS-REPL-WDK711                                           
120800*                                                                         
120900                                                                          
121000        IF MID-TIAAVVD-DC   NOT = ALL '+'                                 
121100        OR MID-IDPSN-DC     NOT = ALL '+'                                 
121200        OR MID-KDARTURS-DC  NOT = ALL '+'                                 
121300           MOVE DCS-IDLANDX2    TO W-IDLAND                               
121400           PERFORM IMS-GET-WDK712                                         
121500           IF SEGMENT-FINNS                                               
121600             IF MID-TIAAVVD-DC NOT = ALL '+'                              
121700                MOVE WS-DAPUBL      TO LART-DAPUBL                        
121800             END-IF                                                       
121900*                                                                         
122000             IF MID-IDPSN-DC   NOT = ALL '+'                              
122100                MOVE MID-IDPSN-DC   TO LART-IDPSN-DC                      
122200             END-IF                                                       
122300*                                                                         
122400             IF MID-KDARTURS-DC NOT = ALL '+'                             
122500                MOVE MID-KDARTURS-DC TO LART-KDARTURS                     
122600             END-IF                                                       
122700*                                                                         
122800             PERFORM IMS-REPL-WDK712                                      
122900           ELSE                                                           
123000             MOVE ALL '+'        TO WDK7-W005WDK7                         
123100             MOVE 'WDK712'       TO WDK7-IDSEGM                           
123200             MOVE W-IDARTNR      TO WDK7-IDARTNR-KFB                      
123300             MOVE W-IDLAND       TO WDK7-IDLANDX2                         
123400             IF MID-TIAAVVD-DC NOT = ALL '+'                              
123500                MOVE WS-DAPUBL      TO WDK7-DAPUBL                        
123600             END-IF                                                       
123700             IF MID-IDPSN-DC   NOT = ALL '+'                              
123800                MOVE MID-IDPSN-DC   TO WDK7-IDPSN-DC                      
123900             END-IF                                                       
124000             IF MID-KDARTURS-DC NOT = ALL '+'                             
124100                MOVE MID-KDARTURS-DC TO WDK7-KDARTURS                     
124200             END-IF                                                       
124300             CALL W005WDK7 USING WDK7-W005WDK7 WDB6-PCB                   
124400                                 WDK6-PCB WDK7-1-PCB                      
124500           END-IF                                                         
124600        END-IF                                                            
124700     END-IF                                                               
124800     .                                                                    
124900     EJECT                                                                
125000                                                                          
125100 S99-ABEND SECTION.                                                       
125200                                                                          
125300     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
125400     .                                                                    
125500     EJECT                                                                
125600 MFS-RENSA-FAELT-UT SECTION.                                              
125700                                                                          
125800*    --- ALLA UTDATA-FÄLT                                                 
125900     MOVE MFS-RENSA-FAELT TO MOD-BEART-ENG                                
126000                             MOD-TIAAVVD-DC                               
126100                             MOD-FLORDSP-EJRO                             
126200                             MOD-KDARTURS-DC                              
126300                             MOD-IDPSN-DC                                 
126400                             MOD-IDLEVNR                                  
126500                             MOD-IDPERSON-BUY                             
126600                             MOD-KDARTURS                                 
126700                             MOD-IDPSN                                    
126800                             MOD-TIFINLV                                  
126900     .                                                                    
127000     SKIP3                                                                
127100 MFS-RENSA-FAELT-IN SECTION.                                              
127200                                                                          
127300*    --- ALLA INDATA-FÄLT                                                 
127400     MOVE MFS-RENSA-FAELT TO MOD-KDCMD-IN                                 
127500                             MOD-TIAAVVD-DC-IN                            
127600                             MOD-FLORDSP-EJRO-IN                          
127700                             MOD-KDARTURS-DC-IN                           
127800                             MOD-IDPSN-DC-IN                              
127900     .                                                                    
128000     EJECT                                                                
128100 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
128200                                                                          
128300*    --- ALLA UTDATA-FÄLT                                                 
128400     MOVE MFS-ROER-EJ-FAELT TO MOD-BEART-ENG                              
128500                               MOD-TIAAVVD-DC                             
128600                               MOD-FLORDSP-EJRO                           
128700                               MOD-KDARTURS-DC                            
128800                               MOD-IDPSN-DC                               
128900                               MOD-IDLEVNR                                
129000                               MOD-IDPERSON-BUY                           
129100                               MOD-KDARTURS                               
129200                               MOD-IDPSN                                  
129300                               MOD-TIFINLV                                
129400     .                                                                    
129500     SKIP3                                                                
129600 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
129700                                                                          
129800*    --- ALLA INDATA-FÄLT                                                 
129900     MOVE MFS-ROER-EJ-FAELT TO MOD-KDCMD-IN                               
130000                               MOD-TIAAVVD-DC-IN                          
130100                               MOD-FLORDSP-EJRO-IN                        
130200                               MOD-KDARTURS-DC-IN                         
130300                               MOD-IDPSN-DC-IN                            
130400     .                                                                    
130500     EJECT                                                                
130600 MFS-FORM-ATTR SECTION.                                                   
130700                                                                          
130800*    --- ALLA INDATA-FÄLT                                                 
130900     MOVE MFS-FORMATETS-ATTR TO MOD-KDCMD-IN-ATTR                         
131000                                MOD-TIAAVVD-DC-IN-ATTR                    
131100                                MOD-FLORDSP-EJRO-IN-ATTR                  
131200                                MOD-KDARTURS-DC-IN-ATTR                   
131300                                MOD-IDPSN-DC-IN-ATTR                      
131400     .                                                                    
131500     SKIP2                                                                
131600 MFS-STAENG-FAELT-IN SECTION.                                             
131700                                                                          
131800*    --- ALLA INDATA-FÄLT                                                 
131900     MOVE MFS-STAENG-FAELT-NOMOD TO                                       
132000                    MOD-KDCMD-IN-ATTR                                     
132100                    MOD-TIAAVVD-DC-IN-ATTR                                
132200                    MOD-FLORDSP-EJRO-IN-ATTR                              
132300                    MOD-KDARTURS-DC-IN-ATTR                               
132400                    MOD-IDPSN-DC-IN-ATTR                                  
132500     .                                                                    
132600     EJECT                                                                
132700* --- IMS SEKTIONER ---                                                   
132800                                                                          
132900 IMS-GET-MSG SECTION.                                                     
133000                                                                          
133100     MOVE '  QC' TO GODK-STATUSKODER                                      
133200     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
133300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
133400     PERFORM IMS-STATUSKONTROLL                                           
133500     .                                                                    
133600     SKIP2                                                                
133700 IMS-INSERT-MSG SECTION.                                                  
133800                                                                          
133900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
134000     MOVE SPACE TO GODK-STATUSKODER                                       
134100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
134200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
134300     PERFORM IMS-STATUSKONTROLL                                           
134400     .                                                                    
134500     EJECT                                                                
134600 IMS-GU-WDK601 SECTION.                                                   
134700     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
134800          DELIMITED BY SIZE INTO SSA1                                     
134900     MOVE '  GE' TO GODK-STATUSKODER                                      
135000     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
135100     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
135200     PERFORM IMS-STATUSKONTROLL                                           
135300     .                                                                    
135400     SKIP3                                                                
135500 IMS-GET-WDK611 SECTION.                                                  
135600     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
135700          DELIMITED BY SIZE INTO SSA1                                     
135800     MOVE '  GE' TO GODK-STATUSKODER                                      
135900     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
136000     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
136100     PERFORM IMS-STATUSKONTROLL                                           
136200     .                                                                    
136300     EJECT                                                                
136400 IMS-GHU-WDK611 SECTION.                                                  
136500     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
136600          DELIMITED BY SIZE INTO SSA1                                     
136700     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
136800          DELIMITED BY SIZE INTO SSA2                                     
136900     MOVE '  ' TO GODK-STATUSKODER                                        
137000     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2              
137100     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
137200     PERFORM IMS-STATUSKONTROLL                                           
137300     .                                                                    
137400     EJECT                                                                
137500 IMS-REPL-WDK611 SECTION.                                                 
137600     MOVE '  ' TO GODK-STATUSKODER                                        
137700     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK611                       
137800     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
137900     PERFORM IMS-STATUSKONTROLL                                           
138000     .                                                                    
138100     EJECT                                                                
138200 IMS-GU-WDK701 SECTION.                                                   
138300     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
138400          DELIMITED BY SIZE INTO SSA1                                     
138500     MOVE '  GE' TO GODK-STATUSKODER                                      
138600     CALL CBLTDLI USING GU WDK7-1-PCB DLI-IO-WDK701 SSA1                  
138700     MOVE WDK7-1-STATUS-CODE TO STATUS-WS                                 
138800     PERFORM IMS-STATUSKONTROLL                                           
138900     .                                                                    
139000     SKIP3                                                                
139100 IMS-GU-WDK711 SECTION.                                                   
139200                                                                          
139300     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
139400          DELIMITED BY SIZE INTO SSA1                                     
139500     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
139600          DELIMITED BY SIZE INTO SSA2                                     
139700     MOVE '  GE' TO GODK-STATUSKODER                                      
139800     CALL CBLTDLI USING GU WDK7-1-PCB DLI-IO-WDK711 SSA1 SSA2             
139900     MOVE WDK7-1-STATUS-CODE TO STATUS-WS                                 
140000     PERFORM IMS-STATUSKONTROLL                                           
140100     .                                                                    
140200     SKIP3                                                                
140300                                                                          
140400 IMS-GET-WDK711 SECTION.                                                  
140500     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
140600          DELIMITED BY SIZE INTO SSA1                                     
140700     MOVE '  GE' TO GODK-STATUSKODER                                      
140800     CALL CBLTDLI USING GHNP WDK7-1-PCB DLI-IO-WDK711 SSA1                
140900     MOVE WDK7-1-STATUS-CODE TO STATUS-WS                                 
141000     PERFORM IMS-STATUSKONTROLL                                           
141100     .                                                                    
141200     EJECT                                                                
141300 IMS-GET-WDK712 SECTION.                                                  
141400     STRING 'WDK712  (IDLAND   =' W-IDLAND-X ')'                          
141500          DELIMITED BY SIZE INTO SSA1                                     
141600     MOVE '  GE' TO GODK-STATUSKODER                                      
141700     CALL CBLTDLI USING GHNP WDK7-1-PCB DLI-IO-WDK712 SSA1                
141800     MOVE WDK7-1-STATUS-CODE TO STATUS-WS                                 
141900     PERFORM IMS-STATUSKONTROLL                                           
142000     .                                                                    
142100     EJECT                                                                
142200 IMS-GNP-WDK712 SECTION.                                                  
142300     STRING 'WDK712  (IDLAND   =' W-IDLAND-X ')'                          
142400          DELIMITED BY SIZE INTO SSA1                                     
142500     MOVE '  GE' TO GODK-STATUSKODER                                      
142600     CALL CBLTDLI USING GNP WDK7-1-PCB DLI-IO-WDK712 SSA1                 
142700     MOVE WDK7-1-STATUS-CODE TO STATUS-WS                                 
142800     PERFORM IMS-STATUSKONTROLL                                           
142900     .                                                                    
143000     EJECT                                                                
143100 IMS-GHU-WDK721 SECTION.                                                  
143200                                                                          
143300     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
143400          DELIMITED BY SIZE INTO SSA1                                     
143500     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
143600          DELIMITED BY SIZE INTO SSA2                                     
143700     STRING 'WDK721  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
143800          DELIMITED BY SIZE INTO SSA3                                     
143900     MOVE '  GE' TO GODK-STATUSKODER                                      
144000     CALL CBLTDLI USING GHU WDK7-2-PCB DLI-IO-WDK721                      
144100                            SSA1 SSA2 SSA3                                
144200     MOVE WDK7-2-STATUS-CODE TO STATUS-WS                                 
144300     PERFORM IMS-STATUSKONTROLL                                           
144400     .                                                                    
144500     EJECT                                                                
144600 IMS-REPL-WDK721 SECTION.                                                 
144700                                                                          
144800     MOVE '  ' TO GODK-STATUSKODER                                        
144900     CALL CBLTDLI USING REPL WDK7-2-PCB DLI-IO-WDK721                     
145000     MOVE WDK7-2-STATUS-CODE TO STATUS-WS                                 
145100     PERFORM IMS-STATUSKONTROLL                                           
145200     .                                                                    
145300     EJECT                                                                
145400 IMS-REPL-WDK711 SECTION.                                                 
145500     MOVE '  ' TO GODK-STATUSKODER                                        
145600     CALL CBLTDLI USING REPL WDK7-1-PCB DLI-IO-WDK711                     
145700     MOVE WDK7-1-STATUS-CODE TO STATUS-WS                                 
145800     PERFORM IMS-STATUSKONTROLL                                           
145900     .                                                                    
146000     EJECT                                                                
146100 IMS-REPL-WDK712 SECTION.                                                 
146200     MOVE '  ' TO GODK-STATUSKODER                                        
146300     CALL CBLTDLI USING REPL WDK7-1-PCB DLI-IO-WDK712                     
146400     MOVE WDK7-1-STATUS-CODE TO STATUS-WS                                 
146500     PERFORM IMS-STATUSKONTROLL                                           
146600     .                                                                    
146700     EJECT                                                                
146800 IMS-GU-WDD301-BSEQ SECTION.                                              
146900     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
147000          DELIMITED BY SIZE INTO SSA1                                     
147100     MOVE '  GE' TO GODK-STATUSKODER                                      
147200     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-WDD301 SSA1                    
147300     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
147400     PERFORM IMS-STATUSKONTROLL                                           
147500     .                                                                    
147600     SKIP3                                                                
147700 IMS-GNP-WDD311 SECTION.                                                  
147800     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
147900          DELIMITED BY SIZE INTO SSA1                                     
148000     MOVE '  GE' TO GODK-STATUSKODER                                      
148100     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-WDD311 SSA1                    
148200     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
148300     PERFORM IMS-STATUSKONTROLL                                           
148400     .                                                                    
148500     EJECT                                                                
148600 IMS-GU-WDB601    SECTION.                                                
148700     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
148800          DELIMITED BY SIZE INTO SSA1                                     
148900     MOVE '  GE' TO GODK-STATUSKODER                                      
149000     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
149100     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
149200     PERFORM IMS-STATUSKONTROLL                                           
149300     IF SEGMENT-SAKNAS                                                    
149400         MOVE SPACE TO DCS-KDDC                                           
149500     END-IF                                                               
149600     .                                                                    
149700 IMS-STATUSKONTROLL SECTION.                                              
149800     SET STATUS-IX TO 1                                                   
149900     SEARCH GODK-STATUS                                                   
150000       AT END                                                             
150100         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
150200         DELIMITED BY SIZE INTO FELTEXT                                   
150300         CALL FELLOG                                                      
150400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
150500         CONTINUE                                                         
150600     END-SEARCH                                                           
150700     .                                                                    
150800     EJECT                                                                
