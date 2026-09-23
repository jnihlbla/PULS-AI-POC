000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4036500.                                                
000400 AUTHOR.         CAMELIA OLGRENER.                                        
000500 DATE-WRITTEN.   90/03/20.                                                
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION.                                                            
001000*        PROGRAMMET HANTERAR VILKA PRINTRAR SOM KOMMER IFRÅGA             
001100*        FÖR VISSA LAGEROMRÅDEN. VIDARE BESTÄMS OM DET SKAPAS             
001200*        NY ORDERDEL FÖR VISSA LAGOMR (FÖRDRÖJD UTSKRIFT).                
001300*        HÄR BESTÄMS OCKSÅ OM VISSA LAGOMR SKALL SKRIVAS EFTER            
001400*        SIDSKIP (PAGESKIP PER PRINTER).                                  
001500*                                                                         
001600*        PROGRAMMET ÄR EN UPPDATERINGS-MPP                                
001700*        PROGRAMMET UPPDATERAR WLXXKL (WDR1)                              
001800*                                                                         
001900*                                                                         
002000*    INDATA.                                                              
002100*        TRANSAKTION: W4T365  W4T365U                                     
002200*        MID:         W4I36501                                            
002300*                                                                         
002400*    UTDATA.                                                              
002500*        MOD:         W4O36501                                            
002600*                                                                         
002700* LINDA NILSSON DEC-2004                                                  
002800* ETRACKER: 1621404                                                       
002900*                                                                         
003000                                                                          
003100     SKIP3                                                                
003200 ENVIRONMENT DIVISION.                                                    
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500 WORKING-STORAGE SECTION.                                                 
003600                                                                          
003700*    -- CHECKED BY WY2000                                                 
003800 77  IDPGM                       PIC X(08)   VALUE 'W4036500'.            
003900                                                                          
004000 77  JA                          PIC X       VALUE 'J'.                   
004100 77  NEJ                         PIC X       VALUE 'N'.                   
004200                                                                          
004300 77  INDX                        PIC S9(9)   VALUE +0   COMP SYNC.        
004400 77  RAD-INDX                    PIC S9(9)   VALUE +0   COMP SYNC.        
004500 77  MAX-TABRADER                PIC S9(9)   VALUE +20  COMP SYNC.        
004510 77  MAX-INDX                    PIC S9(9)   VALUE +99  COMP SYNC.        
004600 77  SPRAK-IX                    PIC S9(9)   VALUE +0   COMP SYNC.        
004700                                                                          
004800                                                                          
004900 77  MAX-MOD-LAENGD              PIC S9(4)   VALUE +700 COMP SYNC.        
005000                                                                          
005100*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005200 77  PRCNR-WS                    PIC X(4)    VALUE SPACE.                 
005300*                                                                         
005400       EJECT                                                              
005500*                                                                         
005600 77  INDATA-SW                   PIC X       VALUE 'J'.                   
005700     88  INDATA-OK                           VALUE 'J'.                   
005800     88  INDATA-FEL                          VALUE 'N'.                   
005900                                                                          
006000 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
006100     88  NYCKLAR-OK                          VALUE 'J'.                   
006200     88  NYCKLAR-FEL                         VALUE 'N'.                   
006300                                                                          
006400 77  PRINTER-SW                  PIC X       VALUE 'J'.                   
006500     88  PRINTER-FINNS                       VALUE 'J'.                   
006600     88  PRINTER-SAKNAS                      VALUE 'N'.                   
006700                                                                          
006800 77  UPPDAT-SW                   PIC X       VALUE 'J'.                   
006900     88  UPPDATERING-OK                      VALUE 'J'.                   
007000     88  UPPDATERING-FEL                     VALUE 'N'.                   
007100                                                                          
007200 77  FOERSTA-FAELT-SW            PIC X       VALUE 'J'.                   
007300     88  FOERSTA-FAELT                       VALUE 'J'.                   
007400     88  EJ-FOERSTA-FAELT                    VALUE 'N'.                   
007500                                                                          
007600 77  PRTGEN-PU-SW                PIC X       VALUE 'N'.                   
007700     88  PRTGENPU-AENDRAD                    VALUE 'J'.                   
007800     88  PRTGENPU-EJ-AENDRAD                 VALUE 'N'.                   
007900                                                                          
008000 77  PRTGEN-PLE-SW               PIC X       VALUE 'N'.                   
008100     88  PRTGENPLE-AENDRAD                   VALUE 'J'.                   
008200     88  PRTGENPLE-EJ-AENDRAD                VALUE 'N'.                   
008300                                                                          
008400 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
008500     88  EGEN-TRANS                          VALUE '4365'.                
008600     88  GODK-TRANS                          VALUE '4364' '4365'          
008700                                                   '4368' '4369'.         
008800                                                                          
008900 01  WS-IDPRTLST.                                                         
009000     03 WS-SYSTDEL               PIC X(1).                                
009100     03 WS-LISTTYP               PIC X(2).                                
009200     03 WS-KDPRT                 PIC X(3).                                
009300     03 FILLER                   PIC X(2)    VALUE SPACE.                 
009400                                                                          
009500 01  WS-IDPRC.                                                            
009600     03  WS-IDPRCBAS             PIC X(3)    VALUE SPACE.                 
009700     03  WS-IDPRCVAR             PIC X       VALUE SPACE.                 
009800                                                                          
009900*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
010000 01  GENERELLA-SUBPROGRAM.                                                
010100     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
010200     03  W006PRT                 PIC X(8)    VALUE 'W006PRT '.            
010300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010500     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
010600     EJECT                                                                
010700*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
010800*   -COPY WMEDAREA                                                        
010900     EJECT                                                                
011000*    --- PARAMETRAR TILL SUBPROGRAM W006PRT                               
011100*   -COPY W006PRT                                                         
011200     EJECT                                                                
011300*                   ****    PARAMETRAR TILL W005INIT                      
011400*01  -COPY WMSGINIT                                                       
011500     EJECT                                                                
011600 01  FELM-CODES.                                                          
011700     03  FILLER                    PIC X(16)   VALUE 'FELM AREA'.         
011800     03  FELM-KOR-UPPLYSTA-FAELT   PIC X(3)    VALUE '001'.               
011900     03  FELM-OTILL-UPPDATERING    PIC X(3)    VALUE '007'.               
012000     03  FELM-FINNS-EJ-PA-BASEN    PIC X(3)    VALUE '010'.               
012100     03  FELM-PF11-O-TOM-INDATRAD  PIC X(3)    VALUE '011'.               
012200     03  FELM-PRC-ELLER-PRINTER    PIC X(3)    VALUE '027'.               
012300     03  FELM-PRC-FINNS-EJ         PIC X(3)    VALUE '073'.               
012400     03  FELM-FEL-NYCKEL           PIC X(3)    VALUE '401'.               
012500     03  FELM-FELAKTIG-PRINTER     PIC X(3)    VALUE '772'.               
012600     SKIP3                                                                
012700 01  MESSAGE-CODES.                                                       
012800     03  FILLER                    PIC X(16)   VALUE 'INFO AREA'.         
012900     03  INFO-TRYCK-PF11           PIC X(3)    VALUE '003'.               
013000     03  INFO-FOERSTA-SIDAN        PIC X(3)    VALUE '006'.               
013100     03  INFO-MER-INFO-FINNS-PF8   PIC X(3)    VALUE '105'.               
013200     03  INFO-UPPDAT-GJORD         PIC X(3)    VALUE '101'.               
013300     EJECT                                                                
013400*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
013500*                                                                         
013600 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
013700     SKIP3                                                                
013800*01  MID -COPY W4I36501                                                   
013900     EJECT                                                                
014000 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
014100     SKIP3                                                                
014200*01  -COPY WMSGAREA                                                       
014300     EJECT                                                                
014400*    03  MOD -COPY W4O36501   -RED MSG-AREA.                              
014500     EJECT                                                                
014600 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
014700     SKIP3                                                                
014800*01  -COPY WMFSAREA                                                       
014900     EJECT                                                                
015000*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
015100*                                                                         
015200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
015300     SKIP2                                                                
015400 01  NYCKLAR-TILL-DLI.                                                    
015500     03  W-WDGXKEY-4453-X.                                                
015600         05  W-IDHDTYP-4453      PIC X(4)    VALUE '4453'.                
015700         05  W-IDDC-4453         PIC X(2)    VALUE '00'.                  
015800         05  W-PRCNR             PIC X(4)    VALUE SPACE.                 
015900         05  FILLER              PIC X(20)   VALUE LOW-VALUE.             
016000                                                                          
016100     03  W-WDGXKEY-4454-X.                                                
016200         05  W-KDSEGKEY          PIC X(1)    VALUE SPACE.                 
016300                                                                          
016400     03  W-WDGXKEY-4447-X.                                                
016500         05  W-IDHDTYP-4447      PIC X(4)    VALUE '4447'.                
016600         05  W-IDDC-4447         PIC X(2)    VALUE '00'.                  
016700         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
016800                                                                          
016900     03  W-WDGXKEY-4448-X.                                                
017000         05  W-IDPRC             PIC X(4)    VALUE SPACE.                 
017100         05  FILLER              PIC X       VALUE LOW-VALUE.             
017200                                                                          
017300     03  W-IDDC-B6-X.                                                     
017400         05 W-IDDC-B6                  PIC X(2).                          
017500                                                                          
017600*    --- STATUS-KOD FRÅN IMS                                              
017700 01  STATUS-WS                   PIC XX.                                  
017800     88  SEGMENT-FINNS                       VALUE '  '.                  
017900     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
018000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
018100     SKIP2                                                                
018200 01  GODK-STATUSKODER.                                                    
018300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
018400     SKIP2                                                                
018500 01  SSA1                        PIC X(64).                               
018600 01  SSA2                        PIC X(64).                               
018700     EJECT                                                                
018800*    --- IMS FUNKTIONSKODER                                               
018900*01  -COPY W0003                                                          
019000     EJECT                                                                
019100*    ---  DLI INPUT-OUTPUT AREA                                           
019200 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
019300     SKIP3                                                                
019400 01  DLI-IO-AREA.                                                         
019500     03  IO-AREA                 PIC X(1300)  VALUE SPACE.                
019600     SKIP3                                                                
019700     03  WLXXKL01 REDEFINES IO-AREA.                                      
019800*        05  -COPY WDGX4453   -PRE XXKL-                                  
019900     EJECT                                                                
020000     03  WLXXKL11 REDEFINES IO-AREA.                                      
020100*        05  -COPY WDGX4454   -PRE XXKL-                                  
020200                                                                          
020300 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
020400 01   DLI-IO-AREA-B601.                                                   
020500*     03  -COPY WDB601                                                    
020600                                                                          
020700     EJECT                                                                
020800 LINKAGE SECTION.                                                         
020900                                                                          
021000*01  -COPY W0009      -PRE MSG-                                           
021100     EJECT                                                                
021200*01  -COPY W0008     -PRE USEA-                                           
021300     05  FILLER              PIC X.                                       
021400     EJECT                                                                
021500*01  -COPY W0008      -PRE XXKL-                                          
021600     05  FILLER                  PIC X.                                   
021700     EJECT                                                                
021800*01  -COPY W0008      -PRE XXKH-                                          
021900     05  FILLER                  PIC X.                                   
022000     EJECT                                                                
022100*01  -COPY W0008      -PRE WDB6-                                          
022200     05  FILLER                  PIC X.                                   
022300     EJECT                                                                
022400 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB                               
022500                                   XXKL-PCB XXKH-PCB WDB6-PCB.            
022600     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB                               
022700                                   XXKL-PCB XXKH-PCB WDB6-PCB.            
022800                                                                          
022900     PERFORM IMS-GET-MSG                                                  
023000     IF SEGMENT-FINNS                                                     
023100       PERFORM A-INIT                                                     
023200       PERFORM B-KOLLA-NYCKLAR                                            
023300       IF NYCKLAR-OK                                                      
023400         PERFORM IMS-GET-XXKL-PRCNR                                       
023500         IF SEGMENT-FINNS                                                 
023600           IF MFS-UPDATE                                                  
023700             PERFORM F-UPPDATERA                                          
023800           ELSE                                                           
023900             IF MFS-FIRST                                                 
024000               PERFORM C-FOERSTA-SIDAN                                    
024100             ELSE                                                         
024200               IF MFS-NEXT                                                
024300                 PERFORM D-NAESTA-SIDAN                                   
024400               ELSE                                                       
024500                 PERFORM E-SAMMA-SIDA                                     
024600               END-IF                                                     
024700             END-IF                                                       
024800           END-IF                                                         
024900           PERFORM G-LAES-VISA-TABELL                                     
025000         ELSE                                                             
025100           PERFORM S01-VISA-TABELL-SAKNAS                                 
025200         END-IF                                                           
025300       END-IF                                                             
025400       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
025500       PERFORM IMS-INSERT-MSG                                             
025600     END-IF                                                               
025700                                                                          
025800     MOVE ZERO TO RETURN-CODE                                             
025900     GOBACK                                                               
026000     .                                                                    
026100     EJECT                                                                
026200 A-INIT SECTION.                                                          
026300                                                                          
026400     IF MSG-DUBBLA-TRANSKODER                                             
026500       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I36501                 
026600       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
026700       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
026800     ELSE                                                                 
026900       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I36501                  
027000       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
027100       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
027200     END-IF                                                               
027300                                                                          
027400     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
027500     MOVE MSG-IDPFK TO MFS-IDPFK                                          
027600     MOVE MFS-IDTRANS TO W-IDTRANS                                        
027700                                                                          
027800     MOVE LOW-VALUE TO MSG-AREA                                           
027900     MOVE 'W4O365N1' TO MFS-IDMOD                                         
028000     MOVE '4365' TO MOD-IDTRANS                                           
028100     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL                                 
028200                             MOD-TEMFSINF                                 
028300     IF NOT EGEN-TRANS                                                    
028400       MOVE SPACE TO MFS-KDTRTYP                                          
028500       MOVE '7' TO MFS-IDPFK                                              
028600     END-IF                                                               
028700     .                                                                    
028800     EJECT                                                                
028900 B-KOLLA-NYCKLAR SECTION.                                                 
029000                                                                          
029100     MOVE ALL '+'           TO MSGI-WMSGINIT                              
029200     MOVE '001'             TO MSGI-KDCALL                                
029300     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
029400     MOVE '4365'            TO MSGI-IDTRANS                               
029500     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
029600     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
029700                                                                          
029800     IF MSGI-IDLAND-SPR = 'GB'                                            
029900       MOVE 'GB ' TO MED-IDSKYLT                                          
030000     ELSE                                                                 
030100       MOVE 'S  ' TO MED-IDSKYLT                                          
030200     END-IF                                                               
030300                                                                          
030400     IF GODK-TRANS                                                        
030500       MOVE JA TO NYCKLAR-SW                                              
030600       MOVE NEJ TO UPPDAT-SW                                              
030700                   PRTGEN-PU-SW                                           
030800                   PRTGEN-PLE-SW                                          
030900                                                                          
031000       MOVE MFS-RENSA-FAELT TO MOD-PRCNR-IN                               
031100                                                                          
031200       IF MID-PRCNR-IN = ALL '+'                                          
031300         MOVE MID-PRCNR-UT TO PRCNR-WS                                    
031400         INSPECT PRCNR-WS REPLACING LEADING SPACE BY ZERO                 
031500         MOVE PRCNR-WS TO WS-IDPRC                                        
031600       ELSE                                                               
031700         MOVE MID-PRCNR-IN TO WS-IDPRC                                    
031800         MOVE '7'         TO MFS-IDPFK                                    
031900         MOVE SPACE       TO MFS-KDTRTYP                                  
032000       END-IF                                                             
032100                                                                          
032200       IF WS-IDPRCBAS NUMERIC AND WS-IDPRCBAS > ZERO                      
032300         MOVE WS-IDPRC TO W-PRCNR                                         
032400       ELSE                                                               
032500         MOVE NEJ TO NYCKLAR-SW                                           
032600       END-IF                                                             
032700                                                                          
032800       MOVE MFS-RENSA-FAELT             TO MOD-IDDC-IN                    
032900                                                                          
033000       MOVE MSGI-IDDC               TO W-IDDC-B6                          
033100       PERFORM IMS-GU-WDB601                                              
033200                                                                          
033300       IF DCS-KDDC = SPACE OR DCS-DDC                                     
033400         MOVE NEJ                     TO NYCKLAR-SW                       
033500       END-IF                                                             
033600                                                                          
033700       IF NYCKLAR-OK                                                      
033800         MOVE DCS-IDDC     TO W-IDDC-4453                                 
033900                              W-IDDC-4447                                 
034000       END-IF                                                             
034100                                                                          
034200       IF GODK-TRANS OR NYCKLAR-OK                                        
034300         MOVE WS-IDPRC TO MOD-PRCNR-UT                                    
034400         MOVE DCS-IDDC TO MOD-IDDC-UT                                     
034500         INSPECT MOD-PRCNR-UT REPLACING LEADING ZERO BY SPACE             
034600       ELSE                                                               
034700         MOVE MFS-RENSA-FAELT TO MOD-PRCNR-UT                             
034800                                 MOD-IDDC-UT                              
034900       END-IF                                                             
035000                                                                          
035100       IF NYCKLAR-FEL                                                     
035200         MOVE FELM-FEL-NYCKEL TO MED-IDMFSFEL                             
035300         CALL WMEDKONV USING MED-WMEDAREA                                 
035400         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
035500         PERFORM MFS-RENSA-FAELT-IN                                       
035600         PERFORM MFS-RENSA-FAELT-UT                                       
035700       END-IF                                                             
035800                                                                          
035900     ELSE                                                                 
036000       MOVE NEJ TO NYCKLAR-SW                                             
036100       MOVE MFS-RENSA-FAELT TO MOD-PRCNR-IN                               
036200       PERFORM MFS-RENSA-FAELT-IN                                         
036300       PERFORM MFS-RENSA-FAELT-UT                                         
036400     END-IF                                                               
036500     .                                                                    
036600     EJECT                                                                
036700 C-FOERSTA-SIDAN SECTION.                                                 
036800                                                                          
036900     MOVE '1' TO W-KDSEGKEY                                               
037000     MOVE +1 TO INDX                                                      
037100     MOVE INFO-FOERSTA-SIDAN TO MED-IDMFSFEL                              
037200     CALL WMEDKONV USING MED-WMEDAREA                                     
037300     MOVE MED-MFSFEL TO MOD-TEMFSFEL                                      
037400     PERFORM MFS-RENSA-FAELT-IN                                           
037500     .                                                                    
037600     EJECT                                                                
037700 D-NAESTA-SIDAN SECTION.                                                  
037800                                                                          
037900     MOVE '1' TO W-KDSEGKEY                                               
038000     MOVE MID-LAGOMR-NEXT TO INDX                                         
038100     PERFORM MFS-RENSA-FAELT-IN                                           
038200     .                                                                    
038300     EJECT                                                                
038400 E-SAMMA-SIDA SECTION.                                                    
038500                                                                          
038600     MOVE '1' TO W-KDSEGKEY                                               
038700     MOVE MID-LAGOMR-ENTER TO INDX                                        
038800                                                                          
038900     IF MID-KDPRTGEN-PU  NOT = ALL '+' OR                                 
039000        MID-KDPRTGEN-PLE NOT = ALL '+' OR                                 
039100        MID-LAGOMR-UPP   NOT = ALL '+' OR                                 
039200        MID-INPUT        NOT = ALL '+'                                    
039300                                                                          
039400       MOVE INFO-TRYCK-PF11 TO MED-IDMFSFEL                               
039500       CALL WMEDKONV USING MED-WMEDAREA                                   
039600       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
039700       PERFORM MFS-ROER-EJ-FAELT-IN                                       
039800       PERFORM MFS-LAES-IN-IGEN                                           
039900                                                                          
040000       MOVE NEJ TO PRTGEN-PU-SW                                           
040100                   PRTGEN-PLE-SW                                          
040200       IF MID-KDPRTGEN-PU NOT = ALL '+'                                   
040300         MOVE JA TO PRTGEN-PU-SW                                          
040400         MOVE MFS-ROER-EJ-FAELT TO MOD-KDPRTGEN-PU                        
040500         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDPRTGEN-PU-ATTR               
040600       END-IF                                                             
040700                                                                          
040800       IF MID-KDPRTGEN-PLE NOT = ALL '+'                                  
040900         MOVE JA TO PRTGEN-PLE-SW                                         
041000         MOVE MFS-ROER-EJ-FAELT TO MOD-KDPRTGEN-PLE                       
041100         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDPRTGEN-PLE-ATTR              
041200       END-IF                                                             
041300                                                                          
041400     ELSE                                                                 
041500       PERFORM MFS-RENSA-FAELT-IN                                         
041600     END-IF                                                               
041700     .                                                                    
041800     EJECT                                                                
041900 F-UPPDATERA SECTION.                                                     
042000                                                                          
042100     PERFORM FA-KOLLA-INPUT                                               
042200                                                                          
042300     IF INDATA-OK AND UPPDATERING-OK                                      
042400       MOVE '1' TO W-KDSEGKEY                                             
042500       PERFORM IMS-GET-XXKL-TABELL                                        
042600                                                                          
042700         IF SEGMENT-FINNS                                                 
042800           PERFORM FB-AENDRA-GENERELLA-PRT                                
042900                                                                          
043000           IF MID-LAGOMR-UPP NOT = ALL '+'                                
043100           PERFORM UNTIL INDX = MID-LAGOMR-UPP OR INDX > MAX-INDX         
043200               ADD +1 TO INDX                                             
043300             END-PERFORM                                                  
043400                                                                          
043500             IF INDX = MID-LAGOMR-UPP                                     
043600               IF MID-INPUT = ALL '+'                                     
043700                 PERFORM FC-RADERA-RAD                                    
043800                                                                          
043900               ELSE                                                       
044000                 PERFORM FD-AENDRA-RAD                                    
044100               END-IF                                                     
044200             END-IF                                                       
044300           END-IF                                                         
044400                                                                          
044500           IF UPPDATERING-OK                                              
044600             PERFORM IMS-REPL-XXKL-TABELL                                 
044700                                                                          
044800             IF MID-KDPRTGEN-PU NOT = ALL '+'                             
044900               MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KDPRTGEN-PU-ATTR         
045000               MOVE NEJ TO PRTGEN-PU-SW                                   
045100             END-IF                                                       
045200             IF MID-KDPRTGEN-PLE NOT = ALL '+'                            
045300               MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KDPRTGEN-PLE-ATTR        
045400               MOVE NEJ TO PRTGEN-PLE-SW                                  
045500             END-IF                                                       
045600                                                                          
045700           END-IF                                                         
045800                                                                          
045900         ELSE                                                             
046000           PERFORM S01-VISA-TABELL-SAKNAS                                 
046100         END-IF                                                           
046200     END-IF                                                               
046300     .                                                                    
046400     EJECT                                                                
046500 FA-KOLLA-INPUT SECTION.                                                  
046600                                                                          
046700     MOVE JA  TO INDATA-SW                                                
046800                 UPPDAT-SW                                                
046900     MOVE NEJ TO PRTGEN-PU-SW                                             
047000                 PRTGEN-PLE-SW                                            
047100     MOVE MID-LAGOMR-ENTER TO INDX                                        
047200     MOVE '1' TO W-KDSEGKEY                                               
047300                                                                          
048700     IF MID-KDPRTGEN-PU    = ALL '+' AND                                  
048800        MID-KDPRTGEN-PLE   = ALL '+' AND                                  
048900        MID-LAGOMR-UPP     = ALL '+' AND                                  
049000        MID-INPUT          = ALL '+'                                      
049100                                                                          
049200       PERFORM S03-VISA-PF11-OCH-EJ-DATA                                  
049300                                                                          
049400     ELSE                                                                 
049500       PERFORM FAA-KOLLA-INPUT-GENERELLA-PRT                              
049600       IF INDATA-FEL                                                      
049700         PERFORM FAB-VISA-FEL-PRTGEN                                      
049800         MOVE NEJ TO UPPDAT-SW                                            
049900                                                                          
050000       ELSE                                                               
050100         PERFORM FAC-KOLLA-INPUT-UPPDAT-RAD                               
050200         IF INDATA-FEL                                                    
050300           PERFORM FAD-VISA-INPUTFEL                                      
050400           MOVE NEJ TO UPPDAT-SW                                          
050500         ELSE                                                             
050600           MOVE JA TO UPPDAT-SW                                           
050700         END-IF                                                           
050800       END-IF                                                             
050900     END-IF                                                               
051100     .                                                                    
051200     EJECT                                                                
051300 FAA-KOLLA-INPUT-GENERELLA-PRT SECTION.                                   
051400                                                                          
051500     IF MID-KDPRTGEN-PU NOT = ALL '+'                                     
051600       MOVE JA TO PRTGEN-PU-SW                                            
051700       MOVE MID-KDPRTGEN-PU TO WS-KDPRT                                   
051800       MOVE 'PU'            TO WS-LISTTYP                                 
051900       PERFORM S04-KOLLA-PRINTER-FINNS                                    
052000       IF PRINTER-SAKNAS                                                  
052100         MOVE MFS-ALFA-FAELT-FEL TO                                       
052200                             MOD-KDPRTGEN-PU-ATTR                         
052300         MOVE NEJ TO INDATA-SW                                            
052400       ELSE                                                               
052500         MOVE MFS-ALFA-FAELT-RAETT TO                                     
052600                             MOD-KDPRTGEN-PU-ATTR                         
052700       END-IF                                                             
052800       MOVE MFS-ROER-EJ-FAELT TO MOD-KDPRTGEN-PU                          
052900     END-IF                                                               
053000                                                                          
053100     IF MID-KDPRTGEN-PLE NOT = ALL '+'                                    
053200       MOVE JA TO PRTGEN-PLE-SW                                           
053300       MOVE MID-KDPRTGEN-PLE TO WS-KDPRT                                  
053400       MOVE 'PE'            TO WS-LISTTYP                                 
053500       PERFORM S04-KOLLA-PRINTER-FINNS                                    
053600       IF PRINTER-SAKNAS                                                  
053700         MOVE MFS-ALFA-FAELT-FEL TO                                       
053800                             MOD-KDPRTGEN-PLE-ATTR                        
053900         MOVE NEJ TO INDATA-SW                                            
054000       ELSE                                                               
054100         MOVE MFS-ALFA-FAELT-RAETT TO                                     
054200                             MOD-KDPRTGEN-PLE-ATTR                        
054300       END-IF                                                             
054400       MOVE MFS-ROER-EJ-FAELT TO MOD-KDPRTGEN-PLE                         
054500     END-IF                                                               
054600     .                                                                    
054700     EJECT                                                                
054800 FAB-VISA-FEL-PRTGEN SECTION.                                             
054900                                                                          
055000     MOVE FELM-FELAKTIG-PRINTER TO MED-IDMFSFEL                           
055100     CALL WMEDKONV USING MED-WMEDAREA                                     
055200     MOVE MED-MFSFEL TO MOD-TEMFSFEL                                      
055300     MOVE MID-LAGOMR-ENTER TO INDX                                        
055400                                                                          
055500     IF MID-LAGOMR-UPP NOT = ALL '+' OR                                   
055600        MID-INPUT      NOT = ALL '+'                                      
055700       PERFORM MFS-ROER-EJ-FAELT-IN                                       
055800       PERFORM MFS-LAES-IN-IGEN                                           
055900     ELSE                                                                 
056000       PERFORM MFS-RENSA-FAELT-IN                                         
056100     END-IF                                                               
056200     .                                                                    
056300     EJECT                                                                
056400 FAC-KOLLA-INPUT-UPPDAT-RAD SECTION.                                      
056500                                                                          
056600     IF MID-LAGOMR-UPP NOT = ALL '+' OR                                   
056700        MID-INPUT NOT = ALL '+'                                           
056800       IF MID-LAGOMR-UPP NOT = ALL '+'                                    
056900         IF MID-LAGOMR-UPP NOT NUMERIC                                    
057000           MOVE MFS-NUM-FAELT-FEL TO                                      
057100                              MOD-LAGOMR-UPP-ATTR                         
057200           MOVE NEJ TO INDATA-SW                                          
057300         ELSE                                                             
057400           IF MID-LAGOMR-UPP < +1                                         
057500             MOVE MFS-NUM-FAELT-FEL TO                                    
057600                                MOD-LAGOMR-UPP-ATTR                       
057700             MOVE NEJ TO INDATA-SW                                        
057800           ELSE                                                           
057900             MOVE MFS-NUM-FAELT-RAETT TO                                  
058000                                MOD-LAGOMR-UPP-ATTR                       
058100             MOVE MID-LAGOMR-UPP TO INDX                                  
058200           END-IF                                                         
058300         END-IF                                                           
058400       ELSE                                                               
058500         MOVE MFS-NUM-FAELT-FEL TO                                        
058600                            MOD-LAGOMR-UPP-ATTR                           
058700         MOVE NEJ TO INDATA-SW                                            
058800       END-IF                                                             
058900                                                                          
059000       IF MID-IDPRC-UPP NOT = ALL '+'                                     
059100         MOVE MID-IDPRC-UPP TO WS-IDPRC                                   
059200         IF WS-IDPRCBAS NOT NUMERIC                                       
059300           MOVE MFS-ALFA-FAELT-FEL TO                                     
059400                               MOD-IDPRC-UPP-ATTR                         
059500           MOVE NEJ TO INDATA-SW                                          
059600         ELSE                                                             
059700           IF MID-IDPRC-UPP = XXKL-4453-IDPRC                             
059800             MOVE MFS-ALFA-FAELT-FEL TO                                   
059900                                 MOD-IDPRC-UPP-ATTR                       
060000             MOVE NEJ TO INDATA-SW                                        
060100           ELSE                                                           
060200             MOVE MID-IDPRC-UPP TO W-IDPRC                                
060300             PERFORM IMS-GET-XXKH-PRC-UNIK                                
060400             IF SEGMENT-SAKNAS                                            
060500               MOVE MFS-ALFA-FAELT-FEL TO                                 
060600                                   MOD-IDPRC-UPP-ATTR                     
060700               MOVE NEJ TO INDATA-SW                                      
060800             ELSE                                                         
060900               MOVE MFS-ALFA-FAELT-RAETT TO                               
061000                                   MOD-IDPRC-UPP-ATTR                     
061100             END-IF                                                       
061200           END-IF                                                         
061300         END-IF                                                           
061400       END-IF                                                             
061500                                                                          
061600       IF MID-KDPRT-PU-UPP NOT = ALL '+'                                  
061700         IF MID-KDPRT-PU-UPP = ZERO                                       
061800           MOVE MFS-ALFA-FAELT-RAETT TO                                   
061900                               MOD-KDPRT-PU-UPP-ATTR                      
062000         ELSE                                                             
062100           MOVE MID-KDPRT-PU-UPP TO WS-KDPRT                              
062200           MOVE 'PU'             TO WS-LISTTYP                            
062300           PERFORM S04-KOLLA-PRINTER-FINNS                                
062400           IF PRINTER-SAKNAS                                              
062500             MOVE MFS-ALFA-FAELT-FEL TO                                   
062600                                 MOD-KDPRT-PU-UPP-ATTR                    
062700             MOVE NEJ TO INDATA-SW                                        
062800           ELSE                                                           
062900             MOVE MFS-ALFA-FAELT-RAETT TO                                 
063000                                 MOD-KDPRT-PU-UPP-ATTR                    
063100           END-IF                                                         
063200         END-IF                                                           
063300       END-IF                                                             
063400                                                                          
063500       IF MID-KDSS-PU-UPP NOT = ALL '+'                                   
063600         MOVE MFS-ALFA-FAELT-RAETT TO                                     
063700                             MOD-KDSS-PU-UPP-ATTR                         
063800       END-IF                                                             
063900                                                                          
064000       IF MID-KDPRT-PLE-UPP NOT = ALL '+'                                 
064100         IF MID-KDPRT-PLE-UPP = ZERO                                      
064200           MOVE MFS-ALFA-FAELT-RAETT TO                                   
064300                               MOD-KDPRT-PLE-UPP-ATTR                     
064400         ELSE                                                             
064500           MOVE MID-KDPRT-PLE-UPP TO WS-KDPRT                             
064600           MOVE 'PE'              TO WS-LISTTYP                           
064700           PERFORM S04-KOLLA-PRINTER-FINNS                                
064800           IF PRINTER-SAKNAS                                              
064900             MOVE MFS-ALFA-FAELT-FEL TO                                   
065000                                 MOD-KDPRT-PLE-UPP-ATTR                   
065100             MOVE NEJ                TO INDATA-SW                         
065200           ELSE                                                           
065300             MOVE MFS-ALFA-FAELT-RAETT TO                                 
065400                                 MOD-KDPRT-PLE-UPP-ATTR                   
065500           END-IF                                                         
065600         END-IF                                                           
065700       END-IF                                                             
065800                                                                          
065900       IF MID-KDSS-PLE-UPP NOT = ALL '+'                                  
066000         MOVE MFS-ALFA-FAELT-RAETT TO                                     
066100                             MOD-KDSS-PLE-UPP-ATTR                        
066200       END-IF                                                             
066300     END-IF                                                               
066400     .                                                                    
066500     EJECT                                                                
066600 FAD-VISA-INPUTFEL SECTION.                                               
066700                                                                          
066800     IF MID-LAGOMR-UPP = +0 OR MID-IDPRC-UPP = XXKL-4453-IDPRC            
066900       MOVE FELM-OTILL-UPPDATERING TO MED-IDMFSFEL                        
067000     ELSE                                                                 
067100       IF MID-LAGOMR-UPP NOT = ALL '+'                                    
067200         IF MID-LAGOMR-UPP NOT NUMERIC                                    
067300           MOVE FELM-KOR-UPPLYSTA-FAELT TO MED-IDMFSFEL                   
067400         ELSE                                                             
067500           IF MID-IDPRC-UPP NOT = ALL '+' AND WS-IDPRCBAS                 
067600              NOT NUMERIC                                                 
067700             MOVE FELM-KOR-UPPLYSTA-FAELT TO MED-IDMFSFEL                 
067800           ELSE                                                           
067900             IF SEGMENT-SAKNAS                                            
068000               MOVE FELM-FINNS-EJ-PA-BASEN TO MED-IDMFSFEL                
068100             ELSE                                                         
068200               IF PRINTER-SAKNAS                                          
068300                 MOVE FELM-FELAKTIG-PRINTER TO MED-IDMFSFEL               
068400               END-IF                                                     
068500             END-IF                                                       
068600           END-IF                                                         
068700         END-IF                                                           
068800       ELSE                                                               
068900         MOVE FELM-KOR-UPPLYSTA-FAELT TO MED-IDMFSFEL                     
069000       END-IF                                                             
069100     END-IF                                                               
069200     CALL WMEDKONV USING MED-WMEDAREA                                     
069300     MOVE MED-MFSFEL TO MOD-TEMFSFEL                                      
069400     MOVE MID-LAGOMR-ENTER TO INDX                                        
069500     PERFORM MFS-ROER-EJ-FAELT-IN                                         
069600                                                                          
069700     IF MID-KDPRTGEN-PU NOT = ALL '+'                                     
069800       MOVE JA TO PRTGEN-PU-SW                                            
069900       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDPRTGEN-PU-ATTR                 
070000       MOVE MFS-ROER-EJ-FAELT TO MOD-KDPRTGEN-PU                          
070100     END-IF                                                               
070200                                                                          
070300     IF MID-KDPRTGEN-PLE NOT = ALL '+'                                    
070400       MOVE JA TO PRTGEN-PLE-SW                                           
070500       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDPRTGEN-PLE-ATTR                
070600       MOVE MFS-ROER-EJ-FAELT TO MOD-KDPRTGEN-PLE                         
070700     END-IF                                                               
070800     .                                                                    
070900     EJECT                                                                
071000 FB-AENDRA-GENERELLA-PRT SECTION.                                         
071100                                                                          
071200     MOVE NEJ TO UPPDAT-SW                                                
071300                                                                          
071400     IF MID-KDPRTGEN-PU NOT = ALL '+'                                     
071500       MOVE MID-KDPRTGEN-PU      TO XXKL-4454-KDPRTGEN-PU                 
071600       MOVE JA TO UPPDAT-SW                                               
071700     END-IF                                                               
071800                                                                          
071900     IF MID-KDPRTGEN-PLE NOT = ALL '+'                                    
072000       MOVE MID-KDPRTGEN-PLE     TO XXKL-4454-KDPRTGEN-PLE                
072100       MOVE JA TO UPPDAT-SW                                               
072200     END-IF                                                               
072300                                                                          
072400     PERFORM FBA-LAES-AENDRA-UNDANTAG-PRT                                 
072500     .                                                                    
072600     EJECT                                                                
072700 FBA-LAES-AENDRA-UNDANTAG-PRT SECTION.                                    
072800                                                                          
072900     IF MID-KDPRTGEN-PU  NOT = ALL '+' OR                                 
073000        MID-KDPRTGEN-PLE NOT = ALL '+'                                    
073100       MOVE +1 TO INDX                                                    
073200       PERFORM UNTIL INDX > MAX-INDX                                      
073300         IF XXKL-4454-KDPRT-PU(INDX) = MID-KDPRTGEN-PU                    
073400           MOVE SPACE TO XXKL-4454-KDPRT-PU(INDX)                         
073500           MOVE JA TO UPPDAT-SW                                           
073600         END-IF                                                           
073700                                                                          
073800         IF XXKL-4454-KDPRT-PLE(INDX) = MID-KDPRTGEN-PLE                  
073900           MOVE SPACE TO XXKL-4454-KDPRT-PLE(INDX)                        
074000           MOVE JA TO UPPDAT-SW                                           
074100         END-IF                                                           
074200                                                                          
074300         ADD +1 TO INDX                                                   
074400       END-PERFORM                                                        
074500                                                                          
074600       IF MID-LAGOMR-UPP = ALL '+'                                        
074700         MOVE MID-LAGOMR-ENTER TO INDX                                    
074800       ELSE                                                               
074900         MOVE MID-LAGOMR-UPP TO INDX                                      
075000       END-IF                                                             
075100                                                                          
075200     END-IF                                                               
075300     .                                                                    
075400     EJECT                                                                
075500 FC-RADERA-RAD SECTION.                                                   
075600                                                                          
075700     MOVE NEJ TO UPPDAT-SW                                                
075800     IF XXKL-4454-KDPRT-PU(INDX) NOT = SPACE                              
075900       MOVE SPACE TO XXKL-4454-KDPRT-PU(INDX)                             
076000       MOVE JA TO UPPDAT-SW                                               
076100     END-IF                                                               
076200                                                                          
076300     IF XXKL-4454-KDPRT-PLE(INDX) NOT = SPACE                             
076400       MOVE SPACE TO XXKL-4454-KDPRT-PLE(INDX)                            
076500       MOVE JA TO UPPDAT-SW                                               
076600     END-IF                                                               
076700                                                                          
076800     IF XXKL-4454-KDSS-PU(INDX) NOT = SPACE                               
076900       MOVE SPACE TO XXKL-4454-KDSS-PU(INDX)                              
077000       MOVE JA TO UPPDAT-SW                                               
077100     END-IF                                                               
077200                                                                          
077300     IF XXKL-4454-KDSS-PLE(INDX) NOT = SPACE                              
077400       MOVE SPACE TO XXKL-4454-KDSS-PLE(INDX)                             
077500       MOVE JA TO UPPDAT-SW                                               
077600     END-IF                                                               
077700                                                                          
077800     IF XXKL-4454-IDPRC(INDX) NOT = SPACE                                 
077900       MOVE SPACE TO XXKL-4454-IDPRC(INDX)                                
078000       MOVE JA TO UPPDAT-SW                                               
078100     END-IF                                                               
078200                                                                          
078300     IF UPPDATERING-FEL                                                   
078400       MOVE MID-LAGOMR-ENTER TO INDX                                      
078500       PERFORM S03-VISA-PF11-OCH-EJ-DATA                                  
078600       MOVE MFS-NUM-FAELT-FEL TO MOD-LAGOMR-UPP-ATTR                      
078700       MOVE MFS-ROER-EJ-FAELT TO MOD-LAGOMR-UPP                           
078800     END-IF                                                               
078900     .                                                                    
079000     EJECT                                                                
079100 FD-AENDRA-RAD SECTION.                                                   
079200                                                                          
079300     PERFORM FDA-KOLLA-AENDRAD-RAD                                        
079400                                                                          
079500     IF INDATA-FEL                                                        
079600       MOVE FELM-KOR-UPPLYSTA-FAELT TO MED-IDMFSFEL                       
079700       CALL WMEDKONV USING MED-WMEDAREA                                   
079800       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
079900       PERFORM MFS-ROER-EJ-FAELT-IN                                       
080000       MOVE MID-LAGOMR-ENTER TO INDX                                      
080100       MOVE NEJ TO UPPDAT-SW                                              
080200     END-IF                                                               
080300                                                                          
080400     IF INDATA-OK AND UPPDATERING-OK                                      
080500       PERFORM FDB-AENDRA-I-DB                                            
080600       MOVE JA TO UPPDAT-SW                                               
080700     END-IF                                                               
080800     .                                                                    
080900     EJECT                                                                
081000 FDA-KOLLA-AENDRAD-RAD SECTION.                                           
081100                                                                          
081200     MOVE JA TO INDATA-SW                                                 
081300                UPPDAT-SW                                                 
081400                                                                          
081500     IF MID-IDPRC-UPP NOT = ALL '+' OR                                    
081600        XXKL-4454-IDPRC(INDX) NOT = SPACE                                 
081700                                                                          
081800       IF MID-KDPRT-PU-UPP   = ALL '+' AND                                
081900          MID-KDSS-PU-UPP    = ALL '+' AND                                
082000          MID-KDPRT-PLE-UPP  = ALL '+' AND                                
082100          MID-KDSS-PLE-UPP   = ALL '+'                                    
082200                                                                          
082300         MOVE MID-LAGOMR-UPP TO INDX                                      
082400         IF XXKL-4454-KDPRT-PU(INDX)  = SPACE AND                         
082500            XXKL-4454-KDSS-PU(INDX)   = SPACE AND                         
082600            XXKL-4454-KDPRT-PLE(INDX) = SPACE AND                         
082700            XXKL-4454-KDSS-PLE(INDX)  = SPACE                             
082800           MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDPRC-UPP-ATTR                
082900         ELSE                                                             
083000           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDPRC-UPP-ATTR                  
083100           PERFORM S02-VISA-KONFLIKT-FEL                                  
083200           PERFORM MFS-ROER-EJ-FAELT-IN                                   
083300           MOVE MID-LAGOMR-ENTER TO INDX                                  
083400           MOVE NEJ TO UPPDAT-SW                                          
083500         END-IF                                                           
083600                                                                          
083700       ELSE                                                               
083800         IF MID-IDPRC-UPP NOT = ALL '+'                                   
083900           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDPRC-UPP-ATTR                  
084000         END-IF                                                           
084100         PERFORM S02-VISA-KONFLIKT-FEL                                    
084200         PERFORM MFS-ROER-EJ-FAELT-IN                                     
084300         MOVE MID-LAGOMR-ENTER TO INDX                                    
084400         MOVE NEJ TO UPPDAT-SW                                            
084500                                                                          
084600         IF MID-KDPRT-PU-UPP NOT = ALL '+'                                
084700           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDPRT-PU-UPP-ATTR               
084800         END-IF                                                           
084900                                                                          
085000         IF MID-KDSS-PU-UPP NOT = ALL '+'                                 
085100           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDSS-PU-UPP-ATTR                
085200         END-IF                                                           
085300                                                                          
085400         IF MID-KDPRT-PLE-UPP NOT = ALL '+'                               
085500           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDPRT-PLE-UPP-ATTR              
085600         END-IF                                                           
085700                                                                          
085800         IF MID-KDSS-PLE-UPP NOT = ALL '+'                                
085900           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDSS-PLE-UPP-ATTR               
086000         END-IF                                                           
086100                                                                          
086200       END-IF                                                             
086300     ELSE                                                                 
086400       IF MID-KDPRT-PU-UPP NOT = ALL '+'                                  
086500         MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDPRT-PU-UPP-ATTR               
086600       END-IF                                                             
086700                                                                          
086800       IF MID-KDSS-PU-UPP NOT = ALL '+'                                   
086900         MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDSS-PU-UPP-ATTR                
087000       END-IF                                                             
087100                                                                          
087200       IF MID-KDPRT-PLE-UPP NOT = ALL '+'                                 
087300         MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDPRT-PLE-UPP-ATTR              
087400       END-IF                                                             
087500                                                                          
087600       IF MID-KDSS-PLE-UPP NOT = ALL '+'                                  
087700         MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDSS-PLE-UPP-ATTR               
087800       END-IF                                                             
087900     END-IF                                                               
088000     .                                                                    
088100     EJECT                                                                
088200 FDB-AENDRA-I-DB SECTION.                                                 
088300                                                                          
088400     MOVE MID-LAGOMR-UPP TO INDX                                          
088500                                                                          
088600     IF MID-IDPRC-UPP NOT = ALL '+'                                       
088700       MOVE MID-IDPRC-UPP         TO XXKL-4454-IDPRC(INDX)                
088800       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDPRC-ATTR(1)                    
088900     END-IF                                                               
089000                                                                          
089100     IF MID-KDPRT-PU-UPP NOT = ALL '+'                                    
089200       IF MID-KDPRT-PU-UPP = XXKL-4454-KDPRTGEN-PU OR                     
089300          MID-KDPRT-PU-UPP = ZERO                                         
089400         MOVE SPACE TO XXKL-4454-KDPRT-PU(INDX)                           
089500       ELSE                                                               
089600         MOVE MID-KDPRT-PU-UPP    TO XXKL-4454-KDPRT-PU(INDX)             
089700       END-IF                                                             
089800       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KDPRT-PU-ATTR(1)                 
089900     END-IF                                                               
090000                                                                          
090100     IF MID-KDSS-PU-UPP NOT = ALL '+'                                     
090200       IF MID-KDSS-PU-UPP = '0'                                           
090300         MOVE SPACE TO XXKL-4454-KDSS-PU(INDX)                            
090400       ELSE                                                               
090500         MOVE MID-KDSS-PU-UPP     TO XXKL-4454-KDSS-PU(INDX)              
090600       END-IF                                                             
090700       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KDSS-PU-ATTR(1)                  
090800     END-IF                                                               
090900                                                                          
091000     IF MID-KDPRT-PLE-UPP NOT = ALL '+'                                   
091100       IF MID-KDPRT-PLE-UPP = XXKL-4454-KDPRTGEN-PLE OR                   
091200          MID-KDPRT-PLE-UPP = ZERO                                        
091300         MOVE SPACE TO XXKL-4454-KDPRT-PLE(INDX)                          
091400       ELSE                                                               
091500         MOVE MID-KDPRT-PLE-UPP   TO XXKL-4454-KDPRT-PLE(INDX)            
091600       END-IF                                                             
091700       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KDPRT-PLE-ATTR(1)                
091800     END-IF                                                               
091900                                                                          
092000     IF MID-KDSS-PLE-UPP NOT = ALL '+'                                    
092100       IF MID-KDSS-PLE-UPP = '0'                                          
092200         MOVE SPACE TO XXKL-4454-KDSS-PLE(INDX)                           
092300       ELSE                                                               
092400         MOVE MID-KDSS-PLE-UPP    TO XXKL-4454-KDSS-PLE(INDX)             
092500       END-IF                                                             
092600       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KDSS-PLE-ATTR(1)                 
092700     END-IF                                                               
092800                                                                          
092900     IF XXKL-4454-IDPRC(INDX)      = SPACE AND                            
093000        XXKL-4454-KDPRT-PU(INDX)   = SPACE AND                            
093100        XXKL-4454-KDSS-PU(INDX)    = SPACE AND                            
093200        XXKL-4454-KDPRT-PLE(INDX)  = SPACE AND                            
093300        XXKL-4454-KDSS-PLE(INDX)   = SPACE                                
093400       MOVE MFS-FORMATETS-ATTR TO MOD-IDPRC-ATTR(1)                       
093500                                  MOD-KDPRT-PU-ATTR(1)                    
093600                                  MOD-KDSS-PU-ATTR(1)                     
093700                                  MOD-KDPRT-PLE-ATTR(1)                   
093800                                  MOD-KDSS-PLE-ATTR(1)                    
093900     END-IF                                                               
094000     .                                                                    
094100     EJECT                                                                
094200 G-LAES-VISA-TABELL SECTION.                                              
094300                                                                          
094400     MOVE +1 TO RAD-INDX                                                  
094500                MOD-LAGOMR-ENTER                                          
094600     MOVE JA TO FOERSTA-FAELT-SW                                          
094700     PERFORM IMS-GET-XXKL-TABELL                                          
094800                                                                          
094900     IF SEGMENT-FINNS                                                     
095000                                                                          
095100       PERFORM GA-VISA-GENERELLA-PRINTRAR                                 
095200                                                                          
095300       PERFORM GB-VISA-TABELL-RADER                                       
095400                                                                          
095500       PERFORM GC-KOLLA-OM-FLER-SIDOR                                     
095600                                                                          
095700       IF UPPDATERING-OK                                                  
095800         MOVE INFO-UPPDAT-GJORD TO MED-IDMFSINF                           
095900         CALL WMEDKONV USING MED-WMEDAREA                                 
096000         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
096100         PERFORM MFS-FORM-ATTR                                            
096200         PERFORM MFS-RENSA-FAELT-IN                                       
096300         PERFORM MFS-RENSA-GEN-PRT                                        
096400       END-IF                                                             
096500                                                                          
096600     ELSE                                                                 
096700       PERFORM S01-VISA-TABELL-SAKNAS                                     
096800     END-IF                                                               
096900     .                                                                    
097000     EJECT                                                                
097100 GA-VISA-GENERELLA-PRINTRAR SECTION.                                      
097200                                                                          
097300     IF PRTGENPU-EJ-AENDRAD                                               
097400       MOVE XXKL-4454-KDPRTGEN-PU TO MOD-KDPRTGEN-PU                      
097500     END-IF                                                               
097600                                                                          
097700     IF PRTGENPLE-EJ-AENDRAD                                              
097800       MOVE XXKL-4454-KDPRTGEN-PLE TO MOD-KDPRTGEN-PLE                    
097900     END-IF                                                               
098000     .                                                                    
098100     EJECT                                                                
098200 GB-VISA-TABELL-RADER SECTION.                                            
098300                                                                          
098400     MOVE  +1 TO INDX                                                     
098500     PERFORM UNTIL RAD-INDX > MAX-TABRADER                                
098510                OR INDX > MAX-INDX                                        
098600       IF XXKL-4454-KDPRT-PU(INDX)     NOT = SPACE OR                     
098700          XXKL-4454-KDPRT-PLE(INDX)    NOT = SPACE OR                     
098800          XXKL-4454-KDSS-PU(INDX)      NOT = SPACE OR                     
098900          XXKL-4454-KDSS-PLE(INDX)     NOT = SPACE OR                     
099000          XXKL-4454-IDPRC(INDX)        NOT = SPACE                        
099100         PERFORM GBA-FLYTTA-FAELT-TILL-MOD                                
099200         ADD +1 TO RAD-INDX                                               
099300         IF FOERSTA-FAELT                                                 
099400           MOVE INDX TO MOD-LAGOMR-ENTER                                  
099500           MOVE NEJ    TO FOERSTA-FAELT-SW                                
099600         END-IF                                                           
099700       END-IF                                                             
099800                                                                          
099900       IF INDX > MAX-INDX AND RAD-INDX NOT > MAX-TABRADER                 
100000         PERFORM UNTIL RAD-INDX > MAX-TABRADER                            
100100           PERFORM MFS-RENSA-TABELLRAD                                    
100200           ADD +1 TO RAD-INDX                                             
100300         END-PERFORM                                                      
100400       END-IF                                                             
100500       ADD +1 TO INDX                                                     
100600     END-PERFORM                                                          
100700     .                                                                    
100800     EJECT                                                                
100900 GBA-FLYTTA-FAELT-TILL-MOD SECTION.                                       
101000                                                                          
101100     MOVE INDX                        TO MOD-LAGOMR(RAD-INDX)             
101200                                                                          
101300     MOVE XXKL-4454-IDPRC(INDX)       TO MOD-IDPRC(RAD-INDX)              
101400                                                                          
101500     MOVE XXKL-4454-KDPRT-PU(INDX)    TO MOD-KDPRT-PU(RAD-INDX)           
101600                                                                          
101700     MOVE XXKL-4454-KDSS-PU(INDX)     TO MOD-KDSS-PU(RAD-INDX)            
101800                                                                          
101900     MOVE XXKL-4454-KDPRT-PLE(INDX) TO MOD-KDPRT-PLE(RAD-INDX)            
102000                                                                          
102100     MOVE XXKL-4454-KDSS-PLE(INDX)    TO MOD-KDSS-PLE(RAD-INDX)           
102200     .                                                                    
102300     EJECT                                                                
102400 GC-KOLLA-OM-FLER-SIDOR SECTION.                                          
102500                                                                          
102600     IF INDX < 100                                                        
102700       PERFORM UNTIL INDX > MAX-INDX OR                                   
102800                     XXKL-4454-KDPRT-PU(INDX)     NOT = SPACE OR          
102900                     XXKL-4454-KDPRT-PLE(INDX)    NOT = SPACE OR          
103000                     XXKL-4454-KDSS-PU(INDX)      NOT = SPACE OR          
103100                     XXKL-4454-KDSS-PLE(INDX)     NOT = SPACE OR          
103200                     XXKL-4454-IDPRC(INDX)        NOT = SPACE             
103300         ADD +1 TO INDX                                                   
103400       END-PERFORM                                                        
103500     END-IF                                                               
103600                                                                          
103700     IF INDX < 100                                                        
103800       MOVE INDX    TO MOD-LAGOMR-NEXT                                    
103900       MOVE INFO-MER-INFO-FINNS-PF8 TO MED-IDMFSINF                       
104000       CALL WMEDKONV USING MED-WMEDAREA                                   
104100       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
104200     END-IF                                                               
104300                                                                          
104400     IF INDX > MAX-INDX                                                   
104500       MOVE MOD-LAGOMR-ENTER TO MOD-LAGOMR-NEXT                           
104600     END-IF                                                               
104700     .                                                                    
104800     EJECT                                                                
104900 S01-VISA-TABELL-SAKNAS SECTION.                                          
105000                                                                          
105100     MOVE FELM-PRC-FINNS-EJ TO MED-IDMFSFEL                               
105200     CALL WMEDKONV USING MED-WMEDAREA                                     
105300     MOVE MED-MFSFEL TO MOD-TEMFSFEL                                      
105400     .                                                                    
105500     SKIP3                                                                
105600 S02-VISA-KONFLIKT-FEL SECTION.                                           
105700                                                                          
105800     MOVE FELM-PRC-ELLER-PRINTER TO MED-IDMFSFEL                          
105900     CALL WMEDKONV USING MED-WMEDAREA                                     
106000     MOVE MED-MFSFEL TO MOD-TEMFSFEL                                      
106100     .                                                                    
106200     EJECT                                                                
106300 S03-VISA-PF11-OCH-EJ-DATA SECTION.                                       
106400                                                                          
106500     MOVE FELM-PF11-O-TOM-INDATRAD TO MED-IDMFSFEL                        
106600     CALL WMEDKONV USING MED-WMEDAREA                                     
106700     MOVE MED-MFSFEL TO MOD-TEMFSFEL                                      
106800     .                                                                    
106900     SKIP3                                                                
107000 S04-KOLLA-PRINTER-FINNS SECTION.                                         
107100                                                                          
107200     MOVE JA TO PRINTER-SW                                                
107300                                                                          
107400     IF WS-KDPRT = 999                                                    
107500       CONTINUE                                                           
107600     ELSE                                                                 
107700                                                                          
107800       MOVE '4'             TO WS-SYSTDEL                                 
107900                                                                          
108000       MOVE 1               TO PRT-KDCALL                                 
108100       MOVE WS-IDPRTLST     TO PRT-IDPRTLST                               
108200       CALL W006PRT      USING PRT-W006PRT                                
108300                                                                          
108400       IF PRT-IDLTERM = 'SAKNAS  '                                        
108500         MOVE NEJ TO PRINTER-SW                                           
108600       END-IF                                                             
108700                                                                          
108800     END-IF                                                               
108900     .                                                                    
109000     EJECT                                                                
109100 MFS-RENSA-FAELT-UT SECTION.                                              
109200                                                                          
109300*    --- ALLA UTDATA-FÄLT                                                 
109400     MOVE MFS-RENSA-FAELT TO MOD-LAGOMR-ENTER                             
109500                             MOD-LAGOMR-NEXT                              
109600                             MOD-KDPRTGEN-PU                              
109700                             MOD-KDPRTGEN-PLE                             
109800     MOVE +1              TO RAD-INDX                                     
109900     PERFORM UNTIL RAD-INDX > MAX-TABRADER                                
110000       PERFORM MFS-RENSA-TABELLRAD                                        
110100       ADD +1 TO RAD-INDX                                                 
110200     END-PERFORM                                                          
110300     .                                                                    
110400     SKIP3                                                                
110500 MFS-RENSA-TABELLRAD SECTION.                                             
110600                                                                          
110700     MOVE MFS-RENSA-FAELT TO MOD-LAGOMR(RAD-INDX)                         
110800                             MOD-IDPRC(RAD-INDX)                          
110900                             MOD-KDPRT-PU(RAD-INDX)                       
111000                             MOD-KDSS-PU(RAD-INDX)                        
111100                             MOD-KDPRT-PLE(RAD-INDX)                      
111200                             MOD-KDSS-PLE(RAD-INDX)                       
111300     .                                                                    
111400     EJECT                                                                
111500 MFS-RENSA-FAELT-IN SECTION.                                              
111600                                                                          
111700*    --- ALLA INDATA-FÄLT                                                 
111800     MOVE MFS-RENSA-FAELT TO MOD-LAGOMR-UPP                               
111900                             MOD-IDPRC-UPP                                
112000                             MOD-KDPRT-PU-UPP                             
112100                             MOD-KDSS-PU-UPP                              
112200                             MOD-KDPRT-PLE-UPP                            
112300                             MOD-KDSS-PLE-UPP                             
112400     .                                                                    
112500     SKIP3                                                                
112600 MFS-RENSA-GEN-PRT SECTION.                                               
112700                                                                          
112800     IF PRTGENPU-AENDRAD                                                  
112900       MOVE MFS-RENSA-FAELT TO MOD-KDPRTGEN-PU                            
113000     END-IF                                                               
113100                                                                          
113200     IF PRTGENPLE-AENDRAD                                                 
113300       MOVE MFS-RENSA-FAELT TO MOD-KDPRTGEN-PLE                           
113400     END-IF                                                               
113500     .                                                                    
113600     EJECT                                                                
113700 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
113800                                                                          
113900*    --- ALLA INDATA-FÄLT                                                 
114000     MOVE MFS-ROER-EJ-FAELT TO MOD-LAGOMR-UPP                             
114100                               MOD-IDPRC-UPP                              
114200                               MOD-KDPRT-PU-UPP                           
114300                               MOD-KDSS-PU-UPP                            
114400                               MOD-KDPRT-PLE-UPP                          
114500                               MOD-KDSS-PLE-UPP                           
114600                                                                          
114700     .                                                                    
114800     SKIP3                                                                
114900 MFS-FORM-ATTR SECTION.                                                   
115000                                                                          
115100*    --- ALLA INDATA-FÄLT                                                 
115200     MOVE MFS-FORMATETS-ATTR TO MOD-LAGOMR-UPP-ATTR                       
115300                                MOD-IDPRC-UPP-ATTR                        
115400                                MOD-KDPRT-PU-UPP-ATTR                     
115500                                MOD-KDSS-PU-UPP-ATTR                      
115600                                MOD-KDPRT-PLE-UPP-ATTR                    
115700                                MOD-KDSS-PLE-UPP-ATTR                     
115800                                                                          
115900     .                                                                    
116000     EJECT                                                                
116100 MFS-LAES-IN-IGEN SECTION.                                                
116200                                                                          
116300*    --- ALLA INDATA-FÄLT                                                 
116400     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-LAGOMR-UPP-ATTR                    
116500                                   MOD-IDPRC-UPP-ATTR                     
116600                                   MOD-KDPRT-PU-UPP-ATTR                  
116700                                   MOD-KDSS-PU-UPP-ATTR                   
116800                                   MOD-KDPRT-PLE-UPP-ATTR                 
116900                                   MOD-KDSS-PLE-UPP-ATTR                  
117000                                                                          
117100     .                                                                    
117200     EJECT                                                                
117300* --- IMS SEKTIONER ---                                                   
117400     SKIP3                                                                
117500 IMS-GET-MSG SECTION.                                                     
117600                                                                          
117700     MOVE '  QC' TO GODK-STATUSKODER                                      
117800     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
117900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
118000     PERFORM IMS-STATUSKONTROLL                                           
118100     .                                                                    
118200     SKIP3                                                                
118300 IMS-INSERT-MSG SECTION.                                                  
118400                                                                          
118500     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
118600       MOVE '0' TO MFS-KDHUVOMR                                           
118700     END-IF                                                               
118800     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
118900     MOVE SPACE TO GODK-STATUSKODER                                       
119000     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
119100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
119200     PERFORM IMS-STATUSKONTROLL                                           
119300     .                                                                    
119400     EJECT                                                                
119500 IMS-GET-XXKL-PRCNR SECTION.                                              
119600                                                                          
119700     STRING 'WLXXKL01(WDGXKEY  =' W-WDGXKEY-4453-X ')'                    
119800          DELIMITED BY SIZE INTO SSA1                                     
119900     MOVE '  GE' TO GODK-STATUSKODER                                      
120000     CALL CBLTDLI USING GHU XXKL-PCB DLI-IO-AREA SSA1                     
120100     MOVE XXKL-STATUS-CODE TO STATUS-WS                                   
120200     PERFORM IMS-STATUSKONTROLL                                           
120300     .                                                                    
120400     SKIP3                                                                
120500 IMS-GET-XXKL-TABELL SECTION.                                             
120600                                                                          
120700     STRING 'WLXXKL11*F(KDSEGKEY =' W-WDGXKEY-4454-X ')'                  
120800          DELIMITED BY SIZE INTO SSA1                                     
120900     MOVE '  GE' TO GODK-STATUSKODER                                      
121000     CALL CBLTDLI USING GHNP XXKL-PCB DLI-IO-AREA SSA1                    
121100     MOVE XXKL-STATUS-CODE TO STATUS-WS                                   
121200     PERFORM IMS-STATUSKONTROLL                                           
121300     .                                                                    
121400     EJECT                                                                
121500 IMS-GET-XXKH-PRC-UNIK SECTION.                                           
121600                                                                          
121700     STRING 'WLXXKH01(WDGXKEY  =' W-WDGXKEY-4447-X ')'                    
121800          DELIMITED BY SIZE INTO SSA1                                     
121900     STRING 'WLXXKH11(WDGXKEY  =' W-WDGXKEY-4448-X ')'                    
122000          DELIMITED BY SIZE INTO SSA2                                     
122100     MOVE '  GE' TO GODK-STATUSKODER                                      
122200     CALL CBLTDLI USING GU XXKH-PCB DLI-IO-AREA SSA1 SSA2                 
122300     MOVE XXKH-STATUS-CODE TO STATUS-WS                                   
122400     PERFORM IMS-STATUSKONTROLL                                           
122500     .                                                                    
122600     SKIP3                                                                
122700 IMS-REPL-XXKL-TABELL SECTION.                                            
122800                                                                          
122900     MOVE '  ' TO GODK-STATUSKODER                                        
123000     CALL CBLTDLI USING REPL XXKL-PCB DLI-IO-AREA                         
123100     MOVE XXKL-STATUS-CODE TO STATUS-WS                                   
123200     PERFORM IMS-STATUSKONTROLL                                           
123300     .                                                                    
123400     EJECT                                                                
123500 IMS-GU-WDB601    SECTION.                                                
123600     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
123700          DELIMITED BY SIZE INTO SSA1                                     
123800     MOVE '  GE' TO GODK-STATUSKODER                                      
123900     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
124000     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
124100     PERFORM IMS-STATUSKONTROLL                                           
124200     IF SEGMENT-SAKNAS                                                    
124300         MOVE SPACE TO DCS-KDDC                                           
124400     END-IF                                                               
124500     .                                                                    
124600 IMS-STATUSKONTROLL SECTION.                                              
124700                                                                          
124800     SET STATUS-IX TO 1                                                   
124900     SEARCH GODK-STATUS                                                   
125000       AT END CALL FELLOG                                                 
125100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
125200     END-SEARCH                                                           
125300     .                                                                    
