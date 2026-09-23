000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W6012500.                                                
000400*AUTHOR.         ROS-MARIE CLASON - GUIDE DATAKONSULT AB.                 
000500*DATE-WRITTEN.   92/03/24.                                                
000600*                                                                         
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        ALLMÄN BESKRIVNING:                                              
001100*        PROGRAMMET ÄR EN MPP SOM REGISTRERAR PARTIER PÅ                  
001200*        EN VAGN ELLER PLACERING.                                         
001300*        BILDEN ÄR EN REGISTRERINGSBILD.                                  
001400*                                                                         
001500*        PROGRAMMET UPPDATERAR W6INLA (W6D1)                              
001600*        PROGRAMMET LÄSER      W6PLAA (W6G1)                              
001700*        PROGRAMMET LÄSER              WDB6                               
001800*                                                                         
001900*    INDATA.                                                              
002000*        TRANSAKTION: W6T125                                              
002100*        MID:         W6I12501                                            
002200*                                                                         
002300*    UTDATA.                                                              
002400*        MOD:         W6O12501                                            
002500                                                                          
002600     SKIP3                                                                
002700 ENVIRONMENT DIVISION.                                                    
002800     EJECT                                                                
002900                                                                          
003000 DATA DIVISION.                                                           
003100 WORKING-STORAGE SECTION.                                                 
003200                                                                          
003300*    -- CHECKED BY WY2000                                                 
003400 77  IDPGM                       PIC X(08)   VALUE 'W6012500'.            
003500                                                                          
003600*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003700 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003800                                                                          
003900 77  JA                          PIC X       VALUE 'J'.                   
004000 77  NEJ                         PIC X       VALUE 'N'.                   
004100                                                                          
004200*    --- INDEX FÖR BLÄDDRINGSRADER                                        
004300 77  IX                          PIC S9(4)  VALUE +0    COMP SYNC.        
004400 77  IX1                         PIC S9(4)  VALUE +0    COMP SYNC.        
004500 77  MAX-IX                      PIC S9(4)  VALUE +14   COMP SYNC.        
004600 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
004700                                                                          
004800*    --- RÄKNARE                                                          
004900 77  FELRAKN                     PIC S9(2)  VALUE +0    COMP SYNC.        
005000                                                                          
005100*   OM SVAR TILL SKÄRM: MAX-MOD-LAENGD = (850) MOD-LÄNGD + 4              
005200 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +854 COMP SYNC.         
005300 77  P-TO-P-PREFIX-LNG           PIC S9(4)   VALUE +17  COMP SYNC.        
005400                                                                          
005500***********************************                                       
005600*        ARBETSFÄLT               *                                       
005700***********************************                                       
005800*                                                                         
005900*    --- ARBETSFÄLT                                                       
006000                                                                          
006100 01  WS-AREA.                                                             
006200     03  SPAR-KDINLSTA-NEW       PIC X(3) VALUE SPACE.                    
006300     03  SPAR-KDINLSTA-OLD       PIC X(3) VALUE SPACE.                    
006400*                                                                         
006500     03  SPAR-PRARTSTD-IX OCCURS 14.                                      
006600         05  SPAR-PRARTSTD       PIC 9(7)V9(2) VALUE ZERO.                
006700*                                                                         
006800     03  W-PTOP1-OCC-LL          PIC S9(4)    COMP-3 VALUE ZERO.          
006900                                                                          
007000*----TILL W60191                                                          
007100     03  SPAR-ADINLOMR-OLD       PIC X(4) VALUE SPACE.                    
007200     03  SPAR-ADINLOMR-NXT-OLD   PIC X(4) VALUE SPACE.                    
007300*                                                                         
007400 77  WS-IDLOPNRM                 PIC S9(9)   VALUE ZERO COMP-3.           
007500 77  WS-BEFT                     PIC S9(3)   VALUE ZERO COMP-3.           
007600 77  WS-IDARTNR                  PIC S9(9)   VALUE ZERO COMP-3.           
007700 77  WS-IDLOPNRM-NUM             PIC 9(9).                                
007800                                                                          
007900       EJECT                                                              
008000*    --- ARBETSFÄLT FÖR SWITCHAR                                          
008100                                                                          
008200 77  INDATA-SW                   PIC X       VALUE 'J'.                   
008300     88  INDATA-OK                           VALUE 'J'.                   
008400     88  INDATA-FEL                          VALUE 'N'.                   
008500                                                                          
008600 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
008700     88  NYCKLAR-OK                          VALUE 'J'.                   
008800     88  NYCKLAR-FEL                         VALUE 'N'.                   
008900                                                                          
009000 77  ALLT-SW                     PIC X       VALUE 'J'.                   
009100     88  ALLT-OK                             VALUE 'J'.                   
009200                                                                          
009300 77  UPD-SW                      PIC X       VALUE 'J'.                   
009400     88  UPD-OK                              VALUE 'J'.                   
009500     88  UPD-NEJ                             VALUE 'N'.                   
009600                                                                          
009700 77  FBRAPP-SW                   PIC X       VALUE 'J'.                   
009800     88  FBRAPP                              VALUE 'J'.                   
009900     88  FBRAPP-FINNS                        VALUE 'N'.                   
010000                                                                          
010100 77  PRIOGODS-SW                 PIC X       VALUE 'N'.                   
010200     88  PRIOGODS                            VALUE 'J'.                   
010300                                                                          
010400 77  TRANS91-SW                  PIC X       VALUE 'J'.                   
010500     88  TRANS91-JA                          VALUE 'J'.                   
010600     88  TRANS91-NEJ                         VALUE 'N'.                   
010700                                                                          
010800 77  RADFEL-SW                   PIC X       VALUE 'N'.                   
010900     88  RADFEL-JA                           VALUE 'J'.                   
011000     88  RADFEL-NEJ                          VALUE 'N'.                   
011100                                                                          
011200 77  RADERFEL-SW                 PIC X       VALUE 'N'.                   
011300     88  RADERFEL-JA                         VALUE 'J'.                   
011400     88  RADERFEL-NEJ                        VALUE 'N'.                   
011500                                                                          
011600 77  PRIM-CONTROL-SW             PIC X       VALUE 'N'.                   
011700     88  PRIM-CONTROL-YES                    VALUE 'J'.                   
011800     88  PRIM-CONTROL-NO                     VALUE 'N'.                   
011900                                                                          
012000*----------------------------------------------------------------*        
012100*   NKLTYP1=VAGN; VAGN, PLAC; VAGN, PLAC, ADR;                            
012200*   NKLTYP2=PLAC                                                          
012300*----------------------------------------------------------------*        
012400 77  NKLTYP-SW                  PIC X.                                    
012500     88  NKLTYP1                            VALUE '1'.                    
012600     88  NKLTYP2                            VALUE '2'.                    
012700                                                                          
012800                                                                          
012900 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
013000     88  EGEN-MID                            VALUE '6125'.                
013100     88  GODK-MID                            VALUE '6124' '6125'          
013200                                                   '6102'.                
013300     88  HELP-MID                            VALUE '0551'.                
013400     EJECT                                                                
013500                                                                          
013600*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
013700 01  GENERELLA-SUBPROGRAM.                                                
013800     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
013900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
014000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
014100     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
014200     EJECT                                                                
014300                                                                          
014400*01 -COPY WMSGINIT                                                        
014500     SKIP3                                                                
014600*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
014700*01 -COPY WMEDAREA                                                        
014800     SKIP3                                                                
014900 01  MESSAGE-CODES.                                                       
015000     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
015100     03  ERR-UPDATE-NOT-VALID    PIC X(3)    VALUE '007'.                 
015200     03  ERR-SAKN-I-REG          PIC X(3)    VALUE '010'.                 
015300     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
015400     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
015500     03  ERR-CODE-NOT-VALID      PIC X(3)    VALUE '416'.                 
015600     03  ERR-PLAC-ADR-FINNS      PIC X(3)    VALUE '196'.                 
015700     03  ERR-RADNR-1-SAKN        PIC X(3)    VALUE '178'.                 
015800                                                                          
015900     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
016000     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
016100     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
016200     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
016300     03  INF-LAST-PAGE           PIC X(3)    VALUE '106'.                 
016400     EJECT                                                                
016500                                                                          
016600*    --- AREOR FÖR BAKGRUNDS MPP:ER / BMP:ER                              
016700*    --- COPYTEXTER FÖR PRINTER, W60191                                   
016800*                                                                         
016900 01  FILLER                      PIC X(16)   VALUE 'P-TO-P-AREA'.         
017000*                                                                         
017100 01  P-TO-P-T91.                                                          
017200*----TILL W60191                                                          
017300     03  PTOP1-LL              PIC S9(4)   COMP SYNC.                     
017400     03  PTOP1-Z1              PIC X(1)    VALUE LOW-VALUE.               
017500     03  PTOP1-Z2              PIC X(1)    VALUE LOW-VALUE.               
017600     03  PTOP1-TRANSKOD        PIC X(7)    VALUE 'W6T191X'.               
017700     03  FILLER                PIC X(1)    VALUE SPACE.                   
017800     03  PTOP1-IDTRANS         PIC X(4)    VALUE '6125'.                  
017900     03  PTOP1-KDMFSFOR        PIC X(1)    VALUE SPACE.                   
018000*    03  MID -COPY W6I19101       -PRE T91-                               
018100     EJECT                                                                
018200                                                                          
018300                                                                          
018400*01  -COPY WMSGSNUF   -PRE  P-TO-P-                                       
018500     EJECT                                                                
018600                                                                          
018700*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
018800*                                                                         
018900 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
019000     SKIP3                                                                
019100*01  MID -COPY W6I12501                                                   
019200     EJECT                                                                
019300                                                                          
019400 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
019500     SKIP3                                                                
019600*01  -COPY WMSGAREA                                                       
019700     EJECT                                                                
019800                                                                          
019900     03  MOD REDEFINES MSG-AREA.                                          
020000*      05  -COPY W6O12501                                                 
020100     EJECT                                                                
020200                                                                          
020300                                                                          
020400 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
020500     SKIP3                                                                
020600*01  -COPY WMFSAREA                                                       
020700     EJECT                                                                
020800                                                                          
020900*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
021000*                                                                         
021100     EJECT                                                                
021200                                                                          
021300*                                                                         
021400*    --- DLI- NYCKLAR TILL IMS-SEKTIONERNA                                
021500*                                                                         
021600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
021700     SKIP3                                                                
021800 01  NYCKLAR-TILL-DLI.                                                    
021900                                                                          
022000     03  W1-IDLOPNRM-X.                                                   
022100         05  W1-IDLOPNRM          PIC S9(9)   COMP-3.                     
022200     03  W-IDRADNR-INL-X.                                                 
022300         05  W-IDRADNR-INL        PIC S9(5)   COMP-3.                     
022400     03  W-IDRADNR-X.                                                     
022500         05  W-IDRADNR            PIC S9(5)   COMP-3.                     
022600     03  W-IDDC-X.                                                        
022700         05  W-IDDC               PIC  X(2).                              
022800     03  W-IDINLVGN-X.                                                    
022900         05  W-IDINLVGN          PIC 9(3).                                
023000     03  W-W6GX01KEY-X.                                                   
023100         05  WGX-IDHTYP           PIC X(4)    VALUE '6005'.               
023200         05  WGX-IDDC             PIC X(2)    VALUE SPACE.                
023300         05  FILLER               PIC X(24)   VALUE LOW-VALUE.            
023400     03  W-W6GX11KEY-X.                                                   
023500         05  W-ADINLOMR           PIC X(4)    VALUE SPACE.                
023600         05  FILLER               PIC X(1)    VALUE LOW-VALUE.            
023700     03  W-W6GXKEY-6006-X.                                                
023800         05  W-6006-ADINLOMR     PIC X(4)    VALUE SPACE.                 
023900         05  FILLER              PIC X(1)    VALUE LOW-VALUE.             
024000     03  W-IDLOPNRM-X.                                                    
024100         05  W-IDLOPNRM-BSEQ     PIC S9(9)   COMP-3 VALUE ZERO.           
024200                                                                          
024300     03  W-IDDC-B6-X.                                                     
024400         05 W-IDDC-B6            PIC X(2).                                
024500                                                                          
024600     SKIP2                                                                
024700                                                                          
024800*    --- STATUS-KOD FRÅN IMS                                              
024900 01  STATUS-WS                   PIC XX.                                  
025000     88  SEGMENT-FINNS                       VALUE '  '.                  
025100     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
025200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
025300     SKIP2                                                                
025400                                                                          
025500 01  GODK-STATUSKODER.                                                    
025600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
025700     SKIP3                                                                
025800                                                                          
025900 01  SSA1                        PIC X(99).                               
026000 01  SSA2                        PIC X(64).                               
026100 01  SSA3                        PIC X(64).                               
026200     EJECT                                                                
026300                                                                          
026400*    --- IMS FUNKTIONSKODER                                               
026500*01  -COPY W0003                                                          
026600     EJECT                                                                
026700                                                                          
026800*    ---  DLI INPUT-OUTPUT AREA                                           
026900 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
027000     SKIP3                                                                
027100                                                                          
027200 01  DLI-IO-AREA.                                                         
027300     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
027400     SKIP3                                                                
027500                                                                          
027600     03  W6INLA11 REDEFINES IO-AREA.                                      
027700*        05  -COPY W6D111                                                 
027800     SKIP3                                                                
027900                                                                          
028000     03  W6PLAA01 REDEFINES IO-AREA.                                      
028100*        05  -COPY W6GX01                                                 
028200     SKIP3                                                                
028300                                                                          
028400     03  W6PLAA11 REDEFINES IO-AREA.                                      
028500*        05  -COPY W6GX6006                                               
028600     EJECT                                                                
028700                                                                          
028800 01  FILLER                    PIC X(16)    VALUE 'DLI-IO-W6D121'.        
028900                                                                          
029000 01  DLI-IO-AREA-W6D121.                                                  
029100     03  W6D121.                                                          
029200*        05  -COPY W6D121                                                 
029300     SKIP3                                                                
029400 01  FILLER                    PIC X(16)    VALUE 'DLI-IO-W6D1B1'.        
029500                                                                          
029600 01  DLI-IO-AREA-W6D1B1.                                                  
029700     03  W6INLC01.                                                        
029800*        05  -COPY W6D1B1                                                 
029900     EJECT                                                                
030000 01  FILLER                    PIC X(16)    VALUE 'DLI-IO-W6PLAA'.        
030100                                                                          
030200 01  DLI-IO-AREA-W6PLAA.                                                  
030300     03  W6GX6006.                                                        
030400*        05  -COPY W6GX6006  -PRE ALT-                                    
030500     EJECT                                                                
030600 01  DLI-IO-AREA-UPFA01.                                                  
030700     03  W6UPFA01.                                                        
030800*        05  -COPY W6L101                                                 
030900     SKIP3                                                                
031000 01  DLI-IO-AREA-UPFA11.                                                  
031100     03  W6UPFA11.                                                        
031200*        05  -COPY W6L111                                                 
031300     SKIP3                                                                
031400 01  DLI-IO-AREA-UPFA12.                                                  
031500     03  W6UPFA12.                                                        
031600*        05  -COPY W6L112                                                 
031700                                                                          
031800 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
031900 01   DLI-IO-AREA-B601.                                                   
032000*     03  -COPY WDB601                                                    
032100                                                                          
032200     SKIP3                                                                
032300                                                                          
032400 LINKAGE SECTION.                                                         
032500                                                                          
032600*01  -COPY W0009   -PRE MSG-                                              
032700     EJECT                                                                
032800                                                                          
032900*01  -COPY W0009   -PRE ALT1-                                             
033000     EJECT                                                                
033100                                                                          
033200*01  -COPY W0008  -PRE USEA-                                              
033300     05  FILLER                  PIC X.                                   
033400     EJECT                                                                
033500                                                                          
033600*01  -COPY W0008  -PRE INLB-                                              
033700     05  FILLER                  PIC X.                                   
033800     EJECT                                                                
033900                                                                          
034000*01  -COPY W0008  -PRE INLA1-                                             
034100     05  FILLER                  PIC X.                                   
034200     EJECT                                                                
034300                                                                          
034400*01  -COPY W0008  -PRE PLAA-                                              
034500     05  FILLER                  PIC X.                                   
034600     EJECT                                                                
034700*01  -COPY W0008  -PRE UPFA-                                              
034800     05  FILLER                  PIC X.                                   
034900     EJECT                                                                
035000*01  -COPY W0008  -PRE WDB6-                                              
035100     05  FILLER                  PIC X.                                   
035200     EJECT                                                                
035300 PROCEDURE DIVISION  USING MSG-PCB                                        
035400                           ALT1-PCB                                       
035500                           USEA-PCB                                       
035600                           INLB-PCB                                       
035700                           INLA1-PCB                                      
035800                           PLAA-PCB                                       
035900                           UPFA-PCB                                       
036000                           WDB6-PCB.                                      
036100     ENTRY 'DLITCBL' USING MSG-PCB                                        
036200                           ALT1-PCB                                       
036300                           USEA-PCB                                       
036400                           INLB-PCB                                       
036500                           INLA1-PCB                                      
036600                           PLAA-PCB                                       
036700                           UPFA-PCB                                       
036800                           WDB6-PCB.                                      
036900                                                                          
037000     EJECT                                                                
037100                                                                          
037200*----------------------------------------------------------------*        
037300     PERFORM IMS-GET-MSG                                                  
037400     IF SEGMENT-FINNS                                                     
037500        PERFORM A-INIT                                                    
037600        PERFORM B-KOLLA-NYCKLAR                                           
037700        IF NYCKLAR-OK                                                     
037800           IF MFS-UPDATE                                                  
037900              PERFORM G-KOLLA-INPUT                                       
038000              IF INDATA-OK                                                
038100                 PERFORM H-UPPDATERA                                      
038200              END-IF                                                      
038300           ELSE                                                           
038400              PERFORM F-LAES-VISA-INFO                                    
038500           END-IF                                                         
038600        END-IF                                                            
038700        PERFORM S90-BLANKUTF-NUM-FAELT                                    
038800        MOVE MAX-MOD-LAENGD TO MSG-KVLL                                   
038900        PERFORM IMS-INSERT-MSG                                            
039000     END-IF                                                               
039100                                                                          
039200     MOVE ZERO TO RETURN-CODE                                             
039300     GOBACK                                                               
039400     .                                                                    
039500     EJECT                                                                
039600*----------------------------------------------------------------*        
039700 A-INIT SECTION.                                                          
039800                                                                          
039900     IF MSG-DUBBLA-TRANSKODER                                             
040000        MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W6I12501                
040100        MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                 
040200        MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                               
040300     ELSE                                                                 
040400        MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W6I12501                 
040500        MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                 
040600        MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                               
040700     END-IF                                                               
040800                                                                          
040900     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
041000     MOVE MSG-IDPFK TO MFS-IDPFK                                          
041100     MOVE MFS-IDTRANS TO W-IDTRANS                                        
041200                                                                          
041300     MOVE LOW-VALUE TO MSG-AREA                                           
041400     MOVE 'W6O125N1' TO MFS-IDMOD                                         
041500     MOVE '6125' TO MOD-IDTRANS                                           
041600     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
041700                                                                          
041800     IF EGEN-MID OR HELP-MID                                              
041900        CONTINUE                                                          
042000     ELSE                                                                 
042100        MOVE SPACE TO MFS-KDTRTYP                                         
042200        MOVE '7' TO MFS-IDPFK                                             
042300     END-IF                                                               
042400                                                                          
042500     PERFORM AA-INIT-NYCKLAR                                              
042600                                                                          
042700     IF MSGI-IDLAND-SPR = 'GB'                                            
042800        MOVE +2 TO SPRAK-IX                                               
042900        MOVE 'GB ' TO MED-IDSKYLT                                         
043000     ELSE                                                                 
043100        MOVE +1 TO SPRAK-IX                                               
043200        MOVE 'S  ' TO MED-IDSKYLT                                         
043300     END-IF                                                               
043400     .                                                                    
043500     EJECT                                                                
043600*----------------------------------------------------------------*        
043700 AA-INIT-NYCKLAR SECTION.                                                 
043800                                                                          
043900     MOVE ALL '+' TO MSGI-WMSGINIT                                        
044000     MOVE '001'                  TO MSGI-KDCALL                           
044100     MOVE MSG-SIGNON-USERID      TO MSGI-IDUSER                           
044200     MOVE MSG-LTERM-NAME         TO MSGI-IDLTERM-USER                     
044300     MOVE '6125'                 TO MSGI-IDTRANS                          
044400     IF MFS-IDTRANS = '6125'                                              
044500       MOVE MID-IDINLVGN-IN      TO MSGI-IDINLVGN                         
044600     END-IF                                                               
044700     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
044800     .                                                                    
044900     EJECT                                                                
045000*----------------------------------------------------------------*        
045100 B-KOLLA-NYCKLAR SECTION.                                                 
045200                                                                          
045300     MOVE JA                 TO NYCKLAR-SW                                
045400     MOVE JA                 TO INDATA-SW                                 
045500     MOVE SPACE              TO NKLTYP-SW                                 
045600*    -- KONTROLL AV IDDC                                                  
045700                                                                          
045800     MOVE MFS-RENSA-FAELT TO MOD-IDDC-IN                                  
045900     IF MID-IDDC-IN = ALL '+'                                             
046000       MOVE MSGI-IDDC   TO W-IDDC-B6                                      
046100     ELSE                                                                 
046200       MOVE MID-IDDC-IN TO W-IDDC-B6                                      
046300       MOVE '7'         TO MFS-IDPFK                                      
046400       MOVE SPACE       TO MFS-KDTRTYP                                    
046500     END-IF                                                               
046600     PERFORM IMS-GU-WDB601                                                
046700                                                                          
046800     IF DCS-KDDC = SPACE OR DCS-DDC                                       
046900         MOVE NEJ       TO NYCKLAR-SW                                     
047000         MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                               
047100         PERFORM S02-CALL-WMEDKONV-FEL                                    
047200     ELSE                                                                 
047300         MOVE DCS-IDDC  TO W-IDDC                                         
047400                           WGX-IDDC                                       
047500                           MOD-IDDC-UT                                    
047600     END-IF                                                               
047700*----NKL-FÄLT-UT                                                          
047800     INSPECT MID-IDINLVGN-IN REPLACING LEADING SPACE BY ZERO              
047900     INSPECT MID-IDINLVGN-UT REPLACING LEADING SPACE BY ZERO              
048000                                                                          
048100     IF MID-IDINLVGN-IN      = ALL '+' AND                                
048200        MID-ADINLOMR-IN      = ALL '+' AND                                
048300        MID-ADINLOMR-NXT-IN  = ALL '+'                                    
048400        IF MID-IDINLVGN-UT      = ZERO  AND                               
048500           MID-ADINLOMR-UT      = SPACE AND                               
048600           MID-ADINLOMR-NXT-UT  = SPACE                                   
048700*-FEL - 401                                                               
048800           MOVE NEJ          TO NYCKLAR-SW                                
048900           MOVE ERR-WRONG-KEY       TO MED-IDMFSFEL                       
049000           PERFORM S02-CALL-WMEDKONV-FEL                                  
049100        ELSE                                                              
049200*----------GAMLA NYCKLAR                                                  
049300           PERFORM BB-GAMLA-NKL                                           
049400        END-IF                                                            
049500     ELSE                                                                 
049600*-------NYA NYCKLAR                                                       
049700        MOVE SPACE TO MFS-KDTRTYP                                         
049800        MOVE '7' TO MFS-IDPFK                                             
049900        PERFORM BD-KONTR-PLAC-ADR-LIKA                                    
050000        IF NYCKLAR-OK                                                     
050100           PERFORM BA-FORMELLA-KONTR                                      
050200           PERFORM MFS-RENSA-FAELT-UT                                     
050300        END-IF                                                            
050400     END-IF                                                               
050500                                                                          
050600     IF NYCKLAR-OK                                                        
050700        PERFORM BC-KOLLA-NKL-BAS                                          
050800     END-IF                                                               
050900                                                                          
051000     IF NYCKLAR-FEL                                                       
051100        PERFORM MFS-RENSA-FAELT-UT                                        
051200     END-IF                                                               
051300                                                                          
051400     IF GODK-MID OR HELP-MID                                              
051500        CONTINUE                                                          
051600     ELSE                                                                 
051700*       MOVE MFS-RENSA-FAELT      TO MOD-IDINLVGN-UT                      
051800        MOVE MFS-RENSA-FAELT   TO MOD-ADINLOMR-UT                         
051900                                  MOD-ADINLOMR-NXT-UT                     
052000                                  MOD-TEMFSFEL                            
052100     END-IF                                                               
052200                                                                          
052300     PERFORM MFS-RENSA-FAELT-UT                                           
052400     PERFORM MFS-RENSA-FAELT-IN                                           
052500     .                                                                    
052600     EJECT                                                                
052700*----------------------------------------------------------------*        
052800 BA-FORMELLA-KONTR SECTION.                                               
052900                                                                          
053000*----TA REDA PÅ VILKEN NKLTYP, KOLLA VAGN,                                
053100*----SAMT FLYTTA MID-IN TILL MOD-UT                                       
053200*----VAGN                                                                 
053300     IF MID-IDINLVGN-IN     NOT = ALL '+' AND                             
053400        MID-ADINLOMR-IN         = ALL '+' AND                             
053500        MID-ADINLOMR-NXT-IN     = ALL '+'                                 
053600        PERFORM BAA-KONTR-VAGN                                            
053700        MOVE MID-IDINLVGN-IN       TO MOD-IDINLVGN-UT                     
053800        MOVE SPACE                 TO MOD-ADINLOMR-UT                     
053900                                      MOD-ADINLOMR-NXT-UT                 
054000        MOVE '1'                   TO NKLTYP-SW                           
054100     ELSE                                                                 
054200*-------VAGN-PLAC                                                         
054300        IF MID-IDINLVGN-IN     NOT = ALL '+' AND                          
054400           MID-ADINLOMR-IN     NOT = ALL '+' AND                          
054500           MID-ADINLOMR-NXT-IN     = ALL '+'                              
054600           PERFORM BAA-KONTR-VAGN                                         
054700           MOVE MID-IDINLVGN-IN    TO MOD-IDINLVGN-UT                     
054800           MOVE MID-ADINLOMR-IN    TO MOD-ADINLOMR-UT                     
054900           MOVE SPACE              TO MOD-ADINLOMR-NXT-UT                 
055000           MOVE '1'                TO NKLTYP-SW                           
055100        ELSE                                                              
055200*----------VAGN-PLAC-ADR                                                  
055300           IF MID-IDINLVGN-IN     NOT = ALL '+' AND                       
055400              MID-ADINLOMR-IN     NOT = ALL '+' AND                       
055500              MID-ADINLOMR-NXT-IN NOT = ALL '+'                           
055600              PERFORM BAA-KONTR-VAGN                                      
055700              MOVE MID-IDINLVGN-IN     TO MOD-IDINLVGN-UT                 
055800              MOVE MID-ADINLOMR-IN     TO MOD-ADINLOMR-UT                 
055900              MOVE MID-ADINLOMR-NXT-IN TO MOD-ADINLOMR-NXT-UT             
056000              MOVE '1'             TO NKLTYP-SW                           
056100           ELSE                                                           
056200*-------------PLAC                                                        
056300              IF MID-IDINLVGN-IN      = ALL '+' AND                       
056400                 MID-ADINLOMR-IN  NOT = ALL '+' AND                       
056500                 MID-ADINLOMR-NXT-IN  = ALL '+'                           
056600                 MOVE MID-ADINLOMR-IN TO MOD-ADINLOMR-UT                  
056700                 MOVE SPACE           TO MOD-ADINLOMR-NXT-UT              
056800                 MOVE +000            TO MOD-IDINLVGN-UT                  
056900                 MOVE '2'          TO NKLTYP-SW                           
057000              ELSE                                                        
057100*----------------PLAC-ADR                                                 
057200                 IF MID-IDINLVGN-IN     = ALL '+' AND                     
057300                    MID-ADINLOMR-IN NOT = ALL '+' AND                     
057400                    MID-ADINLOMR-NXT-IN NOT = ALL '+'                     
057500                    MOVE +000            TO MOD-IDINLVGN-UT               
057600                    MOVE MID-ADINLOMR-IN TO MOD-ADINLOMR-UT               
057700                    MOVE MID-ADINLOMR-NXT-IN TO                           
057800                         MOD-ADINLOMR-NXT-UT                              
057900                    MOVE '2'       TO NKLTYP-SW                           
058000                 ELSE                                                     
058100*-------------------FEL    NYCKEL - 401                                   
058200                    MOVE NEJ              TO NYCKLAR-SW                   
058300                    MOVE ERR-WRONG-KEY    TO MED-IDMFSFEL                 
058400                    IF MID-IDINLVGN-IN = ALL '+'                          
058500                       MOVE +000          TO MOD-IDINLVGN-UT              
058600                    ELSE                                                  
058700                       MOVE MID-IDINLVGN-IN TO MOD-IDINLVGN-UT            
058800                    END-IF                                                
058900                    IF MID-ADINLOMR-IN = ALL '+'                          
059000                       MOVE SPACE         TO MOD-ADINLOMR-UT              
059100                    ELSE                                                  
059200                       MOVE MID-ADINLOMR-IN TO MOD-ADINLOMR-UT            
059300                    END-IF                                                
059400                    IF MID-ADINLOMR-NXT-IN = ALL '+'                      
059500                       MOVE SPACE         TO MOD-ADINLOMR-NXT-UT          
059600                    ELSE                                                  
059700                      MOVE MID-ADINLOMR-NXT-IN TO                         
059800                           MOD-ADINLOMR-NXT-UT                            
059900                    END-IF                                                
060000                    PERFORM S02-CALL-WMEDKONV-FEL                         
060100                 END-IF                                                   
060200              END-IF                                                      
060300           END-IF                                                         
060400        END-IF                                                            
060500     END-IF                                                               
060600     .                                                                    
060700     EJECT                                                                
060800*----------------------------------------------------------------*        
060900 BAA-KONTR-VAGN    SECTION.                                               
061000                                                                          
061100     IF MID-IDINLVGN-IN = ZERO OR                                         
061200        MID-IDINLVGN-IN NOT NUMERIC                                       
061300*-FEL - 401                                                               
061400        MOVE NEJ                   TO NYCKLAR-SW                          
061500        MOVE ERR-WRONG-KEY         TO MED-IDMFSFEL                        
061600        PERFORM S02-CALL-WMEDKONV-FEL                                     
061700     END-IF                                                               
061800     .                                                                    
061900     EJECT                                                                
062000*----------------------------------------------------------------*        
062100 BB-GAMLA-NKL   SECTION.                                                  
062200                                                                          
062300*----VAGN                                                                 
062400     IF MID-IDINLVGN-UT     > ZERO AND                                    
062500        MID-ADINLOMR-UT     = SPACE AND                                   
062600        MID-ADINLOMR-NXT-UT = SPACE                                       
062700        MOVE '1'                   TO NKLTYP-SW                           
062800        MOVE MID-IDINLVGN-UT       TO MOD-IDINLVGN-UT                     
062900        MOVE SPACE                 TO MOD-ADINLOMR-UT                     
063000                                      MOD-ADINLOMR-NXT-UT                 
063100     ELSE                                                                 
063200*-------VAGN-PLAC                                                         
063300        IF MID-IDINLVGN-UT     > ZERO AND                                 
063400           MID-ADINLOMR-UT NOT = SPACE AND                                
063500           MID-ADINLOMR-NXT-UT = SPACE                                    
063600           MOVE '1'                TO NKLTYP-SW                           
063700           MOVE MID-IDINLVGN-UT    TO MOD-IDINLVGN-UT                     
063800           MOVE MID-ADINLOMR-UT    TO MOD-ADINLOMR-UT                     
063900           MOVE SPACE              TO MOD-ADINLOMR-NXT-UT                 
064000        ELSE                                                              
064100*----------VAGN-PLAC-ADR                                                  
064200           IF MID-IDINLVGN-UT         > ZERO AND                          
064300              MID-ADINLOMR-UT     NOT = SPACE AND                         
064400              MID-ADINLOMR-NXT-UT NOT = SPACE                             
064500              MOVE '1'                 TO NKLTYP-SW                       
064600              MOVE MID-IDINLVGN-UT     TO MOD-IDINLVGN-UT                 
064700              MOVE MID-ADINLOMR-UT     TO MOD-ADINLOMR-UT                 
064800              MOVE MID-ADINLOMR-NXT-UT TO MOD-ADINLOMR-NXT-UT             
064900           ELSE                                                           
065000*-------------PLAC                                                        
065100              IF MID-IDINLVGN-UT         = ZERO AND                       
065200                 MID-ADINLOMR-UT     NOT = SPACE AND                      
065300                 MID-ADINLOMR-NXT-UT     = SPACE                          
065400                 MOVE '2'             TO NKLTYP-SW                        
065500                 MOVE +000            TO MOD-IDINLVGN-UT                  
065600                 MOVE SPACE           TO MOD-ADINLOMR-NXT-UT              
065700                 MOVE MID-ADINLOMR-UT TO MOD-ADINLOMR-UT                  
065800              ELSE                                                        
065900*----------------PLAC-ADR                                                 
066000                 IF MID-IDINLVGN-UT   = ZERO AND                          
066100                    MID-ADINLOMR-UT NOT = SPACE AND                       
066200                    MID-ADINLOMR-NXT-UT NOT = SPACE                       
066300                    MOVE '2'             TO NKLTYP-SW                     
066400                    MOVE +000            TO MOD-IDINLVGN-UT               
066500                    MOVE MID-ADINLOMR-UT TO MOD-ADINLOMR-UT               
066600                   MOVE MID-ADINLOMR-NXT-UT TO MOD-ADINLOMR-NXT-UT        
066700                 END-IF                                                   
066800              END-IF                                                      
066900           END-IF                                                         
067000        END-IF                                                            
067100     END-IF                                                               
067200     .                                                                    
067300     EJECT                                                                
067400*----------------------------------------------------------------*        
067500 BC-KOLLA-NKL-BAS SECTION.                                                
067600                                                                          
067700*----KONTROLLER BEROENDE PÅ NKLTYP                                        
067800*---- 1 = MED VAGN                                                        
067900*---- 2 = UTAN VAGN                                                       
068000                                                                          
068100     IF NKLTYP1                                                           
068200        PERFORM BCA-KOLLA-NKLTYP1                                         
068300     END-IF                                                               
068400                                                                          
068500     IF NKLTYP2                                                           
068600        PERFORM BCB-KOLLA-NKLTYP2                                         
068700     END-IF                                                               
068800     .                                                                    
068900     EJECT                                                                
069000*----------------------------------------------------------------*        
069100 BCA-KOLLA-NKLTYP1 SECTION.                                               
069200                                                                          
069300*    HÄR ANVÄNDS UT-NYCKLARNA VID KONTROLL                                
069400*    (DESSA ÄR INITIERADE I BA- RESP BB-)                                 
069500                                                                          
069600     PERFORM BCAF-INIT-INLA1-BAS                                          
069700                                                                          
069800     IF SEGMENT-SAKNAS                                                    
069900        IF MOD-IDINLVGN-UT > ZERO                                         
070000           IF MOD-ADINLOMR-UT NOT = SPACE AND                             
070100              MOD-ADINLOMR-UT NOT = LOW-VALUE                             
070200*-------------OK KOLLA PLAC                                               
070300              PERFORM BCAD-KOLLA-PLAC                                     
070400              IF NYCKLAR-OK                                               
070500                 IF MOD-ADINLOMR-NXT-UT NOT = SPACE AND                   
070600                    MOD-ADINLOMR-NXT-UT NOT = LOW-VALUE                   
070700*-------------------OK KOLLA ADR                                          
070800                    PERFORM BCAE-KOLLA-ADR                                
070900                 END-IF                                                   
071000              END-IF                                                      
071100           ELSE                                                           
071200*-FEL - 401                                                               
071300*-------------PLAC  SAKNAS                                                
071400              MOVE NEJ             TO NYCKLAR-SW                          
071500              MOVE ERR-SAKN-I-REG  TO MED-IDMFSFEL                        
071600              PERFORM S02-CALL-WMEDKONV-FEL                               
071700           END-IF                                                         
071800        END-IF                                                            
071900     ELSE                                                                 
072000*-------VAGN FINNS                                                        
072100        PERFORM BCAG-INIT-INLA                                            
072200        IF SEGMENT-FINNS                                                  
072300           IF MOD-IDINLVGN-UT     > ZERO AND                              
072400              MOD-ADINLOMR-UT     = SPACE AND                             
072500              MOD-ADINLOMR-NXT-UT = SPACE                                 
072600              PERFORM BCAA-VAGN                                           
072700           ELSE                                                           
072800              IF MOD-IDINLVGN-UT      > ZERO AND                          
072900                 MOD-ADINLOMR-UT  NOT = SPACE AND                         
073000                 MOD-ADINLOMR-NXT-UT  = SPACE                             
073100                 PERFORM BCAB-VAGN-PLAC                                   
073200              ELSE                                                        
073300                 IF MOD-IDINLVGN-UT   > ZERO AND                          
073400                    MOD-ADINLOMR-UT NOT = SPACE AND                       
073500                    MOD-ADINLOMR-NXT-UT NOT = SPACE                       
073600                    PERFORM BCAC-VAGN-PLAC-ADR                            
073700                 END-IF                                                   
073800              END-IF                                                      
073900           END-IF                                                         
074000        END-IF                                                            
074100     END-IF                                                               
074200     .                                                                    
074300     EJECT                                                                
074400*----------------------------------------------------------------*        
074500 BCAA-VAGN   SECTION.                                                     
074600                                                                          
074700     MOVE RAD-IDINLVGN             TO MOD-SPAR-IDINLVGN                   
074800     MOVE RAD-ADINLOMR             TO MOD-SPAR-ADINLOMR                   
074900                                      MOD-ADINLOMR-UT                     
075000     MOVE RAD-ADINLOMR-NXT         TO MOD-SPAR-ADINLOMR-NXT               
075100                                      MOD-ADINLOMR-NXT-UT                 
075200     .                                                                    
075300     EJECT                                                                
075400*----------------------------------------------------------------*        
075500 BCAB-VAGN-PLAC   SECTION.                                                
075600                                                                          
075700     IF RAD-ADINLOMR = MOD-ADINLOMR-UT                                    
075800        MOVE RAD-IDINLVGN          TO MOD-SPAR-IDINLVGN                   
075900        MOVE RAD-ADINLOMR          TO MOD-SPAR-ADINLOMR                   
076000        MOVE RAD-ADINLOMR-NXT      TO MOD-SPAR-ADINLOMR-NXT               
076100                                      MOD-ADINLOMR-NXT-UT                 
076200     ELSE                                                                 
076300*-FEL - 196                                                               
076400*------ ANGIVEN PLACERING STÄMMER EJ                                      
076500*       BYT I NKL-FÄLT                                                    
076600        MOVE RAD-IDINLVGN          TO MOD-SPAR-IDINLVGN                   
076700        MOVE RAD-ADINLOMR          TO MOD-SPAR-ADINLOMR                   
076800                                      MOD-ADINLOMR-UT                     
076900        MOVE RAD-ADINLOMR-NXT      TO MOD-SPAR-ADINLOMR-NXT               
077000                                      MOD-ADINLOMR-NXT-UT                 
077100        MOVE ERR-PLAC-ADR-FINNS    TO MED-IDMFSINF                        
077200        PERFORM S03-CALL-WMEDKONV-INF                                     
077300     END-IF                                                               
077400     .                                                                    
077500     EJECT                                                                
077600*----------------------------------------------------------------*        
077700 BCAC-VAGN-PLAC-ADR    SECTION.                                           
077800                                                                          
077900     IF RAD-ADINLOMR = MOD-ADINLOMR-UT                                    
078000        MOVE RAD-IDINLVGN          TO MOD-SPAR-IDINLVGN                   
078100        MOVE RAD-ADINLOMR          TO MOD-SPAR-ADINLOMR                   
078200     ELSE                                                                 
078300*-FEL - 196                                                               
078400*-------ANGIVEN PLACERING STÄMMER EJ                                      
078500*       BYT I NKL-FÄLT                                                    
078600        MOVE RAD-IDINLVGN          TO MOD-SPAR-IDINLVGN                   
078700        MOVE RAD-ADINLOMR          TO MOD-SPAR-ADINLOMR                   
078800                                      MOD-ADINLOMR-UT                     
078900        MOVE ERR-PLAC-ADR-FINNS    TO MED-IDMFSINF                        
079000        PERFORM S03-CALL-WMEDKONV-INF                                     
079100     END-IF                                                               
079200                                                                          
079300     IF RAD-ADINLOMR-NXT = MOD-ADINLOMR-NXT-UT                            
079400        MOVE RAD-ADINLOMR-NXT      TO MOD-SPAR-ADINLOMR-NXT               
079500     ELSE                                                                 
079600*-FEL - 196                                                               
079700*-------ANGIVEN PLACERING STÄMMER EJ                                      
079800*       BYT I NKL-FÄLT                                                    
079900        MOVE RAD-ADINLOMR-NXT      TO MOD-SPAR-ADINLOMR-NXT               
080000                                      MOD-ADINLOMR-NXT-UT                 
080100        MOVE ERR-PLAC-ADR-FINNS    TO MED-IDMFSINF                        
080200        PERFORM S03-CALL-WMEDKONV-INF                                     
080300     END-IF                                                               
080400     .                                                                    
080500     EJECT                                                                
080600*----------------------------------------------------------------*        
080700 BCAD-KOLLA-PLAC   SECTION.                                               
080800                                                                          
080900*----KOLLA PLACERING - LÄS PLAA                                           
081000                                                                          
081100     MOVE '6005'                   TO WGX-IDHTYP                          
081200     MOVE MOD-ADINLOMR-UT          TO W-ADINLOMR                          
081300                                                                          
081400     PERFORM IMS-GU-PLAA-G111                                             
081500     IF SEGMENT-FINNS                                                     
081600        MOVE MOD-IDINLVGN-UT       TO MOD-SPAR-IDINLVGN                   
081700        MOVE MOD-ADINLOMR-UT       TO MOD-SPAR-ADINLOMR                   
081800        MOVE SPACE                 TO MOD-SPAR-ADINLOMR-NXT               
081900     ELSE                                                                 
082000*-FEL - 401                                                               
082100        MOVE +000                  TO MOD-SPAR-IDINLVGN                   
082200        MOVE SPACE                 TO MOD-SPAR-ADINLOMR                   
082300                                      MOD-SPAR-ADINLOMR-NXT               
082400        MOVE NEJ                   TO NYCKLAR-SW                          
082500        MOVE ERR-SAKN-I-REG        TO MED-IDMFSFEL                        
082600        PERFORM S02-CALL-WMEDKONV-FEL                                     
082700     END-IF                                                               
082800     .                                                                    
082900     EJECT                                                                
083000*----------------------------------------------------------------*        
083100 BCAE-KOLLA-ADR SECTION.                                                  
083200                                                                          
083300*----KOLLA ADRESS - LÄS PLAA                                              
083400                                                                          
083500     MOVE '6005'                   TO WGX-IDHTYP                          
083600     MOVE MOD-ADINLOMR-NXT-UT        TO W-ADINLOMR                        
083700                                                                          
083800     PERFORM IMS-GU-PLAA-G111                                             
083900     IF SEGMENT-FINNS                                                     
084000        MOVE MOD-ADINLOMR-NXT-UT     TO MOD-SPAR-ADINLOMR-NXT             
084100     ELSE                                                                 
084200*-FEL - 401                                                               
084300        MOVE +000                    TO MOD-SPAR-IDINLVGN                 
084400        MOVE SPACE                   TO MOD-SPAR-ADINLOMR                 
084500                                        MOD-SPAR-ADINLOMR-NXT             
084600        MOVE NEJ                     TO NYCKLAR-SW                        
084700        MOVE ERR-SAKN-I-REG          TO MED-IDMFSFEL                      
084800        PERFORM S02-CALL-WMEDKONV-FEL                                     
084900     END-IF                                                               
085000     .                                                                    
085100     EJECT                                                                
085200*----------------------------------------------------------------*        
085300 BCAF-INIT-INLA1-BAS SECTION.                                             
085400                                                                          
085500     MOVE MOD-IDINLVGN-UT     TO W-IDINLVGN                               
085600                                                                          
085700     PERFORM IMS-GU-INLA1-D111                                            
085800     .                                                                    
085900     EJECT                                                                
086000*----------------------------------------------------------------*        
086100 BCAG-INIT-INLA     SECTION.                                              
086200                                                                          
086300     MOVE MOD-IDINLVGN-UT     TO W-IDINLVGN                               
086400                                                                          
086500     PERFORM IMS-GNP-INLA1-D121                                           
086600     .                                                                    
086700     EJECT                                                                
086800*----------------------------------------------------------------*        
086900 BCB-KOLLA-NKLTYP2 SECTION.                                               
087000                                                                          
087100     MOVE +000                     TO MOD-SPAR-IDINLVGN                   
087200     MOVE '6005'                   TO WGX-IDHTYP                          
087300                                                                          
087400     PERFORM  BCBA-KOLLA-NKLTYP2-PLAC                                     
087500                                                                          
087600     IF NYCKLAR-OK                                                        
087700        IF MOD-ADINLOMR-NXT-UT NOT = SPACE                                
087800           PERFORM BCBB-KOLLA-NKLTYP2-ADR                                 
087900        ELSE                                                              
088000           MOVE SPACE                TO MOD-SPAR-ADINLOMR-NXT             
088100        END-IF                                                            
088200     END-IF                                                               
088300     .                                                                    
088400     EJECT                                                                
088500*----------------------------------------------------------------*        
088600 BCBA-KOLLA-NKLTYP2-PLAC SECTION.                                         
088700                                                                          
088800*--- KOLLA PLACERING - LÄS PLAA                                           
088900                                                                          
089000     MOVE MOD-ADINLOMR-UT            TO W-ADINLOMR                        
089100                                                                          
089200     PERFORM IMS-GU-PLAA-G111                                             
089300     IF SEGMENT-SAKNAS                                                    
089400*-FEL - 401                                                               
089500        MOVE +000                    TO MOD-SPAR-IDINLVGN                 
089600        MOVE SPACE                   TO MOD-SPAR-ADINLOMR                 
089700                                        MOD-SPAR-ADINLOMR-NXT             
089800        MOVE NEJ                     TO NYCKLAR-SW                        
089900        MOVE ERR-SAKN-I-REG          TO MED-IDMFSFEL                      
090000        PERFORM S02-CALL-WMEDKONV-FEL                                     
090100     ELSE                                                                 
090200        MOVE MOD-ADINLOMR-UT         TO MOD-SPAR-ADINLOMR                 
090300     END-IF                                                               
090400     .                                                                    
090500     EJECT                                                                
090600*----------------------------------------------------------------*        
090700 BCBB-KOLLA-NKLTYP2-ADR  SECTION.                                         
090800                                                                          
090900*--- KOLLA ADRESS - LÄS PLAA                                              
091000     MOVE MOD-ADINLOMR-NXT-UT        TO W-ADINLOMR                        
091100     PERFORM IMS-GU-PLAA-G111                                             
091200     IF SEGMENT-SAKNAS                                                    
091300*-FEL - 401                                                               
091400        MOVE +000                    TO MOD-SPAR-IDINLVGN                 
091500        MOVE SPACE                   TO MOD-SPAR-ADINLOMR                 
091600                                        MOD-SPAR-ADINLOMR-NXT             
091700        MOVE NEJ                     TO NYCKLAR-SW                        
091800        MOVE ERR-SAKN-I-REG          TO MED-IDMFSFEL                      
091900        PERFORM S02-CALL-WMEDKONV-FEL                                     
092000     ELSE                                                                 
092100        MOVE MOD-ADINLOMR-NXT-UT     TO MOD-SPAR-ADINLOMR-NXT             
092200     END-IF                                                               
092300     .                                                                    
092400     EJECT                                                                
092500*----------------------------------------------------------------*        
092600 BD-KONTR-PLAC-ADR-LIKA SECTION.                                          
092700                                                                          
092800*----PLAC-ADR                                                             
092900     IF MID-ADINLOMR-IN     NOT = ALL '+' AND                             
093000        MID-ADINLOMR-NXT-IN NOT = ALL '+'                                 
093100        IF MID-ADINLOMR-IN = MID-ADINLOMR-NXT-IN                          
093200*-FEL - 401                                                               
093300*----------FEL KAN EJ VARA LIKA                                           
093400           MOVE NEJ                       TO NYCKLAR-SW                   
093500           MOVE ERR-WRONG-KEY             TO MED-IDMFSFEL                 
093600           MOVE MID-ADINLOMR-IN           TO MOD-ADINLOMR-UT              
093700           MOVE MID-ADINLOMR-NXT-IN       TO MOD-ADINLOMR-NXT-UT          
093800           PERFORM S02-CALL-WMEDKONV-FEL                                  
093900        END-IF                                                            
094000     END-IF                                                               
094100     .                                                                    
094200     EJECT                                                                
094300*----------------------------------------------------------------*        
094400 F-LAES-VISA-INFO SECTION.                                                
094500                                                                          
094600     IF HELP-MID                                                          
094700        PERFORM FA-MID-TILL-MOD                                           
094800     END-IF                                                               
094900                                                                          
095000     IF HELP-MID OR EGEN-MID                                              
095100        IF MID-IDLOPNRM(1) = ALL '+' AND                                  
095200           MID-IDLOPNRM(2) = ALL '+' AND                                  
095300           MID-IDLOPNRM(3) = ALL '+' AND                                  
095400           MID-IDLOPNRM(4) = ALL '+' AND                                  
095500           MID-IDLOPNRM(5) = ALL '+' AND                                  
095600           MID-IDLOPNRM(6) = ALL '+' AND                                  
095700           MID-IDLOPNRM(7) = ALL '+' AND                                  
095800           MID-IDLOPNRM(8) = ALL '+' AND                                  
095900           MID-IDLOPNRM(9) = ALL '+' AND                                  
096000           MID-IDLOPNRM(10) = ALL '+' AND                                 
096100           MID-IDLOPNRM(11) = ALL '+'                                     
096200*----------OK                                                             
096300           CONTINUE                                                       
096400        ELSE                                                              
096500           IF MFS-FIRST OR HELP-MID                                       
096600              CONTINUE                                                    
096700           ELSE                                                           
096800              MOVE INF-PRESS-PF11  TO MED-IDMFSFEL                        
096900              PERFORM S02-CALL-WMEDKONV-FEL                               
097000              PERFORM MFS-ROER-EJ-FAELT-IN                                
097100              PERFORM MFS-ROER-EJ-FAELT-UT                                
097200              PERFORM MFS-LAES-IN-IGEN                                    
097300           END-IF                                                         
097400        END-IF                                                            
097500     END-IF                                                               
097600     PERFORM MFS-RENSA-FAELT-IN                                           
097700     .                                                                    
097800     EJECT                                                                
097900*----------------------------------------------------------------*        
098000 FA-MID-TILL-MOD  SECTION.                                                
098100                                                                          
098200     MOVE  +1             TO IX                                           
098300     PERFORM UNTIL IX > MAX-IX                                            
098400        IF MID-RAD(IX) = ALL '+'                                          
098500           MOVE MFS-RENSA-FAELT       TO MOD-IDLOPNRM(IX)                 
098600        ELSE                                                              
098700          INSPECT MID-IDLOPNRM(IX) REPLACING LEADING SPACE BY ZERO        
098800           MOVE MID-IDLOPNRM(IX)      TO MOD-IDLOPNRM(IX)                 
098900           MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDLOPNRM-ATTR(IX)            
099000        END-IF                                                            
099100        ADD 1             TO IX                                           
099200     END-PERFORM                                                          
099300     .                                                                    
099400     EJECT                                                                
099500*----------------------------------------------------------------*        
099600 G-KOLLA-INPUT SECTION.                                                   
099700                                                                          
099800     MOVE JA  TO INDATA-SW                                                
099900                                                                          
100000     IF MID-IDLOPNRM(1) = ALL '+' AND                                     
100100        MID-IDLOPNRM(2) = ALL '+' AND                                     
100200        MID-IDLOPNRM(3) = ALL '+' AND                                     
100300        MID-IDLOPNRM(4) = ALL '+' AND                                     
100400        MID-IDLOPNRM(5) = ALL '+' AND                                     
100500        MID-IDLOPNRM(6) = ALL '+' AND                                     
100600        MID-IDLOPNRM(7) = ALL '+' AND                                     
100700        MID-IDLOPNRM(8) = ALL '+' AND                                     
100800        MID-IDLOPNRM(9) = ALL '+' AND                                     
100900        MID-IDLOPNRM(10) = ALL '+' AND                                    
101000        MID-IDLOPNRM(11) = ALL '+' AND                                    
101100        MID-IDLOPNRM(12) = ALL '+' AND                                    
101200        MID-IDLOPNRM(13) = ALL '+' AND                                    
101300        MID-IDLOPNRM(14) = ALL '+'                                        
101400        MOVE ERR-PF11-AND-NO-DATA  TO MED-IDMFSFEL                        
101500        PERFORM S01-CALL-WMEDKONV-FEL                                     
101600        MOVE NEJ                   TO INDATA-SW                           
101700     ELSE                                                                 
101800        IF MID-IDLOPNRM(1) NOT = ALL '+' OR                               
101900           MID-IDLOPNRM(2) NOT = ALL '+' OR                               
102000           MID-IDLOPNRM(3) NOT = ALL '+' OR                               
102100           MID-IDLOPNRM(4) NOT = ALL '+' OR                               
102200           MID-IDLOPNRM(5) NOT = ALL '+' OR                               
102300           MID-IDLOPNRM(6) NOT = ALL '+' OR                               
102400           MID-IDLOPNRM(7) NOT = ALL '+' OR                               
102500           MID-IDLOPNRM(8) NOT = ALL '+' OR                               
102600           MID-IDLOPNRM(9) NOT = ALL '+' OR                               
102700           MID-IDLOPNRM(10) NOT = ALL '+' OR                              
102800           MID-IDLOPNRM(11) NOT = ALL '+' OR                              
102900           MID-IDLOPNRM(12) NOT = ALL '+' OR                              
103000           MID-IDLOPNRM(13) NOT = ALL '+' OR                              
103100           MID-IDLOPNRM(14) NOT = ALL '+'                                 
103200           PERFORM GA-KOLLA-IDLOPNRM                                      
103300        END-IF                                                            
103400     END-IF                                                               
103500                                                                          
103600     IF INDATA-FEL                                                        
103700        PERFORM MFS-ROER-EJ-FAELT-UT                                      
103800     END-IF                                                               
103900     PERFORM MFS-RENSA-FAELT-IN                                           
104000     .                                                                    
104100     EJECT                                                                
104200*----------------------------------------------------------------*        
104300 GA-KOLLA-IDLOPNRM SECTION.                                               
104400                                                                          
104500     MOVE JA                 TO INDATA-SW                                 
104600     MOVE NEJ                TO RADERFEL-SW                               
104700     MOVE ZERO TO IX                                                      
104800     ADD  1    TO IX                                                      
104900                                                                          
105000     PERFORM UNTIL IX > MAX-IX                                            
105100        MOVE NEJ                TO RADFEL-SW                              
105200        INSPECT MID-IDLOPNRM(IX) REPLACING LEADING SPACE BY ZERO          
105300                                                                          
105400        IF MID-IDLOPNRM(IX) NOT = ALL '+' AND                             
105500           MID-IDLOPNRM(IX) > ZERO                                        
105600                                                                          
105700*----------B-INDEX ANVÄNDS VID LÄSNING/KOLL                               
105800           PERFORM GAA-LAES-D111-D121                                     
105900           IF RADFEL-NEJ                                                  
106000              IF (RAD-KDINLSTA = 'FPK' OR 'SAK' OR SPACE)                 
106010                                                                          
106020                 MOVE MID-IDLOPNRM(IX) TO WS-IDLOPNRM-NUM                 
106030                 MOVE MID-IDLOPNRM(IX) TO W-IDLOPNRM-BSEQ                 
106040                 PERFORM S50-PRIM-CONTROL                                 
106050                                                                          
106200              ELSE                                                        
106300*-FEL - 007                                                               
106400                 MOVE JA                     TO RADFEL-SW                 
106500                 MOVE JA                     TO RADERFEL-SW               
106600                 MOVE ERR-UPDATE-NOT-VALID   TO MED-IDMFSFEL              
106700                 PERFORM S01-CALL-WMEDKONV-FEL                            
106800                 MOVE MFS-ALFA-FAELT-FEL                                  
106900                      TO MOD-IDLOPNRM-ATTR(IX)                            
107000              END-IF                                                      
107100           END-IF                                                         
107300        ELSE                                                              
107400           IF MID-IDLOPNRM(IX) NOT = ALL '+' AND                          
107500              MID-IDLOPNRM(IX) NOT NUMERIC                                
107600*-FEL - 001                                                               
107700              MOVE JA                        TO RADFEL-SW                 
107800              MOVE JA                        TO RADERFEL-SW               
107900              MOVE ERR-CORR-HILITE-FLDS      TO MED-IDMFSFEL              
108000              PERFORM S01-CALL-WMEDKONV-FEL                               
108100              MOVE MFS-ALFA-FAELT-FEL                                     
108200                   TO MOD-IDLOPNRM-ATTR(IX)                               
108300           END-IF                                                         
108400        END-IF                                                            
108500        IF RADFEL-NEJ                                                     
108600           MOVE MFS-ALFA-FAELT-RAETT                                      
108700                   TO MOD-IDLOPNRM-ATTR(IX)                               
108800        END-IF                                                            
109200        ADD 1  TO IX                                                      
109300     END-PERFORM                                                          
109400     IF RADERFEL-JA                                                       
109500        MOVE NEJ                             TO INDATA-SW                 
109600     END-IF                                                               
109700     .                                                                    
109800     EJECT                                                                
109900*----------------------------------------------------------------*        
110000 GAA-LAES-D111-D121 SECTION.                                              
110100                                                                          
110200*----LÄSNING FÖR ATT KOLLA RAD SKER ALLTID VIA RADENS                     
110300*    IDLOPNR + RADNR = 1 SOM ÄR UNIKT  (INDEX B ANVÄNDS)                  
110400*    GÄLLER OAVSETT NKLTYP                                                
110500                                                                          
110600     MOVE MID-IDLOPNRM(IX)     TO W1-IDLOPNRM                             
110700     MOVE 1                    TO W-IDRADNR                               
110800                                                                          
110900     PERFORM IMS-GU-INLB-D111                                             
111000                                                                          
111100     IF SEGMENT-SAKNAS                                                    
111200*-FEL - 010                                                               
111300        MOVE JA                    TO RADFEL-SW                           
111400        MOVE JA                    TO RADERFEL-SW                         
111500        MOVE ERR-SAKN-I-REG        TO MED-IDMFSFEL                        
111600        PERFORM S01-CALL-WMEDKONV-FEL                                     
111700        MOVE MFS-ALFA-FAELT-FEL                                           
111800                      TO MOD-IDLOPNRM-ATTR(IX)                            
111900     ELSE                                                                 
112000        MOVE ART-PRARTSTD          TO SPAR-PRARTSTD(IX)                   
112100        PERFORM IMS-GU-INLB-D121                                          
112200        IF SEGMENT-SAKNAS                                                 
112300*-FEL - 178                                                               
112400           MOVE JA                      TO RADFEL-SW                      
112500           MOVE JA                      TO RADERFEL-SW                    
112600           MOVE ERR-RADNR-1-SAKN        TO MED-IDMFSFEL                   
112700           PERFORM S01-CALL-WMEDKONV-FEL                                  
112800           MOVE MFS-ALFA-FAELT-FEL                                        
112900                      TO MOD-IDLOPNRM-ATTR(IX)                            
113000        END-IF                                                            
113100     END-IF                                                               
113200     .                                                                    
113300     EJECT                                                                
113400******************************************************************        
113500*  UPPDATERA REGISTER                                            *        
113600******************************************************************        
113700*----------------------------------------------------------------*        
113800 H-UPPDATERA SECTION.                                                     
113900                                                                          
114000     MOVE ZERO           TO T91-MID-KVPOST                                
114100     MOVE NEJ            TO TRANS91-SW                                    
114200     MOVE +1             TO IX                                            
114300                                                                          
114400     PERFORM UNTIL IX > MAX-IX                                            
114500        IF MID-IDLOPNRM(IX) NOT = ALL '+' AND                             
114600           MID-IDLOPNRM(IX) > ZERO                                        
114700                                                                          
114800           PERFORM IMS-GU-INLB-D111                                       
114900                                                                          
115000           MOVE ART-IDLOPNRM            TO WS-IDLOPNRM                    
115100           MOVE ART-BEFT                TO WS-BEFT                        
115200           MOVE ART-IDARTNR             TO WS-IDARTNR                     
115300           IF ART-KVAVIS-PRIO > ZERO                                      
115400             MOVE JA TO PRIOGODS-SW                                       
115500           END-IF                                                         
115600                                                                          
115700*----------LÄS SEGMENT FÖR UPPDATERING                                    
115800           PERFORM HB-LAES-GHU-D121                                       
115900                                                                          
116000           IF SEGMENT-FINNS                                               
116100              PERFORM HA-UPD-RAD                                          
116200           END-IF                                                         
116300        END-IF                                                            
116400        PERFORM MFS-FORM-ATTR                                             
116500        ADD 1  TO IX                                                      
116600     END-PERFORM                                                          
116700                                                                          
116800     IF TRANS91-JA                                                        
116900*-------SKICKA TRANS TILL MPP                                             
117000        PERFORM S50-SKICKA-W60191                                         
117100     END-IF                                                               
117200                                                                          
117300     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
117400     PERFORM S03-CALL-WMEDKONV-INF                                        
117500     PERFORM MFS-RENSA-FAELT-UT                                           
117600     .                                                                    
117700     EJECT                                                                
117800*----------------------------------------------------------------*        
117900 HA-UPD-RAD     SECTION.                                                  
118000                                                                          
118100     IF RAD-KDINLSTA = 'SAK'                                              
118200        MOVE RAD-KDINLSTA             TO SPAR-KDINLSTA-OLD                
118300        MOVE SPACE                    TO RAD-KDINLSTA                     
118400        MOVE SPACE                    TO SPAR-KDINLSTA-NEW                
118500     ELSE                                                                 
118600        MOVE RAD-KDINLSTA             TO SPAR-KDINLSTA-OLD                
118700                                         SPAR-KDINLSTA-NEW                
118800     END-IF                                                               
118900                                                                          
119000     MOVE MID-IDLOPNRM(IX)            TO MOD-IDLOPNRM(IX)                 
119100     MOVE MID-SPAR-IDINLVGN           TO RAD-IDINLVGN                     
119200     MOVE RAD-ADINLOMR                TO SPAR-ADINLOMR-OLD                
119300     MOVE MID-SPAR-ADINLOMR           TO RAD-ADINLOMR                     
119400     MOVE RAD-ADINLOMR-NXT            TO SPAR-ADINLOMR-NXT-OLD            
119500     MOVE MID-SPAR-ADINLOMR-NXT       TO RAD-ADINLOMR-NXT                 
119600                                                                          
119700     MOVE NEJ                         TO RAD-FLINLFB                      
119800                                         RAD-FLINLFP                      
119900                                                                          
120000     PERFORM IMS-REPL-INLB                                                
120100                                                                          
120200*----INITIERA TILL TRANS BAKGRUNDS-MPP ' W60191'                          
120300     PERFORM S40-TRANS-W60191                                             
120400     .                                                                    
120500     EJECT                                                                
120600*----------------------------------------------------------------*        
120700 HB-LAES-GHU-D121 SECTION.                                                
120800                                                                          
120900*-------LÄSNING FÖR ATT KOLLA RAD SKER ALLTID VIA RADENS                  
121000*       IDLOPNR + RADNR SOM ÄR UNIKT  (INDEX B ANVÄNDS)                   
121100*       GÄLLER OAVSETT NKLTYP                                             
121200                                                                          
121300        MOVE MID-IDLOPNRM(IX)  TO W1-IDLOPNRM                             
121400        MOVE +1                   TO W-IDRADNR                            
121500                                                                          
121600        PERFORM IMS-GHU-INLB-D121                                         
121700     .                                                                    
121800     EJECT                                                                
121900                                                                          
122000******************************************************************        
122100*    MFS-REDIGERING AV BILDENS FÄLT                              *        
122200******************************************************************        
122300*----------------------------------------------------------------*        
122400 MFS-RENSA-FAELT-UT SECTION.                                              
122500                                                                          
122600*    --- ALLA UTDATA-FÄLT                                                 
122700*--- RENSA INDEXERADE RADER                                               
122800                                                                          
122900     MOVE +1 TO IX                                                        
123000     PERFORM UNTIL IX > MAX-IX                                            
123100        PERFORM MFS-RENSA-RAD-FAELT-UT                                    
123200        ADD +1 TO IX                                                      
123300     END-PERFORM                                                          
123400     MOVE ZERO               TO IX                                        
123500     ADD  1                  TO IX                                        
123600     .                                                                    
123700     EJECT                                                                
123800*----------------------------------------------------------------*        
123900 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
124000                                                                          
124100*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
124200                                                                          
124300     MOVE MFS-RENSA-FAELT    TO MOD-IDLOPNRM(IX)                          
124400                                MOD-INFO(IX)                              
124500     .                                                                    
124600     EJECT                                                                
124700*----------------------------------------------------------------*        
124800 MFS-RENSA-FAELT-IN SECTION.                                              
124900                                                                          
125000*    --- ALLA INDATA-FÄLT                                                 
125100     MOVE MFS-RENSA-FAELT TO MOD-IDINLVGN-IN                              
125200                             MOD-ADINLOMR-IN                              
125300                             MOD-ADINLOMR-NXT-IN                          
125400     .                                                                    
125500     EJECT                                                                
125600*----------------------------------------------------------------*        
125700 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
125800                                                                          
125900*    --- ALLA UTDATA-FÄLT                                                 
126000                                                                          
126100*--- RENSA INDEXERADE RADER                                               
126200                                                                          
126300     MOVE +1 TO IX                                                        
126400     PERFORM UNTIL IX > MAX-IX                                            
126500        PERFORM MFS-ROER-EJ-RAD-FAELT-UT                                  
126600        ADD +1 TO IX                                                      
126700     END-PERFORM                                                          
126800     .                                                                    
126900     EJECT                                                                
127000*----------------------------------------------------------------*        
127100 MFS-ROER-EJ-RAD-FAELT-UT  SECTION.                                       
127200                                                                          
127300*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
127400                                                                          
127500     MOVE MFS-ROER-EJ-FAELT   TO MOD-IDLOPNRM(IX)                         
127600     .                                                                    
127700     EJECT                                                                
127800*----------------------------------------------------------------*        
127900 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
128000                                                                          
128100*    --- ALLA INDATA-FÄLT                                                 
128200                                                                          
128300     MOVE MFS-ROER-EJ-FAELT  TO MOD-IDINLVGN-IN                           
128400                                MOD-ADINLOMR-IN                           
128500                                MOD-ADINLOMR-NXT-IN                       
128600     .                                                                    
128700     EJECT                                                                
128800*----------------------------------------------------------------*        
128900 MFS-FORM-ATTR SECTION.                                                   
129000                                                                          
129100*    --- ALLA INDATA-FÄLT                                                 
129200*    --- OBS --- OCCURS 11 PÅ FÄLTEN                                      
129300                                                                          
129400     MOVE MFS-FORMATETS-ATTR    TO MOD-IDLOPNRM-ATTR(IX)                  
129500     .                                                                    
129600     EJECT                                                                
129700*----------------------------------------------------------------*        
129800 MFS-LAES-IN-IGEN SECTION.                                                
129900                                                                          
130000*--- INDEXERADE RADER                                                     
130100                                                                          
130200     MOVE +1 TO IX                                                        
130300     PERFORM UNTIL IX > MAX-IX                                            
130400                                                                          
130500        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDLOPNRM-ATTR(IX)               
130600        ADD +1 TO IX                                                      
130700     END-PERFORM                                                          
130800                                                                          
130900     .                                                                    
131000     EJECT                                                                
131100******************************************************************        
131200*  INFO/FELMEDDELANDE                                            *        
131300******************************************************************        
131400*----------------------------------------------------------------*        
131500 S01-CALL-WMEDKONV-FEL SECTION.                                           
131600                                                                          
131700     CALL WMEDKONV USING MED-WMEDAREA                                     
131800     IF RADFEL-JA                                                         
131900        MOVE MED-MFSFEL TO MOD-INFO(IX)                                   
132000     END-IF                                                               
132100     .                                                                    
132200     EJECT                                                                
132300*----------------------------------------------------------------*        
132400 S02-CALL-WMEDKONV-FEL SECTION.                                           
132500                                                                          
132600     CALL WMEDKONV USING MED-WMEDAREA                                     
132700     MOVE MED-MFSFEL TO MOD-TEMFSFEL                                      
132800     .                                                                    
132900     EJECT                                                                
133000 S03-CALL-WMEDKONV-INF SECTION.                                           
133100                                                                          
133200     CALL WMEDKONV USING MED-WMEDAREA                                     
133300     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
133400     .                                                                    
133500     EJECT                                                                
133600*----------------------------------------------------------------*        
133700 S40-TRANS-W60191      SECTION.                                           
133800                                                                          
133900     IF T91-MID-KVPOST = ZERO                                             
134000        MOVE JA                  TO TRANS91-SW                            
134100        MOVE SPACE               TO T91-MID-W6I19101                      
134200        MOVE +1                  TO T91-MID-KVPOST                        
134300                                                                          
134400        MOVE IDPGM               TO T91-MID-IDPGM                         
134500        MOVE DCS-IDDC            TO T91-MID-IDDC                          
134600                                                                          
134700        MOVE MID-IDLOPNRM(IX)      TO T91-MID-IDLOPNRM(1)                 
134800        INSPECT T91-MID-IDLOPNRM(1)                                       
134900                REPLACING LEADING SPACE BY ZERO                           
135000        MOVE SPAR-PRARTSTD(IX)     TO T91-MID-PRARTSTD(1)                 
135100        MOVE RAD-IDRADNR           TO T91-MID-IDRADNR(1)                  
135200        MOVE RAD-KDINLPRIO         TO T91-MID-KDINLPRIO(1)                
135300        MOVE +0                    TO T91-MID-KVKOLLI  (1)                
135400        MOVE 'N'                   TO T91-MID-FLINLI   (1)                
135500        MOVE SPAR-ADINLOMR-OLD     TO T91-MID-ADINLOMR-OLD(1)             
135600        MOVE RAD-ADINLOMR          TO T91-MID-ADINLOMR-NEW(1)             
135700        MOVE SPAR-ADINLOMR-NXT-OLD TO T91-MID-ADINLOMR-NXT-OLD(1)         
135800        MOVE RAD-ADINLOMR-NXT      TO T91-MID-ADINLOMR-NXT-NEW(1)         
135900        MOVE RAD-KVINLART          TO T91-MID-KVINLART-OLD(1)             
136000        MOVE RAD-KVINLART          TO T91-MID-KVINLART-NEW(1)             
136100        MOVE SPAR-KDINLSTA-OLD     TO T91-MID-KDINLSTA-OLD(1)             
136200        MOVE SPAR-KDINLSTA-NEW     TO T91-MID-KDINLSTA-NEW(1)             
136300     ELSE                                                                 
136400        COMPUTE IX1 = T91-MID-KVPOST + 1                                  
136500        ADD 1                         TO T91-MID-KVPOST                   
136600                                                                          
136700        MOVE MID-IDLOPNRM(IX)      TO T91-MID-IDLOPNRM(IX1)               
136800        INSPECT T91-MID-IDLOPNRM(IX1)                                     
136900                REPLACING LEADING SPACE BY ZERO                           
137000        MOVE SPAR-PRARTSTD(IX)     TO T91-MID-PRARTSTD(IX1)               
137100        MOVE RAD-IDRADNR           TO T91-MID-IDRADNR(IX1)                
137200        MOVE RAD-KDINLPRIO         TO T91-MID-KDINLPRIO(IX1)              
137300        MOVE +0                    TO T91-MID-KVKOLLI  (IX1)              
137400        MOVE 'N'                   TO T91-MID-FLINLI   (IX1)              
137500        MOVE SPAR-ADINLOMR-OLD     TO T91-MID-ADINLOMR-OLD(IX1)           
137600        MOVE RAD-ADINLOMR          TO T91-MID-ADINLOMR-NEW(IX1)           
137700        MOVE SPAR-ADINLOMR-NXT-OLD                                        
137800             TO T91-MID-ADINLOMR-NXT-OLD(IX1)                             
137900        MOVE RAD-ADINLOMR-NXT                                             
138000             TO T91-MID-ADINLOMR-NXT-NEW(IX1)                             
138100        MOVE RAD-KVINLART          TO T91-MID-KVINLART-OLD(IX1)           
138200        MOVE RAD-KVINLART          TO T91-MID-KVINLART-NEW(IX1)           
138300        MOVE SPAR-KDINLSTA-OLD     TO T91-MID-KDINLSTA-OLD(IX1)           
138400        MOVE SPAR-KDINLSTA-NEW     TO T91-MID-KDINLSTA-NEW(IX1)           
138500     END-IF                                                               
138600     .                                                                    
138700     EJECT                                                                
138800*----------------------------------------------------------------*        
138900 S50-SKICKA-W60191 SECTION.                                               
139000                                                                          
139100     COMPUTE W-PTOP1-OCC-LL = T91-MID-KVPOST * 64                         
139200     COMPUTE PTOP1-LL = W-PTOP1-OCC-LL + 35                               
139300     MOVE MFS-KDMFSFOR       TO PTOP1-KDMFSFOR                            
139400                                                                          
139500     PERFORM IMS-ISRT-MSG-ALT1-6191                                       
139600                                                                          
139700     .                                                                    
139800     EJECT                                                                
139900                                                                          
140000 S50-PRIM-CONTROL SECTION.                                                
140100* THIS IS A CONTROL TO CHECK THE OLD PLACE IF FLAG FLKNTRGK = YES         
140200* IF THE FLAG IS YES WE HAVE TO DO A CHECK ON THE OLD PLACE BEFORE        
140300* WE CHECK THE NEW PLACE.                                                 
140400     MOVE RAD-ADINLOMR         TO W-ADINLOMR                              
140500     PERFORM IMS-GU-PLAA-G111                                             
140600     IF SEGMENT-FINNS                                                     
140700       IF 6006-FLKNTRGK = JA                                              
140800         IF MID-ADINLOMR-IN NOT = ALL '+'                                 
140900           MOVE MID-ADINLOMR-IN TO W-ADINLOMR                             
141000         ELSE                                                             
141100           MOVE MID-ADINLOMR-UT TO W-ADINLOMR                             
141200         END-IF                                                           
141300         PERFORM IMS-GU-PLAA-G111                                         
141400         IF (6006-FLKNTRGK = JA )                                         
141500         OR 6006-KDINLOMR = 'LPL'                                         
141600           MOVE 'N' TO PRIM-CONTROL-SW                                    
141700         ELSE                                                             
141800           MOVE 'J' TO PRIM-CONTROL-SW                                    
141900         END-IF                                                           
142000       ELSE                                                               
142100         MOVE 'N' TO PRIM-CONTROL-SW                                      
142200       END-IF                                                             
142300     ELSE                                                                 
142400       MOVE 'N' TO PRIM-CONTROL-SW                                        
142500     END-IF                                                               
142600     IF PRIM-CONTROL-YES                                                  
142700       PERFORM S51-CHECK-OLD-PLACE                                        
142800     END-IF                                                               
142900     .                                                                    
143000     EJECT                                                                
143100                                                                          
143200 S51-CHECK-OLD-PLACE     SECTION.                                         
143300     MOVE WS-IDLOPNRM-NUM  TO W-IDLOPNRM-BSEQ                             
143400     PERFORM IMS-GU-UPFA01                                                
143500     IF SEGMENT-FINNS                                                     
143600       IF UPPF-KVKVAPRIM > ZERO                                           
143700         IF UPPF-KDKVASTA-PRI = '2' OR '3'                                
143800           CONTINUE                                                       
143900         ELSE                                                             
144000           MOVE JA                        TO RADFEL-SW                    
144100           MOVE JA                        TO RADERFEL-SW                  
144200           MOVE '605' TO MED-IDMFSFEL                                     
144300           PERFORM S01-CALL-WMEDKONV-FEL                                  
144400           MOVE MFS-ALFA-FAELT-FEL                                        
144500                   TO MOD-IDLOPNRM-ATTR(IX)                               
144600         END-IF                                                           
144700       END-IF                                                             
144800       IF UPPF-KVKVASEK > ZERO                                            
144900         IF UPPF-KDKVASTA-PRI = '2' OR '3'                                
145000           CONTINUE                                                       
145100         ELSE                                                             
145200           MOVE JA                        TO RADFEL-SW                    
145300           MOVE JA                        TO RADERFEL-SW                  
145400           MOVE '605' TO MED-IDMFSFEL                                     
145500           PERFORM S01-CALL-WMEDKONV-FEL                                  
145600           MOVE MFS-ALFA-FAELT-FEL                                        
145700                   TO MOD-IDLOPNRM-ATTR(IX)                               
145800         END-IF                                                           
145900       END-IF                                                             
146000     END-IF                                                               
146100                                                                          
146200     IF INDATA-OK                                                         
146300       PERFORM IMS-GU-UPFA01                                              
146400       IF SEGMENT-FINNS                                                   
146500         PERFORM IMS-GNP-UPFA11                                           
146600         PERFORM UNTIL SEGMENT-SAKNAS                                     
146700           IF RAPP-KDKVASTA-PRI = '2' OR '3'                              
146800             CONTINUE                                                     
146900           ELSE                                                           
147000             MOVE JA                        TO RADFEL-SW                  
147100             MOVE JA                        TO RADERFEL-SW                
147200             MOVE '605' TO MED-IDMFSFEL                                   
147300             PERFORM S01-CALL-WMEDKONV-FEL                                
147400             MOVE MFS-ALFA-FAELT-FEL                                      
147500                   TO MOD-IDLOPNRM-ATTR(IX)                               
147600           END-IF                                                         
147700           PERFORM IMS-GNP-UPFA11                                         
147800         END-PERFORM                                                      
147900       END-IF                                                             
148000     END-IF                                                               
148100                                                                          
148200     IF INDATA-OK                                                         
148300       PERFORM IMS-GU-UPFA01                                              
148400       IF SEGMENT-FINNS                                                   
148500         PERFORM IMS-GNP-UPFA12                                           
148600         PERFORM UNTIL SEGMENT-SAKNAS                                     
148700           IF SPEC-KDKVASTA-PRI = '2' OR '3'                              
148800             CONTINUE                                                     
148900           ELSE                                                           
149000             MOVE JA                        TO RADFEL-SW                  
149100             MOVE JA                        TO RADERFEL-SW                
149200             MOVE '605' TO MED-IDMFSFEL                                   
149300             PERFORM S01-CALL-WMEDKONV-FEL                                
149400             MOVE MFS-ALFA-FAELT-FEL                                      
149500                   TO MOD-IDLOPNRM-ATTR(IX)                               
149600           END-IF                                                         
149700           PERFORM IMS-GNP-UPFA12                                         
149800         END-PERFORM                                                      
149900       END-IF                                                             
150000     END-IF                                                               
150100     .                                                                    
150200     EJECT                                                                
150300                                                                          
150400******************************************************************        
150500*  BILD-REDIGERING                                               *        
150600******************************************************************        
150700*----------------------------------------------------------------*        
150800 S90-BLANKUTF-NUM-FAELT SECTION.                                          
150900                                                                          
151000*----NKL-FÄLT                                                             
151100     INSPECT MOD-IDINLVGN-UT REPLACING LEADING ZERO BY SPACE              
151200                                                                          
151300*----RAD-FÄLT                                                             
151400*--- INDEXERADE RADER                                                     
151500                                                                          
151600     MOVE +1 TO IX                                                        
151700     PERFORM UNTIL IX > MAX-IX                                            
151800        INSPECT MOD-IDLOPNRM(IX) REPLACING LEADING ZERO BY SPACE          
151900        IF NYCKLAR-OK AND INDATA-OK AND (GODK-MID OR HELP-MID)            
152000           MOVE MFS-OEPPNA-NUM-FAELT TO MOD-IDLOPNRM-ATTR(IX)             
152100        ELSE                                                              
152200           IF INDATA-FEL                                                  
152300              CONTINUE                                                    
152400           ELSE                                                           
152500              MOVE MFS-STAENG-FAELT  TO MOD-IDLOPNRM-ATTR(IX)             
152600           END-IF                                                         
152700        END-IF                                                            
152800        ADD +1 TO IX                                                      
152900     END-PERFORM                                                          
153000     .                                                                    
153100     EJECT                                                                
153200******************************************************************        
153300*  IMS-SECTIONER                                                 *        
153400******************************************************************        
153500     SKIP3                                                                
153600*----------------------------------------------------------------*        
153700 IMS-GET-MSG SECTION.                                                     
153800                                                                          
153900     MOVE '  QC' TO GODK-STATUSKODER                                      
154000     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
154100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
154200     PERFORM IMS-STATUSKONTROLL                                           
154300     .                                                                    
154400     SKIP3                                                                
154500*----------------------------------------------------------------*        
154600 IMS-INSERT-MSG SECTION.                                                  
154700                                                                          
154800     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
154900       MOVE '0' TO MFS-KDHUVOMR                                           
155000     END-IF                                                               
155100     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
155200     MOVE SPACE TO GODK-STATUSKODER                                       
155300     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
155400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
155500     PERFORM IMS-STATUSKONTROLL                                           
155600     .                                                                    
155700     EJECT                                                                
155800******************************************************************        
155900*    ALT1-PCB  (TRANS W60191)                                    *        
156000******************************************************************        
156100*----------------------------------------------------------------*        
156200 IMS-ISRT-MSG-ALT1-6191 SECTION.                                          
156300                                                                          
156400     MOVE SPACE              TO GODK-STATUSKODER                          
156500     CALL CBLTDLI USING      ISRT ALT1-PCB                                
156600                                  P-TO-P-T91                              
156700     MOVE ALT1-STATUS-CODE   TO STATUS-WS                                 
156800     PERFORM IMS-STATUSKONTROLL                                           
156900     .                                                                    
157000     EJECT                                                                
157100******************************************************************        
157200*    INLA1-PCB                                                   *        
157300******************************************************************        
157400     SKIP3                                                                
157500*----------------------------------------------------------------*        
157600 IMS-GU-INLA1-D111  SECTION.                                              
157700     STRING 'W6INLA11(W6D1FSEQ =' W-IDINLVGN-X                            
157800                    '&IDDC     =' W-IDDC-X ')'                            
157900             DELIMITED BY SIZE INTO SSA1                                  
158000     MOVE '  GE'                 TO GODK-STATUSKODER                      
158100     CALL CBLTDLI USING GU       INLA1-PCB                                
158200                                 DLI-IO-AREA                              
158300                                 SSA1                                     
158400     MOVE INLA1-STATUS-CODE      TO STATUS-WS                             
158500     PERFORM IMS-STATUSKONTROLL                                           
158600     .                                                                    
158700     EJECT                                                                
158800 IMS-GNP-INLA1-D121  SECTION.                                             
158900     STRING 'W6INLA21(IDINLVGN =' W-IDINLVGN-X ')'                        
159000             DELIMITED BY SIZE INTO SSA1                                  
159100     MOVE '    '                 TO GODK-STATUSKODER                      
159200     CALL CBLTDLI USING GNP      INLA1-PCB                                
159300                                 DLI-IO-AREA-W6D121                       
159400                                 SSA1                                     
159500     MOVE INLA1-STATUS-CODE      TO STATUS-WS                             
159600     PERFORM IMS-STATUSKONTROLL                                           
159700     .                                                                    
159800     EJECT                                                                
159900******************************************************************        
160000*    INLB-PCB                                                    *        
160100******************************************************************        
160200     SKIP3                                                                
160300*----------------------------------------------------------------*        
160400 IMS-GU-INLB-D111  SECTION.                                               
160500     STRING 'W6INLA11(W6D1BSEQ =' W1-IDLOPNRM-X                           
160600                    '&IDDC     =' W-IDDC ')'                              
160700             DELIMITED BY SIZE INTO SSA1                                  
160800     MOVE '  GE'                 TO GODK-STATUSKODER                      
160900     CALL CBLTDLI USING GU       INLB-PCB                                 
161000                                 DLI-IO-AREA                              
161100                                 SSA1                                     
161200     MOVE INLB-STATUS-CODE       TO STATUS-WS                             
161300     PERFORM IMS-STATUSKONTROLL                                           
161400     .                                                                    
161500     EJECT                                                                
161600*----------------------------------------------------------------*        
161700 IMS-GU-INLB-D121    SECTION.                                             
161800     STRING 'W6INLA11(W6D1BSEQ =' W1-IDLOPNRM-X                           
161900                    '&IDDC     =' W-IDDC ')'                              
162000          DELIMITED BY SIZE INTO SSA1                                     
162100     STRING 'W6INLA21(IDRADNR  =' W-IDRADNR-X ')'                         
162200          DELIMITED BY SIZE INTO SSA2                                     
162300     MOVE '  GE'            TO GODK-STATUSKODER                           
162400     CALL CBLTDLI USING GU  INLB-PCB                                      
162500                            DLI-IO-AREA-W6D121                            
162600                            SSA1                                          
162700                            SSA2                                          
162800     MOVE INLB-STATUS-CODE  TO STATUS-WS                                  
162900     PERFORM IMS-STATUSKONTROLL                                           
163000     .                                                                    
163100     EJECT                                                                
163200******************************************************************        
163300*    IMS-UPPDATERING VIA INLB-PCB                                *        
163400******************************************************************        
163500*----------------------------------------------------------------*        
163600 IMS-GHU-INLB-D121        SECTION.                                        
163700     STRING 'W6INLA11(W6D1BSEQ =' W1-IDLOPNRM-X                           
163800                    '&IDDC     =' W-IDDC ')'                              
163900          DELIMITED BY SIZE INTO SSA1                                     
164000     STRING 'W6INLA21(IDRADNR  =' W-IDRADNR-X ')'                         
164100          DELIMITED BY SIZE INTO SSA2                                     
164200     MOVE '  GE'            TO GODK-STATUSKODER                           
164300     CALL CBLTDLI USING GHU INLB-PCB                                      
164400                            DLI-IO-AREA-W6D121                            
164500                            SSA1                                          
164600                            SSA2                                          
164700     MOVE INLB-STATUS-CODE  TO STATUS-WS                                  
164800     PERFORM IMS-STATUSKONTROLL                                           
164900     .                                                                    
165000     EJECT                                                                
165100*----------------------------------------------------------------*        
165200 IMS-REPL-INLB SECTION.                                                   
165300                                                                          
165400     MOVE '  '               TO GODK-STATUSKODER                          
165500     CALL CBLTDLI USING REPL INLB-PCB                                     
165600                             DLI-IO-AREA-W6D121                           
165700     MOVE INLB-STATUS-CODE   TO STATUS-WS                                 
165800     PERFORM IMS-STATUSKONTROLL                                           
165900     .                                                                    
166000     EJECT                                                                
166100******************************************************************        
166200*    PLAA-PCB                                                    *        
166300******************************************************************        
166400     SKIP3                                                                
166500*----------------------------------------------------------------*        
166600 IMS-GU-PLAA-G111 SECTION.                                                
166700     STRING 'W6PLAA01(W6GXKEY  =' W-W6GX01KEY-X ')'                       
166800          DELIMITED BY SIZE INTO SSA1                                     
166900     STRING 'W6PLAA11(W6GXKEY  =' W-W6GX11KEY-X ')'                       
167000          DELIMITED BY SIZE INTO SSA2                                     
167100     MOVE '  GE'            TO GODK-STATUSKODER                           
167200     CALL CBLTDLI USING GU  PLAA-PCB                                      
167300                            DLI-IO-AREA                                   
167400                            SSA1                                          
167500                            SSA2                                          
167600     MOVE PLAA-STATUS-CODE  TO STATUS-WS                                  
167700     PERFORM IMS-STATUSKONTROLL                                           
167800     .                                                                    
167900     SKIP3                                                                
168000                                                                          
168100 IMS-GU-UPFA01 SECTION.                                                   
168200     STRING 'W6UPFA01(IDLOPNRM =' W-IDLOPNRM-X ')'                        
168300          DELIMITED BY SIZE INTO SSA1                                     
168400     MOVE '  GE' TO GODK-STATUSKODER                                      
168500     CALL CBLTDLI USING GU UPFA-PCB DLI-IO-AREA-UPFA01 SSA1               
168600     MOVE UPFA-STATUS-CODE TO STATUS-WS                                   
168700     PERFORM IMS-STATUSKONTROLL                                           
168800     .                                                                    
168900     SKIP3                                                                
169000                                                                          
169100 IMS-GNP-UPFA11 SECTION.                                                  
169200     STRING 'W6UPFA01(IDLOPNRM =' W-IDLOPNRM-X ')'                        
169300          DELIMITED BY SIZE INTO SSA1                                     
169400     MOVE 'W6UPFA11 ' TO SSA2                                             
169500     MOVE '  GE' TO GODK-STATUSKODER                                      
169600     CALL CBLTDLI USING GNP UPFA-PCB DLI-IO-AREA-UPFA11 SSA1 SSA2         
169700     MOVE UPFA-STATUS-CODE TO STATUS-WS                                   
169800     PERFORM IMS-STATUSKONTROLL                                           
169900     .                                                                    
170000     SKIP3                                                                
170100                                                                          
170200 IMS-GNP-UPFA12 SECTION.                                                  
170300     STRING 'W6UPFA01(IDLOPNRM =' W-IDLOPNRM-X ')'                        
170400          DELIMITED BY SIZE INTO SSA1                                     
170500     MOVE 'W6UPFA12 ' TO SSA2                                             
170600     MOVE '  GE' TO GODK-STATUSKODER                                      
170700     CALL CBLTDLI USING GNP UPFA-PCB DLI-IO-AREA-UPFA12 SSA1 SSA2         
170800     MOVE UPFA-STATUS-CODE TO STATUS-WS                                   
170900     PERFORM IMS-STATUSKONTROLL                                           
171000     .                                                                    
171100     SKIP3                                                                
171200 IMS-GU-WDB601    SECTION.                                                
171300     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
171400          DELIMITED BY SIZE INTO SSA1                                     
171500     MOVE '  GE' TO GODK-STATUSKODER                                      
171600     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
171700     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
171800     PERFORM IMS-STATUSKONTROLL                                           
171900     IF SEGMENT-SAKNAS                                                    
172000         MOVE SPACE TO DCS-KDDC                                           
172100     END-IF                                                               
172200     .                                                                    
172300*----------------------------------------------------------------*        
172400 IMS-STATUSKONTROLL SECTION.                                              
172500                                                                          
172600     SET STATUS-IX TO 1                                                   
172700     SEARCH GODK-STATUS                                                   
172800       AT END                                                             
172900         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
173000         DELIMITED BY SIZE INTO FELTEXT                                   
173100         CALL FELLOG                                                      
173200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
173300         CONTINUE                                                         
173400     END-SEARCH                                                           
173500     .                                                                    
173600     EJECT                                                                
