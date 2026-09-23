000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W6010200.                                                
000400*AUTHOR.         ROS-MARIE CLASON - GUIDE DATAKONSULT AB.                 
000500*DATE-WRITTEN.   92/05/08.                                                
000600*                                                                         
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        ALLMÄN BESKRIVNING:                                              
001100*        PROGRAMMET ÄR EN MPP SOM FLYTTAR KOLLI PÅ                        
001200*        EN VAGN ELLER PLACERING.                                         
001300*        BILDEN ÄR EN REGISTRERINGSBILD.                                  
001400*                                                                         
001500*        PROGRAMMET UPPDATERAR W6INLA (W6D1)                              
001600*        PROGRAMMET LÄSER      W6PLAA (W6G1)                              
001700*                              WDB6                                       
001710*                                                                         
001800*    INDATA.                                                              
001900*        TRANSAKTION: W6T102                                              
002000*        MID:         W6I10201                                            
002100*        MID:         W6I10203 FRÅN HANDDATOR                             
002200*                                                                         
002300*    UTDATA.                                                              
002400*        MOD:         W6O10201                                            
002500                                                                          
002600     SKIP3                                                                
002700 ENVIRONMENT DIVISION.                                                    
002800     EJECT                                                                
002900                                                                          
003000 DATA DIVISION.                                                           
003100 WORKING-STORAGE SECTION.                                                 
003101                                                                          
003110*    -- CHECKED BY WY2000                                                 
003200 77  IDPGM                       PIC X(08)   VALUE 'W6010200'.            
003300                                                                          
003400*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003500 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003600                                                                          
003700 77  JA                          PIC X       VALUE 'J'.                   
003800 77  NEJ                         PIC X       VALUE 'N'.                   
003900                                                                          
004010 77  WS-BEFT                     PIC S9(3)   VALUE ZERO COMP-3.           
004100 77  WS-IDARTNR                  PIC S9(9)   VALUE ZERO COMP-3.           
004200 77  SPAR-ADLAGOMR               PIC S9(3)   VALUE ZERO COMP-3.           
004300 77  SPAR-ADGANG                 PIC S9(3)   VALUE ZERO COMP-3.           
004400 77  SPAR-ADPLATS                PIC S9(5)   VALUE ZERO COMP-3.           
004500 77  TORG-ADINLOMR               PIC X(4)    VALUE SPACE.                 
004600                                                                          
004700*    --- INDEX FÖR BLÄDDRINGSRADER                                        
004800 77  IX                          PIC S9(4)  VALUE +0    COMP SYNC.        
004900 77  IX1                         PIC S9(4)  VALUE +0    COMP SYNC.        
005000 77  MOD-IX                      PIC S9(4)  VALUE +0    COMP SYNC.        
005100 77  MAX-IX                      PIC S9(4)  VALUE +14   COMP SYNC.        
005200 77  T91-IX                      PIC S9(4)  VALUE +0    COMP SYNC.        
005300 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
005400 77  6197-IX                     PIC S9(9)   VALUE +0   COMP SYNC.        
005500 77  MAX-6197-IX                 PIC S9(9)   VALUE +15  COMP SYNC.        
005600                                                                          
005700*   OM SVAR TILL SKÄRM: MAX-MOD-LAENGD = (388) MOD-LÄNGD + 4              
005800 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +392 COMP SYNC.         
005900 77  P-TO-P-PREFIX-LNG           PIC S9(4)   VALUE +17  COMP SYNC.        
006000                                                                          
006100***********************************                                       
006200*        ARBETSFÄLT               *                                       
006300***********************************                                       
006400*                                                                         
006500*                                                                         
006600 01  WS-AREA.                                                             
006700*                                                                         
006800*----TILL W60191                                                          
006900     03  W-PTOP1-OCC-LL          PIC S9(4)    COMP-3 VALUE ZERO.          
007000     03  SPAR-IDLOPNRM           PIC S9(9)    COMP-3 VALUE ZERO.          
007100     03  SPAR-PRARTSTD         PIC S9(7)V9(2) COMP-3 VALUE ZERO.          
007200     03  SPAR-ADINLOMR-OLD       PIC X(4)     VALUE SPACE.                
007300     03  SPAR-ADINLOMR-NXT-OLD   PIC X(4)     VALUE SPACE.                
007400*                                                                         
007500     03  SPAR-KDINLSTA-NEW       PIC X(3)     VALUE SPACE.                
007600     03  SPAR-KDINLSTA-OLD       PIC X(3)     VALUE SPACE.                
007700*                                                                         
007800     03  SPAR-ADINLOMR           PIC X(4)     VALUE SPACE.                
007900     03  SPAR-ADINLOMR-NXT       PIC X(4)     VALUE SPACE.                
008900                                                                          
009000******************************************************************        
009100*    --- ARBETSFÄLT FÖR SWITCHAR                                          
009200                                                                          
009300 77  INDATA-SW                   PIC X       VALUE 'J'.                   
009400     88  INDATA-OK                           VALUE 'J'.                   
009500     88  INDATA-FEL                          VALUE 'N'.                   
009600                                                                          
009700 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
009800     88  NYCKLAR-OK                          VALUE 'J'.                   
009900     88  NYCKLAR-FEL                         VALUE 'N'.                   
010000                                                                          
010100 77  ALLT-SW                     PIC X       VALUE 'J'.                   
010200     88  ALLT-OK                             VALUE 'J'.                   
010300                                                                          
010400 77  UPD-SW                      PIC X       VALUE 'J'.                   
010500     88  UPD-OK                              VALUE 'J'.                   
010600     88  UPD-NEJ                             VALUE 'N'.                   
010700                                                                          
010800 77  FBRAPP-SW                   PIC X       VALUE 'J'.                   
010900     88  FBRAPP                              VALUE 'J'.                   
011000     88  FBRAPP-FINNS                        VALUE 'N'.                   
011100                                                                          
011200 77  PRIOGODS-SW                 PIC X       VALUE 'N'.                   
011300     88  PRIOGODS                            VALUE 'J'.                   
011400                                                                          
011500 77  FP-SW                       PIC X       VALUE 'N'.                   
011600     88  FP                                  VALUE 'J'.                   
011700                                                                          
011800 77  TRAEFF-SW                   PIC X       VALUE 'N'.                   
011900     88  TRAEFF                              VALUE 'J'.                   
012000                                                                          
012100 77  STATUS-SW                   PIC X       VALUE 'N'.                   
012200     88  STATUS-OK                           VALUE 'J'.                   
012300     88  STATUS-NEJ                          VALUE 'N'.                   
012400                                                                          
012500 77  TRANS91-SW                  PIC X       VALUE 'J'.                   
012600     88  TRANS91-JA                          VALUE 'J'.                   
012700     88  TRANS91-NEJ                         VALUE 'N'.                   
012800                                                                          
012900 77  FOERSTA-6197-SW             PIC X       VALUE 'J'.                   
013000     88  FOERSTA-6197                        VALUE 'J'.                   
013100                                                                          
013200 77  PURGE91-SW                  PIC X       VALUE 'N'.                   
013300     88  PURGE-T91                           VALUE 'J'.                   
013400                                                                          
013401 77  PRIM-CONTROL-SW             PIC X       VALUE 'N'.                   
013402     88  PRIM-CONTROL-YES                    VALUE 'J'.                   
013403     88  PRIM-CONTROL-NO                     VALUE 'N'.                   
013404                                                                          
013440       EJECT                                                              
013500*----------------------------------------------------------------*        
013600*   NKLTYP1=VAGN; VAGN, PLAC; VAGN, PLAC, ADR;                            
013700*   NKLTYP2=PLAC                                                          
013800*----------------------------------------------------------------*        
013900 77  NKLTYP-SW                  PIC X.                                    
014000     88  NKLTYP1                            VALUE '1'.                    
014100     88  NKLTYP2                            VALUE '2'.                    
014200                                                                          
014300*----------------------------------------------------------------*        
014400*   BEARBTYP1=  FLDIVKLI = NEJ                                            
014500*   BEARBTYP2=  FLDIVKLI = JA                                             
014600*----------------------------------------------------------------*        
014700 77  BEARBTYP-SW                  PIC X.                                  
014800     88  BEARBTYP1                          VALUE '1'.                    
014900     88  BEARBTYP2                          VALUE '2'.                    
015000                                                                          
015100                                                                          
015200 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
015300     88  EGEN-MID                            VALUE '6102'.                
015400     88  GODK-MID                            VALUE '6102' '6124'          
015500                                                   '6125'.                
015600     88  HELP-MID                            VALUE '0551'.                
015700     EJECT                                                                
015800                                                                          
015900*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
016000 01  GENERELLA-SUBPROGRAM.                                                
016100     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
016200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
016300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
016301     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
016400     EJECT                                                                
016500                                                                          
016510*   -COPY WMSGINIT                                                        
016520     EJECT                                                                
016600*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
016700*01 -COPY WMEDAREA                                                        
016800     SKIP3                                                                
016900 01  MESSAGE-CODES.                                                       
017000     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
017100     03  ERR-UPDATE-NOT-VALID    PIC X(3)    VALUE '007'.                 
017200     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
017300     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
017400     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
017500     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
017600     03  ERR-PLAC-ADR-FINNS      PIC X(3)    VALUE '196'.                 
017700                                                                          
017800     EJECT                                                                
017900                                                                          
018000*    --- AREOR FÖR BAKGRUNDS MPP:ER / BMP:ER                              
018100*    --- COPYTEXTER FÖR PRINTER,  W60191                                  
018200*                                                                         
018300*                                                                         
018400 01  P-TO-P-T91.                                                          
018500*----TILL W60191                                                          
018600     03  PTOP1-LL              PIC S9(4)   COMP SYNC.                     
018700     03  PTOP1-Z1              PIC X(1)    VALUE LOW-VALUE.               
018800     03  PTOP1-Z2              PIC X(1)    VALUE LOW-VALUE.               
018900     03  PTOP1-TRANSKOD        PIC X(7)    VALUE 'W6T191X'.               
019000     03  FILLER                PIC X(1)    VALUE SPACE.                   
019100     03  PTOP1-IDTRANS         PIC X(4)    VALUE '6102'.                  
019200     03  PTOP1-KDMFSFOR        PIC X(1)    VALUE SPACE.                   
019300*    03  MID -COPY W6I19101       -PRE T91-                               
019400     EJECT                                                                
019500                                                                          
019600 01  FILLER                      PIC X(16)   VALUE 'P-TO-P-AREA'.         
019700                                                                          
019800*01  -COPY WMSGSNUF   -PRE  P-TO-P-                                       
019900     EJECT                                                                
020000                                                                          
020100*01  -COPY W6I19701   -PRE 6197-                                          
020200     EJECT                                                                
020300                                                                          
020400*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
020500*                                                                         
020600 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
020700     SKIP3                                                                
020800*01  MID -COPY W6I10201                                                   
020900     EJECT                                                                
021000                                                                          
021100*01  MID -COPY W6I10203                                                   
021200     EJECT                                                                
021300                                                                          
021400 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
021500     SKIP3                                                                
021600*01  -COPY WMSGAREA                                                       
021700     EJECT                                                                
021800                                                                          
021900     03  MOD REDEFINES MSG-AREA.                                          
022000*      05  -COPY W6O10201                                                 
022100     EJECT                                                                
022200                                                                          
022300 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
022400     SKIP3                                                                
022500*01  -COPY WMFSAREA                                                       
022600     EJECT                                                                
022700                                                                          
022800*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
022900*                                                                         
023000     EJECT                                                                
023100                                                                          
023200*                                                                         
023300*    --- DLI- NYCKLAR TILL IMS-SEKTIONERNA                                
023400*                                                                         
023500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
023600     SKIP3                                                                
023700 01  NYCKLAR-TILL-DLI.                                                    
023800                                                                          
024400     03  W-IDRADNR-INL-X.                                                 
024500         05  W-IDRADNR-INL        PIC S9(5)   COMP-3.                     
024600                                                                          
024700     03  W-IDRADNR-X.                                                     
024800         05  W-IDRADNR            PIC S9(5)   COMP-3.                     
024900                                                                          
025000*INLC                                                                     
025100     03  W-W6D1C1KY-X.                                                    
025200         05  WC-IDLEVNR           PIC X(5)   VALUE SPACE.                 
025300         05  WC-IDOKOLLI          PIC 9(9).                               
025400                                                                          
025500*                                                                         
025600     03  W-IDLEVNRK-X.                                                    
025700         05  S-IDLEVNRK           PIC X(5)   VALUE SPACE.                 
025800*                                                                         
025900     03  W-IDOKOLLI-X.                                                    
026000         05  S-IDOKOLLI          PIC 9(9).                                
026100                                                                          
026200*INLA                                                                     
026300*--------FYSISK NKL TILL INLF (EG. INLG)                                  
026400     03  W-W6D1FSEQ-X.                                                    
026500         05  W-IDINLVGN-SEQ       PIC 9(3).                               
026600                                                                          
026620*--------SÖKFÄLT W6D121                                                   
026630     03  W-IDINLVGN-X.                                                    
026640         05  W-IDINLVGN           PIC 9(3).                               
026650                                                                          
026670*--------SÖKFÄLT W6D111                                                   
026680     03  W-IDDC-X.                                                        
026690         05  W-IDDC               PIC X(2).                               
026691                                                                          
026700*PLAA                                                                     
026800     03  W-W6GX01KEY-X.                                                   
026900         05  WGX-IDHTYP           PIC X(4)    VALUE '6005'.               
026910         05  WGX-IDDC             PIC X(2)    VALUE SPACE.                
027000         05  FILLER               PIC X(24)   VALUE LOW-VALUE.            
027100                                                                          
027200     03  W-W6GX11KEY-X.                                                   
027300         05  W-ADINLOMR           PIC X(4)    VALUE SPACE.                
027400         05  FILLER               PIC X(1)    VALUE LOW-VALUE.            
027500                                                                          
027510     03  W-W6GX11KEY-Y.                                                   
027520         05  W-ADINLOMR-KON       PIC X(4)    VALUE SPACE.                
027530         05  FILLER               PIC X(1)    VALUE LOW-VALUE.            
027540                                                                          
027600     03  W-ADINLOMR-PAR           PIC X(4)    VALUE SPACE.                
027700                                                                          
027710     03  W-IDLOPNRM-X.                                                    
027720         05  W-IDLOPNRM          PIC S9(9)   COMP-3 VALUE ZERO.           
027730                                                                          
027800*WDB6                                                                     
027810     03  W-IDDC-B6-X.                                                     
027820         05 W-IDDC-B6            PIC X(2).                                
027840                                                                          
027900*    --- STATUS-KOD FRÅN IMS                                              
028000 01  STATUS-WS                   PIC XX.                                  
028100     88  SEGMENT-FINNS                       VALUE '  '.                  
028200     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
028300     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
028400     SKIP2                                                                
028500                                                                          
028600 01  GODK-STATUSKODER.                                                    
028700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
028800     SKIP3                                                                
028900                                                                          
029000 01  SSA1                        PIC X(99).                               
029100 01  SSA2                        PIC X(64).                               
029200 01  SSA3                        PIC X(64).                               
029300     EJECT                                                                
029400                                                                          
029500*    --- IMS FUNKTIONSKODER                                               
029600*01  -COPY W0003                                                          
029700     EJECT                                                                
029800                                                                          
029900*    ---  DLI INPUT-OUTPUT AREA                                           
030000 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
030100     SKIP3                                                                
030200                                                                          
030300 01  DLI-IO-AREA.                                                         
030400     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
030500     SKIP3                                                                
030600                                                                          
030610 01  FILLER                    PIC X(16)    VALUE 'DLI-IO-W6D111'.        
030620                                                                          
030630 01  DLI-IO-AREA-W6D111.                                                  
030640     03  W6INLA11.                                                        
030650*        05  -COPY W6D111                                                 
030660     EJECT                                                                
031000                                                                          
031010 01  FILLER                    PIC X(16)    VALUE 'DLI-IO-W6D121'.        
031020                                                                          
031030 01  DLI-IO-AREA-W6D121.                                                  
031040     03  W6INLA21.                                                        
031050*        05  -COPY W6D121                                                 
031060     EJECT                                                                
031070                                                                          
031080 01  FILLER                    PIC X(16)    VALUE 'DLI-IO-W6GX01'.        
031090                                                                          
031100 01  DLI-IO-AREA-W6GX01.                                                  
031200     03  W6PLAA01.                                                        
031300*        05  -COPY W6GX01                                                 
031400     EJECT                                                                
031401                                                                          
031410 01  FILLER                  PIC X(16)    VALUE 'DLI-IO-W6GX6006'.        
031420                                                                          
031430 01  DLI-IO-AREA-W6GX6006.                                                
031440     03  W6PLAA11.                                                        
031450*        05  -COPY W6GX6006                                               
031460     EJECT                                                                
031500                                                                          
032500 01  FILLER                    PIC X(16)    VALUE 'DLI-IO-W6D1B1'.        
032600                                                                          
032700 01  DLI-IO-AREA-W6D1B1.                                                  
032800     03  W6INLC01.                                                        
032900*        05  -COPY W6D1B1                                                 
033000     EJECT                                                                
033100 01  FILLER                    PIC X(16)    VALUE 'DLI-IO-W6PLAA'.        
033200                                                                          
033300 01  DLI-IO-AREA-W6PLAA.                                                  
033400     03  W6GX6006.                                                        
033500*        05  -COPY W6GX6006  -PRE ALT-                                    
033600     EJECT                                                                
033700                                                                          
033710 01  DLI-IO-AREA-UPFA01.                                                  
033720     03  W6UPFA01.                                                        
033730*        05  -COPY W6L101                                                 
033740     SKIP3                                                                
033750 01  DLI-IO-AREA-UPFA11.                                                  
033760     03  W6UPFA11.                                                        
033770*        05  -COPY W6L111                                                 
033780     SKIP3                                                                
033790 01  DLI-IO-AREA-UPFA12.                                                  
033791     03  W6UPFA12.                                                        
033792*        05  -COPY W6L112                                                 
033793     SKIP3                                                                
033794                                                                          
033795 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
033796 01   DLI-IO-AREA-B601.                                                   
033797*     03  -COPY WDB601                                                    
033798                                                                          
033800 LINKAGE SECTION.                                                         
033900                                                                          
034000*01  -COPY W0009   -PRE MSG-                                              
034100     EJECT                                                                
034200                                                                          
034300*01  -COPY W0009   -PRE ALT1-                                             
034400     EJECT                                                                
034500                                                                          
034600*01  -COPY W0009   -PRE 6197-                                             
034700     EJECT                                                                
034800                                                                          
034900*01  -COPY W0008      -PRE USEA-                                          
035000     05  FILLER                  PIC X.                                   
035100     EJECT                                                                
035200*01  -COPY W0008  -PRE INLC-                                              
035300     05  FILLER                  PIC X.                                   
035400     EJECT                                                                
035500                                                                          
035600*01  -COPY W0008  -PRE INLA-                                              
035700     05  FILLER                  PIC X.                                   
035800     EJECT                                                                
035900                                                                          
036000                                                                          
036100*01  -COPY W0008  -PRE PLAA-                                              
036200     05  FILLER                  PIC X.                                   
036300     EJECT                                                                
036400                                                                          
036410*01  -COPY W0008  -PRE UPFA-                                              
036420     05  FILLER                  PIC X.                                   
036430     EJECT                                                                
036431                                                                          
036432*01  -COPY W0008  -PRE WDB6-                                              
036433     05  FILLER                  PIC X.                                   
036434     EJECT                                                                
036435                                                                          
036800 PROCEDURE DIVISION  USING MSG-PCB                                        
036900                           ALT1-PCB                                       
037000                           6197-PCB                                       
037100                           USEA-PCB                                       
037200                           INLC-PCB                                       
037300                           INLA-PCB                                       
037400                           PLAA-PCB                                       
037410                           UPFA-PCB                                       
037420                           WDB6-PCB.                                      
037500     ENTRY 'DLITCBL' USING MSG-PCB                                        
037600                           ALT1-PCB                                       
037700                           6197-PCB                                       
037800                           USEA-PCB                                       
037900                           INLC-PCB                                       
038000                           INLA-PCB                                       
038100                           PLAA-PCB                                       
038101                           UPFA-PCB                                       
038102                           WDB6-PCB.                                      
038200     EJECT                                                                
038400                                                                          
038500*----------------------------------------------------------------*        
038600     PERFORM IMS-GET-MSG                                                  
038700     IF SEGMENT-FINNS                                                     
038800        PERFORM A-INIT                                                    
038900        PERFORM B-KOLLA-NYCKLAR                                           
039000        IF NYCKLAR-OK                                                     
039100           IF MFS-UPDATE  OR MFS-UPD-V                                    
039200              PERFORM G-KOLLA-INPUT                                       
039300              IF INDATA-OK                                                
039400                PERFORM H-UPPDATERA                                       
039500              END-IF                                                      
039600           ELSE                                                           
039700              PERFORM F-LAES-VISA-INFO                                    
039800           END-IF                                                         
039900        END-IF                                                            
040000        PERFORM S90-BLANKUTF-NUM-FAELT                                    
040100        MOVE MAX-MOD-LAENGD TO MSG-KVLL                                   
040200        PERFORM IMS-INSERT-MSG                                            
040300     END-IF                                                               
040400                                                                          
040500     MOVE ZERO TO RETURN-CODE                                             
040600     GOBACK                                                               
040700     .                                                                    
040800     EJECT                                                                
040900*----------------------------------------------------------------*        
041000 A-INIT SECTION.                                                          
041100                                                                          
041200     IF MSG-DUBBLA-TRANSKODER                                             
041300        MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W6I10201                
041400                                              HTERM-MID-W6I10203          
041500        MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                 
041600        MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                               
041700     ELSE                                                                 
041800        MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W6I10201                 
041900                                             HTERM-MID-W6I10203           
042000        MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                 
042100        MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                               
042200     END-IF                                                               
042300                                                                          
042400     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
042500     MOVE MSG-IDPFK TO MFS-IDPFK                                          
042600     MOVE MFS-IDTRANS TO W-IDTRANS                                        
042700                                                                          
042800     MOVE LOW-VALUE TO MSG-AREA                                           
042900     MOVE 'W6O102N1' TO MFS-IDMOD                                         
043000     MOVE '6102' TO MOD-IDTRANS                                           
043100     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
043200                                                                          
043300     IF EGEN-MID OR HELP-MID                                              
043400        CONTINUE                                                          
043500     ELSE                                                                 
043600        MOVE SPACE TO MFS-KDTRTYP                                         
043700        MOVE '7' TO MFS-IDPFK                                             
043800     END-IF                                                               
044700                                                                          
044800     IF MFS-UPD-V                                                         
044900       MOVE ALL '+'                   TO MID-W6I10201                     
045000       IF HTERM-MID-IDINLVGN-UT = ALL '+' OR                              
045010          HTERM-MID-IDINLVGN-UT = ALL '-'                                 
045100         MOVE SPACE                     TO MID-IDINLVGN-UT                
045200         MOVE ZERO                      TO MID-SPAR-IDINLVGN              
045300       ELSE                                                               
045400         MOVE HTERM-MID-IDINLVGN-UT     TO MID-IDINLVGN-UT                
045500                                           MID-SPAR-IDINLVGN              
045600       END-IF                                                             
045700       IF HTERM-MID-ADINLOMR-UT = ALL '+' OR                              
045710          HTERM-MID-ADINLOMR-UT = ALL '-'                                 
045800         MOVE SPACE                     TO MID-ADINLOMR-UT                
045900                                           MID-SPAR-ADINLOMR              
046000       ELSE                                                               
046100         MOVE HTERM-MID-ADINLOMR-UT     TO MID-ADINLOMR-UT                
046200                                           MID-SPAR-ADINLOMR              
046300       END-IF                                                             
046400       IF HTERM-MID-ADINLOMR-NXT-UT = ALL '+' OR                          
046410          HTERM-MID-ADINLOMR-NXT-UT = ALL '-'                             
046500         MOVE SPACE                     TO MID-ADINLOMR-NXT-UT            
046600                                           MID-SPAR-ADINLOMR-NXT          
046700       ELSE                                                               
046800         MOVE HTERM-MID-ADINLOMR-NXT-UT TO MID-ADINLOMR-NXT-UT            
046900                                           MID-SPAR-ADINLOMR-NXT          
047000       END-IF                                                             
047100        MOVE +1 TO IX                                                     
047200        PERFORM UNTIL IX > MAX-IX OR                                      
047300                           HTERM-MID-IDLEVNR(IX) = LOW-VALUE              
047310         IF HTERM-MID-IDLEVNR (IX) = ALL '-'                              
047320           MOVE '+++++'              TO MID-IDLEVNR (IX)                  
047330         ELSE                                                             
047331           MOVE HTERM-MID-IDLEVNR (IX) TO MID-IDLEVNR(IX)                 
047340         END-IF                                                           
047350         IF HTERM-MID-IDOKOLLI (IX) = ALL '-'                             
047360           MOVE '+++++++++'            TO MID-IDOKOLLI(IX)                
047510         ELSE                                                             
047512           MOVE HTERM-MID-IDOKOLLI(IX) TO MID-IDOKOLLI(IX)                
047520         END-IF                                                           
047600         ADD +1 TO IX                                                     
047700        END-PERFORM                                                       
047800     END-IF                                                               
047810     PERFORM AA-INIT-NYCKLAR                                              
047811                                                                          
047812     IF MSGI-IDLAND-SPR = 'GB'                                            
047813        MOVE +2 TO SPRAK-IX                                               
047814        MOVE 'GB ' TO MED-IDSKYLT                                         
047815     ELSE                                                                 
047816        MOVE +1 TO SPRAK-IX                                               
047817        MOVE 'S  ' TO MED-IDSKYLT                                         
047818     END-IF                                                               
047820     .                                                                    
047830     EJECT                                                                
047840*----------------------------------------------------------------*        
047850 AA-INIT-NYCKLAR SECTION.                                                 
047860                                                                          
047870     MOVE ALL '+' TO MSGI-WMSGINIT                                        
047880     MOVE '001'                  TO MSGI-KDCALL                           
047890     MOVE MSG-SIGNON-USERID      TO MSGI-IDUSER                           
047891     MOVE MSG-LTERM-NAME         TO MSGI-IDLTERM-USER                     
047892     MOVE '6102'                 TO MSGI-IDTRANS                          
047893     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
047894     .                                                                    
047895     EJECT                                                                
048100*----------------------------------------------------------------*        
048200 B-KOLLA-NYCKLAR SECTION.                                                 
048300                                                                          
048400     MOVE JA                 TO NYCKLAR-SW                                
048500     MOVE SPACE              TO NKLTYP-SW                                 
048502                                                                          
048503     IF MID-IDDC-IN           = ALL '+'                                   
048504       MOVE MSGI-IDDC         TO W-IDDC-B6                                
048506                                                                          
048507     ELSE                                                                 
048508       MOVE MID-IDDC-IN       TO W-IDDC-B6                                
048509       MOVE '7'               TO MFS-IDPFK                                
048510       MOVE SPACE             TO MFS-KDTRTYP                              
048511     END-IF                                                               
048512     PERFORM IMS-GU-WDB601                                                
048513                                                                          
048514     IF DCS-KDDC = SPACE OR DCS-DDC                                       
048515         MOVE NEJ TO NYCKLAR-SW                                           
048516     ELSE                                                                 
048517         MOVE DCS-IDDC       TO W-IDDC                                    
048518                                WGX-IDDC                                  
048519                                MOD-IDDC-UT                               
048521     END-IF                                                               
048530                                                                          
048600*----NKL-FÄLT-UT                                                          
048700     INSPECT MID-IDINLVGN-IN REPLACING LEADING SPACE BY ZERO              
048800     INSPECT MID-IDINLVGN-UT REPLACING LEADING SPACE BY ZERO              
048900                                                                          
049000     IF MID-IDINLVGN-IN      = ALL '+' AND                                
049100        MID-ADINLOMR-IN      = ALL '+' AND                                
049200        MID-ADINLOMR-NXT-IN  = ALL '+'                                    
049300        IF MID-IDINLVGN-UT      = ZERO  AND                               
049400           MID-ADINLOMR-UT      = SPACE AND                               
049500           MID-ADINLOMR-NXT-UT  = SPACE                                   
049600*-FEL - 401                                                               
049700           MOVE NEJ          TO NYCKLAR-SW                                
049800           MOVE ERR-WRONG-KEY       TO MED-IDMFSFEL                       
049900           PERFORM S01-CALL-WMEDKONV-FEL                                  
050000        ELSE                                                              
050100*----------GAMLA NYCKLAR                                                  
050200           PERFORM BB-GAMLA-NKL                                           
050300        END-IF                                                            
050400     ELSE                                                                 
050500*-------NYA NYCKLAR                                                       
050600        MOVE SPACE TO MFS-KDTRTYP                                         
050700        MOVE '7' TO MFS-IDPFK                                             
050800        PERFORM BD-KONTR-PLAC-ADR-LIKA                                    
050900        IF NYCKLAR-OK                                                     
051000           PERFORM BA-FORMELLA-KONTR                                      
051100           PERFORM MFS-RENSA-FAELT-UT                                     
051200        END-IF                                                            
051300     END-IF                                                               
051400                                                                          
051500     IF NYCKLAR-OK                                                        
051600        PERFORM BC-KOLLA-NKL-BAS                                          
051700     END-IF                                                               
051800                                                                          
051900     IF NYCKLAR-FEL                                                       
052000        PERFORM MFS-RENSA-FAELT-UT                                        
052100     END-IF                                                               
052200                                                                          
052300     IF GODK-MID OR HELP-MID                                              
052400        CONTINUE                                                          
052500     ELSE                                                                 
052600        MOVE MFS-RENSA-FAELT      TO MOD-IDINLVGN-UT                      
052700                                  MOD-ADINLOMR-UT                         
052800                                  MOD-ADINLOMR-NXT-UT                     
052810                                  MOD-IDDC-UT                             
052900                                  MOD-TEMFSFEL                            
053200     END-IF                                                               
053300                                                                          
053310     PERFORM MFS-RENSA-FAELT-UT                                           
053400     PERFORM MFS-RENSA-FAELT-IN                                           
053500     .                                                                    
053600     EJECT                                                                
053700*----------------------------------------------------------------*        
053800 BA-FORMELLA-KONTR SECTION.                                               
053900                                                                          
054000*----TA REDA PÅ VILKEN NKLTYP, KOLLA VAGN,                                
054100*----SAMT FLYTTA MID-IN TILL MOD-UT                                       
054200*----VAGN                                                                 
054300     IF MID-IDINLVGN-IN     NOT = ALL '+' AND                             
054400        MID-ADINLOMR-IN         = ALL '+' AND                             
054500        MID-ADINLOMR-NXT-IN     = ALL '+'                                 
054600        PERFORM BAA-KONTR-VAGN                                            
054700        MOVE MID-IDINLVGN-IN       TO MOD-IDINLVGN-UT                     
054800        MOVE SPACE                 TO MOD-ADINLOMR-UT                     
054900                                      MOD-ADINLOMR-NXT-UT                 
055000        MOVE '1'                   TO NKLTYP-SW                           
055100     ELSE                                                                 
055200*-------VAGN-PLAC                                                         
055300        IF MID-IDINLVGN-IN     NOT = ALL '+' AND                          
055400           MID-ADINLOMR-IN     NOT = ALL '+' AND                          
055500           MID-ADINLOMR-NXT-IN     = ALL '+'                              
055600           PERFORM BAA-KONTR-VAGN                                         
055700           MOVE MID-IDINLVGN-IN    TO MOD-IDINLVGN-UT                     
055800           MOVE MID-ADINLOMR-IN    TO MOD-ADINLOMR-UT                     
055900           MOVE SPACE              TO MOD-ADINLOMR-NXT-UT                 
056000           MOVE '1'                TO NKLTYP-SW                           
056100        ELSE                                                              
056200*----------VAGN-PLAC-ADR                                                  
056300           IF MID-IDINLVGN-IN     NOT = ALL '+' AND                       
056400              MID-ADINLOMR-IN     NOT = ALL '+' AND                       
056500              MID-ADINLOMR-NXT-IN NOT = ALL '+'                           
056600              PERFORM BAA-KONTR-VAGN                                      
056700              MOVE MID-IDINLVGN-IN     TO MOD-IDINLVGN-UT                 
056800              MOVE MID-ADINLOMR-IN     TO MOD-ADINLOMR-UT                 
056900              MOVE MID-ADINLOMR-NXT-IN TO MOD-ADINLOMR-NXT-UT             
057000              MOVE '1'             TO NKLTYP-SW                           
057100           ELSE                                                           
057200*-------------PLAC                                                        
057300              IF MID-IDINLVGN-IN      = ALL '+' AND                       
057400                 MID-ADINLOMR-IN  NOT = ALL '+' AND                       
057500                 MID-ADINLOMR-NXT-IN  = ALL '+'                           
057600                 MOVE MID-ADINLOMR-IN TO MOD-ADINLOMR-UT                  
057700                 MOVE SPACE           TO MOD-ADINLOMR-NXT-UT              
057800                 MOVE +000            TO MOD-IDINLVGN-UT                  
057900                 MOVE '2'          TO NKLTYP-SW                           
058000              ELSE                                                        
058100*----------------PLAC-ADR                                                 
058200                 IF MID-IDINLVGN-IN     = ALL '+' AND                     
058300                    MID-ADINLOMR-IN NOT = ALL '+' AND                     
058400                    MID-ADINLOMR-NXT-IN NOT = ALL '+'                     
058500                    MOVE +000            TO MOD-IDINLVGN-UT               
058600                    MOVE MID-ADINLOMR-IN TO MOD-ADINLOMR-UT               
058700                    MOVE MID-ADINLOMR-NXT-IN TO                           
058800                         MOD-ADINLOMR-NXT-UT                              
058900                    MOVE '2'       TO NKLTYP-SW                           
059000                 ELSE                                                     
059100*-FEL - 401                                                               
059200                    MOVE NEJ              TO NYCKLAR-SW                   
059300                    MOVE ERR-WRONG-KEY    TO MED-IDMFSFEL                 
059400                    IF MID-IDINLVGN-IN = ALL '+'                          
059500                       MOVE +000          TO MOD-IDINLVGN-UT              
059600                    ELSE                                                  
059700                       MOVE MID-IDINLVGN-IN TO MOD-IDINLVGN-UT            
059800                    END-IF                                                
059900                    IF MID-ADINLOMR-IN = ALL '+'                          
060000                       MOVE SPACE         TO MOD-ADINLOMR-UT              
060100                    ELSE                                                  
060200                       MOVE MID-ADINLOMR-IN TO MOD-ADINLOMR-UT            
060300                    END-IF                                                
060400                    IF MID-ADINLOMR-NXT-IN = ALL '+'                      
060500                       MOVE SPACE         TO MOD-ADINLOMR-NXT-UT          
060600                    ELSE                                                  
060700                      MOVE MID-ADINLOMR-NXT-IN TO                         
060800                           MOD-ADINLOMR-NXT-UT                            
060900                    END-IF                                                
061000                    PERFORM S01-CALL-WMEDKONV-FEL                         
061100                 END-IF                                                   
061200              END-IF                                                      
061300           END-IF                                                         
061400        END-IF                                                            
061500     END-IF                                                               
061600     .                                                                    
061700     EJECT                                                                
061800*----------------------------------------------------------------*        
061900 BAA-KONTR-VAGN    SECTION.                                               
062000                                                                          
062100     IF MID-IDINLVGN-IN = ZERO OR                                         
062200        MID-IDINLVGN-IN NOT NUMERIC                                       
062300*-FEL - 401                                                               
062400        MOVE NEJ                   TO NYCKLAR-SW                          
062500        MOVE ERR-WRONG-KEY         TO MED-IDMFSFEL                        
062600        PERFORM S01-CALL-WMEDKONV-FEL                                     
062700     END-IF                                                               
062800     .                                                                    
062900     EJECT                                                                
063000*----------------------------------------------------------------*        
063100 BB-GAMLA-NKL   SECTION.                                                  
063200                                                                          
063300*----VAGN                                                                 
063400     IF MID-IDINLVGN-UT     > ZERO AND                                    
063500        MID-ADINLOMR-UT     = SPACE AND                                   
063600        MID-ADINLOMR-NXT-UT = SPACE                                       
063700        MOVE '1'                   TO NKLTYP-SW                           
063800        MOVE MID-IDINLVGN-UT       TO MOD-IDINLVGN-UT                     
063900        MOVE SPACE                 TO MOD-ADINLOMR-UT                     
064000                                      MOD-ADINLOMR-NXT-UT                 
064100     ELSE                                                                 
064200*-------VAGN-PLAC                                                         
064300        IF MID-IDINLVGN-UT     > ZERO AND                                 
064400           MID-ADINLOMR-UT NOT = SPACE AND                                
064500           MID-ADINLOMR-NXT-UT = SPACE                                    
064600           MOVE '1'                TO NKLTYP-SW                           
064700           MOVE MID-IDINLVGN-UT    TO MOD-IDINLVGN-UT                     
064800           MOVE MID-ADINLOMR-UT    TO MOD-ADINLOMR-UT                     
064900           MOVE SPACE              TO MOD-ADINLOMR-NXT-UT                 
065000        ELSE                                                              
065100*----------VAGN-PLAC-ADR                                                  
065200           IF MID-IDINLVGN-UT         > ZERO AND                          
065300              MID-ADINLOMR-UT     NOT = SPACE AND                         
065400              MID-ADINLOMR-NXT-UT NOT = SPACE                             
065500              MOVE '1'                 TO NKLTYP-SW                       
065600              MOVE MID-IDINLVGN-UT     TO MOD-IDINLVGN-UT                 
065700              MOVE MID-ADINLOMR-UT     TO MOD-ADINLOMR-UT                 
065800              MOVE MID-ADINLOMR-NXT-UT TO MOD-ADINLOMR-NXT-UT             
065900           ELSE                                                           
066000*-------------PLAC                                                        
066100              IF MID-IDINLVGN-UT         = ZERO AND                       
066200                 MID-ADINLOMR-UT     NOT = SPACE AND                      
066300                 MID-ADINLOMR-NXT-UT     = SPACE                          
066400                 MOVE '2'             TO NKLTYP-SW                        
066500                 MOVE +000            TO MOD-IDINLVGN-UT                  
066600                 MOVE SPACE           TO MOD-ADINLOMR-NXT-UT              
066700                 MOVE MID-ADINLOMR-UT TO MOD-ADINLOMR-UT                  
066800              ELSE                                                        
066900*----------------PLAC-ADR                                                 
067000                 IF MID-IDINLVGN-UT   = ZERO AND                          
067100                    MID-ADINLOMR-UT NOT = SPACE AND                       
067200                    MID-ADINLOMR-NXT-UT NOT = SPACE                       
067300                    MOVE '2'             TO NKLTYP-SW                     
067400                    MOVE +000            TO MOD-IDINLVGN-UT               
067500                    MOVE MID-ADINLOMR-UT TO MOD-ADINLOMR-UT               
067600                   MOVE MID-ADINLOMR-NXT-UT TO MOD-ADINLOMR-NXT-UT        
067700                 END-IF                                                   
067800              END-IF                                                      
067900           END-IF                                                         
068000        END-IF                                                            
068100     END-IF                                                               
068200     .                                                                    
068300     EJECT                                                                
068400*----------------------------------------------------------------*        
068500 BC-KOLLA-NKL-BAS SECTION.                                                
068600                                                                          
068700*----KONTROLLER BEROENDE PÅ NKLTYP                                        
068800*---- 1 = MED VAGN                                                        
068900*---- 2 = UTAN VAGN                                                       
069000                                                                          
069100     IF NKLTYP1                                                           
069200        PERFORM BCA-KOLLA-NKLTYP1                                         
069300     END-IF                                                               
069400                                                                          
069500     IF NKLTYP2                                                           
069600        PERFORM BCB-KOLLA-NKLTYP2                                         
069700     END-IF                                                               
069800     .                                                                    
069900     EJECT                                                                
070000*----------------------------------------------------------------*        
070100 BCA-KOLLA-NKLTYP1 SECTION.                                               
070200                                                                          
070300*    HÄR ANVÄNDS UT-NYCKLARNA VID KONTROLL                                
070400*    (DESSA ÄR INITIERADE I BA- RESP BB-)                                 
070500                                                                          
070600     PERFORM BCAF-INIT-INLA-BAS                                           
070700                                                                          
070800     IF SEGMENT-SAKNAS                                                    
070900        IF MOD-IDINLVGN-UT > ZERO                                         
071000           IF MOD-ADINLOMR-UT NOT = SPACE AND                             
071100              MOD-ADINLOMR-UT NOT = LOW-VALUE                             
071200*-------------OK KOLLA PLAC                                               
071300              PERFORM BCAD-KOLLA-PLAC                                     
071400              IF NYCKLAR-OK                                               
071500                 IF MOD-ADINLOMR-NXT-UT NOT = SPACE AND                   
071600                    MOD-ADINLOMR-NXT-UT NOT = LOW-VALUE                   
071700*-------------------OK KOLLA ADR                                          
071800                    PERFORM BCAE-KOLLA-ADR                                
071900                 END-IF                                                   
072000              END-IF                                                      
072100           ELSE                                                           
072200*-FEL - 401                                                               
072300*-------------PLAC  SAKNAS                                                
072400              MOVE NEJ             TO NYCKLAR-SW                          
072500              MOVE ERR-WRONG-KEY   TO MED-IDMFSFEL                        
072600              PERFORM S01-CALL-WMEDKONV-FEL                               
072700           END-IF                                                         
072800        END-IF                                                            
072900     ELSE                                                                 
073000*-------VAGN FINNS                                                        
073300         IF MOD-IDINLVGN-UT       > ZERO AND                              
073400            MOD-ADINLOMR-UT       = SPACE AND                             
073500            MOD-ADINLOMR-NXT-UT = SPACE                                   
073600            PERFORM BCAA-VAGN                                             
073700         ELSE                                                             
073800            IF MOD-IDINLVGN-UT        > ZERO AND                          
073900               MOD-ADINLOMR-UT    NOT = SPACE AND                         
074000               MOD-ADINLOMR-NXT-UT    = SPACE                             
074100               PERFORM BCAB-VAGN-PLAC                                     
074200            ELSE                                                          
074300               IF MOD-IDINLVGN-UT     > ZERO AND                          
074400                  MOD-ADINLOMR-UT NOT = SPACE AND                         
074500                  MOD-ADINLOMR-NXT-UT NOT = SPACE                         
074600                  PERFORM BCAC-VAGN-PLAC-ADR                              
074700               END-IF                                                     
074800            END-IF                                                        
074900         END-IF                                                           
075100     END-IF                                                               
075200     .                                                                    
075300     EJECT                                                                
075400*----------------------------------------------------------------*        
075500 BCAA-VAGN   SECTION.                                                     
075600                                                                          
075700     MOVE RAD-IDINLVGN             TO MOD-SPAR-IDINLVGN                   
075800     MOVE RAD-ADINLOMR             TO MOD-SPAR-ADINLOMR                   
075900                                      MOD-ADINLOMR-UT                     
076000     MOVE RAD-ADINLOMR-NXT         TO MOD-SPAR-ADINLOMR-NXT               
076100                                      MOD-ADINLOMR-NXT-UT                 
076200     .                                                                    
076300     EJECT                                                                
076400*----------------------------------------------------------------*        
076500 BCAB-VAGN-PLAC   SECTION.                                                
076600     IF RAD-ADINLOMR = MOD-ADINLOMR-UT                                    
076700        MOVE RAD-IDINLVGN          TO MOD-SPAR-IDINLVGN                   
076800        MOVE RAD-ADINLOMR          TO MOD-SPAR-ADINLOMR                   
076900        MOVE RAD-ADINLOMR-NXT      TO MOD-SPAR-ADINLOMR-NXT               
077000                                      MOD-ADINLOMR-NXT-UT                 
077100     ELSE                                                                 
077200*-FEL - 196                                                               
077300*-------ANGIVEN PLACERING STÄMMER EJ                                      
077400*       BYT I NKL-FÄLT                                                    
077500        MOVE RAD-IDINLVGN          TO MOD-SPAR-IDINLVGN                   
077600        MOVE RAD-ADINLOMR          TO MOD-SPAR-ADINLOMR                   
077700                                      MOD-ADINLOMR-UT                     
077800        MOVE RAD-ADINLOMR-NXT      TO MOD-SPAR-ADINLOMR-NXT               
077900                                      MOD-ADINLOMR-NXT-UT                 
078000        MOVE ERR-PLAC-ADR-FINNS    TO MED-IDMFSINF                        
078100        PERFORM S02-CALL-WMEDKONV-INF                                     
078200     END-IF                                                               
078300     .                                                                    
078400     EJECT                                                                
078500*----------------------------------------------------------------*        
078600 BCAC-VAGN-PLAC-ADR    SECTION.                                           
078700                                                                          
078800     IF RAD-ADINLOMR = MOD-ADINLOMR-UT                                    
078900        MOVE RAD-IDINLVGN          TO MOD-SPAR-IDINLVGN                   
079000        MOVE RAD-ADINLOMR          TO MOD-SPAR-ADINLOMR                   
079100     ELSE                                                                 
079200*-FEL - 196                                                               
079300*-------ANGIVEN PLACERING STÄMMER EJ                                      
079400*       BYT I NKL-FÄLT                                                    
079500        MOVE RAD-IDINLVGN          TO MOD-SPAR-IDINLVGN                   
079600        MOVE RAD-ADINLOMR          TO MOD-SPAR-ADINLOMR                   
079700                                      MOD-ADINLOMR-UT                     
079800        MOVE ERR-PLAC-ADR-FINNS    TO MED-IDMFSINF                        
079900        PERFORM S02-CALL-WMEDKONV-INF                                     
080000     END-IF                                                               
080100                                                                          
080200     IF RAD-ADINLOMR-NXT = MOD-ADINLOMR-NXT-UT                            
080300        MOVE RAD-ADINLOMR-NXT      TO MOD-SPAR-ADINLOMR-NXT               
080400     ELSE                                                                 
080500*-FEL - 196                                                               
080600*-------ANGIVEN PLACERING STÄMMER EJ                                      
080700*       BYT I NKL-FÄLT                                                    
080800        MOVE RAD-ADINLOMR-NXT      TO MOD-SPAR-ADINLOMR-NXT               
080900                                      MOD-ADINLOMR-NXT-UT                 
081000        MOVE ERR-PLAC-ADR-FINNS    TO MED-IDMFSINF                        
081100        PERFORM S02-CALL-WMEDKONV-INF                                     
081200     END-IF                                                               
081300     .                                                                    
081400     EJECT                                                                
081500*----------------------------------------------------------------*        
081600 BCAD-KOLLA-PLAC   SECTION.                                               
081700                                                                          
081800*----KOLLA PLACERING - LÄS PLAA                                           
081900                                                                          
082000     MOVE '6005'                   TO WGX-IDHTYP                          
082100     MOVE MOD-ADINLOMR-UT          TO W-ADINLOMR                          
082200                                                                          
082300     PERFORM IMS-GU-PLAA-G111                                             
082400     IF SEGMENT-FINNS                                                     
082500        MOVE MOD-IDINLVGN-UT       TO MOD-SPAR-IDINLVGN                   
082600        MOVE MOD-ADINLOMR-UT       TO MOD-SPAR-ADINLOMR                   
082700        MOVE SPACE                 TO MOD-SPAR-ADINLOMR-NXT               
082800     ELSE                                                                 
082900*-FEL - 401                                                               
083000        MOVE +000                  TO MOD-SPAR-IDINLVGN                   
083100        MOVE SPACE                 TO MOD-SPAR-ADINLOMR                   
083200                                      MOD-SPAR-ADINLOMR-NXT               
083300        MOVE NEJ                   TO NYCKLAR-SW                          
083400        MOVE ERR-WRONG-KEY         TO MED-IDMFSFEL                        
083500        PERFORM S01-CALL-WMEDKONV-FEL                                     
083600     END-IF                                                               
083700     .                                                                    
083800     EJECT                                                                
083900*----------------------------------------------------------------*        
084000 BCAE-KOLLA-ADR SECTION.                                                  
084100                                                                          
084200*----KOLLA ADRESS - LÄS PLAA                                              
084300                                                                          
084400     MOVE '6005'                   TO WGX-IDHTYP                          
084500     MOVE MOD-ADINLOMR-NXT-UT        TO W-ADINLOMR                        
084600                                                                          
084700     PERFORM IMS-GU-PLAA-G111                                             
084800     IF SEGMENT-FINNS                                                     
084900        MOVE MOD-ADINLOMR-NXT-UT     TO MOD-SPAR-ADINLOMR-NXT             
085000     ELSE                                                                 
085100*-FEL - 401                                                               
085200        MOVE +000                    TO MOD-SPAR-IDINLVGN                 
085300        MOVE SPACE                   TO MOD-SPAR-ADINLOMR                 
085400                                        MOD-SPAR-ADINLOMR-NXT             
085500        MOVE NEJ                     TO NYCKLAR-SW                        
085600        MOVE ERR-WRONG-KEY           TO MED-IDMFSFEL                      
085700        PERFORM S01-CALL-WMEDKONV-FEL                                     
085800     END-IF                                                               
085900     .                                                                    
086000     EJECT                                                                
086100*----------------------------------------------------------------*        
086200 BCAF-INIT-INLA-BAS SECTION.                                              
086300                                                                          
086400     MOVE MOD-IDINLVGN-UT     TO W-IDINLVGN                               
086410                                 W-IDINLVGN-SEQ                           
086500                                                                          
086600     PERFORM IMS-GU-INLA-D121                                             
086700                                                                          
086800     .                                                                    
086900     EJECT                                                                
087000*----------------------------------------------------------------*        
088200*----------------------------------------------------------------*        
088300 BCB-KOLLA-NKLTYP2 SECTION.                                               
088400                                                                          
088500     MOVE +000                     TO MOD-SPAR-IDINLVGN                   
088600     MOVE '6005'                   TO WGX-IDHTYP                          
088700                                                                          
088800     PERFORM  BCBA-KOLLA-NKLTYP2-PLAC                                     
088900                                                                          
089000     IF NYCKLAR-OK                                                        
089100        IF MOD-ADINLOMR-NXT-UT NOT = SPACE                                
089200           PERFORM BCBB-KOLLA-NKLTYP2-ADR                                 
089300        ELSE                                                              
089400           MOVE SPACE                TO MOD-SPAR-ADINLOMR-NXT             
089500        END-IF                                                            
089600     END-IF                                                               
089700     .                                                                    
089800     EJECT                                                                
089900*----------------------------------------------------------------*        
090000 BCBA-KOLLA-NKLTYP2-PLAC SECTION.                                         
090100                                                                          
090200*--- KOLLA PLACERING - LÄS PLAA                                           
090300                                                                          
090400     MOVE MOD-ADINLOMR-UT            TO W-ADINLOMR                        
090500                                                                          
090600     PERFORM IMS-GU-PLAA-G111                                             
090700     IF SEGMENT-SAKNAS                                                    
090800*-FEL - 401                                                               
090900        MOVE +000                    TO MOD-SPAR-IDINLVGN                 
091000        MOVE SPACE                   TO MOD-SPAR-ADINLOMR                 
091100                                        MOD-SPAR-ADINLOMR-NXT             
091200        MOVE NEJ                     TO NYCKLAR-SW                        
091300        MOVE ERR-WRONG-KEY           TO MED-IDMFSFEL                      
091400        PERFORM S01-CALL-WMEDKONV-FEL                                     
091500     ELSE                                                                 
091600        MOVE MOD-ADINLOMR-UT         TO MOD-SPAR-ADINLOMR                 
091700     END-IF                                                               
091800     .                                                                    
091900     EJECT                                                                
092000*----------------------------------------------------------------*        
092100 BCBB-KOLLA-NKLTYP2-ADR  SECTION.                                         
092200                                                                          
092300*--- KOLLA ADRESS - LÄS PLAA                                              
092400     MOVE MOD-ADINLOMR-NXT-UT        TO W-ADINLOMR                        
092500     PERFORM IMS-GU-PLAA-G111                                             
092600     IF SEGMENT-SAKNAS                                                    
092700*-FEL - 401                                                               
092800        MOVE +000                    TO MOD-SPAR-IDINLVGN                 
092900        MOVE SPACE                   TO MOD-SPAR-ADINLOMR                 
093000                                        MOD-SPAR-ADINLOMR-NXT             
093100        MOVE NEJ                     TO NYCKLAR-SW                        
093200        MOVE ERR-WRONG-KEY           TO MED-IDMFSFEL                      
093300        PERFORM S01-CALL-WMEDKONV-FEL                                     
093400     ELSE                                                                 
093500        MOVE MOD-ADINLOMR-NXT-UT     TO MOD-SPAR-ADINLOMR-NXT             
093600     END-IF                                                               
093700     .                                                                    
093800     EJECT                                                                
093900*----------------------------------------------------------------*        
094000 BD-KONTR-PLAC-ADR-LIKA SECTION.                                          
094100                                                                          
094200*----PLAC-ADR                                                             
094300     IF MID-ADINLOMR-IN     NOT = ALL '+' AND                             
094400        MID-ADINLOMR-NXT-IN NOT = ALL '+'                                 
094500        IF MID-ADINLOMR-IN = MID-ADINLOMR-NXT-IN                          
094600*-FEL - 401                                                               
094700*----------FEL KAN EJ VARA LIKA                                           
094800           MOVE NEJ                       TO NYCKLAR-SW                   
094900           MOVE ERR-WRONG-KEY             TO MED-IDMFSFEL                 
095000           MOVE MID-ADINLOMR-IN           TO MOD-ADINLOMR-UT              
095100           MOVE MID-ADINLOMR-NXT-IN       TO MOD-ADINLOMR-NXT-UT          
095200           PERFORM S01-CALL-WMEDKONV-FEL                                  
095300        END-IF                                                            
095400     END-IF                                                               
095500     .                                                                    
095600     EJECT                                                                
095700*----------------------------------------------------------------*        
095800 F-LAES-VISA-INFO SECTION.                                                
095900                                                                          
096000     IF HELP-MID                                                          
096100        PERFORM FA-MID-TILL-MOD                                           
096200     END-IF                                                               
096300     IF HELP-MID OR EGEN-MID                                              
096400        IF MID-RAD(1)  = ALL '+' AND                                      
096500           MID-RAD(2)  = ALL '+' AND                                      
096600           MID-RAD(3)  = ALL '+' AND                                      
096700           MID-RAD(4)  = ALL '+' AND                                      
096800           MID-RAD(5)  = ALL '+' AND                                      
096900           MID-RAD(6)  = ALL '+' AND                                      
097000           MID-RAD(7)  = ALL '+' AND                                      
097100           MID-RAD(8)  = ALL '+' AND                                      
097200           MID-RAD(9)  = ALL '+' AND                                      
097300           MID-RAD(10) = ALL '+' AND                                      
097400           MID-RAD(11) = ALL '+' AND                                      
097500           MID-RAD(12) = ALL '+' AND                                      
097600           MID-RAD(13) = ALL '+' AND                                      
097700           MID-RAD(14) = ALL '+'                                          
097800           CONTINUE                                                       
097900        ELSE                                                              
098000           IF MFS-FIRST OR HELP-MID                                       
098100              CONTINUE                                                    
098200           ELSE                                                           
098300              PERFORM MFS-ROER-EJ-FAELT-IN                                
098400              PERFORM MFS-ROER-EJ-FAELT-UT                                
098500              PERFORM MFS-LAES-IN-IGEN                                    
098600              MOVE INF-PRESS-PF11  TO MED-IDMFSFEL                        
098700              PERFORM S01-CALL-WMEDKONV-FEL                               
098800           END-IF                                                         
098900        END-IF                                                            
099000     END-IF                                                               
099100     PERFORM MFS-RENSA-FAELT-IN                                           
099200     .                                                                    
099300     EJECT                                                                
099400*----------------------------------------------------------------*        
099500 FA-MID-TILL-MOD SECTION.                                                 
099600                                                                          
099700     MOVE  +1             TO IX                                           
099800     PERFORM UNTIL IX > MAX-IX                                            
099900        IF MID-RAD(IX) = ALL '+'                                          
100000           MOVE MFS-RENSA-FAELT TO MOD-IDLEVNR(IX)                        
100100                                   MOD-IDOKOLLI(IX)                       
100200        ELSE                                                              
100400        INSPECT MID-IDOKOLLI(IX) REPLACING LEADING SPACE BY ZERO          
100500           IF MID-IDLEVNR(IX) = ALL '+'                                   
100600              MOVE MFS-RENSA-FAELT    TO MOD-IDLEVNR(IX)                  
100700           ELSE                                                           
100800              MOVE MID-IDLEVNR(IX)    TO MOD-IDLEVNR(IX)                  
100900           END-IF                                                         
101000           IF MID-IDOKOLLI(IX) = ALL '+'                                  
101100              MOVE MFS-RENSA-FAELT    TO MOD-IDOKOLLI(IX)                 
101200           ELSE                                                           
101300              MOVE MID-IDOKOLLI(IX)   TO MOD-IDOKOLLI(IX)                 
101400           END-IF                                                         
101500           MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDLEVNR-ATTR(IX)             
101600                                         MOD-IDOKOLLI-ATTR(IX)            
101700        END-IF                                                            
101800        ADD 1             TO IX                                           
101900     END-PERFORM                                                          
102000     .                                                                    
102100     EJECT                                                                
102200*----------------------------------------------------------------*        
102300 G-KOLLA-INPUT SECTION.                                                   
102400                                                                          
102500     MOVE JA                 TO INDATA-SW                                 
102600     IF MID-RAD(1)     = ALL '+' AND                                      
102700        MID-RAD(2)     = ALL '+' AND                                      
102800        MID-RAD(3)     = ALL '+' AND                                      
102900        MID-RAD(4)     = ALL '+' AND                                      
103000        MID-RAD(5)     = ALL '+' AND                                      
103100        MID-RAD(6)     = ALL '+' AND                                      
103200        MID-RAD(7)     = ALL '+' AND                                      
103300        MID-RAD(8)     = ALL '+' AND                                      
103400        MID-RAD(9)     = ALL '+' AND                                      
103500        MID-RAD(10)    = ALL '+' AND                                      
103600        MID-RAD(11)    = ALL '+' AND                                      
103700        MID-RAD(12)    = ALL '+' AND                                      
103800        MID-RAD(13)    = ALL '+' AND                                      
103900        MID-RAD(14)    = ALL '+'                                          
104000                                                                          
104100        MOVE NEJ                   TO INDATA-SW                           
104200        MOVE ERR-PF11-AND-NO-DATA  TO MED-IDMFSFEL                        
104300        PERFORM S01-CALL-WMEDKONV-FEL                                     
104400     ELSE                                                                 
104500        IF MID-RAD(1) NOT = ALL '+' OR                                    
104600           MID-RAD(2) NOT = ALL '+' OR                                    
104700           MID-RAD(3) NOT = ALL '+' OR                                    
104800           MID-RAD(4) NOT = ALL '+' OR                                    
104900           MID-RAD(5) NOT = ALL '+' OR                                    
105000           MID-RAD(6) NOT = ALL '+' OR                                    
105100           MID-RAD(7) NOT = ALL '+' OR                                    
105200           MID-RAD(8) NOT = ALL '+' OR                                    
105300           MID-RAD(9) NOT = ALL '+' OR                                    
105400           MID-RAD(10) NOT = ALL '+' OR                                   
105500           MID-RAD(11) NOT = ALL '+' OR                                   
105600           MID-RAD(12) NOT = ALL '+' OR                                   
105700           MID-RAD(13) NOT = ALL '+' OR                                   
105800           MID-RAD(14) NOT = ALL '+'                                      
105900           IF MFS-UPD-V                                                   
106000             PERFORM FA-MID-TILL-MOD                                      
106100           END-IF                                                         
106200           PERFORM GA-KOLLA-RADER                                         
106300        END-IF                                                            
106400     END-IF                                                               
106500                                                                          
106600     IF INDATA-FEL                                                        
106700       IF MFS-UPD-V                                                       
106800         CONTINUE                                                         
106900       ELSE                                                               
107000        PERFORM MFS-ROER-EJ-FAELT-UT                                      
107100*-------NYTTJAS LAES-IN-IGEN SÅ FÅR MAN INTE HILITE PÅ FÄLTEN             
107200       END-IF                                                             
107300     END-IF                                                               
107400     PERFORM MFS-RENSA-FAELT-IN                                           
107500     .                                                                    
107600     EJECT                                                                
107700*----------------------------------------------------------------*        
107800 GA-KOLLA-RADER    SECTION.                                               
108100     MOVE ZERO               TO IX                                        
108200     ADD 1                   TO IX                                        
108300     MOVE JA                 TO INDATA-SW                                 
108400                                                                          
108500     PERFORM UNTIL IX > MAX-IX                                            
108700        INSPECT MID-IDOKOLLI(IX) REPLACING LEADING SPACE BY ZERO          
108800                                                                          
108900*-------TESTA LEVNR,KOLLI                                                 
109000        IF MID-IDLEVNR(IX) NOT = ALL '+' OR                               
109100           MID-IDOKOLLI(IX) NOT = ALL '+'                                 
109200           PERFORM GAA-KOLLA-NUM-FAELT                                    
109300           PERFORM GAB-KOLLA-LEV-KOLLI                                    
109400        END-IF                                                            
109500        ADD 1 TO IX                                                       
109600     END-PERFORM                                                          
109700     .                                                                    
109800     EJECT                                                                
109900*----------------------------------------------------------------*        
110000 GAA-KOLLA-NUM-FAELT SECTION.                                             
110500     IF  MID-IDOKOLLI(IX) > ZERO                                          
110600            MOVE MFS-NUM-FAELT-RAETT TO MOD-IDOKOLLI-ATTR(IX)             
110800     ELSE                                                                 
111100*-FEL - 001                                                               
111200           MOVE NEJ                  TO INDATA-SW                         
111300           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
111400           PERFORM S01-CALL-WMEDKONV-FEL                                  
111500           MOVE MFS-NUM-FAELT-FEL    TO MOD-IDOKOLLI-ATTR(IX)             
111800     END-IF                                                               
111900     .                                                                    
112000     EJECT                                                                
112100*----------------------------------------------------------------*        
112200 GAB-KOLLA-LEV-KOLLI SECTION.                                             
112400*----LÄSNING FÖR ATT KOLLA RAD SKER VIA                                   
112500*    IDLEVNR + IDOKOLLI  (INDEX C ANVÄNDS)                                
112600                                                                          
112700     MOVE MID-IDLEVNR(IX)      TO WC-IDLEVNR                              
112800     MOVE MID-IDOKOLLI(IX)     TO WC-IDOKOLLI                             
112900                                                                          
113000     PERFORM IMS-GU-INLC-D111                                             
113100                                                                          
113200     IF SEGMENT-SAKNAS                                                    
113300*-FEL - 001                                                               
113400        MOVE NEJ                   TO INDATA-SW                           
113500        MOVE ERR-CORR-HILITE-FLDS  TO MED-IDMFSFEL                        
113600        PERFORM S01-CALL-WMEDKONV-FEL                                     
113700        MOVE MFS-ALFA-FAELT-FEL                                           
113800                      TO MOD-IDLEVNR-ATTR(IX)                             
113810        MOVE MFS-NUM-FAELT-FEL                                            
113900                      TO MOD-IDOKOLLI-ATTR(IX)                            
114000     ELSE                                                                 
114100        MOVE MID-IDLEVNR(IX)   TO S-IDLEVNRK                              
114200        MOVE MID-IDOKOLLI(IX)  TO S-IDOKOLLI                              
114300        PERFORM IMS-GNP-INLC-D121                                         
114310        PERFORM S50-PRIM-CONTROL                                          
114410        IF INDATA-OK                                                      
114420** SÄTTER TILLBAKA STATUSKODEN FRÅN SENASTE W6D121-LÄSNING.               
114430          MOVE INLC-STATUS-CODE  TO STATUS-WS                             
114500          IF RAD-FLDIVKLI = NEJ                                           
114600*---------   LEV-KOLLI RAD                                                
114700*---------  ALLA SKA KOLLAS OM NÅGON HAR GODKÄND STATUS RÄCKER DET        
114800             MOVE NEJ                    TO STATUS-SW                     
114900             PERFORM UNTIL SEGMENT-SAKNAS OR STATUS-OK                    
115000                IF RAD-KDINLSTA = 'FPK' OR 'SAK' OR SPACE                 
115100                   MOVE JA               TO STATUS-SW                     
115200                END-IF                                                    
115300                PERFORM IMS-GNP-INLC-D121                                 
115400             END-PERFORM                                                  
115500             IF STATUS-NEJ                                                
115600*-FEL   - 007                                                             
115700                MOVE NEJ                    TO INDATA-SW                  
115800                MOVE ERR-UPDATE-NOT-VALID   TO MED-IDMFSFEL               
115900                MOVE MFS-ALFA-FAELT-FEL                                   
116000                     TO MOD-IDLEVNR-ATTR(IX)                              
116010                MOVE MFS-NUM-FAELT-FEL                                    
116100                     TO MOD-IDOKOLLI-ATTR(IX)                             
116200                PERFORM S01-CALL-WMEDKONV-FEL                             
116300             END-IF                                                       
116400          ELSE                                                            
116500*---------   ALLA INGÅENDE ARTIKLAR                                       
116600             MOVE NEJ TO STATUS-SW                                        
116700             PERFORM UNTIL SEGMENT-SAKNAS OR STATUS-OK                    
116800               PERFORM UNTIL SEGMENT-SAKNAS OR STATUS-OK                  
116900                 IF RAD-KDINLSTA = 'FPK' OR 'SAK' OR SPACE                
117000                   MOVE JA               TO STATUS-SW                     
117100                 END-IF                                                   
117200                 PERFORM IMS-GNP-INLC-D121                                
117300               END-PERFORM                                                
117400               PERFORM IMS-GN-INLC-D111                                   
117500             END-PERFORM                                                  
117600             IF STATUS-NEJ                                                
117700*-FEL   - 007                                                             
117800                MOVE NEJ                    TO INDATA-SW                  
117900                MOVE ERR-UPDATE-NOT-VALID   TO MED-IDMFSFEL               
118000                MOVE MFS-ALFA-FAELT-FEL                                   
118100                     TO MOD-IDLEVNR-ATTR(IX)                              
118110                MOVE MFS-NUM-FAELT-FEL                                    
118200                     TO MOD-IDOKOLLI-ATTR(IX)                             
118300                PERFORM S01-CALL-WMEDKONV-FEL                             
118400             END-IF                                                       
118500          END-IF                                                          
118510        END-IF                                                            
118600     END-IF                                                               
118700     .                                                                    
118800     EJECT                                                                
118900******************************************************************        
119000*  UPPDATERA REGISTER                                            *        
119100******************************************************************        
119300 H-UPPDATERA SECTION.                                                     
119400                                                                          
119600     MOVE SPACE          TO SPAR-ADINLOMR                                 
119700                            SPAR-ADINLOMR-NXT                             
119800                            SPAR-KDINLSTA-NEW                             
119900                            SPAR-KDINLSTA-OLD                             
120000     MOVE ZERO           TO T91-MID-KVPOST                                
120100     MOVE NEJ            TO TRANS91-SW                                    
120200     MOVE ZERO           TO IX                                            
120300     ADD  1              TO IX                                            
120400     MOVE ZERO           TO T91-IX                                        
120500                                                                          
120600     IF MID-ADINLOMR-UT NOT = SPACE AND LOW-VALUE                         
120700       MOVE MID-ADINLOMR-UT TO W-ADINLOMR                                 
120800       PERFORM IMS-GU-PLAA-W6G130                                         
120900       IF ALT-6006-KDINLOMR = 'F' OR 'FBP'                                
121000         MOVE JA TO FP-SW                                                 
121100       ELSE                                                               
121200         IF MID-ADINLOMR-NXT-UT NOT = SPACE AND LOW-VALUE                 
121300           MOVE MID-ADINLOMR-NXT-UT TO W-ADINLOMR                         
121400           PERFORM IMS-GU-PLAA-W6G130                                     
121500           IF ALT-6006-KDINLOMR = 'F' OR 'FBP'                            
121600             MOVE JA TO FP-SW                                             
121700           END-IF                                                         
121800         END-IF                                                           
121900       END-IF                                                             
122000     END-IF                                                               
122100                                                                          
122200     PERFORM UNTIL IX > MAX-IX                                            
122300        IF (MID-IDLEVNR(IX) NOT = ALL '+') AND                            
122500           (MID-IDOKOLLI(IX) NOT = ALL '+' AND                            
122600           MID-IDOKOLLI(IX) > ZERO)                                       
122700           PERFORM HA-UPD-RAD                                             
122800           MOVE SPACE    TO SPAR-ADINLOMR                                 
122900                            SPAR-ADINLOMR-NXT                             
123000                            SPAR-KDINLSTA-NEW                             
123100                            SPAR-KDINLSTA-OLD                             
123200        END-IF                                                            
123300        ADD 1  TO IX                                                      
123400     END-PERFORM                                                          
123500                                                                          
123600     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
123700     PERFORM S02-CALL-WMEDKONV-INF                                        
123800     PERFORM MFS-RENSA-FAELT-UT                                           
123900                                                                          
124000     IF TRANS91-JA                                                        
124100*-------SKICKA TRANS TILL MPP MED ART-ANT-LEV-KOLLI                       
124200        PERFORM S45-SKICKA-W60191                                         
124300     END-IF                                                               
124400                                                                          
124500     IF 6197-IX > ZERO                                                    
124600*-------SKICKA TRANS TILL MPP MED ART-ANT-LEV-KOLLI                       
124700        PERFORM S11-STARTA-6197-TRANS                                     
124800     END-IF                                                               
124900     .                                                                    
125000     EJECT                                                                
125100*----------------------------------------------------------------*        
125200 HA-UPD-RAD     SECTION.                                                  
125300                                                                          
125400     PERFORM HAA-LAES-INLC                                                
125500                                                                          
125600     IF BEARBTYP1                                                         
125700        PERFORM HAC-UPD-LEV-KOLLI-RAD                                     
125800     ELSE                                                                 
125900        PERFORM HAB-UPD-LEV-KOLLI-ING-ART                                 
126000     END-IF                                                               
126100     .                                                                    
126200     EJECT                                                                
126300*----------------------------------------------------------------*        
126400 HAA-LAES-INLC              SECTION.                                      
126500                                                                          
126600                                                                          
126700*----LÄSNING FÖR ATT UPPDATERA RAD SKER VIA                               
126800*    IDLEVNR + IDOKOLLI  (INDEX C ANVÄNDS)                                
126900                                                                          
127000     MOVE MID-IDLEVNR(IX)      TO WC-IDLEVNR                              
127100     MOVE MID-IDOKOLLI(IX)     TO WC-IDOKOLLI                             
127200                                                                          
127300     PERFORM IMS-GU-INLC-D111                                             
127400                                                                          
127500     IF SEGMENT-FINNS                                                     
127600        MOVE ART-IDLOPNRM            TO SPAR-IDLOPNRM                     
127700        MOVE ART-PRARTSTD            TO SPAR-PRARTSTD                     
127800        MOVE ART-BEFT                TO WS-BEFT                           
127900        MOVE ART-IDARTNR             TO WS-IDARTNR                        
128000        MOVE ART-ADLAGOMR            TO SPAR-ADLAGOMR                     
128100        MOVE ART-ADGANG              TO SPAR-ADGANG                       
128200        MOVE ART-ADPLATS             TO SPAR-ADPLATS                      
128300        IF ART-KVAVIS-PRIO > ZERO                                         
128400          MOVE JA TO PRIOGODS-SW                                          
128500        END-IF                                                            
128600        MOVE MID-IDLEVNR(IX)   TO S-IDLEVNRK                              
128700        MOVE MID-IDOKOLLI(IX)  TO S-IDOKOLLI                              
128800        PERFORM IMS-GHNP-INLC-D121                                        
128900                                                                          
129000        IF SEGMENT-FINNS                                                  
129100           IF RAD-FLDIVKLI = NEJ                                          
129200*------------ UPPDATERA LEV-KOLLI RAD                                     
129300              MOVE '1'               TO BEARBTYP-SW                       
129400           ELSE                                                           
129500*------------ UPPDATERA ALLA INGÅENDE ARTIKLAR                            
129600              MOVE '2'               TO BEARBTYP-SW                       
129700           END-IF                                                         
129800        END-IF                                                            
129900     END-IF                                                               
130000     .                                                                    
130100     EJECT                                                                
130200*----------------------------------------------------------------*        
130300 HAB-UPD-LEV-KOLLI-ING-ART  SECTION.                                      
130400                                                                          
130500     PERFORM UNTIL SEGMENT-SAKNAS                                         
130600        PERFORM HABA-UPD-LEV-KOLLI-RADER                                  
130700        PERFORM IMS-GN-INLC-D111                                          
130800        IF SEGMENT-FINNS                                                  
130900            MOVE ART-IDLOPNRM            TO SPAR-IDLOPNRM                 
131000            MOVE ART-PRARTSTD            TO SPAR-PRARTSTD                 
131100            MOVE ART-BEFT                TO WS-BEFT                       
131200            MOVE ART-IDARTNR             TO WS-IDARTNR                    
131300            MOVE ART-ADLAGOMR            TO SPAR-ADLAGOMR                 
131400            MOVE ART-ADGANG              TO SPAR-ADGANG                   
131500            MOVE ART-ADPLATS             TO SPAR-ADPLATS                  
131600            IF ART-KVAVIS-PRIO > ZERO                                     
131700              MOVE JA TO PRIOGODS-SW                                      
131800            END-IF                                                        
131900           PERFORM IMS-GHNP-INLC-D121                                     
132000        END-IF                                                            
132100     END-PERFORM                                                          
132200     .                                                                    
132300     EJECT                                                                
132400*----------------------------------------------------------------*        
132500 HABA-UPD-LEV-KOLLI-RADER  SECTION.                                       
132600                                                                          
132700     IF SEGMENT-FINNS                                                     
132800        IF (RAD-KDINLSTA = 'FPK' OR SPACE)                                
132900          PERFORM UNTIL SEGMENT-SAKNAS                                    
133000           MOVE RAD-ADINLOMR          TO SPAR-ADINLOMR                    
133100           MOVE RAD-ADINLOMR-NXT      TO SPAR-ADINLOMR-NXT                
133200           MOVE MID-SPAR-IDINLVGN     TO RAD-IDINLVGN                     
133300           MOVE MID-SPAR-ADINLOMR     TO RAD-ADINLOMR                     
133400           MOVE MID-SPAR-ADINLOMR-NXT TO RAD-ADINLOMR-NXT                 
133500                                                                          
133600           PERFORM IMS-REPL-INLC                                          
133700                                                                          
133800*----------KOLLA OM UPPFÖLJNING SKALL SKE PER TORG                        
133900           IF RAD-ADINLOMR = '10  ' AND RAD-ADINLOMR-NXT = SPACE          
134000             PERFORM S03-KOLLA-TORG                                       
134100           ELSE                                                           
134200             MOVE SPACE TO TORG-ADINLOMR                                  
134300           END-IF                                                         
134400*----------INITIERA TILL TRANS BAKGRUNDS-MPP ' W60191'                    
134500           PERFORM S40-TRANS-W60191                                       
134501           PERFORM IMS-GHNP-INLC-D121                                     
134510          END-PERFORM                                                     
134600        END-IF                                                            
134700     END-IF                                                               
134800     .                                                                    
134900     EJECT                                                                
135000*----------------------------------------------------------------*        
135100 HAC-UPD-LEV-KOLLI-RAD SECTION.                                           
135200                                                                          
135300     IF SEGMENT-FINNS                                                     
135400                                                                          
135500        MOVE RAD-ADINLOMR           TO SPAR-ADINLOMR                      
135600        MOVE RAD-ADINLOMR-NXT       TO SPAR-ADINLOMR-NXT                  
135700        MOVE MID-SPAR-IDINLVGN      TO RAD-IDINLVGN                       
135800        MOVE MID-SPAR-ADINLOMR      TO RAD-ADINLOMR                       
135900        MOVE MID-SPAR-ADINLOMR-NXT  TO RAD-ADINLOMR-NXT                   
136000        MOVE NEJ                    TO RAD-FLINLFB                        
136010                                       RAD-FLINLFP                        
136100                                                                          
136200        IF RAD-KDINLSTA = 'SAK'                                           
136300           MOVE RAD-KDINLSTA        TO SPAR-KDINLSTA-OLD                  
136400           MOVE SPACE               TO RAD-KDINLSTA                       
136500           MOVE SPACE               TO SPAR-KDINLSTA-NEW                  
136600        END-IF                                                            
136700                                                                          
136800        PERFORM IMS-REPL-INLC                                             
136900                                                                          
137000        IF RAD-ADINLOMR = '10  ' AND RAD-ADINLOMR-NXT = SPACE             
137100          PERFORM S03-KOLLA-TORG                                          
137200        ELSE                                                              
137300          MOVE SPACE TO TORG-ADINLOMR                                     
137400        END-IF                                                            
137500                                                                          
137600        PERFORM S40-TRANS-W60191                                          
137700     END-IF                                                               
137800     .                                                                    
137900     EJECT                                                                
138000******************************************************************        
138100*    MFS-REDIGERING AV BILDENS FÄLT                              *        
138200******************************************************************        
138400 MFS-RENSA-FAELT-UT SECTION.                                              
138500                                                                          
138600*--- RENSA INDEXERADE RADER                                               
138700                                                                          
138800     MOVE +1 TO IX                                                        
138900     PERFORM UNTIL IX > MAX-IX                                            
139000        PERFORM MFS-RENSA-RAD-FAELT-UT                                    
139100        ADD +1 TO IX                                                      
139200     END-PERFORM                                                          
139300     MOVE ZERO               TO IX                                        
139400     ADD  1                  TO IX                                        
139500     .                                                                    
139600     SKIP2                                                                
139800*----------------------------------------------------------------*        
139900 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
140000                                                                          
140100*--- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                       
140200                                                                          
140300     MOVE MFS-RENSA-FAELT    TO MOD-IDLEVNR(IX)                           
140400                                MOD-IDOKOLLI(IX)                          
140500     .                                                                    
140700     SKIP2                                                                
140800*----------------------------------------------------------------*        
140900 MFS-RENSA-FAELT-IN SECTION.                                              
141000                                                                          
141100*--- ALLA IN-FÄLT                                                         
141200     MOVE MFS-RENSA-FAELT TO MOD-IDINLVGN-IN                              
141300                             MOD-ADINLOMR-IN                              
141400                             MOD-ADINLOMR-NXT-IN                          
141500                             MOD-IDDC-IN                                  
141510     .                                                                    
141700     SKIP2                                                                
141800*----------------------------------------------------------------*        
141900 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
142000                                                                          
142100*--- INDEXERADE RADER                                                     
142200                                                                          
142300     MOVE +1 TO IX                                                        
142400     PERFORM UNTIL IX > MAX-IX                                            
142500        PERFORM MFS-ROER-EJ-RAD-FAELT-UT                                  
142600        ADD +1 TO IX                                                      
142700     END-PERFORM                                                          
142800     .                                                                    
142900     SKIP2                                                                
143100*----------------------------------------------------------------*        
143200 MFS-ROER-EJ-RAD-FAELT-UT  SECTION.                                       
143300                                                                          
143400*--- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                       
143500                                                                          
143600     MOVE MFS-ROER-EJ-FAELT   TO MOD-IDLEVNR(IX)                          
143700                                 MOD-IDOKOLLI(IX)                         
143800     .                                                                    
144000     SKIP2                                                                
144100*----------------------------------------------------------------*        
144200 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
144300                                                                          
144400*--- ALLA IN-FÄLT                                                         
144500                                                                          
144600     MOVE MFS-ROER-EJ-FAELT  TO MOD-IDINLVGN-IN                           
144700                                MOD-ADINLOMR-IN                           
144800                                MOD-ADINLOMR-NXT-IN                       
144810                                MOD-IDDC-IN                               
144900     .                                                                    
145100     SKIP2                                                                
145200*----------------------------------------------------------------*        
145300 MFS-LAES-IN-IGEN SECTION.                                                
145400                                                                          
145500*--- INDEXERADE RADER                                                     
145600                                                                          
145700     MOVE +1 TO IX                                                        
145800     PERFORM UNTIL IX > MAX-IX                                            
145900        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDLEVNR-ATTR(IX)                
146000                                      MOD-IDOKOLLI-ATTR(IX)               
146100        ADD +1 TO IX                                                      
146200     END-PERFORM                                                          
146300                                                                          
146400     .                                                                    
146500     EJECT                                                                
172300******************************************************************        
172400*  INFO/FELMEDDELANDE                                            *        
172500******************************************************************        
172700 S01-CALL-WMEDKONV-FEL SECTION.                                           
172800                                                                          
172900     CALL WMEDKONV USING MED-WMEDAREA                                     
173000     MOVE MED-MFSFEL TO MOD-TEMFSFEL                                      
173100     .                                                                    
173300     SKIP3                                                                
173400*----------------------------------------------------------------*        
173500 S02-CALL-WMEDKONV-INF SECTION.                                           
173600                                                                          
173700     CALL WMEDKONV USING MED-WMEDAREA                                     
173800     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
173900     .                                                                    
174000     EJECT                                                                
174100 S03-KOLLA-TORG SECTION.                                                  
174300     MOVE SPACE TO TORG-ADINLOMR                                          
174400     MOVE NEJ   TO TRAEFF-SW                                              
174500     MOVE RAD-ADINLOMR TO W-ADINLOMR-PAR                                  
174600     PERFORM IMS-GU-PLAA-W6G101                                           
174700     PERFORM IMS-GNP-PLAA-PAR                                             
174800     PERFORM UNTIL SEGMENT-SAKNAS OR TRAEFF                               
174900       IF ALT-6006-ADGANG-TOM > +0 OR                                     
175000          ALT-6006-ADPLATS-TOM > +0                                       
175100          IF ALT-6006-ADGANG-TOM > +0                                     
175200            IF ALT-6006-ADGANG-FOM <= SPAR-ADGANG AND                     
175300               ALT-6006-ADGANG-TOM >= SPAR-ADGANG                         
175400              MOVE ALT-6006-ADINLOMR TO TORG-ADINLOMR                     
175500              MOVE JA                 TO TRAEFF-SW                        
175600            END-IF                                                        
175700          ELSE                                                            
175800            IF ALT-6006-ADPLATS-FOM <= SPAR-ADPLATS AND                   
175900               ALT-6006-ADPLATS-TOM >= SPAR-ADPLATS                       
176000              MOVE ALT-6006-ADINLOMR TO TORG-ADINLOMR                     
176100              MOVE JA                 TO TRAEFF-SW                        
176200            END-IF                                                        
176300          END-IF                                                          
176400       END-IF                                                             
176500       PERFORM IMS-GNP-PLAA-PAR                                           
176600     END-PERFORM                                                          
176700     .                                                                    
176800     EJECT                                                                
176900 S11-STARTA-6197-TRANS  SECTION.                                          
177000                                                                          
177100     MOVE SPACE                 TO 6197-MID-IDPRTLST                      
177200     MOVE '6L'                  TO 6197-MID-IDPRTLST(1:2)                 
177300     MOVE 'TR  '                TO 6197-MID-IDPRTLST(3:4)                 
177400     MOVE IDPGM                 TO 6197-MID-IDPGM                         
177500     MOVE 6197-IX               TO 6197-MID-KVPOST                        
177510     MOVE 'N'                   TO 6197-MID-FLSVS                         
177600     COMPUTE P-TO-P-MSG-KVLL        = P-TO-P-PREFIX-LNG +                 
177700                                  26 + (6197-MID-KVPOST * 24)             
177800     MOVE 'W6T197X '           TO P-TO-P-MSG-KDTRANS                      
177900     MOVE '6102'               TO P-TO-P-MSG-IDTRANS                      
178000     MOVE MFS-KDMFSFOR         TO P-TO-P-MSG-KDMFSFOR                     
178100     MOVE 6197-MID-W6I19701 TO P-TO-P-MSG-INDATA                          
178200     IF FOERSTA-6197                                                      
178300         PERFORM IMS-ISRT-ALT-MSG-6197                                    
178400         MOVE NEJ              TO FOERSTA-6197-SW                         
178500      ELSE                                                                
178600         PERFORM IMS-PURG-ALT-MSG-6197                                    
178700     END-IF                                                               
178800     MOVE ZERO                 TO 6197-IX                                 
178900     .                                                                    
179000     EJECT                                                                
179100******************************************************************        
179200*  INITERA NKL:AR                                                *        
179300******************************************************************        
179600 S40-TRANS-W60191      SECTION.                                           
179700                                                                          
179800     IF T91-MID-KVPOST = ZERO                                             
179900        MOVE JA                  TO TRANS91-SW                            
180000        MOVE SPACE               TO T91-MID-W6I19101                      
180100        MOVE +1                  TO T91-MID-KVPOST                        
180200                                                                          
180300        MOVE IDPGM               TO T91-MID-IDPGM                         
180310        MOVE DCS-IDDC            TO T91-MID-IDDC                          
180400                                                                          
180500        MOVE SPAR-IDLOPNRM         TO T91-MID-IDLOPNRM(1)                 
180600        MOVE SPAR-PRARTSTD         TO T91-MID-PRARTSTD(1)                 
180700        MOVE RAD-IDRADNR           TO T91-MID-IDRADNR(1)                  
180800        MOVE RAD-KDINLPRIO         TO T91-MID-KDINLPRIO(1)                
180900        MOVE +0                    TO T91-MID-KVKOLLI  (1)                
181000        MOVE 'N'                   TO T91-MID-FLINLI   (1)                
181100        MOVE SPAR-ADINLOMR         TO T91-MID-ADINLOMR-OLD(1)             
181200        MOVE RAD-ADINLOMR          TO T91-MID-ADINLOMR-NEW(1)             
181300        MOVE SPAR-ADINLOMR-NXT     TO T91-MID-ADINLOMR-NXT-OLD(1)         
181400        MOVE RAD-ADINLOMR-NXT      TO T91-MID-ADINLOMR-NXT-NEW(1)         
181500        MOVE RAD-KVINLART          TO T91-MID-KVINLART-OLD(1)             
181600        MOVE RAD-KVINLART          TO T91-MID-KVINLART-NEW(1)             
181700        MOVE SPAR-KDINLSTA-OLD     TO T91-MID-KDINLSTA-OLD(1)             
181800        MOVE SPAR-KDINLSTA-NEW     TO T91-MID-KDINLSTA-NEW(1)             
181900        MOVE +1                    TO IX1                                 
182000     ELSE                                                                 
182100        COMPUTE IX1 = T91-MID-KVPOST + 1                                  
182200        ADD 1                         TO T91-MID-KVPOST                   
182300                                                                          
182400        MOVE SPAR-IDLOPNRM         TO T91-MID-IDLOPNRM(IX1)               
182500        MOVE SPAR-PRARTSTD         TO T91-MID-PRARTSTD(IX1)               
182600        MOVE RAD-IDRADNR           TO T91-MID-IDRADNR(IX1)                
182700        MOVE RAD-KDINLPRIO         TO T91-MID-KDINLPRIO(IX1)              
182800        MOVE +0                    TO T91-MID-KVKOLLI  (IX1)              
182900        MOVE 'N'                   TO T91-MID-FLINLI   (IX1)              
183000        MOVE SPAR-ADINLOMR         TO T91-MID-ADINLOMR-OLD(IX1)           
183100        MOVE RAD-ADINLOMR          TO T91-MID-ADINLOMR-NEW(IX1)           
183200        MOVE SPAR-ADINLOMR-NXT                                            
183300             TO T91-MID-ADINLOMR-NXT-OLD(IX1)                             
183400        MOVE RAD-ADINLOMR-NXT                                             
183500             TO T91-MID-ADINLOMR-NXT-NEW(IX1)                             
183600        MOVE RAD-KVINLART          TO T91-MID-KVINLART-OLD(IX1)           
183700        MOVE RAD-KVINLART          TO T91-MID-KVINLART-NEW(IX1)           
183800        MOVE SPAR-KDINLSTA-OLD     TO T91-MID-KDINLSTA-OLD(IX1)           
183900        MOVE SPAR-KDINLSTA-NEW     TO T91-MID-KDINLSTA-NEW(IX1)           
184000     END-IF                                                               
184100                                                                          
184200     IF T91-MID-KVPOST = 24                                               
184300        PERFORM S45-SKICKA-W60191                                         
184400        MOVE ZERO TO T91-MID-KVPOST                                       
184500        MOVE NEJ  TO TRANS91-SW                                           
184600        MOVE JA   TO PURGE91-SW                                           
184700     END-IF                                                               
184800     .                                                                    
184900     EJECT                                                                
185000*----------------------------------------------------------------*        
185100 S45-SKICKA-W60191 SECTION.                                               
185200                                                                          
185300     COMPUTE W-PTOP1-OCC-LL = T91-MID-KVPOST * 64                         
185400     COMPUTE PTOP1-LL = W-PTOP1-OCC-LL + 35                               
185500     MOVE MFS-KDMFSFOR         TO PTOP1-KDMFSFOR                          
185600                                                                          
185700     IF PURGE-T91                                                         
185800       PERFORM IMS-PURG-MSG-ALT1-6191                                     
185900     ELSE                                                                 
186000       PERFORM IMS-ISRT-MSG-ALT1-6191                                     
186100     END-IF                                                               
186200     .                                                                    
186300     EJECT                                                                
186301                                                                          
186310 S50-PRIM-CONTROL SECTION.                                                
186320* THIS IS A CONTROL TO CHECK THE OLD PLACE IF FLAG FLKNTRGK = YES         
186330* IF THE FLAG IS YES WE HAVE TO DO A CHECK ON THE OLD PLACE BEFORE        
186340* WE CHECK THE NEW PLACE.                                                 
186350     MOVE RAD-ADINLOMR         TO W-ADINLOMR-KON                          
186360     PERFORM IMS-GU-PLAA-W6G130-KONTROLL                                  
186370     IF SEGMENT-FINNS                                                     
186380       IF ALT-6006-FLKNTRGK = JA                                          
186390         MOVE MID-ADINLOMR-UT   TO W-ADINLOMR-KON                         
186391         PERFORM IMS-GU-PLAA-W6G130-KONTROLL                              
186392         IF (ALT-6006-FLKNTRGK = JA )                                     
186393         OR ALT-6006-KDINLOMR = 'LPL'                                     
186394           MOVE 'N' TO PRIM-CONTROL-SW                                    
186395         ELSE                                                             
186396           MOVE 'J' TO PRIM-CONTROL-SW                                    
186397         END-IF                                                           
186398       ELSE                                                               
186399         MOVE 'N' TO PRIM-CONTROL-SW                                      
186400       END-IF                                                             
186401     ELSE                                                                 
186402       MOVE 'N' TO PRIM-CONTROL-SW                                        
186403     END-IF                                                               
186404                                                                          
186405     IF PRIM-CONTROL-YES                                                  
186406       PERFORM S51-CHECK-OLD-PLACE                                        
186407     END-IF                                                               
186418     .                                                                    
186419     EJECT                                                                
186420                                                                          
186421 S51-CHECK-OLD-PLACE     SECTION.                                         
186422     MOVE ART-IDLOPNRM  TO W-IDLOPNRM                                     
186423     PERFORM IMS-GU-UPFA01                                                
186424     IF SEGMENT-FINNS                                                     
186425       IF UPPF-KVKVAPRIM > ZERO                                           
186426         IF UPPF-KDKVASTA-PRI = '2' OR '3'                                
186427           CONTINUE                                                       
186428         ELSE                                                             
186429           MOVE '605' TO MED-IDMFSFEL                                     
186430           MOVE NEJ TO INDATA-SW                                          
186431           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDLEVNR-ATTR(IX)                
186432           MOVE MFS-NUM-FAELT-FEL  TO MOD-IDOKOLLI-ATTR(IX)               
186437         END-IF                                                           
186438       END-IF                                                             
186439       IF UPPF-KVKVASEK > ZERO                                            
186440         IF UPPF-KDKVASTA-PRI = '2' OR '3'                                
186441           CONTINUE                                                       
186442         ELSE                                                             
186443           MOVE '605' TO MED-IDMFSFEL                                     
186444           MOVE NEJ TO INDATA-SW                                          
186445           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDLEVNR-ATTR(IX)                
186446           MOVE MFS-NUM-FAELT-FEL  TO MOD-IDOKOLLI-ATTR(IX)               
186449         END-IF                                                           
186450       END-IF                                                             
186451     END-IF                                                               
186452                                                                          
186453     IF INDATA-OK                                                         
186454       PERFORM IMS-GU-UPFA01                                              
186455       IF SEGMENT-FINNS                                                   
186456         PERFORM IMS-GNP-UPFA11                                           
186457         PERFORM UNTIL SEGMENT-SAKNAS                                     
186458           IF RAPP-KDKVASTA-PRI = '2' OR '3'                              
186459             CONTINUE                                                     
186460           ELSE                                                           
186461             MOVE '605' TO MED-IDMFSFEL                                   
186462             MOVE NEJ TO INDATA-SW                                        
186463             MOVE MFS-ALFA-FAELT-FEL TO MOD-IDLEVNR-ATTR(IX)              
186464             MOVE MFS-NUM-FAELT-FEL  TO MOD-IDOKOLLI-ATTR(IX)             
186467           END-IF                                                         
186468           PERFORM IMS-GNP-UPFA11                                         
186469         END-PERFORM                                                      
186470       END-IF                                                             
186471     END-IF                                                               
186472                                                                          
186473     IF INDATA-OK                                                         
186474       PERFORM IMS-GU-UPFA01                                              
186475       IF SEGMENT-FINNS                                                   
186476         PERFORM IMS-GNP-UPFA12                                           
186477         PERFORM UNTIL SEGMENT-SAKNAS                                     
186478           IF SPEC-KDKVASTA-PRI = '2' OR '3'                              
186479             CONTINUE                                                     
186480           ELSE                                                           
186481             MOVE '605' TO MED-IDMFSFEL                                   
186482             MOVE NEJ TO INDATA-SW                                        
186483             MOVE MFS-ALFA-FAELT-FEL TO MOD-IDLEVNR-ATTR(IX)              
186484             MOVE MFS-NUM-FAELT-FEL  TO MOD-IDOKOLLI-ATTR(IX)             
186487           END-IF                                                         
186488           PERFORM IMS-GNP-UPFA12                                         
186489         END-PERFORM                                                      
186490       END-IF                                                             
186491     END-IF                                                               
186492                                                                          
186493     IF MED-IDMFSFEL = '605'                                              
186494       CALL WMEDKONV USING MED-WMEDAREA                                   
186495       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
186500     END-IF                                                               
186501     .                                                                    
186502     EJECT                                                                
186503                                                                          
186504******************************************************************        
186510*  BILD-REDIGERING                                               *        
186600******************************************************************        
186800 S90-BLANKUTF-NUM-FAELT SECTION.                                          
186900                                                                          
187000*----NKL-FÄLT (HUV-FÄLT)                                                  
187100     INSPECT MOD-IDINLVGN-UT REPLACING LEADING ZERO BY SPACE              
187200                                                                          
187300*----RAD-FÄLT                                                             
187400*--- INDEXERADE RADER                                                     
187500                                                                          
187600     MOVE +1 TO IX                                                        
187700     PERFORM UNTIL IX > MAX-IX                                            
187800        IF NYCKLAR-OK AND INDATA-OK AND                                   
187900           (GODK-MID OR HELP-MID)                                         
188000           MOVE MFS-OEPPNA-ALFA-FAELT TO MOD-IDLEVNR-ATTR(IX)             
188100           MOVE MFS-OEPPNA-NUM-FAELT  TO MOD-IDOKOLLI-ATTR(IX)            
188200        ELSE                                                              
188300           IF INDATA-FEL                                                  
188400              CONTINUE                                                    
188500           ELSE                                                           
188600              MOVE MFS-STAENG-FAELT  TO MOD-IDLEVNR-ATTR(IX)              
188700                                        MOD-IDOKOLLI-ATTR(IX)             
188800           END-IF                                                         
188900        END-IF                                                            
189000        ADD +1 TO IX                                                      
189100     END-PERFORM                                                          
189200     .                                                                    
189400                                                                          
189500******************************************************************        
189600*  IMS-SECTIONER                                                 *        
189700******************************************************************        
189800 IMS-GET-MSG SECTION.                                                     
189900                                                                          
190000     MOVE '  QC' TO GODK-STATUSKODER                                      
190100     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
190200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
190300     PERFORM IMS-STATUSKONTROLL                                           
190400     .                                                                    
190500     SKIP3                                                                
190600*----------------------------------------------------------------*        
190700 IMS-INSERT-MSG SECTION.                                                  
190800                                                                          
190900     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
191000       MOVE '0' TO MFS-KDHUVOMR                                           
191100     END-IF                                                               
191200     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
191300     MOVE SPACE TO GODK-STATUSKODER                                       
191400     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
191500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
191600     PERFORM IMS-STATUSKONTROLL                                           
191700     .                                                                    
191800     SKIP3                                                                
191900******************************************************************        
192000*    ALT1-PCB  (TRANS W60191)                                    *        
192100******************************************************************        
192200 IMS-ISRT-MSG-ALT1-6191 SECTION.                                          
192300                                                                          
192400     MOVE SPACE              TO GODK-STATUSKODER                          
192500     CALL CBLTDLI USING      ISRT ALT1-PCB                                
192600                                  P-TO-P-T91                              
192700     MOVE ALT1-STATUS-CODE   TO STATUS-WS                                 
192800     PERFORM IMS-STATUSKONTROLL                                           
192900     .                                                                    
193000     SKIP3                                                                
193100 IMS-PURG-MSG-ALT1-6191 SECTION.                                          
193200                                                                          
193300     MOVE SPACE              TO GODK-STATUSKODER                          
193400     CALL CBLTDLI USING      PURG ALT1-PCB                                
193500                                  P-TO-P-T91                              
193600     MOVE ALT1-STATUS-CODE   TO STATUS-WS                                 
193700     PERFORM IMS-STATUSKONTROLL                                           
193800     .                                                                    
193900     EJECT                                                                
194000******************************************************************        
194100*    ALT2-PCB  (TRANS W60197)                                    *        
194200******************************************************************        
194300 IMS-ISRT-ALT-MSG-6197  SECTION.                                          
194400     MOVE SPACE TO GODK-STATUSKODER                                       
194500     CALL  CBLTDLI  USING ISRT 6197-PCB P-TO-P-MSG-IO-AREA-SNUF           
194600     MOVE 6197-STATUS-CODE TO STATUS-WS                                   
194700     PERFORM IMS-STATUSKONTROLL                                           
194800     .                                                                    
194900     SKIP3                                                                
195000 IMS-PURG-ALT-MSG-6197  SECTION.                                          
195100     MOVE SPACE TO GODK-STATUSKODER                                       
195200     CALL  CBLTDLI  USING PURG 6197-PCB P-TO-P-MSG-IO-AREA-SNUF           
195300     MOVE 6197-STATUS-CODE TO STATUS-WS                                   
195400     PERFORM IMS-STATUSKONTROLL                                           
195500     .                                                                    
195600     EJECT                                                                
195700******************************************************************        
195800*    INLC-PCB                                                    *        
195900******************************************************************        
196000 IMS-GU-INLC-D111  SECTION.                                               
196100     STRING 'W6INLA11(W6D1CSEQ =' W-W6D1C1KY-X                            
196200                    '&IDDC     =' W-IDDC-X ')'                            
196300             DELIMITED BY SIZE INTO SSA1                                  
196400     MOVE '  GE'                 TO GODK-STATUSKODER                      
196500     CALL CBLTDLI USING GU       INLC-PCB                                 
196600                                 DLI-IO-AREA-W6D111                       
196700                                 SSA1                                     
196800     MOVE INLC-STATUS-CODE       TO STATUS-WS                             
196900     PERFORM IMS-STATUSKONTROLL                                           
197000     .                                                                    
197100     SKIP3                                                                
197200*----------------------------------------------------------------*        
197300 IMS-GN-INLC-D111  SECTION.                                               
197400     STRING 'W6INLA11(W6D1CSEQ =' W-W6D1C1KY-X                            
197500                    '&IDDC     =' W-IDDC-X ')'                            
197600             DELIMITED BY SIZE INTO SSA1                                  
197700     MOVE '  GE'                 TO GODK-STATUSKODER                      
197800     CALL CBLTDLI USING GN       INLC-PCB                                 
197900                                 DLI-IO-AREA-W6D111                       
198000                                 SSA1                                     
198100     MOVE INLC-STATUS-CODE       TO STATUS-WS                             
198200     PERFORM IMS-STATUSKONTROLL                                           
198300     .                                                                    
198400     SKIP3                                                                
198500*----------------------------------------------------------------*        
198600 IMS-GNP-INLC-D121    SECTION.                                            
198700     STRING 'W6INLA11(W6D1CSEQ =' W-W6D1C1KY-X                            
198800                    '&IDDC     =' W-IDDC-X ')'                            
198900          DELIMITED BY SIZE INTO SSA1                                     
199000     STRING 'W6INLA21(IDLEVNRK =' W-IDLEVNRK-X                            
199100                    '&IDOKOLLI =' W-IDOKOLLI-X ')'                        
199200          DELIMITED BY SIZE INTO SSA2                                     
199300     MOVE '  GE'            TO GODK-STATUSKODER                           
199400     CALL CBLTDLI USING GNP INLC-PCB                                      
199500                            DLI-IO-AREA-W6D121                            
199600                            SSA1                                          
199700                            SSA2                                          
199800     MOVE INLC-STATUS-CODE  TO STATUS-WS                                  
199900     PERFORM IMS-STATUSKONTROLL                                           
200000     .                                                                    
200100     EJECT                                                                
200200******************************************************************        
200300*    IMS-UPPDATERING VIA INLC-PCB                                *        
200400******************************************************************        
200500 IMS-GHNP-INLC-D121        SECTION.                                       
200600     STRING 'W6INLA11(W6D1CSEQ =' W-W6D1C1KY-X                            
200700                    '&IDDC     =' W-IDDC-X ')'                            
200800          DELIMITED BY SIZE INTO SSA1                                     
200900     STRING 'W6INLA21(IDLEVNRK =' W-IDLEVNRK-X                            
201000                    '&IDOKOLLI =' W-IDOKOLLI-X ')'                        
201100          DELIMITED BY SIZE INTO SSA2                                     
201200     MOVE '  GE'            TO GODK-STATUSKODER                           
201300     CALL CBLTDLI USING GHNP INLC-PCB                                     
201400                            DLI-IO-AREA-W6D121                            
201500                            SSA1                                          
201600                            SSA2                                          
201700     MOVE INLC-STATUS-CODE  TO STATUS-WS                                  
201800     PERFORM IMS-STATUSKONTROLL                                           
201900     .                                                                    
202000     SKIP3                                                                
202100*----------------------------------------------------------------*        
202200 IMS-REPL-INLC SECTION.                                                   
202300                                                                          
202400     MOVE '  '               TO GODK-STATUSKODER                          
202500     CALL CBLTDLI USING REPL INLC-PCB                                     
202600                             DLI-IO-AREA-W6D121                           
202700     MOVE INLC-STATUS-CODE   TO STATUS-WS                                 
202800     PERFORM IMS-STATUSKONTROLL                                           
202900     .                                                                    
203000     SKIP3                                                                
203100******************************************************************        
203200*    INLA-PCB                                                    *        
203300******************************************************************        
203400 IMS-GU-INLA-D121  SECTION.                                               
203500     STRING 'W6INLA11(W6D1FSEQ =' W-W6D1FSEQ-X                            
203600                    '&IDDC     =' W-IDDC-X ')'                            
203700             DELIMITED BY SIZE INTO SSA1                                  
203800     STRING 'W6INLA21(IDINLVGN =' W-IDINLVGN-X ')'                        
203900             DELIMITED BY SIZE INTO SSA2                                  
204000     MOVE '  GE'                 TO GODK-STATUSKODER                      
204100     CALL CBLTDLI USING GU       INLA-PCB                                 
204200                                 DLI-IO-AREA-W6D121                       
204300                                 SSA1                                     
204400                                 SSA2                                     
204500     MOVE INLA-STATUS-CODE      TO STATUS-WS                              
204600     PERFORM IMS-STATUSKONTROLL                                           
204700     .                                                                    
204800     EJECT                                                                
204900******************************************************************        
205000*    PLAA-PCB                                                    *        
205100******************************************************************        
205200 IMS-GU-PLAA-G111 SECTION.                                                
205300     STRING 'W6PLAA01(W6GXKEY  =' W-W6GX01KEY-X ')'                       
205400          DELIMITED BY SIZE INTO SSA1                                     
205500     STRING 'W6PLAA11(W6GXKEY  =' W-W6GX11KEY-X ')'                       
205600          DELIMITED BY SIZE INTO SSA2                                     
205700     MOVE '  GE'            TO GODK-STATUSKODER                           
205800     CALL CBLTDLI USING GU  PLAA-PCB                                      
205810                            DLI-IO-AREA-W6GX6006                          
206000                            SSA1                                          
206100                            SSA2                                          
206200     MOVE PLAA-STATUS-CODE  TO STATUS-WS                                  
206300     PERFORM IMS-STATUSKONTROLL                                           
206400     .                                                                    
206500     SKIP3                                                                
206600 IMS-GU-PLAA-W6G130 SECTION.                                              
206700                                                                          
206800     STRING 'W6PLAA01(W6GXKEY  =' W-W6GX01KEY-X ')'                       
206900          DELIMITED BY SIZE INTO SSA1                                     
207000     STRING 'W6PLAA11(W6GXKEY  =' W-W6GX11KEY-X ')'                       
207100          DELIMITED BY SIZE INTO SSA2                                     
207200     MOVE '  ' TO GODK-STATUSKODER                                        
207300     CALL CBLTDLI USING GU PLAA-PCB DLI-IO-AREA-W6PLAA SSA1 SSA2          
207400     MOVE PLAA-STATUS-CODE TO STATUS-WS                                   
207500     PERFORM IMS-STATUSKONTROLL                                           
207600     .                                                                    
207700     SKIP3                                                                
207710 IMS-GU-PLAA-W6G130-KONTROLL SECTION.                                     
207720                                                                          
207730     STRING 'W6PLAA01(W6GXKEY  =' W-W6GX01KEY-X ')'                       
207740          DELIMITED BY SIZE INTO SSA1                                     
207750     STRING 'W6PLAA11(W6GXKEY  =' W-W6GX11KEY-Y ')'                       
207760          DELIMITED BY SIZE INTO SSA2                                     
207770     MOVE '  GE' TO GODK-STATUSKODER                                      
207780     CALL CBLTDLI USING GU PLAA-PCB DLI-IO-AREA-W6PLAA SSA1 SSA2          
207790     MOVE PLAA-STATUS-CODE TO STATUS-WS                                   
207791     PERFORM IMS-STATUSKONTROLL                                           
207792     .                                                                    
207793     SKIP3                                                                
207800 IMS-GU-PLAA-W6G101 SECTION.                                              
207900                                                                          
208000     STRING 'W6PLAA01(W6GXKEY  =' W-W6GX01KEY-X ')'                       
208100          DELIMITED BY SIZE INTO SSA1                                     
208200     MOVE '  ' TO GODK-STATUSKODER                                        
208300     CALL CBLTDLI USING GU PLAA-PCB DLI-IO-AREA-W6PLAA SSA1               
208400     MOVE PLAA-STATUS-CODE TO STATUS-WS                                   
208500     PERFORM IMS-STATUSKONTROLL                                           
208600     .                                                                    
208700     SKIP3                                                                
208800 IMS-GNP-PLAA-PAR   SECTION.                                              
208900                                                                          
209000     STRING 'W6PLAA11(ADINLOMP =' W-ADINLOMR-PAR ')'                      
209100          DELIMITED BY SIZE INTO SSA1                                     
209200     MOVE '  GE' TO GODK-STATUSKODER                                      
209300     CALL CBLTDLI USING GNP PLAA-PCB DLI-IO-AREA-W6PLAA SSA1              
209400     MOVE PLAA-STATUS-CODE TO STATUS-WS                                   
209500     PERFORM IMS-STATUSKONTROLL                                           
209600     .                                                                    
209700     SKIP3                                                                
209710 IMS-GU-UPFA01 SECTION.                                                   
209720     STRING 'W6UPFA01(IDLOPNRM =' W-IDLOPNRM-X ')'                        
209730          DELIMITED BY SIZE INTO SSA1                                     
209740     MOVE '  GE' TO GODK-STATUSKODER                                      
209750     CALL CBLTDLI USING GU UPFA-PCB DLI-IO-AREA-UPFA01 SSA1               
209760     MOVE UPFA-STATUS-CODE TO STATUS-WS                                   
209770     PERFORM IMS-STATUSKONTROLL                                           
209780     .                                                                    
209790     SKIP3                                                                
209791                                                                          
209792 IMS-GNP-UPFA11 SECTION.                                                  
209793     STRING 'W6UPFA01(IDLOPNRM =' W-IDLOPNRM-X ')'                        
209794          DELIMITED BY SIZE INTO SSA1                                     
209795     MOVE 'W6UPFA11 ' TO SSA2                                             
209796     MOVE '  GE' TO GODK-STATUSKODER                                      
209797     CALL CBLTDLI USING GNP UPFA-PCB DLI-IO-AREA-UPFA11 SSA1 SSA2         
209798     MOVE UPFA-STATUS-CODE TO STATUS-WS                                   
209799     PERFORM IMS-STATUSKONTROLL                                           
209800     .                                                                    
209801     SKIP3                                                                
209802                                                                          
209803 IMS-GNP-UPFA12 SECTION.                                                  
209804     STRING 'W6UPFA01(IDLOPNRM =' W-IDLOPNRM-X ')'                        
209805          DELIMITED BY SIZE INTO SSA1                                     
209806     MOVE 'W6UPFA12 ' TO SSA2                                             
209807     MOVE '  GE' TO GODK-STATUSKODER                                      
209808     CALL CBLTDLI USING GNP UPFA-PCB DLI-IO-AREA-UPFA12 SSA1 SSA2         
209809     MOVE UPFA-STATUS-CODE TO STATUS-WS                                   
209810     PERFORM IMS-STATUSKONTROLL                                           
209811     .                                                                    
209812     SKIP3                                                                
209813 IMS-GU-WDB601    SECTION.                                                
209814     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
209815          DELIMITED BY SIZE INTO SSA1                                     
209816     MOVE '  GE' TO GODK-STATUSKODER                                      
209817     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
209818     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
209819     PERFORM IMS-STATUSKONTROLL                                           
209820     IF SEGMENT-SAKNAS                                                    
209821         MOVE SPACE TO DCS-KDDC                                           
209822     END-IF                                                               
209823     .                                                                    
209830*----------------------------------------------------------------*        
209900 IMS-STATUSKONTROLL SECTION.                                              
210000                                                                          
210100     SET STATUS-IX TO 1                                                   
210200     SEARCH GODK-STATUS                                                   
210300       AT END                                                             
210400         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
210500         DELIMITED BY SIZE INTO FELTEXT                                   
210600         CALL FELLOG                                                      
210700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
210800         CONTINUE                                                         
210900     END-SEARCH                                                           
211000     .                                                                    
211100     EJECT                                                                
