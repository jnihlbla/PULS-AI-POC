000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W6012400.                                                
000400*AUTHOR.         ROS-MARIE CLASON - GUIDE DATAKONSULT AB.                 
000500*DATE-WRITTEN.   92/04/08.                                                
000600*                                                                         
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        ALLMÄN BESKRIVNING:                                              
001100*        PROGRAMMET ÄR EN MPP SOM REGISTRERAR KOLLIN PÅ                   
001200*        EN VAGN ELLER PLACERING.                                         
001300*        BILDEN ÄR EN REGISTRERINGSBILD.                                  
001400*                                                                         
001500*        PROGRAMMET UPPDATERAR W6INLA (W6D1)                              
001600*        PROGRAMMET LÄSER      W6PLAA (W6G1)                              
001610*        PROGRAMMET LÄSER              WDB6                               
001700*                                                                         
001800*    INDATA.                                                              
001900*        TRANSAKTION: W6T124                                              
002000*        MID:         W6I12401                                            
002100*                     W6I12403                                            
002200*                                                                         
002300*    UTDATA.                                                              
002400*        MOD:         W6O12401                                            
002500                                                                          
002600     SKIP3                                                                
002700 ENVIRONMENT DIVISION.                                                    
002800     EJECT                                                                
002900                                                                          
003000 DATA DIVISION.                                                           
003100 WORKING-STORAGE SECTION.                                                 
003101                                                                          
003110*    -- CHECKED BY WY2000                                                 
003200 77  IDPGM                       PIC X(08)   VALUE 'W6012400'.            
003300                                                                          
003400*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003500 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003600                                                                          
003700 77  JA                          PIC X       VALUE 'J'.                   
003710 77  YES                         PIC X       VALUE 'Y'.                   
003800 77  NEJ                         PIC X       VALUE 'N'.                   
003900                                                                          
004000*    --- INDEX FÖR BLÄDDRINGSRADER                                        
004100 77  IX                          PIC S9(4)  VALUE +0    COMP SYNC.        
004200 77  IX1                         PIC S9(4)  VALUE +0    COMP SYNC.        
004300 77  TABIX                       PIC S9(4)  VALUE +0    COMP SYNC.        
004400 77  MOD-IX                      PIC S9(4)  VALUE +0    COMP SYNC.        
004500 77  MAX-IX                      PIC S9(4)  VALUE +12   COMP SYNC.        
004600 77  MAX-TABIX                   PIC S9(4)  VALUE +0    COMP SYNC.        
004700 77  T91-IX                      PIC S9(4)  VALUE +0    COMP SYNC.        
004800 77  RADNR1-IX                   PIC S9(9)  VALUE +0    COMP SYNC.        
004900 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
005000                                                                          
005100*    --- RÄKNARE                                                          
005200 77  FELRAKN                     PIC S9(2)  VALUE +0    COMP SYNC.        
005300 77  IDLOPNRM-RAKN               PIC S9(2)  VALUE +0    COMP SYNC.        
005400 77  IDLEVNR-RAKN                PIC S9(2)  VALUE +0    COMP SYNC.        
005500                                                                          
005600*   OM SVAR TILL SKÄRM: MAX-MOD-LAENGD = (826) MOD-LÄNGD + 4              
005700 77  MAX-MOD-LAENGD              PIC S9(4) VALUE +830 COMP SYNC.          
005800 77  P-TO-P-PREFIX-LNG           PIC S9(4) VALUE +17  COMP SYNC.          
005900                                                                          
006000***********************************                                       
006100*        ARBETSFÄLT               *                                       
006200***********************************                                       
006300*                                                                         
006400*    --- ARBETSFÄLT                                                       
006500                                                                          
006600*                                                                         
006700 01  WS-AREA.                                                             
006800*                                                                         
006900*----TILL W60191                                                          
007000     03  W-PTOP1-OCC-LL          PIC S9(4)    COMP-3 VALUE ZERO.          
007100     03  SPAR-IDLOPNRM           PIC S9(9)    COMP-3 VALUE ZERO.          
007200     03  SPAR-PRARTSTD         PIC S9(7)V9(2) COMP-3 VALUE ZERO.          
007300     03  SPAR-ADINLOMR-OLD       PIC X(4)     VALUE SPACE.                
007400     03  SPAR-ADINLOMR-NEW       PIC X(4)     VALUE SPACE.                
007500     03  SPAR-ADINLOMR-NXT-OLD   PIC X(4)     VALUE SPACE.                
007600*                                                                         
007700     03  SPAR-FLPRIO             PIC X(1)     VALUE SPACE.                
007800*                                                                         
007900     03  SPAR-KDINLSTA-NEW       PIC X(3)     VALUE SPACE.                
008000     03  SPAR-KDINLSTA-OLD       PIC X(3)     VALUE SPACE.                
008100*                                                                         
008200     03  SPAR-ADINLOMR           PIC X(4)     VALUE SPACE.                
008300     03  SPAR-ADINLOMR-NXT       PIC X(4)     VALUE SPACE.                
008400*                                                                         
008500*----FRÅN RADNR=1                                                         
008600     03  W-RAD1-FLDIVKLI         PIC X(1).                                
008700     03  W-RAD1-FLINLFP          PIC X(1).                                
008800     03  W-RAD1-FLKVAANT         PIC X(1).                                
008900     03  W-RAD1-FLINLFB          PIC X(1).                                
009000     03  W-RAD1-IDANSTNR         PIC S9(5)    COMP-3 VALUE ZERO.          
009100     03  W-RAD1-KDINLPRIO        PIC S9(3)    COMP-3 VALUE ZERO.          
009200     03  W-RAD1-KDINLSTA         PIC X(3).                                
009300*                                                                         
009400*                                                                         
009500*----TILL KOLLI-KONTROLLER                                                
009600     03  SPAR-MID-IDLEVNR-X      PIC X(5).                                
009700     03  SPAR-MID-LEVNR REDEFINES SPAR-MID-IDLEVNR-X.                     
009800         05  SPAR-MID-IDLEVNR    PIC X(5).                                
009900*                                                                         
010000     03  SPAR-MID-IDOKOLLI-X      PIC X(9).                               
010100     03  SPAR-MID-KOLLI REDEFINES SPAR-MID-IDOKOLLI-X.                    
010200         05  SPAR-MID-IDOKOLLI   PIC 9(9).                                
010300*                                                                         
010400     03  SPAR-MID-IDARTNR-X      PIC X(9).                                
010500     03  SPAR-MID-ARTNR REDEFINES SPAR-MID-IDARTNR-X.                     
010600         05  SPAR-MID-IDARTNR    PIC 9(9).                                
010700*                                                                         
010800     03  SPAR-MID-KVINLART-X     PIC X(6).                                
010900     03  SPAR-MID-ANTAL REDEFINES SPAR-MID-KVINLART-X.                    
011000         05  SPAR-MID-KVINLART   PIC 9(6).                                
011100*                                                                         
011200*                                                                         
011300     03  SPAR-IDRADNR            PIC S9(5)    COMP-3 VALUE ZERO.          
011400     03  SPAR-KVINLART           PIC S9(7)    COMP-3 VALUE ZERO.          
011500     03  SPAR-KVINLART-OLD       PIC S9(7)    COMP-3 VALUE ZERO.          
011600     03  SPAR-ACK-KVINLART       PIC S9(7)    COMP-3 VALUE ZERO.          
011700     03  SPAR-RAD1-KVINLART      PIC S9(7)    COMP-3 VALUE ZERO.          
011800                                                                          
011900 01  RED-IDARTNR                 PIC  9(9)           VALUE ZERO.          
012000                                                                          
012200 77  WS-IDLOPNRM                 PIC S9(9)   VALUE ZERO COMP-3.           
012300 77  WS-BEFT                     PIC S9(3)   VALUE ZERO COMP-3.           
012400 77  WS-IDARTNR                  PIC S9(9)   VALUE ZERO COMP-3.           
012440       EJECT                                                              
012500******************************************************************        
012600*    --- INTERN-TABELL                                                    
012700******************************************************************        
012800*                                                                         
012900 01 WT-ENTRY-PARM.                                                        
013000    03 STEGLANGD            PIC S9(9) COMP.                               
013100    03 ANTAL                PIC S9(9) COMP.                               
013200    03 NYCKELLANGD          PIC S9(9) COMP.                               
013300*                                                                         
013400 01 WT-TAB-MAX              PIC S9(9) COMP.                               
013500*                                                                         
013600 01 WT-TAB.                                                               
013700    03 WT-TABELL OCCURS 12.                                               
013800       05 WT-SORT-NKL.                                                    
013900          07 WT-IDARTNR     PIC X(9).                                     
014000          07 WT-IDLEVNR     PIC X(5).                                     
014100          07 WT-IDOKOLLI    PIC X(9).                                     
014200       05 WT-KVINLART       PIC X(6).                                     
014300       05 WT-MID-IX         PIC 9(4).                                     
014400       05 WT-IDLOPNRM       PIC 9(9).                                     
014500*                                                                         
014600******************************************************************        
014700*    --- ARBETSFÄLT FÖR SWITCHAR                                          
014800                                                                          
014900 77  INDATA-SW                   PIC X       VALUE 'J'.                   
015000     88  INDATA-OK                           VALUE 'J'.                   
015100     88  INDATA-FEL                          VALUE 'N'.                   
015200                                                                          
015300 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
015400     88  NYCKLAR-OK                          VALUE 'J'.                   
015500     88  NYCKLAR-FEL                         VALUE 'N'.                   
015600                                                                          
015700 77  ALLT-SW                     PIC X       VALUE 'J'.                   
015800     88  ALLT-OK                             VALUE 'J'.                   
015900                                                                          
016000 77  UPD-SW                      PIC X       VALUE 'J'.                   
016100     88  UPD-OK                              VALUE 'J'.                   
016200     88  UPD-NEJ                             VALUE 'N'.                   
016300                                                                          
016400 77  FBRAPP-SW                   PIC X       VALUE 'J'.                   
016500     88  FBRAPP                              VALUE 'J'.                   
016600     88  FBRAPP-FINNS                        VALUE 'N'.                   
016700                                                                          
016800 77  PRIOGODS-SW                 PIC X       VALUE 'N'.                   
016900     88  PRIOGODS                            VALUE 'J'.                   
017000                                                                          
017100 77  FOERSTA-T91-SW              PIC X       VALUE 'N'.                   
017200     88  FOERSTA-T91-JA                      VALUE 'J'.                   
017300     88  FOERSTA-T91-NEJ                     VALUE 'N'.                   
017400                                                                          
017500 77  TRANS91-SW                  PIC X       VALUE 'J'.                   
017600     88  TRANS91-JA                          VALUE 'J'.                   
017700     88  TRANS91-NEJ                         VALUE 'N'.                   
017800                                                                          
017900 77  RADNR1-SW                   PIC X       VALUE 'J'.                   
018000     88  RADNR1-JA                           VALUE 'J'.                   
018100     88  RADNR1-NEJ                          VALUE 'N'.                   
018200                                                                          
018300 77  RADFEL-SW                   PIC X       VALUE 'N'.                   
018400     88  RADFEL-JA                           VALUE 'J'.                   
018500     88  RADFEL-NEJ                          VALUE 'N'.                   
018600                                                                          
018700 77  RADERFEL-SW                 PIC X       VALUE 'N'.                   
018800     88  RADERFEL-JA                         VALUE 'J'.                   
018900     88  RADERFEL-NEJ                        VALUE 'N'.                   
019000                                                                          
019100 77  NYA-RADER-SW                PIC X       VALUE 'N'.                   
019200     88  NYA-RADER-JA                        VALUE 'J'.                   
019300     88  NYA-RADER-NEJ                       VALUE 'N'.                   
019400                                                                          
019500 77  STATUS-SW                   PIC X       VALUE 'N'.                   
019600     88  OK-STATUS                           VALUE 'J'.                   
019700     88  FEL-STATUS                          VALUE 'N'.                   
019800                                                                          
019900 77  GAMMALT-SW                  PIC X       VALUE 'N'.                   
020000     88  GAMMALT-KOLLI                       VALUE 'J'.                   
020100                                                                          
020110 77  PRIM-CONTROL-SW             PIC X       VALUE 'N'.                   
020120     88  PRIM-CONTROL-YES                    VALUE 'J'.                   
020130     88  PRIM-CONTROL-NO                     VALUE 'N'.                   
020140                                                                          
020200*----------------------------------------------------------------*        
020300*   NKLTYP1=VAGN; VAGN, PLAC; VAGN, PLAC, ADR;                            
020400*   NKLTYP2=PLAC                                                          
020500*----------------------------------------------------------------*        
020600 77  NKLTYP-SW                  PIC X.                                    
020700     88  NKLTYP1                            VALUE '1'.                    
020800     88  NKLTYP2                            VALUE '2'.                    
020900                                                                          
021000*----------------------------------------------------------------*        
021100*   BEARBTYP1=ART,ANT,LEV,KOLLI                                           
021200*   BEARBTYP2=LEV,KOLLI                                                   
021300*----------------------------------------------------------------*        
021400 77  BEARBTYP-SW                  PIC X.                                  
021500     88  BEARBTYP1                          VALUE '1'.                    
021600     88  BEARBTYP2                          VALUE '2'.                    
021700                                                                          
021800                                                                          
021900 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
022000     88  EGEN-MID                            VALUE '6124'.                
022100     88  GODK-MID                            VALUE '6124' '6125'          
022200                                                   '6102'.                
022300     88  HELP-MID                            VALUE '0551'.                
022400     EJECT                                                                
022500                                                                          
022600*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
022700 01  GENERELLA-SUBPROGRAM.                                                
022800     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
022900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
023000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
023100     03  WINTSOR                 PIC X(8)    VALUE 'WINTSOR '.            
023200     03  W611PMRK                PIC X(8)    VALUE 'W611PMRK'.            
023210     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
023300     EJECT                                                                
023400                                                                          
023410*01 -COPY WMSGINIT                                                        
023420     EJECT                                                                
023500*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
023600*01 -COPY WMEDAREA                                                        
023700     EJECT                                                                
023800 01  MESSAGE-CODES.                                                       
023900     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
024000     03  ERR-CONFLICT            PIC X(3)    VALUE '002'.                 
024100     03  ERR-UPDATE-NOT-VALID    PIC X(3)    VALUE '007'.                 
024200     03  ERR-SAKN-I-REG          PIC X(3)    VALUE '010'.                 
024300     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
024400     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
024500     03  ERR-CODE-NOT-VALID      PIC X(3)    VALUE '416'.                 
024600     03  ERR-RADNR-1-SAKN        PIC X(3)    VALUE '178'.                 
024700     03  ERR-ANT-EJ-OK           PIC X(3)    VALUE '181'.                 
024800     03  ERR-DIVKOLLI            PIC X(3)    VALUE '182'.                 
024900     03  ERR-SATS-PRIO-JA        PIC X(3)    VALUE '183'.                 
025000     03  ERR-PLAC-ADR-FINNS      PIC X(3)    VALUE '196'.                 
025100                                                                          
025200     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
025300     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
025400     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
025500     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
025600     03  INF-LAST-PAGE           PIC X(3)    VALUE '106'.                 
025700                                                                          
025800*    --- ÖVERLEVERANS                                                     
025900     03  ERR-ANT-VERKL-ANT       PIC X(3)    VALUE '197'.                 
026000                                                                          
026100*    --- STÄMMER EJ MED AVISERAT                                          
026200     03  ERR-ARTNR-SAKN          PIC X(3)    VALUE '179'.                 
026300                                                                          
026400*    --- ANVÄND BILD 6122                                                 
026500     03  ERR-FLERA-LEV-PARTI-ART PIC X(3)    VALUE '180'.                 
026600     EJECT                                                                
026700                                                                          
026800*    --- AREOR FÖR BAKGRUNDS MPP:ER / BMP:ER                              
026900*    --- COPYTEXTER FÖR PRINTER,  W611PMRK, W60191                        
027000*                                                                         
027100*01  -COPY W611PMRK                                                       
027200     EJECT                                                                
027300 01  FILLER                      PIC X(16)   VALUE 'P-TO-P-AREA'.         
027400*                                                                         
027500*01  -COPY WMSGSNUF   -PRE  P-TO-P-                                       
027600*01  MID -COPY W6I19101       -PRE T91-                                   
027700     EJECT                                                                
027800                                                                          
027900                                                                          
028000*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
028100*                                                                         
028200 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
028300     SKIP3                                                                
028400*01  MID -COPY W6I12401                                                   
028500     EJECT                                                                
028600                                                                          
028800*01  MID -COPY W6I12403                                                   
028900     EJECT                                                                
029000                                                                          
029100 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
029200     SKIP3                                                                
029300*01  -COPY WMSGAREA                                                       
029400     EJECT                                                                
029500                                                                          
029600     03  MOD REDEFINES MSG-AREA.                                          
029700*      05  -COPY W6O12401                                                 
029800     EJECT                                                                
029900                                                                          
030000 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
030100     SKIP3                                                                
030200*01  -COPY WMFSAREA                                                       
030300     EJECT                                                                
030400                                                                          
030500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
030600*                                                                         
030700     EJECT                                                                
030800                                                                          
030900*                                                                         
031000*    --- DLI- NYCKLAR TILL IMS-SEKTIONERNA                                
031100*                                                                         
031200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
031300     SKIP3                                                                
031400 01  NYCKLAR-TILL-DLI.                                                    
031500                                                                          
031600     03  W-IDDC-X.                                                        
031700         05  W-IDDC               PIC  X(2).                              
031800                                                                          
031900     03  W1-IDARTNR-X.                                                    
032000         05  W1-IDARTNR           PIC S9(9)   COMP-3.                     
032100                                                                          
032200     03  W-W6D101KY-X.                                                    
032300         05  W-IDDC-101KY  PIC  X(2).                                     
032310         05  W-IDLEVNR     PIC  X(5)    VALUE SPACE.                      
032400         05  W-IDFS        PIC X(8)     VALUE SPACE.                      
032500         05  W-TIAVIDAT    PIC S9(7)    COMP-3.                           
032600                                                                          
032700     03  W-IDARTNR-X.                                                     
032800         05  W-IDARTNR            PIC S9(9)   COMP-3.                     
032900                                                                          
033000     03  W-IDRADNR-INL-X.                                                 
033100         05  W-IDRADNR-INL        PIC S9(5)   COMP-3.                     
033200                                                                          
033300     03  W-IDRADNR-X.                                                     
033400         05  W-IDRADNR            PIC S9(5)   COMP-3.                     
033500                                                                          
033600*--------FYSISK NKL TILL INLF (EG. INLG)                                  
033700     03  W-W6D1F1KY-MIN-X.                                                
033800         05  WF1-MIN-IDINLVGN    PIC 9(3).                                
033900         05  WF1-MIN-IDRADNR-INL PIC S9(5)    COMP-3 VALUE ZERO.          
033910         05  WF1-MIN-IDDC        PIC  X(2)    VALUE SPACE.                
034000         05  WF1-MIN-IDLEVNR     PIC  X(5)    VALUE SPACE.                
034100         05  WF1-MIN-IDFS        PIC X(8)     VALUE SPACE.                
034200         05  WF1-MIN-TIAVIDAT    PIC S9(7)    COMP-3.                     
034300         05  WF1-MIN-IDRADNR     PIC S9(5)    COMP-3 VALUE ZERO.          
034400                                                                          
034500     03  W-W6D1F1KY-MAX-X.                                                
034600         05  WF1-MAX-IDINLVGN    PIC 9(3).                                
034700         05  WF1-MAX-IDRADNR-INL PIC S9(5)    COMP-3 VALUE ZERO.          
034710         05  WF1-MAX-IDDC        PIC  X(2)    VALUE SPACE.                
034800         05  WF1-MAX-IDLEVNR     PIC  X(5)    VALUE SPACE.                
034900         05  WF1-MAX-IDFS        PIC X(8)     VALUE SPACE.                
035000         05  WF1-MAX-TIAVIDAT    PIC S9(7)    COMP-3.                     
035100         05  WF1-MAX-IDRADNR     PIC S9(5)    COMP-3 VALUE ZERO.          
035200                                                                          
035300*--------FYSISK NKL TILL INLH (EG. INLI)                                  
035400     03  W-W6D1H1KY-MIN-X.                                                
035500         05  WH1-MIN-IDARTNR     PIC S9(9)    COMP-3 VALUE ZERO.          
035510         05  WH1-MIN-IDDC        PIC  X(2)    VALUE SPACE.                
035600         05  WH1-MIN-IDLEVNR     PIC  X(5)    VALUE SPACE.                
035700         05  WH1-MIN-IDRADNR-INL PIC S9(5)    COMP-3.                     
035800         05  WH1-MIN-IDFS        PIC X(8)     VALUE SPACE.                
035900         05  WH1-MIN-TIAVIDAT    PIC S9(7)    COMP-3.                     
036000                                                                          
036100     03  W-W6D1H1KY-MAX-X.                                                
036200         05  WH1-MAX-IDARTNR     PIC S9(9)    COMP-3 VALUE ZERO.          
036210         05  WH1-MAX-IDDC        PIC  X(2)    VALUE SPACE.                
036300         05  WH1-MAX-IDLEVNR     PIC  X(5)    VALUE SPACE.                
036400         05  WH1-MAX-IDRADNR-INL PIC S9(5)    COMP-3.                     
036500         05  WH1-MAX-IDFS        PIC X(8)     VALUE SPACE.                
036600         05  WH1-MAX-TIAVIDAT    PIC S9(7)    COMP-3.                     
036700                                                                          
038200                                                                          
038300     03  W-W6D1B1KY-X.                                                    
038400         05  WB-IDLOPNRM          PIC S9(9)   COMP-3 VALUE ZERO.          
038500                                                                          
038600     03  W-W6D1C1KY-X.                                                    
038700         05  WC-IDLEVNR           PIC X(5)    VALUE SPACE.                
038800         05  WC-IDOKOLLI          PIC 9(9).                               
038900*C-INDEX SÖKNYCKLAR                                                       
039000     03  WCS-IDLEVNR-X.                                                   
039100         05  WCS-IDLEVNR          PIC X(5)    VALUE SPACE.                
039200                                                                          
039300     03  WCS-IDOKOLLI-X.                                                  
039400         05  WCS-IDOKOLLI          PIC 9(9).                              
039500                                                                          
039600     03  W-W6GX01KEY-X.                                                   
039700         05  WGX-IDHTYP           PIC X(4)    VALUE '6005'.               
039710         05  WGX-IDDC             PIC X(2)    VALUE SPACE.                
039800         05  FILLER               PIC X(24)   VALUE LOW-VALUE.            
039900                                                                          
039910     03  W-W6GXKEY-6006-X.                                                
039920         05  W-6006-ADINLOMR     PIC X(4)    VALUE SPACE.                 
039930         05  FILLER              PIC X       VALUE LOW-VALUE.             
039940                                                                          
040000     03  W-W6GX11KEY-X.                                                   
040100         05  W-ADINLOMR           PIC X(4)    VALUE SPACE.                
040200         05  FILLER               PIC X(1)    VALUE LOW-VALUE.            
040300                                                                          
040310     03  W-IDLOPNRM-X.                                                    
040320         05  W-IDLOPNRM          PIC S9(9)   COMP-3 VALUE ZERO.           
040400                                                                          
040410     03  W-IDDC-B6-X.                                                     
040420         05 W-IDDC-B6            PIC X(2).                                
040430                                                                          
040500*    --- STATUS-KOD FRÅN IMS                                              
040600 01  STATUS-WS                   PIC XX.                                  
040700     88  SEGMENT-FINNS                       VALUE '  '.                  
040800     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
040900     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
041000     SKIP2                                                                
041100                                                                          
041200 01  GODK-STATUSKODER.                                                    
041300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
041400     SKIP3                                                                
041500                                                                          
041600 01  SSA1                        PIC X(128).                              
041700 01  SSA2                        PIC X(64).                               
041800 01  SSA3                        PIC X(64).                               
041900     EJECT                                                                
042000                                                                          
042100*    --- IMS FUNKTIONSKODER                                               
042200*01  -COPY W0003                                                          
042300     EJECT                                                                
042400                                                                          
042500*    ---  DLI INPUT-OUTPUT AREA                                           
042600 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
042700     SKIP3                                                                
042800                                                                          
042900 01  DLI-IO-AREA.                                                         
043000     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
043100     SKIP3                                                                
043200                                                                          
043210 01  FILLER                    PIC X(16)    VALUE 'DLI-IO-W6D111'.        
043220                                                                          
043230 01  DLI-IO-AREA-W6D111.                                                  
043240     03  W6INLA11.                                                        
043250*        05  -COPY W6D111                                                 
043260     EJECT                                                                
043270                                                                          
043280 01  FILLER                    PIC X(16)    VALUE 'DLI-IO-W6D121'.        
043290                                                                          
043291 01  DLI-IO-AREA-W6D121.                                                  
043292     03  W6INLA21.                                                        
043293*        05  -COPY W6D121                                                 
043294     EJECT                                                                
043295                                                                          
043296 01  FILLER                    PIC X(16)    VALUE 'DLI-IO-W6GX01'.        
043297                                                                          
043298 01  DLI-IO-AREA-W6GX01.                                                  
043299     03  W6PLAA01.                                                        
043300*        05  -COPY W6GX01                                                 
043301     EJECT                                                                
043302                                                                          
043303 01  FILLER                  PIC X(16)    VALUE 'DLI-IO-W6GX6006'.        
043304                                                                          
043305 01  DLI-IO-AREA-W6GX6006.                                                
043306     03  W6PLAA11.                                                        
043307*        05  -COPY W6GX6006                                               
043308     EJECT                                                                
043600                                                                          
043700 01  FILLER                  PIC X(16)    VALUE 'DLI-IO-W6D1H1'.          
043800                                                                          
043900 01  DLI-IO-AREA-W6D1H1.                                                  
044000     03  W6INLH11.                                                        
044010*        05  -COPY W6D1H1                                                 
044020     EJECT                                                                
044030                                                                          
044040 01  FILLER                  PIC X(16)    VALUE 'DLI-IO-W6D1F1'.          
044050                                                                          
044060 01  DLI-IO-AREA-W6D1F1.                                                  
044070     03  W6INLF11.                                                        
044080*        05  -COPY W6D1F1                                                 
044090     EJECT                                                                
044800                                                                          
045600 01  FILLER                    PIC X(16)    VALUE 'DLI-IO-W6D1B1'.        
045700                                                                          
045800 01  DLI-IO-AREA-W6D1B1.                                                  
045900     03  W6INLC01.                                                        
046000*        05  -COPY W6D1B1                                                 
046100     EJECT                                                                
046200 01  FILLER                    PIC X(16)    VALUE 'DLI-IO-W6PLAA'.        
046300                                                                          
046400 01  DLI-IO-AREA-W6PLAA.                                                  
046500     03  W6GX6006.                                                        
046600*        05  -COPY W6GX6006  -PRE ALT-                                    
046700     EJECT                                                                
046710 01  DLI-IO-AREA-UPFA01.                                                  
046720     03  W6UPFA01.                                                        
046730*        05  -COPY W6L101                                                 
046740     SKIP3                                                                
046750 01  DLI-IO-AREA-UPFA11.                                                  
046760     03  W6UPFA11.                                                        
046770*        05  -COPY W6L111                                                 
046780     SKIP3                                                                
046790 01  DLI-IO-AREA-UPFA12.                                                  
046791     03  W6UPFA12.                                                        
046792*        05  -COPY W6L112                                                 
046793     SKIP3                                                                
046794 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
046795 01   DLI-IO-AREA-B601.                                                   
046796*     03  -COPY WDB601                                                    
046797                                                                          
046900 LINKAGE SECTION.                                                         
047000                                                                          
047100*01  -COPY W0009   -PRE MSG-                                              
047200     EJECT                                                                
047300                                                                          
047400*01  -COPY W0009   -PRE ALT1-                                             
047500     EJECT                                                                
047600                                                                          
047700*01  -COPY W0008  -PRE USEA-                                              
047800     05  FILLER                  PIC X.                                   
047900     EJECT                                                                
048000                                                                          
048010*01  -COPY W0008  -PRE INLA-                                              
048020     05  FILLER                  PIC X.                                   
048030     EJECT                                                                
048040                                                                          
048100*01  -COPY W0008  -PRE INLB-                                              
048200     05  FILLER                  PIC X.                                   
048300     EJECT                                                                
048400                                                                          
048500*01  -COPY W0008  -PRE INLC-                                              
048600     05  FILLER                  PIC X.                                   
048700     EJECT                                                                
048800                                                                          
048900*01  -COPY W0008  -PRE INLF1-                                             
049000     05  FILLER                  PIC X.                                   
049100     EJECT                                                                
049200                                                                          
049300*01  -COPY W0008  -PRE INLH1-                                             
049400     05  FILLER                  PIC X.                                   
049500     EJECT                                                                
049600                                                                          
049700*01  -COPY W0008  -PRE INLB1-                                             
049800     05  FILLER                  PIC X.                                   
049900     EJECT                                                                
050000                                                                          
050100*01  -COPY W0008  -PRE PLAA-                                              
050200     05  FILLER                  PIC X.                                   
050300     EJECT                                                                
050400                                                                          
050500*01  -COPY W0008  -PRE INLB-PMRK-                                         
050600     05  FILLER                  PIC X.                                   
050700     EJECT                                                                
050800                                                                          
050900*01  -COPY W0008  -PRE INLC-PMRK-                                         
051000     05  FILLER                  PIC X.                                   
051100     EJECT                                                                
051200*01  -COPY W0008  -PRE UPFA-                                              
051300     05  FILLER                  PIC X.                                   
051400     EJECT                                                                
051410*01  -COPY W0008  -PRE WDB6-                                              
051420     05  FILLER                  PIC X.                                   
051430     EJECT                                                                
051500                                                                          
051600 PROCEDURE DIVISION  USING MSG-PCB                                        
051700                           ALT1-PCB                                       
051800                           USEA-PCB                                       
051810                           INLA-PCB                                       
051900                           INLB-PCB                                       
052000                           INLC-PCB                                       
052100                           INLF1-PCB                                      
052200                           INLH1-PCB                                      
052300                           INLB1-PCB                                      
052400                           PLAA-PCB                                       
052500                           INLB-PMRK-PCB                                  
052600                           INLC-PMRK-PCB                                  
052610                           UPFA-PCB                                       
052620                           WDB6-PCB.                                      
052700     ENTRY 'DLITCBL' USING MSG-PCB                                        
052800                           ALT1-PCB                                       
052900                           USEA-PCB                                       
052910                           INLA-PCB                                       
053000                           INLB-PCB                                       
053100                           INLC-PCB                                       
053200                           INLF1-PCB                                      
053300                           INLH1-PCB                                      
053400                           INLB1-PCB                                      
053500                           PLAA-PCB                                       
053600                           INLB-PMRK-PCB                                  
053700                           INLC-PMRK-PCB                                  
053710                           UPFA-PCB                                       
053720                           WDB6-PCB.                                      
053800     EJECT                                                                
053900                                                                          
054000*----------------------------------------------------------------*        
054100     PERFORM IMS-GET-MSG                                                  
054200     IF SEGMENT-FINNS                                                     
054300        PERFORM A-INIT                                                    
054400        PERFORM B-KOLLA-NYCKLAR                                           
054500        IF NYCKLAR-OK                                                     
054600           IF MFS-UPDATE  OR MFS-UPD-V                                    
054700              PERFORM G-KOLLA-INPUT                                       
054800              IF INDATA-OK                                                
054900                 PERFORM H-UPPDATERA                                      
055000              END-IF                                                      
055100           ELSE                                                           
055200              PERFORM F-LAES-VISA-INFO                                    
055300           END-IF                                                         
055400        END-IF                                                            
055500        PERFORM S90-BLANKUTF-NUM-FAELT                                    
055600        MOVE MAX-MOD-LAENGD TO MSG-KVLL                                   
055700        PERFORM IMS-INSERT-MSG                                            
055800     END-IF                                                               
055900                                                                          
056000     MOVE ZERO TO RETURN-CODE                                             
056100     GOBACK                                                               
056200     .                                                                    
056300     EJECT                                                                
056400*----------------------------------------------------------------*        
056500 A-INIT SECTION.                                                          
056600                                                                          
056700     IF MSG-DUBBLA-TRANSKODER                                             
056800        MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W6I12401                
056900                                              HTERM-MID-W6I12403          
057000        MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                 
057100        MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                               
057200     ELSE                                                                 
057300        MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W6I12401                 
057400                                             HTERM-MID-W6I12403           
057500        MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                 
057600        MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                               
057700     END-IF                                                               
057800                                                                          
057900     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
058000     MOVE MSG-IDPFK TO MFS-IDPFK                                          
058100     MOVE MFS-IDTRANS TO W-IDTRANS                                        
058200                                                                          
058300     MOVE LOW-VALUE TO MSG-AREA                                           
058400     MOVE 'W6O124N1' TO MFS-IDMOD                                         
058500     MOVE '6124' TO MOD-IDTRANS                                           
058600     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
058700                                                                          
058800     IF EGEN-MID OR HELP-MID                                              
058900        CONTINUE                                                          
059000     ELSE                                                                 
059100        MOVE SPACE TO MFS-KDTRTYP                                         
059200        MOVE '7' TO MFS-IDPFK                                             
059300     END-IF                                                               
060200     IF MFS-UPD-V                                                         
060300       MOVE ALL '+'                      TO MID-W6I12401                  
060400       IF HTERM-MID-IDINLVGN-UT = ALL '+' OR                              
060410          HTERM-MID-IDINLVGN-UT = ALL '-'                                 
060500         MOVE SPACE                      TO MID-IDINLVGN-UT               
060600         MOVE ZERO                       TO MID-SPAR-IDINLVGN             
060700       ELSE                                                               
060800         MOVE HTERM-MID-IDINLVGN-UT      TO MID-IDINLVGN-UT               
060900                                            MID-SPAR-IDINLVGN             
061000       END-IF                                                             
061100       IF HTERM-MID-ADINLOMR-UT = ALL '+' OR                              
061110          HTERM-MID-ADINLOMR-UT = ALL '-'                                 
061200         MOVE SPACE                      TO MID-ADINLOMR-UT               
061300                                            MID-SPAR-ADINLOMR             
061400       ELSE                                                               
061500         MOVE HTERM-MID-ADINLOMR-UT      TO MID-ADINLOMR-UT               
061600                                            MID-SPAR-ADINLOMR             
061700       END-IF                                                             
061800       IF HTERM-MID-ADINLOMR-NXT-UT = ALL '+' OR                          
061810          HTERM-MID-ADINLOMR-NXT-UT = ALL '-'                             
061900         MOVE SPACE                      TO MID-ADINLOMR-NXT-UT           
062000                                            MID-SPAR-ADINLOMR-NXT         
062100       ELSE                                                               
062200         MOVE HTERM-MID-ADINLOMR-NXT-UT  TO MID-ADINLOMR-NXT-UT           
062300                                            MID-SPAR-ADINLOMR-NXT         
062400       END-IF                                                             
062500       MOVE HTERM-MID-FLPRIO             TO MID-FLPRIO                    
062600       MOVE HTERM-MID-FLSATS             TO MID-FLSATS                    
062700       MOVE +1     TO IX                                                  
062800       PERFORM UNTIL IX > MAX-IX OR                                       
062900                   HTERM-MID-IDARTNR(IX) = LOW-VALUE                      
062910         IF HTERM-MID-IDARTNR  (IX) = ALL '-'                             
062911           MOVE '+++++++++'                 TO MID-IDARTNR(IX)            
062920         ELSE                                                             
062921           MOVE HTERM-MID-IDARTNR(IX)       TO RED-IDARTNR                
062922           MOVE RED-IDARTNR                 TO MID-IDARTNR(IX)            
062930         END-IF                                                           
062940         IF HTERM-MID-KVINLART (IX) = ALL '-'                             
062941           MOVE HTERM-MID-KVINLART(IX)      TO MID-KVINLART(IX)           
062942         ELSE                                                             
062943           MOVE '++++++'                    TO MID-KVINLART(IX)           
062944         END-IF                                                           
062950         IF HTERM-MID-IDLEVNR  (IX) = ALL '-'                             
062951           MOVE HTERM-MID-IDLEVNR(IX)       TO MID-IDLEVNR(IX)            
062952         ELSE                                                             
062953           MOVE '+++++'                     TO MID-IDLEVNR(IX)            
062954         END-IF                                                           
062960         IF HTERM-MID-IDOKOLLI (IX) = ALL '-'                             
063400           MOVE HTERM-MID-IDOKOLLI(IX)      TO MID-IDOKOLLI(IX)           
063401         ELSE                                                             
063410           MOVE '+++++++++'                 TO MID-IDOKOLLI(IX)           
063430         END-IF                                                           
063500         ADD +1 TO IX                                                     
063600       END-PERFORM                                                        
063700     END-IF                                                               
063701     PERFORM AA-INIT-NYCKLAR                                              
063702                                                                          
063703     IF MSGI-IDLAND-SPR = 'GB'                                            
063704        MOVE +2 TO SPRAK-IX                                               
063705        MOVE 'GB ' TO MED-IDSKYLT                                         
063706     ELSE                                                                 
063707        MOVE +1 TO SPRAK-IX                                               
063708        MOVE 'S  ' TO MED-IDSKYLT                                         
063709     END-IF                                                               
063710     .                                                                    
063711     EJECT                                                                
063712*----------------------------------------------------------------*        
063713 AA-INIT-NYCKLAR SECTION.                                                 
063714                                                                          
063715     MOVE ALL '+' TO MSGI-WMSGINIT                                        
063716     MOVE '001'                  TO MSGI-KDCALL                           
063717     MOVE MSG-SIGNON-USERID      TO MSGI-IDUSER                           
063718     MOVE MSG-LTERM-NAME         TO MSGI-IDLTERM-USER                     
063719     MOVE '6124'                 TO MSGI-IDTRANS                          
063720     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
063800     .                                                                    
063900     EJECT                                                                
064000*----------------------------------------------------------------*        
064100 B-KOLLA-NYCKLAR SECTION.                                                 
064200                                                                          
064300     MOVE JA                 TO NYCKLAR-SW                                
064400     MOVE SPACE              TO NKLTYP-SW                                 
064500*    -- KONTROLL AV IDDC                                                  
064600                                                                          
064700     MOVE MFS-RENSA-FAELT TO MOD-IDDC-IN                                  
064800     IF MID-IDDC-IN = ALL '+'                                             
064900       MOVE MSGI-IDDC   TO W-IDDC-B6                                      
065000     ELSE                                                                 
065100       MOVE MID-IDDC-IN TO W-IDDC-B6                                      
065200       MOVE '7'         TO MFS-IDPFK                                      
065300       MOVE SPACE       TO MFS-KDTRTYP                                    
065400     END-IF                                                               
065410     PERFORM IMS-GU-WDB601                                                
065500                                                                          
065600     IF DCS-KDDC = SPACE OR DCS-DDC                                       
065700         MOVE NEJ       TO NYCKLAR-SW                                     
065710         MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                               
065720         PERFORM S01-CALL-WMEDKONV-FEL                                    
065730     ELSE                                                                 
065800         MOVE DCS-IDDC  TO W-IDDC                                         
065900                           WGX-IDDC                                       
066000                           MOD-IDDC-UT                                    
066500     END-IF                                                               
066600                                                                          
066700*----NKL-FÄLT-UT                                                          
066800     INSPECT MID-IDINLVGN-IN REPLACING LEADING SPACE BY ZERO              
066900     INSPECT MID-IDINLVGN-UT REPLACING LEADING SPACE BY ZERO              
067000                                                                          
067100     IF MID-IDINLVGN-IN      = ALL '+' AND                                
067200        MID-ADINLOMR-IN      = ALL '+' AND                                
067300        MID-ADINLOMR-NXT-IN  = ALL '+'                                    
067400        IF MID-IDINLVGN-UT      = ZERO  AND                               
067500           MID-ADINLOMR-UT      = SPACE AND                               
067600           MID-ADINLOMR-NXT-UT  = SPACE                                   
067700*-FEL - 401                                                               
067800           MOVE NEJ          TO NYCKLAR-SW                                
067900           MOVE ERR-WRONG-KEY       TO MED-IDMFSFEL                       
068000           PERFORM S01-CALL-WMEDKONV-FEL                                  
068100        ELSE                                                              
068200*----------GAMLA NYCKLAR                                                  
068300           PERFORM BB-GAMLA-NKL                                           
068400        END-IF                                                            
068500     ELSE                                                                 
068600*-------NYA NYCKLAR                                                       
068700        MOVE SPACE TO MFS-KDTRTYP                                         
068800        MOVE '7' TO MFS-IDPFK                                             
068900        PERFORM BD-KONTR-PLAC-ADR-LIKA                                    
069000        IF NYCKLAR-OK                                                     
069100           PERFORM BA-FORMELLA-KONTR                                      
069200           MOVE NEJ         TO MID-FLPRIO                                 
069300                               MID-FLSATS                                 
069400           PERFORM MFS-RENSA-FAELT-UT                                     
069500        END-IF                                                            
069600     END-IF                                                               
069700                                                                          
069800     IF W-IDTRANS = '6102' OR '6125'                                      
069900        MOVE NEJ         TO MOD-FLPRIO                                    
070000                            MOD-FLSATS                                    
070100     END-IF                                                               
070200                                                                          
070300     IF NYCKLAR-OK                                                        
070400        PERFORM BC-KOLLA-NKL-BAS                                          
070500     END-IF                                                               
070600                                                                          
070700     IF GODK-MID OR HELP-MID                                              
070800        CONTINUE                                                          
070900     ELSE                                                                 
071000        MOVE MFS-RENSA-FAELT     TO MOD-IDINLVGN-UT                       
071100                                    MOD-ADINLOMR-UT                       
071200                                    MOD-ADINLOMR-NXT-UT                   
071300                                    MOD-IDDC-UT                           
071400                                    MOD-TEMFSFEL                          
071500                                    MOD-FLPRIO                            
071600                                    MOD-FLSATS                            
071700        PERFORM MFS-RENSA-FAELT-IN                                        
071800        PERFORM MFS-RENSA-FAELT-UT                                        
071900     END-IF                                                               
072000                                                                          
072100     PERFORM MFS-RENSA-FAELT-IN                                           
072200     .                                                                    
072300     EJECT                                                                
072400*----------------------------------------------------------------*        
072500 BA-FORMELLA-KONTR SECTION.                                               
072600                                                                          
072700*----TA REDA PÅ VILKEN NKLTYP, KOLLA VAGN,                                
072800*----SAMT FLYTTA MID-IN TILL MOD-UT                                       
072900*----VAGN                                                                 
073000     IF MID-IDINLVGN-IN     NOT = ALL '+' AND                             
073100        MID-ADINLOMR-IN         = ALL '+' AND                             
073200        MID-ADINLOMR-NXT-IN     = ALL '+'                                 
073300        PERFORM BAA-KONTR-VAGN                                            
073400        MOVE MID-IDINLVGN-IN       TO MOD-IDINLVGN-UT                     
073500        MOVE SPACE                 TO MOD-ADINLOMR-UT                     
073600                                      MOD-ADINLOMR-NXT-UT                 
073700        MOVE '1'                   TO NKLTYP-SW                           
073800     ELSE                                                                 
073900*-------VAGN-PLAC                                                         
074000        IF MID-IDINLVGN-IN     NOT = ALL '+' AND                          
074100           MID-ADINLOMR-IN     NOT = ALL '+' AND                          
074200           MID-ADINLOMR-NXT-IN     = ALL '+'                              
074300           PERFORM BAA-KONTR-VAGN                                         
074400           MOVE MID-IDINLVGN-IN    TO MOD-IDINLVGN-UT                     
074500           MOVE MID-ADINLOMR-IN    TO MOD-ADINLOMR-UT                     
074600           MOVE SPACE              TO MOD-ADINLOMR-NXT-UT                 
074700           MOVE '1'                TO NKLTYP-SW                           
074800        ELSE                                                              
074900*----------VAGN-PLAC-ADR                                                  
075000           IF MID-IDINLVGN-IN     NOT = ALL '+' AND                       
075100              MID-ADINLOMR-IN     NOT = ALL '+' AND                       
075200              MID-ADINLOMR-NXT-IN NOT = ALL '+'                           
075300              PERFORM BAA-KONTR-VAGN                                      
075400              MOVE MID-IDINLVGN-IN     TO MOD-IDINLVGN-UT                 
075500              MOVE MID-ADINLOMR-IN     TO MOD-ADINLOMR-UT                 
075600              MOVE MID-ADINLOMR-NXT-IN TO MOD-ADINLOMR-NXT-UT             
075700              MOVE '1'             TO NKLTYP-SW                           
075800           ELSE                                                           
075900*-------------PLAC                                                        
076000              IF MID-IDINLVGN-IN      = ALL '+' AND                       
076100                 MID-ADINLOMR-IN  NOT = ALL '+' AND                       
076200                 MID-ADINLOMR-NXT-IN  = ALL '+'                           
076300                 MOVE MID-ADINLOMR-IN TO MOD-ADINLOMR-UT                  
076400                 MOVE SPACE           TO MOD-ADINLOMR-NXT-UT              
076500                 MOVE +000            TO MOD-IDINLVGN-UT                  
076600                 MOVE '2'          TO NKLTYP-SW                           
076700              ELSE                                                        
076800*----------------PLAC-ADR                                                 
076900                 IF MID-IDINLVGN-IN     = ALL '+' AND                     
077000                    MID-ADINLOMR-IN NOT = ALL '+' AND                     
077100                    MID-ADINLOMR-NXT-IN NOT = ALL '+'                     
077200                    MOVE +000            TO MOD-IDINLVGN-UT               
077300                    MOVE MID-ADINLOMR-IN TO MOD-ADINLOMR-UT               
077400                    MOVE MID-ADINLOMR-NXT-IN TO                           
077500                         MOD-ADINLOMR-NXT-UT                              
077600                    MOVE '2'       TO NKLTYP-SW                           
077700                 ELSE                                                     
077800*-FEL - 401                                                               
077900                    MOVE NEJ              TO NYCKLAR-SW                   
078000                    MOVE ERR-WRONG-KEY    TO MED-IDMFSFEL                 
078100                    IF MID-IDINLVGN-IN = ALL '+'                          
078200                       MOVE +000          TO MOD-IDINLVGN-UT              
078300                    ELSE                                                  
078400                       MOVE MID-IDINLVGN-IN TO MOD-IDINLVGN-UT            
078500                    END-IF                                                
078600                    IF MID-ADINLOMR-IN = ALL '+'                          
078700                       MOVE SPACE         TO MOD-ADINLOMR-UT              
078800                    ELSE                                                  
078900                       MOVE MID-ADINLOMR-IN TO MOD-ADINLOMR-UT            
079000                    END-IF                                                
079100                    IF MID-ADINLOMR-NXT-IN = ALL '+'                      
079200                       MOVE SPACE         TO MOD-ADINLOMR-NXT-UT          
079300                    ELSE                                                  
079400                      MOVE MID-ADINLOMR-NXT-IN TO                         
079500                           MOD-ADINLOMR-NXT-UT                            
079600                    END-IF                                                
079700                    PERFORM S01-CALL-WMEDKONV-FEL                         
079800                 END-IF                                                   
079900              END-IF                                                      
080000           END-IF                                                         
080100        END-IF                                                            
080200     END-IF                                                               
080300     .                                                                    
080400     EJECT                                                                
080500*----------------------------------------------------------------*        
080600 BAA-KONTR-VAGN    SECTION.                                               
080700                                                                          
080800     IF MID-IDINLVGN-IN = ZERO OR                                         
080900        MID-IDINLVGN-IN NOT NUMERIC                                       
081000*-FEL 401                                                                 
081100        MOVE NEJ                   TO NYCKLAR-SW                          
081200        MOVE ERR-WRONG-KEY         TO MED-IDMFSFEL                        
081300        PERFORM S01-CALL-WMEDKONV-FEL                                     
081400     END-IF                                                               
081500     .                                                                    
081600     EJECT                                                                
081700*----------------------------------------------------------------*        
081800 BB-GAMLA-NKL   SECTION.                                                  
081900                                                                          
082000*----VAGN                                                                 
082100     IF MID-IDINLVGN-UT     > ZERO AND                                    
082200        MID-ADINLOMR-UT     = SPACE AND                                   
082300        MID-ADINLOMR-NXT-UT = SPACE                                       
082400        MOVE '1'                   TO NKLTYP-SW                           
082500        MOVE MID-IDINLVGN-UT       TO MOD-IDINLVGN-UT                     
082600        MOVE SPACE                 TO MOD-ADINLOMR-UT                     
082700                                      MOD-ADINLOMR-NXT-UT                 
082800     ELSE                                                                 
082900*-------VAGN-PLAC                                                         
083000        IF MID-IDINLVGN-UT     > ZERO AND                                 
083100           MID-ADINLOMR-UT NOT = SPACE AND                                
083200           MID-ADINLOMR-NXT-UT = SPACE                                    
083300           MOVE '1'                TO NKLTYP-SW                           
083400           MOVE MID-IDINLVGN-UT    TO MOD-IDINLVGN-UT                     
083500           MOVE MID-ADINLOMR-UT    TO MOD-ADINLOMR-UT                     
083600           MOVE SPACE              TO MOD-ADINLOMR-NXT-UT                 
083700        ELSE                                                              
083800*----------VAGN-PLAC-ADR                                                  
083900           IF MID-IDINLVGN-UT         > ZERO AND                          
084000              MID-ADINLOMR-UT     NOT = SPACE AND                         
084100              MID-ADINLOMR-NXT-UT NOT = SPACE                             
084200              MOVE '1'                 TO NKLTYP-SW                       
084300              MOVE MID-IDINLVGN-UT     TO MOD-IDINLVGN-UT                 
084400              MOVE MID-ADINLOMR-UT     TO MOD-ADINLOMR-UT                 
084500              MOVE MID-ADINLOMR-NXT-UT TO MOD-ADINLOMR-NXT-UT             
084600           ELSE                                                           
084700*-------------PLAC                                                        
084800              IF MID-IDINLVGN-UT         = ZERO AND                       
084900                 MID-ADINLOMR-UT     NOT = SPACE AND                      
085000                 MID-ADINLOMR-NXT-UT     = SPACE                          
085100                 MOVE '2'             TO NKLTYP-SW                        
085200                 MOVE +000            TO MOD-IDINLVGN-UT                  
085300                 MOVE SPACE           TO MOD-ADINLOMR-NXT-UT              
085400                 MOVE MID-ADINLOMR-UT TO MOD-ADINLOMR-UT                  
085500              ELSE                                                        
085600*----------------PLAC-ADR                                                 
085700                 IF MID-IDINLVGN-UT   = ZERO AND                          
085800                    MID-ADINLOMR-UT NOT = SPACE AND                       
085900                    MID-ADINLOMR-NXT-UT NOT = SPACE                       
086000                    MOVE '2'             TO NKLTYP-SW                     
086100                    MOVE +000            TO MOD-IDINLVGN-UT               
086200                    MOVE MID-ADINLOMR-UT TO MOD-ADINLOMR-UT               
086300                   MOVE MID-ADINLOMR-NXT-UT TO MOD-ADINLOMR-NXT-UT        
086400                 END-IF                                                   
086500              END-IF                                                      
086600           END-IF                                                         
086700        END-IF                                                            
086800     END-IF                                                               
086900     .                                                                    
087000     EJECT                                                                
087100*----------------------------------------------------------------*        
087200 BC-KOLLA-NKL-BAS SECTION.                                                
087300                                                                          
087400*----KONTROLLER BEROENDE PÅ NKLTYP                                        
087500*---- 1 = MED VAGN                                                        
087600*---- 2 = UTAN VAGN                                                       
087700                                                                          
087800     IF NKLTYP1                                                           
087900        PERFORM BCA-KOLLA-NKLTYP1                                         
088000     END-IF                                                               
088100                                                                          
088200     IF NKLTYP2                                                           
088300        PERFORM BCB-KOLLA-NKLTYP2                                         
088400     END-IF                                                               
088500     .                                                                    
088600     EJECT                                                                
088700*----------------------------------------------------------------*        
088800 BCA-KOLLA-NKLTYP1 SECTION.                                               
088900                                                                          
089000*    HÄR ANVÄNDS UT-NYCKLARNA VID KONTROLL                                
089100*    (DESSA ÄR INITIERADE I BA- RESP BB-)                                 
089200                                                                          
089300     PERFORM BCAF-INIT-INLF1-BAS                                          
089400                                                                          
089500     IF SEGMENT-SAKNAS                                                    
089600        IF MOD-IDINLVGN-UT > ZERO                                         
089700           IF MOD-ADINLOMR-UT NOT = SPACE AND                             
089800              MOD-ADINLOMR-UT NOT = LOW-VALUE                             
089900*-------------OK KOLLA PLAC                                               
090000              PERFORM BCAD-KOLLA-PLAC                                     
090100              IF NYCKLAR-OK                                               
090200                 IF MOD-ADINLOMR-NXT-UT NOT = SPACE AND                   
090300                    MOD-ADINLOMR-NXT-UT NOT = LOW-VALUE                   
090400*-------------------OK KOLLA ADR                                          
090500                    PERFORM BCAE-KOLLA-ADR                                
090600                 END-IF                                                   
090700              END-IF                                                      
090800           ELSE                                                           
090900*-FEL - 401                                                               
091000*-------------PLAC  SAKNAS                                                
091100              MOVE NEJ             TO NYCKLAR-SW                          
091200              MOVE ERR-WRONG-KEY   TO MED-IDMFSFEL                        
091300              PERFORM S01-CALL-WMEDKONV-FEL                               
091400           END-IF                                                         
091500        END-IF                                                            
091600     ELSE                                                                 
091700*-------VAGN FINNS                                                        
091800        PERFORM BCAG-INIT-INLA                                            
091900        IF SEGMENT-FINNS                                                  
092000           IF MOD-IDINLVGN-UT     > ZERO AND                              
092100              MOD-ADINLOMR-UT     = SPACE AND                             
092200              MOD-ADINLOMR-NXT-UT = SPACE                                 
092300              PERFORM BCAA-VAGN                                           
092400           ELSE                                                           
092500              IF MOD-IDINLVGN-UT      > ZERO AND                          
092600                 MOD-ADINLOMR-UT  NOT = SPACE AND                         
092700                 MOD-ADINLOMR-NXT-UT  = SPACE                             
092800                 PERFORM BCAB-VAGN-PLAC                                   
092900              ELSE                                                        
093000                 IF MOD-IDINLVGN-UT   > ZERO AND                          
093100                    MOD-ADINLOMR-UT NOT = SPACE AND                       
093200                    MOD-ADINLOMR-NXT-UT NOT = SPACE                       
093300                    PERFORM BCAC-VAGN-PLAC-ADR                            
093400                 END-IF                                                   
093500              END-IF                                                      
093600           END-IF                                                         
093700        END-IF                                                            
093800     END-IF                                                               
093900     .                                                                    
094000     EJECT                                                                
094100*----------------------------------------------------------------*        
094200 BCAA-VAGN   SECTION.                                                     
094300                                                                          
094400     MOVE RAD-IDINLVGN             TO MOD-SPAR-IDINLVGN                   
094500     MOVE RAD-ADINLOMR             TO MOD-SPAR-ADINLOMR                   
094600                                      MOD-ADINLOMR-UT                     
094700     MOVE RAD-ADINLOMR-NXT         TO MOD-SPAR-ADINLOMR-NXT               
094800                                      MOD-ADINLOMR-NXT-UT                 
094900     .                                                                    
095000     EJECT                                                                
095100*----------------------------------------------------------------*        
095200 BCAB-VAGN-PLAC   SECTION.                                                
095300     IF RAD-ADINLOMR = MOD-ADINLOMR-UT                                    
095400        MOVE RAD-IDINLVGN          TO MOD-SPAR-IDINLVGN                   
095500        MOVE RAD-ADINLOMR          TO MOD-SPAR-ADINLOMR                   
095600        MOVE RAD-ADINLOMR-NXT      TO MOD-SPAR-ADINLOMR-NXT               
095700                                      MOD-ADINLOMR-NXT-UT                 
095800     ELSE                                                                 
095900*------ ANGIVEN PLACERING STÄMMER EJ                                      
096000*-FEL - 196                                                               
096100*       BYT I NKL-FÄLT                                                    
096200        MOVE RAD-IDINLVGN          TO MOD-SPAR-IDINLVGN                   
096300        MOVE RAD-ADINLOMR          TO MOD-SPAR-ADINLOMR                   
096400                                      MOD-ADINLOMR-UT                     
096500        MOVE RAD-ADINLOMR-NXT      TO MOD-SPAR-ADINLOMR-NXT               
096600                                      MOD-ADINLOMR-NXT-UT                 
096700        MOVE ERR-PLAC-ADR-FINNS    TO MED-IDMFSINF                        
096800        PERFORM S02-CALL-WMEDKONV-INF                                     
096900     END-IF                                                               
097000     .                                                                    
097100     EJECT                                                                
097200*----------------------------------------------------------------*        
097300 BCAC-VAGN-PLAC-ADR    SECTION.                                           
097400                                                                          
097500     IF RAD-ADINLOMR = MOD-ADINLOMR-UT                                    
097600        MOVE RAD-IDINLVGN          TO MOD-SPAR-IDINLVGN                   
097700        MOVE RAD-ADINLOMR          TO MOD-SPAR-ADINLOMR                   
097800     ELSE                                                                 
097900*------ ANGIVEN PLACERING STÄMMER EJ                                      
098000*       BYT I NKL-FÄLT                                                    
098100*-FEL - 196                                                               
098200        MOVE RAD-IDINLVGN          TO MOD-SPAR-IDINLVGN                   
098300        MOVE RAD-ADINLOMR          TO MOD-SPAR-ADINLOMR                   
098400                                      MOD-ADINLOMR-UT                     
098500        MOVE ERR-PLAC-ADR-FINNS    TO MED-IDMFSINF                        
098600        PERFORM S02-CALL-WMEDKONV-INF                                     
098700     END-IF                                                               
098800                                                                          
098900     IF RAD-ADINLOMR-NXT = MOD-ADINLOMR-NXT-UT                            
099000        MOVE RAD-ADINLOMR-NXT      TO MOD-SPAR-ADINLOMR-NXT               
099100     ELSE                                                                 
099200*------ ANGIVEN PLACERING STÄMMER EJ                                      
099300*       BYT I NKL-FÄLT                                                    
099400*-FEL - 196                                                               
099500        MOVE RAD-ADINLOMR-NXT      TO MOD-SPAR-ADINLOMR-NXT               
099600                                      MOD-ADINLOMR-NXT-UT                 
099700        MOVE ERR-PLAC-ADR-FINNS    TO MED-IDMFSINF                        
099800        PERFORM S02-CALL-WMEDKONV-INF                                     
099900     END-IF                                                               
100000     .                                                                    
100100     EJECT                                                                
100200*----------------------------------------------------------------*        
100300 BCAD-KOLLA-PLAC   SECTION.                                               
100400                                                                          
100500*----KOLLA PLACERING - LÄS PLAA                                           
100600                                                                          
100700     MOVE '6005'                   TO WGX-IDHTYP                          
100800     MOVE MOD-ADINLOMR-UT          TO W-ADINLOMR                          
100900                                                                          
101000     PERFORM IMS-GU-PLAA-G111                                             
101100     IF SEGMENT-FINNS                                                     
101200        MOVE MOD-IDINLVGN-UT       TO MOD-SPAR-IDINLVGN                   
101300        MOVE MOD-ADINLOMR-UT       TO MOD-SPAR-ADINLOMR                   
101400        MOVE SPACE                 TO MOD-SPAR-ADINLOMR-NXT               
101500     ELSE                                                                 
101600*-FEL - 401                                                               
101700        MOVE +000                  TO MOD-SPAR-IDINLVGN                   
101800        MOVE SPACE                 TO MOD-SPAR-ADINLOMR                   
101900                                      MOD-SPAR-ADINLOMR-NXT               
102000        MOVE NEJ                   TO NYCKLAR-SW                          
102100        MOVE ERR-WRONG-KEY         TO MED-IDMFSFEL                        
102200        PERFORM S01-CALL-WMEDKONV-FEL                                     
102300     END-IF                                                               
102400     .                                                                    
102500     EJECT                                                                
102600*----------------------------------------------------------------*        
102700 BCAE-KOLLA-ADR SECTION.                                                  
102800                                                                          
102900*----KOLLA ADRESS - LÄS PLAA                                              
103000                                                                          
103100     MOVE '6005'                   TO WGX-IDHTYP                          
103200     MOVE MOD-ADINLOMR-NXT-UT        TO W-ADINLOMR                        
103300                                                                          
103400     PERFORM IMS-GU-PLAA-G111                                             
103500     IF SEGMENT-FINNS                                                     
103600        MOVE MOD-ADINLOMR-NXT-UT     TO MOD-SPAR-ADINLOMR-NXT             
103700     ELSE                                                                 
103800*-FEL - 401                                                               
103900        MOVE +000                    TO MOD-SPAR-IDINLVGN                 
104000        MOVE SPACE                   TO MOD-SPAR-ADINLOMR                 
104100                                        MOD-SPAR-ADINLOMR-NXT             
104200        MOVE NEJ                     TO NYCKLAR-SW                        
104300        MOVE ERR-WRONG-KEY           TO MED-IDMFSFEL                      
104400        PERFORM S01-CALL-WMEDKONV-FEL                                     
104500     END-IF                                                               
104600     .                                                                    
104700     EJECT                                                                
104800*----------------------------------------------------------------*        
104900 BCAF-INIT-INLF1-BAS SECTION.                                             
105000                                                                          
105100     MOVE LOW-VALUE           TO W-W6D1F1KY-MIN-X                         
105200     MOVE MOD-IDINLVGN-UT     TO WF1-MIN-IDINLVGN                         
105300                                                                          
105400     MOVE HIGH-VALUE          TO W-W6D1F1KY-MAX-X                         
105500     MOVE MOD-IDINLVGN-UT     TO WF1-MAX-IDINLVGN                         
105600                                                                          
105700     PERFORM IMS-GU-INLF1-D111                                            
105800     .                                                                    
105900     EJECT                                                                
106000*----------------------------------------------------------------*        
106100 BCAG-INIT-INLA     SECTION.                                              
106200                                                                          
106300     MOVE SEQF-IDLEVNR       TO W-IDLEVNR                                 
106400     MOVE SEQF-IDFS          TO W-IDFS                                    
106410     MOVE W-IDDC             TO W-IDDC-101KY                              
106500     MOVE SEQF-TIAVIDAT      TO W-TIAVIDAT                                
106600     MOVE SEQF-IDRADNR-INL   TO W-IDRADNR-INL                             
106700     MOVE SEQF-IDRADNR       TO W-IDRADNR                                 
106800                                                                          
106900     PERFORM IMS-GU-INLA-D121                                             
107000     .                                                                    
107100     EJECT                                                                
107200*----------------------------------------------------------------*        
107300 BCB-KOLLA-NKLTYP2 SECTION.                                               
107400                                                                          
107500     MOVE +000                     TO MOD-SPAR-IDINLVGN                   
107600     MOVE '6005'                   TO WGX-IDHTYP                          
107700                                                                          
107800     PERFORM  BCBA-KOLLA-NKLTYP2-PLAC                                     
107900                                                                          
108000     IF NYCKLAR-OK                                                        
108100        IF MOD-ADINLOMR-NXT-UT NOT = SPACE                                
108200           PERFORM BCBB-KOLLA-NKLTYP2-ADR                                 
108300        ELSE                                                              
108400           MOVE SPACE                TO MOD-SPAR-ADINLOMR-NXT             
108500        END-IF                                                            
108600     END-IF                                                               
108700     .                                                                    
108800     EJECT                                                                
108900*----------------------------------------------------------------*        
109000 BCBA-KOLLA-NKLTYP2-PLAC SECTION.                                         
109100                                                                          
109200*--- KOLLA PLACERING - LÄS PLAA                                           
109300                                                                          
109400     MOVE MOD-ADINLOMR-UT            TO W-ADINLOMR                        
109500                                                                          
109600     PERFORM IMS-GU-PLAA-G111                                             
109700     IF SEGMENT-SAKNAS                                                    
109800*-FEL - 401                                                               
109900        MOVE +000                    TO MOD-SPAR-IDINLVGN                 
110000        MOVE SPACE                   TO MOD-SPAR-ADINLOMR                 
110100                                        MOD-SPAR-ADINLOMR-NXT             
110200        MOVE NEJ                     TO NYCKLAR-SW                        
110300        MOVE ERR-WRONG-KEY           TO MED-IDMFSFEL                      
110400        PERFORM S01-CALL-WMEDKONV-FEL                                     
110500     ELSE                                                                 
110600        MOVE MOD-ADINLOMR-UT         TO MOD-SPAR-ADINLOMR                 
110700     END-IF                                                               
110800     .                                                                    
110900     EJECT                                                                
111000*----------------------------------------------------------------*        
111100 BCBB-KOLLA-NKLTYP2-ADR  SECTION.                                         
111200                                                                          
111300*--- KOLLA ADRESS - LÄS PLAA                                              
111400                                                                          
111500     MOVE MOD-ADINLOMR-NXT-UT        TO W-ADINLOMR                        
111600     PERFORM IMS-GU-PLAA-G111                                             
111700     IF SEGMENT-SAKNAS                                                    
111800*-FEL - 401                                                               
111900        MOVE +000                    TO MOD-SPAR-IDINLVGN                 
112000        MOVE SPACE                   TO MOD-SPAR-ADINLOMR                 
112100                                        MOD-SPAR-ADINLOMR-NXT             
112200        MOVE NEJ                     TO NYCKLAR-SW                        
112300        MOVE ERR-WRONG-KEY           TO MED-IDMFSFEL                      
112400        PERFORM S01-CALL-WMEDKONV-FEL                                     
112500     ELSE                                                                 
112600        MOVE MOD-ADINLOMR-NXT-UT     TO MOD-SPAR-ADINLOMR-NXT             
112700     END-IF                                                               
112800     .                                                                    
112900     EJECT                                                                
113000*----------------------------------------------------------------*        
113100 BD-KONTR-PLAC-ADR-LIKA SECTION.                                          
113200                                                                          
113300*----PLAC-ADR                                                             
113400     IF MID-ADINLOMR-IN     NOT = ALL '+' AND                             
113500        MID-ADINLOMR-NXT-IN NOT = ALL '+'                                 
113600        IF MID-ADINLOMR-IN = MID-ADINLOMR-NXT-IN                          
113700*----------FEL KAN EJ VARA LIKA                                           
113800*-FEL - 401                                                               
113900           MOVE NEJ                       TO NYCKLAR-SW                   
114000           MOVE ERR-WRONG-KEY             TO MED-IDMFSFEL                 
114100           MOVE MID-ADINLOMR-IN           TO MOD-ADINLOMR-UT              
114200           MOVE MID-ADINLOMR-NXT-IN       TO MOD-ADINLOMR-NXT-UT          
114300           PERFORM S01-CALL-WMEDKONV-FEL                                  
114400        END-IF                                                            
114500     END-IF                                                               
114600     .                                                                    
114700     EJECT                                                                
114800*----------------------------------------------------------------*        
114900 F-LAES-VISA-INFO SECTION.                                                
115000                                                                          
115100     MOVE JA                 TO INDATA-SW                                 
115200     IF HELP-MID                                                          
115300        PERFORM FA-MID-TILL-MOD                                           
115400     END-IF                                                               
115500     IF HELP-MID OR EGEN-MID                                              
115600        IF EGEN-MID                                                       
115700           PERFORM FB-KOLLA-PRIO-SATS                                     
115800        END-IF                                                            
115900        IF INDATA-OK                                                      
116000           IF MID-RAD(1) = ALL '+' AND                                    
116100              MID-RAD(2) = ALL '+' AND                                    
116200              MID-RAD(3) = ALL '+' AND                                    
116300              MID-RAD(4) = ALL '+' AND                                    
116400              MID-RAD(5) = ALL '+' AND                                    
116500              MID-RAD(6) = ALL '+' AND                                    
116600              MID-RAD(7) = ALL '+' AND                                    
116700              MID-RAD(8) = ALL '+' AND                                    
116800              MID-RAD(9) = ALL '+' AND                                    
116900              MID-RAD(10) = ALL '+' AND                                   
117000              MID-RAD(11) = ALL '+' AND                                   
117100              MID-RAD(12) = ALL '+'                                       
117200              MOVE +1 TO IX                                               
117300              PERFORM UNTIL IX > 12                                       
117400                PERFORM MFS-RENSA-RAD-FAELT-UT                            
117500                ADD +1 TO IX                                              
117600              END-PERFORM                                                 
117700*             CONTINUE                                                    
117800           ELSE                                                           
117900              IF MFS-FIRST OR HELP-MID                                    
118000                 CONTINUE                                                 
118100              ELSE                                                        
118200                  MOVE INF-PRESS-PF11 TO MED-IDMFSFEL                     
118300                  PERFORM S01-CALL-WMEDKONV-FEL                           
118400                  PERFORM MFS-ROER-EJ-FAELT-IN                            
118500                  PERFORM MFS-ROER-EJ-FAELT-UT                            
118600                  PERFORM MFS-LAES-IN-IGEN                                
118700              END-IF                                                      
118800           END-IF                                                         
118900        END-IF                                                            
119000     END-IF                                                               
119100     IF INDATA-FEL                                                        
119200        PERFORM MFS-ROER-EJ-FAELT-UT                                      
119300        PERFORM MFS-LAES-IN-IGEN                                          
119400     END-IF                                                               
119500     PERFORM MFS-RENSA-FAELT-IN                                           
119600     .                                                                    
119700     EJECT                                                                
119800*----------------------------------------------------------------*        
119900 FA-MID-TILL-MOD  SECTION.                                                
120000                                                                          
120100     IF MID-FLPRIO = ALL '+' OR                                           
120200        MID-FLPRIO = SPACE                                                
120300        MOVE NEJ                   TO MOD-FLPRIO                          
120400     ELSE                                                                 
120500        MOVE MID-FLPRIO            TO MOD-FLPRIO                          
120600     END-IF                                                               
120700     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLPRIO-ATTR                        
120800                                                                          
120900     IF MID-FLSATS = ALL '+' OR                                           
121000        MID-FLSATS = SPACE                                                
121100        MOVE NEJ                   TO MOD-FLSATS                          
121200     ELSE                                                                 
121300        MOVE MID-FLSATS            TO MOD-FLSATS                          
121400     END-IF                                                               
121500     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLSATS-ATTR                        
121600     PERFORM FAA-MID-RADER-TILL-MOD                                       
121700     .                                                                    
121800     EJECT                                                                
121900*----------------------------------------------------------------*        
122000 FAA-MID-RADER-TILL-MOD  SECTION.                                         
122100                                                                          
122200     MOVE  +1             TO IX                                           
122300     PERFORM UNTIL IX > MAX-IX                                            
122400        IF MID-RAD(IX) = ALL '+'                                          
122500           MOVE MFS-RENSA-FAELT TO MOD-IDARTNR(IX)                        
122600                                   MOD-KVINLART(IX)                       
122700                                   MOD-IDLEVNR(IX)                        
122800                                   MOD-IDOKOLLI(IX)                       
122900        ELSE                                                              
123000          INSPECT MID-IDARTNR(IX)  REPLACING LEADING SPACE BY ZERO        
123100          INSPECT MID-KVINLART(IX) REPLACING LEADING SPACE BY ZERO        
123300          INSPECT MID-IDOKOLLI(IX) REPLACING LEADING SPACE BY ZERO        
123400           IF MID-IDARTNR(IX) = ALL '+'                                   
123500              MOVE SPACE              TO MOD-IDARTNR(IX)                  
123600           ELSE                                                           
123700              MOVE MID-IDARTNR(IX)    TO MOD-IDARTNR(IX)                  
123800           END-IF                                                         
123900           IF MID-KVINLART(IX) = ALL '+'                                  
124000              MOVE SPACE              TO MOD-KVINLART(IX)                 
124100           ELSE                                                           
124200              MOVE MID-KVINLART(IX)   TO MOD-KVINLART(IX)                 
124300           END-IF                                                         
124400           IF MID-IDLEVNR(IX) = ALL '+'                                   
124500              MOVE SPACE              TO MOD-IDLEVNR(IX)                  
124600           ELSE                                                           
124700              MOVE MID-IDLEVNR(IX)    TO MOD-IDLEVNR(IX)                  
124800           END-IF                                                         
124900           IF MID-IDOKOLLI(IX) = ALL '+'                                  
125000              MOVE SPACE              TO MOD-IDOKOLLI(IX)                 
125100           ELSE                                                           
125200              MOVE MID-IDOKOLLI(IX)   TO MOD-IDOKOLLI(IX)                 
125300           END-IF                                                         
125400           MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDARTNR-ATTR(IX)             
125500                                         MOD-KVINLART-ATTR(IX)            
125600                                         MOD-IDLEVNR-ATTR(IX)             
125700                                         MOD-IDOKOLLI-ATTR(IX)            
125800        END-IF                                                            
125900        ADD 1             TO IX                                           
126000     END-PERFORM                                                          
126100     .                                                                    
126200     EJECT                                                                
126300*----------------------------------------------------------------*        
126400 FB-KOLLA-PRIO-SATS SECTION.                                              
126500                                                                          
126600     IF GODK-MID                                                          
126700        IF MID-FLSATS = (JA OR NEJ OR YES)                                
126800           MOVE MID-FLSATS              TO MOD-FLSATS                     
126900        ELSE                                                              
127000           IF EGEN-MID                                                    
127100              IF MID-FLSATS = ALL '+'                                     
127200                 MOVE NEJ               TO MOD-FLSATS                     
127300              ELSE                                                        
127400*-FEL - 001                                                               
127500                 MOVE NEJ                   TO INDATA-SW                  
127600                 MOVE MID-FLSATS            TO MOD-FLSATS                 
127700                 MOVE MFS-ALFA-FAELT-FEL    TO MOD-FLSATS-ATTR            
127800                 MOVE ERR-CORR-HILITE-FLDS  TO MED-IDMFSFEL               
127900                 PERFORM S01-CALL-WMEDKONV-FEL                            
128000              END-IF                                                      
128100           ELSE                                                           
128200              MOVE NEJ                  TO MOD-FLSATS                     
128300           END-IF                                                         
128400        END-IF                                                            
128500        IF MID-FLPRIO = (JA OR NEJ OR YES)                                
128600           MOVE MID-FLPRIO              TO MOD-FLPRIO                     
128700        ELSE                                                              
128800           IF EGEN-MID OR HELP-MID                                        
128900              IF MID-FLPRIO = ALL '+'                                     
129000                 MOVE NEJ               TO MOD-FLPRIO                     
129100              ELSE                                                        
129200*-FEL - 001                                                               
129300                 MOVE NEJ                TO INDATA-SW                     
129400                 MOVE MID-FLPRIO         TO MOD-FLPRIO                    
129500                 MOVE MFS-ALFA-FAELT-FEL TO MOD-FLPRIO-ATTR               
129600                 MOVE ERR-CORR-HILITE-FLDS  TO MED-IDMFSFEL               
129700                 PERFORM S01-CALL-WMEDKONV-FEL                            
129800              END-IF                                                      
129900           ELSE                                                           
130000              MOVE NEJ                  TO MOD-FLPRIO                     
130100           END-IF                                                         
130200        END-IF                                                            
130300     END-IF                                                               
130400     .                                                                    
130500     EJECT                                                                
130600*----------------------------------------------------------------*        
130700 G-KOLLA-INPUT SECTION.                                                   
130800                                                                          
130900     MOVE JA                 TO INDATA-SW                                 
131000     IF MID-RAD(1)     = ALL '+' AND                                      
131100        MID-RAD(2)     = ALL '+' AND                                      
131200        MID-RAD(3)     = ALL '+' AND                                      
131300        MID-RAD(4)     = ALL '+' AND                                      
131400        MID-RAD(5)     = ALL '+' AND                                      
131500        MID-RAD(6)     = ALL '+' AND                                      
131600        MID-RAD(7)     = ALL '+' AND                                      
131700        MID-RAD(8)     = ALL '+' AND                                      
131800        MID-RAD(9)     = ALL '+' AND                                      
131900        MID-RAD(10)    = ALL '+' AND                                      
132000        MID-RAD(11)    = ALL '+' AND                                      
132100        MID-RAD(12)    = ALL '+'                                          
132200                                                                          
132300        MOVE ERR-PF11-AND-NO-DATA  TO MED-IDMFSFEL                        
132400        PERFORM S01-CALL-WMEDKONV-FEL                                     
132500        PERFORM MFS-ROER-EJ-FAELT-UT                                      
132600        PERFORM MFS-LAES-IN-IGEN                                          
132700     ELSE                                                                 
132800        PERFORM GB-KOLLA-PRIO-SATS                                        
132900        IF INDATA-OK                                                      
133000           IF MID-RAD(1) NOT = ALL '+' OR                                 
133100              MID-RAD(2) NOT = ALL '+' OR                                 
133200              MID-RAD(3) NOT = ALL '+' OR                                 
133300              MID-RAD(4) NOT = ALL '+' OR                                 
133400              MID-RAD(5) NOT = ALL '+' OR                                 
133500              MID-RAD(6) NOT = ALL '+' OR                                 
133600              MID-RAD(7) NOT = ALL '+' OR                                 
133700              MID-RAD(8) NOT = ALL '+' OR                                 
133800              MID-RAD(9) NOT = ALL '+' OR                                 
133900              MID-RAD(10) NOT = ALL '+' OR                                
134000              MID-RAD(11) NOT = ALL '+' OR                                
134100              MID-RAD(12) NOT = ALL '+'                                   
134200              IF MFS-UPD-V                                                
134300                PERFORM FAA-MID-RADER-TILL-MOD                            
134400              END-IF                                                      
134500              PERFORM GA-KOLLA-RADER                                      
134600           END-IF                                                         
134700        END-IF                                                            
134800     END-IF                                                               
134900                                                                          
135000     IF INDATA-FEL                                                        
135100       IF MFS-UPD-V                                                       
135200         CONTINUE                                                         
135300       ELSE                                                               
135400         PERFORM MFS-ROER-EJ-FAELT-UT                                     
135500*-------ANVÄNDS MFS-LAES-IN-IGEN FÅR MAN EJ HILITE PÅ RAD-FÄLTEN          
135600*-------EFTER ATT DET VARIT FEL PÅ PRIO/SATS DESSUTOM TAPPAR              
135700*-------MAN RADERNA,                                                      
135800*-------SÅ -- LÄS-IGEN ENBART OM FEL UPPSTÅTT PÅ PRIO-ATTR                
135900         IF RADERFEL-NEJ                                                  
136000            PERFORM MFS-LAES-IN-IGEN                                      
136100         END-IF                                                           
136200       END-IF                                                             
136300     END-IF                                                               
136400     PERFORM MFS-RENSA-FAELT-IN                                           
136500     .                                                                    
136600     EJECT                                                                
136700*----------------------------------------------------------------*        
136800 GA-KOLLA-RADER    SECTION.                                               
136900                                                                          
137000     MOVE NEJ                TO RADERFEL-SW                               
137100     PERFORM GAK-SORTERA-OM                                               
137200     MOVE ZERO               TO MOD-IX                                    
137300     MOVE ZERO               TO IX                                        
137400     ADD 1                   TO IX                                        
137500                                                                          
137600     PERFORM UNTIL IX > MAX-TABIX                                         
137700        MOVE NEJ             TO RADFEL-SW                                 
137800        PERFORM GAH-KOLLA-RAD                                             
137900        IF BEARBTYP1                                                      
138000*----------ARTNR,ANTAL,LEVNR,KOLLI                                        
138100           IF (WT-IDARTNR(IX) = SPAR-MID-IDARTNR-X AND                    
138200               WT-IDLEVNR(IX) = SPAR-MID-IDLEVNR-X)                       
138300*--------------SAMMA ART/LEV                                              
138400               MOVE WT-KVINLART(IX) TO SPAR-MID-KVINLART-X                
138500               ADD SPAR-MID-KVINLART TO SPAR-ACK-KVINLART                 
138600               MOVE SPAR-IDLOPNRM TO WT-IDLOPNRM(IX)                      
138700               PERFORM GAA-BEARB-ART-ANT-LEV-KOLLI                        
138800           ELSE                                                           
138900*-------------NY ART/LEV                                                  
139000              MOVE NEJ                  TO NYA-RADER-SW                   
139100              MOVE ZERO                 TO SPAR-RAD1-KVINLART             
139200              MOVE ZERO                 TO SPAR-ACK-KVINLART              
139300              MOVE WT-IDARTNR(IX) TO SPAR-MID-IDARTNR-X                   
139400              MOVE WT-IDLEVNR(IX) TO SPAR-MID-IDLEVNR-X                   
139500              MOVE WT-KVINLART(IX) TO SPAR-MID-KVINLART-X                 
139600              ADD SPAR-MID-KVINLART TO SPAR-ACK-KVINLART                  
139700              PERFORM GAA-BEARB-ART-ANT-LEV-KOLLI                         
139800           END-IF                                                         
139900           IF NYA-RADER-JA                                                
140000              IF SPAR-ACK-KVINLART > SPAR-RAD1-KVINLART                   
140100*-FEL - 197                                                               
140200                 MOVE NEJ                  TO INDATA-SW                   
140300                 MOVE JA                   TO RADFEL-SW                   
140400                 MOVE ERR-ANT-VERKL-ANT TO MED-IDMFSFEL                   
140500                 PERFORM S01-CALL-WMEDKONV-FEL                            
140600*                MOVE WT-MID-IX(IX)        TO MOD-IX                      
140700                 MOVE IX                   TO MOD-IX                      
140800                 MOVE MFS-ALFA-FAELT-FEL                                  
140900                                 TO MOD-KVINLART-ATTR(MOD-IX)             
141000              END-IF                                                      
141100           END-IF                                                         
141200        ELSE                                                              
141300           IF BEARBTYP2                                                   
141400*-------------LEVNR,KOLLI                                                 
141500              PERFORM GAB-BEARB-LEV-KOLLI                                 
141600           END-IF                                                         
141700        END-IF                                                            
141710        PERFORM S50-PRIM-CONTROL                                          
141800        ADD 1 TO IX                                                       
141900     END-PERFORM                                                          
142000                                                                          
142100     IF RADERFEL-JA                                                       
142200        MOVE NEJ               TO INDATA-SW                               
142300     END-IF                                                               
142400     .                                                                    
142500     EJECT                                                                
142600*----------------------------------------------------------------*        
142700 GAA-BEARB-ART-ANT-LEV-KOLLI SECTION.                                     
142800                                                                          
142900*----LÄSNING FÖR ATT KOLLA RAD SKER FÖRST VIA                             
143000*    IDLEVNR + IDOKOLLI  SOM ÄR UNIKT  (INDEX C ANVÄNDS)                  
143100*----SAKNAS DENNA  SKER LÄSNING OCH KONTROLLER VIA                        
143200*    IDARTNR (INDEX-BAS H ANVÄNDS) SAMT B-INDEX                           
143300                                                                          
143400     MOVE WT-IDLEVNR(IX)       TO WC-IDLEVNR                              
143500                                  WCS-IDLEVNR                             
143600     MOVE WT-IDOKOLLI(IX)      TO WC-IDOKOLLI                             
143700                                  WCS-IDOKOLLI                            
143800                                                                          
143900     PERFORM IMS-GU-INLC-D111                                             
143910     IF SEGMENT-FINNS AND ART-IDDC NOT = W-IDDC                           
143920        MOVE JA                      TO RADFEL-SW                         
143930        MOVE JA                      TO RADERFEL-SW                       
143940        MOVE WT-MID-IX(IX) TO MOD-IX                                      
143950        MOVE ERR-RADNR-1-SAKN TO MED-IDMFSFEL                             
143960        PERFORM S01-CALL-WMEDKONV-FEL                                     
143970        MOVE MFS-ALFA-FAELT-FEL                                           
143980                           TO MOD-IDARTNR-ATTR(MOD-IX)                    
143990                              MOD-IDLEVNR-ATTR(MOD-IX)                    
143991                              MOD-IDOKOLLI-ATTR(MOD-IX)                   
143992                              MOD-KVINLART-ATTR(MOD-IX)                   
144000                                                                          
144010     ELSE                                                                 
144100       IF SEGMENT-SAKNAS                                                  
144200*-------KOLLA   VIDARE                                                    
144300          PERFORM GAAA-BEARB-VIA-INDEX-BAS-H                              
144400       ELSE                                                               
144500          PERFORM GAAB-BEARB-VIA-INDEX-C                                  
144600       END-IF                                                             
144610     END-IF                                                               
144700     .                                                                    
144800     EJECT                                                                
144900*----------------------------------------------------------------*        
145000 GAAA-BEARB-VIA-INDEX-BAS-H SECTION.                                      
145100                                                                          
145200*----FÖRE FORTSATT BEABETNING SKA KONTROLL SKE ATT                        
145300*    ANGIVEN LEVNR ENDAST FINNS EN GÅNG FÖR ARTIKELN                      
145400*    SKULLE DEN FINNAS FLER GGR MÅSTE BARA EN VARA MOTTAGEN               
145500*    KOLLA ATT IDLOPNRM > 0 (=TINLMOT>0)                                  
145600*    IDLOPNRM SPARAS FÖR ATT ANVÄNDAS VID UPPDATERING AV NYA RADER        
145700                                                                          
145800     PERFORM GAAAA-KOLLA-LEVNR                                            
145900                                                                          
146000     IF RADFEL-NEJ                                                        
146100       MOVE WT-IDLOPNRM(IX)      TO WB-IDLOPNRM                           
146200       PERFORM IMS-GU-INLB-D111                                           
146300                                                                          
146400       IF SEGMENT-SAKNAS                                                  
146500*-FEL - 179                                                               
146600          MOVE JA                      TO RADFEL-SW                       
146700          MOVE JA                      TO RADERFEL-SW                     
146800          MOVE WT-MID-IX(IX)           TO MOD-IX                          
146900          MOVE ERR-ARTNR-SAKN        TO MED-IDMFSFEL                      
147000          PERFORM S01-CALL-WMEDKONV-FEL                                   
147100          MOVE MFS-ALFA-FAELT-FEL                                         
147200                     TO MOD-IDARTNR-ATTR(MOD-IX)                          
147300       ELSE                                                               
147400          MOVE ZERO              TO W-IDRADNR                             
147500          PERFORM IMS-GNP-INLB-D121                                       
147600                                                                          
147700          IF SEGMENT-FINNS                                                
147800             IF RAD-IDRADNR = 1                                           
147900                IF RAD-IDLEVNR-KOLLI = SPACE AND                          
148000                   RAD-IDOKOLLI = ZERO                                    
148100                   MOVE RAD-KVINLART TO SPAR-RAD1-KVINLART                
148200                   MOVE JA         TO NYA-RADER-SW                        
148300                ELSE                                                      
148400*-FEL - 182                                                               
148500                   MOVE JA            TO RADFEL-SW                        
148600                   MOVE JA            TO RADERFEL-SW                      
148700                   MOVE WT-MID-IX(IX) TO MOD-IX                           
148800                   MOVE ERR-DIVKOLLI  TO MED-IDMFSFEL                     
148900                   PERFORM S01-CALL-WMEDKONV-FEL                          
149000                   MOVE MFS-ALFA-FAELT-FEL                                
149100                                      TO MOD-IDARTNR-ATTR(MOD-IX)         
149200                                         MOD-IDLEVNR-ATTR(MOD-IX)         
149300                                         MOD-IDOKOLLI-ATTR(MOD-IX)        
149400                                         MOD-KVINLART-ATTR(MOD-IX)        
149500                END-IF                                                    
149600             ELSE                                                         
149700*-FEL - 178                                                               
149800                MOVE JA                TO RADFEL-SW                       
149900                MOVE JA                TO RADERFEL-SW                     
150000                MOVE WT-MID-IX(IX) TO MOD-IX                              
150100                MOVE ERR-RADNR-1-SAKN TO MED-IDMFSFEL                     
150200                PERFORM S01-CALL-WMEDKONV-FEL                             
150300                MOVE MFS-ALFA-FAELT-FEL                                   
150400                                   TO MOD-IDARTNR-ATTR(MOD-IX)            
150500                                      MOD-IDLEVNR-ATTR(MOD-IX)            
150600                                      MOD-IDOKOLLI-ATTR(MOD-IX)           
150700                                      MOD-KVINLART-ATTR(MOD-IX)           
150800             END-IF                                                       
150900          END-IF                                                          
151000       END-IF                                                             
151100     END-IF                                                               
151200     .                                                                    
151300     EJECT                                                                
151400*----------------------------------------------------------------*        
151500 GAAAA-KOLLA-LEVNR      SECTION.                                          
151600                                                                          
151700     MOVE ZERO                 TO IDLEVNR-RAKN                            
151800     MOVE ZERO                 TO IDLOPNRM-RAKN                           
151900     MOVE LOW-VALUE            TO W-W6D1H1KY-MIN-X                        
152000     MOVE WT-IDARTNR(IX)       TO WH1-MIN-IDARTNR                         
152010     MOVE W-IDDC               TO WH1-MIN-IDDC                            
152100     MOVE WT-IDLEVNR(IX)       TO WH1-MIN-IDLEVNR                         
152200                                                                          
152300     MOVE HIGH-VALUE           TO W-W6D1H1KY-MAX-X                        
152400     MOVE WT-IDARTNR(IX)       TO WH1-MAX-IDARTNR                         
152410     MOVE W-IDDC               TO WH1-MAX-IDDC                            
152500     MOVE WT-IDLEVNR(IX)       TO WH1-MAX-IDLEVNR                         
152600                                                                          
152700     PERFORM IMS-GU-INLH1-D111                                            
152800                                                                          
152900     IF SEGMENT-FINNS                                                     
153000        PERFORM UNTIL INLH1-STATUS-CODE = 'GE'                            
153100           MOVE WT-IDLEVNR(IX)      TO  SPAR-MID-IDLEVNR-X                
153200           IF SPAR-MID-IDLEVNR = SEQH-IDLEVNR                             
153300              ADD 1              TO IDLEVNR-RAKN                          
153400           END-IF                                                         
153500           MOVE SEQH-IDLEVNR   TO W-IDLEVNR                               
153600           MOVE SEQH-IDFS      TO W-IDFS                                  
153610           MOVE W-IDDC         TO W-IDDC-101KY                            
153700           MOVE SEQH-TIAVIDAT  TO W-TIAVIDAT                              
153800           MOVE SEQH-IDARTNR   TO W-IDARTNR                               
153900           MOVE SEQH-IDRADNR-INL TO W-IDRADNR-INL                         
154000                                                                          
154100           PERFORM IMS-GU-INLA-D111                                       
154200           IF SEGMENT-FINNS                                               
154300              IF ART-IDLOPNRM > ZERO AND ART-FLKLAR = NEJ                 
154400                 ADD 1              TO IDLOPNRM-RAKN                      
154500                 MOVE ART-IDLOPNRM  TO SPAR-IDLOPNRM                      
154600              END-IF                                                      
154700           END-IF                                                         
154800           PERFORM IMS-GN-INLH1-D111                                      
154900        END-PERFORM                                                       
155000     END-IF                                                               
155100     IF IDLEVNR-RAKN > 1                                                  
155200        IF IDLOPNRM-RAKN > 1                                              
155300*-FEL - 180                                                               
155400           MOVE JA                        TO RADFEL-SW                    
155500           MOVE JA                        TO RADERFEL-SW                  
155600           MOVE WT-MID-IX(IX)             TO MOD-IX                       
155700           MOVE ZERO                      TO WT-IDLOPNRM(IX)              
155800           MOVE ZERO                      TO SPAR-IDLOPNRM                
155900           MOVE ERR-FLERA-LEV-PARTI-ART   TO MED-IDMFSFEL                 
156000           PERFORM S01-CALL-WMEDKONV-FEL                                  
156100           MOVE MFS-ALFA-FAELT-FEL                                        
156200                                    TO MOD-IDARTNR-ATTR(MOD-IX)           
156300                                       MOD-IDLEVNR-ATTR(MOD-IX)           
156400                                       MOD-IDOKOLLI-ATTR(MOD-IX)          
156500                                       MOD-KVINLART-ATTR(MOD-IX)          
156600        ELSE                                                              
156700           IF IDLOPNRM-RAKN = 0                                           
156800*-FEL - 010                                                               
156900*-------------INGET PARTI FINNS FÖR ART                                   
157000              MOVE JA                     TO RADFEL-SW                    
157100              MOVE JA                     TO RADERFEL-SW                  
157200              MOVE WT-MID-IX(IX)          TO MOD-IX                       
157300              MOVE ZERO                   TO WT-IDLOPNRM(IX)              
157400              MOVE ZERO                   TO SPAR-IDLOPNRM                
157500              MOVE ERR-SAKN-I-REG         TO MED-IDMFSFEL                 
157600              PERFORM S01-CALL-WMEDKONV-FEL                               
157700              MOVE MFS-ALFA-FAELT-FEL                                     
157800                                    TO MOD-IDARTNR-ATTR(MOD-IX)           
157900                                       MOD-IDLEVNR-ATTR(MOD-IX)           
158000                                       MOD-IDOKOLLI-ATTR(MOD-IX)          
158100                                       MOD-KVINLART-ATTR(MOD-IX)          
158200           ELSE                                                           
158300              MOVE SPAR-IDLOPNRM          TO WT-IDLOPNRM(IX)              
158400           END-IF                                                         
158500        END-IF                                                            
158600     ELSE                                                                 
158700        IF IDLEVNR-RAKN = ZERO                                            
158800*-FEL - 010                                                               
158900*----------INGET LEVNR FÖR ART                                            
159000           MOVE JA                        TO RADFEL-SW                    
159100           MOVE JA                        TO RADERFEL-SW                  
159200           MOVE WT-MID-IX(IX)             TO MOD-IX                       
159300           MOVE ZERO                      TO WT-IDLOPNRM(IX)              
159400           MOVE ZERO                      TO SPAR-IDLOPNRM                
159500           MOVE ERR-SAKN-I-REG            TO MED-IDMFSFEL                 
159600           PERFORM S01-CALL-WMEDKONV-FEL                                  
159700           MOVE MFS-ALFA-FAELT-FEL                                        
159800                      TO MOD-IDLEVNR-ATTR(MOD-IX)                         
159900        ELSE                                                              
160000           MOVE SPAR-IDLOPNRM             TO WT-IDLOPNRM(IX)              
160100        END-IF                                                            
160200     END-IF                                                               
160300     .                                                                    
160400     EJECT                                                                
160500*----------------------------------------------------------------*        
160600 GAAB-BEARB-VIA-INDEX-C SECTION.                                          
160700                                                                          
160800     MOVE ART-IDLOPNRM         TO SPAR-IDLOPNRM                           
160900                                  WT-IDLOPNRM (IX)                        
161000     MOVE ART-IDRADNR-INL      TO W-IDRADNR-INL                           
161100     MOVE WT-IDARTNR(IX)       TO SPAR-MID-IDARTNR-X                      
161200     IF ART-IDARTNR = SPAR-MID-IDARTNR                                    
161300        MOVE NEJ TO STATUS-SW                                             
161400*--STATUS PÅ SENASTE DL1-ANROP BLIR HÄR ALLTID SEGMENT-FINNS              
161500        PERFORM UNTIL SEGMENT-SAKNAS OR OK-STATUS                         
161600          PERFORM IMS-GNP-INLC-D121                                       
161700          IF RAD-KDINLSTA = 'FPK' OR SPACE OR 'SAK'                       
161800            MOVE JA TO STATUS-SW                                          
161900          END-IF                                                          
162000        END-PERFORM                                                       
162100                                                                          
162200        IF OK-STATUS                                                      
162300          IF SEGMENT-FINNS                                                
162400             MOVE WT-KVINLART(IX) TO SPAR-MID-KVINLART-X                  
162500             IF RAD-KVINLART = SPAR-MID-KVINLART                          
162600                IF (RAD-FLPRIO = JA OR YES) AND                           
162700                  (MOD-FLSATS = JA OR YES)                                
162800*-FEL - 182                                                               
162900                   MOVE JA             TO RADFEL-SW                       
163000                   MOVE JA             TO RADERFEL-SW                     
163100                   MOVE WT-MID-IX(IX)  TO MOD-IX                          
163200                   MOVE ERR-DIVKOLLI   TO MED-IDMFSFEL                    
163300                   PERFORM S01-CALL-WMEDKONV-FEL                          
163400                   MOVE MFS-ALFA-FAELT-FEL                                
163500                                      TO MOD-IDARTNR-ATTR(MOD-IX)         
163600                                         MOD-IDLEVNR-ATTR(MOD-IX)         
163700                                         MOD-IDOKOLLI-ATTR(MOD-IX)        
163800                                         MOD-KVINLART-ATTR(MOD-IX)        
163900                                         MOD-FLSATS-ATTR                  
164000                END-IF                                                    
164100             ELSE                                                         
164200*-FEL - 181                                                               
164300                MOVE JA                TO RADFEL-SW                       
164400                MOVE JA                TO RADERFEL-SW                     
164500                MOVE WT-MID-IX(IX) TO MOD-IX                              
164600                MOVE ERR-ANT-EJ-OK     TO MED-IDMFSFEL                    
164700                PERFORM S01-CALL-WMEDKONV-FEL                             
164800                MOVE MFS-ALFA-FAELT-FEL                                   
164900                           TO MOD-KVINLART-ATTR(MOD-IX)                   
165000             END-IF                                                       
165100           END-IF                                                         
165200        ELSE                                                              
165300          MOVE JA                    TO RADFEL-SW                         
165400          MOVE JA                    TO RADERFEL-SW                       
165500          MOVE WT-MID-IX(IX)         TO MOD-IX                            
165600          MOVE ERR-UPDATE-NOT-VALID TO MED-IDMFSFEL                       
165700          PERFORM S01-CALL-WMEDKONV-FEL                                   
165800          MOVE MFS-ALFA-FAELT-FEL                                         
165900                        TO MOD-IDLEVNR-ATTR(MOD-IX)                       
166000                           MOD-IDOKOLLI-ATTR(MOD-IX)                      
166100        END-IF                                                            
166200     ELSE                                                                 
166300*-FEL - 179                                                               
166400        MOVE JA                      TO RADFEL-SW                         
166500        MOVE JA                      TO RADERFEL-SW                       
166600        MOVE WT-MID-IX(IX)           TO MOD-IX                            
166700        MOVE ERR-ARTNR-SAKN          TO MED-IDMFSFEL                      
166800        PERFORM S01-CALL-WMEDKONV-FEL                                     
166900        MOVE MFS-ALFA-FAELT-FEL                                           
167000                                    TO MOD-IDARTNR-ATTR(MOD-IX)           
167100                                       MOD-IDLEVNR-ATTR(MOD-IX)           
167200     END-IF                                                               
167300     .                                                                    
167400     EJECT                                                                
167500*----------------------------------------------------------------*        
167600 GAB-BEARB-LEV-KOLLI SECTION.                                             
167700                                                                          
167800*----LÄSNING FÖR ATT KOLLA RAD SKER VIA                                   
167900*    IDLEVNR + IDOKOLLI  SOM ÄR UNIKT  (INDEX C ANVÄNDS)                  
168000                                                                          
168100     MOVE WT-IDLEVNR(IX)       TO WC-IDLEVNR                              
168200     MOVE WT-IDOKOLLI(IX)      TO WC-IDOKOLLI                             
168300                                                                          
168400     PERFORM IMS-GU-INLC-D111                                             
168500                                                                          
168600     IF SEGMENT-SAKNAS OR                                                 
168610        (SEGMENT-FINNS AND ART-IDDC NOT = W-IDDC)                         
168700*-FEL - 010                                                               
168800*-------LEVNR OCH KOLLINR SAKNAS                                          
168900        MOVE JA                    TO RADFEL-SW                           
169000        MOVE JA                    TO RADERFEL-SW                         
169100        MOVE WT-MID-IX(IX)         TO MOD-IX                              
169200        MOVE ERR-SAKN-I-REG        TO MED-IDMFSFEL                        
169300        PERFORM S01-CALL-WMEDKONV-FEL                                     
169400        MOVE MFS-ALFA-FAELT-FEL                                           
169500                      TO MOD-IDLEVNR-ATTR(MOD-IX)                         
169600                         MOD-IDOKOLLI-ATTR(MOD-IX)                        
169700     ELSE                                                                 
169800        MOVE WC-IDLEVNR       TO WCS-IDLEVNR-X                            
169900        MOVE WC-IDOKOLLI      TO WCS-IDOKOLLI-X                           
170000        MOVE NEJ              TO STATUS-SW                                
170100        PERFORM IMS-GNP-INLC-D121                                         
170200        IF RAD-FLDIVKLI = NEJ                                             
170300          PERFORM UNTIL SEGMENT-SAKNAS OR OK-STATUS                       
170400            IF RAD-KDINLSTA = 'FPK' OR SPACE OR 'SAK'                     
170500              MOVE JA TO STATUS-SW                                        
170600            END-IF                                                        
170700            PERFORM IMS-GNP-INLC-D121                                     
170800          END-PERFORM                                                     
170900        ELSE                                                              
171000          PERFORM UNTIL SEGMENT-SAKNAS OR OK-STATUS                       
171100            PERFORM UNTIL SEGMENT-SAKNAS OR OK-STATUS                     
171200              IF RAD-KDINLSTA = 'FPK' OR SPACE OR 'SAK'                   
171300                MOVE JA TO STATUS-SW                                      
171400              END-IF                                                      
171500              PERFORM IMS-GNP-INLC-D121                                   
171600            END-PERFORM                                                   
171700            PERFORM IMS-GN-INLC-D111                                      
171800          END-PERFORM                                                     
171900          IF MID-FLSATS = JA OR YES                                       
172000            MOVE JA                    TO RADFEL-SW                       
172100            MOVE JA                    TO RADERFEL-SW                     
172200            MOVE WT-MID-IX(IX)         TO MOD-IX                          
172300            MOVE ERR-DIVKOLLI TO MED-IDMFSFEL                             
172400            PERFORM S01-CALL-WMEDKONV-FEL                                 
172500            MOVE MFS-ALFA-FAELT-FEL                                       
172600                          TO MOD-IDLEVNR-ATTR(MOD-IX)                     
172700                             MOD-IDOKOLLI-ATTR(MOD-IX)                    
172800                             MOD-FLSATS-ATTR                              
172900          END-IF                                                          
173000        END-IF                                                            
173100        IF FEL-STATUS                                                     
173200          MOVE JA                    TO RADFEL-SW                         
173300          MOVE JA                    TO RADERFEL-SW                       
173400          MOVE WT-MID-IX(IX)         TO MOD-IX                            
173500          MOVE ERR-UPDATE-NOT-VALID TO MED-IDMFSFEL                       
173600          PERFORM S01-CALL-WMEDKONV-FEL                                   
173700          MOVE MFS-ALFA-FAELT-FEL                                         
173800                        TO MOD-IDLEVNR-ATTR(MOD-IX)                       
173900                           MOD-IDOKOLLI-ATTR(MOD-IX)                      
174000        END-IF                                                            
174100     END-IF                                                               
174200     .                                                                    
174300     EJECT                                                                
174400*----------------------------------------------------------------*        
174500 GAH-KOLLA-RAD       SECTION.                                             
174600                                                                          
174700     MOVE WT-MID-IX(IX)           TO MOD-IX                               
174800     MOVE MFS-ALFA-FAELT-RAETT    TO MOD-IDARTNR-ATTR (MOD-IX)            
174900                                     MOD-KVINLART-ATTR(MOD-IX)            
175000                                     MOD-IDLEVNR-ATTR (MOD-IX)            
175100                                     MOD-IDOKOLLI-ATTR(MOD-IX)            
175200                                                                          
175300     IF (WT-IDARTNR(IX) NOT = ALL '+' AND                                 
175400         WT-IDARTNR(IX) NUMERIC AND WT-IDARTNR(IX) > ZERO) AND            
175500        (WT-KVINLART(IX) NOT = ALL '+' AND                                
175600         WT-KVINLART(IX) NUMERIC AND WT-KVINLART(IX) > ZERO) AND          
175700        (WT-IDLEVNR(IX) NOT = ALL '+' AND                                 
175800         WT-IDLEVNR(IX) NOT = SPACE) AND                                  
175900        (WT-IDOKOLLI(IX) NOT = ALL '+' AND                                
176000         WT-IDOKOLLI(IX) NUMERIC AND WT-IDOKOLLI(IX) > ZERO)              
176100*-------OK                                                                
176200        MOVE '1'            TO BEARBTYP-SW                                
176300     ELSE                                                                 
176400        IF (WT-IDARTNR(IX) = ALL '+') AND                                 
176500           (WT-KVINLART(IX) = ALL '+') AND                                
176600           (WT-IDLEVNR(IX) NOT = ALL '+' AND                              
176700            WT-IDLEVNR(IX) NOT = SPACE) AND                               
176800           (WT-IDOKOLLI(IX) NOT = ALL '+' AND                             
176900            WT-IDOKOLLI(IX) NUMERIC AND WT-IDOKOLLI(IX) > ZERO)           
177000*----------OK                                                             
177100           MOVE '2'         TO BEARBTYP-SW                                
177200        ELSE                                                              
177300           IF WT-TABELL(IX) NOT = SPACE AND                               
177400              WT-TABELL(IX) NOT NUMERIC                                   
177500*-FEL - 001                                                               
177600              MOVE '0'               TO BEARBTYP-SW                       
177700              MOVE JA                TO RADFEL-SW                         
177800              MOVE JA                TO RADERFEL-SW                       
177900              MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                   
178000              MOVE MFS-ALFA-FAELT-FEL TO MOD-IDARTNR-ATTR (MOD-IX)        
178100                                         MOD-KVINLART-ATTR(MOD-IX)        
178200                                         MOD-IDLEVNR-ATTR (MOD-IX)        
178300                                         MOD-IDOKOLLI-ATTR(MOD-IX)        
178400              PERFORM S01-CALL-WMEDKONV-FEL                               
178500           END-IF                                                         
178600        END-IF                                                            
178700     END-IF                                                               
178800     .                                                                    
178900     EJECT                                                                
179000*----------------------------------------------------------------*        
179100 GAK-SORTERA-OM SECTION.                                                  
179200                                                                          
179300*----FLYTTA MID TILL INTERN-TABELL (ENDAST IFYLLDA RADER)                 
179400*    SOM SEDAN ANVÄNDS VID KONTROLLER OCH SENARE VID UPPDATERING          
179500*----MIDEN'S IX SPARAS FÖR ATT VETA VAR EV FELTEXTER SKA LÄGGAS UT        
179600*    EFTERSOM MID OCH WT-TAB KAN HA OLIKA UTSEENDE EFTER SORTERING        
179700*                                                                         
179800     MOVE ZERO      TO IX                                                 
179900     MOVE ZERO      TO TABIX                                              
180000     ADD  1         TO IX                                                 
180100     PERFORM UNTIL IX > MAX-IX                                            
180200        INSPECT MID-IDARTNR(IX) REPLACING LEADING SPACE BY ZERO           
180300        INSPECT MID-KVINLART(IX) REPLACING LEADING SPACE BY ZERO          
180500        INSPECT MID-IDOKOLLI(IX) REPLACING LEADING SPACE BY ZERO          
180600        IF MID-RAD(IX) NOT = ALL '+'                                      
180700           ADD +1                TO TABIX                                 
180800           MOVE MID-IDARTNR(IX)  TO WT-IDARTNR(TABIX)                     
180900           MOVE MID-KVINLART(IX) TO WT-KVINLART(TABIX)                    
181000           MOVE MID-IDLEVNR(IX)  TO WT-IDLEVNR(TABIX)                     
181100           MOVE MID-IDOKOLLI(IX) TO WT-IDOKOLLI(TABIX)                    
181200           MOVE IX               TO WT-MID-IX(TABIX)                      
181300           MOVE ZERO             TO WT-IDLOPNRM(TABIX)                    
181400        END-IF                                                            
181500        ADD 1  TO IX                                                      
181600     END-PERFORM                                                          
181700                                                                          
181800*----SORTERA INTERN-TABELL ASC 1.ARTNR; 2.LEVNR; 3.KOLLI                  
181900*----DETTA GER ALLA 'LEV-KOLLI' FÖRST                                     
182000                                                                          
182100     MOVE +42                  TO STEGLANGD                               
182200     MOVE +23                  TO NYCKELLANGD                             
182300     COMPUTE ANTAL = TABIX                                                
182400     COMPUTE MAX-TABIX = TABIX                                            
182500                                                                          
182600     IF TABIX > 1                                                         
182700        CALL WINTSOR USING WT-TAB                                         
182800                           STEGLANGD                                      
182900                           ANTAL                                          
183000                           WT-SORT-NKL(1)                                 
183100                           NYCKELLANGD                                    
183200     END-IF                                                               
183300     .                                                                    
183400     EJECT                                                                
183500*----------------------------------------------------------------*        
183600 GB-KOLLA-PRIO-SATS SECTION.                                              
183700                                                                          
183800     IF MID-FLPRIO NOT = ALL '+'                                          
183900        MOVE MID-FLPRIO                  TO MOD-FLPRIO                    
184000     END-IF                                                               
184100                                                                          
184200     IF MID-FLSATS NOT = ALL '+'                                          
184300        MOVE MID-FLSATS                  TO MOD-FLSATS                    
184400     END-IF                                                               
184500                                                                          
184600     IF MOD-FLPRIO = JA OR NEJ OR YES                                     
184700           MOVE MFS-ADD-LAES-IN-FAELT    TO MOD-FLPRIO-ATTR               
184800     ELSE                                                                 
184900*-FEL - 001                                                               
185000           MOVE NEJ                      TO INDATA-SW                     
185100           MOVE NEJ                      TO RADFEL-SW                     
185200           MOVE ERR-CORR-HILITE-FLDS     TO MED-IDMFSFEL                  
185300           PERFORM S01-CALL-WMEDKONV-FEL                                  
185400           MOVE MFS-ALFA-FAELT-FEL       TO MOD-FLPRIO-ATTR               
185500     END-IF                                                               
185600                                                                          
185700     IF MOD-FLSATS = JA OR NEJ OR YES                                     
185800           MOVE MFS-ADD-LAES-IN-FAELT    TO MOD-FLSATS-ATTR               
185900     ELSE                                                                 
186000*-FEL - 001                                                               
186100           MOVE NEJ                      TO INDATA-SW                     
186200           MOVE NEJ                      TO RADFEL-SW                     
186300           MOVE ERR-CORR-HILITE-FLDS     TO MED-IDMFSFEL                  
186400           PERFORM S01-CALL-WMEDKONV-FEL                                  
186500           MOVE MFS-ALFA-FAELT-FEL       TO MOD-FLSATS-ATTR               
186600     END-IF                                                               
186700                                                                          
186800     IF INDATA-OK                                                         
186900        IF ((MOD-FLPRIO = JA OR YES) AND                                  
187000          (MOD-FLSATS = JA OR YES))                                       
187100*-FEL - 183                                                               
187200           MOVE NEJ                      TO INDATA-SW                     
187300           MOVE NEJ                      TO RADFEL-SW                     
187400           MOVE ERR-SATS-PRIO-JA         TO MED-IDMFSFEL                  
187500           PERFORM S01-CALL-WMEDKONV-FEL                                  
187600           MOVE MFS-ALFA-FAELT-FEL       TO MOD-FLPRIO-ATTR               
187700                                            MOD-FLSATS-ATTR               
187800        ELSE                                                              
187900           MOVE MFS-ADD-LAES-IN-FAELT    TO MOD-FLPRIO-ATTR               
188000                                               MOD-FLSATS-ATTR            
188100        END-IF                                                            
188200     END-IF                                                               
188300     .                                                                    
188400     EJECT                                                                
188500******************************************************************        
188600*  UPPDATERA REGISTER                                            *        
188700******************************************************************        
188800*----------------------------------------------------------------*        
188900 H-UPPDATERA SECTION.                                                     
189000                                                                          
189100     MOVE SPACE          TO SPAR-MID-IDARTNR-X                            
189200                            SPAR-MID-IDLEVNR-X                            
189300                            SPAR-ADINLOMR                                 
189400                            SPAR-ADINLOMR-NXT                             
189500                            SPAR-ADINLOMR-NEW                             
189600                            SPAR-KDINLSTA-NEW                             
189700                            SPAR-KDINLSTA-OLD                             
189800     MOVE ZERO           TO T91-MID-KVPOST                                
189900                            SPAR-KVINLART-OLD                             
190000                            SPAR-KVINLART                                 
190100     MOVE NEJ            TO TRANS91-SW                                    
190200     MOVE JA             TO FOERSTA-T91-SW                                
190300                                                                          
190400     MOVE ZERO           TO IX                                            
190500     ADD  1              TO IX                                            
190600     ADD  1              TO MAX-TABIX                                     
190700     MOVE ZERO           TO T91-IX                                        
190800                                                                          
190900                                                                          
191000     PERFORM UNTIL IX > MAX-TABIX                                         
191100        IF ((WT-IDLEVNR(IX) NOT = ALL '+' AND                             
191200           WT-IDLEVNR(IX) NOT = SPACE) AND                                
191300           (WT-IDOKOLLI(IX) NOT = ALL '+' AND                             
191400           WT-IDOKOLLI(IX) > ZERO)) OR                                    
191500           (IX = MAX-TABIX)                                               
191600           PERFORM HA-UPD-RAD                                             
191700           MOVE ZERO     TO SPAR-KVINLART-OLD                             
191800           MOVE SPACE    TO SPAR-ADINLOMR                                 
191900                            SPAR-ADINLOMR-NXT                             
192000                            SPAR-KDINLSTA-NEW                             
192100                            SPAR-KDINLSTA-OLD                             
192200        END-IF                                                            
192300        PERFORM MFS-FORM-ATTR                                             
192400        ADD 1  TO IX                                                      
192500     END-PERFORM                                                          
192600                                                                          
192700     IF TRANS91-JA                                                        
192900        PERFORM S45-SKICKA-W60191                                         
193000     END-IF                                                               
193100     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
193200     PERFORM S02-CALL-WMEDKONV-INF                                        
193300     PERFORM MFS-RENSA-FAELT-UT                                           
193400     .                                                                    
193500     EJECT                                                                
193600*----------------------------------------------------------------*        
193700 HA-UPD-RAD     SECTION.                                                  
193800                                                                          
193900     IF WT-IDARTNR(IX) = ALL '+' AND                                      
194000        IX < MAX-TABIX                                                    
194100*-------BEARBETNING PÅ LEV/KOLLI - RADER                                  
194200        CONTINUE                                                          
194300     ELSE                                                                 
194400        IF WT-IDLEVNR(IX) = SPAR-MID-IDLEVNR-X AND                        
194500           WT-IDARTNR(IX) = SPAR-MID-IDARTNR-X                            
194600*----------INGEN BRYTNING                                                 
194700           CONTINUE                                                       
194800        ELSE                                                              
194900*----------BRYTNING HAR SKETT PÅ ART/ANT/LEV/KOLLI                        
195000*----------BEARBETNING PÅ ART/ANT/LEV/KOLLI - RADER                       
195100*----------HAR BRYTNING SKETT FRÅN LEV/KOLLI SÄNDS TRANS91                
195200*----------TILL W60191 OCH TÖMMES = OMSTART FÖR ART-ANT-LEV-KOLLI         
195300*----------RADNR1                                                         
195400*----------FINNS DENNA SKA INITIERING SKE FÖR TRANS W60191                
195500*----------RADNR 1 OCH DESS FÖRÄNDRINGAR MÅSTE LIGGA FÖRE                 
195600*----------NYA RADER                                                      
195700*----------W60191'S IX FÖR RADNR1 SPARAS I T91-IX                         
195800*----------FÖR ATT SENARE UPPDATERA T91-KVINLART-NEW MED                  
195900*----------RÄTT VÄRDE                                                     
196000*----------SAMT NEDRÄKNING AV KVINLART PÅ RAD 1                           
196100*----------SKULLE KVINLART PÅ RAD 1 BLI = NOLL                            
196200*----------SKER DELETE AV RADNR1 ANNARS REPLACE                           
196300                                                                          
196400           IF SPAR-MID-IDLEVNR-X = SPACE AND                              
196500              SPAR-MID-IDARTNR-X = SPACE                                  
196600*-------------1:A GÅNGEN BRYTNING HAR SKETT FRÅN LEV/KOLLI                
196700*-------------TILL ART/ANT/LEV/KOLLI                                      
196800                                                                          
196900*             IF TRANS91-JA                                               
197000*----------------SKICKA TRANS TILL MPP MED LEV-KOLLI                      
197100                 PERFORM S45-SKICKA-W60191                                
197200*             END-IF                                                      
197300                                                                          
197400              MOVE ZERO              TO T91-MID-KVPOST                    
197500              MOVE NEJ               TO TRANS91-SW                        
197600*-------------TA HAND OM FÖRSTA ART-ANT/LEV/KOLLI                         
197700                                                                          
197800              PERFORM HAC-INIT-RADNR1                                     
197900              MOVE WT-IDLEVNR(IX) TO SPAR-MID-IDLEVNR-X                   
198000              MOVE WT-IDARTNR(IX) TO SPAR-MID-IDARTNR-X                   
198100           ELSE                                                           
198200*-------------TA HAND OM NÄST-KOMMANDE ART-ANT/LEV/KOLLI                  
198300*-------------OM IX = MAX-TABIX ÄR DETTA EN TOM-RAD = AVSLUT              
198400                                                                          
198500              PERFORM HAD-AVSLUTA-RADNR1-FOREGAENDE                       
198600              IF IX < MAX-TABIX                                           
198700                 PERFORM HAC-INIT-RADNR1                                  
198800                 MOVE WT-IDLEVNR(IX) TO SPAR-MID-IDLEVNR-X                
198900                 MOVE WT-IDARTNR(IX) TO SPAR-MID-IDARTNR-X                
199000              END-IF                                                      
199100           END-IF                                                         
199200        END-IF                                                            
199300     END-IF                                                               
199400                                                                          
199500*BEARBETA RADEN                                                           
199600     IF IX < MAX-TABIX                                                    
199700        IF (WT-IDARTNR(IX) NOT = ALL '+' AND                              
199800           WT-IDARTNR(IX) > ZERO) AND                                     
199900           (WT-KVINLART(IX) NOT = ALL '+' AND                             
200000           WT-KVINLART(IX) > ZERO)                                        
200100*----------ARTNR,ANTAL,LEVNR,KOLLI                                        
200200           PERFORM HAA-UPD-ART-ANT-LEV-KOLLI                              
200300        ELSE                                                              
200400*----------LEVNR,KOLLI                                                    
200500           PERFORM HAB-UPD-LEV-KOLLI                                      
200600        END-IF                                                            
200700     END-IF                                                               
200800     .                                                                    
200900     EJECT                                                                
201000*----------------------------------------------------------------*        
201100 HAA-UPD-ART-ANT-LEV-KOLLI  SECTION.                                      
201200                                                                          
201300*----LÄSNING FÖR ATT UPPDATERA RAD SKER FÖRST VIA                         
201400*    IDLEVNR + IDOKOLLI  SOM ÄR UNIKT  (INDEX C ANVÄNDS)                  
201500*    SAKNAS DENNA  SKER LÄSNING VIA                                       
201600*    IDARTNR (INDEX H ANVÄNDS)                                            
201700*    ISRT PÅ NY RAD, HÖGSTA RADNR + 1                                     
201800                                                                          
201900     MOVE WT-IDLEVNR(IX)       TO WC-IDLEVNR                              
202000                                  WCS-IDLEVNR                             
202100     MOVE WT-IDOKOLLI(IX)      TO WC-IDOKOLLI                             
202200                                  WCS-IDOKOLLI                            
202300                                                                          
202400     PERFORM IMS-GU-INLC-D111                                             
202500                                                                          
202600     IF SEGMENT-SAKNAS                                                    
202700*-------LÄS VIDARE                                                        
202800        PERFORM HAAA-UPPD-VIA-INDEX-B                                     
202900     ELSE                                                                 
203000        MOVE ART-IDLOPNRM            TO SPAR-IDLOPNRM                     
203100                                        WS-IDLOPNRM                       
203200        MOVE ART-PRARTSTD            TO SPAR-PRARTSTD                     
203300        MOVE ART-BEFT                TO WS-BEFT                           
203400        MOVE ART-IDARTNR             TO WS-IDARTNR                        
203500        MOVE ART-IDRADNR-INL         TO W-IDRADNR-INL                     
203600        IF ART-KVAVIS-PRIO > ZERO                                         
203700          MOVE JA TO PRIOGODS-SW                                          
203800        END-IF                                                            
203900        PERFORM HAAB-UPPD-VIA-INDEX-C                                     
204000     END-IF                                                               
204100     PERFORM HAAC-KOLLA-PRIOMAERK                                         
204300     .                                                                    
204400     EJECT                                                                
204500*----------------------------------------------------------------*        
204600 HAAA-UPPD-VIA-INDEX-B SECTION.                                           
204700                                                                          
204800     MOVE WT-IDLOPNRM(IX)       TO WB-IDLOPNRM                            
204900                                                                          
205000     PERFORM IMS-GU-INLB-D111                                             
205100                                                                          
205200     IF SEGMENT-FINNS                                                     
205300        MOVE ART-IDLOPNRM            TO SPAR-IDLOPNRM                     
205400                                        WS-IDLOPNRM                       
205500        MOVE ART-PRARTSTD            TO SPAR-PRARTSTD                     
205600        MOVE ART-BEFT                TO WS-BEFT                           
205700        MOVE ART-IDARTNR             TO WS-IDARTNR                        
205800        MOVE ART-IDRADNR-INL         TO W-IDRADNR-INL                     
205900        IF ART-KVAVIS-PRIO > ZERO                                         
206000          MOVE JA TO PRIOGODS-SW                                          
206100        END-IF                                                            
206200                                                                          
206300        IF RADNR1-JA                                                      
206400           MOVE SPAR-IDLOPNRM        TO T91-MID-IDLOPNRM(T91-IX)          
206500           MOVE SPAR-PRARTSTD        TO T91-MID-PRARTSTD(T91-IX)          
206600        END-IF                                                            
206700*-------LÄS SISTA FÖREKOMST FÖR ATT FÅ HÖGSTA RADNR                       
206800        PERFORM IMS-GU-INLB-D111                                          
206900        PERFORM IMS-GHNP-INLB-D121-LAST                                   
207000        PERFORM HAAAA-INIT-ISRT-NY-RAD                                    
207100     END-IF                                                               
207200     .                                                                    
207300     EJECT                                                                
207400*----------------------------------------------------------------*        
207500 HAAAA-INIT-ISRT-NY-RAD SECTION.                                          
207600                                                                          
207700*----INITIERA FÖR INSERT AV NY RAD                                        
207800     COMPUTE SPAR-IDRADNR = RAD-IDRADNR + 1                               
207900                                                                          
208000*----FÄLT FRÅN BILDEN                                                     
208100     MOVE SPAR-IDRADNR              TO RAD-IDRADNR                        
208200     MOVE SPACE                     TO SPAR-ADINLOMR                      
208300     MOVE MID-SPAR-ADINLOMR         TO RAD-ADINLOMR                       
208400                                       SPAR-ADINLOMR-NEW                  
208500     MOVE SPACE                     TO SPAR-ADINLOMR-NXT                  
208600     MOVE MID-SPAR-ADINLOMR-NXT     TO RAD-ADINLOMR-NXT                   
208700     MOVE NEJ                       TO RAD-FLPRIO                         
208800                                       SPAR-FLPRIO                        
208810                                       RAD-FLSVSLS                        
208900     IF MID-FLSATS = ALL '+' OR SPACE                                     
209000        MOVE NEJ                    TO RAD-FLSATS                         
209100     ELSE                                                                 
209110        IF MID-FLSATS = JA OR YES                                         
209111          MOVE JA                     TO RAD-FLSATS                       
209120        ELSE                                                              
209121          MOVE NEJ                    TO RAD-FLSATS                       
209130        END-IF                                                            
209300     END-IF                                                               
209400     MOVE MID-SPAR-IDINLVGN         TO RAD-IDINLVGN                       
209500     MOVE WT-IDLEVNR(IX)            TO SPAR-MID-IDLEVNR-X                 
209600     MOVE SPAR-MID-IDLEVNR          TO RAD-IDLEVNR-KOLLI                  
209700     MOVE WT-IDOKOLLI(IX)           TO SPAR-MID-IDOKOLLI-X                
209800     MOVE SPAR-MID-IDOKOLLI         TO RAD-IDOKOLLI                       
209900     MOVE WT-KVINLART(IX)           TO SPAR-MID-KVINLART-X                
210000     MOVE SPAR-MID-KVINLART         TO RAD-KVINLART                       
210100     MOVE ZERO                      TO SPAR-KVINLART-OLD                  
210200*----FRÅN RADNR 1                                                         
210300     MOVE W-RAD1-FLDIVKLI           TO RAD-FLDIVKLI                       
210400     MOVE W-RAD1-FLINLFP            TO RAD-FLINLFP                        
210500     MOVE W-RAD1-FLKVAANT           TO RAD-FLKVAANT                       
210600     MOVE W-RAD1-FLINLFB            TO RAD-FLINLFB                        
210700     MOVE W-RAD1-IDANSTNR           TO RAD-IDANSTNR                       
210800     MOVE +0                        TO RAD-IDILIRAD                       
210900     MOVE ZERO                      TO RAD-IDILIST                        
211000     MOVE W-RAD1-KDINLPRIO          TO RAD-KDINLPRIO                      
211100     MOVE W-RAD1-KDINLSTA           TO RAD-KDINLSTA                       
211200     MOVE +0000000                  TO RAD-TIUPPDAT                       
211300     COMPUTE SPAR-KVINLART = SPAR-KVINLART - RAD-KVINLART                 
211400                                                                          
211500     PERFORM IMS-ISRT-INLB                                                
211600     PERFORM S40-TRANS-W60191                                             
211700     .                                                                    
211800     EJECT                                                                
211900*----------------------------------------------------------------*        
212000 HAAB-UPPD-VIA-INDEX-C SECTION.                                           
212100                                                                          
212200     PERFORM IMS-GHNP-INLC-D121                                           
212300                                                                          
212400     IF SEGMENT-FINNS                                                     
212500        IF MID-FLSATS = JA OR YES                                         
212600          MOVE JA                  TO RAD-FLSATS                          
212700        END-IF                                                            
212800                                                                          
212900        MOVE MID-SPAR-IDINLVGN      TO RAD-IDINLVGN                       
213000        MOVE RAD-ADINLOMR           TO SPAR-ADINLOMR                      
213100        MOVE MID-SPAR-ADINLOMR      TO RAD-ADINLOMR                       
213200                                       SPAR-ADINLOMR-NEW                  
213300        MOVE RAD-ADINLOMR-NXT       TO SPAR-ADINLOMR-NXT                  
213400        MOVE MID-SPAR-ADINLOMR-NXT  TO RAD-ADINLOMR-NXT                   
213500        MOVE RAD-KVINLART           TO SPAR-KVINLART-OLD                  
213600        MOVE RAD-FLPRIO             TO SPAR-FLPRIO                        
213700                                                                          
213800        IF RAD-KDINLSTA = 'SAK'                                           
213900           MOVE RAD-KDINLSTA        TO SPAR-KDINLSTA-OLD                  
214000           MOVE SPACE               TO RAD-KDINLSTA                       
214100           MOVE SPACE               TO SPAR-KDINLSTA-NEW                  
214200        END-IF                                                            
214300                                                                          
214400        MOVE JA TO GAMMALT-SW                                             
214500                                                                          
214600        PERFORM IMS-REPL-INLC                                             
214700        PERFORM S40-TRANS-W60191                                          
214800     END-IF                                                               
214900     .                                                                    
215000     EJECT                                                                
215100*----------------------------------------------------------------*        
215200 HAAC-KOLLA-PRIOMAERK SECTION.                                            
215300                                                                          
215400     IF GAMMALT-KOLLI AND MOD-FLPRIO = NEJ                                
215500       MOVE NEJ TO GAMMALT-SW                                             
215600     ELSE                                                                 
215700       IF ((MOD-FLPRIO = JA OR YES)  AND                                  
215800           SPAR-FLPRIO = NEJ) OR                                          
215900          ((SPAR-FLPRIO = JA OR YES) AND                                  
216000           MOD-FLPRIO = NEJ)                                              
216100                                                                          
216200          MOVE WT-IDLEVNR(IX)   TO PMRK-IDLEVNR                           
216300          MOVE WT-IDOKOLLI(IX)  TO PMRK-IDOKOLLI                          
216400          MOVE ZERO             TO PMRK-IDLOPNRM                          
216500                                   PMRK-IDRADNR                           
216600                                                                          
216700          CALL W611PMRK USING    PMRK-W611PMRK                            
216800                                 INLB-PMRK-PCB                            
216900                                 INLC-PMRK-PCB                            
217000       END-IF                                                             
217100     END-IF                                                               
217200     .                                                                    
217300     EJECT                                                                
217400*----------------------------------------------------------------*        
217500 HAB-UPD-LEV-KOLLI  SECTION.                                              
217600                                                                          
217700*----LÄS SEGMENT VIA C-INDEX FÖR UPPDATERING                              
217800                                                                          
217900     MOVE WT-IDLEVNR(IX)   TO WC-IDLEVNR                                  
218000                              WCS-IDLEVNR                                 
218100     MOVE WT-IDOKOLLI(IX)  TO WC-IDOKOLLI                                 
218200                              WCS-IDOKOLLI                                
218300     MOVE WT-MID-IX(IX)    TO MOD-IX                                      
218400     PERFORM IMS-GU-INLC-D111                                             
218500                                                                          
218600     PERFORM UNTIL SEGMENT-SAKNAS                                         
218700        MOVE ART-IDLOPNRM            TO SPAR-IDLOPNRM                     
218800                                        WS-IDLOPNRM                       
218900        MOVE ART-PRARTSTD            TO SPAR-PRARTSTD                     
219000        MOVE ART-BEFT                TO WS-BEFT                           
219100        MOVE ART-IDARTNR             TO WS-IDARTNR                        
219200        MOVE ART-IDRADNR-INL         TO W-IDRADNR-INL                     
219300        IF ART-KVAVIS-PRIO > ZERO                                         
219400          MOVE JA TO PRIOGODS-SW                                          
219500        END-IF                                                            
219600        PERFORM HABA-UPD-LEV-KOLLI-RADER                                  
219700        MOVE ZERO           TO W-IDRADNR                                  
219800        PERFORM IMS-GN-INLC-D111                                          
219900     END-PERFORM                                                          
220000     .                                                                    
220100     EJECT                                                                
220200*----------------------------------------------------------------*        
220300 HABA-UPD-LEV-KOLLI-RADER  SECTION.                                       
220400                                                                          
220500     PERFORM IMS-GHNP-INLC-D121                                           
220600                                                                          
220700     PERFORM UNTIL SEGMENT-SAKNAS                                         
220800        IF RAD-KDINLSTA = 'FPK' OR SPACE OR 'SAK'                         
220900                                                                          
221000           IF RAD-KDINLSTA = 'SAK'                                        
221100             MOVE SPACE                 TO RAD-KDINLSTA                   
221200           END-IF                                                         
221300           IF MID-FLSATS = JA OR YES                                      
221400             MOVE JA                  TO RAD-FLSATS                       
221500           END-IF                                                         
221600           MOVE MID-SPAR-IDINLVGN     TO RAD-IDINLVGN                     
221700           MOVE RAD-ADINLOMR          TO SPAR-ADINLOMR                    
221800                                         SPAR-ADINLOMR-NEW                
221900           MOVE MID-SPAR-ADINLOMR     TO RAD-ADINLOMR                     
222000           MOVE RAD-ADINLOMR-NXT      TO SPAR-ADINLOMR-NXT                
222100           MOVE MID-SPAR-ADINLOMR-NXT TO RAD-ADINLOMR-NXT                 
222200           MOVE RAD-KVINLART          TO SPAR-KVINLART-OLD                
222300                                                                          
222400           PERFORM IMS-REPL-INLC                                          
222500           IF (MOD-FLPRIO = JA OR YES) AND RAD-FLPRIO = NEJ               
222600             MOVE RAD-IDLEVNR-KOLLI TO PMRK-IDLEVNR                       
222700             MOVE RAD-IDOKOLLI      TO PMRK-IDOKOLLI                      
222800             MOVE ZERO              TO PMRK-IDLOPNRM                      
222900                                       PMRK-IDRADNR                       
223000             CALL W611PMRK USING PMRK-W611PMRK                            
223100                                 INLB-PMRK-PCB                            
223200                                 INLC-PMRK-PCB                            
223300           END-IF                                                         
223400                                                                          
223500           PERFORM S40-TRANS-W60191                                       
223600        END-IF                                                            
223700        PERFORM IMS-GHNP-INLC-D121                                        
223800     END-PERFORM                                                          
223900     .                                                                    
224000     EJECT                                                                
224100*----------------------------------------------------------------*        
224200 HAC-INIT-RADNR1    SECTION.                                              
224300                                                                          
224400     MOVE NEJ                  TO RADNR1-SW                               
224500     MOVE WT-IDLOPNRM(IX)      TO WB-IDLOPNRM                             
224600     MOVE IX                   TO RADNR1-IX                               
224700                                                                          
224800*----LÄS FÖRSTA RAD FÖR ATT RÄKNA NED KVINLART                            
224900     MOVE 1                    TO W-IDRADNR                               
225000     PERFORM IMS-GU-INLB-D121                                             
225100     IF SEGMENT-FINNS                                                     
225200*-------SPARA    VISSA FÄLT FRÅN RADNR 1 TILL W-FÄLT                      
225300        IF RAD-ADINLOMR = SPACE                                           
225400           MOVE MID-SPAR-ADINLOMR  TO SPAR-ADINLOMR-NEW                   
225500           MOVE SPACE              TO SPAR-ADINLOMR-OLD                   
225600                                      SPAR-ADINLOMR                       
225700        ELSE                                                              
225800           MOVE RAD-ADINLOMR  TO SPAR-ADINLOMR-OLD                        
225900                                 SPAR-ADINLOMR                            
226000                                 SPAR-ADINLOMR-NEW                        
226100        END-IF                                                            
226200        MOVE RAD-ADINLOMR-NXT    TO SPAR-ADINLOMR-NXT-OLD                 
226300                                    SPAR-ADINLOMR-NXT                     
226400        MOVE RAD-FLDIVKLI        TO W-RAD1-FLDIVKLI                       
226500        MOVE RAD-FLINLFP         TO W-RAD1-FLINLFP                        
226600        MOVE RAD-FLKVAANT        TO W-RAD1-FLKVAANT                       
226700        MOVE RAD-FLINLFB         TO W-RAD1-FLINLFB                        
226800        MOVE RAD-IDANSTNR        TO W-RAD1-IDANSTNR                       
226900        MOVE RAD-KDINLPRIO       TO W-RAD1-KDINLPRIO                      
227000        MOVE RAD-KDINLSTA        TO W-RAD1-KDINLSTA                       
227100        MOVE RAD-KVINLART        TO SPAR-KVINLART                         
227200                                    SPAR-KVINLART-OLD                     
227300        PERFORM S40-TRANS-W60191                                          
227400        MOVE IX1                 TO T91-IX                                
227500        MOVE JA                  TO RADNR1-SW                             
227600     END-IF                                                               
227700     .                                                                    
227800     EJECT                                                                
227900*----------------------------------------------------------------*        
228000 HAD-AVSLUTA-RADNR1-FOREGAENDE SECTION.                                   
228100                                                                          
228200     IF RADNR1-JA                                                         
228300        MOVE WT-IDLOPNRM(RADNR1-IX)  TO WB-IDLOPNRM                       
228400                                                                          
228500*------ LÄS FÖRSTA RAD FÖR ATT RÄKNA NED KVINLART                         
228600        MOVE 1                       TO W-IDRADNR                         
228700        PERFORM IMS-GHU-INLB-D121                                         
228800        IF SEGMENT-FINNS                                                  
228900                                                                          
229000           MOVE SPAR-KVINLART     TO RAD-KVINLART                         
229100           MOVE SPAR-KVINLART     TO T91-MID-KVINLART-NEW(T91-IX)         
229200           IF RAD-ADINLOMR = SPACE                                        
229300              MOVE SPAR-ADINLOMR-NEW TO RAD-ADINLOMR                      
229400              MOVE SPACE             TO SPAR-ADINLOMR-OLD                 
229500                                        SPAR-ADINLOMR                     
229600           END-IF                                                         
229700                                                                          
229800           IF RAD-KVINLART = ZERO                                         
229900              PERFORM IMS-DLET-INLB                                       
230000           ELSE                                                           
230100              PERFORM IMS-REPL-INLB                                       
230200           END-IF                                                         
230300           MOVE ZERO            TO SPAR-KVINLART                          
230400           MOVE ZERO            TO T91-IX                                 
230500        END-IF                                                            
230600     END-IF                                                               
230700     .                                                                    
230800     EJECT                                                                
230900******************************************************************        
231000*    MFS-REDIGERING AV BILDENS FÄLT                              *        
231100******************************************************************        
231200*----------------------------------------------------------------*        
231300 MFS-RENSA-FAELT-UT SECTION.                                              
231400                                                                          
231500*--- RENSA INDEXERADE RADER                                               
231600                                                                          
231700     MOVE +1 TO IX                                                        
231800     PERFORM UNTIL IX > MAX-IX                                            
231900        PERFORM MFS-RENSA-RAD-FAELT-UT                                    
232000        ADD +1 TO IX                                                      
232100     END-PERFORM                                                          
232200     MOVE ZERO               TO IX                                        
232300     ADD  1                  TO IX                                        
232400     .                                                                    
232600     EJECT                                                                
232700*----------------------------------------------------------------*        
232800 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
232900                                                                          
233000*--- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                       
233100                                                                          
233200     MOVE MFS-RENSA-FAELT    TO MOD-IDARTNR(IX)                           
233300                                MOD-KVINLART(IX)                          
233400                                MOD-IDLEVNR(IX)                           
233500                                MOD-IDOKOLLI(IX)                          
233600                                MOD-TEMFSMED(IX)                          
233700     .                                                                    
233800     EJECT                                                                
234000*----------------------------------------------------------------*        
234100 MFS-RENSA-FAELT-IN SECTION.                                              
234200                                                                          
234300*--- ALLA IN-FÄLT                                                         
234400     MOVE MFS-RENSA-FAELT TO MOD-IDINLVGN-IN                              
234500                             MOD-ADINLOMR-IN                              
234600                             MOD-ADINLOMR-NXT-IN                          
234700     .                                                                    
234800     EJECT                                                                
235000*----------------------------------------------------------------*        
235100 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
235200                                                                          
235300*--- INDEXERADE RADER                                                     
235400                                                                          
235500     MOVE +1 TO IX                                                        
235600     PERFORM UNTIL IX > MAX-IX                                            
235700        PERFORM MFS-ROER-EJ-RAD-FAELT-UT                                  
235800        ADD +1 TO IX                                                      
235900     END-PERFORM                                                          
236000     .                                                                    
236200     EJECT                                                                
236300*----------------------------------------------------------------*        
236400 MFS-ROER-EJ-RAD-FAELT-UT  SECTION.                                       
236500                                                                          
236600*--- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                       
236700                                                                          
236800     MOVE MFS-ROER-EJ-FAELT   TO MOD-IDARTNR(IX)                          
236900                                 MOD-KVINLART(IX)                         
237000                                 MOD-IDLEVNR(IX)                          
237100                                 MOD-IDOKOLLI(IX)                         
237200     .                                                                    
237300     EJECT                                                                
237500*----------------------------------------------------------------*        
237600 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
237700                                                                          
237800*--- ALLA IN-FÄLT                                                         
237900                                                                          
238000     MOVE MFS-ROER-EJ-FAELT  TO MOD-IDINLVGN-IN                           
238100                                MOD-ADINLOMR-IN                           
238200                                MOD-ADINLOMR-NXT-IN                       
238300     .                                                                    
238400     EJECT                                                                
238600*----------------------------------------------------------------*        
238700 MFS-FORM-ATTR SECTION.                                                   
238800                                                                          
238900*    ALLA INDATA-FÄLT                                                     
239000     MOVE MFS-FORMATETS-ATTR  TO MOD-FLPRIO-ATTR                          
239100                                 MOD-FLSATS-ATTR                          
239200*    --- OBS --- OCCURS 11 PÅ FÄLTEN                                      
239300                                                                          
239400     MOVE MFS-FORMATETS-ATTR    TO MOD-IDARTNR-ATTR(IX)                   
239500                                   MOD-KVINLART-ATTR(IX)                  
239600                                   MOD-IDLEVNR-ATTR(IX)                   
239700                                   MOD-IDOKOLLI-ATTR(IX)                  
239800     .                                                                    
239900     EJECT                                                                
240100*----------------------------------------------------------------*        
240200 MFS-LAES-IN-IGEN SECTION.                                                
240300                                                                          
240400*--- INDEXERADE RADER                                                     
240500                                                                          
240600     MOVE +1 TO IX                                                        
240700     PERFORM UNTIL IX > MAX-IX                                            
240800                                                                          
240900        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDARTNR-ATTR(IX)                
241000                                      MOD-KVINLART-ATTR(IX)               
241100                                      MOD-IDLEVNR-ATTR(IX)                
241200                                      MOD-IDOKOLLI-ATTR(IX)               
241300        ADD +1 TO IX                                                      
241400     END-PERFORM                                                          
241600     .                                                                    
241700     EJECT                                                                
278800******************************************************************        
278900*  INFO/FELMEDDELANDE                                            *        
279000******************************************************************        
279100*----------------------------------------------------------------*        
279200 S01-CALL-WMEDKONV-FEL SECTION.                                           
279300                                                                          
279400     CALL WMEDKONV USING MED-WMEDAREA                                     
279500     IF RADFEL-JA                                                         
279600        MOVE MED-MFSFEL TO MOD-TEMFSMED(MOD-IX)                           
279700     ELSE                                                                 
279800        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
279900     END-IF                                                               
280000     .                                                                    
280100     EJECT                                                                
280300*----------------------------------------------------------------*        
280400 S02-CALL-WMEDKONV-INF SECTION.                                           
280500                                                                          
280600     CALL WMEDKONV USING MED-WMEDAREA                                     
280700     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
280800     .                                                                    
280900     EJECT                                                                
281000*----------------------------------------------------------------*        
281100 S40-TRANS-W60191      SECTION.                                           
281200                                                                          
281300     IF T91-MID-KVPOST = ZERO                                             
281400        MOVE JA                  TO TRANS91-SW                            
281500        MOVE SPACE               TO T91-MID-W6I19101                      
281600        MOVE +1                  TO T91-MID-KVPOST                        
281700                                                                          
281800        MOVE IDPGM               TO T91-MID-IDPGM                         
281810        MOVE DCS-IDDC            TO T91-MID-IDDC                          
281900                                                                          
282000        MOVE SPAR-IDLOPNRM         TO T91-MID-IDLOPNRM(1)                 
282100        MOVE SPAR-PRARTSTD         TO T91-MID-PRARTSTD(1)                 
282200        MOVE RAD-IDRADNR           TO T91-MID-IDRADNR(1)                  
282300        MOVE RAD-KDINLPRIO         TO T91-MID-KDINLPRIO(1)                
282400        MOVE +0                    TO T91-MID-KVKOLLI  (1)                
282500        MOVE 'N'                   TO T91-MID-FLINLI   (1)                
282600        MOVE SPAR-ADINLOMR         TO T91-MID-ADINLOMR-OLD(1)             
282700        MOVE SPAR-ADINLOMR-NEW     TO T91-MID-ADINLOMR-NEW(1)             
282800        MOVE SPAR-ADINLOMR-NXT     TO T91-MID-ADINLOMR-NXT-OLD(1)         
282900        MOVE RAD-ADINLOMR-NXT      TO T91-MID-ADINLOMR-NXT-NEW(1)         
283000        MOVE SPAR-KVINLART-OLD     TO T91-MID-KVINLART-OLD(1)             
283100        MOVE RAD-KVINLART          TO T91-MID-KVINLART-NEW(1)             
283200        MOVE SPAR-KDINLSTA-OLD     TO T91-MID-KDINLSTA-OLD(1)             
283300        MOVE SPAR-KDINLSTA-NEW     TO T91-MID-KDINLSTA-NEW(1)             
283400        MOVE +1                    TO IX1                                 
283500     ELSE                                                                 
283600        COMPUTE IX1 = T91-MID-KVPOST + 1                                  
283700        ADD 1                         TO T91-MID-KVPOST                   
283800                                                                          
283900        MOVE SPAR-IDLOPNRM         TO T91-MID-IDLOPNRM(IX1)               
284000        MOVE SPAR-PRARTSTD         TO T91-MID-PRARTSTD(IX1)               
284100        MOVE RAD-IDRADNR           TO T91-MID-IDRADNR(IX1)                
284200        MOVE RAD-KDINLPRIO         TO T91-MID-KDINLPRIO(IX1)              
284300        MOVE +0                    TO T91-MID-KVKOLLI  (IX1)              
284400        MOVE 'N'                   TO T91-MID-FLINLI   (IX1)              
284500        MOVE SPAR-ADINLOMR         TO T91-MID-ADINLOMR-OLD(IX1)           
284600        MOVE SPAR-ADINLOMR-NEW     TO T91-MID-ADINLOMR-NEW(IX1)           
284700        MOVE SPAR-ADINLOMR-NXT                                            
284800             TO T91-MID-ADINLOMR-NXT-OLD(IX1)                             
284900        MOVE RAD-ADINLOMR-NXT                                             
285000             TO T91-MID-ADINLOMR-NXT-NEW(IX1)                             
285100        MOVE SPAR-KVINLART-OLD     TO T91-MID-KVINLART-OLD(IX1)           
285200        MOVE RAD-KVINLART          TO T91-MID-KVINLART-NEW(IX1)           
285300        MOVE SPAR-KDINLSTA-OLD     TO T91-MID-KDINLSTA-OLD(IX1)           
285400        MOVE SPAR-KDINLSTA-NEW     TO T91-MID-KDINLSTA-NEW(IX1)           
285410        IF T91-MID-KVPOST = 24                                            
285420          PERFORM S45-SKICKA-W60191                                       
285430          MOVE ZERO TO T91-MID-KVPOST                                     
285440          MOVE NEJ TO TRANS91-SW                                          
285450        END-IF                                                            
285500     END-IF                                                               
285600     .                                                                    
285700     EJECT                                                                
285800*----------------------------------------------------------------*        
285900 S45-SKICKA-W60191 SECTION.                                               
286000                                                                          
286100                                                                          
286200     COMPUTE W-PTOP1-OCC-LL = T91-MID-KVPOST * 64                         
286300     COMPUTE P-TO-P-MSG-KVLL = W-PTOP1-OCC-LL + 35                        
286400     MOVE MFS-KDMFSFOR       TO P-TO-P-MSG-KDMFSFOR                       
286500     MOVE 'W6T191X'          TO P-TO-P-MSG-KDTRANS                        
286600     MOVE '6124'             TO P-TO-P-MSG-IDTRANS                        
286700     MOVE T91-MID            TO P-TO-P-MSG-INDATA                         
286800                                                                          
286900     IF FOERSTA-T91-JA                                                    
287000        PERFORM IMS-ISRT-MSG-ALT1-6191                                    
287100        MOVE NEJ     TO FOERSTA-T91-SW                                    
287200     ELSE                                                                 
287300        PERFORM IMS-PURG-MSG-ALT1-6191                                    
287400     END-IF                                                               
287500     .                                                                    
287600     EJECT                                                                
287601                                                                          
287610 S50-PRIM-CONTROL SECTION.                                                
287620* THIS IS A CONTROL TO CHECK THE OLD PLACE IF FLAG FLKNTRGK = YES         
287630* IF THE FLAG IS YES WE HAVE TO DO A CHECK ON THE OLD PLACE BEFORE        
287640* WE CHECK THE NEW PLACE.                                                 
287650     MOVE RAD-ADINLOMR         TO W-ADINLOMR                              
287660     PERFORM IMS-GU-PLAA-G111                                             
287670     IF SEGMENT-FINNS                                                     
287680       IF 6006-FLKNTRGK = JA                                              
287690         MOVE MID-ADINLOMR-UT   TO W-ADINLOMR                             
287691         PERFORM IMS-GU-PLAA-G111                                         
287692         IF (6006-FLKNTRGK = JA )                                         
287693         OR 6006-KDINLOMR = 'LPL'                                         
287694           MOVE 'N' TO PRIM-CONTROL-SW                                    
287695         ELSE                                                             
287696           MOVE 'J' TO PRIM-CONTROL-SW                                    
287697         END-IF                                                           
287698       ELSE                                                               
287699         MOVE 'N' TO PRIM-CONTROL-SW                                      
287700       END-IF                                                             
287701     ELSE                                                                 
287702       MOVE 'N' TO PRIM-CONTROL-SW                                        
287703     END-IF                                                               
287704                                                                          
287705     IF PRIM-CONTROL-YES                                                  
287706       PERFORM S51-CHECK-OLD-PLACE                                        
287707     END-IF                                                               
287722     .                                                                    
287723     EJECT                                                                
287724                                                                          
287725 S51-CHECK-OLD-PLACE     SECTION.                                         
287726     MOVE ART-IDLOPNRM  TO W-IDLOPNRM                                     
287727     PERFORM IMS-GU-UPFA01                                                
287728     IF SEGMENT-FINNS                                                     
287729       IF UPPF-KVKVAPRIM > ZERO                                           
287730         IF UPPF-KDKVASTA-PRI = '2' OR '3'                                
287731           CONTINUE                                                       
287732         ELSE                                                             
287733           MOVE '605' TO MED-IDMFSFEL                                     
287734           MOVE NEJ TO INDATA-SW                                          
287735           MOVE MFS-ALFA-FAELT-FEL                                        
287736                           TO MOD-IDARTNR-ATTR(MOD-IX)                    
287737                              MOD-IDLEVNR-ATTR(MOD-IX)                    
287738                              MOD-IDOKOLLI-ATTR(MOD-IX)                   
287739                              MOD-KVINLART-ATTR(MOD-IX)                   
287741         END-IF                                                           
287742       END-IF                                                             
287743       IF UPPF-KVKVASEK > ZERO                                            
287744         IF UPPF-KDKVASTA-PRI = '2' OR '3'                                
287745           CONTINUE                                                       
287746         ELSE                                                             
287747           MOVE '605' TO MED-IDMFSFEL                                     
287748           MOVE NEJ TO INDATA-SW                                          
287749           MOVE MFS-ALFA-FAELT-FEL                                        
287750                           TO MOD-IDARTNR-ATTR(MOD-IX)                    
287751                              MOD-IDLEVNR-ATTR(MOD-IX)                    
287752                              MOD-IDOKOLLI-ATTR(MOD-IX)                   
287753                              MOD-KVINLART-ATTR(MOD-IX)                   
287755         END-IF                                                           
287756       END-IF                                                             
287757     END-IF                                                               
287758                                                                          
287759     IF INDATA-OK                                                         
287760       PERFORM IMS-GU-UPFA01                                              
287761       IF SEGMENT-FINNS                                                   
287762         PERFORM IMS-GNP-UPFA11                                           
287763         PERFORM UNTIL SEGMENT-SAKNAS                                     
287764           IF RAPP-KDKVASTA-PRI = '2' OR '3'                              
287765             CONTINUE                                                     
287766           ELSE                                                           
287767             MOVE '605' TO MED-IDMFSFEL                                   
287768             MOVE NEJ TO INDATA-SW                                        
287769             MOVE MFS-ALFA-FAELT-FEL                                      
287770                           TO MOD-IDARTNR-ATTR(MOD-IX)                    
287771                              MOD-IDLEVNR-ATTR(MOD-IX)                    
287772                              MOD-IDOKOLLI-ATTR(MOD-IX)                   
287773                              MOD-KVINLART-ATTR(MOD-IX)                   
287775           END-IF                                                         
287776           PERFORM IMS-GNP-UPFA11                                         
287777         END-PERFORM                                                      
287778       END-IF                                                             
287779     END-IF                                                               
287780                                                                          
287781     IF INDATA-OK                                                         
287782       PERFORM IMS-GU-UPFA01                                              
287783       IF SEGMENT-FINNS                                                   
287784         PERFORM IMS-GNP-UPFA12                                           
287785         PERFORM UNTIL SEGMENT-SAKNAS                                     
287786           IF SPEC-KDKVASTA-PRI = '2' OR '3'                              
287787             CONTINUE                                                     
287788           ELSE                                                           
287789             MOVE '605' TO MED-IDMFSFEL                                   
287790             MOVE NEJ TO INDATA-SW                                        
287791             MOVE MFS-ALFA-FAELT-FEL                                      
287792                           TO MOD-IDARTNR-ATTR(MOD-IX)                    
287793                              MOD-IDLEVNR-ATTR(MOD-IX)                    
287794                              MOD-IDOKOLLI-ATTR(MOD-IX)                   
287795                              MOD-KVINLART-ATTR(MOD-IX)                   
287797           END-IF                                                         
287798           PERFORM IMS-GNP-UPFA12                                         
287799         END-PERFORM                                                      
287800       END-IF                                                             
287801     END-IF                                                               
287802                                                                          
287803     IF MED-IDMFSFEL = '605'                                              
287804        CALL WMEDKONV USING MED-WMEDAREA                                  
287805        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
287811        MOVE JA            TO RADFEL-SW                                   
287812        MOVE JA            TO RADERFEL-SW                                 
287813     END-IF                                                               
287814     .                                                                    
287815     EJECT                                                                
287816                                                                          
287817******************************************************************        
287820*  BILD-REDIGERING                                               *        
287900******************************************************************        
288000*----------------------------------------------------------------*        
288100 S90-BLANKUTF-NUM-FAELT SECTION.                                          
288200                                                                          
288300*----NKL-FÄLT                                                             
288400     INSPECT MOD-IDINLVGN-UT REPLACING LEADING ZERO BY SPACE              
288500                                                                          
288600     IF NYCKLAR-OK AND INDATA-OK AND                                      
288700        (GODK-MID OR HELP-MID)                                            
288800        MOVE MFS-OEPPNA-ALFA-FAELT   TO MOD-FLPRIO-ATTR                   
288900                                        MOD-FLSATS-ATTR                   
289000     ELSE                                                                 
289100        IF INDATA-FEL                                                     
289200           CONTINUE                                                       
289300        ELSE                                                              
289400           MOVE MFS-STAENG-FAELT     TO MOD-FLPRIO-ATTR                   
289500                                        MOD-FLSATS-ATTR                   
289600        END-IF                                                            
289700     END-IF                                                               
289800                                                                          
289900*----RAD-FÄLT                                                             
290000*--- INDEXERADE RADER                                                     
290100                                                                          
290200     MOVE +1 TO IX                                                        
290300     PERFORM UNTIL IX > MAX-IX                                            
290400        IF NYCKLAR-OK AND INDATA-OK AND                                   
290500           (GODK-MID OR HELP-MID)                                         
290600           MOVE MFS-OEPPNA-NUM-FAELT  TO MOD-IDARTNR-ATTR(IX)             
290700                                         MOD-KVINLART-ATTR(IX)            
290900                                         MOD-IDOKOLLI-ATTR(IX)            
290910           MOVE MFS-OEPPNA-ALFA-FAELT TO MOD-IDLEVNR-ATTR(IX)             
291000        ELSE                                                              
291100           IF INDATA-FEL                                                  
291200              CONTINUE                                                    
291300           ELSE                                                           
291400              MOVE MFS-STAENG-FAELT  TO MOD-IDARTNR-ATTR(IX)              
291500                                        MOD-KVINLART-ATTR(IX)             
291600                                        MOD-IDLEVNR-ATTR(IX)              
291700                                        MOD-IDOKOLLI-ATTR(IX)             
291800           END-IF                                                         
291900        END-IF                                                            
292000        ADD +1 TO IX                                                      
292100     END-PERFORM                                                          
292200     .                                                                    
292300     EJECT                                                                
292400******************************************************************        
292500*  IMS-SECTIONER                                                 *        
292600******************************************************************        
292700     SKIP3                                                                
292800*----------------------------------------------------------------*        
292900 IMS-GET-MSG SECTION.                                                     
293000                                                                          
293100     MOVE '  QC' TO GODK-STATUSKODER                                      
293200     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
293300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
293400     PERFORM IMS-STATUSKONTROLL                                           
293500     .                                                                    
293600     SKIP3                                                                
293700*----------------------------------------------------------------*        
293800 IMS-INSERT-MSG SECTION.                                                  
293900                                                                          
294000     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
294100       MOVE '0' TO MFS-KDHUVOMR                                           
294200     END-IF                                                               
294300     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
294400     MOVE SPACE TO GODK-STATUSKODER                                       
294500     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
294600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
294700     PERFORM IMS-STATUSKONTROLL                                           
294800     .                                                                    
294900     EJECT                                                                
295000******************************************************************        
295100*    ALT1-PCB  (TRANS W60191)                                    *        
295200******************************************************************        
295300*----------------------------------------------------------------*        
295400 IMS-ISRT-MSG-ALT1-6191 SECTION.                                          
295500                                                                          
295600     MOVE SPACE              TO GODK-STATUSKODER                          
295700     CALL CBLTDLI USING      ISRT ALT1-PCB                                
295800                                  P-TO-P-MSG-IO-AREA-SNUF                 
295900     MOVE ALT1-STATUS-CODE   TO STATUS-WS                                 
296000     PERFORM IMS-STATUSKONTROLL                                           
296100     .                                                                    
296200     EJECT                                                                
296300                                                                          
296400******************************************************************        
296500*    ALT1-PCB  (TRANS W60191)                                    *        
296600******************************************************************        
296700*----------------------------------------------------------------*        
296800 IMS-PURG-MSG-ALT1-6191 SECTION.                                          
296900                                                                          
297000     MOVE SPACE              TO GODK-STATUSKODER                          
297100     CALL CBLTDLI USING      PURG ALT1-PCB                                
297200                                  P-TO-P-MSG-IO-AREA-SNUF                 
297300     MOVE ALT1-STATUS-CODE   TO STATUS-WS                                 
297400     PERFORM IMS-STATUSKONTROLL                                           
297500     .                                                                    
297600     EJECT                                                                
297700******************************************************************        
297800*    INLA-PCB                                                    *        
297900******************************************************************        
298000     SKIP3                                                                
298100*----------------------------------------------------------------*        
298200 IMS-GU-INLA-D111 SECTION.                                                
298300     STRING 'W6INLA01(W6D101KY =' W-W6D101KY-X ')'                        
298400             DELIMITED BY SIZE INTO SSA1                                  
298500     STRING 'W6INLA11(IDRADNRI =' W-IDRADNR-INL-X ')'                     
298600          DELIMITED BY SIZE INTO SSA2                                     
298700     MOVE '  GE'            TO GODK-STATUSKODER                           
298800     CALL CBLTDLI USING GU  INLA-PCB                                      
298900                            DLI-IO-AREA-W6D111                            
299000                            SSA1                                          
299100                            SSA2                                          
299200     MOVE INLA-STATUS-CODE  TO STATUS-WS                                  
299300     PERFORM IMS-STATUSKONTROLL                                           
299400     .                                                                    
299500     EJECT                                                                
299600*----------------------------------------------------------------*        
299700 IMS-GU-INLA-D121 SECTION.                                                
299800     STRING 'W6INLA01(W6D101KY =' W-W6D101KY-X ')'                        
299900             DELIMITED BY SIZE INTO SSA1                                  
300000     STRING 'W6INLA11(IDRADNRI =' W-IDRADNR-INL-X ')'                     
300100          DELIMITED BY SIZE INTO SSA2                                     
300200     STRING 'W6INLA21(IDRADNR  =' W-IDRADNR-X ')'                         
300300          DELIMITED BY SIZE INTO SSA3                                     
300400     MOVE '  GE'            TO GODK-STATUSKODER                           
300500     CALL CBLTDLI USING GU  INLA-PCB                                      
300600                            DLI-IO-AREA-W6D121                            
300700                            SSA1                                          
300800                            SSA2                                          
300900                            SSA3                                          
301000     MOVE INLA-STATUS-CODE  TO STATUS-WS                                  
301100     PERFORM IMS-STATUSKONTROLL                                           
301200     .                                                                    
301300     EJECT                                                                
301400******************************************************************        
301500*    INLB-PCB                                                    *        
301600******************************************************************        
301700     SKIP3                                                                
301800*----------------------------------------------------------------*        
301900 IMS-GU-INLB-D111  SECTION.                                               
302000     STRING 'W6INLA11(W6D1BSEQ =' W-W6D1B1KY-X ')'                        
302100             DELIMITED BY SIZE INTO SSA1                                  
302200     MOVE '  GE'                 TO GODK-STATUSKODER                      
302300     CALL CBLTDLI USING GU       INLB-PCB                                 
302400                                 DLI-IO-AREA-W6D111                       
302500                                 SSA1                                     
302600     MOVE INLB-STATUS-CODE       TO STATUS-WS                             
302700     PERFORM IMS-STATUSKONTROLL                                           
302800     .                                                                    
302900     EJECT                                                                
303000*----------------------------------------------------------------*        
303100 IMS-GU-INLB-D121        SECTION.                                         
303200     STRING 'W6INLA11(W6D1BSEQ =' W-W6D1B1KY-X ')'                        
303300          DELIMITED BY SIZE INTO SSA1                                     
303400     STRING 'W6INLA21(IDRADNR  =' W-IDRADNR-X ')'                         
303500          DELIMITED BY SIZE INTO SSA2                                     
303600     MOVE '  GE'            TO GODK-STATUSKODER                           
303700     CALL CBLTDLI USING GU  INLB-PCB                                      
303800                            DLI-IO-AREA-W6D121                            
303900                            SSA1                                          
304000                            SSA2                                          
304100     MOVE INLB-STATUS-CODE  TO STATUS-WS                                  
304200     PERFORM IMS-STATUSKONTROLL                                           
304300     .                                                                    
304400     EJECT                                                                
304500*----------------------------------------------------------------*        
304600 IMS-GNP-INLB-D121    SECTION.                                            
304700     STRING 'W6INLA11(W6D1BSEQ =' W-W6D1B1KY-X ')'                        
304800          DELIMITED BY SIZE INTO SSA1                                     
304900     STRING 'W6INLA21(IDRADNR  >' W-IDRADNR-X ')'                         
305000          DELIMITED BY SIZE INTO SSA2                                     
305100     MOVE '  GE'            TO GODK-STATUSKODER                           
305200     CALL CBLTDLI USING GNP INLB-PCB                                      
305300                            DLI-IO-AREA-W6D121                            
305400                            SSA1                                          
305500                            SSA2                                          
305600     MOVE INLB-STATUS-CODE  TO STATUS-WS                                  
305700     PERFORM IMS-STATUSKONTROLL                                           
305800     .                                                                    
305900     EJECT                                                                
306000******************************************************************        
306100*    IMS-UPPDATERING VIA INLB-PCB                                *        
306200******************************************************************        
306300*----------------------------------------------------------------*        
306400 IMS-GHU-INLB-D121        SECTION.                                        
306500     STRING 'W6INLA11(W6D1BSEQ =' W-W6D1B1KY-X ')'                        
306600          DELIMITED BY SIZE INTO SSA1                                     
306700     STRING 'W6INLA21(IDRADNR  =' W-IDRADNR-X ')'                         
306800          DELIMITED BY SIZE INTO SSA2                                     
306900     MOVE '  GE'            TO GODK-STATUSKODER                           
307000     CALL CBLTDLI USING GHU INLB-PCB                                      
307100                            DLI-IO-AREA-W6D121                            
307200                            SSA1                                          
307300                            SSA2                                          
307400     MOVE INLB-STATUS-CODE  TO STATUS-WS                                  
307500     PERFORM IMS-STATUSKONTROLL                                           
307600     .                                                                    
307700     EJECT                                                                
307800*----------------------------------------------------------------*        
307900 IMS-GHNP-INLB-D121-LAST    SECTION.                                      
308000     MOVE 'W6INLA21*L'        TO SSA1                                     
308100     MOVE '  '                TO GODK-STATUSKODER                         
308200     CALL CBLTDLI USING GHNP  INLB-PCB                                    
308300                              DLI-IO-AREA-W6D121                          
308400                              SSA1                                        
308500     MOVE INLB-STATUS-CODE  TO STATUS-WS                                  
308600     PERFORM IMS-STATUSKONTROLL                                           
308700     .                                                                    
308800     EJECT                                                                
308900*----------------------------------------------------------------*        
309000 IMS-REPL-INLB SECTION.                                                   
309100                                                                          
309200     MOVE '  '               TO GODK-STATUSKODER                          
309300     CALL CBLTDLI USING REPL INLB-PCB                                     
309400                             DLI-IO-AREA-W6D121                           
309500     MOVE INLB-STATUS-CODE   TO STATUS-WS                                 
309600     PERFORM IMS-STATUSKONTROLL                                           
309700     .                                                                    
309800     EJECT                                                                
309900*----------------------------------------------------------------*        
310000 IMS-ISRT-INLB SECTION.                                                   
310100                                                                          
310200     MOVE 'W6INLA21 '        TO SSA1                                      
310300     MOVE '  '               TO GODK-STATUSKODER                          
310400     CALL CBLTDLI USING ISRT INLB-PCB                                     
310500                             DLI-IO-AREA-W6D121                           
310600                             SSA1                                         
310700     MOVE INLB-STATUS-CODE   TO STATUS-WS                                 
310800     PERFORM IMS-STATUSKONTROLL                                           
310900     .                                                                    
311000     EJECT                                                                
311100*----------------------------------------------------------------*        
311200 IMS-DLET-INLB SECTION.                                                   
311300                                                                          
311400     MOVE '  '               TO GODK-STATUSKODER                          
311500     CALL CBLTDLI USING DLET INLB-PCB                                     
311600                             DLI-IO-AREA-W6D121                           
311700     MOVE INLB-STATUS-CODE   TO STATUS-WS                                 
311800     PERFORM IMS-STATUSKONTROLL                                           
311900     .                                                                    
312000     EJECT                                                                
312100******************************************************************        
312200*    INLC-PCB                                                    *        
312300******************************************************************        
312400     SKIP3                                                                
312500*----------------------------------------------------------------*        
312600 IMS-GU-INLC-D111  SECTION.                                               
312700     STRING 'W6INLA11(W6D1CSEQ =' W-W6D1C1KY-X ')'                        
312800             DELIMITED BY SIZE INTO SSA1                                  
312900     MOVE '  GE'                 TO GODK-STATUSKODER                      
313000     CALL CBLTDLI USING GU       INLC-PCB                                 
313100                                 DLI-IO-AREA-W6D111                       
313200                                 SSA1                                     
313300     MOVE INLC-STATUS-CODE       TO STATUS-WS                             
313400     PERFORM IMS-STATUSKONTROLL                                           
313500     .                                                                    
313600     EJECT                                                                
313700*----------------------------------------------------------------*        
313800 IMS-GN-INLC-D111  SECTION.                                               
313900     STRING 'W6INLA11(W6D1CSEQ =' W-W6D1C1KY-X ')'                        
314000             DELIMITED BY SIZE INTO SSA1                                  
314100     MOVE '  GE'                 TO GODK-STATUSKODER                      
314200     CALL CBLTDLI USING GN       INLC-PCB                                 
314300                                 DLI-IO-AREA-W6D111                       
314400                                 SSA1                                     
314500     MOVE INLC-STATUS-CODE       TO STATUS-WS                             
314600     PERFORM IMS-STATUSKONTROLL                                           
314700     .                                                                    
314800     EJECT                                                                
314900*----------------------------------------------------------------*        
315000 IMS-GNP-INLC-D121    SECTION.                                            
315100     STRING 'W6INLA21(IDLEVNRK =' WCS-IDLEVNR-X                           
315200                    '&IDOKOLLI =' WCS-IDOKOLLI-X ')'                      
315300          DELIMITED BY SIZE INTO SSA1                                     
315400     MOVE '  GE'            TO GODK-STATUSKODER                           
315500     CALL CBLTDLI USING GNP INLC-PCB                                      
315600                            DLI-IO-AREA-W6D121                            
315700                            SSA1                                          
315800     MOVE INLC-STATUS-CODE  TO STATUS-WS                                  
315900     PERFORM IMS-STATUSKONTROLL                                           
316000     .                                                                    
316100     EJECT                                                                
316200******************************************************************        
316300*    IMS-UPPDATERING VIA INLC-PCB                                *        
316400******************************************************************        
316500*----------------------------------------------------------------*        
316600 IMS-GHNP-INLC-D121        SECTION.                                       
316700     STRING 'W6INLA21(IDLEVNRK =' WCS-IDLEVNR-X                           
316800                    '&IDOKOLLI =' WCS-IDOKOLLI-X ')'                      
316900          DELIMITED BY SIZE INTO SSA1                                     
317000     MOVE '  GE'            TO GODK-STATUSKODER                           
317100     CALL CBLTDLI USING GHNP INLC-PCB                                     
317200                            DLI-IO-AREA-W6D121                            
317300                            SSA1                                          
317400     MOVE INLC-STATUS-CODE  TO STATUS-WS                                  
317500     PERFORM IMS-STATUSKONTROLL                                           
317600     .                                                                    
317700     EJECT                                                                
317800*----------------------------------------------------------------*        
317900 IMS-REPL-INLC SECTION.                                                   
318000                                                                          
318100     MOVE '  '               TO GODK-STATUSKODER                          
318200     CALL CBLTDLI USING REPL INLC-PCB                                     
318300                             DLI-IO-AREA-W6D121                           
318400     MOVE INLC-STATUS-CODE   TO STATUS-WS                                 
318500     PERFORM IMS-STATUSKONTROLL                                           
318600     .                                                                    
318700     EJECT                                                                
318800******************************************************************        
318900*    INLF1-PCB                                                   *        
319000******************************************************************        
319100     SKIP3                                                                
319200*----------------------------------------------------------------*        
319300 IMS-GU-INLF1-D111  SECTION.                                              
319400     STRING 'W6INLG01(W6D1F1KY>=' W-W6D1F1KY-MIN-X                        
319500                    '&W6D1F1KY<=' W-W6D1F1KY-MAX-X                        
319600                    '&IDDC     =' W-IDDC ')'                              
319700             DELIMITED BY SIZE INTO SSA1                                  
319800     MOVE '  GE'                 TO GODK-STATUSKODER                      
319900     CALL CBLTDLI USING GU       INLF1-PCB                                
320000                                 DLI-IO-AREA-W6D1F1                       
320100                                 SSA1                                     
320200     MOVE INLF1-STATUS-CODE      TO STATUS-WS                             
320300     PERFORM IMS-STATUSKONTROLL                                           
320400     .                                                                    
320500     EJECT                                                                
320600******************************************************************        
320700*    INLB1-PCB                                                   *        
320800******************************************************************        
320900     SKIP3                                                                
321000******************************************************************        
321100*    INLH1-PCB                                                   *        
321200******************************************************************        
321300     SKIP3                                                                
321400*----------------------------------------------------------------*        
321500 IMS-GU-INLH1-D111  SECTION.                                              
321600     STRING 'W6INLI01(W6D1H1KY>=' W-W6D1H1KY-MIN-X                        
321700                    '&W6D1H1KY<=' W-W6D1H1KY-MAX-X ')'                    
321800             DELIMITED BY SIZE INTO SSA1                                  
321900     MOVE '  GE'                 TO GODK-STATUSKODER                      
322000     CALL CBLTDLI USING GU       INLH1-PCB                                
322100                                 DLI-IO-AREA-W6D1H1                       
322200                                 SSA1                                     
322300     MOVE INLH1-STATUS-CODE      TO STATUS-WS                             
322400     PERFORM IMS-STATUSKONTROLL                                           
322500     .                                                                    
322600     EJECT                                                                
322700*----------------------------------------------------------------*        
322800 IMS-GN-INLH1-D111  SECTION.                                              
322900     STRING 'W6INLI01(W6D1H1KY>=' W-W6D1H1KY-MIN-X                        
323000                    '&W6D1H1KY<=' W-W6D1H1KY-MAX-X ')'                    
323100             DELIMITED BY SIZE INTO SSA1                                  
323200     MOVE '  GE'                 TO GODK-STATUSKODER                      
323300     CALL CBLTDLI USING GN       INLH1-PCB                                
323400                                 DLI-IO-AREA-W6D1H1                       
323500                                 SSA1                                     
323600     MOVE INLH1-STATUS-CODE      TO STATUS-WS                             
323700     PERFORM IMS-STATUSKONTROLL                                           
323800     .                                                                    
323900     EJECT                                                                
324000******************************************************************        
324100*    PLAA-PCB                                                    *        
324200******************************************************************        
324300     SKIP3                                                                
324400*----------------------------------------------------------------*        
324500 IMS-GU-PLAA-G111 SECTION.                                                
324600     STRING 'W6PLAA01(W6GXKEY  =' W-W6GX01KEY-X ')'                       
324700          DELIMITED BY SIZE INTO SSA1                                     
324800     STRING 'W6PLAA11(W6GXKEY  =' W-W6GX11KEY-X ')'                       
324900          DELIMITED BY SIZE INTO SSA2                                     
325000     MOVE '  GE'            TO GODK-STATUSKODER                           
325100     CALL CBLTDLI USING GU  PLAA-PCB                                      
325200                            DLI-IO-AREA-W6GX6006                          
325300                            SSA1                                          
325400                            SSA2                                          
325500     MOVE PLAA-STATUS-CODE  TO STATUS-WS                                  
325600     PERFORM IMS-STATUSKONTROLL                                           
325700     .                                                                    
325800     EJECT                                                                
325900 IMS-GU-UPFA01 SECTION.                                                   
326000     STRING 'W6UPFA01(IDLOPNRM =' W-IDLOPNRM-X ')'                        
326100          DELIMITED BY SIZE INTO SSA1                                     
326200     MOVE '  GE' TO GODK-STATUSKODER                                      
326300     CALL CBLTDLI USING GU UPFA-PCB DLI-IO-AREA-UPFA01 SSA1               
326400     MOVE UPFA-STATUS-CODE TO STATUS-WS                                   
326500     PERFORM IMS-STATUSKONTROLL                                           
326600     .                                                                    
326700     SKIP3                                                                
326800                                                                          
326900 IMS-GNP-UPFA11 SECTION.                                                  
327000     STRING 'W6UPFA01(IDLOPNRM =' W-IDLOPNRM-X ')'                        
327100          DELIMITED BY SIZE INTO SSA1                                     
327200     MOVE 'W6UPFA11 ' TO SSA2                                             
327300     MOVE '  GE' TO GODK-STATUSKODER                                      
327400     CALL CBLTDLI USING GNP UPFA-PCB DLI-IO-AREA-UPFA11 SSA1 SSA2         
327500     MOVE UPFA-STATUS-CODE TO STATUS-WS                                   
327600     PERFORM IMS-STATUSKONTROLL                                           
327700     .                                                                    
327800     SKIP3                                                                
327900                                                                          
328000 IMS-GNP-UPFA12 SECTION.                                                  
328100     STRING 'W6UPFA01(IDLOPNRM =' W-IDLOPNRM-X ')'                        
328200          DELIMITED BY SIZE INTO SSA1                                     
328300     MOVE 'W6UPFA12 ' TO SSA2                                             
328400     MOVE '  GE' TO GODK-STATUSKODER                                      
328500     CALL CBLTDLI USING GNP UPFA-PCB DLI-IO-AREA-UPFA12 SSA1 SSA2         
328600     MOVE UPFA-STATUS-CODE TO STATUS-WS                                   
328700     PERFORM IMS-STATUSKONTROLL                                           
328800     .                                                                    
328900     SKIP3                                                                
328910 IMS-GU-WDB601    SECTION.                                                
328920     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
328930          DELIMITED BY SIZE INTO SSA1                                     
328940     MOVE '  GE' TO GODK-STATUSKODER                                      
328950     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
328960     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
328970     PERFORM IMS-STATUSKONTROLL                                           
328980     IF SEGMENT-SAKNAS                                                    
328990         MOVE SPACE TO DCS-KDDC                                           
328991     END-IF                                                               
328992     .                                                                    
329000*----------------------------------------------------------------*        
329100 IMS-STATUSKONTROLL SECTION.                                              
329200                                                                          
329300     SET STATUS-IX TO 1                                                   
329400     SEARCH GODK-STATUS                                                   
329500       AT END                                                             
329600         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
329700         DELIMITED BY SIZE INTO FELTEXT                                   
329800         CALL FELLOG                                                      
329900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
330000         CONTINUE                                                         
330100     END-SEARCH                                                           
330200     .                                                                    
330300     EJECT                                                                
