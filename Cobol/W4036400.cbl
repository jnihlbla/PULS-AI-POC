000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4036400.                                                
000400 AUTHOR.         GERRY CARMICHAEL.                                        
000500 DATE-WRITTEN.   90/04/09.                                                
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION.                                                            
001000*        UPPDATERING, NYUPPLÄGGNING OCH FRÅGEPROGRAM.                     
001100*                                                                         
001200*        UNDERHÅLL PRC-TABELL.                                            
001300*        PROGRAMMET VISAR OCH GÖR FÖRÄNDRINGAR MOT                        
001400*        PRC-REGISTRET.                                                   
001500*        OM NY PRC-KANAL STARTAR PROGRAMMET BMP W41321.                   
001600*                                                                         
001700*        PROGRAMMET ÄR EN UPPDATERINGS-MPP                                
001800*        PROGRAMMET UPPDATERAR WLXXKH (WDR1)                              
001900*        PROGRAMMET UPPDATERAR WLXXKL (WDR1)                              
002000*        PROGRAMMET UPPDATERAR WLXXKO (WDR4)                              
002100*        PROGRAMMET LÄSER      WLXXKG (WDR1)                              
002200*                                                                         
002400*    INDATA.                                                              
002500*        TRANSAKTION: W4T364                                              
002600*        MID:         W4I36401                                            
002700*                                                                         
002800*    UTDATA.                                                              
002900*        TRANSAKTION: W0T606U                                             
003000*        MOD:         W4O36401                                            
003100                                                                          
003200     SKIP3                                                                
003300 ENVIRONMENT DIVISION.                                                    
003400     EJECT                                                                
003500 DATA DIVISION.                                                           
003600 WORKING-STORAGE SECTION.                                                 
003700                                                                          
003800*    -- CHECKED BY WY2000                                                 
003900 77  IDPGM                       PIC X(08)   VALUE 'W4036400'.            
004000                                                                          
004100 77  JA                          PIC X      VALUE 'J'.                    
004110 77  YES                         PIC X      VALUE 'Y'.                    
004200 77  NEJ                         PIC X      VALUE 'N'.                    
004300 77  IX1                         PIC S9(9)  VALUE +0    COMP SYNC.        
004310 01  FILLER                      PIC X(08)   VALUE 'FELTEXT:'.            
004400 77  FELTEXT                     PIC X(16).                               
004410 01  FILLER                      PIC X(08)   VALUE 'PGMPOS:'.             
004500 77  PGMPOS                      PIC X(16)   VALUE SPACE.                 
004510 01  FILLER                      PIC X(08)   VALUE 'IMSSEC:'.             
004600 77  IMSSEC                      PIC X(32)   VALUE SPACE.                 
004700                                                                          
004800 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
004900 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +512  COMP SYNC.        
005000                                                                          
005100 77  LO-INDX                     PIC S9(9)  VALUE +0    COMP SYNC.        
005200 77  MAX-LO-INDX                 PIC S9(9)  VALUE +10   COMP SYNC.        
005300                                                                          
005400 77  SUB-INDX                    PIC S9(9)  VALUE +0    COMP SYNC.        
005500 77  MAX-SUB-INDX                PIC S9(9)  VALUE +10   COMP SYNC.        
005600                                                                          
005700 77  RAD-INDX                    PIC S9(9)  VALUE +0    COMP SYNC.        
005800 77  MAX-RAD-INDX                PIC S9(9)  VALUE +10   COMP SYNC.        
005900                                                                          
006000 77  TAB-INDX                    PIC S9(9)  VALUE +0    COMP SYNC.        
006100 77  MAX-TAB-INDX                PIC S9(9)  VALUE +10   COMP SYNC.        
006200 77  MAX-TAB-INDX-PLUS1          PIC S9(9)  VALUE +11   COMP SYNC.        
006300                                                                          
006400*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
006500 01  WS-IDPRC.                                                            
006600   03    WS-IDPRCBAS             PIC X(3)    VALUE SPACE.                 
006700   03    WS-IDPRCVAR             PIC X(1)    VALUE SPACE.                 
006800 77  WS-KVVTID                   PIC S9(3)V9(2)   COMP-3.                 
006900 77  WS-VLKOLGR                  PIC S9V9(2)      COMP-3.                 
007000 77  WS-RESPLIT                  PIC S9V9(2)      COMP-3.                 
007100 77  WS-VKPLSNTO                 PIC S9(6)V9(1)   COMP-3.                 
007200 77  WS-VKORDNTO                 PIC S9(6)V9(1)   COMP-3.                 
007300 77  WS-VLPLSNTO                 PIC S9(4)V9(3)   COMP-3.                 
007400 77  WS-VLORDNTO                 PIC S9(4)V9(3)   COMP-3.                 
007500 77  WS-KDPRCGRP                 PIC X(5)    VALUE SPACE.                 
007600                                                                          
007700 01  WS-DCUSER.                                                           
007800     03 FILLER                   PIC X(5)   VALUE 'WIDDC'.                
007900     03 WS-DCUSER-IDDC           PIC X(2)   VALUE SPACE.                  
008000     03 FILLER                   PIC X(1)   VALUE SPACE.                  
008100                                                                          
008200 77  WS-IDDC                     PIC X(2)    VALUE SPACE.                 
008300*                                                                         
008400*          DATE FROM DC-LOCAL                                             
008500*                                                                         
008600 01  FILLER.                                                              
008700     05  WS-LOCAL-DATE         PIC  9(06) VALUE 0.                        
008800 77  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
008900                                                                          
009000 77  INDATA-SW                   PIC X       VALUE 'J'.                   
009100     88  INDATA-OK                           VALUE 'J'.                   
009200     88  INDATA-FEL                          VALUE 'N'.                   
009300                                                                          
009400 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
009500     88  NYCKLAR-OK                          VALUE 'J'.                   
009600     88  NYCKLAR-FEL                         VALUE 'N'.                   
009700                                                                          
009800 77  UPDATE-SW                   PIC X       VALUE 'J'.                   
009900     88  UPDATE-OK                           VALUE 'J'.                   
010000     88  UPDATE-FEL                          VALUE 'N'.                   
010100                                                                          
010200 77  LAGOMR-SW                   PIC X       VALUE 'J'.                   
010300     88  LAGOMR-OK                           VALUE 'J'.                   
010400     88  LAGOMR-FEL                          VALUE 'N'.                   
010500                                                                          
010600 77  HUVPRC-SW                   PIC X       VALUE 'J'.                   
010700     88  HUVPRC-OK                           VALUE 'J'.                   
010800     88  HUVPRC-FEL                          VALUE 'N'.                   
010900                                                                          
011000 77  PLOCKSATS-SW                PIC X       VALUE 'J'.                   
011100     88  PLOCKSATS-OK                        VALUE 'J'.                   
011200     88  PLOCKSATS-FEL                       VALUE 'N'.                   
011300                                                                          
011400 77  FLAGGA-SW                   PIC X       VALUE 'J'.                   
011500     88  FLAGGA-OK                           VALUE 'J'.                   
011600     88  FLAGGA-FEL                          VALUE 'N'.                   
011700                                                                          
011800 77  MATRIX-SW                   PIC X       VALUE 'J'.                   
011900     88  MATRIX-OK                           VALUE 'J'.                   
012000     88  MATRIX-FEL                          VALUE 'N'.                   
012100                                                                          
012200 77  UTSKRIFT-SW                 PIC X       VALUE 'J'.                   
012300     88  UTSKRIFT-FUNNET                     VALUE 'J'.                   
012400     88  UTSKRIFT-SAKNAS                     VALUE 'N'.                   
012500                                                                          
012600 77  STYRTAB-SW                  PIC X       VALUE 'J'.                   
012700     88  STYRTAB-FUNNET                      VALUE 'J'.                   
012800     88  STYRTAB-SAKNAS                      VALUE 'N'.                   
012900                                                                          
013000 77  S05-LO-SW                   PIC X       VALUE 'J'.                   
013100     88  S05-LO-OK                           VALUE 'J'.                   
013200     88  S05-LO-FEL                          VALUE 'N'.                   
013300                                                                          
013400 77  S05-PRC-SW                  PIC X       VALUE 'J'.                   
013500     88  S05-PRC-OK                          VALUE 'J'.                   
013600     88  S05-PRC-FEL                         VALUE 'N'.                   
013700                                                                          
013800 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
013900     88  EGEN-MID                            VALUE '4364'.                
014000     88  GODK-MID                            VALUE '4364' '4365'          
014100                                                   '4368' '4369'.         
014200 01  WS-START-DATUM              PIC 9(6)             VALUE ZERO.         
014300 01  WS-KDCALL                   PIC 9(1)             VALUE ZERO.         
014400*     SKICKAS MED  TILL W41321                                            
014500*     1 = ÄNDRING / TILLÄGG                                               
014600*     2 = BORTTAG                                                         
014700                                                                          
014800 01  TABELL.                                                              
014900     03  HELP-TABELL OCCURS 11.                                           
015000        05  TAB-ADLAGOMR        PIC 9(2).                                 
015100                                                                          
015200 01  TABELL.                                                              
015300     03  HJAELP-TABELL OCCURS 11.                                         
015400        05  TAB-IDPRC-SUB       PIC X(4).                                 
015500                                                                          
015600   03  HELP-KVVTID-HHMM      PIC  9(3)V9(2)           VALUE ZERO.         
015700   03  HELP-KVVTID-TIM-MIN REDEFINES HELP-KVVTID-HHMM.                    
015800       05  FILLER            PIC  9(1).                                   
015900       05  HELP-KVVTID-TIM                                                
016000                             PIC  9(2).                                   
016100       05  HELP-KVVTID-MIN                                                
016200                             PIC  9(2).                                   
016300   03  HELP-KVVTID-REDIGERAD.                                             
016400       05  HELP-KVVTID-TIM-RED                                            
016500                             PIC  X(2)                VALUE SPACE.        
016600       05  HELP-KVVTID-COLON PIC  X(1)                VALUE ':'.          
016700       05  HELP-KVVTID-MIN-RED                                            
016800                             PIC  X(2)                VALUE SPACE.        
016900 01  VAENTETID.                                                           
017000     05 VAENTETID-TIMMA         PIC 9(3).                                 
017100     05 VAENTETID-MINUT         PIC 9(2).                                 
017200        88  MINUT-OK                         VALUE 0 THRU 59.             
017300                                                                          
017400 01  SPAR-IDPRC-UPDATE.                                                   
017500   03    UPDATE-IDPRCBAS         PIC X(3)    VALUE SPACE.                 
017600   03    UPDATE-IDPRCVAR         PIC X(1)    VALUE SPACE.                 
017700                                                                          
017800 01  SPAR-IDPRC-HUV.                                                      
017900   03    HUV-IDPRCBAS            PIC X(3)    VALUE SPACE.                 
018000   03    HUV-IDPRCVAR            PIC X(1)    VALUE SPACE.                 
018100                                                                          
018200 01  TABELL.                                                              
018300     03 SPAR-IDPRC-SUB-TABELL OCCURS 10.                                  
018400        05 SPAR-IDPRC-SUB.                                                
018500          07 SUB-IDPRCBAS        PIC X(3)    VALUE SPACE.                 
018600          07 SUB-IDPRCVAR        PIC X(1)    VALUE SPACE.                 
018700                                                                          
018800 01  ARB-TID-TABELL.                                                      
018900     03 FILLER                OCCURS 3.                                   
019000        05 -COPY WDGX4436  -PRE TAB-                                      
019100     EJECT                                                                
019200*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
019300 01  GENERELLA-SUBPROGRAM.                                                
019400     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
019500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
019600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
019610     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
019700     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
019800     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
019900     EJECT                                                                
019910*    --- PARAMETERS TO ABEND                                              
019920 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
019930 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
019940 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
019950                                                                          
019960 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
019970     EJECT                                                                
020000*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
020100*   -COPY WMEDAREA                                                        
020200     SKIP3                                                                
020300*    --- PARAMETRAR TILL SUBPROGRAM WDECEDIT                              
020400*   -COPY WDECAREA                                                        
020500     EJECT                                                                
020600*                   ****    PARAMETRAR TILL W005INIT                      
020700*01  -COPY WMSGINIT                                                       
020800     SKIP3                                                                
020900*01  -COPY WMSGINIT -PRE DC-                                              
021000     SKIP3                                                                
021100 01  MESSAGE-CODES.                                                       
021200     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
021300     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
021400     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
021500     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
021600     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
021700     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
021800     03  ERR-UPDATE-NOT-ALLOWED  PIC X(3)    VALUE '777'.                 
021900     03  INF-PRESS-PF11-DELETE   PIC X(3)    VALUE '083'.                 
022000     03  ERR-ORDER-IN-QUEUE      PIC X(3)    VALUE '085'.                 
022100     03  ERR-PRC-STEER-EXISTS    PIC X(3)    VALUE '084'.                 
022200     03  ERR-P-TIDTAB-MISSING    PIC X(3)    VALUE '075'.                 
022300     03  INF-GENERAL-TABLE       PIC X(3)    VALUE '024'.                 
022400     03  ERR-INFO-MISSING        PIC X(3)    VALUE '413'.                 
022500     03  ERR-TABLE-MISSING       PIC X(3)    VALUE '023'.                 
022600     EJECT                                                                
022700*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
022800*                                                                         
022900 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
023000     SKIP3                                                                
023100*01  MID -COPY W4I36401                                                   
023200     EJECT                                                                
023300 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
023400     SKIP3                                                                
023500*01  -COPY WMSGAREA                                                       
023600     EJECT                                                                
023700*    03  MOD -COPY W4O36401   -RED MSG-AREA.                              
023800     EJECT                                                                
023900 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
024000     SKIP3                                                                
024100*01  -COPY WMFSAREA                                                       
024200     EJECT                                                                
024300 01    W-PROG-TO-PROG-SW.                                                 
024400*  03    -COPY WMSGSOP                                                    
024500     EJECT                                                                
024600*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
024700*                                                                         
024800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
024900     SKIP3                                                                
025000 01  NYCKLAR-TILL-DLI.                                                    
025100     03  W-4447-IDHTYP-X.                                                 
025200         05  W-4447-IDHTYP       PIC  X(4)    VALUE '4447'.               
025300         05  W-4447-IDDC         PIC  X(2)    VALUE '00'.                 
025400         05  W-4447-LOW-VALUE    PIC  X(24)   VALUE LOW-VALUE.            
025500     03  W-IDPRC-X.                                                       
025600         05  W-4448-IDPRC.                                                
025700           07 W-4448-IDPRCBAS     PIC X(3)    VALUE SPACE.                
025800           07 W-4448-IDPRCVAR     PIC X       VALUE SPACE.                
025900         05  W-4448-LOW-VALUE     PIC X       VALUE LOW-VALUE.            
026000     03  W-IDPRC-DEF-X.                                                   
026100         05  W-4448-IDPRC-DEF     PIC  X(4)    VALUE '9999'.              
026200         05  W-4448-LOW-VALUE-DEF PIC  X       VALUE LOW-VALUE.           
026300     03  W-4445-IDHTYP-X.                                                 
026400         05  W-4445-IDHTYP    PIC  X(4)    VALUE '4445'.                  
026500         05  W-4445-IDDC      PIC  X(2).                                  
026600         05  W-4445-IDPRCTAB  PIC  9(2)    VALUE 00.                      
026700         05  FILLER           PIC  X(22)   VALUE LOW-VALUE.               
026800     03  W-4453-IDHTYP-X.                                                 
026900         05  W-4453-IDHTYP        PIC  X(4)    VALUE '4453'.              
027000         05  W-4453-IDDC          PIC  X(2)    VALUE '00'.                
027100         05  W-4453-IDPRC         PIC  X(4)    VALUE SPACE.               
027200         05  W-4453-LOW-VALUE     PIC  X(20)   VALUE LOW-VALUE.           
027300     03  W-4453-IDHTYP-DEF-X.                                             
027400         05  W-4453-IDHTYP-DEF    PIC  X(4)    VALUE '4453'.              
027500         05  W-4453-IDDC-DEF      PIC  X(2)    VALUE '00'.                
027600         05  W-4453-IDPRC-DEF     PIC  X(4)    VALUE '9999'.              
027700         05  W-4453-LOW-VALUE-DEF PIC  X(20)   VALUE LOW-VALUE.           
027800     03  W-4461-IDHTYP-X.                                                 
027900         05  W-4461-IDHTYP       PIC  X(4)    VALUE '4461'.               
028000         05  W-4461-IDDC         PIC  X(2)    VALUE '00'.                 
028100         05  W-4461-LOW-VALUE    PIC  X(24)   VALUE LOW-VALUE.            
028200     03  W-KDPRCGRP-X.                                                    
028300         05  W-4462-KDPRCGRP     PIC  X(5)    VALUE SPACE.                
028400     03  W-WDQ3C1KY-MIN-X.                                                
028500         05  W-SEQC-IDDC-MIN PIC      X(2)    VALUE '00'.                 
028600         05  W-SEQC-IDPRCBAS-MIN PIC  X(3)    VALUE SPACE.                
028700         05  W-SEQC-IDPRCVAR-MIN PIC  X(1)    VALUE SPACE.                
028800         05  FILLER              PIC  X(34)   VALUE LOW-VALUE.            
028900     03  W-WDQ3C1KY-MAX-X.                                                
029000         05  W-SEQC-IDDC-MAX PIC      X(2)    VALUE '00'.                 
029100         05  W-SEQC-IDPRCBAS-MAX PIC  X(3)    VALUE SPACE.                
029200         05  W-SEQC-IDPRCVAR-MAX PIC  X(1)    VALUE SPACE.                
029300         05  FILLER              PIC  X(34)   VALUE HIGH-VALUE.           
029400     03  W-4435-IDHTYP-X.                                                 
029500         05  W-4435-IDHTYP        PIC  X(4)    VALUE '4435'.              
029600         05  W-4435-IDDC          PIC  X(2).                              
029700         05  W-4435-LOW-VALUE     PIC  X(24)   VALUE LOW-VALUE.           
029800     03  W-4436-IDHTYP-X.                                                 
029900         05  W-4436-IDPRC         PIC  X(4)    VALUE SPACE.               
030000         05  W-4436-KVKALTIM      PIC  9(2)    VALUE 00.                  
030100         05  W-4436-LOW-VALUE     PIC  X(4)    VALUE LOW-VALUE.           
030200*    --- STATUS-KOD FRÅN IMS                                              
030300 01  STATUS-WS                    PIC XX.                                 
030400     88  SEGMENT-FINNS                       VALUE '  '.                  
030500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
030600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
030700     88  BASEN-SLUT                          VALUE 'GB'.                  
030800     SKIP2                                                                
030900 01  GODK-STATUSKODER.                                                    
031000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
031100     SKIP3                                                                
031200 01  SSA1                        PIC X(160).                              
031300 01  SSA2                        PIC X(64).                               
031400     EJECT                                                                
031500*    --- IMS FUNKTIONSKODER                                               
031600*01  -COPY W0003                                                          
031700     EJECT                                                                
031800*    ---  DLI INPUT-OUTPUT AREA                                           
031900 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
032000     SKIP3                                                                
032100 01  DLI-IO-AREA-4447.                                                    
032200     03  WLXXKH01.                                                        
032300*        05  -COPY WDGX4447                                               
032400     EJECT                                                                
032500 01  DLI-IO-AREA-4448.                                                    
032600     03  WLXXKH11.                                                        
032700*        05  -COPY WDGX4448                                               
032800     SKIP3                                                                
032900 01  DLI-IO-AREA-4445.                                                    
033000     03  WLXXKG01.                                                        
033100*        05  -COPY WDGX4445                                               
033200     EJECT                                                                
033300 01  DLI-IO-AREA-4446.                                                    
033400     03  WLXXKG11.                                                        
033500*        05  -COPY WDGX4446                                               
033600     SKIP3                                                                
033700 01  DLI-IO-AREA-4453.                                                    
033800     03  WLXXKL01.                                                        
033900*        05  -COPY WDGX4453                                               
034000     EJECT                                                                
034100 01  DLI-IO-AREA-4454.                                                    
034200     03  WLXXKL11.                                                        
034300*        05  -COPY WDGX4454                                               
034400     EJECT                                                                
034500 01  DLI-IO-AREA-4461.                                                    
034600     03  WLXXKO01.                                                        
034700*        05  -COPY WDGX4461                                               
034800     EJECT                                                                
034900 01  DLI-IO-AREA-4462.                                                    
035000     03  WLXXKO11.                                                        
035100*        05  -COPY WDGX4462                                               
035200     EJECT                                                                
035300 01  DLI-IO-AREA-SEQC.                                                    
035400     03  WLORQD01.                                                        
035500*        05  -COPY WDQ3C1                                                 
035600     EJECT                                                                
035700 01  DLI-IO-AREA-4436.                                                    
035800     03  WLXXKC11.                                                        
035900*        05  -COPY WDGX4436                                               
036000     EJECT                                                                
036100 LINKAGE SECTION.                                                         
036200                                                                          
036300*01  -COPY W0009      -PRE MSG-                                           
036400     EJECT                                                                
036500*01  -COPY W0009      -PRE ALT-                                           
036600     EJECT                                                                
036700*01  -COPY W0008     -PRE USEA-                                           
036800     05  FILLER              PIC X.                                       
036900     EJECT                                                                
037000*01  -COPY W0008      -PRE XXKH-                                          
037100     05  FILLER                  PIC X.                                   
037200     EJECT                                                                
037300*01  -COPY W0008      -PRE XXKG-                                          
037400     05  FILLER                  PIC X.                                   
037500     EJECT                                                                
037600*01  -COPY W0008      -PRE XXKL-                                          
037700     05  FILLER                  PIC X.                                   
037800     EJECT                                                                
037900*01  -COPY W0008      -PRE XXKO-                                          
038000     05  FILLER                  PIC X.                                   
038100     EJECT                                                                
038200*01  -COPY W0008      -PRE ORQD-                                          
038300     05  FILLER                  PIC X.                                   
038400     EJECT                                                                
038500*01  -COPY W0008      -PRE XXKC-                                          
038600     05  FILLER                  PIC X.                                   
038700     EJECT                                                                
038800 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB                                
038900                           USEA-PCB                                       
039000                           XXKH-PCB XXKG-PCB XXKL-PCB                     
039100                           XXKO-PCB                                       
039200                           ORQD-PCB                                       
039300                           XXKC-PCB.                                      
039400     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB                                
039500                           USEA-PCB                                       
039600                           XXKH-PCB XXKG-PCB XXKL-PCB                     
039700                           XXKO-PCB                                       
039800                           ORQD-PCB                                       
039900                           XXKC-PCB.                                      
040000     PERFORM IMS-GET-MSG                                                  
040100     IF SEGMENT-FINNS                                                     
040200       PERFORM A-INIT                                                     
040300       PERFORM B-KOLLA-NYCKLAR                                            
040400       IF NYCKLAR-OK                                                      
040500         IF MFS-UPDATE                                                    
040600           PERFORM G-KOLLA-INPUT                                          
040700           IF INDATA-OK                                                   
040800             IF MID-IDPRC-UPDATE = ZERO                                   
040900               PERFORM HB-TABORT-XXKH-MEDLEM                              
041000             ELSE                                                         
041100               PERFORM H-UPPDATERA                                        
041200             END-IF                                                       
041300           END-IF                                                         
041400         ELSE                                                             
041500           PERFORM F-LAES-VISA-INFO                                       
041600         END-IF                                                           
041700       END-IF                                                             
041800       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
041900       PERFORM IMS-INSERT-MSG                                             
042000     END-IF                                                               
042100                                                                          
042200     MOVE ZERO TO RETURN-CODE                                             
042300     GOBACK                                                               
042400     .                                                                    
042500     EJECT                                                                
042600 A-INIT SECTION.                                                          
042700     MOVE 'STA A-INIT    '           TO PGMPOS                            
042800                                                                          
042900     IF MSG-DUBBLA-TRANSKODER                                             
043000       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I36401                 
043100       MOVE MSG-IDTRANS-2    TO MFS-IDTRANS                               
043200       MOVE MSG-KDMFSFOR-2   TO MFS-KDMFSFOR                              
043300     ELSE                                                                 
043400       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I36401                  
043500       MOVE MSG-IDTRANS-1    TO MFS-IDTRANS                               
043600       MOVE MSG-KDMFSFOR-1   TO MFS-KDMFSFOR                              
043700     END-IF                                                               
043800     MOVE '4364'             TO MSGSOP-IDTRANS                            
043900     MOVE MFS-KDMFSFOR       TO MSGSOP-KDMFSFOR                           
044000     MOVE 'W413S1'           TO MSGSOP-IDPROCESS                          
044100     MOVE 'O'                TO MSGSOP-KDSOPFUNK                          
044200     MOVE ZERO               TO WS-START-DATUM                            
044300                                                                          
044400     MOVE MSG-KDTRTYP        TO MFS-KDTRTYP                               
044500     MOVE MSG-IDPFK          TO MFS-IDPFK                                 
044600     MOVE MFS-IDTRANS        TO W-IDTRANS                                 
044700                                                                          
044800     MOVE LOW-VALUE          TO MSG-AREA                                  
044900     MOVE 'W4O364N1'         TO MFS-IDMOD                                 
045000     MOVE '4364'             TO MOD-IDTRANS                               
045100     MOVE MFS-RENSA-FAELT    TO MOD-TEMFSFEL MOD-TEMFSINF                 
045200                                                                          
045300     IF NOT EGEN-MID                                                      
045400       MOVE SPACE            TO MFS-KDTRTYP                               
045500       MOVE '7'              TO MFS-IDPFK                                 
045600     END-IF                                                               
045700     MOVE 'END A-INIT    '           TO PGMPOS                            
045800     .                                                                    
045900     EJECT                                                                
046000 B-KOLLA-NYCKLAR SECTION.                                                 
046100     MOVE 'STA B-KOLLA-  '           TO PGMPOS                            
046200                                                                          
046300     MOVE ALL '+'           TO MSGI-WMSGINIT                              
046400     MOVE '013'             TO MSGI-KDCALL                                
046500     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
046600     MOVE '4364'            TO MSGI-IDTRANS                               
046700     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
046800     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
046900                                                                          
047000     IF MSGI-IDLAND-SPR = 'GB'                                            
047100       MOVE +2               TO SPRAK-IX                                  
047200       MOVE 'GB '            TO MED-IDSKYLT                               
047300     ELSE                                                                 
047400       MOVE +1               TO SPRAK-IX                                  
047500       MOVE 'S  '            TO MED-IDSKYLT                               
047600     END-IF                                                               
047700                                                                          
047800     IF GODK-MID                                                          
047900       MOVE JA TO NYCKLAR-SW                                              
048000                                                                          
048100       MOVE LOW-VALUE       TO W-WDQ3C1KY-MIN-X                           
048200       MOVE HIGH-VALUE      TO W-WDQ3C1KY-MAX-X                           
048300                                                                          
048400       MOVE MFS-RENSA-FAELT TO MOD-IDPRC-IN                               
048500                                                                          
048600       IF MID-IDPRC-IN = ALL '+'                                          
048700         MOVE MID-IDPRC-UT TO WS-IDPRC                                    
048800         INSPECT WS-IDPRC REPLACING LEADING SPACE BY ZERO                 
048900       ELSE                                                               
049000         MOVE MID-IDPRC-IN TO WS-IDPRC                                    
049100         INSPECT WS-IDPRCBAS REPLACING LEADING SPACE BY ZERO              
049200         MOVE '7'       TO MFS-IDPFK                                      
049300         MOVE SPACE     TO MFS-KDTRTYP                                    
049400       END-IF                                                             
049500                                                                          
049600       IF WS-IDPRCBAS NUMERIC AND WS-IDPRCBAS > 099                       
049700         MOVE WS-IDPRC TO W-4448-IDPRCBAS                                 
049800                          W-SEQC-IDPRCBAS-MIN                             
049900                          W-SEQC-IDPRCBAS-MAX                             
050000       ELSE                                                               
050100         MOVE NEJ TO NYCKLAR-SW                                           
050200       END-IF                                                             
050300                                                                          
050400       IF WS-IDPRCVAR NOT = SPACE                                         
050500         MOVE WS-IDPRCVAR TO W-4448-IDPRCVAR                              
050600                             W-SEQC-IDPRCVAR-MIN                          
050700                             W-SEQC-IDPRCVAR-MAX                          
050800       END-IF                                                             
050900                                                                          
051000       MOVE MFS-RENSA-FAELT TO MOD-IDDC-IN                                
051100                                                                          
051200       MOVE MSGI-IDDC               TO WS-IDDC                            
051300                                                                          
051400       MOVE ALL '+'           TO DC-MSGI-WMSGINIT                         
051500       MOVE '013'             TO DC-MSGI-KDCALL                           
051600       MOVE WS-IDDC           TO WS-DCUSER-IDDC                           
051700       MOVE WS-DCUSER         TO DC-MSGI-IDUSER                           
051800       MOVE '4383'            TO DC-MSGI-IDTRANS                          
051900       MOVE MSG-LTERM-NAME    TO DC-MSGI-IDLTERM-USER                     
052000       CALL W005INIT USING DC-MSGI-WMSGINIT USEA-PCB                      
052100       MOVE DC-MSGI-TILOKDAT  TO WS-LOCAL-DATE                            
052200                                                                          
052300       IF WS-IDDC IS > SPACE                                              
052400         CONTINUE                                                         
052500       ELSE                                                               
052600         MOVE NEJ                     TO NYCKLAR-SW                       
052700       END-IF                                                             
052800                                                                          
052900       IF NYCKLAR-OK                                                      
053000         MOVE WS-IDDC    TO W-4447-IDDC                                   
053100                            W-4445-IDDC                                   
053200                            W-4453-IDDC                                   
053300                            W-4453-IDDC-DEF                               
053400                            W-4461-IDDC                                   
053500                            W-4435-IDDC                                   
053600       END-IF                                                             
053700                                                                          
053800       IF GODK-MID OR NYCKLAR-OK                                          
053900         MOVE WS-IDPRC TO MOD-IDPRC-UT                                    
054000         INSPECT MOD-IDPRC-UT REPLACING LEADING ZERO BY SPACE             
054100         MOVE WS-IDDC  TO MOD-IDDC-UT                                     
054200         INSPECT MOD-IDDC-UT REPLACING LEADING ZERO BY SPACE              
054300       ELSE                                                               
054400         MOVE MFS-RENSA-FAELT TO MOD-IDPRC-UT                             
054500                                 MOD-IDDC-UT                              
054600       END-IF                                                             
054700                                                                          
054800       IF NYCKLAR-FEL                                                     
054900         MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                               
055000         CALL WMEDKONV USING MED-WMEDAREA                                 
055100         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
055200         MOVE MFS-RENSA-FAELT TO MOD-IDPRC-UPDATE                         
055300         PERFORM MFS-RENSA-FAELT-IN                                       
055400         PERFORM MFS-RENSA-FAELT-UT                                       
055500       END-IF                                                             
055600     ELSE                                                                 
055700       MOVE NEJ TO NYCKLAR-SW                                             
055800       PERFORM MFS-RENSA-FAELT-IN                                         
055900       PERFORM MFS-RENSA-FAELT-UT                                         
056000       MOVE MFS-RENSA-FAELT TO MOD-IDPRC-UT                               
056100                               MOD-IDPRC-IN                               
056200                               MOD-IDDC-UT                                
056300                               MOD-IDDC-IN                                
056400     END-IF                                                               
056500     MOVE 'END B-KOLLA-  '           TO PGMPOS                            
056600     .                                                                    
056700     EJECT                                                                
056800 F-LAES-VISA-INFO SECTION.                                                
056900     MOVE 'STA F-LAES-VIS'           TO PGMPOS                            
057000                                                                          
057100     PERFORM IMS-GHU-XXKH-XXKH11                                          
057200     IF SEGMENT-FINNS                                                     
057300       IF EGEN-MID AND (MID-INPUT NOT = ALL '+' OR                        
057400         (MID-FLAGGA-RAD NOT = MID-FLAGGA-SPAR AND                        
057500          MID-FLAGGA-RAD NOT = ALL '+'))                                  
057600         PERFORM MFS-ROR-EJ-FAELT-IN                                      
057700         PERFORM MFS-ROR-EJ-FAELT-UT                                      
057800         MOVE MFS-ROER-EJ-FAELT TO MOD-IDPRC-UPDATE                       
057900         MOVE MFS-ROER-EJ-FAELT TO MOD-FLAGGA-SPAR                        
058000         MOVE MFS-ROER-EJ-FAELT TO MOD-FLAGGA-BORTTAG                     
058100         PERFORM MFS-LAS-IN-IGEN                                          
058200         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
058300         CALL WMEDKONV USING MED-WMEDAREA                                 
058400         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
058500       ELSE                                                               
058600         PERFORM S01-VISA-PRC-TABELL                                      
058700       END-IF                                                             
058800     ELSE                                                                 
058900       MOVE ERR-TABLE-MISSING TO MED-IDMFSFEL                             
059000       CALL WMEDKONV USING MED-WMEDAREA                                   
059100       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
059200       MOVE MFS-RENSA-FAELT TO MOD-IDPRC-UPDATE                           
059300       PERFORM MFS-RENSA-FAELT-IN                                         
059400       PERFORM MFS-RENSA-FAELT-UT                                         
059500       MOVE NEJ TO NYCKLAR-SW                                             
059600     END-IF                                                               
059700     MOVE 'END F-LAES-VIS'           TO PGMPOS                            
059800     .                                                                    
059900     EJECT                                                                
060000 G-KOLLA-INPUT SECTION.                                                   
060100     MOVE 'STA G-KOLLA-IN'           TO PGMPOS                            
060200                                                                          
060300     MOVE JA  TO INDATA-SW                                                
060400     MOVE JA  TO MATRIX-SW                                                
060500                                                                          
060600     IF  MID-INPUT = ALL '+'                                              
060700     AND MID-FLAGGA-RAD = MID-FLAGGA-SPAR                                 
060800       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
060900       CALL WMEDKONV USING MED-WMEDAREA                                   
061000       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
061100       PERFORM MFS-ROR-EJ-FAELT-IN                                        
061200       PERFORM MFS-ROR-EJ-FAELT-UT                                        
061300       MOVE MFS-ROER-EJ-FAELT TO MOD-IDPRC-UPDATE                         
061400       MOVE MFS-ROER-EJ-FAELT TO MOD-FLAGGA-SPAR                          
061500       MOVE MFS-ROER-EJ-FAELT TO MOD-FLAGGA-BORTTAG                       
061600       MOVE NEJ TO INDATA-SW                                              
061700     ELSE                                                                 
061800       IF MID-IDPRC-UPDATE NOT = ALL '+'                                  
061900         MOVE MID-IDPRC-UPDATE TO SPAR-IDPRC-UPDATE                       
062000         IF (UPDATE-IDPRCBAS > +099 OR UPDATE-IDPRCBAS = ZERO) AND        
062100           (UPDATE-IDPRCVAR NOT = SPACE)                                  
062200           MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDPRC-UPDATE-ATTR             
062300         ELSE                                                             
062400           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDPRC-UPDATE-ATTR               
062500           MOVE NEJ TO INDATA-SW                                          
062600         END-IF                                                           
062700       END-IF                                                             
062800                                                                          
062900       IF MID-FLAGGA-RAD = ALL '+'                                        
063000       OR MID-FLAGGA-RAD = JA                                             
063100       OR MID-FLAGGA-RAD = NEJ                                            
063200         MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLAGGA-UT-ATTR                  
063300       ELSE                                                               
063400         MOVE MFS-ALFA-FAELT-FEL TO MOD-FLAGGA-UT-ATTR                    
063500         MOVE NEJ TO INDATA-SW                                            
063600       END-IF                                                             
063610                                                                          
063800       IF MID-FLSTJORD-UPDATE NOT = ALL '+'                               
064000         IF MID-FLSTJORD-UPDATE = JA                                      
064010         OR MID-FLSTJORD-UPDATE = YES                                     
064100         OR MID-FLSTJORD-UPDATE = NEJ                                     
064200           MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLSTJORD-UPDATE-ATTR          
064300         ELSE                                                             
064400           MOVE MFS-ALFA-FAELT-FEL TO MOD-FLSTJORD-UPDATE-ATTR            
064500           MOVE NEJ TO INDATA-SW                                          
064600         END-IF                                                           
064610       END-IF                                                             
064700                                                                          
064800       MOVE +1 TO LO-INDX                                                 
064900       PERFORM UNTIL LO-INDX > MAX-LO-INDX                                
065000         IF MID-ADLAGOMR-UPDATE(LO-INDX) NOT = ALL '+'                    
065100           IF MID-ADLAGOMR-UPDATE(LO-INDX) NOT NUMERIC OR                 
065200             (MID-ADLAGOMR-UPDATE(LO-INDX) = ZERO AND                     
065300                                             LO-INDX = +1)                
065400             MOVE MFS-NUM-FAELT-FEL TO MOD-ADLAGOMR-UPDATE-ATTR           
065500                                                     (LO-INDX)            
065600             MOVE NEJ TO INDATA-SW                                        
065700           ELSE                                                           
065800             MOVE MFS-NUM-FAELT-RAETT TO MOD-ADLAGOMR-UPDATE-ATTR         
065900                                                     (LO-INDX)            
066000           END-IF                                                         
066100         END-IF                                                           
066200         ADD +1 TO LO-INDX                                                
066300       END-PERFORM                                                        
066400                                                                          
066500       IF MID-KDPRCTYP-UPDATE NOT = ALL '+'                               
066600         IF MID-KDPRCTYP-UPDATE = '1' OR                                  
066700           MID-KDPRCTYP-UPDATE = '3' OR                                   
066800           MID-KDPRCTYP-UPDATE = '2'                                      
066900           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDPRCTYP-UPDATE-ATTR          
067000         ELSE                                                             
067100           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDPRCTYP-UPDATE-ATTR            
067200           MOVE NEJ TO INDATA-SW                                          
067300         END-IF                                                           
067400       END-IF                                                             
067500                                                                          
067600       IF MID-KVVTID-UPDATE NOT = ALL '+'                                 
067700         MOVE MID-KVVTID-UPDATE TO DEC-IDFRIDATA                          
067800         MOVE +2 TO DEC-KVHELTAL                                          
067900         MOVE +2 TO DEC-KVDECIMAL                                         
068000         CALL WDECEDIT USING DEC-WDECAREA                                 
068100         MOVE MID-KVVTID-UPDATE TO VAENTETID                              
068200         IF DEC-KDSVAR-OK AND MINUT-OK                                    
068300           MOVE MFS-NUM-FAELT-RAETT TO MOD-KVVTID-UPDATE-ATTR             
068400           MOVE DEC-IDEDITDATA TO WS-KVVTID                               
068500         ELSE                                                             
068600           MOVE MFS-NUM-FAELT-FEL TO MOD-KVVTID-UPDATE-ATTR               
068700           MOVE NEJ TO INDATA-SW                                          
068800         END-IF                                                           
068900       END-IF                                                             
069000                                                                          
069100       IF MID-RESPLIT-UPDATE NOT = ALL '+'                                
069200         MOVE MID-RESPLIT-UPDATE TO DEC-IDFRIDATA                         
069300         MOVE +1 TO DEC-KVHELTAL                                          
069400         MOVE +2 TO DEC-KVDECIMAL                                         
069500         CALL WDECEDIT USING DEC-WDECAREA                                 
069600         IF DEC-KDSVAR-FEL                                                
069700           MOVE MFS-NUM-FAELT-FEL TO MOD-RESPLIT-UPDATE-ATTR              
069800           MOVE NEJ TO INDATA-SW                                          
069900         ELSE                                                             
070000           MOVE DEC-IDEDITDATA TO WS-RESPLIT                              
070100           MOVE MFS-NUM-FAELT-RAETT TO MOD-RESPLIT-UPDATE-ATTR            
070200         END-IF                                                           
070300       END-IF                                                             
070400                                                                          
070500       IF MID-IDPRC-HUV-UPDATE NOT = ALL '+'                              
070600         MOVE MID-IDPRC-HUV-UPDATE TO SPAR-IDPRC-HUV                      
070700         IF (HUV-IDPRCBAS < +100 OR HUV-IDPRCBAS NOT = ZERO) AND          
070800           (HUV-IDPRCVAR = SPACE) OR                                      
070900           MID-IDPRC-HUV-UPDATE = MOD-IDPRC-UT                            
071000           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDPRC-HUV-UPDATE-ATTR           
071100           MOVE NEJ TO INDATA-SW                                          
071200         ELSE                                                             
071300           MOVE MID-IDPRC-HUV-UPDATE TO W-4448-IDPRC                      
071400           PERFORM IMS-GHU-XXKH-XXKH11                                    
071500           IF SEGMENT-SAKNAS                                              
071600             MOVE MFS-ALFA-FAELT-FEL TO MOD-IDPRC-HUV-UPDATE-ATTR         
071700             MOVE NEJ TO INDATA-SW                                        
071800           ELSE                                                           
071900            MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDPRC-HUV-UPDATE-ATTR        
072000           END-IF                                                         
072100           MOVE WS-IDPRC TO W-4448-IDPRC                                  
072200         END-IF                                                           
072300       END-IF                                                             
072400                                                                          
072500       IF MID-VLKOLGR-UPDATE NOT = ALL '+'                                
072600         MOVE MID-VLKOLGR-UPDATE TO DEC-IDFRIDATA                         
072700         MOVE +1 TO DEC-KVHELTAL                                          
072800         MOVE +2 TO DEC-KVDECIMAL                                         
072900         CALL WDECEDIT USING DEC-WDECAREA                                 
073000         IF DEC-KDSVAR-FEL                                                
073100           MOVE MFS-NUM-FAELT-FEL TO MOD-VLKOLGR-UPDATE-ATTR              
073200           MOVE NEJ TO INDATA-SW                                          
073300         ELSE                                                             
073400           MOVE DEC-IDEDITDATA TO WS-VLKOLGR                              
073500           MOVE MFS-NUM-FAELT-RAETT TO MOD-VLKOLGR-UPDATE-ATTR            
073600         END-IF                                                           
073700       END-IF                                                             
073800                                                                          
073900       IF MID-BEPRC-RAD NOT = ALL '+'                                     
074000         MOVE MFS-ALFA-FAELT-RAETT TO MOD-BEPRC-ATTR                      
074100       END-IF                                                             
074200                                                                          
074300       MOVE +1 TO SUB-INDX                                                
074400       PERFORM UNTIL SUB-INDX > MAX-SUB-INDX                              
074500        IF MID-IDPRC-SUB-UPDATE(SUB-INDX) NOT = ALL '+'                   
074600          MOVE MID-IDPRC-SUB-UPDATE(SUB-INDX) TO                          
074700          SPAR-IDPRC-SUB(SUB-INDX)                                        
074800          IF (SUB-IDPRCBAS(SUB-INDX) > +099 OR                            
074900             (SUB-IDPRCBAS(SUB-INDX) = ZERO AND                           
075000              SUB-INDX NOT = +1)) AND                                     
075100            (SUB-IDPRCVAR(SUB-INDX) NOT = SPACE) AND                      
075200            MID-IDPRC-SUB-UPDATE(SUB-INDX) NOT = MOD-IDPRC-UT             
075300            MOVE MFS-ALFA-FAELT-RAETT TO                                  
075400            MOD-IDPRC-SUB-UPDATE-ATTR(SUB-INDX)                           
075500          ELSE                                                            
075600            MOVE MFS-ALFA-FAELT-FEL TO MOD-IDPRC-SUB-UPDATE-ATTR          
075700                                                  (SUB-INDX)              
075800            MOVE NEJ TO INDATA-SW                                         
075900          END-IF                                                          
076000        END-IF                                                            
076100        ADD +1 TO SUB-INDX                                                
076200       END-PERFORM                                                        
076300                                                                          
076400       IF MID-KDPRCGRP-UPDATE NOT = ALL '+'                               
076500         IF MID-KDPRCGRP-UPDATE NOT NUMERIC OR                            
076600           MID-KDPRCGRP-UPDATE = ZERO                                     
076700           MOVE MFS-NUM-FAELT-FEL TO MOD-KDPRCGRP-UPDATE-ATTR             
076800           MOVE NEJ TO INDATA-SW                                          
076900         ELSE                                                             
077000           MOVE MFS-NUM-FAELT-RAETT TO MOD-KDPRCGRP-UPDATE-ATTR           
077100         END-IF                                                           
077200       END-IF                                                             
077300                                                                          
077400       IF MID-KDPRODKL-UPDATE NOT = ALL '+'                               
077500         MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDPRODKL-UPDATE-ATTR            
077600       END-IF                                                             
077700                                                                          
077800       IF MID-KVORDER-UPDATE NOT = ALL '+'                                
077900* 99 ORDER ÄR MAX PÅ MELLANLAGINGSBASEN(HTYP-4002) I UTSKRIFTEN.          
078000         IF MID-KVORDER-UPDATE NOT NUMERIC OR                             
078100           MID-KVORDER-UPDATE > 99 OR                                     
078200           MID-KVORDER-UPDATE = ZERO                                      
078300           MOVE MFS-NUM-FAELT-FEL TO MOD-KVORDER-UPDATE-ATTR              
078400           MOVE NEJ TO INDATA-SW                                          
078500         ELSE                                                             
078600           MOVE MFS-NUM-FAELT-RAETT TO MOD-KVORDER-UPDATE-ATTR            
078700         END-IF                                                           
078800       END-IF                                                             
078900                                                                          
079000       IF MID-KVPLSRAD-UPDATE NOT = ALL '+'                               
079100         IF MID-KVPLSRAD-UPDATE NOT NUMERIC OR                            
079200           MID-KVPLSRAD-UPDATE > 999 OR                                   
079300           MID-KVPLSRAD-UPDATE = ZERO                                     
079400           MOVE MFS-NUM-FAELT-FEL TO MOD-KVPLSRAD-UPDATE-ATTR             
079500           MOVE NEJ TO INDATA-SW                                          
079600         ELSE                                                             
079700           MOVE MFS-NUM-FAELT-RAETT TO MOD-KVPLSRAD-UPDATE-ATTR           
079800         END-IF                                                           
079900       END-IF                                                             
080000                                                                          
080100       IF MID-KVRADER-UPDATE NOT = ALL '+'                                
080200         IF MID-KVRADER-UPDATE NOT NUMERIC OR                             
080300           MID-KVRADER-UPDATE > 999 OR                                    
080400           MID-KVRADER-UPDATE = ZERO                                      
080500           MOVE MFS-NUM-FAELT-FEL TO MOD-KVRADER-UPDATE-ATTR              
080600           MOVE NEJ TO INDATA-SW                                          
080700         ELSE                                                             
080800           MOVE MFS-NUM-FAELT-RAETT TO MOD-KVRADER-UPDATE-ATTR            
080900         END-IF                                                           
081000       END-IF                                                             
081100                                                                          
081200       IF MID-VKPLSNTO-UPDATE NOT = ALL '+'                               
081300         MOVE MID-VKPLSNTO-UPDATE TO DEC-IDFRIDATA                        
081400         MOVE +6 TO DEC-KVHELTAL                                          
081500         MOVE +1 TO DEC-KVDECIMAL                                         
081600         CALL WDECEDIT USING DEC-WDECAREA                                 
081700         IF DEC-KDSVAR-FEL                                                
081800           MOVE MFS-NUM-FAELT-FEL TO MOD-VKPLSNTO-UPDATE-ATTR             
081900           MOVE NEJ TO INDATA-SW                                          
082000         ELSE                                                             
082100           MOVE DEC-IDEDITDATA TO WS-VKPLSNTO                             
082200           MOVE MFS-NUM-FAELT-RAETT TO MOD-VKPLSNTO-UPDATE-ATTR           
082300         END-IF                                                           
082400       END-IF                                                             
082500                                                                          
082600       IF MID-VKORDNTO-UPDATE NOT = ALL '+'                               
082700         MOVE MID-VKORDNTO-UPDATE TO DEC-IDFRIDATA                        
082800         MOVE +6 TO DEC-KVHELTAL                                          
082900         MOVE +1 TO DEC-KVDECIMAL                                         
083000         CALL WDECEDIT USING DEC-WDECAREA                                 
083100         IF DEC-KDSVAR-FEL                                                
083200           MOVE MFS-NUM-FAELT-FEL TO MOD-VKORDNTO-UPDATE-ATTR             
083300           MOVE NEJ TO INDATA-SW                                          
083400         ELSE                                                             
083500           MOVE DEC-IDEDITDATA TO WS-VKORDNTO                             
083600           MOVE MFS-NUM-FAELT-RAETT TO MOD-VKORDNTO-UPDATE-ATTR           
083700         END-IF                                                           
083800       END-IF                                                             
083900                                                                          
084000       IF MID-VLPLSNTO-UPDATE NOT = ALL '+'                               
084100         MOVE MID-VLPLSNTO-UPDATE TO DEC-IDFRIDATA                        
084200         MOVE +4 TO DEC-KVHELTAL                                          
084300         MOVE +3 TO DEC-KVDECIMAL                                         
084400         CALL WDECEDIT USING DEC-WDECAREA                                 
084500         IF DEC-KDSVAR-FEL                                                
084600           MOVE MFS-NUM-FAELT-FEL TO MOD-VLPLSNTO-UPDATE-ATTR             
084700           MOVE NEJ TO INDATA-SW                                          
084800         ELSE                                                             
084900           MOVE DEC-IDEDITDATA TO WS-VLPLSNTO                             
085000           MOVE MFS-NUM-FAELT-RAETT TO MOD-VLPLSNTO-UPDATE-ATTR           
085100         END-IF                                                           
085200       END-IF                                                             
085300                                                                          
085400       IF MID-VLORDNTO-UPDATE NOT = ALL '+'                               
085500         MOVE MID-VLORDNTO-UPDATE TO DEC-IDFRIDATA                        
085600         MOVE +4 TO DEC-KVHELTAL                                          
085700         MOVE +3 TO DEC-KVDECIMAL                                         
085800         CALL WDECEDIT USING DEC-WDECAREA                                 
085900         IF DEC-KDSVAR-FEL                                                
086000           MOVE MFS-NUM-FAELT-FEL TO MOD-VLORDNTO-UPDATE-ATTR             
086100           MOVE NEJ TO INDATA-SW                                          
086200         ELSE                                                             
086300           MOVE DEC-IDEDITDATA TO WS-VLORDNTO                             
086400           MOVE MFS-NUM-FAELT-RAETT TO MOD-VLORDNTO-UPDATE-ATTR           
086500         END-IF                                                           
086600       END-IF                                                             
086700                                                                          
086800       IF INDATA-FEL                                                      
086900         IF MATRIX-FEL                                                    
087000           MOVE ERR-P-TIDTAB-MISSING TO MED-IDMFSFEL                      
087100           CALL WMEDKONV USING MED-WMEDAREA                               
087200           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
087300         ELSE                                                             
087400           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
087500           CALL WMEDKONV USING MED-WMEDAREA                               
087600           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
087700         END-IF                                                           
087800         PERFORM MFS-ROR-EJ-FAELT-UT                                      
087900         MOVE MFS-ROER-EJ-FAELT TO MOD-IDPRC-UPDATE                       
088000         PERFORM MFS-ROR-EJ-FAELT-IN                                      
088100       END-IF                                                             
088200                                                                          
088300     END-IF                                                               
088400     MOVE 'END G-KOLLA-IN'           TO PGMPOS                            
088500     .                                                                    
088600     EJECT                                                                
088700                                                                          
088800 H-UPPDATERA SECTION.                                                     
088900     MOVE 'STA H-UPPDATER'           TO PGMPOS                            
089000                                                                          
089100     MOVE 1                    TO WS-KDCALL                               
089200     PERFORM IMS-GHU-XXKH-XXKH11                                          
089300     IF SEGMENT-FINNS                                                     
089400       IF MID-IDPRC-UT NOT = '9999'                                       
089500         PERFORM HC-AENDRA-XXKH-MEDLEM                                    
089600       ELSE                                                               
089700         MOVE MFS-RENSA-FAELT TO MOD-IDPRC-UPDATE                         
089800         PERFORM MFS-RENSA-FAELT-IN                                       
089900         PERFORM IMS-GHU-XXKH-XXKH11-DEF                                  
090000         PERFORM S01-VISA-PRC-TABELL                                      
090100         MOVE ERR-UPDATE-NOT-ALLOWED TO MED-IDMFSFEL                      
090200         CALL WMEDKONV USING MED-WMEDAREA                                 
090300         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
090400       END-IF                                                             
090500     ELSE                                                                 
090600       PERFORM HD-NYUPPLAEGG-XXKH-MEDLEM                                  
090700     END-IF                                                               
090800     MOVE 'END H-UPPDATER'           TO PGMPOS                            
090900     .                                                                    
091000     EJECT                                                                
091100                                                                          
091200 HB-TABORT-XXKH-MEDLEM SECTION.                                           
091300     MOVE 'STA HB-TABORT '           TO PGMPOS                            
091400                                                                          
091500     MOVE 2                    TO WS-KDCALL                               
091600     IF MID-IDPRC-UPDATE = ZERO AND MID-FLAGGA-BORTTAG = NEJ              
091700       PERFORM HBA-KOLLA-PRC-STYRTABELL                                   
091800       IF STYRTAB-SAKNAS                                                  
091900         PERFORM HBB-KOLLA-PRC-UTSKRIFT                                   
092000       END-IF                                                             
092100       IF UTSKRIFT-SAKNAS AND STYRTAB-SAKNAS                              
092200         MOVE JA TO MOD-FLAGGA-BORTTAG                                    
092300         MOVE INF-PRESS-PF11-DELETE TO MED-IDMFSINF                       
092400         CALL WMEDKONV USING MED-WMEDAREA                                 
092500         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
092600         MOVE MFS-ROER-EJ-FAELT TO MOD-IDPRC-UPDATE                       
092700         PERFORM MFS-ROR-EJ-FAELT-IN                                      
092800         PERFORM MFS-ROR-EJ-FAELT-UT                                      
092900         PERFORM MFS-LAS-IN-IGEN                                          
093000       END-IF                                                             
093100     ELSE                                                                 
093200         PERFORM HBC-DLET-XXKL-MEDLEM                                     
093300         PERFORM HBD-DLET-XXKC-MEDLEM                                     
093400         PERFORM HBE-DLET-XXKH-MEDLEM                                     
093500         PERFORM S11-STARTA-BMP                                           
093600     END-IF                                                               
093700     MOVE 'END HB-TABORT '           TO PGMPOS                            
093800     .                                                                    
093900     EJECT                                                                
094000 HBA-KOLLA-PRC-STYRTABELL SECTION.                                        
094100     MOVE 'STA HBA-KOLLA '           TO PGMPOS                            
094200                                                                          
094300     MOVE NEJ TO STYRTAB-SW                                               
094400     PERFORM IMS-GU-XXKG-XXKG01                                           
094500     PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT OR                        
094600       STYRTAB-FUNNET                                                     
094700       IF SEGMENT-FINNS                                                   
094800         PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT OR                    
094900           STYRTAB-FUNNET                                                 
095000           PERFORM IMS-GNP-XXKG-XXKG11                                    
095100           IF SEGMENT-FINNS                                               
095200             PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT OR                
095300               STYRTAB-FUNNET                                             
095400               IF 4446-IDPRC = WS-IDPRC AND                               
095500                  4445-IDDC = W-4445-IDDC                                 
095600                 MOVE JA TO STYRTAB-SW                                    
095700               ELSE                                                       
095800                 PERFORM IMS-GNP-XXKG-XXKG11                              
095900               END-IF                                                     
096000             END-PERFORM                                                  
096100           END-IF                                                         
096200         END-PERFORM                                                      
096300       END-IF                                                             
096400       PERFORM IMS-GN-XXKG-XXKG01                                         
096500     END-PERFORM                                                          
096600     IF STYRTAB-FUNNET                                                    
096700       MOVE MFS-RENSA-FAELT TO MOD-IDPRC-UPDATE                           
096800       PERFORM MFS-RENSA-FAELT-IN                                         
096900       PERFORM IMS-GHU-XXKH-XXKH11                                        
097000       PERFORM S01-VISA-PRC-TABELL                                        
097100       MOVE ERR-PRC-STEER-EXISTS TO MED-IDMFSFEL                          
097200       CALL WMEDKONV USING MED-WMEDAREA                                   
097300       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
097400     END-IF                                                               
097500     MOVE 'END HBA-KOLLA '           TO PGMPOS                            
097600     .                                                                    
097700     EJECT                                                                
097800 HBB-KOLLA-PRC-UTSKRIFT SECTION.                                          
097900     MOVE 'STA HBB-KOLLA '           TO PGMPOS                            
098000                                                                          
098100     MOVE NEJ TO UTSKRIFT-SW                                              
098200     MOVE WS-IDDC     TO W-SEQC-IDDC-MIN                                  
098300                         W-SEQC-IDDC-MAX                                  
098400     PERFORM IMS-GU-ORQD-ORQD01                                           
098500     IF SEGMENT-FINNS                                                     
098600       MOVE JA TO UTSKRIFT-SW                                             
098700     END-IF                                                               
098800     IF UTSKRIFT-FUNNET                                                   
098900       MOVE MFS-RENSA-FAELT TO MOD-IDPRC-UPDATE                           
099000       PERFORM MFS-RENSA-FAELT-IN                                         
099100       PERFORM IMS-GHU-XXKH-XXKH11                                        
099200       PERFORM S01-VISA-PRC-TABELL                                        
099300       MOVE ERR-ORDER-IN-QUEUE TO MED-IDMFSFEL                            
099400       CALL WMEDKONV USING MED-WMEDAREA                                   
099500       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
099600     END-IF                                                               
099700     MOVE 'END HBB-KOLLA '           TO PGMPOS                            
099800     .                                                                    
099900     EJECT                                                                
100000 HBC-DLET-XXKL-MEDLEM SECTION.                                            
100100     MOVE 'STA HBC-DLET- '           TO PGMPOS                            
100200                                                                          
100300     IF MID-FLAGGA-BORTTAG = JA                                           
100400       MOVE WS-IDPRC TO W-4453-IDPRC                                      
100500       PERFORM IMS-GHU-XXKL-XXKL01                                        
100600       IF SEGMENT-SAKNAS                                                  
100700         CONTINUE                                                         
100800       ELSE                                                               
100900         PERFORM IMS-DLET-XXKL                                            
101000       END-IF                                                             
101100     END-IF                                                               
101200     MOVE 'END HBC-DLET- '           TO PGMPOS                            
101300     .                                                                    
101400     EJECT                                                                
101500 HBD-DLET-XXKC-MEDLEM SECTION.                                            
101600     MOVE 'STA HBD-DLET- '           TO PGMPOS                            
101700                                                                          
101800     IF MID-FLAGGA-BORTTAG = JA                                           
101900        MOVE WS-IDPRC TO W-4436-IDPRC                                     
102000        MOVE 1        TO IX1                                              
102100        PERFORM UNTIL IX1 > 3                                             
102200           IF IX1 = 1                                                     
102300              MOVE 00      TO W-4436-KVKALTIM                             
102400           ELSE                                                           
102500              IF IX1 = 2                                                  
102600                 MOVE 05      TO W-4436-KVKALTIM                          
102700              ELSE                                                        
102800                 IF IX1 = 3                                               
102900                    MOVE 08      TO W-4436-KVKALTIM                       
103000                 END-IF                                                   
103100              END-IF                                                      
103200           END-IF                                                         
103300           PERFORM IMS-GHU-XXKC-XXKC11                                    
103400           IF SEGMENT-FINNS                                               
103500              PERFORM IMS-DLET-XXKC-XXKC11                                
103600           END-IF                                                         
103700           ADD 1 TO IX1                                                   
103800        END-PERFORM                                                       
103900     END-IF                                                               
104000     MOVE 'END HBD-DLET- '           TO PGMPOS                            
104100     .                                                                    
104200     EJECT                                                                
104300 HBE-DLET-XXKH-MEDLEM SECTION.                                            
104400     MOVE 'STA HBE-DLET- '           TO PGMPOS                            
104500                                                                          
104600     IF MID-FLAGGA-BORTTAG = JA                                           
104700       PERFORM IMS-GHU-XXKH-XXKH11                                        
104800       PERFORM IMS-DLET-XXKH                                              
104900       MOVE MFS-RENSA-FAELT TO MOD-IDPRC-UPDATE                           
105000                               MOD-FLAGGA-UT                              
105200       MOVE INF-UPDATE-DONE TO MED-IDMFSINF                               
105300       CALL WMEDKONV USING MED-WMEDAREA                                   
105400       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
105500     END-IF                                                               
105600     MOVE 'END HBE-DLET- '           TO PGMPOS                            
105700     .                                                                    
105800     EJECT                                                                
105900 HC-AENDRA-XXKH-MEDLEM SECTION.                                           
106000     MOVE 'STA HBE-DLET- '           TO PGMPOS                            
106100                                                                          
106200     IF MID-INPUT = ALL '+' AND                                           
106300       MID-FLAGGA-RAD = ALL '+'                                           
106400       MOVE MFS-ROER-EJ-FAELT TO MOD-IDPRC-UPDATE                         
106500       PERFORM MFS-ROR-EJ-FAELT-IN                                        
106600       PERFORM MFS-ROR-EJ-FAELT-UT                                        
106700       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
106800       CALL WMEDKONV USING MED-WMEDAREA                                   
106900       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
107000     ELSE                                                                 
107100       PERFORM HCA-KOLLA-P-TID                                            
107200     END-IF                                                               
107300     MOVE 'END HC-AENDRA-'           TO PGMPOS                            
107400     .                                                                    
107500     EJECT                                                                
107600 HCA-KOLLA-P-TID SECTION.                                                 
107700     MOVE 'STA HCA-KOLLA '           TO PGMPOS                            
107800                                                                          
107900     IF MID-FLAGGA-RAD = ALL '+' OR MID-FLAGGA-RAD = JA                   
108000       PERFORM HCAA-KOLLA-P-TID-JA                                        
108100     ELSE                                                                 
108200       PERFORM HCAB-KOLLA-P-TID-NEJ                                       
108300     END-IF                                                               
108400     MOVE 'END HCA-KOLLA '           TO PGMPOS                            
108500     .                                                                    
108600     EJECT                                                                
108700 HCAA-KOLLA-P-TID-JA SECTION.                                             
108800     MOVE 'STA HCAA-KOLLA'           TO PGMPOS                            
108900                                                                          
109000     MOVE JA TO LAGOMR-SW                                                 
109100     MOVE +1 TO LO-INDX                                                   
109200     PERFORM UNTIL LO-INDX > MAX-LO-INDX                                  
109300       IF MID-ADLAGOMR-UPDATE(LO-INDX) NOT = ALL '+'                      
109400         MOVE MFS-NUM-FAELT-FEL TO MOD-ADLAGOMR-UPDATE-ATTR               
109500                                                  (LO-INDX)               
109600         MOVE NEJ TO LAGOMR-SW                                            
109700       END-IF                                                             
109800       ADD +1 TO LO-INDX                                                  
109900     END-PERFORM                                                          
110000     IF LAGOMR-OK                                                         
110100       MOVE +1 TO LO-INDX                                                 
110200       PERFORM UNTIL LO-INDX > MAX-LO-INDX                                
110300         IF 4448-ADLAGOMR(LO-INDX) NOT = ZERO                             
110400           MOVE ZERO TO MID-ADLAGOMR-UPDATE(LO-INDX)                      
110500         END-IF                                                           
110600         ADD +1 TO LO-INDX                                                
110700       END-PERFORM                                                        
110800       MOVE 'J' TO MOD-FLAGGA-UT                                          
110900       MOVE 'J' TO MOD-FLAGGA-SPAR                                        
111000       PERFORM S03-KOLLA-PRC-TYP                                          
111100     ELSE                                                                 
111200       MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                          
111300       CALL WMEDKONV USING MED-WMEDAREA                                   
111400       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
111500       MOVE MFS-ROER-EJ-FAELT TO MOD-IDPRC-UPDATE                         
111600       PERFORM MFS-ROR-EJ-FAELT-IN                                        
111700       PERFORM MFS-ROR-EJ-FAELT-UT                                        
111800     END-IF                                                               
111900     MOVE 'END HCAA-KOLLA'           TO PGMPOS                            
112000     .                                                                    
112100     EJECT                                                                
112200 HCAB-KOLLA-P-TID-NEJ SECTION.                                            
112300     MOVE 'STA HCAB-KOLLA'           TO PGMPOS                            
112400                                                                          
112500     MOVE NEJ TO LAGOMR-SW                                                
112600     MOVE +1 TO LO-INDX                                                   
112700     PERFORM UNTIL LO-INDX > MAX-LO-INDX                                  
112800       IF MID-ADLAGOMR-UPDATE(LO-INDX) NOT = ALL '+' OR                   
112900         4448-ADLAGOMR(LO-INDX) NOT = ZERO                                
113000         MOVE JA TO LAGOMR-SW                                             
113100       END-IF                                                             
113200       ADD +1 TO LO-INDX                                                  
113300     END-PERFORM                                                          
113400     IF LAGOMR-FEL                                                        
113500       MOVE +1 TO LO-INDX                                                 
113600       PERFORM UNTIL LO-INDX > MAX-LO-INDX                                
113700         MOVE MFS-ADD-LYS-UPP-FAELT TO                                    
113800                  MOD-ADLAGOMR-UPDATE-ATTR                                
113900                                (LO-INDX)                                 
114000         ADD +1 TO LO-INDX                                                
114100       END-PERFORM                                                        
114200       MOVE ERR-INFO-MISSING TO MED-IDMFSFEL                              
114300       CALL WMEDKONV USING MED-WMEDAREA                                   
114400       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
114500       MOVE MFS-ROER-EJ-FAELT TO MOD-IDPRC-UPDATE                         
114600       PERFORM MFS-ROR-EJ-FAELT-IN                                        
114700       PERFORM MFS-ROR-EJ-FAELT-UT                                        
114800     ELSE                                                                 
114900       MOVE 'N' TO MOD-FLAGGA-UT                                          
115000       MOVE 'N' TO MOD-FLAGGA-SPAR                                        
115100       MOVE +1 TO LO-INDX                                                 
115200       PERFORM UNTIL LO-INDX > MAX-LO-INDX                                
115300         MOVE MFS-FORMATETS-ATTR TO MOD-ADLAGOMR-UPDATE-ATTR              
115400                                                    (LO-INDX)             
115500         ADD +1 TO LO-INDX                                                
115600       END-PERFORM                                                        
115700       PERFORM S03-KOLLA-PRC-TYP                                          
115800     END-IF                                                               
115900     MOVE 'END HCAB-KOLLA'           TO PGMPOS                            
116000     .                                                                    
116100     EJECT                                                                
116200 HD-NYUPPLAEGG-XXKH-MEDLEM SECTION.                                       
116300     MOVE 'STA HD-NYUPPLA'           TO PGMPOS                            
116400                                                                          
116500     IF MID-IDPRC-UPDATE NOT = ALL '+'                                    
116600       IF MID-IDPRC-UPDATE NOT = WS-IDPRC                                 
116700         MOVE MID-IDPRC-UPDATE TO W-4448-IDPRC                            
116800         PERFORM IMS-GHU-XXKH-XXKH11                                      
116900         IF SEGMENT-FINNS                                                 
117000           MOVE 4448-KDPRCGRP TO WS-KDPRCGRP                              
117100           MOVE WS-IDPRC TO 4448-IDPRC                                    
117200           PERFORM IMS-ISRT-XXKH-XXKH11                                   
117300           PERFORM HDA-KOPIERA-PRINTERTABELL                              
117400           PERFORM HDC-SKAPA-ARBETSTIDSMATRIS                             
117500           PERFORM S07-NYPLOCKSATSTABELL                                  
117600           PERFORM S11-STARTA-BMP                                         
117700           MOVE MFS-RENSA-FAELT TO MOD-IDPRC-UPDATE                       
117800           PERFORM MFS-RENSA-FAELT-IN                                     
117900           MOVE WS-IDPRC TO W-4448-IDPRC                                  
118000           PERFORM IMS-GHU-XXKH-XXKH11                                    
118100           PERFORM S01-VISA-PRC-TABELL                                    
118200           MOVE INF-UPDATE-DONE TO MED-IDMFSINF                           
118300           CALL WMEDKONV USING MED-WMEDAREA                               
118400           MOVE MED-MFSINF TO MOD-TEMFSINF                                
118500           PERFORM MFS-FORM-ATTR                                          
118600         ELSE                                                             
118700           MOVE ERR-UPDATE-NOT-ALLOWED TO MED-IDMFSFEL                    
118800           CALL WMEDKONV USING MED-WMEDAREA                               
118900           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
119000           MOVE MFS-ROER-EJ-FAELT TO MOD-IDPRC-UPDATE                     
119100           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDPRC-UPDATE-ATTR            
119200           PERFORM MFS-RENSA-FAELT-IN                                     
119300           PERFORM MFS-ROR-EJ-FAELT-UT                                    
119400         END-IF                                                           
119500       ELSE                                                               
119600         MOVE ERR-UPDATE-NOT-ALLOWED TO MED-IDMFSFEL                      
119700         CALL WMEDKONV USING MED-WMEDAREA                                 
119800         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
119900         MOVE MFS-ROER-EJ-FAELT TO MOD-IDPRC-UPDATE                       
120000         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDPRC-UPDATE-ATTR              
120100         PERFORM MFS-RENSA-FAELT-IN                                       
120200         PERFORM MFS-ROR-EJ-FAELT-UT                                      
120300       END-IF                                                             
120400     ELSE                                                                 
120500       PERFORM IMS-GHU-XXKH-XXKH11-DEF                                    
120600       IF SEGMENT-FINNS                                                   
120700         MOVE 4448-KDPRCGRP TO WS-KDPRCGRP                                
120800         MOVE WS-IDPRC TO 4448-IDPRC                                      
120900         PERFORM IMS-ISRT-XXKH-XXKH11                                     
121000         PERFORM HDB-NYPRINTERTABELL                                      
121100         PERFORM HDC-SKAPA-ARBETSTIDSMATRIS                               
121200         PERFORM S07-NYPLOCKSATSTABELL                                    
121300         PERFORM S11-STARTA-BMP                                           
121400         MOVE MFS-RENSA-FAELT TO MOD-IDPRC-UPDATE                         
121500         PERFORM MFS-RENSA-FAELT-IN                                       
121600         MOVE WS-IDPRC TO W-4448-IDPRC                                    
121700         PERFORM IMS-GHU-XXKH-XXKH11                                      
121800         PERFORM S01-VISA-PRC-TABELL                                      
121900         MOVE INF-GENERAL-TABLE TO MED-IDMFSINF                           
122000         CALL WMEDKONV USING MED-WMEDAREA                                 
122100         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
122200         PERFORM MFS-FORM-ATTR                                            
122300       END-IF                                                             
122400     END-IF                                                               
122500     MOVE 'STA HD-NYUPPLA'           TO PGMPOS                            
122600     .                                                                    
122700     EJECT                                                                
122800 HDA-KOPIERA-PRINTERTABELL SECTION.                                       
122900     MOVE 'STA HDA-KOPIER'           TO PGMPOS                            
123000                                                                          
123100     MOVE '4453' TO 4453-IDHTYP                                           
123200     MOVE W-4453-IDDC TO 4453-IDDC                                        
123300     MOVE WS-IDPRC TO 4453-IDPRC                                          
123400     MOVE LOW-VALUE TO 4453-LOW-VALUE                                     
123500     PERFORM IMS-ISRT-XXKL-XXKL01                                         
123600     MOVE MID-IDPRC-UPDATE TO W-4453-IDPRC                                
123700     PERFORM IMS-GHU-XXKL-XXKL01                                          
123800     PERFORM IMS-GHNP-XXKL-XXKL11                                         
123900     IF SEGMENT-FINNS                                                     
124000       MOVE WS-IDPRC TO W-4453-IDPRC                                      
124100       PERFORM IMS-ISRT-XXKL-XXKL11                                       
124200     END-IF                                                               
124300     MOVE 'END HDA-KOPIER'           TO PGMPOS                            
124400     .                                                                    
124500     EJECT                                                                
124600 HDB-NYPRINTERTABELL SECTION.                                             
124700     MOVE 'STA HDB-NYPRIN'           TO PGMPOS                            
124800                                                                          
124900     MOVE '4453' TO 4453-IDHTYP                                           
125000     MOVE W-4453-IDDC TO 4453-IDDC                                        
125100     MOVE WS-IDPRC TO 4453-IDPRC                                          
125200     MOVE LOW-VALUE TO 4453-LOW-VALUE                                     
125300     PERFORM IMS-ISRT-XXKL-XXKL01                                         
125400     PERFORM IMS-GHU-XXKL-XXKL01-DEF                                      
125500     PERFORM IMS-GHNP-XXKL-XXKL11-DEF                                     
125600     IF SEGMENT-FINNS                                                     
125700       MOVE WS-IDPRC TO W-4453-IDPRC                                      
125800       PERFORM IMS-ISRT-XXKL-XXKL11                                       
125900     END-IF                                                               
126000     MOVE 'END HDB-NYPRIN'           TO PGMPOS                            
126100     .                                                                    
126200     EJECT                                                                
126300 HDC-SKAPA-ARBETSTIDSMATRIS SECTION.                                      
126400     MOVE 'STA HDC-SKAPA '           TO PGMPOS                            
126500                                                                          
126600     PERFORM HDCA-LAES-DEFAULT-MATRIS                                     
126700                                                                          
126800     MOVE 1 TO IX1                                                        
126900     PERFORM UNTIL IX1 > 3                                                
127000        IF TAB-4436-WDGX4436 (IX1) NOT = SPACE                            
127100           MOVE TAB-4436-WDGX4436 (IX1) TO 4436-WDGX4436                  
127200           MOVE WS-IDPRC                TO 4436-IDPRC                     
127300           PERFORM IMS-ISRT-XXKC-XXKC11                                   
127400        END-IF                                                            
127500        ADD 1 TO IX1                                                      
127600     END-PERFORM                                                          
127700     MOVE 'END HDC-SKAPA '           TO PGMPOS                            
127800     .                                                                    
127900     EJECT                                                                
128000 HDCA-LAES-DEFAULT-MATRIS SECTION.                                        
128100     MOVE 'STA HDCA-LAES '           TO PGMPOS                            
128200                                                                          
128300     MOVE SPACE   TO ARB-TID-TABELL                                       
128400     MOVE '9999'  TO W-4436-IDPRC                                         
128500     MOVE 1       TO IX1                                                  
128600                                                                          
128700     PERFORM UNTIL IX1 > 3                                                
128800        IF IX1 = 1                                                        
128900           MOVE 00      TO W-4436-KVKALTIM                                
129000        ELSE                                                              
129100           IF IX1 = 2                                                     
129200              MOVE 05      TO W-4436-KVKALTIM                             
129300           ELSE                                                           
129400              IF IX1 = 3                                                  
129500                 MOVE 08      TO W-4436-KVKALTIM                          
129600              END-IF                                                      
129700           END-IF                                                         
129800        END-IF                                                            
129900                                                                          
130000        PERFORM IMS-GU-XXKC-XXKC11                                        
130100        IF SEGMENT-FINNS                                                  
130200           MOVE 4436-WDGX4436 TO TAB-4436-WDGX4436 (IX1)                  
130300        END-IF                                                            
130400        ADD 1 TO IX1                                                      
130500     END-PERFORM                                                          
130600     MOVE 'END HDCA-LAES '           TO PGMPOS                            
130700     .                                                                    
130800     EJECT                                                                
130900 S01-VISA-PRC-TABELL SECTION.                                             
131000     MOVE 'STA S01-VISA- '           TO PGMPOS                            
131100                                                                          
131200     MOVE +1 TO LO-INDX                                                   
131300     MOVE +1 TO TAB-INDX                                                  
131400     PERFORM UNTIL LO-INDX > MAX-LO-INDX                                  
131500       IF 4448-ADLAGOMR(LO-INDX) NOT = ZERO                               
131600         MOVE 4448-ADLAGOMR(LO-INDX) TO MOD-ADLAGOMR-RAD                  
131700                                          (TAB-INDX)                      
131800         ADD +1 TO LO-INDX                                                
131900         ADD +1 TO TAB-INDX                                               
132000       ELSE                                                               
132100         ADD +1 TO LO-INDX                                                
132200       END-IF                                                             
132300     END-PERFORM                                                          
132400     MOVE 4448-BEPRC TO MOD-BEPRC-RAD                                     
132500     MOVE 4448-IDPRC-HUV TO MOD-IDPRC-HUV-RAD                             
132600     INSPECT MOD-IDPRC-HUV-RAD REPLACING LEADING ZERO BY SPACE            
132700     MOVE +1 TO SUB-INDX                                                  
132800     MOVE +1 TO TAB-INDX                                                  
132900     PERFORM UNTIL SUB-INDX > MAX-SUB-INDX                                
133000       IF 4448-IDPRC-SUB(SUB-INDX) NOT = SPACE                            
133100         MOVE 4448-IDPRC-SUB(SUB-INDX) TO MOD-IDPRC-SUB-RAD               
133200                                                   (TAB-INDX)             
133300         ADD +1 TO SUB-INDX                                               
133400         ADD +1 TO TAB-INDX                                               
133500       ELSE                                                               
133600         ADD +1 TO SUB-INDX                                               
133700       END-IF                                                             
133800     END-PERFORM                                                          
133900                                                                          
134020     IF ENGLISH-TEXT                                                      
134021       IF 4448-FLSTJORD = JA                                              
134022         MOVE YES           TO MOD-FLSTJORD-UT                            
134023       ELSE                                                               
134024         MOVE 4448-FLSTJORD TO MOD-FLSTJORD-UT                            
134025       END-IF                                                             
134026     ELSE                                                                 
134027       MOVE 4448-FLSTJORD   TO MOD-FLSTJORD-UT                            
134028     END-IF                                                               
134030                                                                          
134100     MOVE 4448-RESPLIT  TO MOD-RESPLIT-RAD                                
134200     MOVE 4448-KDPRCGRP TO MOD-KDPRCGRP-RAD                               
134300     INSPECT MOD-KDPRCGRP-RAD REPLACING LEADING ZERO BY SPACE             
134400     MOVE 4448-KDPRCTYP TO MOD-KDPRCTYP-RAD                               
134500     MOVE 4448-KDPRODKL TO MOD-KDPRODKL-RAD                               
134600     MOVE 4448-KVORDER  TO MOD-KVORDER-RAD                                
134700     MOVE 4448-KVPLSRAD TO MOD-KVPLSRAD-RAD                               
134800     MOVE 4448-KVRADER TO MOD-KVRADER-RAD                                 
134900                                                                          
135000     MOVE 4448-KVVTID TO HELP-KVVTID-HHMM                                 
135100     MOVE HELP-KVVTID-TIM       TO HELP-KVVTID-TIM-RED                    
135200     MOVE HELP-KVVTID-MIN       TO HELP-KVVTID-MIN-RED                    
135300     MOVE HELP-KVVTID-REDIGERAD TO MOD-KVVTID-RAD                         
135400                                                                          
135500     MOVE 4448-VKPLSNTO TO MOD-VKPLSNTO-RAD                               
135600     MOVE 4448-VKORDNTO TO MOD-VKORDNTO-RAD                               
135700     MOVE 4448-VLKOLGR TO MOD-VLKOLGR-RAD                                 
135800     MOVE 4448-VLPLSNTO TO MOD-VLPLSNTO-RAD                               
135900     MOVE 4448-VLORDNTO TO MOD-VLORDNTO-RAD                               
136000                                                                          
136100     MOVE NEJ TO MOD-FLAGGA-BORTTAG                                       
136200                                                                          
136300     MOVE JA TO FLAGGA-SW                                                 
136400     MOVE +1 TO LO-INDX                                                   
136500     PERFORM UNTIL LO-INDX > MAX-LO-INDX                                  
136600       IF 4448-ADLAGOMR(LO-INDX) NOT = ZERO OR                            
136700         (MID-FLAGGA-RAD = NEJ AND MID-ADLAGOMR-UPDATE(LO-INDX)           
136800         NOT = ALL '+')                                                   
136900         MOVE NEJ TO FLAGGA-SW                                            
137000       END-IF                                                             
137100       ADD +1 TO LO-INDX                                                  
137200     END-PERFORM                                                          
137300     IF FLAGGA-FEL                                                        
137400       MOVE 'N' TO MOD-FLAGGA-UT                                          
137500       MOVE 'N' TO MOD-FLAGGA-SPAR                                        
137600     ELSE                                                                 
137700       MOVE 'J' TO MOD-FLAGGA-UT                                          
137800       MOVE 'J' TO MOD-FLAGGA-SPAR                                        
137900     END-IF                                                               
138000     MOVE MFS-RENSA-FAELT TO MOD-IDPRC-UPDATE                             
138100     PERFORM MFS-RENSA-FAELT-IN                                           
138200     MOVE 'END S01-VISA- '           TO PGMPOS                            
138300     .                                                                    
138400     EJECT                                                                
138500 S03-KOLLA-PRC-TYP SECTION.                                               
138600     MOVE 'STA S03-KOLLA '           TO PGMPOS                            
138700                                                                          
138800     IF MID-KDPRCTYP-UPDATE NOT = ALL '+'                                 
138900       IF MID-KDPRCTYP-UPDATE = '1'                                       
139000         PERFORM S03A-KOLLA-VANLIG                                        
139100       END-IF                                                             
139200       IF MID-KDPRCTYP-UPDATE = '3'                                       
139300         PERFORM S03B-KOLLA-PICKUP                                        
139400       END-IF                                                             
139500       IF MID-KDPRCTYP-UPDATE = '2'                                       
139600         PERFORM S03C-KOLLA-HUVUD                                         
139700       END-IF                                                             
139800     ELSE                                                                 
139900       IF 4448-KDPRCTYP = '1'                                             
140000         PERFORM S03A-KOLLA-VANLIG                                        
140100       END-IF                                                             
140200       IF 4448-KDPRCTYP = '3'                                             
140300         PERFORM S03B-KOLLA-PICKUP                                        
140400       END-IF                                                             
140500       IF 4448-KDPRCTYP = '2'                                             
140600         PERFORM S03C-KOLLA-HUVUD                                         
140700       END-IF                                                             
140800     END-IF                                                               
140900     MOVE 'END S03-KOLLA '           TO PGMPOS                            
141000     .                                                                    
141100     EJECT                                                                
141200 S03A-KOLLA-VANLIG SECTION.                                               
141300     MOVE 'STA S03A-KOLLA'           TO PGMPOS                            
141400                                                                          
141500     MOVE JA TO INDATA-SW                                                 
141600     IF MID-IDPRC-HUV-UPDATE NOT = ALL '+'                                
141700       MOVE MFS-NUM-FAELT-FEL TO MOD-IDPRC-HUV-UPDATE-ATTR                
141800       MOVE NEJ TO INDATA-SW                                              
141900     END-IF                                                               
142000     MOVE +1 TO SUB-INDX                                                  
142100     PERFORM UNTIL SUB-INDX > MAX-SUB-INDX                                
142200       IF MID-IDPRC-SUB-UPDATE(SUB-INDX) NOT = ALL '+'                    
142300         MOVE MFS-NUM-FAELT-FEL TO MOD-IDPRC-SUB-UPDATE-ATTR              
142400                                                  (SUB-INDX)              
142500         MOVE NEJ TO INDATA-SW                                            
142600       END-IF                                                             
142700       ADD +1 TO SUB-INDX                                                 
142800     END-PERFORM                                                          
142900     IF INDATA-FEL                                                        
143000       MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                          
143100       CALL WMEDKONV USING MED-WMEDAREA                                   
143200       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
143300       MOVE MFS-ROER-EJ-FAELT TO MOD-IDPRC-UPDATE                         
143400       PERFORM MFS-ROR-EJ-FAELT-IN                                        
143500       PERFORM MFS-ROR-EJ-FAELT-UT                                        
143600     ELSE                                                                 
143700       IF 4448-IDPRC-HUV NOT = SPACE                                      
143800         MOVE SPACE TO MID-IDPRC-HUV-UPDATE                               
143900       END-IF                                                             
144000       MOVE +1 TO SUB-INDX                                                
144100       PERFORM UNTIL SUB-INDX > MAX-SUB-INDX                              
144200         IF 4448-IDPRC-SUB(SUB-INDX) NOT = SPACE                          
144300           MOVE SPACE TO MID-IDPRC-SUB-UPDATE(SUB-INDX)                   
144400         END-IF                                                           
144500         ADD +1 TO SUB-INDX                                               
144600       END-PERFORM                                                        
144700       PERFORM S05-AENDRA                                                 
144800     END-IF                                                               
144900     MOVE 'END S03A-KOLLA'           TO PGMPOS                            
145000     .                                                                    
145100     EJECT                                                                
145200 S03B-KOLLA-PICKUP SECTION.                                               
145300     MOVE 'STA S03B-KOLLA'           TO PGMPOS                            
145400                                                                          
145500     MOVE JA TO INDATA-SW                                                 
145600     MOVE JA TO HUVPRC-SW                                                 
145700     IF MID-IDPRC-HUV-UPDATE = ALL '+' AND                                
145800       4448-IDPRC-HUV = SPACE                                             
145900       MOVE MFS-NUM-FAELT-FEL TO MOD-IDPRC-HUV-UPDATE-ATTR                
146000       MOVE NEJ TO HUVPRC-SW                                              
146100     END-IF                                                               
146200     MOVE +1 TO SUB-INDX                                                  
146300     PERFORM UNTIL SUB-INDX > MAX-SUB-INDX                                
146400       IF MID-IDPRC-SUB-UPDATE(SUB-INDX) NOT = ALL '+'                    
146500         MOVE MFS-NUM-FAELT-FEL TO MOD-IDPRC-SUB-UPDATE-ATTR              
146600                                                  (SUB-INDX)              
146700         MOVE NEJ TO INDATA-SW                                            
146800       END-IF                                                             
146900       ADD +1 TO SUB-INDX                                                 
147000     END-PERFORM                                                          
147100     IF HUVPRC-FEL OR INDATA-FEL                                          
147200       IF HUVPRC-FEL                                                      
147300         MOVE ERR-INFO-MISSING TO MED-IDMFSFEL                            
147400         CALL WMEDKONV USING MED-WMEDAREA                                 
147500         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
147600       ELSE                                                               
147700         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                        
147800         CALL WMEDKONV USING MED-WMEDAREA                                 
147900         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
148000       END-IF                                                             
148100       MOVE MFS-ROER-EJ-FAELT TO MOD-IDPRC-UPDATE                         
148200       PERFORM MFS-ROR-EJ-FAELT-IN                                        
148300       PERFORM MFS-ROR-EJ-FAELT-UT                                        
148400     ELSE                                                                 
148500       MOVE +1 TO SUB-INDX                                                
148600       PERFORM UNTIL SUB-INDX > MAX-SUB-INDX                              
148700         IF 4448-IDPRC-SUB(SUB-INDX) NOT = SPACE                          
148800           MOVE SPACE TO MID-IDPRC-SUB-UPDATE(SUB-INDX)                   
148900         END-IF                                                           
149000         ADD +1 TO SUB-INDX                                               
149100       END-PERFORM                                                        
149200       PERFORM S05-AENDRA                                                 
149300     END-IF                                                               
149400     MOVE 'END S03B-KOLLA'           TO PGMPOS                            
149500     .                                                                    
149600     EJECT                                                                
149700 S03C-KOLLA-HUVUD SECTION.                                                
149800     MOVE 'STA S03C-KOLLA'           TO PGMPOS                            
149900                                                                          
150000     MOVE JA TO INDATA-SW                                                 
150100     MOVE NEJ TO UPDATE-SW                                                
150200     IF MID-IDPRC-HUV-UPDATE NOT = ALL '+'                                
150300       MOVE MFS-NUM-FAELT-FEL TO MOD-IDPRC-HUV-UPDATE-ATTR                
150400       MOVE NEJ TO INDATA-SW                                              
150500     END-IF                                                               
150600     MOVE +1 TO SUB-INDX                                                  
150700     PERFORM UNTIL SUB-INDX > MAX-SUB-INDX                                
150800       IF MID-IDPRC-SUB-UPDATE(SUB-INDX) NOT = ALL '+' OR                 
150900         4448-IDPRC-SUB(SUB-INDX) NOT = SPACE                             
151000         MOVE JA TO UPDATE-SW                                             
151100       END-IF                                                             
151200       ADD +1 TO SUB-INDX                                                 
151300     END-PERFORM                                                          
151400     IF UPDATE-FEL                                                        
151500       MOVE +1 TO SUB-INDX                                                
151600       PERFORM UNTIL SUB-INDX > MAX-SUB-INDX                              
151700       IF MID-IDPRC-SUB-UPDATE(SUB-INDX) = ALL '+'                        
151800         MOVE MFS-NUM-FAELT-FEL TO MOD-IDPRC-SUB-UPDATE-ATTR              
151900                                                   (SUB-INDX)             
152000       END-IF                                                             
152100       ADD +1 TO SUB-INDX                                                 
152200       END-PERFORM                                                        
152300     END-IF                                                               
152400     IF INDATA-FEL OR UPDATE-FEL                                          
152500       IF UPDATE-FEL                                                      
152600         MOVE ERR-INFO-MISSING TO MED-IDMFSFEL                            
152700         CALL WMEDKONV USING MED-WMEDAREA                                 
152800         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
152900       ELSE                                                               
153000         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                        
153100         CALL WMEDKONV USING MED-WMEDAREA                                 
153200         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
153300       END-IF                                                             
153400       MOVE MFS-ROER-EJ-FAELT TO MOD-IDPRC-UPDATE                         
153500       PERFORM MFS-ROR-EJ-FAELT-IN                                        
153600       PERFORM MFS-ROR-EJ-FAELT-UT                                        
153700     ELSE                                                                 
153800       IF 4448-IDPRC-HUV NOT = SPACE                                      
153900         MOVE SPACE TO MID-IDPRC-HUV-UPDATE                               
154000       END-IF                                                             
154100       PERFORM S05-AENDRA                                                 
154200     END-IF                                                               
154300     MOVE 'STA S03C-KOLLA'           TO PGMPOS                            
154400     .                                                                    
154500     EJECT                                                                
154600 S05-AENDRA SECTION.                                                      
154700     MOVE 'STA S05-AENDRA'           TO PGMPOS                            
154800                                                                          
154900     MOVE NEJ TO PLOCKSATS-SW                                             
155000     PERFORM IMS-GHU-XXKH-XXKH11                                          
155100     IF SEGMENT-FINNS                                                     
155200       IF MID-RESPLIT-UPDATE NOT = ALL '+'                                
155300         MOVE WS-RESPLIT TO 4448-RESPLIT                                  
155400         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-RESPLIT-ATTR                   
155500       ELSE                                                               
155600         MOVE MFS-ROER-EJ-FAELT TO MOD-RESPLIT-RAD                        
155700       END-IF                                                             
155800                                                                          
155900       IF MID-KDPRCTYP-UPDATE NOT = ALL '+'                               
156000         MOVE MID-KDPRCTYP-UPDATE TO 4448-KDPRCTYP                        
156100         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KDPRCTYP-ATTR                  
156200       ELSE                                                               
156300         MOVE MFS-ROER-EJ-FAELT TO MOD-KDPRCTYP-RAD                       
156400       END-IF                                                             
156500                                                                          
156600       IF MID-KVVTID-UPDATE NOT = ALL '+'                                 
156700         MOVE WS-KVVTID TO 4448-KVVTID                                    
156800         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KVVTID-ATTR                    
156900       ELSE                                                               
157000         MOVE MFS-ROER-EJ-FAELT TO MOD-KVVTID-RAD                         
157100       END-IF                                                             
157200                                                                          
157300       IF MID-IDPRC-HUV-UPDATE NOT = ALL '+'                              
157400         MOVE MID-IDPRC-HUV-UPDATE TO 4448-IDPRC-HUV                      
157500         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDPRC-HUV-ATTR                 
157600       ELSE                                                               
157700         MOVE MFS-ROER-EJ-FAELT TO MOD-IDPRC-HUV-RAD                      
157800       END-IF                                                             
157900                                                                          
158000       IF MID-VLKOLGR-UPDATE NOT = ALL '+'                                
158100         MOVE WS-VLKOLGR TO 4448-VLKOLGR                                  
158200         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-VLKOLGR-ATTR                   
158300       ELSE                                                               
158400         MOVE MFS-ROER-EJ-FAELT TO MOD-VLKOLGR-RAD                        
158500       END-IF                                                             
158600                                                                          
158700       IF MID-BEPRC-RAD NOT = ALL '+'                                     
158800         MOVE MID-BEPRC-RAD TO 4448-BEPRC                                 
158900         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-BEPRC-ATTR                     
159000       END-IF                                                             
159100                                                                          
159200       IF MID-KDPRCGRP-UPDATE NOT = ALL '+'                               
159300         MOVE MID-KDPRCGRP-UPDATE TO 4448-KDPRCGRP                        
159400                                     WS-KDPRCGRP                          
159500         MOVE JA TO PLOCKSATS-SW                                          
159600         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KDPRCGRP-ATTR                  
159700       ELSE                                                               
159800         MOVE MFS-ROER-EJ-FAELT TO MOD-KDPRCGRP-RAD                       
159900       END-IF                                                             
160000                                                                          
160100       IF MID-KDPRODKL-UPDATE NOT = ALL '+'                               
160200         MOVE MID-KDPRODKL-UPDATE TO 4448-KDPRODKL                        
160300         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KDPRODKL-ATTR                  
160400       ELSE                                                               
160500         MOVE MFS-ROER-EJ-FAELT TO MOD-KDPRODKL-RAD                       
160600       END-IF                                                             
160700                                                                          
160800       IF MID-FLSTJORD-UPDATE NOT = ALL '+'                               
161010         IF MID-FLSTJORD-UPDATE = YES                                     
161030           MOVE JA                  TO 4448-FLSTJORD                      
161040         ELSE                                                             
161041           MOVE MID-FLSTJORD-UPDATE TO 4448-FLSTJORD                      
161060         END-IF                                                           
161070         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-FLSTJORD-UPDATE-ATTR           
161100       ELSE                                                               
161200         MOVE MFS-ROER-EJ-FAELT     TO MOD-FLSTJORD-UT                    
161300       END-IF                                                             
161400                                                                          
161500       IF MID-KVORDER-UPDATE NOT = ALL '+'                                
161600         MOVE MID-KVORDER-UPDATE TO 4448-KVORDER                          
161700         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KVORDER-ATTR                   
161800       ELSE                                                               
161900         MOVE MFS-ROER-EJ-FAELT TO MOD-KVORDER-RAD                        
162000       END-IF                                                             
162100                                                                          
162200       IF MID-KVPLSRAD-UPDATE NOT = ALL '+'                               
162300         MOVE MID-KVPLSRAD-UPDATE TO 4448-KVPLSRAD                        
162400         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KVPLSRAD-ATTR                  
162500       ELSE                                                               
162600         MOVE MFS-ROER-EJ-FAELT TO MOD-KVPLSRAD-RAD                       
162700       END-IF                                                             
162800                                                                          
162900       IF MID-KVRADER-UPDATE NOT = ALL '+'                                
163000         MOVE MID-KVRADER-UPDATE TO 4448-KVRADER                          
163100         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KVRADER-ATTR                   
163200       ELSE                                                               
163300         MOVE MFS-ROER-EJ-FAELT TO MOD-KVRADER-RAD                        
163400       END-IF                                                             
163500                                                                          
163600       IF MID-VKPLSNTO-UPDATE NOT = ALL '+'                               
163700         MOVE WS-VKPLSNTO TO 4448-VKPLSNTO                                
163800         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-VKPLSNTO-ATTR                  
163900       ELSE                                                               
164000         MOVE MFS-ROER-EJ-FAELT TO MOD-VKPLSNTO-RAD                       
164100       END-IF                                                             
164200                                                                          
164300       IF MID-VKORDNTO-UPDATE NOT = ALL '+'                               
164400         MOVE WS-VKORDNTO TO 4448-VKORDNTO                                
164500         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-VKORDNTO-ATTR                  
164600       ELSE                                                               
164700         MOVE MFS-ROER-EJ-FAELT TO MOD-VKORDNTO-RAD                       
164800       END-IF                                                             
164900                                                                          
165000       IF MID-VLPLSNTO-UPDATE NOT = ALL '+'                               
165100         MOVE WS-VLPLSNTO TO 4448-VLPLSNTO                                
165200         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-VLPLSNTO-ATTR                  
165300       ELSE                                                               
165400         MOVE MFS-ROER-EJ-FAELT TO MOD-VLPLSNTO-RAD                       
165500       END-IF                                                             
165600                                                                          
165700       IF MID-VLORDNTO-UPDATE NOT = ALL '+'                               
165800         MOVE WS-VLORDNTO TO 4448-VLORDNTO                                
165900         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-VLORDNTO-ATTR                  
166000       ELSE                                                               
166100         MOVE MFS-ROER-EJ-FAELT TO MOD-VLORDNTO-RAD                       
166200       END-IF                                                             
166300                                                                          
166400       MOVE +1 TO LO-INDX                                                 
166500       MOVE NEJ TO S05-LO-SW                                              
166600       PERFORM UNTIL LO-INDX > MAX-LO-INDX                                
166700         IF MID-ADLAGOMR-UPDATE(LO-INDX) NOT = ALL '+'                    
166800           MOVE JA TO S05-LO-SW                                           
166900         END-IF                                                           
167000         ADD +1 TO LO-INDX                                                
167100       END-PERFORM                                                        
167200                                                                          
167300       IF S05-LO-OK                                                       
167400         PERFORM S05A-AENDRA-LO                                           
167500       END-IF                                                             
167600       MOVE +1 TO SUB-INDX                                                
167700       MOVE NEJ TO S05-PRC-SW                                             
167800       PERFORM UNTIL SUB-INDX > MAX-SUB-INDX                              
167900         IF MID-IDPRC-SUB-UPDATE(SUB-INDX) NOT = ALL '+'                  
168000           MOVE JA TO S05-PRC-SW                                          
168100         END-IF                                                           
168200         ADD +1 TO SUB-INDX                                               
168300       END-PERFORM                                                        
168400                                                                          
168500       IF S05-PRC-OK                                                      
168600         PERFORM S05B-AENDRA-PRC                                          
168700       END-IF                                                             
168800       PERFORM IMS-REPL-XXKH                                              
168900       IF PLOCKSATS-OK                                                    
169000         PERFORM S07-NYPLOCKSATSTABELL                                    
169100       END-IF                                                             
169200       PERFORM IMS-GHU-XXKH-XXKH11                                        
169300       PERFORM S01-VISA-PRC-TABELL                                        
169400       PERFORM MFS-FORM-ATTR                                              
169500       MOVE MFS-RENSA-FAELT TO MOD-IDPRC-UPDATE                           
169600       PERFORM MFS-RENSA-FAELT-IN                                         
169700       MOVE INF-UPDATE-DONE TO MED-IDMFSINF                               
169800       CALL WMEDKONV USING MED-WMEDAREA                                   
169900       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
170000     END-IF                                                               
170100     MOVE 'END S05-AENDRA'           TO PGMPOS                            
170200     .                                                                    
170300     EJECT                                                                
170400 S05A-AENDRA-LO SECTION.                                                  
170500     MOVE 'STA S05A-AENDR'           TO PGMPOS                            
170600                                                                          
170700     MOVE +1 TO TAB-INDX                                                  
170800     PERFORM UNTIL TAB-INDX > MAX-TAB-INDX-PLUS1                          
170900       MOVE ZERO TO TAB-ADLAGOMR(TAB-INDX)                                
171000       ADD +1 TO TAB-INDX                                                 
171100     END-PERFORM                                                          
171200                                                                          
171300       MOVE +1 TO LO-INDX                                                 
171400       MOVE +1 TO TAB-INDX                                                
171500       PERFORM UNTIL LO-INDX > MAX-LO-INDX                                
171600         IF MID-ADLAGOMR-UPDATE(LO-INDX) NOT = ALL '+'                    
171700           IF MID-ADLAGOMR-UPDATE(LO-INDX) NOT = ZERO                     
171800            MOVE MID-ADLAGOMR-UPDATE(LO-INDX) TO                          
171900                            TAB-ADLAGOMR(TAB-INDX)                        
172000            ADD +1 TO LO-INDX                                             
172100            ADD +1 TO TAB-INDX                                            
172200           ELSE                                                           
172300            MOVE ZERO TO TAB-ADLAGOMR(TAB-INDX)                           
172400            ADD +1 TO LO-INDX                                             
172500           END-IF                                                         
172600         ELSE                                                             
172700           IF 4448-ADLAGOMR(LO-INDX) NOT = ZERO                           
172800             MOVE 4448-ADLAGOMR(LO-INDX) TO                               
172900                               TAB-ADLAGOMR(TAB-INDX)                     
173000             ADD +1 TO LO-INDX                                            
173100             ADD +1 TO TAB-INDX                                           
173200           ELSE                                                           
173300             ADD +1 TO LO-INDX                                            
173400           END-IF                                                         
173500         END-IF                                                           
173600       END-PERFORM                                                        
173700     MOVE +1 TO TAB-INDX                                                  
173800     PERFORM UNTIL TAB-INDX > MAX-TAB-INDX                                
173900       MOVE TAB-ADLAGOMR(TAB-INDX) TO 4448-ADLAGOMR                       
174000                                              (TAB-INDX)                  
174100       ADD +1 TO TAB-INDX                                                 
174200     END-PERFORM                                                          
174300     MOVE +1 TO TAB-INDX                                                  
174400     PERFORM UNTIL TAB-ADLAGOMR(TAB-INDX) = ZERO                          
174500       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-ADLAGOMR-RAD-ATTR                
174600                                            (TAB-INDX)                    
174700       ADD +1 TO TAB-INDX                                                 
174800     END-PERFORM                                                          
174900     MOVE 'END S05A-AENDR'           TO PGMPOS                            
175000     .                                                                    
175100     EJECT                                                                
175200 S05B-AENDRA-PRC SECTION.                                                 
175300     MOVE 'STA S05B-AENDR'           TO PGMPOS                            
175400                                                                          
175500     MOVE +1 TO TAB-INDX                                                  
175600     PERFORM UNTIL TAB-INDX > MAX-TAB-INDX-PLUS1                          
175700       MOVE SPACE  TO TAB-IDPRC-SUB(TAB-INDX)                             
175800       ADD +1 TO TAB-INDX                                                 
175900     END-PERFORM                                                          
176000     MOVE +1 TO SUB-INDX                                                  
176100     MOVE +1 TO TAB-INDX                                                  
176200     PERFORM UNTIL SUB-INDX > MAX-SUB-INDX                                
176300       IF MID-IDPRC-SUB-UPDATE(SUB-INDX) NOT = ALL '+'                    
176400        IF MID-IDPRC-SUB-UPDATE(SUB-INDX) NOT = '0000'                    
176500         MOVE MID-IDPRC-SUB-UPDATE(SUB-INDX) TO                           
176600                         TAB-IDPRC-SUB(TAB-INDX)                          
176700         ADD +1 TO SUB-INDX                                               
176800         ADD +1 TO TAB-INDX                                               
176900        ELSE                                                              
177000         MOVE SPACE TO TAB-IDPRC-SUB(TAB-INDX)                            
177100         ADD +1 TO SUB-INDX                                               
177200        END-IF                                                            
177300       ELSE                                                               
177400         IF 4448-IDPRC-SUB(SUB-INDX) NOT = SPACE                          
177500           MOVE 4448-IDPRC-SUB(SUB-INDX) TO                               
177600                             TAB-IDPRC-SUB(TAB-INDX)                      
177700           ADD +1 TO SUB-INDX                                             
177800           ADD +1 TO TAB-INDX                                             
177900         ELSE                                                             
178000           ADD +1 TO SUB-INDX                                             
178100         END-IF                                                           
178200       END-IF                                                             
178300     END-PERFORM                                                          
178400     MOVE +1 TO TAB-INDX                                                  
178500     PERFORM UNTIL TAB-INDX > MAX-TAB-INDX                                
178600       MOVE TAB-IDPRC-SUB(TAB-INDX) TO 4448-IDPRC-SUB                     
178700                                              (TAB-INDX)                  
178800       ADD +1 TO TAB-INDX                                                 
178900     END-PERFORM                                                          
179000     MOVE +1 TO TAB-INDX                                                  
179100     PERFORM UNTIL TAB-IDPRC-SUB(TAB-INDX) = SPACE                        
179200       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDPRC-SUB-RAD-ATTR               
179300                                            (TAB-INDX)                    
179400       ADD +1 TO TAB-INDX                                                 
179500     END-PERFORM                                                          
179600     MOVE 'END S05B-AENDR'           TO PGMPOS                            
179700     .                                                                    
179800     EJECT                                                                
179900 S07-NYPLOCKSATSTABELL SECTION.                                           
180000     MOVE 'STA S07-NYPLOC'           TO PGMPOS                            
180100                                                                          
180200     MOVE '4461' TO 4461-IDHTYP                                           
180300     MOVE W-4461-IDDC TO 4461-IDDC                                        
180400     MOVE LOW-VALUE TO 4461-LOW-VALUE                                     
180500     PERFORM IMS-GHU-XXKO-XXKO01                                          
180600     IF SEGMENT-SAKNAS                                                    
180700       PERFORM IMS-ISRT-XXKO-XXKO01                                       
180800       PERFORM IMS-GHU-XXKO-XXKO01                                        
180900     END-IF                                                               
181000     MOVE WS-KDPRCGRP TO 4462-KDPRCGRP                                    
181100     MOVE ZERO TO 4462-IDLOPNR-PL                                         
181200     MOVE WS-LOCAL-DATE TO DAGENS-DATUM                                   
181300     MOVE DAGENS-DATUM TO 4462-TIDATUM                                    
181400     PERFORM IMS-ISRT-XXKO-XXKO11                                         
181500     MOVE 'END S07-NYPLOC'           TO PGMPOS                            
181600     .                                                                    
181700     EJECT                                                                
181800 S11-STARTA-BMP SECTION.                                                  
181900     MOVE 'STA S11-STARTA'           TO PGMPOS                            
182000                                                                          
182100     STRING 'IDPRC(' WS-IDPRC ') IDDC(' WS-IDDC ')         TIUPDAT        
182200-           'E(' WS-START-DATUM ') KDCALL(' WS-KDCALL ')'                 
182300          DELIMITED BY SIZE INTO MSGSOP-TESYMBV                           
182400     PERFORM IMS-INSERT-ALTMSG                                            
182500     MOVE 'END S11-STARTA'           TO PGMPOS                            
182600     .                                                                    
182700     EJECT                                                                
182800 MFS-RENSA-FAELT-UT SECTION.                                              
182900                                                                          
183000     MOVE MFS-RENSA-FAELT TO MOD-FLAGGA-UT                                
183100                             MOD-FLSTJORD-UT                              
183200                             MOD-RESPLIT-RAD                              
183300                             MOD-KDPRCTYP-RAD                             
183400                             MOD-KVVTID-RAD                               
183500                             MOD-IDPRC-HUV-RAD                            
183600                             MOD-VLKOLGR-RAD                              
183700                             MOD-BEPRC-RAD                                
183800                             MOD-KDPRCGRP-RAD                             
183900                             MOD-KDPRODKL-RAD                             
184000                             MOD-KVORDER-RAD                              
184100                             MOD-KVPLSRAD-RAD                             
184200                             MOD-KVRADER-RAD                              
184300                             MOD-VKPLSNTO-RAD                             
184400                             MOD-VKORDNTO-RAD                             
184500                             MOD-VLPLSNTO-RAD                             
184600                             MOD-VLORDNTO-RAD                             
184700     MOVE +1 TO LO-INDX                                                   
184800     PERFORM UNTIL LO-INDX > MAX-LO-INDX                                  
184900       MOVE MFS-RENSA-FAELT TO MOD-ADLAGOMR-RAD(LO-INDX)                  
185000       ADD +1 TO LO-INDX                                                  
185100     END-PERFORM                                                          
185200     MOVE +1 TO SUB-INDX                                                  
185300     PERFORM UNTIL SUB-INDX > MAX-SUB-INDX                                
185400       MOVE MFS-RENSA-FAELT TO MOD-IDPRC-SUB-RAD(SUB-INDX)                
185500       ADD +1 TO SUB-INDX                                                 
185600     END-PERFORM                                                          
185700     .                                                                    
185800     EJECT                                                                
185900 MFS-RENSA-FAELT-IN SECTION.                                              
186000                                                                          
186100     MOVE MFS-RENSA-FAELT TO MOD-RESPLIT-UPDATE                           
186300                             MOD-KDPRCTYP-UPDATE                          
186400                             MOD-KVVTID-UPDATE                            
186500                             MOD-IDPRC-HUV-UPDATE                         
186600                             MOD-VLKOLGR-UPDATE                           
186700                             MOD-KDPRCGRP-UPDATE                          
186800                             MOD-KDPRODKL-UPDATE                          
186900                             MOD-KVORDER-UPDATE                           
187000                             MOD-KVPLSRAD-UPDATE                          
187100                             MOD-KVRADER-UPDATE                           
187200                             MOD-VKPLSNTO-UPDATE                          
187300                             MOD-VKORDNTO-UPDATE                          
187400                             MOD-VLPLSNTO-UPDATE                          
187500                             MOD-VLORDNTO-UPDATE                          
187600     MOVE +1 TO LO-INDX                                                   
187700     PERFORM UNTIL LO-INDX > MAX-LO-INDX                                  
187800       MOVE MFS-RENSA-FAELT TO MOD-ADLAGOMR-UPDATE(LO-INDX)               
187900       ADD +1 TO LO-INDX                                                  
188000     END-PERFORM                                                          
188100     MOVE +1 TO SUB-INDX                                                  
188200     PERFORM UNTIL SUB-INDX > MAX-SUB-INDX                                
188300       MOVE MFS-RENSA-FAELT TO MOD-IDPRC-SUB-UPDATE(SUB-INDX)             
188400       ADD +1 TO SUB-INDX                                                 
188500     END-PERFORM                                                          
188600     .                                                                    
188700     EJECT                                                                
188800 MFS-ROR-EJ-FAELT-IN  SECTION.                                            
188900                                                                          
189000     MOVE MFS-ROER-EJ-FAELT TO MOD-FLAGGA-UT                              
189200                               MOD-FLSTJORD-UPDATE                        
189210                               MOD-RESPLIT-UPDATE                         
189300                               MOD-KDPRCTYP-UPDATE                        
189400                               MOD-KVVTID-UPDATE                          
189500                               MOD-IDPRC-HUV-UPDATE                       
189600                               MOD-VLKOLGR-UPDATE                         
189700                               MOD-BEPRC-RAD                              
189800                               MOD-KDPRCGRP-UPDATE                        
189900                               MOD-KDPRODKL-UPDATE                        
190000                               MOD-KVORDER-UPDATE                         
190100                               MOD-KVPLSRAD-UPDATE                        
190200                               MOD-KVRADER-UPDATE                         
190300                               MOD-VKPLSNTO-UPDATE                        
190400                               MOD-VKORDNTO-UPDATE                        
190500                               MOD-VLPLSNTO-UPDATE                        
190600                               MOD-VLORDNTO-UPDATE                        
190700     MOVE +1 TO LO-INDX                                                   
190800     PERFORM UNTIL LO-INDX > MAX-LO-INDX                                  
190900       MOVE MFS-ROER-EJ-FAELT TO MOD-ADLAGOMR-UPDATE(LO-INDX)             
191000       ADD +1 TO LO-INDX                                                  
191100     END-PERFORM                                                          
191200     MOVE +1 TO SUB-INDX                                                  
191300     PERFORM UNTIL SUB-INDX > MAX-SUB-INDX                                
191400       MOVE MFS-ROER-EJ-FAELT TO MOD-IDPRC-SUB-UPDATE(SUB-INDX)           
191500       ADD +1 TO SUB-INDX                                                 
191600     END-PERFORM                                                          
191700     .                                                                    
191800     EJECT                                                                
191900     SKIP2                                                                
192000 MFS-ROR-EJ-FAELT-UT  SECTION.                                            
192100                                                                          
192200     MOVE MFS-ROER-EJ-FAELT TO MOD-FLAGGA-UT                              
192300                               MOD-FLSTJORD-UT                            
192400                               MOD-RESPLIT-RAD                            
192500                               MOD-KDPRCTYP-RAD                           
192600                               MOD-KVVTID-RAD                             
192700                               MOD-IDPRC-HUV-RAD                          
192800                               MOD-VLKOLGR-RAD                            
192900                               MOD-BEPRC-RAD                              
193000                               MOD-KDPRCGRP-RAD                           
193100                               MOD-KDPRODKL-RAD                           
193200                               MOD-KVORDER-RAD                            
193300                               MOD-KVPLSRAD-RAD                           
193400                               MOD-KVRADER-RAD                            
193500                               MOD-VKPLSNTO-RAD                           
193600                               MOD-VKORDNTO-RAD                           
193700                               MOD-VLPLSNTO-RAD                           
193800                               MOD-VLORDNTO-RAD                           
193900     MOVE +1 TO LO-INDX                                                   
194000     PERFORM UNTIL LO-INDX > MAX-LO-INDX                                  
194100       MOVE MFS-ROER-EJ-FAELT TO MOD-ADLAGOMR-RAD(LO-INDX)                
194200       ADD +1 TO LO-INDX                                                  
194300     END-PERFORM                                                          
194400     MOVE +1 TO SUB-INDX                                                  
194500     PERFORM UNTIL SUB-INDX > MAX-SUB-INDX                                
194600       MOVE MFS-ROER-EJ-FAELT TO MOD-IDPRC-SUB-RAD(SUB-INDX)              
194700       ADD +1 TO SUB-INDX                                                 
194800     END-PERFORM                                                          
194900     .                                                                    
195000     SKIP2                                                                
195100 MFS-FORM-ATTR SECTION.                                                   
195200                                                                          
195300     MOVE MFS-FORMATETS-ATTR TO MOD-IDPRC-UPDATE-ATTR                     
195400                                MOD-FLAGGA-UT-ATTR                        
195500                                MOD-FLSTJORD-UPDATE-ATTR                  
195600                                MOD-RESPLIT-UPDATE-ATTR                   
195700                                MOD-KDPRCTYP-UPDATE-ATTR                  
195800                                MOD-KVVTID-UPDATE-ATTR                    
195900                                MOD-IDPRC-HUV-UPDATE-ATTR                 
196000                                MOD-VLKOLGR-UPDATE-ATTR                   
196100                                MOD-KDPRCGRP-UPDATE-ATTR                  
196200                                MOD-KDPRODKL-UPDATE-ATTR                  
196300                                MOD-KVORDER-UPDATE-ATTR                   
196400                                MOD-KVPLSRAD-UPDATE-ATTR                  
196500                                MOD-KVRADER-UPDATE-ATTR                   
196600                                MOD-VKPLSNTO-UPDATE-ATTR                  
196700                                MOD-VKORDNTO-UPDATE-ATTR                  
196800                                MOD-VLPLSNTO-UPDATE-ATTR                  
196900                                MOD-VLORDNTO-UPDATE-ATTR                  
197000     MOVE +1 TO LO-INDX                                                   
197100     PERFORM UNTIL LO-INDX > MAX-LO-INDX                                  
197200       MOVE MFS-FORMATETS-ATTR TO MOD-ADLAGOMR-UPDATE-ATTR                
197300                                               (LO-INDX)                  
197400       ADD +1 TO LO-INDX                                                  
197500     END-PERFORM                                                          
197600     MOVE +1 TO SUB-INDX                                                  
197700     PERFORM UNTIL SUB-INDX > MAX-SUB-INDX                                
197800       MOVE MFS-FORMATETS-ATTR TO MOD-IDPRC-SUB-UPDATE-ATTR               
197900                                                (SUB-INDX)                
198000       ADD +1 TO SUB-INDX                                                 
198100     END-PERFORM                                                          
198200     .                                                                    
198300     EJECT                                                                
198400 MFS-LAS-IN-IGEN SECTION.                                                 
198500                                                                          
198600     IF MID-IDPRC-UPDATE NOT = ALL '+'                                    
198700       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDPRC-UPDATE-ATTR                
198800     END-IF                                                               
198900     IF MID-FLAGGA-RAD NOT = ALL '+'                                      
199000       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLAGGA-UT-ATTR                   
199100     END-IF                                                               
199200     IF MID-FLSTJORD-UPDATE NOT = ALL '+'                                 
199300       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLSTJORD-UPDATE-ATTR             
199400     END-IF                                                               
199500     IF MID-RESPLIT-UPDATE NOT = ALL '+'                                  
199600       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-RESPLIT-UPDATE-ATTR              
199700     END-IF                                                               
199800     IF MID-KDPRCTYP-UPDATE NOT = ALL '+'                                 
199900       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDPRCTYP-UPDATE-ATTR             
200000     END-IF                                                               
200100     IF MID-KVVTID-UPDATE NOT = ALL '+'                                   
200200       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KVVTID-UPDATE-ATTR               
200300     END-IF                                                               
200400     IF MID-IDPRC-HUV-UPDATE NOT = ALL '+'                                
200500       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDPRC-HUV-UPDATE-ATTR            
200600     END-IF                                                               
200700     IF MID-VLKOLGR-UPDATE NOT = ALL '+'                                  
200800       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-VLKOLGR-UPDATE-ATTR              
200900     END-IF                                                               
201000     IF MID-BEPRC-RAD NOT = ALL '+'                                       
201100       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-BEPRC-ATTR                       
201200     END-IF                                                               
201300     IF MID-KDPRCGRP-UPDATE NOT = ALL '+'                                 
201400       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDPRCGRP-UPDATE-ATTR             
201500     END-IF                                                               
201600     IF MID-KDPRODKL-UPDATE NOT = ALL '+'                                 
201700       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDPRODKL-UPDATE-ATTR             
201800     END-IF                                                               
201900     IF MID-KVORDER-UPDATE NOT = ALL '+'                                  
202000       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KVORDER-UPDATE-ATTR              
202100     END-IF                                                               
202200     IF MID-KVPLSRAD-UPDATE NOT = ALL '+'                                 
202300       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KVPLSRAD-UPDATE-ATTR             
202400     END-IF                                                               
202500     IF MID-KVRADER-UPDATE NOT = ALL '+'                                  
202600       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KVRADER-UPDATE-ATTR              
202700     END-IF                                                               
202800     IF MID-VKPLSNTO-UPDATE NOT = ALL '+'                                 
202900       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-VKPLSNTO-UPDATE-ATTR             
203000     END-IF                                                               
203100     IF MID-VKORDNTO-UPDATE NOT = ALL '+'                                 
203200       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-VKORDNTO-UPDATE-ATTR             
203300     END-IF                                                               
203400     IF MID-VLPLSNTO-UPDATE NOT = ALL '+'                                 
203500       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-VLPLSNTO-UPDATE-ATTR             
203600     END-IF                                                               
203700     IF MID-VLORDNTO-UPDATE NOT = ALL '+'                                 
203800       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-VLORDNTO-UPDATE-ATTR             
203900     END-IF                                                               
204000     MOVE +1 TO LO-INDX                                                   
204100     PERFORM UNTIL LO-INDX > MAX-LO-INDX                                  
204200     IF MID-ADLAGOMR-UPDATE(LO-INDX) NOT = ALL '+'                        
204300       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-ADLAGOMR-UPDATE-ATTR             
204400                                               (LO-INDX)                  
204500     END-IF                                                               
204600     ADD +1 TO LO-INDX                                                    
204700     END-PERFORM                                                          
204800     MOVE +1 TO SUB-INDX                                                  
204900     PERFORM UNTIL SUB-INDX > MAX-SUB-INDX                                
205000     IF MID-IDPRC-SUB-UPDATE(SUB-INDX) NOT = ALL '+'                      
205100       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDPRC-SUB-UPDATE-ATTR            
205200                                                (SUB-INDX)                
205300     END-IF                                                               
205400     ADD +1 TO SUB-INDX                                                   
205500     END-PERFORM                                                          
205600     .                                                                    
205700     EJECT                                                                
205800* --- IMS SEKTIONER ---                                                   
205900     SKIP3                                                                
206000 IMS-GET-MSG SECTION.                                                     
206100                                                                          
206200     MOVE '  QC' TO GODK-STATUSKODER                                      
206300     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
206400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
206500     PERFORM IMS-STATUSKONTROLL                                           
206600     .                                                                    
206700     SKIP3                                                                
206800 IMS-INSERT-MSG SECTION.                                                  
206900                                                                          
207000     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
207100       MOVE '0' TO MFS-KDHUVOMR                                           
207200     END-IF                                                               
207300     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
207400     MOVE SPACE TO GODK-STATUSKODER                                       
207500     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
207600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
207700     PERFORM IMS-STATUSKONTROLL                                           
207800     .                                                                    
207900     EJECT                                                                
208000                                                                          
208100 IMS-INSERT-ALTMSG SECTION.                                               
208200     MOVE '  ' TO GODK-STATUSKODER                                        
208300     CALL CBLTDLI USING ISRT ALT-PCB W-PROG-TO-PROG-SW                    
208400     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
208500     PERFORM IMS-STATUSKONTROLL                                           
208600     .                                                                    
208700     EJECT                                                                
208800                                                                          
208900 IMS-GHU-XXKH-XXKH11 SECTION.                                             
209000     MOVE 'STA IMS-GHU-XXKH-XXKH11'  TO IMSSEC                            
209100                                                                          
209200     STRING 'WLXXKH01(WDGXKEY  =' W-4447-IDHTYP-X ')'                     
209300          DELIMITED BY SIZE INTO SSA1                                     
209400     STRING 'WLXXKH11(WDGXKEY  =' W-IDPRC-X ')'                           
209500          DELIMITED BY SIZE INTO SSA2                                     
209600     MOVE '  GE' TO GODK-STATUSKODER                                      
209700     CALL CBLTDLI USING GHU XXKH-PCB DLI-IO-AREA-4448 SSA1 SSA2           
209800     MOVE XXKH-STATUS-CODE TO STATUS-WS                                   
209900     PERFORM IMS-STATUSKONTROLL                                           
210000     .                                                                    
210100                                                                          
210200 IMS-GHU-XXKH-XXKH11-DEF SECTION.                                         
210300     MOVE 'STA IMS-GHU-XXKH-XXKH11-DEF'  TO IMSSEC                        
210400                                                                          
210500     STRING 'WLXXKH01(WDGXKEY  =' W-4447-IDHTYP-X ')'                     
210600          DELIMITED BY SIZE INTO SSA1                                     
210700     STRING 'WLXXKH11(WDGXKEY  =' W-IDPRC-DEF-X ')'                       
210800          DELIMITED BY SIZE INTO SSA2                                     
210900     MOVE '  GE' TO GODK-STATUSKODER                                      
211000     CALL CBLTDLI USING GHU XXKH-PCB DLI-IO-AREA-4448 SSA1 SSA2           
211100     MOVE XXKH-STATUS-CODE TO STATUS-WS                                   
211200     PERFORM IMS-STATUSKONTROLL                                           
211300     .                                                                    
211400                                                                          
211500 IMS-ISRT-XXKH-XXKH11 SECTION.                                            
211600     MOVE 'STA IMS-ISRT-XXKH-XXKH11   '  TO IMSSEC                        
211700                                                                          
211800     STRING 'WLXXKH01(WDGXKEY  =' W-4447-IDHTYP-X ')'                     
211900          DELIMITED BY SIZE INTO SSA1                                     
212000     MOVE 'WLXXKH11 ' TO SSA2                                             
212100     MOVE '  II' TO GODK-STATUSKODER                                      
212200     CALL CBLTDLI USING ISRT XXKH-PCB DLI-IO-AREA-4448 SSA1 SSA2          
212300     MOVE XXKH-STATUS-CODE TO STATUS-WS                                   
212400     PERFORM IMS-STATUSKONTROLL                                           
212500     .                                                                    
212600                                                                          
212700 IMS-GHU-XXKL-XXKL01 SECTION.                                             
212800     MOVE 'STA IMS-GHU-XXKL-XXKL01    '  TO IMSSEC                        
212900                                                                          
213000     STRING 'WLXXKL01(WDGXKEY  =' W-4453-IDHTYP-X ')'                     
213100          DELIMITED BY SIZE INTO SSA1                                     
213200     MOVE '  GE' TO GODK-STATUSKODER                                      
213300     CALL CBLTDLI USING GHU XXKL-PCB DLI-IO-AREA-4453 SSA1                
213400     MOVE XXKL-STATUS-CODE TO STATUS-WS                                   
213500     PERFORM IMS-STATUSKONTROLL                                           
213600     .                                                                    
213700                                                                          
213800 IMS-REPL-XXKH SECTION.                                                   
213900     MOVE 'STA IMS-REPL-XXKH          '  TO IMSSEC                        
214000                                                                          
214100     MOVE '  ' TO GODK-STATUSKODER                                        
214200     CALL CBLTDLI USING REPL XXKH-PCB DLI-IO-AREA-4448                    
214300     MOVE XXKH-STATUS-CODE TO STATUS-WS                                   
214400     PERFORM IMS-STATUSKONTROLL                                           
214500     .                                                                    
214600                                                                          
214700 IMS-DLET-XXKH SECTION.                                                   
214800     MOVE 'STA IMS-DLET-XXKH          '  TO IMSSEC                        
214900                                                                          
215000     MOVE '  ' TO GODK-STATUSKODER                                        
215100     CALL CBLTDLI USING DLET XXKH-PCB DLI-IO-AREA-4448                    
215200     MOVE XXKH-STATUS-CODE TO STATUS-WS                                   
215300     PERFORM IMS-STATUSKONTROLL                                           
215400     .                                                                    
215500                                                                          
215600 IMS-GU-XXKG-XXKG01 SECTION.                                              
215700     MOVE 'IMS-GU-XXKG-XXKG01         '  TO IMSSEC                        
215800                                                                          
215900     STRING 'WLXXKG01(IDHTYP   =' '4445' ')'                              
216000          DELIMITED BY SIZE INTO SSA1                                     
216100     MOVE '  GBGE' TO GODK-STATUSKODER                                    
216200     CALL CBLTDLI USING GU XXKG-PCB DLI-IO-AREA-4445 SSA1                 
216300     MOVE XXKG-STATUS-CODE TO STATUS-WS                                   
216400     PERFORM IMS-STATUSKONTROLL                                           
216500     .                                                                    
216600                                                                          
216700 IMS-GN-XXKG-XXKG01 SECTION.                                              
216800     MOVE 'IMS-GN-XXKG-XXKG01         '  TO IMSSEC                        
216900                                                                          
217000     STRING 'WLXXKG01(IDHTYP   =' '4445' ')'                              
217100          DELIMITED BY SIZE INTO SSA1                                     
217200     MOVE '  GBGE' TO GODK-STATUSKODER                                    
217300     CALL CBLTDLI USING GN XXKG-PCB DLI-IO-AREA-4445 SSA1                 
217400     MOVE XXKG-STATUS-CODE TO STATUS-WS                                   
217500     PERFORM IMS-STATUSKONTROLL                                           
217600     .                                                                    
217700                                                                          
217800 IMS-GNP-XXKG-XXKG11 SECTION.                                             
217900     MOVE 'IMS-GNP-XXKG-XXKG11        '  TO IMSSEC                        
218000                                                                          
218100     MOVE 'WLXXKG11 ' TO SSA1                                             
218200     MOVE '  GBGE' TO GODK-STATUSKODER                                    
218300     CALL CBLTDLI USING GNP XXKG-PCB DLI-IO-AREA-4446 SSA1                
218400     MOVE XXKG-STATUS-CODE TO STATUS-WS                                   
218500     PERFORM IMS-STATUSKONTROLL                                           
218600     .                                                                    
218700                                                                          
218800 IMS-GHU-XXKL-XXKL01-DEF SECTION.                                         
218900     MOVE 'IMS-GHU-XXKL-XXKL01-DEF    '  TO IMSSEC                        
219000                                                                          
219100     STRING 'WLXXKL01(WDGXKEY  =' W-4453-IDHTYP-DEF-X ')'                 
219200          DELIMITED BY SIZE INTO SSA1                                     
219300     MOVE '  GE' TO GODK-STATUSKODER                                      
219400     CALL CBLTDLI USING GHU XXKL-PCB DLI-IO-AREA-4453 SSA1                
219500     MOVE XXKL-STATUS-CODE TO STATUS-WS                                   
219600     PERFORM IMS-STATUSKONTROLL                                           
219700     .                                                                    
219800                                                                          
219900 IMS-GHNP-XXKL-XXKL11-DEF SECTION.                                        
220000     MOVE 'IMS-GHNP-XXKL-XXKL11-DEF   '  TO IMSSEC                        
220100                                                                          
220200     STRING 'WLXXKL01(WDGXKEY  =' W-4453-IDHTYP-DEF-X ')'                 
220300          DELIMITED BY SIZE INTO SSA1                                     
220400     MOVE 'WLXXKL11 ' TO SSA2                                             
220500     MOVE '  GE' TO GODK-STATUSKODER                                      
220600     CALL CBLTDLI USING GHNP XXKL-PCB DLI-IO-AREA-4454 SSA1 SSA2          
220700     MOVE XXKL-STATUS-CODE TO STATUS-WS                                   
220800     PERFORM IMS-STATUSKONTROLL                                           
220900     .                                                                    
221000                                                                          
221100 IMS-GHNP-XXKL-XXKL11 SECTION.                                            
221200     MOVE 'IMS-GHNP-XXKL-XXKL11       '  TO IMSSEC                        
221300                                                                          
221400     STRING 'WLXXKL01(WDGXKEY  =' W-4453-IDHTYP-X ')'                     
221500          DELIMITED BY SIZE INTO SSA1                                     
221600     MOVE 'WLXXKL11 ' TO SSA2                                             
221700     MOVE '  GE' TO GODK-STATUSKODER                                      
221800     CALL CBLTDLI USING GHNP XXKL-PCB DLI-IO-AREA-4454 SSA1 SSA2          
221900     MOVE XXKL-STATUS-CODE TO STATUS-WS                                   
222000     PERFORM IMS-STATUSKONTROLL                                           
222100     .                                                                    
222200                                                                          
222300 IMS-ISRT-XXKL-XXKL01 SECTION.                                            
222400     MOVE 'IMS-ISRT-XXKL-XXKL01       '  TO IMSSEC                        
222500                                                                          
222600     MOVE 'WLXXKL01 ' TO SSA1                                             
222700     MOVE '  II' TO GODK-STATUSKODER                                      
222800     CALL CBLTDLI USING ISRT XXKL-PCB DLI-IO-AREA-4453 SSA1               
222900     MOVE XXKL-STATUS-CODE TO STATUS-WS                                   
223000     PERFORM IMS-STATUSKONTROLL                                           
223100     .                                                                    
223200                                                                          
223300 IMS-ISRT-XXKL-XXKL11 SECTION.                                            
223400     MOVE 'IMS-ISRT-XXKL-XXKL11       '  TO IMSSEC                        
223500                                                                          
223600     STRING 'WLXXKL01(WDGXKEY  =' W-4453-IDHTYP-X ')'                     
223700          DELIMITED BY SIZE INTO SSA1                                     
223800     MOVE 'WLXXKL11 ' TO SSA2                                             
223900     MOVE '  II' TO GODK-STATUSKODER                                      
224000     CALL CBLTDLI USING ISRT XXKL-PCB DLI-IO-AREA-4454 SSA1 SSA2          
224100     MOVE XXKL-STATUS-CODE TO STATUS-WS                                   
224200     PERFORM IMS-STATUSKONTROLL                                           
224300     .                                                                    
224400                                                                          
224500 IMS-DLET-XXKL SECTION.                                                   
224600     MOVE 'IMS-DLET-XXKL              '  TO IMSSEC                        
224700                                                                          
224800     MOVE '  ' TO GODK-STATUSKODER                                        
224900     CALL CBLTDLI USING DLET XXKL-PCB DLI-IO-AREA-4453                    
225000     MOVE XXKL-STATUS-CODE TO STATUS-WS                                   
225100     PERFORM IMS-STATUSKONTROLL                                           
225200     .                                                                    
225300                                                                          
225400 IMS-GHU-XXKO-XXKO01 SECTION.                                             
225500     MOVE 'IMS-GHU-XXKO-XXKO01        '  TO IMSSEC                        
225600                                                                          
225700     STRING 'WLXXKO01(WDGXKEY  =' W-4461-IDHTYP-X ')'                     
225800          DELIMITED BY SIZE INTO SSA1                                     
225900     MOVE '  GE' TO GODK-STATUSKODER                                      
226000     CALL CBLTDLI USING GHU XXKO-PCB DLI-IO-AREA-4461 SSA1                
226100     MOVE XXKO-STATUS-CODE TO STATUS-WS                                   
226200     PERFORM IMS-STATUSKONTROLL                                           
226300     .                                                                    
226400                                                                          
226500 IMS-ISRT-XXKO-XXKO01 SECTION.                                            
226600     MOVE 'IMS-ISRT-XXKO-XXKO01       '  TO IMSSEC                        
226700                                                                          
226800     MOVE 'WLXXKO01 ' TO SSA1                                             
226900     MOVE '  II' TO GODK-STATUSKODER                                      
227000     CALL CBLTDLI USING ISRT XXKO-PCB DLI-IO-AREA-4461 SSA1               
227100     MOVE XXKO-STATUS-CODE TO STATUS-WS                                   
227200     PERFORM IMS-STATUSKONTROLL                                           
227300     .                                                                    
227400                                                                          
227500 IMS-ISRT-XXKO-XXKO11 SECTION.                                            
227600     MOVE 'IMS-ISRT-XXKO-XXKO11       '  TO IMSSEC                        
227700                                                                          
227800     STRING 'WLXXKO01(WDGXKEY  =' W-4461-IDHTYP-X ')'                     
227900          DELIMITED BY SIZE INTO SSA1                                     
228000     MOVE 'WLXXKO11 ' TO SSA2                                             
228100     MOVE '  II' TO GODK-STATUSKODER                                      
228200     CALL CBLTDLI USING ISRT XXKO-PCB DLI-IO-AREA-4462 SSA1 SSA2          
228300     MOVE XXKO-STATUS-CODE TO STATUS-WS                                   
228400     PERFORM IMS-STATUSKONTROLL                                           
228500     .                                                                    
228600                                                                          
228700 IMS-GU-ORQD-ORQD01 SECTION.                                              
228800     MOVE 'IMS-GU-ORQD-ORQD01         '  TO IMSSEC                        
228900                                                                          
229000     STRING 'WLORQD01(WDQ3C1KY>=' W-WDQ3C1KY-MIN-X                        
229100                    '&WDQ3C1KY<=' W-WDQ3C1KY-MAX-X ')'                    
229200          DELIMITED BY SIZE INTO SSA1                                     
229300     MOVE '  GE' TO GODK-STATUSKODER                                      
229400     CALL CBLTDLI USING GU ORQD-PCB DLI-IO-AREA-SEQC SSA1                 
229500     MOVE ORQD-STATUS-CODE TO STATUS-WS                                   
229600     PERFORM IMS-STATUSKONTROLL                                           
229700     .                                                                    
229800                                                                          
229900 IMS-GU-XXKC-XXKC11 SECTION.                                              
230000     MOVE 'IMS-GU-XXKC-XXKC11         '  TO IMSSEC                        
230100                                                                          
230200     STRING 'WLXXKC01(WDGXKEY  =' W-4435-IDHTYP-X ')'                     
230300          DELIMITED BY SIZE INTO SSA1                                     
230400     STRING 'WLXXKC11(WDGXKEY  =' W-4436-IDHTYP-X ')'                     
230500          DELIMITED BY SIZE INTO SSA2                                     
230600     MOVE '  GE' TO GODK-STATUSKODER                                      
230700     CALL CBLTDLI USING GU XXKC-PCB DLI-IO-AREA-4436 SSA1 SSA2            
230800     MOVE XXKC-STATUS-CODE TO STATUS-WS                                   
230900     PERFORM IMS-STATUSKONTROLL                                           
231000     .                                                                    
231100                                                                          
231200 IMS-GHU-XXKC-XXKC11 SECTION.                                             
231300     MOVE 'IMS-GHU-XXKC-XXKC11        '  TO IMSSEC                        
231400                                                                          
231500     STRING 'WLXXKC01(WDGXKEY  =' W-4435-IDHTYP-X ')'                     
231600          DELIMITED BY SIZE INTO SSA1                                     
231700     STRING 'WLXXKC11(WDGXKEY  =' W-4436-IDHTYP-X ')'                     
231800          DELIMITED BY SIZE INTO SSA2                                     
231900     MOVE '  GE' TO GODK-STATUSKODER                                      
232000     CALL CBLTDLI USING GHU XXKC-PCB DLI-IO-AREA-4436 SSA1 SSA2           
232100     MOVE XXKC-STATUS-CODE TO STATUS-WS                                   
232200     PERFORM IMS-STATUSKONTROLL                                           
232300     .                                                                    
232400                                                                          
232500 IMS-ISRT-XXKC-XXKC11 SECTION.                                            
232600     MOVE 'IMS-ISRT-XXKC-XXKC11       '  TO IMSSEC                        
232700                                                                          
232800     STRING 'WLXXKC01(WDGXKEY  =' W-4435-IDHTYP-X ')'                     
232900          DELIMITED BY SIZE INTO SSA1                                     
233000     MOVE 'WLXXKC11 ' TO SSA2                                             
233100     MOVE '  II' TO GODK-STATUSKODER                                      
233200     CALL CBLTDLI USING ISRT XXKC-PCB DLI-IO-AREA-4436 SSA1 SSA2          
233300     MOVE XXKC-STATUS-CODE TO STATUS-WS                                   
233400     PERFORM IMS-STATUSKONTROLL                                           
233500     .                                                                    
233600                                                                          
233700 IMS-DLET-XXKC-XXKC11 SECTION.                                            
233800     MOVE 'IMS-DLET-XXKC-XXKC11       '  TO IMSSEC                        
233900                                                                          
234000     MOVE '    ' TO GODK-STATUSKODER                                      
234100     CALL CBLTDLI USING DLET XXKC-PCB DLI-IO-AREA-4436                    
234200     MOVE XXKC-STATUS-CODE TO STATUS-WS                                   
234300     PERFORM IMS-STATUSKONTROLL                                           
234400     .                                                                    
234500                                                                          
234600 IMS-STATUSKONTROLL SECTION.                                              
234700                                                                          
234800     SET STATUS-IX TO 1                                                   
234900     SEARCH GODK-STATUS                                                   
235000       AT END CALL FELLOG                                                 
235100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
235200     END-SEARCH                                                           
235300     .                                                                    
