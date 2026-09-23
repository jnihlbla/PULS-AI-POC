000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W5020600.                                                
000400 AUTHOR.         ANDERS HENRIKSSON                                        
000500 DATE-WRITTEN.   20120814.                                                
000600**                                                                        
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION.                                                            
001000*        IMSDC UPPDATERINGSPROGRAM FÖR EKONOMI.                           
001100*                                                                         
001200*        UPPDATERINGEN SKER PÅ TVÅ SÄTT:                                  
001300*        - FRÅN INKÖP GENOM TRANS W50206X FRÅN W55374                     
001400*          VIA WLKOMA (WDP8), KOMMUNIKATIONSDATABAS.                      
001500*        - FRÅN SKÄRMEN                                                   
001600*                                                                         
001700*        OM UPPDATERINGEN KOM FRÅN INKÖP OCH                              
001800*        UPPDATERINGEN GICK BRA SKICKAS OK-MEDDELANDE                     
001900*        TILL DISPATCHER ANNARS SKICKAS FELMEDDELANDE.                    
002000*                                                                         
002100*        PROGRAMMET LÄSER WDK7  OCH LÄGGER UPP NYA                        
002200*        BESTÄLLNINGSPRISER ELLER ÄNDRAR BEFINTLIGA PÅ                    
002300*        WDK724. OM INLEVERANS SKETT EFTER AKTUELLT                       
002400*        DATUM UPPDATERAS ÄVEN                                            
002500*        SJÄLVKOSTNADSPRIS PÅ WDK712.                                     
002600*                                                                         
002700*    INDATA.                                                              
002800*        TRANSAKTION: W502T6                                              
002900*                     W502T6U                                             
003000*                     W502T6X                                             
003100*                                                                         
003200*        MID:         W5I20601                                            
003300*                     W5I20601 + WMSGKOM                                  
003400*                                                                         
003500*    UTDATA.                                                              
003600*        MOD:         W5O20601                                            
003700*                                                                         
003800     EJECT                                                                
003900 ENVIRONMENT DIVISION.                                                    
004000 DATA DIVISION.                                                           
004100 WORKING-STORAGE SECTION.                                                 
004200                                                                          
004300 77  IDPGM                   PIC X(8)       VALUE 'W5020600'.             
004400 77  JA                      PIC X          VALUE 'J'.                    
004500 77  NEJ                     PIC X          VALUE 'N'.                    
004600                                                                          
004700 77  NYCKLAR-OK              PIC X          VALUE 'J'.                    
004800 77  UPPDATERING-OK          PIC X          VALUE 'J'.                    
004900 77  SJK-UPPDAT-OK           PIC X          VALUE 'J'.                    
005000 77  FLFEL-FAELT             PIC X          VALUE 'N'.                    
005100 77  FLSLUTA-LAS             PIC X          VALUE 'N'.                    
005200 77  INLEV-PRISRAD           PIC X          VALUE 'N'.                    
005300 77  INLEV-FINNS             PIC X          VALUE 'N'.                    
005400                                                                          
005500 77  INDX                    PIC S9(9)     VALUE +0   COMP SYNC.          
005600 77  INDX-NY                 PIC S9(9)     VALUE +0   COMP SYNC.          
005700                                                                          
005800 77  WS-IDLEVNR              PIC X(5)       VALUE SPACE.                  
005900 77  WS-IDLEVNR-FIRST        PIC X(5)       VALUE SPACE.                  
006000 77  WS-KDPRBEH              PIC X(1)       VALUE SPACE.                  
006100 77  WS-REAENDR              PIC X(6)       VALUE SPACE.                  
006200 77  WS-REAENDR-NUM          PIC 9(4)V9(1)  VALUE ZERO.                   
006300 77  WS-KDFPKPRI             PIC X          VALUE SPACE.                  
006400 77  WS-REEMBHNT             PIC S9(2)V9(3)  VALUE ZERO.                  
006500 77  WS-RELANDCO-EXP         PIC S9(3)V9(3) COMP-3.                       
006600 77  WS-IDDC-SPAR            PIC X(2)       VALUE SPACE.                  
006700 77  W-DATE-AAMM             PIC 9(4)       VALUE ZERO.                   
006800 77  W-KDVALISO-HUV          PIC X(3)       VALUE SPACE.                  
006900                                                                          
007000 01  IDARTNR-WS              PIC X(9)       VALUE SPACE.                  
007100 01  WS-TIUPPDAT             PIC S9(7)     COMP-3.                        
007200 01  WS-TIUPPTID             PIC S9(9)     COMP-3.                        
007300 01  WS-TIPRLIST-6           PIC  9(6).                                   
007400 01  WS-PRARTBES             PIC 9(13).                                   
007500 01  WS-RETULF-SEND          PIC 9(7).                                    
007600*                                                                         
007700 01  IDLAND-FL-SW            PIC X(2)       VALUE '  '.                   
007800     88 IDLAND-US-DC                        VALUE 'US'.                   
007900     88 IDLAND-CA-DC                        VALUE 'CA'.                   
008000     88 IDLAND-CN-DC                        VALUE 'CN'.                   
008100                                                                          
008200 01  SUBPGM.                                                              
008300     03  CBLTDLI             PIC X(8)       VALUE 'CBLTDLI '.             
008400     03  FELLOG              PIC X(8)       VALUE 'FELLOG  '.             
008500     03  WDATKONV            PIC X(8)       VALUE 'WDATKONV'.             
008600     03  WDECEDIT            PIC X(8)       VALUE 'WDECEDIT'.             
008700     03  WDECSTR             PIC X(8)       VALUE 'WDECSTR '.             
008800     03  W005INIT            PIC X(8)       VALUE 'W005INIT'.             
008900     03  W006KOM             PIC X(8)       VALUE 'W006KOM '.             
009000     03  W005WDK7            PIC X(8)       VALUE 'W005WDK7'.             
009100     03  W510PRTR            PIC X(8)       VALUE 'W510PRTR'.             
009200     03  W510CURR            PIC X(8)       VALUE 'W510CURR'.             
009300                                                                          
009400 01  W-IDTRANS               PIC X(4)       VALUE SPACE.                  
009500     88 EGEN-TRANS                          VALUE '5206'.                 
009600     88 GODK-TRANS                          VALUE '5206' '5207'.          
009700                                                                          
009800 01  DIVERSE.                                                             
009900     03  W-KDAVT             PIC S9                    COMP-3.            
010000     03  W-TIPRLIST          PIC 9(6).                                    
010100     03  W-SUINLEV           PIC S9(3)      VALUE +0   COMP-3.            
010200     03  W-KVPB-TOT          PIC S9(6)V9    VALUE +0   COMP-3.            
010300     03  WS-PRKURS           PIC S9(5)V9(5) VALUE +0   COMP-3.            
010400     03  W-PRARTBEL          PIC S9(8)V9(5) VALUE +0.                     
010500     03  WS-PRARTBEL-PR      PIC S9(8)V9(5) VALUE +0   COMP-3.            
010600     03  W-PRARTBES          PIC S9(7)V9(2) VALUE +0   COMP-3.            
010700     03  WS-PRMATRL          PIC S9(7)V9(2) VALUE +0   COMP-3.            
010800     03  WS-PRARTBES-PR      PIC S9(7)V9(2) VALUE +0   COMP-3.            
010900     03  W-PRARTSJK          PIC S9(7)V9(2) VALUE +0   COMP-3.            
011000     03  W-PRLFKST           PIC S9(3)V9(2) VALUE +0   COMP-3.            
011100     03  W-RETULF            PIC S9(3)V9(4) VALUE +0.                     
011200     03  WS-RETULF           PIC S9(3)V9(4) VALUE +0.                     
011300     03  W-PRKURS            PIC S9(5)V9(5) VALUE +0   COMP-3.            
011400     03  W-PRDMTRL           PIC S9(6)V9(3) VALUE +0   COMP-3.            
011500     03  W-PRDIRLON          PIC S9(4)V9(3) VALUE +0   COMP-3.            
011600     03  W-REVALUTA          PIC S9(5)      VALUE +0   COMP-3.            
011700     03  W-PRKURS-SEK        PIC S9(5)V9(5) VALUE +0   COMP-3.            
011800     03  W-REVALUTA-SEK      PIC S9(5)      VALUE +0   COMP-3.            
011900     03  W-SDCSALDO          PIC S9(9)      VALUE +0   COMP-3.            
012000     03  W-SDCSALDO-ALL-DC   PIC S9(9)      VALUE +0   COMP-3.            
012100     03  WS-KDVALISO         PIC X(3)       VALUE SPACE.                  
012200     03  WS-KDVALISO-X       PIC X(3)       VALUE SPACE.                  
012300     03  PRISANDRING         PIC 9(8)V9(10) VALUE  0.                     
012400     03  ANT-INLEV           PIC S9         VALUE +0   COMP-3.            
012500     03  W-IDSTRDATA-X.                                                   
012600         05  W-IDSTRDATA-N   PIC 9(8)V9(5).                               
012700         05  FILLER          PIC XX.                                      
012800                                                                          
012900     03  DAGENS-AAMMDD       PIC 9(6).                                    
013000     03  DAGENS-DAT          PIC 9(8).                                    
013100     03  DAGENS-TID          PIC 9(8).                                    
013200     03  WS-DATUM            PIC 9(8).                                    
013300     03  WS-DATUM-FIRST      PIC 9(8).                                    
013400     03  WS-DAPRLIST-9KOMPL   PIC S9(8) COMP-3.                           
013500     03  WS-DAPRLIST-9KOMPL-1 PIC S9(8) COMP-3.                           
013600     03  WS-DAPRLIST-9KOMPL-2 PIC S9(8) COMP-3.                           
013700                                                                          
013800     03  MAX-PRARTBES        PIC S9(7)V9(2) VALUE +400000 COMP-3.         
013900                                                                          
014000     03  DAINLEV-FAELT.                                                   
014100      05  DAINLEV-NYCKEL     PIC 9(16)  VALUE ZERO.                       
014200      05 DAINLEV-MAX         PIC 9(16)  VALUE 9999999999999999.           
014300                                                                          
014400     03  R25-DATUM-X         PIC 9(16).                                   
014500     03  R25-DATUM-R  REDEFINES  R25-DATUM-X.                             
014600      05 R25-DATUM-SEKEL     PIC 9(02).                                   
014700      05 R25-DATUM           PIC 9(06).                                   
014800      05 R25-DATUM-NOLLOR    PIC 9(08).                                   
014900                                                                          
015000     03  DATUM-FAELT.                                                     
015100      05 W-DAPRLIST-MAX      PIC 9(8)    VALUE 99999999.                  
015200      05 W-DAPRLIST          PIC 9(8).                                    
015300      05 W-DAPRLIST-X  REDEFINES W-DAPRLIST.                              
015400       07 FILLER             PIC 9(2).                                    
015500       07 W-LISTDATUM        PIC 9(6).                                    
015600                                                                          
015700 01  FILLER                      PIC X(16)   VALUE 'P-TO-P-AREA'.         
015800 01  P-TO-P-AREA.                                                         
015900     03  P-TO-P-LL               PIC S9(4)            COMP SYNC.          
016000     03  P-TO-P-Z1               PIC  X(1)   VALUE LOW-VALUE.             
016100     03  P-TO-P-Z2               PIC  X(1)   VALUE LOW-VALUE.             
016200     03  P-TO-P-TRANSKOD         PIC  X(7).                               
016300     03  FILLER                  PIC  X(1)   VALUE SPACE.                 
016400     03  P-TO-P-FROM-MID         PIC  X(4).                               
016500     03  P-TO-P-KDMFSFOR         PIC  X(1).                               
016600     03  P-TO-P-DATA             PIC  X(1000).                            
016700     EJECT                                                                
016800     EJECT                                                                
016900 01  FILLER                  PIC X(16)   VALUE 'DLI-NYCKLAR'.             
017000 01  NYCKLAR-TILL-DLI.                                                    
017100     03  W-IDARTNR-X.                                                     
017200         05  W-IDARTNR       PIC S9(9)   VALUE +0  COMP-3.                
017300     03  W-KDSEGKEY-X.                                                    
017400         05  W-KDSEGKEY      PIC X(1)    VALUE '1'.                       
017500     03  W-DAINLEV-X.                                                     
017600         05    W-DAINLEV     PIC 9(16)   VALUE ZERO.                      
017700     03  W-IDSKYLT-X.                                                     
017800         05    W-IDSKYLT     PIC X(3)    VALUE 'GB '.                     
017900     03  W-IDLEVNR-X.                                                     
018000         05    W-IDLEVNR     PIC X(5)    VALUE SPACE.                     
018100     03  W-IDLAND-X.                                                      
018200         05    W-IDLAND      PIC X(2)    VALUE SPACE.                     
018300     03  W-IDDC-B6-X.                                                     
018400         05 W-IDDC-B6        PIC X(2).                                    
018500     03  W-IDDC-WDK7-MIN-X.                                               
018600         05  W-IDDC-WDK7-MIN PIC X(2).                                    
018700     03  W-IDDC-WDK7-MAX-X.                                               
018800         05  W-IDDC-WDK7-MAX PIC X(2).                                    
018900     03  W-IDLANDX2-X.                                                    
019000         05    W-IDLANDX2    PIC X(2)    VALUE SPACE.                     
019100     03  W-IDGMT-X.                                                       
019200         05  W-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.           
019300         05  W-IDKUNDNR          PIC S9(7)   VALUE ZERO COMP-3.           
019400     SKIP2                                                                
019500     03  W-IDGMT-MIN-X.                                                   
019600         05  W-IDDISTR-MIN       PIC S9(5)   VALUE ZERO COMP-3.           
019700         05  W-IDKUNDNR-MIN      PIC S9(7)   VALUE ZERO COMP-3.           
019800     SKIP2                                                                
019900     03  W-IDGMT-MAX-X.                                                   
020000         05  W-IDDISTR-MAX       PIC S9(5)   VALUE ZERO COMP-3.           
020100         05  W-IDKUNDNR-MAX      PIC S9(7)  VALUE +9999999 COMP-3.        
020200     SKIP2                                                                
020300                                                                          
020400     EJECT                                                                
020500 01  MEDDELANDE.                                                          
020600     03  W-FEL-1               PIC X(26)   VALUE                          
020700             'PART NUMBER MISSING       '.                                
020800                                                                          
020900     03  W-FEL-2               PIC X(26)   VALUE                          
021000             'PART NUMBER EXPIRED       '.                                
021100                                                                          
021200     03  W-FEL-3               PIC X(26)   VALUE                          
021300             'HIGHLIGHT FIELD WRONG     '.                                
021400                                                                          
021500     03  W-FEL-4               PIC X(26)   VALUE                          
021600             'PARTNUMBER NOT NUMERIC    '.                                
021700                                                                          
021800     03  W-FEL-5               PIC X(40)   VALUE                          
021900             'PRESS F11 FOR UPDATE                 '.                     
022000                                                                          
022100     03  W-FEL-6               PIC X(40)   VALUE                          
022200             'GIVE VALUES WHEN UPDATE              '.                     
022300                                                                          
022400     03  W-FEL-7               PIC X(40)   VALUE                          
022500             'PRICE TO HIGH'.                                             
022600                                                                          
022700     03  W-FEL-8               PIC X(26)   VALUE                          
022800             'SUPERSESSION CODE > 9     '.                                
022900                                                                          
023000     03  W-FEL-9               PIC X(26)   VALUE                          
023100             'HF-CODE > 0               '.                                
023200                                                                          
023300     03  W-FEL-10              PIC X(26)   VALUE                          
023400             'NO UPDATE DONE            '.                                
023500                                                                          
023600     03  W-FEL-11              PIC X(40)   VALUE                          
023700             'PART IS MISSING PRICE                '.                     
023800                                                                          
023900     03  W-FEL-12              PIC X(40)   VALUE                          
024000             'SUPPLIER MISSING                      '.                    
024100                                                                          
024200     03  W-FEL-13              PIC X(40)   VALUE                          
024300             'WRONG DATE                            '.                    
024400                                                                          
024500     03  W-FEL-14              PIC X(40)   VALUE                          
024600             'MISSING CURRENCY RATE                 '.                    
024700                                                                          
024800     03  W-FEL-15              PIC X(40)   VALUE                          
024900             'PART NOT CORRECT CREATED              '.                    
025000                                                                          
025100     03  W-FEL-16              PIC X(40)   VALUE                          
025200             'WRONG UPDATE CODE                     '.                    
025300                                                                          
025400     03  W-FEL-17              PIC X(40)   VALUE                          
025500             'WRONG COMPANY CODE                    '.                    
025600                                                                          
025700     03  W-FEL-18              PIC X(40)   VALUE                          
025800             'PRICE IS NOT CORRECT                  '.                    
025900                                                                          
026000     03  W-FEL-19              PIC X(40)   VALUE                          
026100             'CURRENCY IS NOT CORRECT               '.                    
026200                                                                          
026300     03  W-FEL-20              PIC X(40)   VALUE                          
026400             'SUPPLIER HAS NO AGREEMENT             '.                    
026500                                                                          
026600     03  W-MED-1               PIC X(26)   VALUE                          
026700             'UPDATE DONE               '.                                
026800     03  W-MED-1A              PIC X(50)   VALUE                          
026900             'BEST/SJKPRIS UPD , INK/STDPRIS TILL KÖ'.                    
027000     03  W-MED-1B              PIC X(50)   VALUE                          
027100             'BEST/SJKPRIS UPPD, INK/STDPRIS UPPD'.                       
027200     03  W-MED-1C              PIC X(50)   VALUE                          
027300             'NO CHANGE OF PRICE DONE '.                                  
027400     SKIP3                                                                
027500 01  MESSAGE-CODES.                                                       
027600     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
027700     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
027800     EJECT                                                                
027900                                                                          
028000*                   ****    PARAMETRAR TILL W005INIT                      
028100 01  FILLER                    PIC X(16)   VALUE 'WMSGINIT'.              
028200*01  -COPY WMSGINIT                                                       
028300     EJECT                                                                
028400                                                                          
028500                                                                          
028600 01  WDATAREA                  PIC X(8)    VALUE 'WDATAREA'.              
028700*01       -COPY WDATAREA                                                  
028800     EJECT                                                                
028900                                                                          
029000 01  FILLER                    PIC X(8)    VALUE 'WDECAREA'.              
029100*01       -COPY WDECAREA                                                  
029200     EJECT                                                                
029300                                                                          
029400 01  FILLER                    PIC X(8)    VALUE 'WDECSTR'.               
029500*01       -COPY WDECSTR                                                   
029600     EJECT                                                                
029700                                                                          
029800 01  FILLER                    PIC X(8)    VALUE 'W553LVAL'.              
029900*01       -COPY W553LVAL                                                  
030000     EJECT                                                                
030100                                                                          
030200 01 FILLER                     PIC X(8)    VALUE 'W005WDK7'.              
030300*   -COPY W005WDK7                                                        
030400     EJECT                                                                
030500                                                                          
030600 01  FILLER                    PIC X(8)    VALUE 'W510PRTR'.              
030700*01       -COPY W510PRTR                                                  
030800     EJECT                                                                
030900                                                                          
031000*01       -COPY W510CURR                                                  
031100     EJECT                                                                
031200                                                                          
031300*         -COPY WY2000W9                                                  
031400*         -COPY WY2000W1                                                  
031500                                                                          
031600*01       -COPY WWDC99                                                    
031700*01       -COPY WWIDFTG                                                   
031800                                                                          
031900******************************************************************        
032000*                                                                         
032100*                AREOR FOR MFS OCH SKÄRMHANTERING                         
032200*                                                                         
032300******************************************************************        
032400 01    FILLER                    PIC X(16)   VALUE 'MFS-WS'.              
032500     SKIP3                                                                
032600*01  MID -COPY W5I20601 -PRE MID-                                         
032700     EJECT                                                                
032800*01  -COPY WMSGAREA                                                       
032900*  03  MOD  -COPY W5O20601 -PRE MOD-  -RED MSG-AREA                       
033000*                                                                         
033100     EJECT                                                                
033200     EJECT                                                                
033300*01  -COPY WMFSAREA                                                       
033400******************************************************************        
033500*                                                                         
033600*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
033700*                                                                         
033800******************************************************************        
033900                                                                          
034000 01  IMS-WS.                                                              
034100     03    FILLER              PIC X(16)   VALUE 'IMS-WS     '.           
034200     SKIP3                                                                
034300*                        **** STATUS-KOD FRÅN IMS ****                    
034400     03    STATUS-WS           PIC XX.                                    
034500         88    SEGMENT-FINNS               VALUE '  '.                    
034600         88    SEGMENT-SAKNAS              VALUE 'GE'.                    
034700         88    SEGMENT-FINNS-REDAN         VALUE 'II'.                    
034800         88    IMS-EJ-OK                   VALUE 'XD'.                    
034900     SKIP3                                                                
035000     03    GODK-STATUSKODER.                                              
035100         05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.          
035200     SKIP3                                                                
035300 01  SSA1                      PIC X(64).                                 
035400 01  SSA2                      PIC X(64).                                 
035500 01  SSA3                      PIC X(64).                                 
035600     EJECT                                                                
035700*                        **** IMS FUNKTIONSKODER ****                     
035800*01    -COPY W0003                                                        
035900     EJECT                                                                
036000*                            DLI INPUT-OUTPUT AREA                        
036100 01  FILLER                    PIC X(16)  VALUE 'WDK601'.                 
036200*01  WLARTC01 -COPY WDK601                                                
036300     EJECT                                                                
036400                                                                          
036500 01  FILLER                    PIC X(16)  VALUE 'WDK611'.                 
036600*01  WLARTC11 -COPY WDK611                                                
036700     EJECT                                                                
036800                                                                          
036900 01  FILLER                    PIC X(16)  VALUE 'WDK621'.                 
037000*01  WLARTC21 -COPY WDK621                                                
037100     EJECT                                                                
037200                                                                          
037300 01  FILLER                    PIC X(16)  VALUE 'WDK623'.                 
037400*01  WLARTC23 -COPY WDK623                                                
037500     EJECT                                                                
037600                                                                          
037700 01  FILLER                    PIC X(16)  VALUE 'WDD311'.                 
037800*01  WLBENA11 -COPY WDD311 -PRE BEN-                                      
037900     EJECT                                                                
038000                                                                          
038100 01  FILLER                    PIC X(16)  VALUE 'WDF101'.                 
038200*01  WLLEVA01 -COPY WDF101                                                
038300     EJECT                                                                
038400                                                                          
038500 01  FILLER                    PIC X(16)  VALUE 'WDF102'.                 
038600*01  WLLEVA11 -COPY WDF102 -PRE LEV-                                      
038700     EJECT                                                                
038800                                                                          
038900 01  FILLER                    PIC X(16)  VALUE 'WDL201'.                 
039000*01  WLINLE01 -COPY WDL201 -PRE INLE-                                     
039100     EJECT                                                                
039200                                                                          
039300 01  FILLER                    PIC X(16)  VALUE 'WDL211'.                 
039400*01  WLINLE11 -COPY WDL211 -PRE INLE-                                     
039500     EJECT                                                                
039600                                                                          
039700 01  FILLER                    PIC X(16)  VALUE 'WDL221'.                 
039800*01  WLINLE21 -COPY WDL221 -PRE INLE-                                     
039900     EJECT                                                                
040000                                                                          
040100 01  FILLER                    PIC X(16)  VALUE 'WDL222'.                 
040200*01  WLINLE22 -COPY WDL222 -PRE INLE-                                     
040300     EJECT                                                                
040400                                                                          
040500 01  FILLER                    PIC X(16)  VALUE 'WDK701'.                 
040600*01  WDK701   -COPY WDK701                                                
040700     EJECT                                                                
040800                                                                          
040900 01  FILLER                    PIC X(16)  VALUE 'WDK711'.                 
041000*01  WDK711   -COPY WDK711                                                
041100     EJECT                                                                
041200                                                                          
041300 01  FILLER                    PIC X(16)  VALUE 'WDK712'.                 
041400*01  WDK712   -COPY WDK712                                                
041500     EJECT                                                                
041600                                                                          
041700 01  FILLER                    PIC X(16)  VALUE 'WDK724'.                 
041800*01  WDK724   -COPY WDK724                                                
041900     EJECT                                                                
042000                                                                          
042100 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
042200 01   DLI-IO-AREA-B601.                                                   
042300*     03  -COPY WDB601                                                    
042400     EJECT                                                                
042500                                                                          
042600 01  FILLER               PIC X(16)   VALUE 'WDB617 AREA'.                
042700 01   DLI-IO-AREA-B617.                                                   
042800*     03  -COPY WDB617                                                    
042900     EJECT                                                                
043000                                                                          
043100*    --- AREA FÖR KOMMUNIKATION MED DISPATCHER                            
043200 01  FILLER                 PIC X(16)   VALUE 'KOM-DISP-IO-AREA'.         
043300 01  KOM-IO-AREA.                                                         
043400*  03     -COPY WMSGKOM                                                   
043500*                                                                         
043600 01  FILLER                 PIC X(16)   VALUE 'KOM-IO-AREA2'.             
043700 01  KOM-IO-AREA2.                                                        
043800*  03      -COPY W5I11101 -PRE 5111-                                      
043900     EJECT                                                                
044000                                                                          
044100 01  FILLER                 PIC X(16)   VALUE 'KOM-DISP-IO-3'.            
044200 01  KOM-IO-AREA3.                                                        
044300*  03     -COPY WMSGKOM -PRE 55374-                                       
044400*                                                                         
044500 01  FILLER                 PIC X(16)   VALUE 'KOM-DISP-IO-4'.            
044600 01  KOM-IO-AREA4.                                                        
044700*  03     -COPY WMSGKOM -PRE 50111-                                       
044800*                                                                         
044900                                                                          
045000 01  FILLER                      PIC X(16)   VALUE 'WDB201-AREA'.         
045100 01  DLI-IO-AREA-WDB2.                                                    
045200*    03  -COPY WDB201  -PRE WDB2-                                         
045300     EJECT                                                                
045400                                                                          
045500 LINKAGE SECTION.                                                         
045600*01  -COPY W0009     -PRE MSG-                                            
045700     EJECT                                                                
045800                                                                          
045900*01  -COPY W0009     -PRE ALT-                                            
046000     EJECT                                                                
046100                                                                          
046200*01  -COPY W0008     -PRE USEA-                                           
046300     05  FILLER                  PIC X.                                   
046400     EJECT                                                                
046500*01  -COPY W0008     -PRE ARTC-                                           
046600     05  FILLER                  PIC X(13).                               
046700     EJECT                                                                
046800*01  -COPY W0008     -PRE ARTC2-                                          
046900     05  FILLER                  PIC X(13).                               
047000     EJECT                                                                
047100*01  -COPY W0008     -PRE BEN-                                            
047200     05  FILLER                  PIC X(8).                                
047300     EJECT                                                                
047400*01  -COPY W0008     -PRE LEV-                                            
047500     05  FILLER                  PIC X(5).                                
047600     EJECT                                                                
047700*    -COPY W0008     -PRE INLE-                                           
047800     05  FILLER                  PIC X(13).                               
047900     EJECT                                                                
048000*01  -COPY W0008     -PRE WDK7-                                           
048100     05 FILLER                   PIC X(18).                               
048200     EJECT                                                                
048300*01  -COPY W0008     -PRE FILB-                                           
048400     05 FILLER                   PIC X(18).                               
048500     EJECT                                                                
048600*01  -COPY W0008     -PRE WDB6-                                           
048700     05 FILLER                   PIC X(18).                               
048800     EJECT                                                                
048900*01  -COPY W0008     -PRE WDK6-                                           
049000     05 FILLER                   PIC X(13).                               
049100     EJECT                                                                
049200*01  -COPY W0008     -PRE KOMA-                                           
049300     05  FILLER                  PIC X.                                   
049400     EJECT                                                                
049500*01  -COPY W0008     -PRE WDB2-                                           
049600     05  FILLER                  PIC X.                                   
049700     EJECT                                                                
049800*01  -COPY W0008  -PRE 9305-                                              
049900     05  FILLER                  PIC X.                                   
050000                                                                          
050100 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB USEA-PCB                       
050200                           ARTC-PCB ARTC2-PCB BEN-PCB                     
050300                           LEV-PCB                                        
050400                           INLE-PCB WDK7-PCB                              
050500                           FILB-PCB                                       
050600                           WDB6-PCB WDK6-PCB KOMA-PCB                     
050700                           WDB2-PCB 9305-PCB.                             
050800 MAIN SECTION.                                                            
050900     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB USEA-PCB                       
051000                           ARTC-PCB ARTC2-PCB BEN-PCB                     
051100                           LEV-PCB                                        
051200                           INLE-PCB WDK7-PCB                              
051300                           FILB-PCB                                       
051400                           WDB6-PCB WDK6-PCB KOMA-PCB                     
051500                           WDB2-PCB 9305-PCB.                             
051600     PERFORM IMS-GET-MSG                                                  
051700                                                                          
051800     IF SEGMENT-FINNS                                                     
051900       PERFORM A-INIT                                                     
052000       PERFORM B-KOLLA-NYCKEL                                             
052100       IF MFS-UPD-X                                                       
052200         IF MID-KDPRURSP-U = 'D'                                          
052300           MOVE SPACE TO 50111-MSG-KOM-WMSGKOM                            
052400           PERFORM IMS-GN-MSG-KOM-50111                                   
052500         ELSE                                                             
052600           MOVE SPACE TO 55374-MSG-KOM-WMSGKOM                            
052700           PERFORM IMS-GN-MSG-KOM-55374                                   
052800         END-IF                                                           
052900       END-IF                                                             
053000       IF NYCKLAR-OK = JA                                                 
053100         PERFORM S01-HAMTA-SLAGERSALDO                                    
053200         PERFORM S03-HAMTA-SLAGERSALDO-ALL-DC                             
053300         IF (MFS-UPDATE AND MID-IDARTNR-IN = ALL '+') OR                  
053400            (MFS-UPD-X)                                                   
053500           PERFORM C-UPPDATERA                                            
053600         ELSE                                                             
053700           PERFORM D-SOEKNING                                             
053800         END-IF                                                           
053900       ELSE                                                               
054000         PERFORM MFS-RENSA-FAELT-UTDATA                                   
054100         PERFORM MFS-RENSA-FAELT-INDATA                                   
054200       END-IF                                                             
054300       IF MFS-UPD-X                                                       
054400         IF MID-KDPRURSP-U = 'D'                                          
054500           IF 50111-MSG-KOM-IDMFSMED = SPACE                              
054600             MOVE INF-UPDATE-DONE TO 50111-MSG-KOM-IDMFSMED               
054700           END-IF                                                         
054800           MOVE SPACE             TO 50111-MSG-KOM-KDSVAR                 
054900           PERFORM IMS-INSERT-MSG-KOM-50111                               
055000         ELSE                                                             
055100           IF 55374-MSG-KOM-IDMFSMED = SPACE                              
055200             MOVE INF-UPDATE-DONE TO 55374-MSG-KOM-IDMFSMED               
055300           END-IF                                                         
055400           MOVE SPACE             TO 55374-MSG-KOM-KDSVAR                 
055500           PERFORM IMS-INSERT-MSG-KOM-55374                               
055600         END-IF                                                           
055700       ELSE                                                               
055800           COMPUTE  MSG-KVLL  = LENGTH OF MOD-W5O20601 + 4                
055900           PERFORM IMS-INSERT-MSG                                         
056000       END-IF                                                             
056100                                                                          
056200     MOVE ZERO TO RETURN-CODE                                             
056300     GOBACK                                                               
056400     .                                                                    
056500     EJECT                                                                
056600                                                                          
056700 A-INIT SECTION.                                                          
056800     ACCEPT DAGENS-TID    FROM TIME                                       
056900     MOVE FUNCTION CURRENT-DATE (1:8) TO DAGENS-DAT                       
057000     MOVE FUNCTION CURRENT-DATE (3:6) TO DAGENS-AAMMDD                    
057100**** TA FRAM MÅNADENS FÖRSTA DAG                                          
057200     MOVE FUNCTION CURRENT-DATE (3:4) TO W-DATE-AAMM                      
057300                                                                          
057400     IF MSG-DUBBLA-TRANSKODER                                             
057500        MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W5I20601                
057600        MOVE MSG-IDTRANS-2        TO MFS-IDTRANS                          
057700        MOVE MSG-KDMFSFOR-2       TO MFS-KDMFSFOR                         
057800     ELSE                                                                 
057900        MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W5I20601                 
058000        MOVE MSG-IDTRANS-1        TO MFS-IDTRANS                          
058100        MOVE MSG-KDMFSFOR-1       TO MFS-KDMFSFOR                         
058200     END-IF                                                               
058300                                                                          
058400     MOVE MSG-KDTRTYP             TO MFS-KDTRTYP                          
058500     MOVE MFS-IDTRANS             TO W-IDTRANS                            
058600                                                                          
058700     ACCEPT WS-TIUPPDAT           FROM DATE                               
058800     ACCEPT WS-TIUPPTID           FROM TIME                               
058900                                                                          
059000     MOVE LOW-VALUE       TO MSG-AREA                                     
059100     MOVE 'W5O206N1'      TO MFS-IDMOD                                    
059200     MOVE '5206'          TO MOD-IDTRANS                                  
059300                                                                          
059400     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
059500                             MOD-TEMFSFEL                                 
059600                             MOD-TEMFSINF                                 
059700     .                                                                    
059800     EJECT                                                                
059900                                                                          
060000 B-KOLLA-NYCKEL SECTION.                                                  
060100     IF MFS-UPD-X                                                         
060200****************  DISPATCHANROP                                           
060300       IF MID-IDARTNR-IN = ALL '+'                                        
060400         MOVE MID-IDARTNR-UT TO IDARTNR-WS                                
060500         INSPECT IDARTNR-WS REPLACING LEADING SPACE BY ZERO               
060600       ELSE                                                               
060700         MOVE MID-IDARTNR-IN TO IDARTNR-WS                                
060800       END-IF                                                             
060900       IF MID-IDLEVNR-U NOT = ALL '+'                                     
061000         MOVE MID-IDLEVNR-U TO W-IDLEVNR                                  
061100         PERFORM IMS-GU-WLLEVA01                                          
061200         IF SEGMENT-FINNS                                                 
061300           MOVE MID-IDDC      TO W-IDDC-B6                                
061400           PERFORM IMS-GU-WDB601                                          
061500           MOVE DCS-IDLANDX2 TO W-IDLAND                                  
061600           PERFORM IMS-GNP-WLLEVA11                                       
061700           IF SEGMENT-FINNS                                               
061800             PERFORM UNTIL SEGMENT-SAKNAS                                 
061900              IF SEGMENT-FINNS                                            
062000                 IF LEV-TULL-TITULF < DAGENS-AAMMDD                       
062100                   MOVE LEV-TULL-RETULF-1  TO W-RETULF                    
062200                   MOVE LEV-TULL-RETULF-1  TO MOD-RETULF                  
062300                 ELSE                                                     
062400                   MOVE LEV-TULL-RETULF-2  TO W-RETULF                    
062500                   MOVE LEV-TULL-RETULF-2  TO MOD-RETULF                  
062600                 END-IF                                                   
062700                 PERFORM IMS-GNP-WLLEVA11                                 
062800              END-IF                                                      
062900             END-PERFORM                                                  
063000           ELSE                                                           
063100             MOVE 1                        TO W-RETULF                    
063200           END-IF                                                         
063300         END-IF                                                           
063400       END-IF                                                             
063500       IF MID-KDVALISO-U NOT = ALL '+'                                    
063600         MOVE MID-KDVALISO-U TO WS-KDVALISO-X                             
063700       ELSE                                                               
063800         MOVE MID-IDDC       TO W-IDDC-B6                                 
063900         PERFORM IMS-GU-WDB601                                            
064000         MOVE DCS-KDVALISO   TO WS-KDVALISO-X                             
064100       END-IF                                                             
064200     ELSE                                                                 
064300       MOVE ALL '+'           TO MSGI-WMSGINIT                            
064400       MOVE '001'             TO MSGI-KDCALL                              
064500       MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                              
064600       MOVE '5206'            TO MSGI-IDTRANS                             
064700       MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                        
064800       IF MFS-IDTRANS = '5206'                                            
064900       OR (MID-IDARTNR-IN NUMERIC                                         
065000       AND MID-IDARTNR-IN > ZERO)                                         
065100         MOVE MID-IDARTNR-IN  TO MSGI-IDARTNR                             
065200       END-IF                                                             
065300       IF  MFS-IDTRANS = '5206'                                           
065400       AND MID-IDDC NOT = ALL '+'                                         
065500         MOVE MID-IDDC        TO MSGI-IDDC-KEY                            
065600       END-IF                                                             
065700       CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                         
065800       MOVE MSGI-IDARTNR      TO IDARTNR-WS                               
065900       INSPECT IDARTNR-WS REPLACING ALL SPACE BY ZERO                     
066000       MOVE MSGI-IDLAND-SPR   TO W-IDSKYLT                                
066100     END-IF                                                               
066200                                                                          
066300     IF NOT EGEN-TRANS                                                    
066400       MOVE ' '               TO MFS-KDTRTYP                              
066500     END-IF                                                               
066600                                                                          
066700     IF IDARTNR-WS NOT NUMERIC                                            
066800       MOVE NEJ               TO NYCKLAR-OK                               
066900       MOVE W-FEL-4 TO MOD-TEMFSFEL                                       
067000     ELSE                                                                 
067100       MOVE IDARTNR-WS        TO W-IDARTNR                                
067200     END-IF                                                               
067300                                                                          
067400*    -- KONTROLL AV IDLEVNR                                               
067500     MOVE MFS-RENSA-FAELT TO MOD-IDLEVNR-IN                               
067600                                                                          
067700     IF MID-IDLEVNR-IN = ALL '+'                                          
067800       MOVE MID-IDLEVNR-UT    TO WS-IDLEVNR                               
067900     ELSE                                                                 
068000       MOVE MID-IDLEVNR-IN    TO WS-IDLEVNR                               
068100     END-IF                                                               
068200                                                                          
068300*    -- KONTROLL AV IDDC                                                  
068400     IF MFS-UPD-X                                                         
068500       MOVE MID-IDDC TO WS-IDDC                                           
068600                        MOD-IDDC-UT                                       
068700     ELSE                                                                 
068800       MOVE MFS-RENSA-FAELT TO MOD-IDDC-IN                                
068900                                                                          
069000       MOVE MSGI-IDDC-KEY TO WS-IDDC                                      
069100                             MOD-IDDC-UT                                  
069200*      IF  MFS-IDTRANS = '5206'                                           
069300*      AND MID-IDDC  NOT = ALL '+'                                        
069400*        MOVE MID-IDDC      TO WS-IDDC                                    
069500*                              MOD-IDDC-UT                                
069600*      END-IF                                                             
069700     END-IF                                                               
069800                                                                          
069900*    -- KONTROLL AV KDPRBEH                                               
070000     MOVE MFS-RENSA-FAELT     TO MOD-KDPRBEH-IN                           
070100                                                                          
070200     IF MID-KDPRBEH-IN = ALL '+'                                          
070300       MOVE MID-KDPRBEH-UT    TO WS-KDPRBEH                               
070400     ELSE                                                                 
070500       MOVE MID-KDPRBEH-IN    TO WS-KDPRBEH                               
070600     END-IF                                                               
070700                                                                          
070800*    -- KONTROLL AV KDFPKPRI                                              
070900     IF MID-KDFPKPRI-U = 'Y' OR 'J' OR 'N' OR '?' OR SPACE                
071000       IF MID-KDFPKPRI-U = 'J'                                            
071100         MOVE 'Y'               TO WS-KDFPKPRI                            
071200       ELSE                                                               
071300         MOVE MID-KDFPKPRI-U    TO WS-KDFPKPRI                            
071400       END-IF                                                             
071500     ELSE                                                                 
071600       MOVE SPACE               TO WS-KDFPKPRI                            
071700     END-IF                                                               
071800                                                                          
071900*    -- KONTROLL AV IDFTG                                                 
072000     MOVE WS-IDDC       TO W-IDDC-B6                                      
072100     PERFORM IMS-GU-WDB601                                                
072200     MOVE DCS-IDFTG     TO WS-IDFTG                                       
072300     IF MFS-UPD-X                                                         
072400         MOVE DCS-IDFTG    TO WS-IDFTG                                    
072500         MOVE DCS-IDLANDX2 TO W-IDLANDX2                                  
072600                              W-IDLAND                                    
072700     ELSE                                                                 
072800       IF MSGI-IDFTG NUMERIC                                              
072900       AND MSGI-IDFTG > ZERO                                              
073000         MOVE MSGI-IDFTG    TO WS-IDFTG                                   
073100                                 MOD-IDFTG-UT                             
073200         IF NDC-CN OR LDC-CN                                              
073300           IF IDFTG-CN                                                    
073400             CONTINUE                                                     
073500           ELSE                                                           
073600             MOVE W-FEL-17  TO MOD-TEMFSFEL                               
073700             MOVE JA        TO FLFEL-FAELT                                
073800           END-IF                                                         
073900         ELSE                                                             
074000           IF NDC-NA                                                      
074100             IF IDFTG-US                                                  
074200               CONTINUE                                                   
074300             ELSE                                                         
074400               MOVE W-FEL-17  TO MOD-TEMFSFEL                             
074500               MOVE JA        TO FLFEL-FAELT                              
074600             END-IF                                                       
074700           END-IF                                                         
074800         END-IF                                                           
074900       ELSE                                                               
075000         MOVE W-FEL-17  TO MOD-TEMFSFEL                                   
075100         MOVE JA        TO FLFEL-FAELT                                    
075200       END-IF                                                             
075300     END-IF                                                               
075400*    -- KONTROLL AV REAENDR                                               
075500     MOVE MFS-RENSA-FAELT     TO MOD-REAENDR-IN                           
075600                                                                          
075700     IF MID-REAENDR-IN = ALL '+'                                          
075800       MOVE MID-REAENDR-UT    TO WS-REAENDR                               
075900     ELSE                                                                 
076000       MOVE MID-REAENDR-IN    TO WS-REAENDR                               
076100     END-IF                                                               
076200                                                                          
076300     MOVE WS-REAENDR          TO DEC-IDFRIDATA                            
076400     MOVE 4                   TO DEC-KVHELTAL                             
076500     MOVE 1                   TO DEC-KVDECIMAL                            
076600                                                                          
076700     CALL WDECEDIT USING DEC-WDECAREA                                     
076800                                                                          
076900     IF DEC-KDSVAR-OK                                                     
077000       MOVE DEC-IDEDITDATA    TO WS-REAENDR-NUM                           
077100     ELSE                                                                 
077200       MOVE ZERO              TO WS-REAENDR-NUM                           
077300     END-IF                                                               
077400                                                                          
077500     IF NYCKLAR-OK = NEJ                                                  
077600       IF GODK-TRANS                                                      
077700         MOVE IDARTNR-WS      TO MOD-IDARTNR-UT                           
077800         MOVE WS-IDLEVNR      TO MOD-IDLEVNR-UT                           
077900         MOVE WS-KDPRBEH      TO MOD-KDPRBEH-UT                           
078000         MOVE WS-REAENDR-NUM  TO MOD-REAENDR-UT                           
078100       ELSE                                                               
078200         MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-UT                           
078300                                 MOD-IDLEVNR-UT                           
078400                                 MOD-KDPRBEH-UT                           
078500                                 MOD-REAENDR-UT                           
078600       END-IF                                                             
078700     ELSE                                                                 
078800       MOVE IDARTNR-WS        TO MOD-IDARTNR-UT                           
078900       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
079000       MOVE WS-IDLEVNR        TO MOD-IDLEVNR-UT                           
079100       MOVE WS-KDPRBEH        TO MOD-KDPRBEH-UT                           
079200       MOVE WS-REAENDR-NUM    TO MOD-REAENDR-UT                           
079300     END-IF                                                               
079400     .                                                                    
079500     EJECT                                                                
079600                                                                          
079700 C-UPPDATERA  SECTION.                                                    
079800     PERFORM CA-FORMELL-KONTROLL-MID                                      
079900     IF MFS-UPDATE                                                        
080000       IF FLFEL-FAELT = NEJ                                               
080100         PERFORM CB-KONTROLLERA-MID-MOT-BAS                               
080200       END-IF                                                             
080300     END-IF                                                               
080400                                                                          
080500     PERFORM MFS-ROER-EJ-FAELT-UTDATA                                     
080600     IF FLFEL-FAELT = JA                                                  
080700       PERFORM MFS-ROER-EJ-FAELT-INDATA                                   
080800       PERFORM MFS-ADD-LAES-IN-FAELT-INDATA                               
080900     ELSE                                                                 
081000       IF 50111-MSG-KOM-IDSNDJOB = 'W5011100'                             
081100          PERFORM CF-UPD-WDK712-5111-5206                                 
081200       ELSE                                                               
081300          PERFORM CE-UPPDATERA                                            
081400       END-IF                                                             
081500       PERFORM MFS-FORMATETS-ATTR-INDATA                                  
081600       PERFORM MFS-RENSA-FAELT-INDATA                                     
081700     END-IF                                                               
081800     .                                                                    
081900     EJECT                                                                
082000                                                                          
082100 CA-FORMELL-KONTROLL-MID SECTION.                                         
082200     PERFORM IMS-GU-WDK701                                                
082300     IF SEGMENT-FINNS                                                     
082400       MOVE WS-IDDC TO W-IDDC-WDK7-MIN                                    
082500       MOVE WS-IDDC TO W-IDDC-WDK7-MAX                                    
082600       PERFORM IMS-GU-WDK711-DC                                           
082700                                                                          
082800       IF MFS-UPD-X                                                       
082900         IF SEGMENT-SAKNAS                                                
083000           MOVE ERR-WRONG-KEY TO MSG-KOM-IDMFSMED                         
083100           MOVE JA          TO FLFEL-FAELT                                
083200         ELSE                                                             
083300           IF SEGMENT-FINNS                                               
083400             IF MID-IDLEVNR-U NOT = ALL '+'                               
083500               MOVE MID-IDLEVNR-U TO W-IDLEVNR                            
083600             ELSE                                                         
083700               MOVE ERR-WRONG-KEY TO MSG-KOM-IDMFSMED                     
083800               MOVE JA          TO FLFEL-FAELT                            
083900             END-IF                                                       
084000           END-IF                                                         
084100         END-IF                                                           
084200       ELSE                                                               
084300         IF SEGMENT-FINNS                                                 
084400****          HÄR KONTROLLERAS PRISHÄRSTAMNING               **           
084500           IF MID-KDPRURSP-U = 'F' OR 'B' OR 'A' OR 'P' OR 'Å'            
084600           OR 'C'                                                         
084700*            MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDPRURSP-U-ATTR             
084800             CONTINUE                                                     
084900           ELSE                                                           
085000             MOVE W-FEL-16    TO MOD-TEMFSFEL                             
085100             MOVE JA          TO FLFEL-FAELT                              
085200           END-IF                                                         
085300                                                                          
085400****        HÄR KONTROLLERAS OM ANGIVIT DATUM ÄR ETT         **           
085500****        GILTIGT DATUM                                    **           
085600           IF MID-TIPRLIST-U NOT = ALL '+'                                
085700             MOVE 'AAMMDD'         TO DAT-KDDATFORM                       
085800             MOVE MID-TIPRLIST-U TO DAT-I-TIDATUM                         
085900             CALL WDATKONV USING    DAT-KDDATFORM                         
086000                                    DAT-I-TIDATUM                         
086100                                    DAT-O-TIDATUM                         
086200                                    DAT-KDSVAR                            
086300             IF DAT-KDSVAR-FEL                                            
086400               MOVE MFS-NUM-FAELT-FEL TO MOD-TIPRLIST-U-ATTR              
086500               MOVE W-FEL-13            TO MOD-TEMFSFEL                   
086600               MOVE JA                  TO FLFEL-FAELT                    
086700*            ELSE                                                         
086800*              MOVE MFS-NUM-FAELT-RAETT TO MOD-TIPRLIST-U-ATTR            
086900             END-IF                                                       
087000           ELSE                                                           
087100             MOVE MFS-NUM-FAELT-FEL TO MOD-TIPRLIST-U-ATTR                
087200             MOVE W-FEL-13 TO MOD-TEMFSFEL                                
087300             MOVE JA        TO FLFEL-FAELT                                
087400           END-IF                                                         
087500                                                                          
087600* **          HÄR KONTROLLERAS NYTT BEST.PRIS SÅ ATT ANTALET **           
087700* **          HELTALSSIFFROR OCH ANTALET DECIMALER INTE      **           
087800* **          ÖVERSKRIDER MAXANTAL TILLÅTNA                  **           
087900* **     OBS FÖR ATT KLARA 5 DEC ANVÄNDS WDECSTR I STÄLLET   **           
088000           IF MID-PRARTBEL-U NOT = ALL '+'                                
088100             MOVE MID-PRARTBEL-U TO STR-IDFRIDATA                         
088200             MOVE +8               TO STR-KVHELTAL                        
088300             MOVE +5               TO STR-KVDECIMAL                       
088400             MOVE NEJ              TO STR-KDSIGNAT                        
088500             MOVE NEJ              TO STR-KDLEFTJUST                      
088600             CALL WDECSTR    USING STR-WDECSTR                            
088700             IF STR-KDSVAR-FEL                                            
088800               MOVE MFS-NUM-FAELT-FEL TO MOD-PRARTBEL-U-ATTR              
088900               MOVE W-FEL-18       TO MOD-TEMFSFEL                        
089000               MOVE JA             TO FLFEL-FAELT                         
089100             ELSE                                                         
089200               MOVE STR-IDSTRDATA     TO W-IDSTRDATA-X                    
089300               IF W-IDSTRDATA-N <= 0                                      
089400                 MOVE MFS-NUM-FAELT-FEL TO MOD-PRARTBEL-U-ATTR            
089500                 MOVE W-FEL-18 TO MOD-TEMFSFEL                            
089600                 MOVE JA        TO FLFEL-FAELT                            
089700               ELSE                                                       
089800                 MOVE W-IDSTRDATA-N TO W-PRARTBEL                         
089900*                MOVE MFS-NUM-FAELT-RAETT TO MOD-PRARTBEL-U-ATTR          
090000               END-IF                                                     
090100             END-IF                                                       
090200           ELSE                                                           
090300             MOVE MFS-NUM-FAELT-FEL TO MOD-PRARTBEL-U-ATTR                
090400             MOVE W-FEL-18 TO MOD-TEMFSFEL                                
090500             MOVE JA        TO FLFEL-FAELT                                
090600           END-IF                                                         
090700                                                                          
090800****          HÄR KONTROLLERAS VALUTA                        **           
090900           IF MID-KDVALISO-U NOT = ALL '+'                                
091000**** HÄMTA DC HUVUDVALUTA                                                 
091100             MOVE WS-IDDC               TO W-IDDC-B6                      
091200             PERFORM IMS-GU-WDB601                                        
091300             MOVE DCS-KDVALISO          TO CURR-KDVALISO-HUV              
091400             MOVE MID-KDVALISO-U        TO CURR-KDVALISO-ROW              
091500             MOVE W-DATE-AAMM           TO CURR-TIAAMM                    
091600             MOVE 'M'                   TO CURR-KDVALTYP                  
091700             CALL W510CURR USING CURR-W510CURR 9305-PCB                   
091800             IF CURR-KDSVAR = ' '                                         
091900               CONTINUE                                                   
092000             ELSE                                                         
092100               MOVE MFS-ALFA-FAELT-FEL TO MOD-KDVALISO-U-ATTR             
092200               MOVE W-FEL-19 TO MOD-TEMFSFEL                              
092300               MOVE JA        TO FLFEL-FAELT                              
092400             END-IF                                                       
092500           ELSE                                                           
092600             MOVE MFS-ALFA-FAELT-FEL TO MOD-KDVALISO-U-ATTR               
092700             MOVE W-FEL-19 TO MOD-TEMFSFEL                                
092800             MOVE JA        TO FLFEL-FAELT                                
092900           END-IF                                                         
093000                                                                          
093100****     HÄR KONTROLLERAS IDLEVNR PÅ UPPDATERINGSRADEN                    
093200           IF MID-IDLEVNR-U NOT = ALL '+'                                 
093300             IF MID-IDLEVNR-U = SPACE                                     
093400               MOVE MFS-ALFA-FAELT-FEL TO MOD-IDLEVNR-U-ATTR              
093500               MOVE W-FEL-12   TO MOD-TEMFSFEL                            
093600               MOVE JA        TO FLFEL-FAELT                              
093700             ELSE                                                         
093800               MOVE MID-IDLEVNR-U TO W-IDLEVNR                            
093900*              MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDLEVNR-U-ATTR            
094000             END-IF                                                       
094100           ELSE                                                           
094200             MOVE MFS-ALFA-FAELT-FEL TO MOD-IDLEVNR-U-ATTR                
094300             MOVE W-FEL-12   TO MOD-TEMFSFEL                              
094400             MOVE JA        TO FLFEL-FAELT                                
094500           END-IF                                                         
094600                                                                          
094700*  -- KONTROLL AV KDFPKPRI                                                
094800           IF MID-KDFPKPRI-U = 'Y' OR 'J' OR 'N' OR '?' OR SPACE          
094900             IF MID-KDFPKPRI-U = 'J'                                      
095000               MOVE 'Y'                 TO WS-KDFPKPRI                    
095100             ELSE                                                         
095200               MOVE MID-KDFPKPRI-U      TO WS-KDFPKPRI                    
095300             END-IF                                                       
095400*            MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDFPKPRI-U-ATTR             
095500           ELSE                                                           
095600             MOVE SPACE                 TO WS-KDFPKPRI                    
095700*            MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDFPKPRI-U-ATTR             
095800           END-IF                                                         
095900         ELSE                                                             
096000           MOVE W-FEL-1     TO MOD-TEMFSFEL                               
096100           MOVE JA          TO FLFEL-FAELT                                
096200         END-IF                                                           
096300       END-IF                                                             
096400     ELSE                                                                 
096500       MOVE W-FEL-1     TO MOD-TEMFSFEL                                   
096600       MOVE JA          TO FLFEL-FAELT                                    
096700     END-IF                                                               
096800     .                                                                    
096900     EJECT                                                                
097000                                                                          
097100 CB-KONTROLLERA-MID-MOT-BAS SECTION.                                      
097200     IF MID-FLPRIBES-U NOT = JA                                           
097300* **    HÄR LÄSES BEST.PRIS SEGMENT FÖR ATT                  **           
097400* **    KONTROLLERA OM NYUPPLÄGG ELLER                       **           
097500* **    UPPDATERING ÄR TILLÅTEN FÖR ANGIVET                  **           
097600* **    DATUM                                                **           
097700       MOVE 1 TO INDX                                                     
097800       PERFORM IMS-GHNP-WDK724-FIRST                                      
097900       PERFORM UNTIL SEGMENT-SAKNAS OR INDX > 5 OR                        
098000                     FLSLUTA-LAS = JA                                     
098100         IF SEGMENT-FINNS                                                 
098200           IF SPRL-IDLEVNR-PR = MID-IDLEVNR-U                             
098300             MOVE JA  TO INLEV-PRISRAD                                    
098400             IF SPRL-SUINLEV-PR > 0                                       
098500               ADD +1 TO ANT-INLEV                                        
098600             END-IF                                                       
098700             IF  INDX = 5                                                 
098800             AND SPRL-SUINLEV-PR > 0                                      
098900             AND ANT-INLEV = 1                                            
099000               MOVE MFS-NUM-FAELT-FEL TO MOD-TIPRLIST-U-ATTR              
099100               MOVE W-FEL-3 TO MOD-TEMFSFEL                               
099200               MOVE JA      TO FLFEL-FAELT                                
099300               MOVE JA      TO FLSLUTA-LAS                                
099400             ELSE                                                         
099500               SUBTRACT SPRL-DAPRLIST-9KOMPL FROM W-DAPRLIST-MAX          
099600               GIVING W-DAPRLIST                                          
099700               MOVE W-LISTDATUM      TO W-TIPRLIST                        
099800               MOVE MID-TIPRLIST-U   TO TMP1-YYMMDD                       
099900               MOVE W-TIPRLIST       TO TMP2-YYMMDD                       
100000               PERFORM WY2000P1                                           
100100               IF TMP1-YYMMDD > TMP2-YYMMDD                               
100200                 MOVE MFS-NUM-FAELT-RAETT TO MOD-TIPRLIST-U-ATTR          
100300               ELSE                                                       
100400                 IF SPRL-SUINLEV-PR > 0                                   
100500                   MOVE MFS-NUM-FAELT-FEL TO MOD-TIPRLIST-U-ATTR          
100600                   MOVE W-FEL-3 TO MOD-TEMFSFEL                           
100700                   MOVE JA      TO FLFEL-FAELT                            
100800                   MOVE JA      TO FLSLUTA-LAS                            
100900                 ELSE                                                     
101000                   IF MID-TIPRLIST-U = W-TIPRLIST                         
101100                     MOVE MFS-NUM-FAELT-RAETT TO                          
101200                              MOD-TIPRLIST-U-ATTR                         
101300*                    MOVE JA TO FLSLUTA-LAS                               
101400                   END-IF                                                 
101500                 END-IF                                                   
101600               END-IF                                                     
101700             END-IF                                                       
101800             ADD 1 TO INDX                                                
101900           ELSE                                                           
102000             CONTINUE                                                     
102100           END-IF                                                         
102200         ELSE                                                             
102300           MOVE MFS-NUM-FAELT-RAETT TO MOD-TIPRLIST-U-ATTR                
102400         END-IF                                                           
102500         PERFORM IMS-GHNP-WDK724-NEXT                                     
102600       END-PERFORM                                                        
102700       IF  INDX = 5                                                       
102800       AND ANT-INLEV = 0                                                  
102900         MOVE MFS-NUM-FAELT-RAETT TO MOD-TIPRLIST-U-ATTR                  
103000       END-IF                                                             
103100     END-IF                                                               
103200                                                                          
103300* ** HÄR KONTROLLERAS ATT KURS FINNS UPPLAGD FÖR       **                 
103400* ** INMATAD VALUTA ELLER ATT KURS FINNS FÖR VALUTA    **                 
103500* ** SOM GÄLLER FÖR INKÖPSLAND.                        **                 
103600*    MOVE DAT-TIAA             TO TMP1-YY                                 
103700*    MOVE DAGENS-DAT(3:2)      TO TMP2-YY                                 
103800*    PERFORM WY2000P9                                                     
103900*    IF TMP1-YY < TMP2-YY                                                 
104000*      MOVE DAGENS-DAT(3:2)    TO W-WDGXTIAA                              
104100*    ELSE                                                                 
104200*      MOVE DAT-TIAA           TO W-WDGXTIAA                              
104300*    END-IF                                                               
104400                                                                          
104500**** HÄMTA DC HUVUDVALUTA                                                 
104600     MOVE WS-IDDC               TO W-IDDC-B6                              
104700     PERFORM IMS-GU-WDB601                                                
104800     MOVE DCS-KDVALISO          TO CURR-KDVALISO-HUV                      
104900                                   W-KDVALISO-HUV                         
105000     IF MID-KDVALISO-U NOT = ALL '+'                                      
105100       MOVE MID-KDVALISO-U      TO CURR-KDVALISO-ROW                      
105200     ELSE                                                                 
105300       MOVE MFS-ALFA-FAELT-FEL  TO MOD-KDVALISO-U-ATTR                    
105400       MOVE W-FEL-14            TO MOD-TEMFSFEL                           
105500       MOVE JA                  TO FLFEL-FAELT                            
105600     END-IF                                                               
105700     MOVE W-DATE-AAMM           TO CURR-TIAAMM                            
105800     MOVE 'M'                   TO CURR-KDVALTYP                          
105900     CALL W510CURR USING CURR-W510CURR 9305-PCB                           
106000     IF CURR-KDSVAR = ' '                                                 
106100       MOVE CURR-PRKURS-NEW     TO W-PRKURS                               
106200       MOVE CURR-REVALUTA-TO    TO W-REVALUTA                             
106300     ELSE                                                                 
106400       MOVE W-FEL-14            TO MOD-TEMFSFEL                           
106500       MOVE JA                  TO FLFEL-FAELT                            
106600     END-IF                                                               
106700                                                                          
106800****** KOLLA OM MID-IDLEVR FINNS PÅ WDF1-LEVERANTÖRSREG                   
106900     IF MID-IDLEVNR-U NOT = ALL '+'                                       
107000       MOVE MID-IDLEVNR-U         TO W-IDLEVNR                            
107100       PERFORM IMS-GU-WLLEVA01                                            
107200       IF SEGMENT-SAKNAS                                                  
107300         MOVE MFS-ALFA-FAELT-FEL  TO MOD-IDLEVNR-U-ATTR                   
107400         MOVE W-FEL-12            TO MOD-TEMFSFEL                         
107500         MOVE JA                  TO FLFEL-FAELT                          
107600       ELSE                                                               
107700**** HÄMTA NYTT RETULF OM MID-IDLEVNR-U ÄR IFYLLT ****                    
107800         IF SEGMENT-FINNS                                                 
107900           MOVE DCS-IDLANDX2 TO W-IDLAND                                  
108000           PERFORM IMS-GNP-WLLEVA11                                       
108100           IF SEGMENT-FINNS                                               
108200             IF LEV-TULL-TITULF < DAGENS-AAMMDD                           
108300               MOVE LEV-TULL-RETULF-1  TO W-RETULF                        
108400               MOVE LEV-TULL-RETULF-1  TO MOD-RETULF                      
108500             ELSE                                                         
108600               MOVE LEV-TULL-RETULF-2  TO W-RETULF                        
108700               MOVE LEV-TULL-RETULF-2  TO MOD-RETULF                      
108800             END-IF                                                       
108900           ELSE                                                           
109000             MOVE MFS-RENSA-FAELT  TO MOD-RETULF                          
109100             MOVE W-FEL-20            TO MOD-TEMFSFEL                     
109200             MOVE JA                  TO FLFEL-FAELT                      
109300           END-IF                                                         
109400         END-IF                                                           
109500       END-IF                                                             
109600     ELSE                                                                 
109700       MOVE MFS-ALFA-FAELT-FEL  TO MOD-IDLEVNR-U-ATTR                     
109800       MOVE W-FEL-12            TO MOD-TEMFSFEL                           
109900       MOVE JA                  TO FLFEL-FAELT                            
110000     END-IF                                                               
110100                                                                          
110200                                                                          
110300* **    HÄR KONTROLLERAS BEST.PRIS          SÅ ATT    **                  
110400* **    DET INTE ÖVERSKRIDER 400 000 I LOKAL VALUTA   **                  
110500     IF FLFEL-FAELT = NEJ                                                 
110600       COMPUTE W-PRARTBES ROUNDED = W-PRARTBEL * W-RETULF                 
110700                                  * W-PRKURS / W-REVALUTA                 
110800       IF W-PRARTBES = +0                                                 
110900         MOVE +0.01      TO W-PRARTBES                                    
111000       END-IF                                                             
111100       IF W-PRARTBES > MAX-PRARTBES                                       
111200         MOVE MFS-NUM-FAELT-FEL TO MOD-PRARTBEL-U-ATTR                    
111300         MOVE MFS-ALFA-FAELT-FEL TO MOD-KDVALISO-U-ATTR                   
111400         MOVE W-FEL-7 TO MOD-TEMFSFEL                                     
111500         MOVE JA      TO FLFEL-FAELT                                      
111600       END-IF                                                             
111700     END-IF                                                               
111800                                                                          
111900     .                                                                    
112000     EJECT                                                                
112100                                                                          
112200 CE-UPPDATERA SECTION.                                                    
112300     IF MFS-UPD-X                                                         
112400       PERFORM CEA-REDIGERA                                               
112500     END-IF                                                               
112600     PERFORM IMS-GU-WDK701                                                
112700     IF SEGMENT-FINNS                                                     
112800       MOVE WS-IDDC       TO W-IDDC-B6                                    
112900       PERFORM IMS-GU-WDB601                                              
113000       IF SEGMENT-FINNS                                                   
113100         MOVE DCS-IDLANDX2 TO W-IDLANDX2                                  
113200         PERFORM IMS-GNP-WDK712                                           
113300         IF SEGMENT-FINNS                                                 
113400           PERFORM CED-KOLLA-UPPDATERING                                  
113500           PERFORM CEB-UPPDATERA-WDK724                                   
113600           IF UPPDATERING-OK = 'J'                                        
113700           AND INLEV-FINNS = 'N'                                          
113800             PERFORM R-CALCULATE-SJK                                      
113900             PERFORM CEC-UPPDATERA-WDK712                                 
114000           END-IF                                                         
114100           IF UPPDATERING-OK = 'J'                                        
114200             MOVE W-MED-1                TO MOD-TEMFSFEL                  
114300           END-IF                                                         
114400           IF UPPDATERING-OK = 'J'                                        
114500             MOVE SLAG-IDDC              TO PRTR-IDDC                     
114600             MOVE IDARTNR-WS             TO PRTR-IDARTNR                  
114700             MOVE 010                    TO PRTR-KDCALL                   
114800             CALL W510PRTR USING PRTR-W510PRTR WDK6-PCB                   
114900             IF PRTR-KDSVAR = '1'                                         
115000             AND (DCS-FTG-CN OR DCS-FTG-US)                               
115100**** EXPORT TILL SVERIGE FRÅN KINA                                        
115200               PERFORM E-SKAPA-TRANS-5111                                 
115300               PERFORM F-SKICKA-TRANS                                     
115400             ELSE                                                         
115500               MOVE SLAG-IDDC            TO PRTR-IDDC                     
115600               MOVE IDARTNR-WS           TO PRTR-IDARTNR                  
115700               MOVE 030                  TO PRTR-KDCALL                   
115800               CALL W510PRTR USING PRTR-W510PRTR WDK6-PCB                 
115900               IF PRTR-KDSVAR = '1'                                       
116000               AND (DCS-FTG-CN OR DCS-FTG-US)                             
116100****   TIPPAT PRIS SKALL UPPDATERAS                                       
116200                 PERFORM J-SKAPA-TRANS-5111                               
116300                 PERFORM F-SKICKA-TRANS                                   
116400               END-IF                                                     
116500             END-IF                                                       
116600           END-IF                                                         
116700         END-IF                                                           
116800       END-IF                                                             
116900     END-IF                                                               
117000     .                                                                    
117100     EJECT                                                                
117200                                                                          
117300 CEA-REDIGERA  SECTION.                                                   
117400     IF MID-TIPRLIST-U NOT = ALL '+'                                      
117500       IF W-PRARTBEL = +0                                                 
117600         MOVE MID-PRARTBEL-U TO STR-IDFRIDATA                             
117700         MOVE +8             TO STR-KVHELTAL                              
117800         MOVE +5             TO STR-KVDECIMAL                             
117900         MOVE NEJ            TO STR-KDSIGNAT                              
118000         MOVE JA             TO STR-KDLEFTJUST                            
118100         CALL WDECSTR  USING STR-WDECSTR                                  
118200         IF STR-KDSVAR-OK                                                 
118300           MOVE STR-IDSTRDATA  TO W-IDSTRDATA-X                           
118400           MOVE W-IDSTRDATA-N  TO W-PRARTBEL                              
118500         END-IF                                                           
118600       END-IF                                                             
118700       IF W-PRKURS = +0                                                   
118800                                                                          
118900         IF MID-IDLEVNR-U NOT = ALL '+'                                   
119000           MOVE MID-IDLEVNR-U TO W-IDLEVNR                                
119100           PERFORM IMS-GU-WLLEVA01                                        
119200           IF SEGMENT-FINNS                                               
119300             MOVE WS-IDDC       TO W-IDDC-B6                              
119400             PERFORM IMS-GU-WDB601                                        
119500             MOVE DCS-IDLANDX2 TO W-IDLAND                                
119600             PERFORM IMS-GNP-WLLEVA11                                     
119700             IF SEGMENT-FINNS                                             
119800               IF LEV-TULL-TITULF < DAGENS-AAMMDD                         
119900                 MOVE LEV-TULL-RETULF-1  TO W-RETULF                      
120000                 MOVE LEV-TULL-RETULF-1  TO MOD-RETULF                    
120100               ELSE                                                       
120200                 MOVE LEV-TULL-RETULF-2  TO W-RETULF                      
120300                 MOVE LEV-TULL-RETULF-2  TO MOD-RETULF                    
120400               END-IF                                                     
120500             ELSE                                                         
120600               MOVE MFS-RENSA-FAELT  TO MOD-RETULF                        
120700             END-IF                                                       
120800           END-IF                                                         
120900         END-IF                                                           
121000                                                                          
121100         IF MID-KDVALISO-U NOT = ALL '+'                                  
121200           MOVE MID-KDVALISO-U      TO W-IDDC-B6                          
121300                                                                          
121400           PERFORM IMS-GU-WDB601                                          
121500           MOVE DCS-KDVALISO        TO CURR-KDVALISO-HUV                  
121600                                       W-KDVALISO-HUV                     
121700           MOVE MID-KDVALISO-U      TO WS-KDVALISO                        
121800                                                                          
121900           IF CURR-KDVALISO-HUV = WS-KDVALISO                             
122000             MOVE 1                 TO W-PRKURS                           
122100             MOVE 1                 TO W-REVALUTA                         
122200           ELSE                                                           
122300             MOVE WS-KDVALISO       TO CURR-KDVALISO-ROW                  
122400             MOVE W-DATE-AAMM       TO CURR-TIAAMM                        
122500             MOVE 'M'               TO CURR-KDVALTYP                      
122600             CALL W510CURR USING CURR-W510CURR 9305-PCB                   
122700             IF CURR-KDSVAR = ' '                                         
122800               MOVE CURR-PRKURS-NEW  TO W-PRKURS                          
122900               MOVE CURR-REVALUTA-TO TO W-REVALUTA                        
123000             ELSE                                                         
123100               MOVE 1               TO W-PRKURS                           
123200               MOVE 1               TO W-REVALUTA                         
123300             END-IF                                                       
123400           END-IF                                                         
123500         END-IF                                                           
123600       END-IF                                                             
123700                                                                          
123800       IF W-REVALUTA = 0                                                  
123900         MOVE +1                    TO W-REVALUTA                         
124000       END-IF                                                             
124100                                                                          
124200       COMPUTE W-PRARTBES ROUNDED = W-PRARTBEL * W-RETULF                 
124300                                    * W-PRKURS / W-REVALUTA               
124400                                                                          
124500       IF W-PRARTBES = +0                                                 
124600         MOVE +0.01                 TO W-PRARTBES                         
124700       END-IF                                                             
124800     END-IF                                                               
124900       .                                                                  
125000     EJECT                                                                
125100                                                                          
125200 CEB-UPPDATERA-WDK724   SECTION.                                          
125300     PERFORM IMS-GU-WDK701                                                
125400     IF SEGMENT-FINNS                                                     
125500       MOVE WS-IDDC TO W-IDDC-WDK7-MIN                                    
125600       MOVE WS-IDDC TO W-IDDC-WDK7-MAX                                    
125700       PERFORM IMS-GU-WDK711-DC                                           
125800       IF SEGMENT-FINNS                                                   
125900         MOVE 0 TO INDX                                                   
126000         MOVE NEJ TO FLSLUTA-LAS                                          
126100         PERFORM IMS-GHNP-WDK724-FIRST                                    
126200         PERFORM UNTIL INDX = 5 OR FLSLUTA-LAS = JA                       
126300           IF SEGMENT-FINNS                                               
126400             IF SPRL-IDLEVNR-PR = MID-IDLEVNR-U                           
126500               MOVE MID-TIPRLIST-U  TO WS-DATUM(3:6)                      
126600               IF WS-DATUM(3:2) < 50                                      
126700                 MOVE 20            TO WS-DATUM(1:2)                      
126800               ELSE                                                       
126900                 MOVE 19            TO WS-DATUM(1:2)                      
127000               END-IF                                                     
127100               SUBTRACT WS-DATUM FROM W-DAPRLIST-MAX                      
127200               GIVING WS-DAPRLIST-9KOMPL                                  
127300               IF SPRL-DAPRLIST-9KOMPL = WS-DAPRLIST-9KOMPL               
127400                 MOVE JA  TO FLSLUTA-LAS                                  
127500                 MOVE NEJ TO UPPDATERING-OK                               
127600               ELSE                                                       
127700                 ADD +1 TO INDX                                           
127800                 IF INDX = 1                                              
127900                 MOVE SPRL-DAPRLIST-9KOMPL TO WS-DAPRLIST-9KOMPL-1        
128000                 END-IF                                                   
128100               END-IF                                                     
128200             ELSE                                                         
128300               CONTINUE                                                   
128400             END-IF                                                       
128500           ELSE                                                           
128600*     ** SEGMENT-SAKNAS                                                   
128700*     ** NYUPPLÄGG AV BEST. PRIS                                          
128800             MOVE MID-TIPRLIST-U  TO WS-DATUM(3:6)                        
128900             IF WS-DATUM(3:2) < 50                                        
129000               MOVE 20            TO WS-DATUM(1:2)                        
129100             ELSE                                                         
129200               MOVE 19            TO WS-DATUM(1:2)                        
129300             END-IF                                                       
129400             SUBTRACT WS-DATUM FROM W-DAPRLIST-MAX                        
129500             GIVING SPRL-DAPRLIST-9KOMPL                                  
129600             IF INDX = 0                                                  
129700               MOVE SPRL-DAPRLIST-9KOMPL TO WS-DAPRLIST-9KOMPL-1          
129800             END-IF                                                       
129900             MOVE SPRL-DAPRLIST-9KOMPL TO WS-DAPRLIST-9KOMPL-2            
130000             PERFORM S724-NYA-SEGMENT-WDK724                              
130100             MOVE JA              TO FLSLUTA-LAS                          
130200             IF MFS-UPDATE                                                
130300               PERFORM S-LAS-VISA-PRISRADER                               
130400             END-IF                                                       
130500           END-IF                                                         
130600****  ** FEM SEGMENT FINNS. DET ÄLDSTA PLOCKAS BORT FÖRE        **        
130700****  ** NYUPPLÄGG.                                             **        
130800           IF INDX = 5                                                    
130900           AND FLSLUTA-LAS = NEJ                                          
131000             PERFORM IMS-DLET-WDK724                                      
131100             MOVE MID-TIPRLIST-U TO WS-DATUM(3:6)                         
131200             IF WS-DATUM(3:2) < 50                                        
131300               MOVE 20          TO WS-DATUM(1:2)                          
131400             ELSE                                                         
131500               MOVE 19          TO WS-DATUM(1:2)                          
131600             END-IF                                                       
131700             SUBTRACT WS-DATUM FROM W-DAPRLIST-MAX                        
131800             GIVING SPRL-DAPRLIST-9KOMPL                                  
131900             MOVE SPRL-DAPRLIST-9KOMPL TO WS-DAPRLIST-9KOMPL-2            
132000             PERFORM S724-NYA-SEGMENT-WDK724                              
132100             IF MFS-UPDATE                                                
132200               PERFORM S-LAS-VISA-PRISRADER                               
132300             END-IF                                                       
132400             MOVE JA              TO FLSLUTA-LAS                          
132500           ELSE                                                           
132600             PERFORM IMS-GHNP-WDK724-NEXT                                 
132700           END-IF                                                         
132800         END-PERFORM                                                      
132900       END-IF                                                             
133000     END-IF                                                               
133100     .                                                                    
133200     EJECT                                                                
133300                                                                          
133400 CEC-UPPDATERA-WDK712 SECTION.                                            
133500     PERFORM IMS-GU-WDK701                                                
133600     IF SEGMENT-FINNS                                                     
133700       MOVE WS-IDDC       TO W-IDDC-B6                                    
133800       PERFORM IMS-GU-WDB601                                              
133900       IF SEGMENT-FINNS                                                   
134000         MOVE DCS-IDLANDX2 TO W-IDLANDX2                                  
134100         PERFORM IMS-GHNP-WDK712                                          
134200         IF SEGMENT-FINNS                                                 
134300**** PRARTBES ÄR I LOKAL VALUTA OCH PRMATRL SKALL VARA I SEK              
134400**** PRMATRL SKALL INTE JUSTERAS OM DET FINNS SALDO ELLER                 
134500**** PRMATRL REDAN HAR ETT VÄRDE                                          
134600             IF  W-SDCSALDO-ALL-DC   = ZERO                               
134700               MOVE DAT-TIAA       TO TMP1-YY                             
134800               MOVE DAGENS-DAT(3:2) TO TMP2-YY                            
134900               PERFORM WY2000P9                                           
135000               IF TMP1-YY < TMP2-YY                                       
135100                 MOVE DAGENS-DAT(3:2) TO W-DATE-AAMM(1:2)                 
135200               ELSE                                                       
135300                 MOVE DAT-TIAA     TO W-DATE-AAMM(1:2)                    
135400               END-IF                                                     
135500               MOVE 01             TO W-DATE-AAMM(3:2)                    
135600*MID-KDVALISO-U IS MOVED TO WS-KDVALISO                                   
135700*              MOVE WS-KDVALISO    TO W-KDVALISO-Y                        
135800               MOVE 'SEK'          TO CURR-KDVALISO-HUV                   
135900               MOVE W-KDVALISO-HUV TO CURR-KDVALISO-ROW                   
136000               MOVE W-DATE-AAMM    TO CURR-TIAAMM                         
136100               MOVE 'A'            TO CURR-KDVALTYP                       
136200               CALL W510CURR USING CURR-W510CURR 9305-PCB                 
136300               IF CURR-KDSVAR = ' '                                       
136400                 MOVE CURR-PRKURS-NEW  TO W-PRKURS-SEK                    
136500                 MOVE CURR-REVALUTA-TO   TO W-REVALUTA-SEK                
136600               ELSE                                                       
136700                 MOVE 1            TO W-PRKURS-SEK                        
136800                 MOVE 1            TO W-REVALUTA-SEK                      
136900               END-IF                                                     
137000               COMPUTE LART-PRMATRL ROUNDED = W-PRARTBES *                
137100                        W-PRKURS-SEK / W-REVALUTA-SEK                     
137200               MOVE LART-PRMATRL TO MOD-PRMATRL                           
137300             END-IF                                                       
137400           IF WS-DAPRLIST-9KOMPL-1 > WS-DAPRLIST-9KOMPL-2                 
137500           OR WS-DAPRLIST-9KOMPL-1 = WS-DAPRLIST-9KOMPL-2                 
137600             MOVE W-PRARTSJK TO LART-PRARTSJK                             
137700                                MOD-PRARTSJK                              
137800           END-IF                                                         
137900           PERFORM IMS-REPL-WDK712                                        
138000         END-IF                                                           
138100       END-IF                                                             
138200     END-IF                                                               
138300     .                                                                    
138400     EJECT                                                                
138500                                                                          
138600 CED-KOLLA-UPPDATERING SECTION.                                           
138700     PERFORM IMS-GU-WDK701                                                
138800     IF SEGMENT-FINNS                                                     
138900       MOVE WS-IDDC TO W-IDDC-WDK7-MIN                                    
139000       MOVE WS-IDDC TO W-IDDC-WDK7-MAX                                    
139100       PERFORM IMS-GU-WDK711-DC                                           
139200       IF SEGMENT-FINNS                                                   
139300         PERFORM IMS-GHNP-WDK724-FIRST                                    
139400         IF SEGMENT-FINNS                                                 
139500           IF SPRL-SUINLEV-PR > 0                                         
139600             MOVE JA TO INLEV-FINNS                                       
139700           END-IF                                                         
139800         END-IF                                                           
139900         PERFORM UNTIL SEGMENT-SAKNAS                                     
140000           IF SPRL-SUINLEV-PR > 0                                         
140100             MOVE JA TO INLEV-FINNS                                       
140200           END-IF                                                         
140300           PERFORM IMS-GHNP-WDK724-NEXT                                   
140400         END-PERFORM                                                      
140500       END-IF                                                             
140600     END-IF                                                               
140700     .                                                                    
140800     EJECT                                                                
140900                                                                          
141000 CF-UPD-WDK712-5111-5206 SECTION.                                         
141100     PERFORM CFA-CONVERT-PRARTBEL                                         
141200     PERFORM IMS-GU-WDK701                                                
141300     IF SEGMENT-FINNS                                                     
141400       PERFORM IMS-GHNP-WDK712                                            
141500       IF SEGMENT-FINNS                                                   
141600**** PRARTBES ÄR I LOKAL VALUTA OCH PRMATRL SKALL VARA I SEK              
141700**** PRMATRL SKALL INTE JUSTERAS OM DET FINNS SALDO ELLER                 
141800**** PRMATRL REDAN HAR ETT VÄRDE                                          
141900         IF W-SDCSALDO-ALL-DC   = ZERO                                    
142000           COMPUTE LART-PRMATRL ROUNDED = W-PRARTBEL * W-RETULF           
142100           MOVE LART-PRMATRL TO MOD-PRMATRL                               
142200           PERFORM IMS-REPL-WDK712                                        
142300         END-IF                                                           
142400       END-IF                                                             
142500     ELSE                                                                 
142600       IF SEGMENT-SAKNAS                                                  
142700           MOVE ERR-WRONG-KEY TO 50111-MSG-KOM-IDMFSMED                   
142800           MOVE JA            TO FLFEL-FAELT                              
142900       END-IF                                                             
143000     END-IF                                                               
143100     .                                                                    
143200     EJECT                                                                
143300 CFA-CONVERT-PRARTBEL SECTION.                                            
143400     MOVE 0 TO W-PRARTBEL                                                 
143500     MOVE MID-PRARTBEL-U TO STR-IDFRIDATA                                 
143600     MOVE +8             TO STR-KVHELTAL                                  
143700     MOVE +5             TO STR-KVDECIMAL                                 
143800     MOVE NEJ            TO STR-KDSIGNAT                                  
143900     MOVE JA             TO STR-KDLEFTJUST                                
144000     CALL WDECSTR  USING STR-WDECSTR                                      
144100     IF STR-KDSVAR-OK                                                     
144200       MOVE STR-IDSTRDATA  TO W-IDSTRDATA-X                               
144300       MOVE W-IDSTRDATA-N  TO W-PRARTBEL                                  
144400     END-IF                                                               
144500     .                                                                    
144600     EJECT                                                                
144700 D-SOEKNING  SECTION.                                                     
144800     IF (MID-IDARTNR-IN = ALL '+'                                         
144900     AND MID-IDDC = ALL '+')                                              
145000     AND MID-UPPDAT-RAD NOT = ALL '+'                                     
145100     AND EGEN-TRANS                                                       
145200       MOVE W-FEL-5 TO MOD-TEMFSFEL                                       
145300       PERFORM MFS-ROER-EJ-FAELT-UTDATA                                   
145400       PERFORM MFS-ROER-EJ-FAELT-INDATA                                   
145500       PERFORM MFS-ADD-LAES-IN-FAELT-INDATA                               
145600     ELSE                                                                 
145700       PERFORM IMS-GHU-WLARTC01                                           
145800       IF SEGMENT-FINNS                                                   
145900         PERFORM IMS-GU-WDK701                                            
146000         IF SEGMENT-FINNS                                                 
146100           MOVE WS-IDDC TO W-IDDC-WDK7-MIN                                
146200           MOVE WS-IDDC TO W-IDDC-WDK7-MAX                                
146300           PERFORM IMS-GU-WDK711-DC                                       
146400           IF SEGMENT-FINNS                                               
146500             MOVE SLAG-PRAVCOST TO MOD-PRAVCOST                           
146600             MOVE SLAG-KVPB-REF TO MOD-KVPB-TOT                           
146700             MOVE W-SDCSALDO    TO MOD-KVLS-TOT                           
146800             PERFORM DA-BEHANDLA-OVRIGA-SEGMENT                           
146900           ELSE                                                           
147000             MOVE W-FEL-1 TO MOD-TEMFSFEL                                 
147100             PERFORM MFS-RENSA-FAELT-UTDATA                               
147200           END-IF                                                         
147300         ELSE                                                             
147400           MOVE W-FEL-1 TO MOD-TEMFSFEL                                   
147500           PERFORM MFS-RENSA-FAELT-UTDATA                                 
147600         END-IF                                                           
147700       ELSE                                                               
147800         MOVE W-FEL-1 TO MOD-TEMFSFEL                                     
147900         PERFORM MFS-RENSA-FAELT-UTDATA                                   
148000       END-IF                                                             
148100       PERFORM MFS-RENSA-FAELT-INDATA                                     
148200     END-IF                                                               
148300     .                                                                    
148400     EJECT                                                                
148500                                                                          
148600 DA-BEHANDLA-OVRIGA-SEGMENT SECTION.                                      
148700     PERFORM S-LAS-VISA-PRISRADER                                         
148800                                                                          
148900     PERFORM IMS-GU-WLBENA11                                              
149000                                                                          
149100     IF SEGMENT-FINNS                                                     
149200       MOVE BEN-TEXT-BEART  TO MOD-BEART                                  
149300     ELSE                                                                 
149400       MOVE MFS-RENSA-FAELT TO MOD-BEART                                  
149500     END-IF                                                               
149600                                                                          
149700     PERFORM DAD-HAMTA-RETULF                                             
149800     PERFORM DAE-HAMTA-SJK                                                
149900     .                                                                    
150000     EJECT                                                                
150100                                                                          
150200 DAD-HAMTA-RETULF SECTION.                                                
150300     MOVE WS-IDLEVNR-FIRST TO W-IDLEVNR                                   
150400     PERFORM IMS-GU-WLLEVA01                                              
150500     IF SEGMENT-FINNS                                                     
150600       MOVE WS-IDDC       TO W-IDDC-B6                                    
150700       PERFORM IMS-GU-WDB601                                              
150800       MOVE DCS-IDLANDX2 TO W-IDLAND                                      
150900       PERFORM IMS-GNP-WLLEVA11                                           
151000       IF SEGMENT-FINNS                                                   
151100         IF LEV-TULL-TITULF < DAGENS-AAMMDD                               
151200           MOVE LEV-TULL-RETULF-1 TO MOD-RETULF                           
151300                                     WS-RETULF                            
151400         ELSE                                                             
151500           MOVE LEV-TULL-RETULF-2 TO MOD-RETULF                           
151600                                     WS-RETULF                            
151700         END-IF                                                           
151800       ELSE                                                               
151900         MOVE MFS-RENSA-FAELT  TO MOD-RETULF                              
152000         MOVE 0                TO MOD-RETULF                              
152100       END-IF                                                             
152200     END-IF                                                               
152300     .                                                                    
152400     EJECT                                                                
152500                                                                          
152600 DAE-HAMTA-SJK    SECTION.                                                
152700**** HÄMTA PÅLÄGG FRÅN 5102 BILDEN                                        
152800     PERFORM IMS-GHU-WLARTC01                                             
152900     IF SEGMENT-FINNS                                                     
153000       PERFORM IMS-GHNP-WLARTC11                                          
153100       IF SEGMENT-FINNS                                                   
153200         CONTINUE                                                         
153300       ELSE                                                               
153400         MOVE 0 TO CLAG-PRDIRLON                                          
153500         MOVE 0 TO CLAG-PRDMTRL                                           
153600       END-IF                                                             
153700     ELSE                                                                 
153800       MOVE 0 TO CLAG-PRDIRLON                                            
153900       MOVE 0 TO CLAG-PRDMTRL                                             
154000     END-IF                                                               
154100**** HÄMTA PROCENTSATSEN FRÅN 4405 BILDEN                                 
154200     MOVE WS-IDDC       TO W-IDDC-B6                                      
154300     PERFORM IMS-GU-WDB601                                                
154400     MOVE DCS-IDLANDX2 TO W-IDLANDX2                                      
154500     IF SEGMENT-FINNS                                                     
154600       PERFORM IMS-GNP-WDB617                                             
154700       IF SEGMENT-FINNS                                                   
154800**** HÄMTA VALUTAKURS FÖR LOKAL VALUTA, PRISERNA ÄR I SEK I CLAG          
154900         MOVE DAT-TIAA             TO TMP1-YY                             
155000         MOVE DAGENS-DAT(3:2)      TO TMP2-YY                             
155100         PERFORM WY2000P9                                                 
155200         IF TMP1-YY < TMP2-YY                                             
155300           MOVE DAGENS-DAT(3:2)    TO W-DATE-AAMM(1:2)                    
155400         ELSE                                                             
155500           MOVE DAT-TIAA           TO W-DATE-AAMM(1:2)                    
155600         END-IF                                                           
155700         MOVE 01                   TO W-DATE-AAMM(3:2)                    
155800         MOVE 'SEK'                TO CURR-KDVALISO-HUV                   
155900         MOVE W-KDVALISO-HUV       TO CURR-KDVALISO-ROW                   
156000         MOVE W-DATE-AAMM          TO CURR-TIAAMM                         
156100         MOVE 'A'                  TO CURR-KDVALTYP                       
156200         CALL W510CURR USING CURR-W510CURR 9305-PCB                       
156300         IF CURR-KDSVAR = ' '                                             
156400           MOVE CURR-PRKURS-NEW    TO W-PRKURS-SEK                        
156500           MOVE CURR-REVALUTA-TO   TO W-REVALUTA-SEK                      
156600         ELSE                                                             
156700           MOVE 1                  TO W-PRKURS-SEK                        
156800           MOVE 1                  TO W-REVALUTA-SEK                      
156900         END-IF                                                           
157000         COMPUTE W-PRDIRLON ROUNDED = CLAG-PRDIRLON *                     
157100                 PROC-REDIRLON * W-REVALUTA-SEK / W-PRKURS-SEK            
157200         COMPUTE W-PRDMTRL  ROUNDED = CLAG-PRDMTRL  *                     
157300                 PROC-REDMTRL  * W-REVALUTA-SEK / W-PRKURS-SEK            
157400         MOVE W-PRDIRLON TO MOD-PRDIRLON                                  
157500         MOVE W-PRDMTRL  TO MOD-PRDMTRL                                   
157600       ELSE                                                               
157700         MOVE 0 TO CLAG-PRDIRLON                                          
157800         MOVE 0 TO CLAG-PRDMTRL                                           
157900         MOVE CLAG-PRDIRLON TO MOD-PRDIRLON                               
158000         MOVE CLAG-PRDMTRL  TO MOD-PRDMTRL                                
158100       END-IF                                                             
158200     END-IF                                                               
158300**** HÄMTA PRIS FÖR SJK OCH MTRL                                          
158400     PERFORM IMS-GU-WDK701                                                
158500     IF SEGMENT-FINNS                                                     
158600       PERFORM IMS-GNP-WDK712                                             
158700       IF SEGMENT-FINNS                                                   
158800         MOVE LART-PRARTSJK TO MOD-PRARTSJK                               
158900         MOVE LART-PRMATRL TO MOD-PRMATRL                                 
159000       ELSE                                                               
159100         MOVE W-FEL-15    TO MOD-TEMFSFEL                                 
159200       END-IF                                                             
159300     ELSE                                                                 
159400       MOVE W-FEL-15    TO MOD-TEMFSFEL                                   
159500     END-IF                                                               
159600     .                                                                    
159700     EJECT                                                                
159800                                                                          
159900 E-SKAPA-TRANS-5111 SECTION.                                              
160000     MOVE WS-IDDC       TO W-IDDC-B6                                      
160100     PERFORM IMS-GU-WDB601                                                
160200     IF SEGMENT-FINNS                                                     
160300       MOVE DCS-KDVALISO TO W-KDVALISO-HUV                                
160400     END-IF                                                               
160500     MOVE   SPACE                TO MSG-KOM-WMSGKOM                       
160600     MOVE   +54                  TO MSG-KOM-KVLL                          
160700     MOVE   LOW-VALUE            TO MSG-KOM-KDZ1                          
160800     MOVE   LOW-VALUE            TO MSG-KOM-KDZ2                          
160900     MOVE   SPACE                TO MSG-KOM-KDTRANS                       
161000                                                                          
161100     MOVE   SPACE                TO MSG-KOM-IDMFSMED                      
161200                                    MSG-KOM-KDSVAR                        
161300                                                                          
161400     MOVE 'W5I11101'             TO MSG-KOM-IDCPYTXT                      
161500     MOVE 'IPRIS'                TO MSG-KOM-IDSNDNOD                      
161600     MOVE 'W5020600'             TO MSG-KOM-IDSNDJOB                      
161700     ACCEPT MSG-KOM-TIREGDAT  FROM DATE                                   
161800     ACCEPT MSG-KOM-TIKLOCK   FROM TIME                                   
161900                                                                          
162000     COMPUTE P-TO-P-LL = LENGTH OF 5111-W5I11101 + 17                     
162100     MOVE LOW-VALUE               TO P-TO-P-Z1                            
162200     MOVE LOW-VALUE               TO P-TO-P-Z2                            
162300     MOVE 'W5T111X'               TO P-TO-P-TRANSKOD                      
162400     MOVE '5111'                  TO P-TO-P-FROM-MID                      
162500     MOVE '1'                     TO P-TO-P-KDMFSFOR                      
162600                                                                          
162700     MOVE IDARTNR-WS              TO 5111-IDARTNR-IN                      
162800                                                                          
162900****************** DETTA ÄR INLAGT BARA FÖR ATT FYLLA MIDDEN              
163000     MOVE '+++++'                 TO 5111-IDLEVNR-IN                      
163100     MOVE '+'                     TO 5111-KDPRBEH-IN                      
163200     MOVE '++++++'                TO 5111-REAENDR-IN                      
163300                                                                          
163400     MOVE SPACE                   TO 5111-IDLEVNR-UT                      
163500                                     5111-REAENDR-UT                      
163600     MOVE SPACE                   TO 5111-KDPRBEH-UT                      
163700************************************************************              
163800                                                                          
163900     MOVE 'C'                     TO 5111-KDPRURSP-U                      
164000     MOVE MSGI-IDUSER             TO 5111-IDUSER                          
164100                                                                          
164200     MOVE MID-TIPRLIST-U          TO WS-TIPRLIST-6                        
164300     MOVE WS-TIPRLIST-6           TO 5111-TIPRLIST-U                      
164400                                                                          
164500     PERFORM H-GET-REEMBHNT                                               
164600     PERFORM I-GET-RELANDCO-EXP                                           
164700     COMPUTE WS-PRARTBES = 100000 * W-PRARTBES                            
164800     COMPUTE WS-PRARTBES ROUNDED = WS-PRARTBES +                          
164900             WS-PRARTBES * WS-REEMBHNT +                                  
165000             WS-PRARTBES * WS-REEMBHNT * WS-RELANDCO-EXP                  
165100     MOVE WS-PRARTBES(1:8)        TO 5111-PRARTBEL-U(1:8)                 
165200     MOVE '.'                     TO 5111-PRARTBEL-U(9:1)                 
165300     MOVE WS-PRARTBES(9:5)        TO 5111-PRARTBEL-U(10:5)                
165400                                                                          
165500     PERFORM G-GET-RETULF                                                 
165600                                                                          
165700                                                                          
165800     MOVE W-KDVALISO-HUV          TO 5111-KDVALISO-U                      
165900     IF DCS-FTG-CN                                                        
166000       MOVE 'CHN07'               TO 5111-IDLEVNR-U                       
166100     END-IF                                                               
166200     IF DCS-FTG-US                                                        
166300       MOVE '63517'               TO 5111-IDLEVNR-U                       
166400     END-IF                                                               
166500     MOVE WS-KDFPKPRI             TO 5111-KDFPKPRI-U                      
166600                                                                          
166700     MOVE '+'                     TO 5111-FLPRIBES-U                      
166800                                     5111-FLPRIGO-U                       
166900                                                                          
167000     MOVE 5111-W5I11101           TO P-TO-P-DATA                          
167100     .                                                                    
167200     EJECT                                                                
167300                                                                          
167400 F-SKICKA-TRANS SECTION.                                                  
167500     CALL W006KOM USING MSG-PCB                                           
167600                        ALT-PCB                                           
167700                        KOMA-PCB                                          
167800                        MSG-KOM-WMSGKOM                                   
167900                        P-TO-P-AREA                                       
168000                                                                          
168100     .                                                                    
168200     EJECT                                                                
168300                                                                          
168400 G-GET-RETULF SECTION.                                                    
168500****** KOLLA OM CHN07/63517 FINNS PÅ WDF1-LEVERANTÖRSREG                  
168600     IF DCS-FTG-CN                                                        
168700       MOVE 'CHN07'             TO W-IDLEVNR                              
168800     END-IF                                                               
168900     IF DCS-FTG-US                                                        
169000       MOVE '63517'             TO W-IDLEVNR                              
169100     END-IF                                                               
169200     PERFORM IMS-GU-WLLEVA01                                              
169300     IF SEGMENT-SAKNAS                                                    
169400       MOVE 1                   TO W-RETULF                               
169500     ELSE                                                                 
169600**** ALLTID FRÅN DC 71 I DETTA FALL ****                                  
169700       IF DCS-FTG-CN                                                      
169800         MOVE '71'          TO W-IDDC-B6                                  
169900       END-IF                                                             
170000       IF DCS-FTG-US                                                      
170100         MOVE '41'          TO W-IDDC-B6                                  
170200       END-IF                                                             
170300       PERFORM IMS-GU-WDB601                                              
170400       MOVE DCS-IDLANDX2 TO W-IDLAND                                      
170500       PERFORM IMS-GNP-WLLEVA11                                           
170600       IF SEGMENT-FINNS                                                   
170700         IF LEV-TULL-TITULF < DAGENS-AAMMDD                               
170800           MOVE LEV-TULL-RETULF-1  TO W-RETULF                            
170900         ELSE                                                             
171000           MOVE LEV-TULL-RETULF-2  TO W-RETULF                            
171100         END-IF                                                           
171200       ELSE                                                               
171300         MOVE 1                    TO W-RETULF                            
171400       END-IF                                                             
171500     END-IF                                                               
171600     .                                                                    
171700     EJECT                                                                
171800                                                                          
171900 H-GET-REEMBHNT SECTION.                                                  
172000     MOVE 9111 TO W-IDDISTR-MIN                                           
172100     MOVE 9111 TO W-IDDISTR-MAX                                           
172200     MOVE ZERO TO W-IDKUNDNR-MIN                                          
172300     PERFORM IMS-GU-WDB201-FIRST                                          
172400     IF SEGMENT-SAKNAS                                                    
172500       MOVE 0                   TO WS-REEMBHNT                            
172600     ELSE                                                                 
172700       COMPUTE WS-REEMBHNT = WDB2-GMT-REEMBHNT / 100                      
172800     END-IF                                                               
172900     .                                                                    
173000     EJECT                                                                
173100                                                                          
173200 I-GET-RELANDCO-EXP SECTION.                                              
173300     IF DCS-FTG-CN                                                        
173400       MOVE '71'        TO W-IDDC-B6                                      
173500     END-IF                                                               
173600     IF DCS-FTG-US                                                        
173700       MOVE '41'        TO W-IDDC-B6                                      
173800     END-IF                                                               
173900     MOVE 0             TO WS-RELANDCO-EXP                                
174000     PERFORM IMS-GU-WDB601                                                
174100     IF SEGMENT-SAKNAS                                                    
174200       CONTINUE                                                           
174300     ELSE                                                                 
174400       MOVE DCS-IDLANDX2 TO W-IDLANDX2                                    
174500       PERFORM IMS-GNP-WDB617                                             
174600       IF SEGMENT-SAKNAS                                                  
174700         CONTINUE                                                         
174800       ELSE                                                               
174900         COMPUTE WS-RELANDCO-EXP = PROC-RELANDCO-EXP - 1                  
175000       END-IF                                                             
175100     END-IF                                                               
175200     .                                                                    
175300     EJECT                                                                
175400                                                                          
175500 J-SKAPA-TRANS-5111 SECTION.                                              
175600     MOVE WS-IDDC       TO W-IDDC-B6                                      
175700     PERFORM IMS-GU-WDB601                                                
175800     IF SEGMENT-FINNS                                                     
175900       MOVE DCS-KDVALISO TO W-KDVALISO-HUV                                
176000     END-IF                                                               
176100     MOVE   SPACE                TO MSG-KOM-WMSGKOM                       
176200     MOVE   +54                  TO MSG-KOM-KVLL                          
176300     MOVE   LOW-VALUE            TO MSG-KOM-KDZ1                          
176400     MOVE   LOW-VALUE            TO MSG-KOM-KDZ2                          
176500     MOVE   SPACE                TO MSG-KOM-KDTRANS                       
176600                                                                          
176700     MOVE   SPACE                TO MSG-KOM-IDMFSMED                      
176800                                    MSG-KOM-KDSVAR                        
176900                                                                          
177000     MOVE 'W5I11101'             TO MSG-KOM-IDCPYTXT                      
177100     MOVE 'IPRIS'                TO MSG-KOM-IDSNDNOD                      
177200     MOVE 'W5020600'             TO MSG-KOM-IDSNDJOB                      
177300     ACCEPT MSG-KOM-TIREGDAT  FROM DATE                                   
177400     ACCEPT MSG-KOM-TIKLOCK   FROM TIME                                   
177500                                                                          
177600     COMPUTE P-TO-P-LL = LENGTH OF 5111-W5I11101 + 17                     
177700     MOVE LOW-VALUE               TO P-TO-P-Z1                            
177800     MOVE LOW-VALUE               TO P-TO-P-Z2                            
177900     MOVE 'W5T111X'               TO P-TO-P-TRANSKOD                      
178000     MOVE '5111'                  TO P-TO-P-FROM-MID                      
178100     MOVE '1'                     TO P-TO-P-KDMFSFOR                      
178200                                                                          
178300     MOVE IDARTNR-WS              TO 5111-IDARTNR-IN                      
178400                                                                          
178500****************** DETTA ÄR INLAGT BARA FÖR ATT FYLLA MIDDEN              
178600     MOVE '+++++'                 TO 5111-IDLEVNR-IN                      
178700     MOVE '+'                     TO 5111-KDPRBEH-IN                      
178800     MOVE '++++++'                TO 5111-REAENDR-IN                      
178900                                                                          
179000     MOVE SPACE                   TO 5111-IDLEVNR-UT                      
179100                                     5111-REAENDR-UT                      
179200     MOVE SPACE                   TO 5111-KDPRBEH-UT                      
179300************************************************************              
179400                                                                          
179500     MOVE 'C'                     TO 5111-KDPRURSP-U                      
179600     MOVE MSGI-IDUSER             TO 5111-IDUSER                          
179700                                                                          
179800     MOVE MID-TIPRLIST-U          TO WS-TIPRLIST-6                        
179900     MOVE WS-TIPRLIST-6           TO 5111-TIPRLIST-U                      
180000                                                                          
180100     PERFORM H-GET-REEMBHNT                                               
180200     PERFORM I-GET-RELANDCO-EXP                                           
180300     COMPUTE WS-PRARTBES = 100000 * W-PRARTBES                            
180400     COMPUTE WS-PRARTBES ROUNDED = WS-PRARTBES +                          
180500             WS-PRARTBES * WS-REEMBHNT +                                  
180600             WS-PRARTBES * WS-REEMBHNT * WS-RELANDCO-EXP                  
180700     MOVE WS-PRARTBES(1:8)        TO 5111-PRARTBEL-U(1:8)                 
180800     MOVE '.'                     TO 5111-PRARTBEL-U(9:1)                 
180900     MOVE WS-PRARTBES(9:5)        TO 5111-PRARTBEL-U(10:5)                
181000                                                                          
181100     PERFORM G-GET-RETULF                                                 
181200                                                                          
181300     MOVE W-KDVALISO-HUV          TO 5111-KDVALISO-U                      
181400     IF DCS-FTG-CN                                                        
181500       MOVE 'CHN07'               TO 5111-IDLEVNR-U                       
181600     END-IF                                                               
181700     IF DCS-FTG-US                                                        
181800       MOVE '63517'               TO 5111-IDLEVNR-U                       
181900     END-IF                                                               
182000     MOVE WS-KDFPKPRI             TO 5111-KDFPKPRI-U                      
182100                                                                          
182200**** DETTA ÄR FÖRSTA ÄNDRINGEN FÖR ATT UPPDATERA TIPPAT PRIS              
182300     MOVE NEJ                     TO 5111-FLPRIBES-U                      
182400     MOVE '+'                     TO 5111-FLPRIGO-U                       
182500                                                                          
182600     MOVE 5111-W5I11101           TO P-TO-P-DATA                          
182700     .                                                                    
182800     EJECT                                                                
182900                                                                          
183000 R-CALCULATE-SJK SECTION.                                                 
183100     PERFORM IMS-GHU-WLARTC01                                             
183200     IF SEGMENT-FINNS                                                     
183300       PERFORM IMS-GHNP-WLARTC11                                          
183400       IF SEGMENT-FINNS                                                   
183500         MOVE WS-IDDC       TO W-IDDC-B6                                  
183600         PERFORM IMS-GU-WDB601                                            
183700         IF SEGMENT-FINNS                                                 
183800           MOVE DCS-IDLANDX2 TO W-IDLANDX2                                
183900           PERFORM IMS-GNP-WDB617                                         
184000           IF SEGMENT-FINNS                                               
184100****   HÄMTA VALUTAKURS FÖR LOKAL VALUTA, PRISERNA ÄR I SEK I CLAG        
184200             MOVE DAT-TIAA         TO TMP1-YY                             
184300             MOVE DAGENS-DAT(3:2)  TO TMP2-YY                             
184400             PERFORM WY2000P9                                             
184500             IF TMP1-YY < TMP2-YY                                         
184600               MOVE DAGENS-DAT(3:2) TO W-DATE-AAMM(1:2)                   
184700             ELSE                                                         
184800               MOVE DAT-TIAA       TO W-DATE-AAMM(1:2)                    
184900             END-IF                                                       
185000             MOVE 01               TO W-DATE-AAMM(3:2)                    
185100             MOVE 'SEK'            TO CURR-KDVALISO-HUV                   
185200             MOVE W-KDVALISO-HUV   TO CURR-KDVALISO-ROW                   
185300             MOVE W-DATE-AAMM         TO CURR-TIAAMM                      
185400             MOVE 'A'                 TO CURR-KDVALTYP                    
185500             CALL W510CURR USING CURR-W510CURR 9305-PCB                   
185600             IF CURR-KDSVAR = ' '                                         
185700               MOVE CURR-PRKURS-NEW  TO W-PRKURS-SEK                      
185800               MOVE CURR-REVALUTA-TO   TO W-REVALUTA-SEK                  
185900             ELSE                                                         
186000               MOVE 1              TO W-PRKURS-SEK                        
186100               MOVE 1              TO W-REVALUTA-SEK                      
186200             END-IF                                                       
186300             COMPUTE W-PRDIRLON ROUNDED = CLAG-PRDIRLON *                 
186400                     PROC-REDIRLON * W-REVALUTA-SEK / W-PRKURS-SEK        
186500             COMPUTE W-PRDMTRL  ROUNDED = CLAG-PRDMTRL  *                 
186600                     PROC-REDMTRL  * W-REVALUTA-SEK / W-PRKURS-SEK        
186700****   HÄMTA VALUTAKURS , PRISERNA ÄR I LOKAL VALUTA FÖR PRARTBES         
186800****   OCH SKALL RÄKNAS OM TILL SEK                                       
186900             COMPUTE W-PRARTSJK ROUNDED = (W-PRARTBES +                   
187000                         W-PRDIRLON + W-PRDMTRL) *                        
187100                         W-PRKURS-SEK / W-REVALUTA-SEK                    
187200             MOVE W-PRDIRLON        TO MOD-PRDIRLON                       
187300             MOVE W-PRDMTRL         TO MOD-PRDMTRL                        
187400           END-IF                                                         
187500         END-IF                                                           
187600       END-IF                                                             
187700     END-IF                                                               
187800     .                                                                    
187900     EJECT                                                                
188000                                                                          
188100 S-LAS-VISA-PRISRADER SECTION.                                            
188200     MOVE 1 TO INDX                                                       
188300     PERFORM IMS-GHNP-WDK724-FIRST                                        
188400     IF SEGMENT-FINNS                                                     
188500       MOVE SPRL-PRARTBEL-PR TO WS-PRARTBEL-PR                            
188600       MOVE SPRL-PRARTBES-PR TO WS-PRARTBES-PR                            
188700                                MOD-PRARTBES                              
188800       MOVE SPRL-KDVALISO    TO WS-KDVALISO                               
188900       SUBTRACT SPRL-DAPRLIST-9KOMPL FROM W-DAPRLIST-MAX                  
189000       GIVING WS-DATUM-FIRST                                              
189100       MOVE SPRL-IDLEVNR-PR  TO WS-IDLEVNR-FIRST                          
189200       IF SPRL-SUINLEV-PR > 0                                             
189300         MOVE JA TO INLEV-FINNS                                           
189400       END-IF                                                             
189500     END-IF                                                               
189600     PERFORM UNTIL INDX > 5                                               
189700       IF SEGMENT-FINNS                                                   
189800         SUBTRACT SPRL-DAPRLIST-9KOMPL FROM W-DAPRLIST-MAX                
189900         GIVING WS-DATUM                                                  
190000         MOVE WS-DATUM(3:6)          TO MOD-TIPRLIST-PR (INDX)            
190100         MOVE SPRL-IDLEVNR-PR        TO MOD-IDLEVNR-PR (INDX)             
190200*        MOVE SPRL-PRARTBES-PR       TO MOD-PRARTBES-PR (INDX)            
190300         MOVE SPRL-PRARTBEL-PR       TO MOD-PRARTBEL-PR (INDX)            
190400                                        W-PRARTBEL                        
190500**** RÄKNA OM PRISRADER MED AKTUELL MÅNADSKURS                            
190600         MOVE WS-IDDC   TO W-IDDC-B6                                      
190700         PERFORM IMS-GU-WDB601                                            
190800         MOVE DCS-KDVALISO  TO CURR-KDVALISO-HUV                          
190900                               W-KDVALISO-HUV                             
191000         MOVE SPRL-KDVALISO TO WS-KDVALISO                                
191100         IF CURR-KDVALISO-HUV = WS-KDVALISO                               
191200           MOVE 1 TO W-PRKURS                                             
191300           MOVE 1 TO W-REVALUTA                                           
191400         ELSE                                                             
191500           MOVE WS-KDVALISO    TO CURR-KDVALISO-ROW                       
191600           MOVE W-DATE-AAMM    TO CURR-TIAAMM                             
191700           MOVE 'M'            TO CURR-KDVALTYP                           
191800           CALL W510CURR USING CURR-W510CURR 9305-PCB                     
191900           IF CURR-KDSVAR = ' '                                           
192000             MOVE CURR-PRKURS-NEW  TO W-PRKURS                            
192100             MOVE CURR-REVALUTA-TO TO W-REVALUTA                          
192200           ELSE                                                           
192300             MOVE 1            TO W-PRKURS                                
192400             MOVE 1            TO W-REVALUTA                              
192500           END-IF                                                         
192600         END-IF                                                           
192700         MOVE SPRL-IDLEVNR-PR     TO W-IDLEVNR                            
192800         PERFORM IMS-GU-WLLEVA01                                          
192900         IF SEGMENT-SAKNAS                                                
193000           MOVE 1 TO W-RETULF                                             
193100         ELSE                                                             
193200           MOVE DCS-IDLANDX2 TO W-IDLAND                                  
193300           PERFORM IMS-GNP-WLLEVA11                                       
193400           IF SEGMENT-FINNS                                               
193500             IF LEV-TULL-TITULF < DAGENS-AAMMDD                           
193600               MOVE LEV-TULL-RETULF-1 TO W-RETULF                         
193700             ELSE                                                         
193800               MOVE LEV-TULL-RETULF-2 TO W-RETULF                         
193900             END-IF                                                       
194000           ELSE                                                           
194100             MOVE 1 TO W-RETULF                                           
194200           END-IF                                                         
194300         END-IF                                                           
194400         COMPUTE WS-PRARTBES-PR ROUNDED = W-PRARTBEL * W-RETULF           
194500                                        * W-PRKURS / W-REVALUTA           
194600         MOVE WS-PRARTBES-PR TO MOD-PRARTBES-PR (INDX)                    
194700                                                                          
194800****                                                                      
194900         IF SPRL-SUINLEV-PR > 0                                           
195000           MOVE 'DEL  '              TO MOD-KDSTATUS-PR (INDX)            
195100           IF INLEV-FINNS = 'J'                                           
195200             CONTINUE                                                     
195300           ELSE                                                           
195400             MOVE WS-PRARTBES-PR   TO MOD-PRARTBES                        
195500             MOVE JA TO INLEV-FINNS                                       
195600           END-IF                                                         
195700         ELSE                                                             
195800           MOVE 'EST '               TO MOD-KDSTATUS-PR (INDX)            
195900         END-IF                                                           
196000         MOVE SPRL-KDPRURSP          TO MOD-KDPRURSP-PR (INDX)            
196100         MOVE SPRL-KDVALISO          TO MOD-KDVALISO-PR (INDX)            
196200         MOVE SPRL-KDFPKPRI          TO MOD-KDFPKPRI-PR (INDX)            
196300         IF MFS-UPDATE                                                    
196400           IF MID-TIPRLIST-U = W-TIPRLIST                                 
196500             MOVE MFS-ADD-LYS-UPP-FAELT TO                                
196600                  MOD-BEST-PRIS-ATTR (INDX)                               
196700           END-IF                                                         
196800         END-IF                                                           
196900         ADD 1 TO INDX                                                    
197000         IF INDX < 6                                                      
197100           PERFORM IMS-GHNP-WDK724-NEXT                                   
197200         END-IF                                                           
197300       ELSE                                                               
197400         PERFORM UNTIL INDX > 5                                           
197500           MOVE MFS-RENSA-FAELT TO MOD-BEST-PRIS (INDX)                   
197600           ADD 1 TO INDX                                                  
197700         END-PERFORM                                                      
197800       END-IF                                                             
197900     END-PERFORM                                                          
198000     PERFORM S02-KOLLA-PRARTBES                                           
198100     .                                                                    
198200     EJECT                                                                
198300                                                                          
198400 S01-HAMTA-SLAGERSALDO  SECTION.                                          
198500     PERFORM IMS-GU-WDK701                                                
198600     IF STATUS-WS = '  '                                                  
198700       MOVE WS-IDDC TO W-IDDC-WDK7-MIN                                    
198800       MOVE WS-IDDC TO W-IDDC-WDK7-MAX                                    
198900       PERFORM IMS-GU-WDK711-DC                                           
199000       IF SEGMENT-FINNS                                                   
199100*      PERFORM IMS-GNP-WDK711                                             
199200*      PERFORM UNTIL STATUS-WS = 'GE'                                     
199300*       IF NOT DCS-IDDC = SLAG-IDDC                                       
199400*          MOVE SLAG-IDDC       TO W-IDDC-B6                              
199500*          PERFORM IMS-GU-WDB601                                          
199600*       END-IF                                                            
199700*       IF DCS-IDFTG    = MSGI-IDFTG                                      
199800          COMPUTE W-SDCSALDO = W-SDCSALDO + SLAG-KVLS +                   
199900                  SLAG-KVEFRS + SLAG-KVAKS-PAV + SLAG-KVAKS-SDC           
200000        END-IF                                                            
200100*       END-IF                                                            
200200*       PERFORM IMS-GNP-WDK711                                            
200300*      END-PERFORM                                                        
200400     END-IF                                                               
200500     .                                                                    
200600     EJECT                                                                
200700                                                                          
200800 S02-KOLLA-PRARTBES    SECTION.                                           
200900     MOVE NEJ TO INLEV-FINNS                                              
201000     PERFORM IMS-GU-WDK701                                                
201100     IF SEGMENT-FINNS                                                     
201200       MOVE WS-IDDC TO W-IDDC-WDK7-MIN                                    
201300       MOVE WS-IDDC TO W-IDDC-WDK7-MAX                                    
201400       PERFORM IMS-GU-WDK711-DC                                           
201500       IF SEGMENT-FINNS                                                   
201600         PERFORM IMS-GHNP-WDK724-FIRST                                    
201700         IF SEGMENT-FINNS                                                 
201800           IF SPRL-SUINLEV-PR > 0                                         
201900             MOVE JA TO INLEV-FINNS                                       
202000             MOVE SPRL-PRARTBES-PR TO MOD-PRARTBES                        
202100           END-IF                                                         
202200         END-IF                                                           
202300         PERFORM UNTIL SEGMENT-SAKNAS OR INLEV-FINNS = 'J'                
202400           IF SPRL-SUINLEV-PR > 0                                         
202500             MOVE JA TO INLEV-FINNS                                       
202600             MOVE SPRL-PRARTBES-PR TO MOD-PRARTBES                        
202700           END-IF                                                         
202800           PERFORM IMS-GHNP-WDK724-NEXT                                   
202900         END-PERFORM                                                      
203000       END-IF                                                             
203100     END-IF                                                               
203200     .                                                                    
203300     EJECT                                                                
203400                                                                          
203500 S03-HAMTA-SLAGERSALDO-ALL-DC  SECTION.                                   
203600     MOVE WS-IDDC           TO WS-IDDC-SPAR                               
203700     EVALUATE TRUE                                                        
203800      WHEN NDC-US                                                         
203900        MOVE  'US'           TO IDLAND-FL-SW                              
204000      WHEN NDC-CA                                                         
204100        MOVE  'CA'           TO IDLAND-FL-SW                              
204200      WHEN NDC-CN                                                         
204300        MOVE  'CN'           TO IDLAND-FL-SW                              
204400     END-EVALUATE                                                         
204500     PERFORM IMS-GU-WDK701                                                
204600       IF SEGMENT-FINNS                                                   
204700         PERFORM IMS-GNP-WDK711                                           
204800           IF SEGMENT-FINNS                                               
204900             PERFORM UNTIL SEGMENT-SAKNAS                                 
205000               MOVE SLAG-IDDC TO WS-IDDC                                  
205100               IF  IDLAND-US-DC   AND NDC-US                              
205200                COMPUTE W-SDCSALDO-ALL-DC = W-SDCSALDO-ALL-DC +           
205300                      SLAG-KVLS + SLAG-KVEFRS + SLAG-KVAKS-PAV +          
205400                                              SLAG-KVAKS-SDC              
205500               ELSE                                                       
205600                 IF IDLAND-CN-DC  AND (NDC-CN OR LDC-CN)                  
205700                  COMPUTE W-SDCSALDO-ALL-DC = W-SDCSALDO-ALL-DC +         
205800                    SLAG-KVLS + SLAG-KVEFRS + SLAG-KVAKS-PAV +            
205900                                              SLAG-KVAKS-SDC              
206000                 ELSE                                                     
206100                  IF  IDLAND-CA-DC   AND NDC-CA                           
206200                   COMPUTE W-SDCSALDO-ALL-DC =                            
206300                        SLAG-KVLS + SLAG-KVEFRS + SLAG-KVAKS-PAV +        
206400                                                  SLAG-KVAKS-SDC          
206500                  ELSE                                                    
206600                    IF XDC-NON-VCC-OWNED                                  
206700                     COMPUTE W-SDCSALDO-ALL-DC =                          
206800                        SLAG-KVLS + SLAG-KVEFRS + SLAG-KVAKS-PAV +        
206900                                                  SLAG-KVAKS-SDC          
207000                    END-IF                                                
207100                  END-IF                                                  
207200                 END-IF                                                   
207300               END-IF                                                     
207400               PERFORM IMS-GNP-WDK711                                     
207500             END-PERFORM                                                  
207600           END-IF                                                         
207700       END-IF                                                             
207800     MOVE WS-IDDC-SPAR TO WS-IDDC                                         
207900     .                                                                    
208000     EJECT                                                                
208100                                                                          
208200 S724-NYA-SEGMENT-WDK724   SECTION.                                       
208300     MOVE ALL '+'              TO WDK7-W005WDK7                           
208400     MOVE 'WDK724'             TO WDK7-IDSEGM                             
208500     MOVE W-IDARTNR-X          TO WDK7-IDARTNR-KFB                        
208600     MOVE WS-IDDC              TO WDK7-IDDC-KFB                           
208700     MOVE SPRL-DAPRLIST-9KOMPL TO WDK7-DAPRLIST-9KOMPL                    
208800     MOVE W-PRARTBES           TO WDK7-PRARTBES-PR                        
208900     MOVE W-PRARTBEL           TO WDK7-PRARTBEL-PR                        
209000     MOVE MID-KDVALISO-U       TO WDK7-KDVALISO                           
209100     MOVE W-IDLEVNR            TO WDK7-IDLEVNR-PR                         
209200     MOVE WS-KDFPKPRI          TO WDK7-KDFPKPRI                           
209300     MOVE MID-KDPRURSP-U       TO WDK7-KDPRURSP                           
209400     MOVE ZERO                 TO WDK7-SUINLEV-PR                         
209500     MOVE DAGENS-AAMMDD        TO WDK7-TIREGDAT                           
209600     MOVE MSGI-IDUSER          TO WDK7-IDUSER                             
209700                                                                          
209800     CALL W005WDK7 USING WDK7-W005WDK7 WDB6-PCB ARTC-PCB WDK7-PCB         
209900     .                                                                    
210000     EJECT                                                                
210100                                                                          
210200 MFS-FORMATETS-ATTR-INDATA SECTION.                                       
210300     MOVE MFS-FORMATETS-ATTR TO    MOD-KDPRURSP-U-ATTR                    
210400                                   MOD-TIPRLIST-U-ATTR                    
210500                                   MOD-PRARTBEL-U-ATTR                    
210600                                   MOD-KDVALISO-U-ATTR                    
210700                                   MOD-FLPRIBES-U-ATTR                    
210800                                   MOD-IDLEVNR-U-ATTR                     
210900                                   MOD-KDFPKPRI-U-ATTR                    
211000     .                                                                    
211100     SKIP3                                                                
211200                                                                          
211300 MFS-ADD-LAES-IN-FAELT-INDATA SECTION.                                    
211400     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDPRURSP-U-ATTR                    
211500                                   MOD-TIPRLIST-U-ATTR                    
211600                                   MOD-PRARTBEL-U-ATTR                    
211700                                   MOD-KDVALISO-U-ATTR                    
211800                                   MOD-FLPRIBES-U-ATTR                    
211900                                   MOD-IDLEVNR-U-ATTR                     
212000                                   MOD-KDFPKPRI-U-ATTR                    
212100     .                                                                    
212200     EJECT                                                                
212300                                                                          
212400 MFS-STAENG-FAELT-INDATA SECTION.                                         
212500     MOVE MFS-STAENG-FAELT TO      MOD-KDPRURSP-U-ATTR                    
212600                                   MOD-TIPRLIST-U-ATTR                    
212700                                   MOD-PRARTBEL-U-ATTR                    
212800                                   MOD-KDVALISO-U-ATTR                    
212900                                   MOD-FLPRIBES-U-ATTR                    
213000                                   MOD-IDLEVNR-U-ATTR                     
213100                                   MOD-KDFPKPRI-U-ATTR                    
213200     .                                                                    
213300     SKIP3                                                                
213400                                                                          
213500 MFS-OEPPNA-FAELT-INDATA SECTION.                                         
213600     MOVE MFS-OEPPNA-NUM-FAELT  TO MOD-TIPRLIST-U-ATTR                    
213700                                   MOD-PRARTBEL-U-ATTR                    
213800     MOVE MFS-OEPPNA-ALFA-FAELT TO MOD-KDPRURSP-U-ATTR                    
213900                                   MOD-KDVALISO-U-ATTR                    
214000                                   MOD-FLPRIBES-U-ATTR                    
214100                                   MOD-IDLEVNR-U-ATTR                     
214200                                   MOD-KDFPKPRI-U-ATTR                    
214300     .                                                                    
214400     EJECT                                                                
214500                                                                          
214600 MFS-ROER-EJ-FAELT-UTDATA SECTION.                                        
214700     MOVE MFS-ROER-EJ-FAELT TO  MOD-BEART                                 
214800                                MOD-PRAVCOST                              
214900                                MOD-PRMATRL                               
215000                                MOD-PRARTBES                              
215100                                MOD-PRARTSJK                              
215200                                MOD-PRDIRLON                              
215300                                MOD-PRDMTRL                               
215400                                MOD-KVLS-TOT                              
215500                                MOD-KVPB-TOT                              
215600                                MOD-BEST-PRIS(1)                          
215700                                MOD-BEST-PRIS(2)                          
215800                                MOD-BEST-PRIS(3)                          
215900                                MOD-BEST-PRIS(4)                          
216000                                MOD-BEST-PRIS(5)                          
216100                                MOD-RETULF                                
216200     .                                                                    
216300     EJECT                                                                
216400                                                                          
216500 MFS-ROER-EJ-FAELT-INDATA SECTION.                                        
216600     MOVE MFS-ROER-EJ-FAELT TO  MOD-KDPRURSP-U                            
216700                                MOD-TIPRLIST-U                            
216800                                MOD-PRARTBEL-U                            
216900                                MOD-KDVALISO-U                            
217000                                MOD-FLPRIBES-U                            
217100                                MOD-IDLEVNR-U                             
217200                                MOD-KDFPKPRI-U                            
217300     .                                                                    
217400     EJECT                                                                
217500                                                                          
217600 MFS-RENSA-FAELT-UTDATA SECTION.                                          
217700     MOVE MFS-RENSA-FAELT  TO   MOD-BEART                                 
217800                                MOD-PRAVCOST                              
217900                                MOD-PRMATRL                               
218000                                MOD-PRARTBES                              
218100                                MOD-PRARTSJK                              
218200                                MOD-PRDIRLON                              
218300                                MOD-PRDMTRL                               
218400                                MOD-KVLS-TOT                              
218500                                MOD-KVPB-TOT                              
218600                                MOD-BEST-PRIS(1)                          
218700                                MOD-BEST-PRIS(2)                          
218800                                MOD-BEST-PRIS(3)                          
218900                                MOD-BEST-PRIS(4)                          
219000                                MOD-BEST-PRIS(5)                          
219100                                MOD-RETULF                                
219200                                                                          
219300     .                                                                    
219400     EJECT                                                                
219500                                                                          
219600 MFS-RENSA-FAELT-INDATA SECTION.                                          
219700     MOVE MFS-RENSA-FAELT  TO   MOD-KDPRURSP-U                            
219800                                MOD-TIPRLIST-U                            
219900                                MOD-PRARTBEL-U                            
220000                                MOD-KDVALISO-U                            
220100                                MOD-FLPRIBES-U                            
220200                                MOD-IDLEVNR-U                             
220300                                MOD-KDFPKPRI-U                            
220400     .                                                                    
220500     EJECT                                                                
220600                                                                          
220700 IMS-GET-MSG SECTION.                                                     
220800     MOVE '  QC' TO GODK-STATUSKODER                                      
220900     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
221000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
221100     PERFORM IMS-STATUSKONTROLL                                           
221200     .                                                                    
221300     SKIP3                                                                
221400                                                                          
221500 IMS-INSERT-MSG SECTION.                                                  
221600*    IF MSGI-IDLAND-SPR NOT = 'GB'                                        
221700*      MOVE '0' TO MFS-KDHUVOMR                                           
221800*    END-IF                                                               
221900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
222000     MOVE SPACE TO GODK-STATUSKODER                                       
222100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
222200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
222300     PERFORM IMS-STATUSKONTROLL                                           
222400     .                                                                    
222500     SKIP3                                                                
222600                                                                          
222700 IMS-GN-MSG-KOM-55374 SECTION.                                            
222800     MOVE '  QD' TO GODK-STATUSKODER                                      
222900     CALL CBLTDLI USING GN MSG-PCB KOM-IO-AREA3                           
223000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
223100     PERFORM IMS-STATUSKONTROLL                                           
223200     .                                                                    
223300     EJECT                                                                
223400                                                                          
223500                                                                          
223600 IMS-INSERT-MSG-KOM-55374 SECTION.                                        
223700     MOVE SPACE TO GODK-STATUSKODER                                       
223800     CALL CBLTDLI USING ISRT ALT-PCB KOM-IO-AREA3                         
223900     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
224000     PERFORM IMS-STATUSKONTROLL                                           
224100     .                                                                    
224200     EJECT                                                                
224300                                                                          
224400 IMS-GN-MSG-KOM-50111 SECTION.                                            
224500     MOVE '  QD' TO GODK-STATUSKODER                                      
224600     CALL CBLTDLI USING GN MSG-PCB KOM-IO-AREA4                           
224700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
224800     PERFORM IMS-STATUSKONTROLL                                           
224900     .                                                                    
225000     EJECT                                                                
225100                                                                          
225200                                                                          
225300 IMS-INSERT-MSG-KOM-50111 SECTION.                                        
225400     MOVE SPACE TO GODK-STATUSKODER                                       
225500     CALL CBLTDLI USING ISRT ALT-PCB KOM-IO-AREA4                         
225600     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
225700     PERFORM IMS-STATUSKONTROLL                                           
225800     .                                                                    
225900     EJECT                                                                
226000                                                                          
226100 IMS-GHU-WLARTC01 SECTION.                                                
226200                                                                          
226300     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
226400     DELIMITED  BY SIZE INTO SSA1                                         
226500     MOVE '  GE' TO GODK-STATUSKODER                                      
226600     CALL CBLTDLI USING GHU ARTC-PCB WLARTC01 SSA1                        
226700     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
226800     PERFORM IMS-STATUSKONTROLL                                           
226900     .                                                                    
227000     SKIP3                                                                
227100                                                                          
227200 IMS-GHNP-WLARTC11 SECTION.                                               
227300     MOVE 'WLARTC11 ' TO SSA1                                             
227400     MOVE '    ' TO GODK-STATUSKODER                                      
227500     CALL CBLTDLI USING GHNP ARTC-PCB WLARTC11 SSA1                       
227600     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
227700     PERFORM IMS-STATUSKONTROLL                                           
227800     .                                                                    
227900     EJECT                                                                
228000                                                                          
228100 IMS-GNP-WLARTC23 SECTION.                                                
228200     MOVE 'WLARTC11(KDSEGKEY =1)' TO SSA1                                 
228300     MOVE 'WLARTC23 ' TO SSA2                                             
228400     MOVE '  GE' TO GODK-STATUSKODER                                      
228500     CALL CBLTDLI USING GNP ARTC-PCB WLARTC23 SSA1 SSA2                   
228600     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
228700     PERFORM IMS-STATUSKONTROLL                                           
228800     .                                                                    
228900     SKIP3                                                                
229000                                                                          
229100 IMS-GU-WDK701 SECTION.                                                   
229200     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
229300     DELIMITED  BY SIZE INTO SSA1                                         
229400     MOVE '  GE' TO GODK-STATUSKODER                                      
229500     CALL CBLTDLI USING GU WDK7-PCB WDK701 SSA1                           
229600     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
229700     PERFORM IMS-STATUSKONTROLL                                           
229800     .                                                                    
229900     SKIP3                                                                
230000                                                                          
230100 IMS-GNP-WDK711 SECTION.                                                  
230200     MOVE 'WDK711   ' TO SSA1                                             
230300     MOVE '  GE' TO GODK-STATUSKODER                                      
230400     CALL CBLTDLI USING GNP WDK7-PCB WDK711 SSA1                          
230500     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
230600     PERFORM IMS-STATUSKONTROLL                                           
230700     .                                                                    
230800     SKIP2                                                                
230900                                                                          
231000 IMS-GU-WDK711-DC SECTION.                                                
231100     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
231200     DELIMITED  BY SIZE INTO SSA1                                         
231300     STRING 'WDK711  (IDDC    >=' W-IDDC-WDK7-MIN-X                       
231400                    '&IDDC    <=' W-IDDC-WDK7-MAX-X ')'                   
231500            DELIMITED BY SIZE INTO SSA2                                   
231600     MOVE '  GEGB' TO GODK-STATUSKODER                                    
231700     CALL CBLTDLI USING GU WDK7-PCB WDK711 SSA1 SSA2                      
231800     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
231900     PERFORM IMS-STATUSKONTROLL                                           
232000     .                                                                    
232100     SKIP3                                                                
232200                                                                          
232300 IMS-GNP-WDK712 SECTION.                                                  
232400     STRING 'WDK712  (IDLAND   =' W-IDLANDX2-X ')'                        
232500     DELIMITED  BY SIZE INTO SSA1                                         
232600     MOVE '  GE' TO GODK-STATUSKODER                                      
232700     CALL CBLTDLI USING GHNP WDK7-PCB WDK712 SSA1                         
232800     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
232900     PERFORM IMS-STATUSKONTROLL                                           
233000     .                                                                    
233100     SKIP2                                                                
233200                                                                          
233300 IMS-GHNP-WDK712 SECTION.                                                 
233400     STRING 'WDK712  (IDLAND   =' W-IDLANDX2-X ')'                        
233500     DELIMITED  BY SIZE INTO SSA1                                         
233600     MOVE '  GE' TO GODK-STATUSKODER                                      
233700     CALL CBLTDLI USING GHNP WDK7-PCB WDK712 SSA1                         
233800     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
233900     PERFORM IMS-STATUSKONTROLL                                           
234000     .                                                                    
234100     SKIP2                                                                
234200                                                                          
234300 IMS-REPL-WDK712 SECTION.                                                 
234400     MOVE '    ' TO GODK-STATUSKODER                                      
234500     CALL CBLTDLI USING REPL WDK7-PCB WDK712                              
234600     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
234700     PERFORM IMS-STATUSKONTROLL                                           
234800     .                                                                    
234900     EJECT                                                                
235000                                                                          
235100 IMS-GU-WLBENA11 SECTION.                                                 
235200     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
235300     DELIMITED BY SIZE INTO SSA1                                          
235400     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X  ')'                        
235500     DELIMITED BY SIZE INTO SSA2                                          
235600     MOVE '  GE' TO GODK-STATUSKODER                                      
235700     CALL CBLTDLI USING GU BEN-PCB BEN-WLBENA11 SSA1 SSA2                 
235800     MOVE BEN-STATUS-CODE TO STATUS-WS                                    
235900     PERFORM IMS-STATUSKONTROLL                                           
236000     .                                                                    
236100     SKIP3                                                                
236200                                                                          
236300 IMS-GU-WLLEVA01 SECTION.                                                 
236400     STRING 'WLLEVA01(IDLEVNR  =' W-IDLEVNR-X ')'                         
236500     DELIMITED BY SIZE INTO SSA1                                          
236600     MOVE '  GE' TO GODK-STATUSKODER                                      
236700     CALL CBLTDLI USING GU LEV-PCB WLLEVA01 SSA1                          
236800     MOVE LEV-STATUS-CODE TO STATUS-WS                                    
236900     PERFORM IMS-STATUSKONTROLL                                           
237000     .                                                                    
237100     SKIP3                                                                
237200                                                                          
237300 IMS-GNP-WLLEVA11 SECTION.                                                
237400     STRING 'WLLEVA11(IDLAND   =' W-IDLAND-X ')'                          
237500     DELIMITED BY SIZE INTO SSA1                                          
237600     MOVE '  GE' TO GODK-STATUSKODER                                      
237700     CALL CBLTDLI USING GNP LEV-PCB LEV-WLLEVA11 SSA1                     
237800     MOVE LEV-STATUS-CODE TO STATUS-WS                                    
237900     PERFORM IMS-STATUSKONTROLL                                           
238000     .                                                                    
238100     EJECT                                                                
238200                                                                          
238300 IMS-GU-WLINLE01 SECTION.                                                 
238400     STRING 'WLINLE01(IDARTNR  =' W-IDARTNR-X ')'                         
238500              DELIMITED BY SIZE INTO SSA1                                 
238600     MOVE '  GE' TO GODK-STATUSKODER                                      
238700     CALL CBLTDLI USING GU INLE-PCB INLE-WLINLE01 SSA1                    
238800     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
238900     PERFORM IMS-STATUSKONTROLL                                           
239000     .                                                                    
239100     SKIP3                                                                
239200                                                                          
239300 IMS-GNP-WLINLE11 SECTION.                                                
239400     STRING 'WLINLE11(DAINLEV =<' W-DAINLEV-X ')'                         
239500              DELIMITED BY SIZE INTO SSA1                                 
239600     MOVE '  GE' TO GODK-STATUSKODER                                      
239700     CALL CBLTDLI USING GNP INLE-PCB INLE-WLINLE11 SSA1                   
239800     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
239900     PERFORM IMS-STATUSKONTROLL                                           
240000     .                                                                    
240100     EJECT                                                                
240200                                                                          
240300 IMS-GNP-WLINLE21 SECTION.                                                
240400     STRING 'WLINLE11(DAINLEV  =' W-DAINLEV-X ')'                         
240500              DELIMITED BY SIZE INTO SSA1                                 
240600     MOVE 'WLINLE21 ' TO SSA2                                             
240700     MOVE '  GE' TO GODK-STATUSKODER                                      
240800     CALL CBLTDLI USING GNP INLE-PCB INLE-WLINLE21 SSA1 SSA2              
240900     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
241000     PERFORM IMS-STATUSKONTROLL                                           
241100     .                                                                    
241200     SKIP3                                                                
241300                                                                          
241400 IMS-GNP-WLINLE22 SECTION.                                                
241500     STRING 'WLINLE11(DAINLEV  =' W-DAINLEV-X ')'                         
241600              DELIMITED BY SIZE INTO SSA1                                 
241700     MOVE 'WLINLE22 ' TO SSA2                                             
241800     MOVE '  GE' TO GODK-STATUSKODER                                      
241900     CALL CBLTDLI USING GNP INLE-PCB INLE-WLINLE22 SSA1 SSA2              
242000     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
242100     PERFORM IMS-STATUSKONTROLL                                           
242200     .                                                                    
242300     EJECT                                                                
242400                                                                          
242500 IMS-GU-WDB601    SECTION.                                                
242600     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
242700          DELIMITED BY SIZE INTO SSA1                                     
242800     MOVE '  GE' TO GODK-STATUSKODER                                      
242900     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
243000     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
243100     PERFORM IMS-STATUSKONTROLL                                           
243200     IF SEGMENT-SAKNAS                                                    
243300         MOVE SPACE TO DCS-KDDC                                           
243400     END-IF                                                               
243500     .                                                                    
243600                                                                          
243700 IMS-GNP-WDB617    SECTION.                                               
243800     MOVE 'WDB617   ' TO SSA1                                             
243900     MOVE '  GE' TO GODK-STATUSKODER                                      
244000     CALL CBLTDLI USING GNP WDB6-PCB DLI-IO-AREA-B617 SSA1                
244100     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
244200     PERFORM IMS-STATUSKONTROLL                                           
244300     .                                                                    
244400                                                                          
244500 IMS-GHNP-WDK724-FIRST SECTION.                                           
244600     MOVE 'WDK724  *F' TO SSA1                                            
244700     MOVE '  GE' TO GODK-STATUSKODER                                      
244800     CALL CBLTDLI USING GHNP WDK7-PCB WDK724 SSA1                         
244900     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
245000     PERFORM IMS-STATUSKONTROLL                                           
245100     .                                                                    
245200     SKIP3                                                                
245300                                                                          
245400 IMS-GHNP-WDK724-NEXT SECTION.                                            
245500     MOVE 'WDK724   ' TO SSA1                                             
245600     MOVE '  GE' TO GODK-STATUSKODER                                      
245700     CALL CBLTDLI USING GHNP WDK7-PCB WDK724 SSA1                         
245800     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
245900     PERFORM IMS-STATUSKONTROLL                                           
246000     .                                                                    
246100     EJECT                                                                
246200                                                                          
246300 IMS-REPL-WDK724 SECTION.                                                 
246400     MOVE '    ' TO GODK-STATUSKODER                                      
246500     CALL CBLTDLI USING REPL WDK7-PCB WDK724                              
246600     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
246700     PERFORM IMS-STATUSKONTROLL                                           
246800     .                                                                    
246900     EJECT                                                                
247000                                                                          
247100 IMS-DLET-WDK724   SECTION.                                               
247200*    MOVE '  GE' TO GODK-STATUSKODER                                      
247300     MOVE '  ' TO GODK-STATUSKODER                                        
247400     CALL CBLTDLI USING DLET WDK7-PCB WDK724                              
247500     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
247600     PERFORM IMS-STATUSKONTROLL                                           
247700     .                                                                    
247800     SKIP3                                                                
247900                                                                          
248000 IMS-GU-WDB201-FIRST SECTION.                                             
248100     STRING 'WDB201  (IDGMT   >=' W-IDGMT-MIN-X                           
248200                    '&IDGMT   <=' W-IDGMT-MAX-X ')'                       
248300          DELIMITED BY SIZE INTO SSA1                                     
248400     MOVE '  GEGB' TO GODK-STATUSKODER                                    
248500     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-AREA-WDB2 SSA1                 
248600     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
248700     PERFORM IMS-STATUSKONTROLL                                           
248800     .                                                                    
248900     SKIP3                                                                
249000                                                                          
249100 IMS-STATUSKONTROLL SECTION.                                              
249200     SET STATUS-IX TO 1                                                   
249300     SEARCH GODK-STATUS AT END CALL FELLOG                                
249400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
249500     END-SEARCH                                                           
249600     .                                                                    
249700     EJECT                                                                
249800*    -COPY WY2000P1                                                       
249900*    -COPY WY2000P9                                                       
