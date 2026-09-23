000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W5011100.                                                
000400 AUTHOR.         ROYNA LUND.                                              
000500 DATE-WRITTEN.   MAJ   89.                                                
000600**                                                                        
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION.                                                            
001000*        IMSDC UPPDATERINGSPROGRAM FÖR EKONOMI.                           
001100*                                                                         
001200*        UPPDATERINGEN SKER PÅ TVÅ SÄTT:                                  
001300*        - FRÅN INKÖP GENOM TRANS W5T111X FRÅN W55314                     
001400*          VIA WLKOMA (WDP8), KOMMUNIKATIONSDATABAS.                      
001500*        - FRÅN KINA GENOM TRANS W5T111X FRÅN W50206                      
001600*          VIA WLKOMA (WDP8), KOMMUNIKATIONSDATABAS.                      
001700*          DÄR REFILLER KOMMER TILL CDC                                   
001800*        - FRÅN SKÄRMEN                                                   
001900*                                                                         
002000*        OM UPPDATERINGEN KOM FRÅN INKÖP OCH                              
002100*        UPPDATERINGEN GICK BRA SKICKAS OK-MEDDELANDE                     
002200*        TILL DISPATCHER ANNARS SKICKAS FELMEDDELANDE.                    
002300*                                                                         
002400*                                                                         
002500*        PROGRAMMET LÄSER WLARTC (WDK6) OCH LÄGGER UPP NYA                
002600*        BESTÄLLNINGSPRISER ELLER ÄNDRAR BEFINTLIGA PÅ                    
002700*        WLARTC21 (WDK621). OM INLEVERANS SKETT EFTER AKTUELLT            
002800*        DATUM UPPDATERAS ÄVEN BESTÄLLNINGSPRIS OCH                       
002900*        SJÄLVKOSTNADSPRIS PÅ WLARTC11 (WDK611).                          
003000*                                                                         
003100*    INDATA.                                                              
003200*        TRANSAKTION: W5T111                                              
003300*                     W5T111U                                             
003400*                     W5T111X                                             
003500*                                                                         
003600*        MID:         W5I11101                                            
003700*                     W5I11101 + WMSGKOM                                  
003800*                                                                         
003900*    UTDATA.                                                              
004000*        MOD:         W5O11101                                            
004100*                                                                         
004200*  E-TRACKER 1286763, MÄRKNING AV PRISÄNDRING PÅ ONDEMANDLISTOR           
004300     EJECT                                                                
004400 ENVIRONMENT DIVISION.                                                    
004500     SKIP3                                                                
004600 DATA DIVISION.                                                           
004700                                                                          
004800 WORKING-STORAGE SECTION.                                                 
004900                                                                          
005000*    -COPY WY2000W9                                                       
005100*    -COPY WY2000W1                                                       
005200*    -COPY WWDC99                                                         
005210*    -COPY WWPRODSL                                                       
005300                                                                          
005400 77  IDPGM                   PIC X(8)       VALUE 'W5011100'.             
005500 77  JA                      PIC X          VALUE 'J'.                    
005600 77  NEJ                     PIC X          VALUE 'N'.                    
005700                                                                          
005800 77  NYCKLAR-OK              PIC X          VALUE 'J'.                    
005900 77  FLFEL-FAELT             PIC X          VALUE 'N'.                    
006000 77  FLSLUTA-LAS             PIC X          VALUE 'N'.                    
006100 77  FLLEV-TULLF             PIC X          VALUE 'N'.                    
006200 77  FL-DLET-21SEG           PIC X          VALUE 'N'.                    
006300 77  INLEV-PRISRAD           PIC X          VALUE 'N'.                    
006400 77  INLEV-FINNS             PIC X          VALUE 'N'.                    
006500                                                                          
006600 77  WS-IX-MAX               PIC S9(9)     VALUE +50  COMP SYNC.          
006700 77  WS-IY-MAX               PIC S9(9)     VALUE +50  COMP SYNC.          
006800 77  WS-IX                   PIC S9(9)     VALUE +0   COMP SYNC.          
006900 77  WS-IY                   PIC S9(9)     VALUE +0   COMP SYNC.          
007000 77  INDX                    PIC S9(9)     VALUE +0   COMP SYNC.          
007100 77  INDX-NY                 PIC S9(9)     VALUE +0   COMP SYNC.          
007200                                                                          
007300 77  WS-IDUSER               PIC X(8)       VALUE SPACE.                  
007400 77  WS-IDLEVNR              PIC X(5)       VALUE SPACE.                  
007500 77  WS-IDLEVNR-1441         PIC X(5)       VALUE '1441'.                 
007600 77  WS-ART-IDLEVNR          PIC X(5)       VALUE SPACE.                  
007700 77  WS-KDPRBEH              PIC X(1)       VALUE SPACE.                  
007800 77  WS-REAENDR              PIC X(6)       VALUE SPACE.                  
007900 77  WS-REAENDR-NUM          PIC 9(4)V9(1)  VALUE ZERO.                   
008000 77  WS-KDFPKPRI             PIC X          VALUE SPACE.                  
008100 77  W-DATE-AAMM             PIC 9(4)       VALUE ZERO.                   
008200                                                                          
008300 77  SW-MED                  PIC X          VALUE SPACE.                  
008400     88 MED-1A                              VALUE 'A'.                    
008500     88 MED-1B                              VALUE 'B'.                    
008600     88 MED-1C                              VALUE 'C'.                    
008700     EJECT                                                                
008800 01  WS-W510PRMT.                                                         
008900     03  WS-PRMT-KDSVAR    PIC X.                                         
009000     03  WS-PRMT-RESP OCCURS 50 TIMES.                                    
009100        05 WS-PRMT-IDDC      PIC X(2).                                    
009200 01  FILLER                      PIC X(16)   VALUE 'P-TO-P-AREA'.         
009300                                                                          
009400 01  P-TO-P-AREA.                                                         
009500     03  P-TO-P-LL               PIC S9(4)            COMP SYNC.          
009600     03  P-TO-P-Z1               PIC  X(1)   VALUE LOW-VALUE.             
009700     03  P-TO-P-Z2               PIC  X(1)   VALUE LOW-VALUE.             
009800     03  P-TO-P-TRANSKOD         PIC  X(7).                               
009900     03  FILLER                  PIC  X(1)   VALUE SPACE.                 
010000     03  P-TO-P-FROM-MID         PIC  X(4).                               
010100     03  P-TO-P-KDMFSFOR         PIC  X(1).                               
010200     03  P-TO-P-DATA             PIC  X(1000).                            
010300                                                                          
010400     EJECT                                                                
010500 01  IDARTNR-WS              PIC X(9)       VALUE SPACE.                  
010600 01  WS-IDARTNR-N            PIC 9(9)       VALUE ZERO.                   
010700 01  WS-TIUPPDAT             PIC S9(7)     COMP-3.                        
010800 01  WS-TIUPPTID             PIC S9(9)     COMP-3.                        
010900                                                                          
011000 01  SUBPGM.                                                              
011100     03  CBLTDLI             PIC X(8)       VALUE 'CBLTDLI '.             
011200     03  FELLOG              PIC X(8)       VALUE 'FELLOG  '.             
011300     03  WDATKONV            PIC X(8)       VALUE 'WDATKONV'.             
011400     03  WDECEDIT            PIC X(8)       VALUE 'WDECEDIT'.             
011500     03  WDECSTR             PIC X(8)       VALUE 'WDECSTR '.             
011600     03  W005INIT            PIC X(8)       VALUE 'W005INIT'.             
011700     03  W006KOM             PIC X(8)       VALUE 'W006KOM '.             
011800     03  W510PRMT            PIC X(8)       VALUE 'W510PRMT'.             
011900     03  W510CURR            PIC X(8)       VALUE 'W510CURR'.             
012000                                                                          
012100 01  W-IDTRANS               PIC X(4)       VALUE SPACE.                  
012200     88 EGEN-TRANS                          VALUE '5111'.                 
012300     88 GODK-TRANS                          VALUE '5111' '5112'.          
012400                                                                          
012500 01  DIVERSE.                                                             
012600     03  W-KDAVT             PIC S9                    COMP-3.            
012700     03  W-TIPRLIST          PIC 9(6).                                    
012800     03  W-KDERS             PIC S9(3)      VALUE +0   COMP-3.            
012900     03  W-SUINLEV           PIC S9(3)      VALUE +0   COMP-3.            
013000     03  W-KVPB-TOT          PIC S9(6)V9    VALUE +0   COMP-3.            
013100     03  W-PRARTBEL          PIC S9(8)V9(5) VALUE +0.                     
013200     03  WS-PRARTBEL         PIC 9(13)      VALUE  0.                     
013300     03  WS-PRARTBES         PIC 9(13)      VALUE  0.                     
013400     03  W-PRARTBES          PIC S9(7)V9(2) VALUE +0   COMP-3.            
013500     03  W-PRARTSJK          PIC S9(7)V9(2) VALUE +0   COMP-3.            
013600     03  W-PRINK             PIC S9(7)V9(2) VALUE +0   COMP-3.            
013700     03  W-PRINK-O           PIC S9(7)V9(2) VALUE +0   COMP-3.            
013800     03  W-PRARTSTD          PIC S9(7)V9(2) VALUE +0   COMP-3.            
013900     03  W-PRLFKST           PIC S9(3)V9(2) VALUE +0   COMP-3.            
014000     03  W-RETULF            PIC S9(3)V9(4) VALUE +0.                     
014100     03  W-PRKURS            PIC S9(5)V9(5) VALUE +0   COMP-3.            
014200     03  W-REVALUTA          PIC S9(5)      VALUE +0   COMP-3.            
014300     03  W-REDIRLEV          PIC S9V9(2)    VALUE +0   COMP-3.            
014400     03  W-TOTSALDO          PIC S9(9)      VALUE +0   COMP-3.            
014500     03  W-CDCSALDO          PIC S9(9)      VALUE +0   COMP-3.            
014600     03  W-SDCSALDO          PIC S9(9)      VALUE +0   COMP-3.            
014700     03  W-KDVALISO          PIC X(3)       VALUE SPACE.                  
014800     03  WS-KDVALISO         PIC X(3)       VALUE SPACE.                  
014900     03  WS-KDVALISO-5206    PIC X(3)       VALUE SPACE.                  
015000     03  PRISANDRING         PIC 9(8)V9(10) VALUE  0.                     
015100     03  ANT-INLEV           PIC S9         VALUE +0   COMP-3.            
015200     03  W-IDSTRDATA-X.                                                   
015300         05  W-IDSTRDATA-N   PIC 9(8)V9(5).                               
015400         05  FILLER          PIC XX.                                      
015500                                                                          
015600     03  DAGENS-AAMMDD       PIC 9(6).                                    
015700     03  DAGENS-DAT          PIC 9(8).                                    
015800     03  DAGENS-TID          PIC 9(8).                                    
015900     03  WS-DATUM            PIC 9(8).                                    
016000                                                                          
016100     03  MAX-PRARTBES        PIC S9(7)V9(2) VALUE +400000 COMP-3.         
016200                                                                          
016300     03  DAINLEV-FAELT.                                                   
016400      05  DAINLEV-NYCKEL     PIC 9(16)  VALUE ZERO.                       
016500      05 DAINLEV-MAX         PIC 9(16)  VALUE 9999999999999999.           
016600                                                                          
016700     03  R25-DATUM-X         PIC 9(16).                                   
016800     03  R25-DATUM-R  REDEFINES  R25-DATUM-X.                             
016900      05 R25-DATUM-SEKEL     PIC 9(02).                                   
017000      05 R25-DATUM           PIC 9(06).                                   
017100      05 R25-DATUM-NOLLOR    PIC 9(08).                                   
017200                                                                          
017300     03  DATUM-FAELT.                                                     
017400      05 W-DAPRLIST-MAX      PIC 9(8)    VALUE 99999999.                  
017500      05 W-DAPRLIST          PIC 9(8).                                    
017600      05 W-DAPRLIST-X  REDEFINES W-DAPRLIST.                              
017700       07 FILLER             PIC 9(2).                                    
017800       07 W-LISTDATUM        PIC 9(6).                                    
017900                                                                          
018000     EJECT                                                                
018100 01  FILLER                  PIC X(16)   VALUE 'DLI-NYCKLAR'.             
018200 01  NYCKLAR-TILL-DLI.                                                    
018300     03  W-IDARTNR-X.                                                     
018400         05  W-IDARTNR       PIC S9(9)   VALUE +0  COMP-3.                
018500     03  W-KDSEGKEY-X.                                                    
018600         05  W-KDSEGKEY      PIC X(1)    VALUE '1'.                       
018700     03  W-KDNOTTYP-X.                                                    
018800         05    W-KDNOTTYP    PIC S9(1)   VALUE +8  COMP-3.                
018900     03  W-DAINLEV-X.                                                     
019000         05    W-DAINLEV     PIC 9(16)   VALUE ZERO.                      
019100     03  W-IDSKYLT-X.                                                     
019200         05    W-IDSKYLT     PIC X(3)    VALUE 'S  '.                     
019300     03  W-IDLEVNR-X.                                                     
019400         05    W-IDLEVNR     PIC X(5)    VALUE SPACE.                     
019500     03  W-IDLAND-X.                                                      
019600         05    W-IDLAND      PIC X(2)    VALUE SPACE.                     
019700     03   W-WDK621KY-X.                                                   
019800       05  W-DAPRLIST-9KOMPL PIC 9(8)    VALUE ZERO.                      
019900       05  W-IDLEVNR-21      PIC X(5)    VALUE LOW-VALUE.                 
020000     03  W-WDH801-X.                                                      
020100         05    W-IDARTNR-H8  PIC S9(9)   VALUE ZERO COMP-3.               
020200         05    W-DAREGDAT    PIC 9(8)    VALUE ZERO.                      
020300         05    W-TIREGTID    PIC S9(7) COMP-3 VALUE ZERO.                 
020400                                                                          
020500     03  W-IDDC-B6-X.                                                     
020600         05 W-IDDC-B6            PIC X(2).                                
020700     SKIP2                                                                
020800                                                                          
020900     EJECT                                                                
021000 01  MEDDELANDE.                                                          
021100     03  W-FEL-1               PIC X(26)   VALUE                          
021200             'ARTIKEL SAKNAS            '.                                
021300                                                                          
021400     03  W-FEL-2               PIC X(26)   VALUE                          
021500             'ARTIKEL UTGÅNGEN          '.                                
021600                                                                          
021700     03  W-FEL-3               PIC X(26)   VALUE                          
021800             'UPPLYSTA FÄLT FEL         '.                                
021900                                                                          
022000     03  W-FEL-4               PIC X(26)   VALUE                          
022100             'ARTIKELNUMMER EJ NUMERISKT'.                                
022200                                                                          
022300     03  W-FEL-5               PIC X(40)   VALUE                          
022400             'TRYCK PF-TANGENT FÖR UPPDATERING     '.                     
022500                                                                          
022600     03  W-FEL-6               PIC X(40)   VALUE                          
022700             'FYLL I INMATNINGSFÄLT VID UPPDATERING'.                     
022800                                                                          
022900     03  W-FEL-7               PIC X(40)   VALUE                          
023000             'STOR PRISÄNDRING  *** BEKRÄFTA (J/N) ***'.                  
023100                                                                          
023200     03  W-FEL-8               PIC X(26)   VALUE                          
023300             'ERS-KOD > 9               '.                                
023400                                                                          
023500     03  W-FEL-9               PIC X(26)   VALUE                          
023600             'HF-KOD > 0                '.                                
023700                                                                          
023800     03  W-FEL-10              PIC X(26)   VALUE                          
023900             'INGEN UPPDATERING UTFÖRD  '.                                
024000                                                                          
024100     03  W-FEL-11              PIC X(40)   VALUE                          
024200             'ARTIKELN SAKNAR STANDARDPRIS         '.                     
024300                                                                          
024400     03  W-FEL-12              PIC X(40)   VALUE                          
024500             'LEVERANTÖR SAKNAS PÅ LEVERANTÖRSREG   '.                    
024600                                                                          
024700     03  W-FEL-14              PIC X(40)   VALUE                          
024800             'STANDARDKURS SAKNAS FÖR AKTUELL VALUTA'.                    
024900                                                                          
025000     03  W-MED-1               PIC X(26)   VALUE                          
025100             'UPPDATERING UTFÖRD        '.                                
025200     03  W-MED-1A              PIC X(50)   VALUE                          
025300             'BEST/SJKPRIS UPPD, INK/STDPRIS TILL KÖ'.                    
025400     03  W-MED-1B              PIC X(50)   VALUE                          
025500             'BEST/SJKPRIS UPPD, INK/STDPRIS UPPD'.                       
025600     03  W-MED-1C              PIC X(50)   VALUE                          
025700             'INGEN PRISÄNDRING UTFÖRD'.                                  
025800     SKIP3                                                                
025900 01  MESSAGE-CODES.                                                       
026000     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
026100     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
026200     EJECT                                                                
026300*                   ****    PARAMETRAR TILL W005INIT                      
026400 01  FILLER                    PIC X(16)   VALUE 'WMSGINIT'.              
026500*01  -COPY WMSGINIT                                                       
026600     EJECT                                                                
026700 01  WDATAREA                  PIC X(8)    VALUE 'WDATAREA'.              
026800     SKIP3                                                                
026900*01       -COPY WDATAREA                                                  
027000     EJECT                                                                
027100 01  FILLER                    PIC X(8)    VALUE 'WDECAREA'.              
027200     SKIP3                                                                
027300*01       -COPY WDECAREA                                                  
027400     EJECT                                                                
027500 01  FILLER                    PIC X(8)    VALUE 'WDECSTR'.               
027600     SKIP3                                                                
027700*01       -COPY WDECSTR                                                   
027800     EJECT                                                                
027900 01  FILLER                    PIC X(8)    VALUE 'W553LVAL'.              
028000                                                                          
028100*01       -COPY W553LVAL                                                  
028200     EJECT                                                                
028300 01  FILLER                    PIC X(16)   VALUE 'SORT-AREA'.             
028400*01  POST -COPY W092W001    -PRE SORT-                                    
028500     EJECT                                                                
028600******************************************************************        
028700*                                                                         
028800*                AREOR FOR MFS OCH SKÄRMHANTERING                         
028900*                                                                         
029000******************************************************************        
029100 01    FILLER                    PIC X(16)   VALUE 'MFS-WS'.              
029200     SKIP3                                                                
029300*01  MID -COPY W5I11101 -PRE MID-                                         
029400     EJECT                                                                
029500*01  -COPY W510PRMT                                                       
029600     EJECT                                                                
029700*01  -COPY W510CURR                                                       
029800     EJECT                                                                
029900*01  -COPY WMSGAREA                                                       
030000     EJECT                                                                
030100*  03  MOD -COPY W5O11101 -PRE MOD- -RED MSG-AREA                         
030200     EJECT                                                                
030300*01  -COPY WMFSAREA                                                       
030400******************************************************************        
030500*                                                                         
030600*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
030700*                                                                         
030800******************************************************************        
030900                                                                          
031000 01  IMS-WS.                                                              
031100     03    FILLER              PIC X(16)   VALUE 'IMS-WS     '.           
031200     SKIP3                                                                
031300*                        **** STATUS-KOD FRÅN IMS ****                    
031400     03    STATUS-WS           PIC XX.                                    
031500         88    SEGMENT-FINNS               VALUE '  '.                    
031600         88    SEGMENT-SAKNAS              VALUE 'GE'.                    
031700         88    SEGMENT-FINNS-REDAN         VALUE 'II'.                    
031800     SKIP3                                                                
031900     03    GODK-STATUSKODER.                                              
032000         05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.          
032100     SKIP3                                                                
032200 01  SSA1                      PIC X(64).                                 
032300 01  SSA2                      PIC X(64).                                 
032400 01  SSA3                      PIC X(64).                                 
032500     EJECT                                                                
032600*                        **** IMS FUNKTIONSKODER ****                     
032700*01    -COPY W0003                                                        
032800     EJECT                                                                
032900*                            DLI INPUT-OUTPUT AREA                        
033000 01  FILLER                    PIC X(16)  VALUE 'WDK601'.                 
033100                                                                          
033200*01  WLARTC01 -COPY WDK601                                                
033300     EJECT                                                                
033400 01  FILLER                    PIC X(16)  VALUE 'WDK611'.                 
033500                                                                          
033600*01  WLARTC11 -COPY WDK611                                                
033700     EJECT                                                                
033800 01  FILLER                    PIC X(16)  VALUE 'WDK621'.                 
033900                                                                          
034000*01  WLARTC21 -COPY WDK621                                                
034100     EJECT                                                                
034200 01  FILLER                    PIC X(16)  VALUE 'WDK622'.                 
034300                                                                          
034400*01  WLARTC22 -COPY WDK622                                                
034500     EJECT                                                                
034600 01  FILLER                    PIC X(16)  VALUE 'WDK623'.                 
034700                                                                          
034800*01  WLARTC23 -COPY WDK623                                                
034900     EJECT                                                                
035000 01  FILLER                    PIC X(16)  VALUE 'WDK625'.                 
035100                                                                          
035200*01  WLARTC25 -COPY WDK625                                                
035300     EJECT                                                                
035400 01  FILLER                    PIC X(16)  VALUE 'WDD311'.                 
035500                                                                          
035600*01  WLBENA11 -COPY WDD311 -PRE BEN-                                      
035700     EJECT                                                                
035800 01  FILLER                    PIC X(16)  VALUE 'WDF101'.                 
035900                                                                          
036000*01  WLLEVA01 -COPY WDF101                                                
036100     EJECT                                                                
036200 01  FILLER                    PIC X(16)  VALUE 'WDF102'.                 
036300                                                                          
036400*01  WLLEVA11 -COPY WDF102 -PRE LEV-                                      
036500     EJECT                                                                
036600 01  FILLER                    PIC X(16)  VALUE 'WDL201'.                 
036700                                                                          
036800*01  WLINLE01 -COPY WDL201 -PRE INLE-                                     
036900     EJECT                                                                
037000 01  FILLER                    PIC X(16)  VALUE 'WDL211'.                 
037100                                                                          
037200*01  WLINLE11 -COPY WDL211 -PRE INLE-                                     
037300     EJECT                                                                
037400 01  FILLER                    PIC X(16)  VALUE 'WDL221'.                 
037500                                                                          
037600*01  WLINLE21 -COPY WDL221 -PRE INLE-                                     
037700     EJECT                                                                
037800 01  FILLER                    PIC X(16)  VALUE 'WDL222'.                 
037900                                                                          
038000*01  WLINLE22 -COPY WDL222 -PRE INLE-                                     
038100     EJECT                                                                
038200 01  FILLER                    PIC X(16)  VALUE 'WDK701'.                 
038300                                                                          
038400*01  WLARTS01 -COPY WDK701                                                
038500     EJECT                                                                
038600 01  FILLER                    PIC X(16)  VALUE 'WDK711'.                 
038700                                                                          
038800*01  WLARTS11 -COPY WDK711                                                
038900     EJECT                                                                
039000 01  FILLER                    PIC X(16)  VALUE 'WDH801'.                 
039100                                                                          
039200*01  WLPRIG01 -COPY WDH801                                                
039300                                                                          
039400 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
039500 01   DLI-IO-AREA-B601.                                                   
039600*     03  -COPY WDB601                                                    
039700                                                                          
039800*    --- AREA FÖR KOMMUNIKATION MED DISPATCHER                            
039900*                                                                         
040000 01  FILLER                 PIC X(16)   VALUE 'KOM-DISP-IO-AREA'.         
040100     SKIP3                                                                
040200 01  KOM-IO-AREA.                                                         
040300*    03  -COPY WMSGKOM                                                    
040400     EJECT                                                                
040500 01  FILLER                 PIC X(16)   VALUE 'KOM-IO-AREA-5206'.         
040600     SKIP3                                                                
040700 01  KOM-IO-AREA1.                                                        
040800*    03  -COPY WMSGKOM    -PRE 5206-                                      
040900     EJECT                                                                
041000 01  FILLER                 PIC X(16)   VALUE 'KOM-IO-AREA2'.             
041100 01  KOM-IO-AREA2.                                                        
041200*  03      -COPY W5I20601 -PRE 5206-                                      
041300     EJECT                                                                
041400 LINKAGE SECTION.                                                         
041500*01    -COPY W0009     -PRE MSG-                                          
041600     EJECT                                                                
041700*01    -COPY W0009     -PRE ALT-                                          
041800     EJECT                                                                
041900*01  -COPY W0008     -PRE USEA-                                           
042000         05  FILLER           PIC X.                                      
042100     EJECT                                                                
042200*01    -COPY W0008     -PRE ARTC-                                         
042300     05  FILLER                  PIC X(13).                               
042400     EJECT                                                                
042500*01    -COPY W0008     -PRE ARTC2-                                        
042600     05  FILLER                  PIC X(13).                               
042700     EJECT                                                                
042800*01    -COPY W0008     -PRE BEN-                                          
042900     05  FILLER                  PIC X(8).                                
043000     EJECT                                                                
043100*01    -COPY W0008     -PRE LEV-                                          
043200     05  FILLER                  PIC X(5).                                
043300     EJECT                                                                
043400*01    -COPY W0008     -PRE 9305-                                         
043500     05  FILLER                  PIC X(30).                               
043600     EJECT                                                                
043700*      -COPY W0008     -PRE INLE-                                         
043800     05  FILLER                  PIC X(13).                               
043900     EJECT                                                                
044000*01       -COPY W0008     -PRE ARTS-                                      
044100      05 FILLER                  PIC X(18).                               
044200     EJECT                                                                
044300*01       -COPY W0008     -PRE PRIG-                                      
044400      05 FILLER                  PIC X(18).                               
044500     EJECT                                                                
044600*01       -COPY W0008     -PRE PRIG2-                                     
044700      05 FILLER                  PIC X(18).                               
044800     EJECT                                                                
044900*01       -COPY W0008     -PRE FILB-                                      
045000      05 FILLER                  PIC X(18).                               
045100     EJECT                                                                
045200*01       -COPY W0008     -PRE ARTS2-                                     
045300      05 FILLER                  PIC X(18).                               
045400     EJECT                                                                
045500*01       -COPY W0008     -PRE WDB6-                                      
045600      05 FILLER                  PIC X(18).                               
045700     EJECT                                                                
045800*01       -COPY W0008     -PRE PRMT-WDK7-                                 
045900      05 FILLER                  PIC X(18).                               
046000     EJECT                                                                
046100*01       -COPY W0008     -PRE PRMT-WDB6-                                 
046200      05 FILLER                  PIC X(18).                               
046300     EJECT                                                                
046400*01       -COPY W0008     -PRE PRDC-WDB6-                                 
046500      05 FILLER                  PIC X(18).                               
046600     EJECT                                                                
046700*01       -COPY W0008     -PRE KOMA-                                      
046800      05 FILLER                  PIC X.                                   
046900     EJECT                                                                
047000 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB USEA-PCB                       
047100                           ARTC-PCB ARTC2-PCB BEN-PCB                     
047200                           LEV-PCB  9305-PCB                              
047300                           INLE-PCB ARTS-PCB PRIG-PCB                     
047400                           PRIG2-PCB FILB-PCB ARTS2-PCB                   
047500                           WDB6-PCB PRMT-WDK7-PCB                         
047600                           PRMT-WDB6-PCB PRDC-WDB6-PCB KOMA-PCB.          
047700 MAIN SECTION.                                                            
047800     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB USEA-PCB                       
047900                           ARTC-PCB ARTC2-PCB BEN-PCB                     
048000                           LEV-PCB  9305-PCB                              
048100                           INLE-PCB ARTS-PCB PRIG-PCB                     
048200                           PRIG2-PCB FILB-PCB ARTS2-PCB                   
048300                           WDB6-PCB PRMT-WDK7-PCB                         
048400                           PRMT-WDB6-PCB PRDC-WDB6-PCB KOMA-PCB.          
048500                                                                          
048600     PERFORM IMS-GET-MSG                                                  
048700                                                                          
048800     IF SEGMENT-FINNS                                                     
048900       PERFORM A-INIT                                                     
049000       PERFORM B-KOLLA-NYCKEL                                             
049100       IF MFS-UPD-X                                                       
049200         PERFORM IMS-GN-MSG-KOM                                           
049300       END-IF                                                             
049400       IF NYCKLAR-OK = JA                                                 
049500         PERFORM S01-HAMTA-SLAGERSALDO                                    
049600         IF (MFS-UPDATE AND MID-IDARTNR-IN = ALL '+') OR                  
049700            (MFS-UPD-X)                                                   
049800           PERFORM C-UPPDATERA                                            
049900         ELSE                                                             
050000           PERFORM D-SOEKNING                                             
050100         END-IF                                                           
050200       ELSE                                                               
050300         MOVE W-FEL-4 TO MOD-TEMFSFEL                                     
050400         PERFORM MFS-RENSA-FAELT-UTDATA                                   
050500         PERFORM MFS-RENSA-FAELT-INDATA                                   
050600       END-IF                                                             
050700       IF MFS-UPD-X                                                       
050800         IF MSG-KOM-IDMFSMED = SPACE                                      
050900           MOVE INF-UPDATE-DONE TO MSG-KOM-IDMFSMED                       
051000         END-IF                                                           
051100         MOVE SPACE             TO MSG-KOM-KDSVAR                         
051200         PERFORM IMS-INSERT-MSG-KOM                                       
051300       ELSE                                                               
051400         COMPUTE  MSG-KVLL  = LENGTH OF MOD-W5O11101 + 4                  
051500         PERFORM IMS-INSERT-MSG                                           
051600       END-IF                                                             
051700     END-IF                                                               
051800                                                                          
051900                                                                          
052000     MOVE ZERO TO RETURN-CODE                                             
052100     GOBACK                                                               
052200     .                                                                    
052300     EJECT                                                                
052400 A-INIT SECTION.                                                          
052500                                                                          
052600     ACCEPT DAGENS-TID    FROM TIME                                       
052700     MOVE FUNCTION CURRENT-DATE (1:8) TO DAGENS-DAT                       
052800     MOVE FUNCTION CURRENT-DATE (3:6) TO DAGENS-AAMMDD                    
052900                                                                          
053000     MOVE SPACE    TO  WLPRIG01                                           
053100     MOVE ZERO     TO  PRI-IDARTNR                                        
053200                       PRI-DAREGDAT                                       
053300                       PRI-TIREGTID                                       
053400                       PRI-REDIRLEV                                       
053500                       PRI-O-KDSTATUS-PR                                  
053600                       PRI-O-PRARTBEL-PR                                  
053700                       PRI-O-PRARTBES-PR                                  
053800                       PRI-O-SUINLEV-PR                                   
053900                       PRI-O-TIPRLIST                                     
054000                       PRI-N-KDSTATUS-PR                                  
054100                       PRI-N-PRARTBEL-PR                                  
054200                       PRI-N-PRARTBES-PR                                  
054300                       PRI-N-SUINLEV-PR                                   
054400                       PRI-N-TIPRLIST                                     
054500                       PRI-O-PRARTBES                                     
054600                       PRI-O-PRARTSJK                                     
054700                       PRI-O-PRARTSTD                                     
054800                       PRI-O-PRDIRLON                                     
054900                       PRI-O-PRDMTRL                                      
055000                       PRI-O-PRINK                                        
055100                       PRI-O-PRLFKST                                      
055200                       PRI-O-PROVRPAL                                     
055300                       PRI-O-RETULF                                       
055400                       PRI-N-PRARTBES                                     
055500                       PRI-N-PRARTSJK                                     
055600                       PRI-N-PRARTSTD                                     
055700                       PRI-N-PRDIRLON                                     
055800                       PRI-N-PRDMTRL                                      
055900                       PRI-N-PRINK                                        
056000                       PRI-N-PRLFKST                                      
056100                       PRI-N-PROVRPAL                                     
056200                       PRI-N-RETULF                                       
056300     MOVE SPACE    TO  PRI-N-IDLEVNR-PR                                   
056400                       PRI-O-IDLEVNR-PR                                   
056500                                                                          
056600     IF MSG-DUBBLA-TRANSKODER                                             
056700        MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W5I11101                
056800        MOVE MSG-IDTRANS-2        TO MFS-IDTRANS                          
056900        MOVE MSG-KDMFSFOR-2       TO MFS-KDMFSFOR                         
057000     ELSE                                                                 
057100        MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W5I11101                 
057200        MOVE MSG-IDTRANS-1        TO MFS-IDTRANS                          
057300        MOVE MSG-KDMFSFOR-1       TO MFS-KDMFSFOR                         
057400     END-IF                                                               
057500                                                                          
057600     MOVE MSG-KDTRTYP             TO MFS-KDTRTYP                          
057700     MOVE MFS-IDTRANS             TO W-IDTRANS                            
057800                                                                          
057900     MOVE LOW-VALUE       TO MSG-AREA                                     
058000     MOVE 'W5O111N1'      TO MFS-IDMOD                                    
058100     MOVE '5111'          TO MOD-IDTRANS                                  
058200                                                                          
058300     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
058400                             MOD-TEMFSFEL                                 
058500                             MOD-TEMFSINF                                 
058600     .                                                                    
058700     EJECT                                                                
058800 B-KOLLA-NYCKEL SECTION.                                                  
058900                                                                          
059000     IF MFS-UPD-X                                                         
059100****************  DISPATCHANROP                                           
059200                                                                          
059300       IF MID-IDARTNR-IN = ALL '+'                                        
059400         MOVE MID-IDARTNR-UT TO IDARTNR-WS                                
059500         INSPECT IDARTNR-WS REPLACING LEADING SPACE BY ZERO               
059600       ELSE                                                               
059700         MOVE MID-IDARTNR-IN TO IDARTNR-WS                                
059800       END-IF                                                             
059900     ELSE                                                                 
060000       MOVE ALL '+'           TO MSGI-WMSGINIT                            
060100       MOVE '001'             TO MSGI-KDCALL                              
060200       MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                              
060300       MOVE '5111'            TO MSGI-IDTRANS                             
060400       MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                        
060500       IF MFS-IDTRANS = '5111'                                            
060600       OR (MID-IDARTNR-IN NUMERIC                                         
060700       AND MID-IDARTNR-IN > ZERO)                                         
060800         MOVE MID-IDARTNR-IN  TO MSGI-IDARTNR                             
060900       END-IF                                                             
061000       CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                         
061100       MOVE MSGI-IDARTNR      TO IDARTNR-WS                               
061200       INSPECT IDARTNR-WS REPLACING LEADING SPACE BY ZERO                 
061300*CODE TO HANDLE SPACE IN THE SUFFIX OF THE PART NUMBER                    
061400       MOVE FUNCTION TRIM(IDARTNR-WS)                                     
061500                              TO WS-IDARTNR-N                             
061600       MOVE WS-IDARTNR-N      TO IDARTNR-WS                               
061700     END-IF                                                               
061800     IF NOT EGEN-TRANS                                                    
061900       MOVE ' '               TO MFS-KDTRTYP                              
062000     END-IF                                                               
062100                                                                          
062200     IF IDARTNR-WS NOT NUMERIC                                            
062300       MOVE NEJ               TO NYCKLAR-OK                               
062400     ELSE                                                                 
062500       MOVE IDARTNR-WS        TO W-IDARTNR                                
062600     END-IF                                                               
062700                                                                          
062800*    -- KONTROLL AV IDLEVNR                                               
062900     MOVE MFS-RENSA-FAELT TO MOD-IDLEVNR-IN                               
063000                                                                          
063100     IF MID-IDLEVNR-IN = ALL '+'                                          
063200       MOVE MID-IDLEVNR-UT    TO WS-IDLEVNR                               
063300     ELSE                                                                 
063400       MOVE MID-IDLEVNR-IN    TO WS-IDLEVNR                               
063500     END-IF                                                               
063600                                                                          
063700*    -- KONTROLL AV KDPRBEH                                               
063800     MOVE MFS-RENSA-FAELT     TO MOD-KDPRBEH-IN                           
063900                                                                          
064000     IF MID-KDPRBEH-IN = ALL '+'                                          
064100       MOVE MID-KDPRBEH-UT    TO WS-KDPRBEH                               
064200     ELSE                                                                 
064300       MOVE MID-KDPRBEH-IN    TO WS-KDPRBEH                               
064400     END-IF                                                               
064500                                                                          
064600*    -- KONTROLL AV KDFPKPRI                                              
064700     IF MID-KDFPKPRI-U = 'Y' OR 'J' OR 'N' OR '?' OR SPACE                
064800       IF MID-KDFPKPRI-U = 'J'                                            
064900         MOVE 'Y'               TO WS-KDFPKPRI                            
065000       ELSE                                                               
065100         MOVE MID-KDFPKPRI-U    TO WS-KDFPKPRI                            
065200       END-IF                                                             
065300     ELSE                                                                 
065400       MOVE SPACE               TO WS-KDFPKPRI                            
065500     END-IF                                                               
065600                                                                          
065700*    -- KONTROLL AV REAENDR                                               
065800     MOVE MFS-RENSA-FAELT     TO MOD-REAENDR-IN                           
065900                                                                          
066000     IF MID-REAENDR-IN = ALL '+'                                          
066100       MOVE MID-REAENDR-UT    TO WS-REAENDR                               
066200     ELSE                                                                 
066300       MOVE MID-REAENDR-IN    TO WS-REAENDR                               
066400     END-IF                                                               
066500                                                                          
066600     MOVE WS-REAENDR          TO DEC-IDFRIDATA                            
066700     MOVE 4                   TO DEC-KVHELTAL                             
066800     MOVE 1                   TO DEC-KVDECIMAL                            
066900                                                                          
067000     CALL WDECEDIT USING DEC-WDECAREA                                     
067100                                                                          
067200     IF DEC-KDSVAR-OK                                                     
067300       MOVE DEC-IDEDITDATA    TO WS-REAENDR-NUM                           
067400     ELSE                                                                 
067500       MOVE ZERO              TO WS-REAENDR-NUM                           
067600     END-IF                                                               
067700                                                                          
067800     IF NYCKLAR-OK = NEJ                                                  
067900       IF GODK-TRANS                                                      
068000         MOVE IDARTNR-WS      TO MOD-IDARTNR-UT                           
068100         MOVE WS-IDLEVNR      TO MOD-IDLEVNR-UT                           
068200         MOVE WS-KDPRBEH      TO MOD-KDPRBEH-UT                           
068300         MOVE WS-REAENDR-NUM  TO MOD-REAENDR-UT                           
068400       ELSE                                                               
068500         MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-UT                           
068600                                 MOD-IDLEVNR-UT                           
068700                                 MOD-KDPRBEH-UT                           
068800                                 MOD-REAENDR-UT                           
068900       END-IF                                                             
069000     ELSE                                                                 
069100       MOVE IDARTNR-WS        TO MOD-IDARTNR-UT                           
069200       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
069300       MOVE WS-IDLEVNR        TO MOD-IDLEVNR-UT                           
069400       MOVE WS-KDPRBEH        TO MOD-KDPRBEH-UT                           
069500       MOVE WS-REAENDR-NUM    TO MOD-REAENDR-UT                           
069600     END-IF                                                               
069700     .                                                                    
069800     EJECT                                                                
069900                                                                          
070000 C-UPPDATERA  SECTION.                                                    
070100     IF MID-FLPRIGO-U = ALL '+'                                           
070200       PERFORM CA-FORMELL-KONTROLL-MID                                    
070300       IF MFS-UPDATE                                                      
070400         IF FLFEL-FAELT = NEJ                                             
070500           IF MID-TIPRLIST-U NOT = ALL '+'                                
070600             PERFORM CB-KONTROLLERA-MID-MOT-BAS                           
070700           END-IF                                                         
070800           IF FLFEL-FAELT = NEJ                                           
070900             IF MID-TIPRLIST-U NOT = ALL '+'                              
071000               PERFORM IMS-GHU-WLARTC01                                   
071100               IF SEGMENT-FINNS                                           
071200                 PERFORM IMS-GHNP-WLARTC11                                
071300                 IF CLAG-PRARTSTD = +0                                    
071400                   MOVE W-FEL-11 TO MOD-TEMFSFEL                          
071500                   MOVE JA TO FLFEL-FAELT                                 
071600                 ELSE                                                     
071700                   COMPUTE PRISANDRING ROUNDED =                          
071800                           W-PRARTBEL *                                   
071900                           W-RETULF * W-PRKURS /                          
072000                           W-REVALUTA / W-PRARTBES                        
072100                            ON SIZE ERROR                                 
072200                              MOVE 999.99 TO PRISANDRING                  
072300                   END-COMPUTE                                            
072400                 END-IF                                                   
072500               END-IF                                                     
072600             END-IF                                                       
072700           END-IF                                                         
072800         END-IF                                                           
072900       ELSE                                                               
073000         PERFORM CC-KONTROLLERA-BESTPRIS                                  
073100       END-IF                                                             
073200     ELSE                                                                 
073300       IF MID-FLPRIGO-U = JA OR NEJ                                       
073400         MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLPRIGO-U-ATTR                  
073500         IF MID-IDLEVNR-U NOT  = ALL '+'                                  
073600           MOVE MID-IDLEVNR-U TO W-IDLEVNR                                
073700         END-IF                                                           
073800         PERFORM IMS-GHU-WLARTC01                                         
073900         PERFORM CC-KONTROLLERA-BESTPRIS                                  
074000       ELSE                                                               
074100         PERFORM MFS-STAENG-FAELT-INDATA                                  
074200         MOVE MFS-ALFA-FAELT-FEL TO MOD-FLPRIGO-U-ATTR                    
074300         MOVE MFS-ROER-EJ-FAELT  TO MOD-FLPRIGO-U                         
074400                                    MOD-SVAR-U                            
074500         MOVE W-FEL-3            TO MOD-TEMFSFEL                          
074600         MOVE JA                 TO FLFEL-FAELT                           
074700       END-IF                                                             
074800     END-IF                                                               
074900                                                                          
075000     PERFORM MFS-ROER-EJ-FAELT-UTDATA                                     
075100     IF FLFEL-FAELT = JA                                                  
075200       PERFORM MFS-ROER-EJ-FAELT-INDATA                                   
075300     ELSE                                                                 
075400       IF MID-FLPRIGO-U = NEJ                                             
075500         PERFORM CD-ANGRA-STOR-PRISOKNING                                 
075600       ELSE                                                               
075700         IF MID-FLPRIGO-U = JA                                            
075800           PERFORM MFS-OEPPNA-FAELT-INDATA                                
075900           PERFORM MFS-FORMATETS-ATTR-INDATA                              
076000           MOVE '    '           TO MOD-SVAR-U                            
076100           MOVE MFS-RENSA-FAELT  TO MOD-FLPRIGO-U                         
076200           MOVE MFS-STAENG-FAELT TO MOD-FLPRIGO-U-ATTR                    
076300         END-IF                                                           
076400       END-IF                                                             
076500       IF PRISANDRING > 1.20 OR                                           
076600          (PRISANDRING > 0 AND < 0.80)                                    
076700         PERFORM MFS-ROER-EJ-FAELT-INDATA                                 
076800         PERFORM MFS-STAENG-FAELT-INDATA                                  
076900         MOVE 'SVAR'           TO MOD-SVAR-U                              
077000         MOVE MFS-RENSA-FAELT  TO MOD-FLPRIGO-U                           
077100         MOVE MFS-OEPPNA-ALFA-FAELT-HI TO                                 
077200                                 MOD-FLPRIGO-U-ATTR                       
077300         MOVE W-FEL-7          TO MOD-TEMFSFEL                            
077400       ELSE                                                               
077500         IF MID-FLPRIGO-U NOT = NEJ                                       
077600           PERFORM MFS-RENSA-FAELT-INDATA                                 
077700           PERFORM MFS-FORMATETS-ATTR-INDATA                              
077800           PERFORM CE-UPPDATERA                                           
077900           MOVE W-MED-1 TO MOD-TEMFSINF                                   
078000           IF MED-1A                                                      
078100             MOVE W-MED-1A     TO MOD-TEMFSFEL                            
078200           ELSE                                                           
078300             IF MED-1B                                                    
078400               MOVE W-MED-1B   TO MOD-TEMFSFEL                            
078500             ELSE                                                         
078600               IF MED-1C                                                  
078700                 MOVE W-MED-1C TO MOD-TEMFSFEL                            
078800               END-IF                                                     
078900             END-IF                                                       
079000           END-IF                                                         
079100         END-IF                                                           
079200       END-IF                                                             
079300     END-IF                                                               
079400                                                                          
079500     .                                                                    
079600     EJECT                                                                
079700 CA-FORMELL-KONTROLL-MID SECTION.                                         
079800                                                                          
079900     PERFORM IMS-GHU-WLARTC01                                             
080000                                                                          
080100     IF MFS-UPD-X                                                         
080200       IF SEGMENT-SAKNAS                                                  
080300         MOVE ERR-WRONG-KEY TO MSG-KOM-IDMFSMED                           
080400         MOVE JA            TO FLFEL-FAELT                                
080500       ELSE                                                               
080600         IF SEGMENT-FINNS                                                 
080700           IF MID-IDLEVNR-U NOT = ALL '+'                                 
080800             MOVE MID-IDLEVNR-U TO W-IDLEVNR                              
080900           END-IF                                                         
081000         END-IF                                                           
081100       END-IF                                                             
081200     ELSE                                                                 
081300       IF SEGMENT-FINNS                                                   
081400         IF MID-UPPDAT-RAD = ALL '+'                                      
081500           MOVE W-FEL-6     TO MOD-TEMFSFEL                               
081600           MOVE JA          TO FLFEL-FAELT                                
081700         ELSE                                                             
081800           IF (MID-TEARTNOT-U NOT = ALL '+') AND                          
081900              (MID-KDPRURSP-U     = ALL '+') AND                          
082000              (MID-TIPRLIST-U     = ALL '+') AND                          
082100              (MID-PRARTBEL-U     = ALL '+') AND                          
082200*             (MID-RETULF-U       = ALL '+') AND                          
082300              (MID-KDVALISO-U     = ALL '+') AND                          
082400              (MID-PRLFKST-U      = ALL '+') AND                          
082500              (MID-FLPRIBES-U     = '+')     AND                          
082600              (MID-IDLEVNR-U      = '+')                                  
082700* **          ENDAST NOTERINGSFÄLT FÖR STÄLLKOSTNAD ÄR IFYLLT **          
082800             MOVE MFS-ALFA-FAELT-RAETT TO MOD-TEARTNOT-U-ATTR             
082900           ELSE                                                           
083000* **          HÄR KONTROLLERAS PRISHÄRSTAMNING               **           
083100                                                                          
083200             IF MID-KDPRURSP-U = 'F' OR 'B' OR 'A' OR 'P' OR 'Å'          
083300               MOVE MFS-ALFA-FAELT-RAETT TO                               
083400                      MOD-KDPRURSP-U-ATTR                                 
083500             ELSE                                                         
083600               IF MID-FLPRIBES-U NOT = JA                                 
083700                 MOVE MFS-ALFA-FAELT-FEL TO                               
083800                         MOD-KDPRURSP-U-ATTR                              
083900                 MOVE W-FEL-3 TO MOD-TEMFSFEL                             
084000                 MOVE JA      TO FLFEL-FAELT                              
084100               ELSE                                                       
084200                 MOVE MFS-ALFA-FAELT-RAETT TO                             
084300                         MOD-KDPRURSP-U-ATTR                              
084400               END-IF                                                     
084500             END-IF                                                       
084600                                                                          
084700* **          HÄR KONTROLLERAS OM ANGIVIT DATUM ÄR ETT       **           
084800* **          GILTIGT DATUM                                  **           
084900                                                                          
085000             IF MID-TIPRLIST-U NOT = ALL '+'                              
085100               MOVE 'AAMMDD'       TO DAT-KDDATFORM                       
085200               MOVE MID-TIPRLIST-U TO DAT-I-TIDATUM                       
085300               CALL WDATKONV USING    DAT-KDDATFORM                       
085400                                      DAT-I-TIDATUM                       
085500                                      DAT-O-TIDATUM                       
085600                                      DAT-KDSVAR                          
085700               IF DAT-KDSVAR-FEL                                          
085800                 MOVE MFS-NUM-FAELT-FEL TO MOD-TIPRLIST-U-ATTR            
085900                 MOVE W-FEL-3           TO MOD-TEMFSFEL                   
086000                 MOVE JA                TO FLFEL-FAELT                    
086100               ELSE                                                       
086200                 MOVE MFS-NUM-FAELT-RAETT TO                              
086300                          MOD-TIPRLIST-U-ATTR                             
086400               END-IF                                                     
086500             ELSE                                                         
086600               MOVE MFS-NUM-FAELT-FEL TO                                  
086700                     MOD-TIPRLIST-U-ATTR                                  
086800               MOVE W-FEL-3 TO MOD-TEMFSFEL                               
086900               MOVE JA      TO FLFEL-FAELT                                
087000             END-IF                                                       
087100                                                                          
087200* **          HÄR KONTROLLERAS NYTT BEST.PRIS SÅ ATT ANTALET **           
087300* **          HELTALSSIFFROR OCH ANTALET DECIMALER INTE      **           
087400* **          ÖVERSKRIDER MAXANTAL TILLÅTNA                  **           
087500* **     OBS FÖR ATT KLARA 5 DEC ANVÄNDS WDECSTR I STÄLLET   **           
087600                                                                          
087700             IF MID-PRARTBEL-U NOT = ALL '+'                              
087800               MOVE MID-PRARTBEL-U TO STR-IDFRIDATA                       
087900                                      WS-PRARTBEL                         
088000               MOVE +8             TO STR-KVHELTAL                        
088100               MOVE +5             TO STR-KVDECIMAL                       
088200               MOVE NEJ            TO STR-KDSIGNAT                        
088300               MOVE NEJ            TO STR-KDLEFTJUST                      
088400               CALL WDECSTR  USING STR-WDECSTR                            
088500               IF STR-KDSVAR-FEL                                          
088600                 MOVE MFS-NUM-FAELT-FEL TO                                
088700                        MOD-PRARTBEL-U-ATTR                               
088800                 MOVE W-FEL-3      TO MOD-TEMFSFEL                        
088900                 MOVE JA           TO FLFEL-FAELT                         
089000               ELSE                                                       
089100                 MOVE STR-IDSTRDATA   TO W-IDSTRDATA-X                    
089200                 IF W-IDSTRDATA-N <= 0                                    
089300                   MOVE MFS-NUM-FAELT-FEL TO                              
089400                             MOD-PRARTBEL-U-ATTR                          
089500                   MOVE W-FEL-3 TO MOD-TEMFSFEL                           
089600                   MOVE JA      TO FLFEL-FAELT                            
089700                 ELSE                                                     
089800                   MOVE W-IDSTRDATA-N TO W-PRARTBEL                       
089900                   MOVE MFS-NUM-FAELT-RAETT TO                            
090000                          MOD-PRARTBEL-U-ATTR                             
090100                 END-IF                                                   
090200               END-IF                                                     
090300             ELSE                                                         
090400               MOVE MFS-NUM-FAELT-FEL TO                                  
090500                      MOD-PRARTBEL-U-ATTR                                 
090600               MOVE W-FEL-3 TO MOD-TEMFSFEL                               
090700               MOVE JA      TO FLFEL-FAELT                                
090800             END-IF                                                       
090900                                                                          
091000* **          HÄR KONTROLLERAS TULLKURSEN SÅ ATT ANTALET **               
091100* **          HELTALSSIFFROR OCH ANTALET DECIMALER INTE  **               
091200* **          ÖVERSKRIDER MAXANTAL TILLÅTNA              **               
091300                                                                          
091400*            IF MID-RETULF-U NOT = ALL '+'                                
091500*              MOVE MID-RETULF-U TO DEC-IDFRIDATA                         
091600*            ELSE                                                         
091700               MOVE MID-RETULF   TO DEC-IDFRIDATA                         
091800*            END-IF                                                       
091900             MOVE +3             TO DEC-KVHELTAL                          
092000             MOVE +4             TO DEC-KVDECIMAL                         
092100             CALL WDECEDIT USING DEC-WDECAREA                             
092200             IF DEC-KDSVAR-FEL                                            
092300*              MOVE MFS-NUM-FAELT-FEL TO                                  
092400*                    MOD-RETULF-U-ATTR                                    
092500               MOVE W-FEL-3 TO MOD-TEMFSFEL                               
092600               MOVE JA      TO FLFEL-FAELT                                
092700             ELSE                                                         
092800               IF DEC-IDEDITDATA = 0                                      
092900                 IF MID-RETULF-LEV = +0                                   
093000*                  MOVE MFS-NUM-FAELT-FEL TO                              
093100*                          MOD-RETULF-U-ATTR                              
093200                   MOVE W-FEL-3 TO MOD-TEMFSFEL                           
093300                   MOVE JA      TO FLFEL-FAELT                            
093400                 ELSE                                                     
093500                   MOVE JA             TO FLLEV-TULLF                     
093600                   MOVE MID-RETULF-LEV TO W-RETULF                        
093700*                  MOVE MFS-NUM-FAELT-RAETT TO                            
093800*                            MOD-RETULF-U-ATTR                            
093900                 END-IF                                                   
094000               ELSE                                                       
094100                 MOVE DEC-IDEDITDATA TO W-RETULF                          
094200*                MOVE MFS-NUM-FAELT-RAETT TO                              
094300*                        MOD-RETULF-U-ATTR                                
094400               END-IF                                                     
094500             END-IF                                                       
094600*                                                                         
094700* **          HÄR KONTROLLERAS VALUTA                        **           
094800                                                                          
094900             IF MID-KDVALISO-U NOT = ALL '+'                              
095000               MOVE MFS-ALFA-FAELT-RAETT TO                               
095100                      MOD-KDVALISO-U-ATTR                                 
095200             ELSE                                                         
095300               IF MID-KDVALISO = SPACE                                    
095400                 MOVE MFS-ALFA-FAELT-FEL TO                               
095500                        MOD-KDVALISO-U-ATTR                               
095600                 MOVE W-FEL-3 TO MOD-TEMFSFEL                             
095700                 MOVE JA      TO FLFEL-FAELT                              
095800               END-IF                                                     
095900             END-IF                                                       
096000                                                                          
096100* **          HÄR KONTROLLERAS FÖRPACKNINGSKOSTNAD SÅ ATT       **        
096200* **          ANTALET HELTALSSIFFROR OCH ANTALET DECIMALER INTE **        
096300* **          INTE ÖVERSKRIDER MAXANTAL TILLÅTNA                **        
096400                                                                          
096500             IF MID-PRLFKST-U NOT = ALL '+'                               
096600               MOVE MID-PRLFKST-U TO DEC-IDFRIDATA                        
096700               MOVE +3            TO DEC-KVHELTAL                         
096800               MOVE +2            TO DEC-KVDECIMAL                        
096900               CALL WDECEDIT USING DEC-WDECAREA                           
097000               IF DEC-KDSVAR-FEL                                          
097100                 MOVE MFS-NUM-FAELT-FEL TO                                
097200                        MOD-PRLFKST-U-ATTR                                
097300                 MOVE W-FEL-3 TO MOD-TEMFSFEL                             
097400                 MOVE JA      TO FLFEL-FAELT                              
097500               ELSE                                                       
097600                 MOVE DEC-IDEDITDATA TO W-PRLFKST                         
097700                 MOVE MFS-NUM-FAELT-RAETT TO                              
097800                          MOD-PRLFKST-U-ATTR                              
097900               END-IF                                                     
098000             END-IF                                                       
098100                                                                          
098200* **          FLAGGAT PRIS                        **                      
098300             IF MID-FLPRIBES-U = JA                                       
098400               MOVE MFS-ALFA-FAELT-RAETT TO                               
098500                      MOD-FLPRIBES-U-ATTR                                 
098600             ELSE                                                         
098700               IF MID-FLPRIBES-U NOT = '+'                                
098800                 MOVE MFS-ALFA-FAELT-FEL TO                               
098900                       MOD-FLPRIBES-U-ATTR                                
099000                 MOVE W-FEL-3 TO MOD-TEMFSFEL                             
099100                 MOVE JA      TO FLFEL-FAELT                              
099200               END-IF                                                     
099300             END-IF                                                       
099400                                                                          
099500****       HÄR KONTROLLERAS IDLEVNR PÅ UPPDATERINGSRADEN                  
099600                                                                          
099700             IF MID-IDLEVNR-U NOT = ALL '+'                               
099800               IF MID-IDLEVNR-U = SPACE                                   
099900                 MOVE MFS-ALFA-FAELT-FEL TO                               
100000                 MOD-IDLEVNR-U-ATTR                                       
100100                 MOVE W-FEL-3  TO MOD-TEMFSFEL                            
100200                 MOVE JA      TO FLFEL-FAELT                              
100300               ELSE                                                       
100400                 MOVE MID-IDLEVNR-U TO W-IDLEVNR                          
100500                 MOVE MFS-ALFA-FAELT-RAETT TO                             
100600                       MOD-IDLEVNR-U-ATTR                                 
100700               END-IF                                                     
100800             END-IF                                                       
100900                                                                          
101000* **          NOTERINGSFÄLT FÖR STÄLLKOSTNAD      **                      
101100                                                                          
101200             IF MID-TEARTNOT-U NOT = ALL '+'                              
101300               MOVE MFS-ALFA-FAELT-RAETT TO                               
101400                       MOD-TEARTNOT-U-ATTR                                
101500             END-IF                                                       
101600*    -- KONTROLL AV KDFPKPRI                                              
101700             IF MID-KDFPKPRI-U = 'Y' OR 'J' OR 'N' OR '?' OR SPACE        
101800               IF MID-KDFPKPRI-U = 'J'                                    
101900                 MOVE 'Y'               TO WS-KDFPKPRI                    
102000               ELSE                                                       
102100                 MOVE MID-KDFPKPRI-U    TO WS-KDFPKPRI                    
102200               END-IF                                                     
102300               MOVE MFS-ALFA-FAELT-RAETT TO                               
102400                       MOD-KDFPKPRI-U-ATTR                                
102500             ELSE                                                         
102600               MOVE SPACE               TO WS-KDFPKPRI                    
102700               MOVE MFS-ALFA-FAELT-FEL TO                                 
102800                       MOD-KDFPKPRI-U-ATTR                                
102900               MOVE JA      TO FLFEL-FAELT                                
103000             END-IF                                                       
103100                                                                          
103200           END-IF                                                         
103300         END-IF                                                           
103400       ELSE                                                               
103500         MOVE W-FEL-1 TO MOD-TEMFSFEL                                     
103600         PERFORM MFS-RENSA-FAELT-UTDATA                                   
103700         PERFORM MFS-RENSA-FAELT-INDATA                                   
103800         MOVE JA      TO FLFEL-FAELT                                      
103900       END-IF                                                             
104000     END-IF                                                               
104100     .                                                                    
104200     EJECT                                                                
104300 CB-KONTROLLERA-MID-MOT-BAS SECTION.                                      
104400                                                                          
104500     IF MID-FLPRIBES-U NOT = JA                                           
104600* **    HÄR LÄSES BEST.PRIS SEGMENT FÖR ATT                  **           
104700* **    KONTROLLERA OM NYUPPLÄGG ELLER                       **           
104800* **    UPPDATERING ÄR TILLÅTEN FÖR ANGIVET                  **           
104900* **    DATUM                                                **           
105000                                                                          
105100       MOVE 1 TO INDX                                                     
105200       PERFORM UNTIL SEGMENT-SAKNAS OR INDX > 5 OR                        
105300                     FLSLUTA-LAS = JA                                     
105400         PERFORM IMS-GHNP-WLARTC21-NEXT                                   
105500         IF SEGMENT-FINNS                                                 
105600           IF PRL-FLHUVLEV = JA                                           
105700             IF PRL-KDSTATUS-PR = 1                                       
105800               MOVE JA  TO INLEV-PRISRAD                                  
105900               IF PRL-SUINLEV-PR > 0                                      
106000                 ADD +1 TO ANT-INLEV                                      
106100               END-IF                                                     
106200             END-IF                                                       
106300             IF INDX = 5 AND PRL-SUINLEV-PR > 0 AND                       
106400                PRL-KDSTATUS-PR = 1 AND ANT-INLEV = 1                     
106500               MOVE MFS-NUM-FAELT-FEL TO                                  
106600                        MOD-TIPRLIST-U-ATTR                               
106700               MOVE W-FEL-3 TO MOD-TEMFSFEL                               
106800               MOVE JA      TO FLFEL-FAELT                                
106900               MOVE JA      TO FLSLUTA-LAS                                
107000             ELSE                                                         
107100               SUBTRACT PRL-DAPRLIST-9KOMPL FROM W-DAPRLIST-MAX           
107200               GIVING W-DAPRLIST                                          
107300               MOVE W-LISTDATUM  TO W-TIPRLIST                            
107400               MOVE MID-TIPRLIST-U   TO TMP1-YYMMDD                       
107500               MOVE W-TIPRLIST       TO TMP2-YYMMDD                       
107600               PERFORM WY2000P1                                           
107700               IF TMP1-YYMMDD > TMP2-YYMMDD                               
107800                 MOVE MFS-NUM-FAELT-RAETT TO                              
107900                          MOD-TIPRLIST-U-ATTR                             
108000                 ADD 1 TO INDX                                            
108100               ELSE                                                       
108200                 IF PRL-SUINLEV-PR > 0 AND                                
108300                    PRL-KDSTATUS-PR = 1                                   
108400                   MOVE MFS-NUM-FAELT-FEL TO                              
108500                            MOD-TIPRLIST-U-ATTR                           
108600                   MOVE W-FEL-3 TO MOD-TEMFSFEL                           
108700                   MOVE JA      TO FLFEL-FAELT                            
108800                   MOVE JA      TO FLSLUTA-LAS                            
108900                 ELSE                                                     
109000                   IF MID-TIPRLIST-U = W-TIPRLIST                         
109100                     MOVE MFS-NUM-FAELT-RAETT TO                          
109200                              MOD-TIPRLIST-U-ATTR                         
109300                     MOVE JA TO FLSLUTA-LAS                               
109400                   ELSE                                                   
109500                     ADD  1  TO INDX                                      
109600                   END-IF                                                 
109700                 END-IF                                                   
109800               END-IF                                                     
109900             END-IF                                                       
110000           END-IF                                                         
110100         ELSE                                                             
110200           MOVE MFS-NUM-FAELT-RAETT TO                                    
110300                  MOD-TIPRLIST-U-ATTR                                     
110400         END-IF                                                           
110500       END-PERFORM                                                        
110600       IF INDX > 5 AND ANT-INLEV = 0                                      
110700         MOVE MFS-NUM-FAELT-RAETT TO                                      
110800                MOD-TIPRLIST-U-ATTR                                       
110900       END-IF                                                             
111000     END-IF                                                               
111100*                                                                         
111200* ** HÄR KONTROLLERAS ATT KURS FINNS UPPLAGD FÖR       **                 
111300* ** INMATAD VALUTA ELLER ATT KURS FINNS FÖR VALUTA    **                 
111400* ** SOM GÄLLER FÖR INKÖPSLAND.                        **                 
111500                                                                          
111600     MOVE DAT-TIAA             TO TMP1-YY                                 
111700     MOVE DAGENS-DAT(3:2)      TO TMP2-YY                                 
111800     PERFORM WY2000P9                                                     
111900     IF TMP1-YY < TMP2-YY                                                 
112000       MOVE DAGENS-DAT(3:2)    TO W-DATE-AAMM(1:2)                        
112100     ELSE                                                                 
112200       MOVE DAT-TIAA           TO W-DATE-AAMM(1:2)                        
112300     END-IF                                                               
112400     MOVE 01                   TO W-DATE-AAMM(3:2)                        
112500                                                                          
112600     IF MID-KDVALISO-U NOT = ALL '+'                                      
112700       MOVE MID-KDVALISO-U     TO CURR-KDVALISO-ROW                       
112800     ELSE                                                                 
112900       MOVE MID-KDVALISO       TO CURR-KDVALISO-ROW                       
113000     END-IF                                                               
113100     MOVE 'SEK'                TO CURR-KDVALISO-HUV                       
113200                                                                          
113300****** KOLLA OM MID-IDLEVR FINNS PÅ WDF5-LEVERANTÖRSREG                   
113400                                                                          
113500     IF MID-IDLEVNR-U NOT = ALL '+'                                       
113600       MOVE MID-IDLEVNR-U         TO W-IDLEVNR                            
113700       PERFORM IMS-GU-WLLEVA01                                            
113800       IF SEGMENT-SAKNAS                                                  
113900         MOVE MFS-ALFA-FAELT-FEL  TO MOD-IDLEVNR-U-ATTR                   
114000         MOVE W-FEL-3             TO MOD-TEMFSFEL                         
114100         MOVE JA                  TO FLFEL-FAELT                          
114200       ELSE                                                               
114300**** HÄMTA NYTT RETULF OM MID-IDLEVNR-U ÄR IFYLLT ****                    
114400         IF SEGMENT-FINNS                                                 
114500           MOVE 'SE' TO W-IDLAND                                          
114600           PERFORM IMS-GNP-WLLEVA11                                       
114700           IF SEGMENT-FINNS                                               
114800             IF LEV-TULL-TITULF < DAGENS-AAMMDD                           
114900               MOVE LEV-TULL-RETULF-1  TO W-RETULF                        
115000                                       MOD-RETULF-LEV                     
115100             ELSE                                                         
115200               MOVE LEV-TULL-RETULF-2  TO W-RETULF                        
115300                                       MOD-RETULF-LEV                     
115400             END-IF                                                       
115500           END-IF                                                         
115600         END-IF                                                           
115700       END-IF                                                             
115800     END-IF                                                               
115900                                                                          
116000     MOVE W-DATE-AAMM         TO CURR-TIAAMM                              
116100     MOVE 'A'                 TO CURR-KDVALTYP                            
116200     CALL W510CURR USING CURR-W510CURR 9305-PCB                           
116300     IF CURR-KDSVAR = ' '                                                 
116400       MOVE CURR-PRKURS-NEW    TO W-PRKURS                                
116500       MOVE CURR-REVALUTA-TO   TO W-REVALUTA                              
116600       IF MID-KDVALISO-U NOT = ALL '+'                                    
116700         MOVE MFS-ALFA-FAELT-RAETT TO                                     
116800               MOD-KDVALISO-U-ATTR                                        
116900       END-IF                                                             
117000                                                                          
117100* **    HÄR KONTROLLERAS BEST.PRIS I KRONOR SÅ ATT    **                  
117200* **    DET INTE ÖVERSKRIDER 400 000 KRONOR           **                  
117300                                                                          
117400       COMPUTE W-PRARTBES ROUNDED = W-PRARTBEL * W-RETULF *               
117500                                    W-PRKURS / W-REVALUTA                 
117600       IF W-PRARTBES = +0                                                 
117700         MOVE +0.01      TO W-PRARTBES                                    
117800       END-IF                                                             
117900       IF W-PRARTBES > MAX-PRARTBES                                       
118000         MOVE MFS-NUM-FAELT-FEL TO                                        
118100                MOD-PRARTBEL-U-ATTR                                       
118200*               MOD-RETULF-U-ATTR                                         
118300         MOVE MFS-ALFA-FAELT-FEL TO                                       
118400                MOD-KDVALISO-U-ATTR                                       
118500         MOVE W-FEL-3 TO MOD-TEMFSFEL                                     
118600         MOVE JA      TO FLFEL-FAELT                                      
118700       END-IF                                                             
118800                                                                          
118900     ELSE                                                                 
119000       MOVE MFS-ALFA-FAELT-FEL TO                                         
119100            MOD-KDVALISO-U-ATTR                                           
119200       MOVE W-FEL-14 TO MOD-TEMFSFEL                                      
119300       MOVE JA       TO FLFEL-FAELT                                       
119400     END-IF                                                               
119500     .                                                                    
119600     EJECT                                                                
119700 CC-KONTROLLERA-BESTPRIS SECTION.                                         
119800                                                                          
119900     MOVE 0 TO INDX                                                       
120000     PERFORM UNTIL SEGMENT-SAKNAS OR INDX > 5                             
120100     OR FLSLUTA-LAS = JA                                                  
120200       PERFORM IMS-GHNP-WLARTC21-NEXT                                     
120300       IF SEGMENT-FINNS                                                   
120400         IF PRL-FLHUVLEV = JA                                             
120500           ADD 1    TO INDX                                               
120600           IF PRL-KDSTATUS-PR = 1                                         
120700             MOVE JA  TO INLEV-PRISRAD                                    
120800             MOVE JA  TO FLSLUTA-LAS                                      
120900           END-IF                                                         
121000         END-IF                                                           
121100       END-IF                                                             
121200     END-PERFORM                                                          
121300     .                                                                    
121400     EJECT                                                                
121500 CD-ANGRA-STOR-PRISOKNING SECTION.                                        
121600                                                                          
121700     PERFORM MFS-RENSA-FAELT-INDATA                                       
121800     PERFORM MFS-FORMATETS-ATTR-INDATA                                    
121900     MOVE '    '           TO MOD-SVAR-U                                  
122000     MOVE MFS-RENSA-FAELT  TO MOD-FLPRIGO-U                               
122100     MOVE MFS-STAENG-FAELT TO MOD-FLPRIGO-U-ATTR                          
122200     MOVE W-FEL-10         TO MOD-TEMFSFEL                                
122300     .                                                                    
122400     EJECT                                                                
122500 CE-UPPDATERA SECTION.                                                    
122600                                                                          
122700     IF (MID-FLPRIBES-U = '+') AND                                        
122800        (MID-TIPRLIST-U NOT = ALL '+')                                    
122900       PERFORM CEH-KONTROLLERA-OM-INLEV-SKETT                             
123000     END-IF                                                               
123100                                                                          
123200* ** OM X-TRANS ELLER                                 **                  
123300* ** OM UPPDATERING MED STOR PRISÖKNING BEKRÄFTATS    **                  
123400* ** FINNS EJ REDIGERADE VÄRDEN AV NYTT BEST.PRIS     **                  
123500* ** OCH TULLKURS SPARADE. STANDARDKURS SAKNAS OCKSÅ. **                  
123600                                                                          
123700     MOVE DAGENS-DAT       TO PRI-DAREGDAT                                
123800     MOVE DAGENS-TID(1:7)  TO PRI-TIREGTID                                
123900     IF MFS-UPD-X                                                         
124000       MOVE MID-IDUSER     TO PRI-IDUSER                                  
124100                              WS-IDUSER                                   
124200       MOVE JA             TO PRI-FLPRFIL                                 
124300     ELSE                                                                 
124400       MOVE MSGI-IDUSER    TO PRI-IDUSER                                  
124500                              WS-IDUSER                                   
124600       MOVE NEJ            TO PRI-FLPRFIL                                 
124700     END-IF                                                               
124800     MOVE SPACE            TO PRI-FLPRIGO                                 
124900     MOVE SPACE            TO PRI-FLPRIBES                                
125000                                                                          
125100     PERFORM CEA-REDIGERA                                                 
125200     PERFORM IMS-GHU-WLARTC01                                             
125300                                                                          
125400     IF SEGMENT-FINNS                                                     
125500       IF MID-IDLEVNR-U = ALL '+'                                         
125600         MOVE ART-IDLEVNR     TO PRI-N-IDLEVNR-PR                         
125700                                 W-IDLEVNR                                
125800       ELSE                                                               
125900         MOVE MID-IDLEVNR-U   TO PRI-N-IDLEVNR-PR                         
126000       END-IF                                                             
126100                                                                          
126200       MOVE W-PRARTBEL        TO PRI-N-PRARTBEL-PR                        
126300       IF MID-FLPRIBES-U = JA                                             
126400         MOVE MID-FLPRIBES-U  TO PRI-FLPRIBES                             
126500       ELSE                                                               
126600         MOVE SPACE           TO PRI-FLPRIBES                             
126700       END-IF                                                             
126800       IF MID-FLPRIGO-U          = JA                                     
126900         MOVE MID-FLPRIGO-U   TO PRI-FLPRIGO                              
127000       ELSE                                                               
127100         MOVE SPACE           TO PRI-FLPRIGO                              
127200       END-IF                                                             
127300       IF (MID-FLPRIBES-U = '+') AND                                      
127400          (MID-TIPRLIST-U NOT = ALL '+')                                  
127500         PERFORM CEB-UPPDATERA-WLARTC21                                   
127600       END-IF                                                             
127700                                                                          
127800       PERFORM CEC-WDK625                                                 
127900* **    EKONOMI-INFO -- WLARTC11 -- WDK611                      **        
128000* **    UPPDATERING                                             **        
128100                                                                          
128200       IF (MID-TIPRLIST-U NOT = ALL '+') OR                               
128300          (MID-FLPRIBES-U NOT = '+')                                      
128400         PERFORM IMS-GHU-WLARTC01                                         
128500         PERFORM IMS-GHNP-WLARTC11                                        
128600*                                                                         
128700*******   RÄKNA UT SALDOT FÖR CDC                                         
128800*******   SPARA-UPPGIFTER FÖR UPPFÖLJNING                                 
128900                                                                          
129000         PERFORM CED-SALDO                                                
129100                                                                          
129200         IF MID-FLPRIBES-U = JA                                           
129300           MOVE SPACE            TO PRI-O-KDPRURSP                        
129400           MOVE ZERO             TO PRI-O-TIPRLIST                        
129500                                    PRI-O-PRARTBES-PR                     
129600                                    PRI-O-PRARTBEL-PR                     
129700                                    PRI-O-KDSTATUS-PR                     
129800                                    PRI-O-SUINLEV-PR                      
129900           MOVE SPACE            TO PRI-O-KDVALISO                        
130000                                    PRI-O-KDCMD                           
130100                                    PRI-O-IDLEVNR-PR                      
130200                                                                          
130300           MOVE SPACE            TO PRI-N-KDPRURSP                        
130400           IF MID-TIPRLIST-U NOT = ALL '+'                                
130500             MOVE MID-TIPRLIST-U TO PRI-N-TIPRLIST                        
130600           ELSE                                                           
130700             MOVE ZERO           TO PRI-N-TIPRLIST                        
130800           END-IF                                                         
130900           MOVE ZERO             TO PRI-N-PRARTBES-PR                     
131000                                    PRI-N-KDSTATUS-PR                     
131100                                    PRI-N-SUINLEV-PR                      
131200           MOVE SPACE            TO PRI-N-KDVALISO                        
131300                                    PRI-N-KDCMD                           
131400                                                                          
131500           MOVE IDARTNR-WS       TO PRI-IDARTNR                           
131600           MOVE 'J'              TO PRI-KDPRIBEH                          
131700           MOVE 'J'              TO PRI-FLKLAR                            
131800           PERFORM R-UPPD-BES-SJK                                         
131900           IF ART-KDSORT = 'SW'                                           
132000             MOVE ZERO           TO CLAG-PRHEMTAG                         
132100           END-IF                                                         
132200         ELSE                                                             
132300           IF INLEV-PRISRAD = NEJ                                         
132400             PERFORM CEE-NY-PRISRAD                                       
132500           ELSE                                                           
132600             IF INLEV-FINNS = JA                                          
132700               PERFORM CEF-INLEV-POST                                     
132800             ELSE                                                         
132900               PERFORM CEG-INLEV-SAKNAS-POST                              
133000             END-IF                                                       
133100           END-IF                                                         
133200         END-IF                                                           
133300         PERFORM IMS-ISRT-WDH801                                          
133400                                                                          
133500* '      IF MID-RETULF-U NOT   = ALL '+'                                  
133600           IF FLLEV-TULLF = JA                                            
133700             MOVE ZERO            TO CLAG-RETULF                          
133800             MOVE MID-RETULF-LEV  TO PRI-N-RETULF                         
133900                                     MOD-RETULF                           
134000             MOVE MFS-RENSA-FAELT TO MOD-FLART-RETULF                     
134100           ELSE                                                           
134200             IF MFS-UPDATE                                                
134300               MOVE W-RETULF      TO CLAG-RETULF                          
134400                                     PRI-N-RETULF                         
134500                                     MOD-RETULF                           
134600               MOVE 'A'           TO MOD-FLART-RETULF                     
134700             END-IF                                                       
134800           END-IF                                                         
135100           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-RETULF-ATTR                  
135200                                         MOD-FLART-RETULF-ATTR            
135300*        ELSE                                                             
135400*          MOVE W-RETULF          TO PRI-N-RETULF                         
135500*        END-IF                                                           
135600                                                                          
135700         IF W-PRLFKST      NOT  = +0                                      
135800           MOVE W-PRLFKST         TO CLAG-PRLFKST                         
135900                                     PRI-N-PRLFKST                        
136000                                     MOD-PRLFKST                          
136100           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-PRLFKST-ATTR                 
136200         ELSE                                                             
136300           MOVE CLAG-PRLFKST      TO PRI-N-PRLFKST                        
136400         END-IF                                                           
136500                                                                          
136600         MOVE W-REDIRLEV          TO PRI-REDIRLEV                         
136700         IF W-SUINLEV > 0                                                 
136800**INBOUND                                                                 
136900            MOVE 1                TO CLAG-KDTIPPR                         
137000         ELSE                                                             
137100**PRICE ROW INSERTED / UPDATED                                            
137200           IF FLSLUTA-LAS = JA                                            
137300             MOVE 3               TO CLAG-KDTIPPR                         
137400           END-IF                                                         
137500         END-IF                                                           
137700         PERFORM IMS-REPL-WLARTC-WDK611                                   
137800       END-IF                                                             
137900     END-IF                                                               
138000     .                                                                    
138100     EJECT                                                                
138200 CEA-REDIGERA  SECTION.                                                   
138300                                                                          
138400     IF MID-TIPRLIST-U NOT = ALL '+'                                      
138500       IF W-PRARTBEL = +0                                                 
138600         MOVE MID-PRARTBEL-U TO STR-IDFRIDATA                             
138700         MOVE +8             TO STR-KVHELTAL                              
138800         MOVE +5             TO STR-KVDECIMAL                             
138900         MOVE NEJ            TO STR-KDSIGNAT                              
139000         MOVE JA             TO STR-KDLEFTJUST                            
139100         CALL WDECSTR  USING STR-WDECSTR                                  
139200         IF STR-KDSVAR-OK                                                 
139300           MOVE STR-IDSTRDATA  TO W-IDSTRDATA-X                           
139400           MOVE W-IDSTRDATA-N  TO W-PRARTBEL                              
139500         END-IF                                                           
139600       END-IF                                                             
139700       IF W-RETULF = +0                                                   
139800*        IF MID-RETULF-U NOT = ALL '+'                                    
139900*          MOVE MID-RETULF-U TO DEC-IDFRIDATA                             
140000*        ELSE                                                             
140100           MOVE MID-RETULF   TO DEC-IDFRIDATA                             
140200*        END-IF                                                           
140300         MOVE +3             TO DEC-KVHELTAL                              
140400         MOVE +4             TO DEC-KVDECIMAL                             
140500         CALL WDECEDIT USING DEC-WDECAREA                                 
140600         IF DEC-KDSVAR-OK                                                 
140700           IF DEC-IDEDITDATA = +0                                         
140800             MOVE MID-RETULF-LEV TO W-RETULF                              
140900             MOVE JA TO FLLEV-TULLF                                       
141000           ELSE                                                           
141100             MOVE DEC-IDEDITDATA TO W-RETULF                              
141200           END-IF                                                         
141300         END-IF                                                           
141400       END-IF                                                             
141500       IF W-PRKURS = +0                                                   
141600         MOVE 'AAMMDD' TO DAT-KDDATFORM                                   
141700         MOVE MID-TIPRLIST-U TO DAT-I-TIDATUM                             
141800         CALL WDATKONV USING    DAT-KDDATFORM                             
141900                                DAT-I-TIDATUM                             
142000                                DAT-O-TIDATUM                             
142100                                DAT-KDSVAR                                
142200         MOVE DAT-TIAA             TO TMP1-YY                             
142300         MOVE DAGENS-DAT(3:2)      TO TMP2-YY                             
142400         PERFORM WY2000P9                                                 
142500         IF TMP1-YY < TMP2-YY                                             
142600           MOVE DAGENS-DAT(3:2)    TO W-DATE-AAMM(1:2)                    
142700         ELSE                                                             
142800           MOVE DAT-TIAA           TO W-DATE-AAMM(1:2)                    
142900         END-IF                                                           
143000         MOVE 01                   TO W-DATE-AAMM(3:2)                    
143100         IF MID-KDVALISO-U NOT = ALL '+'                                  
143200           MOVE MID-KDVALISO-U     TO CURR-KDVALISO-ROW                   
143300         ELSE                                                             
143400           MOVE MID-KDVALISO       TO CURR-KDVALISO-ROW                   
143500         END-IF                                                           
143600         MOVE 'SEK'                TO CURR-KDVALISO-HUV                   
143700         MOVE W-DATE-AAMM          TO CURR-TIAAMM                         
143800         MOVE 'A'                  TO CURR-KDVALTYP                       
143900         CALL W510CURR USING CURR-W510CURR 9305-PCB                       
144000         IF CURR-KDSVAR = ' '                                             
144100           MOVE CURR-PRKURS-NEW    TO W-PRKURS                            
144200           MOVE CURR-REVALUTA-TO   TO W-REVALUTA                          
144300         END-IF                                                           
144400         IF W-PRLFKST = +0                                                
144500           IF MID-PRLFKST-U NOT = ALL '+'                                 
144600             MOVE MID-PRLFKST-U TO DEC-IDFRIDATA                          
144700             MOVE +3           TO DEC-KVHELTAL                            
144800             MOVE +2           TO DEC-KVDECIMAL                           
144900             CALL WDECEDIT USING DEC-WDECAREA                             
145000             IF DEC-KDSVAR-OK                                             
145100               MOVE DEC-IDEDITDATA TO W-PRLFKST                           
145200             END-IF                                                       
145300           END-IF                                                         
145400         END-IF                                                           
145500         IF MID-IDLEVNR-U NOT = ALL '+'                                   
145600           MOVE MID-IDLEVNR-U TO W-IDLEVNR                                
145700           PERFORM IMS-GU-WLLEVA01                                        
145800           IF SEGMENT-FINNS                                               
145900             MOVE 'SE' TO W-IDLAND                                        
146000             PERFORM IMS-GNP-WLLEVA11                                     
146100             IF SEGMENT-FINNS                                             
146200               IF LEV-TULL-TITULF < DAGENS-AAMMDD                         
146300                 MOVE LEV-TULL-RETULF-1  TO W-RETULF                      
146400                                         MOD-RETULF-LEV                   
146500               ELSE                                                       
146600                 MOVE LEV-TULL-RETULF-2  TO W-RETULF                      
146700                                         MOD-RETULF-LEV                   
146800               END-IF                                                     
146900             END-IF                                                       
147000           END-IF                                                         
147100         END-IF                                                           
147200                                                                          
147300         IF W-REVALUTA = 0                                                
147400           MOVE +1         TO W-REVALUTA                                  
147500         END-IF                                                           
147600         COMPUTE W-PRARTBES ROUNDED = W-PRARTBEL * W-RETULF *             
147700                                      W-PRKURS / W-REVALUTA               
147800         IF W-PRARTBES = +0                                               
147900           MOVE +0.01      TO W-PRARTBES                                  
148000         END-IF                                                           
148100* **    HÄR KONVERTERAS DIREKTLEVERANSANDEL FRÅN           **             
148200* **    FRITT FORMAT TILL FAST FORMAT                      **             
148300                                                                          
148400         MOVE MID-REDIRLEV TO DEC-IDFRIDATA                               
148500         MOVE +1           TO DEC-KVHELTAL                                
148600         MOVE +2           TO DEC-KVDECIMAL                               
148700         CALL WDECEDIT USING DEC-WDECAREA                                 
148800                                                                          
148900         IF DEC-KDSVAR-OK                                                 
149000           MOVE DEC-IDEDITDATA TO W-REDIRLEV                              
149100         END-IF                                                           
149200       END-IF                                                             
149300       .                                                                  
149400     EJECT                                                                
149500 CEB-UPPDATERA-WLARTC21 SECTION.                                          
149600                                                                          
149700* ** BEST.PRISER -- WLARTC21 -- WDK621                          **        
149800                                                                          
149900     MOVE 1 TO INDX                                                       
150000     MOVE NEJ TO FLSLUTA-LAS                                              
150100     PERFORM UNTIL INDX > 5 OR FLSLUTA-LAS = JA                           
150200       PERFORM IMS-GHNP-WLARTC21-NEXT                                     
150300       IF SEGMENT-FINNS                                                   
150400         SUBTRACT PRL-DAPRLIST-9KOMPL FROM W-DAPRLIST-MAX                 
150500                                    GIVING W-DAPRLIST                     
150600         MOVE W-LISTDATUM           TO W-TIPRLIST                         
150700         IF PRL-FLHUVLEV = JA OR                                          
150800            (MID-TIPRLIST-U = W-TIPRLIST AND                              
150900             PRL-IDLEVNR    = W-IDLEVNR)                                  
151000           IF MID-TIPRLIST-U = W-TIPRLIST                                 
151100* **     UPPDATERING AV BEST.PRIS                          **             
151200             IF PRL-IDLEVNR = W-IDLEVNR                                   
151300               MOVE PRL-IDLEVNR     TO PRI-O-IDLEVNR-PR                   
151400               MOVE PRL-PRARTBES-PR TO PRI-O-PRARTBES-PR                  
151500               MOVE PRL-PRARTBEL-PR TO PRI-O-PRARTBEL-PR                  
151600               MOVE PRL-KDSTATUS-PR TO PRI-O-KDSTATUS-PR                  
151700               MOVE PRL-SUINLEV-PR  TO PRI-O-SUINLEV-PR                   
151800               MOVE PRL-KDPRURSP    TO PRI-O-KDPRURSP                     
151900               MOVE PRL-KDVALISO    TO PRI-O-KDVALISO                     
152000               MOVE 'R'             TO PRI-O-KDCMD                        
152100               MOVE 'R'             TO PRI-N-KDCMD                        
152200                                                                          
152300               MOVE W-TIPRLIST      TO PRI-O-TIPRLIST                     
152400               MOVE JA              TO PRL-FLHUVLEV                       
152500               MOVE MID-KDPRURSP-U  TO PRL-KDPRURSP                       
152600                                       PRI-N-KDPRURSP                     
152700                                       MOD-KDPRURSP-PR(INDX)              
152800               MOVE MID-TIPRLIST-U  TO PRI-N-TIPRLIST                     
152900                                       MOD-TIPRLIST-PR(INDX)              
153000               MOVE W-IDLEVNR       TO PRL-IDLEVNR                        
153100                                       MOD-IDLEVNR-PR(INDX)               
153200               MOVE W-PRARTBES      TO PRL-PRARTBES-PR                    
153300                                       PRI-N-PRARTBES-PR                  
153400                                       MOD-PRARTBES-PR(INDX)              
153500               MOVE W-PRARTBEL      TO PRL-PRARTBEL-PR                    
153600                                       MOD-PRARTBEL-PR(INDX)              
153700               MOVE 1               TO PRL-KDSTATUS-PR                    
153800                                       PRI-N-KDSTATUS-PR                  
153900               MOVE W-SUINLEV       TO PRL-SUINLEV-PR                     
154000                                       PRI-N-SUINLEV-PR                   
154100               IF W-SUINLEV > 0                                           
154200                 MOVE 'INLEV'       TO MOD-KDSTATUS-PR(INDX)              
154300               ELSE                                                       
154400                 MOVE 'GODK'        TO MOD-KDSTATUS-PR(INDX)              
154500               END-IF                                                     
154600               IF MID-KDVALISO-U NOT = ALL '+'                            
154700                 MOVE MID-KDVALISO-U TO PRL-KDVALISO                      
154800                                       PRI-N-KDVALISO                     
154900                                       MOD-KDVALISO-PR(INDX)              
155000               ELSE                                                       
155100                 MOVE MID-KDVALISO  TO PRL-KDVALISO                       
155200                                       PRI-N-KDVALISO                     
155300                                       MOD-KDVALISO-PR(INDX)              
155400               END-IF                                                     
155500               MOVE WS-KDFPKPRI     TO PRL-KDFPKPRI                       
155600                                       MOD-KDFPKPRI-PR(INDX)              
155700               MOVE MFS-ADD-LYS-UPP-FAELT                                 
155800                                    TO MOD-BEST-PRIS-ATTR (INDX)          
155900               MOVE DAGENS-AAMMDD   TO PRL-TIREGDAT                       
156000               IF MID-IDUSER = SPACES                                     
156100                 MOVE MSGI-IDUSER   TO PRL-IDUSER                         
156200               ELSE                                                       
156300                 MOVE MID-IDUSER    TO PRL-IDUSER                         
156400               END-IF                                                     
156500               PERFORM IMS-REPL-WLARTC21                                  
156600               MOVE JA              TO FLSLUTA-LAS                        
156700             END-IF                                                       
156800           ELSE                                                           
156900             IF INDX-NY = 0                                               
157000               MOVE MID-TIPRLIST-U  TO TMP1-YYMMDD                        
157100               MOVE W-TIPRLIST      TO TMP2-YYMMDD                        
157200               PERFORM WY2000P1                                           
157300               IF TMP1-YYMMDD > TMP2-YYMMDD                               
157400                 MOVE INDX          TO INDX-NY                            
157500               END-IF                                                     
157600             END-IF                                                       
157700             ADD 1                  TO INDX                               
157800           END-IF                                                         
157900         END-IF                                                           
158000       ELSE                                                               
158100                                                                          
158200* ** SEGMENT-SAKNAS                                                       
158300* ** NYUPPLÄGG AV BEST. PRIS                                              
158400                                                                          
158500         MOVE SPACE               TO PRI-O-KDPRURSP                       
158600         MOVE ZERO                TO PRI-O-TIPRLIST                       
158700                                     PRI-O-PRARTBES-PR                    
158800                                     PRI-O-PRARTBEL-PR                    
158900                                     PRI-O-KDSTATUS-PR                    
159000                                     PRI-O-SUINLEV-PR                     
159100         MOVE SPACE               TO PRI-O-KDVALISO                       
159200                                     PRI-O-IDLEVNR-PR                     
159300         MOVE 'I'                 TO PRI-O-KDCMD                          
159400         MOVE MID-KDPRURSP-U      TO PRL-KDPRURSP                         
159500                                     PRI-N-KDPRURSP                       
159600         MOVE MID-TIPRLIST-U      TO WS-DATUM(3:6)                        
159700         IF WS-DATUM(3:2) < 50                                            
159800           MOVE 20                TO WS-DATUM(1:2)                        
159900         ELSE                                                             
160000           MOVE 19                TO WS-DATUM(1:2)                        
160100         END-IF                                                           
160200         SUBTRACT WS-DATUM FROM W-DAPRLIST-MAX                            
160300         GIVING PRL-DAPRLIST-9KOMPL                                       
160400         MOVE MID-TIPRLIST-U      TO PRI-N-TIPRLIST                       
160500         MOVE JA                  TO PRL-FLHUVLEV                         
160600         MOVE W-IDLEVNR           TO PRL-IDLEVNR                          
160700         MOVE W-PRARTBES          TO PRL-PRARTBES-PR                      
160800                                     PRI-N-PRARTBES-PR                    
160900         MOVE W-PRARTBEL          TO PRL-PRARTBEL-PR                      
161000         MOVE 1                   TO PRL-KDSTATUS-PR                      
161100                                     PRI-N-KDSTATUS-PR                    
161200         MOVE W-SUINLEV           TO PRL-SUINLEV-PR                       
161300                                     PRI-N-SUINLEV-PR                     
161400         IF MID-KDVALISO-U NOT = ALL '+'                                  
161500           MOVE MID-KDVALISO-U    TO PRL-KDVALISO                         
161600                                     PRI-N-KDVALISO                       
161700                                     WS-KDVALISO                          
161800         ELSE                                                             
161900           MOVE MID-KDVALISO      TO PRL-KDVALISO                         
162000                                     PRI-N-KDVALISO                       
162100                                     WS-KDVALISO                          
162200         END-IF                                                           
162300         MOVE WS-KDFPKPRI         TO PRL-KDFPKPRI                         
162400         MOVE 'I'                 TO PRI-N-KDCMD                          
162500         IF INDX-NY = 0                                                   
162600           MOVE MFS-ADD-LYS-UPP-FAELT TO                                  
162700                 MOD-BEST-PRIS-ATTR(INDX)                                 
162800         ELSE                                                             
162900           MOVE MFS-ADD-LYS-UPP-FAELT TO                                  
163000                 MOD-BEST-PRIS-ATTR(INDX-NY)                              
163100         END-IF                                                           
163200         MOVE DAGENS-AAMMDD       TO PRL-TIREGDAT                         
163300         IF MID-IDUSER = SPACES                                           
163400           MOVE MSGI-IDUSER       TO PRL-IDUSER                           
163500         ELSE                                                             
163600           MOVE MID-IDUSER        TO PRL-IDUSER                           
163700         END-IF                                                           
163800         PERFORM IMS-ISRT-WLARTC21                                        
163900         MOVE JA                  TO FLSLUTA-LAS                          
164000*****CALL W510PRMT WHICH CHECKS WDK7 IF THE DC'S                          
164100*****IF THE DC'S ARE NON-VCC HAS THE PART                                 
164200*****IF THE PRICE AT NON-VCC IS NOT A FIXED PRICE                         
164300         MOVE W-IDARTNR              TO PRMT-IDARTNR                      
164400         MOVE 010                    TO PRMT-KDCALL                       
164500         CALL W510PRMT USING PRMT-W510PRMT PRMT-WDK7-PCB                  
164600                             PRMT-WDB6-PCB PRDC-WDB6-PCB                  
164700         MOVE PRMT-KDSVAR TO WS-PRMT-KDSVAR                               
164800         IF WS-PRMT-KDSVAR = '1'                                          
164900           MOVE +1 TO WS-IY                                               
165000           PERFORM WS-IY-MAX TIMES                                        
165100             IF PRMT-IDDC(WS-IY) > SPACE                                  
165200               MOVE PRMT-IDDC(WS-IY) TO WS-PRMT-IDDC(WS-IY)               
165300                                        WS-IDDC                           
165400               IF (MSG-KOM-IDCPYTXT  = 'W5I11101')                        
165500               AND ((MID-IDLEVNR-U = 'CHN07' AND NDC-CN)                  
165600               OR   (MID-IDLEVNR-U = '63517' AND NDC-US))                 
165700                 CONTINUE                                                 
165800               ELSE                                                       
165900                 PERFORM CEBA-CREATE-TRANS-5206                           
166000                 PERFORM CEBB-SEND-TRANS                                  
166100               END-IF                                                     
166200               ADD +1               TO WS-IY                              
166300             END-IF                                                       
166400           END-PERFORM                                                    
166500         ELSE                                                             
166600           CONTINUE                                                       
166700         END-IF                                                           
166800         IF MFS-UPDATE                                                    
166900           PERFORM S-LAS-VISA-PRISRADER                                   
167000         END-IF                                                           
167100       END-IF                                                             
167200     END-PERFORM                                                          
167300                                                                          
167400* ** FEM SEGMENT FINNS. DET ÄLDSTA PLOCKAS BORT FÖRE            **        
167500* ** NYUPPLÄGG.                                                 **        
167600                                                                          
167700     IF INDX-NY = 0 AND INDX > 5                                          
167800       MOVE 5 TO INDX-NY                                                  
167900     END-IF                                                               
168000                                                                          
168100     IF INDX > 5 AND FLSLUTA-LAS = NEJ                                    
168200       MOVE PRL-KDPRURSP      TO PRI-O-KDPRURSP                           
168300       SUBTRACT PRL-DAPRLIST-9KOMPL FROM W-DAPRLIST-MAX                   
168400       GIVING W-DAPRLIST                                                  
168500       MOVE W-LISTDATUM       TO PRI-O-TIPRLIST                           
168600       MOVE PRL-IDLEVNR       TO PRI-O-IDLEVNR-PR                         
168700       MOVE PRL-PRARTBES-PR   TO PRI-O-PRARTBES-PR                        
168800       MOVE PRL-PRARTBEL-PR   TO PRI-O-PRARTBEL-PR                        
168900       MOVE PRL-KDSTATUS-PR   TO PRI-O-KDSTATUS-PR                        
169000       MOVE PRL-SUINLEV-PR    TO PRI-O-SUINLEV-PR                         
169100       MOVE PRL-KDVALISO      TO PRI-O-KDVALISO                           
169200       MOVE 'D'               TO PRI-O-KDCMD                              
169300       PERFORM IMS-DLET-WLARTC21                                          
169400       MOVE MID-KDPRURSP-U    TO PRL-KDPRURSP                             
169500                                 PRI-N-KDPRURSP                           
169600       MOVE MID-TIPRLIST-U    TO WS-DATUM(3:6)                            
169700       IF WS-DATUM(3:2) < 50                                              
169800         MOVE 20              TO WS-DATUM(1:2)                            
169900       ELSE                                                               
170000         MOVE 19              TO WS-DATUM(1:2)                            
170100       END-IF                                                             
170200       SUBTRACT WS-DATUM FROM W-DAPRLIST-MAX                              
170300       GIVING PRL-DAPRLIST-9KOMPL                                         
170400       MOVE MID-TIPRLIST-U    TO PRI-N-TIPRLIST                           
170500       MOVE JA                TO PRL-FLHUVLEV                             
170600       MOVE W-IDLEVNR         TO PRL-IDLEVNR                              
170700       MOVE W-PRARTBES        TO PRL-PRARTBES-PR                          
170800                                 PRI-N-PRARTBES-PR                        
170900       MOVE W-PRARTBEL        TO PRL-PRARTBEL-PR                          
171000       MOVE 1                 TO PRL-KDSTATUS-PR                          
171100                                 PRI-N-KDSTATUS-PR                        
171200       MOVE W-SUINLEV         TO PRL-SUINLEV-PR                           
171300                                 PRI-N-SUINLEV-PR                         
171400       IF MID-KDVALISO-U NOT = ALL '+'                                    
171500         MOVE MID-KDVALISO-U  TO PRL-KDVALISO                             
171600                                 PRI-N-KDVALISO                           
171700                                 WS-KDVALISO                              
171800       ELSE                                                               
171900         MOVE MID-KDVALISO    TO PRL-KDVALISO                             
172000                                 PRI-N-KDVALISO                           
172100                                 WS-KDVALISO                              
172200       END-IF                                                             
172300       MOVE WS-KDFPKPRI       TO PRL-KDFPKPRI                             
172400       MOVE 'I'               TO PRI-N-KDCMD                              
172500       MOVE MFS-ADD-LYS-UPP-FAELT TO                                      
172600             MOD-BEST-PRIS-ATTR (INDX-NY)                                 
172700       MOVE DAGENS-AAMMDD     TO PRL-TIREGDAT                             
172800       IF MID-IDUSER = SPACES                                             
172900         MOVE MSGI-IDUSER     TO PRL-IDUSER                               
173000       ELSE                                                               
173100         MOVE MID-IDUSER      TO PRL-IDUSER                               
173200       END-IF                                                             
173300       PERFORM IMS-ISRT-WLARTC21                                          
173400*****CALL W510PRMT WHICH CHECKS WDK7 IF THE DC'S                          
173500*****IF THE NON-VCC DC'S HAVE THE PART AND                                
173600*****IF THE PRICE AT NON VCC DC'S IS NOT A FIXED PRICE                    
173700       MOVE W-IDARTNR              TO PRMT-IDARTNR                        
173800       MOVE 010                    TO PRMT-KDCALL                         
173900       CALL W510PRMT USING PRMT-W510PRMT PRMT-WDK7-PCB                    
174000                           PRMT-WDB6-PCB PRDC-WDB6-PCB                    
174100       MOVE PRMT-KDSVAR TO WS-PRMT-KDSVAR                                 
174200       IF WS-PRMT-KDSVAR = '1'                                            
174300         MOVE +1 TO WS-IY                                                 
174400         PERFORM WS-IY-MAX TIMES                                          
174500           IF PRMT-IDDC(WS-IY) > SPACE                                    
174600             MOVE PRMT-IDDC(WS-IY) TO WS-PRMT-IDDC(WS-IY)                 
174700                                      WS-IDDC                             
174800             IF (MSG-KOM-IDCPYTXT  = 'W5I11101')                          
174900             AND ((MID-IDLEVNR-U = 'CHN07' AND NDC-CN)                    
175000             OR   (MID-IDLEVNR-U = '63517' AND NDC-US))                   
175100               CONTINUE                                                   
175200             ELSE                                                         
175300               PERFORM CEBA-CREATE-TRANS-5206                             
175400               PERFORM CEBB-SEND-TRANS                                    
175500             END-IF                                                       
175600             ADD +1               TO WS-IY                                
175700           END-IF                                                         
175800         END-PERFORM                                                      
175900       ELSE                                                               
176000         CONTINUE                                                         
176100       END-IF                                                             
176200       IF MFS-UPDATE                                                      
176300         PERFORM S-LAS-VISA-PRISRADER                                     
176400       END-IF                                                             
176500     END-IF                                                               
176600     .                                                                    
176700     EJECT                                                                
176800 CEBA-CREATE-TRANS-5206 SECTION.                                          
176900     MOVE WS-PRMT-IDDC(WS-IY) TO W-IDDC-B6                                
177000     PERFORM IMS-GU-WDB601                                                
177100     IF SEGMENT-FINNS                                                     
177200       MOVE DCS-KDVALISO TO WS-KDVALISO-5206                              
177300     END-IF                                                               
177400     MOVE   SPACE                TO 5206-MSG-KOM-WMSGKOM                  
177500     MOVE   +54                  TO 5206-MSG-KOM-KVLL                     
177600     MOVE   LOW-VALUE            TO 5206-MSG-KOM-KDZ1                     
177700     MOVE   LOW-VALUE            TO 5206-MSG-KOM-KDZ2                     
177800     MOVE   SPACE                TO 5206-MSG-KOM-KDTRANS                  
177900                                                                          
178000     MOVE   SPACE                TO 5206-MSG-KOM-IDMFSMED                 
178100                                    5206-MSG-KOM-KDSVAR                   
178200                                                                          
178300     MOVE 'W5I20601'             TO 5206-MSG-KOM-IDCPYTXT                 
178400     MOVE 'MPRIS'                TO 5206-MSG-KOM-IDSNDNOD                 
178500     MOVE 'W5011100'             TO 5206-MSG-KOM-IDSNDJOB                 
178600     ACCEPT 5206-MSG-KOM-TIREGDAT  FROM DATE                              
178700     ACCEPT 5206-MSG-KOM-TIKLOCK   FROM TIME                              
178800                                                                          
178900     COMPUTE P-TO-P-LL = LENGTH OF 5206-W5I20601 + 17                     
179000     MOVE LOW-VALUE               TO P-TO-P-Z1                            
179100     MOVE LOW-VALUE               TO P-TO-P-Z2                            
179200     MOVE 'W5T206X'               TO P-TO-P-TRANSKOD                      
179300     MOVE '5206'                  TO P-TO-P-FROM-MID                      
179400     MOVE '1'                     TO P-TO-P-KDMFSFOR                      
179500                                                                          
179600     MOVE PRMT-IDARTNR            TO 5206-IDARTNR-IN                      
179700                                                                          
179800*******************DETTA ÄR INLAGT BARA FÖR ATT FYLLA MIDDEN              
179900     MOVE '+++++'                 TO 5206-IDLEVNR-IN                      
180000     MOVE '+'                     TO 5206-KDPRBEH-IN                      
180100     MOVE '++++++'                TO 5206-REAENDR-IN                      
180200                                                                          
180300     MOVE SPACE                   TO 5206-IDARTNR-UT                      
180400     MOVE SPACE                   TO 5206-IDLEVNR-UT                      
180500                                     5206-REAENDR-UT                      
180600     MOVE SPACE                   TO 5206-KDPRBEH-UT                      
180700************************************************************              
180800     MOVE WS-PRMT-IDDC(WS-IY)     TO 5206-IDDC                            
180900                                                                          
181000     MOVE 'D'                     TO 5206-KDPRURSP-U                      
181100     MOVE WS-IDUSER               TO 5206-IDUSER                          
181200                                                                          
181300     MOVE DAGENS-AAMMDD           TO 5206-TIPRLIST-U                      
181400                                                                          
181500     COMPUTE WS-PRARTBEL = 100000 * W-PRARTBES                            
181600     MOVE WS-PRARTBEL(1:8)        TO 5206-PRARTBEL-U(1:8)                 
181700     MOVE '.'                     TO 5206-PRARTBEL-U(9:1)                 
181800     MOVE WS-PRARTBEL(9:5)        TO 5206-PRARTBEL-U(10:5)                
181900                                                                          
182000     MOVE ZEROS                   TO 5206-RETULF                          
182100                                                                          
182200     MOVE WS-KDVALISO-5206        TO 5206-KDVALISO-U                      
182300                                                                          
182400     MOVE WS-IDLEVNR-1441         TO 5206-IDLEVNR-U                       
182500                                                                          
182600     MOVE SPACE                   TO 5206-KDFPKPRI-U                      
182700                                                                          
182800     MOVE '+'                     TO 5206-FLPRIBES-U                      
182900                                                                          
183000     MOVE 5206-W5I20601           TO P-TO-P-DATA                          
183100     .                                                                    
183200     EJECT                                                                
183300                                                                          
183400 CEBB-SEND-TRANS SECTION.                                                 
183500     CALL W006KOM USING MSG-PCB                                           
183600                        ALT-PCB                                           
183700                        KOMA-PCB                                          
183800                        5206-MSG-KOM-WMSGKOM                              
183900                        P-TO-P-AREA                                       
184000     .                                                                    
184100     EJECT                                                                
184200                                                                          
184300                                                                          
184400 CEC-WDK625  SECTION.                                                     
184500                                                                          
184600* **    NOTERINGAR-STÄLLKOST -- WLARTC25 -- WDK625           **           
184700* **    NYUPPLÄGG BORTTAG ELLER UPPDATERING                  **           
184800                                                                          
184900     IF MID-TEARTNOT-U NOT = ALL '+'                                      
185000       PERFORM IMS-GHNP-WLARTC25                                          
185100       IF MID-TEARTNOT-U  = SPACE                                         
185200         IF SEGMENT-FINNS                                                 
185300           MOVE NOT-TEARTNOT    TO PRI-O-TEARTNOT                         
185400           PERFORM IMS-DLET-WLARTC25                                      
185500           MOVE MID-TEARTNOT-U  TO PRI-N-TEARTNOT                         
185600           MOVE MFS-RENSA-FAELT TO MOD-TEARTNOT                           
185700         ELSE                                                             
185800           MOVE SPACE           TO PRI-O-TEARTNOT                         
185900           MOVE MID-TEARTNOT-U  TO PRI-N-TEARTNOT                         
186000         END-IF                                                           
186100       ELSE                                                               
186200         IF SEGMENT-FINNS                                                 
186300           MOVE NOT-TEARTNOT    TO PRI-O-TEARTNOT                         
186400         ELSE                                                             
186500           MOVE SPACE           TO PRI-O-TEARTNOT                         
186600         END-IF                                                           
186700         MOVE +8                TO NOT-KDNOTTYP                           
186800         MOVE MID-TEARTNOT-U    TO NOT-TEARTNOT                           
186900                                   PRI-N-TEARTNOT                         
187000                                   MOD-TEARTNOT                           
187100         IF SEGMENT-FINNS                                                 
187200           PERFORM IMS-REPL-WLARTC25                                      
187300         ELSE                                                             
187400           PERFORM IMS-ISRT-WLARTC25                                      
187500         END-IF                                                           
187600       END-IF                                                             
187700       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TEARTNOT-ATTR                    
187800       IF (MID-TEARTNOT-U NOT = ALL '+') AND                              
187900          (MID-KDPRURSP-U     = ALL '+') AND                              
188000          (MID-TIPRLIST-U     = ALL '+') AND                              
188100          (MID-PRARTBEL-U     = ALL '+') AND                              
188200*         (MID-RETULF-U       = ALL '+') AND                              
188300          (MID-KDVALISO-U     = ALL '+') AND                              
188400          (MID-PRLFKST-U      = ALL '+') AND                              
188500          (MID-FLPRIBES-U     = '+')                                      
188600                                                                          
188700* **          ENDAST NOTERINGSFÄLT FÖR STÄLLKOSTNAD ÄR IFYLLT * **        
188800                                                                          
188900         MOVE SPACE             TO PRI-O-KDPRURSP                         
189000         MOVE ZERO              TO PRI-O-TIPRLIST                         
189100                                   PRI-O-PRARTBES-PR                      
189200                                   PRI-O-PRARTBEL-PR                      
189300                                   PRI-O-KDSTATUS-PR                      
189400                                   PRI-O-SUINLEV-PR                       
189500         MOVE SPACE             TO PRI-O-KDVALISO                         
189600                                   PRI-O-IDLEVNR-PR                       
189700         MOVE ZERO              TO PRI-O-PRARTBES                         
189800                                   PRI-O-PRARTSJK                         
189900                                   PRI-O-RETULF                           
190000                                   PRI-O-PRLFKST                          
190100                                   PRI-REDIRLEV                           
190200         MOVE SPACE             TO PRI-O-KDCMD                            
190300         MOVE SPACE             TO PRI-N-KDPRURSP                         
190400         MOVE ZERO              TO PRI-N-TIPRLIST                         
190500                                   PRI-N-PRARTBES-PR                      
190600                                   PRI-N-KDSTATUS-PR                      
190700                                   PRI-N-SUINLEV-PR                       
190800         MOVE SPACE             TO PRI-N-KDVALISO                         
190900         MOVE ZERO              TO PRI-N-PRARTBES                         
191000                                   PRI-N-PRARTSJK                         
191100                                   PRI-N-RETULF                           
191200                                   PRI-N-PRLFKST                          
191300         MOVE SPACE             TO PRI-N-KDCMD                            
191400       END-IF                                                             
191500     ELSE                                                                 
191600       MOVE SPACE               TO PRI-O-TEARTNOT                         
191700                                   PRI-N-TEARTNOT                         
191800     END-IF                                                               
191900      .                                                                   
192000     EJECT                                                                
192100 CED-SALDO  SECTION.                                                      
192200                                                                          
192300     COMPUTE W-CDCSALDO = CLAG-KVAKS-PAV + CLAG-KVAKS-CDC +               
192400                           CLAG-KVAKS-T   + CLAG-KVRESS    +              
192500                           CLAG-KVEFRS    + CLAG-KVLS                     
192600     MOVE W-PRARTBES           TO PRI-O-PRARTBES                          
192700     MOVE CLAG-PRARTSJK        TO PRI-O-PRARTSJK                          
192800     MOVE CLAG-PRINK           TO PRI-O-PRINK                             
192900     MOVE CLAG-PRARTSTD        TO PRI-O-PRARTSTD                          
193000                                                                          
193100     IF CLAG-RETULF = +0                                                  
193200       MOVE MID-RETULF-LEV     TO PRI-O-RETULF                            
193300     ELSE                                                                 
193400       MOVE CLAG-RETULF        TO PRI-O-RETULF                            
193500     END-IF                                                               
193600                                                                          
193700     MOVE CLAG-PRLFKST         TO PRI-O-PRLFKST                           
193800     MOVE W-REDIRLEV           TO PRI-REDIRLEV                            
193900     IF W-CDCSALDO = ZERO                                                 
194000       EVALUATE CLAG-KDTIPPR                                              
194100         WHEN 1                                                           
194200           MOVE 'Y'            TO MOD-KDTIPPR                             
194300         WHEN 0                                                           
194400           MOVE 'N'            TO MOD-KDTIPPR                             
194500         WHEN 3                                                           
194600           MOVE 'A'            TO MOD-KDTIPPR                             
194700       END-EVALUATE                                                       
194800     ELSE                                                                 
194900       MOVE 1                  TO CLAG-KDTIPPR                            
195000       MOVE 'Y'                TO MOD-KDTIPPR                             
195100     END-IF                                                               
195200     MOVE MFS-FORMATETS-ATTR   TO MOD-KDTIPPR-ATTR                        
195300     .                                                                    
195400     EJECT                                                                
195500 CEE-NY-PRISRAD SECTION.                                                  
195600                                                                          
195700     PERFORM R-UPPD-BES-SJK                                               
195800     COMPUTE W-PRINK ROUNDED = (W-PRARTBEL *                              
195900                             W-RETULF * W-PRKURS )                        
196000                             / W-REVALUTA                                 
196100     IF W-PRINK = +0                                                      
196200       MOVE +0.01        TO W-PRINK                                       
196300     END-IF                                                               
196400     COMPUTE W-PRARTSTD ROUNDED = W-PRINK +                               
196500                 CLAG-PRDIRLON + CLAG-PRDMTRL + CLAG-PROVRPAL             
196600     MOVE W-PRINK        TO PRI-N-PRINK                                   
196700     MOVE W-PRARTSTD     TO PRI-N-PRARTSTD                                
196800     MOVE IDARTNR-WS     TO PRI-IDARTNR                                   
196900                                                                          
197000     IF W-CDCSALDO = ZERO AND W-SDCSALDO = ZERO                           
197100       IF INLEV-FINNS = JA                                                
197200         MOVE 'V'        TO PRI-KDPRIBEH                                  
197300         MOVE 'N'        TO PRI-FLKLAR                                    
197400         MOVE 'A'        TO SW-MED                                        
197500       ELSE                                                               
197600         MOVE CLAG-PRINK TO W-PRINK-O                                     
197700         MOVE W-PRINK    TO CLAG-PRINK                                    
197800         IF ART-KDSORT = 'SW'                                             
197900           MOVE 1.0000          TO PRI-N-RETULF                           
198000         ELSE                                                             
198100           IF W-RETULF = 0                                                
198200             MOVE 1.0812        TO PRI-N-RETULF                           
198300           ELSE                                                           
198400             MOVE W-RETULF      TO PRI-N-RETULF                           
198500           END-IF                                                         
198600         END-IF                                                           
198700         IF ART-KDSORT = 'SW'                                             
198800           MOVE ZERO            TO CLAG-PRHEMTAG                          
198900         ELSE                                                             
199000           IF CLAG-PRHEMTAG = 0                                           
199100             COMPUTE CLAG-PRHEMTAG ROUNDED =                              
199200               CLAG-PRINK * (PRI-N-RETULF - 1) / PRI-N-RETULF             
199300           ELSE                                                           
199400             IF W-PRINK-O NOT = 0                                         
199500               COMPUTE CLAG-PRHEMTAG ROUNDED =                            
199600                 CLAG-PRHEMTAG * CLAG-PRINK / W-PRINK-O                   
199700             END-IF                                                       
199800           END-IF                                                         
199900         END-IF                                                           
200000         MOVE W-PRARTSTD TO CLAG-PRARTSTD                                 
200100         MOVE 'J'        TO PRI-KDPRIBEH                                  
200200         MOVE 'J'        TO PRI-FLKLAR                                    
200300         MOVE 'B'        TO SW-MED                                        
200400       END-IF                                                             
200500     ELSE                                                                 
200600       IF MID-KDPRURSP-U = 'S'                                            
200700         MOVE 'V'        TO PRI-KDPRIBEH                                  
200800         MOVE 'N'        TO PRI-FLKLAR                                    
200900       ELSE                                                               
201000         MOVE 'J'        TO PRI-KDPRIBEH                                  
201100         MOVE 'J'        TO PRI-FLKLAR                                    
201200         MOVE ZERO       TO PRI-O-PRARTSTD                                
201300                            PRI-N-PRARTSTD                                
201400                            PRI-O-PRINK                                   
201500                            PRI-N-PRINK                                   
201600       END-IF                                                             
201700       MOVE 'A'          TO SW-MED                                        
201800     END-IF                                                               
201900     .                                                                    
202000     EJECT                                                                
202100 CEF-INLEV-POST  SECTION.                                                 
202200                                                                          
202300     MOVE IDARTNR-WS         TO PRI-IDARTNR                               
202400     IF MID-KDPRURSP-U = 'S' OR                                           
202500       (W-CDCSALDO = ZERO AND W-SDCSALDO = ZERO AND                       
202600          W-SUINLEV > 0)                                                  
202700       PERFORM R-UPPD-BES-SJK                                             
202800       MOVE W-PRARTBES       TO PRI-N-PRINK                               
202900       MOVE W-PRARTSJK       TO PRI-N-PRARTSTD                            
203000       MOVE 'V'              TO PRI-KDPRIBEH                              
203100       MOVE 'N'              TO PRI-FLKLAR                                
203200       MOVE 'A'              TO SW-MED                                    
203300     ELSE                                                                 
203400       IF W-SUINLEV > 0                                                   
203500         PERFORM R-UPPD-BES-SJK                                           
203600         MOVE 'J'              TO PRI-KDPRIBEH                            
203700         MOVE 'J'              TO PRI-FLKLAR                              
203800         MOVE 'C'              TO SW-MED                                  
203900       ELSE                                                               
203910**** TEMP SOLUTION FOR LYNK                                               
203920         MOVE ART-KDPRODSL TO TEST-KDPRODSL                               
203930         IF KDPRODSL-LYNK                                                 
203940           PERFORM R-UPPD-BES-SJK                                         
203950         END-IF                                                           
203960**** ALSO REMOVE WWPRODSL COPYTEXT                                        
204000         MOVE 'J'              TO PRI-KDPRIBEH                            
204100         MOVE 'J'              TO PRI-FLKLAR                              
204200         MOVE ' '              TO SW-MED                                  
204300       END-IF                                                             
204400       IF ART-KDSORT = 'SW'                                               
204500         MOVE ZERO             TO CLAG-PRHEMTAG                           
204600       END-IF                                                             
204700     END-IF                                                               
204800     .                                                                    
204900     EJECT                                                                
205000 CEG-INLEV-SAKNAS-POST  SECTION.                                          
205100                                                                          
205200     PERFORM R-UPPD-BES-SJK                                               
205300     COMPUTE W-PRINK ROUNDED = (W-PRARTBEL *                              
205400                         W-RETULF * W-PRKURS )                            
205500                         / W-REVALUTA                                     
205600     IF W-PRINK = +0                                                      
205700       MOVE +0.01        TO W-PRINK                                       
205800     END-IF                                                               
205900     COMPUTE W-PRARTSTD ROUNDED = W-PRINK +                               
206000                 CLAG-PRDIRLON + CLAG-PRDMTRL + CLAG-PROVRPAL             
206100     MOVE W-PRINK        TO PRI-N-PRINK                                   
206200     MOVE W-PRARTSTD     TO PRI-N-PRARTSTD                                
206300     MOVE IDARTNR-WS     TO PRI-IDARTNR                                   
206400     IF W-CDCSALDO = ZERO AND W-SDCSALDO = ZERO                           
206500       MOVE CLAG-PRINK TO W-PRINK-O                                       
206600       MOVE W-PRINK    TO CLAG-PRINK                                      
206700       IF ART-KDSORT = 'SW'                                               
206800         MOVE 1.0000          TO PRI-N-RETULF                             
206900       ELSE                                                               
207000         IF W-RETULF = 0                                                  
207100           MOVE 1.0812          TO PRI-N-RETULF                           
207200         ELSE                                                             
207300           MOVE W-RETULF        TO PRI-N-RETULF                           
207400         END-IF                                                           
207500       END-IF                                                             
207600       IF ART-KDSORT = 'SW'                                               
207700         MOVE ZERO            TO CLAG-PRHEMTAG                            
207800       ELSE                                                               
207900         IF CLAG-PRHEMTAG = 0                                             
208000           COMPUTE CLAG-PRHEMTAG ROUNDED =                                
208100             CLAG-PRINK * (PRI-N-RETULF - 1) / PRI-N-RETULF               
208200         ELSE                                                             
208300           IF W-PRINK-O NOT = 0                                           
208400             COMPUTE CLAG-PRHEMTAG ROUNDED =                              
208500               CLAG-PRHEMTAG * CLAG-PRINK / W-PRINK-O                     
208600           END-IF                                                         
208700         END-IF                                                           
208800       END-IF                                                             
208900       MOVE W-PRARTSTD   TO CLAG-PRARTSTD                                 
209000       MOVE 'J'          TO PRI-KDPRIBEH                                  
209100       MOVE 'J'          TO PRI-FLKLAR                                    
209200       MOVE 'B'          TO SW-MED                                        
209300     ELSE                                                                 
209400       MOVE 'V'          TO PRI-KDPRIBEH                                  
209500       MOVE 'N'          TO PRI-FLKLAR                                    
209600       MOVE 'A'          TO SW-MED                                        
209700     END-IF                                                               
209800     .                                                                    
209900     EJECT                                                                
210000 CEH-KONTROLLERA-OM-INLEV-SKETT SECTION.                                  
210100                                                                          
210200     MOVE ZERO                TO W-SUINLEV                                
210300     MOVE ZERO                TO R25-DATUM-NOLLOR                         
210400                                                                          
210500     PERFORM IMS-GU-WLINLE01                                              
210600     IF SEGMENT-FINNS                                                     
210700       MOVE JA                TO INLEV-FINNS                              
210800       MOVE MID-TIPRLIST-U    TO PRI-N-TIPRLIST                           
210900       MOVE MID-TIPRLIST-U    TO R25-DATUM                                
211000       IF MID-TIPRLIST-U NOT = ZERO                                       
211100         IF MID-TIPRLIST-U < 500000                                       
211200           MOVE 20            TO R25-DATUM-SEKEL                          
211300         ELSE                                                             
211400           MOVE 19            TO R25-DATUM-SEKEL                          
211500         END-IF                                                           
211600       END-IF                                                             
211700       COMPUTE DAINLEV-NYCKEL = DAINLEV-MAX - R25-DATUM-X                 
211800       MOVE DAINLEV-NYCKEL    TO W-DAINLEV                                
211900       PERFORM IMS-GNP-WLINLE11                                           
212000       PERFORM UNTIL  SEGMENT-SAKNAS OR W-SUINLEV > 0                     
212100         MOVE INLE-INL-DAINLEV  TO W-DAINLEV                              
212200         PERFORM IMS-GNP-WLINLE21                                         
212300         IF SEGMENT-FINNS                                                 
212400           IF INLE-MOT-IDLEVNR   = W-IDLEVNR AND                          
212500              (INLE-MOT-KDRT     = 00 OR 01 OR 02 OR                      
212600                                   03 OR 04 OR 05)                        
212700             IF INLE-MOT-IDPTYP = 'R31' OR 'R32'                          
212800               MOVE INLE-MOT-TIAVIDAT   TO TMP1-YYMMDD                    
212900               MOVE MID-TIPRLIST-U      TO TMP2-YYMMDD                    
213000               PERFORM WY2000P1                                           
213100               IF TMP1-YYMMDD >= TMP2-YYMMDD                              
213200                 ADD 1          TO   W-SUINLEV                            
213300               END-IF                                                     
213400             END-IF                                                       
213500           END-IF                                                         
213600         ELSE                                                             
213700           PERFORM IMS-GNP-WLINLE22                                       
213800           IF SEGMENT-FINNS                                               
213900             IF INLE-DIR-IDLEVNR    = W-IDLEVNR AND                       
214000                (INLE-DIR-KDRT      = 00 OR 01 OR 02 OR                   
214100                                      03 OR 04 OR 05)                     
214200               IF INLE-DIR-IDPTYP = 'R33' OR 'R34'                        
214300                 MOVE INLE-DIR-TIAVSDAT   TO TMP1-YYMMDD                  
214400                 MOVE MID-TIPRLIST-U      TO TMP2-YYMMDD                  
214500                 PERFORM WY2000P1                                         
214600                 IF TMP1-YYMMDD >= TMP2-YYMMDD                            
214700                   ADD 1        TO   W-SUINLEV                            
214800                 END-IF                                                   
214900               END-IF                                                     
215000             END-IF                                                       
215100           END-IF                                                         
215200         END-IF                                                           
215300                                                                          
215400         MOVE DAINLEV-NYCKEL   TO W-DAINLEV                               
215500         PERFORM IMS-GNP-WLINLE11                                         
215600       END-PERFORM                                                        
215700     END-IF                                                               
215800     .                                                                    
215900     EJECT                                                                
216000 D-SOEKNING  SECTION.                                                     
216100                                                                          
216200     IF MID-IDARTNR-IN = ALL '+' AND                                      
216300        MID-UPPDAT-RAD NOT = ALL '+' AND EGEN-TRANS                       
216400       MOVE W-FEL-5 TO MOD-TEMFSFEL                                       
216500       PERFORM MFS-ROER-EJ-FAELT-UTDATA                                   
216600       PERFORM MFS-ROER-EJ-FAELT-INDATA                                   
216700       PERFORM MFS-ADD-LAES-IN-FAELT-INDATA                               
216800     ELSE                                                                 
216900       PERFORM IMS-GHU-WLARTC01                                           
217000       IF SEGMENT-FINNS                                                   
217100         MOVE ART-TIURPROD          TO MOD-TIURPROD                       
217200         IF ART-KDERS-UTG = 0                                             
217300           MOVE ART-IDLEVNR  TO MOD-IDLEVNR                               
217400                                W-IDLEVNR                                 
217500           PERFORM DA-BEHANDLA-OVRIGA-SEGMENT                             
217600         ELSE                                                             
217700           MOVE W-FEL-2 TO MOD-TEMFSFEL                                   
217800           PERFORM MFS-RENSA-FAELT-UTDATA                                 
217900         END-IF                                                           
218000       ELSE                                                               
218100         MOVE W-FEL-1 TO MOD-TEMFSFEL                                     
218200         PERFORM MFS-RENSA-FAELT-UTDATA                                   
218300       END-IF                                                             
218400       PERFORM MFS-RENSA-FAELT-INDATA                                     
218500     END-IF                                                               
218600     .                                                                    
218700     EJECT                                                                
218800 DA-BEHANDLA-OVRIGA-SEGMENT SECTION.                                      
218900                                                                          
219000     PERFORM DAA-BEHANDLA-ART-CLAGER-INFO                                 
219100     PERFORM S-LAS-VISA-PRISRADER                                         
219200     PERFORM DAB-BEHANDLA-BEST-INFO                                       
219300                                                                          
219400     IF W-KDAVT = 0 OR 1                                                  
219500       PERFORM DAC-BEHANDLA-AVTALS-INFO                                   
219600     ELSE                                                                 
219700       MOVE MFS-RENSA-FAELT TO MOD-IDBESTNR (2)                           
219800                               MOD-IDLEVNR-B (2)                          
219900                               MOD-KVBEST (2)                             
220000                               MOD-TIBEST (2)                             
220100     END-IF                                                               
220200                                                                          
220300     PERFORM IMS-GHNP-WLARTC25                                            
220400                                                                          
220500     IF SEGMENT-FINNS                                                     
220600       MOVE NOT-TEARTNOT   TO MOD-TEARTNOT                                
220700     ELSE                                                                 
220800       MOVE MFS-RENSA-FAELT  TO MOD-TEARTNOT                              
220900     END-IF                                                               
221000                                                                          
221100     PERFORM IMS-GU-WLBENA11                                              
221200                                                                          
221300     IF SEGMENT-FINNS                                                     
221400       MOVE BEN-TEXT-BEART  TO MOD-BEART                                  
221500     ELSE                                                                 
221600       MOVE MFS-RENSA-FAELT TO MOD-BEART                                  
221700     END-IF                                                               
221800                                                                          
221900     PERFORM DAD-HAMTA-RETULF-KDVALISO                                    
222000     .                                                                    
222100     EJECT                                                                
222200 DAA-BEHANDLA-ART-CLAGER-INFO SECTION.                                    
222300                                                                          
222400     PERFORM IMS-GHNP-WLARTC11                                            
222500                                                                          
222600     IF SEGMENT-FINNS                                                     
222700       MOVE CLAG-IDANSK            TO MOD-IDANSK                          
222800       MOVE CLAG-IDINK             TO MOD-IDINK                           
222900*****  MOVE CLAG-TIURPROD          TO MOD-TIURPROD                        
223000       MOVE CLAG-KDAVT             TO W-KDAVT                             
223100       IF   CLAG-KDHF > 0                                                 
223200         MOVE W-FEL-9              TO MOD-TEMFSFEL                        
223300       END-IF                                                             
223400       COMPUTE W-KVPB-TOT = CLAG-KVPB-SATS + CLAG-KVPB-SEP                
223500       MOVE W-KVPB-TOT             TO MOD-KVPB-TOT                        
223600       MOVE CLAG-IDLEVNR-SEN       TO MOD-IDLEVNR-SEN                     
223700       MOVE CLAG-TIAVIDAT-SEN      TO MOD-TIAVIDAT-SEN                    
223800       MOVE CLAG-REDIRLEV          TO MOD-REDIRLEV                        
223900       MOVE CLAG-PRINK             TO MOD-PRINK                           
224000       MOVE CLAG-PRARTSTD          TO MOD-PRARTSTD                        
224100       MOVE CLAG-PRARTSJK          TO MOD-PRARTSJK                        
224200       COMPUTE W-PRARTBES = CLAG-PRARTSJK -                               
224300               CLAG-PRDIRLON - CLAG-PRDMTRL - CLAG-PROVRPAL               
224400       MOVE W-PRARTBES             TO MOD-PRARTBES                        
224500       EVALUATE CLAG-KDTIPPR                                              
224600         WHEN 1                                                           
224700           MOVE 'Y'            TO MOD-KDTIPPR                             
224800         WHEN 0                                                           
224900           MOVE 'N'            TO MOD-KDTIPPR                             
225000         WHEN 3                                                           
225100           MOVE 'A'            TO MOD-KDTIPPR                             
225200       END-EVALUATE                                                       
225300       IF CLAG-PRLFKST = +0                                               
225400         MOVE MFS-RENSA-FAELT      TO MOD-PRLFKST                         
225500       ELSE                                                               
225600         MOVE CLAG-PRLFKST         TO MOD-PRLFKST                         
225700       END-IF                                                             
225800       MOVE CLAG-RETULF            TO MOD-RETULF                          
225900                                      W-RETULF                            
226000       MOVE 'A'                    TO MOD-FLART-RETULF                    
226100       COMPUTE W-TOTSALDO = CLAG-KVAKS-PAV + CLAG-KVAKS-CDC +             
226200                            CLAG-KVAKS-T   + CLAG-KVRESS    +             
226300                            CLAG-KVEFRS    + CLAG-KVLS      +             
226400                            W-SDCSALDO                                    
226500       MOVE W-TOTSALDO             TO MOD-KVLS-TOT                        
226600       MOVE CLAG-KDERS             TO W-KDERS                             
226700                                                                          
226800       IF W-KDERS > 9                                                     
226900         MOVE W-FEL-8 TO MOD-TEMFSFEL                                     
227000       END-IF                                                             
227100     ELSE                                                                 
227200       MOVE MFS-RENSA-FAELT        TO MOD-IDLEVNR-SEN                     
227300                                      MOD-TIAVIDAT-SEN                    
227400     END-IF                                                               
227500     .                                                                    
227600     EJECT                                                                
227700 DAB-BEHANDLA-BEST-INFO SECTION.                                          
227800                                                                          
227900      MOVE +1 TO INDX                                                     
228000      PERFORM UNTIL INDX > 1                                              
228100        PERFORM IMS-GNP-WLARTC22                                          
228200        IF SEGMENT-FINNS                                                  
228300          IF BEST-KDBEH-BEST = 1 OR 5 OR 6                                
228400            IF BEST-IDBEST > 0                                            
228500              MOVE BEST-IDBEST       TO MOD-IDBESTNR (1)                  
228600              MOVE BEST-KVBEST       TO MOD-KVBEST (1)                    
228700              MOVE BEST-TIBEST       TO MOD-TIBEST (1)                    
228800              MOVE BEST-IDLEVNR-BEST TO MOD-IDLEVNR-B (1)                 
228900              ADD 1                  TO INDX                              
229000            END-IF                                                        
229100          ELSE                                                            
229200            MOVE MFS-RENSA-FAELT     TO MOD-IDBESTNR (1)                  
229300                                        MOD-KVBEST (1)                    
229400                                        MOD-TIBEST (1)                    
229500                                        MOD-IDLEVNR-B (1)                 
229600          END-IF                                                          
229700        ELSE                                                              
229800          MOVE MFS-RENSA-FAELT       TO MOD-IDBESTNR (1)                  
229900                                        MOD-KVBEST (1)                    
230000                                        MOD-TIBEST (1)                    
230100                                        MOD-IDLEVNR-B (1)                 
230200          ADD 1 TO INDX                                                   
230300        END-IF                                                            
230400      END-PERFORM                                                         
230500     .                                                                    
230600     EJECT                                                                
230700 DAC-BEHANDLA-AVTALS-INFO SECTION.                                        
230800                                                                          
230900     PERFORM IMS-GNP-WLARTC23                                             
231000                                                                          
231100     IF SEGMENT-FINNS                                                     
231200       MOVE AVT-IDAVTAL       TO MOD-IDBESTNR (2)                         
231300       MOVE AVT-IDLEVNR-AVT   TO MOD-IDLEVNR-B (2)                        
231400       MOVE AVT-KVAVTANT      TO MOD-KVBEST (2)                           
231500       MOVE AVT-TIAVTAL       TO MOD-TIBEST (2)                           
231600     ELSE                                                                 
231700       MOVE MFS-RENSA-FAELT   TO MOD-IDBESTNR (2)                         
231800                                 MOD-IDLEVNR-B (2)                        
231900                                 MOD-KVBEST (2)                           
232000                                 MOD-TIBEST (2)                           
232100     END-IF                                                               
232200     .                                                                    
232300     EJECT                                                                
232400 DAD-HAMTA-RETULF-KDVALISO SECTION.                                       
232500                                                                          
232600     PERFORM IMS-GU-WLLEVA01                                              
232700     IF SEGMENT-FINNS                                                     
232800       MOVE 'SE' TO W-IDLAND                                              
232900       PERFORM IMS-GNP-WLLEVA11                                           
233000       IF SEGMENT-FINNS                                                   
233100         IF LEV-TULL-TITULF < DAGENS-AAMMDD                               
233200           MOVE LEV-TULL-RETULF-1 TO MOD-RETULF-LEV                       
233300         ELSE                                                             
233400           MOVE LEV-TULL-RETULF-2 TO MOD-RETULF-LEV                       
233500         END-IF                                                           
233600         IF W-RETULF = ZERO                                               
233700           MOVE MOD-RETULF-LEV  TO MOD-RETULF                             
233800           MOVE MFS-RENSA-FAELT TO MOD-FLART-RETULF                       
233900         END-IF                                                           
234000                                                                          
234100         MOVE SPACE             TO W-KDVALISO                             
234200         MOVE +1                TO VAL-IX                                 
234300         PERFORM VAL-IX-MAX TIMES                                         
234400           IF LEV-TULL-KDVALLEV = VAL-KDVALUTA(VAL-IX)                    
234500             MOVE VAL-KDVALISO(VAL-IX) TO W-KDVALISO                      
234600           END-IF                                                         
234700           ADD +1               TO VAL-IX                                 
234800         END-PERFORM                                                      
234900         IF W-KDVALISO = ' '                                              
235000           MOVE MFS-RENSA-FAELT TO MOD-KDVALISO                           
235100         ELSE                                                             
235200           MOVE W-KDVALISO      TO MOD-KDVALISO                           
235300         END-IF                                                           
235400       ELSE                                                               
235500         IF MOD-RETULF = ZERO                                             
235600           MOVE MFS-RENSA-FAELT  TO MOD-RETULF                            
235700         END-IF                                                           
235800         MOVE MFS-RENSA-FAELT    TO MOD-KDVALISO                          
235900                                    MOD-RETULF-LEV                        
236000       END-IF                                                             
236100     ELSE                                                                 
236200       MOVE MFS-RENSA-FAELT      TO MOD-RETULF-LEV                        
236300       MOVE W-FEL-12             TO MOD-TEMFSFEL                          
236400     END-IF                                                               
236500     .                                                                    
236600     EJECT                                                                
236700                                                                          
236800 R-UPPD-BES-SJK SECTION.                                                  
236900     MOVE W-PRARTBES TO        PRI-N-PRARTBES                             
237000                               MOD-PRARTBES                               
237100     MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-PRARTBES-ATTR                      
237200                                                                          
237300     COMPUTE W-PRARTSJK ROUNDED = W-PRARTBES +                            
237400                CLAG-PRDIRLON + CLAG-PRDMTRL + CLAG-PROVRPAL              
237500     MOVE W-PRARTSJK        TO CLAG-PRARTSJK                              
237600                               PRI-N-PRARTSJK                             
237700                               MOD-PRARTSJK                               
237800     MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-PRARTSJK-ATTR                      
237900     .                                                                    
238000     EJECT                                                                
238100                                                                          
238200 S-LAS-VISA-PRISRADER SECTION.                                            
238300     MOVE 1 TO INDX                                                       
238400     PERFORM IMS-GHNP-WLARTC21-FIRST                                      
238500     PERFORM UNTIL INDX > 5                                               
238600                                                                          
238700       IF SEGMENT-FINNS                                                   
238800         IF PRL-FLHUVLEV = JA                                             
238900           SUBTRACT PRL-DAPRLIST-9KOMPL FROM W-DAPRLIST-MAX               
239000           GIVING WS-DATUM                                                
239100           MOVE WS-DATUM(3:6)       TO MOD-TIPRLIST-PR (INDX)             
239200           MOVE PRL-IDLEVNR         TO MOD-IDLEVNR-PR (INDX)              
239300           MOVE PRL-PRARTBES-PR     TO MOD-PRARTBES-PR (INDX)             
239400           MOVE PRL-PRARTBEL-PR     TO MOD-PRARTBEL-PR (INDX)             
239500           IF PRL-SUINLEV-PR > 0 AND PRL-KDSTATUS-PR = 1                  
239600             MOVE 'INLEV'           TO MOD-KDSTATUS-PR (INDX)             
239700           ELSE                                                           
239800             IF PRL-KDSTATUS-PR = 1                                       
239900               MOVE 'GODK'          TO  MOD-KDSTATUS-PR (INDX)            
240000             ELSE                                                         
240100               MOVE 'PREL'          TO MOD-KDSTATUS-PR (INDX)             
240200             END-IF                                                       
240300           END-IF                                                         
240400           MOVE PRL-KDPRURSP        TO MOD-KDPRURSP-PR (INDX)             
240500           MOVE PRL-KDVALISO        TO MOD-KDVALISO-PR (INDX)             
240600           MOVE PRL-KDFPKPRI        TO MOD-KDFPKPRI-PR (INDX)             
240700           IF MFS-UPDATE                                                  
240800             IF MID-TIPRLIST-U = W-TIPRLIST                               
240900               MOVE MFS-ADD-LYS-UPP-FAELT TO                              
241000                      MOD-BEST-PRIS-ATTR (INDX)                           
241100             END-IF                                                       
241200           END-IF                                                         
241300           ADD 1 TO INDX                                                  
241400         END-IF                                                           
241500         IF INDX < 6                                                      
241600           PERFORM IMS-GHNP-WLARTC21-NEXT                                 
241700         END-IF                                                           
241800       ELSE                                                               
241900         PERFORM UNTIL INDX > 5                                           
242000           MOVE MFS-RENSA-FAELT TO MOD-BEST-PRIS (INDX)                   
242100           ADD 1 TO INDX                                                  
242200         END-PERFORM                                                      
242300       END-IF                                                             
242400                                                                          
242500     END-PERFORM                                                          
242600     .                                                                    
242700     EJECT                                                                
242800 S01-HAMTA-SLAGERSALDO  SECTION.                                          
242900                                                                          
243000     PERFORM IMS-GU-WLARTS01                                              
243100     IF STATUS-WS = '  '                                                  
243200       PERFORM IMS-GNP-WLARTS11                                           
243300       PERFORM UNTIL STATUS-WS = 'GE'                                     
243400        IF NOT DCS-IDDC = SLAG-IDDC                                       
243500           MOVE SLAG-IDDC       TO W-IDDC-B6                              
243600           PERFORM IMS-GU-WDB601                                          
243700        END-IF                                                            
243800        IF DCS-NDC-NA OR DCS-LAND-NON-VCC-OWNED                           
243900          CONTINUE                                                        
244000        ELSE                                                              
244100          COMPUTE W-SDCSALDO = W-SDCSALDO + SLAG-KVLS +                   
244200              SLAG-KVEFRS + SLAG-KVAKS-PAV + SLAG-KVAKS-SDC               
244300        END-IF                                                            
244400        PERFORM IMS-GNP-WLARTS11                                          
244500       END-PERFORM                                                        
244600     END-IF                                                               
244700     .                                                                    
244800     EJECT                                                                
244900 MFS-FORMATETS-ATTR-INDATA SECTION.                                       
245000     SKIP3                                                                
245100     MOVE MFS-FORMATETS-ATTR TO    MOD-KDPRURSP-U-ATTR                    
245200                                   MOD-TIPRLIST-U-ATTR                    
245300                                   MOD-PRARTBEL-U-ATTR                    
245400*                                  MOD-RETULF-U-ATTR                      
245500                                   MOD-KDVALISO-U-ATTR                    
245600                                   MOD-PRLFKST-U-ATTR                     
245700                                   MOD-FLPRIBES-U-ATTR                    
245800                                   MOD-TEARTNOT-U-ATTR                    
245900                                   MOD-IDLEVNR-U-ATTR                     
246000                                   MOD-KDFPKPRI-U-ATTR                    
246100     .                                                                    
246200     SKIP3                                                                
246300 MFS-ADD-LAES-IN-FAELT-INDATA SECTION.                                    
246400     SKIP3                                                                
246500     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDPRURSP-U-ATTR                    
246600                                   MOD-TIPRLIST-U-ATTR                    
246700                                   MOD-PRARTBEL-U-ATTR                    
246800*                                  MOD-RETULF-U-ATTR                      
246900                                   MOD-KDVALISO-U-ATTR                    
247000                                   MOD-PRLFKST-U-ATTR                     
247100                                   MOD-TEARTNOT-U-ATTR                    
247200                                   MOD-FLPRIBES-U-ATTR                    
247300                                   MOD-IDLEVNR-U-ATTR                     
247400                                   MOD-KDFPKPRI-U-ATTR                    
247500     .                                                                    
247600     EJECT                                                                
247700 MFS-STAENG-FAELT-INDATA SECTION.                                         
247800     SKIP3                                                                
247900     MOVE MFS-STAENG-FAELT TO      MOD-KDPRURSP-U-ATTR                    
248000                                   MOD-TIPRLIST-U-ATTR                    
248100                                   MOD-PRARTBEL-U-ATTR                    
248200*                                  MOD-RETULF-U-ATTR                      
248300                                   MOD-KDVALISO-U-ATTR                    
248400                                   MOD-PRLFKST-U-ATTR                     
248500                                   MOD-TEARTNOT-U-ATTR                    
248600                                   MOD-FLPRIBES-U-ATTR                    
248700                                   MOD-IDLEVNR-U-ATTR                     
248800                                   MOD-KDFPKPRI-U-ATTR                    
248900     .                                                                    
249000     SKIP3                                                                
249100 MFS-OEPPNA-FAELT-INDATA SECTION.                                         
249200     SKIP3                                                                
249300     MOVE MFS-OEPPNA-NUM-FAELT  TO MOD-TIPRLIST-U-ATTR                    
249400     MOVE MFS-OEPPNA-ALFA-FAELT TO MOD-KDPRURSP-U-ATTR                    
249500                                   MOD-PRARTBEL-U-ATTR                    
249600*                                  MOD-RETULF-U-ATTR                      
249700                                   MOD-KDVALISO-U-ATTR                    
249800                                   MOD-PRLFKST-U-ATTR                     
249900                                   MOD-TEARTNOT-U-ATTR                    
250000                                   MOD-FLPRIBES-U-ATTR                    
250100                                   MOD-IDLEVNR-U-ATTR                     
250200                                   MOD-KDFPKPRI-U-ATTR                    
250300     .                                                                    
250400     EJECT                                                                
250500 MFS-ROER-EJ-FAELT-UTDATA SECTION.                                        
250600     SKIP3                                                                
250700     MOVE MFS-ROER-EJ-FAELT TO  MOD-RETULF-LEV                            
250800                                MOD-BEART                                 
250900                                MOD-PRINK                                 
251000                                MOD-PRARTSTD                              
251100                                MOD-PRARTBES                              
251200                                MOD-PRARTSJK                              
251300                                MOD-KDTIPPR                               
251400                                MOD-IDANSK                                
251500                                MOD-IDINK                                 
251600                                MOD-TIURPROD                              
251700                                MOD-IDLEVNR                               
251800                                MOD-KVLS-TOT                              
251900                                MOD-KVPB-TOT                              
252000                                MOD-IDLEVNR-SEN                           
252100                                MOD-TIAVIDAT-SEN                          
252200                                MOD-BEST-PRIS(1)                          
252300                                MOD-BEST-PRIS(2)                          
252400                                MOD-BEST-PRIS(3)                          
252500                                MOD-BEST-PRIS(4)                          
252600                                MOD-BEST-PRIS(5)                          
252700                                MOD-BESTALLNING(1)                        
252800                                MOD-BESTALLNING(2)                        
252900                                MOD-TEARTNOT                              
253000                                MOD-FLART-RETULF                          
253100                                MOD-RETULF                                
253200                                MOD-KDVALISO                              
253300                                MOD-PRLFKST                               
253400                                MOD-REDIRLEV                              
253500     .                                                                    
253600     EJECT                                                                
253700 MFS-ROER-EJ-FAELT-INDATA SECTION.                                        
253800     SKIP3                                                                
253900                                                                          
254000     MOVE MFS-ROER-EJ-FAELT TO  MOD-KDPRURSP-U                            
254100                                MOD-TIPRLIST-U                            
254200                                MOD-PRARTBEL-U                            
254300*                               MOD-RETULF-U                              
254400                                MOD-KDVALISO-U                            
254500                                MOD-PRLFKST-U                             
254600                                MOD-TEARTNOT-U                            
254700                                MOD-FLPRIBES-U                            
254800                                MOD-IDLEVNR-U                             
254900                                MOD-KDFPKPRI-U                            
255000     .                                                                    
255100     EJECT                                                                
255200 MFS-RENSA-FAELT-UTDATA SECTION.                                          
255300     SKIP3                                                                
255400                                                                          
255500     MOVE MFS-RENSA-FAELT  TO   MOD-RETULF-LEV                            
255600                                MOD-BEART                                 
255700                                MOD-PRINK                                 
255800                                MOD-PRARTSTD                              
255900                                MOD-PRARTBES                              
256000                                MOD-PRARTSJK                              
256100                                MOD-KDTIPPR                               
256200                                MOD-IDANSK                                
256300                                MOD-IDINK                                 
256400                                MOD-TIURPROD                              
256500                                MOD-IDLEVNR                               
256600                                MOD-KVLS-TOT                              
256700                                MOD-KVPB-TOT                              
256800                                MOD-IDLEVNR-SEN                           
256900                                MOD-TIAVIDAT-SEN                          
257000                                MOD-BEST-PRIS(1)                          
257100                                MOD-BEST-PRIS(2)                          
257200                                MOD-BEST-PRIS(3)                          
257300                                MOD-BEST-PRIS(4)                          
257400                                MOD-BEST-PRIS(5)                          
257500                                MOD-BESTALLNING(1)                        
257600                                MOD-BESTALLNING(2)                        
257700                                MOD-TEARTNOT                              
257800                                MOD-FLART-RETULF                          
257900                                MOD-RETULF                                
258000                                MOD-KDVALISO                              
258100                                MOD-PRLFKST                               
258200                                MOD-REDIRLEV                              
258300                                                                          
258400     .                                                                    
258500     EJECT                                                                
258600 MFS-RENSA-FAELT-INDATA SECTION.                                          
258700     SKIP3                                                                
258800                                                                          
258900     MOVE MFS-RENSA-FAELT  TO   MOD-KDPRURSP-U                            
259000                                MOD-TIPRLIST-U                            
259100                                MOD-PRARTBEL-U                            
259200*                               MOD-RETULF-U                              
259300                                MOD-KDVALISO-U                            
259400                                MOD-PRLFKST-U                             
259500                                MOD-TEARTNOT-U                            
259600                                MOD-FLPRIBES-U                            
259700                                MOD-IDLEVNR-U                             
259800                                MOD-KDFPKPRI-U                            
259900     .                                                                    
260000     EJECT                                                                
260100                                                                          
260200 IMS-GET-MSG SECTION.                                                     
260300                                                                          
260400     MOVE '  QC' TO GODK-STATUSKODER                                      
260500     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
260600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
260700     PERFORM IMS-STATUSKONTROLL                                           
260800     .                                                                    
260900     SKIP3                                                                
261000 IMS-GN-MSG-KOM SECTION.                                                  
261100                                                                          
261200     MOVE '  QD' TO GODK-STATUSKODER                                      
261300     CALL CBLTDLI USING GN MSG-PCB KOM-IO-AREA                            
261400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
261500     PERFORM IMS-STATUSKONTROLL                                           
261600     .                                                                    
261700     EJECT                                                                
261800 IMS-INSERT-MSG SECTION.                                                  
261900                                                                          
262000     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
262100       MOVE '0' TO MFS-KDHUVOMR                                           
262200     END-IF                                                               
262300     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
262400     MOVE SPACE TO GODK-STATUSKODER                                       
262500     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
262600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
262700     PERFORM IMS-STATUSKONTROLL                                           
262800     .                                                                    
262900     SKIP3                                                                
263000 IMS-INSERT-MSG-KOM SECTION.                                              
263100                                                                          
263200     MOVE SPACE TO GODK-STATUSKODER                                       
263300     CALL CBLTDLI USING ISRT ALT-PCB KOM-IO-AREA                          
263400     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
263500     PERFORM IMS-STATUSKONTROLL                                           
263600     .                                                                    
263700     EJECT                                                                
263800                                                                          
263900 IMS-GHU-WLARTC01 SECTION.                                                
264000                                                                          
264100     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
264200     DELIMITED  BY SIZE INTO SSA1                                         
264300     MOVE '  GE' TO GODK-STATUSKODER                                      
264400     CALL CBLTDLI USING GHU ARTC-PCB WLARTC01 SSA1                        
264500     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
264600     PERFORM IMS-STATUSKONTROLL                                           
264700     .                                                                    
264800     SKIP3                                                                
264900 IMS-GHNP-WLARTC11 SECTION.                                               
265000                                                                          
265100     MOVE 'WLARTC11 ' TO SSA1                                             
265200     MOVE '    ' TO GODK-STATUSKODER                                      
265300     CALL CBLTDLI USING GHNP ARTC-PCB WLARTC11 SSA1                       
265400     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
265500     PERFORM IMS-STATUSKONTROLL                                           
265600     .                                                                    
265700     EJECT                                                                
265800 IMS-GHNP-WLARTC21-FIRST SECTION.                                         
265900                                                                          
266000     MOVE 'WLARTC11(KDSEGKEY =1)' TO SSA1                                 
266100     MOVE 'WLARTC21*F' TO SSA2                                            
266200     MOVE '  GE' TO GODK-STATUSKODER                                      
266300     CALL CBLTDLI USING GHNP ARTC-PCB WLARTC21 SSA1 SSA2                  
266400     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
266500     PERFORM IMS-STATUSKONTROLL                                           
266600     .                                                                    
266700     SKIP3                                                                
266800 IMS-GHNP-WLARTC21-KVAL SECTION.                                          
266900                                                                          
267000     MOVE 'WLARTC11(KDSEGKEY =1)' TO SSA1                                 
267100     STRING 'WLARTC21(WDK621KY= ' W-WDK621KY-X ')'                        
267200          DELIMITED BY SIZE INTO SSA2                                     
267300     MOVE '  GE' TO GODK-STATUSKODER                                      
267400     CALL CBLTDLI USING GHNP ARTC-PCB WLARTC21 SSA1 SSA2                  
267500     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
267600     PERFORM IMS-STATUSKONTROLL                                           
267700     .                                                                    
267800     SKIP3                                                                
267900 IMS-GHNP-WLARTC21-NEXT SECTION.                                          
268000                                                                          
268100     MOVE 'WLARTC11(KDSEGKEY =1)' TO SSA1                                 
268200     MOVE 'WLARTC21 ' TO SSA2                                             
268300     MOVE '  GE' TO GODK-STATUSKODER                                      
268400     CALL CBLTDLI USING GHNP ARTC-PCB WLARTC21 SSA1 SSA2                  
268500     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
268600     PERFORM IMS-STATUSKONTROLL                                           
268700     .                                                                    
268800     EJECT                                                                
268900 IMS-GNP-WLARTC22 SECTION.                                                
269000                                                                          
269100     MOVE 'WLARTC11(KDSEGKEY =1)' TO SSA1                                 
269200     MOVE 'WLARTC22 ' TO SSA2                                             
269300     MOVE '  GE' TO GODK-STATUSKODER                                      
269400     CALL CBLTDLI USING GNP ARTC-PCB WLARTC22 SSA1 SSA2                   
269500     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
269600     PERFORM IMS-STATUSKONTROLL                                           
269700     .                                                                    
269800     SKIP3                                                                
269900 IMS-GNP-WLARTC23 SECTION.                                                
270000                                                                          
270100     MOVE 'WLARTC11(KDSEGKEY =1)' TO SSA1                                 
270200     MOVE 'WLARTC23 ' TO SSA2                                             
270300     MOVE '  GE' TO GODK-STATUSKODER                                      
270400     CALL CBLTDLI USING GNP ARTC-PCB WLARTC23 SSA1 SSA2                   
270500     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
270600     PERFORM IMS-STATUSKONTROLL                                           
270700     .                                                                    
270800     SKIP3                                                                
270900 IMS-GHNP-WLARTC25 SECTION.                                               
271000                                                                          
271100     MOVE 'WLARTC11(KDSEGKEY =1)' TO SSA1                                 
271200     STRING 'WLARTC25(KDNOTTYP =' W-KDNOTTYP-X ')'                        
271300            DELIMITED BY SIZE INTO SSA2                                   
271400     MOVE '  GE' TO GODK-STATUSKODER                                      
271500     CALL CBLTDLI USING GHNP ARTC-PCB WLARTC25 SSA1 SSA2                  
271600     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
271700     PERFORM IMS-STATUSKONTROLL                                           
271800     .                                                                    
271900     EJECT                                                                
272000 IMS-ISRT-WLARTC21 SECTION.                                               
272100                                                                          
272200     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
272300     DELIMITED  BY SIZE INTO SSA1                                         
272400     MOVE 'WLARTC11(KDSEGKEY =1)' TO SSA2                                 
272500     MOVE 'WLARTC21 ' TO SSA3                                             
272600     MOVE '  GE' TO GODK-STATUSKODER                                      
272700     CALL CBLTDLI USING ISRT ARTC-PCB WLARTC21 SSA1 SSA2 SSA3             
272800     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
272900     PERFORM IMS-STATUSKONTROLL                                           
273000     .                                                                    
273100     SKIP3                                                                
273200 IMS-ISRT-WLARTC25 SECTION.                                               
273300                                                                          
273400     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
273500     DELIMITED  BY SIZE INTO SSA1                                         
273600     MOVE 'WLARTC11(KDSEGKEY =1)' TO SSA2                                 
273700     MOVE 'WLARTC25 ' TO SSA3                                             
273800     MOVE '  GE' TO GODK-STATUSKODER                                      
273900     CALL CBLTDLI USING ISRT ARTC-PCB WLARTC25 SSA1 SSA2 SSA3             
274000     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
274100     PERFORM IMS-STATUSKONTROLL                                           
274200     .                                                                    
274300     EJECT                                                                
274400 IMS-DLET-WLARTC21 SECTION.                                               
274500                                                                          
274600     MOVE '  GE' TO GODK-STATUSKODER                                      
274700     CALL CBLTDLI USING DLET ARTC-PCB WLARTC21                            
274800     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
274900     PERFORM IMS-STATUSKONTROLL                                           
275000     .                                                                    
275100     SKIP3                                                                
275200 IMS-DLET-WLARTC25 SECTION.                                               
275300                                                                          
275400     MOVE '  GE' TO GODK-STATUSKODER                                      
275500     CALL CBLTDLI USING DLET ARTC-PCB WLARTC25                            
275600     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
275700     PERFORM IMS-STATUSKONTROLL                                           
275800     .                                                                    
275900     SKIP3                                                                
276000 IMS-REPL-WLARTC-WDK611 SECTION.                                          
276100                                                                          
276200     MOVE '    ' TO GODK-STATUSKODER                                      
276300     CALL CBLTDLI USING REPL ARTC-PCB WLARTC11                            
276400     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
276500     PERFORM IMS-STATUSKONTROLL                                           
276600     .                                                                    
276700     SKIP3                                                                
276800 IMS-REPL-WLARTC21 SECTION.                                               
276900                                                                          
277000     MOVE '    ' TO GODK-STATUSKODER                                      
277100     CALL CBLTDLI USING REPL ARTC-PCB WLARTC21                            
277200     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
277300     PERFORM IMS-STATUSKONTROLL                                           
277400     .                                                                    
277500     EJECT                                                                
277600 IMS-REPL-WLARTC25 SECTION.                                               
277700                                                                          
277800     MOVE '    ' TO GODK-STATUSKODER                                      
277900     CALL CBLTDLI USING REPL ARTC-PCB WLARTC25                            
278000     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
278100     PERFORM IMS-STATUSKONTROLL                                           
278200     .                                                                    
278300     EJECT                                                                
278400 IMS-GU-WLARTS01 SECTION.                                                 
278500                                                                          
278600     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
278700     DELIMITED  BY SIZE INTO SSA1                                         
278800     MOVE '  GE' TO GODK-STATUSKODER                                      
278900     CALL CBLTDLI USING GU ARTS-PCB WLARTS01 SSA1                         
279000     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
279100     PERFORM IMS-STATUSKONTROLL                                           
279200     .                                                                    
279300     SKIP3                                                                
279400 IMS-GNP-WLARTS11 SECTION.                                                
279500                                                                          
279600     MOVE 'WLARTS11 ' TO SSA1                                             
279700     MOVE '  GE' TO GODK-STATUSKODER                                      
279800     CALL CBLTDLI USING GNP ARTS-PCB WLARTS11 SSA1                        
279900     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
280000     PERFORM IMS-STATUSKONTROLL                                           
280100     .                                                                    
280200     SKIP2                                                                
280300 IMS-ISRT-WDH801   SECTION.                                               
280400                                                                          
280500     MOVE 'WLPRIG01 ' TO SSA1                                             
280600     MOVE '  II'            TO GODK-STATUSKODER                           
280700     CALL CBLTDLI USING ISRT PRIG-PCB WLPRIG01 SSA1                       
280800     MOVE PRIG-STATUS-CODE  TO STATUS-WS                                  
280900     PERFORM IMS-STATUSKONTROLL                                           
281000     .                                                                    
281100     EJECT                                                                
281200 IMS-GU-WLBENA11 SECTION.                                                 
281300                                                                          
281400     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
281500     DELIMITED BY SIZE INTO SSA1                                          
281600     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X  ')'                        
281700     DELIMITED BY SIZE INTO SSA2                                          
281800     MOVE '  GE' TO GODK-STATUSKODER                                      
281900     CALL CBLTDLI USING GU BEN-PCB BEN-WLBENA11 SSA1 SSA2                 
282000     MOVE BEN-STATUS-CODE TO STATUS-WS                                    
282100     PERFORM IMS-STATUSKONTROLL                                           
282200     .                                                                    
282300     SKIP3                                                                
282400 IMS-GU-WLLEVA01 SECTION.                                                 
282500                                                                          
282600     STRING 'WLLEVA01(IDLEVNR  =' W-IDLEVNR-X ')'                         
282700     DELIMITED BY SIZE INTO SSA1                                          
282800     MOVE '  GE' TO GODK-STATUSKODER                                      
282900     CALL CBLTDLI USING GU LEV-PCB WLLEVA01 SSA1                          
283000     MOVE LEV-STATUS-CODE TO STATUS-WS                                    
283100     PERFORM IMS-STATUSKONTROLL                                           
283200     .                                                                    
283300     SKIP3                                                                
283400 IMS-GNP-WLLEVA11 SECTION.                                                
283500                                                                          
283600     STRING 'WLLEVA11(IDLAND   =' W-IDLAND-X ')'                          
283700     DELIMITED BY SIZE INTO SSA1                                          
283800     MOVE '  GE' TO GODK-STATUSKODER                                      
283900     CALL CBLTDLI USING GNP LEV-PCB LEV-WLLEVA11 SSA1                     
284000     MOVE LEV-STATUS-CODE TO STATUS-WS                                    
284100     PERFORM IMS-STATUSKONTROLL                                           
284200     .                                                                    
284300     EJECT                                                                
284400 IMS-GU-WLINLE01 SECTION.                                                 
284500                                                                          
284600     STRING 'WLINLE01(IDARTNR  =' W-IDARTNR-X ')'                         
284700              DELIMITED BY SIZE INTO SSA1                                 
284800     MOVE '  GE' TO GODK-STATUSKODER                                      
284900     CALL CBLTDLI USING GU INLE-PCB INLE-WLINLE01 SSA1                    
285000     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
285100     PERFORM IMS-STATUSKONTROLL                                           
285200     .                                                                    
285300     SKIP3                                                                
285400 IMS-GNP-WLINLE11 SECTION.                                                
285500                                                                          
285600     STRING 'WLINLE11(DAINLEV =<' W-DAINLEV-X ')'                         
285700              DELIMITED BY SIZE INTO SSA1                                 
285800     MOVE '  GE' TO GODK-STATUSKODER                                      
285900     CALL CBLTDLI USING GNP INLE-PCB INLE-WLINLE11 SSA1                   
286000     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
286100     PERFORM IMS-STATUSKONTROLL                                           
286200     .                                                                    
286300     EJECT                                                                
286400 IMS-GNP-WLINLE21 SECTION.                                                
286500                                                                          
286600     STRING 'WLINLE11(DAINLEV  =' W-DAINLEV-X ')'                         
286700              DELIMITED BY SIZE INTO SSA1                                 
286800     MOVE 'WLINLE21 ' TO SSA2                                             
286900     MOVE '  GE' TO GODK-STATUSKODER                                      
287000     CALL CBLTDLI USING GNP INLE-PCB INLE-WLINLE21 SSA1 SSA2              
287100     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
287200     PERFORM IMS-STATUSKONTROLL                                           
287300     .                                                                    
287400     SKIP3                                                                
287500 IMS-GNP-WLINLE22 SECTION.                                                
287600                                                                          
287700     STRING 'WLINLE11(DAINLEV  =' W-DAINLEV-X ')'                         
287800              DELIMITED BY SIZE INTO SSA1                                 
287900     MOVE 'WLINLE22 ' TO SSA2                                             
288000     MOVE '  GE' TO GODK-STATUSKODER                                      
288100     CALL CBLTDLI USING GNP INLE-PCB INLE-WLINLE22 SSA1 SSA2              
288200     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
288300     PERFORM IMS-STATUSKONTROLL                                           
288400     .                                                                    
288500     EJECT                                                                
288600 IMS-GU-WDB601    SECTION.                                                
288700     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
288800          DELIMITED BY SIZE INTO SSA1                                     
288900     MOVE '  GE' TO GODK-STATUSKODER                                      
289000     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
289100     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
289200     PERFORM IMS-STATUSKONTROLL                                           
289300     IF SEGMENT-SAKNAS                                                    
289400         MOVE SPACE TO DCS-KDDC                                           
289500     END-IF                                                               
289600     .                                                                    
289700     EJECT                                                                
289800 IMS-STATUSKONTROLL SECTION.                                              
289900                                                                          
290000     SET STATUS-IX TO 1                                                   
290100     SEARCH GODK-STATUS AT END CALL FELLOG                                
290200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
290300     END-SEARCH                                                           
290400     .                                                                    
290500     EJECT                                                                
290600*    -COPY WY2000P1                                                       
290700*    -COPY WY2000P9                                                       
