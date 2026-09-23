000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W1022100.                                                
000400 AUTHOR.         HENRIK ARONSSON.                                         
000500 DATE-WRITTEN.   MAJ 1990.                                                
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION.                                                            
001000*        - VISAR INFORMATION OM ARTIKEL SAMT INFORMATION OM               
001100*          DE STRUKTURER SÖKT ARTIKEL INGÅR I.                            
001200*                                                                         
001300*        - MED DETTA PROGRAM VÄLJER MAN UT I VILKA STRUKTURER             
001400*          SÖKT ARTIKEL SKALL TAS BORT/BYTAS UT. SJÄLVA BYTET/            
001500*          BORTTAGNINGEN AV ARTIKEL SKER FRÅN BILD 1222.                  
001600*                                                                         
001700*        - PRINTNING AV LISTA SKER MED HJÄLP AV BAKGRUNDS-                
001800*          PROGRAMMET W10291.                                             
001900*                                                                         
002000*        PROGRAMMET ÄR EN UPPDATERINGS-MPP                                
002100*        PROGRAMMET UPPDATERAR WLSATB (WDJ1)                              
002200*        PROGRAMMET UPPDATERAR WLXXAZ (WDR5)                              
002300*        PROGRAMMET LÄSER      WLBENA (WDD3)                              
002400*        PROGRAMMET LÄSER      WLARTC (WDK6)                              
002500*                                                                         
002600*        ÄT SPLIT 930404 BL                                               
002700*           - ÄNDRING GODKÄNDA KDPRODSL                                   
002800*                                                                         
002900*    INDATA.                                                              
003000*        TRANSAKTION: W1T221                                              
003100*        MID:         W1I22101                                            
003200*                                                                         
003300*    UTDATA.                                                              
003400*        MOD:         W1O22101                                            
003500                                                                          
003600     SKIP3                                                                
003700 ENVIRONMENT DIVISION.                                                    
003800     EJECT                                                                
003900 DATA DIVISION.                                                           
004000 WORKING-STORAGE SECTION.                                                 
004100                                                                          
004200*    -COPY WY2000W1                                                       
004300     SKIP3                                                                
004400 77  IDPGM                       PIC X(08)   VALUE 'W1022100'.            
004500                                                                          
004600 77  JA                          PIC X       VALUE 'J'.                   
004700 77  NEJ                         PIC X       VALUE 'N'.                   
004800                                                                          
004900*    --- INDEX FÖR BLÄDDRINGSRADER                                        
005000 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
005100 77  MAX-INDX                    PIC S9(4)  VALUE +10   COMP SYNC.        
005200                                                                          
005300 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
005400                                                                          
005500*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005600 77  WS-IDLEVNR                  PIC X(5)    VALUE SPACE.                 
005700 77  WS-IDLEVNR-TIDIGARE-STRUKT  PIC X(5)    VALUE SPACE.                 
005800 77  WS-IDLEVNR-TIDIGARE-RADVAL  PIC X(5)    VALUE SPACE.                 
005900 77  WS-IDARTNR                  PIC X(9)    VALUE SPACE.                 
006000 77  WS-IDARTNR-2                PIC X(9)    VALUE SPACE.                 
006100 77  WS-IDARTNR-NUM              PIC 9(9)    VALUE ZERO.                  
006200 77  WS-IDARTNR-KONVERTERAT      PIC 9(9)    VALUE ZERO.                  
006300 77  WS-BELEVART                 PIC X(30)   VALUE SPACE.                 
006400 77  WS-IDSKYLT                  PIC X(3)    VALUE SPACE.                 
006500 77  WS-1002-SATS                PIC X(1)    VALUE SPACE.                 
006600 77  WS-KDPRODSL                 PIC X(2)    VALUE SPACE.                 
006700 77  WS-KDPRODSL-NUM             PIC 9(2)    VALUE ZERO.                  
006800 77  WS-IDRADNR                  PIC X(4)    VALUE SPACE.                 
006900                                                                          
007000 77  INDATA-SW                   PIC X       VALUE 'J'.                   
007100     88  INDATA-OK                           VALUE 'J'.                   
007200     88  INDATA-FEL                          VALUE 'N'.                   
007300                                                                          
007400 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
007500     88  NYCKLAR-OK                          VALUE 'J'.                   
007600     88  NYCKLAR-FEL                         VALUE 'N'.                   
007700                                                                          
007800 77  ALLT-SW                     PIC X       VALUE 'J'.                   
007900     88  ALLT-OK                             VALUE 'J'.                   
008000                                                                          
008100 77  MID-INPUT-SW                PIC X       VALUE 'J'.                   
008200     88  MID-INPUT-IFYLLD                    VALUE 'J'.                   
008300     88  MID-INPUT-EJ-IFYLLD                 VALUE 'N'.                   
008400                                                                          
008500 77  RAD-SW                      PIC X       VALUE 'J'.                   
008600     88  RAD-OK                              VALUE 'J'.                   
008700                                                                          
008800 77  RADVAL-SW                   PIC X       VALUE 'J'.                   
008900     88  RADVAL-OK                           VALUE 'J'.                   
009000     88  RADVAL-FEL                          VALUE 'N'.                   
009100                                                                          
009200 77  SOEKNYCKELTYP-SW            PIC X       VALUE SPACE.                 
009300     88  SOEKNYCKEL-IDARTNR                  VALUE 'A'.                   
009400     88  SOEKNYCKEL-IDLEVNR-O-BELEVART       VALUE 'I'.                   
009500                                                                          
009600 77  SOEKNYCKEL-LAAST-AV-ANNAN-SW PIC X      VALUE 'N'.                   
009700     88  SOEKNYCKEL-LAAST-AV-ANNAN-USER      VALUE 'J'.                   
009800                                                                          
009900 77  SOEKNYCKEL-LAAST-AV-EGET-ID-SW PIC X    VALUE 'N'.                   
010000     88  SOEKNYCKEL-LAAST-AV-EGET-ID         VALUE 'J'.                   
010100                                                                          
010200 77  IDARTNR-ALT-ERS-PAA-ARTREG-SW  PIC X    VALUE 'N'.                   
010300     88 IDARTNR-ALT-ERS-PAA-ARTREG           VALUE 'J'.                   
010400                                                                          
010500 77  TILLKOMMANDE-ARTIKLAR-FINNS-SW PIC X    VALUE 'N'.                   
010600     88 TILLKOMMANDE-ARTIKLAR-FINNS          VALUE 'J'.                   
010700                                                                          
010800 77  TIDIGARE-VALDA-STRUKT-FINNS-SW PIC X    VALUE 'N'.                   
010900     88 TIDIGARE-VALDA-STRUKT-FINNS          VALUE 'J'.                   
011000                                                                          
011100 77  TIDIGARE-VALD-RAD-FINNS-SW     PIC X    VALUE 'N'.                   
011200     88 TIDIGARE-VALD-RAD-FINNS              VALUE 'J'.                   
011300                                                                          
011400 77  SIGNON-USER-HAR-KONV-STRUKT-SW PIC X    VALUE 'N'.                   
011500     88 SIGNON-USERID-HAR-KONV-STRUKT        VALUE 'J'.                   
011600                                                                          
011700 77  STRUKTURNR-FINNS-PAA-WDK6-SW PIC X.                                  
011800     88 STRUKTURNR-FINNS-PAA-WDK6            VALUE 'J'.                   
011900                                                                          
012000 77  SATS-I-STR-RA           PIC X(3)    VALUE '242'.                     
012100 77  SATS-I-STR-RB           PIC X(3)    VALUE '243'.                     
012200 77  SATS-I-STR-BERPV        PIC X(3)    VALUE '246'.                     
012300 77  SATS-I-STR-CARP         PIC X(3)    VALUE '241'.                     
012400                                                                          
012500 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
012600     88  EGEN-MID                            VALUE '1221'.                
012700     88  GODK-MID                            VALUE '1221' '1222'.         
012800     EJECT                                                                
012900*                                                                         
013000*01    -COPY WWPRODSL                                                     
013100*      --- VALID IDDC CODES                                               
013200*                                                                         
013300*01    -COPY WWDCKONS                                                     
013400*01    -COPY WWDC99                                                       
013500       EJECT                                                              
013600*    --- DIVERSE VARIABLER                                                
013700                                                                          
013800 01  WS-MAX-IDARTNR              PIC S9(9)   VALUE +100000000.            
013900 01  WS-MIN-IDARTNR-KONV         PIC S9(9)   VALUE +100000000.            
014000 01  WS-MAX-IDARTNR-KONV         PIC S9(9)   VALUE +999999999.            
014100 01  WS-BEART                    PIC X(25)   VALUE SPACE.                 
014200 01  WS-KDBENHOM                 PIC S9(1)   VALUE ZERO.                  
014300                                                                          
014400 01  IDARTNR                     PIC X       VALUE 'A'.                   
014500 01  IDLEVNR-O-BELEVART          PIC X       VALUE 'I'.                   
014600                                                                          
014700 01  DAGENS-DATUM                PIC 9(6).                                
014800                                                                          
014900 01  WS-SPAR-SPAERWDATUM         PIC 9(6).                                
015000                                                                          
015100 01  TRANS-TILL-1291             PIC X       VALUE SPACE.                 
015200                                                                          
015300 01  MEDDELANDEN.                                                         
015400     03  FEL-1.                                                           
015500         05 FILLER               PIC X(40)   VALUE                        
015600             'UPPDATERING EJ TILLÅTEN                 '.                  
015700         05 FILLER               PIC X(40)   VALUE                        
015800             'UPDATE NOT ALLOWED                      '.                  
015900     03  FILLER REDEFINES FEL-1.                                          
016000         05 FEL1 OCCURS 2        PIC X(40).                               
016100                                                                          
016200     03  FEL-2.                                                           
016300         05 FILLER               PIC X(40)   VALUE                        
016400             'STRUKTURER SAKNAS                       '.                  
016500         05 FILLER               PIC X(40)   VALUE                        
016600             'STRUCTURES ARE MISSING                  '.                  
016700     03  FILLER REDEFINES FEL-2.                                          
016800         05 FEL2 OCCURS 2        PIC X(40).                               
016900                                                                          
017000     03  FEL-3.                                                           
017100         05 FILLER               PIC X(40)   VALUE                        
017200             'ARTIKEL ALTERNATIVT ERSATT              '.                  
017300         05 FILLER               PIC X(40)   VALUE                        
017400             'PART NOT UNAMBIGUOUS SUPERSEDED         '.                  
017500     03  FILLER REDEFINES FEL-3.                                          
017600         05 FEL3 OCCURS 2        PIC X(40).                               
017700                                                                          
017800     03  MED-1.                                                           
017900         05 FILLER               PIC X(61)   VALUE                        
018000             'NYCKEL SPÄRRAD AV ANNAN ANVÄNDARE       '.                  
018100         05 FILLER               PIC X(61)   VALUE                        
018200             'KEY LOCKED BY ANOTHER USER              '.                  
018300     03  FILLER REDEFINES MED-1.                                          
018400         05 MED1 OCCURS 2        PIC X(61).                               
018500                                                                          
018600     03  MED-2.                                                           
018700         05 FILLER               PIC X(61)   VALUE                        
018800             'DU HAR STRUKTUR UNDER BEARBETNING    '.                     
018900         05 FILLER               PIC X(61)   VALUE                        
019000             'YOU HAVE STRUCTURE IN USE               '.                  
019100     03  FILLER REDEFINES MED-2.                                          
019200         05 MED2 OCCURS 2        PIC X(61).                               
019300                                                                          
019400     03  MED-3.                                                           
019500         05 FILLER               PIC X(61)   VALUE                        
019600             'STRUKTUR ÄR UNDER BEARBETNING           '.                  
019700         05 FILLER               PIC X(61)   VALUE                        
019800             'STRUCTURE IS IN USE                     '.                  
019900     03  FILLER REDEFINES MED-3.                                          
020000         05 MED3 OCCURS 2        PIC X(61).                               
020100                                                                          
020200     03  MED-4.                                                           
020300         05 FILLER               PIC X(61)   VALUE                        
020400             'TILLKOMMANDE ARTIKLAR FINNS REGISTRERADE'.                  
020500         05 FILLER               PIC X(61)   VALUE                        
020600             'NEW PART NO. ARE REGISTRED (ON 1222)    '.                  
020700     03  FILLER REDEFINES MED-4.                                          
020800         05 MED4 OCCURS 2        PIC X(61).                               
020900                                                                          
021000     03  MED-5.                                                           
021100         05 FILLER               PIC X(61)   VALUE                        
021200             'BORTTAG EJ MÖJLIG                       '.                  
021300         05 FILLER               PIC X(61)   VALUE                        
021400             'DELETE NOT POSSIBLE                     '.                  
021500     03  FILLER REDEFINES MED-5.                                          
021600         05 MED5 OCCURS 2        PIC X(61).                               
021700                                                                          
021800     03  MED-6.                                                           
021900         05 FILLER               PIC X(27)   VALUE                        
022000             'LISTA KÖAD FÖR UTSKRIFT PÅ '.                               
022100         05 FILLER               PIC X(34).                               
022200         05 FILLER               PIC X(27)   VALUE                        
022300             'LIST IS QUEUED TO PRINTER  '.                               
022400         05 FILLER               PIC X(34).                               
022500                                                                          
022600     03  FILLER REDEFINES MED-6.                                          
022700         05 MED6 OCCURS 2.                                                
022800            07 FILLER            PIC X(27).                               
022900            07 MED6-IDLTERM      PIC X(8).                                
023000            07 MED6-KOMMATECKEN  PIC X.                                   
023100            07 MED6-BEPRT        PIC X(25).                               
023200                                                                          
023300     03  MED-7.                                                           
023400         05 FILLER               PIC X(61)   VALUE                        
023500             'RADEN U-MÄRKT                           '.                  
023600         05 FILLER               PIC X(61)   VALUE                        
023700             'LINE IS MARKED TO BE DELETED            '.                  
023800     03  FILLER REDEFINES MED-7.                                          
023900         05 MED7 OCCURS 2        PIC X(61).                               
024000                                                                          
024100     03  MED-8.                                                           
024200         05 FILLER               PIC X(61)   VALUE                        
024300            'VAL AV BÅDE 1002-SATS OCH EJ-1002-SATS EJ TILLÅTEN'.         
024400         05 FILLER               PIC X(61)   VALUE                        
024500        'SELECTION OF BOTH 1002-KIT AND NON-1002-KIT NOT ALLOWED'.        
024600     03  FILLER REDEFINES MED-8.                                          
024700         05 MED8 OCCURS 2        PIC X(61).                               
024800                                                                          
024900     03  MED-9.                                                           
025000         05 FILLER           PIC X(61)   VALUE                            
025100         'FEL I W006PRT DEFINITION, KONTAKTA SYSTEMAVD'.                  
025200         05 FILLER           PIC X(61)   VALUE                            
025300         'MAJOR ERROR IN W006PRT, CONTACT YOUR SYSTEM SUPPORT'.           
025400     03  FILLER REDEFINES MED-9.                                          
025500         05 MED9 OCCURS 2 PIC X(61).                                      
025600                                                                          
025700     EJECT                                                                
025800*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
025900 01  GENERELLA-SUBPROGRAM.                                                
026000     03  W006PRT                 PIC X(8)    VALUE 'W006PRT '.            
026100     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
026200     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY '.            
026300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
026400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
026500     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
026600     03  W009CIA                 PIC X(8)    VALUE 'W009CIA '.            
026700     SKIP3                                                                
026800*01  -COPY W009CIA                                                        
026900     EJECT                                                                
027000*   -COPY WWLAND03                                                        
027100     EJECT                                                                
027200*   -COPY WORKAREA                                                        
027300     EJECT                                                                
027400*    --- PARAMETRAR TILL SUBPROGRAM W006PRT                               
027500*   -COPY W006PRT                                                         
027600     EJECT                                                                
027700*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
027800*   -COPY WMEDAREA                                                        
027900     EJECT                                                                
028000*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
028100*   -COPY WMSGINIT                                                        
028200     SKIP3                                                                
028300 01  MESSAGE-CODES.                                                       
028400     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
028500     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
028600     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
028700     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
028800     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
028900     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
029000     03  INF-LAST-PAGE           PIC X(3)    VALUE '106'.                 
029100     EJECT                                                                
029200*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
029300*                                                                         
029400 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
029500     SKIP3                                                                
029600*01  MID -COPY W1I22101                                                   
029700     EJECT                                                                
029800 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
029900     SKIP3                                                                
030000*01  -COPY WMSGAREA                                                       
030100     EJECT                                                                
030200     03  MOD REDEFINES MSG-AREA.                                          
030300*      05  -COPY W1O22101                                                 
030400     EJECT                                                                
030500 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
030600     SKIP3                                                                
030700*01  -COPY WMFSAREA                                                       
030800     EJECT                                                                
030900*    ---------------------ALT-AREA                                        
031000 01  FILLER                      PIC X(16)   VALUE 'ALT-AREA'.            
031100 01  W-PROG-TO-PROG-SW.                                                   
031200     03  M-SW-LL                 PIC S9(4)   VALUE +300                   
031300                                             COMP SYNC.                   
031400     03  M-SW-Z1-Z2              PIC X(2)    VALUE LOW-VALUE.             
031500     03  M-SW-KDTRANS            PIC X(8)    VALUE 'W1T291X'.             
031600     03  M-SW-IDTRANS            PIC X(4)    VALUE '1291'.                
031700     03  M-SW-KDMFSTYP           PIC X(1)    VALUE '1'.                   
031800     03  MID-W1I29101.                                                    
031900*        05  MID -COPY W1I22101   -PRE R1291-                             
032000     EJECT                                                                
032100*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
032200*                                                                         
032300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
032400     SKIP3                                                                
032500 01  NYCKLAR-TILL-DLI.                                                    
032600     03  W-WDJ1CSEQ-X.                                                    
032700         05  W-IDLEVNR-X.                                                 
032800             07   W-IDLEVNR      PIC X(5)   VALUE SPACE.                  
032900         05  W-BELEVART-X.                                                
033000             07   W-BELEVART     PIC X(30)  VALUE SPACE.                  
033100         05  W-IDARTNR-X.                                                 
033200             07   W-IDARTNR      PIC S9(9)  VALUE ZERO COMP-3.            
033300                                                                          
033400     03  W-WDJ111KY-X.                                                    
033500         05  W-KDSTRRAD          PIC X      VALUE SPACE.                  
033600         05  W-IDRADNR           PIC S9(5)  VALUE ZERO COMP-3.            
033700                                                                          
033800     03  W-WDGXKEY-X.                                                     
033900         05  W-IDHTYP            PIC X(4)   VALUE SPACE.                  
034000         05  W-LOW-VALUE-2       PIC X(26)  VALUE SPACE.                  
034100                                                                          
034200     03  W-IDARTNR-2-X.                                                   
034300         05  W-IDARTNR-2         PIC S9(9)   VALUE ZERO COMP-3.           
034400                                                                          
034500     03  W-BEART-X.                                                       
034600         05  W-BEART             PIC X(25)  VALUE SPACE.                  
034700                                                                          
034800     03  W-IDUSER-X.                                                      
034900         05  W-IDUSER            PIC X(8)   VALUE SPACE.                  
035000                                                                          
035100     03  W-LOW-VALUE-X.                                                   
035200         05  W-LOW-VALUE         PIC X(4)   VALUE SPACE.                  
035300                                                                          
035400     03  W-IDSKYLT-X.                                                     
035500         05  W-IDSKYLT           PIC X(3)   VALUE SPACE.                  
035600                                                                          
035700     03  W-KDNOTTYP-X.                                                    
035800         05  W-KDNOTTYP          PIC S9(1)  VALUE ZERO COMP-3.            
035900                                                                          
036000     03  W-KDCLAGER-X.                                                    
036100         05  W-KDCLAGER          PIC S9(1)  VALUE ZERO COMP-3.            
036200                                                                          
036300     03  W-MAX-IDARTNR-KONV-X.                                            
036400         05  W-MAX-IDARTNR-KONV  PIC S9(9)  VALUE ZERO COMP-3.            
036500                                                                          
036600     03  W-MIN-IDARTNR-KONV-X.                                            
036700         05  W-MIN-IDARTNR-KONV  PIC S9(9)  VALUE ZERO COMP-3.            
036800                                                                          
036900*    --- STATUS-KOD FRÅN IMS                                              
037000 01  STATUS-WS                   PIC XX.                                  
037100     88  SEGMENT-FINNS                       VALUE '  '.                  
037200     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
037300     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
037400     88  SEGMENT-HOEGRE                      VALUE 'GB'.                  
037500     SKIP2                                                                
037600 01  GODK-STATUSKODER.                                                    
037700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
037800     SKIP3                                                                
037900 01  SSA1                        PIC X(96).                               
038000 01  SSA2                        PIC X(96).                               
038100 01  SSA3                        PIC X(64).                               
038200     EJECT                                                                
038300*    --- IMS FUNKTIONSKODER                                               
038400*01  -COPY W0003                                                          
038500     EJECT                                                                
038600*    ---  DLI INPUT-OUTPUT AREA                                           
038700 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
038800     SKIP3                                                                
038900 01  DLI-IO-AREA.                                                         
039000     03  IO-AREA                 PIC X(928)  VALUE SPACE.                 
039100     SKIP3                                                                
039200     03  WLARTC01 REDEFINES IO-AREA.                                      
039300*        05  -COPY WDK601                                                 
039400     SKIP3                                                                
039500     03  WLARTC12 REDEFINES IO-AREA.                                      
039600*        05  -COPY WDK611                                                 
039700     SKIP3                                                                
039800     03  WLARTC25 REDEFINES IO-AREA.                                      
039900*        05  -COPY WDK625                                                 
040000     SKIP3                                                                
040100     03  WLSATB01 REDEFINES IO-AREA.                                      
040200*        05  -COPY WDJ101     -PRE SATB01-                                
040300     SKIP3                                                                
040400     03  WLSATB11 REDEFINES IO-AREA.                                      
040500*        05  -COPY WDJ111     -PRE SATB11-                                
040600     SKIP3                                                                
040700     03  WLBENA01 REDEFINES IO-AREA.                                      
040800*        05  -COPY WDD301     -PRE BENA01-                                
040900     SKIP3                                                                
041000     03  WLBENA11 REDEFINES IO-AREA.                                      
041100*        05  -COPY WDD311     -PRE BENA11-                                
041200     SKIP3                                                                
041300     03  WLXXAZ11 REDEFINES IO-AREA.                                      
041400*        05  -COPY WDGX1152   -PRE XXAZ11-                                
041500                                                                          
041600     EJECT                                                                
041700*    ---  DLI INPUT-OUTPUT AREA-2                                         
041800 01  FILLER                      PIC X(16) VALUE 'DLI-IO-AREA-2'.         
041900     SKIP3                                                                
042000 01  DLI-IO-AREA-2.                                                       
042100     03  IO-AREA-2               PIC X(350)  VALUE SPACE.                 
042200     SKIP3                                                                
042300     03  WLSATB-CSEQ REDEFINES IO-AREA-2.                                 
042400         05 WLSATB11.                                                     
042500*            07 -COPY WDJ111     -PRE SATB11C-                            
042600         05 WLSATB01.                                                     
042700*            07 -COPY WDJ101     -PRE SATB01C-                            
042800     EJECT                                                                
042900*    ---  DLI INPUT-OUTPUT AREA-3                                         
043000 01  FILLER                      PIC X(16) VALUE 'DLI-IO-AREA-3'.         
043100     SKIP3                                                                
043200 01  DLI-IO-AREA-3.                                                       
043300     03  IO-AREA-3               PIC X(110) VALUE SPACE.                  
043400     SKIP3                                                                
043500     03  WLARTC01 REDEFINES IO-AREA-3.                                    
043600*        05  -COPY WDK601     -PRE ARTC012-                               
043700     SKIP3                                                                
043800     EJECT                                                                
043900 LINKAGE SECTION.                                                         
044000                                                                          
044100*01  -COPY W0009      -PRE MSG-                                           
044200     EJECT                                                                
044300*01  -COPY W0009      -PRE ALT-                                           
044400     EJECT                                                                
044500*01  -COPY W0008      -PRE USEA-                                          
044600     05  FILLER                  PIC X.                                   
044700     EJECT                                                                
044800*01  -COPY W0008      -PRE ARTC2-                                         
044900     05  FILLER                  PIC X.                                   
045000     EJECT                                                                
045100*01  -COPY W0008      -PRE ARTC-                                          
045200     05  FILLER                  PIC X.                                   
045300     EJECT                                                                
045400*01  -COPY W0008      -PRE SATB-                                          
045500     05  FILLER                  PIC X.                                   
045600     EJECT                                                                
045700*01  -COPY W0008      -PRE SATB-C-                                        
045800     05  FILLER                  PIC X.                                   
045900     EJECT                                                                
046000*01  -COPY W0008      -PRE SATB-D-                                        
046100     05  FILLER                  PIC X.                                   
046200     EJECT                                                                
046300*01  -COPY W0008      -PRE BENA-A-                                        
046400     05  FILLER                  PIC X.                                   
046500     EJECT                                                                
046600*01  -COPY W0008      -PRE BENA-B-                                        
046700     05  FILLER                  PIC X.                                   
046800     EJECT                                                                
046900*01  -COPY W0008      -PRE XXAZ-                                          
047000     05  FILLER                  PIC X.                                   
047100     EJECT                                                                
047200 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB USEA-PCB                       
047300                                           ARTC2-PCB ARTC-PCB             
047400                                   SATB-PCB SATB-C-PCB SATB-D-PCB         
047500                                   BENA-A-PCB BENA-B-PCB XXAZ-PCB.        
047600     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB USEA-PCB                       
047700                                           ARTC2-PCB ARTC-PCB             
047800                                   SATB-PCB SATB-C-PCB SATB-D-PCB         
047900                                   BENA-A-PCB BENA-B-PCB XXAZ-PCB.        
048000                                                                          
048100     PERFORM IMS-GET-MSG                                                  
048200     IF SEGMENT-FINNS                                                     
048300       PERFORM A-INIT                                                     
048400       PERFORM B-KOLLA-NYCKLAR                                            
048500       IF NYCKLAR-OK                                                      
048600         IF MFS-UPDATE                                                    
048700           PERFORM G-KOLLA-INPUT                                          
048800           IF INDATA-OK                                                   
048900             PERFORM H-UPPDATERA                                          
049000           END-IF                                                         
049100         ELSE                                                             
049200           IF MFS-FIRST                                                   
049300             PERFORM C-FOERSTA-SIDA                                       
049400           ELSE                                                           
049500             IF MFS-NEXT                                                  
049600               PERFORM D-NAESTA-SIDA                                      
049700             ELSE                                                         
049800               IF MFS-PRINT                                               
049900                 PERFORM I-KOLLA-INPUT-PRINTER                            
050000                 IF INDATA-OK                                             
050100                   PERFORM J-SKICKA-TRANS-TILL-1291                       
050200                 END-IF                                                   
050300               ELSE                                                       
050400                 PERFORM E-SAMMA-SIDA                                     
050500               END-IF                                                     
050600             END-IF                                                       
050700           END-IF                                                         
050800           IF ALLT-OK                                                     
050900             PERFORM F-LAES-VISA-INFO                                     
051000           END-IF                                                         
051100         END-IF                                                           
051200       END-IF                                                             
051300       MOVE LENGTH OF MOD-W1O22101 TO MSG-KVLL                            
051400       ADD            +4           TO MSG-KVLL                            
051500       PERFORM IMS-INSERT-MSG                                             
051600     END-IF                                                               
051700                                                                          
051800     MOVE ZERO TO RETURN-CODE                                             
051900     GOBACK                                                               
052000     .                                                                    
052100     EJECT                                                                
052200 A-INIT SECTION.                                                          
052300                                                                          
052400     IF MSG-DUBBLA-TRANSKODER                                             
052500       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W1I22101                 
052600       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
052700       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
052800     ELSE                                                                 
052900       MOVE MSG-INDATA-MINUS-1-TRANSKOD TO MID-W1I22101                   
053000       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
053100       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
053200     END-IF                                                               
053300                                                                          
053400     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
053500     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
053600     MOVE MFS-IDTRANS TO W-IDTRANS                                        
053700                                                                          
053800     MOVE LOW-VALUE  TO MSG-AREA                                          
053900     MOVE 'W1O221N1' TO MFS-IDMOD                                         
054000     MOVE '1221'     TO MOD-IDTRANS                                       
054100     MOVE SPACE      TO MOD-TEMFSFEL MOD-TEMFSINF                         
054200                                                                          
054300     IF NOT EGEN-MID                                                      
054400       MOVE SPACE TO MFS-KDTRTYP                                          
054500       MOVE '7'   TO MFS-IDPFK                                            
054600     END-IF                                                               
054700                                                                          
054800     ACCEPT DAGENS-DATUM FROM DATE                                        
054900     MOVE NEJ TO TRANS-TILL-1291                                          
055000     .                                                                    
055100     EJECT                                                                
055200 B-KOLLA-NYCKLAR SECTION.                                                 
055300                                                                          
055400     MOVE JA TO NYCKLAR-SW                                                
055500                                                                          
055600     MOVE MFS-RENSA-FAELT TO MOD-IDLEVNR-IN                               
055700                             MOD-BELEVART-IN                              
055800                             MOD-IDSKYLT-IN                               
055900                             MOD-1002-SATS-IN                             
056000                             MOD-KDPRODSL-IN                              
056100                                                                          
056200     IF GODK-MID                                                          
056300       MOVE ALL '+' TO MSGI-WMSGINIT                                      
056400       MOVE '001'             TO MSGI-KDCALL                              
056500       MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                              
056600       MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                        
056700       MOVE '1221'            TO MSGI-IDTRANS                             
056800       IF MID-IDLEVNR-IN = ALL '+'                                        
056900         MOVE MID-IDLEVNR-UT TO WS-IDLEVNR                                
057000       ELSE                                                               
057100         MOVE MID-IDLEVNR-IN TO WS-IDLEVNR                                
057200                                MSGI-IDLEVNR                              
057300         MOVE '7'            TO MFS-IDPFK                                 
057400         MOVE SPACE          TO MFS-KDTRTYP                               
057500       END-IF                                                             
057600                                                                          
057700       IF MID-BELEVART-IN = ALL '+'                                       
057800******** KAN ANTINGEN VARA EN ARTIKELBENÄMNING                            
057900******** ELLER ETT ARTIKELNUMMER                                          
058000         MOVE MID-BELEVART-UT TO WS-BELEVART                              
058100         MOVE MID-IDARTNR-UT  TO WS-IDARTNR                               
058200       ELSE                                                               
058300         MOVE MID-BELEVART-IN TO WS-BELEVART                              
058400         MOVE 'VO' TO CIA-IDARTPRE-IN                                     
058500         MOVE MID-IDARTNR-IN  TO CIA-IDARTBET-IN                          
058600         CALL W009CIA USING CIA-W009CIA                                   
058700         IF CIA-KDSVAR = 'F'                                              
058800            MOVE NEJ TO NYCKLAR-SW                                        
058900            MOVE ZERO              TO WS-IDARTNR                          
058910            MOVE MFS-ROER-EJ-FAELT TO MOD-BELEVART-IN                     
059000         ELSE                                                             
059100            MOVE CIA-IDARTNR  TO WS-IDARTNR                               
059200         END-IF                                                           
059300         MOVE '7'             TO MFS-IDPFK                                
059400         MOVE SPACE           TO MFS-KDTRTYP                              
059500       END-IF                                                             
059600       INSPECT WS-IDARTNR REPLACING LEADING SPACE BY ZERO                 
059700                                                                          
059800       IF WS-IDARTNR NUMERIC AND WS-IDARTNR > ZERO                        
059900         MOVE WS-IDARTNR        TO MSGI-IDARTNR                           
060000       END-IF                                                             
060100                                                                          
060200       CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                         
060300                                                                          
060400       IF MID-IDSKYLT-IN = ALL '+'                                        
060500         MOVE MID-IDSKYLT-UT TO WS-IDSKYLT                                
060600       ELSE                                                               
060700         MOVE MID-IDSKYLT-IN TO WS-IDSKYLT                                
060800         MOVE '7'            TO MFS-IDPFK                                 
060900         MOVE SPACE          TO MFS-KDTRTYP                               
061000       END-IF                                                             
061100                                                                          
061200       IF MID-1002-SATS-IN = ALL '+'                                      
061300         MOVE MID-1002-SATS-UT TO WS-1002-SATS                            
061400       ELSE                                                               
061500         MOVE MID-1002-SATS-IN TO WS-1002-SATS                            
061600         MOVE '7'              TO MFS-IDPFK                               
061700         MOVE SPACE            TO MFS-KDTRTYP                             
061800       END-IF                                                             
061900                                                                          
062000       IF MID-KDPRODSL-IN = ALL '+'                                       
062100         MOVE MID-KDPRODSL-UT TO WS-KDPRODSL                              
062200         INSPECT WS-KDPRODSL REPLACING LEADING SPACE BY ZERO              
062300       ELSE                                                               
062400         MOVE MID-KDPRODSL-IN TO WS-KDPRODSL                              
062500         MOVE '7'             TO MFS-IDPFK                                
062600         MOVE SPACE           TO MFS-KDTRTYP                              
062700       END-IF                                                             
062800                                                                          
062900     ELSE                                                                 
063000        MOVE ZERO TO   WS-KDPRODSL                                        
063100        MOVE SPACE TO  WS-BELEVART                                        
063200                       WS-IDLEVNR                                         
063300                       WS-1002-SATS                                       
063400        MOVE ALL '+' TO MSGI-WMSGINIT                                     
063500        MOVE '001'             TO MSGI-KDCALL                             
063600        MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                             
063700        MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                       
063800        MOVE '1221'            TO MSGI-IDTRANS                            
063900        IF (MID-IDARTNR-IN NUMERIC                                        
064000        AND MID-IDARTNR-IN > ZERO)                                        
064100            MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                           
064200        END-IF                                                            
064300        CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                        
064400        MOVE MSGI-IDARTNR TO WS-IDARTNR                                   
064500        INSPECT WS-IDARTNR REPLACING ALL SPACE BY ZERO                    
064600     END-IF                                                               
064700                                                                          
064800     IF MSGI-IDLAND-SPR = 'GB'                                            
064900       MOVE +2    TO SPRAK-IX                                             
065000       MOVE 'GB ' TO MED-IDSKYLT                                          
065100     ELSE                                                                 
065200       MOVE +1    TO SPRAK-IX                                             
065300       MOVE 'S  ' TO MED-IDSKYLT                                          
065400     END-IF                                                               
065500                                                                          
065600     IF WS-1002-SATS = 'Y' OR 'J' OR 'N' OR ' '                           
065700       CONTINUE                                                           
065800     ELSE                                                                 
065900       MOVE NEJ TO NYCKLAR-SW                                             
066000     END-IF                                                               
066100                                                                          
066200     IF WS-KDPRODSL NUMERIC                                               
066300       IF WS-KDPRODSL = ZERO                                              
066400         CONTINUE                                                         
066500       ELSE                                                               
066600         MOVE WS-KDPRODSL TO TEST-KDPRODSL                                
066700         IF KDPRODSL-VOLVO-BIMA                                           
066800           CONTINUE                                                       
066900         ELSE                                                             
067000           MOVE NEJ TO NYCKLAR-SW                                         
067100         END-IF                                                           
067200       END-IF                                                             
067300     ELSE                                                                 
067400       MOVE NEJ TO NYCKLAR-SW                                             
067500     END-IF                                                               
067600                                                                          
067700     IF WS-IDSKYLT = SPACE                                                
067800       IF SPRAK-IX = 1                                                    
067900         MOVE 'S' TO WS-IDSKYLT                                           
068000       ELSE                                                               
068100         MOVE 'GB' TO WS-IDSKYLT                                          
068200       END-IF                                                             
068300     END-IF                                                               
068400                                                                          
068500     SET WWLAND03-IX TO +1                                                
068600     SEARCH WWLAND03-IDSKYLT-RAD                                          
068700       AT END                                                             
068800         MOVE NEJ TO NYCKLAR-SW                                           
068900       WHEN WWLAND03-IDSKYLT(WWLAND03-IX) = WS-IDSKYLT                    
069000         CONTINUE                                                         
069100     END-SEARCH                                                           
069200                                                                          
069300     IF WS-IDLEVNR(1:1) NOT = '0' AND '+'                                 
069400                                                                          
069500****   OM IDLEVNR > ' ' ÄR SÖKNYCKELN IDLEVNR TILLS. MED BELEVART         
069600       IF WS-IDLEVNR NOT = SPACE                                          
069700         MOVE IDLEVNR-O-BELEVART TO SOEKNYCKELTYP-SW                      
069800         MOVE WS-IDLEVNR         TO W-IDLEVNR                             
069900         MOVE WS-BELEVART        TO W-BELEVART                            
070000         MOVE ZERO               TO W-IDARTNR                             
070100                                                                          
070200         MOVE ZERO               TO WS-IDARTNR                            
070300       ELSE                                                               
070400         IF WS-IDLEVNR = SPACE                                            
070500******   OM IDLEVNR = TOMT, ÄR SÖKNYCKELN IDARTNR                         
070600           MOVE IDARTNR TO SOEKNYCKELTYP-SW                               
070700           IF (WS-IDARTNR NUMERIC) AND                                    
070800               (WS-IDARTNR > ZERO) AND                                    
070900               (WS-IDARTNR < WS-MIN-IDARTNR-KONV)                         
071000             MOVE WS-IDARTNR TO W-IDARTNR                                 
071100             MOVE SPACE      TO W-IDLEVNR                                 
071200                                WS-IDLEVNR                                
071300                                W-BELEVART                                
071400                                WS-BELEVART                               
071500           ELSE                                                           
071600             MOVE NEJ TO NYCKLAR-SW                                       
071700           END-IF                                                         
071800         ELSE                                                             
071900           MOVE NEJ TO NYCKLAR-SW                                         
072000         END-IF                                                           
072100       END-IF                                                             
072200     ELSE                                                                 
072300       MOVE NEJ                TO NYCKLAR-SW                              
072400       MOVE IDLEVNR-O-BELEVART TO SOEKNYCKELTYP-SW                        
072500     END-IF                                                               
072600                                                                          
072700     IF GODK-MID OR NYCKLAR-OK                                            
072800       MOVE WS-IDLEVNR TO MOD-IDLEVNR-UT                                  
072900       IF SOEKNYCKEL-IDARTNR                                              
073000         MOVE WS-IDARTNR TO MOD-IDARTNR-UT                                
073100                            MOD-IDARTNR-SPAR                              
073200         INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE           
073300       INSPECT MOD-IDARTNR-SPAR REPLACING LEADING ZERO BY SPACE           
073400       ELSE                                                               
073500         MOVE WS-BELEVART TO MOD-BELEVART-UT                              
073600       END-IF                                                             
073700       MOVE WS-IDSKYLT   TO MOD-IDSKYLT-UT                                
073800       MOVE WS-1002-SATS TO MOD-1002-SATS-UT                              
073900       MOVE WS-KDPRODSL  TO MOD-KDPRODSL-UT                               
074000       INSPECT MOD-KDPRODSL-UT REPLACING LEADING ZERO BY SPACE            
074100                                                                          
074200       PERFORM BA-KOLLA-OM-SOEKNYCKEL-LAAST                               
074300       IF SOEKNYCKEL-LAAST-AV-ANNAN-USER                                  
074400         CONTINUE                                                         
074500       ELSE                                                               
074600         IF SOEKNYCKEL-IDARTNR                                            
074700           PERFORM BB-KOLLA-OM-IDARTNR-ALT-ERSATT                         
074800         END-IF                                                           
074900       END-IF                                                             
075000     ELSE                                                                 
075100       IF NOT GODK-MID                                                    
075200          MOVE MFS-RENSA-FAELT TO MOD-IDLEVNR-UT                          
075300                                  MOD-BELEVART-UT                         
075400                                  MOD-IDARTNR-SPAR                        
075500                                  MOD-IDSKYLT-UT                          
075600                                  MOD-1002-SATS-UT                        
075700                                  MOD-KDPRODSL-UT                         
075800       END-IF                                                             
075900     END-IF                                                               
076000                                                                          
076100     IF NYCKLAR-FEL                                                       
076200       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
076300       CALL WMEDKONV USING MED-WMEDAREA                                   
076400       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
076500       PERFORM MFS-RENSA-FAELT-IN                                         
076600       PERFORM MFS-RENSA-FAELT-UT                                         
076700     END-IF                                                               
076800     .                                                                    
076900     SKIP3                                                                
077000 BA-KOLLA-OM-SOEKNYCKEL-LAAST SECTION.                                    
077100**************************************************                        
077200* OM LÅSNING AV SÖKNYCKEL FINNS OCH DEN ÄR ÄLDRE *                        
077300* ÄN 2 DAGAR TAS LÅSNINGEN BORT. ÄR SÖKNYCKELN   *                        
077400* LÅSTA AV ANNAN ANVÄNDARE SÄTTS EN FLAGGA FÖR   *                        
077500* ATT HINDRA UPPDATERING. OM LÅSNING FINNS UNDER *                        
077600* EGET USERID SPARAS SPÄRWDATUM FÖR ATT ANVÄNDAS *                        
077700* SOM TIREGDAT VID KONV. AV STRUKTUR.  OM TILL-  *                        
077800* KOMMANDE ARTIKLAR FINNS REGISTRERADE (FRÅN     *                        
077900* BILD 1222) SÄTTS EN FLAGGA FÖR ATT HINDRA UPPD.*                        
078000**************************************************                        
078100                                                                          
078200                                                                          
078300     MOVE NEJ       TO SOEKNYCKEL-LAAST-AV-ANNAN-SW                       
078400                       SOEKNYCKEL-LAAST-AV-EGET-ID-SW                     
078500                       TILLKOMMANDE-ARTIKLAR-FINNS-SW                     
078600                                                                          
078700     MOVE '1151'    TO W-IDHTYP                                           
078800     MOVE LOW-VALUE TO W-LOW-VALUE-2                                      
078900     PERFORM IMS-GET-XXAZ-XXAZ01                                          
079000                                                                          
079100     MOVE LOW-VALUE         TO W-LOW-VALUE                                
079200     IF SOEKNYCKEL-IDARTNR                                                
079300       PERFORM IMS-GET-XXAZ-XXAZ11-IDARTNR                                
079400     ELSE                                                                 
079500       PERFORM IMS-GET-XXAZ-XXAZ11-IDLEV-BELE                             
079600     END-IF                                                               
079700                                                                          
079800     IF SEGMENT-FINNS                                                     
079900       MOVE 001                  TO WORK-KDCALL                           
080000       MOVE WC-CDC-SE            TO WORK-IDDC                             
080100       MOVE XXAZ11-1152-TIREGDAT TO WORK-TIAAMMDD-FOM                     
080200       MOVE DAGENS-DATUM         TO WORK-TIAAMMDD-TOM                     
080300       CALL WORKDAY USING WORK-KDCALL                                     
080400                          WORK-DATE-AREA                                  
080500                          WORK-KDSVAR                                     
080600       IF (WORK-KVWORKD > 2) OR (WORK-KDSVAR-FEL)                         
080700*******  ARB-KDSVAR-FEL INNEBÄR ATT ANTALET DAGAR > 999.                  
080800         PERFORM IMS-DLET-XXAZ-XXAZ11                                     
080900       ELSE                                                               
081000         IF XXAZ11-1152-IDUSER = MSG-SIGNON-USERID                        
081100           MOVE JA TO SOEKNYCKEL-LAAST-AV-EGET-ID-SW                      
081200           MOVE XXAZ11-1152-TIREGDAT TO WS-SPAR-SPAERWDATUM               
081300           MOVE WS-IDARTNR           TO W-IDARTNR                         
081400           MOVE WS-IDLEVNR           TO W-IDLEVNR                         
081500           MOVE WS-BELEVART          TO W-BELEVART                        
081600           MOVE MSG-SIGNON-USERID    TO W-IDUSER                          
081700           PERFORM IMS-GET-XXAZ-XXAZ21                                    
081800           IF SEGMENT-FINNS                                               
081900             MOVE JA TO TILLKOMMANDE-ARTIKLAR-FINNS-SW                    
082000           END-IF                                                         
082100         ELSE                                                             
082200           MOVE JA TO SOEKNYCKEL-LAAST-AV-ANNAN-SW                        
082300         END-IF                                                           
082400       END-IF                                                             
082500     END-IF                                                               
082600     .                                                                    
082700     SKIP3                                                                
082800 BB-KOLLA-OM-IDARTNR-ALT-ERSATT SECTION.                                  
082900                                                                          
083000     MOVE NEJ TO IDARTNR-ALT-ERS-PAA-ARTREG-SW                            
083100                                                                          
083200     MOVE WS-IDARTNR TO W-IDARTNR-2                                       
083300     PERFORM IMS-GET-ARTC-ARTC01                                          
083400     IF SEGMENT-FINNS                                                     
083500       PERFORM IMS-GET-ARTC-ARTC11                                        
083600       IF SEGMENT-FINNS                                                   
083700         IF CLAG-KDERS = 04 OR 05 OR 06                                   
083800           MOVE JA TO IDARTNR-ALT-ERS-PAA-ARTREG-SW                       
083900         END-IF                                                           
084000       END-IF                                                             
084100     END-IF                                                               
084200                                                                          
084300     .                                                                    
084400     EJECT                                                                
084500 C-FOERSTA-SIDA SECTION.                                                  
084600                                                                          
084700*    --- BLANKA/NOLLA UT BLÄDDRINGSNYCKEL                                 
084800     MOVE JA   TO ALLT-SW                                                 
084900     .                                                                    
085000     EJECT                                                                
085100 D-NAESTA-SIDA SECTION.                                                   
085200                                                                          
085300     MOVE JA TO ALLT-SW                                                   
085400     .                                                                    
085500     EJECT                                                                
085600 E-SAMMA-SIDA SECTION.                                                    
085700                                                                          
085800     PERFORM S05-KOLLA-OM-MID-INPUT-IFYLLD                                
085900     IF MID-INPUT-EJ-IFYLLD                                               
086000       MOVE JA TO ALLT-SW                                                 
086100     ELSE                                                                 
086200       MOVE NEJ TO ALLT-SW                                                
086300       MOVE INF-PRESS-PF11 TO MED-IDMFSINF                                
086400       CALL WMEDKONV USING MED-WMEDAREA                                   
086500       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
086600       PERFORM MFS-ROR-EJ-FAELT-IN                                        
086700       PERFORM MFS-ROR-EJ-FAELT-UT                                        
086800       PERFORM MFS-LAS-IN-IGEN                                            
086900     END-IF                                                               
087000     .                                                                    
087100     EJECT                                                                
087200 F-LAES-VISA-INFO SECTION.                                                
087300                                                                          
087400     MOVE +1  TO INDX                                                     
087500     PERFORM S06-LAES-RADDATA-FOERSTA                                     
087600     IF RAD-OK                                                            
087700       PERFORM FA-LAEGG-UT-ARTIKEL-INFO                                   
087800       MOVE SATB01C-STR-IDARTNR  TO MOD-IDARTNR-ENTER                     
087900       MOVE SATB11C-RAD-KDSTRRAD TO MOD-KDSTRRAD-ENTER                    
088000       MOVE SATB11C-RAD-IDRADNR  TO MOD-IDRADNR-ENTER                     
088100     ELSE                                                                 
088200       MOVE ZERO  TO MOD-IDARTNR-ENTER                                    
088300                     MOD-IDRADNR-ENTER                                    
088400       MOVE SPACE TO MOD-KDSTRRAD-ENTER                                   
088500       MOVE FEL2 (SPRAK-IX) TO MOD-TEMFSFEL                               
088600     END-IF                                                               
088700                                                                          
088800     PERFORM UNTIL INDX > MAX-INDX                                        
088900       IF RAD-OK                                                          
089000         PERFORM FB-REDIGERA-UTRAD                                        
089100         PERFORM S07-LAES-RADDATA-NAESTA                                  
089200       ELSE                                                               
089300         MOVE MFS-RENSA-FAELT    TO MOD-SELECT(INDX)                      
089400         MOVE MFS-FORMATETS-ATTR TO MOD-SELECT-ATTR(INDX)                 
089500         PERFORM MFS-RENSA-RAD-FAELT-UT                                   
089600       END-IF                                                             
089700       ADD 1 TO INDX                                                      
089800     END-PERFORM                                                          
089900                                                                          
090000     IF RAD-OK                                                            
090100       MOVE SATB01C-STR-IDARTNR  TO MOD-IDARTNR-NEXT                      
090200       MOVE SATB11C-RAD-KDSTRRAD TO MOD-KDSTRRAD-NEXT                     
090300       MOVE SATB11C-RAD-IDRADNR  TO MOD-IDRADNR-NEXT                      
090400       MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                          
090500       CALL WMEDKONV USING MED-WMEDAREA                                   
090600       MOVE MED-TEMFSINF TO MOD-TEMFSINF                                  
090700     ELSE                                                                 
090800       MOVE ZERO  TO MOD-IDARTNR-NEXT                                     
090900                     MOD-IDRADNR-NEXT                                     
091000       MOVE SPACE TO MOD-KDSTRRAD-NEXT                                    
091100       IF MFS-IDPFK NOT = '7'                                             
091200         MOVE INF-LAST-PAGE  TO MED-IDMFSINF                              
091300         CALL WMEDKONV USING MED-WMEDAREA                                 
091400         MOVE MED-TEMFSINF TO MOD-TEMFSINF                                
091500       END-IF                                                             
091600     END-IF                                                               
091700                                                                          
091800     MOVE MFS-ROER-EJ-FAELT TO MOD-KDPRTVAL                               
091900     IF MOD-TEMFSFEL = SPACE                                              
092000       IF IDARTNR-ALT-ERS-PAA-ARTREG                                      
092100********* OM FELTEXT EJ IFYLLD VISA ATT                                   
092200********* ARTIKEL ÄR ALTERNATIVT ERSATT                                   
092300         MOVE FEL3 (SPRAK-IX) TO MOD-TEMFSFEL                             
092400       END-IF                                                             
092500     END-IF                                                               
092600     .                                                                    
092700     SKIP3                                                                
092800 FA-LAEGG-UT-ARTIKEL-INFO SECTION.                                        
092900**************************************************                        
093000* HÄR LÄGGS INFORMATION OM DEN SÖKTA ARTIKELN UT *                        
093100**************************************************                        
093200                                                                          
093300     MOVE WS-IDARTNR TO W-IDARTNR-2                                       
093400     PERFORM IMS-GET-ARTC-ARTC01                                          
093500     IF SEGMENT-FINNS                                                     
093600       MOVE 3 TO W-KDNOTTYP                                               
093700       PERFORM IMS-GET-ARTC-ARTC25                                        
093800       IF SEGMENT-FINNS                                                   
093900         MOVE NOT-TEARTNOT TO MOD-TEARTNOT                                
094000       ELSE                                                               
094100         MOVE MFS-RENSA-FAELT TO MOD-TEARTNOT                             
094200       END-IF                                                             
094300                                                                          
094400       MOVE 7 TO W-KDNOTTYP                                               
094500       PERFORM IMS-GET-ARTC-ARTC25                                        
094600       IF SEGMENT-FINNS                                                   
094700         MOVE NOT-TEARTNOT TO MOD-TEARTNOT-7                              
094800       ELSE                                                               
094900         MOVE MFS-RENSA-FAELT TO MOD-TEARTNOT-7                           
095000       END-IF                                                             
095100                                                                          
095200       PERFORM S01-HAEMTA-BEART-BSEQ                                      
095300       MOVE WS-BEART TO MOD-BEART-UT                                      
095400     ELSE                                                                 
095500       IF WS-IDSKYLT = 'S  '                                              
095600         MOVE SATB11C-RAD-BEART-SVE TO MOD-BEART-UT                       
095700       ELSE                                                               
095800         MOVE SATB11C-RAD-BEART-SVE TO W-BEART                            
095900         MOVE SATB11C-RAD-KDBENHOM  TO WS-KDBENHOM                        
096000         PERFORM S02-HAEMTA-BEART-ASEQ                                    
096100         MOVE WS-BEART TO MOD-BEART-UT                                    
096200       END-IF                                                             
096300                                                                          
096400       MOVE MFS-RENSA-FAELT TO MOD-TEARTNOT                               
096500                               MOD-TEARTNOT-7                             
096600                                                                          
096700     END-IF                                                               
096800     .                                                                    
096900     SKIP3                                                                
097000 FB-REDIGERA-UTRAD SECTION.                                               
097100                                                                          
097200     MOVE SATB01C-STR-IDARTNR  TO MOD-IDARTNR (INDX)                      
097300     MOVE SATB01C-STR-IDSTRTYP TO MOD-IDSTRTYP(INDX)                      
097400     MOVE SATB11C-RAD-IDRADNR  TO MOD-IDRADNR (INDX)                      
097500     MOVE SATB11C-RAD-REANTPSA TO MOD-REANTPSA(INDX)                      
097600                                                                          
097700     MOVE SATB01C-STR-IDARTNR TO W-IDARTNR-2                              
097800     PERFORM IMS-GET-ARTC-ARTC01                                          
097900     IF SEGMENT-FINNS                                                     
098000****** OM DEN HITTADE STRUKTUREN FINNS PÅ ARTREG                          
098100       MOVE ART-IDLEVNR   TO MOD-IDLEVNR(INDX)                            
098200       MOVE ART-KDERS-UTG TO MOD-KDERS  (INDX)                            
098300       PERFORM IMS-GET-ARTC-ARTC11                                        
098400       IF SEGMENT-FINNS                                                   
098500         MOVE CLAG-KDERS    TO MOD-KDERS  (INDX)                          
098600         MOVE CLAG-KDPSLLOC TO MOD-KDPSLLOC  (INDX)                       
098700       END-IF                                                             
098800       IF STRUKTURNR-FINNS-PAA-WDK6                                       
098900******   ART-KDPRODSL LIGGER I IO-AREA-3 EFTER LÄSNING I S04              
099000         MOVE ARTC012-ART-KDPRODSL TO MOD-KDPRODSL(INDX)                  
099100       ELSE                                                               
099200         MOVE MFS-RENSA-FAELT TO MOD-KDPRODSL(INDX)                       
099300       END-IF                                                             
099400       PERFORM S01-HAEMTA-BEART-BSEQ                                      
099500       MOVE WS-BEART TO MOD-BEART(INDX)                                   
099600     ELSE                                                                 
099700       MOVE SATB01C-STR-KDPRODSL TO MOD-KDPRODSL(INDX)                    
099800       MOVE MFS-RENSA-FAELT      TO MOD-KDERS(INDX)                       
099900                                    MOD-IDLEVNR(INDX)                     
100000       IF WS-IDSKYLT = 'S  '                                              
100100         MOVE SATB01C-STR-BEART-SVE TO MOD-BEART(INDX)                    
100200       ELSE                                                               
100300         MOVE SATB01C-STR-BEART-SVE TO W-BEART                            
100400         MOVE SATB01C-STR-KDBENHOM  TO WS-KDBENHOM                        
100500         PERFORM S02-HAEMTA-BEART-ASEQ                                    
100600         MOVE WS-BEART TO MOD-BEART(INDX)                                 
100700       END-IF                                                             
100800     END-IF                                                               
100900                                                                          
101000     COMPUTE WS-IDARTNR-KONVERTERAT = 999999999 -                         
101100                                      SATB01C-STR-IDARTNR                 
101200     MOVE WS-IDARTNR-KONVERTERAT TO W-IDARTNR-2                           
101300     PERFORM IMS-GET-SATB-SATB01                                          
101400     IF SEGMENT-FINNS                                                     
101500       MOVE 001                 TO WORK-KDCALL                            
101600       MOVE WC-CDC-SE           TO WORK-IDDC                              
101700       MOVE SATB01-STR-TIREGDAT TO WORK-TIAAMMDD-FOM                      
101800       MOVE DAGENS-DATUM        TO WORK-TIAAMMDD-TOM                      
101900       CALL WORKDAY USING WORK-KDCALL                                     
102000                          WORK-DATE-AREA                                  
102100                          WORK-KDSVAR                                     
102200       IF (WORK-KVWORKD > 2) OR (WORK-KDSVAR-FEL)                         
102300*******  OM MAN TRÄFFAR PÅ EN KONVERTERAD STRUKTUR SOM                    
102400*******  ÄR ÄLDRE ÄN 2 DAGAR TAS DEN BORT.                                
102500*******  ARB-KDSVAR-FEL INNEBÄR ATT ANTALET DAGAR > 999.                  
102600         PERFORM IMS-DLET-SATB                                            
102700       ELSE                                                               
102800         IF (SATB01-STR-IDUSER = MSG-SIGNON-USERID) AND                   
102900             (SOEKNYCKEL-LAAST-AV-EGET-ID)                                
103000*********  OM DEN KONVERTERADE STRUKTUREN KONVERTERATS                    
103100*********  AV SIGNON-USERID OCH 'UNDER' DENNA SÖKNYCKEL                   
103200                                                                          
103300           MOVE SPACE               TO W-KDSTRRAD                         
103400*********  RADERNA LIGGER MED KDSTRRAD = ' ' I VALDA STRUKTUR             
103500           MOVE SATB11C-RAD-IDRADNR TO W-IDRADNR                          
103600           PERFORM IMS-GET-SATB-SATB11                                    
103700           IF SEGMENT-FINNS                                               
103800***********  RADEN UTVALD                                                 
103900             MOVE MFS-FORMATETS-ATTR TO MOD-SELECT-ATTR(INDX)             
104000             MOVE '*'                TO MOD-SELECT(INDX)                  
104100           ELSE                                                           
104200             MOVE MFS-FORMATETS-ATTR TO MOD-SELECT-ATTR(INDX)             
104300             MOVE MFS-RENSA-FAELT    TO MOD-SELECT(INDX)                  
104400           END-IF                                                         
104500                                                                          
104600         ELSE                                                             
104700           MOVE MFS-FORMATETS-ATTR TO MOD-SELECT-ATTR(INDX)               
104800           MOVE MFS-RENSA-FAELT    TO MOD-SELECT(INDX)                    
104900         END-IF                                                           
105000       END-IF                                                             
105100     ELSE                                                                 
105200       MOVE MFS-FORMATETS-ATTR TO MOD-SELECT-ATTR(INDX)                   
105300       MOVE MFS-RENSA-FAELT    TO MOD-SELECT(INDX)                        
105400     END-IF                                                               
105500     .                                                                    
105600     EJECT                                                                
105700                                                                          
105800 G-KOLLA-INPUT SECTION.                                                   
105900                                                                          
106000     MOVE JA TO INDATA-SW                                                 
106100     IF (SOEKNYCKEL-LAAST-AV-ANNAN-USER) OR                               
106200         (TILLKOMMANDE-ARTIKLAR-FINNS)                                    
106300       MOVE NEJ TO INDATA-SW                                              
106400       PERFORM MFS-ROR-EJ-FAELT-IN                                        
106500       PERFORM MFS-ROR-EJ-FAELT-UT                                        
106600       MOVE FEL1 (SPRAK-IX) TO MOD-TEMFSFEL                               
106700       IF SOEKNYCKEL-LAAST-AV-ANNAN-USER                                  
106800         MOVE MED1 (SPRAK-IX) TO MOD-TEMFSINF                             
106900       ELSE                                                               
107000         MOVE MED4 (SPRAK-IX) TO MOD-TEMFSINF                             
107100       END-IF                                                             
107200     ELSE                                                                 
107300       PERFORM S05-KOLLA-OM-MID-INPUT-IFYLLD                              
107400       IF MID-INPUT-EJ-IFYLLD                                             
107500         MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                        
107600         CALL WMEDKONV USING MED-WMEDAREA                                 
107700         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
107800         PERFORM MFS-ROR-EJ-FAELT-IN                                      
107900         PERFORM MFS-ROR-EJ-FAELT-UT                                      
108000         MOVE NEJ TO INDATA-SW                                            
108100       ELSE                                                               
108200         PERFORM GA-KOLLA-OM-UPPDAT-TILLATEN                              
108300         IF INDATA-OK                                                     
108400           PERFORM GB-FORMELL-INDATAKONTROLL                              
108500           IF INDATA-OK                                                   
108600             MOVE +1  TO INDX                                             
108700             MOVE NEJ TO TIDIGARE-VALD-RAD-FINNS-SW                       
108800             PERFORM UNTIL INDX > MAX-INDX                                
108900               IF MID-SELECT(INDX) NOT = ALL '+'                          
109000                 IF MID-SELECT(INDX) = 'S'                                
109100                   PERFORM GC-KOLLA-UPPL-KONV                             
109200                 ELSE                                                     
109300                   IF MID-SELECT(INDX) = 'B' OR 'D'                       
109400                     PERFORM GD-KOLLA-BORTTAG                             
109500                   ELSE                                                   
109600                     IF MID-SELECT(INDX) = '*'                            
109700                       MOVE MFS-ALFA-FAELT-RAETT TO                       
109800                              MOD-SELECT-ATTR(INDX)                       
109900                     ELSE                                                 
110000                       IF MID-SELECT(INDX) = ' '                          
110100                         MOVE MFS-RENSA-FAELT TO MOD-SELECT(INDX)         
110200                       END-IF                                             
110300                     END-IF                                               
110400                   END-IF                                                 
110500                 END-IF                                                   
110600               END-IF                                                     
110700               ADD 1 TO INDX                                              
110800             END-PERFORM                                                  
110900                                                                          
111000             IF INDATA-FEL                                                
111100               MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                  
111200               CALL WMEDKONV USING MED-WMEDAREA                           
111300               MOVE MED-MFSFEL TO MOD-TEMFSFEL                            
111400               PERFORM MFS-ROR-EJ-FAELT-UT                                
111500               PERFORM MFS-ROR-EJ-FAELT-IN                                
111600             END-IF                                                       
111700           ELSE                                                           
111800             MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                    
111900             CALL WMEDKONV USING MED-WMEDAREA                             
112000             MOVE MED-MFSFEL TO MOD-TEMFSFEL                              
112100             PERFORM MFS-ROR-EJ-FAELT-UT                                  
112200             PERFORM MFS-ROR-EJ-FAELT-IN                                  
112300           END-IF                                                         
112400         ELSE                                                             
112500           MOVE FEL1 (SPRAK-IX) TO MOD-TEMFSFEL                           
112600           PERFORM MFS-LAS-IN-IGEN                                        
112700           PERFORM MFS-ROR-EJ-FAELT-UT                                    
112800           PERFORM MFS-ROR-EJ-FAELT-IN                                    
112900         END-IF                                                           
113000       END-IF                                                             
113100                                                                          
113200     END-IF                                                               
113300     MOVE MFS-ROER-EJ-FAELT TO MOD-KDPRTVAL                               
113400     .                                                                    
113500     SKIP3                                                                
113600 GA-KOLLA-OM-UPPDAT-TILLATEN SECTION.                                     
113700****************************************************************          
113800* UPPDATERING ENDAST TILLÅTEN OM 'SIGNON-USERID' :             *          
113900* ¤ INTE HAR NÅGON GÄLLANDE KONVERTERAD STRUKTUR ALLS    *                
114000* ¤ HAR KONVERTERAD STRUKTUR/ER OCH SÖKT ARTIKEL (NYCKEL)      *          
114100*   FINNS REGISTRERAD PÅ WLXXAZ(WDR5,LÅSNING AV ART KOMB USER) *          
114200*   I 'GILTIG FORM' (EJ ÄLDRE ÄN 2 DAGAR).                     *          
114300*   DETTA INNEBÄR ATT DE KONVERTERADE STRUKTURERNA HAR KONVERT-*          
114400*   ERATS UNDER AKTUELL SÖKNYCKEL OCH USER-ID.                 *          
114500****************************************************************          
114600                                                                          
114700     MOVE MSG-SIGNON-USERID   TO W-IDUSER                                 
114800     MOVE WS-MIN-IDARTNR-KONV TO W-MIN-IDARTNR-KONV                       
114900     MOVE WS-MAX-IDARTNR-KONV TO W-MAX-IDARTNR-KONV                       
115000                                                                          
115100     PERFORM IMS-GET-SATB-DSEQ-UNIK                                       
115200     PERFORM UNTIL SEGMENT-SAKNAS                                         
115300       IF SEGMENT-FINNS                                                   
115400         MOVE 001                 TO WORK-KDCALL                          
115500         MOVE WC-CDC-SE           TO WORK-IDDC                            
115600         MOVE SATB01-STR-TIREGDAT TO WORK-TIAAMMDD-FOM                    
115700         MOVE DAGENS-DATUM        TO WORK-TIAAMMDD-TOM                    
115800         CALL WORKDAY USING WORK-KDCALL                                   
115900                            WORK-DATE-AREA                                
116000                            WORK-KDSVAR                                   
116100                                                                          
116200         IF (WORK-KVWORKD > 2) OR (WORK-KDSVAR-FEL)                       
116300*******  ARB-KDSVAR-FEL INNEBÄR ATT ANTALET DAGAR > 999.                  
116400           MOVE SATB01-STR-IDARTNR TO W-IDARTNR-2                         
116500           PERFORM IMS-GET-SATB-SATB01                                    
116600           PERFORM IMS-DLET-SATB                                          
116700                                                                          
116800           PERFORM IMS-GET-SATB-DSEQ-NEXT                                 
116900         ELSE                                                             
117000********** 'GÄLLANDE' KONVERTERAD STRUKTUR. MAN SÄTTER                    
117100********** STATUS TILL SEGMENT-SAKNAS FÖR ATT BRYTA                       
117200           MOVE 'GE' TO STATUS-WS                                         
117300           IF SOEKNYCKEL-LAAST-AV-EGET-ID                                 
117400********** STRUKTUREN KONV. 'UNDER' DENNA NYCKEL & USERID                 
117500             MOVE JA TO TIDIGARE-VALDA-STRUKT-FINNS-SW                    
117600             MOVE SATB01-STR-IDLEVNR TO WS-IDLEVNR-TIDIGARE-STRUKT        
117700             CONTINUE                                                     
117800           ELSE                                                           
117900             MOVE NEJ             TO INDATA-SW                            
118000             MOVE MED2 (SPRAK-IX) TO MOD-TEMFSINF                         
118100           END-IF                                                         
118200         END-IF                                                           
118300       END-IF                                                             
118400     END-PERFORM                                                          
118500     .                                                                    
118600     SKIP3                                                                
118700 GB-FORMELL-INDATAKONTROLL SECTION.                                       
118800******************************************                                
118900* KONTROLL ATT DEN VALDA RADEN ÄR IFYLLD *                                
119000* OCH ATT INPUT I SÅ FALL ÄR GODKÄND     *                                
119100******************************************                                
119200                                                                          
119300     MOVE +1 TO INDX                                                      
119400     PERFORM UNTIL INDX > MAX-INDX                                        
119500       IF (MID-SELECT(INDX) = ALL '+') OR                                 
119600          (MID-SELECT(INDX) = SPACE)                                      
119700         CONTINUE                                                         
119800       ELSE                                                               
119900         INSPECT MID-IDRADNR(INDX) REPLACING LEADING SPACE BY ZERO        
120000         INSPECT MID-IDARTNR(INDX) REPLACING LEADING SPACE BY ZERO        
120100                                                                          
120200         IF MID-IDRADNR(INDX) = ZERO AND MID-IDARTNR(INDX) = ZERO         
120300           MOVE NEJ                TO INDATA-SW                           
120400           MOVE MFS-ALFA-FAELT-FEL TO MOD-SELECT-ATTR(INDX)               
120500         ELSE                                                             
120600           IF (MID-SELECT(INDX) = 'S') OR                                 
120700               (MID-SELECT(INDX) = 'B' OR 'D') OR                         
120800                (MID-SELECT(INDX) = '*')                                  
120900             MOVE MFS-ALFA-FAELT-RAETT TO MOD-SELECT-ATTR(INDX)           
121000           ELSE                                                           
121100             MOVE NEJ                TO INDATA-SW                         
121200             MOVE MFS-ALFA-FAELT-FEL TO MOD-SELECT-ATTR(INDX)             
121300           END-IF                                                         
121400         END-IF                                                           
121500       END-IF                                                             
121600       ADD 1 TO INDX                                                      
121700     END-PERFORM                                                          
121800     .                                                                    
121900     SKIP3                                                                
122000 GC-KOLLA-UPPL-KONV SECTION.                                              
122100************************************************************              
122200* KONTROLL ATT DEN VALDA RADEN EJ ÄR U-MÄRKT, ATT DEN      *              
122300* VALDA RADEN (STRUKTUREN) EJ FINNS UNDER BEARBETNING AV   *              
122400* ANNAN ANVÄNDARE SAMT ATT OM ARTIKEL ALTERNATIVT ERSATT   *              
122500* SÅ FÅR MAN VÄLJA ANTINGEN ENBART 1002-SATSER ELLER ENBART*              
122600* ICKE-1002-SATSER . EVENTUELL UTGÅNGEN KONVERTERAD        *              
122700* STRUKTUR TOGS BORT I FC-RED ..  .                        *              
122800************************************************************              
122900                                                                          
123000     MOVE JA TO RADVAL-SW                                                 
123100                                                                          
123200     INSPECT MID-IDARTNR(INDX) REPLACING LEADING SPACE BY ZERO            
123300     MOVE MID-IDARTNR(INDX) TO WS-IDARTNR-NUM                             
123400                                                                          
123500     MOVE MID-IDRADNR(INDX) TO WS-IDRADNR                                 
123600     INSPECT WS-IDRADNR REPLACING LEADING SPACE BY ZERO                   
123700                                                                          
123800     MOVE WS-IDARTNR-NUM TO W-IDARTNR-2                                   
123900     PERFORM IMS-GET-SATB-SATB01                                          
124000     IF SEGMENT-FINNS                                                     
124100       MOVE '0'        TO W-KDSTRRAD                                      
124200       MOVE WS-IDRADNR TO W-IDRADNR                                       
124300       PERFORM IMS-GET-SATB-SATB11                                        
124400       IF SEGMENT-FINNS                                                   
124500         IF SATB11-RAD-KDISATS = 'U'                                      
124600           MOVE NEJ TO INDATA-SW                                          
124700                       RADVAL-SW                                          
124800         END-IF                                                           
124900       ELSE                                                               
125000         MOVE '9'        TO W-KDSTRRAD                                    
125100         MOVE WS-IDRADNR TO W-IDRADNR                                     
125200         PERFORM IMS-GET-SATB-SATB11                                      
125300         IF SATB11-RAD-KDISATS = 'U'                                      
125400           MOVE NEJ TO INDATA-SW                                          
125500                       RADVAL-SW                                          
125600         END-IF                                                           
125700       END-IF                                                             
125800                                                                          
125900       IF RADVAL-FEL                                                      
126000         MOVE MFS-ALFA-FAELT-FEL TO MOD-SELECT-ATTR(INDX)                 
126100         IF MOD-TEMFSINF = SPACE                                          
126200           MOVE MED7 (SPRAK-IX) TO MOD-TEMFSINF                           
126300         END-IF                                                           
126400       END-IF                                                             
126500     END-IF                                                               
126600                                                                          
126700     IF RADVAL-OK                                                         
126800       COMPUTE WS-IDARTNR-KONVERTERAT = 999999999 -                       
126900                                        WS-IDARTNR-NUM                    
127000       MOVE WS-IDARTNR-KONVERTERAT TO W-IDARTNR-2                         
127100       PERFORM IMS-GET-SATB-SATB01                                        
127200       IF SEGMENT-FINNS                                                   
127300         IF SATB01-STR-IDUSER = MSG-SIGNON-USERID                         
127400********   STRUKTUREN ÄR REDAN KONVERTERAD AV ANVÄNDAREN.                 
127500********   KOLLA OM RAD REDAN VALD                                        
127600                                                                          
127700           MOVE SPACE             TO W-KDSTRRAD                           
127800*********  RADERNA LIGGER MED KDSTRRAD = ' ' I VALDA STRUKTUR             
127900           MOVE MID-IDRADNR(INDX) TO WS-IDRADNR                           
128000           INSPECT WS-IDRADNR REPLACING LEADING SPACE BY ZERO             
128100           MOVE WS-IDRADNR TO W-IDRADNR                                   
128200           PERFORM IMS-GET-SATB-SATB11                                    
128300           IF SEGMENT-FINNS                                               
128400***********  RADEN REDAN UTVALD                                           
128500***********  SÄTTER MID-SELECT(INDX) TILL '*' (BEHANDLAS EJ).             
128600             MOVE '*' TO MID-SELECT(INDX)                                 
128700           END-IF                                                         
128800         ELSE                                                             
128900           MOVE NEJ                TO INDATA-SW                           
129000           MOVE MFS-ALFA-FAELT-FEL TO MOD-SELECT-ATTR(INDX)               
129100           IF MOD-TEMFSINF = SPACE                                        
129200             MOVE MED3 (SPRAK-IX) TO MOD-TEMFSINF                         
129300           END-IF                                                         
129400         END-IF                                                           
129500       ELSE                                                               
129600         IF IDARTNR-ALT-ERS-PAA-ARTREG                                    
129700           PERFORM GCA-KOLLA-ATT-IDLEVNR-RAETT                            
129800           IF INDATA-FEL                                                  
129900             IF MOD-TEMFSINF = SPACE                                      
130000               MOVE MED8 (SPRAK-IX) TO MOD-TEMFSINF                       
130100             END-IF                                                       
130200           END-IF                                                         
130300         END-IF                                                           
130400       END-IF                                                             
130500     END-IF                                                               
130600     .                                                                    
130700     SKIP3                                                                
130800 GCA-KOLLA-ATT-IDLEVNR-RAETT SECTION.                                     
130900************************************************************              
131000* NÄR ARTIKEL ALTERNATIVT ERSATT FÅR MAN VÄLJA ANTINGEN    *              
131100* BARA 1002-SATSER ELLER BARA ICKE-1002-SATSER.            *              
131200************************************************************              
131300                                                                          
131400     MOVE WS-IDARTNR-NUM TO W-IDARTNR-2                                   
131500     PERFORM IMS-GET-SATB-SATB01                                          
131600     IF SEGMENT-FINNS                                                     
131700       IF TIDIGARE-VALDA-STRUKT-FINNS                                     
131800********* JÄMFÖR MED TIDIGARE VALDA STRUKTURER                            
131900         IF WS-IDLEVNR-TIDIGARE-STRUKT = '1002 '                          
132000           IF SATB01-STR-IDLEVNR = '1002 '                                
132100             MOVE MFS-ALFA-FAELT-RAETT TO MOD-SELECT-ATTR(INDX)           
132200           ELSE                                                           
132300             MOVE NEJ                TO INDATA-SW                         
132400             MOVE MFS-ALFA-FAELT-FEL TO MOD-SELECT-ATTR(INDX)             
132500           END-IF                                                         
132600         ELSE                                                             
132700           IF SATB01-STR-IDLEVNR NOT = '1002 '                            
132800             MOVE MFS-ALFA-FAELT-RAETT TO MOD-SELECT-ATTR(INDX)           
132900           ELSE                                                           
133000             MOVE NEJ                TO INDATA-SW                         
133100             MOVE MFS-ALFA-FAELT-FEL TO MOD-SELECT-ATTR(INDX)             
133200           END-IF                                                         
133300         END-IF                                                           
133400       ELSE                                                               
133500********* JÄMFÖR MED TIDIGARE VALD RAD                                    
133600         IF TIDIGARE-VALD-RAD-FINNS                                       
133700           IF WS-IDLEVNR-TIDIGARE-RADVAL = '1002 '                        
133800             IF SATB01-STR-IDLEVNR = '1002 '                              
133900               MOVE MFS-ALFA-FAELT-RAETT TO MOD-SELECT-ATTR(INDX)         
134000             ELSE                                                         
134100               MOVE NEJ                TO INDATA-SW                       
134200               MOVE MFS-ALFA-FAELT-FEL TO MOD-SELECT-ATTR(INDX)           
134300             END-IF                                                       
134400           ELSE                                                           
134500             IF SATB01-STR-IDLEVNR NOT = '1002 '                          
134600               MOVE MFS-ALFA-FAELT-RAETT TO MOD-SELECT-ATTR(INDX)         
134700             ELSE                                                         
134800               MOVE NEJ                TO INDATA-SW                       
134900               MOVE MFS-ALFA-FAELT-FEL TO MOD-SELECT-ATTR(INDX)           
135000             END-IF                                                       
135100           END-IF                                                         
135200         ELSE                                                             
135300           MOVE JA                 TO TIDIGARE-VALD-RAD-FINNS-SW          
135400           MOVE SATB01-STR-IDLEVNR TO WS-IDLEVNR-TIDIGARE-RADVAL          
135500         END-IF                                                           
135600       END-IF                                                             
135700     END-IF                                                               
135800     .                                                                    
135900     SKIP3                                                                
136000 GD-KOLLA-BORTTAG SECTION.                                                
136100************************************************************              
136200* BORTTTAG FÅR ENDAST SKE AV VALD STRUKTURRAD DÄR KONVERT- *              
136300* AD STRUKTUR FINNS UNDER EGET USER-ID OCH STRUKTURRADEN   *              
136400* FINNS UPPLAGD (VALD TIDIGARE)                            *              
136500************************************************************              
136600                                                                          
136700     MOVE JA TO RADVAL-SW                                                 
136800                                                                          
136900     MOVE MID-IDARTNR(INDX) TO WS-IDARTNR-2                               
137000     INSPECT WS-IDARTNR-2 REPLACING LEADING SPACE BY ZERO                 
137100     MOVE WS-IDARTNR-2 TO WS-IDARTNR-NUM                                  
137200     COMPUTE WS-IDARTNR-KONVERTERAT = 999999999 -                         
137300                                      WS-IDARTNR-NUM                      
137400     MOVE WS-IDARTNR-KONVERTERAT TO W-IDARTNR-2                           
137500     PERFORM IMS-GET-SATB-SATB01                                          
137600     IF SEGMENT-FINNS                                                     
137700       IF MSG-SIGNON-USERID = SATB01-STR-IDUSER                           
137800         MOVE SPACE           TO W-KDSTRRAD                               
137900******   RADERNA LIGGER MED KDSTRRAD = ' ' I VALDA STRUKTUR               
138000         MOVE MID-IDRADNR(INDX) TO WS-IDRADNR                             
138100         INSPECT WS-IDRADNR REPLACING LEADING SPACE BY ZERO               
138200         MOVE WS-IDRADNR TO W-IDRADNR                                     
138300         PERFORM IMS-GET-SATB-SATB11                                      
138400         IF SEGMENT-FINNS                                                 
138500           CONTINUE                                                       
138600         ELSE                                                             
138700           MOVE NEJ TO INDATA-SW                                          
138800                       RADVAL-SW                                          
138900         END-IF                                                           
139000       ELSE                                                               
139100         MOVE NEJ TO INDATA-SW                                            
139200                     RADVAL-SW                                            
139300       END-IF                                                             
139400     ELSE                                                                 
139500       MOVE NEJ TO INDATA-SW                                              
139600                   RADVAL-SW                                              
139700     END-IF                                                               
139800                                                                          
139900     IF RADVAL-FEL                                                        
140000       MOVE MFS-ALFA-FAELT-FEL TO MOD-SELECT-ATTR(INDX)                   
140100       IF MOD-TEMFSINF = SPACE                                            
140200         MOVE MED5 (SPRAK-IX) TO MOD-TEMFSINF                             
140300       END-IF                                                             
140400     END-IF                                                               
140500     .                                                                    
140600     EJECT                                                                
140700 H-UPPDATERA SECTION.                                                     
140800                                                                          
140900     MOVE +1 TO INDX                                                      
141000     PERFORM UNTIL INDX > MAX-INDX                                        
141100       IF MID-SELECT(INDX) = 'S'                                          
141200         PERFORM HA-LAEGG-UPP-STRUKTUR-RAD-KONV                           
141300         MOVE MFS-FORMATETS-ATTR TO MOD-SELECT-ATTR(INDX)                 
141400         MOVE '*'                TO MOD-SELECT(INDX)                      
141500       ELSE                                                               
141600         IF MID-SELECT(INDX) = 'B' OR 'D'                                 
141700           PERFORM HB-TABORT-STRUKTUR-RAD                                 
141800           MOVE MFS-FORMATETS-ATTR TO MOD-SELECT-ATTR(INDX)               
141900           MOVE MFS-RENSA-FAELT    TO MOD-SELECT(INDX)                    
142000         ELSE                                                             
142100           IF MID-SELECT(INDX) = '*'                                      
142200             MOVE MFS-FORMATETS-ATTR TO MOD-SELECT-ATTR(INDX)             
142300             MOVE '*'                TO MOD-SELECT(INDX)                  
142400           ELSE                                                           
142500             MOVE MFS-ROER-EJ-FAELT TO MOD-SELECT(INDX)                   
142600           END-IF                                                         
142700         END-IF                                                           
142800       END-IF                                                             
142900       ADD 1 TO INDX                                                      
143000     END-PERFORM                                                          
143100                                                                          
143200     PERFORM MFS-ROR-EJ-FAELT-UT                                          
143300                                                                          
143400     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
143500     CALL WMEDKONV USING MED-WMEDAREA                                     
143600     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
143700* * * MFS-ROR-EJ-FAELT TILL FASTA VÄRDEN                                  
143800     .                                                                    
143900     EJECT                                                                
144000 HA-LAEGG-UPP-STRUKTUR-RAD-KONV SECTION.                                  
144100                                                                          
144200     IF SOEKNYCKEL-LAAST-AV-EGET-ID                                       
144300       CONTINUE                                                           
144400     ELSE                                                                 
144500       PERFORM HAA-LAGG-UPP-ARTIKEL-PA-WLXXAZ                             
144600     END-IF                                                               
144700                                                                          
144800     MOVE MID-IDARTNR(INDX) TO WS-IDARTNR-2                               
144900     INSPECT WS-IDARTNR-2 REPLACING LEADING SPACE BY ZERO                 
145000     MOVE WS-IDARTNR-2 TO WS-IDARTNR-NUM                                  
145100     COMPUTE WS-IDARTNR-KONVERTERAT = 999999999 -                         
145200                                      WS-IDARTNR-NUM                      
145300                                                                          
145400*********** LÄGG UPP STRUKTUR I KONVERTERAD FORM                          
145500                                                                          
145600     MOVE WS-IDARTNR-KONVERTERAT TO W-IDARTNR-2                           
145700     PERFORM IMS-GET-SATB-SATB01                                          
145800     IF SEGMENT-FINNS                                                     
145900       CONTINUE                                                           
146000     ELSE                                                                 
146100       MOVE WS-IDARTNR-2 TO W-IDARTNR-2                                   
146200       PERFORM IMS-GET-SATB-SATB01                                        
146300                                                                          
146400       MOVE WS-IDARTNR-KONVERTERAT TO SATB01-STR-IDARTNR                  
146500       MOVE MSG-SIGNON-USERID      TO SATB01-STR-IDUSER                   
146600       MOVE WS-SPAR-SPAERWDATUM    TO SATB01-STR-TIREGDAT                 
146700       PERFORM IMS-ISRT-SATB-SATB01                                       
146800     END-IF                                                               
146900                                                                          
147000*********** KOPIERA BERÖRD STRUKTURRAD                                    
147100                                                                          
147200     MOVE WS-IDARTNR-2 TO W-IDARTNR-2                                     
147300     PERFORM IMS-GET-SATB-SATB01                                          
147400                                                                          
147500     MOVE MID-IDRADNR(INDX) TO WS-IDRADNR                                 
147600     INSPECT WS-IDRADNR REPLACING LEADING SPACE BY ZERO                   
147700     MOVE WS-IDRADNR TO W-IDRADNR                                         
147800     MOVE '0'        TO W-KDSTRRAD                                        
147900     PERFORM IMS-GET-SATB-SATB11                                          
148000     IF SEGMENT-FINNS                                                     
148100       CONTINUE                                                           
148200     ELSE                                                                 
148300       MOVE '9'      TO W-KDSTRRAD                                        
148400       PERFORM IMS-GET-SATB-SATB11                                        
148500     END-IF                                                               
148600                                                                          
148700     IF SEGMENT-FINNS                                                     
148800       MOVE WS-IDARTNR-KONVERTERAT TO W-IDARTNR-2                         
148900       MOVE SPACE                  TO SATB11-RAD-KDSTRRAD                 
149000       PERFORM IMS-ISRT-SATB-SATB11                                       
149100     END-IF                                                               
149200     .                                                                    
149300     SKIP3                                                                
149400 HAA-LAGG-UPP-ARTIKEL-PA-WLXXAZ SECTION.                                  
149500***********************************************************               
149600* SÖKT ARTIKEL (IDARTNR ELLER IDLEVNR+BELEVART) LÄGGS UPP *               
149700* OCH ÄR DÄRMED 'LÅST' FÖR ANDRA USERID                   *               
149800***********************************************************               
149900                                                                          
150000     MOVE '1151'    TO W-IDHTYP                                           
150100     MOVE LOW-VALUE TO W-LOW-VALUE-2                                      
150200     IF SOEKNYCKEL-IDARTNR                                                
150300       MOVE WS-IDARTNR      TO XXAZ11-1152-IDARTNR                        
150400       MOVE SPACE           TO XXAZ11-1152-IDLEVNR                        
150500                               XXAZ11-1152-BELEVART                       
150600     ELSE                                                                 
150700       MOVE WS-IDLEVNR      TO XXAZ11-1152-IDLEVNR                        
150800       MOVE WS-BELEVART     TO XXAZ11-1152-BELEVART                       
150900       MOVE ZERO            TO XXAZ11-1152-IDARTNR                        
151000     END-IF                                                               
151100     MOVE MSG-SIGNON-USERID TO XXAZ11-1152-IDUSER                         
151200     MOVE LOW-VALUE         TO XXAZ11-1152-LOW-VALUE                      
151300     MOVE SPACE             TO XXAZ11-1152-FLAGGA                         
151400                               XXAZ11-1152-IDAO                           
151500     MOVE DAGENS-DATUM      TO XXAZ11-1152-TIREGDAT                       
151600**** SPÄRWDATUM SPARAS UNDAN                                              
151700                               WS-SPAR-SPAERWDATUM                        
151800     MOVE ZERO              TO XXAZ11-1152-TISTODAT                       
151900     PERFORM IMS-ISRT-XXAZ-XXAZ11                                         
152000     MOVE JA TO SOEKNYCKEL-LAAST-AV-EGET-ID-SW                            
152100     .                                                                    
152200     SKIP3                                                                
152300 HB-TABORT-STRUKTUR-RAD SECTION.                                          
152400***********************************************************               
152500* BORTTAG (ÅNGRA) VALD STRUKTURRAD. OM INGA FLER RADER    *               
152600* FINNS EFTER BORTTAG TAS OCKSÅ DEN KONV. STRUKTUREN BORT *               
152700***********************************************************               
152800                                                                          
152900     MOVE MID-IDARTNR(INDX) TO WS-IDARTNR-2                               
153000     INSPECT WS-IDARTNR-2 REPLACING LEADING SPACE BY ZERO                 
153100     MOVE WS-IDARTNR-2 TO WS-IDARTNR-NUM                                  
153200     COMPUTE WS-IDARTNR-KONVERTERAT = 999999999 -                         
153300                                      WS-IDARTNR-NUM                      
153400     MOVE WS-IDARTNR-KONVERTERAT TO W-IDARTNR-2                           
153500     PERFORM IMS-GET-SATB-SATB01                                          
153600     IF SEGMENT-FINNS                                                     
153700       MOVE SPACE             TO W-KDSTRRAD                               
153800****** RADERNA LIGGER MED KDSTRRAD = ' ' I VALDA STRUKTUR                 
153900       MOVE MID-IDRADNR(INDX) TO WS-IDRADNR                               
154000       INSPECT WS-IDRADNR REPLACING LEADING SPACE BY ZERO                 
154100       MOVE WS-IDRADNR TO W-IDRADNR                                       
154200       PERFORM IMS-GET-SATB-SATB11                                        
154300       IF SEGMENT-FINNS                                                   
154400         PERFORM IMS-DLET-SATB                                            
154500                                                                          
154600         PERFORM IMS-GET-SATB-SATB01                                      
154700         PERFORM IMS-GET-SATB-SATB11-OKVAL                                
154800         IF SEGMENT-FINNS                                                 
154900*********  FLER RADER FINNS VALDA FÖR AKTUELL STRUKTUR                    
155000           CONTINUE                                                       
155100         ELSE                                                             
155200*********  INGA RADER FINNS. TA BORT KONVERTERAD STRUKTUR                 
155300           PERFORM IMS-GET-SATB-SATB01                                    
155400           PERFORM IMS-DLET-SATB                                          
155500         END-IF                                                           
155600       END-IF                                                             
155700     END-IF                                                               
155800                                                                          
155900     PERFORM HBA-KOLLA-OM-SPAERR-SKALL-BORT                               
156000     .                                                                    
156100     SKIP3                                                                
156200 HBA-KOLLA-OM-SPAERR-SKALL-BORT SECTION.                                  
156300***********************************************                           
156400* OM INGA GÄLLANDE KONVERTERADE STRUKTURER    *                           
156500* FINNS KVAR TAS 'SPÄRREN' BORT PÅ SÖKNYCKELN *                           
156600***********************************************                           
156700                                                                          
156800     MOVE NEJ TO SIGNON-USER-HAR-KONV-STRUKT-SW                           
156900                                                                          
157000     MOVE MSG-SIGNON-USERID   TO W-IDUSER                                 
157100     MOVE WS-MIN-IDARTNR-KONV TO W-MIN-IDARTNR-KONV                       
157200     MOVE WS-MAX-IDARTNR-KONV TO W-MAX-IDARTNR-KONV                       
157300                                                                          
157400     PERFORM IMS-GET-SATB-DSEQ-UNIK                                       
157500     PERFORM UNTIL SEGMENT-SAKNAS                                         
157600       IF SEGMENT-FINNS                                                   
157700         MOVE 001                 TO WORK-KDCALL                          
157800         MOVE WC-CDC-SE           TO WORK-IDDC                            
157900         MOVE SATB01-STR-TIREGDAT TO WORK-TIAAMMDD-FOM                    
158000         MOVE DAGENS-DATUM        TO WORK-TIAAMMDD-TOM                    
158100         CALL WORKDAY USING WORK-KDCALL                                   
158200                            WORK-DATE-AREA                                
158300                            WORK-KDSVAR                                   
158400         IF (WORK-KVWORKD > 2) OR (WORK-KDSVAR-FEL)                       
158500*******  ARB-KDSVAR-FEL INNEBÄR ATT ANTALET DAGAR > 999.                  
158600           MOVE SATB01-STR-IDARTNR TO W-IDARTNR-2                         
158700           PERFORM IMS-GET-SATB-SATB01                                    
158800           PERFORM IMS-DLET-SATB                                          
158900                                                                          
159000           PERFORM IMS-GET-SATB-DSEQ-NEXT                                 
159100         ELSE                                                             
159200********** 'GÄLLANDE' KONVERTERAD STRUKTUR. MAN SÄTTER                    
159300********** STATUS TILL SEGMENT-SAKNAS FÖR ATT BRYTA                       
159400           MOVE 'GE' TO STATUS-WS                                         
159500           MOVE JA   TO SIGNON-USER-HAR-KONV-STRUKT-SW                    
159600         END-IF                                                           
159700       END-IF                                                             
159800     END-PERFORM                                                          
159900                                                                          
160000                                                                          
160100     IF SIGNON-USERID-HAR-KONV-STRUKT                                     
160200       CONTINUE                                                           
160300     ELSE                                                                 
160400*****  'SIGNON-USERID' SAKNAR GÄLLANDE KONVERTERADE STRUKTURER            
160500       PERFORM IMS-GET-XXAZ-XXAZ01                                        
160600       MOVE WS-IDARTNR        TO W-IDARTNR                                
160700       MOVE WS-IDLEVNR        TO W-IDLEVNR                                
160800       MOVE WS-BELEVART       TO W-BELEVART                               
160900       MOVE MSG-SIGNON-USERID TO W-IDUSER                                 
161000       PERFORM IMS-GET-XXAZ-XXAZ11                                        
161100       IF SEGMENT-FINNS                                                   
161200         PERFORM IMS-DLET-XXAZ-XXAZ11                                     
161300         MOVE NEJ TO SOEKNYCKEL-LAAST-AV-EGET-ID-SW                       
161400       END-IF                                                             
161500     END-IF                                                               
161600     .                                                                    
161700     EJECT                                                                
161800 I-KOLLA-INPUT-PRINTER SECTION.                                           
161900                                                                          
162000     MOVE NEJ TO ALLT-SW                                                  
162100                                                                          
162200     IF MID-KDPRTVAL = ALL '+'                                            
162300       MOVE NEJ TO INDATA-SW                                              
162400     ELSE                                                                 
162500       IF MID-KDPRTVAL = 'A' OR 'B' OR 'C' OR 'D'                         
162600         MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDPRTVAL-ATTR                   
162700         EVALUATE MID-KDPRTVAL                                            
162800           WHEN 'A' MOVE SATS-I-STR-RA     TO PRT-IDPRTLST                
162900           WHEN 'B' MOVE SATS-I-STR-RB     TO PRT-IDPRTLST                
163000           WHEN 'C' MOVE SATS-I-STR-BERPV  TO PRT-IDPRTLST                
163100           WHEN 'D' MOVE SATS-I-STR-CARP   TO PRT-IDPRTLST                
163200         END-EVALUATE                                                     
163300*        **************************************************               
163400*        *  HÄMTAR PRINTERNS LOGISKA NAMN TILL TEMFSINF   *               
163500*        **************************************************               
163600                                                                          
163700         MOVE 1                TO PRT-KDCALL                              
163800         CALL W006PRT USING PRT-W006PRT                                   
163900         IF PRT-IDLTERM = 'SAKNAS  '                                      
164000           MOVE NEJ TO INDATA-SW                                          
164100           MOVE MED9 (SPRAK-IX) TO MOD-TEMFSINF                           
164200         ELSE                                                             
164300           MOVE PRT-IDLTERM  TO MED6-IDLTERM     (SPRAK-IX)               
164400           MOVE PRT-BEPRTLST TO MED6-BEPRT       (SPRAK-IX)               
164500           MOVE ','          TO MED6-KOMMATECKEN (SPRAK-IX)               
164600         END-IF                                                           
164700       ELSE                                                               
164800         MOVE NEJ TO INDATA-SW                                            
164900       END-IF                                                             
165000     END-IF                                                               
165100                                                                          
165200     PERFORM MFS-ROR-EJ-FAELT-IN                                          
165300     PERFORM MFS-ROR-EJ-FAELT-UT                                          
165400     PERFORM MFS-LAS-IN-IGEN                                              
165500                                                                          
165600     IF INDATA-FEL                                                        
165700       MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDPRTVAL-ATTR                     
165800       MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                          
165900       CALL WMEDKONV USING MED-WMEDAREA                                   
166000       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
166100     END-IF                                                               
166200     .                                                                    
166300     EJECT                                                                
166400 J-SKICKA-TRANS-TILL-1291 SECTION.                                        
166500*****************************************                                 
166600* STARTA BAKGRUNDSPGM SOM PRINTAR LISTA *                                 
166700*****************************************                                 
166800                                                                          
166900     MOVE MID-W1I22101 TO MID-W1I29101                                    
167000     PERFORM IMS-INSERT-ALT-MSG                                           
167100     MOVE MED6 (SPRAK-IX) TO MOD-TEMFSINF                                 
167200     .                                                                    
167300     EJECT                                                                
167400 S01-HAEMTA-BEART-BSEQ SECTION.                                           
167500                                                                          
167600     PERFORM IMS-GET-BENA-BENA01-BSEQ                                     
167700     IF SEGMENT-FINNS                                                     
167800       MOVE WS-IDSKYLT TO W-IDSKYLT                                       
167900       PERFORM IMS-GET-BENA-BENA11-BSEQ                                   
168000       IF SEGMENT-FINNS                                                   
168100         MOVE BENA11-TEXT-BEART TO WS-BEART                               
168200       ELSE                                                               
168300         MOVE SPACE             TO WS-BEART                               
168400       END-IF                                                             
168500     ELSE                                                                 
168600       MOVE SPACE TO WS-BEART                                             
168700     END-IF                                                               
168800     .                                                                    
168900     EJECT                                                                
169000 S02-HAEMTA-BEART-ASEQ SECTION.                                           
169100                                                                          
169200     MOVE 'S' TO W-IDSKYLT                                                
169300     PERFORM IMS-GET-BENA-BENA01-ASEQ                                     
169400     PERFORM UNTIL SEGMENT-SAKNAS OR                                      
169500                    WS-KDBENHOM = BENA01-BEN-KDHOMONYM                    
169600       IF SEGMENT-FINNS                                                   
169700         IF WS-KDBENHOM = BENA01-BEN-KDHOMONYM                            
169800           CONTINUE                                                       
169900         ELSE                                                             
170000           PERFORM IMS-GET-BENA-BENA01-ASEQ-NEXT                          
170100         END-IF                                                           
170200       ELSE                                                               
170300         MOVE SPACE TO WS-BEART                                           
170400       END-IF                                                             
170500     END-PERFORM                                                          
170600                                                                          
170700     IF SEGMENT-FINNS                                                     
170800       MOVE WS-IDSKYLT TO W-IDSKYLT                                       
170900       PERFORM IMS-GET-BENA-BENA11-ASEQ                                   
171000       IF SEGMENT-FINNS                                                   
171100         MOVE BENA11-TEXT-BEART TO WS-BEART                               
171200       ELSE                                                               
171300         MOVE SPACE             TO WS-BEART                               
171400       END-IF                                                             
171500     END-IF                                                               
171600     .                                                                    
171700     EJECT                                                                
171800 S04-KOLLA-OM-RAD-SKA-MED SECTION.                                        
171900                                                                          
172000     MOVE NEJ         TO RAD-SW                                           
172100                         STRUKTURNR-FINNS-PAA-WDK6-SW                     
172200     MOVE WS-KDPRODSL TO WS-KDPRODSL-NUM                                  
172300                                                                          
172400     MOVE SATB01C-STR-IDARTNR TO W-IDARTNR-2                              
172500     PERFORM IMS-GET-ARTC01-PCB2                                          
172600     IF SEGMENT-FINNS                                                     
172700       MOVE JA TO STRUKTURNR-FINNS-PAA-WDK6-SW                            
172800     END-IF                                                               
172900                                                                          
173000     IF WS-1002-SATS = 'Y' OR 'J'                                         
173100       IF SATB01C-STR-IDLEVNR = '1002 '                                   
173200         IF WS-KDPRODSL-NUM = 0                                           
173300           MOVE JA TO RAD-SW                                              
173400         ELSE                                                             
173500           IF STRUKTURNR-FINNS-PAA-WDK6                                   
173600             IF ARTC012-ART-KDPRODSL = WS-KDPRODSL-NUM                    
173700               MOVE JA TO RAD-SW                                          
173800             END-IF                                                       
173900           ELSE                                                           
174000             IF SATB01C-STR-KDPRODSL = WS-KDPRODSL-NUM                    
174100               MOVE JA TO RAD-SW                                          
174200             END-IF                                                       
174300           END-IF                                                         
174400         END-IF                                                           
174500       END-IF                                                             
174600     ELSE                                                                 
174700       IF WS-1002-SATS = 'N'                                              
174800         IF SATB01C-STR-IDLEVNR NOT = '1002 '                             
174900           IF WS-KDPRODSL-NUM = 0                                         
175000             MOVE JA TO RAD-SW                                            
175100           ELSE                                                           
175200             IF STRUKTURNR-FINNS-PAA-WDK6                                 
175300               IF ARTC012-ART-KDPRODSL = WS-KDPRODSL-NUM                  
175400                 MOVE JA TO RAD-SW                                        
175500               END-IF                                                     
175600             ELSE                                                         
175700               IF SATB01C-STR-KDPRODSL = WS-KDPRODSL-NUM                  
175800                 MOVE JA TO RAD-SW                                        
175900               END-IF                                                     
176000             END-IF                                                       
176100           END-IF                                                         
176200         END-IF                                                           
176300       ELSE                                                               
176400******** WS-1002-SATS = SPACE                                             
176500         IF WS-KDPRODSL-NUM = 0                                           
176600           MOVE JA TO RAD-SW                                              
176700         ELSE                                                             
176800           IF STRUKTURNR-FINNS-PAA-WDK6                                   
176900             IF ARTC012-ART-KDPRODSL = WS-KDPRODSL-NUM                    
177000               MOVE JA TO RAD-SW                                          
177100             END-IF                                                       
177200           ELSE                                                           
177300             IF SATB01C-STR-KDPRODSL = WS-KDPRODSL-NUM                    
177400               MOVE JA TO RAD-SW                                          
177500             END-IF                                                       
177600           END-IF                                                         
177700         END-IF                                                           
177800                                                                          
177900       END-IF                                                             
178000                                                                          
178100     END-IF                                                               
178200     .                                                                    
178300     EJECT                                                                
178400 S05-KOLLA-OM-MID-INPUT-IFYLLD SECTION.                                   
178500                                                                          
178600     MOVE NEJ TO MID-INPUT-SW                                             
178700                                                                          
178800     MOVE +1 TO INDX                                                      
178900     PERFORM UNTIL (INDX > MAX-INDX) OR (MID-INPUT-IFYLLD)                
179000       IF MID-SELECT(INDX) NOT = ALL '+'                                  
179100         IF MID-SELECT(INDX) = '*' OR ' '                                 
179200           CONTINUE                                                       
179300         ELSE                                                             
179400           MOVE JA TO MID-INPUT-SW                                        
179500         END-IF                                                           
179600       END-IF                                                             
179700       ADD +1 TO INDX                                                     
179800     END-PERFORM                                                          
179900     .                                                                    
180000     EJECT                                                                
180100 S06-LAES-RADDATA-FOERSTA SECTION.                                        
180200                                                                          
180300     MOVE NEJ TO RAD-SW                                                   
180400                                                                          
180500                                                                          
180600     IF (MFS-ENTER) AND (MID-IDARTNR-ENTER > 0)                           
180700       MOVE MID-IDARTNR-ENTER  TO W-IDARTNR-2                             
180800       MOVE MID-KDSTRRAD-ENTER TO W-KDSTRRAD                              
180900       MOVE MID-IDRADNR-ENTER  TO W-IDRADNR                               
181000       PERFORM IMS-GET-SATB-CSEQ-UNIK                                     
181100       IF SEGMENT-FINNS                                                   
181200         CONTINUE                                                         
181300       ELSE                                                               
181400         PERFORM IMS-GET-SATB-CSEQ-NEXT                                   
181500       END-IF                                                             
181600     ELSE                                                                 
181700       IF (MFS-NEXT) AND (MID-IDARTNR-NEXT > 0)                           
181800         MOVE MID-IDARTNR-NEXT  TO W-IDARTNR-2                            
181900         MOVE MID-KDSTRRAD-NEXT TO W-KDSTRRAD                             
182000         MOVE MID-IDRADNR-NEXT  TO W-IDRADNR                              
182100         PERFORM IMS-GET-SATB-CSEQ-UNIK                                   
182200         IF SEGMENT-FINNS                                                 
182300           CONTINUE                                                       
182400         ELSE                                                             
182500           PERFORM IMS-GET-SATB-CSEQ-NEXT                                 
182600         END-IF                                                           
182700       ELSE                                                               
182800         PERFORM IMS-GET-SATB-CSEQ-NEXT                                   
182900       END-IF                                                             
183000     END-IF                                                               
183100                                                                          
183200     PERFORM UNTIL SEGMENT-SAKNAS OR RAD-OK                               
183300                                                                          
183400       IF SEGMENT-FINNS                                                   
183500         IF SATB01C-STR-IDARTNR NOT < WS-MAX-IDARTNR                      
183600********** NÄR MAN 'FÅR IN' ETT KONVERTERAT STRUKTURNR                    
183700********** FINNS DET BARA KONV STRUKTURER KVAR OCH                        
183800********** MAN SÄTTER STATUS TILL SEGMENT-SAKNAS                          
183900           MOVE 'GE' TO STATUS-WS                                         
184000         ELSE                                                             
184100           IF SATB01C-STR-TIBORT > 0                                      
184200             PERFORM IMS-GET-SATB-CSEQ-NEXT                               
184300           ELSE                                                           
184400             MOVE SATB11C-RAD-TISTODAT   TO TMP1-YYMMDD                   
184500             MOVE DAGENS-DATUM           TO TMP2-YYMMDD                   
184600             PERFORM WY2000P1                                             
184700             IF TMP1-YYMMDD > TMP2-YYMMDD                                 
184800               PERFORM S04-KOLLA-OM-RAD-SKA-MED                           
184900               IF RAD-OK                                                  
185000                 CONTINUE                                                 
185100               ELSE                                                       
185200                 PERFORM IMS-GET-SATB-CSEQ-NEXT                           
185300               END-IF                                                     
185400             ELSE                                                         
185500               PERFORM IMS-GET-SATB-CSEQ-NEXT                             
185600             END-IF                                                       
185700           END-IF                                                         
185800         END-IF                                                           
185900       END-IF                                                             
186000                                                                          
186100     END-PERFORM                                                          
186200     .                                                                    
186300     EJECT                                                                
186400 S07-LAES-RADDATA-NAESTA SECTION.                                         
186500                                                                          
186600     MOVE NEJ TO RAD-SW                                                   
186700                                                                          
186800     PERFORM IMS-GET-SATB-CSEQ-NEXT                                       
186900                                                                          
187000     PERFORM UNTIL (SEGMENT-SAKNAS OR RAD-OK)                             
187100                                                                          
187200       IF SEGMENT-FINNS                                                   
187300         IF SATB01C-STR-IDARTNR NOT < WS-MAX-IDARTNR                      
187400********** NÄR MAN 'FÅR IN' ETT KONVERTERAT STRUKTURNR                    
187500********** FINNS DET BARA KONV STRUKTURER KVAR OCH                        
187600********** MAN SÄTTER STATUS TILL SEGMENT-SAKNAS                          
187700           MOVE 'GE' TO STATUS-WS                                         
187800         ELSE                                                             
187900           IF SATB01C-STR-TIBORT > 0                                      
188000             PERFORM IMS-GET-SATB-CSEQ-NEXT                               
188100           ELSE                                                           
188200             MOVE SATB11C-RAD-TISTODAT   TO TMP1-YYMMDD                   
188300             MOVE DAGENS-DATUM           TO TMP2-YYMMDD                   
188400             PERFORM WY2000P1                                             
188500             IF TMP1-YYMMDD > TMP2-YYMMDD                                 
188600               PERFORM S04-KOLLA-OM-RAD-SKA-MED                           
188700               IF RAD-OK                                                  
188800                 CONTINUE                                                 
188900               ELSE                                                       
189000                 PERFORM IMS-GET-SATB-CSEQ-NEXT                           
189100               END-IF                                                     
189200             ELSE                                                         
189300               PERFORM IMS-GET-SATB-CSEQ-NEXT                             
189400             END-IF                                                       
189500           END-IF                                                         
189600         END-IF                                                           
189700       END-IF                                                             
189800                                                                          
189900     END-PERFORM                                                          
190000     .                                                                    
190100     EJECT                                                                
190200 MFS-RENSA-FAELT-UT SECTION.                                              
190300                                                                          
190400*    --- ALLA UTDATA-FÄLT                                                 
190500*    --- INKL. BLÄDDRINGSNYCKLAR                                          
190600     MOVE MFS-RENSA-FAELT TO MOD-BEART-UT                                 
190700                             MOD-TEARTNOT                                 
190800                             MOD-TEARTNOT-7                               
190900                             MOD-IDARTNR-ENTER                            
191000                             MOD-KDSTRRAD-ENTER                           
191100                             MOD-IDRADNR-ENTER                            
191200                             MOD-IDARTNR-NEXT                             
191300                             MOD-KDSTRRAD-NEXT                            
191400                             MOD-IDRADNR-NEXT                             
191500     MOVE +1 TO INDX                                                      
191600     PERFORM UNTIL INDX > MAX-INDX                                        
191700       PERFORM MFS-RENSA-RAD-FAELT-UT                                     
191800       ADD +1 TO INDX                                                     
191900     END-PERFORM                                                          
192000     .                                                                    
192100     SKIP2                                                                
192200 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
192300                                                                          
192400*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
192500     MOVE MFS-RENSA-FAELT TO MOD-IDRADNR (INDX)                           
192600                             MOD-IDARTNR (INDX)                           
192700                             MOD-BEART   (INDX)                           
192800                             MOD-REANTPSA(INDX)                           
192900                             MOD-IDSTRTYP(INDX)                           
193000                             MOD-KDERS   (INDX)                           
193100                             MOD-IDLEVNR (INDX)                           
193200                             MOD-KDPRODSL(INDX)                           
193300                             MOD-KDPSLLOC(INDX)                           
193400     .                                                                    
193500     SKIP2                                                                
193600 MFS-RENSA-FAELT-IN SECTION.                                              
193700                                                                          
193800*    --- ALLA INDATA-FÄLT                                                 
193900     MOVE +1 TO INDX                                                      
194000     PERFORM UNTIL INDX > MAX-INDX                                        
194100       MOVE MFS-RENSA-FAELT TO MOD-SELECT(INDX)                           
194200       ADD 1 TO INDX                                                      
194300     END-PERFORM                                                          
194400     MOVE MFS-RENSA-FAELT TO MOD-KDPRTVAL                                 
194500     .                                                                    
194600     EJECT                                                                
194700 MFS-ROR-EJ-FAELT-UT  SECTION.                                            
194800                                                                          
194900*    --- ALLA UTDATA-FÄLT                                                 
195000*    --- INKL BLÄDDRINGSNYCKLAR OCH RAD-DATA                              
195100     MOVE MFS-ROER-EJ-FAELT TO MOD-IDLEVNR-UT                             
195200                               MOD-BELEVART-UT                            
195300                               MOD-IDARTNR-SPAR                           
195400                               MOD-IDSKYLT-UT                             
195500                               MOD-1002-SATS-UT                           
195600                               MOD-KDPRODSL-UT                            
195700                               MOD-BEART-UT                               
195800                               MOD-TEARTNOT                               
195900                               MOD-TEARTNOT-7                             
196000                               MOD-IDARTNR-ENTER                          
196100                               MOD-KDSTRRAD-ENTER                         
196200                               MOD-IDRADNR-ENTER                          
196300                               MOD-IDARTNR-NEXT                           
196400                               MOD-KDSTRRAD-NEXT                          
196500                               MOD-IDRADNR-NEXT                           
196600                                                                          
196700     MOVE +1 TO INDX                                                      
196800     PERFORM UNTIL INDX > MAX-INDX                                        
196900       PERFORM MFS-ROR-EJ-RAD-FAELT-UT                                    
197000       ADD +1 TO INDX                                                     
197100     END-PERFORM                                                          
197200     .                                                                    
197300     SKIP2                                                                
197400 MFS-ROR-EJ-RAD-FAELT-UT  SECTION.                                        
197500                                                                          
197600*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
197700     MOVE MFS-ROER-EJ-FAELT TO MOD-IDRADNR (INDX)                         
197800                               MOD-IDARTNR (INDX)                         
197900                               MOD-BEART   (INDX)                         
198000                               MOD-REANTPSA(INDX)                         
198100                               MOD-IDSTRTYP(INDX)                         
198200                               MOD-KDERS   (INDX)                         
198300                               MOD-IDLEVNR (INDX)                         
198400                               MOD-KDPRODSL(INDX)                         
198500                               MOD-KDPSLLOC(INDX)                         
198600     .                                                                    
198700     SKIP2                                                                
198800 MFS-ROR-EJ-FAELT-IN  SECTION.                                            
198900                                                                          
199000*    --- ALLA INDATA-FÄLT                                                 
199100     MOVE +1 TO INDX                                                      
199200     PERFORM UNTIL INDX > MAX-INDX                                        
199300       MOVE MFS-ROER-EJ-FAELT TO MOD-SELECT(INDX)                         
199400       ADD 1 TO INDX                                                      
199500     END-PERFORM                                                          
199600     MOVE MFS-ROER-EJ-FAELT TO MOD-KDPRTVAL                               
199700     .                                                                    
199800     EJECT                                                                
199900 MFS-LAS-IN-IGEN SECTION.                                                 
200000                                                                          
200100*    --- ALLA INDATA-FÄLT                                                 
200200     MOVE +1 TO INDX                                                      
200300     PERFORM UNTIL INDX > MAX-INDX                                        
200400       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-SELECT-ATTR(INDX)                
200500       ADD 1 TO INDX                                                      
200600     END-PERFORM                                                          
200700     .                                                                    
200800     EJECT                                                                
200900* --- IMS SEKTIONER ---                                                   
201000     SKIP3                                                                
201100 IMS-GET-MSG SECTION.                                                     
201200                                                                          
201300     MOVE '  QC' TO GODK-STATUSKODER                                      
201400     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
201500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
201600     PERFORM IMS-STATUSKONTROLL                                           
201700     .                                                                    
201800     SKIP3                                                                
201900 IMS-INSERT-MSG SECTION.                                                  
202000                                                                          
202100     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
202200       MOVE '0' TO MFS-KDHUVOMR                                           
202300     END-IF                                                               
202400     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
202500     MOVE SPACE TO GODK-STATUSKODER                                       
202600     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
202700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
202800     PERFORM IMS-STATUSKONTROLL                                           
202900     .                                                                    
203000     EJECT                                                                
203100 IMS-INSERT-ALT-MSG SECTION.                                              
203200                                                                          
203300     MOVE SPACE TO GODK-STATUSKODER                                       
203400     CALL CBLTDLI USING ISRT ALT-PCB W-PROG-TO-PROG-SW                    
203500     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
203600     PERFORM IMS-STATUSKONTROLL                                           
203700     .                                                                    
203800     EJECT                                                                
203900 IMS-DLET-SATB SECTION.                                                   
204000                                                                          
204100     MOVE '  ' TO GODK-STATUSKODER                                        
204200     CALL CBLTDLI USING DLET SATB-PCB DLI-IO-AREA                         
204300     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
204400     PERFORM IMS-STATUSKONTROLL                                           
204500     .                                                                    
204600     SKIP3                                                                
204700 IMS-GET-SATB-SATB01 SECTION.                                             
204800                                                                          
204900     STRING 'WLSATB01(IDARTNR  =' W-IDARTNR-2-X ')'                       
205000          DELIMITED BY SIZE INTO SSA1                                     
205100     MOVE '  GE' TO GODK-STATUSKODER                                      
205200     CALL CBLTDLI USING GHU SATB-PCB DLI-IO-AREA SSA1                     
205300     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
205400     PERFORM IMS-STATUSKONTROLL                                           
205500     .                                                                    
205600     SKIP3                                                                
205700 IMS-GET-SATB-SATB11 SECTION.                                             
205800                                                                          
205900     STRING 'WLSATB11*F(WDJ111KY =' W-WDJ111KY-X ')'                      
206000          DELIMITED BY SIZE INTO SSA1                                     
206100     MOVE '  GE' TO GODK-STATUSKODER                                      
206200     CALL CBLTDLI USING GHNP SATB-PCB DLI-IO-AREA SSA1                    
206300     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
206400     PERFORM IMS-STATUSKONTROLL                                           
206500     .                                                                    
206600     SKIP3                                                                
206700 IMS-GET-SATB-SATB11-OKVAL SECTION.                                       
206800                                                                          
206900     MOVE 'WLSATB11 ' TO SSA1                                             
207000     MOVE '  GE' TO GODK-STATUSKODER                                      
207100     CALL CBLTDLI USING GHNP SATB-PCB DLI-IO-AREA SSA1                    
207200     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
207300     PERFORM IMS-STATUSKONTROLL                                           
207400     .                                                                    
207500     SKIP3                                                                
207600 IMS-ISRT-SATB-SATB01 SECTION.                                            
207700                                                                          
207800     MOVE 'WLSATB01 ' TO SSA1                                             
207900     MOVE '  ' TO GODK-STATUSKODER                                        
208000     CALL CBLTDLI USING ISRT SATB-PCB DLI-IO-AREA SSA1                    
208100     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
208200     PERFORM IMS-STATUSKONTROLL                                           
208300     .                                                                    
208400     SKIP3                                                                
208500 IMS-ISRT-SATB-SATB11 SECTION.                                            
208600                                                                          
208700     STRING 'WLSATB01(IDARTNR  =' W-IDARTNR-2-X ')'                       
208800          DELIMITED BY SIZE INTO SSA1                                     
208900     MOVE 'WLSATB11 ' TO SSA2                                             
209000     MOVE '  ' TO GODK-STATUSKODER                                        
209100     CALL CBLTDLI USING ISRT SATB-PCB DLI-IO-AREA SSA1 SSA2               
209200     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
209300     PERFORM IMS-STATUSKONTROLL                                           
209400     .                                                                    
209500     EJECT                                                                
209600 IMS-GET-SATB-CSEQ-NEXT SECTION.                                          
209700                                                                          
209800     STRING 'WLSATB11*D(WDJ1CSEQ =' W-WDJ1CSEQ-X ')'                      
209900          DELIMITED BY SIZE INTO SSA1                                     
210000     MOVE 'WLSATB01 ' TO SSA2                                             
210100     MOVE '  GE' TO GODK-STATUSKODER                                      
210200     CALL CBLTDLI USING GN SATB-C-PCB DLI-IO-AREA-2 SSA1 SSA2             
210300     MOVE SATB-C-STATUS-CODE TO STATUS-WS                                 
210400     PERFORM IMS-STATUSKONTROLL                                           
210500     .                                                                    
210600     SKIP3                                                                
210700 IMS-GET-SATB-CSEQ-UNIK SECTION.                                          
210800                                                                          
210900     STRING 'WLSATB11*D(WDJ1CSEQ =' W-WDJ1CSEQ-X                          
211000                      '&WDJ111KY =' W-WDJ111KY-X ')'                      
211100          DELIMITED BY SIZE INTO SSA1                                     
211200     STRING 'WLSATB01(IDARTNR  =' W-IDARTNR-2-X ')'                       
211300          DELIMITED BY SIZE INTO SSA2                                     
211400     MOVE '  GE' TO GODK-STATUSKODER                                      
211500     CALL CBLTDLI USING GU SATB-C-PCB DLI-IO-AREA-2 SSA1 SSA2             
211600     MOVE SATB-C-STATUS-CODE TO STATUS-WS                                 
211700     PERFORM IMS-STATUSKONTROLL                                           
211800     .                                                                    
211900     EJECT                                                                
212000 IMS-GET-SATB-DSEQ-NEXT SECTION.                                          
212100**** OBS! ENDAST KONVERTERADE STRUKTURER 'TAS IN'                         
212200                                                                          
212300     STRING 'WLSATB01(WDJ1DSEQ>=' W-IDUSER-X                              
212400                                  W-MIN-IDARTNR-KONV-X                    
212500                    '&WDJ1DSEQ<=' W-IDUSER-X                              
212600                                  W-MAX-IDARTNR-KONV-X ')'                
212700          DELIMITED BY SIZE INTO SSA1                                     
212800     MOVE '  GE' TO GODK-STATUSKODER                                      
212900     CALL CBLTDLI USING GHN SATB-D-PCB DLI-IO-AREA SSA1                   
213000     MOVE SATB-D-STATUS-CODE TO STATUS-WS                                 
213100     PERFORM IMS-STATUSKONTROLL                                           
213200     .                                                                    
213300     SKIP3                                                                
213400 IMS-GET-SATB-DSEQ-UNIK SECTION.                                          
213500**** OBS! ENDAST KONVERTERADE STRUKTURER 'TAS IN'                         
213600                                                                          
213700     STRING 'WLSATB01(WDJ1DSEQ>=' W-IDUSER-X                              
213800                                  W-MIN-IDARTNR-KONV-X                    
213900                    '&WDJ1DSEQ<=' W-IDUSER-X                              
214000                                  W-MAX-IDARTNR-KONV-X ')'                
214100          DELIMITED BY SIZE INTO SSA1                                     
214200     MOVE '  GE' TO GODK-STATUSKODER                                      
214300     CALL CBLTDLI USING GHU SATB-D-PCB DLI-IO-AREA SSA1                   
214400     MOVE SATB-D-STATUS-CODE TO STATUS-WS                                 
214500     PERFORM IMS-STATUSKONTROLL                                           
214600     .                                                                    
214700     EJECT                                                                
214800 IMS-GET-XXAZ-XXAZ01 SECTION.                                             
214900                                                                          
215000     STRING 'WLXXAZ01(WDGXKEY  =' W-WDGXKEY-X ')'                         
215100          DELIMITED BY SIZE INTO SSA1                                     
215200     MOVE '    ' TO GODK-STATUSKODER                                      
215300     CALL CBLTDLI USING GU XXAZ-PCB DLI-IO-AREA SSA1                      
215400     MOVE XXAZ-STATUS-CODE TO STATUS-WS                                   
215500     PERFORM IMS-STATUSKONTROLL                                           
215600     .                                                                    
215700     SKIP3                                                                
215800 IMS-GET-XXAZ-XXAZ11 SECTION.                                             
215900                                                                          
216000     STRING 'WLXXAZ11*F(WDGXKEY  =' W-IDLEVNR-X                           
216100                                    W-BELEVART-X                          
216200                                    W-IDARTNR-X                           
216300                                    W-IDUSER-X                            
216400                                    W-LOW-VALUE-X ')'                     
216500          DELIMITED BY SIZE INTO SSA1                                     
216600     MOVE '  GE' TO GODK-STATUSKODER                                      
216700     CALL CBLTDLI USING GHNP XXAZ-PCB DLI-IO-AREA SSA1                    
216800     MOVE XXAZ-STATUS-CODE TO STATUS-WS                                   
216900     PERFORM IMS-STATUSKONTROLL                                           
217000     .                                                                    
217100     SKIP3                                                                
217200 IMS-GET-XXAZ-XXAZ11-IDARTNR SECTION.                                     
217300                                                                          
217400     STRING 'WLXXAZ11*F(IDARTNR  =' W-IDARTNR-X ')'                       
217500          DELIMITED BY SIZE INTO SSA1                                     
217600     MOVE '  GE' TO GODK-STATUSKODER                                      
217700     CALL CBLTDLI USING GHNP XXAZ-PCB DLI-IO-AREA SSA1                    
217800     MOVE XXAZ-STATUS-CODE TO STATUS-WS                                   
217900     PERFORM IMS-STATUSKONTROLL                                           
218000     .                                                                    
218100     SKIP3                                                                
218200 IMS-GET-XXAZ-XXAZ11-IDLEV-BELE SECTION.                                  
218300                                                                          
218400     STRING 'WLXXAZ11*F(IDLEVNR  =' W-IDLEVNR-X                           
218500                    '&BELEVART =' W-BELEVART-X ')'                        
218600          DELIMITED BY SIZE INTO SSA1                                     
218700     MOVE '  GE' TO GODK-STATUSKODER                                      
218800     CALL CBLTDLI USING GHNP XXAZ-PCB DLI-IO-AREA SSA1                    
218900     MOVE XXAZ-STATUS-CODE TO STATUS-WS                                   
219000     PERFORM IMS-STATUSKONTROLL                                           
219100     .                                                                    
219200     SKIP3                                                                
219300 IMS-ISRT-XXAZ-XXAZ11 SECTION.                                            
219400                                                                          
219500     STRING 'WLXXAZ01(WDGXKEY  =' W-WDGXKEY-X ')'                         
219600          DELIMITED BY SIZE INTO SSA1                                     
219700     MOVE 'WLXXAZ11 ' TO SSA2                                             
219800     MOVE '  ' TO GODK-STATUSKODER                                        
219900     CALL CBLTDLI USING ISRT XXAZ-PCB DLI-IO-AREA SSA1 SSA2               
220000     MOVE XXAZ-STATUS-CODE TO STATUS-WS                                   
220100     PERFORM IMS-STATUSKONTROLL                                           
220200     .                                                                    
220300     SKIP3                                                                
220400 IMS-DLET-XXAZ-XXAZ11 SECTION.                                            
220500                                                                          
220600     MOVE '  ' TO GODK-STATUSKODER                                        
220700     CALL CBLTDLI USING DLET XXAZ-PCB DLI-IO-AREA                         
220800     MOVE XXAZ-STATUS-CODE TO STATUS-WS                                   
220900     PERFORM IMS-STATUSKONTROLL                                           
221000     .                                                                    
221100     SKIP2                                                                
221200 IMS-GET-XXAZ-XXAZ21 SECTION.                                             
221300                                                                          
221400     STRING 'WLXXAZ11*F(WDGXKEY  =' W-IDLEVNR-X                           
221500                                    W-BELEVART-X                          
221600                                    W-IDARTNR-X                           
221700                                    W-IDUSER-X                            
221800                                    W-LOW-VALUE-X ')'                     
221900          DELIMITED BY SIZE INTO SSA1                                     
222000     MOVE 'WLXXAZ21 ' TO SSA2                                             
222100     MOVE '  GE'      TO GODK-STATUSKODER                                 
222200     CALL CBLTDLI USING GHNP XXAZ-PCB DLI-IO-AREA SSA1 SSA2               
222300     MOVE XXAZ-STATUS-CODE TO STATUS-WS                                   
222400     PERFORM IMS-STATUSKONTROLL                                           
222500     .                                                                    
222600     EJECT                                                                
222700 IMS-GET-ARTC01-PCB2 SECTION.                                             
222800                                                                          
222900     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-2-X ')'                       
223000          DELIMITED BY SIZE INTO SSA1                                     
223100     MOVE '  GE' TO GODK-STATUSKODER                                      
223200     CALL CBLTDLI USING GU ARTC2-PCB DLI-IO-AREA-3 SSA1                   
223300     MOVE ARTC2-STATUS-CODE TO STATUS-WS                                  
223400     PERFORM IMS-STATUSKONTROLL                                           
223500     .                                                                    
223600     EJECT                                                                
223700 IMS-GET-ARTC-ARTC01 SECTION.                                             
223800                                                                          
223900     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-2-X ')'                       
224000          DELIMITED BY SIZE INTO SSA1                                     
224100     MOVE '  GE' TO GODK-STATUSKODER                                      
224200     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA SSA1                      
224300     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
224400     PERFORM IMS-STATUSKONTROLL                                           
224500     .                                                                    
224600     SKIP3                                                                
224700 IMS-GET-ARTC-ARTC25 SECTION.                                             
224800                                                                          
224900     MOVE 'WLARTC11 ' TO SSA1                                             
225000     STRING 'WLARTC25(KDNOTTYP =' W-KDNOTTYP-X ')'                        
225100          DELIMITED BY SIZE INTO SSA2                                     
225200     MOVE '  GE' TO GODK-STATUSKODER                                      
225300     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA SSA1 SSA2                
225400     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
225500     PERFORM IMS-STATUSKONTROLL                                           
225600     .                                                                    
225700     SKIP3                                                                
225800 IMS-GET-ARTC-ARTC11 SECTION.                                             
225900                                                                          
226000     MOVE 'WLARTC11 ' TO SSA1                                             
226100     MOVE '  GE' TO GODK-STATUSKODER                                      
226200     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA SSA1                     
226300     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
226400     PERFORM IMS-STATUSKONTROLL                                           
226500     .                                                                    
226600     EJECT                                                                
226700 IMS-GET-BENA-BENA01-ASEQ SECTION.                                        
226800                                                                          
226900     STRING 'WLBENA01(WDD3ASEQ =' W-IDSKYLT-X                             
227000                                  W-BEART-X ')'                           
227100          DELIMITED BY SIZE INTO SSA1                                     
227200     MOVE '  GE' TO GODK-STATUSKODER                                      
227300     CALL CBLTDLI USING GU BENA-A-PCB DLI-IO-AREA SSA1                    
227400     MOVE BENA-A-STATUS-CODE TO STATUS-WS                                 
227500     PERFORM IMS-STATUSKONTROLL                                           
227600     .                                                                    
227700     SKIP3                                                                
227800 IMS-GET-BENA-BENA01-ASEQ-NEXT SECTION.                                   
227900                                                                          
228000     STRING 'WLBENA01(WDD3ASEQ =' W-IDSKYLT-X                             
228100                                  W-BEART-X ')'                           
228200          DELIMITED BY SIZE INTO SSA1                                     
228300     MOVE '  GE' TO GODK-STATUSKODER                                      
228400     CALL CBLTDLI USING GN BENA-A-PCB DLI-IO-AREA SSA1                    
228500     MOVE BENA-A-STATUS-CODE TO STATUS-WS                                 
228600     PERFORM IMS-STATUSKONTROLL                                           
228700     .                                                                    
228800     SKIP3                                                                
228900 IMS-GET-BENA-BENA11-ASEQ SECTION.                                        
229000                                                                          
229100     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
229200          DELIMITED BY SIZE INTO SSA1                                     
229300     MOVE '  GE' TO GODK-STATUSKODER                                      
229400     CALL CBLTDLI USING GNP BENA-A-PCB DLI-IO-AREA SSA1                   
229500     MOVE BENA-A-STATUS-CODE TO STATUS-WS                                 
229600     PERFORM IMS-STATUSKONTROLL                                           
229700     .                                                                    
229800     EJECT                                                                
229900 IMS-GET-BENA-BENA01-BSEQ SECTION.                                        
230000                                                                          
230100     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-2-X ')'                       
230200          DELIMITED BY SIZE INTO SSA1                                     
230300     MOVE '  GE' TO GODK-STATUSKODER                                      
230400     CALL CBLTDLI USING GU BENA-B-PCB DLI-IO-AREA SSA1                    
230500     MOVE BENA-B-STATUS-CODE TO STATUS-WS                                 
230600     PERFORM IMS-STATUSKONTROLL                                           
230700     .                                                                    
230800     SKIP3                                                                
230900 IMS-GET-BENA-BENA11-BSEQ SECTION.                                        
231000                                                                          
231100     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
231200          DELIMITED BY SIZE INTO SSA1                                     
231300     MOVE '  GE' TO GODK-STATUSKODER                                      
231400     CALL CBLTDLI USING GNP BENA-B-PCB DLI-IO-AREA SSA1                   
231500     MOVE BENA-B-STATUS-CODE TO STATUS-WS                                 
231600     PERFORM IMS-STATUSKONTROLL                                           
231700     .                                                                    
231800     EJECT                                                                
231900 IMS-STATUSKONTROLL SECTION.                                              
232000                                                                          
232100     SET STATUS-IX TO 1                                                   
232200     SEARCH GODK-STATUS                                                   
232300       AT END CALL FELLOG                                                 
232400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
232500     END-SEARCH                                                           
232600     .                                                                    
232700     EJECT                                                                
232800*    -COPY WY2000P1                                                       
